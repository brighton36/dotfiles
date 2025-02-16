#!/usr/bin/env python3

# Mostly this script contains all the hyprland functionality that we need, that
# isn't natively supported by hyprland. Which, makes this a bit of a potporri
# script. I think I prefer this uberscript, over having a dozen scripts in my
# bin.

import os, subprocess, socket, re, argparse, time, json

OPERATIONS=['switch', 'next', 'prev', 'movespecial', 'togglespecial', 'monitor',
            'togglebrightness', 'openurl']
FIRST_WORKSPACE=1
LAST_WORKSPACE=9

HYPRCTL="/usr/bin/hyprctl"
BRIGHTNESSCTL="/usr/bin/brightnessctl"
BROTAB="brotab"
NOTIFY="/usr/bin/notify-send"
FIREFOX="/usr/bin/firefox"

def run(*args,**kwargs):
  result = subprocess.run(list(map(lambda a: str(a), args)),
                          input=kwargs.get('input', None),
                          close_fds=kwargs.get('close_fds', None),
                          capture_output=True,
                          text=True)
  if result.returncode != 0:
    raise Exception("Error running {} ({}): {}".format(args[0],
                                                       result.returncode,
                                                       result.stdout))
  return result.stdout

def hyprctl(*args, **kwargs):
  stdout = run(HYPRCTL, *args)
  # TODO It might be smarter to just support a assertOutput=/^ok$/ param to run...
  if (kwargs.get("assertOk") and not re.match(r'^ok$', stdout)):
    raise Exception("Error in hyprctl: {}".format(stdout))
  return stdout

def togglebrightness(device):
  if (int(brightnessctl("-d", device, "g")) == 0):
    run(BRIGHTNESSCTL,"-d", device, "s", run(BRIGHTNESSCTL,"-d", device, "m"))
  else:
    run(BRIGHTNESSCTL,"-d", device, "s", 0)
  return

def active_workspace():
  return int(json.loads(hyprctl('activeworkspace', '-j'))['id'])

def operation_check(arg_value, supported_operations):
  if not arg_value in supported_operations:
    raise argparse.ArgumentTypeError("Unrecognized operation \"{}\".".format(arg_value))
  return arg_value

def active_window():
  return json.loads(hyprctl('activewindow', '-j'))

def monitors():
  ret = []
  for monitor_parts in re.findall(r"^Monitor ([^ ]+) \(([^\)]+)\):\n\t([^\n]+)\n(.+?)\n\n",
                                  hyprctl('monitors'),
                                  re.MULTILINE | re.DOTALL):
    ret.append( {**{'port': monitor_parts[0], 'id': monitor_parts[1], 'resolution': monitor_parts[2]},
            **dict(re.findall(r'^[ \t]+([^:]+):[ ]*(.+)$', monitor_parts[3], flags=re.MULTILINE))})
  return ret

def focus_workspace(n):
  hyprctl('dispatch', 'workspace', n, assertOk=True)

def focus_window(address):
  hyprctl('dispatch', 'focuswindow', "address:"+address, assertOk=True)

def move_to_workspace(s):
  hyprctl('dispatch', 'movetoworkspacesilent', s, assertOk=True)

def focus_special_workspace(n):
  hyprctl('dispatch', 'togglespecialworkspace', n, assertOk=True)

def hyprpaper_change(to):
  hyprctl('hyprpaper', 'wallpaper', ",~/.config/hypr/workspace-{}.png".format(to), assertOk=True)

def hyprctl_clients():
  return json.loads(hyprctl('clients', '-j'))

def brotab_list():
  ret = []
  for tab in re.findall(r"([^\.]+\.[^\.]+)\.([^\t]+)\t([^\t]+)\t([^\n]+)\n", run(BROTAB, 'list'), re.MULTILINE | re.DOTALL):
    ret.append({'window': tab[0], 'tabno': tab[1], 'title': tab[2], 'url': tab[3]})
  return ret

def brotab_active():
  ret = []
  for tab in re.findall(r"([^\.]+\.[^\.]+)\.([^\t]+)\t([^\t]+)\t([^\t]+)\t([^\t]+)\t([^\n]+)\n", run(BROTAB, 'active'), re.MULTILINE | re.DOTALL):
    ret.append({'window': tab[0], 'tabno': tab[1], 'prefix': tab[2], 'host': tab[3], 'pid': tab[4], 'class': tab[5]})
  return ret

def on_event(line):
  parts = re.match(re.compile(r'^([^\>]+)>>(.*)'), line)

  match parts[1]:
    case 'workspace':
      # This override is here to support the configreloaded. Where, for some reason hyprland triggers a spurious workspace
      # switch after monitor disable
      to = on_event.override_workspace if hasattr(on_event, 'override_workspace') and on_event.override_workspace else parts[2]
      on_event.override_workspace = None

      # NOTE at the time of writing, this feature doesn't work: hyprctl keyword misc:background_color 65535
      #      so we instead use wallpapers.
      hyprpaper_change(to)
    case 'activespecial':
      specialparts = re.match(re.compile(r'^(?:special:([\d]*)|),'), parts[2])
      if specialparts[1]:
        hyprpaper_change(specialparts[1]+"special")
      else:
        active = active_workspace()
        hyprpaper_change(active)
    case 'configreloaded':
      # Set the background, which, triggers after the monitor is destroyed, in 'workspace' above:
      on_event.override_workspace=active_workspace()

      # Disable any monitor that isn't focused:
      for m in filter( lambda m: m['focused'] != 'yes', monitors()):
        hyprctl('keyword', 'monitor', "{}, disable".format(m['port']), assertOk=True)

# main()
parser = argparse.ArgumentParser(description='A smart(er) operation handler intended for use with bind, in the hyprland.conf.')
parser.add_argument("operation",
                    help="One of our supported operations: {}".format(', '.join(OPERATIONS)),
                    type=lambda v: operation_check(v, OPERATIONS))
parser.add_argument('operation_args',
                    help="(Optional) A variable number of arguments, provided to the operation.",
                    nargs=argparse.REMAINDER)
args = parser.parse_args()

match args.operation:
  case 'switch':
    if len(args.operation_args) != 1 or not re.match(r'^[\d]$', args.operation_args[0]):
      raise Exception("Invalid operation_args. One single digit number expected.")
    focus_workspace(int(args.operation_args[0]))
  case 'next':
    active = active_workspace()
    focus_workspace(FIRST_WORKSPACE if (active == LAST_WORKSPACE) else active+1)
  case 'prev':
    active = active_workspace()
    focus_workspace(LAST_WORKSPACE if (active == FIRST_WORKSPACE) else active-1)
  case 'movespecial':
    workspace = re.match(re.compile(r'^(special:|)([\d]+)'), active_window()['workspace']['name'])
    move_to_workspace(workspace[2] if workspace[1] else "special:{}".format(workspace[2]))
  case 'togglebrightness':
    togglebrightness(args.operation_args[0])
  case 'togglespecial':
    focus_special_workspace(active_workspace())
  case 'openurl':
    try:
      # Here, we open the url in firefox. However, we check to see which windows are open, and open to the
      # the window in the current workspace, or the closest workspace to the left. Barring that, we just
      # spawn a firefx in the current workspace. Then we focus to that window
      if len(args.operation_args) != 1:
        raise Exception("Invalid operation_args. One url expected.")

      clients = hyprctl_clients()
      # This raises an error, if firefox is closed
      tabs = brotab_list()
      active_ws = active_workspace()

      # Attach hyprland client info to our brotab windows:
      targets = []
      for window in brotab_active():
        # This gets us the title and url of the active window:
        tab = next((t for t in tabs if t['window'] == window['window'] and t['tabno'] == window['tabno']), None)

        if tab is None:
          next

        # This gets ups the corresponding hyprland client info to this window
        # This is flawed in a few ways. Mostly there's a bug, if two active windows are open to the same page
        # (say Google). Also, swim might change the hyprland settings to display window titles differently,
        # and then this compare could fail. But this is all we can do for now.
        client = None
        for c in clients:
          if c['class'] != window['class']:
            next

          # NOTE: an empty firefox window is titled 'Mozilla Firefox' in hyprland and
          # 'New Tab' in firefox. Not sure about chrome...
          if ((c['title'] == 'Mozilla Firefox' and tab['title'] == 'New Tab') or
              c['title'].startswith(tab['title'])):
            client = c
            break

        if client is None:
          next

        targets.append({
          'window': window['window'],
          'tabno': window['tabno'],
          'address': client['address'],
          'workspace_no': client['workspace']['id'],
          'gt_active': client['workspace']['id'] > active_ws,
          'title': tab['title']
        })

      window = '0'
      address = None
      if len(targets) > 0 :
        # This sorts windows by: current_workspace, closest workspace to the left, closest to right
        targets.sort(key=lambda t: active_ws + t['workspace_no'] if t['gt_active'] else (active_ws - t['workspace_no']))
        window = targets[0]['window']
        address = targets[0]['address']

      # The goal! lol:
      run(BROTAB, 'open', window, input=''.join([args.operation_args[0], '\n']))
      if address:
        focus_window(address)

    except Exception as e:
      run(NOTIFY, "hypr-helper.py openurl: ", str(e))
      if len(args.operation_args) > 0:
        # Close fds spawns the process, and doesn't keep this script from terminating
        run(FIREFOX, "--new-window", args.operation_args[0], close_fds=True)


  case 'monitor':
    BUFFER_SIZE = 1024
    SOCKET_PATH = "/".join([os.environ['XDG_RUNTIME_DIR'],'hypr',os.environ['HYPRLAND_INSTANCE_SIGNATURE'],'.socket2.sock'])

    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as client:
      client.connect(SOCKET_PATH)

      buf = ""
      while True:
        buf += client.recv(BUFFER_SIZE).decode('utf-8', 'ignore')

        lines = buf.splitlines(True)
        buf = "" if lines[-1][-1] == "\n" else lines.pop()

        for line in lines: on_event(line)

      client.close()

  case _:
    raise Exception("Unable to execute operation. This should never happen")
