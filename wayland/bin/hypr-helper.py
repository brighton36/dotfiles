#!/usr/bin/env python3

import os, subprocess, socket, re, argparse, time

OPERATIONS=['switch', 'next', 'prev', 'movespecial', 'togglespecial', 'monitor']
FIRST_WORKSPACE=1
LAST_WORKSPACE=9

HYPRCTL="/usr/bin/hyprctl"

def hyprctl(*args, **kwargs):
  cmd = [HYPRCTL]+list(map(lambda a: str(a), args))
  result = subprocess.run(cmd, capture_output=True, text=True)
  if (kwargs.get("assertOk") and not re.match(r'^ok$', result.stdout)) or result.returncode != 0:
    raise Exception("Error encountered when running \"{}\" ({}): \"{}\"".format(" ".join(cmd),
                                                                                result.returncode,
                                                                                repr(result.stdout)))
  return result.stdout

def active_workspace():
  stdout = hyprctl('activeworkspace')
  return int(re.search(r'^workspace ID ([^ ]+)',stdout).group(1))

def operation_check(arg_value, supported_operations):
  if not arg_value in supported_operations:
    raise argparse.ArgumentTypeError("Unrecognized operation \"{}\".".format(arg_value))
  return arg_value

def active_window():
  parts = re.match(re.compile(r"^Window ([^ ]+)[^\n]+\n(.+)", re.MULTILINE | re.DOTALL), hyprctl('activewindow'))
  return {**{'windowid': parts[1]},
          **dict(re.findall(r'^[ \t]+([^:]+):[ ]*([^\n]+)$', parts[2], flags=re.MULTILINE))}

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

def move_to_workspace(s):
  hyprctl('dispatch', 'movetoworkspacesilent', s, assertOk=True)

def focus_special_workspace(n):
  hyprctl('dispatch', 'togglespecialworkspace', n, assertOk=True)

def hyprpaper_change(to):
  hyprctl('hyprpaper', 'wallpaper', ",~/.config/hypr/workspace-{}.png".format(to), assertOk=True)

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
    workspace = re.match(re.compile(r'[^\(]+\((special:|)([^\)]+)\)'), active_window()['workspace'])
    move_to_workspace(workspace[2] if workspace[1] == 'special:' else "special:{}".format(workspace[2]))
  case 'togglespecial':
    focus_special_workspace(active_workspace())
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
