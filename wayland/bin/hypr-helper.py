#!/usr/bin/env python3

# Mostly this script contains all the hyprland functionality that we need, that
# isn't natively supported by hyprland. Which, makes this a bit of a potporri
# script. I think I prefer this uberscript, over having a dozen scripts in my
# bin.

import os
import subprocess
import re
import argparse
import json

OPERATIONS = ['openurl', 'screenshot']
FIRST_WORKSPACE = 1
LAST_WORKSPACE = 9

HYPRCTL = "/usr/bin/hyprctl"
BROTAB = "brotab"
NOTIFY = "/usr/bin/notify-send"
FIREFOX = "/usr/bin/librewolf"

BROWSER_WINDOW_TITLE = 'LibreWolf'


def run(*args, **kwargs):
    result = subprocess.run(
        list(map(lambda a: str(a), args)),
        input=kwargs['input'] if 'input' in kwargs else None,
        close_fds=kwargs.get('close_fds', None),
        capture_output=kwargs.get('capture_output', True),
        stdout=kwargs.get('stdout', None),
        shell=kwargs.get('shell', False)
    )
    if result.returncode != 0:
        raise Exception("Error running {} ({}): {}".format(args[0],
                                                           result.returncode,
                                                           result.stdout))

    if kwargs.get('capture_output', True):
        return result.stdout.decode()
    else:
        result.stdout


def hyprctl(*args, **kwargs):
    stdout = run(HYPRCTL, *args)
    # TODO: It might be smarter to just support a assertOutput=/^ok$/ param...
    if (kwargs.get("assertOk") and not re.match(r'^ok$', stdout)):
        raise Exception("Error in hyprctl: {}".format(stdout))
    return stdout


def active_workspace():
    return int(json.loads(hyprctl('activeworkspace', '-j'))['id'])


def operation_check(arg_value, supported_operations):
    if not arg_value in supported_operations:
        raise argparse.ArgumentTypeError("Unrecognized operation \"{}\".".format(arg_value))
    return arg_value


def focus_window(address):
    # Since 0.55 (lua), `hyprctl dispatch` evaluates its argument as a lua
    # dispatcher expression; old `dispatch focuswindow address:0x...` is dead.
    hyprctl('dispatch', "hl.dsp.focus({{ window = 'address:{}' }})".format(address), assertOk=True)


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


def screenshot(domain):
    from datetime import datetime

    DIR_OUTPUT = "~/Pictures/Screenshots"
    FILENAME = "Screenshot %Y-%m-%d %I%M%S.png"

    output_path = os.path.expanduser(
        '/'.join([DIR_OUTPUT, datetime.now().strftime(FILENAME)]))

    # Now Drop shadow:
    run("/usr/bin/magick", "convert", "png:-", "(", "-clone", "0", "-background",
        "black", "-shadow", "80x3+5+5", ")",
        "+swap", "-background", "none", "-layers", "merge", "+repage", output_path,
        input=run("/usr/bin/magick", "convert", "png:-",
                  "(", "+clone", "-alpha", "extract",
                  "-draw", 'fill black polygon 0,0 0,15 15,0 fill white circle 15,15 15,0',
                  "(", "+clone", "-flip", ")", "-compose", "Multiply", "-composite",
                  "(", "+clone", "-flop", ")", "-compose", "Multiply", "-composite",
                  ")", "-alpha", "off", "-compose", "CopyOpacity", "-composite", "png:-",
                  stdout=subprocess.PIPE,
                  capture_output=False,
                  input=run("/usr/bin/hyprshot", "-m", "region", "-r", "-s", "-z",
                            stdout=subprocess.PIPE,
                            capture_output=False)))

    # And view it:
    run('/usr/bin/feh', output_path)


def open_url(url):
    try:
        # Here, we open the url in firefox. However, we check to see which windows are open, and open to the
        # the window in the current workspace, or the closest workspace to the left. Barring that, we just
        # spawn a firefox in the current workspace. Then we focus to that window
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
                if ((c['title'] == BROWSER_WINDOW_TITLE and tab['title'] == 'New Tab') or
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
        if len(targets) > 0:
            # This sorts windows by: current_workspace, closest workspace to the left, closest to right
            targets.sort(key=lambda t: active_ws + t['workspace_no'] if t['gt_active'] else (active_ws - t['workspace_no']))
            window = targets[0]['window']
            address = targets[0]['address']

        # The goal! lol:
        run(BROTAB, 'open', window, input=''.join([url, '\n']).encode('utf-8'))
        if address:
            focus_window(address)

    except Exception as e:
        run(NOTIFY, "hypr-helper.py openurl: ", str(e))
        if len(args.operation_args) > 0:
            # Close fds spawns the process, and doesn't keep this script from terminating
            run(FIREFOX, "--new-window", args.operation_args[0], close_fds=True)


# main()
parser = argparse.ArgumentParser(description='A smart(er) operation handler intended for use with bind, in the hyprland.lua.')
parser.add_argument("operation",
                    help="One of our supported operations: {}".format(', '.join(OPERATIONS)),
                    type=lambda v: operation_check(v, OPERATIONS))
parser.add_argument('operation_args',
                    help="(Optional) A variable number of arguments, provided to the operation.",
                    nargs=argparse.REMAINDER)
args = parser.parse_args()

match args.operation:
    case 'openurl':
        if len(args.operation_args) != 1:
            raise Exception("Invalid operation_args. One url expected.")
        open_url(args.operation_args[0])
    case 'screenshot':
        if len(args.operation_args) != 1:
            raise Exception("Invalid operation_args. A screenshot domain was expected.")
        screenshot(args.operation_args[0])
    case _:
        raise Exception("Unable to execute operation. This should never happen")
