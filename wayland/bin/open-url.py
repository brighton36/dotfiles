#!/usr/bin/env python3

# This script routes a given url, to the browser window that's 'closest'.
# Proximity is determined by whether any mozilla windows are available on
# the current display. And, lacking that, we take the closest workspace
# and focus to it after dispatch.

import subprocess
import re
import argparse
import json

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
        run(NOTIFY, "open-url.py openurl: ", str(e))
        if len(args.operation_args) > 0:
            # Close fds spawns the process, and doesn't keep this script from terminating
            run(FIREFOX, "--new-window", args.operation_args[0], close_fds=True)


# main()
parser = argparse.ArgumentParser(description='A smarter way to open urls in the (mozilla) webbrowser, based on brotab')
parser.add_argument('operation_args',
                    help="(Optional) A variable number of arguments, provided to the operation.",
                    nargs=argparse.REMAINDER)
args = parser.parse_args()

if len(args.operation_args) > 0:
    open_url(args.operation_args[0])
else:
    raise Exception("Invalid operation_args. One url expected.")
