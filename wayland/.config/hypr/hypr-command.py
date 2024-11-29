#!/usr/bin/env python3

import re, argparse
from functools import reduce
from hyprlib import *

OPERATIONS=['switch', 'next', 'prev', 'movespecial', 'togglespecial']
FIRST_WORKSPACE=1
LAST_WORKSPACE=9

def active_window():
  parts = re.match(re.compile(r"^Window ([^ ]+)[^\n]+\n(.+)", re.MULTILINE | re.DOTALL), hyprctl('activewindow'))
  return {**{'windowid': parts[1]},
          **dict(re.findall(r'^[ \t]+([^:]+):[ ]*(.+)$', parts[2], flags=re.MULTILINE))}

def focus_workspace(n):
  hyprctl('dispatch', 'workspace', n, assertOk=True)

def move_to_workspace(s):
  hyprctl('dispatch', 'movetoworkspacesilent', s, assertOk=True)

def focus_special_workspace(n):
  hyprctl('dispatch', 'togglespecialworkspace', n, assertOk=True)

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
  case _:
    raise Exception("Unable to execute operation. This should never happen")