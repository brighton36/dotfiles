#!/usr/bin/env python3

import re
import argparse
import subprocess
from functools import reduce

HYPRCTL="/usr/bin/hyprctl"
OPERATIONS=['switch', 'next', 'prev', 'movespecial', 'togglespecial']

def hyprctl(*args, **kwargs):
  cmd = [HYPRCTL]+list(map(lambda a: str(a), args))
  result = subprocess.run(cmd, capture_output=True, text=True)
  if (kwargs.get("assertOk") and not re.match(r'^ok$', result.stdout)) or result.returncode != 0:
    raise Exception("Error encountered when running \"{}\" ({}): \"{}\"".format(" ".join(cmd),
                                                                                result.returncode,
                                                                                repr(result.stdout)))
  return result.stdout

def operation_check(arg_value):
  if not arg_value in OPERATIONS:
    raise argparse.ArgumentTypeError("Unrecognized operation \"{}\".".format(arg_value))
  return arg_value

def active_workspace():
  stdout = hyprctl('activeworkspace')
  return int(re.search(r'^workspace ID ([^ ]+)',stdout).group(1))

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
                    type=lambda v: operation_check(v))
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
    focus_workspace(1 if (active == 9) else active+1)
  case 'prev':
    active = active_workspace()
    focus_workspace(9 if (active == 1) else active-1)
  case 'movespecial':
    workspace = re.match(re.compile(r'[^\(]+\((special:|)([^\)]+)\)'), active_window()['workspace'])
    if workspace[1] == 'special:':
      move_to_workspace(workspace[2])
    else:
      move_to_workspace("special:{}".format(workspace[2]))
  case 'togglespecial':
    focus_special_workspace(active_workspace())
  case _:
    raise Exception("Unable to execute operation. This should never happen")
