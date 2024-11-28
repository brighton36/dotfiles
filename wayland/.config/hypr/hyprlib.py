#!/usr/bin/env python3

import subprocess, re

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
