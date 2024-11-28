#!/usr/bin/env python3

import os, socket, re
from hyprlib import *

BUFFER_SIZE = 1024
SOCKET_PATH = "/".join([os.environ['XDG_RUNTIME_DIR'],'hypr',os.environ['HYPRLAND_INSTANCE_SIGNATURE'],'.socket2.sock'])

def hyprpaper_change(to):
  hyprctl('hyprpaper', 'wallpaper', ",~/.config/hypr/workspace-{}.png".format(to), assertOk=True)

def handle(line):
  parts = re.match(re.compile(r'^([^\>]+)>>(.*)'), line)

  match parts[1]:
    case 'workspace':
      # NOTE at the time of writing, this feature doesn't work: hyprctl keyword misc:background_color 65535
      print(parts[2])
      hyprpaper_change(parts[2])
    case 'activespecial':
      specialparts = re.match(re.compile(r'^(?:special:([\d]*)|),'), parts[2])
      if specialparts[1]:
        hyprpaper_change(specialparts[1]+"special")
      else:
        active = active_workspace()
        hyprpaper_change(active)


with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as client:
  client.connect(SOCKET_PATH)

  buf = ""
  while True:
    buf += client.recv(BUFFER_SIZE).decode('utf-8', 'ignore')

    lines = buf.splitlines(True)
    buf = "" if lines[-1][-1] == "\n" else lines.pop()

    for line in lines: handle(line)

  client.close()
