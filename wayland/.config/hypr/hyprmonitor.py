#!/usr/bin/env python3

import os, socket, re
from hyprlib import hyprctl

BUFFER_SIZE = 1024
SOCKET_PATH = "/".join([os.environ['XDG_RUNTIME_DIR'],'hypr',os.environ['HYPRLAND_INSTANCE_SIGNATURE'],'.socket2.sock'])

def handle(line):
  parts = re.match(re.compile(r'^([^\>]+)>>(.*)'), line)

  match parts[1]:
    case 'workspace':
      # NOTE at the time of writing, this feature doesn't work: hyprctl keyword misc:background_color 65535
      hyprctl('hyprpaper', 'wallpaper', ",~/.config/hypr/workspace-{}.png".format(parts[2]), assertOk=True)

with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as client:
  client.connect(SOCKET_PATH)

  buf = ""
  while True:
    buf += client.recv(BUFFER_SIZE).decode('utf-8', 'ignore')

    lines = buf.splitlines(True)
    buf = "" if lines[-1][-1] == "\n" else lines.pop()

    for line in lines: handle(line)

  client.close()
