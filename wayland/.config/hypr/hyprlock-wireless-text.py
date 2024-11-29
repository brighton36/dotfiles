#!/usr/bin/env python3

import re, argparse, subprocess
from hyprlib import *

OPERATIONS=['wifi', 'bluetooth']
NMCLI="/usr/bin/nmcli"
IWCONFIG="/usr/bin/iwconfig"
BLUETOOTHCTL="/usr/bin/bluetoothctl"

def exec(cmd):
  result = subprocess.run(cmd, capture_output=True, text=True)
  if result.returncode != 0:
    raise Exception("Error encountered when running {} ({}): \"{}\"".format( cmd[0], result.returncode, repr(result.stdout)))
  return result.stdout

# We ended up not needing this. But, That may change in the future, so I'll keep this for now
def nmcli_device_show():
  return map( lambda dev: dict(re.findall(r'^([^:]+):[ ]*(.+)$', dev, flags=re.MULTILINE)),
              exec([NMCLI, "device", "show"]).split("\n\n"))

# NOTE: I'm certain this will break under weird inputs. The iwconfig output isn't ideal...
def iwconfig():
  return list(map( lambda device: {**{'device': device[0][0], 'standard': device[1][0]}, **dict(device[2:])},
           map( lambda dev: re.findall(r'([^\:=]+?)(?:[\:=][\"]?(.+?)[\"]?|)(?:[\n ]{2,}|$)', dev),
                exec([IWCONFIG]).strip().split("\n\n"))
           )
         )

def bt_connections():
  return dict(re.findall(r'^Device ([^ ]+) (.+)$',exec([BLUETOOTHCTL, 'devices', 'Connected']), re.MULTILINE ))

parser = argparse.ArgumentParser(description='A convenient tool to output prettier wifi and bluetooth stats to our hyprlock.')
parser.add_argument("operation",
                    help="One of our supported operations: {}".format(', '.join(OPERATIONS)),
                    type=lambda v: operation_check(v, OPERATIONS))
args = parser.parse_args()

match args.operation:
  case 'wifi':
    wifis = iwconfig()
    if len(wifis) > 0:
      print("Wifi: {} ({}) @ {}".format(wifis[0]['ESSID'], wifis[0]['Link Quality'], wifis[0]['Bit Rate']))
  case 'bluetooth':
    connected_devices = bt_connections()
    print("Bluetooth: {}".format("(None)" if bool(connected_devices) == False else ", ".join(connected_devices.values())))
  case _:
    raise Exception("Unable to execute operation. This should never happen")
