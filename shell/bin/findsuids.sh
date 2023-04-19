#!/bin/bash

find / -perm +6000 -type f -exec ls -ld {} \;
