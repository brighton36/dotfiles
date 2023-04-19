#!/bin/bash

du -ks "$@" | sort -nr | head --lines=100 | cut -f2 | bash -c 'while read file;do du -mhs "$file";done;'
