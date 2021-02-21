#!/bin/bash

sudo cdrdao write --device 0,0,0 --driver generic-mmc --speed 8 $1
