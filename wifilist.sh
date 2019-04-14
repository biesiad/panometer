#!/usr/bin/env bash

iwlist wlan0 s | grep ESSID | grep -v '' | sed -E 's/^.+ESSID:"(.*)"/\3/g' | sort | uniq
