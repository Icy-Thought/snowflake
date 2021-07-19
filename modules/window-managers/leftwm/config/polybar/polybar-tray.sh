#!/usr/bin/env bash
SCRIPTPATH="$( cd "$(dirname "$0")" || exit ; pwd -P )"

pgrep -a polybar | grep polybar-tray | awk '{print $1}' | xargs kill || \
polybar --config="$SCRIPTPATH"/polybar-tray.config mainbar0
