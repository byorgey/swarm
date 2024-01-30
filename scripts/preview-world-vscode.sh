#!/bin/bash -xe

# Opens a live-reloading preview of the world
#
# Prerequisites:
# --------------
# Install inotify-wait:
#
#    sudo apt install inotify-tools
#
# Usage:
# --------------
# Once the VS Code editor tabs are opened, one can press
# CTRL+\ (backslash) with the image selected to split the
# editor pane horizontally.
# One may then navigate to the left-pane's copy of the image
# preview with CTRL+PageUp, and then
# CTRL+w will close the redundant image preview.

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/..


SCENARIO_PATH=${1?"Usage: $0 SCENARIO_PATH"}

IMG_WIDTH=200
IMG_HEIGHT=150

IMG_OUTPUT_PATH=output.png
RENDER_IMG_COMMAND="stack exec swarm-scene -- $SCENARIO_PATH --fail-blank --dest $IMG_OUTPUT_PATH --png --width $IMG_WIDTH --height $IMG_HEIGHT"

stack build --fast swarm:swarm-scene

$RENDER_IMG_COMMAND
code --reuse-window $SCENARIO_PATH && code --reuse-window $IMG_OUTPUT_PATH 

while inotifywait -e close_write $SCENARIO_PATH; do $RENDER_IMG_COMMAND; done
