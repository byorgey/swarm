#!/bin/bash -xe

cd $(git rev-parse --show-toplevel)

SCENARIO_PATH=${1?"Usage: $0 SCENARIO_PATH"}

IMG_WIDTH=200
IMG_HEIGHT=150

IMG_OUTPUT_PATH=output.png
RENDER_IMG_COMMAND="cabal run swarm-scene -- $SCENARIO_PATH --fail-blank --dest $IMG_OUTPUT_PATH --png --width $IMG_WIDTH --height $IMG_HEIGHT"

cabal build -j -O0 swarm:swarm-scene

$RENDER_IMG_COMMAND
code --reuse-window $SCENARIO_PATH && code --reuse-window $IMG_OUTPUT_PATH

while inotifywait -e close_write $SCENARIO_PATH; do $RENDER_IMG_COMMAND; done
