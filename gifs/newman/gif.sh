#!/usr/bin/env bash
set -euxo pipefail

# usage: gif.sh <youtube url> <duration> [<start time>]

URL=$1
DURATION=$2
START=${3:-0}

if [ ! -f /tmp/out.mp4 ]; then
    ffmpeg -ss $START \
           -i "$(youtube-dl -g "$URL" | head -n1)" \
           -f mp4 \
           -t $DURATION \
           -c copy \
           /tmp/out.mp4
fi

ffmpeg -i "/tmp/out.mp4" -filter_complex "[0:v]
fps=12,
scale=640:-1,
split [a][b];[a] palettegen [p];[b][p] paletteuse,
colorlevels=rimax=0.702:gimax=0.702:bimax=0.702,
drawtext='
    fontfile=/Users/llimllib/Library/Fonts/ProximaNova-Bold.ttf:
    borderw=3:
    bordercolor=white:
    fontcolor=#118762:
    fontsize=50:
    enable=gt(t,4.5):
    x=(main_w/2-text_w/2):
    y=420:
    text=GDIT'
" "out.gif"
