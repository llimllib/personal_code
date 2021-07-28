#!/usr/bin/env bash
set -euxo pipefail

if [ ! -f ayup.mov ]; then
    ffmpeg -i "$(youtube-dl -g 'https://www.youtube.com/watch?v=obZ7_c4BrDc' | head -n1)" -f mp4 -ss 00:15 -t 4 -c copy ayup.mp4
fi
