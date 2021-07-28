#!/usr/bin/env bash
if [ ! -f out.mp4 ]; then
    youtube-dl https://www.youtube.com/watch?v=SIxvwa1t-yA -o out
fi
# https://ffmpeg.org/ffmpeg-filters.html#drawtext-1
ffmpeg -ss 89.4 -t 10 -i "out.mp4" -filter_complex "[0:v]
fps=12,
scale=640:-1,
split [a][b];[a] palettegen [p];[b][p] paletteuse,
colorlevels=rimax=0.702:gimax=0.702:bimax=0.702
" "bees.gif"
