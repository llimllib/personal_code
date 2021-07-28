#!/usr/bin/env bash
# to download only part of a video:
# ffmpeg $(youtube-dl -g 'https://www.youtube.com/watch?v=X6I_dKUYyI4' | sed "s/.*/-ss 00:53 -i &/") -t 00:07 -c copy out.mkv
if [ ! -f out.mp4 ]; then
    youtube-dl https://www.youtube.com/watch?v=JSkcCIeSTAU -o out
fi

# https://ffmpeg.org/ffmpeg-filters.html#drawtext-1
ffmpeg -ss 78.4 -t 1.5 -i "out.mp4" -filter_complex "[0:v]
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
    enable=lt(t,4):
    x=(main_w/2-text_w/2):
    y=420:
    text=I love line'
" "love.gif"
