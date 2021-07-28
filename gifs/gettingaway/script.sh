#!/usr/bin/env bash
set -euxo pipefail

# to download only part of a video:
if [ ! -f out.mkv ]; then
    # As long as you put the -ss parameter before the -i parameter, then it
    # will start downloading from the time you specified with -ss. However, if
    # you place -ss after the -i parameter, then ffmpeg will download from the
    # very beginning of the video and start encoding from where -ss specifies
    ffmpeg -ss 00:01:19.00 -i "$(youtube-dl -g "https://www.youtube.com/watch?v=eFXswv-xFIE" | head -n1)" -t 00:00:07.00 -c copy out.mkv
fi

# the video is flipped so that Youtube doesn't detect the copyrighted material;
# let's flip it back
if [ ! -f flipped.mkv ]; then
    ffmpeg -i out.mkv -vf hflip -c:a copy flipped.mkv
fi

# https://ffmpeg.org/ffmpeg-filters.html#drawtext-1
ffmpeg -ss 2.0 -t 5 -i "flipped.mkv" -y \
-filter_complex "[0:v]
fps=12,
scale=640:-1,
split [a][b];[a] palettegen [p];[b][p] paletteuse,
colorlevels=rimax=0.702:gimax=0.702:bimax=0.702,
drawtext='
    fontfile=/Users/llimllib/Library/Fonts/ProximaNova-Bold.ttf:
    borderw=2:
    bordercolor=white:
    fontcolor=#111111:
    fontsize=40:
    enable=lt(t,3):
    x=(main_w/2-text_w/2):
    y=280:
    text=They can’t keep',
drawtext='
    fontfile=/Users/llimllib/Library/Fonts/ProximaNova-Bold.ttf:
    borderw=2:
    bordercolor=white:
    fontcolor=#111111:
    fontsize=40:
    enable=lt(t,3):
    x=(main_w/2-text_w/2):
    y=320:
    text=getting away with this'
" "gettingaway.gif"
