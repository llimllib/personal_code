set -euxo pipefail

# jumanji: https://www.youtube.com/watch?v=dTllG2KdPMQ
# to download only part of a video:
if [ ! -f out.mkv ]; then
    # As long as you put the -ss parameter before the -i parameter, then it
    # will start downloading from the time you specified with -ss. However, if
    # you place -ss after the -i parameter, then ffmpeg will download from the
    # very beginning of the video and start encoding from where -ss specifies
    ffmpeg -ss 00:00:34.00 -i "$(youtube-dl -g "$1" | head -n1)" -t 00:00:02.00 -c copy out.mkv
fi

# https://ffmpeg.org/ffmpeg-filters.html#drawtext-1
ffmpeg -ss 0.0 -t 2 -i "recut.mov" -y -filter_complex "[0:v]
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
    enable=gt(t,.9):
    x=90:
    y=390:
    text=Add',
drawtext='
    fontfile=/Users/llimllib/Library/Fonts/ProximaNova-Bold.ttf:
    borderw=3:
    bordercolor=white:
    fontcolor=#118762:
    fontsize=50:
    enable=gt(t,1.1):
    x=190:
    y=390:
    text=Some',
drawtext='
    fontfile=/Users/llimllib/Library/Fonts/ProximaNova-Bold.ttf:
    borderw=3:
    bordercolor=white:
    fontcolor=#118762:
    fontsize=50:
    enable=gt(t,1.4):
    x=330:
    y=390:
    text=Fish',
drawtext='
    fontfile=/Users/llimllib/Library/Fonts/ProximaNova-Bold.ttf:
    borderw=3:
    bordercolor=white:
    fontcolor=#118762:
    fontsize=50:
    enable=gt(t,1.6):
    x=440:
    y=390:
    text=Sauce'
" "fishsauce.gif"
