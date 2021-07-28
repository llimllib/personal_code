set -euxo pipefail

# jumanji: https://www.youtube.com/watch?v=dTllG2KdPMQ
# to download only part of a video:
if [ ! -f out.mkv ]; then
    # As long as you put the -ss parameter before the -i parameter, then it
    # will start downloading from the time you specified with -ss. However, if
    # you place -ss after the -i parameter, then ffmpeg will download from the
    # very beginning of the video and start encoding from where -ss specifies
    ffmpeg -ss 00:02:15.00 -i "$(youtube-dl -g "https://www.youtube.com/watch?v=P8GGwgI2vGU" | head -n1)" -t 00:00:10.00 -c copy out.mkv
fi

# https://ffmpeg.org/ffmpeg-filters.html#drawtext-1
ffmpeg -ss 0.0 -t 4 -i "out.mkv" -y -filter_complex "[0:v]
fps=12,
scale=640:-1,
split [a][b];[a] palettegen [p];[b][p] paletteuse,
colorlevels=rimax=0.702:gimax=0.702:bimax=0.702,
drawtext='
    fontfile=/Users/llimllib/Library/Fonts/ProximaNova-Bold.ttf:
    borderw=1:
    bordercolor=white:
    fontcolor=#118762:
    fontsize=40:
    enable=lt(t,4):
    x=(main_w/2-text_w/2):
    y=320:
    text=CVP vs our web server'
" "matrix.gif"
