
# to download only part of a video:
# ffmpeg $(youtube-dl -g 'https://www.youtube.com/watch?v=X6I_dKUYyI4' | sed "s/.*/-ss 00:53 -i &/") -t 00:07 -c copy out.mkv

# https://ffmpeg.org/ffmpeg-filters.html#drawtext-1
ffmpeg -ss 5.6 -t 6.2 -i "out.mkv" -filter_complex "[0:v]
fps=12,
scale=640:-1,
split [a][b];[a] palettegen [p];[b][p] paletteuse,
colorlevels=rimax=0.702:gimax=0.702:bimax=0.702,
drawtext='
    fontfile=/Users/llimllib/Library/Fonts/ProximaNova-Bold.ttf:
    borderw=.5:
    bordercolor=white:
    fontcolor=#118762:
    fontsize=30:
    enable=lt(t,4):
    x=(main_w/2-text_w/2):
    y=320:
    text=they say all good things must come to an end',
drawtext='
    fontfile=/Users/llimllib/Library/Fonts/ProximaNova-Bold.ttf:
    borderw=.5:
    bordercolor=white:
    fontcolor=#118762:
    fontsize=35:
    enable=gt(t,4):
    x=(main_w/2-text_w/2):
    y=320:
    text=what's that got to do with this show?'
'" "waldorf.gif"
