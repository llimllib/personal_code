# https://ffmpeg.org/ffmpeg-filters.html#drawtext-1
ffmpeg -ss 163 -t 10 -i "$1" -filter_complex "[0:v]
fps=12,
scale=640:-1,
split [a][b];[a] palettegen [p];[b][p] paletteuse,
colorlevels=rimax=0.702:gimax=0.702:bimax=0.702,
drawtext='
    fontfile=/Users/llimllib/Library/Fonts/ProximaNova-Bold.ttf:
    borderw=1:
    bordercolor=white:
    fontcolor=#118762:
    fontsize=50:
    x=(main_w/2-text_w/2):
    y=250:
    text=just chugging along'" "train.gif"
