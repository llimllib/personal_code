#!/usr/bin/env bash
ffmpeg -i cool.mp4 -filter_complex "[0:v] fps=12,scale=640:-1,split [a][b];[a] palettegen [p];[b][p] paletteuse,colorlevels=rimax=0.702:gimax=0.702:bimax=0.702" cool.gif
