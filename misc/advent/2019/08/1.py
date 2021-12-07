from collections import Counter
import numpy as np
from sys import maxsize

layersize = 6 * 25
img = list(open("in").read().strip())
layers = []
while img:
    layer, img = img[:layersize], img[layersize:]
    layers.append(np.array(layer, dtype="c").reshape((6, 25)))

zeros = maxsize
product = 0
for layer in layers:
    counts = dict(zip(*np.unique(layer, return_counts=True)))
    if counts[b"0"] < zeros:
        zeros = counts[b"0"]
        product = counts[b"1"] * counts[b"2"]


print(product)

BLACK = b"0"
WHITE = b"1"
TRANSPARENT = b"2"


def renderpixel(row, col, layers):
    for layer in layers:
        if layer[row, col] != TRANSPARENT:
            return "🤍" if layer[row, col] == WHITE else "🖤"
    return " "


finalimg = np.zeros((6, 25), dtype=np.unicode_)
for row in range(6):
    for col in range(25):
        finalimg[row, col] = renderpixel(row, col, layers)

for row in finalimg:
    print("".join(row))
