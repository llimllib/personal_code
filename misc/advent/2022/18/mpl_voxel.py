import matplotlib.pyplot as plt
import numpy as np


def parse(text):
    space = np.zeros((22, 22, 22), dtype=bool)
    for a, b, c in (l.split(",") for l in text.strip().split("\n")):
        space[int(a), int(b), int(c)] = True
    return space


# based on
# https://matplotlib.org/stable/gallery/mplot3d/voxels_numpy_logo.html#sphx-glr-gallery-mplot3d-voxels-numpy-logo-py
pts = parse(open("input.txt").read())

ax = plt.figure().add_subplot(projection="3d")
ax.voxels(pts, facecolors="blue", edgecolors="black")
ax.set_aspect("equal")

plt.show()
