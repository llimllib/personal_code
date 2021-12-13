import re
import sys

# pip install pillow pytesseract
from PIL import Image, ImageDraw
import pytesseract


def parse(it):
    pts = set()
    folds = []
    for line in it:
        if not line.strip():
            break
        pts.add(tuple(map(int, line.split(","))))

    for line in it:
        ax, n = re.findall(r"(\w)=(\d+)", line)[0]
        folds.append((ax, int(n)))

    return pts, folds


def fold(pts, fold):
    f = fold[1]
    newpts = set()
    if fold[0] == "x":
        for pt in pts:
            if pt[0] > f:
                newpts.add((f - (pt[0] - f), pt[1]))
            else:
                newpts.add(pt)
    if fold[0] == "y":
        for pt in pts:
            if pt[1] > f:
                newpts.add((pt[0], f - (pt[1] - f)))
            else:
                newpts.add(pt)
    return newpts


pts, folds = parse(open("small.txt"))
print(len(fold(pts, folds[0])))
pts, folds = parse(open("input.txt"))
print(len(fold(pts, folds[0])))

for f in folds:
    pts = fold(pts, f)

rowmax = max([p[1] for p in pts])
colmax = max([p[0] for p in pts])

for row in range(rowmax + 1):
    for col in range(colmax + 1):
        if (col, row) in pts:
            sys.stdout.write("█")
        else:
            sys.stdout.write("░")
    sys.stdout.write("\n")

sz = 10
with Image.new("RGBA", (800, 500), (255, 255, 255)) as im:
    draw = ImageDraw.Draw(im)
    for row in range(rowmax + 1):
        for col in range(colmax + 1):
            if (col, row) in pts:
                draw.rectangle(
                    (
                        200 + col * sz,
                        200 + row * sz,
                        200 + col * sz + sz,
                        200 + row * sz + sz,
                    ),
                    fill=(0, 0, 0, 255),
                )

    # Sadly this gets it wrong - it interprets "K" as "OR" and "Z" as "2"
    print("OCR unfortunately gets it a bit wrong:")
    print(pytesseract.image_to_string(im))
    im.save("out.png")
