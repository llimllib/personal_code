import numpy as np
import time

card = open("input8.txt")

def put_cursor(x,y):
    print("\x1b[{};{}H".format(y+1,x+1))

def printscr(screen):
    for row in screen:
        print(''.join(map(str, row)).replace("True", "#").replace('False', ' '))

screen = np.zeros((6,50), dtype=np.bool_)
for instruction in card:
    parts = instruction.split()
    if parts[0] == "rect":
        dim = [int(x) for x in  parts[1].split('x')]
        screen[0:dim[1], 0:dim[0]] = True
    elif parts[1] == "row":
        row = int(parts[2].split('=')[1])
        screen[row,:] = np.roll(screen[row,:], int(parts[4]))
    elif parts[1] == "column":
        col = int(parts[2].split('=')[1])
        screen[:,col] = np.roll(screen[:,col], int(parts[4]))
    else:
        raise Exception("fuck {}".format(parts))
    put_cursor(0,2)
    printscr(screen)
    time.sleep(0.03)
#part 1
print(sum(sum(screen)))

#part 2
#for row in screen:
#    print(''.join(map(str, row.tolist())).replace("True", "#").replace('False', ' '))
#from PIL import Image
#im = Image.fromarray(255 * screen.astype(np.uint8))
#im.show()
