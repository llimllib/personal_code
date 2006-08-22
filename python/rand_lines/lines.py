#!/usr/bin/python
import Image, ImageDraw, tempfile, random

def _show(im):
    f = tempfile.mkstemp('.png')[1]
    im.save(file(f, 'w'))
    x = Image.open(f)
    x.show()

if __name__=="__main__":
    w = 800
    h = 600
    i = 1000
    mux = w/2
    muy = h/2
    sigma = 60
    rand = random.Random()
    im = Image.new('1', (w,h), 1)
    d = ImageDraw.Draw(im)
    lines = []
    for i in range(i):
        tooclose = 0
        #x = round(rand.gauss(mux, sigma))
        #y = round(rand.gauss(muy, sigma))
        x = rand.randint(0,w-1)
        y = rand.randint(0,h-1)
        dir = rand.randint(0,1)
        len = int(rand.gauss(100, 20))
        if dir:
            for j in range((x-3)%w, (x+3)%w):
                if im.getpixel((j, y)) == 0: tooclose = 1
            for j in range((x+len-3)%w, (x+len+3)%w):
                if im.getpixel((j, y)) == 0: tooclose = 1
        else:
            for j in range((y-3)%h, (y+3)%h):
                if im.getpixel((x, j)) == 0: tooclose = 1
            for j in range((y+len-3)%h, (y+len-3)%h):
                if im.getpixel((x, j)) == 0: tooclose = 1
        if not tooclose:
            if dir: 
                d.line([(x,y), (x+len, y)])
            else:
                d.line([(x,y), (x, y+len)])
    _show(im)
    im.save(file('g.png','w'))
