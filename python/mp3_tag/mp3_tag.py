#!/usr/bin/python
import os
from ID3 import *
from path import path

def make_tag(id3):
    id3.delete()
    f = id3.filename
    f = f.split('-')
    print f[0], f[1][1:-4]
    id3.artist = f[0][f[0].rfind('/')+1:]
    id3.title = f[1][1:-4]
    id3.write()

d ='/music/music'
for i in os.listdir(d):
    if i != 'rip':
        p = path(d + '/' + i)
        if p.isdir():
            for f in p.files('*.mp3'):
                make_tag(ID3(str(f)))
