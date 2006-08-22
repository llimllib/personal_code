#!/usr/bin/python
import sys, getopt, readline, commands, pp
from path import path, os
from PIL import Image
from print_html import *

class photo:
    def __init__(pic, comment = ''):
        self.pic = pic
        self.comment = pic.name

def resize_image(im, size):
    dim = im.size
    hrat = size[0] / float(dim[0])
    vrat = size[1] / float(dim[1])
    if hrat > vrat: scale = hrat
    elif hrat < vrat: scale = vrat
    else: scale = hrat
    return im.resize((int(dim[0] * scale), int(dim[1] * scale)))

def make_new_album(from_dir):
    #currently, no error handling
    frd = path(from_dir)
    d = raw_input("What is the album's name? ")
    try:
        os.mkdir(path.joinpath('photos', d))
        os.chdir(path.joinpath('photos', d))
        cwd = path.getcwd()
        included = []
        for f in frd.files():
            commands.getoutput('eog %s' % f)
            ans = raw_input('include this file in the album? (y/[n]) ')
            if ans.lower() == 'y' or ans.lower() == 'yes':
                cmt = raw_input('picture caption: (blank for none)')
                path.copy(f, cwd)
                n = f.name.replace(f.name[-4:], '.thumb' + f.name[-4:])
                thumb = path.joinpath(cwd, n)
                path.copy(f, thumb)
                cur_pic = path.joinpath(cwd, f.name)
                included.append(Photo(cur_pic, thumb, cmt)) 
                im = resize_image(Image.open(thumb), (200, 150))
                im.save(file(thumb, 'w'))
        return included

def insert_pics(pics):
    t = file('photos/template.htpy')


if __name__ == "__main__":
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'd:')
    except getopt.GetoptError:
        print 'type new_album <dir> to make a new album from photos in <dir>'
    for o, a in opts:
        if o == "-d": from_dir = a
    try: pics = make_new_album(from_dir)
    except NameError:
        print 'type new_album <dir> to make a new album from photos in <dir>'
    insert_pics(pics)
