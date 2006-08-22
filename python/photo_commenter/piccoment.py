#!/usr/bin/env python
import os, sys, getopt, Itpl

pp = Itpl.itpl

IMAGES = ['.jpg', '.jpeg']
DISPLAY = 'eog'

def comment_dir(dir):
    for f in os.listdir(dir):
        tempf = os.path.join(dir, pp('.$f'))
        f = os.path.join(dir, f)
        if os.path.isfile(f) and os.path.splitext(f)[1] in IMAGES:
            os.system(pp('$DISPLAY $f &'))
            com = raw_input('comment: ')
            os.system(pp('wrjpgcom -comment "$com" $f > $tempf'))
            os.system(pp('mv $tempf $f'))
            os.system(pp('killall $DISPLAY'))
            print

if __name__ == "__main__":
    comment_dir('testpics')
