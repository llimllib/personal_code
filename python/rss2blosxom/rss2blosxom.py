#!/usr/bin/env python
import cElementTree as ce
import re, os

infile = file("ted.xml")
outdir = "."

#\W = not [a-zA-Z0-9], except unicode-aware
mkfilename = re.compile('\W').sub

for e, item in ce.iterparse(infile):
    #print e, item
    if item.tag == 'item':
        title = item.findtext('title')
        dir = item.findtext('category')
        if not os.path.isdir(item.findtext('category')):
            os.makedirs(item.findtext('category'))
        filename = os.path.join(outdir, dir, mkfilename('_', title)) + '.txt'
        filename = filename[:200]
        if not os.path.isfile(filename):
            outfile = file(filename, 'w')
        else:
            raise "file %s already exists" % filename
        print >> outfile, title
        for n in item:
            if n.tag.find('content') != -1:
                print >> outfile, n.text
            elif n.tag == 'description':
                print >> outfile, n.text
