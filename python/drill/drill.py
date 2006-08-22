#!/usr/bin/python

import os, sys, getopt

#options
NUM_TO_PRINT = 10
EXACT_NUMS = 0
EXCLUDE_DIRS = []
USE_CACHE = 1

class Drill:
    def __init__(self):
        self.cache = {}
    
    def addToCache (self, sizes, dir):
        if not USE_CACHE or self.cache.has_key(dir):
            return 1
        self.cache[dir] = sizes
    
    def findSizes (self, path, doSort=1):
        if not path[-1:] == '/':
            path += '/'
        if USE_CACHE and self.cache.has_key(path):
            return self.cache[path]
        dirlist = os.listdir(path)
        sizes = []
        for subdir in dirlist:
            if subdir not in EXCLUDE_DIRS:
                size = float(os.popen('du -s %s%s' % (path,subdir)).readline().split()[0])
                print "size of dir: %s" % size
                sizes.append((subdir, size))
                hsize = self.makeHuman(size)
        #why in the hell doesn't this work?
        if doSort:
            def dircmp(a, b):
                if a[1] < b[1]: return 1 #switch if b > a
                return -1
            sizes.sort(dircmp)
            return sizes
        return sizes

    def makeHuman (self, size):
        """takes a size in blocks of 1024 and returns a human-readable size val"""
        blocks = round(size / 1024.0)
        if 0 <= blocks < 1:
            return str(int(size)) + 'B'
        elif 1 <= blocks <= 1000:
            return str(int(round(size / 1024))) + 'M'
        elif 1000 <= blocks <= 1000000:
            return str(int(round(size / 1024000))) + 'G'
        else:
            return str(int(round(size / 1024000))) + '>G'
    
    def prettyPrint (self, out, basedir, sizes):
        l = len(sizes)
        i = 1
        for path, size in sizes:
            out.write("%d: %s%s" % (i, basedir, path))
            if len(basedir) + len(path) + len(str(i)) < 75:
                for x in range(70 - int(len(basedir) + len(path) + len(str(i)))):
                    out.write(" ")
            else:
                out.write("      ")
            if EXACT_NUMS:
                out.write("%f\n" % size)
            else:
                out.write("%s\n" % self.makeHuman(size))
            if i % NUM_TO_PRINT == 0 or i == l:
                print "   Shown %d of %d dirs" % (i, len(sizes))
                ret = self.showMenu()
                if 0 < ret <= i:
                    print "found an int"
                    return "%s%s/" % (basedir, sizes[ret-1][0])
                elif ret in ("up", ".."):
                    return basedir[0:basedir.rfind('/',0,len(basedir)-1)+1]
                elif ret in ("l", "list"):
                    #TODO: implement this
                    pass
                elif ret in ("quit", "exit"):
                    return 0
            i+=1
    
    def showMenu (self):
        while 1:
            print 'which folder to drill down? (try "help")'
            res = sys.stdin.readline().strip()
            res = res.lower()
            if res in ("h", "help"):
                menuUsage()
            else:
                break
        return res
    
    def is_int(self, string):
        t = 1
        for x in string:
            if x not in "01234567890":
                t = 0
        return t

##########
# usage strings

def menuUsage ():
    doc = """your options are:
    
    h, help     print this help message
    m, more     print the next %d directories
    up, ..      go to parent directory
    l, list     list the last entries again
    [number]    drill down directory [number]
    exit, quit  exit the program
""" % NUM_TO_PRINT
    print doc

def usage ():
    doc = """Usage: usagemon [options] directory
Interactively summarize disk usage of each subdir of directory

Options:
    -h, --help   print this message
    -n           number of directories to print at once (default 10)
    -e, --exact  don't print out human-readable numbers
    -x dir       exclude dir from the search
    -N --nocache do not cache search results
    
example call: "usagemon -n 12 /usr/local/\""""
    print doc
    
#########
# main loop

def main (dir):
    d = Drill()
    if dir[-1:] != '/':
        dir += '/'
    while dir:
        try:
            sizes = d.findSizes(dir)
        except OSError:
            print "error finding dir: %s" % dir
            usage()
            sys.exit()
        d.addToCache(sizes, dir)
        dir = d.prettyPrint(sys.stdout, dir, sizes)

if __name__ == "__main__":
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hen:x:", ["help", "exact"])
        print args
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    output = None
    for o, a in opts:
        if o in ("-h", "--help"):
            usage()
            sys.exit()
        elif o == "-n":
            NUM_TO_PRINT = int(a)
        elif o in ("-e", "--exact"):
            EXACT_NUMS = 1
        elif o in ("-x"):
            EXCLUDE_DIRS.append(a)
        elif o in ("-N", "--nocache"):
            USE_CACHE = 0
    try:
        main(args[0])
    except IndexError:
        usage()
        sys.exit()
