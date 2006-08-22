#!/usr/bin/python
import readline
from path import path
import getopt
import sys
import commands

class drill:
    """TODO: implement the cache"""
    def __init__(self, d, n, exclude, exact=1, use_cache=1):
        self.curdir = path(d)
        self.display_n = n
        self.exact = exact
        self.use_cache = use_cache
        self.exclude = [path(d) for d in exclude]
        self.sizes = [] #tuples of (dir, size)
        self.pp_bookmark = 1 #current prettyprint dir
        self.getsizes(self.curdir) #set up self.sizes

    def cmdloop(self):
        cmd = ''
        while cmd.lower() not in ['exit', 'x', 'quit', 'die', 'groovy']:
            self.prettyprint()
            cmd = raw_input('> ')
            try:
                cmd = int(cmd)
                #check if this is a dir - where?
                self.changedir(self.sizes[cmd-1][0])
            except ValueError: pass
            if cmd.lower() in ['h', 'help']: self.print_help()
            if cmd[:2] == 'cd': self.changedir(cmd.split()[1])

    def changedir(self, _dir):
        """change curdir to path _dir"""
        self.curdir = _dir
        self.getsizes(self.curdir)

    def getsizes(self, _dir):
        """update the sizes array and sort it
        
        TODO: test this for faster ways?"""
        fsizes = [(p, p.size) for p in _dir.files()]
        dsizes = [(d, long(self.getdirsize(d))) for d in _dir.dirs()]
        self.sizes = fsizes + dsizes
        def cmp(a, b):
            if a[1] > b[1]: return -1
            else: return 1
        self.sizes.sort(cmp)

    def prettyprint(self):
        """print out self.sizes"""
        max = self.sizes[0][1]      #get the biggest size
        mark = self.pp_bookmark
        for i in range(mark, mark + self.display_n):
            d, sz = self.sizes[i-1]
            print "%3d: %30s %10s" % (i, d, sz)

    def getdirsize(self, _dir):
        """return the recursive size of _dir
        
        doesn't work on windows, but would be trivial to implement"""
        return commands.getoutput('du -sb %s' % _dir).split()[0]

def usage():
    print """Usage: usagemon [options] directory
Interactively summarize disk usage of each subdir of directory

Options:
    -h, --help   print this message
    -n           number of directories to print at once (default 10)
    -e, --exact  don't print out human-readable numbers
    -x dir       exclude dir from the search
    -N --nocache do not cache search results
    
example call: "usagemon -n 12 /usr/local/\""""

if __name__=="__main__":
    n_print = 10
    exact = 0
    exclude = []
    use_cache = 1
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hen:x:", ["help", "exact"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    output = None
    for o, a in opts:
        if o in ("-h", "--help"):
            usage()
            sys.exit()
        elif o == "-n":
            n_print = int(a)
        elif o in ("-e", "--exact"):
            exact = 1
        elif o in ("-x"):
            exclude.append(a) #currently ignored
        elif o in ("-N", "--nocache"):
            use_cache = 0
    t = drill(args[0], n_print, exclude, exact, use_cache)
    t.cmdloop()
