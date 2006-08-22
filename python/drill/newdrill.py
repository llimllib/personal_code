#!/usr/bin/python
import commands, sys, getopt, os, cmd
try: import readline
except: pass

#options
NUM_TO_PRINT = 10
EXACT_NUMS = 0
EXCLUDE_DIRS = []
USE_CACHE = 1
CACHE = {}

def humanReadable(size):
    """takes a size in blocks of 1024 and returns a human-readable size val"""
    blocks = round(size / 1024.0)
    if size < 1024: return "%dB" % int(size)
    elif blocks <= 1000: return "%dK" % int(blocks)
    elif 1000 <= blocks <= 1000000: return "%dM" % int(round(blocks / 1024))
    else: return "%dG" % int(round(blocks / 1024**2))

def getSizes(dir):
    (status, cmdout) = commands.getstatusoutput("du -ab --max-depth=1 %s" % dir)
    if status: raise OSError
    cmdout = cmdout.split()
    #dir_size contains a bunch of (size, dir) tuples
    dir_size = [(int(cmdout[i]), cmdout[i+1]) for i in range(0, len(cmdout), 2)]
    del dir_size[len(dir_size)-1]
    dir_size.sort()
    dir_size.reverse()
    return dir_size

def addToCache(dir, sizes):
    CACHE[dir] = sizes

def prettyPrint(sizes, start):
    for i in range(min(len(sizes) - start, NUM_TO_PRINT)):
        if not EXACT_NUMS: psize = humanReadable(sizes[start+i][0])
        else: psize = str(sizes[start+i][0])
        print "%d: %6s %-40s" % (i, psize, sizes[start+i][1])

##########
# usage strings

def menuUsage():
    doc = """your options are:
    
    h, help     print this help message
    m, more     print the next %d directories
    up, ..      go to parent directory
    [number]    drill down directory [number]
    cd          change directory
    exit, quit  exit the program
""" % NUM_TO_PRINT
    print doc

def usage():
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

def main(dir):
    cur_ptr = 0
    while dir:
        if USE_CACHE and CACHE.has_key(dir): sizes = CACHE[dir]
        else:
            try: sizes = getSizes(dir)
            except OSError:
                print "error finding dir: %s" % dir
                usage()
                sys.exit()
            if USE_CACHE: addToCache(dir, sizes)
        prettyPrint(sizes, cur_ptr)
        cmd = raw_input('>>> ').split()
        if cmd: cmd[0] = cmd[0].lower()
        #this one has to go first bc "if not cmd"
        if not cmd or cmd[0] == 'm' or cmd[0] == 'M':
            cur_ptr += NUM_TO_PRINT
            prettyPrint(sizes, cur_ptr)
        elif cmd[0] == 'h' or cmd[0] == 'help': menuUsage()
        elif cmd[0] == 'cd':
            dir = cmd[1]
        elif cmd[0] == '..' or cmd[0] == 'up': dir = dir[:dir.find('/')]
        else:
            try: cmd[0] = int(cmd[0])
            except ValueError: dir = 0
            else: dir = sizes[(cmd[0]-1)*2+1][1]

if __name__ == "__main__":
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
            NUM_TO_PRINT = int(a)
        elif o in ("-e", "--exact"):
            EXACT_NUMS = 1
        elif o in ("-x"):
            ECLUDE_DIRS.append(a)
        elif o in ("-N", "--nocache"):
            USE_CACHE = 0
    main(args[0])
