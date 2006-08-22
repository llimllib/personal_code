#!/usr/bin/python
#############################################
# Not Copyrighted! This file is Public Domain
# written by Bill Mill, 2003
#############################################
import sys, urllib, getopt, re

USER = None
VERBOSE = 0
REGEX = re.compile(r'^\s+<TD><.+>Found (more than )?([0-9]+)')
RATINGS_REGEX = re.compile(r'^\s+<B>([0-9]+)\)')

def K5CountComments(offset):
    found = 0
    page = 'http://www.kuro5hin.org/?op=search&offset='+str(offset)+\
            '&old_count=0&type=comment_by&topic=&section=&string='+USER+\
            '&search=Search&count=50&next=Next+Page+%3E%3E'
    print page
    try:
        f = urllib.urlopen(page)
    except urllib.HTTPError, msg:
        print "Unable to open page. User may not exist, or K5 may be down."
        print msg
        sys.exit(1)
    debug = open('debug.txt', 'w')
    for line in f.readlines():
        r = REGEX.match(line)
        debug.write(line)
        if r:
            print line
            r_ = r
            #break
    debug.close()
    if r_ and int(r_.group(2)) == 50:
        found = 1
        comments = 50 + K5CountComments(offset + 50)
    elif r_:
        found = 1
        comments = int(r_.group(2))

    if not found: return 0
    return comments

def K5CountDiaries(offset):
    found = 0
    page = 'http://www.kuro5hin.org/?op=search&offset='+str(offset)+\
            '&old_count=0&type=diary_by&topic=&section=&string='+USER+\
            '&count=50&next=Next+Page+%3E%3E'

    try:
        f = urllib.urlopen(page)
    except urllib.HTTPError, msg:
        print "Unable to open page. User may not exist, or K5 may be down."
        print msg
        sys.exit(1)
    for line in f.readlines():
        r = REGEX.match(line)
        if r and int(r.group(2)) == 50:
            found = 1
            print r.group(0)
            diaries = 50 + K5CountDiaries(offset + 50)
        elif r:
            found = 1
            diaries = int(r.group(2))

    if not found: return 0
    return diaries

def K5CountRatings(offset):
    page = 'http://www.kuro5hin.org/?op=user&tool=ratings&nick='+USER+\
            '&start='+str(offset)

    try:
        f = urllib.urlopen(page)
    except urllib.HTTPError, msg:
        print "Unable to open page. User may not exist, or K5 may be down."
        print msg
        sys.exit(1)

    for line in f.readlines():
        r = RATINGS_REGEX.match(line)
        if r: last_r = r
    if last_r and int(last_r.group(1)) == offset + 30:
        ratings = K5CountRatings(offset+30)
    elif last_r:
        ratings = int(last_r.group(1))
    else:
        ratings = 0
    
    return ratings

def K5CountStories(offset):
    found = 0
    page = 'http://www.kuro5hin.org/?op=search&offset='+str(offset)+\
            '&old_count=50&type=author&topic=&section=&string='+USER+\
            '&search=Search&count=50'
    
    try:
        f = urllib.urlopen(page)
    except urllib.HTTPError, msg:
        print "Unable to open page. User may not exist, or K5 may be down."
        print msg
        sys.exit(1)
    
    for line in f.readlines():
        r = REGEX.match(line)
        if r and int(r.group(2)) == 50:
            found = 1
            stories = 50 + K5CountDiaries(offset + 50)
        elif r:
            found = 1
            stories = int(r.group(2))

    if not found: return 0
    return stories

def display(comments, diaries, ratings, stories):
    print "Statistics for user %s:" % USER
    if comments > -1:
        print "    %d comments" % comments
    if diaries > -1:
        print "    %d diaries" % diaries
    if ratings > -1:
        print "    %d ratings" % ratings
    if stories > -1:
        print "    %s stories" % stories
    
def usage():
    print """k5_count is a utility to give some simple k5 statistics
Usage: k5_count -u name [options]
your options are:
    -u [name]    Give the user about whom you'd like to find statistics
    -c           Count their comments
    -d           Count their diaries
    -h           Print this message
    -r           Count their ratings
    -s           Count their stories

example usage: "k5_count -u rusty -c"
"""

if __name__ == "__main__":
    comments = ratings = diaries = stories = -1
    try:
        opts, args = getopt.getopt(sys.argv[1:], "u:cdrsh", ["help"])
    except getopt.GetoptError:
        # print help information and exit:
        usage()
        sys.exit(2)
    output = None
    for o, a in opts:
        if o in ("-h", "--help"):
            usage()
            sys.exit(1)
        elif o == "-u":
            USER = urllib.quote(a)
        elif o == "-c":
            sys.stdout.write('Counting comments...')
            comments = K5CountComments(0)
            sys.stdout.write('done\n')
        elif o == "-d":
            sys.stdout.write('Counting diaries...')
            diaries = K5CountDiaries(0)
            sys.stdout.write('done\n')
        elif o == "-r":
            sys.stdout.write('Counting ratings...')
            ratings = K5CountRatings(0)
            sys.stdout.write('done\n')
        elif o == "-s":
            sys.stdout.write('Counting stories...')
            stories = K5CountStories(0)
            sys.stdout.write('done\n')
    display(comments, diaries, ratings, stories)
