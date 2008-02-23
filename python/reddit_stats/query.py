import re, urllib, cPickle, os
from time import sleep

list_stories = re.compile(r'<div id="pre_(\w+)"').findall
list_dates = re.compile(r'&nbsp;posted&nbsp;([\w ]+)&nbsp;ago&nbsp;by&nbsp;').findall

def list_comments(text):
    #not at all accurate; see
    #http://reddit.com/r/programming/info/22vqq/comments/
    #for a particularly egregious example (no number, but "comments"?)
    return [int(z or 0) for z in 
                re.compile(r'>(\d+) comments?</a>|>comments?</a>').findall(text)]

url = "http://reddit.com/r/programming/new?sort=new&after=%s"

if os.path.isfile("stories.pkl"):
    stories = cPickle.load(file("stories.pkl"))
    seed = stories[-1][0]
else:
    stories = []
    seed = "t3_6914u"

class LlimllibURLopener(urllib.FancyURLopener):
    version = "user llimllib wants statistics" #set the user-agent string
urllib._urlopener = LlimllibURLopener()

for i in range(1000):
    print "opening page %s" % url % seed
    text = urllib.urlopen(url % seed).read()
    s, c, d = list_stories(text), list_comments(text), list_dates(text)
    if (len(s), len(c), len(d)) != (25,25,25):
        print "error parsing page: %s (%d) %s (%d) %s (%d)" % \
            (s, len(s), c, len(c), d, len(d))
        raise
    stories.extend(zip(s,c,d))
    seed = stories[-1][0]
    print "last story from %s" % stories[-1][2]
    sleep(.5)

cPickle.dump(stories, file("stories.pkl"))
