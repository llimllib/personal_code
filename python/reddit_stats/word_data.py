import cPickle, random, urllib, re
from pylab import plot, show
from numpy import median, average, array, std
from itertools import chain

from dataplot import group

class LlimllibURLopener(urllib.FancyURLopener):
    version = "user llimllib wants statistics" #set the user-agent string
urllib._urlopener = LlimllibURLopener()

stories = cPickle.load(file("stories.pkl"))
storydict = dict((a, (b,c)) for a,b,c in stories)

#The procedure will look something like this:
#1) Group the comments
#2) pick a certain number to sample the comments from
#3) get the comments, gather statistics on them, make awesome graphs and charts

storygrps = list(group(stories, 250))

url = "http://reddit.com/info/%s/comments"

def get_stories():
    for grp in storygrps:
        nonzero = [g for g in grp if g[1] > 0]
        for story, n, dt in random.sample(nonzero, min(5, len(nonzero))):
            print "getting url %s" % url % story[3:]
            text = urllib.urlopen(url % story[3:]).read()
            comments, dt = storydict[story]
            storydict[story] = (comments, dt, map(len, get_comments(text, comments)))

deleteme = ''
def get_comments(text, n_expected_comments):
    comm_re = re.compile('class="commentbody "><div class="md">(.*?)</div>', 
                         re.S)
    comments = comm_re.findall(text)
    #if len(comments) != n_expected_comments:
    #    print comments, "%s != %s" % (len(comments), n_expected_comments)
    #    deleteme=text
    #    raise
    return comments

def flatten(listOfLists):
    return list(chain(*listOfLists))

def munge(d):
    pts = []
    for grp in storygrps:
        gpts = []
        for story,_,_ in grp:
            if len(d[story]) > 2:
                gpts.append(d[story][2])
        pts.append(flatten(gpts))
    return pts
