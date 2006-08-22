import sys, urllib, re, random

PAGE = 'http://www.kuro5hin.org/user/$/comments'
K5ERS = ['rusty', 'llimllib', 'gordonjcp', 'dominantparadigm']
REGEX = re.compile(r'^\s+<.*?>([0-9]+)\)\s<[a|A] HREF="(\S*?)"')

def webOpen(page):
    try:
        f = urllib.urlopen(page)
    except urllib.HTTPError, msg:
        print "Unable to open page. User may not exist, or K5 may be down."
        print msg
        sys.exit(1)
    return f

def findRandomCommentURL(user):
    page = PAGE.replace('$', user)
    rand_int = random.Random().randrange(0, 30)

    for line in webOpen(page).readlines():
        r = REGEX.match(line)
        if r and int(r.groups(1)[0]) == rand_int:
            return r.groups(0)[1]

def getComment(url):
    page = 'http://www.kuro5hin.org%s' % url
    
    
    
