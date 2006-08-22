#!/usr/bin/python
import re, urllib, sys, getopt

#TODO: use asynchat to speed it up
#TODO: download the images?

FS_RE = re.compile('<A HREF=\"(\S*)\">Full Story</A>')
COM_RE = re.compile('<A HREF=\"(\S*)\">Comments</A>')

def regex_test(line):
    res = FS_RE.search(line)
    if res: return res.group(1)
    else: res = COM_RE.search(line)
    if res: return res.group(1)

def get_diary_links(user, page):
    p = urllib.urlopen('http://www.kuro5hin.org/user/%s/diary/%d' % (user, page))
    hrefs = []
    for line in p:
        res = regex_test(line)
        if res: hrefs.append(res)
    return hrefs

def save_diary(href):
    params = urllib.urlencode({'commentmode': 'nested'})
    d = urllib.urlopen('http://www.kuro5hin.org%s' % href, params)
    href = href.replace('/', '_')
    f = open(href[1:] + '.html', 'w')
    f.write(d.read())
    f.close()

def get_all_diaries(user):
    count = 1
    hrefs = get_diary_links(user, count)
    for h in hrefs: save_diary(h)
    while len(hrefs) > 0:
        count += 1
        hrefs = get_diary_links(user, count)
        for h in hrefs: save_diary(h)

if __name__=="__main__":
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'u:')
    except getopt.GetoptError:
        print 'type "python diary_ext.py -u <username>" to use'
    username = ''
    for o, a in opts:
        if o == "-u": username = a
    if not username: print 'type "python diary_ext.py -u <username>" to use'
    else: get_all_diaries(username)
