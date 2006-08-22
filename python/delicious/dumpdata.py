#!/usr/bin/env python2.4
import sys, md5, sets, random, time, urllib
import feedparser as rss

randint = random.Random().randint

tags_found = sets.Set()
users_found = sets.Set()
alreadyparsed = sets.Set()

def eat_delicious_rss(fin, fout=sys.stdout):
    global tags_found, users_found, alreadyparsed
    entries = rss.parse(fin)['entries']
    for e in entries:
        desc = e.get('description', u'')
        tags = e.get('category', u'')
        t = (e['author'], tags, e['title'], desc, e['link'], e['modified'])
        #make sure there's no '\t's
        assert [elt.find(u'\t') for elt in t] == [-1] * 6
        s = u'\t'.join(t).encode('utf8')
        hash = md5.md5(s).hexdigest() #not == del.icio.us hash
        if not hash in alreadyparsed:
            fout.write(s)
            fout.write('\n')
            alreadyparsed.add(hash)
            users_found.add(e['author'])
            for t in tags.split(' '):
                if t not in tags_found:
                    tags_found.add(t)

if __name__ == "__main__":
    dataout = file('data.delicious', 'w')
    count = 0
    while 1:
        xmlout = file('rss.xml', 'w')
        xmlin = file('rss.xml', 'rb')
        if count % 30 == 0:
            url = 'http://del.icio.us/rss'
        elif count % 2 == 0:
            user = list(users_found)[randint(0, len(users_found)-1)]
            url = 'http://del.icio.us/rss/%s' % user
        else:
            tag = list(tags_found)[randint(0, len(tags_found)-1)]
            url = 'http://del.icio.us/rss/tag/%s' % tag
        print "parsing %s" % url
        for line in urllib.urlopen(url):
            xmlout.write(line)
        eat_delicious_rss(xmlin, dataout)
        count += 1
        time.sleep(60)
