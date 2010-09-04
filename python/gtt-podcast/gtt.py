import os
import re
from subprocess import Popen, call
from xml.etree.ElementTree import parse

filedir = os.path.dirname(os.path.abspath(__file__))
datadir = os.path.join(filedir, "data")
os.chdir(filedir)

tree = parse("test.rss")
root = tree.getroot()

for item in root.findall("channel/item"):
    print item
    children = dict((i.tag, i) for i in item)
    url = children['link'].text
    yname = re.search(r"v=(\w+)&", url).groups()[0] 
    dlname = os.path.join(datadir, yname + '.flv')
    oname = os.path.join(datadir, yname + '.mp3')
    #{'category': <Element category at 1004d3c20>, 'description': <Element description at 1004d3cf8>, 'pubDate': <Element pubDate at 1004d3a70>, 'title': <Element title at 1004d3c68>, 'author': <Element author at 1004d3e18>, '{http://www.w3.org/2005/Atom}updated': <Element {http://www.w3.org/2005/Atom}updated at 1004d3b48>, 'link': <Element link at 1004d3d88>, 'guid': <Element guid at 1004d3a28>}
    if not os.path.isfile(oname):
        print "downloading %s" % url
        call(["./youtube-dl", url, "-o", dlname])
        print "converting %s %s" % (dlname, oname)
        call(["ffmpeg", "-i", dlname, oname])
        os.remove(dlname)
    else:
        print "skipping %s" % oname
