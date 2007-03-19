from pysvn import Client, ClientError, Revision, opt_revision_kind
import Image
from difflib import Differ

def getrev(client, url, rev):
    try:
        return client.cat(url, revision=Revision(opt_revision_kind.number, rev))
    except ClientError:
        print "Error grabbing revision %s of %s" % (rev, url)
        return None

def genimage(diff):
    #TODO
    pass

def groupdiffs(diffs):
    #TODO
    return []

def applydiff(text, diff):
    print text
    print diff
    raw_input("<enter> to continue")

def makemovie(url, size):
    client = Client()
    images = []
    r1 = ""
    d = Differ()
    maxrev = client.info2(url)[0][1]['rev'].number

    for rev in range(1, maxrev):
        r2 = getrev(client, url, rev)
        if r2:
            diffs = list(d.compare(r1.splitlines(), r2.splitlines()))
            print diffs
            for group in groupdiffs(diffs):
                images.append(genimage(r2))
            r1 = r2

if __name__=="__main__":
    testfile = "http://billmill.org/svn/personal_code/python/svnvideo/svnvideo.py"
    makemovie(testfile, (300, 300))
