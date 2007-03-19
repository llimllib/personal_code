from pysvn import Client, ClientError, Revision, opt_revision_kind
import Image

def getrev(client, url, rev):
    diff = None
    try:
        diff = client.diff('/tmp/', url, 
            revision1=Revision(opt_revision_kind.number, rev),
            revision2=Revision(opt_revision_kind.number, rev+1))
    except ClientError:
        print "Error grabbing revision %s of %s" % (rev, url)
    return diff

def genimage(diff):
    pass

def applydiff(text, diff):
    print text
    print diff

def makemovie(url, size):
    client = Client()
    images = []
    r1 = ""
    maxrev = client.info2(url)[0][1]['rev'].number

    for rev in range(1, maxrev):
        diff = getrev(client, url, rev)
        if diff:
            r2 = applydiff(r1, diff)
            images.append(genimage(r2))
            r1 = r2

if __name__=="__main__":
    testfile = "http://billmill.org/svn/buffet/trunk/buffet.py"
    makemovie(testfile, (300, 300))
