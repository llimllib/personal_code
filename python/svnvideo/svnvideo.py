from pysvn import Client, ClientError, Revision, opt_revision_kind
import Image
from ImageDraw import Draw
import ImageFont

def getrev(client, url, rev):
    try:
        print "getting rev %s" % rev
        catout = client.cat(url, revision=Revision(opt_revision_kind.number, rev))
        diff = None
        try:
            diff = client.diff('/tmp/', url,
                revision1=Revision(opt_revision_kind.number, rev-1),
                revision2=Revision(opt_revision_kind.number, rev))
        except ClientError, e:
            print "Couldn't diff versions %s and %s of %s" % (rev, rev-1, url)
        return (catout, diff)
    except ClientError, e:
        print "Error grabbing revision %s of %s: %s" % (rev, url, e)
        return (None, None)

LMARGIN = 3
FONT = ImageFont.truetype('ProFontWindows.ttf', 12)
DELTA_Y = FONT.getsize('|')[1] + 3

def gen_one(text, size):
    i = Image.new("RGB", size, (255, 255, 255))
    d = Draw(i)
    y = 0
    for line in text.splitlines():
        d.text((LMARGIN, y), line, font=FONT, fill='black')
        y += DELTA_Y
    i.show()

def gen_diff(r1, r2, size):
    i = Image.new("RGB", size, (255, 255, 255))
    d = Draw(i)
    font = ImageFont.truetype('ProFontWindows.ttf', 12)
    y = 0
    import pdb; pdb.set_trace()
    for l1, l2 in map(None, r1.splitlines(), r2.splitlines()):
        if l1 != l2:
            d.text((LMARGIN, y), l2, font=FONT, fill='green')
        else:
            d.text((LMARGIN, y), l2, font=FONT, fill='black')
        y += DELTA_Y
    i.show()

def groupdiffs(diffs):
    state = diffs[0][0:2]
    storage = []
    for line in diffs[1:]:
        new_state = line[0:2]
        if new_state == state: storage.append(line)
        else:
            yield storage
            state = new_state
            storage = [line]

def makemovie(url, size):
    client = Client()
    images = []
    r1 = ""
    maxrev = client.info2(url)[0][1]['rev'].number

    for rev in range(1, maxrev):
        (r2, diff) = getrev(client, url, rev)
        if r2 and not diff:
            #really wish I had multiple dispatch here
            images.append(gen_one(r2, size))
        elif r2 and diff:
            images.append(gen_diff(r1, r2, size))
        r1 = r2
        raw_input('<enter> to continue')

if __name__=="__main__":
    testfile = "http://billmill.org/svn/personal_code/python/svnvideo/svnvideo.py"
    makemovie(testfile, (500, 800))
