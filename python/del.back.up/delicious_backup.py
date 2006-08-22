#!/c/Python23/python.exe
"""
Backup del.icio.us to Gmail

Released into the public domain 4.10.05 by Bill Mill

Please use as you see fit, but this code carries NO EXPRESS WARRANTIES.
I don't even know what that means, but all the legal types put that in caps.
This code might make your computer gain intelligence and start to take over
the world for all I know. However, it also might just save a copy of each
of the pages you've linked to from your del.icio.us account to your gmail
account.

The only non-standard library this script requires is cElementTree; it's
available at http://effbot.org/zone/celementtree.htm . Thanks to Fredrik
for his parser.

To set up, simply fill in your delicious and gmail user name and password 
where the script tells you to. Then run this file with "python delbackup.py"
and sit back and relax. If it fails with a "server reset connection" error, 
or something similar, just start it to running again - it should be smart
enough to pick up where it left off.

If you hack on this file to add something, if you love or hate the script, 
or if you just got something to get off your chest, drop me a line at 
bill.mill@gmail.com.

TODO:
o command line parameters:
    o suppress output
    o delete the backup directory when done
    o save only the content of the HTML pages downloaded; I like it outputting
      the full HTML the way it does, but others won't
o detect what pages have already been uploaded to gmail by some 
  non-brain dead mechanism 
o Figure out how to get the connection to gmail SMTP to quit gracefully
o Allow some other XML parsers (I def won't do this, but others might want to)
o Make i18n actually work? It might, but I haven't tested.
o Let the good times roll
"""
import cElementTree as ce
import urllib2, os, md5, sys, re, smtplib
#necessary because of smtplib bug, or gmail SMTP server bug?
from socket import sslerror

######################
# Fill in your user name and pw; this is all you should *have* to configure
######################
delicious_user = ""
delicious_pw = ""
gmail_user = ""
gmail_pw = ""

update = "http://del.icio.us/api/posts/update"
all = "http://del.icio.us/api/posts/all"

def gmail_connect(user, pw):
    server = smtplib.SMTP('smtp.gmail.com')
    server.ehlo()
    server.starttls()
    server.ehlo()
    server.login(user, pw)
    return server

def gmail_quit(server):
    server.quit()

def send_msg(msg, server):
    global gmail_user, gmail_pw
    to = gmail_user + '+del.icio.us@gmail.com'
    from_ = gmail_user + '+del.icio.us@gmail.com'
    title = msg.split('\n', 1)[0]
    msg = "To: %s\r\nFrom: %s\r\nSubject: %s\r\n\r\n%s\r\n" % \
        (to, from_, title, msg)
    server.sendmail(from_, to, msg)

def size(path):
    bytes = os.stat(path)[6]
    return bytes / (1024.0 * 1024) #return size in megabytes

def make_auth(username,password):
    authinfo = urllib2.HTTPBasicAuthHandler()
    authinfo.add_password('del.icio.us API', 'http://del.icio.us', username, password)
    opener = urllib2.build_opener(authinfo)
    urllib2.install_opener(opener)

print "Checking if del.icio.us has been updated"
auth = make_auth(delicious_user, delicious_pw)
update = urllib2.urlopen(update).read()
try:
    if update != open('update', 'r').read():
        open('update', 'w').write(update)
        sys.exit(0)
        print "Updating all.xml"
        open('all.xml', 'w').write(urllib2.urlopen(all).read())
    else:
        print "all.xml is fresh"
except IOError:
    print "Creating all.xml"
    open('update', 'w').write(update)
    x = urllib2.urlopen(all).read()
    open('all.xml', 'w').write(x)

try:
    os.mkdir('./backup')
except OSError:
    pass
os.chdir('./backup')

gmail = gmail_connect(gmail_user, gmail_pw)

for e, post in ce.iterparse('../all.xml'):
    if post.tag != "post":
        continue
    data = dict(post.items())
    #turn any utf-8 data into binary strings
    name = data['href'][7:].encode('utf-8')
    desc = data['description'].encode('utf-8')
    ext = data.get('extended', '').encode('utf-8')
    tags = data.get('tag', '').encode('utf-8')
    #make the url into a valid filename
    #\W = not([a-zA-Z0-9_]), except unicode-aware
    name = re.sub('\W', '_', name)[-200:]
    try:
        if not os.path.isfile(name):
            print "getting %s as %s" % (data['href'], name)
            f = open(name, 'w')
            f.write(desc + '\n')
            f.write(ext + '\n')
            f.write('Tags: ' + tags + '\n')
            f.write(data['href'] + '\n')
            f.write(urllib2.urlopen(data['href']).read())
            f.close()
            if size(name) < 1:
                print "Emailing %s to %s+del.icio.us@gmail.com" % \
                (name, gmail_user)
                msg = open(name, 'r').read()
                send_msg(msg, gmail)
    except urllib2.URLError:
        print "couldn't find page: %s" % (data['href'])
        f.close()
        continue
