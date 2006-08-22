#!/usr/bin/python
#TODO:
# o move htdocs to somewhere on /home
# o make preprocessor work
# o access form data
# o connect to database

import path, re
from ftplib import FTP
from print_html import *
from mod_python import apache, util
from blog import Blog

def process(txt, out):
    """find one-line python statements"""
    r = re.compile("<py (.*) >")
    txt = txt.split('\n')
    for line in txt:
        res = r.match(line)
        if res: out.write(eval(res.group(1)))
        else: out.write(line + '\n')

def compile_html(req=None):
    #print >>req, '"', req.filename, req.canonical_filename, req.path_info, '"'
    #print >>req, req.parsed_uri
    #we're gonna try a unix-only hack here. TODO: fix platformity if it works
    d = req.filename[:req.filename.rfind('/')]
    for p in path.glob.glob('%s/*.htpy' % d):
        if req: print >>req, 'processed %s <br>' % p
        process(file(p).read(), file(p[:-4] + 'html', 'w'))

def upload_files():
    #TODO: update to upload files recursively
    #TODO: upload new images (how to determine newness?)
    site = 'llimllib.f2o.org'
    user = 'llimllib'
    #passwd = raw_input('please enter the password for %s at %s: ' % (user, site)
    ftp = FTP(site, user, passwd)
    ftp.cwd('www/public_html')
    for f in path.glob.glob('*.html'):
        print "uploading file %s" % f
        ftp.storlines('STOR %s' % f, file(f))

def handler(req):
    req.content_type = 'text/html'
    fields = util.FieldStorage(req)
    uri = req.parsed_uri[apache.URI_PATH][1:] #remove the leading '/'
    if uri == 'blog.py' or uri == '': b = Blog(req, fields)
    elif uri == 'pp.py':
        compile_html(req)
        #print >>req, "here I would process htpy files"
    else: req.write("page not found: %s" % uri)
    #for x in dir(req): req.write(x + '<br>')
    return apache.OK

if __name__ == "__main__":
    compile_html()
    #upload_files()
