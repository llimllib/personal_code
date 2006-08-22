#!/usr/bin/python
import path, re
from ftplib import FTP
from print_html import *

def process(txt, out):
    """find one-line python statements"""
    r = re.compile("<py (.*) >")
    txt = txt.split('\n')
    for line in txt:
        res = r.match(line)
        if res: out.write(eval(res.group(1)))
        else: out.write(line + '\n')

def compile_html():
    for p in path.glob.glob('*.htpy'):
        process(file(p).read(), file(p[:-4] + 'html', 'w'))

def upload_files():
    #TODO: update to upload files recursively
    #TODO: upload new images (how to determine newness?)
    site = 'llimllib.f2o.org'
    user = 'llimllib'
    passwd = raw_input('please enter the password for %s at %s: ' %\
        (user, site))
    ftp = FTP(site, user, passwd)
    ftp.cwd('www/public_html')
    for f in path.glob.glob('*.html'):
        print "uploading file %s" % f
        ftp.storlines('STOR %s' % f, file(f))

if __name__ == "__main__":
    compile_html()
    upload_files()
