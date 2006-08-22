#!/usr/bin/python
import sys, readline
from ftplib import FTP

def upload_file(files):
    site = 'llimllib.f2o.org'
    user = 'llimllib'
    passwd = raw_input('please enter the password for %s at %s: ' %\
        (user, site))
    ftp = FTP(site, user, passwd)
    ftp.cwd('www/public_html')
    for f in files:
        print "uploading file %s" % f
        ftp.storlines('STOR %s' % f.name, f)

if __name__ == "__main__":
    start = 0
    files = []
    for f in sys.argv:
        if start:
            files.append(file(f))
        if f == '-f': start += 1
    upload_file(files)
