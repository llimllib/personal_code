#!/usr/bin/python
import path, re
from ftplib import FTP
from print_html import *
from mod_python import apache, util

class Blog:
    def __init__(self, req, args):
        self.req = req
        self.blog(args)
    
    def blog(self, args):
        try: act = args['action']
        except KeyError:
            self.print_staticblog()
            return
        if act == 'add':
            try:
                title = args['title']
                body = args['blog_body']
                self.add_new_blog(title, body)
            except KeyError: self.print_addblog()

    def add_new_blog(self, title, body):
        print >>self.req, "here is where I would put blog data into a db"
        print >>self.req, title
        print >>self.req, body

    def print_addblog(self):
        #print >>self.req, "here is where I would put a blog entry form"
        print >>self.req, "what the fuck"
        self.req.sendfile('blog_enter.html')

    def print_staticblog(self):
        print >>self.req, "here I would print the static blog"
