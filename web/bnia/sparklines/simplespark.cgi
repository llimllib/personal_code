#!/usr/bin/python
import cgi
import cgitb
import sys
import os

cgitb.enable()

import Image, ImageDraw
import StringIO
import urllib


def plot_sparkline(f, results):
   """Returns a sparkline image as a data: URI.
       The source data is a list of values between
       0 and 100. Values greater than 95
       are displayed in red, otherwise they are displayed
       in green"""
   im = Image.new("RGB", (len(results)*2, 15), 'white')
   draw = ImageDraw.Draw(im)
   for (r, i) in zip(results, range(0, len(results)*2, 2)):
       color = (r > 50) and "red" or "gray"
       draw.line((i, im.size[1]-r/10-4, i, (im.size[1]-r/10)), fill=color)
   del draw

   im.save(f, "PNG")

def plot_error(f):
   im = Image.new("RGB", (40, 15), 'white')
   draw = ImageDraw.Draw(im)
   draw.line((0, 0) + im.size, fill="red")
   draw.line((0, im.size[1], im.size[0], 0), fill="red")
   del draw                                                      
   im.save(f, "PNG")

def error(status="Status: 400 Bad Request"):
    print "Content-type: image/png"
    print status 
    print ""
    plot_error(sys.stdout)
    sys.exit()

def cgi_param(form, name, default):
    return form.has_key(name) and form[name].value or default

if not os.environ['REQUEST_METHOD'] in ['GET', 'HEAD']:
    error("Status: 405 Method Not Allowed")
form = cgi.FieldStorage()
raw_data = cgi_param(form, 'd', '')
if not raw_data: 
    error()
data = [int(d) for d in raw_data.split(",") if d]
if min(data) < 0 or max(data) > 100:
    error()

print "Content-type: image/png"
print "Status: 200 Ok"
print ""
plot_sparkline(sys.stdout, data)
