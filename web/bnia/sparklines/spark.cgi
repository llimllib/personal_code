#!/usr/bin/python -u

"""spark.cgi

A web service for generating sparklines.

Requires the Python Imaging Library 
"""

__author__ = "Joe Gregorio (joe@bitworking.org)"
__copyright__ = "Copyright 2005, Joe Gregorio"
__contributors__ = ['Alan Powell']
__version__ = "1.0.0 $Rev: 323 $"
__license__ = "MIT"
__history__ = """

"""

import cgi
import cgitb
import sys
import os
import time

cgitb.enable()

from pngcanvas import PNGCanvas 
import rgb
import StringIO


last_modified = os.stat('spark.cgi').st_mtime

def plot_sparkline_discrete(results, args, longlines=False):
    """The source data is a list of values between
      0 and 100 (or 'limits' if given). Values greater than 95 
      (or 'upper' if given) are displayed in red, otherwise 
      they are displayed in green"""
    width = int(args.get('width', '2'))
    height = int(args.get('height', '14'))
    upper = int(args.get('upper', '50'))
    below_color = args.get('below-color', 'gray')
    above_color = args.get('above-color', 'red')
    gap = 4
    if longlines:
        gap = 0
    im = PNGCanvas(len(results)*width-1, height)

    (dmin, dmax) = [int(x) for x in args.get('limits', '0,100').split(',')]
    if dmax < dmin:
        dmax = dmin
    zero = im.size[1] - 1
    if dmin < 0 and dmax > 0:
        zero = im.size[1] - (0 - dmin) / (float(dmax - dmin + 1) / (height - gap))
    for (r, i) in zip(results, range(0, len(results)*width, width)):
        color = (r >= upper) and above_color or below_color
        if r < 0:
            y_coord = im.size[1] - (r - dmin) / (float(dmax - dmin + 1) / (height - gap))
        else:
            y_coord = im.size[1] - (r - dmin) / (float(dmax - dmin + 1) / (height - gap))
        im.color = rgb.colors[color]
        if longlines:
            im.rectangle(i, zero, i+width-2, y_coord)
        else:
            im.rectangle(i, y_coord - gap, i+width-2, y_coord)
    return im.dump()

def plot_sparkline_smooth(results, args):
   step = int(args.get('step', '2'))
   height = int(args.get('height', '20'))
   (dmin, dmax) = [int(x) for x in args.get('limits', '0,100').split(',')]
   im = PNGCanvas((len(results)-1)*step+4, height)
   coords = zip(range(1,len(results)*step+1, step), [height - 3  - (y-dmin)/(float(dmax - dmin +1)/(height-4)) for y in results])
   im.color = [128, 128, 128]
   lastx, lasty = coords[0]
   for x0, y0, in coords:
     im.line(lastx, lasty, x0, y0)
     lastx, lasty = x0, y0
   min_color = rgb.colors[args.get('min-color', 'green')]
   max_color = rgb.colors[args.get('max-color', 'red')]
   last_color = rgb.colors[args.get('last-color', 'blue')]
   has_min = args.get('min-m', 'false')
   has_max = args.get('max-m', 'false')
   has_last = args.get('last-m', 'false')
   if has_min == 'true':
      min_pt = coords[results.index(min(results))]
      im.color = min_color
      im.rectangle(min_pt[0]-1, min_pt[1]-1, min_pt[0]+1, min_pt[1]+1)
   if has_max == 'true':
      im.color = max_color
      max_pt = coords[results.index(max(results))]
      im.rectangle(max_pt[0]-1, max_pt[1]-1, max_pt[0]+1, max_pt[1]+1)
   if has_last == 'true':
      im.color = last_color
      end = coords[-1]
      im.rectangle(end[0]-1, end[1]-1, end[0]+1, end[1]+1)
   return im.dump()

def plot_error(results, args):
   im = PNGCanvas(40, 15)
   im.color = rgb.colors['red']
   im.line(0, 0, im.width, im.height)
   im.line(0, im.height, im.width, 0)
   return im.dump()

def not_modified():
    print "Status: 304 Not Modified"
    print ""
    sys.exit()


def ok():
    print "Status: 200 Ok"
    print "Content-type: image/png"
    print "Last-Modified: " + time.ctime(last_modified) + " GMT"
    print 'ETag: "%s"' % str(hash(os.environ['QUERY_STRING'] + __version__))
    print ""

def error(status="Status: 400 Bad Request"):
    print "Content-type: image/png"
    print "X-Other: testing"
    print status 
    print ""
    sys.stdout.write(plot_error([], {}))
    sys.exit()

def cgi_param(form, name, default):
    return form.has_key(name) and form[name].value or default

plot_types = {'discrete': plot_sparkline_discrete, 
               'impulse': lambda data, args: plot_sparkline_discrete(data, args, True), 
                'smooth': plot_sparkline_smooth,
                 'error': plot_error
    }

if not os.environ['REQUEST_METHOD'] in ['GET', 'HEAD']:
    error("Status: 405 Method Not Allowed")
if_none_match = os.environ.get('HTTP_IF_NONE_MATCH', '')
if if_none_match and str(hash(os.environ.get('QUERY_STRING', '') + __version__)) == if_none_match:
    not_modified()
form = cgi.FieldStorage()
raw_data = cgi_param(form, 'd', '')
if not raw_data:
    error("Status: 400 No data supplied")
try:
    (data_min, data_max) = [int(x) for x in cgi_param(form, 'limits', '0,100').split(',')]

    data = [int(d) for d in raw_data.split(",") if d]
    if min(data) < data_min or max(data) > data_max:
        error("Status: 400 Data out of range")

    args = dict([(key, form[key].value) for key in form.keys()])
    type = cgi_param(form, 'type', 'discrete')
    if not plot_types.has_key(type):
        error("Status: 400 Unknown plot type.")

    image_data = plot_types[type](data, args)
    ok()
    sys.stdout.write(image_data)
except:
    error("Status: 500 Exception")

