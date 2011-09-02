import re
import sparklines.spark
import cPickle
import simplejson
from PIL import Image, ImageDraw, ImageFont
from geopy.geocoders import Google, Yahoo
from googlemaps import GoogleMaps, GoogleMapsError
from time import sleep
from datetime import datetime
from urllib2 import HTTPError, URLError, urlopen, quote
from group import group
from zillow_hoods import zhoods
from carto_utils import is_in, centerpt, centroidpt

class Foreclosure(object):
  def __init__(self, v):
    self.hood = v["Neighborhood"]
    self.n = v["Number"]
    self.street = v["Street"]
    self.city = v["City"]
    self.state = v["State"]
    self.zip = v["Zip"]
    
    #added later
    self.lat = None
    self.lon = None
    self.bniahood = None

    self.amt = v.get('Amount', None)
    if self.amt:
      try:
        #good god there are a lot of bugs in the amount value
        #XXX: fix $xxx.yyy.zz to $xxxyyy.zz
        self.amt = float(re.sub("[,$ ]", "", self.amt).rstrip("."))
        #remove zeroes from the dataset too
        #XXX: what do they mean, really?
        if int(self.amt) == 0:
          self.amt = None
      except ValueError:
        print "couldn't parse: ", self.amt
        self.amt = None

    self.dt = v.get('Date', None)
    if self.dt:
      self.dt = datetime.strptime(self.dt, "%m/%d/%Y")

  def __unicode__(self):
    return "%s %s: %s" % (self.n, self.street.title(), self.amt or "unknown")

  def __str__(self): return self.__unicode__()

  def __repr__(self): return self.__unicode__()

  @property
  def street_address(self):
    return self.n + " " + self.street

def clean_hood_name(name):
    return re.sub("\W", "", name)

#XXX TODO this is ugly
delay = 1
delaysum = 0

def ygeocode(addr):
    global delay, delaysum
    key = 'bKVpVX_V34Ec62VfSmdXD1Sow.dHyfSQfxCp5qeug95VuX1.mxPFUZvqYDR1HUb8WOyE.d.q'
    url = 'http://local.yahooapis.com/MapsService/V1/geocode?'
    url += 'appid=' + key
    url += '&location=' + quote(addr + ", Baltimore, MD")
    try:
        sleep(delay)
        location = urlopen(url).read()
        if re.search("limit exceeded", location, re.I|re.S):
            raise Exception("too many queries")
        lat = re.findall("<latitude>(.*?)<", location, re.I|re.S)[0]
        lon = re.findall("<longitude>(.*?)<", location, re.I|re.S)[0]
        return (float(lat), float(lon))
    except (HTTPError, URLError), e:
        print e
        return (None, None)

g = GoogleMaps('ABQIAAAAi38qWc-9V8q5b6bgPClsfxTfKEWMeMOdp19wa3uONsY3R1LXvRTgTR8o6snPUFoi7M-AG31rpq6VWQ')
def geocode(addr):
  global delay, delaysum
  addr += ", Baltimore, MD"
  #in here so that I can add one if I start to get errors
  try:
    sleep(delay)
    delaysum += delay
    if delaysum > 1000:
      delay = delay / 2
      delaysum = 0
    return g.address_to_latlng(addr)
  except GoogleMapsError, exc:
    if exc.status == GoogleMapsError.G_GEO_TOO_MANY_QUERIES:
      print "too fast!"
      delay += 2
      geocode(addr)
    if exc.status == GoogleMapsError.G_GEO_UNKNOWN_ADDRESS or GoogleMapsError.G_GEO_UNAVAILABLE_ADDRESS:
      return (None, None)
    else:
      print "wtf, mate!"
      import pdb; pdb.set_trace()

def get_geolocate():
    #let's make better hoods
    found = 0
    notfound = 0
    notfounds = []
    for fc in foreclosures:
        if fc.lat:
            found += 1
            continue

        fc.lat, fc.lon = geocode(fc.street_address)
        if fc.lat is None:
            notfounds.append(fc)
            notfound += 1
        else:
            found += 1
            if found % 300 == 0:
                print "found: %s out of 13333, %.0f%% done" % (found, (found/13333.)*100)
                print "notfound: %s, %.0f%% fail" % (notfound, (notfound/float(found+notfound))*100)
                cPickle.dump(foreclosures, file("latlon_foreclosures.pkl", "w"))
                cPickle.dump(notfounds, file("notfounds.pkl", "w"))

def find_zillow_hood(pt):
    for hood_name, boundary in zhoods.iteritems():
        if is_in(pt, boundary):
            return hood_name
    return 'unknown'

def set_zillow_hoods():
    foreclosures = cPickle.load(file("latlon_foreclosures.pkl"))

    found = 0
    failed = 0
    for fc in foreclosures:
        if not fc.lat: continue

        fc.zhood = find_zillow_hood((fc.lat, fc.lon))

        if fc.zhood == 'unknown': failed += 1
        else:                     found  += 1

    print "found %s, failed %s success %.2f%%" % (found, failed, (float(found)/found+failed)*100)

    cPickle.dump(foreclosures, file("zhood_foreclosures.pkl", "w"))

class HoodNotFoundException(Exception): pass

def find_bnia_hood(pt, bnia_hoods):
    for hood_name, boundary in bnia_hoods.iteritems():
        if is_in(pt, boundary):
            return hood_name
    raise HoodNotFoundException(str(pt))

def set_bnia_hoods():
    foreclosures = cPickle.load(file("latlon_foreclosures.pkl"))
    bniahoods = cPickle.load(file("bnia_hoods.pkl"))

    found = 0
    failed = 0
    for fc in foreclosures:
        if not fc.lat: continue

        try:
            fc.bniahood = find_bnia_hood((fc.lat, fc.lon), bniahoods)
            found += 1
        except HoodNotFoundException:
            failed += 1

        if (found+failed) % 10 == 0:
            print "found %s, failed %s success %.2f%%" % (found, failed, (float(found)/13333)*100)

    print "found %s, failed %s success %.2f%%" % (found, failed, (float(found)/13333)*100)

    cPickle.dump(foreclosures, file("bniahood_foreclosures.pkl", "w"))

def add_bnia_hood_centers():
    hoods = cPickle.load(file("bnia_hoods.pkl"))
    for hood, borders in hoods.iteritems():
        hoods[hood] = (borders[0], borders[1], centroidpt(borders))
    cPickle.dump(hoods, file("bnia_hoods_centers.pkl", "w"))

def make_hoods_json():
    hoods = cPickle.load(file("bniahood_foreclosures.pkl"))
    bniahoods = cPickle.load(file("bnia_hoods_centers.pkl"))

    def bniahood(fc):
        if not getattr(fc, "bniahood", None): return None
        return fc.bniahood

    #group by hood, then just count per hood values and calc an avg
    hoods = group(hoods, bniahood)
    hood_lens = dict((k, (len(v), len(v) and float(sum(vv.amt for vv in v if vv.amt))/len(v)))
                     for k,v in hoods.iteritems() if k)
    for hood, v in hood_lens.iteritems():
        bh = bniahoods[hood]
        hood_lens[hood] = (v[0], v[1], bh[0], bh[1], bh[2], clean_hood_name(hood))

    simplejson.dump(hood_lens, file("hoods.json", "w"))

def make_hoods_quarters_averages():
    hoods = cPickle.load(file("bniahood_foreclosures.pkl"))

    def mkquarter(fc):
        if not fc.dt: return None
        return str(fc.dt.year)+"Q"+str((fc.dt.month-1)/3)

    def bniahood(fc):
        if not getattr(fc, "bniahood", None): return None
        return fc.bniahood

    hoods = group(group(hoods, bniahood), mkquarter)

    avgs = {}
    for hood, quarters in hoods.iteritems():
      quarters = sorted(list(quarters.iterkeys()))
      for quarter in quarters:
        q = hoods[hood][quarter]

        a = [fc.amt for fc in q if fc.amt]
        if not a: continue

        avg = sum(a) / float(len(a))
        n = len(a)
        avgs.setdefault(hood, []).append((avg, n, quarter))

def make_circles():
    #to install this, see options 2 and 4 at:
    #http://nodebox.net/code/index.php/Console . Mac Only!

    from AppKit import NSApplication
    NSApplication.sharedApplication().activateIgnoringOtherApps_(0)

    from nodebox.graphics import Context
    from nodebox.util import random, choice, grid, files

    hoods = simplejson.load(file("hoods.json"))
    max_n = float(max(v[0] for k,v in hoods.iteritems()))
    for hood, vals in hoods.iteritems():
        ctx = Context()
        colors = ctx.ximport("colors")

        n = vals[0]
        a = vals[1]

        ctx.background(None)

        maxw = 100
        pct = n / max_n
        d = pct * maxw
        margin = 2
        w = h = d + (margin*2)
        mid = w/2
        ctx.size(maxw+margin*2,maxw+margin*2)

        ctx.fill(colors.hex("#608902d"))
        ctx.oval(2,2, d,d)

        ctx.fill(colors.hex("#E6FA87"))
        n = str(n)
        ctx.fontsize(int(24 * pct))
        tw = ctx.textwidth(n)
        th = ctx.textheight(n)
        ctx.text(n, mid-(tw/2), mid+(th/4))

        ctx.save("circles/%s.png" % clean_hood_name(hood))
        print clean_hood_name(hood)

if __name__=="__main__":
    add_bnia_hood_centers()
    make_hoods_json()
    make_circles()
