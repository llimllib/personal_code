#from web import template
from xyaptu import xcopier
import sys, cStringIO, timeit

class bloxparse(xcopier):
    def __init__(self, dns, ouf=sys.stdout, dbg=0):
        xcopier.__init__(self, dns, ouf=ouf, dbg=dbg)

    def _handleBadExps(self, s):
        if self.dbg: 
            self.dbgOuf.write('!!! ERROR: failed to evaluate expression: %s \n'\
            % s)
        return ''

class photo:
    def __init__(self, i): 
        self.location = "This is at %d" % i

def parse(file, dict, debug=0):
    outbuf = cStringIO.StringIO()
    xy = bloxparse(dict, ouf=outbuf, dbg=debug)
    xy.xcopy(file)
    return outbuf.getvalue()

title = "this is a web page"
var = "variable"
photos = [photo(i) for i in range(4)]

yap_template = file('template.html')
print parse(yap_template, locals())
