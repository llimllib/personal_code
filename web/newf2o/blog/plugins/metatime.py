"""This is a simple plugin to allow you to change the pyBlosxom time of a blog
entry. It is based on the meta plugin, so you will need to have that installed
already before you use this file. See meta.py for details.

To set a time for your file, simply include a line which is entirely:

C{<!--time: mm/dd/yy HH:MM -->}

Where mm, dd, and yy are month, date, and year, and HH and MM are hour (24-hour
time) and minute. If your file has already been indexed by meta, you will have
to manually delete the C{yourfile.meta} file which is located in your
py['meta_dir'] directory, as meta does not yet (v0.1) notice if files have
been updated.

There are no configuration options for this plugin."""

__author__ = "Bill Mill - bill.mill@gmail.com"
__version__ = "0.1"
__url__ = "http://llimllib.f2o.org/blog"
__description__ = "Allows you to change a blog entry's time"

from meta import metasearch
from Pyblosxom import tools
import time, sys

if sys.platform == 'win32':
    tools.make_logger('c:/code/web/newf2o/blog/log/log.out')
    log = tools.log
else: log = lambda x: None

def cb_prepare(args):
    """Checks to see if an entry has its time set inside the file. If it does,
    set the time correctly and reorder the files to be displayed.

    @param args: The pyBlosxom request object
    @type args: Dictionary

    @return: stores data in the request.data['entry_list'] list
    """
    log('prepare')
    request = args["request"]
    data = request.getData()
    config = request.getConfiguration()
    entry_list = data['entry_list']
    entries = {}
    for entry in entry_list:
        fname = entry.getId().replace('\\', '/')
        fname = fname.split(config['datadir'])[1].lstrip('/')
        entries[fname] = entry

    times = metasearch('time')
    #log("dirs: %s  %s" % (times, entries))
    for f in times:
        t = time.strptime(times[f], '%m-%d-%y %H:%M')
        if entries.has_key(f): entries[f].setTime(t)

    el = [[e['mtime'], e] for e in entry_list]
    el.sort()
    el.reverse()
    el = [e[1] for e in el]
    data['entry_list'] = el

def cb_renderer(args):
    log('render')
