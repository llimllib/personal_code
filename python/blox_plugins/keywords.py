"""The keywords plugin implements a keyword-based logging system, similar
to the bookmarking system already in place at http://del.icio.us. When you
write a blog entry, you simply include a comment of the form:

C{<!--keywords: python, programming, fun-->}

Where C{python}, C{programming}, and c{fun} are the keywords for your article. 
The
comment must begin at the beginning of the line, and contain no trailing
characters.

The keywords plugin will parse this keywords line and save it as metadata to
be quickly read in later. For this reason, it requires that the C{meta} module
be installed. A url of the form C{http://$base_url/$keyword_trigger/kw} will
return all blog entries which reference the keyword "kw".

The only configuration option for this plugin is the C{$keyword_trigger} option.
Put a line in your config.py telling this plugin what to use as the keyword
trigger (default 'keyword') like this:

C{py['keyword_trigger'] = 'keyword'}

To display a linked list of keywords in your article, simple include the
$keywords variable in the story template.
"""

__author__ = "Bill Mill - bill.mill@gmail.com"
__version__ = "0.1"
__url__ = "http://llimllib.f2o.org/blog"
__description__ = "Provides a pyblosxom keyword system"

from Pyblosxom import tools
import meta
import os

metasearch = meta.metasearch

def verify_installation(request):
    """This is for verifying that the plugin is installed correctly."""
    #not sure what to do here
    return 1

def parsekw(kwlist):
    if not kwlist: return ''
    return [kw.strip() for kw in kwlist.split(',')]

def cb_start(args):
    request = args['request']
    config = request.getConfiguration()
    all_keywords = config.get('all_keywords', 0)

    #move this to keywords.py
    if all_keywords:
        if not meta.DATA_DIR: meta.cb_start(args)   #assure meta initialization
        base = config['base_url']
        kwlist = []
        md = metasearch('keywords')
        for f in md:
            for kw in parsekw(md[f]):
                if kw not in kwlist:
                    kwlist.append(kw)
        kwlist.sort()
        for i in range(len(kwlist)):
            kwlist[i] = '<a href="%s/keyword/%s">%s</a><br>\n' % \
                (base, kwlist[i], kwlist[i])

        config['all_keywords'] = ''.join(kwlist)

def cb_story(args):
    """Create the $keywords variable for the story template

    If there is meta data about the file being processed, grab it and make
    an html-linked keyword list.

    @param args: A dictionary containing the entry to be parsed and the request
            object.
    @type args: dictionary
    @return: Simply sets the entry['keyword'] element, no return necessary
    """
    entry = args["entry"]
    request = args["request"]
    data = request.getData()
    config = request.getConfiguration()

    base_url = config['base_url']

    fname = entry.getId().replace('\\', '/')
    fname = fname.split(config['datadir'])[1].lstrip('/')

    k = parsekw(metasearch('keywords', fname))
    #TODO: improve templating language to avoid this hack; add simple for loop
    #      should that really be done though?
    linkstr = ''
    for i in range(len(k)):
        linkstr += '<a href="%s/keyword/%s">%s</a>, ' % (base_url, k[i], k[i])
    linkstr = linkstr[:-2]
        
    entry["keywords"] = linkstr

def cb_pathinfo(args):
    """Parse the path, checking for a keyword search

    Looks for URLs of the form $base_dir/$keyword_trigger/my_kw . If it finds
    one, it sets data['keyword'] = my_kw .

    It always lets the default handler run, to grab ?flav requests and set its
    other variables. Perhaps that should be copied here to avoid the
    unnecessary date processing code in the default handler, but that shouldn't
    take much time at all. Bored? Profile it for me!

    @param args: A dictionary containing the request object
    @type args: Dictionary
    @return: None, so that the default path handler is always used
    """
    request = args['request']
    urltrigger = request.getConfiguration().get('keyword_trigger', 'keyword')

    path = request.getHttp().get('PATH_INFO', '')
    path = [x for x in path.split('/') if x != '']

    if urltrigger in path and len(path) == 2:
        request.getData()['keyword'] = path[1]
    return None     #no matter what, keep processing like normal

def cb_filelist(args):
    """Select the proper file entries to be displayed

    cb_filelist first looks for a keyword that is being searched. If it doesn't
    find it, it relinquishes control to the default filelist handler. If it
    does find a keyword, it returns file objects for all blog entries matching
    that keyword.

    @param args: A dictionary containing the request object
    @type args: Dictionary
    @return: A list of FileEntry objects to be rendered
    """
    from Pyblosxom.entries.fileentry import FileEntry
    from meta import pathjoin

    request = args['request']
    data = request.getData()
    config = request.getConfiguration()
    if not data.has_key('keyword'): return None

    entrylist = []
    metadata = metasearch('keywords')
    for f in metadata:
        if data['keyword'] in parsekw(metadata[f]):
            path = pathjoin(config['datadir'], f)
            entry = FileEntry(request, path, data['root_datadir'])
            entrylist.append((entry._mtime, entry))

    entrylist.sort()
    entrylist.reverse()
    entrylist = [x[1] for x in entrylist] #remove dates

    return entrylist
