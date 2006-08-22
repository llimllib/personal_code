"""A drop-in caching system for pyblosxom. Makes pyblosxom somewhere between
0%-30% faster, in my tests. Simply put it in your plugins folder (specified in
config.py) and set the configuration variable cache_dir to the directory where
you would like the cache to be kept.

Works by overriding the default filelist function with a caching filelist
function.

Configuration variables:
cache_dir: Directory in which to store cache files"""

import sys, os, cPickle
from Pyblosxom import tools
from Pyblosxom.entries.fileentry import FileEntry

def cb_start_deactivate(args):
    """if you want to activate a log for debugging, remove the "_deactivate"
    from the name of this function and edit the file names below to your 
    liking. Then simply put a log('mystring') call anywhere you want in this
    or any other pyblosxom file.
    """
    global log
    if sys.platform == 'win32':
        tools.make_logger('c:/code/web/blox_devel/pyblosxom/log/pi.out')
        log = tools.log
    else:
        tools.make_logger('/c/code/web/cleanblox/log/npi.out')
        log = tools.log

def sort(filelist):
    """Sorts a filelist.

    Implemented separately because there was talk that this might want to be a
    callback?

    @params filelist: List of FileEntry objects to sort
    @type filelist: List

    @returns: sorted List
    """
    entries = []
    for f in filelist:
        entries.append((f._mtime, f))
    entries.sort()
    entries.reverse()
    return [e[1] for e in entries]

def pathjoin(*dirs):
    """join directories with the '/' character. We want to use this instead of
    os.path.join() because life is easier if everything uses '/' characters
    (which work in Windows and Posix) for path seperation.

    @param dirs: any number of directory strings
    @type dirs: Strings

    @return: string containing all the dirs joined in order
    """
    #putting this here slightly speeds up filelist
    return '/'.join(dirs)

def walkpath(request, path, depth):
    """Walk a directory, returning all files that we can handle. Uses 
    os.path.walk instead of the path walker in tools. Why? just because I wanted
    to reimplement that function to tune for speed. I couldn't get any extra
    speed out of my function (although I think it runs as fast), so if this
    causes problems, let me know and I'll return to L{tools.Walk} .

    @param request: the request object
    @type request: Dictionary

    @param path: the path from which to start walking
    @type path: String

    @param depth: Unimplemented. TODO (as described in config.py)
    @type depth: Integer

    @returns: All files with registered extensions in subdirs of L{root}
    """
    ext = request.getData()['extensions'].keys()
    
    ignore = request.getConfiguration().get("ignore_directories", None)
    if not os.path.isdir(path): return []

    files = []
    os.path.walk(path, filterfiles, (ext, files))
    return files

def filterfiles(args, dirname, fnames):
    """Add all matching files to the files list - read L{walkpath} docs"""
    for f in fnames:
        for ext in args[0]:
            if f.endswith(ext): args[1].append(pathjoin(dirname, f))

def get_cache(filename):
    """Load the cache of a file
    
    @param filename: The name of the file to check for a cache
    @type filename: String
    
    @returns: The associated cache Dictionary if it exists, else {}
    """
    try:
        f = file(filename, 'rb')
        tools.lock(f, tools.LOCK_EX)
        c = cPickle.load(f)
        f.close()
        return c
    except IOError: return {}

def put_cache(cache, filename):
    """Write L{cache} to L{filename}

    @param cache: Dictionary to store in cache
    @type cache: Dictionary

    @param filename: name of file to put the cache in
    @type filename: String

    @returns: None
    """
    f = file(filename, 'wb')
    tools.lock(f, tools.LOCK_EX)
    cb = cPickle.Pickler(f, -1)
    cb.dump(cache)
    f.close()

def cb_filelist(args):
    """Override the filelist callback to provide caching"""
    request = args['request']
    http = request.getHttp()
    config = request.getConfiguration()

    dirname = config.get("cache_dir", "/tmp/cache")
    path = http.get('PATH_INFO', '').lstrip('/') or 'root'
    f = pathjoin(dirname, path)
    c = get_cache(f)

    #if this request has already been cached, get it and return it
    incache = c.get('filelist', None)
    if incache: 
        return incache

    #if no cache found, we need to load our data
    data = request.getData()
    req_type = data['bl_type']

    if req_type == 'file':
        filelist = [data['root_datadir']]
    elif req_type == 'dir':
        filelist = walkpath(request, data['root_datadir'], 1)
    else: filelist = []
    for i in range(len(filelist)):
        filelist[i] = FileEntry(request, filelist[i], data['root_datadir'])
    filelist = sort(filelist)  #make this a sort callback?
    filelist = filelist[:config['num_entries']]

    #now cache the result
    for file_ in filelist: 
        #we can't store the request (it breaks cPickle - why?), and we need 
        #the metadata anyway, so generate it and remove the request link
        file_.getMetadata('')   
        file_._request = None
    c['filelist'] = filelist
    put_cache(c, f)
    return filelist
