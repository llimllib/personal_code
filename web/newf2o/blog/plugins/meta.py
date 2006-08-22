"""Creates meta information about a file

Right now, it only reads any C{<!--keyword: x,y,z -->}
lines in a file and pickles them into meta_dir.

There are two configuration options:

    - meta_dir: Directory in which to store meta information
    - all_keywords: If true, generate a list of all keywords in
                    C{data['meta']['all_keywords']}

Is there any other metadata that could be implemented this way? If you have
an idea, drop me a line.
"""
# Probably going to need an ignore_meta and a load_mem_meta (so comments don't
# get loaded into memory)?
# TODO: notice when a file has been updated and reload its meta-data

__author__ = "Bill Mill - bill.mill@gmail.com"
__version__ = "0.1"
__url__ = "http://llimllib.f2o.org/blog"
__description__ = "Provides a framework for meta-blog information"

from Pyblosxom import tools
import os, cPickle

#tools.make_logger('c:/code/web/newf2o/blog/log/meta.log')
#log = tools.log

def verify_installation(request):
    """This is for verifying that this is installed correctly."""
    #what to put here?
    return 1

#keep this global so the recursive function can always get it
META_DIR = ''
DATA_DIR = ''

def pathjoin(*dirs):
    """join directories with the '/' character

    @param dirs: any number of directory strings
    @type dirs: Strings

    @return: string containing all the dirs joined in order
    """
    return '/'.join(dirs)

def metasearch(keyword, fname=None):
    """searches for any entries with meta data key L{keyword}

    @param keyword: meta-data key to search for
    @type keyword: String

    @param fname: A particular file or list of files to check for L{keyword}.
                  Should be full path to file after $data_dir
    @type fname: String or List

    @returns: dictionary keyed by filename containing meta-data, value is
              the actual meta-data
    """
    if type(fname) == type(''):
        pickle = pathjoin(META_DIR, fname[:-3] + 'meta')
        return cPickle.load(file(pickle, 'r')).get(keyword, '')
    elif type(fname) == type([]):
        for f in fname:
            pickle = pathjoin(META_DIR, fname.replace('.txt', '.meta'))
            return cPickle.load(file(pickle, 'r'))
    data = (keyword, {})
    os.path.walk(META_DIR, walkgetmeta, data)
    return data[1]

def walkgetmeta(data, dirname, fnames):
    """Walks the meta dir and loads the requested data

    @param data: (kw, {}) tuple. kw is the keyword to search for, with data
                 to be stored in the dictionary.
    @type data: Tuple

    @return: returns data in the data[1] dictionary
    """
    kw, data = data
    for f in fnames:
        if f.endswith('.meta'):
            md = cPickle.load(file(pathjoin(dirname, f), 'r'))
            fullname = dirname.split(DATA_DIR)[1].replace('\\', '/').lstrip('/')
            val = md.get(kw, '')
            key = md['datafile']      #it's an error if this doesn't exist
            f = ''.join([f[:-4], 'txt'])    #end f with .txt
            if val: data[key] = val

def cb_start(args):
    """sets up meta information

    Called at plugin initialization, this function starts the recursive data
    directory search. It calls walkmeta for every directory it finds.

    @param args: A dictionary containing the request object
    @type args: Dictionary
    @return: None
    """
    if DATA_DIR: return   #somebody already ran our start routine; this is
                          #required to use metasearch
    config = args['request'].getConfiguration()
    data = args['request'].getData()

    data['meta'] = {}

    global DATA_DIR, META_DIR 
    DATA_DIR = config['datadir']
    META_DIR = config.get('meta_dir', '')

    if not META_DIR or not os.path.isdir(META_DIR):
        raise "You need to specify a valid py['meta_dir'] in your config.py"

    os.path.walk(DATA_DIR, walkmeta, data)

    base = config['base_url']

def walkmeta(data, dirname, fnames):
    """Loads meta-data if it exists, creates it if not

    Called on each directory in the datadir, simply makes sure that each *.txt
    file has a corresponding *.meta file. If not, it calls the parse function 
    and creates the file.

    @param data: A reference to the requests' data dictionary
    @type data: Dictionary

    @param dirname: The name of the current directory
    @type dirname: String

    @param fnames: The names of the files in the current directory
    @type fnames: List

    @return: None; stores data in the data['meta'] dictionary
    """
    #TODO: should .txt be a configuration option?
    dirname = dirname.replace('\\', '/')
    for f in fnames:
        if f.endswith('.txt'):
            subdir = dirname.split(DATA_DIR)[1].lstrip('/')
            pickle_dir = pathjoin(META_DIR, subdir)
            if not os.path.isdir(pickle_dir):
                os.makedirs(pickle_dir)
            pickle = pathjoin(pickle_dir, f[:-3] + 'meta')
            fullpath = pathjoin(dirname, f)
            if not os.path.isfile(pickle):
                md = parsemeta(file(fullpath, 'r'))
                md['datafile'] = pathjoin(subdir, f)
                pickle = file(pickle, 'w')
                #TODO: use protocol 1 or 2 to speed it up?
                cPickle.dump(md, pickle)
                pickle.close()

def parsemeta(f):
    """Parse an opened file and return any meta-data

    @params f: An open file
    @type f: File
    @return: A dictionary where 'keywords' references a list of keywords
    """
    md = {}
    for line in f:
        if line.startswith('<!--') and line.endswith('-->\n'):
            try: key, val = line[4:-4].split(':', 1)
            except ValueError: continue     #ignore real comments!
            md[key.strip()] = val.strip()
    return md
