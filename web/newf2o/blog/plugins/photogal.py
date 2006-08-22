import os, re, cStringIO, cgi
from Pyblosxom import tools
try:
    import sys
    sys.path.insert(0, '/home/sites/llimllib/www/public_html/blog')
    from PIL import Image
except ImportError:
    raise "You need to have PIL installed to run the photo gallery"

#debugging; remember to remove dependancy on Itpl for release
#import Itpl
#pp = Itpl.itpl

def pathjoin(*args):
    return '/'.join(args)

def delroot(root, filename):
    """remove the root from the filename"""
    return filename.split(root)[1].strip('/').strip('\\')

class EntryBase:
    def __init__(self): raise NotImplementedError

    def get(self, itemname, default=None):
        return self.__getitem__(itemname) or default

    def getData(self): return self._data

    def __getitem__(self, itemname):
        return self._data.get(itemname)

class PhotoEntry(EntryBase):
    def __init__(self, filename, config):
        root = config['root_photodir']
        photobase = config['photo_base']
        thumb_size = config['thumb_size']
        medium_size = config['medium_size']
        p = delroot(root, filename)
        thumb = '.thumb'.join(os.path.splitext(filename))
        medium = '.medium'.join(os.path.splitext(filename))
        #do I want to delay this, like in FileEntry, or get it done at the
        #first chance?
        if not os.path.isfile(thumb):
            self.gen_thumb(filename, thumb, thumb_size)
        if not os.path.isfile(medium):
            self.gen_thumb(filename, medium, medium_size)
        thumb = '/'.join((photobase, delroot(root, thumb)))
        medium = '/'.join((photobase, delroot(root, medium)))
        self._data = {'photofile': p,
                      'filename': filename,
                      'thumb': thumb,
                      'medium': medium,
                      'img_title': p[p.rfind('/')+1:],
                      'img_description': "image descriptions aren't implemented"}

    def gen_thumb(self, orig, tfile, thumb_size):
        i = Image.open(orig)
        #TODO: smarter resizing
        i2 = i.resize(thumb_size) #thumb size as config option?
        i2.save(tfile)

class DirEntry(EntryBase):
    def __init__(self, path, root, photobase):
        self.path = path
        self.photobase = photobase
        self.relpath = delroot(root, path)
        thumb = self.get_thumb()
        desc = "This will be a gallery description, eventually"
        self._data = {'gallery': self.relpath, 
                      'gallery_desc': desc,
                      'thumb': thumb,
                      'gallery_title': path[path.rfind('/')+1:]}

    def get_thumb(self):
        idx = pathjoin(self.photobase, self.relpath, 'index.jpg')#TODO: config option?
        return idx

def cb_handle(args):
    request = args['request']
    request.addHttp({"form": cgi.FieldStorage()})
    urltrigger = request.getConfiguration().get('photo_trigger', 'photos')

    path = request.getHttp().get('PATH_INFO', '')
    path = [x for x in path.split('/') if x != '']

    if len(path) and path[0] == urltrigger:
        request.getData()['photo_gal_path'] = '/'.join(path[1:])
        handle_photo_request(request)
        return 1
    return None

def handle_photo_request(request):
    config = request.getConfiguration()
    data = request.getData()

    root_photodir = config.get('root_photodir', '').replace(os.sep, '/')
    if not root_photodir:
        raise "you need to specify py['root_photodir'] in your config.py"

    cur_photodir = '/'.join((root_photodir, data['photo_gal_path']))
    print "Content-type: text/html\n\n"
    if os.path.isfile(cur_photodir):
        render_medium(request, cur_photodir)
    elif os.path.isdir(cur_photodir):
        render_gallery(request, cur_photodir)
    else:
        print "file not found!"

def render_medium(request, photo):
    config = request.getConfiguration()
    root = config['root_photodir']
    base = config['photo_base']
    thumb_size = config['thumb_size']
    encoding = config['blog_encoding']
    template_dir = config['photo_template']
    
    p = PhotoEntry(photo, config)

    head = file(pathjoin(template_dir, 'head.html'), 'r').read()
    medium = file(pathjoin(template_dir, 'photo-medium.html'), 'r').read()
    foot = file(pathjoin(template_dir, 'foot.html'), 'r').read()

    print tools.parse(request, encoding, config, head)
    mdict = merge_dicts(p.getData(), config)
    print tools.parse(request, encoding, mdict, medium)
    print tools.parse(request, encoding, config, foot)

def render_gallery(request, pdir):
    #this should be moved into a filelist function? maybe?
    config = request.getConfiguration()
    http = request.getHttp()
    root = config['root_photodir']
    base = config['photo_base']
    thumb_size = config['thumb_size']
    photo_types = ['jpg', 'jpeg', 'gif', 'png']
    photos = []
    subdirs = []

    for f in os.listdir(pdir):
        path = pathjoin(pdir, f)
        if f.startswith('.'): continue
        if os.path.isdir(path):
            subdirs.append(DirEntry(path, root, base))
        else:
            for type in photo_types:
                if f.lower().endswith('.thumb.' + type):
                    break
                elif f.lower().endswith('.' + type):
                    photos.append(PhotoEntry(path, config))
    if subdirs: print_dirs(request, subdirs)
    else: print_gallery(request, photos)

def print_dirs(request, subdirs):
    config = request.getConfiguration()
    http = request.getHttp()
    encoding = config['blog_encoding']
    template_dir = config['photo_template']
    
    num_dirs = config['num_dirs']
    page = get_pagenum(http)
    start = num_dirs * page
    end = start + num_dirs
    filelist = subdirs[start:end]

    head = file(pathjoin(template_dir, 'head.html'), 'r').read()
    dirs = file(pathjoin(template_dir, 'photo-dirs.html'), 'r').read()
    foot = file(pathjoin(template_dir, 'foot.html'), 'r').read()

    config['next_page'] = config['prev_page'] = "%s%s" %\
        (config['base_url'], http['PATH_INFO'])
    if page-1 >= 0: pp = page - 1
    else: pp = page
    if end < len(subdirs): np = page + 1
    else: np = page
    config['prev_page'] = "%s?page=%s" % (config['prev_page'], pp)
    config['next_page'] = "%s?page=%s" % (config['next_page'], np)

    config['blog_title'] = "%s : %s" % (config['blog_title'], http['PATH_INFO'])

    print tools.parse(request, encoding, config, head)
    for d in subdirs:
        mdict = merge_dicts(d.getData(), config)
        print tools.parse(request, encoding, mdict, dirs)
    print tools.parse(request, encoding, config, foot)

def print_gallery(request, photos):
    config = request.getConfiguration()
    http = request.getHttp()
    encoding = config['blog_encoding']

    template_dir = config['photo_template']  #throws error if not set
    head = file(pathjoin(template_dir, 'head.html'), 'r').read()
    gallery = file(pathjoin(template_dir, 'photo-gallery.html'), 'r').read()
    foot = file(pathjoin(template_dir, 'foot.html'), 'r').read()

    num = get_numphotos(gallery)
    page = get_pagenum(http)
    start = num * page
    end = start + num
    filelist = photos[start:end]
    config['next_page'] = config['prev_page'] = "%s%s" %\
        (config['base_url'], http['PATH_INFO'])
    if page-1 >= 0: pp = page - 1
    else: pp = page
    if end < len(photos): np = page + 1
    else: np = page
    config['prev_page'] = "%s?page=%s" % (config['prev_page'], pp)
    config['next_page'] = "%s?page=%s" % (config['next_page'], np)

    config['blog_title'] = "%s : %s" % (config['blog_title'], http['PATH_INFO'])

    print tools.parse(request, encoding, config, head)
    print photo_parse(gallery, filelist, config, request, encoding)
    print tools.parse(request, encoding, config, foot)

def get_pagenum(http):
    if http['form'].has_key('page'):
        return int(http['form']['page'].value)
    return 0

def get_numphotos(text):
    #lines = text.split('\n')
    #for line in lines:
    #    if line.startswith('<!--numphotos:'):
    #        return int(line[14:line.find('-', 14)])
    return text.count('<photo>')

def merge_dicts(pmary, sdary):
    #merge two dicts, preferring the primary
    d = pmary
    for key in sdary:
        if not d.has_key(key): d[key] = sdary[key]
    return d

def photo_parse(gallery, photos, config, request, encoding):
    output = cStringIO.StringIO()
    gallery = gallery.split('<photo>')
    #first one can't be a photo
    output.write(tools.parse(request, encoding, config, gallery.pop(0)))
    for line in gallery:
        try: cur_photo = photos.pop(0)
        except IndexError:
            p = line.find('</photo>')
            line = line[p+8:]
            output.write(tools.parse(request, encoding, config, line))
        else:
            p = line.find('</photo>')
            line = line.replace('</photo>', '')
            mdict = merge_dicts(cur_photo.getData(), config)
            output.write(tools.parse(request, encoding, mdict, line[:p]))
            if p < len(line):
                output.write(tools.parse(request, encoding, config, line[p:]))
    return output.getvalue()
