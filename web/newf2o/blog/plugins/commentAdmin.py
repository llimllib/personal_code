import comments, os, cgi, time
from Pyblosxom import tools

ADMINPAGE = '/comments/admin'

def cb_handle(args):
    request = args['request']
    config = request.getConfiguration()
    path = request.getHttp().get('PATH_INFO', '')
    tools.log('%s' % (path))

    if path.lower() == ADMINPAGE:
        comments_admin(request)
        return 1
    return 0

def comments_admin(request):
    config = request.getConfiguration()
    http = request.getHttp()
    form = cgi.FieldStorage()
    page = '%s%s' % (config['base_url'], ADMINPAGE)

    print """Content-Type: text/html\n\n
    <html><head><title>pyblosxom comments-admin</title></head><body>\n"""
    comdir = config['comment_dir']
    ext = '.%s' % config['comment_ext']
    actions = {'delete': delete_comment, 'edit': edit_comment}
    found = 0
    for a in actions:
        if a in form and form[a].value.find(comdir) > -1:
            print '%s %s' % (a, form[a].value)
            page = '%s?%s' % (page, http['QUERY_STRING'])
            actions[a](config, form, page)
            found = 1
    if not found:
        print_all_comments(comdir, ext)
    print """</body></html>"""

def edit_comment(config, form, page):
    c = form['edit'].value
    cmt = comments.readComment(c)
    page, query = page.split('?', 1)
    if not 'pd' in form:
        print """<form method="POST" action="%s">
    <input type="hidden" name="edit" value="%s">
    <table><tr><td> 
    Title:</td><td><input type="text" name="title" value="%s" size="50"></td></tr><tr><td>
    Author:</td><td><input type="text" name="author" value="%s" size="50"></td></tr><tr><td>
    Link:</td><td><input type="text" name="link" value="%s" size="50"></td></tr><tr><td>
    Date:</td><td><input type="text" name="pubDate" value="%s" size="50"></td></tr><tr><td>
    Text:</td><td><textarea rows="10" cols="50" name="description">%s</textarea></td></tr><tr valign="bottom"><td>
    <br><b>Password</b>:</td><td><input type="password" name="pd"></td></tr><tr><td>
    <input type="submit" value="submit"></td></tr>
    </table>""" % (page, c, cmt['cmt_title'], cmt['cmt_author'], 
        cmt['cmt_link'], cmt['cmt_pubDate'], cmt['cmt_description'])
    elif form['pd'].value == config['cmt_password']:
        print "updating %s" % c
        cmt = make_comment(form)
        write_comment(file(c, 'w'), cmt, config)
    else:
        print "<br /><h2>invalid password</h2>"

def make_comment(form):
    """generate a comment dictionary from HTTP form values"""
    cmt = {}
    #these fields should all be present
    fields = ['author', 'title', 'link', 'pubDate', 'description']
    for f in fields:
        cmt[f] = form[f].value
    cmt['pubDate'] = str(time.mktime(time.strptime(cmt['pubDate'])))
    #wtf is the source field?
    cmt['source'] = ''
    print cmt
    return cmt

def write_comment(cfile, comment, config):
    def makeXMLField(name, field, encoding):
        return "<"+name+">"+cgi.escape(field[name])+"</"+name+">\n ";
    encoding = config.get('blog_encoding', 'iso-8859-1')
    cfile.write('<?xml version="1.0" encoding="%s"?>\n' % encoding)
    cfile.write("<item>\n")
    cfile.write(makeXMLField('title',comment))
    cfile.write(makeXMLField('author',comment))
    cfile.write(makeXMLField('link',comment))
    cfile.write(makeXMLField('source',comment))
    cfile.write(makeXMLField('pubDate',comment))
    cfile.write(makeXMLField('description',comment))
    cfile.write("</item>\n")
    cfile.close()

def delete_comment(config, form, page):
    c = form['delete'].value
    page, query = page.split('?', 1)
    if not 'pd' in form:
        print """<form method="POST" action="%s">
    <input type="hidden" name="delete" value="%s">
    password:<input type="password" name="pd">
    <input type="submit" value="submit">
    </form><p>You are trying to delete:<p>""" \
        % (page, c)
        cmt = comments.readComment(c)
        cmt['filename'] = c
        print_comment(cmt)
    elif form['pd'].value == config['cmt_password']:
        print "deleting %s" % c
        os.unlink(c)
    else:
        print "<br /><h2>invalid password</h2>"

def print_all_comments(comdir, ext):
    coms = []
    for root, dirs, files in os.walk(comdir):
        for f in files:
            if f.endswith(ext) and f != 'LATEST.cmt':
                fname = '%s/%s' % (root, f)
                c = comments.readComment(fname)
                c['filename'] = fname
                coms.append(c)
    print len(coms)
    try:
        coms = [(time.strptime(c['cmt_pubDate']), c) for c in coms] # DSU pattern
    except KeyError:
        print "<p><h3>comment %s has an error in it</h2>" % c['filename']
        return
    coms.sort()
    coms.reverse()
    coms = [c[1] for c in coms]
    print len(coms)
    for c in coms:
        print_comment(c)

def print_comment(c):
    #TODO: make this function suck less
    #XXX: what's the right way to do this? I really don't understand character
    #     encodings
    for key in c: c[key] = c[key].encode('iso-8859-1')
    print """<table border="1" cellspacing="5"><tr><td>
Title:</td><td>%s<br></td></tr><tr><td>
Author:</td><td>%s<br></td></tr><tr><td>
Link:</td><td>%s<br></td></tr><tr><td>
Date:</td><td>%s<br></td></tr><tr><td>
Text:</td><td>%s<br></td><td></tr><tr><td>
Filename:</td><td>%s<br></td></tr><tr><td>
<a href="%s?edit=%s">edit</a> <a href="%s?delete=%s">delete</a></td></tr>
</table>""" % (c['cmt_title'], c['cmt_author'], c['cmt_link'],
    c['cmt_pubDate'], c['cmt_description'], c['filename'], 'admin', 
    c['filename'], 'admin', c['filename'])
