import cherrypy as cpy
from hashlib import sha1
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

def config(key, default=None, path='/'):
    if cpy.config.get(path):
        return cpy.config[path].get(key, default)
    return default

def save_args(d, args):
    s = []
    for arg in args:
        if arg in d:
            s.append("%s=%s" % (arg, d[arg]))
    return "&".join(s)

def argstring(d, args, error):
    save = save_args(d, args)
    if save: return "error=%s&%s" % (error, save)
    return "error=%s" % error

def encrypt(_pass): return sha1(_pass).hexdigest()


def Session():
    #make an engine and an ORM session
    engine = create_engine('sqlite:///fm.db')
    return sessionmaker(bind=engine, autoflush=True, transactional=True)()
