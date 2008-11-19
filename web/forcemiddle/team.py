import cherrypy as cpy
from hashlib import sha1
from util import config, argstring, encrypt, Session
from Form import Form, PartialForm, TextField, SubmitField, HiddenField
from db import Team
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

def auth():
    session = Session()
    users = dict((str(t.name), str(t.password)) for t in session.query(Team).all())
    session.close()
    return users

class TeamAdmin:
    _cp_config = {"tools.basic_auth.on": True,
        "tools.basic_auth.realm": "localhost",
        "tools.basic_auth.users": auth,
        "tools.basic_auth.encrypt": encrypt,
        "tools.expires.on": True}

    @cpy.expose
    def index(self):
        return "welcome!"
