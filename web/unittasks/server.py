#!/usr/bin/env python
import os
import cherrypy as cpy

from md5 import md5
from datetime import datetime
from couchdb import Server, ResourceConflict
from couchdb.schema import Document, TextField, DateTimeField

class User(Document):
    _id      = TextField()
    password = TextField()
    type     = TextField(default='User')
    added    = DateTimeField(default=datetime.now)

class Unittask:
    @cpy.expose
    def index(self, *args, **kwargs):
        return file("static/test.html")

    def hash(self, password):
        return md5(password).hexdigest()

    @cpy.expose
    def adduser(self, user, pw):
        db = Server("http://localhost:8888/")['unit_tasks']
        try:
            User(_id=user, password=self.hash(pw)).store(db)
            return "added user"
        except ResourceConflict:
            return "user already exists"

    @cpy.expose
    def isuserp(self, user, pw):
        db = Server("http://localhost:8888/")['unit_tasks']
        if len(list(db.view('_design/users/isvalid', 
                        key=[user, self.hash(pw)]))) == 1:
            return "Success!"
        return "Failure :("

    @cpy.expose
    def newTaskAjax(self, taskName, frequency):
        return json({"taskName": taskName, "frequency": frequency})

def create_database():
    server = Server('http://localhost:8888/')
    db = server.create('unit_tasks')

    #XXX: figure out how to run python view functions?
    db.create({
        "_id":    "_design/users",
        "views": {
            "isvalid": """
                function(doc) { 
                    if (doc.type == 'User')
                        map([doc._id, doc.password], doc); 
                }"""
        }
    })

    User(_id="llimllib", password=md5("tao").hexdigest()).store(db)

def debug():
    #just a place for putting handy stuff to run
    db = Server('http://localhost:8888/')['unit_tasks']

    f = db["_design/users"]
    f["views"] = {
            "isvalid": """
                function(doc) {
                    if (doc.type == 'User')
                        map([doc._id, doc.password], doc);
                }"""
        }
    db["_design/users"] = f

    #User(_id="llimllib", password=md5("tao").hexdigest()).store(db)

if __name__ == "__main__":
    debug()
    if 'unit_tasks' not in Server('http://localhost:8888/'):
        create_database()
    pwd = os.path.dirname(os.path.abspath(__file__))
    conf = {'/static':    {'tools.staticdir.on': True,
                           'tools.staticdir.dir': os.path.join(pwd, "static")
                          },
           }
    cpy.quickstart(Unittask(), '/', config=conf)
