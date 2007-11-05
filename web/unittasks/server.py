#!/usr/bin/env python
import os
import cherrypy as cpy

from datetime import datetime
from simplejson import dumps as json
from couchdb import Server
from couchdb.schema import Document, TextField, DateTimeField

class User(Document):
    username = TextField()
    password = TextField()
    type     = 'User'
    added    = DateTimeField(default=datetime.now)

class aTunes:
    @cpy.expose
    def index(self, *args, **kwargs):
        return file("static/test.html")

    @cpy.expose
    def adduser(self, user, pw):
        db = Server("http://localhost:8888/")['unit_tests']
        User(username=user, password=pw)

    @cpy.expose
    def isuserp(self, user, pw):
        db = Server("http://localhost:8888/")['unit_tests']
        if len(db.view('isuserp', key={"username": user, "password": pw})):
            return 1
        return 0

    @cpy.expose
    def newTaskAjax(self, taskName, frequency):
        return json({"taskName": taskName, "frequency": frequency})

def create_database():
    server = Server('http://localhost:8888/')
    db = server.create('unit_tasks')

    #XXX: figure out how to run python view functions?
    db.create({
        "id":    "_design/isuserp",
        "views": {
            "isuserp": """
                function(doc) {
                    if (doc.Type == 'User' &&
                        key.username == doc.username &&
                        key.password == doc.password)
                    {
                        map(null, doc);
                    }
                }"""
        }
    })

if __name__ == "__main__":
    if 'unit_tasks' not in Server('http://localhost:8888/'):
        create_database()
    pwd = os.path.dirname(os.path.abspath(__file__))
    conf = {'/static':    {'tools.staticdir.on': True,
                           'tools.staticdir.dir': os.path.join(pwd, "static")
                          },
           }
    cpy.quickstart(aTunes(), '/', config=conf)
