#!/usr/bin/env python
import os
import cherrypy as cpy
import mako.template
import simplejson
import sqlite3
from buffet import BuffetTool
from util import config, Session
from db import Team

class Forcemiddle:
    _cp_config = {"tools.buffet.on": True,
                  "tools.staticdir.root": os.path.abspath(os.curdir),
                  "tools.encode.on": True,
                  "tools.encode.encoding": "utf-8"}

    def __init__(self):
        cpy.tools.buffet = BuffetTool(config("template_engine"))
        #set the output encoding
        self._cp_config["cpy.tools.encode.encoding"] = "utf-8"

        #make our stylesheet {divname: (top, left, size)}
        self.discs = {"top":      (50, 650, 146),
                      "topright": (230, 560, 146),
                      "right":    (345, 370, 146),
                      "botright": (381, 140, 146),
                      "botleft":  (230, 0, 146),
                      "left":     (260, 25, 146),
                      "topleft":  (80, 75, 146)}
        styletemp = mako.template.Template(filename="templates/styles.mak")
        fout = file("static/styles.css", "w")\
                .write(styletemp.render(discs=self.discs))

        #load the admin page. We have to import Admin after the config has
        #loaded or else it borks
        from admin import Admin
        self.admin = Admin()

        from team import TeamAdmin
        self.team_admin = TeamAdmin()

        from cherryblossom import BlogRoot
        self.blog = BlogRoot('/blog')

    @cpy.expose
    def index(self, *args, **kwargs):
        discstr = simplejson.dumps(self.discs)
        return ('index', {'title': 'Forcemiddle.com', 'discs': discstr})

    @cpy.expose
    def About(self, *args, **kwargs):
        return ('about', {'title': 'Forcemiddle.com'})

    @cpy.expose
    def default(self, team):
        s = Session()
        our_teams = dict((t.name, t) for t in s.query(Team).all())
        s.close()

        if team in our_teams:
            team = our_teams[team]
            return ("teampage", {'team': team})

        return "Invalid page"

if __name__ == "__main__":
    #unfortunately, we need to do this twice
    cpy.config.update('forcemiddle.conf')
    cpy.config.update('site.conf')
    cpy.quickstart(Forcemiddle(), '/', config='forcemiddle.conf')
