import cherrypy as cpy
from hashlib import sha1, md5 #like to switch to sha?
from util import config, argstring, encrypt, Session
from Form import Form, PartialForm, TextField, SubmitField, HiddenField, TextArea
from db import Team

class Admin:
    _cp_config = {"tools.basic_auth.on": True,
        "tools.basic_auth.realm": "localhost",
        "tools.basic_auth.users": {config("admin_user"):
                                   md5(config("admin_pw")).hexdigest()},
        "tools.expires.on": True}

    def __init__(self): pass

    @cpy.expose
    def index(self, *args, **kwargs):
        return self.lsteam()

    _addteamform = Form(
        TextField(required=True, name="name", label="Name: "),
        TextField(name="thumb", label="Thumbnail: "),
        TextField(name="fullsize", label="Fullsize: "),
        TextField(required=True, name="password", label="Password: "),
        TextField(name="description", label="Description: "),
        SubmitField(value="Submit"),
        name="addteam",
        method="POST",
        action="/admin/addteam_",
        missing_message="Empty Field %s")

    @cpy.expose
    def addteam(self, **kwargs):
        __form = kwargs.get("__form", None)
        if __form: form = PartialForm(self._addteamform, Form.Decode(__form))
        else:      form = self._addteamform

        return ("addteam", {"form": form})

    @cpy.expose
    def addteam_(self, **kwargs):
        success, serial = self._addteamform.Validate(kwargs)
        if not success:
            raise cpy.HTTPRedirect("/admin/addteam?%s" % Form.Encode(serial))

        session = Session()
        team = Team(kwargs['name'], 
                    kwargs['thumb'],
                    kwargs['fullsize'],
                    encrypt(kwargs['password']),
                    1,
                    kwargs['description'])
        session.save(team)
        session.commit()
        session.close()

        raise cpy.HTTPRedirect("/admin/lsteam/")

    @cpy.expose
    def lsteam(self):
        session = Session()
        teams = session.query(Team).all()
        session.close()
        #TODO: move the admin templates into a subdir?
        return ("lsteam", {"teams": teams})

    _editteamform = Form(
        HiddenField(required=True, name="team_id"),
        TextField(required=True, name="name", label="Name: "),
        #TextField(name="thumb", label="Thumbnail: "),
        TextField(name="fullsize", label="Fullsize: "),
        TextField(name="password", label="Password: "),
        TextArea(required=True, name="description", label="Description: ", 
                 rows=10, cols=80),
        SubmitField(value="Submit"),
        name="editteam",
        method="POST",
        action="/admin/editteam_",
        missing_message="Empty Field %s")

    def getteam(self, name, default):
        session = Session()
        team = session.query(Team).filter_by(name=name).first() or default
        session.close()
        return team

    @cpy.expose
    def editteam(self, team, **kwargs):
        if not 'team_id' in kwargs:
            team = self.getteam(team, None)
            if not team: raise cpy.HTTPRedirect("/admin/lsteam/")
            team_info = { 'team_id':      (team.id, []),
                          'name':         (team.name, []),
                          'description':  (team.description or "", []),
                          'fullsize':     (team.fullsize or "", [])}
        else:
            team_info = kwargs

        __form = kwargs.get("__form", None)
        if __form: form = PartialForm(self._editteamform, Form.Decode(__form))
        else:      form = PartialForm(self._editteamform, team_info)

        return ("editteam", {"form": form, "team": team})

    @cpy.expose
    def editteam_(self, **kwargs):
        success, serial = self._editteamform.Validate(kwargs)
        if not success:
            raise cpy.HTTPRedirect("/admin/editteam?%s" % Form.Encode(serial))

        session = Session()
        #TODO: load the team, see what's different, and change those fields
        team = session.query(Team).filter_by(id=kwargs["team_id"]).first()
        if team.name != kwargs["name"]: 
            team.name = kwargs.name
        if team.description != kwargs["description"]: 
            team.description = kwargs["description"]
        if team.fullsize != kwargs["fullsize"]:
            team.fullsize = kwargs["fullsize"]
        if "password" in kwargs:
            team.password = encrypt(kwargs["password"])
        session.commit()
        session.close()
        raise cpy.HTTPRedirect("/admin/lsteam/")

    @cpy.expose
    def deleteteam(self, team):
        self.getteamid(team, "")
