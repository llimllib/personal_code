from sqlalchemy import Table, Column, Integer, String, MetaData, ForeignKey,\
                       create_engine
from sqlalchemy.orm import mapper

engine = create_engine('sqlite:///fm.db')

metadata = MetaData()
team_table = Table('team', metadata,
    Column('id', Integer, primary_key=True),
    Column('name', String(255)),
    Column('thumb', String(255)),
    Column('fullsize', String(255)),
    Column('password', String(255)),
    Column('active', Integer),
    Column('description', String(4096))
)
metadata.create_all(engine)

class Team(object):
    def __init__(self, name, thumb, fullsize, password, active, description):
        self.name = name
        self.thumb = thumb
        self.fullsize = fullsize
        self.password = password
        self.active = active
        self.description = description

    def __str__(self): return "<Team %s>" % self.name
mapper(Team, team_table)
