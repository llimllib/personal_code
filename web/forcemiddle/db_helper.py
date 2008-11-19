from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

engine = create_engine('sqlite:///fm.db')
sessionType = sessionmaker(bind=engine, autoflush=True, transactional=True)
session = sessionType()
