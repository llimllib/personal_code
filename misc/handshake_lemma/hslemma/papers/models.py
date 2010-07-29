from django.db.models import Model, CharField, DateTimeField, ForeignKey, IntegerField

class AuthoredThingFields(Model):
    author = CharField(max_length=2048, null=True)
    editor = CharField(max_length=2048, null=True)
    title = CharField(max_length=2048, null=True)
    booktitle = CharField(max_length=2048, null=True)
    pages = CharField(max_length=2048, null=True)
    year = CharField(max_length=2048, null=True)
    address = CharField(max_length=2048, null=True)
    journal = CharField(max_length=2048, null=True)
    volume = CharField(max_length=2048, null=True)
    number = CharField(max_length=2048, null=True)
    month = CharField(max_length=2048, null=True)
    url = CharField(max_length=2048, null=True)
    ee = CharField(max_length=2048, null=True)
    cdrom = CharField(max_length=2048, null=True)
    cite = CharField(max_length=2048, null=True)
    publisher = CharField(max_length=2048, null=True)
    note = CharField(max_length=2048, null=True)
    crossref = CharField(max_length=2048, null=True)
    isbn = CharField(max_length=2048, null=True)
    series = CharField(max_length=2048, null=True)
    school = CharField(max_length=2048, null=True)
    chapter = CharField(max_length=2048, null=True)

    class Meta:
        abstract = True

class Article(AuthoredThingFields):
    key = CharField(max_length=1024)
    reviewid = CharField(max_length=1024)
    rating = CharField(max_length=1024) #POS, SUPERB, or NEUTRAL. WTF?
    mdate = DateTimeField()

class InProceedings(AuthoredThingFields):
    key = CharField(max_length=1024)
    mdate = DateTimeField()

class Proceedings(AuthoredThingFields):
    key = CharField(max_length=1024)
    mdate = DateTimeField()

class Book(AuthoredThingFields):
    key = CharField(max_length=1024)
    mdate = DateTimeField()

class InCollection(AuthoredThingFields):
    key = CharField(max_length=1024)
    mdate = DateTimeField()

class PhdThesis(AuthoredThingFields):
    key = CharField(max_length=1024)
    mdate = DateTimeField()

class MsThesis(AuthoredThingFields):
    key = CharField(max_length=1024)
    mdate = DateTimeField()

class Website(AuthoredThingFields):
    key = CharField(max_length=1024)
    mdate = DateTimeField()
