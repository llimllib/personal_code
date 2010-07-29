# -*- coding: utf-8 -*-
from os.path import dirname, join
from django.core.management.base import BaseCommand, CommandError
from papers.models import *
import xml.etree.cElementTree as ce

tags = {
    "article": Article,
    "inproceedings": InProceedings,
    "proceedings": Proceedings,
    "book": Book,
    "incollection": InCollection,
    "phdthesis": PhdThesis,
    "mastersthesis": MsThesis,
    "www": Website,
}

#we're not getting nearly enough... 1700 instead of over 2 million
#$ egrep '<incoll|<inprocee|<article|<proceed|<book>|<phdt|<masterst|<www' dblp.xml | wc
# 2135479 
f = "/Users/bill/code/cite/dblp.xml"

def insert():
    elements = 0
    fname = join(dirname(__file__), "small_dblp.xml")
    for event, elem in ce.iterparse(fname, events=("start", "end")):
        if event == "start" and elem.tag in tags:
            elements += 1
            m = tags[elem.tag]()
            for attr in elem.keys():
                setattr(m, attr, elem.get(attr))
            for child in elem.getchildren():
                setattr(m, child.tag, child.text) 
            m.save()

            if (elements % 100) == 0: print "%d elements" % elements

        #don't use the root.clear() idiom unless/until we need it
        elem.clear()

class Command(BaseCommand):
    args = ''
    help = 'insert papers from dblp into the db'

    def handle(self, *args, **options):
        insert()
