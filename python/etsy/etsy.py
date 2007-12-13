#!/usr/bin/env python
"""
Code for etsy job application. Requires python 2.5 (or ElementTree
http://effbot.org/zone/element-index.htm)

Simply execute this file to count the number of users in each city and print 
out a running total.

Bill Mill
12/13/07
http://billmill.org
"""

from urllib import urlopen
from xml.etree.ElementTree import iterparse

ids = [42346, 77290, 729]
prefix = "http://api.etsy.com/feeds/xml_user_details.php?id="
docs = [urlopen(prefix + str(id)) for id in ids]

totals = {}
for doc in docs:
    for event, elem in iterparse(doc):
        if elem.tag == "city":
            totals[elem.text] = totals.get(elem.text, 0) + 1
            print totals
