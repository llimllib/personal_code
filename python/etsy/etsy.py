from urllib import urlopen
from xml.etree.ElementTree import XML, iterparse

ids = [42346, 77290, 729]
prefix = "http://api.etsy.com/feeds/xml_user_details.php?id="
docs = [urlopen(prefix + str(id)) for id in ids]

totals = {}
for doc in docs:
    for event, elem in iterparse(doc):
        if elem.tag == "city":
            totals[elem.text] = totals.get(elem.text, 0) + 1
            print "found " + elem.tag
            print totals
