#!/usr/bin/python

print "Content-Type: text/html\n\n"
#print "hellow"
import os, cgi, sys


print """<html><head><title>:: Error Log ::</title>
</head>
<body>"""

accountname = "llimllib" #your account name ex: if coolsite.f2o.org put coolsite

linestoprint = 25 #number of lines to print by default (prints the last 25)
                           #to print all the lines call errorlog.py?t
path = '/'.join(("/home/sites", accountname, "log/error_log"))
f = open(path, 'r')
lines = f.readlines()
f.close()

print "The ERROR_LOG file has", len(lines) ,"lines.<br><hr>"
print "<pre>"

if len(sys.argv) > 1 and sys.argv[1] == 't': #print all lines
    for line in lines:
        print cgi.escape(line),
else: #print just the last x lines as defined in linestoprint
    for line in lines[-linestoprint:]:
        print cgi.escape(line),

print "</pre>"
print "<br><hr>"
print """<center>***** End of List *****</center>
</body>
</html>"""
