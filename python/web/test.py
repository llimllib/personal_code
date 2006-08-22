#!/usr/bin/python
import cgi

print "Content-Type: text/html\n\n"
print "<html><body>"
cgi.print_environ()
print "<h1>Hello World</h1>"
print "</html></body>" 
