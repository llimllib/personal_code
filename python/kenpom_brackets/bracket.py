from BeautifulSoup import BeautifulSoup
import re

bracket = eval(file("teams.dat").read())

teamre = '\s*(\d+) <a href="[^"]+">([^<]+)</a>\s*<a href=.*?</a></a>\s*(\d+-\d+)\s*(\.\d+)\s*([\d.]*)\/(\d+)\s*([\d.]*)\/(\d+)'
kenpom = [x.groups() for x in [re.search(teamre, line) for line in file("kenpom.html")] if x]
#kenpom is a list of tuples:
#(rank, name, record, pythagorean score, adjusted O, adj O rank, adjusted D, adj D rank)
kenpom = dict((x[1], x) for x in kenpom)

for r in bracket:
  for t in bracket[r].values():
    if type(t) == type([]):
      for t2 in t:
        if t2 not in kenpom: print t2
    elif t not in kenpom: print t
