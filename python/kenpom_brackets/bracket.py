import re

bracket = eval(file("teams.dat").read())

teamre = '\s*(\d+) <a href="[^"]+">([^<]+)</a>\s*<a href=.*?</a></a>\s*(\d+-\d+)\s*(\.\d+)\s*([\d.]*)\/(\d+)\s*([\d.]*)\/(\d+)'
kenpom = [x.groups() for x in [re.search(teamre, line) for line in file("kenpom.html")] if x]
#kenpom is a list of tuples:
#(rank, name, record, pythagorean score, adjusted O, adj O rank, adjusted D, adj D rank)
kenpom = dict((x[1], x) for x in kenpom)

#make sure we agree on all our team names
for r in bracket:
  for t in bracket[r].values():
    if type(t) == type([]):
      for t2 in t:
        assert(t2 in kenpom)
    else:
      assert(t in kenpom)

#let's build a dictionary {"region" -> {seed -> [name, pyth, adjo, adjd]}}
merged = {}
for region in ("Midwest", "West", "East", "South"):
  merged[region] = {}
  for seed, teamname in bracket[region].items():
    if type(teamname) == type([]):
      t1, t2 = teamname 
      rank, name, record, pyth, adjo, adjorank, adjd, adjdrank = kenpom[t1]
      merged[region][16] = [name, pyth, adjo, adjd]
      rank, name, record, pyth, adjo, adjorank, adjd, adjdrank = kenpom[t2]
      merged[region][17] = [name, pyth, adjo, adjd]
    else:
      rank, name, record, pyth, adjo, adjorank, adjd, adjdrank = kenpom[teamname]
      merged[region][seed] = [name, pyth, adjo, adjd]

#next task: find a simple way to print out a bracket:
#out = file("out.html", "w")
#out.write("""<html>
#<head><style>
#.top { border-style: none none solid none; }
#.bottom { border-style: none solid solid none; }
#.middle { border-style: none solid none none; padding: 0px 5px 0px 5px; }
#tr { padding-bottom: 10px; }
#</style>
#<body><table cellspacing=0>
#""")
#for region in ("Midwest", "West", "East", "South"):
#  out.write("<tr><td colspan=11><h1>%s</h1></td></tr>" % region)
#  for seed in (1,8,5,4,6,3,7,2):
#    t = bracket[region][seed]
#    oppseed = 17-seed
#    t2 = bracket[region][oppseed]
#    if type(t2) == type([]):
#      rank, name, record, pyth, adjo, adjorank, adjd, adjdrank = kenpom[t]
#      row(
#      out.write('<tr><td></td><td class="top">1 %s (%s, %s, %s)</td></tr>\n' % (t, pyth, adjo, adjd))
#      tx, ty = t2
#      rank, name, record, pyth, adjo, adjorank, adjd, adjdrank = kenpom[tx]
#      out.write('<tr><td class="top">%s (%s, %s, %s)</td><td class="bottom"></tr>\n' % (tx, pyth, adjo, adjd))
#      rank, name, record, pyth, adjo, adjorank, adjd, adjdrank = kenpom[ty]
#      out.write('<tr><td class="bottom">%s (%s, %s, %s)</td><td></td></tr>\n' % (ty, pyth, adjo, adjd))
#      out.write("</tr>\n")
#    else:
#      out.write("<tr><td></td>")
#      rank, name, record, pyth, adjo, adjorank, adjd, adjdrank = kenpom[t]
#      out.write('<td class="top">%s %s (%s, %s, %s)</td></tr>\n' % (seed, t, pyth, adjo, adjd))
#      rank, name, record, pyth, adjo, adjorank, adjd, adjdrank = kenpom[t2]
#      out.write('<tr><td></td><td class="bottom">%s %s (%s, %s, %s)</td></tr>\n' % (oppseed, t2, pyth, adjo, adjd))
#out.write("</table></html>")
#out.close()
