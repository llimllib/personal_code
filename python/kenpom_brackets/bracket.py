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

class Game(object):
  def __init__(self, region, round, gameno, seed1=None, team1=None, seed2=None, team2=None):
    self.region = region
    self.round = round
    self.gameno = gameno
    self.seed1 = seed1 or ""
    self.team1 = team1 or [""]
    self.seed2 = seed2 or ""
    self.team2 = team2 or [""]
    self.child = None
    self.rows = [None, None]

  def addchild(self, game):
    self.child = game

  def __repr__(self):
    return "%s vs %s round %s game %s region %s rows %s" % (self.team1[0],
      self.team2[0], self.round, self.gameno, self.region, self.rows)

games = []
game = 1
for region in merged:
  for seed in (1,8,5,4,6,3,7,2):
    oppseed = 17-seed
    t1 = merged[region][seed]
    t2 = merged[region][oppseed]
    games.append(Game(region, 1, game, seed, t1, oppseed, t2))
    game += 1

round = 2
i = iter(games)
roundg = []
for g in i:
  g2 = i.next()
  gg = Game(g.region, 2, game)
  roundg.append(gg)
  g.child = g2.child = gg
  game += 1
games.extend(roundg)

class Table(object):
  def __init__(self):
    self.data = [[]]

  def __getitem__(self, (x,y)):
    return self.data[x][y]

  def __setitem__(self, (x,y), val):
    try:
      self.data[x][y] = val
    except IndexError:
      self.resize(x, y)
      self.data[y][x] = val

  #we're fine with *horrible* efficiency in this particular app
  def resize(self, x, y):
    for i in range(y - len(self.data) + 1):
      self.data.append([])
    for row in self.data:
      for i in range(x - len(row) + 1):
        row.append(None)

  def __repr__(self):
    return self.data.__repr__()

  def tableize(self):
    o = []
    for y, row in enumerate(self.data):
      o.append("<tr>")
      for x, elt in enumerate(row):
        if not elt: o.append('<td class="none"></td>')
        elif elt.rows[0] == y: 
          if x in (0,1):
            o.append('''<td class="top">%s. <a href="#"
                            class="round-%s game-%s"
                            >%s (%s, %s, %s)</a></td>''' % (
              elt.seed1, elt.round, elt.gameno, elt.team1[0], elt.team1[1],
              elt.team1[2], elt.team1[3]))
          else:
            o.append('''<td class="top">&nbsp;<a href="#"
                            class="round=%s game-%s"></a>''' % (
              elt.round, elt.gameno))
        elif elt.rows[1] == y:
          if x in (0,1):
            o.append('''<td class="bottom">%s. <a href="#"
                            class="round-%s game-%s"
                            >%s (%s, %s, %s)</a></td>''' % (
              elt.seed2, elt.round, elt.gameno, elt.team2[0], elt.team2[1],
              elt.team2[2], elt.team2[3]))
          else:
            o.append('''<td class="bottom">&nbsp;<a href="#"
                            class="round=%s game-%s"></a>''' % (
              elt.round, elt.gameno))
        else:
          o.append('<td class="middle">&nbsp;</td>')
    return '\n'.join(o)
    
t = Table()
row = 0
MIDDLE = "middle"
BOTTOM = "bottom"
for g in [x for x in games if x.round==1 and x.region=="Midwest"]:
  g.rows = [row, row+2]
  t[1, row] = g
  t[1, row+1] = g
  t[1, row+2] = g
  g.child.rows[1 if g.child.rows[0] else 0] = row+1
  row += 3

for g in [x for x in games if x.round==2 and x.region=="Midwest"]:
  for i in range(0, g.rows[1] - g.rows[0] + 1):
    t[2, g.rows[0] + i] = g

out = file("out.html", "w")
out.write("""
<html><head>
<script src="jquery-1.3.2.min.js" type="text/javascript"></script>
<script>
rounds = {0:0, 1: 32, 2: 48, 3:56, 4:60, 5:62, 6:63};

function handleClick(that) {
  var atts = that.attr("class");
  var game = parseFloat(atts.match(/game-(\d+)/)[1]);
  var round = parseFloat(atts.match(/round-(\d+)/)[1]);
  var game_parity = (game - rounds[round-1]) % 2 == 0 ? 1 : 0;
  var nextgame = (Math.ceil((game-rounds[round-1])/2) + rounds[round]).toString();
  var firstorlast = game_parity ? ":last" : ":first";
  that.click(function() {
    console.log("game, round, nextgame, (g-r[r-1]) ", game, round, nextgame, game - rounds[round-1], firstorlast);
    $(".game-" + nextgame + firstorlast).html(that.html());
  });
}

$(document).ready(function() {
  $(".round-1").each(function(i) { handleClick($(this)); });
  $(".round-2").each(function(i) { handleClick($(this)); });
});
</script>
<style>
.top { border-style: none none solid none; padding: 0px 5px 0px 5px; }
.bottom { border-style: none solid solid none; padding: 0px 5px 0px 5px; }
.middle { border-style: none solid none none; padding: 0px 5px 0px 5px; }
tr { padding-bottom: 10px; }
</style>
</head>
<body><table cellspacing=0>
""")
out.write(t.tableize())
out.write("</table></body></html>")
out.close()
