import graph
import colors
size(500,500)
g = graph.create(iterations=1000, distance=2,layout="spring", depth=True)

d = {"friendfeed":    ["twitter", "reddit", "tumblr", "blog", "news.yc", "flickr"],
     "twitter":       [],
     "flickr":        [],
     "news.yc":       [],
     "reddit":        [],
     "tumblr":        ["flickr"],
     "blog":          [],
     "stackoverflow": [],
     "facebook":      [],
     "gmail":         []}

for k,vs in d.iteritems():
    g.add_node(k)
    for v in vs:
        g.add_edge(k, v)

g.solve()
g.layout.tweak(k=8, m=0.01, w=10, d=0.5, r=15)
g.styles.fontsize = 20
g.styles.textwidth=200
g.styles.background=color(.5,0.4,0.6)
g.draw(directed=True)