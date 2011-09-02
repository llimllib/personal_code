simplejson = ximport("simplejson")
colors = ximport("colors")

hoods = simplejson.load(file("hoods.json"))
h = (hoods.keys()[-1], hoods[hoods.keys()[-1]])
name = h[0]
n = h[1][0]
a = h[1][1]

background(None)

d = 100
margin = 2
w = h = d + (margin*2)
mid = w/2
size(w,w)

fill(colors.hex("#608902d"))
oval(2,2, d,d)

fill(colors.hex("#E6FA87"))
n = str(n)
tw = textwidth(n)
th = textheight(n)
text(n, mid-(tw/2), mid+(th/4))
print n, tw, th
print fontsize()