#the first thing we need are lines with cirlce ends
def cline(x1,y1,x2,y2):
    push()
    w=4
    clr=color(0)
    nostroke()
    fill(0,0,0)
    oval(x1-w/2, y1-w/2, w, w)
    stroke(clr)
    strokewidth(w)
    line(x1,y1,x2,y2)
    nostroke()
    oval(x2-w/2, y2-w/2, w, w)
    pop()
    

cline(10,10,10,30)
cline(15,10,30,30)
cline(50,10,35,30)
cline(10,35,15,50)
cline(25,35,20,50)
cline(25,50,40,45)
cline(30,35,45,40)