from math import sqrt, asin, pi

def arrowline(x1, y1, x2, y2, double = False):
    #http://nodebox.net/code/index.php/shared_2008-04-12-15-52-56
    [x1, x2, y1, y2] = [float(x1), float(x2), float(y1), float(y2)]
    l = sqrt((x2-x1)**2 + (y2-y1)**2)
    if y2 > y1:
        q = asin((x2-x1)/l)+pi*3*0.5
    elif y2 < y1:
        q = -asin((x2-x1)/l)+pi*0.5      
    elif y2 == y1:
        q = 0
    push()
    
    translate(x1, y1)
    transform(CORNER)
    rotate(radians=q)
    
    #draw line
    #strokewidth(3)
    beginpath(0,0)
    lineto(l-10,0)
    endpath()
    #strokewidth(1)
    
    #draw head
    beginpath(l, 0)
    lineto(l-10, -5)
    lineto(l-10, 5)
    endpath()
    
    #double-headed arrow
    if double:
        beginpath(0, 0)
        lineto(0+10, -5)
        lineto(0+10, 5)
        endpath()    
    pop()

size(960, 200)
stroke(0)
fill(0)
strokewidth(1)
pt = (10,20)
font("Calibri")
text("rows", pt[0], pt[1]+70)
text("columns", pt[0]+80, pt[1])
translate(textwidth("rows")+pt[0], pt[1]-8)
arrowline(pt[0],pt[1],pt[0],pt[1]+100)
arrowline(pt[0],pt[1],pt[0]+150,pt[1])
#rect(pt[0]-1,pt[1]-1, 1,1) #shim the corner

translate(20, 45)
font("Calibri Italic")
text("red", 0,0)
text("green", 80,0)
text("blue", 0,30)
text("black", 80,30)
text("yellow", 0,60)
text("orange", 80,60)

strokewidth(5)
arrowline(200,30,250,30)

translate(280,20)
import colors
fill(colors.named_color("orange"))
rect(0,0,100,100)