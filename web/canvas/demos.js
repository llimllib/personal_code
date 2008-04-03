demos = [
"//source derived from: \n\
//http://nodebox.net/code/index.php/Superfolia_|_root_source_code \n\
//caution! this may take a bit, just be patient \n\
\n\
function radial_gradient(colors, x, y, radius)\n\
{\n\
    //we'll use canvas' native createRadialGradient, but\n\
    //we could just as easily have used oval()\n\
    push();\n\
    g = ctx.createRadialGradient(x, y, 0, x, y, radius)\n\
    for (c=0; c < colors.length; c++)\n\
        g.addColorStop(c, colors[c])\n\
    ctx.fillStyle=g\n\
    rect(0,0,WIDTH,HEIGHT);\n\
    pop();\n\
}\n\
\n\
function root(x, y, angle, depth, alpha, decay) {\n\
    if (angle == undefined) angle=0\n\
    if (depth == undefined) depth=5\n\
    if (alpha == undefined) alpha=1\n\
    if (decay == undefined) decay=.005\n\
\n\
    // Recursive root branches to smaller roots.\n\
    var w = depth*6\n\
    for(i=0; i < depth * random(10,20); i++) {\n\
        var v = depth/5\n\
        alpha -= i*decay\n\
        alpha = Math.max(0, alpha)\n\
        \n\
        if (alpha > 0) {\n\
            // Next direction to grow in.,\n\
            // e.g. between -60 and 60 degrees of current heading.\n\
            angle += random(-60, 60)\n\
            var dx = x + Math.cos(radians(angle)) * w\n\
            var dy = y + Math.sin(radians(angle)) * w\n\
            \n\
            // Oval dropshadow.\n\
            nostroke()\n\
            fill(0, 0, 0, alpha*0.25)\n\
            oval(x-w/6+depth, y-w/6+depth, w/3, w/3)\n\
 \n\
            // Line segment to next position.\n\
            nofill()\n\
            stroke(0.8-v*0.25, 0.8, 0.8-v, alpha)\n\
            strokewidth((depth+1)*0.5)\n\
            line(x, y, dx, dy)\n\
            \n\
            // Colored oval.\n\
            strokewidth((depth+1)*0.25)\n\
            fill(0.8-v*0.25, 0.8, 0.8-v, alpha*.5)\n\
            oval(x-w/6, y-w/6, w/3, w/3)\n\
            \n\
            // Create a branching root.\n\
            if (random() > 0.85 && depth > 0)\n\
                root(x, y, angle, depth-1, alpha);\n\
            \n\
            x = dx\n\
            y = dy\n\
        }\n\
    }\n\
\n\
    // Continue growing at less alpha and depth.\n\
    if (depth > 0)\n\
        root(x, y, angle, depth-1, alpha)\n\
}\n\
 \n\
size(600, 600)\n\
radial_gradient([\"rgb(32, 38, 0)\", \"rgb(13, 15, 0)\"], 300, 300, 600)\n\
root(300, 300, angle=-90, depth=6)\n",
"//derived from: \n\
//http://nodebox.net/code/index.php/Fireworks\n\
\n\
size(800, 800)\n\
\n\
fill(0.2, 0.05, 0)\n\
rect(0, 0, WIDTH, HEIGHT)\n\
nofill()\n\
for (i=0; i < WIDTH/2; i++)\n\
{\n\
    r = i * 2\n\
    a = 1.0 * i / WIDTH/2\n\
    stroke(.99,1,0, 0.2 - a)\n\
    strokewidth(1)\n\
    oval(WIDTH/2-r*0.5, HEIGHT/2-r*0.5, r, r)\n\
}\n\
\n\
translate(WIDTH/2, HEIGHT/2)\n\
strokewidth(0.25)\n\
 \n\
n = 80\n\
for (i=0; i<n; i++)\n\
{\n\
  r = 50.0\n\
  dy = random(n)\n\
  d = 1.0*i/n  \n\
\n\
  for (j=0; j<i/2; j++)\n\
  {\n\
    nostroke()\n\
    fill(d*0.9, d*0.6, 1.0*j/i/2, 0.2*d)\n\
    stroke(d*0.9, d*0.8, 1.0*j/i/2, d*0.7)\n\
    push()\n\
    translate(i/0.75, dy)\n\
    rotate(10*j)\n\
    r2 = 4+ 10.0 * j/i\n\
    oval(0-r2*0.5+j*5, 0-r2*0.5, r2, r2)\n\
    line(j*5, 0, 0, 0)\n\
    pop()\n\
  }\n\
 \n\
  fill(d*0.9, d*0.6, 0, 1.0-d)\n\
  nostroke()\n\
  rotate(5)\n\
 \n\
  stroke(d*0.9, d*0.6, 0, 1.0-d)\n\
  nofill()\n\
  beginpath(0, 0)\n\
  curveto(0, 0, i, i, i/0.75, dy)\n\
  endpath()\n\
}\n",
"push();\n\
translate(30,30);\n\
rect(10,10,100,100);\n\
rotate(20);\n\
fill(0,255,0);\n\
rect(20,20,50,50);\n\
rotate(20);\n\
fill(255,0,255);\n\
rect(30,30,25,25);\n\
rotate(20);\n\
fill(0,255,255);\n\
rect(40,40,13,13);\n\
pop();\n\
\n\
fill(100,155,204);\n\
oval(100, 200, 100, 200);\n\
\n\
stroke(0,0,0);\n\
beginpath(150,200);\n\
lineto(200,300);\n\
lineto(150,400);\n\
lineto(100,300);\n\
endpath();\n\
\n\
beginpath(50,300);\n\
lineto(300,300);\n\
endpath();\n"
]
