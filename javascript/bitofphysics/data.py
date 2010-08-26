[
{"name": "lastbounce",
"title": "Last Bounce",
"explain_before": """Storing dx and dy as values is a way of saying which way
we are going. This can be done in other ways. the changes here use lastx and
lasty to calculate dx and dy. lastx,lasty simply says where we were last time.
The essential difference with the two forms is that dx and dy says what
direction the ball is moving, whereas using lastx and lasty changes that to say
"move the ball in the same direction as last time".

<p>dx and dy can still be calculated as needed, but the important thing is that
dx and dy don't need to be stored. I bet you're wondering why you'd want to
change to this way. read on...""",
"code": """var lastx = x - dx;
var lasty = y - dy;

function draw() {
  clear();
  circle(x, y, 10);

  dx = x - lastx;
  dy = y - lasty;
  
  if (x + dx > WIDTH || x + dx < 0)
    dx = -dx;
  if (y + dy > HEIGHT || y + dy < 0)
    dy = -dy;

  lastx=x;
  lasty=y;
  x += dx;
  y += dy;
}

init();""",
"explain_after": """Try to change the draw() function so that the ball
accelerates or decelerates every time it hits a wall.""",
"library": """var x = 150;
var y = 150;
var dx = 2;
var dy = 4;
var ctx;
var WIDTH = $("#canvas").width()
var HEIGHT = $("#canvas").height()

function circle(x,y,r) {
  ctx.beginPath();
  ctx.arc(x, y, r, 0, Math.PI*2, true);
  ctx.closePath();
  ctx.fill();
}

function rect(x,y,w,h) {
  ctx.beginPath();
  ctx.rect(x,y,w,h);
  ctx.closePath();
  ctx.fill();
}

function clear() {
  ctx.clearRect(0, 0, WIDTH, HEIGHT);
}

function init() {
  ctx = $('#canvas')[0].getContext("2d");
  return setInterval(draw, 10);
} """
},
{"name": "gravity",
"title": "Gravity",
"explain_before": """Here's a small change to make a bouncing ball. When dy is
calculated we add a little bit extrea to it for gravity. If you let the ball
bounce for a while you'll see that the ball starts bouncing more and more. In
this example there is no friction or wind resistance. This allows tiny errors
to addd up over time to become significant effects""",
"code": """var GRAVITY = 0.2;

function draw() {
  clear();
  circle(x, y, 10);

  var dx = x - lastx;
  var dy = (y - lasty)+GRAVITY;
  
  if (x + dx > WIDTH || x + dx < 0)
    dx = -dx;
  if (y + dy > HEIGHT || y + dy < 0)
    dy = -dy;

  lastx=x;
  lasty=y;
  x += dx;
  y += dy;
}

init(); """,
"explain_after": """Add the lines dx*=0.999; and dy*=0.999; to see what
happens. This makes the x and y speed 99.9% of its previous value. That's like
a little bit of air resistance.""",
"library": """var x = 150;
var y = 150;
var lastx = x - 2;
var lasty = y - 4;

var ctx;
var WIDTH = $("#canvas").width()
var HEIGHT = $("#canvas").height()

function circle(x,y,r) {
  ctx.beginPath();
  ctx.arc(x, y, r, 0, Math.PI*2, true);
  ctx.closePath();
  ctx.fill();
}

function rect(x,y,w,h) {
  ctx.beginPath();
  ctx.rect(x,y,w,h);
  ctx.closePath();
  ctx.fill();
}

function clear() {
  ctx.clearRect(0, 0, WIDTH, HEIGHT);
}

function init() {
  ctx = $('#canvas')[0].getContext("2d");
  return setInterval(draw, 10);
} """
},
{"name": "moreballs",
"title": "More Balls",
"explain_before": """ If we are going to have more than one ball we'll need
more variables. We could do this by adding x2,y2 lastx2, lasty2 but if we
wanted to have more than a few balls, things would get very messy. a tidier way
would be to use an object to hold all the values for each ball. we can do this
with the line<br><code>var ball2 = new Object;</code>

<p>This code makes a new object called ball2 and gives it x ,y ,lastx and lasty
values. a cirlce is drawn at ball2.x, ball2.y but it doesn't move yet.""",
"code": """var ball2 = new Object;
ball2.x=100;
ball2.y=100;
ball2.lastx=100-2;
ball2.lasty=100-1;

function draw() {
  clear();
  circle(x, y, 10);
  
  circle(ball2.x,ball2.y,5);

  var dx = x - lastx;
  var dy = (y - lasty)+GRAVITY;
  
  if (x + dx > WIDTH || x + dx < 0)
    dx = -dx;
  if (y + dy > HEIGHT || y + dy < 0)
    dy = -dy;

  lastx=x;
  lasty=y;
  x += dx;
  y += dy;
}

init(); """,
"explain_after": """try changing x , y, lastx, lasty to use a ball1.x,
ball1.y, ball1.lastx, ball1.lasty""",
"library": """var x = 150;
var y = 150;
var lastx = x - 2;
var lasty = y - 4;


var ctx;
var WIDTH = $("#canvas").width()
var HEIGHT = $("#canvas").height()
var GRAVITY = 0.2;

function circle(x,y,r) {
  ctx.beginPath();
  ctx.arc(x, y, r, 0, Math.PI*2, true);
  ctx.closePath();
  ctx.fill();
}

function rect(x,y,w,h) {
  ctx.beginPath();
  ctx.rect(x,y,w,h);
  ctx.closePath();
  ctx.fill();
}

function clear() {
  ctx.clearRect(0, 0, WIDTH, HEIGHT);
}

function init() {
  ctx = $('#canvas')[0].getContext("2d");
  return setInterval(draw, 10);
}"""
},
{"name": "movefunction",
"title": "Ball Movement",
"explain_before": """ Because all of the values of ball2 were attached to a
single object, it is easy to refer to all of the values of the ball together.
The code below adds a new function called moveBall The code in the function is
exactly the same as the code for bouncing the ball from earlier, only it
applies to the x, y, lastx, and lasty values of whatever is passed to the
function as a parameter. If you call moveBall(ball1) it will apply to ball1, if
you call moveBall(ball3) it will apply to ball3.

<p>After ball movment has been moved to a seperate function you will see the
draw() function is quite tidy, it clears, moves 4 balls and draws 4 circles.
Nothing could be simpler. Until....  """,
"code": """var ball1 = new Object;
ball1.x=150;
ball1.y=150;
ball1.lastx=150-2;
ball1.lasty=150-4;

var ball2 = new Object;
ball2.x=100;
ball2.y=100;
ball2.lastx=100-2;
ball2.lasty=100-1;

var ball3 = new Object;
ball3.x=150;
ball3.y=100;
ball3.lastx=150+1;
ball3.lasty=100-3;

var ball4 = new Object;
ball4.x=100;
ball4.y=150;
ball4.lastx=100+2;
ball4.lasty=150+4;

function moveBall(ball) {
  var dx = ball.x - ball.lastx;
  var dy = (ball.y - ball.lasty)+GRAVITY;
  
  if (ball.x + dx > WIDTH || ball.x + dx < 0)
    dx = -dx;
  if (ball.y + dy > HEIGHT || ball.y + dy < 0)
    dy = -dy;

  ball.lastx=ball.x;
  ball.lasty=ball.y;
  ball.x += dx;
  ball.y += dy;
}

function draw() {
  clear();
  
  moveBall(ball1);
  moveBall(ball2);
  moveBall(ball3);
  moveBall(ball4);
  
  circle(ball1.x,ball1.y,5);
  circle(ball2.x,ball2.y,5);
  circle(ball3.x,ball3.y,5);
  circle(ball4.x,ball4.y,5);

}

init();""",
"explain_after": """
""",
"library": """var x = 150;
var y = 150;
var lastx = x - 2;
var lasty = y - 4;


var ctx;
var WIDTH = $("#canvas").width()
var HEIGHT = $("#canvas").height()
var GRAVITY = 0.2;

function circle(x,y,r) {
  ctx.beginPath();
  ctx.arc(x, y, r, 0, Math.PI*2, true);
  ctx.closePath();
  ctx.fill();
}

function rect(x,y,w,h) {
  ctx.beginPath();
  ctx.rect(x,y,w,h);
  ctx.closePath();
  ctx.fill();
}

function clear() {
  ctx.clearRect(0, 0, WIDTH, HEIGHT);
}

function init() {
  ctx = $('#canvas')[0].getContext("2d");
  return setInterval(draw, 10);
} """
},
{"name": "constraints",
"title": "Constraints",
"explain_before": """<p>The four balls in the previous example started in a
square, but they were all moving in different directions, so they seperated
quickly. What would happen if you bound them together so they couldn't leave
each other? We can try this by pushing and pulling the balls. If they get too
far apart then push them together. If they get too close, push them apart.

<p>For starters to do this, it would be useful to know how far apart they are and
how far apart you want them. The first part of this is easy if you listened at
school. Repeat after me... <em>"the square of the hypotenuse is equal to the sum of
the squares of the other two sides"</em>.

<p>better yet, how about

<p><code>distance * distance = Xdifference * Xdifference + Ydifference +
yDifference;</code>

<p>The second value, what distance should they be, is much easier. Whatever it was
when we started. The function to constrain the balls to a desired distance is
actually fairly simple. The balls move in opposite directions to each other
(either closer together of further apart). and each ball moves half the
distance of what is needed to do the correction.

<p>in the code below ball1 and ball2 are constrained to stay at a distance of
70.71, ball3 and ball4 are also constrained the same amount. We're also drawing
a line between the balls just to see where that constraint is.""",
"code": """function constrain(ballA,ballB,desiredDistance) {
  var dx = ballA.x-ballB.x;
  var dy = ballA.y-ballB.y;
  var distancesquared = dx*dx + dy*dy;
  var distance = Math.sqrt(distancesquared);
  var CorrectionRequired = desiredDistance-distance;
  
  dx = (dx / distance) * (CorrectionRequired / 2)
  dy = (dy / distance) * (CorrectionRequired / 2)
  

  shoveBall(ballA,dx,dy);
  shoveBall(ballB,-dx,-dy);

}

function shoveBall(ball,dx,dy) {
  if (ball.x + dx > WIDTH || ball.x + dx < 0)
    dx = -dx;
  if (ball.y + dy > HEIGHT || ball.y + dy < 0)
    dy = -dy;
  
  ball.x+=dx;
  ball.y+=dy;
}

function moveBall(ball) {
  var dx = ball.x - ball.lastx;
  var dy = (ball.y - ball.lasty)+GRAVITY;
  
  if (ball.x + dx > WIDTH || ball.x + dx < 0)
    dx = -dx;
  if (ball.y + dy > HEIGHT || ball.y + dy < 0)
    dy = -dy;

  ball.lastx=ball.x;
  ball.lasty=ball.y;
  ball.x += dx;
  ball.y += dy;
}

function draw() {
  clear();
  
  moveBall(ball1);
  moveBall(ball2);
  moveBall(ball3);
  moveBall(ball4);
  
  constrain(ball1,ball2, 70.71);
  line(ball1.x,ball1.y,ball2.x,ball2.y);

  constrain(ball3,ball4, 70.71);
  line(ball3.x,ball3.y,ball4.x,ball4.y);

  circle(ball1.x,ball1.y,5);
  circle(ball2.x,ball2.y,5);
  circle(ball3.x,ball3.y,5);
  circle(ball4.x,ball4.y,5);

}

init(); """,
"explain_after": """ It looks like a couple of sticks. What happens when you
constrain ball2 to ball3? It should link them all together like a chain. use a
constrain(ball2,ball3, 50); Don't forget to draw a line so you can see the
connection

<p>if you are wondering why the length is 50 instead of 70 for the other two, it
is because the first two constraints we made connected the diagonals of the
square. so they started 70.71 apart ball2 and ball3 start at a distance of 50
from each other.""",
"library": """var ball1 = new Object;
ball1.x=150;
ball1.y=150;
ball1.lastx=150-2;
ball1.lasty=150-4;

var ball2 = new Object;
ball2.x=100;
ball2.y=100;
ball2.lastx=100-2;
ball2.lasty=100-1;

var ball3 = new Object;
ball3.x=150;
ball3.y=100;
ball3.lastx=150+1;
ball3.lasty=100-3;

var ball4 = new Object;
ball4.x=100;
ball4.y=150;
ball4.lastx=100+2;
ball4.lasty=150+4;



var ctx;
var WIDTH = $("#canvas").width()
var HEIGHT = $("#canvas").height()
var GRAVITY = 0.1;

function circle(x,y,r) {
  ctx.beginPath();
  ctx.arc(x, y, r, 0, Math.PI*2, true);
  ctx.closePath();
  ctx.fill();
}

function line(x1,y1,x2,y2) {
  ctx.beginPath();
  ctx.moveTo(x1,y1);
  ctx.lineTo(x2,y2);
  ctx.stroke();
}

function rect(x,y,w,h) {
  ctx.beginPath();
  ctx.rect(x,y,w,h);
  ctx.closePath();
  ctx.fill();
}

function clear() {
  ctx.clearRect(0, 0, WIDTH, HEIGHT);
}

function init() {
  ctx = $('#canvas')[0].getContext("2d");
  return setInterval(draw, 10);
}"""
},
{"name": "box",
"title": "Box",
"explain_before": """This code has connectred all four balls up using six
constraints. Two diagonals and four around the border. If we removed the
diagonal constraints the edges would be all the right length but the shape
would fold up like a cardboard box.""",
"code": """function moveBall(ball) {
  var dx = ball.x - ball.lastx;
  var dy = (ball.y - ball.lasty)+GRAVITY;
  
  if (ball.x + dx > WIDTH || ball.x + dx < 0)
    dx = -dx;
  if (ball.y + dy > HEIGHT || ball.y + dy < 0)
    dy = -dy;

  ball.lastx=ball.x;
  ball.lasty=ball.y;
  ball.x += dx;
  ball.y += dy;
}


function draw() {
  clear();
  
  moveBall(ball1);
  moveBall(ball2);
  moveBall(ball3);
  moveBall(ball4);
  
  constrain(ball1,ball2, 70.71);
  line(ball1.x,ball1.y,ball2.x,ball2.y);

  constrain(ball3,ball4, 70.71);
  line(ball3.x,ball3.y,ball4.x,ball4.y);

  constrain(ball2,ball3, 50);
  line(ball2.x,ball2.y,ball3.x,ball3.y);

  constrain(ball1,ball3, 50);
  line(ball1.x,ball1.y,ball3.x,ball3.y);

  constrain(ball2,ball4, 50);
  line(ball2.x,ball2.y,ball4.x,ball4.y);

  constrain(ball1,ball4, 50);
  line(ball1.x,ball1.y,ball4.x,ball4.y);

  circle(ball1.x,ball1.y,5);
  circle(ball2.x,ball2.y,5);
  circle(ball3.x,ball3.y,5);
  circle(ball4.x,ball4.y,5);

}

init();""",
"explain_after": """ It's a box and it stays in shape, even while spinning, it
doesn't look quite right when it hits things though. Next up, we'll refine the
move function to refine the collision with the walls and floor.""",
"library": """var ball1 = new Object;
ball1.x=150;
ball1.y=150;
ball1.lastx=150-2;
ball1.lasty=150-4;

var ball2 = new Object;
ball2.x=100;
ball2.y=100;
ball2.lastx=100-2;
ball2.lasty=100-1;

var ball3 = new Object;
ball3.x=150;
ball3.y=100;
ball3.lastx=150+1;
ball3.lasty=100-3;

var ball4 = new Object;
ball4.x=100;
ball4.y=150;
ball4.lastx=100+2;
ball4.lasty=150+4;



var ctx;
var WIDTH = $("#canvas").width()
var HEIGHT = $("#canvas").height()
var GRAVITY = 0.1;

function constrain(ballA,ballB,desiredDistance) {
  var dx = ballA.x-ballB.x;
  var dy = ballA.y-ballB.y;
  var distancesquared = dx*dx + dy*dy;
  var distance = Math.sqrt(distancesquared);
  var CorrectionRequired = desiredDistance-distance;
  
  dx = (dx / distance) * (CorrectionRequired / 2)
  dy = (dy / distance) * (CorrectionRequired / 2)
  

  shoveBall(ballA,dx,dy);
  shoveBall(ballB,-dx,-dy);

}

function shoveBall(ball,dx,dy) {
  if (ball.x + dx > WIDTH || ball.x + dx < 0)
    dx = -dx;
  if (ball.y + dy > HEIGHT || ball.y + dy < 0)
    dy = -dy;
  
  ball.x+=dx;
  ball.y+=dy;
}

function circle(x,y,r) {
  ctx.beginPath();
  ctx.arc(x, y, r, 0, Math.PI*2, true);
  ctx.closePath();
  ctx.fill();
}

function line(x1,y1,x2,y2) {
  ctx.beginPath();
  ctx.moveTo(x1,y1);
  ctx.lineTo(x2,y2);
  ctx.stroke();
}

function rect(x,y,w,h) {
  ctx.beginPath();
  ctx.rect(x,y,w,h);
  ctx.closePath();
  ctx.fill();
}

function clear() {
  ctx.clearRect(0, 0, WIDTH, HEIGHT);
}

function init() {
  ctx = $('#canvas')[0].getContext("2d");
  return setInterval(draw, 10);
}"""
},
{"name": "betterbounce",
"title": "Better Bounce",
"explain_before": """ The bouncing used from the original example with a single
ball was quite simple. It checked to see if the ball would be out of bounds on
the next movement and if so, it would change direction and go the other way.
That's good enough for a simple object but to make the box bounce look more
realistic it really needs to be a bit better.

<p>Instead of just changing the direction, a more precise approach would be to
check how far into the edge the ball was going to penetrate and bounce back by
that amount. At the same time a minor tweak to the lastx/lasty can be done to
act as if the ball had come from the other side of the barrier.""",
"code": """function moveBall(ball) {
  var dx = ball.x - ball.lastx;
  var dy = (ball.y - ball.lasty)+GRAVITY;
  var penetration

  ball.lasty=ball.y;
  ball.lastx=ball.x;
  
  if (dy > 0) {
    penetration = (ball.y+dy) - HEIGHT;

    if (penetration > 0) {
      ball.lasty = HEIGHT + (HEIGHT-ball.lasty)      
      ball.y = HEIGHT-penetration;
      dx*=0.9;
    } else ball.y += dy;
  
      
  } else {
    penetration = 0 - (ball.y+dy);

    if (penetration > 0) {
      ball.lasty = 0 + (0-ball.lasty)      
      ball.y = 0+penetration;
      dx*=0.9;
    } else ball.y += dy;

  }

  if (dx > 0) {
    penetration = (ball.x+dx) - WIDTH;

    if (penetration > 0) {
      ball.lastx = WIDTH + (WIDTH-ball.lastx)      
      ball.x = WIDTH-penetration;
    } else ball.x += dx;
  
      
  } else {
    penetration = 0 - (ball.x+dx);

    if (penetration > 0) {
      ball.lastx = 0 + (0-ball.lastx)      
      ball.x = 0+penetration;
    } else ball.x += dx;

  }
}

function draw() {
  clear();
  
  moveBall(ball1);
  moveBall(ball2);
  moveBall(ball3);
  moveBall(ball4);
  
  constrain(ball1,ball2, 70.71);
  line(ball1.x,ball1.y,ball2.x,ball2.y);

  constrain(ball3,ball4, 70.71);
  line(ball3.x,ball3.y,ball4.x,ball4.y);

  constrain(ball2,ball3, 50);
  line(ball2.x,ball2.y,ball3.x,ball3.y);

  constrain(ball1,ball3, 50);
  line(ball1.x,ball1.y,ball3.x,ball3.y);

  constrain(ball2,ball4, 50);
  line(ball2.x,ball2.y,ball4.x,ball4.y);

  constrain(ball1,ball4, 50);
  line(ball1.x,ball1.y,ball4.x,ball4.y);

  circle(ball1.x,ball1.y,5);
  circle(ball2.x,ball2.y,5);
  circle(ball3.x,ball3.y,5);
  circle(ball4.x,ball4.y,5);

}

init();""",
"explain_after": """the new move function initially appears much more complex,
but most of that is because there are four seperate checks instead of two
previously. calculating the edge pentration needs to be done for all four sides
seperately. Up until now the code had been using only one check for top/bottom
and one for left/right.

<p>In addition to the new bouncing, there is one other little tweak. When a
collision occurs against the wall or ceiling there is dx*=1/(1+penetration);.
That is a little bit of a friction hack. x motion slows a bit when it hits the
floor or ceiling. Most collisions are with the floor when you have gravity,
this stops them sliding all over the place. calculation of 1/(1+penetration)
equals 1 for no penetration and decreases as the penetration increases.""",
"library": """var ball1 = new Object;
ball1.x=150;
ball1.y=150;
ball1.lastx=150+2;
ball1.lasty=150+4;

var ball2 = new Object;
ball2.x=100;
ball2.y=100;
ball2.lastx=100+2;
ball2.lasty=100+1;

var ball3 = new Object;
ball3.x=150;
ball3.y=100;
ball3.lastx=150+1;
ball3.lasty=100+3;

var ball4 = new Object;
ball4.x=100;
ball4.y=150;
ball4.lastx=100+2;
ball4.lasty=150+4;



var ctx;
var WIDTH = $("#canvas").width()
var HEIGHT = $("#canvas").height()
var GRAVITY = 0.05

function constrain(ballA,ballB,desiredDistance) {
  var dx = ballA.x-ballB.x;
  var dy = ballA.y-ballB.y;
  var distancesquared = dx*dx + dy*dy;
  var distance = Math.sqrt(distancesquared);
  var CorrectionRequired = desiredDistance-distance;
  
  dx = (dx / distance) * (CorrectionRequired / 2)
  dy = (dy / distance) * (CorrectionRequired / 2)
  

  shoveBall(ballA,dx,dy);
  shoveBall(ballB,-dx,-dy);

}

function shoveBall(ball,dx,dy) {
  if (ball.x + dx > WIDTH || ball.x + dx < 0)
    dx = -dx;
  if (ball.y + dy > HEIGHT || ball.y + dy < 0)
    dy = -dy;
  
  ball.x+=dx;
  ball.y+=dy;
}

function circle(x,y,r) {
  ctx.beginPath();
  ctx.arc(x, y, r, 0, Math.PI*2, true);
  ctx.closePath();
  ctx.fill();
}

function line(x1,y1,x2,y2) {
  ctx.beginPath();
  ctx.moveTo(x1,y1);
  ctx.lineTo(x2,y2);
  ctx.stroke();
}

function rect(x,y,w,h) {
  ctx.beginPath();
  ctx.rect(x,y,w,h);
  ctx.closePath();
  ctx.fill();
}

function clear() {
  ctx.clearRect(0, 0, WIDTH, HEIGHT);
}

function init() {
  ctx = $('#canvas')[0].getContext("2d");
  return setInterval(draw, 10);
}"""
},
{"name": "justthebox",
"title": "Finishing Touches",
"explain_before": """Finally, a little bit of tidyup. The circles and diagonal
lines have been removed. This leaves us with just the box outline. In addition
the motion of the box had been randomised at the start so that you can keep
clicking 'run code' and it'll show you a different bounce each time.""",
"code": """function draw() {
  clear();
  
  moveBall(ball1);
  moveBall(ball2);
  moveBall(ball3);
  moveBall(ball4);
  
  constrain(ball1,ball2, 70.71);

  constrain(ball3,ball4, 70.71);

  constrain(ball2,ball3, 50);
  line(ball2.x,ball2.y,ball3.x,ball3.y);

  constrain(ball1,ball3, 50);
  line(ball1.x,ball1.y,ball3.x,ball3.y);

  constrain(ball2,ball4, 50);
  line(ball2.x,ball2.y,ball4.x,ball4.y);

  constrain(ball1,ball4, 50);
  line(ball1.x,ball1.y,ball4.x,ball4.y);
}

init();""",
"explain_after": """ Of course there is more to a physics engine than this.
this code gives you one object. If you had two boxes they would not interact
with each other. In addition to this, many physics engines have data stuctures
to aid in eliminating as many collision checks as possible, Most such
optimisations are not part of the physical behaviour itself, they are just
ways to make it run faster.

Nevertheless the fundimentals of the system are here. Adding object interaction
is just a matter of detecting the points that were going to cause a collision
and moving them accordingly. both the detection and the response are more
complicated than a flat wall or floor, but ultimately it comes down to shoving
a few points around. Bouncing and spinning are just natural byproducts of the
shoving.""",
"library": """var ball1 = new Object;
ball1.x=150;
ball1.y=150;
ball1.lastx=150+Math.random()*4.0-2;
ball1.lasty=150+Math.random()*8.0;

var ball2 = new Object;
ball2.x=100;
ball2.y=100;
ball2.lastx=100+Math.random()*4.0-2;
ball2.lasty=100+Math.random()*8.0;

var ball3 = new Object;
ball3.x=150;
ball3.y=100;
ball3.lastx=150+Math.random()*4.0-2;
ball3.lasty=100+Math.random()*8.0;

var ball4 = new Object;
ball4.x=100;
ball4.y=150;
ball4.lastx=100+Math.random()*4.0-2;
ball4.lasty=150+Math.random()*8.0;
ball4.lasty=150+4;



var ctx;
var WIDTH = $("#canvas").width()
var HEIGHT = $("#canvas").height()
var GRAVITY = 0.05

function constrain(ballA,ballB,desiredDistance) {
  var dx = ballA.x-ballB.x;
  var dy = ballA.y-ballB.y;
  var distancesquared = dx*dx + dy*dy;
  var distance = Math.sqrt(distancesquared);
  var CorrectionRequired = desiredDistance-distance;
  
  dx = (dx / distance) * (CorrectionRequired / 2)
  dy = (dy / distance) * (CorrectionRequired / 2)
  

  shoveBall(ballA,dx,dy);
  shoveBall(ballB,-dx,-dy);

}

function shoveBall(ball,dx,dy) {
  if (ball.x + dx > WIDTH || ball.x + dx < 0)
    dx = -dx;
  if (ball.y + dy > HEIGHT || ball.y + dy < 0)
    dy = -dy;
  
  ball.x+=dx;
  ball.y+=dy;
}

function moveBall(ball) {
  var dx = ball.x - ball.lastx;
  var dy = (ball.y - ball.lasty)+GRAVITY;
  var penetration

  ball.lasty=ball.y;
  ball.lastx=ball.x;
  
  if (dy > 0) {
    penetration = (ball.y+dy) - HEIGHT;

    if (penetration > 0) {
      ball.lasty = HEIGHT + (HEIGHT-ball.lasty)      
      ball.y = HEIGHT-penetration;
      dx*=0.9;
    } else ball.y += dy;
  
      
  } else {
    penetration = 0 - (ball.y+dy);

    if (penetration > 0) {
      ball.lasty = 0 + (0-ball.lasty)      
      ball.y = 0+penetration;
      dx*=0.9;
    } else ball.y += dy;

  }

  if (dx > 0) {
    penetration = (ball.x+dx) - WIDTH;

    if (penetration > 0) {
      ball.lastx = WIDTH + (WIDTH-ball.lastx)      
      ball.x = WIDTH-penetration;
    } else ball.x += dx;
  
      
  } else {
    penetration = 0 - (ball.x+dx);

    if (penetration > 0) {
      ball.lastx = 0 + (0-ball.lastx)      
      ball.x = 0+penetration;
    } else ball.x += dx;

  }
}


function circle(x,y,r) {
  ctx.beginPath();
  ctx.arc(x, y, r, 0, Math.PI*2, true);
  ctx.closePath();
  ctx.fill();
}

function line(x1,y1,x2,y2) {
  ctx.beginPath();
  ctx.moveTo(x1,y1);
  ctx.lineTo(x2,y2);
  ctx.stroke();
}

function rect(x,y,w,h) {
  ctx.beginPath();
  ctx.rect(x,y,w,h);
  ctx.closePath();
  ctx.fill();
}

function clear() {
  ctx.clearRect(0, 0, WIDTH, HEIGHT);
}

function init() {
  ctx = $('#canvas')[0].getContext("2d");
  return setInterval(draw, 10);
}"""
},
]
