class Spec(object):
    def __init__(self, size, timeLimit):
        self.s = """
    "size" : %s,
    "timeLimit" : %s,\n""" % (size, timeLimit)

    def __str__(self): return self.s

class Vehicle(object):
    def __init__(self, maxSpeed=20, accel=2, brake=3, turn=20,
                        hardTurn=60, frontView=60, rotAccel=120,
                        rearView=30):
        self.s = self.type() + """
        "maxSpeed" : %s,
        "accel" : %s,
        "brake" : %s,
        "turn" : %s,
        "hardTurn" : %s,
        "frontView" : %s,
        "rotAccel" : %s,
        "rearView" : %s
      },\n""" % (maxSpeed, accel, brake, turn,
                        hardTurn, frontView, rotAccel,
                        rearView)

    def __str__(self): return self.s

class Rover(Vehicle):
    def type(self): return '      "vehicleParams" : {'

class Martian(Vehicle):
    def type(self): return '      "martianParams" : {'

class Position(object):
    def __init__(self, x, y, dir, speed=None, view=None):
        if speed:
            head = "             {"
        else:
            head = '            "vehicle" : {'
        self.s = head + """
                    "x" : %s,
                    "y" : %s,
                    "dir" : %s""" % (x, y, dir)
        if speed:
            self.s += """,
                    "speed" : %s,
                    "view" : %s""" % (speed, view)
        self.s += "\n            }"

    def __str__(self): return self.s

class Enemies(object):
    def __init__(self, *positions):
        self.s = '            "enemies" : [\n'
        for enemy in positions[:-1]:
            self.s += str(enemy) + ",\n"
        for enemy in positions[-1:]:
            self.s += str(enemy) + "\n"
        self.s += '             ]\n'

    def __str__(self): return self.s

class Run(object):
    def __init__(self, vehicle, enemies):
        self.s = str(vehicle) + ",\n" + str(enemies)

    def __str__(self): return self.s

class Runs(object):
    def __init__(self, *runs):
        self.s = '      "runs" : [\n'
        for r in runs[:-1]:
            self.s += '     {\n'
            self.s += str(r)
            self.s += '     },\n'
        for r in runs[-1:]:
            self.s += '     {\n'
            self.s += str(runs[-1])
            self.s += '     }\n'
        self.s += '\n    ]'

    def __str__(self): return self.s

class Obstacle(object):
    def __init__(self, x, y, r):
        self.s = """
            "x" : %s,
            "y" : %s,
            "r" : %s
""" % (x,y,r)

    def __str__(self): return self.s

class Craters(object):
    def __init__(self, *obs):
        self.s = '      "craters" : [\n'
        for o in obs[:-1]:
            self.s += '     {'
            self.s += str(o)
            self.s += '     },\n'
        for o in obs[-1:]:
            self.s += '     {'
            self.s += str(obs[-1])
            self.s += '     }\n'
        self.s += '      ],\n'

    def __str__(self): return self.s

class Boulders(Obstacle):
    def __init__(self, *obs):
        self.s = '      "boulders" : [\n'
        for o in obs[:-1]:
            self.s += '     {\n'
            self.s += str(o)
            self.s += '     },\n'
        for o in obs[-1:]:
            self.s += '     {\n'
            self.s += str(obs[-1])
            self.s += '     }\n'
        self.s += '      ],\n'

    def __str__(self): return self.s

class Map(object):
    def __init__(self, spec, roverParams, martianParams,
                       craters, boulders, runs):
        self.s = '{%s%s%s%s%s%s\n}\n' % (spec, roverParams,
                                         martianParams, craters,
                                         boulders, runs)

    def __str__(self): return self.s

if __name__=="__main__":
    print Map(Spec(200, 30000), Rover(), Martian(), Craters(), Boulders(), 
        Runs(Run(Position(0,50,270), Enemies()),
             Run(Position(-25,25,-45), Enemies())))
