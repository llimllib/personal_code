discs = {"top":      (50, 650, 146),
         "topright": (230, 560, 146),
         "right":    (345, 370, 146),
         "botright": (381, 140, 146),
         "botleft":  (360, 220, 146),
         "left":     (260, 25, 146),
         "topleft":  (80, 75, 146)}

from mako.template import Template

t = Template(filename='templates/styles.mak')
file("static/styles.css", 'w').write(t.render(discs=discs))
