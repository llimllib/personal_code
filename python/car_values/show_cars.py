import cherrypy as cpy
from buffet import TemplateFilter
from make_codes import make_codes

class CarPrices:
    _cpFilterList = [TemplateFilter('cheetah', 'templates')]

    @cpy.expose
    def index(self):
        #sort make_codes alphabetically
        makes = sorted([(make, code) for make, code in make_codes.iteritems()])
        return ('entryform', {'make_codes': makes})

cpy.root = CarPrices()
cpy.config.update(file="cherrypy.conf")
cpy.server.start()
