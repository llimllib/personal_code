from os import path

import cherrypy
from cherrypy import Tool
import pkg_resources

available_engines = {}

class TemplateEngineMissing(Exception): pass

def cherrypy_vars():
    return {'cherrypy':cherrypy}

ep_group = 'python.templating.engines'
for entry_point in pkg_resources.iter_entry_points(ep_group):
    Engine = entry_point.load()
    available_engines[entry_point.name] = Engine

def using_template(template_name):
    def wrapper(f):
        def inner(*args, **params):
            return (template_name, f(*args, **params))
        return inner
    return wrapper

def all(seq, predicate):
    for i in seq:
        if not predicate(i):
            return False
    return True

def _requires_template(body, expandtypes=(list, tuple)):
    for _type in expandtypes:
        if isinstance(body, _type):
            if _is_template_request(body): return True
            if all(body, _is_template_request): return True
            break
    return False

def _is_template_request(body):
    if isinstance(body, tuple) and len(body) == 2:
        if isinstance(body[0], str) and isinstance(body[1], dict):
            return True
    return False

def flatten(seq, expandtypes=(list, tuple)):
    lst = []
    flattened = False
    for i in seq:
        for _type in expandtypes:
            if isinstance(i, _type):
                flattened = True
                lst.extend(flatten(i))
        if not flattened:
            lst.append(i)
    return lst

class BuffetTool(Tool):
    def __init__(self, engine_name, template_root=None, config_section=None):
        if template_root:
            self.template_root = template_root
        else:
            self.template_root = '.'
        if not config_section:
            config_section = engine_name + '_settings'
        engine_opts = cherrypy.config.get(config_section, {})
        Engine = available_engines.get(engine_name, None)
        if not Engine:
            msg = 'Please install a plugin for "%s" to use its functionality'
            raise TemplateEngineMissing(msg % engine_name)
        self.engine = Engine(cherrypy_vars, engine_opts)

        #initialize the parent, hooking before_finalize to our before_finalize
        #method
        Tool.__init__(self, 'before_finalize', self.before_finalize)
        
    def before_finalize(self):
        body = cherrypy.response.body
        if _is_template_request(body):
            template_path, vars_ = body
            cherrypy.response.body = self.render_template(template_path, vars_)
        elif _requires_template(body):
            result = []
            for template_path, vars_ in body:
                result.append(self.render_template(template_path, vars_))
            cherrypy.response.body = flatten(result)
        return

    def render_template(self, template_path, vars_):
        base_path_parts = self.template_root.split('/')
        tmpl_path_parts = template_path.split('/')
        full_path = path.join(*(base_path_parts + tmpl_path_parts))
        # at this point, python.templating.engines plugins require a dotted
        # path to the template - blame TurboGears ;-)
        dotted_path = full_path.replace(path.sep, '.')
        dotted_path = dotted_path.lstrip('.')
        page_data = self.engine.render(vars_, template=dotted_path)
        return page_data.splitlines(1)
