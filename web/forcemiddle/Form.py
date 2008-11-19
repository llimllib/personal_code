import simplejson, urllib
from copy import deepcopy

class Form:
    def __init__(self, *args, **kwargs):
        self.fields = args
        self.name = kwargs.pop("name", "")
        self.method = kwargs.pop("method", "")
        self.action = kwargs.pop("action", "")
        self.missing_message = kwargs.pop("missing_message", "missing %s")
        self.atts = kwargs

    def Validate(self, values):
        """Each Field type should return a tuple of type:
        (string, (string, list<string>))
        where the first value is the name of the field, the second is a tuple
        consisting of the current value of the field and a list of any errors
        encountered when validating the field"""

        d = {}
        for field in self.fields:
            if hasattr(field, "Validate") and hasattr(field, "name"):
                if hasattr(field, "required") and field.required:
                    if field.name not in values:
                        d[field.name] = ("", self.missing_message % field.name)
                    else:
                        d[field.name] = field.Validate(values[field.name])
        if any(d[name][1] for name in d):
            return (False, d)
        return (True, d)

    @staticmethod
    def Encode(formdict):
        return "__form=" + urllib.quote_plus(simplejson.dumps(formdict))

    @staticmethod
    def Decode(formdict):
        return simplejson.loads(urllib.unquote_plus(formdict))

def PartialForm(form, partialform):
    form = deepcopy(form) 
    form.errors = []
    for field in form.fields:
        if hasattr(field, "name") and field.name in partialform:
            #XXX: should I write a __getattr__ for Form?
            partialval, errors = partialform[field.name]
            field.value = partialval
            form.errors.extend(errors)
    return form

class TextField:
    def __init__(self, **kwargs):
        self.label = kwargs.pop("label", "")
        self.required = kwargs.pop("required", False)
        self.name = kwargs.pop("name", "")
        self.value = kwargs.pop("value", "")
        self.validate = kwargs.pop("validate", None)
        self.atts = kwargs
        self._type = "text"

    def Validate(self, value):
        errors = []
        if self.required and not value:
            errors.append("Field %s needs a value" % self.name)
        if self.validate: pass #TODO: use this as a regex
        return (value, errors)

    def __str__(self):
        return '<input name="%s" value="%s">' % (self.name, self.value)

class TextArea:
    def __init__(self, **kwargs):
        pop = kwargs.pop
        self.label    = pop("label", "")
        self.required = pop("required", False)
        self.name     = pop("name", "")
        self.value    = pop("value", "")
        self.validate = pop("validate", None)
        self.rows     = pop("rows", 5)
        self.cols     = pop("cols", 40)
        self.atts     = kwargs
        self._type    = "textarea"

    def validate(self, value):
        errors = []
        if self.required and not value:
            errors.append("Field %s needs a value" % self.name)
        if self.validate: pass #TODO
        return (value, errors)

    def __str__(self):
        return '<textarea name="%s" rows="%s", cols="%s">%s</textarea>' \
            % (self.name, self.rows, self.cols, self.value)

class SubmitField:
    def __init__(self, **kwargs):
        self.value = kwargs.pop("value", "")
        self._type = "submit"

    def __str__(self):
        return '<input type="submit" value="%s">' % self.value

class HiddenField:
    def __init__(self, **kwargs):
        self.value = kwargs.pop("value", "")
        self.name = kwargs.pop("name", "")
        self._type = "hidden"

    def __str__(self):
        return '<input type="hidden" name="%s" value="%s">' % (self.name, self.value)
