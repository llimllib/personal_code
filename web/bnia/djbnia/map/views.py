from django.http import HttpResponse
from django.shortcuts import render_to_response
from os.path import abspath, dirname, join
from functools import partial

selfdir = partial(join, dirname(__file__))

def map(request):
    hoods_json = file(selfdir("hoods.json")).read()
    return render_to_response("map.html", locals())
