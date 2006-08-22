import timeit, urllib

PAGE = "http://localhost/newf2o/blog/serve"
url = urllib.URLopener()

t = timeit.Timer('url.open(PAGE).read()', 'from __main__ import url, PAGE')
print t.timeit(100)
