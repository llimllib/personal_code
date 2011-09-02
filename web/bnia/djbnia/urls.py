from django.conf.urls.defaults import *

# Uncomment the next two lines to enable the admin:
# from django.contrib import admin
# admin.autodiscover()

urlpatterns = patterns('',
    (r'^/?$', 'djbnia.map.views.map'),

    (r'^static/(?P<path>.*)$', 'django.views.static.serve',
        {'document_root': "/Users/llimllib/Dropbox/bnia/djbnia/static/"}),
    # Example:
    # (r'^djbnia/', include('djbnia.foo.urls')),

    # Uncomment the admin/doc line below and add 'django.contrib.admindocs' 
    # to INSTALLED_APPS to enable admin documentation:
    # (r'^admin/doc/', include('django.contrib.admindocs.urls')),

    # Uncomment the next line to enable the admin:
    # (r'^admin/(.*)', admin.site.root),
)
