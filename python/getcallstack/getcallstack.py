
x = sys._getframe(1)
print "gb called by: %s" % x.f_code.co_name
