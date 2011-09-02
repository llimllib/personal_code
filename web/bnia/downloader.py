import os

for year in ["2007", "2008", "2009"]:
  for q in ["1","2","3","4"]:
    os.system("wget http://ubalt.edu/bnia/spreadsheet%s.cfm" % (year+"Q"+q))
