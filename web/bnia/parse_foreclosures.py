import re
import cPickle
from foreclosure import Foreclosure

foreclosures = []
for year in ["2007", "2008", "2009"]:
  for q in ["1","2","3","4"]:
    f = file("data/spreadsheet%s.cfm" % (year+"Q"+q)).read()
    pl = len(foreclosures)

    first = True
    print year, q
    for row in re.findall("<tr>(.*?)</tr>", f, re.S):
      if first:
        format = [r.strip() for r in re.findall("<th>(.*?)</th>", row, re.S)]
        first = False
        continue
      vals = [r.strip() for r in re.findall("<td>(.*?)</td>", row, re.S)]
      foreclosures.append(Foreclosure(dict(zip(format, vals))))

#manually counted foreclosures as of nov/2/09
assert len(foreclosures) == 13333

cPickle.dump(foreclosures, file("theforeclosures.pkl", "w"))
