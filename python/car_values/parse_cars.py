import re

YEAR_RE = re.compile("(\d{4})&nbsp;")
MODEL_RE = re.compile("a href=.*?>([\w\s]*)", re.M)
MILE_RE = re.compile('<div align="center">([\d,]*?)</div>')

def xcars(fin):
    year = 0
    model = ''
    price = 0
    miles = 0
    for line in fin:
        if not year:
            m = YEAR_RE.search(line)
            if m:
                year = int(m.groups()[0])
                if year < 1980 or year > 2006:
                    year = 0
        elif not model:
            while not line.find('</td>') > -1:
                line += fin.next()
            m = MODEL_RE.search(line)
            if m:
                model = m.groups()[0].strip().replace('\n', ' ')
        elif not price:
            while not line.find('$') > -1:
                line = fin.next()
            price = int(line.strip('$ \n\t').replace(',', ''))
            if price < 500  or price > 200000:
                price = 0
        elif not miles:
            m = MILE_RE.search(line)
            while not m:
                line = fin.next()
                m = MILE_RE.search(line)
            miles = int(m.groups()[0].replace(',', ''))
            yield year, model, price, miles
        else: #reset values
            year = 0
            model = ''
            price = 0
            miles = 0

if __name__ == "__main__":
    fin = file('search_results.jsp')
    fin = file('mazda.html')
    fin = file('testing.out.html')
    #cost per mile for each car
    cpm = []
    makes = {}
    for year, make, cost, miles in xcars(fin):
        #split model information off of make
        make = make.split('  ')[0]
        miles = float(miles)

        cpm.append((cost/miles, (year, make, cost, miles)))
        if make in makes:
            makes[make][0] += cost
            makes[make][1] += miles
        else:
            makes[make] = [cost, miles]
    cpm.sort()
    sort_makes = [(cost/float(miles), make) for make, (cost, miles) in makes.iteritems()]
    sort_makes.sort()
    print "cheapest models by average for all cars of that model:"
    for value, make in sort_makes:
        print "$%.2f %s" % (round(value, 2), make)
    print
    print "cheapest cars available:"
    for value, (year, make, cost, miles) in cpm[:5]:
        miles = int(miles)
        cost = int(cost)
        print "$%.2f %s %-30s %6d $%d" % (round(value, 2), year, make, miles, cost)
