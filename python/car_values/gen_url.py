import urllib
from Itpl import itpl
from make_codes import make_codes

BASE_URL = "http://cars.com/go/search/search_results.jsp?tracktype=usedcc&searchType=21&sort=true&pageNumber=0&numResultsPerPage=50&largeNumResultsPerPage=1500&sortorder=ascending&sortfield=MILES%2CPRICE+descending&certifiedOnly=false&criteria=K-%7CE-%7CM-_${make}_%7CN-N%7CR-${radius}%7CI-1%7CP-PRICE+descending%7CQ-descending%7CZ-${zipcode}&aff=national"

def gen_url(radius, zipcode, make):
    return urllib.urlopen(itpl(BASE_URL))

if __name__ == "__main__":
    radius = "30"
    zipcode = "06511"
    make = make_codes["Mazda"]
    ifile = gen_url(radius, zipcode, make)
    fout = file('testing.out.html', 'w')
    for line in ifile:
        fout.write(line)
