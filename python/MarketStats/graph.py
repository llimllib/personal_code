import MySQLdb
from datetime import date, timedelta
from pylab import figure, show
from matplotlib.dates import YearLocator, MonthLocator, DayLocator, DateFormatter, drange

prod = MySQLdb.connect(host="liono.proquobooks.com", 
                    user="warehouseManager", 
                    passwd="manage1", 
                    db="morathi")

def n_orders():
    sql = """
select market, DAY(Date_Received), MONTH(Date_Received), YEAR(Date_Received), count(*)
from salesorder
where DATEDIFF(NOW(), Date_Received) <= 7
group by DATEDIFF(NOW(), Date_Received), market;"""

    cur = prod.cursor()
    cur.execute(sql)

    result = {}
    for market, d,m,y, cnt in cur.fetchall():
        result.setdefault(market, []).append((date(y,m,d), int(cnt)))

    cur.close()

    return result

def graph_orders(orders):
    fig = figure()
    ax = fig.add_subplot(111)

    for market, res in orders.iteritems():
        dates  = [r[0] for r in res]
        counts = [r[1] for r in res]
        oneday = timedelta(days=1)
        x = drange(min(dates), max(dates) + oneday, oneday)

        ax.plot_date(x, counts, '-')

    ax.xaxis.set_major_locator(DayLocator())
    ax.xaxis.set_major_formatter(DateFormatter("%x"))

    show()

graph_orders(n_orders())
