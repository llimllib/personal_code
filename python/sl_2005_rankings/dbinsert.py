import sqlite, time
from datetime import date

def s2dt(s):
    return date(*time.strptime(s, '%b %d %Y %I:%M%p')[:3])

def quote(s):
    return s.replace("'", r"\'")

def get_tid(cursor, team):
    cursor.execute("SELECT tid FROM teams WHERE name=%s", team)
    #print cursor.fetchall(), cursor.sql
    try:
        return cursor.fetchall()[0][0]
    except IndexError:
        return 0

def n_games(cursor):
    cursor.execute("SELECT * FROM games")
    return cursor.rowcount

def n_teams(cursor):
    cursor.execute("SELECT * FROM teams")
    return cursor.rowcount

def early_vs_late(cur):
    cur.execute("SELECT * FROM games, teams where winner_id = tid")
    for line in cursor.rowcount()

con = sqlite.connect('sldb')
cur = con.cursor()
ex = cur.execute

def execute_ddl():
    ex("""CREATE TABLE teams(
    tid integer,
    ranking integer,
    name varchar(100))""")

    ex("""CREATE TABLE games(
    gid integer,
    id_winner integer,
    id_loser integer,
    score_winner integer,
    score_loser integer,
    date varchar(100))""")

def clear_tables():
    ex("""delete from teams""")
    ex("""delete from games""")

def parse_file():
    for line in file('scores.dat'):
        if not line.strip(): continue
        try:
            dt, field, team1, team2, score, winner = \
                [x.strip() for x in line.split('\t')]
            if score == 'N/A': 
                continue #rain out or unreported
            score_l, score_w = \
                sorted([int(x) for x in score.strip().split('-')])
            dt = dt.strip()
        except ValueError:
            print "Could not decode line:\n%s" % line
            raise
        if winner == team2:
            loser = team1
        elif winner == team1:
            loser = team2
        else:
            #it's a tie; pick a name for each
            winner = team1
            loser = team2
        teams = ((winner, get_tid(cur, winner)), (loser, get_tid(cur, loser)))
        for name, tid in teams:
            if tid == 0:
                ex("INSERT INTO teams VALUES(%d, 1000, %s)", \
                (n_teams(cur) + 1, name))
            else:
                print tid
        ex("""INSERT INTO games 
        (gid, id_winner, id_loser, score_winner, score_loser, date)
        VALUES(%s, %s, %s, %s, %s, %s)""", \
        (n_games(cur), teams[0][1], score_w, teams[1][1], score_l, dt))

if __name__ == "__main__":
    #execute_ddl()
    clear_tables()
    parse_file()
    cur.close()
    con.commit()
    con.close()
