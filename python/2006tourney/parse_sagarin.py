RATINGS = {}
def parse_sagarin(fin):
    for line in fin:
        line = [x.strip() for x in line.strip().split()]
        try:
            int(line[0])
            line = line[1:]
        except ValueError:
            continue
        team = ''
        for is_name in line:
            if is_name == '=' or is_name == '$':
                break
            team = team + is_name + ' '
        team = team.strip()
        #now pop the rating, which is right after the '='
        avg_rating = float(line.pop(line.index('=') + 1))
        elo = -1
        predictor = -1
        for is_rating in line:
            #import pdb; pdb.set_trace()
            if len(is_rating) == 5 and is_rating[:2].isdigit() \
            and is_rating[2] == '.':
                if elo == -1:
                    elo = float(is_rating)
                else:
                    predictor = float(is_rating)
        RATINGS[team] = (avg_rating, elo, predictor)

parse_sagarin(file('sagarin.dat'))

def get_team(name):
    return RATINGS[name]
