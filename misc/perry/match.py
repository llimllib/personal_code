import csv
from matching.games import HospitalResident

# a list of the classes, and their max capacity
classes1 = {
    "Gymnastics Party": 40,
    "Cooking": 20,
    "Sciencenter": 20,
    "Pottery/Clay": 20,
    "Engineering toys (grades 2-5)": 20,
    "Science project - making oobleck": 20,
    "Hip-hop dance": 20,
    "Make your own puppet": 20,
    "Makerspace": 20,
    "Piano Talent Share": 20,
    "Strawbees robots (grades 2-5)": 20,
    "Magic the Gathering (card game)": 20,
    "Legos": 20,
    "Science - Amazing Reactions": 20,
    "Zen Group - relax and color": 20,
    "Make Comic Books": 20,
    "Creative writing": 20,
    "Sewing": 20,
    "Disc Golf": 20,
    "Planets!": 20,
    "Make Snowflakes": 20,
    "Arts and Crafts - Make a Winter Craft": 20,
    "Puzzles": 20,
}
classes2 = {
    "Gymnastics Party": 40,
    "Cooking": 20,
    "Pottery/Clay": 20,
    "Science project - making oobleck": 20,
    "Sciencenter": 20,
    "Hip-hop dance": 20,
    "Magic the Gathering (card game)": 20,
    "Makerspace": 20,
    "Legos": 20,
    "Planets!": 20,
    "Make your own puppet": 20,
    "Science - Amazing Reactions": 20,
    "Sewing": 20,
    "Piano Talent Share": 20,
    "Make Comic Books": 20,
    "Disc Golf": 20,
    "Make Snowflakes": 20,
    "Zen Group - relax and color": 20,
    "Engineering toys (grades 2-5)": 20,
    "Arts and Crafts - Make a Winter Craft": 20,
    "Strawbees robots (grades 2-5)": 20,
    "Feminist Group": 20,
    "Puzzles": 20,
}

students_sess1 = {}
students_sess2 = {}
classes = set()
ids = set()
with open("prefs.csv") as cf:
    reader = csv.reader(cf)
    next(reader)
    for row in reader:
        id, grade, a, b, c, d, _, _ = row
        # dinosaurs and card making were cut
        prefs1 = [x for x in (a, b, c, d) if x and x in classes1]
        students_sess1[id] = prefs1

        prefs2 = [x for x in (a, b, c, d) if x and x in classes2]
        students_sess2[id] = prefs2

        classes |= set(prefs1) | set(prefs2)
        ids.add(id)

    # assert that there are no classes that aren't in our class list
    for klass in classes:
        assert (klass in classes1) or (klass in classes2), print(f"{klass} not found")

game = HospitalResident.create_from_dictionaries(
    students_sess1, {c: list(ids) for c in classes1.keys()}, classes1
)

# fails with "x not in list"
matching = game.solve(optimal="resident")
