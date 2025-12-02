import csv

classes1 = {
    "Watercolor Sharpie Art": 40,
    "Sports/games with visiting High School Athletes": 40,
    "Embroidery, Cross-Stitch, Friendship Bracelets, Rubber Band lanyards": 20,
    "Learn about service & therapy dogs": 20,
    "Crafts - candles & flower vases": 15,
    "Hip-hop dance": 20,
    "Japanese Games": 20,
    "Bingo": 20,
    "Pottery": 20,
    "Magic the Gathering (card game) and Board Games": 20,
    "Create with Computers": 20,
    "Musical Talent Share": 20,
    "Theatre and Improv Games": 20,
    "bowling": 30,
    "Sewing": 20,
    "Legos": 40,
    "Makerspace": 15,
    "Disc Golf": 10,
    "Engineering toys (grades 2-5)": 20,
    "Exploring Outer Space": 15,
    "Crafts - Build a Windmill": 20,
}
classes2 = {
    "Watercolor Sharpie Art": 40,
    "Sports/games with visiting High School Athletes": 40,
    "Embroidery, Cross-Stitch, Friendship Bracelets, Rubber Band lanyards": 20,
    "Learn about service & therapy dogs": 20,
    "Crafts - candles & flower vases": 15,
    "Hip-hop dance": 20,
    "Japanese Games": 20,
    "Bingo": 20,
    "Pottery": 20,
    "Magic the Gathering (card game) and Board Games": 20,
    "Create with Computers": 20,
    "Musical Talent Share": 20,
    "Theatre and Improv Games": 20,
    "bowling": 30,
    "Sewing": 20,
    "Legos": 40,
    "Makerspace": 15,
    "Disc Golf": 10,
    "Engineering toys (grades 2-5)": 20,
    "Exploring Outer Space": 15,
    "Crafts - Build a Windmill": 20,
}

classes = set(classes1.keys()) | set(classes2.keys())
prefs = {}
ids = set()
with open("prefs.csv") as cf:
    reader = csv.reader(cf)
    next(reader)
    for row in reader:
        id, _, grade, a, b, c, d = row
        id = int(id)
        ids.add(id)

        # the "bowling" activities have somewhat different formatting -
        # normalize them to "bowling"
        a, b, c, d = ["bowling" if "bowling" in x.lower() else x for x in [a, b, c, d]]

        # dinosaurs and card making were cut. I manually changed "science
        # experiments" to Science - Amazing Reactions"
        assert all(
            x in classes1 or not x for x in [a, b, c, d]
        ), f"{[a, b, c, d]} {[x in classes1 for x in [a, b, c, d]]}"
        prefs[id] = [x for x in (a, b, c, d) if x and x in classes]


def fit_session1(assignments, prefs, student, pref) -> bool:
    if pref in classes1 and len(assignments[(pref, 1)]) < classes1[pref]:
        assignments[(pref, 1)].append(student)
        prefs[student].remove(pref)
        return True
    return False


def fit_session2(assignments, prefs, student, pref) -> bool:
    if pref in classes2 and len(assignments[(pref, 2)]) < classes2[pref]:
        assignments[(pref, 2)].append(student)
        prefs[student].remove(pref)
        return True

    return False


def findswap(assignments, students, prefs, student, pref):
    # if they don't have a second session class, find them a swap in the second
    # session
    if students[student][1] == None:
        for student2 in reversed(assignments[(pref, 2)]):
            for pref2 in prefs[student2][:]:
                # if they have a preference that we can put them in
                # there, execute the swap
                if pref2 in classes2:
                    if fit_session2(assignments, prefs, student2, pref2):
                        # we fit student2 into a different class - now remove them
                        # from the current class so we can swap student1 in
                        assignments[(pref, 2)].remove(student2)
                        students[student][1] = pref
                        students[student2][1] = pref2
                        assert fit_session2(assignments, prefs, student, pref)
                        return True

    # now try the same with session 1
    if students[student][0] == None:
        for student2 in reversed(assignments[(pref, 1)]):
            for pref2 in prefs[student2][:]:
                # if they have a preference that we can put them in
                # there, execute the swap
                if pref2 in classes1:
                    if fit_session1(assignments, prefs, student2, pref2):
                        assignments[(pref, 1)].remove(student2)
                        students[student][0] = pref
                        students[student2][0] = pref2
                        assert fit_session1(assignments, prefs, student, pref)
                        return True

    return False


# student id -> [session 1 class, session 2 class]
assignments = {(c, 1): [] for c in classes1} | {(c, 2): [] for c in classes2}
students = {id: [None, None] for id in ids}
for round in range(2):
    for student in ids:
        if not prefs[student]:
            continue

        for pref in prefs[student][:]:
            if students[student][0] is None and fit_session1(
                assignments, prefs, student, pref
            ):
                students[student][0] = pref
                break
            if students[student][1] is None and fit_session2(
                assignments, prefs, student, pref
            ):
                students[student][1] = pref
                break
        else:
            # now we have a student that we couldn't match properly, let's see
            # if we can find a swap
            for pref in prefs[student][:]:
                if findswap(assignments, students, prefs, student, pref):
                    print(f"found a swap #{student}")
                    break
            else:
                print(f"student {student} not assigned {prefs[student]}")


# print(assignments)

with open("matches.csv", "w") as cf:
    writer = csv.writer(cf)
    writer.writerow(["class", "session", "students"])
    for (assignment, session), student_ids in assignments.items():
        if session == 1:
            writer.writerow([assignment, 1] + student_ids)
    for (assignment, session), student_ids in assignments.items():
        if session == 2:
            writer.writerow([assignment, 2] + student_ids)

with open("students.csv", "w") as cf:
    writer = csv.writer(cf)
    writer.writerow(["student", "session 1", "session 2"])
    for student in sorted(students):
        s1, s2 = students[student]
        writer.writerow([student, s1, s2])
