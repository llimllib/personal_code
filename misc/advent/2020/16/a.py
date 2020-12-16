import operator
import re
from functools import reduce


def valid(n, validations):
    for _, (range1, range2) in validations.items():
        if inrange(n, range1) or inrange(n, range2):
            return True
    return False


def inrange(n, range_):
    return range_[0] <= n <= range_[1]


def validationmatches(column, ranges):
    for n in column:
        if not (inrange(n, ranges[0]) or inrange(n, ranges[1])):
            return False
    return True


# return a list of all validations that match every number in column
def valid_validations(column, validations):
    valid = []
    for key, (range1, range2) in validations.items():
        if all(inrange(n, range1) or inrange(n, range2) for n in column):
            valid.append(key)
    return valid


def parse_validations(rawvalidations):
    validations = {}
    for validation in rawvalidations.split("\n"):
        parts = re.split(r"[ :]", validation)
        name = parts[:-3]
        range1, range2 = (
            tuple(map(int, parts[-3].split("-"))),
            tuple(map(int, parts[-1].split("-"))),
        )
        validations[" ".join(name).strip()] = (range1, range2)

    return validations


rawvalidations, myticket, tickets = open("input.txt").read().split("\n\n")
# rawvalidations, myticket, tickets = open("small.txt").read().split("\n\n")
# rawvalidations, myticket, tickets = open("small2.txt").read().split("\n\n")


validations = parse_validations(rawvalidations)

invalid = 0
valid_tickets = []
for ticket in tickets.strip().split("\n")[1:]:
    is_valid_ticket = True
    ns = tuple(map(int, ticket.strip().split(",")))
    for n in ns:
        if not valid(n, validations):
            invalid += n
            is_valid_ticket = False
    if is_valid_ticket:
        valid_tickets.append(ns)

print(invalid)

columns = {}
for col in range(len(valid_tickets[0])):
    columns[col] = valid_validations([t[col] for t in valid_tickets], validations)

# now we can start with the known columns, eliminate their value from the other
# columns, and we pop out with the full column list
known = {}
i = 0
while 1:
    if len(columns) == 0:
        break

    for k, v in columns.copy().items():
        if len(v) == 1:
            known[k] = v[0]
            del columns[k]
            for key in columns:
                try:
                    columns[key].remove(v[0])
                except ValueError:
                    continue

print(known)

departure_indexes = [k for k, v in known.items() if v.startswith("departure")]
myticket = [int(x) for x in myticket.split("\n")[1].split(",")]
print(reduce(operator.mul, [myticket[i] for i in departure_indexes]))
