data = [l.strip() for l in open("input.txt")]
# data = [l.strip() for l in open("small.txt")]

passports = []
passport = {}
for line in data:
    if not line:
        passports.append(passport)
        passport = {}
        continue
    datas = line.split(" ")
    for data in datas:
        parts = data.split(":")
        passport[parts[0]] = parts[1]
passports.append(passport)

valid = 0
invalid = 0
for passport in passports:
    if (
        "byr" in passport
        and "iyr" in passport
        and "eyr" in passport
        and "hgt" in passport
        and "hcl" in passport
        and "ecl" in passport
        and "pid" in passport
    ):
        valid += 1
    else:
        invalid += 1

print(valid, invalid)
