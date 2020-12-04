import re

data = [l.strip() for l in open("input.txt")]
# data = [l.strip() for l in open("small.txt")]
# data = [l.strip() for l in open("small2.txt")]

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
        try:
            byr = int(passport["byr"])
            if byr < 1920 or byr > 2002:
                print("invalid byr", passport)
                invalid += 1
                continue
            iyr = int(passport["iyr"])
            if iyr < 2010 or iyr > 2020:
                print("invalid iyr", passport)
                invalid += 1
                continue
            eyr = int(passport["eyr"])
            if eyr < 2020 or eyr > 2030:
                print("invalid eyr", passport)
                invalid += 1
                continue
            m = re.findall(r"(\d+)(in|cm)", passport["hgt"])
            if not m:
                invalid += 1
                continue
            hgt = int(m[0][0])
            if m[0][1] == "in" and (hgt < 59 or hgt > 76):
                print("invalid hgt in", passport)
                invalid += 1
                continue
            if m[0][1] == "cm" and (hgt < 150 or hgt > 193):
                print("invalid hgt in", passport)
                invalid += 1
                continue
            m = re.findall(r"^#[0-9a-f]{6}$", passport["hcl"])
            if not m:
                print("invalid hcl", passport)
                invalid += 1
                continue
            m = re.findall(r"^amb|blu|brn|gry|grn|hzl|oth$", passport["ecl"])
            if not m:
                print("invalid ecl", passport)
                invalid += 1
                continue
            m = re.findall(r"^(\d{9})$", passport["pid"])
            if not m:
                print("invalid pid", passport)
                invalid += 1
                continue
        except Exception as e:
            invalid += 1
            continue
        valid += 1
        print("valid", passport)
    else:
        invalid += 1

print(valid, invalid)
