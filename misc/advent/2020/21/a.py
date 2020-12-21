from collections import Counter
from ipdb import set_trace as debug

recipes = open("input.txt").read().strip().split("\n")

all_ingredients = Counter()
allergen_idx = {}

for recipe in recipes:
    ingredients, allergens = recipe.split(" (contains ")
    for ingredient in ingredients.split(" "):
        all_ingredients[ingredient] += 1

    ingredients = set(ingredients.split(" "))
    allergens = allergens.strip(")").split(", ")
    for allergen in allergens:
        allergen_idx.setdefault(allergen, ingredients.copy()).intersection_update(
            ingredients
        )

impossible_ingredients = set(all_ingredients.keys())
for allergen, poss in allergen_idx.items():
    impossible_ingredients -= poss

print("part 1:", sum(all_ingredients[i] for i in impossible_ingredients))

while any(len(v) > 1 for v in allergen_idx.values()):
    known = set.union(*[v for v in allergen_idx.values() if len(v) == 1])
    for k in [k for k, v in allergen_idx.items() if len(v) > 1]:
        allergen_idx[k] -= known

print("part 2", ",".join(allergen_idx[k].pop() for k in sorted(allergen_idx.keys())))
