# bot: (low, high)
bots = {}
# bot: (passes to low, passes to high)
passes = {}
outputs = {}

for ins in open("input10.txt"):
    parts = ins.split()
    if parts[0] == "value":
        bot = int(parts[5])
        bots[bot] = tuple(sorted((int(parts[1]), bots[bot][0]))) if bot in bots else (int(parts[1]),)
    else:
        bot, o1, low, o2, high = int(parts[1]), parts[5], int(parts[6]), parts[10], int(parts[11])
        passes[bot] = ((o1, low), (o2, high))

while 1:
    movement = False
    for bot in list(bots.keys()):
        if len(bots[bot]) == 2:
            movement = True
            low, high = bots[bot]
            if low == 17 and high == 61: print("FOUND IT________ BOT {}".format(bot))
            (o1, lowbot), (o2, highbot) = passes[bot]
            if o1 == "bot":
                if lowbot in bots:
                    bots[lowbot] = tuple(sorted([low, bots[lowbot][0]]))
                else:
                    bots[lowbot] = (low,)
            else:
                outputs.setdefault(lowbot, []).append(low)
            if o2 == "bot":
                if highbot in bots:
                    bots[highbot] = tuple(sorted([high, bots[highbot][0]]))
                else:
                    bots[highbot] = (high,)
            else:
                outputs.setdefault(highbot, []).append(high)
            bots[bot] = ()
    if not movement:
        break

print(outputs[0][0] * outputs[1][0] * outputs[2][0])
