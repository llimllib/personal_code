val = {
    ")": 3,
    "]": 57,
    "}": 1197,
    ">": 25137,
}


def filter(f):
    score = 0
    complete_scores = []
    for line in f:
        if not line.strip():
            continue
        stack = []
        for char in line.strip():
            if char in "[({<":
                stack.append(char)
            else:
                c = stack.pop()
                if c == "(" and char != ")":
                    score += val[char]
                    stack.append(c)
                    break
                if c == "[" and char != "]":
                    score += val[char]
                    stack.append(c)
                    break
                if c == "<" and char != ">":
                    score += val[char]
                    stack.append(c)
                    break
                if c == "{" and char != "}":
                    score += val[char]
                    stack.append(c)
                    break
        # valid string, score the autocompletion
        else:
            complete_score = 0
            for char in reversed(stack):
                if char == "(":
                    complete_score = complete_score * 5 + 1
                if char == "[":
                    complete_score = complete_score * 5 + 2
                if char == "{":
                    complete_score = complete_score * 5 + 3
                if char == "<":
                    complete_score = complete_score * 5 + 4
            complete_scores.append(complete_score)
    scores = list(sorted(complete_scores))
    print(score, scores[len(scores) // 2])


filter(open("small.txt"))
filter(open("input.txt"))
