def go(stream):
    depth = 0
    dsum = 0
    canceled_chars = 0
    state = "ON"
    escaped = False
    for i, char in enumerate(stream):
        if char == "{" and state == "ON":
            depth += 1
            dsum += depth
        elif char == "}" and state == "ON":
            depth -= 1
        elif char == "<" and state == "ON":
            state = "OFF"
        elif state == "OFF":
            if char == "!" and not escaped:
                escaped = True
            elif char == ">" and not escaped:
                state = "ON"
            elif escaped:
                escaped = False
            else:
                canceled_chars += 1
    print(dsum)
    print(canceled_chars)
    assert depth == 0

#go('{{<ab>},{<ab>},{<ab>},{<ab>}}')
#go('{{<!!>},{<!!>},{<!!>},{<!!>}}')
#go('{{<a!>},{<a!>},{<a!>},{<ab>}}')
go(open("input.txt").read())
