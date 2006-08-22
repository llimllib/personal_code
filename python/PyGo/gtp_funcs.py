def convert_to_GTP(move, color, size):
    """Convert a move from (x,y) tuple to GTP style"""
    x, y = move
    
    if str(color).lower() != 'w' and str(color).lower() != 'white' \
        and str(color).lower() != 'b' and str(color).lower() != 'black':
        raise InvalidMoveSyntaxError, "color is invalid"
    elif not 0 <= x <= (size - 1):
        raise InvalidMoveSyntaxError, "x is invalid: %d" % x
    elif not 0 <= y <= (size - 1):
        raise InvalidMoveSyntaxError, "y is invalid: %d" % y

    if x < 8:
        x = chr(x + 65)
    else:
        x = chr(x + 66)
    y = str(size - y)
    move = "%s %s%s" % (color, x, y)
    return move

def convert_from_GTP(move, size):
    """Convert a move from GTP style to an (x,y) tuple"""
    move = str(move).strip().upper()
    play_color, vertex = move.split(" ")
    x = vertex[0]
    if x > "H":                          #exclude "I"
        x = ord(x) - 66
    else:
        x = ord(x) - 65
    y = size - int(vertex[1:])
    if play_color == "WHITE":
        play_color = "W"
    elif play_color == "BLACK":
        play_color = "B"
    else: play_color = play_color[0]

    if play_color != "W" and play_color != "B":
        raise InvalidMoveSyntaxError, "color is invalid"
    elif not 0 <= x <= (size - 1):
        raise InvalidMoveSyntaxError, "x is invalid: %d" % x
    elif not 0 <= y <= (size - 1):
        raise InvalidMoveSyntaxError, "y is invalid: %d" % y
    else: return (play_color, (x, y))
