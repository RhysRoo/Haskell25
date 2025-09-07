def onlyDigits(msg):
    l = []
    for c in msg:
        if c.isdigit():
            l.append(c)
    return l
