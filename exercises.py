# def removeFirst (x, xs):
#     xs.remove(x) # would remove have to be rewritten in CPS?
#     return xs

# def removeFirstCPS (x, xs, k):
#     xs.remove(x, k)
#     return k(xs)

def removeFirst (x, xs):
    nxs = []
    flag = True
    for i in xs:
        if i == x and flag:
            flag = False
        else:
            nxs += [i]
    return nxs

def removeFirstCPS (x, xs, k):
    nxs = []
    flag = True
    for i in xs:
        if i == x & flag:
            flag = False
        else:
            nxs += i
    return k (nxs)

def removeFirstRecursively(x, xs):
    if xs[0] == x:
        return xs[1::]
    else:
        return [xs[0]] + removeFirstRecursively(x, xs[1::])
    
def removeFirstRecursivelyCPS(x, xs, k): # doesn't seem to hit recursion depth error, even without trampoline?
    if xs[0] == x:
        return k(xs[1::]) # can i assume [i::] is a CPS friendly operation?
    else:
        return removeFirstRecursivelyCPS(x, xs[1::], lambda y: k([xs[0]] + y))
    
def listSum(xs):
    s = 0
    for x in xs:
        s +=x
    return s

def listSumCPS(xs, k):
    s = 0
    for x in xs:
        s += x
    return k(s)

