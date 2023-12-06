from dataclasses import dataclass
from typing import Callable

# non cps function ######################################################################################################


def fact(n):
    if n == 1:
        return 1
    else:
        return n * fact(n - 1)

# cps alternative ########################################################################################################


def factCPS0(n, k):  # add continuation parameter
    if n == 1:
        return k(1)  # pass variable to cont
    else:
        return factCPS0(n - 1, lambda x: k(x * n))  # call the rekursion with new cont that continues the computation
                                                       # cont only ever takes one arg
                                                       # outermost action of new cont is to pass to current cont

# and now to handle rekursion using dataclasses ##########################################################################


@dataclass
class Goto:
    fun: Callable
    args: list[int | Callable]


def factCPS1(n: int, k: Callable) -> Goto:
    if n == 1:
        return Goto(k, [1])
    else:
        return Goto(factCPS1,
                    [(n - 1), lambda x: Goto(k, [n * x])])


def trampoline0(fun, *args):  # info lies on the heap, stack frame points to heap
    v = fun(*args)
    while isinstance(v, Goto):
        v = v.fun(*v.args)
    return v

# using lists ############################################################################################################


def factCPS2(n, k):
    if n == 1:
        return ['apply-cont', k, 1]  # substitute direkt passing with list element
    else:
        return ['go-to', factCPS2, n - 1, lambda x: ['apply-cont', k, x * n]]  # substitute rekursion+passing with list element


def trampoline1(result):  # add trampoline funktion to control flow, turn rekusion into iterative stack -> homemade stack!
    while isinstance(result, list):
        if result[0] == 'apply-cont':  # if direkt apply, apply cont
            result = result[1](result[2])
        elif result[0] == 'go-to':  # if rekursion, apply rekursively
            result = result[1](result[2], result[3])
    return result

# using functions #########################################################################################################
# like dataclasses, wrapping within a function saves arguments/data/etc within one stack frame instead of growing the stack


def factCPS3(n, k):  # add continuation parameter
    if n == 1:
        return k(1)  # pass variable to cont
    else:
        return lambda: factCPS3(n - 1, lambda x: lambda: k(x * n))


def trampoline2(f, *args):
    v = f(*args)
    while callable(v):
        v = v()
    return v


if __name__ == "__main__":
    print("no trampoline:", factCPS0(20, lambda x: x))  # may cause recursion depth error
    print("trampoline 0:", trampoline0(factCPS1, 20, lambda x: x))
    print("trampoline 1:", trampoline1(factCPS2(20, lambda x: x)))
    print("trampoline 2:", trampoline2(factCPS3, 20, lambda x: x))
