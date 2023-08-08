
from dataclasses import dataclass # non cps function ######################################################################################################
# def fact(n):
#     if n == 1:
#         return 1
#     else:
#         return n * fact(n-1)

# cps alternative ########################################################################################################
# def fact_cps(n, cont): # add continuation parameter
#     if n == 1:
#         return cont(1) # pass variable to cont
#     else:
#         return fact_cps(n - 1, lambda x: cont(x * n)) # call the rekursion with new cont that continues the computation
#                                                       # cont only ever takes one arg
#                                                       # outermost action of new cont is to pass to current cont
    
# and now to handle rekursion ###########################################################################################
# from dataclasses import dataclass


# def fact_cps(n, cont): 
#     if n == 1:
#         return ['apply-cont', cont, 1] # substitute direkt passing with list element
#     else:
#         return ['go-to', fact_cps, n - 1, lambda x: ['apply-cont', cont, x * n]] # substitute rekursion+passing with list element
    
# def trampoline(result): # add trampoline funktion to control flow, turn rekusion into iterative stack -> homemade stack!
#     while isinstance(result, list):
#         if result[0] == 'apply-cont': # if direkt apply, apply cont
#             result = result[1](result[2])
#         elif result[0] == 'go-to':    # if rekursion, apply rekursively
#             result = result[1](result[2], result[3])
#     return result

# trampoline(fact_cps(5, lambda x : x))

@dataclass
class Goto:
    fun: any
    args: any
    
def fact_tramp(n : int, cont) -> Goto:
    if n == 1:
        return Goto(cont, [1])
    else:
        return Goto(fact_tramp, [(n-1), lambda x : Goto(cont, [n*x])])
    
def trampolien(fun, *args):
    v = fun(*args)
    while isinstance(v, Goto):
        v = v.fun(*v.args)
    return v

print(trampolien(fact_tramp, 2000, lambda x: x)) # info liegt auf dem heap, stack frame verweist darauf

# attempt at list-free cps #########################################
# WHY DOES THIS WORK
# def fact_cps(n, cont): # add continuation parameter
#     if n == 1:
#         return cont(1) # pass variable to cont
#     else:
#         return lambda: fact_cps(n - 1, lambda x: lambda: cont(x * n))
    
# def trampoline(f, *args):
#     v = f(*args)
#     while callable(v):
#         v = v()
#     return v

# print(trampoline(fact_cps(20, lambda x : x)))