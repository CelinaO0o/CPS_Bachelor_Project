def fib_non_cps (n): # takes forever but no rekursion depth error
    return 1 if n < 2 else fib_non_cps(n-1) + fib_non_cps(n-2) # not tail recursive, because rekursion in opperant positions

# def fib_cps(n, k): # rekursion depth error: max n = 12
#     if n<2:
#         return k(1)
#     else:
#         return fib_cps(n-1, lambda left: fib_cps(n-2, lambda right : k(left + right))) # operant liegt ganz 'innen', pro rekursiv aufruf ein lambda
    

def fib_cps(n, k): # fixes rekursion depth problem BUT WHY
    if n < 2:
        return k(1)
    else:
        return lambda: fib_cps(
                         n - 1,
                         lambda left:
                           lambda: fib_cps(
                                     n - 2,
                                     lambda right:
                                       lambda: k(left + right)))
   
def trampoline(f, *args):
    v = f(*args)
    while callable(v):
        v = v()
    return v
            
print(trampoline(fib_cps, 30, lambda value: value))
