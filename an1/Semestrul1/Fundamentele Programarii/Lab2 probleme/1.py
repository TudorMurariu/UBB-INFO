''' Eratostene :
n = int(input())
d = {0:0 , 1:0}

def Eratostene(n):
    for i in range(3,n,2):
        if not(i in d.keys()):
            d[i] = 1
            j = i*i
            while j < n*n:  
                d[j] = 0
                j += i

Eratostene(n)

n += 1 #numarul prim despre care e vorba nu poate fi n
i = 0
if not (n == 2):
    i = n + (n-1)%2    # daca n e par il crestem cu 1 (numere pare nu pot fi prime cu exceptia lui 2)

while d[i] != 1:
    i += 2

print(i)
'''
n = int(input())

def isPrime(n):
    if n == 2:
        return True
    elif n % 2 == 0:
        return False
    
    i = 3
    while i*i <= n:
        if n % i == 0:
            return False
        i += 2
    return True

n += 1 #numarul prim despre care e vorba nu poate fi n
i = 0
if not (n == 2):
    i = n + (n-1)%2    # daca n e par il crestem cu inca un 1 (numere pare nu pot fi prime cu exceptia lui 2)

while not isPrime(i):
    i += 2

print(i)

