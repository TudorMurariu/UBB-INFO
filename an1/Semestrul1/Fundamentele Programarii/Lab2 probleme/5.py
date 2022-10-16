n = int(input())

def isPrime(n): #sau cu eratostene
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

i = 0
if n+1 != 2:
    i = n+1 + n%2 #doar numerele impare pot fi prime(cu exc lui 2)

while not(isPrime(i) and isPrime(i+2)):
    i += 2

print(i , i+2)

