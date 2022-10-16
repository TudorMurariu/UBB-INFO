n = int(input())

p = 1 
i = 2
while i*i < n:
    if n % i == 0:
        p *= i
        p *= n // i  
        #stim deja ca n e divizibil cu i dar, nu vrem ca raspunsul sa fie in double
    i += 1

if n == i*i:
    p *= i

print(p)