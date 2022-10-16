n = int(input())

d = {1:1} #pentru numere !compuse , nu prime
def Eratostene(n):
    for i in range(2,n+1):
        if not(i in d.keys()):
            d[i] = 1
            j = i*i
            while j < n+1:  
                d[j] = 0
                j += i
Eratostene(n)

a = 1
b = 1
while n:
    if d[a]:
        b = a
        n -= 1
    else:
        i = 2
        while i <= a/2 and n:
            if a%i == 0:
                n -= 1
                b = i
            i += 1
    a += 1

print(b)