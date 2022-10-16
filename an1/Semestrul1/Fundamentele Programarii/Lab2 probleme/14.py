n = int(input())

def isPerfect(x):
    if x == 1: 
        return False
    s = 1 #
    i = 2
    while i*i < x:
        if x % i == 0:
            s = s + i + x/i
        i += 1
    
    if i*i == x:
        s += i
    
    return x == s

r = n + 1
while not isPerfect(r):
    r += 1

print(r)