n = int(input())

d = {0:0 , 1:0}
def Eratostene(n):
    for i in range(3,n,2):
        if not(i in d.keys()):
            d[i] = 1
            j = i*i
            while j < n:  
                d[j] = 0
                j += i

Eratostene(n)
'''
for i in range(n):
    if i in d.keys():
        print(i,d[i],"\n")
    else:
        print(i,0,"\n")

'''
p1 = p2 = 0
ok = True

if (n-2) in d.keys():
    if d[n-2] == 1:
        p1 = 2
        p2 = n-2
        ok = False

i = 3
while i < n and ok:
    if i in d.keys() and (n-i) in d.keys():
        if d[i] == 1 and d[n-i] == 1:
            p1 = i
            p2 = n-i
            break
    i += 2

if p1 == 0 and p2 == 0:
    print("Nu exista")
else:
    print(p1,p2)


