input = input()
n = input.split()
n1 = int(n[0])
n2 = int(n[1])  
# cu citirea in acest mod putem citi doua numere pe acelasi rand

v1 = [0,0,0,0,0,0,0,0,0,0]
v2 = [0,0,0,0,0,0,0,0,0,0]
#     0 1 2 3 4 5 6 7 8 9    

while n1:
    v1[n1%10] = 1
    n1 = n1 // 10

while n2:
    v2[n1%10] = 1
    n2 = n2 // 10

ok = True
for i in range(10):
    if v1[i] != v2[i]:
        ok = False
        break

if ok:
    print("Numerele AU proprietatea P")
else:
    print("Numerele NU AU proprietatea P")