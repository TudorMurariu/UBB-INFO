an = int(input())
zile = int(input())
# scrieti anul si numarul de zile pe randuri diferite

NrZileLuna = [31,28,31,30,31,30,31,31,30,31,30,31,100]

if an%4 == 0:
    NrZileLuna[1] += 1 #an bisect
luna = 0

while zile > NrZileLuna[luna]:
    zile -= NrZileLuna[luna]
    luna += 1

s = str(zile) + "\\" + str(luna+1) + "\\" + str(an)
print(s)
