n = int(input())

d = [0,0,0,0,0,0,0,0,0,0]
#    0 1 2 3 4 5 6 7 8 9    vector de frecventa

while n:
    d[n%10] += 1
    n = n // 10

minN = 0
if d[0] > 0:
    i = 1
    while i < 10:
        if d[i] > 0:
            break
        i += 1
    if i < 10: # pentru cazul in care n este 0
        minN = i
        d[i] -= 1
    

for i in range(10):
    for j in range(d[i]):
        minN = minN * 10 + i

print(minN)
    