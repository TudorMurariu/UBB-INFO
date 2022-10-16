n = int(input())

d = [0,0,0,0,0,0,0,0,0,0]
#    0 1 2 3 4 5 6 7 8 9    vector de frecventa

while n:
    d[n%10] += 1
    n = n // 10

maxN = 0
for i in range(9,-1,-1):
    for j in range(d[i]):
        maxN = maxN * 10 + i

print(maxN)
    