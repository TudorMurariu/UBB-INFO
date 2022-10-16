n = int(input())

a = 1
b = 1
k = 2
while a <= n:
    c = a+b
    b = a
    a = c
    k += 1

print(a,"este elementul cu nr de ordine",k)
