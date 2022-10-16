n = int(input())

d = {0:0 , 1:0}
def Eratostene(n):
    for i in range(3,n,2):
        if not(i in d.keys()):
            d[i] = 1
            j = i*i
            while j < n*n:  
                d[j] = 0
                j += i
Eratostene(n)

if n > 3:
    i = n-1 - n%2
    while not d[i]:
        i -= 2
    print(i)
elif n == 3:
    print(2)
else:
    print("Nu exista un astfel de numar . ")
