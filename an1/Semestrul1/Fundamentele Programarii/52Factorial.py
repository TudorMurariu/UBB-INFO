def Factorial(n):
    x = 1
    for i in range(2,n+1):
        x = x * i
    return x

while True:
    n = int(input())
    print(n,"!  =  ",Factorial(n))