n = int(input()) #citire

inv = 0 
while n :  #in while construim inversul
    inv = inv * 10 + n % 10
    n = n // 10

print(inv) # afisare
