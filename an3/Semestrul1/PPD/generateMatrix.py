import random
n = 10000
m = 10

with open('data.txt', 'w') as f:
    for i in range(n):
        for j in range(m):
            f.write(str(random.randint(1, 100)) + " ")
        f.write('\n')
