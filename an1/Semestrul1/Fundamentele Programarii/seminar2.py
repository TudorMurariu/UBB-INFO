n = 0
movies = []
years = []

def func2(movies,n):
    for i in range(n):
        if movies[i][0] == 'B':
            print(movies[i])

def func3(movies,years,n):
    for i in range(n):
        #ind = lst[i].find('_')
        if years[i] == 2016:
            print(movies[i])

def func4(years,n):
    for i in range(n-1):
        if abs(years[i] - years[i+1]) > 10:
            return False
    return True

# Main
while True:
    command = int(input("Scrieti o comanda.\n"))
    if command == 1:
        n = int(input("Enter the number of elements:\n"))
        print("Enter the numbers:")
        for i in range(0,n):
            element = input()
            aux = element.split('_')
            movies.append(aux[0])
            years.append(int(aux[1]))

    elif command == 2:
        func2(movies,n)
    elif command == 3:
       func3(movies,years,n)
    elif command == 4:
        if func4(years,n):
            print("Filmele sunt din acelasi deceniu.","red")
        else :
            print("Filmele NU sunt din acelasi deceniu.")

    elif command == 5:
        print("Exit")
        break
    else:
        print("???")
