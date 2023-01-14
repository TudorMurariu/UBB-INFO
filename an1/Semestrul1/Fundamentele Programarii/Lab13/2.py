#2) Se dă un întreg pozitiv, găsiți toate descompunerile în sumă de numere prime
def prim(n):
    for i in range(2,n//2+1):
        if n % i == 0:
            return False
    return True

def back(lista, suma, k):
    if suma == 0:
        for element in lista:
            if not prim(element):
                return
        print(lista)
    elif suma > 0:
        for i in range(2,suma+1):
            lista.append(i)
            back(lista, suma-i, k+1)
            lista.pop()
            
back([], 17, 0)
