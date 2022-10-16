import datetime
n = input()    #zi/luna/an
data = n.split('/')

ziNastere = int(data[0])
lunaNastere = int(data[1])
anNastere = int(data[2])

#print(ziNastere,lunaNastere,anNastere)

currentTime = datetime.datetime.now()

zi = int(currentTime.day)
luna = int(currentTime.month)
an = int(currentTime.year)

suma  = (an - anNastere) * 365 + (an - anNastere)//4 + (luna - lunaNastere)*30 + (zi - ziNastere)
# (an - anNastere)/4 vine de la anii bisecti care sunt o data la 4 ani
# de notat (luna - lunaNastere) poate fi si o valoare negativa fiindca am efectuat calculul fara sa ne imprumutam de la ani

print(suma , "zile")

