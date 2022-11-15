#Enunt: Clientul trimite serverului un sir de numere. Serverul le primeste, le afiseaza pe ecran si trimite clientului suma lor.
#
#Rulare in doua terminale diferite:
#	python server.py
#	python client.py
import socket

UDP_IP = "127.0.0.1"
UDP_PORT = 1234
MESSAGE = str(input("Dati un numar: "))

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
print "Trimit la server lungimea: ",MESSAGE
sock.sendto(MESSAGE, (UDP_IP, UDP_PORT))

for x in xrange(0, int(MESSAGE)):
	MESSAGE2 = str(input("Dati al doilea numar: "))
	print "Trimit la server: ",MESSAGE2
	sock.sendto(MESSAGE2, (UDP_IP, UDP_PORT))

data, addr = sock.recvfrom(1024) 
print "Am primit de la server numarul: ", data