#Enunt: Clientul trimite serverului un numar. Serverul il primeste, il afiseaza pe ecran si trimite clientului dublul numarului.
#
#Rulare in doua terminale diferite:
#	python server.py
#	python client.py
import socket

UDP_IP = "127.0.0.1"
UDP_PORT = 1234

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((UDP_IP, UDP_PORT))

while True:
	data, addr = sock.recvfrom(1024) 
	print "Am primit de la client numarul: ", data
	NR = str(2*int(data))
	print "Trimit spre client: ",NR
	sock.sendto(NR, addr)