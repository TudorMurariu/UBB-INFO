#Enunt: Clientul trimite serverului un numar. Serverul il primeste si il afiseaza pe ecran.
#
#Rulare in doua terminale diferite:
#	python server.py
#	python client.py

import socket

UDP_IP = "127.0.0.1"
UDP_PORT = 1234
MESSAGE = str(input("Dati un numar: "))

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.sendto(MESSAGE, (UDP_IP, UDP_PORT))