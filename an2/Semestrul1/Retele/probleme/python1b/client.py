#Enunt: Un server primeste numere de la clienti si le afiseaza pe ecran. Serverul trimite inapoi fiecarui client dublul numarului.
#
#Rulare in doua terminale diferite:
#	python server.py
#	python client.py
import socket

#print "nume: ", socket.gethostname()
#print "IP: ", socket.gethostbyname(socket.gethostname())

TCP_IP = "192.168.0.73" #str(socket.gethostbyname(socket.gethostname()))#"127.0.0.1"
TCP_PORT = 8082
FORMAT = "utf-8"

a = str(raw_input("Dati un numar: "))
b = str(raw_input("Dati un numar: "))

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((TCP_IP, TCP_PORT))
s.send(str(a).encode(FORMAT))
s.send(str(b).encode(FORMAT))
print "Am trimis la server: ", a + " " + b
data = s.recv(10).decode(FORMAT)
s.close()

print "Am primit de la server: ", data