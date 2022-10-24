#Enunt: Un server primeste numere de la clienti si le afiseaza pe ecran. Serverul trimite inapoi fiecarui client dublul numarului.
#
#Rulare in doua terminale diferite:
#	python server.py
#	python client.py
import socket

TCP_IP = str(socket.gethostbyname(socket.gethostname()))#"127.0.0.1"
TCP_PORT = socket.htons(8082)

#print "nume: ", socket.gethostname()
#print "IP: ", socket.gethostbyname(socket.gethostname())

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind((TCP_IP, TCP_PORT))
s.listen(1)

while 1:
	conn, addr = s.accept()
	print 'Connection address:', addr
	data = conn.recv(10)
	print "Am primit de la client", data
	if not data: 
		break
	print "Am trimis la client", 2*int(data)
	conn.send(str(2*int(data)))  # echo
conn.close()