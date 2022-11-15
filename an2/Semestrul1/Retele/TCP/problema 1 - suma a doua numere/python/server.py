#Enunt: Clientul trimite serverului doua numere. Serverul le primeste, le afiseaza pe ecran si trimite clientului suma lor.
#
#Rulare in doua terminale diferite:
#	python server.py
#	python client.py
import socket

TCP_IP = "127.0.0.1"
TCP_PORT = 8888

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind((TCP_IP, TCP_PORT))
s.listen(1)

while 1:
	conn, addr = s.accept()
	print 'Connection address:', addr
	data = conn.recv(10)
	print "Am primit de la client", data
	if not data: break
	data1 = conn.recv(10)
	print "Am primit de la client", data1
	if not data1: break
	print "Am trimis la client", int(data)+int(data1)
	conn.send(str(int(data)+int(data1)))  # echo
conn.close()