#Enunt:  Clientul trimite serverului o propozitie. Serverul ii raspunde serverului cu propozitia scrisa cu majuscule.
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
	print "Am trimis la client", data.upper()
	conn.send(data.upper())  # echo
conn.close()