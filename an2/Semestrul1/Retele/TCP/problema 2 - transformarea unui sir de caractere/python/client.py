#Enunt: Clientul trimite serverului o propozitie. Serverul ii raspunde serverului cu propozitia scrisa cu majuscule.
#
#Rulare in doua terminale diferite:
#	python server.py
#	python client.py
import socket

TCP_IP = "127.0.0.1"
TCP_PORT = 8888
MESSAGE = raw_input("Dati o propozitie: ")

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((TCP_IP, TCP_PORT))
s.send(MESSAGE)
print "Am trimis la server: ",MESSAGE
data = s.recv(10)
s.close()

print "Am primit de la server: ", data