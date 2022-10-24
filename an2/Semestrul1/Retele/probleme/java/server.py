#testam ceva, nu bagati in seama codul de aici
import socket

TCP_IP = "192.168.0.73" #str(socket.gethostbyname(socket.gethostname()))
TCP_PORT = socket.htons(7070)


s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind((TCP_IP, TCP_PORT))
s.listen(1)

while 1:
	conn, addr = s.accept()
	print 'Connection address:', addr
	a = socket.ntohs(conn.recv(4))
	b = socket.ntohs(conn.recv(4))
	print "Am primit de la client", a + " " + b

	print "Am trimis la client", int(a) + int(b)
	conn.send(socket.htons(int(a) + int(b)))  # echo
conn.close()