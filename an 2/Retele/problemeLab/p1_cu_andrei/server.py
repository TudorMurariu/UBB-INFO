import socket

TCP_IP = "172.30.112.225"
TCP_PORT = 8899
FORMAT = 'utf-8'

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind((TCP_IP, TCP_PORT))
s.listen(1)

while 1:
	conn, addr = s.accept()
	print 'Connection address:', addr
	data = conn.recv(10).decode(FORMAT) #string
	print "Am primit de la client", data
	if not data: 
		break
	print "Am trimis la client", data
	conn.send(data.encode(FORMAT))  # echo
conn.close()