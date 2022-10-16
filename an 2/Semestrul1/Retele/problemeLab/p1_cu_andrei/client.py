import socket

TCP_IP = "172.30.117.249" # "172.30.112.119" #"172.30.117.249"   # 172.30.112.225
TCP_PORT = 8888 # 9988 # 8889
MESSAGE = raw_input("Dati un numar: ")
FORMAT = 'utf-8'

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((TCP_IP, TCP_PORT))
s.send(MESSAGE.encode(FORMAT))
print "Am trimis la server: ",MESSAGE
data = s.recv(10).decode(FORMAT)
s.close()

print "Am primit de la server: ", data