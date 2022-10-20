import socket
import os

TCP_IP = "172.30.112.143" #"172.30.117.249" # "172.30.112.119" #"172.30.117.249"   # 172.30.112.225
TCP_PORT = 32000 # 8080 # 8888 # 9988 # 8889
#MESSAGE = raw_input("Dati un mesaj: ")
FORMAT = 'utf-8'

for i in range(1,20):
    pid = os.fork()
    if pid > 0:
        MESSAGE = "Ana are " + str(i) + " mere\n"
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((TCP_IP, TCP_PORT))
        s.send(MESSAGE.encode(FORMAT))
        print "Am trimis la server: ",MESSAGE
        data = s.recv(10).decode(FORMAT)
        s.close()
        #print "Am primit de la server: ", data
        break;
    else: # pid == 0
        continue
