/* Enunt: Un server primeste numere de la clienti si le afiseaza pe ecran. Serverul trimite inapoi fiecarui client dublul numarului.

Compilare: 
	gcc server.c -o server
	gcc client.c -o client
	
Rulare in doua terminale diferite:
	./server
	./client
*/
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>
#include <errno.h>

int main(){
  int clientSocket;
  char buffer[1024];
  struct sockaddr_in serverAddr;

  char s[100], rez[100];

  clientSocket = socket(AF_INET, SOCK_STREAM, 0);				/* Create the socket. The three arguments are:  1) Internet domain 2) Stream socket 3) Default protocol (TCP in this case) */
  
  serverAddr.sin_family = AF_INET;						/* Address family = Internet */
  serverAddr.sin_port = 9988;							/* Set port number*/
  serverAddr.sin_addr.s_addr = inet_addr("172.30.112.119");	 //  172.30.112.119 "172.30.112.225" 172.30.117.249
   
  if(connect(clientSocket, (struct sockaddr *) &serverAddr, sizeof(serverAddr)) < 0) {
    printf("ERROR LA CONNECT");
    return 0;
  }

  printf("Introduceti stringul:\n");
  scanf("%s",s);
  printf("Trimit la server %s\n",s); 

  if(send(clientSocket, s, sizeof(char)*10, 0) < 0) {
    printf("ERROR LA SEND");
    return 0;
  }

  if(recv(clientSocket, rez, sizeof(char)*10, 0) < 0 ){
    printf("ERROR LA RECV");
    return 0;
  }					/* Read the message from the server into the buffer */

  printf("Data received: %s\n",rez);   					/* Print the received message */

  close(clientSocket);

  return 0;
}