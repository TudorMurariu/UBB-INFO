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

int main(){
  int clientSocket, nr,rez;
  char buffer[1024];
  struct sockaddr_in serverAddr;

  clientSocket = socket(AF_INET, SOCK_STREAM, 0);				/* Create the socket. The three arguments are:  1) Internet domain 2) Stream socket 3) Default protocol (TCP in this case) */
  
  serverAddr.sin_family = AF_INET;						/* Address family = Internet */
  serverAddr.sin_port = 8888;							/* Set port number*/
  serverAddr.sin_addr.s_addr = inet_addr("127.0.0.1");				/* Set IP address to localhost */
   
  connect(clientSocket, (struct sockaddr *) &serverAddr, sizeof serverAddr);
  printf("Introduceti numarul:\n");
  scanf("%d",&nr);
  printf("Trimit la server %d\n",nr); 

  send(clientSocket,&nr,sizeof(int),0);

  recv(clientSocket, &rez, sizeof(int), 0);						/* Read the message from the server into the buffer */

  printf("Data received: %d\n",rez);   					/* Print the received message */

  close(clientSocket);

  return 0;
}