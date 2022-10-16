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
  int welcomeSocket, newSocket, nr, rez;
  char buffer[1024];
  struct sockaddr_in serverAddr;
  struct sockaddr_storage serverStorage;
  socklen_t addr_size;
  
  welcomeSocket = socket(AF_INET, SOCK_STREAM, 0);				/* Create the socket. The three arguments are:  1) Internet domain 2) Stream socket 3) Default protocol (TCP in this case) */
  
  serverAddr.sin_family = AF_INET;						/* Address family = Internet */
  serverAddr.sin_port = 8888;							/* Set port number*/
  serverAddr.sin_addr.s_addr = inet_addr("127.0.0.1");				/* Set IP address to localhost */

  bind(welcomeSocket, (struct sockaddr *) &serverAddr, sizeof(serverAddr));	/* Bind the address struct to the socket */

  listen(welcomeSocket,5);							/* Listen on the socket, with 5 max connection requests queued */

  addr_size = sizeof serverStorage;
  while(1)
  {
  newSocket = accept(welcomeSocket, (struct sockaddr *) &serverStorage, &addr_size);
  
  printf("Waiting for clients...\n");

  recv(newSocket, &nr, sizeof(int), 0);						/* Read the message from the server into the buffer */

  printf("Data received: %d\n",nr);   					/* Print the received message */
  rez=2*nr;
  printf("Trimit %d\n",rez);
  send(newSocket,&rez,sizeof(int),0);
  }
  close(newSocket);
  close(welcomeSocket);

  return 0;
}