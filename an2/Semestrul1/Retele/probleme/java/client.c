/* Enunt: Un server primeste numere de la clienti si le afiseaza pe ecran. Serverul trimite inapoi fiecarui client dublul numarului.

Compilare: 
	gcc server.c -o server
	gcc client.c -o client
	
Rulare in doua terminale diferite:
	./server
	./client
*/
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <string.h>

int main(){
  int clientSocket, a,b ,rez;
  char buffer[1024];
  struct sockaddr_in serverAddr;

  clientSocket = socket(AF_INET, SOCK_STREAM, 0);				/* Create the socket. The three arguments are:  1) Internet domain 2) Stream socket 3) Default protocol (TCP in this case) */
  if (clientSocket < 0) {
    printf("Eroare la crearea socketului client\n");
    return 1;
  }

  memset(&serverAddr, 0, sizeof(serverAddr));
  serverAddr.sin_family = AF_INET;					
  serverAddr.sin_port = htons(7072);				
  serverAddr.sin_addr.s_addr = inet_addr("172.25.14.227");
   
  if(connect(clientSocket, (struct sockaddr *) &serverAddr, sizeof serverAddr) < 0) {
    printf("Eroare la conectarea la server\n");
    return 1;
  }

  printf("a :\n");
  scanf("%d",&a);
  printf("b :\n");
  scanf("%d",&b);
  printf("Trimit la server %d %d\n",a, b); 

  a = htonl(a);
  b = htonl(b);

  send(clientSocket,&a,sizeof(int),0);
  send(clientSocket,&b,sizeof(int),0);

  recv(clientSocket, &rez, sizeof(int), 0);						/* Read the message from the server into the buffer */

  rez = ntohl(rez);
  printf("Data received: %d\n",rez);   					/* Print the received message */

  close(clientSocket);

  return 0;
}