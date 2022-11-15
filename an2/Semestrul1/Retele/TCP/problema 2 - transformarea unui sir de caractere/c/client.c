/* Enunt: Clientul trimite serverului o propozitie. Serverul ii raspunde serverului cu propozitia scrisa cu majuscule.

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
  uint16_t clientSocket, nr,rez,length;
  char buffer[1024], bufferRez[1024];
  struct sockaddr_in serverAddr;
memset(bufferRez,0,1024);
memset(buffer,0,1024);
  clientSocket = socket(AF_INET, SOCK_STREAM, 0);				/* Create the socket. The three arguments are:  1) Internet domain 2) Stream socket 3) Default protocol (TCP in this case) */
  
  serverAddr.sin_family = AF_INET;						/* Address family = Internet */
  serverAddr.sin_port = 8888;							/* Set port number*/
  serverAddr.sin_addr.s_addr = inet_addr("127.0.0.1");				/* Set IP address to localhost */
   
  connect(clientSocket, (struct sockaddr *) &serverAddr, sizeof serverAddr);
  printf("Introduceti sirul de caractere:\n");
  scanf("%s",buffer);

  length=strlen(buffer);
  printf("Trimit %hu,%s\n",length,buffer);
  length=htons(strlen(buffer));
  send(clientSocket,&length,sizeof(uint16_t),0);
  length=ntohs(length);
  printf("Trimit %hu,%s\n",length,buffer);
  send(clientSocket,buffer,length*sizeof(char),0);
  
  recv(clientSocket, &length, sizeof(uint16_t), 0);						/* Read the message from the server into the buffer */
  length=ntohs(length);
  recv(clientSocket, bufferRez, length*sizeof(char), 0);						/* Read the message from the server into the buffer */

  printf("Data received: %d, %s\n",length,bufferRez);   					/* Print the received message */
  

  printf("Data received: %s\n",bufferRez);   					/* Print the received message */

memset(bufferRez,0,strlen(bufferRez));
memset(buffer,0,strlen(buffer));
  close(clientSocket);

  return 0;
}