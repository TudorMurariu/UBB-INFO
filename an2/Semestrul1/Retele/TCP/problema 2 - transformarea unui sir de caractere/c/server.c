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
  uint16_t welcomeSocket, newSocket, nr, rez,i,length;
  char buffer[1024], bufferRez[1024];
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

  recv(newSocket, &length, sizeof(uint16_t), 0);						/* Read the message from the server into the buffer */
  length=ntohs(length);
  recv(newSocket, buffer, length*sizeof(char), 0);						/* Read the message from the server into the buffer */
	

  printf("Data received: %hu,%s\n",length,buffer);   					/* Print the received message */
  for (i = 0; buffer[i]!=0; i++) {
      if(buffer[i] >= 'a' && buffer[i] <= 'z') {
         buffer[i] = buffer[i] - 32;
      }
   }
  length=htons(strlen(buffer));
  printf("Trimit %hu,%s\n",strlen(buffer),buffer);
  send(newSocket,&length,sizeof(uint16_t),0);
  length=ntohs(length);
  send(newSocket,buffer,length*sizeof(char),0);
  
memset(bufferRez,0,1024);
memset(buffer,0,1024);
  }
  close(newSocket);
  close(welcomeSocket);

  return 0;
}