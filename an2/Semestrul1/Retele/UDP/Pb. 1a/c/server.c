/* Enunt: Clientul trimite serverului un numar. Serverul il primeste si il afiseaza pe ecran.

Compilare: 
	gcc server.c -o server
	gcc client.c -o client
	
Rulare in doua terminale diferite:
	./server
	./client
*/
#include<stdio.h> 
#include<string.h> 
#include<stdlib.h> 
#include<arpa/inet.h>
#include<sys/socket.h>
 
#define BUFLEN 512  
#define PORT 1234   
 
int main(void)
{
    struct sockaddr_in serveraddr, clientaddr;
     
    int s, i, slen = sizeof(clientaddr) ,nr;
	char buf[512];
    
    s=socket(AF_INET, SOCK_DGRAM, 0);
     
    memset((char *) &serveraddr, 0, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;
    serveraddr.sin_port = htons(PORT);
    serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
     
    bind(s , (struct sockaddr*)&serveraddr, sizeof(serveraddr));
     
    while(1)
    {
        printf("Waiting for data...\n");     
        recvfrom(s, buf, sizeof(buf), 0, (struct sockaddr *) &clientaddr, &slen);
		nr=atoi(buf);
        printf("Received packet from %s:%d\n", inet_ntoa(clientaddr.sin_addr), ntohs(clientaddr.sin_port));
        printf("Data: %d\n" , nr);        
    }
 
    close(s);
    return 0;
}