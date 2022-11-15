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
 
#define SERVER "127.0.0.1"
#define BUFLEN 512  
#define PORT 1234   

int main(void)
{
    struct sockaddr_in serveraddr;
    int s, i, slen=sizeof(serveraddr),nr;
	char buf[512];
 
    s=socket(AF_INET, SOCK_DGRAM, 0);
 
    memset((char *) &serveraddr, 0, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;
    serveraddr.sin_port = htons(PORT);
    serveraddr.sin_addr.s_addr = inet_addr(SERVER);
    
    printf("Trimiteti numarul : ");
    scanf("%s",&buf);
    sendto(s, &buf, sizeof(buf) , 0 , (struct sockaddr *) &serveraddr, slen);
 
    close(s);
    return 0;
}