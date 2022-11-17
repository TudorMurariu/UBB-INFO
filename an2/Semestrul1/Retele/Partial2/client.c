/* Enunt: Clientul trimite serverului un numar. Serverul il primeste, il afiseaza pe ecran si trimite clientului dublul numarului.

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
 
#define SERVER "172.30.112.200"
#define BUFLEN 512   
#define PORT 8800   

int main(void)
{
    struct sockaddr_in serveraddr;
    int s, i, slen=sizeof(serveraddr), clientaddr,nr,rez;
	char buf[512],bufr[512];
 
    s=socket(AF_INET, SOCK_DGRAM, 0);
 
    memset((char *) &serveraddr, 0, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;
    serveraddr.sin_port = htons(PORT);
    serveraddr.sin_addr.s_addr = inet_addr(SERVER);
    
    printf("Trimiteti numarul : ");
    scanf("%s",&buf);
    sendto(s, &buf, strlen(buf)*sizeof(char) , 0 , (struct sockaddr *) &serveraddr, slen);
 
    recvfrom(s, &bufr, sizeof(bufr), 0, (struct sockaddr *) &clientaddr, &slen);
    printf("Am primit de la server: %s\n" , bufr);        
		
    close(s);
    return 0;
}