/* Enunt: Clientul trimite serverului un sir de numere. Serverul le primeste, le afiseaza pe ecran si trimite clientului suma lor.

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
    int s, i, slen=sizeof(serveraddr), clientaddr,nr,rez;
	char len[512],buf[512],bufr[512];
 
    s=socket(AF_INET, SOCK_DGRAM, 0);
 
    memset((char *) &serveraddr, 0, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;
    serveraddr.sin_port = htons(PORT);
    serveraddr.sin_addr.s_addr = inet_addr(SERVER);
    
    printf("Trimiteti lungimea sirului: ");
    scanf("%s",&len);
    sendto(s, &len, strlen(len)*sizeof(char) , 0 , (struct sockaddr *) &serveraddr, slen);
	
	for(int i=0; i<atoi(len);i++)
	{
	printf("Trimiteti sir[%d]: \n",i);
    scanf("%s",&buf);
    sendto(s, &buf, strlen(buf)*sizeof(char) , 0 , (struct sockaddr *) &serveraddr, slen);
	}
 
    recvfrom(s, &bufr, sizeof(bufr), 0, (struct sockaddr *) &clientaddr, &slen);
    printf("Am primit de la server: %s\n" , bufr);        
		
    close(s);
    return 0;
}