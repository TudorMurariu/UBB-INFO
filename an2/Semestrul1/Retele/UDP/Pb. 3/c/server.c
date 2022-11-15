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
 
#define BUFLEN 512  
#define PORT 1234   
 
int main(void)
{
    struct sockaddr_in serveraddr, clientaddr;
     
    int s, i, slen = sizeof(clientaddr) , nr1,nr2, rez=0;
	char len[512],buf1[512],buf2[512], bufr[512];
    
    s=socket(AF_INET, SOCK_DGRAM, 0);
     
    memset((char *) &serveraddr, 0, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;
    serveraddr.sin_port = htons(PORT);
    serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
     
    bind(s , (struct sockaddr*)&serveraddr, sizeof(serveraddr));
     
    while(1)
    {
		rez=0;
        printf("Waiting for data...\n");   
		memset((char *) &len, 0, sizeof(len));		
        recvfrom(s, &len, sizeof(len), 0, (struct sockaddr *) &clientaddr, &slen);
        printf("Received packet from %s:%d\n", inet_ntoa(clientaddr.sin_addr), ntohs(clientaddr.sin_port));
		
		for(int i=0;i<atoi(len);i++)
		{
		memset((char *) &buf2, 0, sizeof(buf2));		
        recvfrom(s, &buf2, sizeof(buf2), 0, (struct sockaddr *) &clientaddr, &slen);
        printf("Received packet from %s:%d\n", inet_ntoa(clientaddr.sin_addr), ntohs(clientaddr.sin_port));
		nr2=atoi(buf2);       
        printf("Nr: %d\n" , nr2);             
		rez = rez + nr2;
        printf("Rez curent: %d\n" , rez);   
		}	
		
		memset((char *) &bufr, 0, sizeof(bufr));
		sprintf(bufr ,"%d" , rez); 
		printf("Trimit inapoi: %s\n",bufr);
		sendto(s, &bufr, strlen(bufr)*sizeof(char) , 0 , (struct sockaddr *) &clientaddr, slen);
    }
 
    close(s);
    return 0;
}