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

int main()
{
    int clientSocket ,rez;
    char buffer[1024];
    struct sockaddr_in serverAddr;

    clientSocket = socket(AF_INET, SOCK_STREAM, 0);				/* Create the socket. The three arguments are:  1) Internet domain 2) Stream socket 3) Default protocol (TCP in this case) */
    if (clientSocket < 0) {
        printf("Eroare la crearea socketului client\n");
        return 1;
    }

    memset(&serverAddr, 0, sizeof(serverAddr));
    serverAddr.sin_family = AF_INET;						/* Address family = Internet */
    serverAddr.sin_port = htons(7070);							/* Set port number*/
    serverAddr.sin_addr.s_addr = inet_addr("192.168.0.73");				/* Set IP address to localhost */
   
    if(connect(clientSocket, (struct sockaddr *) &serverAddr, sizeof serverAddr) < 0) {
        printf("Eroare la conectarea la server\n");
        return 1;
    }

    int n,nc;
    printf("n :\n");
    scanf("%d",&n);
    int v[n+1];


    printf("vectorul:\n");
    for(int i=0;i<n;++i)
    {
        scanf("%d",&v[i]);
        v[i] = htonl(v[i]);
    }

    nc = n;
    nc = htonl(nc);
    if(send(clientSocket,&nc,sizeof(int),0) < 0){
        printf("Eroare la trimiterea lui n\n");
        return 1;
    }

    if(send(clientSocket, &v ,sizeof(int)*n ,0) < 0){
        printf("Eroare la trimiterea vectorului\n");
        return 1;
    }

    printf("Am trimis datele la server\n");

    if(recv(clientSocket, &rez, sizeof(int), 0) < 0){
        printf("Eroare la primire\n");
        return 1;
    }

    printf("Suma numerelor din vector este : %d\n", ntohl(rez));  

    close(clientSocket);

    return 0;
}