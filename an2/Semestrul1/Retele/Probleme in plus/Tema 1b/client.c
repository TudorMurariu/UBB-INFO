/*
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
    struct sockaddr_in serverAddr;

    clientSocket = socket(AF_INET, SOCK_STREAM, 0);				/* Create the socket. The three arguments are:  1) Internet domain 2) Stream socket 3) Default protocol (TCP in this case) */
    if (clientSocket < 0) {
        printf("Eroare la crearea socketului client\n");
        return 1;
    }

    memset(&serverAddr, 0, sizeof(serverAddr));
    serverAddr.sin_family = AF_INET;						/* Address family = Internet */
    serverAddr.sin_port = htons(6768);							/* Set port number*/
    serverAddr.sin_addr.s_addr = inet_addr("192.168.0.73");				/* Set IP address to localhost */
   
    if(connect(clientSocket, (struct sockaddr *) &serverAddr, sizeof serverAddr) < 0) {
        printf("Eroare la conectarea la server\n");
        return 1;
    }

    int n;
    scanf("%d", &n);

    n = htonl(n);
    if(send(clientSocket, &n, sizeof(int), 0) < 0) {
        printf("Eroare la trimiterea numarului\n");
        return 1;
    }

    if(recv(clientSocket, &n, sizeof(int), 0) < 0) {
        printf("Eroare la primire\n");
        return 1;
    }

    n = ntohl(n);
    printf("Numarul primit: %d\n",n);

    close(clientSocket);


    return 0;
}