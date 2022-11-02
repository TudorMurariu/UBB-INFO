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
    serverAddr.sin_port = htons(12301);							/* Set port number*/
    serverAddr.sin_addr.s_addr = inet_addr("192.168.0.73");				/* Set IP address to localhost */
   
    if(connect(clientSocket, (struct sockaddr *) &serverAddr, sizeof serverAddr) < 0) {
        printf("Eroare la conectarea la server\n");
        return 1;
    }

    printf("Dati primul sir de caractere:\n");
    char buffer1[1024];
    scanf("%s", buffer1);

    printf("Dati al doilea sir de caractere:\n");
    char buffer2[1024];
    scanf("%s", buffer2);

    int n = strlen(buffer1);
    int nc = htonl(n), m = n;

    send(clientSocket, &nc, sizeof(int), 0);
    if(send(clientSocket, buffer1, sizeof(char) * n, 0) < 0) { 
        printf("Eroare la trimiterea sirului1\n");
        return 1;
    } 

    n = strlen(buffer2);
    nc = htonl(n);
    send(clientSocket, &nc, sizeof(int), 0);
    if(send(clientSocket, buffer2, sizeof(char) * n, 0) < 0) { 
        printf("Eroare la trimiterea sirului2\n");
        return 1;
    } 

    char new_buffer[2048];
    if(recv(clientSocket, new_buffer, sizeof(char) * (m + n), 0) < 0) {
        printf("Eroare la primirea sirului\n");
        return 1;
    }

    printf("Sirul este: %s\n", new_buffer);

    close(clientSocket);


    return 0;
}