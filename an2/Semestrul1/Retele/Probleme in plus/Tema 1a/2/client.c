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
    serverAddr.sin_port = htons(7099);							/* Set port number*/
    serverAddr.sin_addr.s_addr = inet_addr("172.30.113.5");				/* Set IP address to localhost */
   
    if(connect(clientSocket, (struct sockaddr *) &serverAddr, sizeof serverAddr) < 0) {
        printf("Eroare la conectarea la server\n");
        return 1;
    }

    printf("Dati un sir de caractere:\n");
    char buffer[1024];
    scanf("%[^\n]s", buffer);

    printf("Sirul de caractere este: %s\n", buffer);

    int n = strlen(buffer);

    int n_Server = htonl(n);
    if(send(clientSocket, &n_Server, sizeof(int), 0) < 0) {
        printf("Eroare la trimiterea lungimii\n");
        return 1;
    }

    if(send(clientSocket, buffer, sizeof(char) * n, 0) < 0) { 
        printf("Eroare la trimiterea sirului\n");
        return 1;
    } 

    int number_of_spaces;
    if(recv(clientSocket, &number_of_spaces, sizeof(int), 0) < 0) {
        printf("Eroare la primire\n");
        return 1;
    }

    number_of_spaces = ntohl(number_of_spaces);
    printf("Numarul de space uri: %d\n", number_of_spaces);

    close(clientSocket);


    return 0;
}