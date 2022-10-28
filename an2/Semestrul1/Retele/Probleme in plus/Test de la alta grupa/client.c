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
    serverAddr.sin_port = htons(12300);							/* Set port number*/
    serverAddr.sin_addr.s_addr = inet_addr("192.168.0.73");				/* Set IP address to localhost */
   
    if(connect(clientSocket, (struct sockaddr *) &serverAddr, sizeof serverAddr) < 0) {
        printf("Eroare la conectarea la server\n");
        return 1;
    }

    printf("Dati un sir de caractere:\n");
    char buffer[1024];
    scanf("%s", buffer);

    printf("Sirul de caractere este: %s\n", buffer);

    int n = strlen(buffer);
    int nc = htonl(n);

    send(clientSocket, &nc, sizeof(int), 0);
    if(send(clientSocket, buffer, sizeof(char) * n, 0) < 0) { 
        printf("Eroare la trimiterea sirului\n");
        return 1;
    } 


    int guess = -1, raspuns;
    do
    {
        printf("Ghiciti : ");
        scanf("%d", &guess);
        guess = htonl(guess);
        if(send(clientSocket, &guess, sizeof(int), 0) < 0){
            printf("Eroare la trimiterea numarului ghicit\n");
            return 1;
        }
        if(recv(clientSocket, &raspuns, sizeof(int), 0) < 0){
            printf("Eroare la primirea raspunsului\n");
            return 1;
        }
        raspuns = ntohl(raspuns);
        printf("Raspuns: %d\n", raspuns);

        if(raspuns == -2)
        {
            printf("Ati gresit de 5 ore mai vreti sa jucati?(1 - da, 0 - nu)\n");
            scanf("%d", &raspuns);
            if(raspuns == 0)
            {
                raspuns = htonl(raspuns);
                send(clientSocket, &raspuns, sizeof(int), 0);
                break;
            }
            else
            {
                raspuns = htonl(raspuns);
                send(clientSocket, &raspuns, sizeof(int), 0);
                raspuns = -1;
                continue;
            }
        }

    }while(raspuns < 0);
    
    if(raspuns >= 0)
        printf("Raspuns corect!\n %d este numarul de incercari.", raspuns+1);

    close(clientSocket);


    return 0;
}