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
    serverAddr.sin_port = htons(7083);							/* Set port number*/
    serverAddr.sin_addr.s_addr = inet_addr("172.30.112.207");				/* Set IP address to localhost */
   
    if(connect(clientSocket, (struct sockaddr *) &serverAddr, sizeof serverAddr) < 0) {
        printf("Eroare la conectarea la server\n");
        return 1;
    }

    int id_joc = -1, nr_client = -1;

    if(recv(clientSocket, &id_joc, sizeof(int), 0) < 0) {
        printf("Eroare la primirea id-ului de la server\n");
        return 1;
    }

    id_joc = ntohl(id_joc);
    printf("ID: %d\n", id_joc);

    if(recv(clientSocket, &nr_client, sizeof(int), 0) < 0){
        printf("Eroare la primirea nr_client de la server\n");
        return 1;
    }

    nr_client = ntohl(nr_client);
    printf("Suntem clientul cu nr: %d\n", nr_client);

    switch (nr_client)
    {
        //client 1
    case 1:
        while(1)
        {
            char litera;
            if(recv(clientSocket, &litera, sizeof(char), 0) < 0){
                printf("Eroare la primirea literei de la server\n");
                return 1;
            }

            // trimitere
            printf("Scrieti un cuvant care incepe cu litera %c :\n", litera);
            char cuvant[1024];
            scanf("%s", cuvant);

            int n = strlen(cuvant),nc;
            nc = htonl(n);
            if(send(clientSocket, &nc, sizeof(int), 0) < 0) {
                printf("Eroare la trimiterea lungimii sirului\n");
                return 1;
            }

            if(send(clientSocket, cuvant, sizeof(char) * n, 0) < 0) {
                printf("Eroare la trimiterea sirului\n");
                return 1;
            }

            char litera2[1];
            printf("Alegeti o noua litera:");
            scanf("%s", litera2);

            if(send(clientSocket, litera2, sizeof(char), 0) < 0) {
                printf("Eroare la trimiterea literei noi\n");
                return 1;
            }


        }
        break;

        //client 2
    case 2:
        while(1)
        {
            int n;
            if(recv(clientSocket, &n, sizeof(int), 0) < 0){
                printf("Eroare la primirea literei de la server\n");
                return 1;
            } 
            n = ntohl(n);

            if(n < 0)
                break;

            char cuvant[n+1];

            if(recv(clientSocket, cuvant, sizeof(char) * n, 0) < 0){
                printf("Eroare la primirea literei de la server\n");
                return 1;
            } 
            char litera;
            if(recv(clientSocket, &litera, sizeof(char), 0) < 0){
                printf("Eroare la primirea literei de la server\n");
                return 1;
            }   

            // // trimitere
            // printf("Scrieti un cuvant care incepe cu litera %c :\n", litera);
            // char cuvant[1024];
            // scanf("%s", cuvant);

            // int n = strlen(cuvant),nc;
            // nc = htonl(n);
            // if(send(clientSocket, &nc, sizeof(int), 0) < 0) {
            //     printf("Eroare la trimiterea lungimii sirului\n");
            //     return 1;
            // }

            // if(send(clientSocket, cuvant, sizeof(char) * n, 0) < 0) {
            //     printf("Eroare la trimiterea sirului\n");
            //     return 1;
            // }

            // char litera2[1];
            // printf("Alegeti o noua litera:");
            // scanf("%s", litera2);

            // if(send(clientSocket, litera2, sizeof(char), 0) < 0) {
            //     printf("Eroare la trimiterea literei noi\n");
            //     return 1;
            // }
        }
        break;

    default:
        printf("Eroare!");
        break;
    }
   

    close(clientSocket);


    return 0;
}