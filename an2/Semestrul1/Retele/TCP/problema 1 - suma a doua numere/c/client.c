/*
Clientul citeste 2 numere de la tastatura si i le trimite serverului, acesta trimitandu-i inapoi clientului suma lor.
gcc client.c -o client
*/
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <string.h>
 
int main() {
  int c;
  struct sockaddr_in server;
  uint16_t a, b, suma;
  
  c = socket(AF_INET, SOCK_STREAM, 0);
  if (c < 0) {
    printf("Eroare la crearea socketului client\n");
    return 1;
  }
  
  memset(&server, 0, sizeof(server));
  server.sin_port = htons(8888);
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = inet_addr("127.0.0.1");
  
  if (connect(c, (struct sockaddr *) &server, sizeof(server)) < 0) {
    printf("Eroare la conectarea la server\n");
    return 1;
  }
  
  printf("a = \n");
  scanf("%hu", &a);
  printf("b = \n");
  scanf("%hu", &b);
  a = htons(a);
  b = htons(b);
  send(c, &a, sizeof(a), 0);
  send(c, &b, sizeof(b), 0);
  recv(c, &suma, sizeof(suma), 0);
  suma = ntohs(suma);
  printf("Suma este %hu\n", suma);
  close(c);
}