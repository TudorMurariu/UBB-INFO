#include<sys/types.h>
#include<sys/socket.h>
#include<stdio.h>
#include<netinet/in.h>
#include<netinet/ip.h>
#include<string.h>


void deservire_client(int c){
    char a[100],b[100];
    uint16_t m,n;

    int u;

    uint16_t g;

    recv(c, &a, sizeof(a), MSG_WAITALL);
    recv(c,&m,sizeof(m),MSG_WAITALL);
    recv(c,&n,sizeof(n),MSG_WAITALL);
    m=ntohs(m);
    n=ntohs(n);
printf("Am primit %s, %d, %d\n",a,m,n);
	int i,p=0;

    memset(&b,0,sizeof(b));
    if(m<strlen(a)&&(m+n)<strlen(a)&&n<strlen(a))
	for(i=n;i<(m+n);i++){
			b[p]=a[i];
			p++;
			}
	else strcpy(b,"incorect\n");

    send(c, b, strlen(b), 0);
printf("Am trimis %s\n",b);
    close(c);
}

    int main(int argc,const char* argv[]){
        int s;
        struct sockaddr_in server,client;
        int c,l;

        s=socket(AF_INET,SOCK_STREAM,0);
        if(s<0){
             printf("Eroare la crearea socketului server\n");
             return 1;
        }

		int port;
        port=atoi(argv[1]);

        memset(&server,0,sizeof(server));
        server.sin_port=htons(port);
        server.sin_family=AF_INET;
        server.sin_addr.s_addr=INADDR_ANY;


        if(bind(s,(struct sockaddr *)&server,sizeof(server))<0)         {
              printf("Eroare la bind\n");
              return 1;
        }
		
		listen(s,5);

        l=sizeof(client);
        memset(&client,0,sizeof(client));

        while(1){
                c=accept(s,(struct sockaddr *)&client,&l);
                printf("S-a conectat un client.\n");
                if(fork()==0){
                        //fiu
                        deservire_client(c);
                        return 0;
                }
        }
}


