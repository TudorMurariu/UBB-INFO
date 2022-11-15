<?php
 /* Enunt: Clientul trimite serverului un numar. Serverul il primeste si il afiseaza pe ecran.
	
 Rulare in doua terminale diferite:
	php server.php
	php client.php
 */
 $s = socket_create(AF_INET,SOCK_DGRAM,0);
 socket_bind($s,"127.0.0.1",1234);
 while(1)
 {
 socket_recvfrom($s,$buf,100,0,$from,$port);
 echo "Received $buf from remote address $from and remote port $port\n";
 }
 socket_close($s);
 ?>