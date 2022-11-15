 <?php
 /* Enunt: Clientul trimite serverului un numar. Serverul il primeste, il afiseaza pe ecran si trimite clientului dublul numarului.
	
 Rulare in doua terminale diferite:
	php server.php
	php client.php
 */
 $s = socket_create(AF_INET,SOCK_DGRAM,0);
 socket_bind($s,"127.0.0.1",1234);
 while(1)
 {
 socket_recvfrom($s,$nr,100,0,$from,$port);
 echo "Am primit $nr de la $from:$port\n";
 $rez=$nr*2;
 echo "Trimit la client ".$rez."\n";
 socket_sendto($s, $rez, 100, 0, $from,$port);
 }
 socket_close($s);
 ?>