 <?php
 /* Enunt: Clientul trimite serverului doua numere. Serverul le primeste, le afiseaza pe ecran si trimite clientului suma lor.
	
 Rulare in doua terminale diferite:
	php server.php
	php client.php
 */
 $s = socket_create(AF_INET,SOCK_DGRAM,0);
 socket_bind($s,"127.0.0.1",1234);
 while(1)
 {
 socket_recvfrom($s,$nr1,100,0,$from,$port);
 echo "Am primit $nr1 de la $from:$port\n";
 socket_recvfrom($s,$nr2,100,0,$from,$port);
 echo "Am primit $nr2 de la $from:$port\n";
 $rez=$nr1+$nr2;
 echo "Trimit la client ".$rez."\n";
 socket_sendto($s, $rez, 100, 0, $from,$port);
 }
 socket_close($s);
 ?>