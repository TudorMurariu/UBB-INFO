 <?php
 /* Enunt: Clientul trimite serverului doua numere. Serverul le primeste, le afiseaza pe ecran si trimite clientului suma lor.
	
 Rulare in doua terminale diferite:
	php server.php
	php client.php
 */ 
 $s = socket_create(AF_INET,SOCK_STREAM,0);
 socket_bind($s,"127.0.0.1",8888);
 socket_listen ($s,5);
 while(1)
 {
 $c = socket_accept($s);
 socket_recv($c,$nr1,100,0);
 echo "Am primit de la client nr1=$nr1\n";
 socket_recv($c,$nr2,100,0);
 echo "Am primit de la client nr2=$nr2\n";
 $rez=$nr1+$nr2;
 socket_send($c, $rez, 100, 0);
 echo "Am trimis la client $rez\n";
 }
 socket_close($s);
 ?>