 <?php
 /* Enunt: Clientul trimite serverului o propozitie. Serverul ii raspunde serverului cu propozitia scrisa cu majuscule.
	
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
 $rez="";
 socket_recv($c,$propozitie,100,0);
 echo "Am primit de la client propozitia $propozitie\n";
 $rez=strtoupper($propozitie);
 socket_send($c, $rez, 100, 0);
 echo "Am trimis la client $rez\n";
 }
 socket_close($s);
 ?>