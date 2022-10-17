 <?php
 /* Enunt: Un server primeste numere de la clienti si le afiseaza pe ecran. Serverul trimite inapoi fiecarui client dublul numarului.
	
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
 socket_recv($c,$nr,100,0);
 echo "Am primit de la client $nr\n";
 $rez=2*$nr;
 socket_send($c, $rez, 100, 0);
 echo "Am trimis la client $rez\n";
 }
 socket_close($s);
 ?>