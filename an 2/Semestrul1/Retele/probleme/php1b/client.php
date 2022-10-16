 <?php
 /* Enunt: Un server primeste numere de la clienti si le afiseaza pe ecran. Serverul trimite inapoi fiecarui client dublul numarului.
	
 Rulare in doua terminale diferite:
	php server.php
	php client.php
 */ 
 $s = socket_create(AF_INET, SOCK_STREAM, 0);
 socket_connect($s , "127.0.0.1",8888);
 $nr=readline("Dati un numar: ");
 socket_send($s, $nr, 100, 0);
 socket_recv($s,$rez,100,0);
 echo "Am primit de la server: $rez\n";
 socket_close($s);
?>