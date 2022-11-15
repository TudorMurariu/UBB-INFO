 <?php
 /* Enunt: Clientul trimite serverului doua numere. Serverul le primeste, le afiseaza pe ecran si trimite clientului suma lor.
	
 Rulare in doua terminale diferite:
	php server.php
	php client.php
 */ 
 $s = socket_create(AF_INET, SOCK_STREAM, 0);
 socket_connect($s , "127.0.0.1",8888);
 $nr1=readline("Dati primul numar: ");
 socket_send($s, $nr1, 100, 0);
 $nr2=readline("Dati al doilea numar: ");
 socket_send($s, $nr2, 100, 0);
 socket_recv($s,$rez,100,0);
 echo "Am primit de la server: $rez\n";
 socket_close($s);
?>