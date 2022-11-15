 <?php
 /* Enunt: Clientul trimite serverului un numar. Serverul il primeste, il afiseaza pe ecran si trimite clientului dublul numarului.
	
 Rulare in doua terminale diferite:
	php server.php
	php client.php
 */
 $s = socket_create(AF_INET, SOCK_DGRAM, 0);
 $nr=readline("Dati un numar: ");
 socket_sendto($s, $nr, 100, 0, "127.0.0.1", 1234);
 echo "Trimit la server ".$nr."\n";
 socket_recvfrom($s,$rez,100,0,$from,$port);
 echo "Am primit de la server: ".$rez."\n";
 socket_close($s);
?>