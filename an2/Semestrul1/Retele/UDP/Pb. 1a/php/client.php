<?php
 /* Enunt: Clientul trimite serverului un numar. Serverul il primeste si il afiseaza pe ecran.
	
 Rulare in doua terminale diferite:
	php server.php
	php client.php
 */
 $s = socket_create(AF_INET, SOCK_DGRAM, 0);
 $nr=readline("Dati un numar: ");
 socket_sendto($s, $nr, 100, 0, "127.0.0.1", 1234);
 socket_close($s);
?>