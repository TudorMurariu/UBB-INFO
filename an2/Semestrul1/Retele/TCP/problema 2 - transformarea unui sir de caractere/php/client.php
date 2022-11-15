 <?php
 /* Enunt: Clientul trimite serverului o propozitie. Serverul ii raspunde serverului cu propozitia scrisa cu majuscule.
	
 Rulare in doua terminale diferite:
	php server.php
	php client.php
 */ 
 $s = socket_create(AF_INET, SOCK_STREAM, 0);
 socket_connect($s , "127.0.0.1",8888);
 $propozitie=readline("Dati propozitia: "); 
 socket_send($s, $propozitie, strlen($propozitie), 0);
 echo "Trimit la server propozitia ".$propozitie."\n";

 socket_recv($s,$rez,100,0);
 echo "Am primit de la server: $rez\n";
 socket_close($s);
?>