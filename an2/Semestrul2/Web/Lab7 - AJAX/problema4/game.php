<?php
	if (!isset($_SESSION["tic"]))
  {
    session_start();
  }
	
	do{
		$poz = rand (0,8); 
	}while($_SESSION["tic"][$poz] != 0);
	
	$_SESSION["tic"][$poz] = 1;
	echo $poz;
?>