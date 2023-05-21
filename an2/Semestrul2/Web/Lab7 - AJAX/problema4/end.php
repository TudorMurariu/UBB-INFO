<?php
	if (!isset($_SESSION["tic"]))
  {
    session_start();
  }
	$endGame = false;
	
	
	for ($i =0;$i<3;$i++){
		if($_SESSION["tic"][$i*3+0] == 1 && $_SESSION["tic"][$i*3+1] == 1 && $_SESSION["tic"][$i*3+2] == 1)
			$endGame = true;
		if($_SESSION["tic"][0*3+$i] == 1 && $_SESSION["tic"][1*3+$i] == 1 && $_SESSION["tic"][2*3+$i] == 1)
			$endGame = true;
	}
	if($_SESSION["tic"][0] == 1 && $_SESSION["tic"][1*3+1] == 1 && $_SESSION["tic"][2*3+2] == 1)
			$endGame = true;
	
	echo $endGame;
?> 