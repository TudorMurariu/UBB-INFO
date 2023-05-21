<?php
	if (!isset($_SESSION["tic"]))
  {
    session_start();
  }
	
	$row = $_GET["row"];
	$col = $_GET["col"];
	
	$_SESSION["tic"][$row * 3 + $col] = 1;

?>