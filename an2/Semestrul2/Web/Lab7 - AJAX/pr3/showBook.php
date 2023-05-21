<?php
	$con = mysqli_connect("localhost", "root", "","carti_db");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}

	$idBook = $_GET["id"];
	$result = mysqli_query($con, "SELECT Titlu, Autor, Gen FROM carte WHERE cid=" .$idBook);
	
	$row = mysqli_fetch_array($result);
	echo "<input type='hidden' type='text' name='id' value = '" .$idBook. "'>";
	echo "<label>Titlu: </label>";
	echo "<input id='myTitle' onchange='myFunction()' type='text' name='titlu' value = '" .$row["Titlu"] . "'>";
	echo "<br>";
	echo "<br>";
	echo "<label>Autor: </label>";
	echo "<input id='myAutor' onchange='myFunction()' type='text' name='autor' value = '" .$row["Autor"] . "'>";
	echo "<br>";
	echo "<br>";
	echo "<label>Gen: </label>";
	echo "<input id='myGen' onchange='myFunction()' type='text' name='gen' value = '" .$row["Gen"] . "'>";
	echo "<br>";
	echo "<br>";
	echo "<input onchange='myFunction()' id='myBtn' type='submit' value ='Save' disabled>";
	mysqli_close($con);
?> 