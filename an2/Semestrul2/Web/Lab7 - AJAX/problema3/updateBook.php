<?php
	$con = mysqli_connect("localhost", "root", "","carti_db");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}
	$titlu = $_POST["titlu"];
	$autor = $_POST["autor"];
	$gen = $_POST["gen"];
	$id = $_POST["id"];
	$sql =  "UPDATE carte SET Titlu = '" .$titlu ."', Autor = '" .$autor ."', Gen = '" .$gen ."' WHERE cid = '".$id ."';";
	if (mysqli_query($con, $sql)) {
		echo "Record updated successfully";
	} else {
		echo "Error updating record: " . mysqli_error($con);
	}
	
	mysqli_close($con);
?> 