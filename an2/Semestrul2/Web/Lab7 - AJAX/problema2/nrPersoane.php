<?php
	$con = mysqli_connect("localhost", "root", "","persoane_db");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}

	$pageNr = $_GET["page"];
	$pageLimit = 3;
	$result = mysqli_query($con, "SELECT * FROM persoana limit " .$pageNr ."," . $pageLimit .";");
	
	echo $result->num_rows;
	
	mysqli_close($con);
?> 