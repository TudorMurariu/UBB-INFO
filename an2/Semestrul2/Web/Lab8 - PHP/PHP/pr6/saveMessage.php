<?php
	$con = mysqli_connect("localhost", "root", "","lab8");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}
	$id = $_GET["id"];
	$sql = "UPDATE messages SET active = 1 WHERE id = " . $id .";";
	mysqli_query($con,$sql);
	//echo 'da';
	header("Location: http://localhost/pr6/indexAdmin.php");
	mysqli_close($con);
?> 