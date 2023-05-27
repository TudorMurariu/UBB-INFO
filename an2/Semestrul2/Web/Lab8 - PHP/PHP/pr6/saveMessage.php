<?php
	$con = mysqli_connect("localhost", "root", "","problema6php_db");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}
	$id = $_GET["id"];
	$sql = "UPDATE messages SET active = 1 WHERE id = " . $id .";";
	$result=mysqli_query($con,$sql);
	$row=mysqli_fetch_array($result);
	//echo 'da';
	header("Location: http://localhost/phpApp/pr6/indexAdmin.php");
	mysqli_close($con);
?> 