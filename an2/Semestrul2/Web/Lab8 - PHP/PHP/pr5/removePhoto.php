<?php
	$con = mysqli_connect("localhost", "root", "","lab8");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}
	$id = $_POST["id"];
	$sqlUpdate = "DELETE FROM imagesuploaded WHERE id = '$id' ";
	if ($con->query($sqlUpdate) === TRUE) {
		header("Location: http://localhost/pr5/indexMyPhotos.php");
	} else {
		echo "Error: " . $sqlUpdate . "<br>" . $con->error;
	}

?>