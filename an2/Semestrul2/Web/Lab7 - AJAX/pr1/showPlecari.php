<?php
	$con = mysqli_connect("localhost", "root", "","trenuri_db");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}

	$result = mysqli_query($con, "SELECT distinct locPlecare FROM trenuri");

	while($row = mysqli_fetch_array($result)){
		echo "<option onclick='getSosiri(this)' value=" . $row[0] . ">" . $row[0] . "</option>";
	}
	mysqli_close($con);
?> 