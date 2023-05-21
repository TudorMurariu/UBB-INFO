<?php
	$con = mysqli_connect("localhost", "root", "","carti_db");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}

	$result = mysqli_query($con, "SELECT cid FROM carte");
	while($row = mysqli_fetch_array($result)){
		echo "<li onclick='showBook(".$row[0].")'>" .$row[0] ."</li>";
	}
	mysqli_close($con);
?> 