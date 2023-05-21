<?php
	$con = mysqli_connect("localhost", "root", "","persoane_db");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}

	$pageNr = $_GET["page"];
	$pageLimit = 3;
	$result = mysqli_query($con, "SELECT * FROM persoana limit " .$pageNr ."," . $pageLimit .";");

	echo "<tr><th>Nume</th><th>Prenume</th><th>Telefon</th><th>Email</th></tr>";
	while($row = mysqli_fetch_array($result)){
		echo "<tr><td>" .$row["Nume"] ."</td><td>" .$row["Prenume"] ."</td><td>".$row["Telefon"] ."</td><td>" .$row["Email"] . "</td></tr>";
	}
	mysqli_close($con);
?> 