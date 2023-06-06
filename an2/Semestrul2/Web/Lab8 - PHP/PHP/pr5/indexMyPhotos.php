<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Problema 5</title>
	<link rel="stylesheet" type="text/css" href="pageStyle.css">
</head>
<body>
	<div class="header">
	  <h1>Photopedia</h1>
	</div>

	<div class="topnav">
	  <a href="indexMyPhotos.php">Profile</a>
	  <a href="addPhoto.php">Add photos</a>
	  <a href="viewPhotos.php">Feed</a>
	  <a href="index.php">Logout</a>
	</div>

	<div class='container'>
		<?php
		$con = mysqli_connect("localhost", "root", "","lab8");
		if (!$con) {
			die('Could not connect: ' . mysqli_error());
		}
		
		// USERNAME
		$sql="SELECT username FROM user_curent";
		$result = mysqli_query($con, $sql);
		$username= '';
		while($row = mysqli_fetch_array($result)) {
			$username = $row["username"];
		}
		
		// POZE
		$sql2 = "SELECT * FROM imagesuploaded WHERE username ='". $username. "'";
		$result = mysqli_query($con, $sql2);
		while($row = mysqli_fetch_array($result)) {
			echo "<img src = 'images/" . $row["image"] . "'>";
			echo "<p>" . $row["image_text"] . "</p>";
			echo "<form action ='removePhoto.php' method = 'POST'> ";
			echo "<input type = 'hidden' name = 'id' value ='" . $row['id'] ."'>";
			echo "<button type = 'submit'>Delete</button>";
			echo "</form>";
		}
		?>
	</div>
	

</body>
</html>