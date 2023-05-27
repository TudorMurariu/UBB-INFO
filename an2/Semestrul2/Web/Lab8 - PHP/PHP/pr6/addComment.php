<?php
	function make_safe($variable) 
	{
		$variable = strip_tags(mysql_real_escape_string(trim($variable)));
		return $variable; 
	}

	$con = mysqli_connect("localhost", "root", "","problema6php_db");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}
	$username = make_safe($_GET["username"]);
	$message = make_safe($_GET["message"]);
	
	$sql = "INSERT INTO messages(username, message,active) VALUES ('" . $username ."','" . $message . "', 0);";
	$result=mysqli_query($con,$sql);
	$row=mysqli_fetch_array($result);
	header("Location: http://localhost/phpApp/pr6/index.php");
	mysqli_close($con);
?> 