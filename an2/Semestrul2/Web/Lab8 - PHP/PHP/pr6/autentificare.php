<?php
	$con = mysqli_connect("localhost", "root", "","problema6php_db");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}
	$username = $_POST["username"];
	$password = $_POST["password"];
	
	// verificare patterne
	
	$pattern = '/^[a-zA-Z][a-zA-Z0-9-_\.]{1,30}$/';
	preg_match($pattern, $username, $matches, PREG_OFFSET_CAPTURE);
	if (count($matches)==0){
		echo "U: ". count($matches);
		echo file_get_contents( "index.html" );
	}
	else{
		// verificare existenta user
		$sql = "SELECT count(*) FROM users WHERE username = '" . $username ."' AND password = '" . $password . "';";
		$result=mysqli_query($con,$sql);
		$row=mysqli_fetch_array($result);
		if ($row[0]==1){
			header("Location: http://localhost/phpApp/pr6/indexAdmin.php");
		}
		else {
			echo file_get_contents( "index.php" );
		}
	}
	mysqli_close($con);
?> 