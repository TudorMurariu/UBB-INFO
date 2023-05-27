<?php
	$con = mysqli_connect("localhost", "root", "","problema4php_db");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}
	$username = $_POST["username"];
	$password = $_POST["password"];
	
	// verificare patterne
	
	$pattern = '/^[a-zA-Z][a-zA-Z0-9-_\.]{1,30}$/';
	$patternPass = '/^(?=.*\d)(?=.*[a-z])(?=.*[A-Z])(?!.*\s).*$/';
	preg_match($pattern, $username, $matches, PREG_OFFSET_CAPTURE);
	preg_match($patternPass, $password, $matchesss, PREG_OFFSET_CAPTURE);
	if (count($matches)==0 || count($matchesss)==0){
		echo "U: ". count($matches);
		echo "P: ". count($matchesss);
		echo file_get_contents( "index.html" );
	}
	else{
		// verificare existenta user
		$sql = "SELECT count(*) FROM users WHERE username = '" . $username ."' AND password = '" . $password . "';";
		$result=mysqli_query($con,$sql);
		$row=mysqli_fetch_array($result);
		if ($row[0]==1){
			echo "user logat";
		}
		else {
			echo "nu exista user-ul";
			echo file_get_contents( "index.html" );
		}
	}
	mysqli_close($con);
?> 