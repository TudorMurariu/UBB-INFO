<?php
	$con = mysqli_connect("localhost", "root", "","problema4php_db");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}
	$username = $_POST["username"];
	$password = $_POST["password"];
	$email = $_POST["email"];
	
	// verificare patterne
	
	$pattern = '/^[a-zA-Z][a-zA-Z0-9-_\.]{1,30}$/';
	$patternPass = '/^(?=.*\d)(?=.*[a-z])(?=.*[A-Z])(?!.*\s).*$/';
	preg_match($pattern, $username, $matches, PREG_OFFSET_CAPTURE);
	preg_match($patternPass, $password, $matchesss, PREG_OFFSET_CAPTURE);
	if (count($matches)==0){
		echo file_get_contents( "index.html" );
		echo 'Auuch username';
	}
	if (count($matchesss) == 0){
		echo file_get_contents( "index.html" );
		echo 'Auuch password';
	}
	else{
		// verificare existenta user
		$sql = "SELECT count(*) FROM users WHERE username = '" . $username ."' AND password = '" . $password . "';";
		$result=mysqli_query($con,$sql);
		$row=mysqli_fetch_array($result);
		if ($row[0]==1){
			echo file_get_contents( "index.html" );
			echo 'Username existent';
			
		}
		else {
			// the message
			$msg = "First line of text\nSecond line of text";
			$msg = wordwrap($msg,70);

			// send email
			mail($email,"Confirma - Exercitiu PHP",$msg);
			
			$sql = "INSERT INTO users(username, password, email) VALUES ('". $username ."', '" . $password . "','" .$email."');";
			if ($con->query($sql) === TRUE) {
				echo "User intregistrat";
				echo file_get_contents( "index.html" );
			} else {
				echo "Error: " . $sql . "<br>" . $con->error;
				echo file_get_contents( "index.html" );
			}
		
		}
	}
	mysqli_close($con);
?> 