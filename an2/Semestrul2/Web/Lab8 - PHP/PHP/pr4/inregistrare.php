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
			$to = "teofanaenachioiu@yahoo.com";
			$subject = "This is subject";
         
			$message = "<b>This is HTML message.</b>";
			$message .= "<h1>This is headline.</h1>";
			ini_set('SMTP', "smtp.gmail.com");
			ini_set('smtp_port', "25");
			ini_set('sendmail_from', "teofanaenachioiu@yahoo.com");
         //$header = "From:abc@somedomain.com \r\n";
         //$header .= "Cc:afgh@somedomain.com \r\n";
         $header = "Content-type: text/html\r\n";
         
         $retval = mail ($to,$subject,$message,$header);
         
         if( $retval == true ) {
            echo "Message sent successfully...";
         }else {
            echo "Message could not be sent...";
         }
		}
	}
	mysqli_close($con);
?> 