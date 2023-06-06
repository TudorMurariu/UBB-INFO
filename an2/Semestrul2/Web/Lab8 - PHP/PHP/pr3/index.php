<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Problema 3</title>
	<link rel="stylesheet" type="text/css" href="mystyle.css">
</head>
<body>
	<div class ="container">
		<h1>Login</h1>
		<hr>
		<form action = "<?php echo htmlspecialchars($_SERVER['PHP_SELF']);?>" method = "POST" enctype="multipart/form-data">
			<label>Username</label><br>
			<input type="text" name='username'  placeholder="Enter Username" required><br>
			<label>Password</label><br>
			<input type="password" name='password'  placeholder="Enter Password" required>
			<br><br>
			<button type="submit" >Login </button>
		</form>
		<hr>
	<div>
	
	<?php
	function test_input($data) {
	  $data = trim($data);
	  $data = stripslashes($data);
	  $data = htmlspecialchars($data);
	  return $data;
	}
	
	$username = "";
	$password = "";
	if ($_SERVER["REQUEST_METHOD"] == "POST") {
		$username = test_input($_POST["username"]);
		$password = test_input($_POST["password"]);
		
		if (preg_match("/^[a-zA-Z][a-zA-Z0-9-_\.]{1,30}$/",$username)) {
			$con = mysqli_connect("localhost", "root", "","lab8");
			if (!$con) {
				die('Could not connect: ' . mysqli_error());
			}
			$sql = "SELECT count(*) FROM users WHERE username = '" . $username ."' AND password = '" . $password . "';";
			$result=mysqli_query($con,$sql);
			$row=mysqli_fetch_array($result);
			
			if ($row[0] == 1){	
				header("Location: http://localhost/pr3/addNota.php");	
			}
			else {
				header("Location: http://localhost/pr3/index.php");
			}
			mysqli_close($con);
		}
	}
	?>
</body>
</html>