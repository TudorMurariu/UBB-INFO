<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Problema 6</title>
	<link rel="stylesheet" type="text/css" href="mystyle.css">
</head>
<body>

<div class ="container">
	<table>
	<tr>
	<td>
	<h2>Authentication</h2>
	<form action="index.php">
		<button type="submit" >Log out</button>
	</form>
	</td>
	<td>
	<h3>Comentarii in asteptare</h3>
	<?php
		$con = mysqli_connect("localhost", "root", "","lab8");
		if (!$con) {
			die('Could not connect: ' . mysqli_error());
		}


		$username = $_GET['username'];  

		$sql = "SELECT * FROM messages WHERE active = 0 AND username = '" . $username . "';";
		$result = mysqli_query($con, $sql);
		
		
		while($row = mysqli_fetch_array($result)){
			echo "<table class = 'mess' border = 1> <tr><td>";
			echo "<form action ='saveMessage.php'>";
				echo "<input type='hidden' name='id' value = '".$row["id"]."'>";
				echo "<button type='submit'> Aproba </button>";
			echo "</form>";
			echo "</td><td>";
			echo "<strong>" .$row["username"]. ":" ."</strong>";
			echo "<p>" .$row["message"] . "</p>";
			echo "</td></tr></table>";
		}
		
	?>
	
	</td>
	</tr>
	</table>
	</div>
</body>
</html>