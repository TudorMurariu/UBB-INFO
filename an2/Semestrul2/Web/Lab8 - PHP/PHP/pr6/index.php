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
	<form  "<?php echo htmlspecialchars($_SERVER['PHP_SELF']);?>" method = "POST" enctype="multipart/form-data">
		<input type = 'hidden' name = 'tip' value ='login'>
		<label>Username</label><br>
		<input type="text" name='username'  placeholder="Enter Username" required><br>
		<label>Password</label><br>
		<input type="password" name='password'  placeholder="Enter Password" required>
		<br><br>
		<button type="submit" >Login </button>
	</form>
	</td>
	<td>
	<article>
	<h1>Clatite clasice – reteta simpla</h1>
	<p>Secretul celei mai bune retete de clatite? Sta in numarul de oua si raportul acestora cu restul de ingrediente. 
	Cu alte cuvinte? Cu cat pui oua cu mai multa dare de inima in compozitia pentru clatite, cu atat iti vor iesi mai moi, 
	mai pufoase si vei evita situatiile in care iti ies niste clatite gumoase sau chiar casante!</p>

	<p>Folosind cantitatile ingredientelor mentionate in reteta de clatite obtii, in medie, 4 portii de clatite. Daca astepti 
	musafiri si doresti sa pregatesti mai multe clatite, pur si simplu dubleaza sau tripleaza cantitatile de ingrediente, 
	mentinand mereu raportul.</p>
	<p>INGREDIENTE</p>
	<cite>
		<ul>
		<li>130 g faina</li>
		<li>2 oua mari</li>
		<li>300 ml lapte</li>
		<li>esenta vanilie</li>
		<li>2 linguri ulei</li>
		<li>o lingura zahar</li>
		<li>un praf sare</li>
	</ul>
	</cite>
	
	<p>
	MOD DE PREPARARE
		<ol>
		<li>Combina ouale cu laptele, ambele la temperatura camerei. Incorporeaza zaharul, eenta de vanilie si praful de sare, apoi adauga uleiul.</li>
		<li>Incorporeaza faina treptat si mixeaza aluatul pana la incorporarea fainii.</li>
		<li>Incinge tigaia de clatite la foc mediu si unge-o cu o picatura de ulei, pe care o intinzi cu o pensula de silicon.</li>
		<li>Intinde aluatul pentru clatite (cca un polonic/tigaie, in functie de cat de mare este tigaia de clatite) si raspandeste aluatul rapid in tigaie.</li>
		<li>Frige clatitele pe ambele parti timp de cca 1 minut, apoi scoate-le si umple-le cu gem, dulceata, crema de ciocolata, fructe si/sau inghetata.</li>
		</ol>
	</p>
	</article>
	<h3> Comentarii</h3>
	<?php
		$con = mysqli_connect("localhost", "root", "","lab8");
		if (!$con) {
			die('Could not connect: ' . mysqli_error());
		}
		
		$sql = "SELECT * FROM messages WHERE active = 1 ;";
		$result = mysqli_query($con, $sql);
		echo "<table class = 'myTable'>";
		while($row = mysqli_fetch_array($result)){
			
			echo "<tr><th><hr>" .$row["username"]. ":" ."</th></tr>";
			echo "<tr><td>" .$row["message"] . "</td> </tr>";
		}
		echo "</table>";
	?>
	<br>
	<h3> Adauga un nou comentariu </h3>
	<form "<?php echo htmlspecialchars($_SERVER['PHP_SELF']);?>" method = "POST">
		<input type = 'hidden'  name = 'tip' value = 'comentariu'>
		<input type = 'text' name = 'username' placeholder = 'Username'><br>
		<input type = 'text' name = 'message' placeholder = 'Comentariu'><br>
	
		<button type ='submit'>Submit</button>
	</form>
	</td>
	</tr>
	</table>
	</div>
	
	<?php
	function test_input($data) {
	  $data = trim($data);
	  $data = stripslashes($data);
	  $data = htmlspecialchars($data);
	  return $data;
	}
	
	$username = "";
	$password = "";
	$message = "";
	$tip = "";
	if ($_SERVER["REQUEST_METHOD"] == "POST") {
		$tip = test_input($_POST["tip"]);
		if ($tip =='login'){	
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
				if ($row[0]==1){
					header("Location: http://localhost/pr6/indexAdmin.php?username=".$username);
				}
				else {
					header("Location: http://localhost/pr6/index.php");
				}
					mysqli_close($con);
				}
			else{
				echo "erro";
			}			
		}
		else  {
			if ($tip =='comentariu'){
				$con = mysqli_connect("localhost", "root", "","lab8");
				if (!$con) {
					die('Could not connect: ' . mysqli_error());
				}
				$username = test_input($_POST["username"]);
				$message = test_input($_POST["message"]);
				
				$sql = "INSERT INTO messages(username, message, active) VALUES ('" . $username ."','" . $message . "', 0);";
				$result=mysqli_query($con,$sql);
				//header("Location: http://localhost/pr6/index.php");
				mysqli_close($con);
				
			}
		}
	}
	?>
</body>
</html>