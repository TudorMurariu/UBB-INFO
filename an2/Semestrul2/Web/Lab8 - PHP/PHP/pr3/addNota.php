<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Problema 3</title>
</head>
<body>
	<div class ="container">
		<h1>Adaugare Nota</h1>
		<hr>
	
		Student: 
		<select name ="idStudent" form="notare">
		<?php
			$con = mysqli_connect("localhost", "root", "","lab8");
			if (!$con) {
				die('Could not connect: ' . mysqli_error());
			}
			$sql = "SELECT * FROM students";
			$result = mysqli_query($con, $sql);
	
			while($row = mysqli_fetch_array($result)){
				echo "<option  value = '" .$row["id"] ."'>" .$row["nume"] ."</option>";
				}
		?>
		</select>
		<br><br>
		
		Disciplina: 
		<select name ='idDisciplina' form="notare">
		<?php
			$con = mysqli_connect("localhost", "root", "","lab8");
			if (!$con) {
				die('Could not connect: ' . mysqli_error());
			}
			$sql = "SELECT * FROM subjects";
			$result = mysqli_query($con, $sql);
	
			while($row = mysqli_fetch_array($result)){
				echo "<option   value = '" .$row["id"] ."'>" .$row["nume"] ."</option>";
				}
		?>
		</select>
		<br><br>
		
		Nota:
		<select name="idNota" form="notare">
		  <option value="1">1</option>
		  <option value="2">2</option>
		  <option value="3">2</option>
		  <option value="4">4</option>
		  <option value="5">5</option>
		  <option value="6">6</option>
		  <option value="7">7</option>
		  <option value="8">8</option>
		  <option value="9">9</option>
		  <option value="10">10</option>
		</select>
		<br>
		<br>
		<form id ="notare" action = "<?php echo htmlspecialchars($_SERVER['PHP_SELF']);?>" method = "POST" enctype="multipart/form-data">
			  <input type="submit" value="Noteaza">
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
	
	$idS = "";
	$idD = "";
	$nota = "";
	if ($_SERVER["REQUEST_METHOD"] == "POST") {
		$idS = test_input($_POST["idStudent"]);
		$idD = test_input($_POST["idDisciplina"]);
		$nota = test_input($_POST["idNota"]);
		$con = mysqli_connect("localhost", "root", "","lab8");
		if (!$con) {
			die('Could not connect: ' . mysqli_error());
		}
		$sql = "INSERT INTO notare(idS, idD, nota) VALUES ('$idS', '$idD', $nota)";
		if ($con->query($sql) === TRUE) {
			header("Location: http://localhost/pr3/showNote.php");
		} else {
			echo "Error: " . $sql . "<br>" . $con->error;
		}
		mysqli_close($con);
	}
	?>
</body>
</html>