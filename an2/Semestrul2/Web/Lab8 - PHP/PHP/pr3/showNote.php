<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Problema 3</title>
</head>
<body>
	<div class ="container">
		<h1>Afisare Note</h1>
		<hr>
	
		<table>
		<tr>
		<th>Nume</th>
		<th>Denumire</th>
		<th>Nota</th>
		</tr>
		<?php
			$con = mysqli_connect("localhost", "root", "","problema3php_db");
			if (!$con) {
				die('Could not connect: ' . mysqli_error());
			}
			$sql = "SELECT st.nume as 'nume', sb.denumire as 'denumire', n.nota as nota FROM students st, subjects sb, notare n WHERE st.id = n.idS and sb.id = n.idD ";
			$result = mysqli_query($con, $sql);
	
			while($row = mysqli_fetch_array($result)){
				echo "<tr>";
				echo "<td>" .$row["nume"] ."</td>";
				echo "<td>" .$row["denumire"] ."</td>";
				echo "<td>" .$row["nota"] ."</td>";
				echo "</tr>";
				}
		?>
		</table>

	<div>
</body>
</html>