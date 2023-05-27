<?php
	$con = mysqli_connect("localhost", "root", "","magazin_db");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}
	
	$pageLimit = $_GET["laptop"];
	
	$pattern = '/[0-9]{1,30}$/';
	preg_match($pattern, $pageLimit, $matches, PREG_OFFSET_CAPTURE);
	if (count($matches)==0){
		header("Location: http://localhost/phpApp/pr2/");
	}
	
	$pageNr = $_GET["nrPag"];
	$sql = "SELECT * FROM laptop limit " .$pageNr *$pageLimit ."," . $pageLimit .";";
	$result = mysqli_query($con, $sql);
	
	echo "<form action='showProduse.php' method = 'GET'>
		<input type='text' hidden name='nrPag' value=0 >".
		"<input list='laptops' name='laptop' >
			<datalist id='laptops' readonly>
			<option value='1'>
			<option value='2'>
			<option value='3'>
			<option value='4'>
			<option value='5'>
		</datalist>
	<input type='submit'  value ='Send'> </form>";
	echo "<hr>";
	echo "<table><tr><th>Denumire</th><th>Producator</th><th>Procesor</th><th>Memorie</th><th>CapacitateHDD</th><th>PlacaVideo</th><th>Diagonala</th></tr>";
	
	while($row = mysqli_fetch_array($result)){
		echo "<tr><td>" .$row["denumire"] ."</td><td>" .$row["producator"] ."</td><td>".$row["procesor"] ."</td>";
		echo "<td>" .$row["memorie"] . "</td><td>" .$row["capacitateHdd"] . "</td> <td>" .$row["placaVideo"] . "</td>";
		echo "<td>" .$row["diagonala"] . "</td></tr>";
	}
	echo "</table>";
	echo "<hr>";
	
	echo "<table><tr> <td>";
	$existaPrev = false;
	if($pageNr -1 > -1){
		$existaPrev = true;
	}
	echo "<form action= 'showProduse.php' method = 'GET'>";
	echo "<input type='text' hidden name='nrPag' value=" . ($pageNr - 1 ). ">";
	echo "<input type='text' hidden name='laptop' value=" . $pageLimit. ">";
	if($existaPrev == false){
		echo "<input type = 'submit' value = 'Prev' disabled>";
	}
	else {
		echo "<input type = 'submit' value = 'Prev'>";
	}
	echo "</form></td><td>";
	
	$existaNext = false;
	$sql = "SELECT * FROM laptop limit " .($pageNr+1) *$pageLimit ."," . $pageLimit .";";
	$result = mysqli_query($con, $sql);
	while($row = mysqli_fetch_array($result)){
		$existaNext = true;
		break;
	}
	
	echo "<form action= 'showProduse.php' method = 'GET'>";
	echo "<input type='text' hidden name='nrPag' value=" . ($pageNr + 1 ). ">";
	echo "<input type='text' hidden name='laptop' value=" . $pageLimit. ">";
	if ($existaNext == true){	
		echo "<input type = 'submit' value = 'Next'> ";
	}
	else {
		echo "<input type = 'submit' value = 'Next' disabled> ";
	}
	echo "</form></td></tr></table>";
	mysqli_close($con);
?> 