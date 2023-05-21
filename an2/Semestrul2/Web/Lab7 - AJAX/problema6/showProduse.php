<?php
	$con = mysqli_connect("localhost", "root", "","magazin_db");
	if (!$con) {
		die('Could not connect: ' . mysqli_error());
	}

	$items = $_GET["items"];
	
	function getItem($i) {
		switch ($i) {
			case 0:
				$item = "producator";
				break;
			case 1:
				$item = "procesor";
				break;
			case 2:
				$item = "memorie";
				break;
			case 3:
				$item = "capacitateHdd";
				break;
			case 4:
				$item = "placaVideo";
				break;
			case 5:
				$item = "dagonala";
				break;
		}
		return $item;
	}
	
	$condition="";
	
	for ($i=0;$i<count($items);$i++) {
		$item = getItem($i);
		
		if ($items[$i]!=''){
			if ($condition==''){
				$condition ="WHERE ";
			}
			else{
				$condition = $condition ."and ";
			}
			$condition = $condition . $item ."='" .$items[$i]."' ";
		}
		
	}
	
	$sql =  "SELECT * FROM laptop " . $condition;
	$result = mysqli_query($con, $sql);
	echo "<tr><th>Denumire</th><th>Producator</th><th>Procesor</th><th>Memorie</th><th>CapacitateHDD</th><th>PlacaVideo</th><th>Diagonala</th></tr>";
	while($row = mysqli_fetch_array($result)){
		echo "<tr><td>" .$row["denumire"] ."</td><td>" .$row["producator"] ."</td><td>".$row["procesor"] ."</td>";
		echo "<td>" .$row["memorie"] . "</td><td>" .$row["capacitateHdd"] . "</td> <td>" .$row["placaVideo"] . "</td>";
		echo "<td>" .$row["diagonala"] . "</td></tr>";
	}
	mysqli_close($con);
?> 