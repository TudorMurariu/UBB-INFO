<?php
	if (!isset($_SESSION["tic"]))
  {
	session_start();
	}
?>

<!DOCTYPE html>
<html>
	<head>
		<title>Problema 4</title>
		<style>
			 h1 {
			  text-align: center;
			}
			td {
			  width: 100px;
			  height: 100px;
			  font-size: 50px;
			  text-align: center;
			}
			table {
			  margin: 5px auto;
			}
			.vert {
			  border-left: 2px solid black;
			  border-right: 2px solid black;
			}
			.hori {
			  border-top: 2px solid black;
			  border-bottom: 2px solid black;
			}
		</style>
	
		<script src="jquery-2.0.3.js"></script>
		<script>
			var end = false;
			function isEnd(){
				$.ajax({
					type : "GET",
					url : "end.php", 	
					cache: true,
					success: function(data,status) {
						  end = data;
						}
				});
			}
		
			function nextStep(){
				$.ajax({
					type : "GET",
					url : "game.php", 	
					cache: true,
					success: function(data,status) {
						   setTimeout(()=>{	
						   $('table tr').eq(Math.floor(data/3)).find('td').eq(data%3).text("0");	
						   }, 500);
						   isEnd();
						   setTimeout(()=>{	
							if(end == true){
								//alert("You lose!");
							}
						   }, 500);
						}
				});
			}
			
			function myStep(row,col){
				$.ajax({
					type : "GET",
					url : "myPut.php", 	
					cache: true,
					data: "row=" + row + "&col=" + col,
					success: function() {
						    $('table tr').eq(row).find('td').eq(col).text("X");	
							isEnd();
							setTimeout(()=>{	
							if(end == false){
								nextStep();
							}
							else{
								alert("You win!");
							}
						   }, 500);
						}
				});
			}
			
					
			$(document).ready(function(){
				$("table").on("click", "td", function(){		
					
					var col   = $(this).index();
					var row   = $(this).closest('tr').index();
					
					myStep(row,col);
					//nextStep();
					});	
				});
				
		</script>
	</head>
	<body>
	<?php
		$m = array(0,0,0,0,0,0,0,0,0);
		$_SESSION["tic"]= $m;
	?>
	
 
		<h1>Tic Tac Toe</h1>
		<table>
		  <tr>
			<td></td>
			<td class="vert"></td>
			<td></td>
		  </tr>
		  <tr>
			<td class="hori"></td>
			<td class="vert hori"></td>
			<td class="hori"></td>
		  </tr>
		  <tr>
			<td></td>
			<td class="vert"></td>
			<td></td>
		  </tr>
		</table>
		
	</body>
</html>