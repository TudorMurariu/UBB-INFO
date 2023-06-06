
<!DOCTYPE html>
<html>
<head>
<title>Image Upload</title>
<style type="text/css">
   #content{
   	width: 50%;
   	margin: 20px auto;
   	border: 1px solid #cbcbcb;
   }
   form{
   	width: 50%;
   	margin: 20px auto;
   }
   form div{
   	margin-top: 5px;
   }
   #img_div{
   	width: 80%;
   	padding: 5px;
   	margin: 15px auto;
   	border: 1px solid #cbcbcb;
   }
   #img_div:after{
   	content: "";
   	display: block;
   	clear: both;
   }
   img{
   	float: left;
   	margin: 5px;
   	width: 150px;
   	height: 300px;
   }
   .butonas{
		text-align: right;
	}
	.butn {
	  -webkit-border-radius: 17;
	  -moz-border-radius: 17;
	  border-radius: 17px;
	  font-family: Arial;
	  color: #ffffff;
	  font-size: 20px;
	  background: #0424c2;
	  padding: 8px 20px 10px 20px;
	  text-decoration: none;
	}

	.butn:hover {
	  background: #3cb0fd;
	  background-image: -webkit-linear-gradient(top, #3cb0fd, #3498db);
	  background-image: -moz-linear-gradient(top, #3cb0fd, #3498db);
	  background-image: -ms-linear-gradient(top, #3cb0fd, #3498db);
	  background-image: -o-linear-gradient(top, #3cb0fd, #3498db);
	  background-image: linear-gradient(to bottom, #3cb0fd, #3498db);
	  text-decoration: none;
	}
</style>
</head>
<body>
<div class="butonas">
		<form action="succes4.php" method="post">
			<input type="submit" value="backToMain" class="butn">
		</form>
	</div>
<div id="content">

  <form method="POST" action="<?php echo htmlspecialchars($_SERVER['PHP_SELF']);?>" enctype="multipart/form-data">
  	<input type="hidden" name="size" value="1000000">
  	<div>
  	  <input type="file" name="image">
  	</div>
  	<div>
      <textarea 
      	id="text" 
      	cols="40" 
      	rows="4" 
      	name="image_text" 
      	placeholder="Say something about this photo"></textarea>
  	</div>
  	<div>
  		<button type="submit" name="upload">POST</button>
  	</div>
  </form>
  <?php
  // Create database connection
  $db = new mysqli("localhost", "root", "", "lab8");
 $con = new mysqli("localhost","root","","lab8");
  // Initialize message variable
  $msg = $u= "";
  $image_text="";
  $s="SELECT username from user_curent where id=1";
  	$stmt =  $con->query($s);
  	while($row = $stmt->fetch_assoc()){
  		$u = $row['username'];
  	}
  $sss = "SELECT image, image_text FROM images where user='$u'";
  $result = $db->query($sss);
  
     while ($row2 = $result->fetch_assoc()) {
      	echo "<div id='img_div'>";
      	echo "<img src='images/".$row2['image']."'>";
      	echo "<p>".$row2['image_text']."</p>";
      echo "</div>";
    }
  // If upload button is clicked ...
  if (isset($_POST['upload'])) {
  	// Get image name
  	$image = $_FILES['image']['name'];
  	// Get text
  	$image_text = mysqli_real_escape_string($db, $_POST['image_text']);
  	// image file directory
  	$target = "images/".basename($image);
  	
  	$sql = "INSERT INTO images (user,image, image_text) VALUES ('$u','$image', '$image_text')";
  	// execute query
  	$ceva = $db->query($sql);

  	if (move_uploaded_file($_FILES['image']['tmp_name'], $target)) {
  		$msg = "Image uploaded successfully";
  		header ('Location: uploadPhoto.php');
  	}else{
  		$msg = "Failed to upload image";
  	}
  }
  ?>
</div>
</body>
</html>