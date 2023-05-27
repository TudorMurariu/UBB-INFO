<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Problema 5</title>
	<link rel="stylesheet" type="text/css" href="pageStyle.css">
</head>
<body>
	<div class="header">
	  <h1>Photopedia</h1>
	</div>

	<div class="topnav">
	  <a href="indexMyPhotos.php">Profile</a>
	  <a href="addPhoto.php">Add photos</a>
	  <a href="viewPhotos.php">Feed</a>
	  <a href="index.php">Logout</a>
	</div>

	<div class='container'>
		<form method="POST" action="<?php echo htmlspecialchars($_SERVER['PHP_SELF']);?>" enctype="multipart/form-data">
			<input type="hidden" name="size" value="1000000">
			<input type="file" name="image">
			<textarea 
				id="text" 
				cols="40" 
				rows="4" 
				name="image_text" 
				placeholder="Say something about this photo">
			</textarea>
			<button type="submit" name="upload">Upload</button>
		</form>
			<?php
			$con = mysqli_connect("localhost", "root", "","problema5php_db");
			if (!$con) {
				die('Could not connect: ' . mysqli_error());
			}
			
			// USERNAME
			$sql="SELECT username FROM user_curent";
			$result = mysqli_query($con, $sql);
			$username= '';
			while($row = mysqli_fetch_array($result)) {
				$username = $row["username"];
			}
			
			// POZE
			if (isset($_POST['upload'])) {
			// Get image name
			$image = $_FILES['image']['name'];
			// Get text
			$image_text = mysqli_real_escape_string($con, $_POST['image_text']);
			// image file directory
			$target = "images/".basename($image);
			
			$sql = "INSERT INTO imagesuploaded (username,image, image_text) VALUES ('$username','$image', '$image_text')";
			// execute query
			$ceva = $con->query($sql);

			if (move_uploaded_file($_FILES['image']['tmp_name'], $target)) {
				$msg = "Image uploaded successfully";
			}else{
				$msg = "Failed to upload image";
			}
			echo $msg;
		  }
			?>
		</div>
	</div>
</body>
</html>