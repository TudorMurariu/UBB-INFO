<?php
    $path = $_GET['path'];
    $myfile = fopen($path, "r") or die("Unable to open file!");
    echo fread($myfile,filesize($path));
    fclose($myfile);
?>