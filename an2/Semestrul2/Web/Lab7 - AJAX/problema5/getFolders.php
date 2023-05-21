<?php
    $path = $_GET['path'];
    $dir = scandir($path);
    printf(json_encode($dir));
?>