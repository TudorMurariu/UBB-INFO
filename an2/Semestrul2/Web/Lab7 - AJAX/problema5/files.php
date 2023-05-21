<?php

if($_GET['dir']){
    $path = 'E:\XAMPP\htdocs\ajaxApp' . '\\' . $_GET['dir'];
    if(is_dir($path)) {
        $dir = new DirectoryIterator($path);
        $childs = array();
        foreach ($dir as $fileinfo) {
            if (!$fileinfo->isDot()) {
                array_push($childs, $fileinfo->getFilename());
            }

        }
        print json_encode($childs);
    }
    else{
        $json = array();
        $homepage = file_get_contents($path);
        array_push($json, $homepage);
        print json_encode($json);
    }
}
?>