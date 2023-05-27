<?php
/**
 * Created by PhpStorm.
 * User: Teo
 * Date: 5/1/2019
 * Time: 12:08 AM
 */

require_once 'Oras.php';
require_once 'Legatura.php';

class View
{

    /**
     * View constructor.
     */
    public function __construct()
    {
    }

    public function printOras($oras){
        echo '<option onclick="showSosiri(this)">' .$oras->getName() .'</option>';
    }

    // as table
    public  function printOrase($oraseArray){
        echo '<label>Statie plecare: <select size =6 multiple>';
        foreach($oraseArray as $oras){
            $this->printOras($oras);
        }
        echo '</select></<label>';
    }
}