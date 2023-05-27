<?php
/**
 * Created by PhpStorm.
 * User: Teo
 * Date: 5/1/2019
 * Time: 11:33 AM
 */
require_once 'DBUtils.php';
require_once 'Oras.php';

class Model
{
    private $db;
    public function __construct()
    {
        $this->db = new DBUtils();
    }


    public function getOrase(){

        $resultSet = $this->db->selectPlecare();

        $oraseList = new ArrayObject();

        foreach($resultSet as $value){

            $oraseList->append(new Oras($value['locPlecare']));
        }
        return $oraseList;
    }
}