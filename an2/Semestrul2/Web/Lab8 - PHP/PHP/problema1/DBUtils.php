<?php
/**
 * Created by PhpStorm.
 * User: Teo
 * Date: 5/1/2019
 * Time: 11:26 AM
 */

class DBUtils
{
    private $host = '127.0.0.1';
    private $db   = 'trenuri_db';
    private $user = 'root';
    private $pass = '';
    private $charset = 'utf8';

    private $pdo;
    private $error;

    public function __construct () {
        $dsn = "mysql:host=$this->host;dbname=$this->db;charset=$this->charset";

        $opt = array(PDO::ATTR_ERRMODE   => PDO::ERRMODE_EXCEPTION,
            PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,
            PDO::ATTR_EMULATE_PREPARES   => false);

        try {
            $this->pdo = new PDO($dsn, $this->user, $this->pass, $opt);
        }
        catch(PDOException $e){
            $this->error = $e->getMessage();
            echo "Error connecting to DB: " . $this->error;
        }
    }

    public function selectPlecare() {
        $stmt = $this->pdo->prepare("SELECT distinct locPlecare FROM trenuri");
        $stmt->execute();
        return $stmt->fetchAll();
    }
}