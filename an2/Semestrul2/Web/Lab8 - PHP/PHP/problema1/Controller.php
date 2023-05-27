<?php
/**
 * Created by PhpStorm.
 * User: Teo
 * Date: 5/1/2019
 * Time: 11:38 AM
 */
require_once 'Model.php';
require_once 'Oras.php';
require_once 'View.php';

class Controller
{
    private $model;
    private $view;

    /**
     * Controller constructor.
     * @param $model
     * @param $view
     */
    public function __construct()
    {
        $this->model = new Model();
        $this->view = new View();
    }

    public function getAllOrasePlecare(){
        $this->view->printOrase($this->model->getOrase());
    }

    public function service(){

        if (isset($_GET['action']) && !empty($_GET['action'])) {
            $this->{$_GET['action']}();
        }
        if (isset($_POST['action']) && !empty($_POST['action'])) {
            $this->{$_POST['action']}();
        }
    }
}

$ctrl = new Controller();
$ctrl->service();
