<?php
/**
 * Created by PhpStorm.
 * User: Teo
 * Date: 5/1/2019
 * Time: 11:23 AM
 */

class Oras
{
    private $nume;

    /**
     * Users constructor.
     * @param $nume
     */
    public function __construct($nume)
    {
        $this->nume = $nume;
    }
    /**
     * @return mixed
     */
    public function getName()
    {
        return $this->nume;
    }

    /**
     * @param mixed $name
     */
    public function setName($name): void
    {
        $this->nume = $name;
    }

    public function __toString()
    {

        return '' . $this->nume;
    }
}