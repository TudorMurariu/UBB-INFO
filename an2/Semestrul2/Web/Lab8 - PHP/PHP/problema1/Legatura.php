<?php


class Legatura
{
    /**
     * Legatura constructor.
     * @param $locPlecare
     * @param $locSosire
     */
    public function __construct($locPlecare, $locSosire)
    {
        $this->locPlecare = $locPlecare;
        $this->locSosire = $locSosire;
    }

    /**
     * @return mixed
     */
    public function getLocPlecare()
    {
        return $this->locPlecare;
    }

    /**
     * @param mixed $locPlecare
     */
    public function setLocPlecare($locPlecare)
    {
        $this->locPlecare = $locPlecare;
    }

    /**
     * @return mixed
     */
    public function getLocSosire()
    {
        return $this->locSosire;
    }

    /**
     * @param mixed $locSosire
     */
    public function setLocSosire($locSosire)
    {
        $this->locSosire = $locSosire;
    }
    private $locPlecare;
    private $locSosire;


}