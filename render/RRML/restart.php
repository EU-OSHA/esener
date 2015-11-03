<?php

/*********************************************************
* Eworx S.A.- 2012
* @Author kp@eworx.gr
* EFWEB-596 R Resources Management Layer (RRML)
* File uses the stop and start RRML calls
**********************************************************/

	require_once("stop.php");
	time_nanosleep(0, 200000000); //wait for 0.2 sec
	require_once("start.php");

?>
