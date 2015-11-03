<?php
/*********************************************************
* Eworx S.A.- 2012
* @Author kp@eworx.gr
* EFWEB-596 R Resources Management Layer (RRML)
* File for defining the functions that reserve an available R instance and determining that the reserved R instance has completed its task 
**********************************************************/

	require_once("settings.php");
	require_once("db/settings.php");
	require_once("db/RRML.php");

	function reserveRDaemon(){
		global $projectHome;
		for ( $counter = 1; $counter <= 30; $counter ++) { // wait up to 3 seconds
			$reservedRInstance = reserveRInstance();
			if($reservedRInstance != NULL)
				return $reservedRInstance;
			time_nanosleep(0, 100000000);
		}
		return null;
	}

	function isReservedRDaemonAvailable($rInstanceScreenName){
		return isRInstanceAvailable($rInstanceScreenName);
	}

