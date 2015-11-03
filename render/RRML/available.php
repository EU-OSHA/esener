<?php
/*********************************************************
* Eworx S.A.- 2012
* @Author kp@eworx.gr
* EFWEB-596 R Resources Management Layer (RRML)
* File shows the available R instances by evaluating the existence of files in the file system
**********************************************************/

	require_once("settings.php");
	require_once("db/settings.php");
	require_once("db/RRML.php");
	
	echo '<pre>';
	var_dump( getAvailableRInstances() );
	echo '</pre>';


