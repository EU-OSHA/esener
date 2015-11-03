<?php

/*********************************************************
* Eworx S.A.- 2012
* @Author kp@eworx.gr
* EFWEB-596 R Resources Management Layer (RRML)
* File shows the used R instances using the RRML db api
**********************************************************/

	require_once("settings.php");
	require_once("db/settings.php");
	require_once("db/RRML.php");

	echo '<pre>';
	var_dump( getUsedRInstances() );
	echo '</pre>';

