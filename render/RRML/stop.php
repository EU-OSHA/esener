<?php

/*********************************************************
* Eworx S.A.- 2012
* @Author kp@eworx.gr
* EFWEB-596 R Resources Management Layer (RRML)
* File terminates the created R instances
**********************************************************/

	require_once("settings.php");
	require_once("db/settings.php");
	require_once("db/RRML.php");

	
	serviceInitiating();
	
	// list r intances using screen
	$output = shell_exec('screen -ls | grep r.instance');
	$output = str_replace("(Detached)", "", str_replace("\t", "", $output));
	$output = str_replace("(Attached)", "", str_replace("\t", "", $output));
	$output = trim(preg_replace("/\(.*\)/", "", $output));

	$instances = explode("\n", $output);

	//issue an exist signal in all R instances

	foreach ($instances as $instance){
		if(trim($instance) != ""){
			initializingRInstance($instance); //-1
			shell_exec('screen -L -S '. $instance .' -p 0 -X stuff "q()
			"' );
			setUsedRInstance($instance); //0
		}
	}

	deleteRInstances();
	serviceStoped();
