<?php

/*********************************************************
* Eworx S.A. - 2012 - 2014
* @Author kp@eworx.gr
* EFWEB-596 R Resources Management Layer (RRML)
* File shows the size of the plots directory containing all cached resources and the logs folder
**********************************************************/

	$output = shell_exec('du /web-pub/foundation/html/DVS/render/plots/');
	$output = str_replace("/web-pub/foundation/html/DVS/render/", "", $output);
	echo "<pre>$output</pre>";

	$output = shell_exec('du /web-pub/foundation/html/DVS/render/logs/');
	$output = str_replace("/web-pub/foundation/html/DVS/render/", "", $output);
	echo "<pre>$output</pre>";

	// show network information
	//$output = shell_exec('/sbin/ifconfig');
	//echo "<pre>$output</pre>";

