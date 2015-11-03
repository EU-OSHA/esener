<?php

/*********************************************************
* Eworx S.A.- 2012
* @Author kp@eworx.gr
* EFWEB-596 R Resources Management Layer (RRML)
* File shows the created number of R instance using screen
**********************************************************/

	$output = shell_exec('screen -ls | grep r.instance');
	$output = str_replace("(Detached)", "", str_replace("\t", "", $output));
	$output = trim(preg_replace("/([0-9]+\.)/", "", $output));
	$output = trim(preg_replace("/\(.*\)/", "", $output));
	if($output == "")$output = "None";

	echo "<pre>$output</pre>";

