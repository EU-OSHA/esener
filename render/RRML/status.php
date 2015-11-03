<?php
/*********************************************************
* Eworx S.A.- 2012
* @Author kp@eworx.gr
* EFWEB-596 R Resources Management Layer (RRML)
* File shows the overall service status 
**********************************************************/

	require_once("settings.php");
	require_once("db/settings.php");
	require_once("db/RRML.php");


	echo '<h2>RRML Service Status</h2><hr/>';
	
	echo '<h4>Screen R Instances : </h4>';
	echo '<pre>';
	require_once("created.php");
	
	echo '<hr/><h4>Db status : </h4><pre>';
	echo getServiceStatus() ;
	echo '</pre>';

	echo '</pre><hr/>';

	echo '<h3>R Instances db status</h3>';
	echo '<pre>';
	var_dump( getRInstancesStatus() );
	echo '</pre>';

?>

<hr/>
	<ul>
	<li>Status [&nbsp;0] = Stopped or Used</li>
	<li>Status [-1] = Initializing </li>
	<li>Status [&nbsp;1] = Initialized or Available</li>
	</ul>
<hr/>
Note: If "Screen R Instances" are None, this means that the service is not initialized. The db status complements the overall RRML service status.
<hr/>

<?php


