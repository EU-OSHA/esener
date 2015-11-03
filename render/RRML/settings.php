<?php

/*********************************************************
* Eworx S.A.- 2012
* @Author kp@eworx.gr
* EFWEB-596 R Resources Management Layer (RRML)
* File contains all necessary project variables (settings)
**********************************************************/

	$maxNumberOfDaemons = 8;
	$projectHome = "/web-pub/foundation/html/DVS/render/";
	$layerHome = "RRML";

	$useXVirtualFrameBuffer = FALSE ; // use xvfb in a headless linux server (EFWEB-589) 
	$setXVirtualFrameBufferBitDepth = TRUE ; // for particular complicated visualizations and in particular platforms we may have to also specify the bit depth of X11
	$verbose = FALSE;

//-------------------------------
// TODO place those function in a different visible place 

function logMessage($message){
	global $verbose; 
	if($verbose){
		echo $message . "\n";
	}
}

if($verbose){
	error_reporting(-1); //used to debug the RRML
}

//-------------------------------
