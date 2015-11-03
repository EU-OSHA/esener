<?php

/*********************************************************
* Eworx S.A.- 2012
* @Author kp@eworx.gr
* EFWEB-596 R Resources Management Layer (RRML)
* File creates an X number of R instance using screen, if they haven't already been created. Defines the function that sends an R command to an R instance given its name
**********************************************************/

	require_once("settings.php");
	require_once("db/settings.php");
	require_once("db/RRML.php");

	function isDaemonsCreated(){
		$output = shell_exec('screen -ls | grep r.instance');
		$output = str_replace("(Detached)", "", str_replace("\t", "", $output));
		$output = str_replace("(Attached)", "", str_replace("\t", "", $output));
		$output = trim(preg_replace("/([0-9]+\.)/", "", $output));
		if($output == "")
			return false;
		return true;
	}

	function executeRCommand($rInstanceScreenName, $command, $markInstanceAsAvailable = 1){
		if($markInstanceAsAvailable == 1){
			$command = $command . "mark.as.available();";
		}
		$cmd = 'screen -L -S '. $rInstanceScreenName. ' -p 0 -X stuff "'. $command . "\n". '"';
		logMessage(" - " . $command );
		shell_exec($cmd);
	}

	//initialize service if not already initialized
	if(!isDaemonsCreated()){
		logMessage('<pre>');
		if(!isServiceInitiating()){
			deleteRInstances();
			serviceInitiating();

			for($daemonIndex = 1; $daemonIndex <= $maxNumberOfDaemons; $daemonIndex++){
				$rInstanceScreenName = 'r.instance.'.$daemonIndex;
				initializingRInstance($rInstanceScreenName);

				// Initialize a screen with R either with the useXVirtualFrameBuffer or not
				$initiateRInstanceCommand =				  
					'cd ' . $projectHome.'/workspace/;export LC_ALL=en_US.UTF-8;export LANG=en_US.UTF-8;export LANGUAGE=en_US.UTF-8;export LESSCHARSET=utf-8;' .
					'screen -U -A -m -d -S ' . $rInstanceScreenName .
				  ($useXVirtualFrameBuffer ? ' xvfb-run -a ' . ($setXVirtualFrameBufferBitDepth ? '-s "-screen 0 1024x768x16" ' : '')  : '') .
					' R --no-save'
				;

				logMessage($initiateRInstanceCommand);
				shell_exec($initiateRInstanceCommand);

				// inform R instance of its name
				$RInitializeCommand = 'r.instance.screen.name <- \"' . $rInstanceScreenName . '\";';
				// load its sources
				$RInitializeCommand = $RInitializeCommand . 'source(\"'. $projectHome . $layerHome . '/initialization/0.initialize.R\");' ;
				//the last daemon sets the service as initialized
				if($daemonIndex == $maxNumberOfDaemons){
					$RInitializeCommand = $RInitializeCommand . 'mark.service.as.available();';
				}
				executeRCommand($rInstanceScreenName, $RInitializeCommand, 0);
			}
			logMessage('</pre>');
		}
	}

