<?php
/*********************************************************
* Eworx S.A. - 2012 / 2013
* @Author kp@eworx.gr
* Render Visualization Engine with caching
* serves pdf, csv, png, downloadable png using the Validation layer and the rendering engine of DVS
* upon every valid request, user and system information is also stored
**********************************************************/
	//var_dump ("hasata aqui si");
	//return false;

	$projectHome = "/web-pub/foundation/html/DVS/render";
	$plotsDirectory = "plots/";
	$logsDirectory = "logs/";
	$cache = TRUE;

	require_once('RRML/start.php'); // initializes the RRML in case it hasn't been initialized

	require_once('VL/validParametersFunctions.php');

	if(isPlotParametersValid($_GET)){
		#-----------------------------------------------------------------------------

		$millisecondsStart = round(microtime(true) * 1000);

		$plotFileName = $projectHome . $plotsDirectory . getPlotFileName($_GET);
		$isCached = FALSE;

		if(file_exists($plotFileName . ($_GET["media"] == "svg" ? ".gz":"") ) && $cache){
			$isCached = TRUE;
		}else{

		include 'RRML/reserve.php';
		$rInstanceScreenName = reserveRDaemon();

		if(!is_null($rInstanceScreenName)){

			if(file_exists($plotFileName))
				unlink($plotFileName);

			$reservedRDaemonAvailable = false;
			executeRCommand($rInstanceScreenName, getRFunctionCall($_GET));

			for ( $counter = 1; $counter <= 1000; $counter ++) {
				$reservedRDaemonAvailable = isReservedRDaemonAvailable($rInstanceScreenName);
				if($reservedRDaemonAvailable){
					break;
				}
				time_nanosleep(0, 100000000);
			}

			if(!$reservedRDaemonAvailable){
				die('Please try again in a few minutes');
			}

		}else{
			die('Could not reserve Instance. Please try again in a few seconds');
		}
			}
			$millisecondsEnd = round(microtime(true) * 1000);

			//---------------------------------------
			// log requests
			$fh = fopen($projectHome . "/" .$logsDirectory . "requests", 'a');
			$stringData = $_SERVER['REMOTE_ADDR'] . "\t" . $millisecondsStart . "\t" . $_SERVER['REQUEST_URI'] . "\t" . ($isCached?"CACHED":"RENDERED") . "\t" . ($millisecondsEnd - $millisecondsStart) . "\n";
			fwrite($fh, $stringData);
			fclose($fh);
			//---------------------------------------

			switch($_GET["media"]){

				case 'pdf':
					header('Content-type: application/pdf');
					header('Content-Disposition: attachment; filename="'. getPlotFileName($_GET) .'"');
					readfile($plotFileName);
					break;

				case 'xls':
					header('Content-type: application/vnd.ms-excel');
					header('Content-Disposition: attachment; filename="'. getPlotFileName($_GET) .'"');
					readfile($plotFileName);
					break;
				case 'svg':

					if(isset($_GET['download'])){
						header('Content-Encoding: gzip');
						header('Content-type: image/svg+xml');// IE gets confused with this default export option with download appended.
						readfile($plotFileName.".gz");						
					}else{
						header('Content-Encoding: gzip');// image/svg+xml');
						header('Content-type: text/html'); // this is needed for DVS
						//header('Content-Disposition: attachment; filename="'. getPlotFileName($_GET) .'"');
						//EFDVS-138
						readfile($plotFileName.".gz");
					}
					
					break;

				case 'eps':
					header('Content-type: application/postscript');
					header('Content-Disposition: attachment; filename="'. getPlotFileName($_GET) .'"');
					readfile($plotFileName);
					break;	
					
				default:

					header("Content-type: image/png");

					if(isset($_GET['download'])){

						header('Content-Disposition: attachment; filename="'. getPlotFileName($_GET) .'"');
						readfile($plotFileName);

					}else{
						$image = imagecreatefrompng($plotFileName);
						imagepng($image);
					}
				continue;
			}

	}else{
		echo 'Not valid Parameters';
	}
