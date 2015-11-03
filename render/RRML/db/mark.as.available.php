<?php
if(!empty($argv) && !empty($argv[1])){
		$name = $argv[1];
		if (preg_match("/r\.instance\.[0-9]+/", $name, $result)) {
			require_once("RRML.php");
			setAvailableRInstance($result[0]);
		}
}