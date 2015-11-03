<?php
//kp//eworx//gr//EWORX S.A.
/////////////////////////////////////////////////
$version = "325";
$locale = "EN";

$survey = "esener04";

require_once("../render/VL/valid.surveys.php");

if(!empty($_GET["dataSource"])){
	if(in_array($_GET["dataSource"], $validSurveys)) {
		$survey = $_GET["dataSource"];
	}
}

// find survey's available localizations
$findSurveyLocales = scandir("model/" . $survey . "/php/");
$surveyLocales =  array();

foreach($findSurveyLocales as $entry) {
	$pos = -1;
	if(!(($pos = strrpos($entry, ".php")) === false)) {
		array_push($surveyLocales, substr($entry, 0, $pos));
	}
}

//-----------------------------

if(!empty($_GET["locale"])){
	$parameter = $_GET["locale"];
	if(in_array($parameter, $surveyLocales)) {
		$locale = $parameter ;
	}
}

//-----------------------------
require_once("model/" . $survey . "/php/" . $locale . ".php");
require_once("model/fetchTranslation.php");

$printStandaloneExportOptions = true;

/////////////////////////////////////////////////
// SEO google enhancement
/////////////////////////////////////////////////

$pageTitle = fetchTitle("survey");
$visualizationUrl = "/DVS/render/?locale=EN&dataSource=3RDEQLS&media=png&width=740&question=Y11_Q8_PreferredWorkingHours&plot=heatMap&countryGroup=linear&subset=Y11_Agecategory&subsetValue=All&answer=1--Work-less";
$visualizationDescription = fetchTooltip("heatMap");

require_once('../render/VL/validParametersFunctions.php');


if(isPlotParametersValid($_GET)){
	$pageTitle .=  " : " . fetchValue($_GET['plot'], fetchVisualizations()) . ", " . fetchLabel("question") . " : " . $_GET['question'];

	$visualizationUrl = "/DVS/render/?" . $_SERVER['QUERY_STRING'];
	$plotDescriptionFileName = getPlotBodyFileName($_GET);
	$plotDescriptionFileName = substr($plotDescriptionFileName, 0, strlen($plotDescriptionFileName)-4);//remove the width
	$plotDescriptionFileName = "/DVS/render/plots/txt/" . $plotDescriptionFileName . ".htm";

	if(file_exists($plotDescriptionFileName)){
		$visualizationDescription = file_get_contents($plotDescriptionFileName, true);
		$visualizationDescription = trim($visualizationDescription);
	}

}

/////////////////////////////////////////////////
/////////////////////////////////////////////////
/////////////////////////////////////////////////
?>
