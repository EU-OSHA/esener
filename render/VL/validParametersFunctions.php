<?php
/*********************************************************
* Eworx S.A. - 2012/2013
* @Author kp@eworx.gr
* EFDVS-7 R DVS Layer - Validation Layer of System
* File for defining the functions that validate the parameters using the defined valid combinations per survey
* + constructs the R function call based on the parameters
* + constructs the result file name of the R rendering engine.
**********************************************************/

//------------------------------------------------------

/**
 * the visualization functions of the R rendering engine and their mandatory parameter names
 */

 
$plotsParameters = array( //R function order
	"heatMap" => array("dataSource", "subset", "subsetValue", "question", "answer", "locale", "media", "width", "onlyEU"),
	"wordMap" => array("dataSource", "subset", "subsetValue", "question", "answer", "locale", "media", "width"),
	"euCompass" => array("dataSource", "subset", "subsetValue", "question", "answer", "locale", "media", "width"),
	"euBars" => array("dataSource", "subset", "subsetValue", "question", "locale", "media", "width", "onlyEU"),
	"inCountry"  => array("dataSource", "subset", "question", "country", "locale", "media", "width"),
	"crossCountry"  => array("dataSource", "subset", "subsetValue", "question", "country", "countryB", "locale", "media", "width"),
	"maskMap" => array("media", "width"),
	"euMatrix" => array("dataSource", "question", "locale", "media", "width", "onlyEU"),
	"cwbTimeSeries" => array("queryString", "media", "width"),
	"cwbContextMap" => array("queryString", "media", "width"),
	"cwbOutcomesMap" => array("queryString", "media", "width"),

	"subsetTimeSeries" => array("dataSource", "subset", "question", "answer", "country", "locale", "media", "width", "onlyEU"),
); 

$validMedia  = array("png", "xls", "pdf", "eps", "svg");
$validWidths = array(1024, 920, 740, 500);

//------------------------------------------------------

require_once('valid.surveys.php');

if(isset($_GET['plot']) && $_GET['plot'] != "maskMap" && $_GET['plot'] != "cwbTimeSeries" && $_GET['plot'] != "cwbContextMap" && $_GET['plot'] != "cwbOutcomesMap" )
	if(empty($_GET["dataSource"]) || !in_array($_GET["dataSource"], $GLOBALS['validSurveys'])){
		die('Not valid Parameters...');
	}else{
		require_once($_GET["dataSource"].'.valid.parameters.php');
		require_once($_GET["dataSource"].'.valid.locales.php');
	}

//------------------------------------------------------

/**
 * validates if all the provided parameters are a valid request
 * @return boolean if the provided parameters are valid for the functionn plot ame
 */

function isPlotParametersValid($parameters){
	if(!isset($parameters['plot'])) return false;

	if(!isRFunctionValid($parameters["plot"]))
		return false;

	return isRFunctionParametersValid($parameters["plot"], $parameters);
}

//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------
	
function isQueryStringValid($value){//CWB related
	if(!isset($value)) return false;
	if(preg_replace("/[^a-z0-9_,]/", "-", $value) == $value){
		return true;
	}
	return false;
}

function isLocaleValid($value){
	if(!isset($value)) return false;
	return in_array($value, $GLOBALS['surveyLocales']);
}

function isMediaValid($value){
	if(!isset($value)) return false;
	return in_array($value, $GLOBALS['validMedia']);
}

function isWidthValid($value){
	if(!isset($value)) return false;
	return in_array($value, $GLOBALS['validWidths']);
}

/**
 * validates if the question provided exists for the provided datasource
 * @return boolean if question code exists in the survey
 */

function isQuestionValid($question){
	return(array_key_exists($question, $GLOBALS['validParameterCombinations']["questions"]));
}

/**
 * validates if the answer provided for the question provided exists for the provided datasource
 * @return boolean if answer exists for the question code in the survey
 */

function isAnswerValid($question, $value){
	//return(in_array($answer, $GLOBALS['validParameterCombinations']["questions"][$question]));
	$values = $GLOBALS['validParameterCombinations']["questions"][$question];
	foreach ($values as $originalValue){
		if(preg_replace("/[^a-zA-Z0-9]/", "-", $originalValue) == $value){
			return true;
		}
	}
	return false;
}

function getAnswerOriginal($question, $value){
	//return(in_array($answer, $GLOBALS['validParameterCombinations']["questions"][$question]));
	$values = $GLOBALS['validParameterCombinations']["questions"][$question];
	foreach ($values as $originalValue){
		if(preg_replace("/[^a-zA-Z0-9]/", "-", $originalValue) == $value){
			return $originalValue;
		}
	}
	return false;
}

//------------------------------------------------------

/**
 * validates if the subset provided exists for the provided datasource
 * @return boolean if the subset is valid in the survey
 */

function isSubsetValid($subset){
	return(array_key_exists($subset, $GLOBALS['validParameterCombinations']["subsets"]));
}

/**
 * validates if the subset value provided for the subset provided exists for the provided datasource
 * @return boolean if subset value exists for the subset in the survey
 */

function isSubsetValueValid($subset, $value){
	$values = $GLOBALS['validParameterCombinations']["subsets"][$subset];
	foreach ($values as $originalValue){
		if(preg_replace("/[^a-zA-Z0-9]/", "-", $originalValue) == $value){
			return true;
		}
	}
	return false;
}

function getSubsetValueOriginal($subset, $value){
	$values = $GLOBALS['validParameterCombinations']["subsets"][$subset];
	foreach ($values as $originalValue){
		if(preg_replace("/[^a-zA-Z0-9]/", "-", $originalValue) == $value){
			return $originalValue;
		}
	}
	return false;
}


/**
 * validates if the country value provided exist
 * @return boolean if country value exists in the survey
 */

function isCountryValueValid($value){
	return(in_array($value, $GLOBALS['validParameterCombinations']["countries"]));
}

//------------------------------------------------------

/**
 * validates if the provided function exist
 * @return boolean if the function plot exists
 */

function isRFunctionValid($functionName){
	return(array_key_exists($functionName, $GLOBALS['plotsParameters']));
}

//------------------------------------------------------

/**
 * validates if the the provided parameters are valid for the provided function plot name
 * @return boolean if the provided parameters are valid for the functionn plot ame
 */

function isRFunctionParametersValid($functionName, $parameters){

	if(!isRFunctionValid($functionName))
		return false;

	if(in_array("onlyEU", $GLOBALS['plotsParameters'][$functionName]) && !in_array("onlyEU", $parameters)){
		$parameters["onlyEU"] = "0";
	}


	$validPlotParametersCount = count($GLOBALS['plotsParameters'][$functionName]);
	$parametersCount = 0;

	foreach (array_keys($parameters) as $parameter){
		if(in_array($parameter, $GLOBALS['plotsParameters'][$functionName])){
			$parametersCount ++;
		}
	}

	if($validPlotParametersCount != $parametersCount)
		return false;

	foreach (array_keys($parameters) as $parameter){
	
		switch($parameter){

			case 'question':
				if(!isQuestionValid($parameters["question"] ))
					return false;
				break;
			case 'answer':
				if(!isAnswerValid($parameters["question"], $parameters["answer"]  ))
					return false;
				break;
			case 'subset':
				if(!isSubsetValid($parameters["subset"] ))
					return false;
				break;
			case 'subsetValue':
				if(!isSubsetValueValid($parameters["subset"], $parameters["subsetValue"] ))
					return false;
				break;
			case 'country':
				if(!isCountryValueValid($parameters["country"] ))
					return false;
				break;
			case 'countryB':
				if(!isCountryValueValid($parameters["countryB"] ))
					return false;
				break;
			case 'locale':
				if(!isLocaleValid($parameters["locale"]))
					return false;
				break;
			case 'media':
				if(!isMediaValid($parameters["media"]))
					return false;
				break;
			case 'width':
				if(!isWidthValid($parameters["width"]))
					return false;
				break;
			case 'queryString':
				if(!isQueryStringValid($parameters["queryString"] ))
					return false;
				break;
			default:
			continue;
		}
	}

	return true;
}

//------------------------------------------------------

/**
 * Constructs an R statement based on the parameters provided
 * @return string of the R function call statement based on the parameters provided
 */

function getRFunctionCall($parameters){

	$result = $parameters["plot"]. "(" ;
	$plotParameters = $GLOBALS['plotsParameters'][$parameters["plot"]];
	$parametersCount = 0;

	foreach ($plotParameters as $plotParameter){
		if(isset($parameters[$plotParameter])){
			$parameterValue = $parameters[$plotParameter];
			$parametersCount++;
			if($plotParameter == "width")
				$result = $result . ($parametersCount == 1 ? '' : ',') . $parameterValue ;
			else
			if($plotParameter == "subsetValue"){
				$result = $result . ($parametersCount == 1 ? '' : ',') .'\"' . getSubsetValueOriginal($parameters["subset"], $parameters["subsetValue"] ) . '\"';
			}
			else
			if($plotParameter == "answer"){
				$result = $result . ($parametersCount == 1 ? '' : ',') .'\"' . getAnswerOriginal($parameters["question"], $parameters["answer"]  ) . '\"';
			}
			else
			if($plotParameter == "onlyEU"){
				if($parameterValue == "onlyEU")
				$result = $result . ($parametersCount == 1 ? '' : ',') . "1";
			}		
			else
				$result = $result . ($parametersCount == 1 ? '' : ',') .'\"' . $parameterValue . '\"';
		}
	}

	$result = $result . ");";
	return $result;
}

//------------------------------------------------------

 /**
 * Constructs the plot file name, R will construct
 * in cooperation with the visualization functions. The file name is based on the order and names of the parameters of the function call and the function call name
 * This combination is unique and can be used as a request - result file name. This function leaves the file extention to be filled by a wrapper function so as to be used for various media.
 * @return string the body file name of the result file based on the provided parameters.
 */

function getPlotBodyFileName($parameters){
	$result = $parameters["plot"];
	$plotParameters = $GLOBALS['plotsParameters'][$parameters["plot"]];

	foreach ($plotParameters as $plotParameter){

		if($plotParameter != "media"){
			if($plotParameter == "width"){
				 if($parameters["media"] == 'png' || $parameters["media"] == 'svg' ||$parameters["media"] == 'pdf' || $parameters["media"] == 'eps')
						$result = $result . "-" . $parameters[$plotParameter];
			}else{
				if($plotParameter == 'onlyEU'){
					if(isset($parameters[$plotParameter]) && $parameters[$plotParameter] == 'onlyEU')
						$result = $result . "-EU";
				}else
				$result = $result . "-" . $parameters[$plotParameter];
			}
		}
	}

	return $result;
}

//------------------------------------------------------
 /**
 * adds the png file extention after calling the getPlotBodyFileName($parameters)
 * @return string file name of the visualization media.
 */

function getPlotFileName($parameters){
	return ($parameters['media'] == 'png' ? '' : $parameters['media']. "/")  . getPlotBodyFileName($parameters) . "." . $parameters['media'];
}

//------------------------------------------------------
#
#echo "<pre>";
#var_dump(isPlotParametersValid($_GET));
#if(isPlotParametersValid($_GET)){
#	echo (getRFunctionCall($_GET));
#	echo getPlotFileName($_GET);
#}
#echo "</pre>";

#?plot=heatMap&dataSource=3RDEQLS&subset=Y11_HH2a&question=Y11_Q13a&answer=Yes&subsetValue=Male
