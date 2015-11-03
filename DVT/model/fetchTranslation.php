<?php

function fetchValue($key, $keyValuesInArray){
	$result = "";
	foreach ($keyValuesInArray as $keyValue){
		if($keyValue[0] == $key){
			return $keyValue[1];
		}
	}
	return $result;

}

function fetchTitle($key){
	global $titles_;
	return fetchValue($key, $titles_);
}

function fetchLinkTitle($key){
	global $link_titles;
	return fetchValue($key, $link_titles);
}

function fetchLabel($key){
	global $labels_;
	return fetchValue($key, $labels_);
}

function fetchTooltip($key){
	global $tooltips_;
	return fetchValue($key, $tooltips_);
}

function fetchTooltipMouse($key){
	global $tooltipsMouse_;
	return fetchValue($key, $tooltipsMouse_);
}



function fetchVisualizationCount($key){
	global $visualizationsCount_;
	return fetchValue($key, $visualizationsCount_);
}

function fetchVisualizations(){
	global $plots_;
	return $plots_;
}

function fetchViewCSVData(){
	global $dataViews_;	
	return $dataViews_[0][1];
}

function fetchWord($key){
	global $words_;
	return fetchValue($key, $words_);
}

$countryToLanguage = array(
		array('AT', 'Austria'),
		array('BE', 'Belgium'),
		array('BG', 'български'),
		array('CY', 'Ελληνικά'),
		array('CZ', 'čeština'),
		array('CS', 'čeština'), //
		array('DA', 'Dansk'),
		array('DE', 'Deutsch'),
		array('DK', 'Dansk'),
		array('EE', 'Eesti keel'),
		array('EL', 'Ελληνικά'),
		array('ES', 'Español'),
		array('ET', 'eesti'),
		array('FI', 'Suomi'),
		array('FR', 'Français'),
		array('HU', 'Magyar'),
		array('HR', 'Hrvatski'),
		array('IE', 'Irish'),
		array('IT', 'Italiano'),
		array('LT', 'Lietuvių kalba'),
		array('LU', 'Luxembourg'),
		array('LV', 'Latviešu valoda'),
		array('MT', 'Malti'),
		array('MK', 'Slavic'),
		array('NL', 'Dutch'),
		array('PL', 'Polski'),
		array('PT', 'Português'),
		array('RO', 'Română'),
		array('SE', 'Sweden'),
		array('SI', 'Slovenščina'),
		array('SK', 'Slovenčina'),
		array('SL', 'Slovenščina'),
		array('SV', 'Svensk'),
		array('TR', 'Turkish'),
		array('UK', 'English'),
		array('EN', 'English')
);
