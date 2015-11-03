//kp//eworx//gr// EWORX S.A. 2013
var widthState = 1;
var	$window = $(window);
var	$wrapper = $("#wrapper");
var	previousWindowWidth = $wrapper.width() + 1;
var mediaAvailableResponsiveWidths = [920, 740, 500];
var cssAttributesLeftRightMargin = ["margin-left", "margin-right"];
var cssAttributesLeftRightPadding = ["padding-left", "padding-right"];

//javascript functions commenting based on http://code.google.com/p/jgrousedoc/wiki/AdvancedFunction

/**
@function expandUntilChangeOfHeigth Accents the value for a set of cssAttributes for a set of elements while the height of ussually a parent element remains unchanged. The function implements the notion of constant acceleration for resolving the right value, minimizing the required checks very efficiently.
@param {String} elementToExpand - jquery query capturing the desired elements to expand. If instead a jquery object is passed the jquery object will be used.
@param {String} elementToObserve - jquery query capturing the desired element to observe for height change
@param {Array} cssAttributes - an array of css propery names to be accented untill there is a height change
@param {optional Number} minValue - Optional minimoum value to start from.  Default value 1. the metric used is the maxValueIn
@param {optional Number} maxValue - Optional maximoum value to be checked.  Default value 800 (px)
@param {optional String} maxValueIn - The metric, to be used. Default value in px. As the process finds the right value automatically any metric is valid, but px more appropriate.
*/

function expandUntilChangeOfHeigth(elementToExpand, elementToObserve, cssAttributes, minValue, maxValue, maxValueIn){

	maxValue = maxValue || 800;
	maxValueIn = maxValueIn || "px";
	minValue = minValue || 1;

	$elementToExpand = $(elementToExpand);
	$elementToObserve = $(elementToObserve);

	var prevElementHeight = $elementToObserve.height();

	var step = 1;
	var valueToSet = minValue;

	for(var run = 0; run < 2; run++)
		for(;valueToSet < maxValue; valueToSet = valueToSet + step){
			setValueInCSSAttributes($elementToExpand, cssAttributes, valueToSet, maxValueIn);
			curentElementHeigth = $elementToObserve.height();
			if(prevElementHeight != curentElementHeigth){
				if(valueToSet != minValue)
					valueToSet = valueToSet - step;
				setValueInCSSAttributes($elementToExpand, cssAttributes, valueToSet, maxValueIn);

				step = 1;
				break;
			}
			step++;
			if(step > 20)
				step = 10;
			if(run == 1)
				step = 1;
			prevElementHeight = curentElementHeigth;
		}
}

/**
@function setValueInCSSAttributes Sets the value of a set of cssAttributes for a set of elements.
@param {Object} $elements - jquery object containing the desired elements
@param {Array} cssAttributes - an array of css property names to be set.
@param {Number} properiesValue - Optional maximum value to be checked.  Default value 800 (px)
@param {optional String} maxValueIn - The metric, to be used. Default value in px.
*/

function setValueInCSSAttributes($elements, cssAttributes, properiesValue, valueIn){
	valueIn = valueIn || "px";
	for(var cssAttributeIndex = 0; cssAttributeIndex < cssAttributes.length; cssAttributeIndex++)
    $elements.css(cssAttributes[cssAttributeIndex], properiesValue + valueIn)
}

/**
@function findMaxWidthAmongElements finds the maximum width of an element among a set of elements
@param {Object} $findMaxWidthAmongElements - jquery object containing the desired elements
*/
function findMaxWidthAmongElements($elements){
	var maxWidth = 0;
	$elements.each(function() {
		var thisWidth=$(this).width();
		if(thisWidth>maxWidth)
			maxWidth=thisWidth;
	});
	return maxWidth + 2;
}

/**
@function indentElements finds the maximum width of an element among a set of elements and sets this value to all elements
@param {Object} $elements - jquery object containing the desired elements
*/
function indentElements($elements){
	$elements.css("width", "auto");
	$elements.css("width", findMaxWidthAmongElements($elements));
}

/**
@function ifInRowsOptimizeRows evaluates if a set of elements is layed out in more than one row. If so, performs css value optimization on the provided attributes on every row/layer.
@param {String} elementToExpand - jquery query capturing the desired elements to expand. The ":visible" string will be appended in the query.
@param {String} elementToObserve - jquery query capturing the desired element to observe for height change
@param {Array} cssAttributes - an array of css property names to be accented until there is a height change
@param {optional Number} minValue - Optional minimum value to start from.  Default value 1.
*/

function ifInRowsOptimizeRows(elementToExpand, elementToObserve, cssAttributes, minValue){
	$elements = $(elementToExpand);
	$elements.filter(':visible');

	var rowCount = 0;
	var previousTop = "none";

	for(var i=0;i<20;i++)
		$(".layer" + i).removeClass("layer" + i);

	$elements.each(function() {
		$this = $(this);
		var thisTop = $this.position().top;
		if(previousTop != thisTop){
			rowCount++;
			previousTop = thisTop;
			$this.addClass("layer" + rowCount);
		}else{
			$this.addClass("layer" + rowCount);
		}
	});

	if(rowCount > 1){
		for(var i=1;i<=rowCount;i++){
			expandUntilChangeOfHeigth($elements.filter(".layer" + i) , elementToObserve, cssAttributes, minValue);
			expandUntilChangeOfHeigth($elements.filter(".layer" + i) , elementToObserve, cssAttributes, minValue);
		}
	}

	return rowCount;
}

/**
@function responsiveAutoArrange arranges the layout, based on the available width and how the arbitrary content is rendered in the browser.
*/
function responsiveAutoArrange(){
	calculateResponsiveHeightForAndroid();

	rwdQuestionSelection();
	rwdVisualizationSelection();
	rwdVisualizationFilters();
	updateProxyPlot();
	
}

function rwdQuestionSelection(){
	return "disabled";//CP
	
	$questionSection = $(".questionSection");
	
	if($questionSection.hasClass("hidden"))
		return "not necessary";

	$questionSectionLabel = $(".questionSection label");

	if(widthState != 1)
		indentElements($questionSectionLabel.find("span:visible"));
	expandUntilChangeOfHeigth($questionSectionLabel.find("select:visible"), $questionSection, ["width"]);
	if($wrapper.width() > 700)
		expandUntilChangeOfHeigth("#question", $questionSection, ["width"]);
}

 
function rwdVisualizationFilters(){
	return "disabled";//CP

	$visualizationFilters = $(".visualizationFilters");
	var isInOneLine = $visualizationFilters.find(".filtersLeft").position().top == $visualizationFilters.find(".filtersRight").position().top;

	if(!isInOneLine)
		indentElements($visualizationFilters.find("div label span:visible"));

	var maxVisualizationFiltersWidth = (widthState == 1 ? 180 : 700);
	expandUntilChangeOfHeigth($visualizationFilters.find("label select:visible"), $visualizationFilters, ["width"], 1, maxVisualizationFiltersWidth);
	expandUntilChangeOfHeigth($visualizationFilters.find(".filtersRight label select:visible"), $visualizationFilters, ["width"], 1, maxVisualizationFiltersWidth + 140);

	if(!isInOneLine)
		expandUntilChangeOfHeigth($visualizationFilters.find("#label_subset_value select:visible"), $visualizationFilters, ["width"], 1, maxVisualizationFiltersWidth);

}





function rwdVisualizationSelection(){
	return "disabled rwdVisualizationSelection";

	$visualizationSelection = $(".visualizationSelection");

	$visualizationSelectionLabel = $visualizationSelection.find("label:visible");


	setValueInCSSAttributes($visualizationSelectionLabel, cssAttributesLeftRightPadding, 3 );

	if(widthState == 3){
		expandUntilChangeOfHeigth($visualizationSelectionLabel.find("select:visible"), $visualizationSelection, ["width"], 3);
	}
	else
		expandUntilChangeOfHeigth($visualizationSelectionLabel.find("span:visible"), $visualizationSelection, cssAttributesLeftRightPadding, 3);

	expandUntilChangeOfHeigth($visualizationSelection.find(".filtersRight label span:visible"), $visualizationSelection, cssAttributesLeftRightPadding, 3);

	ifInRowsOptimizeRows("#formatSelector label", $visualizationSelection, cssAttributesLeftRightPadding, 3);
}


function hasAttribute($element, attribute){
	var attr = $($element).attr(attribute);

	if (typeof attr !== 'undefined' && attr !== false) {
			return true;
	}

	return false;
}

function rwdExportSection(){
	return "";

	$exportSection = $("#exportSection :visible");
	if($exportSection.length == 0)
		return "";


	$exportSectionLinks = $exportSection.find("a");

	if(widthState == 3){
		$exportSectionLinks.css("padding", 15);
		return;
	}

	expandUntilChangeOfHeigth($exportSectionLinks, $exportSection, cssAttributesLeftRightPadding, 5);
	var exportSectionRows =  ifInRowsOptimizeRows($exportSectionLinks, $exportSection, cssAttributesLeftRightPadding, 5);
	//for(var i=1;i<=exportSectionRows;i++){
	//	$exportSectionLinks.filter(".layer" +i+ ":first").css("margin-left","0px");
	//	$exportSectionLinks.filter(".layer" +i+ ":last").css("margin-right","0px");
	//}
}


//-------------------------------
//-------------------------------
//-------------------------------
//-------------------------------

function initResizeDetection(){
	$window = $(window);
	$wrapper = $("#wrapper");
	previousWindowWidth = $wrapper.width() + 1;
	setInterval(attemptToResize, 2000);
}

function attemptToResize(){
	if($wrapper.width() != previousWindowWidth){
		previousWindowWidth = $wrapper.width();
		onWindowResize();
	}
}

function onWindowResize(){
	updateMediaBasedOnWidth();
	var start = new Date().getTime();
	responsiveAutoArrange();
	responsiveAutoArrange();
	rwdExportSection();
	onWindowResizeExtra();
	$("#time").text('RWD Execution time: ' + (new Date().getTime() - start));
}

//function to be overiden
function onWindowResizeExtra(){
}

function updateMediaBasedOnWidth(){
	var w = $('#visualizationSection').width();

	if(w < 50){
		w = mediaAvailableResponsiveWidths[0];
	}

	if(w > mediaAvailableResponsiveWidths[1]){
		if(widthState != 1){
			widthState = 1;
			$("#pngWidth").val(mediaAvailableResponsiveWidths[0]);
			updateVisualization();
			updateMaskMap();
		}
	}else
		if ( w > mediaAvailableResponsiveWidths[2] && w <= mediaAvailableResponsiveWidths[1] ){
			if(widthState != 2){
				widthState = 2;
				$("#pngWidth").val(mediaAvailableResponsiveWidths[1]);
				updateVisualization();
				updateMaskMap();
			}
		}
	else
		if ( w <= mediaAvailableResponsiveWidths[2]){
			if(widthState != 3){
				widthState = 3;
				$("#pngWidth").val(mediaAvailableResponsiveWidths[2]);
				updateVisualization();
				updateMaskMap();
			}
		}
}
