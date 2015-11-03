
var currentMouseOverCountry = "", mouseOverCountry = "previous", maskCanvas, maskCanvas2dContext;
var visTooltip; 
var visualizationMask = new Image();

function getCountryFromColor(color){
	
	for (var index = 0; index < colorToCountry.length; index++) {
		if(colorToCountry[index][0] == color){
			return colorToCountry[index][1];
		}
	}
	return "";
}

function findPos(obj) {setTimeout
	var curleft = 0, curtop = 0;
	if(obj.offsetParent) {
		do {
			curleft += obj.offsetLeft;
			curtop += obj.offsetTop;
		}while(obj = obj.offsetParent);
		return {x: curleft, y: curtop };
	}
	return undefined;
}

function rgbToHex(r, g, b) {
	if(r > 255 || g > 255 || b > 255)
		throw "Invalid color component";
	return ((r << 16) | (g << 8) | b).toString(16);
}



function getVisualizationMaskUrl(width){
	return "/DVS/render/?plot=maskMap&media=png&width=" + width;
}

var previousBaseWidth = 0;
var previousVisWidth = 0;

function changePreviousVisWidth(){
	previousVisWidth = 1024;
}

///////////////////////////////////

function updateMaskMap(){
	if(!canvasSupported)
		return "canvas not Supported";

	var width = $("#pngWidth").val();

	$visualization = $("#visualization");

	if(svgSupported)
		$visualization = $("#svgContainer");

	//var visWidth = $("#svgContainer").width();
	//var visHeight = $visualization.height();

	var visWidth = $visualization.width();
	var visHeight = $visualization.height();

	if(visHeight < 100){
		// setTimeout(updateMaskMap, 500);
	}

	if(previousBaseWidth!= width || previousVisWidth != visWidth ){
		visualizationMask.src = getVisualizationMaskUrl(width);

		visualizationMask.onload = function() {
			maskCanvas.getContext("2d").drawImage(visualizationMask, 0, 0, visWidth, visHeight );
			maskCanvas2dContext = maskCanvas.getContext("2d");
		};
		
		visualizationMask.onerror = function() {
			changePreviousVisWidth();
		};
	}

	previousBaseWidth = width;
	previousVisWidth = visWidth;

}

///////////////////////////////////

function getMapVisTooltipTheme(){
	return {
		skin: 'light',
		hook: 'topmiddle',
		target: 'mouse',
		background: {opacity:1.0}
	};
}

function enhanceMapWithTooltips(){
	
	if(!canvasSupported)
		return "canvas not Supported";

		if(svgSupported)
			$('#svgContainer').mousemove(mapTooltip).click(mapTooltip);
		else
			$('#visualization').mousemove(mapTooltip).click(mapTooltip);
}

var $visualization; // set by mapTooltip

function mapTooltip(e){

	if($("#plot").val() == "heatMap"){
		$visualization = $(this);

		var hex = getHexColor(e, this);

		currentMouseOverCountry = getCountryFromColor(hex);

		if(currentMouseOverCountry != ""){
			var tooltipHTML = getCountryMouseOverHTMLTooltip(currentMouseOverCountry);
			
			if(tooltipHTML == ""){
				removeEUMapTooltips();
				currentMouseOverCountry = "";
			}

			if(isUndefined(visTooltip) || isUndefined(visTooltip) || visTooltip == null || visTooltip.items().length == 0 ){
				visTooltip = Tipped.create($visualization, tooltipHTML, getMapVisTooltipTheme());
			}
			
			$visTooltipContainer = $(visTooltip.items()[0].container).find(".t_ContentContainer");
			
			if( mouseOverCountry != currentMouseOverCountry || $visTooltipContainer.html() == ""){
				mouseOverCountry = currentMouseOverCountry;

				$visTooltipContainer.html(getCountryMouseOverHTMLTooltip(currentMouseOverCountry));

				if(currentMouseOverCountry == "")
					removeEUMapTooltips();
				else
					visTooltip.items()[0].options.background.opacity = 1;
			}
			if(visTooltip!=null){
				visTooltip.refresh();
				visTooltip.show();
			}

		}else{
			removeEUMapTooltips();
		}
	}else{
		removeEUMapTooltips();
	}
	
	//mouseOverCountry = currentMouseOverCountry;
}

function getEUMapTooltipContent(currentMouseOverCountry){
	return (getCountryMouseOverHTMLTooltip(currentMouseOverCountry));
}

function isUndefined(aReference){
	return typeof(aReference) == 'undefined' || typeof aReference === 'undefined';
}

function removeEUMapTooltips(){
	currentMouseOverCountry = "";
	//mouseOverCountry = "previous";
	if(!isUndefined(visTooltip) && visTooltip !=null){

		visTooltip.hide();
		//visTooltip.remove();
		//visTooltip = null;

	}
}

function goToInCountry(e){
	if($("#plot").val() == "heatMap"){
		if(currentMouseOverCountry != ""){
			selectOptionChange("country", currentMouseOverCountry);
			selectOptionTriggerChange("plot", "inCountry");
			updateProxyPlot();
			$("#country").trigger("chosen:updated");
		}
	}
}

function updateProxyPlot(){
	$(".visualisationSelections li.selected").removeClass("selected");
	$(".visualisationSelections li [proxyPlot='"+$("#plot").val()+"']").each(function(){
		$(this).parent().addClass("selected");
	})
}

function getHexColor(e, element){

	var pos = findPos(element);
	var x = e.pageX - pos.x;
	var y = e.pageY - pos.y;

	var enlarge = 1;
	if(isUndefined(maskCanvas2dContext))
		return "";
	var p = maskCanvas2dContext.getImageData(x * enlarge, y  , 1, 1).data;
	var hex = ("000000" + rgbToHex(p[0], p[1], p[2])).slice(-6);

	return hex;

}

function getCountryMouseOverHTMLTooltip(country){
	
	if($("#plot").val()!="heatMap" || heatMapTooltips == null || isUndefined(heatMapTooltips) )
		return(country);

	if(country == "")return country;

	var result = "";
	var countryName = "";
	var count =0;

	for(var i=0;i<heatMapTooltips.length;i++)	{
		var row = heatMapTooltips[i];

		if(row[0] == country){
			count ++;
			if(countryName == ""){
				countryName = row[1];
			}

			var isSelected = (row[2] == $("#answer option:selected").text());

			result = result +
				"<li>" +
					row[2] + " : " +
					( isSelected ? "[" : "" ) +
						row[3] +
					( isSelected ? "]" : "" ) +

					" %" +

				" </li>\t";
		}else if(countryName != "") break;
	}

	if(count==1)
		result = result.replace(/[\%]/g,"");

	if(result.length == ""){
		removeEUMapTooltips();
		return "";
	}

	return "<a href='javascript:goToInCountry()' style='color:black!important' >" + "<h4><img src='/DVS/DVT/garnish/arrowRight.png'/> "+countryName+"</h4><hr/><ul>"+result+"</ul>" + "</a>"
}
