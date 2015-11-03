var svgOriginalWidth = 740;
var svgOriginalHeight = 740;
var svgResponsiveHeight = 740;
var resolveCountries = false;
var alertFill = false;
var alertFill = false;
var colorsToIgnore = ["rgb(255, 255, 255)", "rgb(242, 242, 242)", "none", "#ffffff", "#39a935", "black", "#000000", "rgb(53.333333%,61.176471%,75.686275%)", "rgb(0, 0, 0)"];
var requestedVisualizationUrl = "";
var isFirst = true;

//-----------------------

var DVSPalletHex = 
	["#A7BD43", "#7FADBA", "#DCAB5D",
	 "#E5DCBB", "#C5C8BF", "#92A8B4", 
	 "#CF9173", "#E4D6A3"];

var DVSPalletRGB = 
	["rgb(167, 189, 67)", "rgb(127, 173, 186)", "rgb(220, 171, 93)",
	 "rgb(229, 220, 187)", "rgb(197, 200, 191)", "rgb(146, 168, 180)",
	 "rgb(207, 145, 115)", "rgb(228, 214, 163)"];

function getGroupIndexByColor(colorToCompare){
	for(var i=0;i<DVSPalletHex.length;i++){
		if(
			colorToCompare == DVSPalletHex[i].toLowerCase() ||
			colorToCompare == DVSPalletRGB[i]
		) return i;
	}
	return -1;
}

//-----------------------

	var previousLink = "";
	function updateSVG(){
		var requestLink = $("#svgExport").attr("href");
		if(requestLink == previousLink){
			return "same requestLink";
		}
		setTimeout(surfaceFadeOut, 10);
		previousLink = requestLink;


		if(!isFirst){
			$("#loadingVisualizationSection") 
				.css("opacity", 0)
				.removeClass("inv")
				.css("opacity", 0)
				.css('display',"block")
				.find("svg")
				.css("display", "block")
				.css("width","96%")
				.css("height", "auto")
				.css("height", widthState==3?"60%":"80%")
				.parent().fadeTo('fast', 1);

			stopvibrateLoading = false;
			vibrateLoading();
		}
	
		$.ajax({
			url:requestLink,
			success: function(response) {
				if(response.length<100){
					//alert(requestLink);
					return;
				}
				if(requestedVisualizationUrl == (requestLink + "visited")){
					setTimeout(removeLoading, 200);
					return;
				}
				
				requestedVisualizationUrl = requestLink + "visited";
				
				$svgContainer	= $("#svgContainer");
				$visualization = $("#visualization");
				
				$svgContainer.html(response);

				$svgContainerSvg = $svgContainer.find("svg");
				$svgContainerSvg.css("display", "none");
				$svgContainerSvg.css("display", "block");
				

				svgOriginalWidth = parseFloat($svgContainerSvg.attr("width").replace("pt",""));
				svgOriginalHeight = parseFloat($svgContainerSvg.attr("height").replace("pt",""));

				enhanceSVG();
				isFirst = false;

			},
			error:function (xhr, ajaxOptions, thrownError){
			}
		});
		setTimeout(checkSVGContent, 5000);
	}

	function fadeLoadingVisualizationSection(){
		$("#loadingVisualizationSection").fadeTo('slow', 0);
		$("#loadingVisualizationSection svg").fadeTo('slow', 0);
		setTimeout(hideLoadingVisualizationSection, 1600);
	}
	function hideLoadingVisualizationSection(){
		$("#loadingVisualizationSection").css('display',"none")
	}

	var stopvibrateLoading = false;
	function removeLoading(){
		stopvibrateLoading = true;
		fadeLoadingVisualizationSection();
	}

	function vibrateLoading(){
		if(!stopvibrateLoading)
			jQuery("#loadingVisualizationSection svg").each(function(){
				if(!stopvibrateLoading)
					jQuery(this).fadeTo( "slow" , 0.6, function() {
						if(!stopvibrateLoading)
							jQuery(this).fadeTo( "slow" , 1, function() {
								vibrateLoading();
							});	
						else jQuery(this).fadeTo( "slow" , 0)
						});
				else jQuery(this).fadeTo( "slow" , 0)

			});	
	}	

	function checkSVGContent(){
		if($("#svgContainer").html().length == 0 ){
			updateSVG();
		}
	}

	function calculateResponsiveHeightForAndroid(){
		$svgContainerSvg = $("#svgContainer svg");

		var svgResponsiveWidth = $svgContainerSvg.width();

		if(svgResponsiveWidth != null){
			if(svgResponsiveWidth != 100){
				svgResponsiveHeight = (svgResponsiveWidth/svgOriginalWidth) * svgOriginalHeight;
				setTimeout(resizeSVGforAndroid, 100);
			}
		}else{
			setTimeout(calculateResponsiveHeightForAndroid, 200);
		}
	}

	function resizeSVGforAndroid(){
		$('#svgContainer svg').css('height', svgResponsiveHeight);
		$('#svgContainer svg').css('left', "0px");
		$('#svgContainer svg').css('margin-left', "0px");
		setTimeout(updateMaskMap, 1220)
	}
	
	function enhanceSVG(){
	

		$visualization = $("#visualization");
		$svgContainer	= $("#svgContainer");

		$svgContainer.css("opacity", 0);

		$svgContainerSvg = $svgContainer.find("svg");

		$svgContainerSvg.css("width","98%");
		$svgContainerSvg.css("height", "auto");
		$svgContainerSvg.css("height", "100%");

		calculateResponsiveHeightForAndroid();

		//$surface = $("#svgContainer g[id*='surface']");;
		//$($surface.children()).css("opacity", 0);

		//$svgContainer.removeClass("hidden");

		makePathInteractive();
		setTimeout(changePreviousVisWidth, 900);
		setTimeout(removeLoading, 800);
		setTimeout(surfaceMapBodyFadeIn, 1000);
	}

	function swapWithSVGContainer(){
		$("#media").val("svg");
		$("#svgContainer").removeClass("hidden");
		$("#visualization").addClass("inv");
	}

	var surfaceChilds;
	var surfaceChildIndex = 1 ;

	function surfaceMapBodyFadeIn(){

		var speed = 30;
		var plot = $("#plot").val();
		if(plot == "inCountry")speed = 20;
		if(plot == "crossCountry")speed = 40;
		if(plot == "heatMap")speed = 60;

		fadeChildrenOf("#svgContainer g[id*='surface']", speed);
		$("#svgContainer").css("opacity", 1);
	}

 
	function fadeChildrenOf(elementPath, speed){
		$surface = $(elementPath);
		surfaceChilds = $surface.children();
		surfaceChilds.css("opacity", 0);
		surfaceChildIndex = 0;
		surfaceChildrenCount = surfaceChilds.length;

		for(var i = 0; i <= surfaceChildrenCount;i++){
			setTimeout(
				function() {
					surfaceChildIndex++;
					$(surfaceChilds[surfaceChildIndex]).fadeTo("slow", 1);
				},
			i * speed);
		}
	}

 
	function surfaceFadeOut(){
		$surface = $("#surface0");
		surfaceChilds = $surface.children();
		surfaceChildIndex = surfaceChilds.length-2;
		var speed = 30;
		if($("#plot").val() == "inCountry")speed = 10;
		for(var i = surfaceChildIndex+1; i>=0;i--)
			setTimeout(function() {
				surfaceChildIndex--;
			$(surfaceChilds[surfaceChildIndex]).fadeTo("slow", 0);
			}, i * speed);
	}

	function makePathInteractive(){
		var elementQuery = "#svgContainer path";

		$(elementQuery).mouseenter(function(e) {

			$this = $(this);
			var plot = $("#plot").val();

			var fill = $this.css("fill");

			for(var i = 0; i<colorsToIgnore.length; i++)
				if(colorsToIgnore[i] == fill)
					return "ignore color";

			var dAttr = $this.attr("d").substring(0, 21);

			if(alertFill)
				alert(fill);

			$elementsOfSameStyle = $this; // select this
			var currentClass = $elementsOfSameStyle.attr("class");
			var isMalta = false;
			
			if(plot == "heatMap"){

				if(fill == "rgb(26, 26, 26)" || fill == "#1a1a1a"){
					isMalta = true;
					currentMouseOverCountry = "MT";
				}else{
					currentMouseOverCountry = getCountryForPath(dAttr);
				}

				Tipped.create(
					$this,
					function(element) {
						return getCountryMouseOverHTMLTooltip(currentMouseOverCountry);
					},
					getMapVisTooltipTheme()
				);			

				var countryMouseOver = currentMouseOverCountry;

			}

			var thisClass = $this.attr("class");
			if(resolveCountries || plot == "inCountry" || plot == "subsetTimeSeries"){
				$elementsOfSameStyle = $("[class='"+ thisClass +"']");
				//$("h1").text(currentMouseOverCountry);
			}

			if(plot == "subsetTimeSeries"){
 
				Tipped.create(
					$this,
					function(element) {
 
						var groupIndex = getGroupIndexByColor(fill);
						if(groupIndex != -1){

						 
							var $previousElement = $this.prev();
							var counter = 0;
							var _fill=$this.css("fill");

							while($previousElement.length != 0){
								if($previousElement.css("fill")!=_fill)
									break;
								counter++;
								$previousElement = $previousElement.prev();
							}
 
 							var subset_value = uniqueArrayTableBy(csvDataArray, 3)[groupIndex];
							var onFocusRow = orderArrayTableBy(indexTable(filterArrayTableBy(csvDataArray, 3, subset_value)), 5, 1)[counter];
 							var answerText = getSelectOptionTextBySanitizedKey("#answer", onFocusRow[4] );
							return  "<h3>" 
									//+ onFocusRow[1] +
									+ getSelectOptionTextByKey("#country", onFocusRow[1] ) + " - " +onFocusRow[0]+
									"</h3><div>"
									+ "Subset: "+ getSelectOptionTextBySanitizedKey("#subsetValue", onFocusRow[3] ) +
									  "</div><div> Answer: "+ answerText +
									"</div>"+
									 
									"<h1>" 
									+ onFocusRow[5] +
									
									"</h1>";

									//"<dl class='major'><dt class='variableName'>Value</dt><dd class='variableValue'>"+ onFocusRow[5] +"</dd></dl>" ;
							
						} 
					},
					getMapVisTooltipTheme()
				);	

			}



			if(plot != "euMatrix")
			if(!isMalta)
				$elementsOfSameStyle.attr("class", "visualizationElementHighlighted");
					
			$this.mouseleave(function() {
				$elementsOfSameStyle.attr("class", currentClass);
				
				if(plot == "euMatrix"){
					positionHorizontalHair("#horizontalHair", "-1");
					positionVerticalHair("#verticalHair", "-1");
				}

			});

			if(resolveCountries)
				$this.click(function(e) {
					alert($elementsOfSameStyle.length);
					var patterns="";
					for(var i=0;i<$elementsOfSameStyle.length;i++){
						patterns+= "\n" + currentMouseOverCountry + "\t" + $($elementsOfSameStyle[i]).attr("d").substring(0, 21);
					}
					alert(patterns);
				});

			if(plot == "euMatrix"){
				var pathMidXY = getPathMidXY($this);
				positionHorizontalHair("#horizontalHair", pathMidXY[0]);
				positionVerticalHair("#verticalHair", pathMidXY[1]);
			}

		});
		
	}

	function makeTextInteracrive(){
			var elementQuery="use";
			$(elementQuery).mouseenter(function() {
			$this = $(this);
			$elementsOfSameStyle = $(
				"[style='"+ $this.parent().attr("style") +"']"
			);

			$elementsOfSameStyle.css("fill-opacity", 0.5 );

			$this.mouseleave(function() {
				$elementsOfSameStyle.css("fill-opacity", 1 );
			});
		});
	}

function getSelectOptionTextBySanitizedKey(select, _key){
	return getSelectOptionTextByKey(select, _key.replace(/[^a-zA-Z0-9]/g, "-"));
}

function getSelectOptionTextByKey(select, key){

	var $options = $(select + " option");
	for(var i=0;i<=$options.length;i++){
		if($($options[i]).val() == key)
			return $($options[i]).text();
	}
	return "Unknown";
}

////////////////////////////////////

//var csvDataArrayInverted = [];
var csvDataArray = [];

function invertArrayTable(result){
	return result[0].map(function(col, i) {
			    return result.map(function(row) {
			        return row[i];
			    });
	});
}

function updateTooltipsJs(){
	if(!svgSupported){
		return false;
	}

	if($("#plot").val() == "heatMap")
		$.getScript("/DVS/render/plots/js/"+ getPlotBodyFileName()+ ".js" ).done(
			function(script, textStatus) {
		}).fail(function(jqxhr, settings, exception) {
		});

	if($("#plot").val() == "subsetTimeSeries"){
		$.get($("#csvExport").attr("href"), function(data) {
			dataRows = data.replace(/\"/g, '').replace(/\n$/g, "").split("\n");
			var result = [];
			for(var rowIndex=0;rowIndex<dataRows.length;rowIndex++){
				if(rowIndex>0)
					result[rowIndex-1] = dataRows[rowIndex].split("\t");
			}
			csvDataArray = result;
			//csvDataArrayInverted = invertArrayTable(result);			
		});
	}

}


function positionHorizontalHair($horizontalHair, y){

	$horizontalHair = $($horizontalHair);

	$horizontalHair.attr("x1","0%");
	$horizontalHair.attr("x2","100%");

	$horizontalHair.attr("y1", y);
	$horizontalHair.attr("y2", y);
}


function positionVerticalHair($verticalHair, x){

	$verticalHair = $($verticalHair);

	$verticalHair.attr("y1","0%");
	$verticalHair.attr("y2","100%");

	$verticalHair.attr("x1", x);
	$verticalHair.attr("x2", x);
}

function getPathMidXY(elementPath){
	var elementMinMaxXY = getPathMinMaxXY(elementPath);
	return [(elementMinMaxXY[0] + elementMinMaxXY[1]) / 2,  (elementMinMaxXY[2] + elementMinMaxXY[3]) / 2 ];
}

function getPathMinMaxXY(elementPath){

	var pathData = $(elementPath).attr("d").split(" ");
	var pathDataNumbers = [];

	for(var i = 0 ; i < pathData.length;i++){
		if(!isNaN(parseFloat(pathData[i]))){
			pathDataNumbers[pathDataNumbers.length] = parseFloat(pathData[i]);
		}
	}

	var minX = 1000;
	var maxX = 0;
	
	var minY = 1000;
	var maxY = 0;
	
	for(var i = 0 ; i < pathDataNumbers.length;i++){  
	  if(i%2 == 0){//y
	    if(pathDataNumbers[i] < minY)
	        minY = pathDataNumbers[i];
	    if(pathDataNumbers[i] > maxY)
	        maxY = pathDataNumbers[i];    
	  }else{//x
	      if(pathDataNumbers[i] < minX)
	        minX = pathDataNumbers[i];
	    if(pathDataNumbers[i] > maxX)
	        maxX = pathDataNumbers[i];
	  
	  }
	}
	
	return [minX, maxX, minY, maxY];
} 

function getCountryForPath(pathD){
	var widthVariation = 740;
	if(widthState == 2)
		widthVariation = 740;
	if(widthState == 1)
		widthVariation = 920;
	if(widthState == 3)
		widthVariation = 500;


	for(i=0;i<countryToPathD.length;i++){
		//if(widthVariation == countryToPathD[i][2])
			if(pathD.indexOf(countryToPathD[i][1])!=-1){
				return countryToPathD[i][0];
			}
	}
	return "";
}

