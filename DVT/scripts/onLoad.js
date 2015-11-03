var canvasSupported = false;
var svgSupported = false;

var plotSelectionTooltipOn = false;

var hadUrlParameters = false;
var isInitialized = false;


function supportsCanvas(){
	return resolveCountries;
	//return !! document.createElement("canvas").getContext;
}


function supportsSvg() {
	//return false;
	var N=document.createElement("div");
	N.innerHTML="<svg></svg>";
	var O=N.firstChild&&"namespaceURI" in N.firstChild&&N.firstChild.namespaceURI=="http://www.w3.org/2000/svg";
	return O;
	//http://html5test.com/index.html
}


$(function(){

	canvasSupported = supportsCanvas();

	if(supportsSvg() == true){ 
		svgSupported = true;
		$("#visualization").addClass("inv").remove();

		
		$.ajax({
			url:"http://89.0.1.77/DVS/DVT/img/loading-animation.svg",
			dataType: "text",
			success: function(response) {
				$("#loadingVisualizationSection").html(response).removeClass("inv");
			}, error:function (xhr, ajaxOptions, thrownError){}
		});


	}else{

		$("#loadingVisualizationSection").removeClass("inv").html("<img id='gifLoadingAnimation' src='img/loading-animation.gif' title='Loading...' width='740px' height='740px' />");
			//http://gifmake.com/
	}

	$(".secondary").addClass("hidden");
	
	if($(window).width() < 805){
		$("#leftColumn").addClass("hidden");
	}else{
		if($("#leftColumn").length==0)
			$(".questionSection").removeClass("hidden");
	}

	//-----------------------------
	var secondaryVisualizations = "";
	//proxyPlotSelection remove secondary
	for(var i = primaryVisualizationUpto;i<plotFilesParameters.length/2;i++){
		$proxyPlot = $("#proxy_plot_label"+i);
		$proxyPlot.addClass("secondary");
		$proxyPlot.addClass("hidden");
	}

	//-----------------------------
	isInitialized = false;

	if(window.location.search.indexOf("?") != -1){
		hadUrlParameters = true;
	}else{
		hadUrlParameters = false;
	}

	loadSelectValues($("#subset")[0], subset_);	
	initLinkedSelect($("#subset")[0], $("#subsetValue")[0], subset_subsetValue);
	
	loadSelectValues($("#topic")[0], topic_);
	
	loadOptGroupSelectValues($("#question")[0], topic_question)
	
	//initLinkedSelect($("#topic")[0], $("#question")[0], topic_question);
	
	initLinkedSelect($("#question")[0], $("#answer")[0], question_answer);

	loadSelectValues($("#country")[0], country_);
	loadInverseSelectValues($("#countryB")[0], country_);

	onChangeUpdateVisualization($("#plot"));
	onChangeUpdateVisualization($("#question"));
	onChangeUpdateVisualization($("#subset"));
	onChangeUpdateVisualization($("#subsetValue"));
	onChangeUpdateVisualization($("#answer"));
	onChangeUpdateVisualization($("#country"));
	onChangeUpdateVisualization($("#countryB"));

	//changeClassOnHover("section", "current");

	$("#plot").change(function(){
		visualizationFunctionChange(this);
	});

	$("#locale").change(function(){
		window.location = window.location.pathname + "?" + getFormParameters($("#graphControlsForm"));
	});

	$("#plot").trigger("change");

	$("#prevQuest, .prevQuest").click(function(){prevQuestion();});
	$("#nextQuest, .nextQuest").click(function(){nextQuestion();});



	Tipped.create(
		$("#prevQuest"),
		function(element) {
				var $questions	= $("#question")[0];
				var questionIndex = $questions.selectedIndex;
				var questionsLength = $questions.length;
				var prevQuestTitle = $questions[questionIndex > 0 ? questionIndex - 1 : questionsLength - 1].text;
				
				if(prevQuestTitle.length == 0){
					prevQuestTitle=$questions[questionsLength - 1].text;
				}
			
			return prevQuestTitle;
		}, getTopTooltipSkin()
	);
	
	Tipped.create(
		$(".prevQuest"),
		function(element) {
				var $questions	= $("#question")[0];
				var questionIndex = $questions.selectedIndex;
				var questionsLength = $questions.length;
				var prevQuestTitle = $questions[questionIndex > 0 ? questionIndex - 1 : questionsLength - 1].text;
				if(prevQuestTitle.length == 0){
					prevQuestTitle=$questions[questionsLength - 1].text;
				}
			return prevQuestTitle;
		}, getTopTooltipSkin()
	);
	

	Tipped.create(
		$("#nextQuest"),
		function(element) {
				var $questions	= $("#question")[0];
				var questionIndex = $questions.selectedIndex;
				var questionsLength = $questions.length;
				var nextQuestTitle = $questions[questionIndex < questionsLength - 1 ? questionIndex + 1 : 0].text;
				if(nextQuestTitle.length == 0){
					nextQuestTitle=$questions[1].text;
				}
			return nextQuestTitle;
		}, getTopTooltipSkin()
	);
	
	Tipped.create(
		$(".nextQuest"),
		function(element) {
				var $questions	= $("#question")[0];
				var questionIndex = $questions.selectedIndex;
				var questionsLength = $questions.length;
				var nextQuestTitle = $questions[questionIndex < questionsLength - 1 ? questionIndex + 1 : 0].text;
				if(nextQuestTitle.length == 0){
					nextQuestTitle=$questions[1].text;
				}
			return nextQuestTitle;
		}, getTopTooltipSkin()
	);

	$("#moreVisualizations span").click(function(){ 
		var $this = $(this).parent();
		$this.toggleClass("checked");
		var $thisSpan = $this.find("span");
		if($this.hasClass("checked")){
			$thisSpan.text($thisSpan.attr("pressed"))
		}else{
			$thisSpan.text($thisSpan.attr("unpressed"))
		}
		$(".secondary").toggleClass("hidden");
	});
	
	$("#csvView").click(function(){ if($(this).is(':checked')) viewVisualizationData(); else swapVisualizationData(); });
	
	
 

	$("#onlyEU").click(function(){
		updateVisualization();
	});

	dialogInit();
	dialogURLInit();

	$("#toolUrlExport, #toolUrlExportProxy").click(function(event){
		$("#dialogContentURL").text($("#toolUrlExport").attr("href")).dialog("open").dialog('option', 'title', toCopy + ": " + $(this).text());
		$("#dialogContentURL").selectText();
		event.preventDefault();
	});

	$("#htmlExport, #htmlExportProxy").click(function(event){
		$("#dialogContent").text(getVisualizationHTML()).dialog("open").dialog('option', 'title', toCopy + ": "+ $(this).text());
		$("#dialogContent").selectText();
		event.preventDefault();
	});

	$("#citeExport, #citeExportProxy").click(function(event){
		$("#dialogContent").text(getVisualizationCiteReference()).dialog("open").dialog('option', 'title', toCopy + ": " + $(this).text());
		$("#dialogContent").selectText();
		event.preventDefault();
	});

	$("#label_csvView, #label_onlyEU").click(function(event){
		$this = $(this);

		$thisInput = $this.find("input");

		$thisInput.prop(!$thisInput.prop("checked"));
		$this.removeClass("checked");
		if($thisInput.is(':checked')){
			$this.addClass("checked");
		}

	});


	responsiveVisualizationSelection();

	selectElementsBasedOnUrl();

	updateVisualizationDepedencies();
	tooltipEnhancements();

	setTimeout(responsiveAutoArrange, 1300);
	//setTimeout(rwdExportSection, 5000);
	setTimeout(changePreviousVisWidth, 1200);
	setTimeout(changePreviousVisWidth, 4200);

	initResizeDetection();
	setTimeout(updateMediaBasedOnWidth, 2000)


	enhanceMapWithTooltips();

	$("#formatSelectorTooltips a").click(function(event){
		$($(this).attr("src")).click();
	});


	$("a.proxyable").each(function() {
		$this = $(this);
		$this.click(function(event){
			$this = $(this);
			$targetElement = $($this.attr("src"));	
			$targetElement.prop("checked", !$targetElement.prop('checked'));
			
			targetElementIsChecked = $targetElement.prop('checked');

			isCsvView= $this.attr("src") == "#csvView";
			
			if(isCsvView)$this = $this.parent();
							
			
			$this.removeClass("selected");			
			if(targetElementIsChecked)
				$this.addClass("selected");
			
			if(isCsvView){
 				if(targetElementIsChecked){
 					viewVisualizationData(); 
 				}else {
 					swapVisualizationData();
 				}
			}else{			 
				updateVisualization();
			}

		});		
	});
	setTimeout(selectAQuestion, 1600);

});

function selectAQuestion(){
	if($("#question").find(":selected").val() == ""){
		nextQuestion();
	}
	$("div#leftmenu, article").css("opacity",1);
}


function dialogInit(){
	$( "#dialogContent" ).dialog({
			autoOpen: false,
			width: 400,
			buttons: [
				{
					text: selectContent ,
					click: function() {
						$("#dialogContent").selectText();
					}
				},
				{
					text: ok,
					click: function() {
						$( this ).dialog( "close" );
					}
				}
			]
		});
}

function dialogURLInit(){
	$( "#dialogContentURL" ).dialog({
			autoOpen: false,
			width: 400,
			buttons: [
				{
					text: selectContent ,
					click: function() {
						$("#dialogContentURL").selectText();
					}
				},
				{
					text: visitURL,
					click: function() {
						window.location.href = $("#toolUrlExport").attr("href");
					}
				},
				{
					text: ok,
					click: function() {
						$( this ).dialog( "close" );
					}
				}
			]
		});
}

jQuery.fn.selectText = function(){
	var doc = document, element = this[0], range, selection;
	if (doc.body.createTextRange) {
		range = document.body.createTextRange();
		range.moveToElementText(element);
		range.select();
	} else if (window.getSelection) {
		selection = window.getSelection();
		range = document.createRange();
		range.selectNodeContents(element);
		selection.removeAllRanges();
		selection.addRange(range);
	}
};

function getBaseUrl(){
	return location.protocol+'//'+location.hostname+(location.port ? ':'+location.port: '');
}

function getVisualizationHTML(){

	var pngImageURL = $("#pngExport").first().attr('href');
	pngImageURL= pngImageURL.substring(0, pngImageURL.indexOf("&down"));

	return ""+
	"<section>\n" +
	"	<a href='"+$("#toolUrlExport").attr("href")+"'>\n" +
	"		<img src='"+getBaseUrl()+ pngImageURL + "'/>\n" +
	"	</a>\n" +
	"	<p>" + $(".freetext p").text().trim()+"</p>\n" +
	"</section>";
}

function getVisualizationCiteReference(){
	return ""+
	"@ONLINE{EF:2012:Online,\n" +
	"	author = {"+ $('header h1').text().replace("\\t","").trim() +"},\n" +
	"	title = {" + getVisualizationTitle() + "},\n" +
	"	year = {2012},\n" +
	"	url = {"+ $('#toolUrlExport').attr('href') + "}\n" +
	"}";
}

function tooltipEnhancements(){

	Tipped.create(
		'.buttom-tooltip',
		getButtomTooltipSkin()
	);

	Tipped.create(
		'.side-tooltip',
		getSideTooltipSkin()
	);

	Tipped.create(
		'.top-tooltip',
		getTopTooltipSkin()
	);

	var exportTitle = $(".exportOptions h2").html();
	var $exportLinks = $(".exportOptions a");

	for(var i = 0; i<$exportLinks.length;i++){
		var $exportLink = $($exportLinks[i]);
		$exportLink.find("img").attr("src","/DVS/DVT/garnish/"+$exportLink.attr("id")+".png");
		exportLinkTitle = exportTitle + " > " + $exportLink.text();
		$exportLink.attr("title", exportLinkTitle);
	}
	
	Tipped.create(".exportOptions a", getButtomTooltipSkin());

	for(var i=0;i<plotFilesParameters.length;i++)
		Tipped.create('#proxy_plot_label' + i, 'plotSelectionTooltip' + i, getPlotSelectionTooltipSkin());

	Tipped.create('#moreVisualizations', 'moreVisualizationsTooltip', getPlotSelectionTooltipSkin());
	Tipped.create('#label_csvView', 'tableViewTooltip', getPlotSelectionTooltipSkin());
	Tipped.create('#label_onlyEU', 'onlyEUTooltip', getPlotSelectionTooltipSkin());
}

function getButtomTooltipSkin(){
	return {
			skin: 'light',
			hook: 'bottomleft',
			target: 'mouse',
			maxWidth: 185,
			closeButtonSkin: 'white',
			closeButton: true
		 };
}


function getSideTooltipSkin(){
	return {
			skin: 'light',
			hook: 'rightmiddle',
			target: 'mouse',
			maxWidth: 185,
			closeButtonSkin: 'white',
			closeButton: true
		 };
}

function getPlotSelectionTooltipSkin(){
	return {
			skin: 'light',
			hook: 'bottomleft',
			target: 'mouse',
			maxWidth: 200,
			inline:true,
			closeButtonSkin: 'white',
			closeButton: true,
			onShow: function(content, element) {
				plotSelectionTooltipOn = true;
				if($window.height()<700){
				$(".plotSelectionTooltip img").addClass("hidden");
				plotSelectionTooltipOn = false;
			}else{
				$(".plotSelectionTooltip img").removeClass("hidden");
			}

			Tipped.refresh(element);
		},
			onHide: function(content, element) {
				plotSelectionTooltipOn = false;
				// content and element are available here
			}
	};
}

function getTopTooltipSkin(){
	return {
			skin: 'light',
			hook: 'topright',
			target: 'mouse',
			maxWidth: 185,
			closeButtonSkin: 'white',
			closeButton: true
		 };
}

function getTopLeftTooltipSkin(){
	return {
			skin: 'light',
			hook: 'topleft',
			target: 'mouse',
			maxWidth: 185,
			closeButtonSkin: 'white',
			closeButton: true
		 };
}

function responsiveVisualizationSelection(){
	var $plot = $("#plot")[0];

	for (var i = 0; i < $plot.options.length; i++) {
		//$plot.options[$plot.options.length]

		$(".proxy_plot_leftlabel" + i).click(function(){
			$this = $(this);
		
			$(".visualisationSelections li.selected").removeClass("selected");
			$this.parent().addClass("selected");
			var proxyPlot =  $this.attr("proxyPlot");
			selectOptionTriggerChange("plot", proxyPlot);
		});
 
	}
}

//-------------------------------


function createResponsiveProxyLinksFromSelect(from, nextTo, inDivWithId, titled){

		$from = $(from);
		$from.addClass("hidden");

		$nextTo = $(nextTo);

		var html = "<div id='"+ inDivWithId + "' style='margin:0px;'>";

		if(titled){
			html += "<strong>" + $from.attr("title") + "</strong>";
		}

 		html += "<ul>";

 		var selectedValue = $("#" + $from.attr("id") + " option:selected").val();

		for (var i = 0; i < from.options.length; i++) {
			html += "<li "+( selectedValue ==  from.options[i].value ? " class='current' " : "" ) + "><a href='#' title='" + from.options[i].text +"' class='buttom-tooltip' rel='" + from.options[i].value + "' >" + from.options[i].value.toLowerCase() + "</a></li>";
		}

		html += "</ul></div>";

		$nextTo.parent().append(html);

		$("#" + inDivWithId + " ul li a").click(function(){
			$from.val($(this).attr("rel")).attr('selected', true);
			$from.trigger('change');
		});
}
