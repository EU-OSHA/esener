<?php //kp//eworx//gr//EWORX S.A.
///////////////////////////////////////////////////////////////////////////
require_once("DVT.top.php");
//not possible to be fetched when hosted on a different domain	by some browsers due to the https
//https://www.eurofound.europa.eu/css/default.css
//"https://www.eurofound.europa.eu/css/default_print.css
///////////////////////////////////////////////////////////////////////////
$printStandaloneExportOptions = false;
?>
<!DOCTYPE HTML>
<html lang="en">
	<head>




		<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
		<meta name="viewport" content="width=device-width, initial-scale=1.0" />
		<title><?php echo $pageTitle; ?></title>
		<meta name="description" content="<?php echo $visualizationDescription; ?>" />
<?php /////////////////////////////////////////////////////////////////////////// ?>
		<link href="css/default.EF.css?v=<?php echo $version;?>" rel="stylesheet" media="screen" type="text/css" />
		<link href="css/default.EF_print.css?v=<?php echo $version;?>" rel="stylesheet" media="print" type="text/css" />

		<link href="css/common.css?v=<?php echo $version;?>" rel="stylesheet" type="text/css">
		<link href="css/screen.EF.css?v=<?php echo $version;?>" rel="stylesheet" type="text/css">

		<link href="scripts/3rd/udm-resources/udm-style.css" rel="stylesheet" media="screen" type="text/css" />

		<?php require_once("DVT.head.css.php");?>

<?php /////////////////////////////////////////////////////////////////////////// ?>

		<script type="text/javascript">
			var toCopy = "<?php echo fetchWord('toCopy');?>";
			var selectContent = "<?php echo fetchWord('selectContent');?>";
			var ok = "<?php echo fetchWord('ok');?>";
			var visitURL =  "<?php echo fetchWord('visitURL');?>";
		</script>
		
		<?php require_once("DVT.head.js.php");?>
		<script type="text/javascript">
		<!--
			var leftMenuHtml = "";


			function toggleProxyAble(){
				$("#exportSection").toggleClass("hidden");
				$(".proxyAble").each(function() {
					$this = $(this);
					var elementId = $this.attr("id");
					var proxyIdIndex = elementId.indexOf("Proxy");
					if(proxyIdIndex!=-1){
						$this.attr("id", elementId.substring(0, proxyIdIndex));
					}else{
						$this.attr("id", elementId+"Proxy");
					}

				});
			}

			$(function(){
				toggleProxyAble();
				$("#themeSwap").removeClass("hidden");

				$(".openable").click(function() {
					$this = $(this);
					if($this.next().hasClass("hidden")){
						$this.next().removeClass("hidden");
					}else{
						$this.next().addClass("hidden");
					}
				});

				//$(".visualizationSelection .filtersRight").addClass("hidden");

				createResponsiveProxyLinksFromSelect($("#locale")[0], $("h1")[0], "languages", true);
				Tipped.create('#languages ul li a', getButtomTooltipSkin());

				var topicMenuLink = "none";


				for(var questionIndex=0;questionIndex<topic_question.length;questionIndex++){
					if(topicMenuLink!=topic_question[questionIndex][0]){
						topicMenuLink = topic_question[questionIndex][0];
						if(questionIndex>0){
							leftMenuHtml += "</ul></li>";
						}
							leftMenuHtml += "<li class=\"open\"><a href=\"javascript:\" class=\"menulevelALink\">" +
								topic_question[questionIndex][0] +
								'<img class="udmA" alt=".." title="" src="scripts/3rd/udm-resources/right-navblue.gif" style="display: block; margin-top: 6px; clip: rect(0px, 4px, 11px, 0px); visibility: visible;">' + "</a>";
							leftMenuHtml += "<ul class=\"menulevelBul\">";

					}
					leftMenuHtml += "<li><a href=\"javascript:\" for=\""+ topic_question[questionIndex][1] + "\" class=\"questionProxy\">" +
					topic_question[questionIndex][2] + //" - " +
					// topic_question[questionIndex][1] +
					 "</a></li>" ;
				}
				leftMenuHtml += "</ul></li>";

				$("#udm").html(leftMenuHtml);

	
				$(".menulevelALink").hover(function(){
					var $this = $(this);
					$(".visibleSubMenu").removeClass("visibleSubMenu");
					$this.next().addClass("visibleSubMenu");
				});
				
				$(".menulevelALink").click(function(){
					var $this = $(this);
					$(".visibleSubMenu").removeClass("visibleSubMenu");
					$this.next().addClass("visibleSubMenu");
				});
	
				$("#udm").mouseleave(function(){
					$(".visibleSubMenu").removeClass("visibleSubMenu");
				});
				
				$(".questionProxy").click(function() {
					var questionCode = $(this).attr("for");
					var $question = $("#question");
					$question.find("option[value='"+questionCode+"']").attr("selected", "selected");
					$question.change();
					$(".visibleSubMenu").removeClass("visibleSubMenu");
				});

				function enhanceWithChosen(){
					var standardOptions = {disable_search_threshold: 10, no_results_text: "Oops, nothing found!", width:"100%"};

					$("#question").chosen(standardOptions);
					if($("#question_chosen").length==0){
						$("#question").parent().parent().addClass("withoutChoosen");
					}
					//$("#subset").chosen(standardOptions);
					//$("#subsetValue").chosen(standardOptions);
					$("#country").chosen(standardOptions);
					$("#countryB").chosen(standardOptions);
					//$("#answer").chosen(standardOptions);
				}

				enhanceWithChosen();

				$.each($("[proxyPlot='"+ $("#plot :selected").val() +"']"), function() {
					$(this).parent().addClass("selected");
				});

			});


			//function to be overiden
			function onWindowResizeExtra(){
				var contentWidth = $("#content").width();
				var mainWidth = $("body").width();

				var $leftDiv = $(".leftDiv");
				var $questionSection = $(".questionSection");
		 		var $centralColumn3 = $("#centralColumn3");
		 		var $eflogo = $("#eflogo");
		 		//var $comonVisControl = $(".visualizationSelection .filtersRight");

				if(mainWidth>805){//enable left menu
					$leftDiv.removeClass("hidden");
					//$questionSection.addClass("hidden");
					$centralColumn3.css("width", "78%");
					//if(!$comonVisControl.hasClass("hidden"));
					//	$comonVisControl.addClass("hidden");

					if($("#exportSection #toolUrlExport").length > 0){
						toggleProxyAble();
					}
					$(".filtersExtra .visualisationSelections").addClass("hidden");
				}else{//remove left menu
					$(".filtersExtra .visualisationSelections").removeClass("hidden");
					if($("#exportSection #toolUrlExport").length == 0){
						toggleProxyAble();
					}
					$leftDiv.addClass("hidden");
					//$questionSection.removeClass("hidden");
					$centralColumn3.css("width", "98%");
					//$comonVisControl.removeClass("hidden");
				}
				if(mainWidth>700){//logo resize
					$eflogo.attr("src","garnish/efLogo.png")
				}else{
					$eflogo.attr("src","garnish/efLogoSmall.png");
				}
			}

		//-->
		</script>



<?php /////////////////////////////////////////////////////////////////////////// ?>
	</head>

	<body>
	<div class="wailinks">
		<div id="tabs">
			<ul>
				<li id="ef">
					<a href="https://www.ohsa.europa.eu/"><abbr title="European Foundation for the Improvement of Living and Working Conditions">Eurofound</abbr></a>
				</li>
				<li id="eiro">
					<a href="https://www.ohsa.europa.eu/eiro/"><abbr title="European industrial relations observatory">EIRO</abbr></a></li><li id="emcc"><a href="https://www.ohsa.europa.eu/emcc/"><abbr title="European monitoring centre on change">EMCC</abbr></a></li><li id="ewco"><a href="https://www.ohsa.europa.eu/ewco/"><abbr title="European working conditions observatory">EWCO</abbr></a>
				</li>
			</ul>
		</div>
		<div id="waitoolbar">|<a accesskey="8" href="#contentpage">Skip to contents</a> |</div>
	</div>
	<div id="efbanner">
		<div>
			<a style="width: 200px; display: block; float: left;" accesskey="1" href="https://www.ohsa.europa.eu/index.htm">
			<img id="eflogo" src="garnish/efLogo.png" alt="European Foundation for the Improvement of Living and Working Conditions" width="161" height="80"></a>
			<div class="bgcolor">|</div>
		</div>
		<div id="servicetools">
			<ul>
				<li><a accesskey="2" href="https://www.ohsa.europa.eu/help/azindex.htm">A-Z index</a></li>
				<li><a accesskey="3" href="https://www.ohsa.europa.eu/help/sitemap.htm">Site Map</a></li>
				<li><a accesskey="4" href="https://www.ohsa.europa.eu/about/faq/index.htm"><acronym title="Frequently Asked Questions">F.A.Q.</acronym></a></li>
				<li><a accesskey="6" href="https://www.ohsa.europa.eu/help/index.htm">Help</a></li>
				<li class="last"><a accesskey="7" href="https://www.ohsa.europa.eu/help/contact.htm">Contact</a></li>
			</ul>
		</div>
		<div id="searchbox">
			<form id="searchForm" action="https://www.ohsa.europa.eu/search/search.php" enctype="application/x-www-form-urlencoded" method="post">
				<p><label for="searchText">Search Terms:</label> </p>
				<p><input id="searchText" class="box" name="searchText" size="15" type="text"> <input id="restrict" name="restrict" type="hidden" value="efweb"></p>
				<p><input class="button" type="submit" value="Search"></p>
			</form>
		</div>

	</div>
	<div id="path">
		<div class="Text">
			<span style="float: left;">
				<strong>You are here:</strong>&nbsp;<a href="https://www.ohsa.europa.eu/index.htm"><abbr title="European Foundation for the Improvement of Living and Working Conditions">Eurofound</abbr></a> &gt; <a href="https://www.ohsa.europa.eu/surveys/index.htm">Surveys</a> &gt; 
				<a href="https://www.ohsa.europa.eu/surveys/smt/index.htm">Survey Mapping Tool</a> &gt; 
			
			<?php if($survey =="3RDEQLS"){?>
				<a href="/surveys/smt/eqls/results.htm">3EQLS</a>
			<?php }else{ ?>
				<a href="/DVS/DVT/?dataSource=<?php print($survey);?>"><?php print($survey);?></a>
			<?php } ?>
				
				&gt; <span class="currpath">Survey Mapping Tool</span></span>
			<span style="text-align:right; float:right;">
			My Eurofound: <a href="https://www.ohsa.europa.eu/myeurofound.htm" style="color:#ffffff; font-weight:bold;" onmouseover="javascript: this.style.color = '#FFCC00';" onmouseout="javascript: this.style.color = '#ffffff';">Login or Sign Up</a> &nbsp;&nbsp;
			</span>
			<div class="clearfloat"></div>
		</div>
	</div>
	<div id="main">








	<div id="leftColumn" class="leftDiv">

		<div id="leftmenu">
			<ul>
			<li class="open"><a href="index.EF.php?dataSource=<?php print(survey);?>&locale=<?php print (!empty($_GET["locale"])?$_GET["locale"]:"EN");  ?>"><?php echo fetchTitle("survey");?></a></li>
			</ul>
		</div>
		<ul id="udm" class="udm">
			<li class="open hidden">
				<a href="#" class="menulevelALink">
					Job context (11)
					<span class="udmA" style="display: block; margin-top: 6px; clip: rect(0px 4px 11px 0px); visibility: visible;">
						<img src="scripts/3rd/udm-resources/right-navblue.gif" alt=".." title="">
					</span>
			</a>

			<ul class="menulevelBul">
				<li><a href="#">What is your employment status? (q6)</a></li>
			</ul>

			</li>
		</ul>

	<div id="leftmenu" class="udmbottom ">

		<ul>

			<li class="open leftVisualisationSelection">

				<h4 id="VisualizationsTitle"><?php echo fetchLabel("plot") ;?></h4>



				<ul class="visualisationSelections">
					<?php
					$theVisualizations =  fetchVisualizations();
					foreach(fetchVisualizations() as $index => $visualization){?>
						<li>
							<a href="javascript:" class="proxy_plot_leftlabel<?php echo $index?> <?php if($index == 0){?> checked<?php }?> " title="<?php echo fetchTooltip($visualization[0]) ;?>" proxyPlot="<?php echo $theVisualizations[$index][0]; ?>">
								<?php echo $visualization[1];?>
							</a>
						</li>
					<?php } ?>				

				</ul>

				<h4 id="VisualizationsTitle"><?php echo fetchLabel("plot") ;?> data</h4>
				<ul class="visualisationSelections">
					<li><a href="javascript:" class="proxy_plot_leftlabel7 proxyable" src="#csvView" title="<?php echo fetchTooltip("dataTable") ;?>"><?php echo (fetchViewCSVData());?></a></li>
				</ul>

			</li>
		</ul>

		<ul>

			<li class="open leftExportOptions" >

				<h4><?php echo fetchTitle("exportOptions");?></h4>
				<ul class="menuExportOptions">
					<li><a href="javascript:" id="toolUrlExportProxy" class="exportAction proxyAble"><?php echo fetchLinkTitle("toolUrlExport");?></a></li>
					<li><a href="javascript:" id="pngExportProxy" class="exportAction proxyAble" target="_blank"><?php echo fetchLinkTitle("pngExport");?></a></li>
					<li><a href="javascript:" id="htmlExportProxy" class="exportAction proxyAble"><?php echo fetchLinkTitle("htmlExport");?></a></li>
					<li><a href="javascript:" id="citeExportProxy" class="exportAction proxyAble"><?php echo fetchLinkTitle("citeExport");?></a></li>
					<li><a href="javascript:" id="csvExportProxy" class="exportAction proxyAble" target="_blank" ><?php echo fetchLinkTitle("csvExport");?></a></li>
					<li><a href="javascript:" id="svgExportProxy" class="exportAction proxyAble" target="_blank"><?php echo fetchLinkTitle("svgExport");?></a></li>
					<li><a href="javascript:" id="epsExportProxy" class="exportAction proxyAble" target="_blank"><?php echo fetchLinkTitle("epsExport");?></a></li>
					<li><a href="javascript:" id="pdfExportProxy" class="exportAction proxyAble" target="_blank"><?php echo fetchLinkTitle("pdfExport");?></a></li>
				</ul>

			</li>
		</ul>


	</div>

	<div id="iconsdiv">
		<a href="http://twitter.com/eurofound"><img src=
		"img/icons/t1.png" alt="Follow us on Twitter" title=
		"Follow us on Twitter"></a> <a href=
		"http://www.facebook.com/pages/Dublin-Ireland/Eurofound/66500373395"><img src=
		"img/icons/fb1.png" alt="Follow us on Facebook"
		title="Follow us on Facebook"></a> <a href=
		"http://www.youtube.com/user/eurofound"><img src=
		"img/icons/y1.png" alt="Follow us on YouTube" title=
		"Follow us on YouTube"></a> <a href=
		"https://www.ohsa.europa.eu/press/newsfeeds/index.htm"><img src=
		"img/icons/rss1.png" alt="Follow us on RSS" title=
		"Follow us on RSS"></a> <a href="http://google.com/+eurofound"><img src=
		"img/icons/gplus.png" alt="Follow us on Google+"
		title="Follow us on Google+"></a>
	</div>

	</div>






<div id="centralColumn3">

	<a id="contentpage"></a>
		<div id="content">

<?php /////////////////////////////////////////////////////////////////////////// ?>
		<?php require_once("DVT.body.php");?>
<?php /////////////////////////////////////////////////////////////////////////// themeSwap?>

<?php/*
			<div id="date">Page last updated: 20 Jan, 2014</div>
			<div class="backtop"><a href="#contentpage">^Back to top of page</a></div>
*/?>

		</div>
</div>

		<div id="rightColumn">
		</div>

		<div class="clearfloat"></div>

	</div>

	<div id="footer">
		<ul>
			<li><a href="https://www.ohsa.europa.eu/legal/copyright.htm">Copyright</a></li>
			<li><a href="https://www.ohsa.europa.eu/legal/legalnotices.htm">Legal notices</a></li>
			<li><a href="https://www.ohsa.europa.eu/legal/dataprotection.htm">Data protection</a></li>
			<li class="last"><a href="mailto:web@eurofound.europa.eu">Webmaster</a></li>
		</ul>
	</div>
	</body>
</html>
