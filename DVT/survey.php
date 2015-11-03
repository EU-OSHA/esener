 
<?php 
///////////////////////////////////////////////////////////////////////////
require_once("DVT.top.php");
///////////////////////////////////////////////////////////////////////////
$printStandaloneExportOptions = false;
?>

		<link href="/DVS/DVT/css/common.css?v=<?php echo $version;?>" rel="stylesheet" type="text/css">
		<link href="/DVS/DVT/css/drupal.css?v=<?php echo $version;?>" rel="stylesheet" type="text/css">
		<link href="/DVS/DVT/scripts/3rd/udm-resources/udm-style-drupal.css" rel="stylesheet" media="screen" type="text/css" />

		<?php require_once("DVT.head.css.php");?>

<?php /////////////////////////////////////////////////////////////////////////// ?>
<!-- melt-->
		<script type="text/javascript">
			var toCopy = "<?php echo fetchWord('toCopy');?>";
			var selectContent = "<?php echo fetchWord('selectContent');?>";
			var ok = "<?php echo fetchWord('ok');?>";
			var visitURL =  "<?php echo fetchWord('visitURL');?>";
			//alert("<?php echo fetchWord('selectContent');?>");
			
		</script>
		
		<?php 
		require_once("DVT.head.js.drupal.php");?>

		
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
				//toggleProxyAble();

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


				
				for(var questionIndex=0; questionIndex<topic_question.length; questionIndex++){
					if(topicMenuLink != topic_question[questionIndex][0]){
						topicMenuLink = topic_question[questionIndex][0];
						
						if(questionIndex > 0){
							leftMenuHtml += "</ul></li>";
						}

							leftMenuHtml += "<li class='surveyQuestionTopicMenus closed'><a href='javascript:'>" + topic_question[questionIndex][0] + "</a>";
							leftMenuHtml += "<ul class=\"menu\">";

					}
					//RRL Create subcategories*******************************************
					var ti =topic_question[questionIndex][1]; //Topic index
					var tq =topic_question[questionIndex][2]; //Topic question
					
					tq = tq.charAt(0).toUpperCase() + tq.substring(1);
					var posicion = tq.indexOf(':');
					
					if (ti =="Q150_1" || ti == "Q158_1" || ti == "Q200_1" || ti == "Q202_1" || ti == "Q252_1"|| ti == "Q256_1"|| ti == "Q261_1" ||ti == "Q308_1"
					||ti == "Q356_1" ||ti == "Q201_1" ||ti == "Q210_10"||ti == "Q303_1" ||ti == "Q264_1" ||ti == "Q265_1" ||ti == "Q306a_3" ||ti == "Q166_1" ||ti == "Q202_10"
					||ti == "MM150_1"||ti == "MM173_1"||ti == "MM200_1"||ti == "MM202_1"||ti == "MM253_1"||ti == "MM171_1"||ti == "MM172_1"){
						var ctitle = tq.substring(0,posicion);
						leftMenuHtml +="<li class='subcategory'>"+ ctitle +"<span class='subspam'>&nbsp;&nbsp;</span></li>";
					}
					
					if ( ti =="Q150_1" || ti =="Q150_2"  || ti =="Q150_3" ||ti =="Q150_4" ||ti =="Q150_5" 
					||ti == "Q158_1" ||ti == "Q158_2" ||ti == "Q158_3"||ti == "Q158_4"
					||ti == "Q200_1" ||ti == "Q200_2"||ti == "Q200_3"||ti == "Q200_4"||ti == "Q200_5"||ti == "Q200_6"||ti == "Q200_7"||ti == "Q200_8"||ti == "Q200_9"
					||ti == "Q202_1" ||ti == "Q202_2"||ti == "Q202_3"||ti == "Q202_4"||ti == "Q202_5"||ti == "Q202_6"||ti == "Q202_7"||ti == "Q202_8"||ti == "Q202_9"
					||ti == "Q252_1" ||ti == "Q252_2"||ti == "Q252_3"||ti == "Q252_4"||ti == "Q252_5"||ti == "Q252_6"
					||ti == "Q256_1" ||ti == "Q256_2"||ti == "Q256_3"||ti == "Q256_4"||ti == "Q256_5"
					||ti == "Q261_1" ||ti == "Q261_2"||ti == "Q261_3"||ti == "Q261_4"
					||ti == "Q308_1" ||ti == "Q308_2"||ti == "Q308_3"||ti == "Q308_4"
					||ti == "Q356_1" ||ti == "Q356_2"||ti == "Q356_3"||ti == "Q356_4"||ti == "Q356_5"
					||ti == "Q201_1" ||ti == "Q201_2"||ti == "Q201_3"||ti == "Q201_4"||ti == "Q201_5"||ti == "Q201_6"||ti == "Q201_7"
					||ti == "Q210_10"||ti == "Q210_11"||ti == "Q210_12"||ti == "Q210_13"||ti == "Q210_14"||ti == "Q210_15"
					||ti == "Q303_1" ||ti == "Q303_2"||ti == "Q303_3"||ti == "Q303_4"
					||ti == "Q264_1" ||ti == "Q264_2"||ti == "Q264_3"||ti == "Q264_4"||ti == "Q264_5"||ti == "Q264_6"
					||ti == "Q265_1" ||ti == "Q265_2"||ti == "Q265_3"||ti == "Q265_4"||ti == "Q265_5"||ti == "Q265_6"||ti == "Q265_7"
					||ti == "Q306a_3" ||ti == "Q306a_4"||ti == "Q306a_5"||ti == "Q306a_6"
					||ti == "Q166_1" ||ti == "Q166_2"||ti == "Q166_3"||ti == "Q166_4"					
					||ti == "Q202_10" ||ti == "Q202_11"||ti == "Q202_12"||ti == "Q202_13"||ti == "Q202_14"||ti == "Q202_15"
					||ti == "MM150_1" ||ti == "MM150_2"||ti == "MM150_3"||ti == "MM150_4"||ti == "MM150_5"||ti == "MM150_6"
					||ti == "MM173_1" ||ti == "MM173_2"||ti == "MM173_3"||ti == "MM173_4"||ti == "MM173_5"||ti == "MM173_6"||ti == "MM173_7"||ti == "MM173_8" 
					||ti == "MM200_1" ||ti == "MM200_2"||ti == "MM200_3"||ti == "MM200_4"||ti == "MM200_5"||ti == "MM200_6"||ti == "MM200_7"
					||ti == "MM202_1" ||ti == "MM202_2"||ti == "MM202_3"||ti == "MM202_4"||ti == "MM202_5"||ti == "MM202_6"||ti == "MM202_7"||ti == "MM202_8"||ti == "MM202_9"||ti == "MM202_10"
					||ti == "MM253_1" ||ti == "MM253_2"||ti == "MM253_3"||ti == "MM253_4"||ti == "MM253_5"||ti == "MM253_6"
					||ti == "MM171_1" ||ti == "MM171_2"||ti == "MM171_3"||ti == "MM171_4"||ti == "MM171_5"||ti == "MM171_6"
					||ti == "MM172_1" ||ti == "MM172_2"||ti == "MM172_3"||ti == "MM172_4"||ti == "MM172_5"||ti == "MM172_6"
					){
						classsangria = "leaf questionleaf sangria";
						tq = tq.substring(posicion +1);
						tq = $.trim( tq );
						tq = tq.charAt(0).toUpperCase() + tq.substring(1);
					}else{
						classsangria = "leaf questionleaf";
					}
					//********************************************************************
					
					leftMenuHtml += "<li class='"+ classsangria +"'><a href='javascript:' for='"+ topic_question[questionIndex][1] + "' class='questionProxy'>" +
									 tq + "</a></li>" ;
 
				}			
				leftMenuHtml += "</ul></li>";

				lenQ = $("#locale option:selected").val();

				leftMenuHtml = "<li id='surveyMenuItem' class='active-trail'><a class='active-trail icon-<?php print($survey);?>'>" + $("section.questionSection header h2").text()+"</a></li>"+leftMenuHtml;
				
				var surveyid = $("#dataSource").val();
				
				if (surveyid=="esener04"){
					leftMenuHtml = leftMenuHtml + "<li id='surveyMenuItem' class='active-trail finalquestion'><a href='/DVS/import/questionnaire/Questionnaire_"+lenQ+".pdf' target='_blank'><?php echo fetchLinkTitle("Questionnaire");?></a></li>";
				}
				
				if (surveyid=="esener1"){
					if (lenQ =="IS"){
						lenQ = "EN";
					}
					leftMenuHtml = leftMenuHtml + "<li id='surveyMenuItem' class='active-trail finalquestion'><a href='/DVS/import/questionnaire/"+lenQ+"_QUESTIONNAIRE.rar' target='_blank'><?php echo fetchLinkTitle("Questionnaire");?></a></li>";
				}
				
				
				$(".region-sidebar-first p").html("<ul class='menu'>" + leftMenuHtml+ "</ul>") ;
				
				var noclose= 0;
				var noscroll= 0;		
				$(".subcategory").click(function(){
					noclose =1;
					noscroll=1;
				});
				$(".questionleaf").click(function(){
					noclose =1;
				});
				
				$(".surveyQuestionTopicMenus").click(function(){
					if (noclose==1){//A topic or subcategory
						noclose = 0;
						if (noscroll==1){
							noscroll=0;
							return false;
						}
						scrollTo($("#content"));
						return false;
					}
					var $this = $(this);
					if($this.hasClass("surveyQuestionTopicMenus")){
						if($this.hasClass("questionleaf")){
						 }
					
						if ($this.hasClass("closed")){
							$(".surveyQuestionTopicMenus").removeClass("open");
							$(".surveyQuestionTopicMenus").addClass("closed");
							$this.removeClass("closed").addClass("open");
							
						}else{
							$(".surveyQuestionTopicMenus").removeClass("open");
							$(".surveyQuestionTopicMenus").addClass("closed");
						}
					}
						
					scrollTo($("#content"));
				} );

				jQuery(".block-menu-block .active a").append($("#DVSLeftColumn").html());
				$("#DVSLeftColumn").remove();
				
				jQuery(".questionProxy").click(function(){
					var $this = $(this);
					
					$(".menuactive").each(function(){
						$(this).removeClass("menuactive");
					});
					
					
					$this.parent().addClass("menuactive");
				});
				
				
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
	 
				
				$(".questionProxy").click(function() {
					var questionCode = $(this).attr("for");
					var $question = $("#question");
					$question.find("option[value='"+questionCode+"']").attr("selected", "selected");
					$question.change();
					$(".visibleSubMenu").removeClass("visibleSubMenu");
				});
				
				$(".buttomlan").click(function(){
					var lan = $(this).attr('rel');
					
					$("#locale option").each(function(){
						$(this).attr("selected",false)
					});
					
					$("#locale option[value="+ lan +"]").attr("selected",true);
					window.location = window.location.pathname + "?" + getFormParameters($("#graphControlsForm"))
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
		 	 
		 		//var $comonVisControl = $(".visualizationSelection .filtersRight");
/*
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
					*/
					//$(".filtersExtra .visualisationSelections").removeClass("hidden");
					if($("#exportSection #toolUrlExport").length == 0){
						toggleProxyAble();
					}
					//$leftDiv.addClass("hidden");
					//$questionSection.removeClass("hidden");
					//$centralColumn3.css("width", "98%");
					//$comonVisControl.removeClass("hidden");
				//}
 
			}
			$(document).ready(function() {
				acordeon();
			});
			var pepe=1;
			function acordeon(){
				$("#natver").click(function() {
					$(".languagesdiv").slideToggle();
					
				});
			}
		//-->
		</script>

<?php /////////////////////////////////////////////////////////////////////////// ?>

<?/*

	<div id="DVSLeftColumn" class="leftDiv">

 
 
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

			<li class="open leftExportOptions hidden" >

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

 
	</div>


*/?>

 
		<div id="DVSContent">

<?php /////////////////////////////////////////////////////////////////////////// ?>
		<?php 
		require_once("DVT.body.drupal.php");?>
<?php /////////////////////////////////////////////////////////////////////////// themeSwap?>

 
		</div>
 
 
