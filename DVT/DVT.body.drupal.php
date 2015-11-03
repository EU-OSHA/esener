
<?php /////////////////////////////////////////////////////////////////////////// ?>

		<form action="#" class="graphControls survey<?php echo $survey;?>" id="graphControlsForm" >

			<div id="wrapper">
				<header id="header">
					<h1 id="headerTitle">
						<?php echo fetchTitle("survey");?>

					</h1>

					<h2 class="hidden"></h2>

					<div id="langSelect">
						<label for="locale" class="hidden"></label>
						<select id="locale" name="locale" class="side-tooltip" title="Available languages">
<?php 
	
						foreach($surveyLocales as $entry) {?>
							<option <?php echo (($locale == $entry)?"selected":""); ?> value="<?php echo $entry;?>"><?php echo fetchValue($entry, $countryToLanguage);?></option>
<?php } ?>
						</select>
					</div>
				</header>


				<article>

					<section class="hidden">
						<input name="dataSource" id="dataSource" type="hidden" value="<?php echo $survey;?>">
						<input name="media" id="media" type="hidden" value="png">
						<input name="width" type="hidden" id="pngWidth" value="740">
					</section>

<?php ///////// VISUALIZATION MAIN PARAMETERS START /////////////////////////?>
					<?php if ($survey=="esener04"){ //End if survey is esener 04, then We add the language section div
					?>
					<section class="languageSection">
						<div><h2 class="natver" ><a id="natver">This item is available in other national versions</a></h2></div>
						<div class="languagesdiv" style="display: none;">
						<span class="countrylan">Albania</span> (<a class="buttomlan" rel="AL" title="shqiptar" href="#">AL</a>) | 
						<span class="countrylan">Austria</span> (<a class="buttomlan" rel="DE_1" title="Deutsch" href="#")>AT</a>) | 
						<span class="countrylan">Belgium</span> (<a class="buttomlan" rel="FR_1" title="Français" href="#")>FR</a>) (<a class="buttomlan" rel="NL_1" title="Nederlands" href="#")>NL</a>) | 
						<span class="countrylan">Bulgaria</span> (<a class="buttomlan" rel="BG" title="български език" href="#")>BG</a>) | 
						<span class="countrylan">Croatia</span> (<a class="buttomlan" rel="HR" title="Croatian" href="#")>HR</a>) | 
						<span class="countrylan">Cyprus</span> (<a class="buttomlan" rel="EL_1" title="elli_niká" href="#")>CY</a>) | 
						<span class="countrylan">Czech Republic</span> (<a class="buttomlan" rel="CS" title="Czech" href="#")>CZ</a>) | 
						<span class="countrylan">Denmark</span> (<a class="buttomlan" rel="DA" title="Dansk" href="#")>DK</a>) | 
						<span class="countrylan">Estonia</span> (<a class="buttomlan" rel="ET" title="Eesti" href="#")>EE</a>) (<a class="buttomlan" rel="RU_1" title="ру́сский язы́к" href="#")>RU</a>) |
						<span class="countrylan">Finland</span> (<a class="buttomlan" rel="FI" title="Suomi" href="#")>FI</a>) (<a class="buttomlan" rel="SV_1" title="Svenska" href="#")>SE</a>) | 
						<span class="countrylan">France</span> (<a class="buttomlan" rel="FR" title="Français" href="#")>FR</a>) | 
						<span class="countrylan">FYROM</span> (<a class="buttomlan" rel="AL_1" title="shqiptar" href="#">AL</a>) (<a class="buttomlan" rel="MK" title="македонски јазик" href="#">MK</a>) | 
						<span class="countrylan">Germany</span> (<a class="buttomlan" rel="DE" title="Deutsch" href="#")>DE</a>) | 
						<span class="countrylan">Greece</span> (<a class="buttomlan" rel="EL" title="elli_niká" href="#")>EL</a>) | 
						<span class="countrylan">Hungary</span> (<a class="buttomlan" rel="HU" title="Magyar" href="#")>HU</a>) | 
						<span class="countrylan">Iceland</span> (<a class="buttomlan" rel="IS" title="<?php echo utf8_encode("�slenska")?>" href="#")>IS</a>) | 
						<span class="countrylan">Ireland</span> (<a class="buttomlan" rel="EN_1" title="English" href="#">IE</a>) | 
						<span class="countrylan">Italy</span> (<a class="buttomlan" rel="IT" title="Italiano" href="#")>IT</a>) | 
						<span class="countrylan">Latvia</span> (<a class="buttomlan" rel="LV" title="Latviešu" href="#">LV</a>) (<a class="buttomlan" rel="RU_2" title="ру́сский язы́к" href="#")>RU</a>) |
						<span class="countrylan">Lithuania</span> (<a class="buttomlan" rel="LT" title="Lietuviu" href="#">LT</a>) (<a class="buttomlan" rel="RU_3" title="ру́сский язы́к" href="#")>RU</a>) | 
						<span class="countrylan">Luxembourg</span> (<a class="buttomlan" rel="DE_2" title="Deutsch" href="#")>DE</a>) (<a class="buttomlan" rel="FR_2" title="Français" href="#")>FR</a>) (<a class="buttomlan" rel="LU" title="<?php echo utf8_encode('L�tzebuergesch')?>" href="#")>LU</a>) | 
						<span class="countrylan">Malta</span> (<a class="buttomlan" rel="EN_2" title="English" href="#">EN</a>) (<a class="buttomlan" rel="MT" title="Malti" href="#">MT</a>) | 
						<span class="countrylan">Montenegro</span> (<a class="buttomlan" rel="ME" title="Crnogorski" href="#")>ME</a>) | 
						<span class="countrylan">Netherlands</span> (<a class="buttomlan" rel="NL" title="Nederlands" href="#")>NL</a>) | 
						<span class="countrylan">Norway</span> (<a class="buttomlan" rel="NO" title="Norsk" href="#">NO</a>) | 
						<span class="countrylan">Poland</span> (<a class="buttomlan" rel="PL" title="Polski" href="#")>PL</a>) | 
						<span class="countrylan">Portugal</span> (<a class="buttomlan" rel="PT" title="Português" href="#")>PT</a>) | 
						<span class="countrylan">Romania</span> (<a class="buttomlan" rel="RO" title="Româna" href="#")>RO</a>) |
						<span class="countrylan">Serbia</span> (<a class="buttomlan" rel="SR" title="srpski" href="#")>RS</a>) |
						<span class="countrylan">Slovakia</span> (<a class="buttomlan" rel="SK" title="Slovencina" href="#")>SK</a>) | 
						<span class="countrylan">Slovenia</span> (<a class="buttomlan" rel="SL" title="Slovenšcina" href="#")>SI</a>) | 
						<span class="countrylan">Spain</span> (<a class="buttomlan" rel="ES" title="<?php echo utf8_encode("Espa�ol")?>" href="#")>ES</a>) | 
						<span class="countrylan">Sweden</span> (<a class="buttomlan" rel="SV" title="Svenska" href="#")>SE</a>) | 
						<span class="countrylan">Switzerland</span> (<a class="buttomlan" rel="DE_3" title="Deutsch" href="#")>DE</a>) (<a class="buttomlan" rel="FR_3" title="français" href="#")>FR</a>) (<a class="buttomlan" rel="IT_1" title="Italiano" href="#")>IT</a>) | 
						<span class="countrylan">Turkey</span> (<a class="buttomlan" rel="TR" title="<?php echo utf8_encode("T�rk�e")?>" href="#")>TR</a>) |
						<span class="countrylan">United Kingdom</span> (<a class="buttomlan" rel="EN" title="English" href="#">UK</a>)
						</div>
					</section>
					<?php 
					}else{
					?>
					<section class="languageSection">
						<div><h2 class="natver" ><a id="natver">This item is available in other languages</a></h2></div>
						
						<div class="languagesdiv" style="display: none;">
						<span class="countrylan"><a class="buttomlan" rel="BG" title="български" href="#">български</a></span> | 	
						<span class="countrylan"><a class="buttomlan" rel="CS" title=" Čeština" href="#"> Čeština</a> | 
						<span class="countrylan"><a class="buttomlan" rel="DA" title="Dansk" href="#">Dansk</a></span> | 
						<span class="countrylan"><a class="buttomlan" rel="DE" title="Deutsch" href="#">Deutsch</a> | 
						<span class="countrylan"><a class="buttomlan" rel="ET" title="Eesti" href="#")>Eesti</a> |
						<span class="countrylan"><a class="buttomlan" rel="EN" title="English" href="#">English</a> |
						<span class="countrylan"><a class="buttomlan" rel="ES" title="<?php echo utf8_encode("Espa�ol")?>" href="#"><?php echo utf8_encode("Espa�ol")?></a> | 
						<span class="countrylan"><a class="buttomlan" rel="FR" title="Français" href="#">Français</a> | 
						<span class="countrylan"><a class="buttomlan" rel="HR" title="Hrvatski" href="#")>Hrvatski</a> | 
						<span class="countrylan"><a class="buttomlan" rel="IT" title="Italiano" href="#")>Italiano</a> | 
						<span class="countrylan"><a class="buttomlan" rel="LV" title="Latviešu" href="#">Latviešu</a> |
						<span class="countrylan"><a class="buttomlan" rel="LT" title="Lietuvių" href="#">Lietuvių</a> | 
						<span class="countrylan"><a class="buttomlan" rel="HU" title="Magyar" href="#")>Magyar</a> | 
						<span class="countrylan"><a class="buttomlan" rel="MT" title="Malti" href="#">Malti</a> | 
						<span class="countrylan"><a class="buttomlan" rel="NL" title="Nederlands" href="#")>Nederlands</a> | 
						<span class="countrylan"><a class="buttomlan" rel="NO" title="Norsk" href="#">Norsk</a> | 
						<span class="countrylan"><a class="buttomlan" rel="PL" title="Polski" href="#")>Polski</a> | 
						<span class="countrylan"><a class="buttomlan" rel="PT" title="Português" href="#")>Português</a> | 
						<span class="countrylan"><a class="buttomlan" rel="RO" title="Română" href="#")>Română</a> |
						<span class="countrylan"><a class="buttomlan" rel="SK" title="Slovenčina" href="#")>Slovenčina</a> | 
						<span class="countrylan"><a class="buttomlan" rel="SL" title="Slovenščina" href="#")>Slovenščina</a> | 
						<span class="countrylan"><a class="buttomlan" rel="FI" title="Suomi" href="#")>Suomi</a> | 
						<span class="countrylan"><a class="buttomlan" rel="SV" title="Svenska" href="#")>Svenska</a> | 
						<span class="countrylan"><a class="buttomlan" rel="TR" title="<?php echo utf8_encode("T�rk�e")?>" href="#")><?php echo utf8_encode("T�rk�e")?></a> |
						<span class="countrylan"><a class="buttomlan" rel="EL" title="Ελληνικά" href="#")>Ελληνικά</a> | 
						</div>
					</section>
					
					<?php
					}//End if survey is esener 04
					?>
					<section class="questionSection">

						<header>
							<h2><?php echo fetchTitle("plotParameters");?></h2>
						</header>

						<label id="label_topic" class="hidden">
							<span><?php echo fetchLabel("topic");?> :</span>
							<select id="topic">
								<option value="">Please enable Javascript</option>
							</select>
						</label>

						<label id="label_question">
							<span><?php echo fetchLabel("question");?> :</span>
							<select name="question" id="question" class="chosen-select">
								<option value="">Please enable Javascript</option>
							</select>
						</label>



					</section>

					<section id="visualizationTitleSection">
						<header id="visualizationTitle" class="">
							<span id="title_topic">&nbsp;</span>
							<span id="webTitle_question" class="hidden"> &nbsp; </span>
							<h2 id="title_question">&nbsp;</h2>
						</header>
						<a href="javascript:" class="prevQuest"></a>
						<a href="javascript:" class="nextQuest"></a>
					</section>

<?php //----------------------------------------------------------------// ?>

					<section class="visualizationSelection  hidden">
						<div class="filtersLeft">

				<?php //  Select visualization///?>

							<label id="label_plot">
								<span><?php echo fetchLabel("plot");?> :</span>
								<select id="plot" name = "plot" >
									<?php foreach(fetchVisualizations() as $visualization){ ?>
										<option value="<?php echo $visualization[0];?>" title="<?php echo fetchTooltip($visualization[0]);?>" ><?php echo $visualization[1];?></option>
									<?php }?>
								</select>
							</label>

<?php //  Radio button used as a dropdown wrapper///?>



<?php //  Rich tooltips///?>

							<div id="formatSelectorTooltips" class="hidden">
								<?php foreach(fetchVisualizations() as $index => $visualization){?>
									<div id="plotSelectionTooltip<?php echo $index?>" class="plotSelectionTooltip">
										<a href="javascript:" src="#proxy_plot<?php echo $index?>">
											<h2><?php echo $visualization[1];?></h2>
											<div><img src="/DVS/DVT/garnish/visualization-thumbnail-<?php echo $visualization[0];?>.png?v=<?php echo $version?>" alt="Viualization Description"/></div>
											<p><?php echo fetchTooltip($visualization[0]) ;?></p>
										</a>
									</div>
								<?php } ?>
									<div id="tableViewTooltip" class="plotSelectionTooltip">
										<h2><?php echo (fetchViewCSVData());?></h2>
										<a href="javascript:" src="#csvView">
											<img src="/DVS/DVT/garnish/visualization-thumbnail-tableView.png?v=<?php echo $version?>"  alt="Viualization Description"/>
											<p><?php echo fetchTooltip("dataTable") ;?></p>
										</a>
									</div>

									<div id="onlyEUTooltip" class="plotSelectionTooltip">
										<h2><?php echo (fetchWord("euonlytt"));?></h2>
										<a href="javascript:" src="#onlyEU">
											<p><?php echo fetchWord("euonlytt") ;?></p>
										</a>
									</div>

									<div id="moreVisualizationsTooltip">Shows / hides aditional visualization actions</div>
							</div>

						</div>

						<div class="filtersRight buttonMenu hidden">

								<label class="label_countryGroup hidden">
									<input type="radio" name="countryGroup" id="countryGroupLinear" value="linear" checked> <span>linear</span>
								</label>

								<label class="label_countryGroup hidden">
									<input type="radio" name="countryGroup" id="countryGroupRadial" value="radial">	<span>radial</span>
								</label>

								<label id="label_onlyEU">
									<input type="checkbox" name="onlyEU" id="onlyEU" value="onlyEU">
									<span pressed="<?php echo fetchLabel('ShowOnlyEUdataPressed');?>" unpressed="<?php echo fetchLabel('showOnlyEUdata');?>"><?php echo fetchLabel("showOnlyEUdata");?></span>
								</label>

								<label id="label_csvView" class="secondary">
									<input type="checkbox" name="dataView" id="csvView" value="csv">
									<span><?php echo (fetchViewCSVData());?></span>
								</label>

						</div>

<?php //----------------------------------------------------------------// ?>

					</section>




<?php //----------------------------------------------------------------// ?>
			<section id="sectionVisualisationSelections">
				<?php if ($survey=="esener1"){?>
				
					<ul class="visualisationSelections">
						<?php
						$theVisualizations =  fetchVisualizations();
						foreach(fetchVisualizations() as $index => $visualization){?>
							<li>
								<a href="javascript:" class="icon-<?php echo $visualization[0];?> proxy_plot_leftlabel<?php echo $index?> <?php if($index == 0){?> checked<?php }?> " title="<?php echo fetchTooltip($visualization[0].'Mouse') ;?>" proxyPlot="<?php echo $theVisualizations[$index][0]; ?>">
								<?php //var_dump ($visualization[0]+"Mouse");?>
									<?php echo $visualization[1];?>
								</a>
							</li>
						<?php } ?>										
					</ul>
				<?php 
				}
				else
				{
				?>
					<ul class="visualisationSelections">
						<?php
						$theVisualizations =  fetchVisualizations();
						foreach(fetchVisualizations() as $index => $visualization){?>
							<li>
								<a href="javascript:" class="icon-<?php echo $visualization[0];?> proxy_plot_leftlabel<?php echo $index?> <?php if($index == 0){?> checked<?php }?> " title="<?php echo fetchTooltip($visualization[0]) ;?>" proxyPlot="<?php echo $theVisualizations[$index][0]; ?>">
									<?php echo $visualization[1];?>
								</a>
							</li>
						<?php } ?>				
						<!--<li><a href="javascript:" class="icon-DataTable proxy_plot_leftlabel7 proxyable" src="#csvView" title="<?php echo fetchTooltipMouse("dataTable") ;?>">Data Table</a></li>-->
					</ul>
				<?php } ?>
				
			</section>
					<section class="visualizationFilters">

						<div class="filtersLeft">

							<label id="label_subset">
								<span><?php echo fetchLabel("subset");?> : </span>
								<select name="subset" id="subset">
									<option value="">Please enable Javascript</option>
								</select>
							</label>

							<label id="label_subset_value">
								<span><?php echo fetchLabel("subsetValue");?></span>
								<select name="subsetValue" id="subsetValue">
									<option value="">Please enable Javascript</option>
								</select>
							</label>

						</div>


						<div class="filtersRight">

							<label id="label_country" class="hidden">
								<span><?php echo fetchLabel("country");?> :</span>
								<select name="country" id="country">
									<option value="">Please enable Javascript</option>
								</select>
							</label>

							<label id="label_countryB" class="hidden">
								<span><?php echo fetchLabel("countryB");?> :</span>
								<select name="countryB" id="countryB">
									<option value="">Please enable Javascript</option>
								</select>
							</label>

							<label id="label_answer">
								<span><?php echo fetchLabel("answer");?> :</span>
								<select name="answer" id="answer">
									<option value="">Please enable Javascript</option>
								</select>
							</label>
						</div>


						<div class="filtersExtra">


							<ul class="visualisationSelections">
								<?php 
								$theVisualizations =  fetchVisualizations();
								foreach(fetchVisualizations() as $index => $visualization){?>
									<li>
										<a href="javascript:" class="icon-<?php echo $visualization[0];?> proxy_plot_leftlabel<?php echo $index?><?php if($index == 0){?> checked<?php }?> " title="<?php echo fetchTooltip($visualization[0]) ;?>" proxyPlot="<?php echo $theVisualizations[$index][0]; ?>">
											<?php echo $visualization[1];?>
										</a>
									</li>
								<?php } ?>
								<li><a href="javascript:" class="icon-DataTable proxy_plot_leftlabel7 proxyable" src="#csvView" title="<?php echo fetchTooltipMouse("dataTable") ;?>">Data Table</a></li>
							</ul>
 
							<ul class="visualisationSelectionsExtra">
								<li><a href="javascript:" class="euOnly proxyable" src="#onlyEU" title="<?php echo fetchWord("euonlytt");?>"><?php echo fetchWord("euonly")?></a></li>
							</ul>
							
						</div>
						
					</section>

<?php ///////// VISUALIZATION MAIN PARAMETERS END /////////////////////////?>

<?php ///////// VISUALIZATION SECTION START /////////////////////////?>
				
				<?php //////////////////////////////////////////////////////////////////////////////// ?>							
				<section id="loadingVisualizationSection" class="inv">
					<?php //include "img/loading-animation.svg"; ?>
				</section>
				<?php ////////////////////////////////////////////////////////////////////////////////?>

					<section id="visualizationSection">
						<div id="graph" >

							<div id="svgContainer" class="hidden"></div>
							
							<img id="visualization" width="920" height="950" src="<?php echo $visualizationUrl; ?>" alt="<?php echo $visualizationDescription; ?>">
							<canvas id="maskCanvas" width="920" height="950" class="hidden"></canvas>
							<div id="visualizationData" class="hidden">
								<div class="table-wrapper">
									<table id="csvData">
										<thead>
											<tr>
												<th class="essential persist"><a href="javascript:" onclick="sortBy(0)">CountryCode</a></th>
												<th class="essential"><a href="javascript:" onclick="sortBy(1)">question_code</a></th>
												<th class="essential"><a href="javascript:" onclick="sortBy(2)">subset</a></th>
												<th class="optional"><a href="javascript:" onclick="sortBy(3)">answer</a></th>
												<th class="optional"><a href="javascript:" onclick="sortBy(4)">percentage</a></th>
											</tr>
										</thead>
										<tbody>
											<tr class="hidden">
												<td></td><td></td><td></td><td></td><td></td>
											</tr>
										</tbody>
									</table>
								</div>

							</div>
						</div>

						<div id="answerInfo">
							<div id="answerInfoLeft">
								<div class="freetext">
									<p class="visualizationDynamicDescription"><?php echo $visualizationDescription; ?></p>
									<p id="questionNoteP" class=""></p>
								</div>
							</div>
						</div>

					</section>

					<section id="exportSection">
						<div class="exportOptions">
							<h2><?php echo fetchTitle("exportOptions");?></h2>
							<a href="javascript:" id="toolUrlExport" class="exportAction proxyAble"><img src="/DVS/DVT/garnish/1x1.gif" width="32" height="32" alt="Export Action"/><?php echo fetchLinkTitle("toolUrlExport");?></a>
							<a href="javascript:" id="pngExport" class="exportAction proxyAble" target="_blank" ><img src="/DVS/DVT/garnish/1x1.gif" width="32" height="32" alt="Export Action" /><?php echo fetchLinkTitle("pngExport");?></a>
							<a href="javascript:" id="htmlExport" class="exportAction proxyAble"><img src="/DVS/DVT/garnish/1x1.gif" width="32" height="32" alt="Export Action"/><?php echo fetchLinkTitle("htmlExport");?></a>
							<a href="javascript:" id="citeExport" class="exportAction proxyAble"><img src="/DVS/DVT/garnish/1x1.gif" width="32" height="32" alt="Export Action"/><?php echo fetchLinkTitle("citeExport");?></a>
							<a href="javascript:" id="csvExport" class="exportAction proxyAble" target="_blank"><img src="/DVS/DVT/garnish/1x1.gif" width="32" height="32" alt="Export Action"/><?php echo fetchLinkTitle("csvExport");?></a>
							<a href="javascript:" id="svgExport" class="exportAction proxyAble" target="_blank"><img src="/DVS/DVT/garnish/1x1.gif" width="32" height="32" alt="Export Action"/><?php echo fetchLinkTitle("svgExport");?></a>
							<a href="javascript:" id="epsExport" class="exportAction proxyAble" target="_blank"><img src="/DVS/DVT/garnish/1x1.gif" width="32" height="32" alt="Export Action"/><?php echo fetchLinkTitle("epsExport");?></a>
							<a href="javascript:" id="pdfExport" class="exportAction proxyAble" target="_blank"><img src="/DVS/DVT/garnish/1x1.gif" width="32" height="32" alt="Export Action"/><?php echo fetchLinkTitle("pdfExport");?></a>
						</div>
					</section>

<?php ///////// VISUALIZATION SECTION END /////////////////////////?>



					<section id="time" class="hidden"></section>

					<pre id="dialogContent"></pre>
					<pre id="dialogContentURL"></pre>

				</article>

				<section>
					<div class="navigation">
						<a href="javascript:" id="prevQuest" class="" ><?php echo fetchLinkTitle("prevQuest");?></a>
						<a href="javascript:" id="nextQuest" class="" ><?php echo fetchLinkTitle("nextQuest");?></a>
					</div>
				</section>

			</div>
			
			<ul><li class="hidden" id="alertnotdis"><?php print fetchWord("alertnotdis");?></li></ul>
			
		</form>
