<?php /////////////////////////////////////////////////////////////////////////// ?>

		<form action="#" class="graphControls survey<?php echo $survey;?>" id="graphControlsForm" >

			<div id="wrapper">
				<header id="header">
					<h1 id="headerTitle">
						<?php echo fetchTitle("survey");?>

					</h1>

					<h2 class="hidden">Eurofound Surveys</h2>

					<div id="langSelect">
						<label for="locale" class="hidden"></label>
						<select id="locale" name="locale" class="side-tooltip" title="Available languages">
<?php foreach($surveyLocales as $entry) {?>
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

					<section class="visualizationSelection hidden">
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
										<h2><?php echo (fetchLabel("showOnlyEUdata"));?></h2>
										<a href="javascript:" src="#onlyEU">
											<p><?php echo fetchTooltip("showOnlyEUdata") ;?></p>
										</a>
									</div>

									<div id="moreVisualizationsTooltip">Shows / hides aditional visualization actions</div>
							</div>

						</div>

						<div class="filtersRight buttonMenu">

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


							<ul class="visualisationSelections hidden">
								<?php 
								$theVisualizations =  fetchVisualizations();
								foreach(fetchVisualizations() as $index => $visualization){?>
									<li>
										<a href="javascript:" class="proxy_plot_leftlabel<?php echo $index?> <?php if($index == 0){?> checked<?php }?> " title="<?php echo fetchTooltip($visualization[0]) ;?>" proxyPlot="<?php echo $theVisualizations[$index][0]; ?>">
											<?php echo $visualization[1];?>
										</a>
									</li>
								<?php } ?>
								<li><a href="javascript:" class="proxy_plot_leftlabel7 proxyable" src="#csvView" title="<?php echo fetchTooltip("dataTable") ;?>">Data Table</a></li>
							</ul>
<?php /*				
							<ul class="visualisationSelections hidden">
								<li><a href="javascript:" class="proxy_plot_leftlabel0" proxyPlot="heatMap">European map</a></li>
								<li><a href="javascript:" class="proxy_plot_leftlabel1" proxyPlot="euBars" >European bar chart</a></li>
								<li><a href="javascript:" class="proxy_plot_leftlabel2" proxyPlot="inCountry">National bar chart</a></li>
								<li><a href="javascript:" class="proxy_plot_leftlabel3" proxyPlot="crossCountry" >National comparisons</a></li>
								<li><a href="javascript:" class="proxy_plot_leftlabel4" proxyPlot="euMatrix" >Data matrix</a></li>
								<li><a href="javascript:" class="proxy_plot_leftlabel5" proxyPlot="wordMap">Country groups (linear)</a></li>
								<li><a href="javascript:" class="proxy_plot_leftlabel6" proxyPlot="euCompass" >Country groups (radial)</a></li>
								<li><a href="javascript:" class="proxy_plot_leftlabel7 proxyable" src="#csvView">Data Table</a></li>
							</ul>
*/?>
							<ul class="visualisationSelectionsExtra">
								<li><a href="javascript:" class="euOnly proxyable" src="#onlyEU" title="<?php echo fetchTooltip("showOnlyEUdata");?>">EU only</a></li>
							</ul>
				
						</div>
						
					</section>

<?php ///////// VISUALIZATION MAIN PARAMETERS END /////////////////////////?>

<?php ///////// VISUALIZATION SECTION START /////////////////////////?>
				
				<?php //////////////////////////////////////////////////////////////////////////////// ?>							
				<section id="loadingVisualizationSection" class="inv">
					<?php //include "img/loading-animation.svg"; ?>
				</section>
				<?php //////////////////////////////////////////////////////////////////////////////// ?>

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
		</form>
