<?php
require_once("DVT.top.php");

$case = "http://eurofound.europa.eu/DVS/";
$case = str_replace("../", $case, $visualizationUrl);
$caseB = $visualizationUrl;

$visualizationUrl = $case ;
?>


<!DOCTYPE HTML>
<html lang="en">
	<head>
		<link href="http://www.eurofound.europa.eu/css/default.css?v=<?php echo $version;?>" rel="stylesheet" media="screen" type="text/css" />
		<link href="http://www.eurofound.europa.eu/css/default_print.css?v=<?php echo $version;?>" rel="stylesheet" media="print" type="text/css" />
		<link href="css/screen.EF.css?v=<?php echo $version;?>" rel="stylesheet" type="text/css">
		
		<?php require_once("DVT.head.css.php");?>
			 <style>
			 	body {
					background-image: url("../img/ef_bg.gif-");
				}
img
{
vertical-align:text-top;
} 				
h1 {
	font-size: 1.2em !important;
}
h2 {
	font-size: 0.95em !important;
	margin: 0 0 15px !important;
}
			</style>
	</head>
	<body>
		<section style="width:100%">
			<h1><?php echo $pageTitle; ?> </h1>
			<p>
				<?php echo $visualizationDescription; ?>
				<h2>DVS1 in standard resize vs DVS2 responsive rendering engine</h2>
			</p>
			<hr>
			<h1>920px width</h1>
			
			<img class="visualization" style="width:920px" src="<?php echo $case; ?>">
			<img class="visualization" style="width:920px" src="<?php echo $caseB; ?>">
		</section> <br/>
		<hr>


		<section style="width:100%">
			<h1>620px width</h1>
			<h2>DVS1* vs DVS2 </h2>
			<img class="visualization" style="width:620px"  src="<?php echo $case; ?>">
			<img class="visualization" style="width:620px"  src="<?php echo str_replace("width=920","width=740", $caseB); ?>">
		</section> <br/>

		<?php $visualizationUrl = str_replace("width=740","width=500", $visualizationUrl); ?>

		<section style="width:100%">
			<h1>400px  width</h1>
			<h2>DVS1* vs DVS2 </h2>
			<img class="visualization" style="width:400px"  src="<?php echo $case; ?>">
			<img class="visualization" style="width:400px"  src="<?php echo str_replace("width=920","width=500", $caseB); ?>">
		</section><br/>
	</body>
</html>