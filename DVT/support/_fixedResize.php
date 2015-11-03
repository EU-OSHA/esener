<?php require_once("DVT.top.php");?>
<?php $visualizationUrl = str_replace("../","http://194.30.34.27/DVS/", $visualizationUrl); ?>

<html>
	<body>
		<section style="width:920px">
			<h1><?php echo $pageTitle; ?> </h1>
			<hr>
			<h2>Standard resize - 920px width *</h2>
			<img id="visualization" width="100%" src="<?php echo $visualizationUrl; ?>">
			<p><?php echo $visualizationDescription; ?></p>
		</section> <br/>
		<hr>
		<section style="width:830px">
			<h2>Standard resize - 830px width </h2>
			<img id="visualization" width="100%" src="<?php echo $visualizationUrl; ?>">
		</section> <br/>
		
		<section style="width:740px">
			<h2>Standard resize - 740px width * </h2>
			<img id="visualization" width="100%" src="<?php echo $visualizationUrl; ?>">
		</section> <br/>
		
		<section style="width:620px">
			<h2>Standard resize - 620px width </h2>
			<img id="visualization" width="100%" src="<?php echo $visualizationUrl; ?>">
		</section> <br/>
		
		<section style="width:500px">
			<h2>Standard resize - 500px width * </h2>
			<img id="visualization" width="100%" src="<?php echo $visualizationUrl; ?>">
		</section><br/>
		
		<section style="width:400px">
			<h2>Standard resize - 400px width </h2>
			<img id="visualization" width="100%" src="<?php echo $visualizationUrl; ?>">
		</section><br/>
	</body>
</html>