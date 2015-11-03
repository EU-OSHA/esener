<?php /////////////////////////////////////////////////////////////////////////// ?>
<?php //JS 3d Party basic// 
		//unstable jquery versions 
		//http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js
		//http://code.jquery.com/jquery-migrate-1.0.0.js
		//scripts/3rd/jquery-2.0.3.min.js
		//jquery.min.js
		/// jquery-1.8.3.min.js  provides the most stable / consistent behavour among browsers
?>
		<script type="text/javascript" src="/DVS/DVT/scripts/3rd/jquery-1.8.3.min.js?v=<?php echo $version?>"></script>
		<script type="text/javascript" src="/DVS/DVT/scripts/3rd/jquery-ui-1.10.4.custom.min.js?v=<?php echo $version?>"></script>
		<script type="text/javascript" src="/DVS/DVT/scripts/3rd/chosen/chosen.jquery.min.js?v=<?php echo $version?>"></script>

		<!--[if lt IE 9]>
			<script type="text/javascript" src="scripts/3rd/html5shiv.js"></script>
			<script type="text/javascript" src="scripts/3rd/css3-mediaqueries.js"></script>
		<![endif]-->

		<script type="text/javascript" src="/DVS/DVT/scripts/3rd/spinners/spinners.min.js?v=<?php echo $version?>"></script>
		<script type="text/javascript" src="/DVS/DVT/scripts/3rd/tipped/tipped.js?v=<?php echo $version?>"></script>
<?php //JS Controllers// ?>



<?php //JS Model// ?>
		<script type="text/javascript" src="/DVS/DVT/model/<?php echo $survey;?>/js/<?php echo $locale;?>.js?v=<?php echo $version;?>"></script>
		
<?php /*
		<script type="text/javascript" src="scripts/visualizations.js?v=<?php echo $version?>"></script>
		<script type="text/javascript" src="scripts/onChange.js?v=<?php echo $version?>"></script>
		<script type="text/javascript" src="scripts/csvViewer.js?v=<?php echo $version?>"></script>
		<script type="text/javascript" src="scripts/rwd-table.js?v=<?php echo $version?>"></script>
		<script type="text/javascript" src="scripts/rwd-tweeks.js?v=<?php echo $version?>"></script>
		<script type="text/javascript" src="scripts/8bitMaskMap.js?v=<?php echo $version?>"></script>
		<script type="text/javascript" src="scripts/svgMaskMap.js?v=<?php echo $version?>"></script>
		<script type="text/javascript" src="scripts/resolveFromMask.js?v=<?php echo $version?>"></script>
		<script type="text/javascript" src="scripts/advancedInteractivity.js?v=<?php echo $version?>"></script>
		<script type="text/javascript" src="scripts/onLoad.js?v=<?php echo $version?>"></script>
<?php */ 


	

?>

	<script type="text/javascript" src="/DVS/DVT/scripts/combine.js.php<?php if(file_exists("scripts/combine.js.php.min.js"))print(".min.js");?>?v=<?php echo $version?>"></script>


<?php /////////////////////////////////////////////////////////////////////////// ?>

<?php 
	if($_SERVER['SERVER_NAME'] == 'eurofound.europa.eu'){
		//require_once("DVT.EF.googleAnalytics.php"); 
	}
?>
		<!-- Dev Author kp at eworx.gr -->
