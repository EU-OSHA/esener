<?php //kp//eworx//gr//EWORX S.A.
///////////////////////////////////////////////////////////////////////////
require_once("DVT.top.php");
///////////////////////////////////////////////////////////////////////////
?>
<!DOCTYPE HTML>
<html lang="en">
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<?php // <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" /> ?>
		<meta name="viewport" content="width=device-width, initial-scale=1.0" />
		<title><?php echo $pageTitle; ?></title>
		<meta name="description" content="<?php echo $visualizationDescription; ?>" />
<?php /////////////////////////////////////////////////////////////////////////// ?>
		<link href="css/screen.css?v=<?php echo $version;?>" rel="stylesheet" type="text/css">
		<link href="css/common.css?v=<?php echo $version;?>" rel="stylesheet" type="text/css">
<?php require_once("DVT.head.css.php");?>

		<?php /////////////////////////////////////////////////////////////////////////// ?>
<?php require_once("DVT.head.js.php");?>
		<?php /////////////////////////////////////////////////////////////////////////// ?>
	</head>
	<body>
		<?php /////////////////////////////////////////////////////////////////////////// ?>
<?php require_once("DVT.body.php");?>
		<?php /////////////////////////////////////////////////////////////////////////// ?>
	</body>
</html>
