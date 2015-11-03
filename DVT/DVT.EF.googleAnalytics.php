<?php if(false){ // old g analytics code
	$_SERVER['SERVER_NAME'] = "eurofound.europa.eu"; // it is not resolved correctly
?>

		<script type="text/javascript" src="http://<?php echo $_SERVER['SERVER_NAME']; ?>/scripts/GA2/base64.js"></script>
		<script type="text/javascript" src="http://<?php echo $_SERVER['SERVER_NAME']; ?>/scripts/GA2/analytics.js"></script>
		<script type='text/javascript'>
			var _gaq = _gaq || [];
			var pluginUrl ='//www.google-analytics.com/plugins/ga/inpage_linkid.js';
			_gaq.push(['_require', 'inpage_linkid', pluginUrl]);
			var sra = '<?php echo base64_encode(((isset($_SERVER['REMOTE_ADDR']))?$_SERVER['REMOTE_ADDR']:'1.1.1.1')); ?>';

			_gaq.push(['_setAccount', 'UA-845481-1']);
			_gaq.push(['_setDomainName', 'eurofound.europa.eu']);
			_gaq.push(['_setCustomVar', 1, 'SRA', base64_decode(sra), 3]);

			 if(document.referrer.match(/google\.com\/(cse|custom)/)) {
				ref = document.referrer;
				re = /(\?|&)q=([^&]*)/;
				searchq = re.exec(ref);
				if(searchq) {
					_gaq.push(['_addIgnoredOrganic', searchq[2]]);
				}
			}

			(function() {
				var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
				ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
				var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
			})();

		</script>

		<script type="text/javascript">
		    var _paq = _paq || [];
		    (function(){ var u=(("https:" == document.location.protocol) ? "https://www.eurofound.europa.eu/piwik/" : "http://www.eurofound.europa.eu/piwik/");
		    _paq.push(['setSiteId', 1]);
		    _paq.push(['setTrackerUrl', u+'piwik.php']);
		    _paq.push(['setLinkTrackingTimer', 750]);
		    _paq.push(['trackPageView']);
		    _paq.push(['enableLinkTracking']);
		    var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0]; g.type='text/javascript'; g.defer=true; g.async=true; g.src=u+'piwik.js';
		    s.parentNode.insertBefore(g,s); })();
		 </script>
		<noscript>
		    <?php $siteUrl = ($_SERVER['HTTPS']) ? 'https://www.eurofound.europa.eu/piwik/' : 'http://www.eurofound.europa.eu/piwik/'; ?>
		    <p><img src="<?php echo $siteUrl; ?>piwik.php?idsite=1&rec=1" style="border:0" alt="" /></p>
		</noscript>

<?php }?>		

		<script type="text/javascript">
		<!--
			$(function(){
				$("#csvExport").click(function(){
					_gaq.push(['_trackEvent', 'DVS: XLS Export', $("#dataSource").val(), 'URL: '+$(this).attr("href")]);
				});

				$("#pngExport").click(function(){
					_gaq.push(['_trackEvent', 'DVS: PNG Export', $("#dataSource").val(), 'URL: '+$(this).attr("href")]);
				});

				$("#epsExport").click(function(){
					_gaq.push(['_trackEvent', 'DVS: EPS Export', $("#dataSource").val(), 'URL: '+$(this).attr("href")]);
				});

				$("#svgExport").click(function(){
					_gaq.push(['_trackEvent', 'DVS: SVG Export', $("#dataSource").val(), 'URL: '+$(this).attr("href")]);
				});

				$("#pdfExport").click(function(){
					_gaq.push(['_trackEvent', 'DVS: PDF Export', $("#dataSource").val(), 'URL: '+$(this).attr("href")]);
				});

				$("#htmlExport").click(function(){
					_gaq.push(['_trackEvent', 'DVT: Embeddable HTML', $("#dataSource").val(), 'URL: '+$(this).attr("href")]);
				});

				$("#toolUrlExport").click(function(){
					_gaq.push(['_trackEvent', 'DVT: Page URL', $("#dataSource").val(), 'URL: '+$(this).attr("href")]);
				});

				$("#citeExport").click(function(){
					_gaq.push(['_trackEvent', 'DVT: Page reference', $("#dataSource").val(), 'URL: '+$(this).attr("href")]);
				});

				$("#urlExport").click(function(){
					_gaq.push(['_trackEvent', 'DVT: Bookmark', $("#dataSource").val(), 'URL: '+$(this).attr("href")]);
				});
			});

			function trackPageView(imgURL){
	 			//_gaq.push(['_trackPageview', imgURL ]);
				//_gaq.push(['_trackEvent', $("#dataSource").val(), $('#question').val(), $('#plot').val()]);
			}

		//-->
		</script>
