<html>

<head>
<script src="http://code.jquery.com/jquery-latest.min.js" type="text/javascript"></script>

<SCRIPT language="JavaScript">
<!--
    var colorToCountry =
    [
    ["f8766d","AL"],
    ["f27d53","AT"],
    ["ea8331","BA"],
    ["e28a00","BE"],
    ["d89000","BG"],
    ["cd9600","BY"],
	["70b000","CH"],
    ["c09b00","CS"],
    ["b2a000","CY"],
    ["a3a500","CZ"],
    ["91aa00","DE"],
    ["7cae00","DK"],
    ["61b200","EE"],
    ["39b600","EH"],
    ["00b92a","EL"],
    ["00bb4e","ES"],
    ["00be67","FI"],
    ["00bf7d","FR"],
    ["00c091","HR"],
    ["00c1a3","HU"],
    ["00c0b4","IE"],
    ["00bfc4","IT"],
    ["00bdd3","LI"],
    ["00bae0","LT"],
    ["00b5ec","LU"],
    ["00b0f6","LV"],
    ["00a9ff","MD"],
    ["35a2ff","ME"],
    ["7099ff","MK"],
    ["9590ff","MT"],
    ["b186ff","NL"],
    ["c77cff","NO"],
    ["d973fc","PL"],
    ["e76bf3","PT"],
    ["f265e8","RO"],
    ["fa62db","SE"],
    ["ff61cc","SI"],
    ["ff62bc","SK"],
    ["ff65aa","TR"],
    ["ff6a98","UA"],
    ["fd7083","UK"]
    ];

function getCountryFromColor(color){
   for (var index = 0; index < colorToCountry.length; index++) {
     if(colorToCountry[index][0] == color){
       return colorToCountry[index][1];
     }
   }
   return "-";
}

function findPos(obj) {
    var curleft = 0, curtop = 0;
    if (obj.offsetParent) {
        do {
            curleft += obj.offsetLeft;
            curtop += obj.offsetTop;
        } while (obj = obj.offsetParent);
        return { x: curleft, y: curtop };
    }
    return undefined;
}

function rgbToHex(r, g, b) {
    if (r > 255 || g > 255 || b > 255)
        throw "Invalid color component";
    return ((r << 16) | (g << 8) | b).toString(16);
}

//-->
</script>

<SCRIPT language="JavaScript">
<!--

    if (!document.images){
      alert("Browser not capable to preload images");
    }
    
    var visualization = new Image();
    
    var pageHref = window.location.href ;
    
    if(pageHref.indexOf('?') == -1){
      visualization.src = "http://194.30.34.27/DVS/render/?dataSource=3RDEQLS&locale=EN&media=png&width=1024&question=Y11_Q29d&plot=heatMap&subset=Y11_Agecategory&subsetValue=All&answer=1--Strongly-agree";
    }else{
      visualization.src = "http://194.30.34.27/DVS/render/get.php" + pageHref.substring(pageHref.indexOf("?"));
    }

    var visualizationMask = new Image();
    visualizationMask.src = "http://194.30.34.27/DVS/render/?plot=maskMap&width=1024&media=png";

//-->
</script>
        
</head>

<body>

    <canvas id="R2Canvas" width="1024" height="920">Your browser does not support the HTML5 canvas tag.</canvas>
    <canvas id="R2MaskCanvas" width="1024" height="1300" style="display:none" >Your browser does not support the HTML5 canvas tag.</canvas>
    <center><div id="status">mouse over the map</div></center>
    
    <script>
    <!--

        visualization.onload = function() {
            var context = document.getElementById("R2Canvas").getContext("2d"); //faster rendering time
            context.drawImage(visualization, 0, 0);
        };
        
        $(function(){

            var $R2MaskCanvas = $("#R2MaskCanvas")[0];
            var $R2MaskCanvas2dContext = $R2MaskCanvas.getContext('2d');
            visualizationMask.onload = function() {
                $R2MaskCanvas.getContext("2d").drawImage(visualizationMask, 0, 0);
            };

            var $R2Canvas = $("#R2Canvas");
            
            $R2Canvas.mousemove(function(e) {
                var pos = findPos(this);
                var x = e.pageX - pos.x;
                var y = e.pageY - pos.y;
                var coord = "x=" + x + ", y=" + y;
                var c = $R2MaskCanvas2dContext;

                var p = c.getImageData(x, y, 1, 1).data;
                var hex = ("000000" + rgbToHex(p[0], p[1], p[2])).slice(-6);
                
                $('#status').html(getCountryFromColor(hex));
            });

            $R2Canvas.click(function(e) {
                var pos = findPos(this);
                var x = e.pageX - pos.x;
                var y = e.pageY - pos.y;
                var coord = "x=" + x + ", y=" + y;

                var c = $R2MaskCanvas2dContext;
                var p = c.getImageData(x, y, 1, 1).data;
                var hex = ("000000" + rgbToHex(p[0], p[1], p[2])).slice(-6);
                alert(getCountryFromColor(hex));
            });
        })
    //-->
    </script>

</body>
</html>
