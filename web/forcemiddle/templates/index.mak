<html><head>
	<title>${title}</title>
    <link rel="shortcut icon" href="static/favicon.ico" type="image/vnd.microsoft.icon">
	<link rel="stylesheet" type="text/css" href="/static/styles.css" />	
	<script type="text/javascript" src="/static/jquery-1.2.1.min.js"></script>

	<%include file="tracker.html"/>

	<script type="text/javascript">
	var discs = ${discs};

	$(document).ready(function() {
		$("#frisbees > a > img").width(146);
		$("#frisbees > a").hover(
			function() {
				var big = 200;
				var img = $(this).children(0)[0];
				var xoffset = (big-img.width) / 2;
				var yoffset = (big-img.height) / 2;

				$(this).children(0).animate({
					"top": "-=" + yoffset + "px",
					"left": "-=" + xoffset + "px",
					"width": big}, 
					/*speed:*/ 150);
				//$("#desc")[0].innerHTML = $(this).attr("href");
			},
			function() {
				var disc = discs[$(this).children(0).attr("id")]

				$(this).children(0).animate({
					"top": disc[0] + "px",
					"left": disc[1] + "px",
					"width": disc[2] + "px"}, 
 			       /*speed:*/ 150);
				$("#desc")[0].innerHTML = "";
			}
		)
	});
	</script>
</head>
<body STYLE="background-image: url('/static/back2.jpg'); 
			 background-repeat: no-repeat; 
			 background-position: 50% 0px;">
<div id="logo">
	<a href="/"><img src="/static/logo.gif" border="0"></a>
</div>
<div id="frisbees">
	<h2 id="desc"></h2>
    <a href="UGMO"> 
		<img src="/static/ugmo1.gif" border=0 id="top">
	</a>
    <a href="Harvard"> 
		<img src="/static/harvard.gif" border=0 id="topright">
	</a>
    <a href="Jojah">
		<img src="/static/georgia.gif" border=0 id="right">
	</a>
    <a href="Uriel">
		<img src="/static/uriel.gif" border=0 id="botright">
	</a>
	<a href="Williams">
		<img src="/static/williams.gif" border=0 id="botleft">
	</a>
</div>
	<div style="margin-top:475px;" id="footer">
		<a href="/">Home</a> | <a href="/About">About Us</a>
	</div>
</body>
</html>
