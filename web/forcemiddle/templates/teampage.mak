<html><head>
	<title>${team.name} discs at forcemiddle.com</title>
    <link rel="shortcut icon" href="static/favicon.ico" type="image/vnd.microsoft.icon">
	<link rel="stylesheet" type="text/css" href="/static/styles.css" />	

	<%include file="tracker.html"/>
	<%
		#TODO: this needs to go into the database at some point?
		name_sku = {
			"Jojah": ("Jojah", "Jojah-2008"),
			"Harvard": ("Harvard", "Harvard-2008"),
			"UGMO": ("UGMO", "UGMO-2008"),
			"Uriel": ("Uriel", "Uriel-2008"),
			"Williams": ("Williams", "Williams-2008")}
		item_name, sku = name_sku[team.name]
	%>
<body>
<div id="singledisc">
	<div id="logo2">
		<a href="/"><img src="/static/logo.gif" border="0"></a>
	</div>
	<img src="${team.fullsize}" id="bigdisc">
	<div id="disctext">
		${team.description}
		<div id="addtocart">
			##TODO: separate this into a template.
			<form target="paypal" 
					action="https://www.paypal.com/cgi-bin/webscr" 
					method="post">
			<p class="addtocart">
				Price: $10<p>
				<input type="image" 
					   src="/static/addtocart.gif"
					   border="0" name="submit" 
					   alt="Make payments with PayPal - it's fast, free and secure!">
			</p>
			<input type="hidden" name="add" value="1">
			<input type="hidden" name="cmd" value="_cart">
			<input type="hidden" name="business" value="forcemiddlediscs@gmail.com">
			<input type="hidden" name="item_name" value="${item_name}">
			<input type="hidden" name="item_number" value="${sku}">
			<input type="hidden" name="shopping_url" value="http://forcemiddle.com">
			<input type="hidden" name="amount" value="10.00">
			<input type="hidden" name="no_shipping" value="0">
			<input type="hidden" name="no_note" value="1">
			<input type="hidden" name="currency_code" value="USD">
			<input type="hidden" name="lc" value="US">
			<input type="hidden" name="bn" value="PP-ShopCartBF">
			</form>
		</div>
	</div>
	<div id="footer">
		<a href="/">Home</a> | <a href="/About">About Us</a>
	</div>
</div>
</body></html>
