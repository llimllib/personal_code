<?php
    session_start();
    if(empty($_SESSION['username'])) {
        die('<h3>You are not logged in. I hope you feel dirty.<br>Try <a href="example1.html">logging in</a> first.</h3>');
    }
?>
<html><head><title>SECRETS</title></head>
<body>
<h3>Bill Mill's Secrets Page!</h3>
<ul>
<li>The moon really IS made of cheese</li>
<li>Momma didn't tell me there'd be days like this</li>
<li>John F. Kennedy is alive and shacked up with Castro</li>
<li>The world is square</li>
</ul>
</body>
</html>
