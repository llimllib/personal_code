<?php
$con=mysql_connect('localhost', 'codeigniter','codeonfire');
echo "if you get an error message above OR the line below this one reads (bool) false, then your settings are not correct<br>";
var_dump($con);
?> 
