<html><head><title>loops</title></head>
<body>
<h4>
<?php

//test numbers
$int = 1;
$float = 2.4;
print "int = $int float = $float  <br><br>\n";

//test strings
$string = "PHP rocks";
print "$string";
print "<br><br>\n";

$words = explode(" ", $string);
print_r($words);
print "<br><br>\n";

//test arrays
$array = array(1=>20);
$array[0] = 15;
print_r($array);
print "<br><br>\n";

$hash = array('banana' => 'yellow', 'orange' => 'orange', 'apple' => 'red');
print "{$hash['banana']}";
print "<br><br>\n";

?>
</h4>
</body>
</html>
