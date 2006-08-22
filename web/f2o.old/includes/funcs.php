<?php
function printDate($date) {
	$year = substr($date, 2, 2);
	$month = substr($date,5, 2);
	$day = substr($date,8,2);
	print "$month.$day.$year";
}

function printDateTime($datetime) {
	$month = substr($datetime, 5, 2);
	$day = substr($datetime, 8, 2);
	$hour = substr($datetime, 11, 2) - 12;
	$min = substr($datetime, 14, 2);
	print "$month.$day $hour:$min";
}

function printHeader($title = 'Bill Mill World Domination Network'
                        , $admin = false) {
    print '
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" 
    "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<title>' . $title . '</title>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">';
if($admin)
    print '<link href="../includes/style.css" rel="stylesheet" 
            type="text/css">';
else
    print '<link href="includes/style.css" rel="stylesheet" 
            type="text/css">';
print '
</head>
<body class="body">
<div class="main">
';
}

function printNav($admin = false) {
if($admin) {
print'
<div class="navbar">
    <div class="nav">
        <a href="../index.php" class="navtext">Home</a> &nbsp;
        <a href="index.php" class="navtext">Admin Login</a> &nbsp;
        <a href="addblog.php" class="navtext">Add Blog</a> &nbsp;
        <a href="editblog.php" class="navtext">Edit Blog</a> &nbsp;
    </div>
</div>

<a href="http://llimllib.f2o.org">
    <img src="../images/billmill.gif" width="153"
	  height="23" border="0" align="right" class=navimg>
</a>
';
}
else {
print'
<div class="navbar">
	<div class="nav"> 
		<a href="index.php" class=navtext>Me</a> &nbsp;
  		<a href="writing.php" class=navtext>Writing</a> &nbsp;
		<a href="should.php" class=navtext>Should</a> &nbsp;
		<a href="admin/index.php" class=navtext>Admin</a> &nbsp;
		<a href="mailto:wimill@ursinus.edu" class=navtext>Email</a>
	</div>
</div>

<a href="http://llimllib.f2o.org">
    <img src="images/billmill.gif" width="153"
	  height="23" border="0" align="right" class=navimg>
</a>
';
}

print'
<!-- Why does this have to be here? I guess I may never know,
	but it pisses me off. Anyway, if I remove it, the float
	right box breaks. Go Figure. -->	
<br>
';
}

function printMessageBox($msg) {
print '
	<div class="rightbox">
    	<p>' . $msg . '</p>
    </div>
';
}

function printShoutBox() {

	print'
	<div class="rightbox">
		<strong>Shout out!</strong>
		<p>
		<form name="shout" method="post" action="'. $_SERVER['PHP_SELF'] .'">
			<input type="text" size="8" name="shout_msg">
			<input type="submit" value="shout">
		</form></p>
	</div>
	<div class="rightbox">
';
	$query = "
		SELECT msg,add_date
		FROM shoutbox
		WHERE msg != ''
		ORDER BY add_date DESC
		LIMIT 5";
	$res = query($query);
		testError($res);
	$row = fetchArray($res);
	while(!isset($row['error'])) {
		print "<p>";
		printDateTime($row['add_date']);
		print "<br>{$row['msg']}</p>\n";
		$row = fetchArray($res);
	}
print'
	</div>

	';
}

function printFooter() {
print '

</div>
</body class=body>
</html>
';
}

function validate() {
    if(isset($_SESSION['user_name'])) {
        $user_name = $_SESSION['user_name'];
        $permission = $_SESSION['permission'];
        return true;
    }
    else {
        return false;
    }
}

function testError($obj) {
   if(isset($obj['error'])) {
        print 'database error: ' . $obj['error'];
        print '<br>Please contact the <a href="mailto:llimllib@f2o.org>
        administrator</a> to inform him of the error';
        printFooter();
        die();
    }
}
?>