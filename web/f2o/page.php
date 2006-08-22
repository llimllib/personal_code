<?php
	session_start();
	if(empty($_SESSION['user_name'])) {
		die('An error has ocurred. It may be that you have not logged in, or that your session has expired.
			Please try <a href="login.php">logging in</a> again or contact the 
			<a href="mailto:admin@example.com">system administrator</a>');
	}
	print "Welcome, validated user!";
?>
