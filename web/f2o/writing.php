<?php
	require_once 'includes/db.php';
	require_once 'includes/debug.php';
	require_once 'includes/funcs.php';

	connect();
	select_db('llimllib');
	
	printHeader();

	printNav();
	
	print "<div class=\"floatright\">\n";
	
	    $msg = 'PHP rocks! site coming soon - I\'m developing my libraries
               right now';
		printMessageBox($msg);
		printShoutBox();
		
	print "</div>\n";
	
	print '<div class="bodytext">';
	
	print '
		<p>
		<a href="loginScript1.php" class="link">
			Log in Tutorial Part 1</a>
		</p><p>
			Part 1 of my login tutorial, which (maybe?) will be posted in the
			future on Jason Lotito\'s website, 
			<a href="http://phpcomplete.com" class="link">phpcomplete.com</a>.
			In it, I try to be as simple as possible,
			leading the reader through each step from db to html to php to a
			complete but very basic authentication system. Shortly, I plan to
			write a second part in which I try to discuss some of the security
			concerns inherent in such a system.<br>
			Please note, it\'s currently formatted for his page, not mine.
		</p>
	';
	
	printFooter();
?>