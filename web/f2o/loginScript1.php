<?php
	require_once 'includes/db.php';
	require_once 'includes/debug.php';
	require_once 'includes/funcs.php';

	$connection = connect();
		testError($connection);
	$res = select_db('llimllib');
		testError($res);
		
	printHeader();
    
    printNav();
    
	print "<div class=\"floatright\">\n";
	
		printShoutBox();
		printF2oThanks();
		
	print "</div>\n";
	
	print '<div class="bodytext">';
	
	include_once('LoginScriptpart1.html');
	
	printFooter();
