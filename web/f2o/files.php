<?php
	require_once 'includes/db.php';
	require_once 'includes/debug.php';
	require_once 'includes/funcs.php';

	$result = connect();
		testError($result);
	select_db('llimllib');
	
    printHeader();
    
    printNav();
	
	print '<div class="bodytext">
	<strong>Tutorial File</strong><br />
	An archive of the files referenced in my <a href="loginScript1.php">login</a>
	tutorial.<br /><br />
	<a href="files/login_tut.tar.gz">download (1k)</a>
</div>
';
    
    printFooter();
?>
