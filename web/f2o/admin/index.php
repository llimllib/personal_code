<?php
    session_start();

    require_once '../includes/db.php';
    require_once '../includes/debug.php';
    require_once '../includes/funcs.php';
    
    connect();
    select_db('llimllib');

    printHeader('Admin Section. How cool I am. mwahahaha',true);
    
    printNav(true);
    
    $msg = 'You better be me or else! grrrr <mean face>';
	print '<div class="floatright">';
	    $msg = 'Add a blog, dawg!</p>
	            <p>Remember to start your query with a &lt;p> tag,
	            or else you messed it up';
		printMessageBox($msg);
		printShoutBox();
	print '
</div>
	';
    
    if(isset($_POST['user_name'])) {
        $query = "
        SELECT user_name, permission
        FROM user
        WHERE user_name = '{$_POST['user_name']}'
        AND password = md5('{$_POST['password']}')";
        
        $result = query($query);
        
        if(isset($result['error'])) {
            print 'query error: ' . $result['error'];
            printFooter();
            die();
        }
        
        $row = fetchArray($result);
        
        if(isset($row[0])) {
            print 'validated! score!';
            $_SESSION['user_name'] = $row['user_name'];
            $_SESSION['permission'] = $row['permission'];
        }
        else {
            print 'you loser. I hate you. log in right.';
            displayLogin();
	        $_SESSION = array();
	        session_destroy();   
        }
        printFooter();
    }
    else {
        displayLogin();
        printFooter();
    }

function displayLogin() {
	print '
	<div class="bodytext">
	<table width="250">
	<form name="login" action="index.php" method="post">
		<tr><td>
            username:</td><td><input type="text" name="user_name"></td></tr>
		<tr><td>
            password:</td><td><input type="password" name="password"></td></tr>
		<tr><td>
			</td><td><input type="submit" value="submit"></td></tr>
    </form>
	</table>
	</div>';
}
?>