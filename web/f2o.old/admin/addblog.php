<?php
    session_start();

    require_once '../includes/db.php';
    require_once '../includes/debug.php';
    require_once '../includes/funcs.php';
    
	connect();
	select_db('llimllib');
	
    printHeader('Admin Section. How cool I am. mwahahaha',true);
    
    printNav(true);
    
	print '<div class="floatright">';
	    $msg = 'Add a blog, dawg!</p>
	            <p>Remember to start your query with a &lt;p> tag,
	            or else you messed it up';
		printMessageBox($msg);
		printShoutBox();
	print '</div>';
    
    if(!validate()) {
        print 'please <a href="index.php">login</a> again';
        printFooter();
        die();
    }
    
    connect();
    select_db('llimllib');
    
    if(isset($_POST['blogtext'])) {
        $query = "
        INSERT INTO blog VALUES(
            NULL,
            NOW(),
            '{$_POST['blogtitle']}',
            '{$_POST['blogtext']}')
        ";
        $result = query($query);
        
        if(isset($result['error'])) {
            print 'query error: ' . $result['error'];
            printFooter();
            die();
        }
        elseif(isset($_SESSION['lastblog']) &&
                isset($_POST['blogtitle']) &&
                !strcmp($_SESSION['lastblog'], $_POST['blogtitle'])) {
            print 'no refreshes allowed, jerk.';
            printFooter();
            die();
        }
        else {
            print 'successful blog entry of:<br><br>
            Title: ' . $_POST['blogtitle'] . '<br>
            Text: ' . $_POST['blogtext'];
            printFooter();
            
            //prevent refresh
            $_SESSION['lastblog'] = $_POST['blogtitle'];
            
            die();
        }
    }
    
    printAddBlogForm();
    
    printFooter();
    
    function printAddBlogForm() {
        print'
		<div class="bodytext">
        <table width="700" align="center" border="0">
        <form name="addblog" action="addblog.php" method="post">
            <tr><td>
            Blog Title:</td><td><input type="text" name="blogtitle">
                </td></tr>
            <tr><td>
            Blog Text:</td><td>
                <textarea name="blogtext" rows="15" cols="35"></textarea>
                </td></tr>
            <tr><td colspan="2" height="50" valign="middle" 
                align="center">
                <input type="submit" value="submit">
                </td></tr>
        </form>
        </table>
		</div>
        ';
    }
?>