<?php 
    session_start(); 
 
    require_once '../includes/db.php'; 
    require_once '../includes/debug.php'; 
    require_once '../includes/funcs.php'; 
	 
    $connection = connect();
    	testError($connection);
    $res = select_db('llimllib');
    	testError($res);
    printHeader('Admin Section. How cool I am. mwahahaha',true); 
     
    printNav(true); 
     
    $msg = 'Step 1:<br>Choose a blog to edit'; 
	print '<div class="floatright">'; 
		printMessageBox($msg); 
		printShoutBox(); 
	print '</div>'; 
     
    if(!validate()) { 
        print 'please <a href="index.php">login</a> again'; 
        printFooter(); 
        die(); 
    } 
     
    $result = connect(); 
    testError($result); 
    $result = select_db('llimllib'); 
    testError($result); 
     
    if(!isset($_POST['blogChooser']) && 
        !isset($_POST['blogtext'])) { 
        printSelectBlog(); 
    } 
    elseif (isset($_POST['blogChooser'])) { 
        printEditBlog(); 
    } 
    else { 
        printCommitBlog(); 
        printSelectBlog(); 
    } 
     
    printFooter(); 
     
    function printCommitBlog() { 
        $query = " 
        UPDATE blog 
        SET title = '{$_POST['title']}', 
        blog_date = '{$_POST['blog_date']}', 
        blogtext  = '{$_POST['blogtext']}' 
        WHERE blog_id = '{$_POST['blog_id']}'"; 
        $result = query($query); 
        testError($result); 
         
        print 'your blog has been successfully edited. Fun stuff, eh? 
        '; 
    } 
         
     
    function printEditBlog() { 
        $query= " 
        SELECT blog_id, blog_date, title, blogtext 
        FROM blog 
        WHERE blog_id = {$_POST['blogChooser']}"; 
        $result = query($query); 
        testError($result); 
         
        $arr = fetchArray($result); 
         
        print ' 
        <table width="400" border="0"> 
        <form name="blogEdit" action="editblog.php" method="post"> 
            <input type="hidden" value="'. $_POST['blogChooser'] . ' 
                " name="blog_id"> 
            <tr><td> 
                Blog Title:</td><td> 
                <input type="text" value="' . $arr['title'] . '"  
                    name="title"> 
                </td></tr> 
            <tr><td> 
                Blog Date:</td><td> 
                <input type="text" value="' . $arr['blog_date'] . '" 
                    name="blog_date"> 
                </td></tr> 
            <tr><td> 
                Blog Text:</td><td> 
                <textarea name="blogtext" rows="15" cols="35">' . 
                $arr['blogtext'] . '</textarea> 
                </td></tr> 
            <tr><td colspan="2" align="center"> 
                <input type="submit" value="submit"> 
                </td></tr> 
       </form> 
       </table>'; 
    } 
     
    function printSelectBlog() { 
        $query = " 
        SELECT blog_id, blog_date, title 
        FROM blog"; 
        $result = query($query); 
        testError($result); 
         
        $arr = fetchAssoc($result); 
         
        print' 
        <form name="selectBlog" action="editblog.php" method="post"> 
        <select name="blogChooser"> 
        '; 
         
        while(!isset($arr['error'])) { 
            print' 
            <option value="' . $arr['blog_id'] . '">' . 
            $arr['blog_date'] . ' ' . substr($arr['title'], 0, 30) . ' 
            </option> 
            '; 
            $arr = fetchArray($result, 'ASSOC'); 
        } 
         
        print' 
        </select><br><br> 
        <input type="submit" value="submit"> 
        </form> 
        '; 
    } 
         
         
?>