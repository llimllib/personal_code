<?php 
    session_start(); 
 
    require_once '../includes/db.php'; 
    require_once '../includes/debug.php'; 
    require_once '../includes/funcs.php'; 

    $connection = connect();
    	testError($connection);
    $res = select_db('llimllib');
    	testError($res);
    printHeader('Clip it up, yo',true); 
     
    printNav(true);
    
    if(!validate()) { 
        print 'please <a href="index.php">login</a> again'; 
        printFooter(); 
        die(); 
    }
    
    if(!isset($_GET['act'])) {
        printUploadFile();
        printDisplayFiles();
    }
    elseif(strtolower($_GET['act']) == 'upload') {
        uploadFile();
        printUploadFile();
        printDisplayFiles();
    }
    else {
        printUploadFile();
        printDisplayFiles();
    }
    
    printFooter();
 
function uploadFile()
{
    // TODO: check that this is a valid dir
    $uploaddir = 'files/';

    // TODO: handle duplicate names?
    //       get file description?
    $query = "
        INSERT INTO clipboard
        SET
        name = '{$_FILES['clipfile']['name']}',
        path = $uploaddir . $_FILES['clipfile']['name']',
        description = '',
        date = now(),
        clip_id = NULL";
    
    query($query);
        testError($query);
    
    move_uploaded_file($_FILES['clipfile']['tmp_name'], $uploaddir . $_FILES['clipfile']['name'])
        or die('fucking file failed');
    
    print "file {$_FILES['clipfile']['name']} uploaded successfully. Word Up!";
}

function printUploadFile()
{
    print '
    <form enctype="multipart/form-data" action="clipboard.php?act=upload" method="POST">
    <table><tr align="left"><td>
    <input type="hidden" name="MAX_FILE_SIZE" value="1000000"> <!--1 meg max-->
    Add to Clipboard: <input name="clipfile" type="file"><br></td></tr>
    <tr align="left"><td>
    <input type="submit" value="Clip"></td></tr>
    </table>
    </form>
    ';
}

function printDisplayFiles()
{
    $query = "
        SELECT *
        FROM clipboard
    ";

    $result = query($query);
        testError($result);
    $row = fetchArray($result);
    
    while(!isset($row['error'])) {
        print $row['filename'] . "<br>\n";
    }
}

?>
    
