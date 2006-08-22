<?php
session_start();
?>
<html><head>
<title>validate.php</title>
</head>
<body>
<?php
    $db_user = 'bill';
    $db_pass = 'ginger22';
    $username = $_POST['username'];
    $password = $_POST['password'];

    $connection = mysql_connect('localhost', $db_user, $db_pass)
        or die(mysql_error());
    mysql_select_db('dictators', $connection)
        or die(mysql_error());

    $query = "SELECT * FROM users
                WHERE user_name = '$username'
                AND password = '$password'";

    $result = mysql_query($query, $connection)
        or die('error making query');
    $affected_rows = mysql_num_rows($result);

    if($affected_rows == 1) {
        print '<h3>validated.<br><br><a href="members_only.php">Continue to 
                the members only page</a></h3>';
        $_SESSION['username'] = $username;
    }
    else {
        print 'validation failed';
        $_SESSION['username'] = "";
    }
?>
</body>
</html>
