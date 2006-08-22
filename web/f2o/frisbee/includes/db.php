<?php

	
function connect (){

	$dbname = 'llimllib';
	$dbpass = 'ginger22';
	
    $connection = mysql_connect("localhost", $dbname, $dbpass)
		or $errors['error'] = mysql_error();

    @mysql_select_db("llimllib") 
		or $errors['error'] = mysql_error();
		
	if (empty($errors)) {
		return true;
	}
	else {
		return $errors;
	}
}

function select_db ($db_name) {
	$result = mysql_select_db($db_name)
		or $errors['error'] = mysql_error();
		
	if (empty($errors)) {
		return $result;
	}
	else {
		return $errors;
	}
}

function query ($query) {
    $result = mysql_query($query)
		or $errors['error'] = mysql_error();
	
	if (empty($errors)) {
		return $result;
	} 
	else {
		return $errors;
	}
}

function fetchArray ($result) {
    $row = mysql_fetch_array($result)
        or $errors['error'] = mysql_error();

	if (empty($errors)){
		return $row;
	}
	else {
		return $errors;
	}
}

function fetchAssoc ($result) {
    $row = mysql_fetch_assoc($result)
        or $errors['error'] = mysql_error();

	if (empty($errors)){
		return $row;
	}
	else {
		return $errors;
	}
}
?>
