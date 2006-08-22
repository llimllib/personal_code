<?php
	require_once 'includes/db.php';
	require_once 'includes/debug.php';
	require_once 'includes/funcs.php';

	$result = connect();
		testError($result);
	select_db('llimllib');

	if(isset($_POST['comment'])) {
		$comment = $_POST['comment'];
		addslashes($comment);
		$query = "
			INSERT INTO comments
            SET
			id=NULL,
			name='{$_POST['name']}',
			com_date=NOW(),
			comment='$comment',
            response_to='{$_POST['response_to']}',
            subject='{$_POST['subject']}'
        ";
		
		$result = query($query);
			testError($result);
	}
	
    printHeader();
    
    printNav();

	print '<div class="bodytext">';

    print '<h2>Team Name Ideas</h2>';

    printIdeas();

	$i = 0;
	$query = "SELECT * FROM `comments` ORDER BY `id` ASC";
        
	$result = query($query);
    $row = fetchAssoc($result);

	while(!isset($row['error'])) {
		print '<div class="comment">';
        print "\n\n<strong>";
        print $row['subject'];
        print "</strong> by:";
		print $row['name'];
        print "  at ";
        print $row['com_date'];
		print "</strong><br><br>";
		print $row['comment'];
        print "</div>";

		$row = fetchAssoc($result);
	}

    printCommentFields();
    
    printFooter();

    function printCommentFields() {
print '
<h2>Enter a Comment:</h2><br>
<table>
<form name="addComment" method="POST" action="names.php">
<input type="hidden" name="response_to" value="0">
<tr><td width="100">Your Name:</td><td><input type="text" name="name" size="25"></td></tr>
<tr><td width="100" maxlength="50">Subject:</td><td><input type="text" name="subject" size="25"></td></tr>
<tr><td>Comment:</td><td><textarea name="comment" cols="45" rows="10"></textarea></td></tr>
<tr><td colspan="2" align="left"><input type="submit" value="Submit Comment"></td></tr>
</form>
</table>';
    }


    function printIdeas() {
print '
<table border="0" cellpadding="0" cellspacing="6">
<tr><td><strong>Names</strong></td><td><strong>Comments</strong></td></tr>
<tr><td>The Socialists</td><td>Da Comrade!</td></tr>
<tr><td>The Bolsheviks</td><td>Our Revolutionary fervor will win games</td></tr>
<tr><td>The Commies</td><td>Do I get to be Stalin?</td></tr>
<tr><td>Marry me Melissa</td><td>Will you?</td></tr>
<tr><td>Nads</td><td>Then we can yell "go nads!"</td></tr>
<tr><td>Heavy Flow</td><td>An excuse to whine about bad calls</td></tr>
<tr><td>Run Away You Pansies&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td><td>Let\'s call the other team names!</td></tr>
<tr><td>Good In Bed</td><td>Think of how great our cheers would be</td></tr>
<tr><td>Right Said Fred</td><td>We\'re too sexy</td></tr>
<tr><td>Drop Dead Gorgeous</td><td>Hey, I\'m on a theme here</td></tr>
<tr><td>The Johns</td><td>Might as well run with it</td></tr>
<tr><td>Cunning Runts</td><td>Tall team irony</td></tr>
</table>
<br><h2>Comments:</h2><br>
';
    }

?>
