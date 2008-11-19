<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1" />
<title><?php echo isset($title) ? $title : 'CMUA - Central Maryland Ultimate Association' ?></title>
<link href="<?php echo base_url(); ?>static/css/cmua.css" type="text/css" rel="stylesheet" media="screen"/>
<script type="text/javascript" src="static/jquery-1.2.6.js"></script>
</head>

<body>

<div id="container">
	<div id="header">
		<div id="top">
			<div id="weather">
			    <p><?php // Weather Module ?></p>
			</div>
			<div id="date">
				<p><?php echo(date('D, F jS')); ?></p>
			</div>
		</div>
		
		<div id="logo">
			<a href="#"><h1><strong>CMUA</strong></h1></a>
		</div>
			
		<div id="navigation">
			<ul id="nav">
				<li id="events"><a href="#" class="on"><strong>Events</strong></a></li>
				<li id="leagues"><a href="#" class="on"><strong>Leagues</strong></a></li>
				<li id="directions"><a href="/directions" class="on"><strong>Directions</strong></a></li>
				<li id="links"><a href="#" class="on"><strong>Links</strong></a></li>
			</ul>
		</div>
		
		<div id="login">

<?php /* comment out the login form for now
<?php $CI =& get_instance();
    $user = $CI->db_session->userdata('LOGGED_IN');
    if ($user): ?>
        <h3>Hello, <b><?php echo $user; ?></b></h3>
        <p><a href="#">Check Schedule</a> | <span>Message Team</span>
        | <a href="<?php echo base_url();?>auth/logout">logout</a></p>
<?php else: ?>
    <?php echo form_open('auth/login'); ?>
    User:
        <? //TODO: insert username on a failed attempt ?>
        <?php echo  form_input(array('name'      => 'username',
                             'id'        => 'username',
                             'maxlength' => '30',
                             'size'      => '8',
                        ));
        ?>
    Pass:
        <?php echo  form_password(array('name'      => 'password',
                                'id'        => 'password',
                                'maxlength' => '30',
                                'size'      => '8',
                        ));
        ?><?php echo  form_submit(array('name'  => 's',
                          'id'    => 's',
                          'value' => 'login',
                    ));
        ?>
<?php echo  form_close(); ?>
    <!-- <a href="<?php echo base_url();?>auth/login">login</a> -->
<?php endif; ?>
*/ ?>
            </div>
		</div>	

