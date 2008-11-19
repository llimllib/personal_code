<?php include_once('head.php'); ?>
<div id="wrap">
    <div id="side">
    </div>
    <div id="content">

<?= form_open('auth/login'); ?>
<div id="login">
    User Name:
        <?= form_input(array('name'      => 'username',
                             'id'        => 'username',
                             'maxlength' => '30',
                             'size'      => '30',
                             'value'     => $username
                        ));
        ?>
    <br>
    Password:
        <?= form_password(array('name'      => 'password',
                                'id'        => 'password',
                                'maxlength' => '30',
                                'size'      => '30',
                        ));
        ?>
    <br>
    <?= form_submit(array('name'  => 'login',
                          'id'    => 'login',
                          'value' => 'Log In',
                    ));
    ?>
<?= form_close(); ?>

    </div>
</div>
<?php include_once('foot.php'); ?>
