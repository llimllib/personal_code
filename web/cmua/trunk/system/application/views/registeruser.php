<?php echo  form_open('user/insertnew'); ?>
<table id="registeruser">
<tr><td>
    User Name:
    </td><td>
        <?php echo  form_input(array('name'      => 'username',
                             'id'        => 'username',
                             'maxlength' => '45',
                             'size'      => '30',
                        ));
        ?>
    </td></tr>
    <tr><td>
    Password:
        </td><td>
        <?php echo  form_password(array('name'      => 'password1',
                                'id'        => 'password1',
                                'maxlength' => '99',
                                'size'      => '30',
                        ));
        ?>
    </td></tr>
    <tr><td>
    Confirm Password:
        </td><td>
        <?php echo  form_password(array('name'      => 'password2',
                                'id'        => 'password2',
                                'maxlength' => '99',
                                'size'      => '30',
                        ));
        ?>
    </td></tr>
    <tr><td colspan="2" align="center">
    <?php echo  form_submit(array('name'  => 'registeruser',
                          'id'    => 'registeruser',
                          'value' => 'Register User',
                    ));
    ?>
</tr></table>
<?php echo  form_close(); ?>
