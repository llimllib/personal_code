<?php include_once('head.php');
$hidden = isset($id) ? array('id' => $id) : array();
?>
<?= form_open('news/insertnew', '', $hidden); ?>
<div id="wrap">
    <div id="side">
    </div>
    <div id="content">
<table id="addnews">
<tr><td>
    Title:
    </td><td>
        <?= form_input(array('name'      => 'title',
                             'id'        => 'title',
                             'maxlength' => '99',
                             'size'      => '50',
                             'value'     => isset($title) ? $title : '',
                        ));
        ?>
    </td></tr>
    <tr><td>
    Body:
        </td><td>
        <?= form_textarea(array('name'  => 'body',
                                'id'    => 'body',
                                'rows'  => '25',
                                'cols'  => '60',
                                'value' => isset($body) ? $body : '',
                        ));
        ?>
    </td></tr>
    <tr><td colspan="2" align="center">
    <?php if (!isset($id)) { ?>
        <?= form_submit(array('name'  => 'addnews',
                              'id'    => 'addnews',
                              'value' => 'Add News',
                        )); ?>
    <?php } else { ?>
        <?= form_submit(array('name'  => 'addnews',
                              'id'    => 'addnews',
                              'value' => 'Update News',
                        )); ?>
    <?php } ?>
</tr></table>
<?= form_close(); ?>
	</div>
</div>
<?php include_once('foot.php') ?>
