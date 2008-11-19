<?php include_once('head.php') ?>

<div id="wrap">
    <div id="side">
        <h3>Events</h3>
        <p>Come out to pickup on Mondays!</p>
    </div>
    <div id="content">
<?php if (isset($GROUP_Admin)): ?>
    <p><a href="/news/add_">Add News</a>
<?php endif; ?>
        <?php foreach($news as $item): ?>
            <h2><?php echo  $item->title; ?></h2>
                <?php echo  $item->body; ?>
            <?php if (isset($GROUP_Admin)): ?>
                <a href="/news/del/<?php echo  $item->id; ?>">delete</a>
                <a href="/news/edit/<?php echo $item->id; ?>">edit</a>
            <?php endif; ?>

        <?php endforeach; ?>
    </div>
</div>

<?php include_once('foot.php') ?>
