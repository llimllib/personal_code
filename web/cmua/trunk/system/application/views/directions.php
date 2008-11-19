
<?php include_once('head.php') ?>

<div id="wrap">
    <div id="side">
        <h3>Events</h3>
        <p>Come out to pickup on Mondays!</p>
    </div>
    <script type="text/javascript">
    maps = new Array();
    maps['banneker'] = '<h3><a name="beanneker">Banneker</a></h3><br />\n'
            + '<iframe width="425" height="350" frameborder="0" scrolling="no" marginheight="0" marginwidth="0" src="http://maps.google.com/maps/ms?ie=UTF8&amp;hl=en&amp;t=h&amp;s=AARTsJpIxwe-8bS8nCHopUckrSTYlFtrfA&amp;msa=0&amp;msid=113945437447713014824.00044f85304f1760894ce&amp;ll=39.283194,-76.739931&amp;spn=0.011626,0.018239&amp;z=15&amp;output=embed"></iframe><br /><small><a href="http://maps.google.com/maps/ms?ie=UTF8&amp;hl=en&amp;t=h&amp;msa=0&amp;msid=113945437447713014824.00044f85304f1760894ce&amp;ll=39.283194,-76.739931&amp;spn=0.011626,0.018239&amp;z=15&amp;source=embed" style="color:#0000FF;text-align:left">View Larger Map</a></small>';
    maps['ccbc'] = '<h3><a name="ccbc">CCBC</a></h3><br />\n'
            + '<iframe width="425" height="350" frameborder="0" scrolling="no" marginheight="0" marginwidth="0" src="http://maps.google.com/maps/ms?ie=UTF8&amp;hl=en&amp;t=h&amp;s=AARTsJrWxR5ad1-7KQywf0NZNp9PGkPAIQ&amp;msa=0&amp;msid=113945437447713014824.00044f8518fbba689979a&amp;ll=39.248546,-76.73447&amp;spn=0.011632,0.018239&amp;z=15&amp;output=embed"></iframe><br /><small><a href="http://maps.google.com/maps/ms?ie=UTF8&amp;hl=en&amp;t=h&amp;msa=0&amp;msid=113945437447713014824.00044f8518fbba689979a&amp;ll=39.248546,-76.73447&amp;spn=0.011632,0.018239&amp;z=15&amp;source=embed" style="color:#0000FF;text-align:left">View Larger Map</a></small>';
    maps['CatHigh'] = '<h3><a name="CatHigh">Catonsville High School</a></h3><br />\n'
            + '<iframe width="425" height="350" frameborder="0" scrolling="no" marginheight="0" marginwidth="0" src="http://maps.google.com/maps/ms?ie=UTF8&amp;hl=en&amp;t=h&amp;s=AARTsJpvE5XfSNcZjg-pTdcw7FklIAKSbA&amp;msa=0&amp;msid=113945437447713014824.00044f8549f7d01a31b7d&amp;ll=39.26047,-76.727314&amp;spn=0.01163,0.018239&amp;z=15&amp;output=embed"></iframe><br /><small><a href="http://maps.google.com/maps/ms?ie=UTF8&amp;hl=en&amp;t=h&amp;msa=0&amp;msid=113945437447713014824.00044f8549f7d01a31b7d&amp;ll=39.26047,-76.727314&amp;spn=0.01163,0.018239&amp;z=15&amp;source=embed" style="color:#0000FF;text-align:left">View Larger Map</a></small>';
    maps['CatPark'] = '<h3><a name="CatPark">Catonsville Community Park</a></h3><br />\n'
            + '<iframe width="425" height="350" frameborder="0" scrolling="no" marginheight="0" marginwidth="0" src="http://maps.google.com/maps/ms?ie=UTF8&amp;hl=en&amp;t=h&amp;s=AARTsJrCs5_Dv91zNY4tREsXB6_NRKzW3w&amp;msa=0&amp;msid=113945437447713014824.00044f8563c817d84c6e2&amp;ll=39.281417,-76.751432&amp;spn=0.005813,0.00912&amp;z=16&amp;output=embed"></iframe><br /><small><a href="http://maps.google.com/maps/ms?ie=UTF8&amp;hl=en&amp;t=h&amp;msa=0&amp;msid=113945437447713014824.00044f8563c817d84c6e2&amp;ll=39.281417,-76.751432&amp;spn=0.005813,0.00912&amp;z=16&amp;source=embed" style="color:#0000FF;text-align:left">View Larger Map</a></small>';

    function showmap(loc) {
        $("#map").html(maps[loc]);
    }
    </script>

    <div id="content">
        <?php
            //TODO: make there be one "map" div, and dynamically load
            //      the iframe into it so that the browser doesn't suck up
            //      massive bandwidth as soon as you get onto this page ?>
        <table><tr><td valign="top" width="115px">
        Fields:<br><ul style="margin-left: -25px;">
        <li><a href="#banneker" onclick="showmap('banneker')">Banneker</a> 
        <li><a href="#ccbc" onclick="showmap('ccbc');">CCBC</a>
        <li><a href="#CatHigh" onclick="showmap('CatHigh');">Catonsville High School</a> 
        <li><a href="#CatPark" onclick="showmap('CatPark');">Catonsville Community Park</a>
        </ul>
        </td><td>
        <div class="map" id="map" style="margin-left: 30px;"> </div>
        </td></tr></table>
    </div>
</div>

<?php include_once('foot.php') ?>
