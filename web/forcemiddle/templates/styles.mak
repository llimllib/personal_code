body {
    /* for centering */
        text-align: center;
        /*min-width: 800px;*/
    /* /for */
}

#logo {
    /* for centering */
        margin:0 auto;
        text-align: left;
    /* /for */
    position:relative;
    top:0;
    left:40;
    width:800;
}

#singledisc {
    /* for centering */
        margin:0 auto;
        text-align: left;
    /* /for */
    width:800;
}
#bigdisc {
    float:right;
    margin-left: 15px;
}
#disctext {
    font-size:20px;
    margin-top: 40px;
}
#abouttext {
	margin-top: 20px;
}
#footer {
    position:relative;
    padding: 60px;
    clear:right;
    text-align:center;
}
a:link, a:hover, a:active, a:visited {color:black;}

#frisbees {
    /* for centering */
        margin:0 auto;
        text-align: left;
    /* /for */
    position:relative;
    top:-70;
    left:10;
    width:800;
}

#frisbees h2 {
    position:absolute; top:150; left:350; text-align:justify; width:200;
}

#addtocart {
	position:relative;
	top: 0px;
	left: 100px;
}

% for discname, (top, left, width) in discs.iteritems():
img#${discname} {
	position: absolute; top:${top}; left:${left}; width:${width};
}
% endfor
