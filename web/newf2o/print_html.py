def print_header(title, img_src):
    return """<html><head><title>Bill Mill - %s</title>
<link href="style.css" rel="stylesheet" type="text/css">
</head>
<body>
<div id="main">
    <div id="img">
        <img src="%s">
    </div>
    <div id="floatleft">
        <a href="code.html">code</a><br>
        <a href="words.html">words</a><br>
        <a href="blog">blog</a><br>
        <a href="photos.html">photos</a><p>
        <a href="http://del.icio.us/llimllib">bookmarks</a><p>
        hosting by:<br><a href="http://f2o.org">f2o</a><p>
    </div>
    <div id="bodytext">
"""  % (title, img_src)

def print_footer():
    return """
    </div>
        <div id="footer">
        Everything on this site is entirely copyright free unless otherwise
        noted.<br>Distribute it as
        you wish, but <a href="mailto:llimllib@f2o.org">let me know</a> if you
        like it.
        </div>
</div>
</body></html>
"""

def print_image(img):
    return """    <div id="gallery_thumb">
        <img src=%s>
    </div>
"""

def new_image_row(): return "<p>\n"

def insert_pics(dir):
    pass
    #TODO: do somethin
