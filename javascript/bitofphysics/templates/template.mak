<html>
 <head>
  <style type="text/css">
body, html {margin:0;
            padding:0;
            background:white;
            color:#000;
}
body { min-width: 750px; 
       font-size: 62.5%;}
#wrap { margin:0 auto; width:100%; }
#canvascontainer { margin-left: 5px;
                   margin-top: 5px;
                   width: 320px;
                   float: left;
                   position: fixed;
                   font-size: 1.8em;
}
#explain { font-size: 1.4em; }
<%
    codebox_height = 26 + code.count("\n") * 15
    codebox_width = 700
    librarybox_height = 26 + library.count("\n") * 15
%>
#codebox {border: 1px solid LightGray;
          width:${codebox_width+4}px;
          height:${codebox_height}px;
          margin-top: 10px;
}
/*#libraryContainer {border: 1px solid LightGray;
          width:${codebox_width+4}px;
          height:${codebox_height}px;
          margin-top: 10px;
}*/
#textcontainer {font: 1.4em;
    margin-left: 320px;
    margin-right: 10px;
    margin-top:   10px;
}

#footer {clear:both;}

#canvas { border: 1px solid DarkGray; }
#libraryBox { border: 1px solid DarkGray; }
a:link { color: #333; }
a:visited { color: #999; }
h1 { font: Strong 18px Cambria, Georgia, Times New Roman, Calibri, serif;
     margin-left: 10px;
}
#nextLink { float:right;
    margin-right:30px
}

    </style>
    <script type="text/javascript" src="jquery-1.2.6.js"></script>
    <script type="text/javascript" src="jquery-ui-1.6rc2.js"></script>
    <link rel="stylesheet" href="theme/ui-theme.css" type="text/css" media="screen">
    <script type="text/javascript" src="codemirror/codemirror.js"></script>
    <title>Canvas Tutorial - ${title}</title>
<script type="text/javascript">
var editor = undefined;
var libEditor = undefined;
var intervalID = undefined;

function runCode() {
    if (intervalID != undefined)
        clearInterval(intervalID);
    //TODO: handle exceptions somehow
    $("#canvas")[0].getContext("2d").clearRect(0,0,
      $("#canvas")[0].width,
      $("#canvas")[0].height);

    //if there's a library defined, eval it
    if (libEditor != undefined) eval(libEditor.getCode());

    % if hidden_code:
        intervalID = eval($("#hidden_code").val());
    % else:
        intervalID = eval(editor.getCode());
    % endif
}

$(document).ready(function(){
    % if code:
        editor = CodeMirror.fromTextArea("code", {
          parserfile: ["tokenizejavascript.js", "parsejavascript.js"],
          path: "codemirror/",
          stylesheet: "codemirror/jscolors.css",
          width: "${codebox_width}px",
          height: "${codebox_height}px",
        });
    % endif
    
    % if library:
        libEditor = CodeMirror.fromTextArea("library", {
          parserfile: ["tokenizejavascript.js", "parsejavascript.js"],
          path: "codemirror/",
          stylesheet: "codemirror/jscolors.css",
          width: "${codebox_width}px",
          height: "${librarybox_height}px",
        });
    % endif

    $("#textcontainer > ul").tabs();
    % if not library:
        $("#textcontainer > ul").tabs("remove", 1);
    % endif

	$("#runButton").click(runCode).removeAttr("disabled");
});
  </script>
 </head>
 <body>
 % if hidden_code:
     <textarea id="hidden_code" style="display:none">${hidden_code}</textarea>
 % endif
 <div id="wrap">
   <!-- <div id="header">
   <h2>Breakout Tutorial</h2>
    </div>-->

    <div id="canvascontainer">
         <canvas id="canvas" width="300" height="300"></canvas>
         <div style="text-align:center">
             <input type="submit" value="run code" id="runButton" disabled/>
         </div>

         <ol id="toc">
             % for pagetitle, link in toc:
                 <li><a href="${link}">${pagetitle}</a></li>
             % endfor
         </ol>
    </div>

    <div id="textcontainer">
        <h1>${title}</h1>
        <ul><li class="ui-tabs-nav-item"><a href="#explain"><span>Code</span></a></li>
            <li class="ui-tabs-nav-item"><a href="#libraryContainer"><span>Library</span></a></li>
            <li class="ui-tabs-nav-item"><a href="#comments"><span>Comments</span></a></li>
        </ul>
        <div id="explain">${explain_before}
            % if code:
                 <div id="codebox">
                 <textarea id="code" rows=${code.count("\n")+1} cols=100>${code}</textarea>
                 </div>
                 <p>${explain_after}
            % endif
            <p>
            % if next:
                <a href="${next}.html" id="nextLink">next</a>
            % endif
            % if prev:
                <a href="${prev}.html" id="prevLink">prev</a><br>
            % endif
        </div>
     
        <div id="libraryContainer">
            % if library:
                <div id="libraryBox">
                    <textarea id="library" rows=${library.count("\n")+1}
                              cols=100>${library}</textarea>
                </div>
            % endif
        </div>

        <div id="comments">
<!-- begin disqus block -->
<div id="disqus_thread"></div><script type="text/javascript" src="http://disqus.com/forums/canvastutorial/embed.js"></script><noscript><a href="http://canvastutorial.disqus.com/?url=ref">View the discussion thread.</a></noscript><a href="http://disqus.com" class="dsq-brlink">blog comments powered by <span class="logo-disqus">Disqus</span></a>
<!-- end disqus block -->
        
        </div>
    </div>

<!-- begin disqus block -->
<script type="text/javascript">
//<![CDATA[
(function() {
        var links = document.getElementsByTagName('a');
        var query = '?';
        for(var i = 0; i < links.length; i++) {
            if(links[i].href.indexOf('#disqus_thread') >= 0) {
                query += 'url' + i + '=' + encodeURIComponent(links[i].href) + '&';
            }
        }
        document.write('<script type="text/javascript" src="http://disqus.com/forums/canvastutorial/get_num_replies.js' + query + '"></' + 'script>');
    })();
<!-- end disqus block -->
//]]>
</script>

 </body>
</html>
