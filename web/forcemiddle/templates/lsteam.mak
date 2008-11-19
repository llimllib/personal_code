<html><head><title>Admin Page</title></head>
<body>
<a href="/">home</a>
<table border=0 cellpadding=4>
%for team in teams:
	<tr><td>
	<a href="/admin/editteam/${team.name}">${team.name}</a>
		</td><td><a href="deleteteam/${team.name}">delete</a>
	</td></tr>
%endfor
</table>
</body></html>
