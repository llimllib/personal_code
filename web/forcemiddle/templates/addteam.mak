<html><head><title>Add a team</title></head>
<body>

%if hasattr(form, "errors") and len(form.errors) > 0:
	Errors:<br>
	%for error in form.errors:
		<font color="red">${error}</font><br>
	%endfor
	<br>
%endif

<form method="${form.method}" name="${form.name}" action="${form.action}">

<table>
%for field in form.fields:
	%if field._type == "submit":
		<tr><td colspan="2" align="center">${field}</td></tr>
	%else:
		<tr><td>${field.label}</td><td>${field}</td></tr>
	%endif
%endfor
</table>

</form>
