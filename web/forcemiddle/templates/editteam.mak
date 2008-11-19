<html><head><title>Edit a team</title></head>
<body>

%if hasattr(form, "errors") and len(form.errors) > 0:
    Errors:<br>
    %for error in form.errors:
        <font color="red">${error}</font><br>
    %endfor
    <br>
%endif

All fields are required:<br>

<form method=${form.method} name=${form.name} action=${form.action}>

<table>
%for field in form.fields:
    %if field._type == "hidden":
        ${field}
    %elif field._type == "submit":
        <tr><td colspan="2" align="left">${field}</td></tr>
    %else:
        <tr><td>${field.label}</td><td>${field}</td></tr>
    %endif
%endfor
</table>

</form>
</body>
</html>
