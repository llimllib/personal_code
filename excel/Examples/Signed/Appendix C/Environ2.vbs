Set WshShell = WScript.CreateObject("WScript.Shell")
'
' Run through the environment variables
'
For Each e In WshShell.Environment("Process")
    '
    ' Look for the PATH variable
    '
    If InStr(e, "PATH") Then
        '
        ' Grab everything after the "="
        ' and then display the PATH
        '
        strPath = Right(e, Len(e) - InStr(e, "="))
        WScript.Echo strPath
        Exit For
    End If
Next
