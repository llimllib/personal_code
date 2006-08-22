Set WshShell = WScript.CreateObject("WScript.Shell")
'
' Run through the environment variables
'
str = ""
For Each e In WshShell.Environment("Process")
    str = str & e & Chr(13)
Next
WScript.Echo str
