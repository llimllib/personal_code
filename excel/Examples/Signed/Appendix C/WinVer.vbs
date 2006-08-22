Set WshShell = WScript.CreateObject("WScript.Shell")
winVer = WshShell.RegRead("HKLM\Software\Microsoft\Windows\CurrentVersion\VersionNumber")
WScript.Echo winVer

