Set WshShell = WScript.CreateObject("WScript.Shell")
Set WshShortcut = WshShell.CreateShortcut("C:\Windows\Desktop\Edit CONFIG.SYS.lnk")
With WshShortcut
    .TargetPath = "C:\Windows\Notepad.exe"
    .Arguments = "C:\Config.sys"
    .WorkingDirectory = "C:\"
    .Description = "Opens CONFIG.SYS in Notepad"
    .Hotkey = "Ctrl+Alt+7"
    .IconLocation = "C:\Windows\Shell\Shell32.dll,21"
    .WindowStyle = 3
    .Save
End With

