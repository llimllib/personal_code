Attribute VB_Name = "Listing_25_08"
Option Explicit
'
' Listing 25.8. Declares the ExitWindows function as Public
' and then runs the VBAShutDown user form.
'
Public Declare Function ExitWindowsEx Lib "user32" (ByVal uFlags As Long, ByVal dwReserved As Long) As Long

Public Const EWX_LOGOFF = 0
Public Const EWX_SHUTDOWN = 1
Public Const EWX_REBOOT = 2

Sub ShutDownFromExcel()
    VBAShutDown.Show
    Set VBAShutDown = Nothing
End Sub


