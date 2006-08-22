VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} VBAShutDown 
   Caption         =   "VBA Shut Down"
   ClientHeight    =   2535
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4710
   OleObjectBlob   =   "Chaptr25.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "VBAShutDown"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
'
' The event handler for the OK button's Click event.
'
Private Sub cmdOK_Click()
    Dim dwReserved As Long
    Dim retval As Long
    '
    ' Exit Windows according to the option selected
    '
    If optShutDown = True Then
        retval = ExitWindowsEx(EWX_SHUTDOWN, dwReserved)
    ElseIf optRestart = True Then
        retval = ExitWindowsEx(EWX_REBOOT, dwReserved)
    Else
        retval = ExitWindowsEx(EWX_LOGOFF, dwReserved)
    End If
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub


