Attribute VB_Name = "Listing_25_01"
' Listing 25.1. In this example, the ShowWindow API function
' accepts several constants.
'
Option Explicit
'
' Declare the Win32 API functions
'
Declare Function FindWindow Lib "user32" Alias "FindWindowA" (ByVal lpClassName As String, ByVal lpWindowName As String) As Long
Declare Function ShowWindow Lib "user32" (ByVal hwnd As Long, ByVal nCmdShow As Long) As Long
'
' Set up the constants
'
Public Const SW_NORMAL = 1
Public Const SW_MAXIMIZE = 3
Public Const SW_MINIMIZE = 6
Public Const SW_RESTORE = 9

Sub WindowTest()
    Dim hInst As Long
    '
    ' Look for the window of the current application
    '
    hInst = FindWindow(vbNullString, Application.Caption)
    If hInst <> 0 Then
        '
        ' If we found it, minimize it
        '
        ShowWindow hInst, SW_MINIMIZE
    End If
End Sub
