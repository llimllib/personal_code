Attribute VB_Name = "Listing_25_04"
Option Explicit
'
' Listing 25.4. A procedure that uses several win32 API functions
' to return the pathnames of various Windows folders.
'
Declare Function GetWindowsDirectory Lib "kernel32" Alias "GetWindowsDirectoryA" (ByVal lpBuffer As String, ByVal nSize As Long) As Long
Declare Function GetSystemDirectory Lib "kernel32" Alias "GetSystemDirectoryA" (ByVal lpBuffer As String, ByVal nSize As Long) As Long
Declare Function GetTempPath Lib "kernel32" Alias "GetTempPathA" (ByVal nBufferLength As Long, ByVal lpBuffer As String) As Long
Declare Function SetCurrentDirectory Lib "kernel32" Alias "SetCurrentDirectoryA" (ByVal lpPathName As String) As Long
Declare Function GetCurrentDirectory Lib "kernel32" Alias "GetCurrentDirectoryA" (ByVal nBufferLength As Long, ByVal lpBuffer As String) As Long

Sub WinDirInfo()
    Dim strBuffer As String * 256
    Dim retval As Long
    '
    ' Get the main Windows folder
    '
    retval = GetWindowsDirectory(strBuffer, Len(strBuffer))
    If retval <> 0 Then
        Debug.Print "Main Windows folder: "; strBuffer
    End If
    '
    ' Get the Windows system folder
    '
    strBuffer = ""
    retval = GetSystemDirectory(strBuffer, Len(strBuffer))
    If retval <> 0 Then
        Debug.Print "Windows system folder: "; strBuffer
    End If
    '
    ' Get the folder for temporary files
    '
    strBuffer = ""
    retval = GetTempPath(Len(strBuffer), strBuffer)
    If retval <> 0 Then
        Debug.Print "Windows temporary folder: "; strBuffer
    End If
    '
    ' Get the current folder
    '
    strBuffer = ""
    retval = GetCurrentDirectory(Len(strBuffer), strBuffer)
    If retval <> 0 Then
        Debug.Print "Windows current folder: "; strBuffer
    End If

End Sub
