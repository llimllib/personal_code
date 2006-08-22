Attribute VB_Name = "Listing_25_02"
' Listing 25.2. This example uses the GetVersionEx API function, which accepts an
' argument that uses a custom type definition.
'
Option Explicit
'
' The type definition
'
Type OSVERSIONINFO
        dwOSVersionInfoSize As Long
        dwMajorVersion As Long
        dwMinorVersion As Long
        dwBuildNumber As Long
        dwPlatformId As Long
        szCSDVersion As String * 128
End Type
'
' Declare the function
'
Declare Function GetVersionEx Lib "kernel32" Alias "GetVersionExA" (lpVersionInformation As OSVERSIONINFO) As Long
'
' Constants used with OSVERSIONINFO.dwPlatformId
'
Public Const VER_PLATFORM_WIN32s = 0
Public Const VER_PLATFORM_WIN32_WINDOWS = 1
Public Const VER_PLATFORM_WIN32_NT = 2

Sub GetWinVer()
    Dim os As OSVERSIONINFO
    Dim msg As String
    
    os.dwOSVersionInfoSize = Len(os)
    GetVersionEx os
    
    msg = "Major Version: " & os.dwMajorVersion & Chr(13) & _
          "Minor Version: " & os.dwMinorVersion & Chr(13) & _
          "Build Number: " & os.dwBuildNumber & Chr(13) & _
          "Platform ID: "
    Select Case os.dwPlatformId
        Case VER_PLATFORM_WIN32s
            msg = msg & "Win32s"
        Case VER_PLATFORM_WIN32_WINDOWS
            msg = msg & "Windows"
        Case VER_PLATFORM_WIN32_NT
            msg = msg & "Windows NT"
    End Select
    MsgBox msg, vbOKOnly + vbInformation, "Windows Version"
End Sub

