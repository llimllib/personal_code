Attribute VB_Name = "Listing_26_2"
Option Explicit
'
' Listing 26.2. A revised version of GetWinVer that uses
' conditional compilation to differentiate between a Win32 call
' and a Win16 call.
'
#If Win32 Then
    Declare Function GetVersionEx Lib "kernel32" Alias "GetVersionExA" (lpVersionInformation As OSVERSIONINFO) As Long
    Type OSVERSIONINFO
        dwOSVersionInfoSize As Long
        dwMajorVersion As Long
        dwMinorVersion As Long
        dwBuildNumber As Long
        dwPlatformId As Long
        szCSDVersion As String * 128
    End Type
    Public Const VER_PLATFORM_WIN32s = 0
    Public Const VER_PLATFORM_WIN32_WINDOWS = 1
    Public Const VER_PLATFORM_WIN32_NT = 2
#Else
    Declare Function GetVersion Lib "kernel" () As Long
#End If

Sub GetWinVer2()
    Dim os As OSVERSIONINFO
    Dim msg As String
    
    #If Win32 Then
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
    #Else
        msg = "Windows Version: " & GetVersion
    #End If
    MsgBox msg, vbOKOnly + vbInformation, "Windows Version"
End Sub


