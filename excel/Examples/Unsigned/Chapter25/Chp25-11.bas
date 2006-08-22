Attribute VB_Name = "Listing_25_11"
Option Explicit
'
' Listing 25.11. Using Win32 API functions to change the default beep in the Registry.
'
Declare Function RegOpenKeyEx Lib "advapi32.dll" Alias "RegOpenKeyExA" (ByVal hKey As Long, ByVal lpSubKey As String, ByVal ulOptions As Long, ByVal samDesired As Long, phkResult As Long) As Long
Declare Function RegCloseKey Lib "advapi32.dll" (ByVal hKey As Long) As Long
Declare Function RegSetValueEx Lib "advapi32.dll" Alias "RegSetValueExA" (ByVal hKey As Long, ByVal lpValueName As String, ByVal Reserved As Long, ByVal dwType As Long, ByVal lpData As Any, ByVal cbData As Long) As Long
Declare Function GetWindowsDirectory Lib "kernel32" Alias "GetWindowsDirectoryA" (ByVal lpBuffer As String, ByVal nSize As Long) As Long
Declare Function MessageBeep Lib "user32" (ByVal wType As Long) As Long
'
' You may not need all these variables!
'
Public Const DELETE = &H10000
Public Const READ_CONTROL = &H20000
Public Const WRITE_DAC = &H40000
Public Const WRITE_OWNER = &H80000
Public Const SYNCHRONIZE = &H100000
Public Const STANDARD_RIGHTS_READ = (READ_CONTROL)
Public Const STANDARD_RIGHTS_WRITE = (READ_CONTROL)
Public Const STANDARD_RIGHTS_EXECUTE = (READ_CONTROL)
Public Const STANDARD_RIGHTS_REQUIRED = &HF0000
Public Const STANDARD_RIGHTS_ALL = &H1F0000
Public Const KEY_QUERY_VALUE = &H1
Public Const KEY_SET_VALUE = &H2
Public Const KEY_CREATE_SUB_KEY = &H4
Public Const KEY_ENUMERATE_SUB_KEYS = &H8
Public Const KEY_NOTIFY = &H10
Public Const KEY_CREATE_LINK = &H20
Public Const KEY_READ = ((STANDARD_RIGHTS_READ Or KEY_QUERY_VALUE Or KEY_ENUMERATE_SUB_KEYS Or KEY_NOTIFY) And (Not SYNCHRONIZE))
Public Const KEY_WRITE = ((STANDARD_RIGHTS_WRITE Or KEY_SET_VALUE Or KEY_CREATE_SUB_KEY) And (Not SYNCHRONIZE))
Public Const KEY_EXECUTE = (KEY_READ)
Public Const KEY_ALL_ACCESS = ((STANDARD_RIGHTS_ALL Or KEY_QUERY_VALUE Or KEY_SET_VALUE Or KEY_CREATE_SUB_KEY Or KEY_ENUMERATE_SUB_KEYS Or KEY_NOTIFY Or KEY_CREATE_LINK) And (Not SYNCHRONIZE))
Public Const ERROR_SUCCESS = 0&
Public Const HKEY_CLASSES_ROOT = &H80000000
Public Const HKEY_CURRENT_USER = &H80000001
Public Const HKEY_LOCAL_MACHINE = &H80000002
Public Const HKEY_USERS = &H80000003
Public Const REG_SZ = 1
Public Const REG_BINARY = 3
Public Const REG_DWORD = 4

Sub ChangeDefaultSound()
    Dim strSubKey As String
    Dim strSetting As String
    Dim strData As String
    Dim strWinDir As String * 256
    Dim lngType As Long
    Dim lngDataLen As Long
    Dim hResult As Long
    Dim retval As Long
    '
    ' Specify the subkey
    '
    strSubKey = "AppEvents\Schemes\Apps\.Default\.Default\.Current"
    '
    ' Open the subkey
    '
    retval = RegOpenKeyEx(HKEY_CURRENT_USER, strSubKey, 0&, KEY_SET_VALUE, hResult)
    '
    ' Did it work?
    '
    If retval = ERROR_SUCCESS Then
        '
        ' Get the main Windows folder (assume it works)
        '
        retval = GetWindowsDirectory(strWinDir, Len(strWinDir))
        '
        ' We're changing a default setting, so send the null string
        '
        strSetting = ""
        '
        ' Store the new value in the buffer
        '
        strData = Left$(strWinDir, retval) & "\Media\The Microsoft Sound.wav"
        lngDataLen = Len(strData)
        '
        ' Make sure we've got a good handle
        '
        If hResult <> 0 Then
            '
            ' Set the value
            '
            retval = RegSetValueEx(hResult, strSetting, 0&, REG_SZ, strData, lngDataLen)
            '
            ' If everything's okay, play the new sound
            '
            If retval = ERROR_SUCCESS Then
                MessageBeep 0
            End If
        End If
        '
        ' Close the key
        '
        retval = RegCloseKey(hResult)
    Else
        MsgBox "Error opening key: " & retval
    End If
End Sub

