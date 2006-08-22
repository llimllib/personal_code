Attribute VB_Name = "Listing_25_10"
Option Explicit
'
' Listing 25.10. Using Win32 API functions to read the Windows 95 sub version from the Registry.
'
Declare Function RegOpenKeyEx Lib "advapi32.dll" Alias "RegOpenKeyExA" (ByVal hKey As Long, ByVal lpSubKey As String, ByVal ulOptions As Long, ByVal samDesired As Long, phkResult As Long) As Long
Declare Function RegQueryValueEx Lib "advapi32.dll" Alias "RegQueryValueExA" (ByVal hKey As Long, ByVal lpValueName As String, ByVal lpReserved As Long, lpType As Long, ByVal lpData As Any, lpcbData As Long) As Long
Declare Function RegCloseKey Lib "advapi32.dll" (ByVal hKey As Long) As Long
Declare Function RegSetValueEx Lib "advapi32.dll" Alias "RegSetValueExA" (ByVal hKey As Long, ByVal lpValueName As String, ByVal Reserved As Long, ByVal dwType As Long, ByVal lpData As Any, ByVal cbData As Long) As Long
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

Sub GetWin95SubVersion()
    Dim strSubKey As String
    Dim strSetting As String
    Dim strData As String * 256
    Dim lngType As Long
    Dim lngDataLen As Long
    Dim hResult As Long
    Dim retval As Long
    '
    ' Specify the subkey
    '
    strSubKey = "SOFTWARE\Microsoft\Windows\CurrentVersion"
    '
    ' Open the subkey
    '
    retval = RegOpenKeyEx(HKEY_LOCAL_MACHINE, strSubKey, 0&, KEY_QUERY_VALUE, hResult)
    '
    ' Did it work?
    '
    If retval = ERROR_SUCCESS Then
        '
        ' Initialize the buffer length variable
        '
        lngDataLen = Len(strData)
        '
        ' Make sure we've got a good handle
        '
        If hResult <> 0 Then
            '
            ' First see if this is a Windows 95 machine
            '
            strSetting = "Version"
            '
            ' Get the setting's value
            '
            retval = RegQueryValueEx(hResult, strSetting, 0&, lngType, strData, lngDataLen)
            '
            ' Everything okay?
            '
            If retval = ERROR_SUCCESS Then
                If Left(strData, 10) = "Windows 98" Then
                    MsgBox "You're running Windows 98."
                Else
                    '
                    ' Specify the setting name
                    '
                    strSetting = "SubVersionNumber"
                    '
                    ' Get the setting's value
                    '
                    retval = RegQueryValueEx(hResult, strSetting, 0&, lngType, strData, lngDataLen)
                    '
                    ' Everything okay?
                    '
                    If retval = ERROR_SUCCESS Then
                        '
                        ' Test the returned string
                        '
                        If InStr(UCase$(strData), "B") <> 0 Then
                            MsgBox "You're running Windows 95 OSR2."
                        Else
                            MsgBox "You're running regular Windows 95."
                        End If
                    End If
                End If
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


