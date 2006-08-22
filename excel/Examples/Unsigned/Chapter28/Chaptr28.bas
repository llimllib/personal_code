Attribute VB_Name = "Backup"
Option Explicit
'
' API Declares
'
Declare Function GetDriveType Lib "kernel32" Alias "GetDriveTypeA" (ByVal nDrive As String) As Long
Declare Function GetDiskFreeSpace Lib "kernel32" Alias "GetDiskFreeSpaceA" (ByVal lpRootPathName As String, lpSectorsPerCluster As Long, lpBytesPerSector As Long, lpNumberOfFreeClusters As Long, lpTotalNumberOfClusters As Long) As Long
'
' Constants used with GetDriveType result
'
Public Const DRIVE_REMOVABLE = 2
Public Const DRIVE_FIXED = 3
Public Const DRIVE_REMOTE = 4
Public Const DRIVE_CDROM = 5
Public Const DRIVE_RAMDISK = 6
'
' This Type is used to hold properties of the open documents
'
Type BackupDoc
    Name As String
    Path As String
    State As String
    Size As Long
    Selected As Boolean
End Type
'
' Use this procedure to display the Backup form
'
Sub ShowBackup()
    '
    ' Ignore the error that occurs if this procedure is
    ' executed while the form is already displayed.
    '
    On Error Resume Next
    frmBackup.Show
    Set frmBackup = Nothing
End Sub

