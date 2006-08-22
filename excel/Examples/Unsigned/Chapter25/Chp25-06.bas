Attribute VB_Name = "Listing_25_06"
Option Explicit
'
' Listing 25.6. A procedure that determines the drive type.
'
Declare Function GetDriveType Lib "kernel32" Alias "GetDriveTypeA" (ByVal ndrive As String) As Long

Public Const DRIVE_REMOVABLE = 2
Public Const DRIVE_FIXED = 3
Public Const DRIVE_REMOTE = 4
Public Const DRIVE_CDROM = 5
Public Const DRIVE_RAMDISK = 6

Sub DetermineDriveType()
    Dim ndrive As String
    Dim done As Boolean
    Dim retval As Long
    done = False
    '
    ' Loop until Cancel is clicked
    '
    While Not done
        '
        ' Get the drive root
        '
        ndrive = InputBox("Enter the root address of the drive: (e.g., A:\)")
        If ndrive <> "" Then
            '
            ' Get the drive type
            '
            retval = GetDriveType(ndrive)
            '
            ' Translate the return value
            '
            Select Case retval
                Case 0
                    MsgBox "Can't determine the drive type!"
                Case 1
                    MsgBox "Root directory does not exist!"
                Case DRIVE_REMOVABLE
                    MsgBox Left(ndrive, 2) & " is a removable drive."
                Case DRIVE_FIXED
                    MsgBox Left(ndrive, 2) & " is a fixed drive."
                Case DRIVE_REMOTE
                    MsgBox Left(ndrive, 2) & " is a network drive."
                Case DRIVE_CDROM
                    MsgBox Left(ndrive, 2) & " is a CD-ROM drive."
                Case DRIVE_RAMDISK
                    MsgBox Left(ndrive, 2) & " is a RAM disk."
            End Select
        Else
            done = True
        End If
    Wend
End Sub

