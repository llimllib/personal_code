Attribute VB_Name = "Chapter23"
Option Explicit
'
' Listing 23.1. The GetNumbers procedure prompts the user for a dividend and a divisor.
'
Sub GetNumbers()
    Dim done As Boolean
    Dim divisor As Variant
    Dim dividend As Variant
    '
    ' Prompt user for dividend and divisor.
    '
    done = False
    Do While Not done
        dividend = InputBox("Enter the dividend:", "Divider")
        divisor = InputBox("Enter the divisor:", "Divider")
        done = Divide(dividend, divisor)
    Loop
End Sub
'
' Listing 23.2. The Divide function divides the dividend by the divisor.
' The function traps "division by zero" errors.
'
Function Divide(dividend, divisor) As Boolean
    Dim msg As String
    Dim result As Single
    '
    ' Set the trap
    '
    On Error GoTo DivByZeroHandler
    '
    ' Peform the division
    '
    result = dividend / divisor
    '
    ' If it went okay, display the result
    '
    msg = dividend & _
          " divided by " & _
          divisor & _
          " equals " & _
          result
    MsgBox msg
    '
    ' Set the return value and bypass the error handler
    '
    Divide = True
    Exit Function
    '
    ' Code branches here if an error occurs
    '
DivByZeroHandler:
    '
    ' Display the error message
    '
    result = MsgBox("You entered 0 as the divisor! Try again?", _
                    vbYesNo + vbQuestion, _
                    "Divider")
    '
    ' Return the user's choice
    '
    If result = vbYes Then
        Divide = False
    Else
        Divide = True
    End If
End Function
'
' Listing 23.3 Backs up the active workbook to a drive specified by
' the user. Traps any errors (such as having no disk in the drive).
'
Sub BackUpToFloppy()
    Dim backupDrive As String
    Dim backupName As String
    Dim msg As String
    Dim done As Boolean
    Dim result As Integer
    '
    ' Define the location of the error handler
    '
    On Error GoTo ErrorHandler
    '
    ' Initialize some variables and then loop
    '
    Application.DisplayAlerts = False
    done = False
    backupDrive = "A:"
    While Not done
        '
        ' Get the drive to use for the backup
        '
        backupDrive = InputBox( _
            Prompt:="Enter the drive letter for the backup:", _
            Title:="Backup", _
            Default:=backupDrive)
        '
        ' Check to see if OK was selected
        '
        If backupDrive <> "" Then
            '
            ' Make sure the backup drive contains a colon (:)
            '
            If InStr(backupDrive, ":") = 0 Then
                backupDrive = Left(backupDrive, 1) & ":"
            End If
            '
            ' First, save the file
            '
            ActiveWorkbook.Save
            '
            ' Assume the backup will be successful,
            ' so set done to True to exit the loop
            '
            done = True
            '
            ' Concatenate drive letter and workbook name
            '
            backupName = backupDrive & ActiveWorkbook.Name
            '
            ' Make a copy on the specified drive
            '
            ActiveWorkbook.SaveCopyAs FileName:=backupName
        Else
            Exit Sub
        End If
    Wend
    '
    ' Bypass the error handler
    '
    Exit Sub
    '
    ' Code branches here if an error occurs
    '
ErrorHandler:
    msg = "An error has occurred!" & Chr(13) & Chr(13) & _
          "Select Abort to bail out, Retry to re-enter the drive" & Chr(13) & _
          "letter, or Ignore to attempt the backup again."
    result = MsgBox(msg, vbExclamation + vbAbortRetryIgnore)
    Select Case result
        Case vbAbort
            done = True
        Case vbRetry
            done = False
            Resume Next
        Case vbIgnore
            Resume
    End Select
End Sub
'
' Listing 23.4. This procedure divides two numbers. It traps three specific
' errors: division by zero, overflow, and type mismatch.
'
Sub DivideNumbers()
    Dim msg As String
    Dim result As Single
    Dim divisor As Variant
    Dim dividend As Variant
    '
    ' Set the trap
    '
    On Error GoTo DivByZeroHandler
    '
    ' Prompt user for the dividend
    '
GetDividendAndDivisor:
    dividend = InputBox("Enter the dividend:", "Divider")
    If dividend = "" Then Exit Sub
    '
    ' Prompt user for the divisor
    '
GetDivisorOnly:
    divisor = InputBox("Enter the divisor:", "Divider")
    If divisor = "" Then Exit Sub
    '
    ' Peform the division
    '
    result = dividend / divisor
    '
    ' If it went okay, display the result
    '
    msg = dividend & _
          " divided by " & _
          divisor & _
          " equals " & _
          result
    MsgBox msg
    '
    ' Bypass the error handler
    '
    Exit Sub
    '
    ' Code branches here if an error occurs
    '
DivByZeroHandler:
    '
    ' Display the error message
    '
    msg = "An error occurred!" & Chr(13) & Chr(13) & _
          "Error number:  " & Err.Number & Chr(13) & _
          "Error message: " & Err.Description
    MsgBox msg, vbOKOnly + vbCritical
    '
    ' Check the error number
    '
    Select Case Err.Number
        '
        ' Division by zero
        '
        Case 11
            Resume GetDivisorOnly
        '
        ' Overflow
        '
        Case 6
            Resume GetDividendAndDivisor
        '
        ' Type mismatch
        '
        Case 13
            If Not IsNumeric(dividend) Then
                Resume GetDividendAndDivisor
            Else
                Resume GetDivisorOnly
            End If
        '
        ' Anything else, just quit
        '
        Case Else
            Exit Sub
    End Select
End Sub
