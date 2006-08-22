Attribute VB_Name = "Chapter17"
Option Explicit

' Listing 17.5. The AddCustomer procedure runs when you select
' the Add Customer command or click the Add Customer button.
'
Sub AddCustomer()
    '
    ' Set up the form and then Show it
    '
    With frmCustomer
        .Caption = "Add Customer"   ' Set form title
        .Controls("cmdAction").Caption = "Add"      ' Make sure first button is Add
        .Controls("cmdCancel").Caption = "Cancel"   ' Start second button as Cancel
        .Show
    End With
    Set frmCustomer = Nothing
End Sub

' Listing 17.9. The EditCustomer procedure runs when you select
' the Edit Customer command or click the Edit Customer button.
'
Sub EditCustomer()
    '
    ' Make sure selection is inside database
    '
    If Not InsideDatabase(ActiveCell.Row) Then
        Exit Sub
    End If
    '
    ' Set up the form and then Show it
    '
    With frmCustomer
        .Caption = "Edit Customer"
        .Controls("cmdAction").Caption = "OK"
        .Show
    End With
    Set frmCustomer = Nothing
End Sub

' Listing 17.10. This function that determines whether
' or not the active cell is inside the Database range.
'
Function InsideDatabase(currRow As Integer)

    With Range("Database")
        If .Rows.Count = 1 Then
            MsgBox Prompt:="There are no records in the database.", _
                   Title:="Customer Database", _
                   Buttons:=vbExclamation
            InsideDatabase = False
            Exit Function
        End If
        If currRow <= .Row Or currRow >= (.Row + .Rows.Count) Then
            MsgBox Prompt:="You must select a record inside the database.", _
                   Title:="Customer Database", _
                   Buttons:=vbExclamation
            InsideDatabase = False
        Else
            InsideDatabase = True
        End If
    End With

End Function

' Listing 17.11. The FilterCustomers procedure runs when you
' select the Filter Customers command or click the Filter
' Customers button.
'
Sub FilterCustomers()
    Dim criteriaCells As Range
    Dim c As Range
    Dim criteriaEmpty As Boolean
    '
    ' Make sure the Criteria range contains a value
    '
    criteriaEmpty = True
    Set criteriaCells = Range("Criteria").Offset(1).Resize(RowSize:=1)
    For Each c In criteriaCells
        If c.Value <> "" Then criteriaEmpty = False
    Next 'c
    If criteriaEmpty Then
        MsgBox "The Criteria range is empty!" & Chr(13) & _
               "Please enter criteria before filtering the database."
        Exit Sub
    End If
    '
    ' Filter the database according the the Criteria range values
    '
    Range("Database").AdvancedFilter _
        Action:=xlFilterInPlace, _
        CriteriaRange:=Range("Criteria")
End Sub

' Listing 17.12. The ShowAllCustomers procedure runs when you
' select the Show All Customers command or click the Show All
' Customers button.
'
Sub ShowAllCustomers()
    With ActiveSheet
        If .FilterMode Then .ShowAllData
    End With
End Sub

' Listing 17.13. The CountCustomers procedure runs when you
' select the Count Customers command or click the Count
' Customers button.
'
Sub CountCustomers()
    Dim totalRows As Integer
    Dim alertMsg As String, alertButtons As Integer, alertTitle As String
    '
    ' Customer count is total rows in Database, minus 1
    '
    totalRows = Range("Database").Rows.Count - 1
    
    alertMsg = "There are currently " & _
        totalRows & _
        " customers in the database."
    alertButtons = vbInformation
    alertTitle = "Customer Database"
    MsgBox alertMsg, alertButtons, alertTitle
    
End Sub

' PhoneCustomer()
' The PhoneCustomer procedure runs when you select the
' Phone Customer command or click the Phone Customer button.
'
Sub PhoneCustomer()
    On Error GoTo BadStart
    Dim currCell As Range
    Dim currRow As Integer
    Dim response As Integer
    Dim phoneNumber As String
    Dim firstName As String
    Dim lastName As String
    Dim alertMsg As String
    Dim alertButtons As Integer
    Dim alertTitle As String
    Dim winDrive As String
    Dim winFolder As String
    '
    ' Turn off screen updates and save the active cell
    '
    Application.ScreenUpdating = False
    Set currCell = ActiveCell
    currRow = currCell.Row
    '
    ' Make sure selection is inside database
    '
    If Not InsideDatabase(currRow) Then
        Exit Sub
    End If
    '
    ' Get data for MsgBox message
    '
    firstName = Cells(currRow, Range("FirstNameField").Column)
    lastName = Cells(currRow, Range("FirstNameField").Column + 1)
    Cells(currRow, Range("PhoneNumberField").Column).Select
    '
    ' Check to see if phone number is blank
    '
    phoneNumber = ActiveCell
    If phoneNumber = "" Then
        MsgBox Prompt:="There is no phone number for this customer.", _
               Title:="Customer Database", _
               Buttons:=vbExclamation
        Exit Sub
    End If
    '
    ' Display the message
    '
    alertMsg = "About to dial the following customer:" & _
        Chr(13) & Chr(13) & _
        firstName & " " & lastName & _
        Chr(13) & _
        phoneNumber & _
        Chr(13) & Chr(13) & _
        "Please make sure your modem is turned on."
    alertButtons = vbOKCancel + vbExclamation
    alertTitle = "Phone Customer"
    response = MsgBox(alertMsg, alertButtons, alertTitle)
    '
    ' If user Cancels, return to active cell and bail out
    '
    If response = vbCancel Then
        currCell.Select
        Exit Sub
    End If
    '
    ' Otherwise, copy phone number to Clipboard and phone the customer
    '
    ActiveCell.Copy
    '
    ' Start Phone Dialer with the focus
    '
    If InStr(1, Application.OperatingSystem, "NT") Then
        '
        ' Use this line with Windows NT:
        '
        winDrive = Left(Environ("WINDIR"), 3)
        Shell winDrive & "Program Files\Windows NT\dialer.exe", 1
    Else
        '
        ' Use this line with Windows 95/98:
        '
        winFolder = Environ("WINDIR")
        Shell winFolder & "\dialer.exe", 1
    End If
    '
    ' Paste the copied phone number with Ctrl+V and
    ' then press Enter to select the Dial button
    '
    SendKeys "^v~", True
    '
    ' Wait eight seconds to give the modem time to dial
    '
    Application.Wait Now + TimeValue("00:00:08")
    '
    ' Close the dialog boxes and exit Phone Dialer
    '
    SendKeys "~{ESC}%{F4}"
    '
    ' Get rid of Excel's Copy mode indicators and
    ' select the original cell
    '
    Application.CutCopyMode = False
    currCell.Select

    Exit Sub

BadStart:
    MsgBox "Could not start Phone Dialer!", _
        vbOKOnly + vbExclamation
End Sub

' DeleteCustomer()
' The DeleteCustomer procedure runs when you select the
' Delete Customer command or click the Delete Customer button.
'
Sub DeleteCustomer()
    '
    ' Make sure selection is inside database
    '
    If Not InsideDatabase(ActiveCell.Row) Then
        Exit Sub
    End If
    '
    ' Set up the form and then Show it
    '
    With frmCustomer
        .Caption = "Delete Customer"   ' Set form title
        .Controls("cmdAction").Caption = "Delete"   ' Make sure first button is Add
        .Controls("cmdCancel").Caption = "Cancel"   ' Start second button as Cancel
        .Show
    End With
    Set frmCustomer = Nothing
End Sub
