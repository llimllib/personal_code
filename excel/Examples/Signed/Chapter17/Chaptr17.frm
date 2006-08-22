VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmCustomer 
   Caption         =   "Add Customer"
   ClientHeight    =   3690
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4950
   OleObjectBlob   =   "Chaptr17.frx":0000
   StartUpPosition =   1  'CenterOwner
   Tag             =   "Delete"
End
Attribute VB_Name = "frmCustomer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' Listing 17.6. Some global variables and the code for
' the form's Activate event.
'
Dim textBoxes As New Collection ' Holds the form's text boxes
Dim totalFields As Integer      ' The total number of fields
Dim textboxNames() As String    ' Holds the names of the text boxes
'
' This event fires just before the form appears on screen
'
Private Sub UserForm_Activate()
    Dim ctrl As Control
    '
    ' Set up the custom textBoxes collection
    '
    For Each ctrl In Me.Controls
        If Left(ctrl.Name, 3) = "txt" Then
            textBoxes.Add Item:=ctrl, key:=ctrl.Name
        End If
    Next 'ctrl
    '
    ' Get the total number of fields in the list
    ' and then redimension the array of text box names
    '
    totalFields = Range("Database").Columns.Count
    ReDim textboxNames(totalFields)
    '
    ' Initialize the array of text box names in the
    ' order they appear in the list.
    '
    textboxNames(0) = "txtTitle"
    textboxNames(1) = "txtFirst"
    textboxNames(2) = "txtLast"
    textboxNames(3) = "txtPosition"
    textboxNames(4) = "txtCompany"
    textboxNames(5) = "txtAccount"
    textboxNames(6) = "txtAddress1"
    textboxNames(7) = "txtAddress2"
    textboxNames(8) = "txtCity"
    textboxNames(9) = "txtState"
    textboxNames(10) = "txtPostal"
    textboxNames(11) = "txtCountry"
    textboxNames(12) = "txtPhone"
    textboxNames(13) = "txtFax"
    textboxNames(14) = "txtMemo"
    '
    ' Set up the form using the caption on the cmdAction
    ' button to differentiate the various modules
    '
    Select Case cmdAction.Caption
        Case "Add"      ' The Add Customer command
            ' Nothing to do!
        Case "OK"       ' The Edit Customer command
            ReadData ActiveCell.Row
        Case "Delete"   ' The Delete Customer command
            ReadData ActiveCell.Row
            '
            ' Disable the text boxes
            '
            For Each ctrl In textBoxes
                ctrl.Enabled = False
            Next 'ctrl
            cmdCancel.SetFocus
    End Select
End Sub
'
' Listing 17.7. This event handler fires when the user clicks the
' cmdAction button. It uses the button caption to
' differentiate the various actions.
'
Private Sub cmdAction_Click()
    Dim dbTopRow As Integer
    Dim dbRows As Integer
    Dim dbNewRow As Integer
    Dim ctrl As Control

    Select Case cmdAction.Caption
        Case "Add"
            With Range("Database")
                '
                ' Insert the new row
                '
                dbTopRow = .Row
                dbRows = .Rows.Count
                dbNewRow = dbTopRow + dbRows
                WriteData dbNewRow
                '
                ' Define new Database range name
                '
                .Select
                Selection.Resize(RowSize:=dbRows + 1, ColumnSize:=totalFields).Select
                Names.Add Name:="Database", RefersTo:=Selection
                '
                ' Select first cell in new record
                '
                Cells(dbNewRow, 1).Select
            End With
            '
            ' Clear the text boxes
            '
            For Each ctrl In textBoxes
                ctrl.Text = ""
            Next 'ctrl
            '
            ' Change Cancel to Close
            '
            cmdCancel.Caption = "Close"
        Case "OK"
            WriteData ActiveCell.Row
            Unload Me
        Case "Delete"
            ActiveCell.EntireRow.Delete
            Unload Me
    End Select
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub UserForm_Terminate()
    Set textBoxes = Nothing
End Sub

' Listing 17.8. The WriteData procedure writes the
' form data to the specified row in the database.
'
Sub WriteData(dbRow As Integer)
    Dim i As Integer, fieldValue As Variant
    '
    ' Enter data in fields
    '
    For i = 1 To totalFields
        fieldValue = textBoxes(textboxNames(i - 1)).Text
        Cells(dbRow, i).Value = fieldValue
    Next 'i
    '
    ' Select first cell in record
    '
    Cells(dbRow, 1).Select
End Sub

' Listing 17.11. The ReadData procedure reads database
' data from the specified row into the form.
'
Sub ReadData(dbRow As Integer)
    Dim i As Integer, fieldValue As Variant
    '
    ' Enter data into form text boxes
    '
    For i = 1 To totalFields
        fieldValue = Cells(dbRow, i).Value
        textBoxes(textboxNames(i - 1)).Text = fieldValue
    Next 'i
End Sub




