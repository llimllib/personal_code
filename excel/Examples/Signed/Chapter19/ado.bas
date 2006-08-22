Attribute VB_Name = "ADO"
' Listing 19.11. A procedure that runs various
' Connection object methods.
'
Sub ConnectionObjectTest()
    Dim con As ADODB.Connection
    Dim lngRecs As Long
    Dim strSQL As String
    '
    ' Create the Connection object and then open it
    '
    Set con = CreateObject("ADODB.Connection")
    con.Open "Northwind"
    '
    ' Create a SQL UPDATE query
    '
    strSQL = "UPDATE Customers " & _
        "SET ContactTitle = 'Head Honcho'" & _
        "WHERE ContactTitle='Owner';"
    '
    ' Execute the query
    '
    On Error GoTo Rollback
    With con
        '
        ' Start a transaction and then execute the command
        '
        .BeginTrans
        .Execute strSQL, lngRecs
        '
        ' If no errors occurred, commit the changes
        ' and then close the Connection
        '
        .CommitTrans
        .Close
    End With
    '
    ' The lngRecs variable holds the number of records affected
    '
    MsgBox lngRecs & " records updated"
    '
    ' We're done!
    '
    Set con = Nothing
    Exit Sub
Rollback:
    '
    ' If an error occurs, cancel any changes
    '
    con.RollbackTrans
    con.Close
    Set con = Nothing
End Sub
'
' Listing 19.12. A procedure that runs various
' Recordset object methods.
'
Sub RecordsetObjectTest()
    Dim rs As ADODB.Recordset
    Dim lngRecs As Long
    Dim done As Boolean
    '
    ' Create the Recordset object and then
    ' open it it batch mode
    '
    Set rs = CreateObject("ADODB.Recordset")
    With rs
        .Source = "Customers"
        .ActiveConnection = "Northwind"
        .CursorType = adOpenStatic
        .LockType = adLockBatchOptimistic
        .Open
        '
        ' Find the records where Title = "Head Honcho"
        '
        lngRecs = 0
        done = False
        Do While Not done
            .Find "ContactTitle = 'Head Honcho'"
            '
            ' We go to the end of the recordset
            ' if the Find method failed
            '
            If Not .EOF Then
                '
                ' If Find was successful, change the record
                '
                !ContactTitle = "Owner"
                lngRecs = lngRecs + 1
            Else
                '
                ' Otherwise, bail out of the loop
                '
                done = True
            End If
        Loop
        '
        ' Write all the changed records
        '
        If lngRecs > 0 Then .UpdateBatch
        MsgBox lngRecs & " records updated"
        '
        ' That's it
        '
        .Close
    End With
    Set rs = Nothing
End Sub
