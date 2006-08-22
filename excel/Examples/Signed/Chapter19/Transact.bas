Attribute VB_Name = "Transactions"
Const DBLOCATION = "C:\Program Files\Microsoft Office\Office\Samples\Northwind.mdb"
'
' Listing 19.3. A procedure that demonstrates transaction processing.
'
Sub TransactionTest()
    Dim nwDB As Database
    Dim rs As Recordset
    Dim newTitle As String
    Dim updates As Integer
    '
    ' Open the Northwind database (check the path!)
    '
    If Dir(DBLOCATION) = "" Then
        MsgBox "The location of the NorthWind sample " & _
        "database is incorrect." & Chr(13) & _
        "Please adjust the path and then run this " & _
        "procedure again."
        Exit Sub
    End If
    Set nwDB = OpenDatabase(DBLOCATION)
    '
    ' Open the Customers table
    '
    Set rs = nwDB.OpenRecordset("Customers")
    '
    ' Start the transaction
    '
    BeginTrans
    '
    ' Make the changes
    '
    updates = 0
    With rs
        Do While Not .EOF
            If !ContactTitle = "Owner" Then
                .Edit
                !ContactTitle = "Head Honcho"
                .Update
                updates = updates + 1
            End If
            .MoveNext
        Loop
        '
        ' Ask to save changes
        '
        If MsgBox("Do you want to save all the changes?", vbYesNo) = vbYes Then
            '
            ' Save the changes to disk
            '
            CommitTrans
            MsgBox updates & " changes committed!"
        Else
            '
            ' Discard the changes
            '
            Rollback
        End If
    End With
    '
    ' Close the database
    '
    nwDB.Close
End Sub

