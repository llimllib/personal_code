Attribute VB_Name = "Chapter16"
Private Declare Function GetWindowsDirectory Lib "kernel32" Alias "GetWindowsDirectoryA" (ByVal lpBuffer As String, ByVal nSize As Long) As Long

' Listing 16.1. A procedure that places all "Budget"
' worksheets into a custom collection.
'
Sub GatherBudgetSheets()
    Dim myCollection As New Collection
    Dim ws As Worksheet

    For Each ws In ThisWorkbook.Worksheets
        If InStr(ws.Name, "Budget") Then
            myCollection.Add Item:=ws, Key:=ws.Name
        End If
    Next 'ws
    MsgBox "The custom collection contains " & _
        myCollection.Count & " worksheets."
End Sub

' Listing 16.2. A procedure that clears all objects
' from a custom collection.
'
Sub ClearCollection(coll As Collection)
    Do While coll.Count > 0
        coll.Remove Index:=1
    Loop
End Sub

' Listing 16.3. Using an object in a custom class.
'
Sub CustomClassObjectTest()
    Dim newCustomer As New CCustomer
    With newCustomer
        .Account = "12-3456"
        .Name = "ACME Coyote Supplies"
        .Address = "123 Wily Way"
        Set .Invoices = New CInvoices
    End With
    With newCustomer.Invoices
        .Add CustAcct:=newCustomer.Account, Amt:=1234.56
        .Add CustAcct:=newCustomer.Account, Amt:=432.1
        MsgBox .Count
    End With
End Sub
