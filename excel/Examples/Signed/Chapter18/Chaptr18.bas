Attribute VB_Name = "Chapter18"
Const DBLOCATION = "C:\Program Files\Microsoft Office\Office\Samples\Northwind.mdb"
' Listing 18.1. A procedure that connects to an Access database.
'
Sub DatabaseConnection()

    Dim db As Database
    Dim rs As Recordset
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
    Set db = OpenDatabase(DBLOCATION)
    '
    ' Open the Customers table (recordset)
    '
    Set rs = db.OpenRecordset("Customers")
    '
    ' Display confirmation message
    '
    MsgBox "Opened " & db.Name & " Successfully!" & _
           Chr(13) & Chr(13) & _
           "The open Recordset is " & rs.Name
    '
    ' Close and release the objects
    '
    rs.Close
    db.Close
    Set rs = Nothing
    Set db = Nothing
End Sub

' Listing 18.2. A procedure that connects to a non-Jet database.
'
Sub NonJetConnection()
    Dim db As Database
    Dim tdDBASE As TableDef
    Dim rs As Recordset
    '
    ' Open the Jet database (check the path!)
    '
    If Dir(DBLOCATION) = "" Then
        MsgBox "The location of the NorthWind sample " & _
        "database is incorrect." & Chr(13) & _
        "Please adjust the path and then run this " & _
        "procedure again."
        Exit Sub
    End If
    Set db = OpenDatabase(DBLOCATION)
    '
    ' Create TableDef and set the connection information.
    ' This code assumes the CUSTOMER.DBF file (it's on the
    ' CD) is in the same folder as this workbook.
    '
    Set tdDBASE = db.CreateTableDef("Linked dBASE Table")
    tdDBASE.Connect = "dBASE IV;DATABASE=" & ThisWorkbook.Path
    tdDBASE.SourceTableName = "Customer"
    '
    ' Append the TableDef to create the link
    '
    db.TableDefs.Append tdDBASE
    '
    ' Open the recordset
    '
    Set rs = db.OpenRecordset("Linked dBASE Table", dbOpenSnapshot)
    '
    ' Display confirmation message
    '
    MsgBox "Opened " & db.Name & " Successfully!" & _
            Chr(13) & Chr(13) & _
            "The open Recordset is " & rs.Name & _
            Chr(13) & _
            "The source table is " & tdDBASE.SourceTableName
    '
    ' Close and release the objects
    '
    rs.Close
    db.Close
    Set rs = Nothing
    Set tdDBASE = Nothing
    Set db = Nothing
End Sub

' Listing 18.3. A procedure that displays information on
' all the fields in a Recordset.
'
Sub DisplayFieldInfo()
    Dim db As Database
    Dim rs As Recordset
    Dim fld As Field
    Dim i As Integer
    Dim fieldInfo As String
    '
    ' Open the Northwind database
    '
    If Dir(DBLOCATION) = "" Then
        MsgBox "The location of the NorthWind sample " & _
        "database is incorrect." & Chr(13) & _
        "Please adjust the path and then run this " & _
        "procedure again."
        Exit Sub
    End If
    Set db = OpenDatabase(DBLOCATION)
    '
    ' Open the Categories table
    '
    Set rs = db.OpenRecordset("Categories", dbOpenSnapshot)
    '
    ' Enumerate all fields in the Recordset
    '
    For i = 0 To rs.Fields.Count - 1
        fieldInfo = "Recordset: " & rs.Name & Chr(13) & _
            "Field " & _
            i + 1 & " of " & _
            rs.Fields.Count & Chr(13) & Chr(13)
        '
        ' Set the Field variable and then run through the properties
        '
        Set fld = rs.Fields(i)
        fieldInfo = fieldInfo & _
            "Name: " & fld.Name & Chr(13) & _
            "Allow Zero Length: " & fld.AllowZeroLength & Chr(13) & _
            "Attributes: " & fld.Attributes & Chr(13) & _
            "Collating Order: " & fld.CollatingOrder & Chr(13) & _
            "Default Value: " & fld.DefaultValue & Chr(13) & _
            "Ordinal Position: " & fld.OrdinalPosition & Chr(13) & _
            "Required: " & fld.Required & Chr(13) & _
            "Size: " & fld.Size & Chr(13) & _
            "Source Field: " & fld.SourceField & Chr(13) & _
            "Source Table: " & fld.SourceTable & Chr(13) & _
            "Type of Field: " & TypeOfField(fld.Type) & Chr(13) & _
            "Validation Rule: " & fld.ValidationRule & Chr(13) & _
            "Validation Text: " & fld.ValidationText
        MsgBox Prompt:=fieldInfo, Title:="Field Information"
    Next i
    '
    ' Close and release the objects
    '
    rs.Close
    db.Close
    Set rs = Nothing
    Set fld = Nothing
    Set db = Nothing
End Sub

' TypeOfField()
' Function to translate the constant returned by a Field object's
' Type property into a descriptive string.
'
Function TypeOfField(fldConstant As Integer) As String

    Select Case fldConstant
        Case 1   ' dbBoolean
            TypeOfField = "Boolean"
        Case 2   ' dbByte
            TypeOfField = "Byte"
        Case 3   ' dbInteger
            TypeOfField = "Integer"
        Case 4   ' dbLong
            TypeOfField = "Long Integer"
        Case 5   ' dbCurrency
            TypeOfField = "Currency"
        Case 6   ' dbSingle
            TypeOfField = "Single"
        Case 7   ' dbDouble
            TypeOfField = "Double"
        Case 8   ' dbDate
            TypeOfField = "Date"
        Case 10  ' dbText
            TypeOfField = "Text"
        Case 11  'dbLongBinary
            TypeOfField = "OLE Object"
        Case 12  ' dbMemo
            TypeOfField = "Memo"
        Case 15  ' dbGUID
            TypeOfField = "GUID"
    End Select
End Function

' Listing 18.4. A procedure that opens a recordset using
' a SQL SELECT expression.
'
Sub QueryCustomers()
    Dim db As Database
    Dim strSELECT As String
    Dim rs As Recordset
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
    Set db = OpenDatabase(DBLOCATION)
    '
    ' Store the SELECT statement in a string variable
    '
    strSELECT = "SELECT CompanyName,Region,Country " & _
                "FROM Customers " & _
                "WHERE Country = 'Canada' " & _
                "ORDER BY CompanyName"
    '
    ' Open the recordset
    '
    Set rs = db.OpenRecordset(strSELECT)
    '
    ' Display confirmation message
    '
    MsgBox "The filtered Recordset contains " & _
    rs.RecordCount & " records."
    '
    ' Close and release the objects
    '
    rs.Close
    db.Close
    Set rs = Nothing
    Set db = Nothing
End Sub

' Listing 18.5. A procedure that creates a recordset from
' a QueryDef object.
'
Sub QueryDefExample()
    Dim db As Database
    Dim qd As QueryDef
    Dim rs As Recordset
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
    Set db = OpenDatabase(DBLOCATION)
    '
    ' Assign the QueryDef object
    '
    Set qd = db.QueryDefs("Products Above Average Price")
    '
    ' Open the recordset
    '
    Set rs = qd.OpenRecordset()
    '
    ' Display confirmation message
    '
    MsgBox "The filtered Recordset contains " & _
        rs.RecordCount & " records."
    '
    ' Close and release the objects
    '
    rs.Close
    db.Close
    Set rs = Nothing
    Set qd = Nothing
    Set db = Nothing
End Sub

' Listing 18.6. A procedure that reads 100 rows from a
' recordset into a worksheet.
'
Sub ReadDataIntoExcel()
    Dim db As Database
    Dim qd As QueryDef
    Dim rs As Recordset
    Dim recArray As Variant
    Dim i As Integer, j As Integer
    '
    ' Open the Jet database, QueryDef, and Recordset
    '
    If Dir(DBLOCATION) = "" Then
        MsgBox "The location of the NorthWind sample " & _
        "database is incorrect." & Chr(13) & _
        "Please adjust the path and then run this " & _
        "procedure again."
        Exit Sub
    End If
    Set db = OpenDatabase(DBLOCATION)
    Set qd = db.QueryDefs("Invoices")
    Set rs = qd.OpenRecordset()
    '
    ' Head for Database Records and clear the sheet
    '
    Worksheets("Database Records").Activate
    With Worksheets("Database Records").[a1]
        .CurrentRegion.Clear
        '
        ' Read the data using GetRows
        '
        recArray = rs.GetRows(100)
        For i = 0 To UBound(recArray, 2)
            For j = 0 To UBound(recArray, 1)
                .Offset(i + 1, j) = recArray(j, i)
            Next j
        Next i
        '
        ' Enter the field names and format the cells
        '
        For j = 0 To rs.Fields.Count - 1
            .Offset(0, j) = rs.Fields(j).Name
            .Offset(0, j).Font.Bold = True
            .Offset(0, j).EntireColumn.AutoFit
        Next j

    End With
    '
    ' Close and release the objects
    '
    rs.Close
    db.Close
    Set rs = Nothing
    Set qd = Nothing
    Set db = Nothing
End Sub

' Listing 18.7. A procedure that filters out OLE Object
' fields before retrieving a recordset.
'
Sub RetrieveCategories()
    Dim db As Database
    Dim rs As Recordset
    Dim fld As Field
    Dim strSELECT As String, i As Integer
    '
    ' Open the Jet database
    '
    If Dir(DBLOCATION) = "" Then
        MsgBox "The location of the NorthWind sample " & _
        "database is incorrect." & Chr(13) & _
        "Please adjust the path and then run this " & _
        "procedure again."
        Exit Sub
    End If
    Set db = OpenDatabase(DBLOCATION)
    '
    ' Open the full Categories table
    '
    Set rs = db.OpenRecordset("Categories")
    '
    ' The strSELECT variable will hold the SQL SELECT statement
    ' that filters the Recordset to remove OLE Object fields
    '
    strSELECT = "SELECT "
    '
    ' Run through the recordset fields
    '
    For Each fld In rs.Fields
        '
        ' Check for OLE Object fields
        '
        If fld.Type <> dbLongBinary Then
            '
            ' If it's not an OLE Object field, add it to the SELECT statement
            '
            strSELECT = strSELECT & fld.Name & ","
        End If
    Next fld
    '
    ' Remove the trailing comma
    '
    strSELECT = Left(strSELECT, Len(strSELECT) - 1)
    '
    ' Add the FROM clause
    '
    strSELECT = strSELECT & " FROM Categories"
    '
    ' Open the filtered recordset
    '
    Set rs = db.OpenRecordset(strSELECT)
    '
    ' Retrieve the records
    '
    Worksheets("Database Records").Activate
    With Worksheets("Database Records").[a1]
        .CurrentRegion.Clear
        .Offset(1).CopyFromRecordset rs
        '
        ' Enter the field names and format the cells
        '
        For i = 0 To rs.Fields.Count - 1
            .Offset(0, i) = rs.Fields(i).Name
            .Offset(0, i).Font.Bold = True
            .Offset(0, i).EntireColumn.AutoFit
        Next i
    End With
    '
    ' Close and release the objects
    '
    rs.Close
    db.Close
    Set rs = Nothing
    Set fld = Nothing
    Set db = Nothing
End Sub

