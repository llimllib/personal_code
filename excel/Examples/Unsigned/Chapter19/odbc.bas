Attribute VB_Name = "ODBC"
' Listing 19.4. Using OpenDatabase to connect to a SQL Server
' database in an ODBCDirect workspace.
'
Sub OpenDatabaseTest()
    Dim wsODBC As DAO.Workspace
    Dim db As DAO.Database
    '
    ' Create the ODBCDirect workspace
    '
    Set wsODBC = DBEngine.CreateWorkspace( _
        Name:="ODBCWorkspace", _
        UserName:="sa", _
        Password:="", _
        UseType:=dbUseODBC)
    '
    ' Connect to the database
    '
    Set db = wsODBC.OpenDatabase( _
        Name:="Northwind Sample Database", _
        Options:=dbDriverNoPrompt, _
        ReadOnly:=False, _
        Connect:="ODBC;DSN=Northwind;UID=;PWD=;")
    '
    ' Display the database name. Notice that the Name property is
    ' the same as the string specified in the OpenDatabase method's
    ' Name argument.
    '
    MsgBox db.Name
    '
    ' Shut everything down
    '
    db.Close
    wsODBC.Close
    Set wsODBC = Nothing
    Set db = Nothing
End Sub
'
' Listing 19.5. Using OpenConnection to connect to a SQL Server
' database in an ODBCDirect workspace.
'
Sub OpenConnectionTest()
    Dim con As DAO.Connection
    '
    ' We don't have to create an explicit ODBCDirect
    ' workspace if we set the DBEngine object's
    ' DefaultType property to dbUseODBC
    '
    DBEngine.DefaultType = dbUseODBC
    '
    ' Create the Connection object
    '
    Set con = OpenConnection( _
        Name:="Connection Test", _
        Options:=dbDriverNoPrompt, _
        ReadOnly:=False, _
        Connect:="ODBC;DSN=Northwind;UID=;PWD=;")
    '
    ' Display the connection name
    '
    MsgBox con.Name
    '
    ' Shut everything down
    '
    con.Close
    Set con = Nothing
End Sub
'
' Listing 19.6. A procedure that returns multiple recordsets.
'
Sub MultipleRecordsetTest()
    Dim wsODBC As DAO.Workspace
    Dim con As DAO.Connection
    Dim rs As DAO.Recordset
    Dim i As Integer, j As Integer
    '
    ' Set up the workspace
    '
    Set wsODBC = DBEngine.CreateWorkspace( _
        Name:="ODBCWorkspace", _
        UserName:="", _
        Password:="", _
        UseType:=dbUseODBC)
    '
    ' Use local cursors
    '
    wsODBC.DefaultCursorDriver = dbUseODBCCursor
    '
    ' Create the Connection object
    '
    Set con = wsODBC.OpenConnection( _
        Name:="Multiple Recordsets", _
        Options:=dbDriverNoPrompt, _
        ReadOnly:=False, _
        Connect:="ODBC;DSN=Northwind;UID=;PWD=;")
    '
    ' Use multiple SELECTs to return the recordsets
    '
    Set rs = con.OpenRecordset( _
        "SELECT * FROM [Customers]; " & _
        "SELECT * FROM [Employees];")
    '
    ' Loop through the returned objects
    '
    i = 1
    With rs
        Do
            j = 0
            '
            ' Loop through the current recordset
            '
            Do While Not .EOF
                j = j + 1
                .MoveNext
            Loop
            MsgBox "Recordset #" & i & " has " & j & " records."
            i = i + 1
        Loop Until (rs.NextRecordset = False)
    End With
    '
    ' Shut everything down
    '
    rs.Close
    con.Close
    wsODBC.Close
    Set wsODBC = Nothing
    Set con = Nothing
    Set rs = Nothing
End Sub

