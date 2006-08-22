VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmCreateDB 
   Caption         =   "Create Jet Database"
   ClientHeight    =   2850
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4980
   OleObjectBlob   =   "Create.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "frmCreateDB"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
'
' Listing 19.1. Event handler code for the Create Jet
' Database form
'
' The Browse button event handler. This
' procedure displays the built-in Save As dialog box and
' grabs the specified file path and name.
'
Private Sub cmdBrowse_Click()
    Dim dbFile As Variant
    '
    ' Get the database filename
    '
    dbFile = Application.GetSaveAsFilename( _
        filefilter:="Jet Databases (*.mdb),*.mdb", _
        Title:="Create Jet Database")
    If dbFile = False Then Exit Sub
    txtDBName = dbFile
End Sub
'
' The Exit event handler for the text box. This procedure
' ensures that the filename uses the .MDB extension.
'
Private Sub txtDBName_Exit(ByVal Cancel As MSForms.ReturnBoolean)
    Dim dbName As String
    Dim dotPosition As Integer
    
    dbName = txtDBName
    If dbName = "" Or UCase$(Left(dbName, 4)) = ".MDB" Then
        '
        ' Filename is blank or okay, so never mind
        '
        Exit Sub
    Else
        dotPosition = InStr(dbName, ".")
        If dotPosition = 0 Then
            '
            ' If there's no extension, add one
            '
            dbName = dbName + ".mdb"
        Else
            '
            ' If there's a different extension, change it
            '
            dbName = Left$(dbName, dotPosition) + "mdb"
        End If
        txtDBName = dbName
    End If
End Sub
'
' The Create button event handler. This
' procedure creates the new database.
'
Private Sub cmdCreate_Click()
    On Error GoTo ErrorHandler
    Dim intOption As Integer
    '
    ' Make sure a database name was entered
    '
    If txtDBName <> "" Then
        '
        ' Calculate the Option argument
        '
        If opt10 Then
            intOption = opt10.Tag
        ElseIf opt11 Then
            intOption = opt11.Tag
        ElseIf opt20 Then
            intOption = opt20.Tag
        Else
            intOption = opt30.Tag
        End If
        intOption = intOption + chkEncrypt * (-2)
        '
        ' Create the database
        '
        CreateDatabase _
            Name:=txtDBName, _
            locale:=dbLangGeneral, _
            Option:=intOption
        If chkOpenDB = True Then
            OpenDB (txtDBName)
            frmDBMaintainer.lblOpen.Caption = "Yes"
        Else
            frmDBMaintainer.lblOpen.Caption = "No"
        End If
        Unload Me
    Else
        MsgBox "You must enter a name for the new database!", _
            vbExclamation + vbOKOnly, _
            "Jet Database Maintainer"
        txtDBName.SetFocus
    End If
    Exit Sub
'
' Trap the "Database already exists" error (3204)
'
ErrorHandler:
    If Err = 3204 Then
        If MsgBox("The database name you entered already exists. Do you want to replace the existing database?", _
            vbQuestion + vbYesNo, _
            "Jet Database Maintainer") = vbYes Then
            '
            ' If the database to be replaced is already
            ' open, close it first
            '
            With frmDBMaintainer
                If .lblDatabase = txtDBName And _
                   .lblOpen.Caption = "Yes" Then
                    CloseDB (txtDBName)
                End If
            End With
            Kill txtDBName
            Resume
        Else
            txtDBName.SetFocus
            Exit Sub
        End If
    End If
        
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

