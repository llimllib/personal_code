VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmDBMaintainer 
   Caption         =   "Jet Database Maintainer"
   ClientHeight    =   3195
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   6255
   OleObjectBlob   =   "Maintain.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "frmDBMaintainer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cmdCreateDB_Click()
    frmCreateDB.Show
    Set frmCreateDB = Nothing
    If Not db Is Nothing Then
        lblDatabase.Caption = db.Name
    Else
        lblDatabase.Caption = "None"
    End If
End Sub

Private Sub cmdOpenDB_Click()
    Dim dbFile As Variant
    '
    ' Get the database filename
    '
    dbFile = Application.GetOpenFilename( _
        filefilter:="Jet Databases (*.mdb),*.mdb", _
        Title:="Open Jet Database")
    If dbFile = False Then Exit Sub
    '
    ' Close the current database
    '
    If Not db Is Nothing Then db.Close
    '
    ' Open the database
    '
    OpenDB (dbFile)
End Sub

Private Sub cmdDelete_Click()
    Dim dbFile As Variant
    '
    ' Get the database filename
    '
    dbFile = Application.GetOpenFilename( _
        filefilter:="Jet Databases (*.mdb),*.mdb", _
        Title:="Delete Jet Database")
    If dbFile = False Then Exit Sub
    '
    ' Delete the database file
    '
    If db.Name = dbFile Then
        db.Close
        Set db = Nothing
        lblDatabase.Caption = "None"
    End If
    Kill dbFile
    
End Sub

Private Sub cmdCompactDB_Click()
    frmCompactDB.Show
    Set frmCompactDB = Nothing
    If Not db Is Nothing Then lblDatabase.Caption = db.Name
End Sub

Private Sub cmdClose_Click()
    If Not db Is Nothing Then
        db.Close
        Set db = Nothing
    End If
    Unload Me
End Sub

