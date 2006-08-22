VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmCompactDB 
   Caption         =   "Compact Jet Database"
   ClientHeight    =   4170
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5355
   OleObjectBlob   =   "Compact.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "frmCompactDB"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
'
' Listing 19.2. Event handler code for the Compact Jet
' Database form
'
' Initialize with the open database
'
Private Sub UserForm_Initialize()
    If Not db Is Nothing Then
        txtSrcName = db.Name
        txtDstName = txtSrcName
    End If
End Sub
'
' The Source database Browse button event handler. This
' procedure displays the built-in Save As dialog box and
' grabs the specified file path and name.
'
Private Sub cmdSrcBrowse_Click()
    Dim dbFile As Variant
    '
    ' Get the source database filename
    '
    dbFile = Application.GetSaveAsFilename( _
        filefilter:="Jet Databases (*.mdb),*.mdb", _
        Title:="Source Database")
    If dbFile = False Then Exit Sub
    txtSrcName = dbFile
    txtDstName = dbFile
End Sub
'
' The Destination database Browse button event handler. This
' procedure displays the built-in Save As dialog box and
' grabs the specified file path and name.
'
Private Sub cmdDstBrowse_Click()
    Dim dbFile As Variant
    '
    ' Get the database file to be compacted
    '
    dbFile = Application.GetSaveAsFilename( _
        filefilter:="Jet Databases (*.mdb),*.mdb", _
        Title:="Destination Database")
    If dbFile = False Then Exit Sub
    txtDstName = dbFile
End Sub
'
' The Exit event handler for the Source Database Name text box.
' This procedure ensures that the source file exists.
'
Private Sub txtSrcName_Exit(ByVal Cancel As MSForms.ReturnBoolean)
    Dim dbName As String
    dbName = txtSrcName
    If dbName = "" Then
        '
        ' Filename is blank, so never mind
        '
        Exit Sub
    ElseIf Dir(dbName) <> "" Then
        '
        ' File exists, so copy name to destination and bail out
        '
        txtDstName = dbName
        Exit Sub
    Else
        MsgBox "Source database does not exist!", _
            vbExclamation, _
            "Jet Database Maintainer"
        txtSrcName.SetFocus
    End If
End Sub
'
' The Compact button event handler. This
' procedure compacts the source database.
'
Private Sub cmdCompact_Click()
    On Error GoTo ErrorHandler
    Dim intOptions As Integer
    Dim compactToSource As Boolean
    compactToSource = False
    '
    ' Make sure the source database name was entered
    '
    If txtSrcName = "" Then
        MsgBox "You must enter a name for the source database!", _
            vbExclamation + vbOKOnly, _
            "Jet Database Maintainer"
        txtSrcName.SetFocus
    '
    ' Make sure the destination database name was entered
    '
    ElseIf txtDstName = "" Then
        MsgBox "You must enter a name for the destination database!", _
            vbExclamation + vbOKOnly, _
            "Jet Database Maintainer"
        txtDstName.SetFocus
    Else
        '
        ' Close the current database if it's the source or
        ' if the Open Destination Database After Compacting
        ' check box is activated
        '
        If Not db Is Nothing Then
            With db
                If txtSrcName = .Name Or chkOpenDB Then CloseDB (.Name)
            End With
        End If
        '
        ' If source and destination are the same,
        ' use a temporary name for the destination
        '
        If txtSrcName = txtDstName Then
            Randomize
            txtDstName = CStr(Int(89999999 * Rnd + 10000000)) & ".mdb"
            compactToSource = True
        End If
        '
        ' Calculate the Options argument
        '
        If opt10 Then
            intOptions = opt10.Tag
        ElseIf opt11 Then
            intOptions = opt11.Tag
        ElseIf opt20 Then
            intOptions = opt20.Tag
        Else
            intOptions = opt30.Tag
        End If
        intOptions = intOptions + chkEncrypt * (-2)
        '
        ' Compact the source database
        '
        DBEngine.CompactDatabase _
            srcname:=txtSrcName, _
            dstname:=txtDstName, _
            dstlocale:=dbLangGeneral, _
            Options:=intOptions
        '
        ' If the source and destination are the same,
        ' delete the source and then rename the temporary
        ' file to the source name
        '
        If compactToSource = True Then
            Kill txtSrcName
            Name txtDstName As txtSrcName
            txtDstName = txtSrcName
        End If
        '
        ' Open the destination, if applicable
        '
        If chkOpenDB = True Then
            OpenDB (txtDstName)
        End If
        Unload Me
          
    End If
    Exit Sub
'
' Trap errors
'
ErrorHandler:
    '
    ' Destination database already exists
    '
    If Err = 3204 Then
        If MsgBox("The destination database name you entered already exists. Do you want to replace the existing database?", _
            vbQuestion + vbYesNo, _
            "Jet Database Maintainer") = vbYes Then
            Kill txtDstName
            Resume
        Else
            txtDstName.SetFocus
            Exit Sub
        End If
    '
    ' Can't compact to an earlier version
    '
    ElseIf Err = 3301 Then
        MsgBox "Can't compact to an earlier version!", _
            vbExclamation, _
            "Jet Database Maintainer"
            Exit Sub
    End If
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

