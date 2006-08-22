VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmInput 
   Caption         =   "Text File Displayer"
   ClientHeight    =   5730
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   6060
   OleObjectBlob   =   "Chap26-1.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "frmInput"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit
'
' Listing 26.10. Support procedures for displaying a text file in a form text box.
'
' This is the Click event handler for the Browse button.
'
Private Sub cmdBrowse_Click()
    Dim strFileName As Variant
    '
    ' Get the name of a text file
    '
    strFileName = Application.GetOpenFilename("Text Files (*.txt; *.bas; *.bat; *.ini), *.txt; *.bas; *.bat; *.ini")
    '
    ' Was Cancel clicked?
    '
    If strFileName <> False Then
        '
        ' If not, clear the text box and read the file contents
        '
        txtContents = ""
        txtPathName = strFileName
        ReadFileContents (strFileName)
        txtContents.SetFocus
    End If
End Sub
'
' This is the BeforeUpdate event handler for the "path" text box
'
Private Sub txtPathName_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)
    '
    ' Does the file exist?
    '
    If txtPathName <> "" Then
        If Dir(txtPathName) <> "" Then
            '
            ' If so, clear the text box and read the file contents
            '
            txtContents = ""
            ReadFileContents (txtPathName)
            txtContents.SetFocus
        Else
            '
            ' If not, display an error message and cancel the update
            '
            If MsgBox("The filename you entered doesn't exist!", vbRetryCancel) = vbCancel Then
                txtPathName = ""
            End If
            Cancel = True
        End If
    End If
End Sub
'
' This procedure reads the file contents into the text box
'
Sub ReadFileContents(strName As String)
    Dim fn As Integer
    '
    ' Get the next free file handle
    '
    fn = FreeFile
    '
    ' Open the file for input
    '
    Open strName For Input As #fn
    '
    ' Use LOF to read the entire contents into the text box
    '
    txtContents = Input(LOF(fn), #fn)
    '
    ' Close the file
    '
    Close #fn
End Sub

Private Sub cmdClose_Click()
    Unload Me
End Sub

