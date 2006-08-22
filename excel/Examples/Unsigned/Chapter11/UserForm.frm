VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} UserForm1 
   Caption         =   "UserForm1"
   ClientHeight    =   2775
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   3915
   OleObjectBlob   =   "UserForm.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "UserForm1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub ListBox1_Click()

End Sub

' Listing 11.1. Event handler code that keeps a text box and
' spin button in synch.
'
Private Sub TextBox1_Change()
    SpinButton1.Value = TextBox1.Value
End Sub

Private Sub SpinButton1_Change()
    TextBox1.Value = SpinButton1.Value
End Sub

Private Sub cmdCancel_Click()
    Dim result As Integer
    result = MsgBox("Are you sure you want to Cancel?", vbYesNo + vbQuestion)
    If result = vbYes Then Unload Me
End Sub


Private Sub cmdOK_Click()
    Dim result As Integer
    If ListBox1.Text = "" Then
        result = MsgBox("You didn't select a dwarf!", vbOKCancel + vbExclamation)
        If result = vbOK Then Unload Me
    Else
        MsgBox "You chose " & ListBox1.Text & "!"
        Unload Me
    End If

End Sub

Private Sub UserForm_Click()

End Sub

Private Sub UserForm_Terminate()

End Sub
