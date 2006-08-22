VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmConvertCase 
   Caption         =   "Convert Case"
   ClientHeight    =   1950
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4395
   OleObjectBlob   =   "ConvCase.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "frmConvertCase"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

' Listing 11.4. A procedure that processes the Convert Case
' custom form.
'
Private Sub cmdOK_Click()
    Dim c As Range
    For Each c In Selection
        If optProper.Value = True Then
            c.Value = StrConv(c, vbProperCase)
        ElseIf optUpper.Value = True Then
            c.Value = StrConv(c, vbUpperCase)
        ElseIf optLower.Value = True Then
            c.Value = StrConv(c, vbLowerCase)
        End If
    Next 'c
    Unload Me
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub
