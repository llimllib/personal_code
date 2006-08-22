VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} TabStripTest 
   Caption         =   "TabStrip Test"
   ClientHeight    =   2715
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   3300
   OleObjectBlob   =   "TabStrip.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "TabStripTest"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' Listing 11.2. An Initialize event procedure that
' sets up a TabStrip.
'
Private Sub UserForm_Initialize()
    '
    ' Rename the existing tabs
    '
    With TabStrip1
        .Tabs(0).Caption = "Division I"
        .Tabs(1).Caption = "Division II"
        '
        ' Add a new tab
        '
        .Tabs.Add "Division III"
    End With
    '
    ' Enter the intial data for Division I
    '
    With Worksheets("2000 Budget")
        txtSales = .[B2]
        txtExpenses = .[B12]
        txtGrossProfit = .[B13]
    End With
End Sub

' Listing 11.3. A Change event procedure that modifies
' the controls within a tab strip whenever the user
' selects a different tab.
'
Private Sub TabStrip1_Change()
        With Worksheets("2000 Budget")
            Select Case TabStrip1.Value
                Case 0
                '
                ' Enter the data for Division I
                '
                txtSales = .[B2]
                txtExpenses = .[B12]
                txtGrossProfit = .[B13]
                Case 1
                '
                ' Enter the data for Division II
                '
                txtSales = .[C2]
                txtExpenses = .[C12]
                txtGrossProfit = .[C13]
                Case 2
                '
                ' Enter the data for Division III
                '
                txtSales = .[D2]
                txtExpenses = .[D12]
                txtGrossProfit = .[D13]
            End Select
        End With

End Sub

Private Sub cmdClose_Click()
    Unload Me
End Sub

