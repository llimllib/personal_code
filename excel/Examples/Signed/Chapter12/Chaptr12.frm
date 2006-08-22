VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} CommandBarInfo 
   Caption         =   "Command Bar Info"
   ClientHeight    =   3720
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   6015
   OleObjectBlob   =   "Chaptr12.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "CommandBarInfo"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' Global variable
Dim currCB As CommandBar

Private Sub UserForm_Initialize()
    Dim cb As CommandBar
    Dim cbType As String
    '
    ' Add the command bar names to the list
    '
    For Each cb In CommandBars
        cboName.AddItem cb.Name
    Next cb
    cboName.ListIndex = 0
End Sub

Private Sub cboName_Change()
    Dim ctrl As CommandBarControl
    Set currCB = CommandBars(cboName.Value)
    With currCB
        '
        ' Write the Type and Index for the current command bar
        '
        lblType.Caption = CommandBarType(.Type)
        lblIndex.Caption = .Index
        '
        ' Clear the Control list
        '
        cboCaption.Clear
        '
        ' Add the control names from the current command bar
        '
        For Each ctrl In .Controls
            cboCaption.AddItem ctrl.Caption
        Next ctrl
        cboCaption.ListIndex = 0
    End With
End Sub

Private Sub cboCaption_Change()
    Dim ctrl As CommandBarControl
    Dim currCtrl As CommandBarControl
    Dim currCB As CommandBar
    With cboCaption
        '
        ' This event fires when the list's Clear method is run
        '
        If .ListCount = 0 Then Exit Sub
        '
        ' Write the ID, Type, and Index for the current control
        '
        Set currCB = CommandBars(cboName.Value)
        Set currCtrl = currCB.Controls(.ListIndex + 1)
        With currCtrl
            lblID.Caption = .ID
            lblCtrlType.Caption = ControlType(.Type)
            lblCtrlIndex.Caption = .Index
            '
            ' Clear the Menu Bar Commands list
            '
            cboCommandCaption.Clear
            '
            ' Add the subcontrol names from the current control
            '
            If currCB.Type = msoBarTypeMenuBar Then
                For Each ctrl In .Controls
                    cboCommandCaption.AddItem ctrl.Caption
                Next ctrl
                cboCommandCaption.ListIndex = 0
            Else
                lblCommandID.Caption = ""
                lblCommandType.Caption = ""
                lblCommandIndex.Caption = ""
            End If
        End With
    End With
End Sub

Private Sub cboCommandCaption_Change()
    Dim ctrl As CommandBarControl
    Dim currCtrl As CommandBarControl
    Dim currCommand As CommandBarControl
    Dim currCB As CommandBar
    With cboCommandCaption
        '
        ' This event fires when the list's Clear method is run
        '
        If .ListCount = 0 Then Exit Sub
        '
        ' Write the ID, Type, and Index for the current control
        '
        Set currCB = CommandBars(cboName.Value)
        Set currCtrl = currCB.Controls(cboCaption.ListIndex + 1)
        With currCtrl.Controls(.ListIndex + 1)
            lblCommandID.Caption = .ID
            lblCommandType.Caption = ControlType(.Type)
            lblCommandIndex.Caption = .Index
        End With
    End With
End Sub


Private Sub cmdClose_Click()
    Unload Me
End Sub

' This procedure takes the Type property of a command bar
' and converts it into its English language equivalent.
'
Function CommandBarType(cbType) As String
    Select Case cbType
        Case msoBarTypeNormal   '0
            CommandBarType = "Toolbar"
        Case msoBarTypeMenuBar  '1
            CommandBarType = "Menu Bar"
        Case msoBarTypePopup    '2
            CommandBarType = "Shortcut Menu"
    End Select
End Function

' This procedure takes the Type property of a control
' and converts it into a string that is the same as
' the name of the constant.
'
Function ControlType(ctrlType) As String
    Select Case ctrlType
        Case msoControlButton
            ControlType = "msoControlButton"
        Case msoControlButtonDropdown
            ControlType = "msoControlButtonDropdown"
        Case msoControlButtonPopup
            ControlType = "msoControlButtonPopup"
        Case msoControlComboBox
            ControlType = "msoControlComboBox"
        Case msoControlDropdown
            ControlType = "msoControlDropdown"
        Case msoControlEdit
            ControlType = "msoControlEdit"
        Case msoControlExpandingGrid
            ControlType = "msoControlExpandingGrid"
        Case msoControlGauge
            ControlType = "msoControlGauge"
        Case msoControlGraphicCombo
            ControlType = "msoControlGraphicCombo"
        Case msoControlGraphicDropdown
            ControlType = "msoControlGraphicDropdown"
        Case msoControlGrid
            ControlType = "msoControlGrid"
        Case msoControlOCXDropdown
            ControlType = "msoControlOCXDropdown"
        Case msoControlPopup
            ControlType = "msoControlPopup"
        Case msoControlSplitButtonMRUPopup
            ControlType = "msoControlSplitButtonMRUPopup"
        Case msoControlSplitButtonPopup
            ControlType = "msoControlSplitButtonPopup"
        Case msoControlSplitDropdown
            ControlType = "msoControlSplitButtonPopup"
    End Select
End Function
