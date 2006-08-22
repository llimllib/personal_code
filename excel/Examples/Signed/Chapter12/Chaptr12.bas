Attribute VB_Name = "Chapter12"
' Listing 12.1. A procedure that runs through Excel's
' CommandBars collection and writes the name, type, and
' index number of each command bar.
'
Sub ListExcelCommandBars()
    Dim i As Integer
    Dim cb As CommandBar
    Dim cbType As String
    i = 0
    For Each cb In CommandBars
        Select Case cb.Type
            Case msoBarTypeNormal   '0
                cbType = "Toolbar"
            Case msoBarTypeMenuBar  '1
                cbType = "Menu Bar"
            Case msoBarTypePopup    '2
                cbType = "Shortcut Menu"
        End Select
        With Worksheets("Sheet1").[a2]
            .Offset(i, 0) = cb.Name
            .Offset(i, 1) = cbType
            .Offset(i, 2) = cb.Index
        End With
        i = i + 1
    Next
    Set cb = Nothing
End Sub

' Listing 12.2. A procedure that creates a new toolbar
' after first checking to see if an existing command bar
' with the same name already exists.
'
Sub AddToolbar()
    Dim cb As CommandBar
    Dim cbExists As Boolean
    
    cbExists = False
    For Each cb In CommandBars
        If cb.Name = "My Toolbar" Then
            cbExists = True
            Exit For
        End If
    Next cb
    If cbExists Then
        MsgBox "A command bar named ""My Toolbar"" already exists!"
    Else
        Set cb = CommandBars.Add( _
            Name:="My Toolbar", _
            Position:=msoBarFloating, _
            Temporary:=True)
    End If
    Set cb = Nothing
End Sub

' Listing 12.3. A procedure that runs through the
' CommandBars collection and resets the built-in command
' bars and deletes the custom command bars.
'
Sub CleanUpCommandBars()
    Dim cb As CommandBar
    For Each cb In CommandBars
        If cb.BuiltIn Then
            cb.Reset
        Else
            cb.Delete
        End If
    Next cb
    Set cb = Nothing
End Sub

' Listing 12.4. A procedure that modifies the Tools menu
' by adding a command to execute the RunCommandBarInfo
' procedure.
'
Sub AddToolsMenuCommand()
    Dim cb As CommandBar
    Dim menuTools As CommandBarControl
    Dim ctrl As CommandBarControl
    Dim ctrlExists As Boolean
    
    ctrlExists = False
    '
    ' Get the Tools menu (ID=30007)
    '
    Set menuTools = CommandBars.FindControl(ID:=30007)
    '
    ' Make sure the command doesn't exist
    '
    For Each ctrl In menuTools.Controls
        If ctrl.Caption = "Command &Bar Info" Then
            ctrlExists = True
            Exit For
        End If
    Next ctrl
    '
    ' If the command doesn't exist, add it
    '
    If Not ctrlExists Then
        Set ctrl = menuTools.Controls.Add(Type:=msoControlButton)
        With ctrl
            .Caption = "Command &Bar Info"
            .OnAction = "RunCommandBarInfo"
        End With
    End If
    Set cb = Nothing
End Sub

' This procedure runs the CommandBarInfo utility.
'
Sub RunCommandBarInfo()
    CommandBarInfo.Show
End Sub

' A procedure that runs through Word's CommandBars
' collection and writes the name, type, and
' index number of each command bar.
'
Sub ListWordCommandBars()
    Dim cb As CommandBar
    Dim cbType As String
    
    For Each cb In CommandBars
        Select Case cb.Type
            Case msoBarTypeNormal   '0
                cbType = "Toolbar"
            Case msoBarTypeMenuBar  '1
                cbType = "Menu Bar"
            Case msoBarTypePopup    '2
                cbType = "Shortcut Menu"
        End Select
        With ActiveDocument.Paragraphs(2).Range
            With .ParagraphFormat.TabStops
                .Add Position:=InchesToPoints(2)
                .Add Position:=InchesToPoints(3.5)
            End With
            .InsertAfter cb.Name & vbTab
            .InsertAfter cbType & vbTab
            .InsertAfter cb.Index
            .InsertParagraphAfter
        End With
    Next
    Set cb = Nothing
End Sub

