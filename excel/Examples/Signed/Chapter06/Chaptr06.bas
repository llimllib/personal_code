Attribute VB_Name = "Chapter06"
' Listing 6.1. A procedure that toggles the display of
' nonprinting characters on and off.
'
Sub ToggleNonprinting()

    With ActiveWindow.View
        .ShowAll = Not .ShowAll
    End With

End Sub

' Listing 6.2. A procedure that lists all the format
' names, class names, and save format values for
' Word's installed external file converters.
'
Sub ListFileFormats()
    Dim fc As FileConverter
    Dim lastparagraph As Paragraph
    For Each fc In FileConverters
        With ThisDocument
            Set lastparagraph = .Paragraphs(.Paragraphs.Count)
        End With
        With lastparagraph
            .Range.InsertParagraphAfter
            If fc.CanSave Then
                .Range.Text = fc.FormatName & vbTab & fc.ClassName & _
                              vbTab & fc.SaveFormat
            Else
                .Range.Text = fc.FormatName & vbTab & fc.ClassName & _
                              vbTab & "-"
            End If
        End With
    Next 'fc
End Sub

' Listing 6.3. A procedure that moves the Word window
' into the top left corner of the screen.
'
Sub TopLeftCorner()
    With Application
        If .WindowState <> wdWindowStateMaximize _
            And .WindowState <> wdWindowStateMinimize _
            Then .Move 0, 0
    End With
End Sub

' Listing 6.4. Procedures that create and open
' a workspace of files.
'
' CreateWorkspace()
' Saves the path and filename data of all the
' open files to the Windows Registry. Before
' running this procedure, make sure only the
' files you want in the workspace are open.
'
Sub CreateWorkspace()
    Dim total As Integer
    Dim doc As Document
    Dim i As Integer
    '
    ' Delete the old workspace settings
    ' First, get the total number of files
    '
    total = GetSetting("Word", "Workspace", "TotalFiles", 0)
    For i = 1 To total
        '
        ' Delete each setting
        '
        DeleteSetting "Word", "Workspace", "Document" & i
    Next 'i
    '
    ' Create the new workspace
    '
    i = 0
    For Each doc In Documents
        '
        ' Make sure it's not a new, unsaved file
        '
        If doc.Path <> "" Then
            '
            ' Use i to create unique setting names
            '
            i = i + 1
            '
            ' Save the FullName (path and filename) to the Registry
            '
            SaveSetting "Word", "Workspace", "Document" & i, doc.FullName
        End If
    Next 'doc
    '
    ' Save tht total number of files
    '
    SaveSetting "Word", "Workspace", "TotalFiles", i
End Sub
'
' OpenWorkspace()
' Accesses the Registry's workspace settings
' and then opens each workspace file.
'
Sub OpenWorkspace()
    Dim total As Integer
    Dim i As Integer
    Dim filePath As String
    Dim doc As Document
    Dim fileAlreadyOpen As Boolean
    '
    ' Get the total number of files
    '
    total = GetSetting("Word", "Workspace", "TotalFiles", 0)
    For i = 1 To total
        '
        ' Get the path and filename
        '
        filePath = GetSetting("Word", "Workspace", "Document" & i)
        '
        ' Make sure the file isn't already open
        '
        fileAlreadyOpen = False
        For Each doc In Documents
            If filePath = doc.FullName Then
                fileAlreadyOpen = True
                Exit For
            End If
        Next 'doc
        '
        ' Open it
        '
        If Not fileAlreadyOpen Then
            Documents.Open filePath
        End If
    Next 'i
End Sub

Sub TestStylesAddMethod()
Set newStyle = ActiveDocument.Styles.Add("PageTitle", wdStyleTypeParagraph)
With newStyle
    .Font.Bold = True
    .Font.Underline = True
    .Font.Size = 24
    .Font.Name = "Arial"
    .ParagraphFormat.Alignment = wdAlignParagraphCenter
    .ParagraphFormat.SpaceAfter = 12
    .NextParagraphStyle = wdStyleNormal
End With
End Sub

' Listing 6.5. A procedure that creates a backup copy of
' the active document on a floppy disk.
'
Sub MakeBackup()
    Dim backupFile As String
    Dim currFile As String

    With ActiveDocument
        '
        ' Don't bother if the document is unchanged or new
        '
        If .Saved Or .Path = "" Then Exit Sub
        '
        ' Mark current position in document
        '
        .Bookmarks.Add Name:="LastPosition"
        '
        ' Turn off screen updating
        '
        Application.ScreenUpdating = False
        '
        ' Save the file
        '
        .Save
        '
        ' Store the current file path, construct the path for
        ' the path for the backup file, and then save it to
        ' Drive A
        '
        currFile = .FullName
        backupFile = "A:\" + .Name
        .SaveAs FileName:=backupFile
    End With
    '
    ' Close the backup copy (which is now active)
    '
    ActiveDocument.Close
    '
    ' Reopen the current file
    '
    Documents.Open FileName:=currFile
    '
    ' Return to pre-backup position
    '
    Selection.GoTo what:=wdGoToBookmark, Name:="LastPosition"
End Sub

Sub TestRangeProperties()
    With ThisDocument.Paragraphs(1).Range
        .Bold = True
        .Case = wdTitleWord
    End With
End Sub

' Listing 6.6. A procedure that asks the user
' if the selected text should be replaced.
'
Sub AskToReplaceText()
    Dim result As Integer
    '
    ' Are we at the intertion point or is ReplaceSelection off?
    '
    If Selection.Type = wdSelectionIP Or _
       Not Options.ReplaceSelection Then
       '
       ' If so, just insert the text
       '
       Selection.TypeText "Howdy"
    Else
        '
        ' Otherwise, ask the user about replacing the text
        '
        result = MsgBox("Replace the selected text?", vbYesNo + vbQuestion)
        If result = vbNo Then
            '
            ' Turn off ReplaceSelection to insert without replacing
            '
            Options.ReplaceSelection = False
            Selection.TypeText "Howdy"
            Options.ReplaceSelection = True
        Else
            '
            ' Replace the selected test
            '
            Selection.TypeText "Howdy"
        End If
    End If
End Sub

' Listing 6.7. A function that counts the number of instances
' of a specified character in an object.
'
Function CountCharacters(countObject As Object, letter As String) As Long
    Dim i As Long, char As Range
    i = 0
    For Each char In countObject.Characters
        If char = letter Then i = i + 1
    Next char
    CountCharacters = i
End Function

Sub TestCountCharacters()
    MsgBox CountCharacters(ActiveDocument, "e")
End Sub

' Listing 6.8. A function that counts the number of "real"
' words in an object. Punctuation marks and paragraph
' marks are ignored.
'
Function CountWords(countObject As Object) As Long
    Dim i As Long, word As Range
    i = 0
    For Each word In countObject.Words
        Select Case Asc(Left(word, 1))
            Case 48 To 90, 97 To 122
                i = i + 1
        End Select
    Next 'word
    CountWords = i
End Function

Sub TestCountWords()
    With ActiveDocument
        MsgBox "Words.Count reports " & .Words.Count & Chr(13) & _
               "CountWords reports " & CountWords(.Range)
    End With
End Sub

