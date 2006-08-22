Attribute VB_Name = "Chapter04"
' Listing 4.1. A procedure that formats a range.
'
Sub FormatParagraph()
    ThisDocument.Paragraphs(1).Style = "Heading 1"
    ThisDocument.Paragraphs(1).Alignment = wdAlignParagraphCenter
    ThisDocument.Paragraphs(1).Range.Font.Size = 16
    ThisDocument.Paragraphs(1).Range.Font.Bold = True
    ThisDocument.Paragraphs(1).Range.Font.Color = RGB(255, 0, 0) ' Red
    ThisDocument.Paragraphs(1).Range.Font.Name = "Times New Roman"
End Sub

' Listing 4.2. A more efficient version of FormatParagraph().
'
Sub FormatParagraph2()
    With ThisDocument.Paragraphs(1)
        .Style = "Heading 1"
        .Alignment = wdAlignParagraphCenter
        .Range.Font.Size = 16
        .Range.Font.Bold = True
        .Range.Font.Color = RGB(255, 0, 0) ' Red
        .Range.Font.Name = "Times New Roman"
    End With
End Sub

' Listing 4.3. A procedure that checks the spelling of
' an entered word.
'
Sub SpellCheckTest()
    Dim word2Check As String, result As Boolean
    '
    ' Get the word from the user
    '
    word2Check = InputBox("Enter a word:")
    '
    ' Spell-check it
    '
    result = Application.CheckSpelling(word2Check)
    '
    ' Display the result to the user
    '
    If result = True Then
        MsgBox "'" & word2Check & "' is spelled correctly!"
    Else
        MsgBox "Oops! '" & word2Check & "' is spelled incorrectly."
    End If
End Sub

