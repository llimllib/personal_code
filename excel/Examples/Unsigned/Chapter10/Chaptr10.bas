Attribute VB_Name = "Chapter10"
' Listing 10.1. A procedure that runs another procedure
' and then beeps the speaker three times.
'

Sub InsertHeadingHyperlinks()
    Dim i As Integer
    '
    ' Run the InsertHyperlink procedure
    '
    InsertHyperlinks "Heading 3"
    '
    ' Signal the end of the procedure with 3 beeps
    '
    For i = 1 To 3
        Beep
    Next 'i
End Sub

' Listing 10.2. A procedure that inserts hyperlinks
' for a specified style of heading.
'
Sub InsertHyperlinks(heading As String)
    Dim b As Bookmark
    Dim p As Paragraph
    Dim lastParagraph As Paragraph
    Dim anchorText As Range
    Dim strBookmark As String
    Dim totalParagraphs As Integer
    Dim i As Integer
    Dim j As Integer
    i = 0
    j = 0
    With ActiveDocument
        '
        ' Delete the existing "Anchor" bookmarks
        '
        For Each b In .Bookmarks
            If InStr(b.Name, "Anchor") Then b.Delete
        Next 'b
        '
        ' Run through the paragraphs
        '
        totalParagraphs = .Paragraphs.Count
        For Each p In .Paragraphs
            '
            ' Display the progress in the status bar
            '
            j = j + 1
            Application.StatusBar = "Checking paragraph " _
                & j & " of " & totalParagraphs
            '
            ' Look for the specified style
            '
            If p.Style = heading Then
                '
                ' Create a bookmark
                '
                i = i + 1
                .Bookmarks.Add "Anchor" & i, p.Range
                '
                ' Add a hyperlink for the heading
                '
                Set lastParagraph = .Paragraphs(.Paragraphs.Count)
                Set anchorText = .Range(p.Range.Start, p.Range.End - 1)
                lastParagraph.Range.InsertParagraphAfter
                lastParagraph.Range.Hyperlinks.Add _
                    Anchor:=lastParagraph.Range, _
                    Address:="", _
                    SubAddress:="Anchor" & i, _
                    ScreenTip:="", _
                    TextToDisplay:=anchorText
                
            End If
        Next 'p
        Application.StatusBar = ""
    End With
End Sub

Sub MsgBoxTest()
    MsgBox "You must enter a number between 1 and 100!", , "Warning"
End Sub

' Listing 10.5. A procedure that creates a message dialog box.
'
Sub ButtonTest()

    Dim msgPrompt As String, msgTitle As String
    Dim msgButtons As Integer, msgResult As Integer

    msgPrompt = "Are you sure you want to insert" & Chr(13) & _
                "the heading hyperlinks?"
    msgButtons = vbYesNo + vbQuestion + vbDefaultButton2
    msgTitle = "Insert Heading Hyperlinks"

    msgResult = MsgBox(msgPrompt, msgButtons, msgTitle)

End Sub

' Listing 10.6. This example uses Select Case to test the
' return value of the MsgBox function.
'
Sub ButtonTest2()

    Dim msgPrompt As String, msgTitle As String
    Dim msgButtons As Integer, msgResult As Integer

    msgPrompt = "Are you sure you want to insert" & Chr(13) & _
                "the heading hyperlinks?"
    msgButtons = vbYesNo + vbQuestion + vbDefaultButton2
    msgTitle = "Insert Heading Hyperlinks"

    msgResult = MsgBox(msgPrompt, msgButtons, msgTitle)

    If msgResult = vbYes Then
        InsertHyperlinks "Heading 3"
    Else
        Exit Sub
    End If

End Sub

Sub GetInterestRateTester()
    Dim ir As Single
    ir = GetInterestRate
    If ir > 0 Then MsgBox "You entered " & ir * 100 & "%."
End Sub

' Listing 10.7. A procedure that prompts the user for an
' interest rate value.
'
Function GetInterestRate()

    Dim done As Boolean
    '
    ' Initialize the loop variable
    '
    done = False

    While Not done
        '
        ' Get the interest rate
        '
        GetInterestRate = InputBox( _
                          Prompt:="Enter an interest rate between 0 and 1:", _
                          Title:="Enter Interest Rate")
        '
        ' First, check to see if the user cancelled
        '
        If GetInterestRate = "" Then
            GetInterestRate = 0
            Exit Function
        Else
            '
            ' Now make sure the entered rate is betwen 0 and 1
            '
            If GetInterestRate >= 0 And GetInterestRate <= 1 Then
                done = True
            End If
        End If
    Wend

End Function

' Listing 10.8. A function procedure that uses Word's
' Word Count dialog box to get the total number of
' words in the active document.
'
Function CountDocumentWords() As Long
    With Dialogs(wdDialogToolsWordCount)
        .Execute
        CountDocumentWords = .Words
    End With
End Function

Sub DisplayWordCount()
    MsgBox "This document contains " & CountDocumentWords & " words."
End Sub
