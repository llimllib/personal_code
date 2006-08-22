Attribute VB_Name = "Chapter15"
' Listing 15.1. Using Automation to create a new Word document.
'
Sub CreateWordDocument()
    Dim wordApp As word.Application
    '
    ' Create the Word Application object
    '
    Application.StatusBar = "Creating Word Application object..."
    Set wordApp = CreateObject("Word.Application")
    '
    ' Work with Word's Application object directly
    '
    With wordApp
        '
        ' Create a new document and add some text
        '
        Application.StatusBar = "Creating new Word document..."
        .Documents.Add
        .ActiveDocument.Paragraphs(1).Range.InsertBefore "This is an Automation test."
        '
        ' Save the document
        '
        Application.StatusBar = "Saving Word document..."
        .ActiveDocument.SaveAs "C:\My Documents\OLETest.doc"
        '
        ' We're done, so quit Word
        '
        Application.StatusBar = "Shutting down Word..."
        .Quit
    End With
    Set wordApp = Nothing
    Application.StatusBar = False
End Sub

' Listing 15.2. Using Automation to work with a Word document.
'
Sub DocumentWordCount()
    Dim wordDoc As word.Document
    '
    ' Get the Word Document object
    '
    Application.StatusBar = "Getting Word Document object..."
    Set wordDoc = GetObject("C:\My Documents\OLETest.doc", "Word.Document")
    '
    ' Get the word count
    '
    Application.StatusBar = "Getting word count..."
    MsgBox wordDoc.Name & " has " & wordDoc.Words.Count & " words."
    '
    ' We're done, so quit Word and release object variable
    '
    Application.StatusBar = "Shutting down Word..."
    wordDoc.Application.Quit
    Set wordDoc = Nothing
    Application.StatusBar = False
End Sub

' Listing 15.3. Using Automation to run a PowerPoint
' presentation slide show.
'
Sub RunPresentation()
    On Error GoTo OpenPowerPoint
    Dim ppApp As PowerPoint.Application
    '
    ' Reference the existing PowerPoint Application object
    '
    Set ppApp = GetObject(, "PowerPoint.Application")
    '
    ' Work with PowerPoint's Application object directly
    '
    With ppApp
        '
        ' Display PowerPoint
        '
        .Visible = True
        '
        ' Open and then run the presentation's slide show
        '
        .Presentations.Open "C:\My Documents\Juggling.ppt"
        .Presentations("Juggling.ppt").SlideShowSettings.Run
    End With
    Set ppApp = Nothing
'
' Program branches here if PowerPoint isn't running
'
OpenPowerPoint:
    ' Create a new instance of PowerPoint's Application object
    '
    Set ppApp = CreateObject("PowerPoint.Application")
    '
    ' Continue after the statement that caused the error
    '
    Resume Next
End Sub

