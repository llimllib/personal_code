Attribute VB_Name = "Chapter20"
Option Explicit
'
' Listing 20.1. Procedures that publish various Excel objects.
'
' Publish an AutoFilter range
'
Sub PublishAutoFilterRange()
    Sheets("Query").AutoFilterMode = False
    Range("Query_From_Northwind").Select
    Range("Query_From_Northwind").AutoFilter _
        Field:=1, _
        Criteria1:="Condiments"
    ThisWorkbook.PublishObjects.Add( _
        SourceType:=xlSourceAutoFilter, _
        Filename:="C:\Publish01.htm", _
        Sheet:="Query", _
        Source:=Sheets("Query").AutoFilter.Range, _
        HtmlType:=xlHtmlStatic, _
        DivID:="ExcelAutoFilter", _
        Title:="Excel AutoFilter Range").Publish True
End Sub
'
' Publish a chart sheet
'
Sub PublishChartSheet()
    ThisWorkbook.PublishObjects.Add( _
        SourceType:=xlSourceChart, _
        Filename:="C:\Publish02.htm", _
        Sheet:="Chart1", _
        HtmlType:=xlHtmlChart, _
        DivID:="ExcelChartSheet", _
        Title:="Excel Chart Sheet").Publish True
End Sub
'
' Publish an embedded chart
'
Sub PublishEmbeddedChart()
    ThisWorkbook.PublishObjects.Add( _
        SourceType:=xlSourceChart, _
        Filename:="C:\Publish03.htm", _
        Sheet:="2000 Budget", _
        Source:="Chart 1", _
        HtmlType:=xlHtmlStatic, _
        DivID:="ExcelEmbeddedChart", _
        Title:="Excel Embedded Chart").Publish True
End Sub
'
' Publish a PivotTable
'
Sub PublishPivotTable()
    ThisWorkbook.PublishObjects.Add( _
        SourceType:=xlSourcePivotTable, _
        Filename:="C:\Publish04.htm", _
        Sheet:="PivotTable", _
        Source:="PivotTable1", _
        HtmlType:=xlHtmlList, _
        DivID:="ExcelPivotTable", _
        Title:="Excel PivotTable").Publish True
End Sub
'
' Publish a print area
'
Sub PublishPrintArea()
    Worksheets("2000 Budget").PageSetup.PrintArea = "A1:B13"
    ThisWorkbook.PublishObjects.Add( _
        SourceType:=xlSourcePrintArea, _
        Filename:="C:\Publish05.htm", _
        Sheet:="2000 Budget", _
        Source:=Worksheets("2000 Budget").PageSetup.PrintArea, _
        HtmlType:=xlHtmlStatic, _
        DivID:="ExcelPrintArea", _
        Title:="Excel Print Area").Publish True
End Sub
'
' Publish a query table
'
Sub PublishQueryTable()
    ThisWorkbook.PublishObjects.Add( _
        SourceType:=xlSourceQuery, _
        Filename:="C:\Publish06.htm", _
        Sheet:="Query", _
        Source:="Query from Northwind", _
        HtmlType:=xlHtmlCalc, _
        DivID:="ExcelQueryTable", _
        Title:="Excel Query Table").Publish True
End Sub
'
' Publish a range using coordinates
'
Sub PublishRangeCoordinates()
    ThisWorkbook.PublishObjects.Add( _
        SourceType:=xlSourceRange, _
        Filename:="C:\Publish07.htm", _
        Sheet:="2000 Budget", _
        Source:="A1:B17", _
        HtmlType:=xlHtmlCalc, _
        DivID:="ExcelRangeCoordinates", _
        Title:="Excel Range Coordinates").Publish True
End Sub
'
' Publish a range using a name
'
Sub PublishRangeName()
    ThisWorkbook.PublishObjects.Add( _
        SourceType:=xlSourceRange, _
        Filename:="C:\Publish08.htm", _
        Source:="Expenses", _
        HtmlType:=xlHtmlStatic, _
        DivID:="ExcelRangeName", _
        Title:="Excel Range Name").Publish True
End Sub
'
' Publish a worksheet
'
Sub PublishWorksheet()
    ThisWorkbook.PublishObjects.Add( _
        SourceType:=xlSourceSheet, _
        Filename:="C:\Publish09.htm", _
        Sheet:="2000 Budget", _
        HtmlType:=xlHtmlCalc, _
        DivID:="ExcelWorksheet", _
        Title:="Excel Worksheet").Publish True
End Sub
'
' Listing 20.2. Republishing a PublishObject.
'
Sub RepublishObject()
    Dim strID As String
    Dim po As PublishObject
    strID = "ExcelRangeCoordinates"
    For Each po In ThisWorkbook.PublishObjects
        If po.DivID = strID Then
            po.Publish
            Exit For
        End If
    Next 'po
End Sub
'
' Listing 20.3. A procedure that deletes all the PublishObjects
'
Sub DeletePublishObjects()
    Dim po As PublishObject
    For Each po In ThisWorkbook.PublishObjects
        po.Delete
    Next 'po
End Sub
'
' Listing 20.4. A procedure that adds a Hyperlink object.
'
Sub AddLink()
    Dim r As Range
    '
    ' Add a paragraph to the end of the document
    '
    With ThisDocument.Paragraphs
        .Item(.Count).Range.InsertParagraphAfter
        Set r = .Item(.Count).Range
    End With
    r.Text = "Sams' Home Page"
    r.Hyperlinks.Add _
        Anchor:=r, _
        Address:="http://www.mcp.com/sams/", _
        ScreenTip:="Click here to visit the home page of Sams!"
End Sub
'
' Listing 20.5. Procedures that add a link for the Yahoo!
' search engine and run a query on the Yahoo! database.
'
Sub AddYahoo()
    Dim r As Range
    '
    ' Add a paragraph to the end of the document
    '
    With ThisDocument.Paragraphs
        .Item(.Count).Range.InsertParagraphAfter
        Set r = .Item(.Count).Range
    End With
    r.Text = "Yahoo Search"
    r.Hyperlinks.Add _
        Anchor:=r, _
        Address:="http://search.yahoo.com/bin/search"
End Sub

Sub SearchYahoo()
    Dim link As Hyperlink
    Dim keyword As String
    Set link = ThisDocument.Hyperlinks("http://search.yahoo.com/bin/search")
    keyword = InputBox("Enter a search keyword:")
    link.Follow _
        ExtraInfo:="p=" & keyword, _
        Method:=msoMethodGet
End Sub
'
' Listing 20.6. Using the FollowHyperlink method to display
' a target document without an existing Hyperlink object.
'
Sub FollowHyperlinkTest()
    Dim keyword As String
    keyword = InputBox("Enter a search keyword:")
    ThisDocument.FollowHyperlink _
        Address:="http://search.yahoo.com/bin/search", _
        ExtraInfo:="p=" & keyword, _
        Method:=msoMethodGet
End Sub
'
' Listing 20.7. Some event handlers that are used to
' display a Web page.
'
' This event handler fires when you first open the form
'
Private Sub UserForm_Initialize()
    Dim maxWidth As Integer
    Dim maxHeight As Integer
    With webWWW
        '
        ' Display and save the initial URL
        '
        If txtLocation <> "" Then
            topPage = txtLocation
            .Navigate txtLocation
        End If
        '
        ' Adjust the width and height of the control
        '
        maxWidth = Me.Width - .Left - 10
        maxHeight = Me.Height - .Top - 20
        If Application.UsableWidth > maxWidth Then
            .Width = maxWidth
        End If
        If Application.UsableHeight > maxHeight Then
            .Height = maxHeight
        End If
    End With
End Sub
'
' This event handler fires when you enter the text box
'
Private Sub txtLocation_Enter()
    '
    ' Make sure Surf! button is the default
    '
    cmdSurf.Default = True
End Sub
'
' This event handler fires when you click the Surf! button
'
Private Sub cmdSurf_Click()
    '
    ' Surf to the URL specified in the Location text box
    '
    If txtLocation <> "" Then
        webWWW.Navigate txtLocation
    Else
        txtLocation.SetFocus
        Beep
    End If
End Sub
'
' This event handler fires once the Web page navigation is done
'
Private Sub webWWW_DocumentComplete(ByVal pDisp As Object, URL As Variant)
    lblProgress.Caption = " Done"
    txtLocation = URL
End Sub
'
' This event handler fires at the start of the download
'
Private Sub webWWW_DownloadBegin()
    lblProgress.Caption = " Downloading..."
End Sub
'
' This event handler fires when the URL title changes
'
Private Sub webWWW_TitleChange(ByVal Text As String)
    '
    ' Update the form's caption to reflect the new title
    '
    Me.Caption = "The Word Wide Web - " & webWWW.LocationName
End Sub
'
' This event handler fires when the status text changes
'
Private Sub webWWW_StatusTextChange(ByVal Text As String)
    lblStatus = Text
End Sub
'
' Listing 20.8. Event handlers for the navigation
' buttons in the custom Web browser.
'
'
' This event handler fires when you click the Back button
'
Private Sub cmdBack_Click()
    '
    ' An error occurs if there is no page to go back to
    '
    On Error Resume Next
    webWWW.GoBack
End Sub
'
' This event handler fires when you click the Forward button
'
Private Sub cmdForward_Click()
    '
    ' An error occurs if there is no page to go forward to
    '
    On Error Resume Next
    webWWW.GoForward
End Sub
'
' This event handler fires when you click the Top button
'
Private Sub cmdTop_Click()
    webWWW.Navigate topPage
End Sub
'
' This event handler fires when you click the Refresh button
'
Private Sub cmdRefresh_Click()
    webWWW.Refresh
End Sub
'
' This event handler fires when you click the Stop button
'
Private Sub cmdStop_Click()
    webWWW.Stop
End Sub
'
' This event handler fires when you click the Home button
'
Private Sub cmdHome_Click()
    webWWW.GoHome
End Sub
'
' This event handler fires when you click the Search button
'
Private Sub cmdSearch_Click()
    webWWW.GoSearch
End Sub
'
' This event handler fires when you click the Exit button
'
Private Sub cmdExit_Click()
    Unload Me
End Sub
'
' Listing 20.9. A procedure that manipulates Internet Explorer
' via Automation using various members of the
' InternetExplorer class.
'
Sub AutomateInternetExplorer()
    Dim ie As Object
    Dim result As Integer
    '
    ' Set up the Automation object
    '
    Set ie = CreateObject("InternetExplorer.Application")
    '
    ' Navigate to a page and customize the browser window
    '
    ie.Navigate "http://www.microsoft.com/ie/"
    ie.Toolbar = False
    ie.StatusBar = False
    ie.MenuBar = False
    '
    ' Twiddle thumbs while the page loads
    '
    Do While ie.Busy
        DoEvents
    Loop
    '
    ' Display page info
    '
    result = MsgBox( _
        "Current URL:  " & ie.LocationURL & Chr(13) & _
        "Current Title: " & ie.LocationName & Chr(13) & _
        "Document type: " & ie.Type & Chr(13) & Chr(13) & _
        "Would you like to view this document?", _
        vbYesNo + vbQuestion)
    If result = vbYes Then
        '
        ' If Yes, make browser visible and activate it
        '
        ie.Visible = True
        AppActivate "Microsoft Internet Explorer"
    Else
        '
        ' If no, bail out
        '
        ie.Quit
    End If
    Set ie = Nothing
End Sub
