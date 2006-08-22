Attribute VB_Name = "Chapter14"
' Listing 14.1. Using the AddOLEObject method to
' embed a new object in a Word document.
'
Sub InsertExcelChart()

    Dim msgPrompt As String, msgButtons As Integer
    Dim response As Integer, embeddedeExcelChart As Object

    msgPrompt = "Are you sure you want to insert a new Excel Chart object?"
    msgButtons = vbOKCancel + vbQuestion
    response = MsgBox(msgPrompt, msgButtons)

    If response = vbOK Then
        Application.StatusBar = "Loading Excel Chart..."
        Set embeddedExcelChart = ActiveDocument.Shapes.AddOLEObject _
            (ClassType:="Excel.Chart.8")
        Application.StatusBar = False
    End If
    Set embeddedExcelChart = Nothing
End Sub

' Listing 14.2. Using the AddOLEObject method to
' embed an existing bitmap file into a Word document.
'
Sub EmbedBitmap()
    Dim winFolder As String
    Dim filePath As String
    Dim paintPath As String
    '
    ' Get the Windows folder
    '
    winFolder = Environ("WINDIR")
    '
    ' Build the file's full path name
    '
    If InStr(1, System.OperatingSystem, "NT") Then
        '
        ' Use these values with Windows NT:
        '
        filePath = winFolder & "\winnt256.bmp"
        paintPath = winFolder & "\system32\mspaint.exe"
    Else
        '
        ' Use these values with Windows 95/98:
        '
        filePath = winFolder & "\setup.bmp"
        paintPath = Left(winFolder, 3) & "Program Files\Accessories\mspaint.exe"
    End If
    '
    ' Make sure the file exists
    '
    If Dir(filePath) = "" Then
        MsgBox "The file " & filePath & " doesn't exist!"
        Exit Sub
    End If
    '
    ' Embed the bitmap file
    '
    ActiveDocument.Shapes.AddOLEObject _
        FileName:=filePath, _
        DisplayAsIcon:=True, _
        IconFileName:=paintPath, _
        IconIndex:=1, _
        IconLabel:="A Windows bitmap - Double-click to open"
End Sub

' Listing 14.3. Using the AddOLEObject method to insert
' an existing file as a linked object.
'
Sub InsertPictureAsLinked()
    Dim winFolder As String
    Dim filePath As String
    '
    ' Get the Windows folder
    '
    winFolder = Environ("WINDIR")
    '
    ' Build the file's full path name
    '
    If InStr(1, System.OperatingSystem, "NT") Then
        '
        ' Use these values with Windows NT:
        '
        filePath = winFolder & "\winnt256.bmp"
    Else
        '
        ' Use these values with Windows 95/98:
        '
        filePath = winFolder & "\setup.bmp"
    End If
    '
    ' Make sure the file exists
    '
    If Dir(filePath) = "" Then
        MsgBox "The file " & filePath & " doesn't exist!"
        Exit Sub
    End If
    '
    ' Insert the bitmap file as linked
    '
    ActiveDocument.Shapes.AddOLEObject _
        FileName:=filePath, _
        LinkToFile:=True
End Sub

' Listing 14.4. Using the InlineShapes object's
' AddOLEObject method to embed a picture file as an
' embedded inline image.
'
Sub InsertInlineObject()
    Dim filePath As String
    '
    ' Build the location of the Office sample files
    '
    filePath = Application.Path & "\Samples\"
    '
    ' Make sure the file exists
    '
    If Dir(filePath) = "" Then
        MsgBox "The file " & filePath & " doesn't exist!"
        Exit Sub
    End If
    '
    ' Insert the picture as an inline shape
    '
    With ActiveDocument
        .InlineShapes.AddOLEObject _
            FileName:=filePath & "nwlogo.gif", _
            Range:=.Paragraphs(2).Range
    End With
End Sub

' Listing 14.5. Using the Update method to update
' linked OLE objects.
'
Sub UpdateAllObjects()
    Dim obj As Object

    Application.StatusBar = _
        "Now updating linked objects..."
    For Each obj In ActiveDocument.Shapes
        If obj.Type = msoLinkedOLEObject Then
            obj.LinkFormat.Update
        End If
    Next obj
    
    Application.StatusBar = False
    MsgBox "Link update complete."
    Set obj = Nothing
End Sub



