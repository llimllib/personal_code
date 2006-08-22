Attribute VB_Name = "Chapter26"
Option Explicit
'
' Type definition used in Listing 26.11
'
Type Quote
    qDate As String
    qOpen As Single
    qHigh As Single
    qLow As Single
    qClose As Single
    qVolume As Long
End Type
'
' This procedure runs a simple test of the
' SaveSetting function.
'
Sub SaveSettingExample()
    SaveSetting "VBA Unleashed", "Chapter 26", "Test", "OK"
End Sub
'
' Listing 26.4. This procedure uses GetAllSettings to return
' every setting in the Chapter 26 subkey, and then prints out
' the setting names and values.
'
Sub GetAllChapter26Settings()
    Dim ch26Settings As Variant
    Dim i As Integer
    '
    ' Get the settings
    '
    ch26Settings = GetAllSettings("VBA Unleashed", "Chapter 26")
    '
    ' Run through the key settings, displaying the name and value
    '
    For i = 0 To UBound(ch26Settings, 1)
        Debug.Print ch26Settings(i, 0); ": "; ch26Settings(i, 1)
    Next 'i
End Sub
'
' Listing 26.5. This procedure reads all the filenames from
' the root folder of drive C, stores them in Sheet1, and
' sorts them by name.
'
Sub GetFilenames()
    Dim i As Integer
    i = 0
    '
    ' Start at cell A1
    '
    With Worksheets("Sheet1").[A1]
        '
        ' Clear the current values, if any
        '
        .CurrentRegion.Clear
        '
        ' Get the initial file and store it in A1
        '
        .Value = UCase$(Dir("C:\", vbNormal))
        '
        ' Get the rest of the files and store them in Column A
        '
        Do While .Offset(i, 0) <> ""
            i = i + 1
            .Offset(i, 0) = UCase$(Dir)
        Loop
        '
        ' Sort the filenames
        '
        .Sort Key1:=Worksheets("Sheet1").Columns("A")
    End With
End Sub
'
' Listing 26.6. This procedure combines Dir and FileLen to
' determine the number of bytes used by the files in a folder.
'
Sub GetFolderUsage()
    Dim folder As String
    Dim filename As String
    Dim totalBytes As Long
    '
    ' Get the folder name
    '
    folder = InputBox("Enter the folder name:", "Bytes Used in Folder")
    '
    ' See if the user clicked Cancel
    
    If folder <> "" Then
        '
        ' Make sure there's a backslash at the end
        '
        If Right(folder, 1) <> "\" Then
            folder = folder & "\"
        End If
        '
        ' Get the first filename
        '
        filename = Dir(folder, vbNormal)
        totalBytes = 0
        '
        ' Loop through the rest of the files
        '
        Do While filename <> ""
            '
            ' Update the total number of bytes
            '
            totalBytes = totalBytes + FileLen(folder & filename)
            '
            ' Get the next filename
            '
            filename = Dir
        Loop
        '
        ' Display the total
        '
        MsgBox "The folder " & folder & " uses " & totalBytes & " bytes."
    End If
End Sub
'
' Listing 26.7. A procedure that prompts for a filename and
' then returns the attributes for the selected file.
'
Sub GetAttributes()
    Dim pathname As String
    Dim attr As Integer
    Dim msg As String
    '
    ' Get the filename
    '
    pathname = Application.GetOpenFilename("All Files (*.*), *.*")
    '
    ' Check to see if Cancel was clicked
    '
    If pathname <> "" Then
        '
        ' Get the file's attributes
        '
        attr = GetAttr(pathname)
        msg = "Attributes for " & pathname & ":" & Chr(13)
        '
        ' Determine the file's attributes and display them
        '
        If attr And vbReadOnly Then msg = msg & Chr(13) & "Read-Only"
        If attr And vbHidden Then msg = msg & Chr(13) & "Hidden"
        If attr And vbSystem Then msg = msg & Chr(13) & "System"
        If attr And vbDirectory Then msg = msg & Chr(13) & "Directory"
        If attr And vbArchive Then msg = msg & Chr(13) & "Archive"
        MsgBox msg
    End If
End Sub
'
' Listing 26.8. This procedure deletes a folder. If the folder
' contains files, the procedure first deletes the files.
'
Sub DeleteFolder()
    Dim folder As String
    Dim filename As String
    Dim totalFiles As Integer
    '
    ' Get the folder name
    '
    folder = InputBox("Enter the name of the folder to delete:")
    '
    ' See if the user clicked Cancel
    '
    If folder <> "" Then
        '
        ' Make sure there's a backslash at the end
        '
        If Right(folder, 1) <> "\" Then
            folder = folder & "\"
        End If
        '
        ' Get the folder
        '
        filename = Dir(folder, vbDirectory)
        '
        ' Bail out if the folder doesn't exist
        '
        If filename = "" Then
            MsgBox "Folder doesn't exist!"
            Exit Sub
        End If
        '
        ' Loop through the rest to get the file total
        '
        totalFiles = 0
        Do While filename <> ""
            '
            ' Get the next filename
            '
            filename = Dir
            '
            ' Ignore the parent (..) and the last Dir
            '
            If filename <> ".." And filename <> "" Then
                '
                ' Update the total number of files
                '
                totalFiles = totalFiles + 1
            End If
        Loop
        '
        ' Check the total
        '
        If totalFiles > 0 Then
            '
            ' If there are files,let the user know
            '
            If MsgBox("The folder " & folder & _
                    " contains " & totalFiles & _
                    IIf(totalFiles > 1, " files.", "file.") & _
                    Chr(13) & _
                    "Are you sure you want to delete it?", _
                    vbOKCancel + vbQuestion) = vbCancel Then
                Exit Sub
            End If
            '
            ' Get the first filename
            '
            filename = Dir(folder, vbNormal)
            '
            ' Loop through and Kill the rest of the files
            '
            Do While filename <> ""
                Kill folder & filename
                '
                ' Get the next filename
                '
                filename = Dir
            Loop
        End If
        '
        ' Delete the folder
        '
        RmDir folder
    End If
End Sub
'
' Listing 26.9. This procedure opens WIN.INI for input, reads the
' lines until the RUN= line is found, and then displays the line.
'
Sub GetRUNLine()
    Dim fn As Integer
    Dim winINI As String
    Dim str As String
    Dim lineExists As Boolean
    '
    ' Get the next free file handle
    '
    fn = FreeFile
    '
    ' Make sure WIN.INI exists
    '
    winINI = "C:\Windows\Win.ini"
    If Dir(winINI) <> "" Then
        '
        ' If it does, open it
        '
        Open winINI For Input As #fn
        '
        ' Read the file line-by-line until the end
        '
        lineExists = False
        Do While Not EOF(fn)
            '
            ' Store each line in the str variable
            '
            Line Input #fn, str
            '
            ' Check the beginning of each line for "RUN"
            '
            If Left(UCase$(str), 3) = "RUN" Then
                '
                ' If we find it, bail out of the loop
                '
                lineExists = True
                Exit Do
            End If
        Loop
        '
        ' Close the file
        '
        Close #fn
        '
        ' Display the message
        '
        If lineExists Then
            MsgBox "Here's the RUN= line from WIN.INI:" & _
                   Chr(13) & Chr(13) & str
        Else
            MsgBox "The RUN line was not found!"
        End If
    End If
End Sub
'
' Listing 26.11. This procedure opens MSFT.TXT (supplied on the CD)
' for input, and reads the line items into variables.
'
Sub ReadStockQuotes()
    Dim fn As Integer
    Dim i As Integer
    Dim q As Quote
    '
    ' Get a free handle and then open MSFT.txt (the code
    ' assume this file is in the workbook's folder)
    '
    fn = FreeFile
    Open ThisWorkbook.Path & "\MSFT.txt" For Input As #fn
    i = 1
    With Worksheets("Stock Quotes").[A1]
        '
        ' Clear any existing quotes
        '
        .CurrentRegion.Offset(1).Clear
        '
        ' Loop through the entire file
        '
        Do While Not EOF(fn)
            '
            ' Read the data into the Quote structure
            '
            Input #fn, q.qDate, q.qOpen, q.qHigh, q.qLow, q.qClose, q.qVolume
            '
            ' Write it to the Stock Quotes worksheet
            '
            .Offset(i, 0) = CDate(Mid(q.qDate, 3, 2) & "/" & Right(q.qDate, 2) & "/" & Left(q.qDate, 2))
            .Offset(i, 1) = q.qOpen
            .Offset(i, 2) = q.qHigh
            .Offset(i, 3) = q.qLow
            .Offset(i, 4) = q.qClose
            .Offset(i, 5) = q.qVolume
            i = i + 1
        Loop
    End With
    '
    ' Close the file
    '
    Close #fn
End Sub
'
' Listing 26.13. A procedure that tests the
' difference between an optimized and an unoptimized loop.
'
Sub TimingTest()
    Dim i As Long, currCell As Variant
    Dim start1 As Long, finish1 As Long
    Dim start2 As Long, finish2 As Long
    '
    ' Start timing the unoptimized loop
    '
    start1 = Timer
    For i = 1 To 50000
        Application.StatusBar = "The value is " & Worksheets("Sheet1").[A1].Value
    Next i
    finish1 = Timer
    '
    ' Start timing the optimized loop
    '
    start2 = Timer
    currCell = Worksheets("Sheet1").[A1].Value
    For i = 1 To 50000
        Application.StatusBar = "The value is " & currCell
    Next i
    finish2 = Timer

    MsgBox "The first loop took " & finish1 - start1 & " seconds." & _
           Chr(13) & _
           "The second loop took " & finish2 - start2 & " seconds."

    Application.StatusBar = False
End Sub


