VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmBackup 
   Caption         =   "Backup"
   ClientHeight    =   4425
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5880
   OleObjectBlob   =   "Chaptr28.frx":0000
   StartUpPosition =   2  'CenterScreen
End
Attribute VB_Name = "frmBackup"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
'
' Global variables
'
Dim currDestDrive As String ' Holds the current destination disk drive
Dim initializing As Boolean ' True during the Initialize event
Dim nextReminder As Date    ' The time of the next scheduled reminder
Dim arrDocs() As BackupDoc  ' This array holds info for all the open documents
'                             The Type definition is in the Backup module
'
' Listing 28.1. Use the Initialize event handler to set up the form each time it's displayed
'
Private Sub UserForm_Initialize()
    Dim doc As Workbook ' Loop object
    Dim i As Integer    ' Counter
    Dim str As String   ' Temporary string variable
    
    initializing = True
    '
    ' Build the list of recently-used folders
    '
    With cboDestination
        For i = 0 To 9
            str = GetSetting("VBA Unleashed", "Backup", "MRUFolder" & i, "")
            If str <> "" Then .AddItem str
        Next 'i
        If .ListCount > 0 Then .ListIndex = 0
    End With
    '
    ' Create the document array
    '
    ReDim arrDocs(Workbooks.Count)
    i = 0
    For Each doc In Workbooks
        arrDocs(i).Name = doc.Name
        arrDocs(i).Path = doc.Path
        '
        ' Make sure we're not dealing with a new, unsaved file
        '
        If doc.Path <> "" Then
            If doc.Saved = True Then
                arrDocs(i).State = "Saved"
            Else
                arrDocs(i).State = "Unsaved"
            End If
            arrDocs(i).Size = FileLen(doc.FullName)
        Else
            arrDocs(i).State = "Unsaved"
            arrDocs(i).Size = 0
        End If
        i = i + 1
    Next 'doc
    '
    ' Build the document list
    '
    Call BuildDocList
    '
    ' Now that we have the documents list, get the setting
    ' that determines which documents are selected. Activating
    ' an option will fire that option's Click event.
    '
    str = GetSetting("VBA Unleashed", "Backup", "BackupFilter", "All")
    Select Case str
        Case "All"
            optAll = True
        Case "Unsaved"
            optUnsaved = True
        Case "Active"
            optActive = True
        Case "Selected"
            optSelected = True
            lstDocs.Enabled = True
            Call ParseSelectedFiles
    End Select
    '
    ' Get the check box values from the Registry
    '
    chkIgnoreNew = GetSetting("VBA Unleashed", "Backup", "IgnoreNewAndUnsaved", True)
    chkOverwrite = GetSetting("VBA Unleashed", "Backup", "OverwriteDestFiles", True)
    '
    ' Set the Backup Mode option
    '
    If GetSetting("VBA Unleashed", "Backup", "BackupMode", "Manual") = "Manual" Then
        optManual = True
    Else
        optReminder = True
    End If
    '
    ' Get the Reminder interval settings
    '
    txtIntervalHours = GetSetting("VBA Unleashed", "Backup", "IntervalHours", "0")
    txtIntervalMinutes = GetSetting("VBA Unleashed", "Backup", "IntervalMinutes", "30")
    txtIntervalSeconds = GetSetting("VBA Unleashed", "Backup", "IntervalSeconds", "0")
    nextReminder = TimeValue(GetSetting("VBA Unleashed", "Backup", "NextReminder", "12:00:00 AM"))
    '
    ' Finished initializing, so display data
    '
    initializing = False
    Call DisplayDestinationData
End Sub
'
'============================
' SOURCE PAGE EVENT HANDLERS
'============================
'
' Listing 28.2. The Click event handler for the "All Open Documents" option button
'
Private Sub optAll_Click()
    Dim i As Integer    ' Counter
    '
    ' Set Selected property to True for all documents.
    '
    For i = 0 To UBound(arrDocs) - 1
        '
        ' Check to see if the "Ignore New, Unsaved
        ' Documents" check box is activated.
        '
        If chkIgnoreNew Then
            If arrDocs(i).Path <> "" Then
                arrDocs(i).Selected = True
            End If
        Else
            arrDocs(i).Selected = True
        End If
    Next 'i
    '
    ' Disable and rebuild the list
    '
    lstDocs.Enabled = False
    Call BuildDocList
End Sub
'
' Listing 28.3. The Click event handler for the "Unsaved Open Documents Only" option button
'
Private Sub optUnsaved_Click()
    Dim i As Integer    ' Counter
    '
    ' Set Selected property to True for all documents
    ' that have State = "Unsaved"
    '
    For i = 0 To UBound(arrDocs) - 1
        With arrDocs(i)
            If .State = "Unsaved" Then
                '
                ' Check to see if the "Ignore New,
                ' Unsaved Documents" check box is activated.
                '
                If chkIgnoreNew Then
                    If .Path <> "" Then
                        .Selected = True
                    End If
                Else
                    .Selected = True
                End If
            Else
                .Selected = False
            End If
        End With
    Next 'i
    '
    ' Disable and rebuild the list
    '
    lstDocs.Enabled = False
    Call BuildDocList
End Sub
'
' Listing 28.4. The Click event handler for the "Active Document Only" option button
'
Private Sub optActive_Click()
    Dim i As Integer    ' Counter
    '
    ' Set Selected property to True for active document
    '
    For i = 0 To UBound(arrDocs) - 1
        With arrDocs(i)
            If .Name = ActiveWorkbook.Name Then
                '
                ' Check to see if the "Ignore New,
                ' Unsaved Documents" check box is activated.
                '
                If chkIgnoreNew Then
                    If .Path <> "" Then
                        .Selected = True
                    End If
                Else
                    .Selected = True
                End If
            Else
                .Selected = False
            End If
        End With
    Next 'i
    '
    ' Disable and rebuild the list
    '
    lstDocs.Enabled = False
    Call BuildDocList
End Sub
'
' Listing 28.5. The Click event handler for the "Selected Open Documents Only" option button
'
Private Sub optSelected_Click()
    lstDocs.Enabled = True
End Sub
'
' Listing 28.6. The Change event handler for the list of documents
'
Private Sub lstDocs_Change()
    Dim i As Integer    ' Counter
    '
    ' Avoid this event while we're populating
    ' the list box during the Initialize event
    '
    If initializing Then Exit Sub
    With lstDocs
        '
        ' Run through the document array
        '
        For i = 0 To UBound(arrDocs) - 1
            '
            ' Look for the array document that has the
            ' same name as the selected list item
            '
            If arrDocs(i).Name = .List(.ListIndex, 0) Then
                '
                ' Adjust its Selected property
                '
                arrDocs(i).Selected = .Selected(.ListIndex)
            End If
        Next 'i
    End With
    Call DisplaySourceData
End Sub
'
' Listing 28.7. The Click event handler for the "Ignore New, Unsaved Documents" check box
'
Private Sub chkIgnoreNew_Click()
    Dim i As Integer
    If initializing Then Exit Sub
    '
    ' Run through the document array
    '
    For i = 0 To UBound(arrDocs) - 1
        '
        ' Look for array documents with no path
        '
        If arrDocs(i).Path = "" Then
            '
            ' Adjust its Selected property
            '
            If chkIgnoreNew = True Then
                '
                ' Selected is always False here
                '
                arrDocs(i).Selected = False
            Else
                '
                ' Selected depends on current option
                '
                If optAll Or optUnsaved Then
                    '
                    ' Select it if option is All or Unsaved
                    '
                    arrDocs(i).Selected = True
                ElseIf optActive And arrDocs(i).Name = ActiveWorkbook.Name Then
                    '
                    ' If option is Active, only select if doc is active
                    '
                    arrDocs(i).Selected = True
                End If
            End If
        End If
    Next 'i
    '
    ' Redisplay the list of documents
    '
    Call BuildDocList
End Sub
'
'=================================
' DESTINATION PAGE EVENT HANDLERS
'=================================
'
' Listing 28.8. The BeforeUpdate event handler for the Back Up To list box
'
Private Sub cboDestination_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)
    Dim i As Integer    ' Counter
    
    With cboDestination
        '
        ' Make sure the path ends with "\"
        '
        If Right(.Value, 1) <> "\" Then .Value = .Value & "\"
        '
        ' See if the current value is already in the list
        '
        For i = 0 To .ListCount - 1
            If .Value = .List(i) Then
                '
                ' Found it, so just redisplay the drive data
                '
                Call DisplayDestinationData
                Exit Sub
            End If
        Next 'i
        '
        ' Display the drive data. DisplayDestinationData only returns
        ' True if the destination drive is available or otherwise valid.
        '
        If DisplayDestinationData = True Then
            '
            ' Check the destination folder
            '
            If Dir(.Value) <> "" Then
                '
                ' Destination folder exists, so add it to the list
                '
                .AddItem .Value
            End If
        End If
    End With
End Sub
'
' Listing 28.9. The Click event handler for the Back Up To list box
'
Private Sub cboDestination_Click()
    If Not initializing Then
        '
        ' Display the data for the selected destination
        '
        DisplayDestinationData
    End If
End Sub
'
'==========================
' MODE PAGE EVENT HANDLERS
'==========================
'
' The Click event handler for the Manual option button
'
Private Sub optManual_Click()
    lblModeDesc = "Manual Mode: Backs up the selected files and then unloads the Backup dialog box."
    txtIntervalHours.Enabled = False
    txtIntervalMinutes.Enabled = False
    txtIntervalSeconds.Enabled = False
End Sub
'
' Listing 28.11. The Click event handler for the Reminder option button
'
Private Sub optReminder_Click()
    lblModeDesc = "Reminder Mode: Backs up the selected files and then redisplays the Backup dialog box at the scheduled time."
    txtIntervalHours.Enabled = True
    txtIntervalMinutes.Enabled = True
    txtIntervalSeconds.Enabled = True
End Sub
'
' Listing 28.12. This event handler checks the value of the Hour(s) text box
'
Private Sub txtIntervalHours_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)
    With txtIntervalHours
        If .Value = "" Then
            .Value = 0
        ElseIf CInt(.Value) < 0 Then
            .Value = 0
        End If
    End With
End Sub
'
' Listing 28.13. This event handler checks the value of the Minute(s) text box
'
Private Sub txtIntervalMinutes_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)
    With txtIntervalMinutes
        If .Value = "" Then
            .Value = 0
        ElseIf CInt(.Value) < 0 Then
            .Value = 0
        ElseIf CInt(.Value) >= 60 Then
            txtIntervalHours = txtIntervalHours + Int(CInt(.Value) / 60)
            .Value = .Value Mod 60
        End If
    End With
End Sub
'
' Listing 28.14. This event handler checks the value of the Second(s) text box
'
Private Sub txtIntervalSeconds_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)
    With txtIntervalSeconds
        If .Value = "" Then
            .Value = 0
        ElseIf CInt(.Value) < 0 Then
            .Value = 0
        ElseIf CInt(.Value) >= 60 Then
            txtIntervalMinutes = txtIntervalMinutes + Int(CInt(.Value) / 60)
            .Value = .Value Mod 60
        End If
    End With
End Sub
'
'===============================
' COMMAND BUTTON EVENT HANDLERS
'===============================
'
' Listing 28.15. The Click event handler for the Back Up button
'
Private Sub cmdBackUp_Click()
    '
    ' Make sure at least one interval box has data
    '
    If optReminder And _
       txtIntervalHours = 0 And _
       txtIntervalMinutes = 0 And _
       txtIntervalSeconds = 0 Then
            MsgBox "Please enter a Reminder interval!"
            Exit Sub
    End If
    '
    ' Run the backup
    '
    Call BackUpSelectedFiles
    '
    ' Process the Backup Mode
    '
    Call ProcessBackupMode
    '
    ' Save the settings and then dump the form
    '
    Call SaveRegistrySettings
    Unload Me
End Sub
'
' Listing 28.16. The Click event handler for the Save Settings button
'
Private Sub cmdSaveSettings_Click()
    '
    ' Process the Backup Mode
    '
    Call ProcessBackupMode
    '
    ' Save the settings and shut down the form
    '
    Call SaveRegistrySettings
    Unload Me
End Sub
'
' Listing 28.17. The Click event handler for the Cancel button
'
Private Sub cmdCancel_Click()
    '
    ' Close the form
    '
    Unload Me
End Sub
'
'====================
' SUPPORT PROCEDURES
'====================
'
' Listing 28.18. This procedure parses the Registry's list of selected files
'
Private Sub ParseSelectedFiles()
    Dim filenames As String ' Temporary string buffer
    Dim filename As String  ' Temporary string buffer
    Dim start As Long       ' Parse: start position
    Dim scPos As Integer    ' Parse: semi-colon position
    Dim i As Integer        ' Counter
    '
    ' Get the list of selected files from the Registry
    '
    filenames = GetSetting("VBA Unleashed", "Backup", "SelectedDocuments", "")
    '
    ' Parse the filenames from the string
    ' String structure is "filename1;filename2;..."
    '
    If filenames <> "" Then
        '
        ' Set the initial start and end points
        '
        start = 1
        scPos = InStr(filenames, ";")
        Do While scPos <> 0
            '
            ' Extract the filename
            '
            filename = Mid$(filenames, start, scPos - start)
            '
            ' Look for the filename in the document array
            '
            For i = 0 To UBound(arrDocs) - 1
                '
                ' If found, set Selected to True
                '
                If arrDocs(i).Name = filename Then
                    arrDocs(i).Selected = True
                    Exit For
                 End If
            Next 'i
            '
            ' Update the start and end points
            '
            start = scPos + 1
            scPos = InStr(scPos + 1, filenames, ";")
        Loop
        Call BuildDocList
    End If
End Sub
'
' Listing 28.19. This procedure uses the arrDocs array to build the list box items
'
Private Sub BuildDocList()
    Dim i As Integer    ' Counter
    '
    ' Run through the list of documents
    '
    With lstDocs
        .Clear
        optActive.Caption = "Active Document Only"
        For i = 0 To UBound(arrDocs) - 1
            .AddItem arrDocs(i).Name
            .List(i, 1) = arrDocs(i).State
            .Selected(i) = arrDocs(i).Selected
            If arrDocs(i).Name = ActiveWorkbook.Name Then
                '
                ' Include active filename in Active caption
                '
                With optActive
                    .Caption = .Caption & " (" & arrDocs(i).Name & ")"
                End With
            End If
        Next 'i
    End With
    Call DisplaySourceData
End Sub
'
' Listing 28.20. This procedure calculates the total number of files
' selected for backup as well as the total number of
' bytes in the selected files
'
Private Sub DisplaySourceData()
    Dim i As Integer        ' Counter
    Dim totFiles As Integer ' Holds the total files selected
    Dim totBytes As Long    ' Holds the total bytes selected
    '
    ' Add up file sizes for all documents that have
    ' Selected=True and then display the total
    '
    totFiles = 0
    totBytes = 0
    For i = 0 To UBound(arrDocs) - 1
        If arrDocs(i).Selected Then
            totFiles = totFiles + 1
            totBytes = totBytes + arrDocs(i).Size
        End If
    Next 'i
    '
    ' Write the data
    '
    lblTotalFiles.Caption = totFiles
    lblTotalBytes.Caption = Format(totBytes, "#,##0")
    Call CheckBackupStatus
End Sub
'
' Listing 28.21. This procedure displays data for the selected destination drive.
'
Private Function DisplayDestinationData() As Boolean
    Dim lpSectorsPerCluster As Long     ' Used in GetFreeDiskSpace
    Dim lpBytesPerSector As Long        ' Used in GetFreeDiskSpace
    Dim lpNumberOfFreeClusters As Long  ' Used in GetFreeDiskSpace
    Dim lpTotalNumberOfClusters As Long ' Used in GetFreeDiskSpace
    Dim retVal As Long                  ' Return value for API functions
    Dim nDrive As String                ' Holds the destination drive
    '
    ' Determine the root folder
    '
    nDrive = GetRoot(cboDestination.Value)
    '
    ' Is it the same drive?
    '
    If nDrive = currDestDrive Then
        '
        ' If so, exit if there was no problem
        '
        If lblBytesFree <> "N/A" Then Exit Function
    Else
        '
        ' Otherwise, set the global variable
        '
        currDestDrive = nDrive
    End If
    '
    ' nDrive will be "\" if no drive was specified
    '
    If nDrive = "\" Then
        lblDriveType = "N/A"
        lblBytesFree = "N/A"
        fraDriveStatus.Caption = "Drive Status:"
        lblDriveStatus = "No drive specified."
        cboDestination.Value = ""
        DisplayDestinationData = False
    Else
        fraDriveStatus.Caption = "Drive Status (" & nDrive & "):"
        '
        ' Get the drive type and translate it
        '
        retVal = GetDriveType(nDrive)
        Select Case retVal
            Case 0, 1
                lblDriveType = "N/A"
            Case DRIVE_REMOVABLE
                lblDriveType = "Removable drive"
            Case DRIVE_FIXED
                lblDriveType = "Fixed drive"
            Case DRIVE_REMOTE
                lblDriveType = "Network drive"
            Case DRIVE_CDROM
                lblDriveType = "CD-ROM drive"
            Case DRIVE_RAMDISK
                lblDriveType = "RAM disk"
        End Select
        '
        ' Check the free disk space
        '
        retVal = GetDiskFreeSpace(nDrive, lpSectorsPerCluster, lpBytesPerSector, lpNumberOfFreeClusters, lpTotalNumberOfClusters)
        If retVal <> 0 Then
            lblBytesFree = Format(lpSectorsPerCluster * lpBytesPerSector * lpNumberOfFreeClusters, "#,##0")
            DisplayDestinationData = True
        Else
            '
            ' Couldn't get free space value
            '
            lblDriveStatus = "Drive is unavailable or invalid."
            lblBytesFree = "N/A"
            DisplayDestinationData = False
        End If
    End If
    Call CheckBackupStatus
End Function
'
' This function extracts the root folder of the specified drive
'
Private Function GetRoot(drive As String) As String
    Dim firstSlash As Integer
    Dim secondSlash As Integer
    '
    ' Are we dealing with a UNC path?
    '
    If Left(drive, 2) = "\\" Then
        firstSlash = InStr(3, drive, "\")
        secondSlash = InStr(firstSlash + 1, drive, "\")
        If secondSlash = 0 Then
            '
            ' Make sure the root ends with "\"
            '
            GetRoot = drive & "\"
        Else
            GetRoot = Left(drive, secondSlash)
        End If
    Else
        drive = Left(drive, 3)
        '
        ' Make sure the root ends with "\"
        '
        If Right(drive, 1) <> "\" Then
            GetRoot = drive & "\"
        Else
            GetRoot = drive
        End If
    End If
End Function
'
' Listing 28.22. This procedure checks various items to see if the backup can
' proceed. If not, the Back Up button is disabled.
'
Private Sub CheckBackupStatus()
    '
    ' Skip this if we're initializing
    '
    If initializing Then Exit Sub
    '
    ' Can't back up if no files are selected
    '
    If CInt(lblTotalFiles) = 0 Then
        cmdBackUp.Enabled = False
        Exit Sub
    End If
    '
    ' Can't back up if the destination drive isn't valid
    '
    If lblBytesFree = "N/A" Then
        Beep
        cmdBackUp.Enabled = False
        Exit Sub
    End If
    '
    ' Can't back up if the destination folder doesn't exist
    '
    If Dir(cboDestination.Value) = "" Then
        Beep
        cmdBackUp.Enabled = False
        lblDriveStatus = "The specified folder does not exist."
        Exit Sub
    End If
    '
    ' Can't back up if there isn't enough space to hold the selected files
    '
    If CSng(lblTotalBytes) < CSng(lblBytesFree) Then
        lblDriveStatus = "Drive is OK."
    Else
        Beep
        lblDriveStatus = "There is not enough room on the drive to back up all the selected files."
        cmdBackUp.Enabled = False
        Exit Sub
    End If
    '
    ' If we made it this far, all is well
    '
    cmdBackUp.Enabled = True
End Sub
'
' Listing 28.23. This procedure performs the backup
'
Private Sub BackUpSelectedFiles()
    Dim i As Integer         ' Counter
    Dim str As Variant       ' Temporary string (variant) buffer
    Dim strSource As String  ' The full path for each source file
    Dim strDest As String    ' The destination path for each file
    Dim fn As Long           ' File number for low-level I/O
    Dim buff As String       ' String buffer for low-level I/O
    '
    ' Turn off screen updating
    '
    Application.ScreenUpdating = False
    '
    ' Run through the documents array
    '
    For i = 0 To UBound(arrDocs) - 1
        '
        ' Only back up documents where Selected = True
        '
        If arrDocs(i).Selected Then
            With Workbooks(arrDocs(i).Name)
                '
                ' Is the file new? If so, save it.
                ' Note that you must save a new file or
                ' this loop will continue indefinitely.
                '
                If .Path = "" Then
                    Do
                        str = Application.GetSaveAsFilename(, "Excel Files (*.xls), *.xls")
                    Loop Until str <> False
                    .SaveAs filename:=str
                End If
                '
                ' If the file is unsaved, save it
                '
                If Not .Saved Then .Save
                '
                ' Build the destination
                '
                If Not chkOverwrite Then
                    strDest = InputBox(.Name & _
                        " already exists on the destination drive." & _
                        Chr(13) & _
                        "Please enter a new filename for the backup file:", _
                        "Backup", .Name)
                    If strDest = "" Then Exit Sub
                Else
                    strDest = .Name
                End If
                If Right(cboDestination.Value, 1) <> "\" Then
                    strDest = cboDestination.Value & "\" & strDest
                Else
                    strDest = cboDestination.Value & strDest
                End If
                '
                ' Store the source path
                '
                strSource = .FullName
                '
                ' Open the source file in Binary mode
                '
                fn = FreeFile
                Open strSource For Binary Access Read As #fn
                '
                ' Allocate space and then Get the entire file
                '
                buff = Space(LOF(fn))
                Get #fn, , buff
                Close #fn
                '
                ' Open the destination file in Binary mode
                '
                fn = FreeFile
                Open strDest For Binary Access Write As #fn
                Application.StatusBar = "Backing up to " & strDest
                '
                ' Write the buffer to the destination file
                '
                Put #fn, , buff
                Close #fn
            End With
            Application.StatusBar = ""
        End If
    Next 'i
    '
    ' Resume screen updating
    '
    Application.ScreenUpdating = True
End Sub
'
' Listing 28.24. This procedure processes the selected Backup mode
'
Private Sub ProcessBackupMode()
    '
    ' Check for Reminder mode selected
    '
    If optReminder Then
        '
        ' Determine the next backup time
        '
        nextReminder = Now + TimeValue(txtIntervalHours & ":" & txtIntervalMinutes & ":" & txtIntervalSeconds)
        '
        ' Set the timer
        '
        Application.OnTime nextReminder, "Chaptr28.Backup.ShowBackup"
    Else
        '
        ' Cancel pending OnTime, if there is one
        '
        If nextReminder <> TimeValue("12:00:00 AM") Then
            Application.OnTime nextReminder, "Chaptr28.Backup.ShowBackup", , False
        End If
   End If
End Sub
'
' Listing 28.25. This procedure saves the current Backup settings to the Registry
'
Private Sub SaveRegistrySettings()
    Dim j As Integer    ' Counter
    Dim k As Integer    ' Counter
    Dim str As String   ' Temporary string buffer
    '
    ' Save destination items to the Registry
    '
    With cboDestination
        '
        ' Add the current item first
        '
        SaveSetting "VBA Unleashed", "Backup", "MRUFolder0", .Value
        '
        ' Now add the rest of the items (maximum 10)
        '
        k = 1
        For j = 0 To .ListCount - 1
            If .List(j) <> .Value Then
                SaveSetting "VBA Unleashed", "Backup", "MRUFolder" & k, .List(j)
                k = k + 1
                If k = 10 Then Exit For
            End If
        Next 'j
    End With
    '
    ' Save other options to the Registry
    '
    If optAll Then
        SaveSetting "VBA Unleashed", "Backup", "BackupFilter", "All"
    ElseIf optUnsaved Then
        SaveSetting "VBA Unleashed", "Backup", "BackupFilter", "Unsaved"
    ElseIf optActive Then
        SaveSetting "VBA Unleashed", "Backup", "BackupFilter", "Active"
    Else
        SaveSetting "VBA Unleashed", "Backup", "BackupFilter", "Selected"
        str = ""
        For j = 0 To UBound(arrDocs) - 1
            If arrDocs(j).Selected Then
                str = str & arrDocs(j).Name & ";"
            End If
        Next 'j
        SaveSetting "VBA Unleashed", "Backup", "SelectedDocuments", str
    End If
        
    SaveSetting "VBA Unleashed", "Backup", "IgnoreNewAndUnsaved", chkIgnoreNew
    SaveSetting "VBA Unleashed", "Backup", "OverwriteDestFiles", chkOverwrite
    If optManual Then
        SaveSetting "VBA Unleashed", "Backup", "BackupMode", "Manual"
        SaveSetting "VBA Unleashed", "Backup", "NextReminder", "12:00:00 AM"
    Else
        SaveSetting "VBA Unleashed", "Backup", "BackupMode", "Reminder"
        SaveSetting "VBA Unleashed", "Backup", "NextReminder", nextReminder
    End If
    SaveSetting "VBA Unleashed", "Backup", "IntervalHours", txtIntervalHours
    SaveSetting "VBA Unleashed", "Backup", "IntervalMinutes", txtIntervalMinutes
    SaveSetting "VBA Unleashed", "Backup", "IntervalSeconds", txtIntervalSeconds
End Sub

