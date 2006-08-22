Attribute VB_Name = "Chapter13"
' Listing 13.1. Using the Shell function to start an
' application.
'
Sub StartControlPanelIcon(cplFile As String)
    Dim winFolder As String
    On Error GoTo BadStart
    '
    ' Get the Windows folder
    '
    winFolder = Environ("WINDIR")
    '
    ' Launch Control Panel
    '
    If InStr(1, Application.OperatingSystem, "NT") Then
        '
        ' Use this line with Windows NT:
        '
        Shell winFolder & "\System32\Control.exe " & cplFile, 1
    Else
        '
        ' Use this line with Windows 95/98:
        '
        Shell winFolder & "\Control.exe " & cplFile, 1
    End If
    Exit Sub

BadStart:
    MsgBox "Could not start Control Panel!", _
        vbOKOnly + vbExclamation
End Sub

' This procedure calls StartControlPanelIcon with
' "Inetcpl.cpl" to open the Data Sources dialog box.
'
Sub ChangeDataSource()
    StartControlPanelIcon ("Inetcpl.cpl")
End Sub

' Listing 13.2. Using the AppActivate statement to switch to
' a running application. This procedure loads the Phone Dialer
' accessory.
'
Sub LoadPhoneDialer()
    Dim winFolder As String
    Dim winDrive As String
    On Error GoTo BadStart
    '
    ' Start Phone Dialer without the focus
    '
    If InStr(1, Application.OperatingSystem, "NT") Then
        '
        ' Use this line with Windows NT:
        '
        winDrive = Left(Environ("WINDIR"), 3)
        Shell winDrive & "Program Files\Windows NT\dialer.exe", 4
    Else
        '
        ' Use this line with Windows 95/98:
        '
        winFolder = Environ("WINDIR")
        Shell winFolder & "\dialer.exe", 4
    End If
    '
    ' Set up the Ctrl+Shift+P shortcut key
    '
    Application.OnKey _
        Key:="^+P", _
        Procedure:="ActivatePhoneDialer"

    MsgBox "Phone Dialer loaded!" & Chr(13) & _
        "Press Ctrl+Shift+P to activate.", _
        vbInformation

    Exit Sub
BadStart:
    MsgBox "Could not start Phone Dialer!", _
        vbOKOnly + vbExclamation

End Sub

' This procedure copies the current cell and activates
' Phone Dialer when the user presses Ctrl+Shift+P
'
Sub ActivatePhoneDialer()

    Dim result As Integer

    On Error GoTo NotRunning
    '
    ' Copy the contents (a phone number?) of the current cell
    '
    ActiveCell.Copy
    '
    ' Activate Phone Dialer
    '
    AppActivate "Phone Dialer"

    Exit Sub

NotRunning:
    result = MsgBox("Phone Dialer is not loaded! " & _
                    "Do you want to load it now?", _
                    vbYesNo + vbExclamation)

    If result = vbYes Then
        LoadPhoneDialer
    End If

End Sub

' Listing 13.3. Controlling an application using the SendKeys
' statement.
'
Sub LoadAndDialPhoneDialer()
    Dim winFolder As String
    Dim winDrive As String
    Dim msg As String
    Dim buttons As Integer
    Dim response As Integer
    On Error GoTo BadStart

    msg = "About to dial the following number:" & _
          Chr(13) & Chr(13) & _
          "   " & ActiveCell & _
          Chr(13) & Chr(13) & _
          "Please make sure your modem is turned on."
    buttons = vbOKCancel + vbExclamation
    response = MsgBox(msg, buttons)

    If response = vbCancel Then Exit Sub
    '
    ' Copy the contents (a phone number?) of the current cell
    '
    ActiveCell.Copy
    '
    ' Start Phone Dialer with the focus
    '
    If InStr(1, Application.OperatingSystem, "NT") Then
        '
        ' Use this line with Windows NT:
        '
        winDrive = Left(Environ("WINDIR"), 3)
        Shell winDrive & "Program Files\Windows NT\dialer.exe", 1
    Else
        '
        ' Use this line with Windows 95/98:
        '
        winFolder = Environ("WINDIR")
        Shell winFolder & "\dialer.exe", 1
    End If
    '
    ' Paste the copied phone number with Ctrl+V and
    ' then press Enter to select the Dial button
    '
    SendKeys "^v~", True
    '
    ' Wait eight seconds to give the modem time to dial
    '
    Application.Wait Now + TimeValue("00:00:08")
    '
    ' Close the dialog boxes and exit Phone Dialer
    '
    SendKeys "~{ESC}%{F4}"
    '
    ' Get rid of Excel's Copy mode indicators
    '
    Application.CutCopyMode = False

    Exit Sub

BadStart:
    MsgBox "Could not start Phone Dialer!", _
        vbOKOnly + vbExclamation
End Sub

' Listing 13.4. Using the DDEInitiate method to open a DDE channel.
'
Sub TestIt()
   Dim result As Integer
    result = OpenHailingFrequencies
    DDETerminate result
End Sub

Function OpenHailingFrequencies() As Integer
    Dim channel As Integer
    
    On Error GoTo BadConnection
    '
    ' Establish the DDE connection to Program Manager
    '
    channel = DDEInitiate("Progman", "Progman")
    
    MsgBox "A channel to Program Manager is now open.", vbInformation
    '
    ' Return the channel number
    '
    OpenHailingFrequencies = channel
    Exit Function
    
BadConnection:
    MsgBox "Could not open a channel to Program Manager!", vbExclamation
    '
    ' Return 0
    '
    OpenHailingFrequencies = 0
        
End Function

' Listing 13.5. Using DDEExecute to control a server application.
'
Sub CreateWorkbookIcon()

    Dim channel As Integer
    Dim strPath As String, strName As String, strApp As String
    
    On Error GoTo BadConnection
    '
    ' Get info required for program item
    '
    strPath = ActiveWorkbook.Path & "\" & ActiveWorkbook.Name
    strName = Left(ActiveWorkbook.Name, Len(ActiveWorkbook.Name) - 4)
    strApp = Application.Path & "\Excel.exe"
    '
    ' Establish the DDE connection to Program Manager
    '
    channel = DDEInitiate("Progman", "Progman")
    '
    ' Create the group and item
    '
    DDEExecute channel, "[CreateGroup(""Excel Workbooks"")]"
    DDEExecute channel, "[AddItem(""" & strPath & """,""" & strName & """,""""" & strApp & """"")]"
    DDETerminate channel

    Exit Sub
    
BadConnection:
    MsgBox "Could not open a channel to Program Manager!", vbExclamation
            
End Sub

' Listing 13.6. Using DDERequest to retrieve data from an
' application.
'
Sub RequestWordData()
    Dim channel As Integer
    Dim wordData As Variant
    Dim getString As String
    On Error GoTo BailOut
    '
    ' Set up the application
    '
    Application.StatusBar = "Starting Word..."
    Application.DisplayAlerts = False
    '
    ' Initiate channel with System topic
    '
    channel = DDEInitiate("Winword", "System")
    '
    ' Open the document we want to work with
    '
    Application.StatusBar = "Opening Word document..."
    DDEExecute channel, "[FileOpen ""C:\My Documents\Chaptr13.doc""]"
    DDETerminate channel
    '
    ' Initiate new channel with document
    '
    channel = DDEInitiate("Winword", "C:\My Documents\Chaptr13.doc")
    '
    ' Find keyword and add a bookmark
    '
    DDEExecute channel, "[StartOfDocument]"
    DDEExecute channel, "[EditFind .Find = ""ACME""]"
    DDEExecute channel, "[SelectCurSentence]"
    DDEExecute channel, "[EditBookmark .Name = ""Gotcha""]"
    '
    ' Retrieve the bookmark and store it
    '
    wordData = DDERequest(channel, "Gotcha")
    getString = wordData(1)
    Worksheets("Sheet1").[A2].Value = getString
    '
    ' Quit Word and terminate channel
    '
    DDEExecute channel, "[FileExit 1]"
    DDETerminate channel

    Exit Sub

BailOut:
    DDETerminate channel
    MsgBox "DDE operation failed!", vbExclamation

End Sub

' Listing 13.7. Using DDEPoke to send data to an application.
'
Sub SendDataToWord()

    Dim channel As Integer, pokeData As Variant
    On Error GoTo BailOut
    '
    ' Set up the application
    '
    Application.StatusBar = "Starting Word..."
    Application.DisplayAlerts = False
    '
    ' Initiate channel with System topic
    '
    channel = DDEInitiate("Winword", "System")
    '
    ' Open the document we want to work with
    '
    Application.StatusBar = "Opening Word document..."
    DDEExecute channel, "[FileOpen ""C:\My Documents\Chaptr13.doc""]"
    DDETerminate channel
    '
    ' Initiate new channel with document
    '
    channel = DDEInitiate("Winword", "C:\My Documents\Chaptr13.doc")
    '
    'Get the data to be sent
    '
    Application.StatusBar = "Sending data..."
    Set pokeData = Worksheets("Sheet1").[A1]
    '
    'Send it to the "Gotcha" bookmark
    '
    DDEPoke channel, "Gotcha", pokeData
    '
    ' Quit Word and terminate channel
    '
    Application.StatusBar = "Shutting down Word..."
    DDEExecute channel, "[FileExit 1]"
    DDETerminate channel
    Application.StatusBar = False

    Exit Sub

BailOut:
    DDETerminate channel
    MsgBox "DDE operation failed!", vbExclamation
    Application.StatusBar = False

End Sub

