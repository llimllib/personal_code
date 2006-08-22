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
    If InStr(1, System.OperatingSystem, "NT") Then
        '
        ' Use this line with Windows NT:
        '
        Shell winFolder & "\System32\Control.exe " & cplFile, 1
    Else
        '
        ' Use this line with Windows 95/98:
        '
        Shell winFolder & "\Control.exe " & cplFile, 1
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
    If InStr(1, System.OperatingSystem, "NT") Then
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
    CustomizationContext = NormalTemplate
    KeyBindings.Add _
        KeyCode:=BuildKeyCode(wdKeyP, wdKeyControl, wdKeyShift), _
        KeyCategory:=wdKeyCategoryMacro, _
        Command:="ActivatePhoneDialer"
    
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
    ' Copy the contents (a phone number?)
    ' of the current selection
    '
    With Selection
        If .Type <> wdSelectionIP Then .Copy
    End With
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
    Dim startTime As Long
    On Error GoTo BadStart
    '
    ' Make sure we have a selection
    '
    If Selection.Type = wdSelectionIP Then
        MsgBox "Please select a phone number!"
        Exit Sub
    End If
    '
    ' Warn the user
    '
    msg = "About to dial the following number:" & _
          Chr(13) & Chr(13) & _
          "   " & Selection.Text & _
          Chr(13) & Chr(13) & _
          "Please make sure your modem is turned on."
    buttons = vbOKCancel + vbExclamation
    response = MsgBox(msg, buttons)

    If response = vbCancel Then Exit Sub
    '
    ' Copy the contents (a phone number?) of the current selection
    '
    Selection.Copy
    '
    ' Start Phone Dialer with the focus
    '
    If InStr(1, System.OperatingSystem, "NT") Then
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
    startTime = Timer
    Do While (Timer - startTime) < 8
        DoEvents
    Loop
    '
    ' Close the dialog boxes and exit Phone Dialer
    '
    SendKeys "~{ESC}%{F4}"
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
Sub CreateDocumentShortcut()
    Dim channel As Integer
    Dim strPath As String, strName As String, strApp As String
    
    On Error GoTo BadConnection
    '
    ' Get info required for program item
    '
    strPath = ActiveDocument.Path & "\" & ActiveDocument.Name
    strName = Left(ActiveDocument.Name, Len(ActiveDocument.Name) - 4)
    strApp = Application.Path & "\Winword.exe"
    '
    ' Establish the DDE connection to Program Manager
    '
    channel = DDEInitiate("Progman", "Progman")
    '
    ' Create the group and item
    '
    DDEExecute channel, "[CreateGroup(""Word Documents"")]"
    DDEExecute channel, "[AddItem(""" & strPath & """,""" & strName & """,""""" & strApp & """"")]"
    DDETerminate channel

    Exit Sub
    
BadConnection:
    MsgBox "Could not open a channel to Program Manager!", vbExclamation
            
End Sub

' Listing 13.6. Using DDERequest to retrieve data from an
' application.
'
Sub RequestExcelData()
    Dim channel As Integer
    Dim xlData As Variant
    Dim getString As String
    On Error GoTo BailOut
    '
    ' Set up the application
    '
    Application.StatusBar = "Starting Excel..."
    Application.DisplayAlerts = False
    '
    ' Initiate channel with System topic
    '
    channel = DDEInitiate("Excel", "System")
    '
    ' Open the document we want to work with
    '
    Application.StatusBar = "Opening Excel workbook..."
    DDEExecute channel, "[OPEN(""C:\My Documents\Chaptr13.xls"")]"
    DDETerminate channel
    '
    ' Initiate new channel with document
    '
    Application.StatusBar = "Requesting data..."
    channel = DDEInitiate("Excel", "C:\My Documents\Chaptr13.xls")
    '
    ' Retrieve data from cell A1 and then display it
    '
    xlData = DDERequest(channel, "R1C1")
    MsgBox xlData
    DDETerminate channel
    Application.StatusBar = ""
    Exit Sub

BailOut:
    DDETerminateAll
    MsgBox "DDE operation failed!", vbExclamation

End Sub

' Listing 13.7. Using DDEPoke to send data to an application.
'
Sub SendDataToExcel()
    Dim channel As Integer
    Dim pokeData As String
    On Error GoTo BailOut
    '
    ' Initiate channel with System topic
    '
    Application.StatusBar = "Starting Excel..."
    channel = DDEInitiate("Excel", "System")
    '
    ' Open the document we want to work with
    '
    Application.StatusBar = "Opening Excel workbook..."
    DDEExecute channel, "[OPEN(""C:\My Documents\Chaptr13.xls"")]"
    DDETerminate channel
    '
    ' Initiate new channel with document
    '
    Application.StatusBar = "Sending data..."
    channel = DDEInitiate("Excel", "C:\My Documents\Chaptr13.xls")
    '
    ' Get the data to be sent
    '
    pokeData = ThisDocument.Paragraphs(3).Range.Text
    '
    ' Send it to cell A2
    '
    DDEPoke channel, "R2C1", pokeData
    '
    ' We're done
    '
    DDETerminate channel
    Application.StatusBar = ""
    Exit Sub

BailOut:
    DDETerminateAll
    MsgBox "DDE operation failed!", vbExclamation
    Application.StatusBar = ""
End Sub

