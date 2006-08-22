VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmWWW 
   Caption         =   "The Word Wide Web"
   ClientHeight    =   8625
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   11910
   OleObjectBlob   =   "browser.frx":0000
   ShowModal       =   0   'False
   StartUpPosition =   3  'Windows Default
End
Attribute VB_Name = "frmWWW"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim topPage As String
'
' Listing 20.4. Some event handlers that are used to
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
' Listing 20.5. Event handlers for the navigation
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


