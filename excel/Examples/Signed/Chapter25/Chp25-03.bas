Attribute VB_Name = "Listing_25_03"
Option Explicit
'
' Listing 25.3. Using the Beep API function to control
' the speaker in Windows NT.
'
Declare Function Beep Lib "kernel32" (ByVal dwFreq As Long, ByVal dwDuration As Long) As Long
Declare Function MessageBeep Lib "user32" (ByVal wType As Long) As Long

Sub BeepTest()
    Dim i As Integer
    For i = 100 To 2000 Step 100
        Beep i, 50
    Next 'i
End Sub

Sub MessageBeepTest()
    Dim i As Integer
    For i = 0 To 64 Step 16
        MessageBeep i
        Application.Wait Now + TimeValue("00:00:02")
    Next 'i
End Sub
