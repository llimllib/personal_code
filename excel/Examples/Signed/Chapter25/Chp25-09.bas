Attribute VB_Name = "Listing_25_09"
Option Explicit
'
' Listing 25.9. This example compares application performance using
' DoEvents by itself and using the GetQueueStatus API function.
'
Declare Function GetQueueStatus Lib "user32" (ByVal fuFlags As Long) As Long

Public Const QS_KEY = &H1
Public Const QS_MOUSEMOVE = &H2
Public Const QS_MOUSEBUTTON = &H4
Public Const QS_MOUSE = (QS_MOUSEMOVE Or QS_MOUSEBUTTON)
Public Const QS_INPUT = (QS_MOUSE Or QS_KEY)

Sub DoEventsTester()
    Dim i As Long
    Dim start As Long
    '
    ' Get the current Timer value and
    ' start the "DoEvents only" loop
    '
    start = Timer
    For i = 1 To 100000
        DoEvents
    Next 'i
    '
    ' Display the results
    '
    Debug.Print "With DoEvents by itself: "; Timer - start; " seconds"
    '
    ' Get the current Timer value and
    ' start the "GetQueueStatus" loop
    '
    start = Timer
    For i = 1 To 100000
        If GetQueueStatus(QS_INPUT) <> 0 Then DoEvents
    Next 'i
    '
    ' Display the results
    '
    Debug.Print "With GetQueueStatus check: "; Timer - start; " seconds"
End Sub
