Attribute VB_Name = "Listing_25_07"
Option Explicit
'
' Listing 25.7. This example polls the GetSystemMetrics API function.
'
Declare Function GetSystemMetrics Lib "user32" (ByVal nIndex As Long) As Long
'
' Constants for GetSystemMetrics
'
Private Const SM_CXSCREEN = 0
Private Const SM_CYSCREEN = 1
Private Const SM_CXVSCROLL = 2
Private Const SM_CYHSCROLL = 3
Private Const SM_CYCAPTION = 4
Private Const SM_CXBORDER = 5
Private Const SM_CYBORDER = 6
Private Const SM_CXDLGFRAME = 7
Private Const SM_CYDLGFRAME = 8
Private Const SM_CYVTHUMB = 9
Private Const SM_CXHTHUMB = 10
Private Const SM_CXICON = 11
Private Const SM_CYICON = 12
Private Const SM_CXCURSOR = 13
Private Const SM_CYCURSOR = 14
Private Const SM_CYMENU = 15
Private Const SM_CXFULLSCREEN = 16
Private Const SM_CYFULLSCREEN = 17
Private Const SM_CYKANJIWINDOW = 18
Private Const SM_MOUSEPRESENT = 19
Private Const SM_CYVSCROLL = 20
Private Const SM_CXHSCROLL = 21
Private Const SM_DEBUG = 22
Private Const SM_SWAPBUTTON = 23
Private Const SM_RESERVED1 = 24
Private Const SM_RESERVED2 = 25
Private Const SM_RESERVED3 = 26
Private Const SM_RESERVED4 = 27
Private Const SM_CXMIN = 28
Private Const SM_CYMIN = 29
Private Const SM_CXSIZE = 30
Private Const SM_CYSIZE = 31
Private Const SM_CXFRAME = 32
Private Const SM_CYFRAME = 33
Private Const SM_CXMINTRACK = 34
Private Const SM_CYMINTRACK = 35
Private Const SM_CXDOUBLECLK = 36
Private Const SM_CYDOUBLECLK = 37
Private Const SM_CXICONSPACING = 38
Private Const SM_CYICONSPACING = 39
Private Const SM_MENUDROPALIGNMENT = 40
Private Const SM_PENWINDOWS = 41
Private Const SM_DBCSENABLED = 42
Private Const SM_CMOUSEBUTTONS = 43
'
' This procedure accepts a constant value, the constant name,
' as well as a decription of the metric, and displays the
' results in the Immediate window.
'
Sub GetMetric(nIndex As Long, constantName As String, metricDesc As String)
    Dim retval As Long

    retval = GetSystemMetrics(nIndex)
    Debug.Print metricDesc; Tab(35); constantName; Tab; retval
End Sub
'
' This procedure runs through each of the available metrics.
'
Sub RunThroughMetrics()
    GetMetric SM_CXSCREEN, "SM_CXSCREEN", "Screen width in pixels"
    GetMetric SM_CYSCREEN, "SM_CYSCREEN", "Screen height in pixels"
    GetMetric SM_CXVSCROLL, "SM_CXVSCROLL", "Vertical scroll arrow width"
    GetMetric SM_CYHSCROLL, "SM_CYHSCROLL", "Horizontal scroll arrow height"
    GetMetric SM_CYCAPTION, "SM_CYCAPTION", "Caption bar height"
    GetMetric SM_CXBORDER, "SM_CXBORDER", "Window border width"
    GetMetric SM_CYBORDER, "SM_CYBORDER", "Window border height"
    GetMetric SM_CXDLGFRAME, "SM_CXDLGFRAME", "Dialog window frame width"
    GetMetric SM_CYDLGFRAME, "SM_CYDLGFRAME", "Dialog window frame height"
    GetMetric SM_CYVTHUMB, "SM_CYVTHUMB", "Vertical scroll thumb height"
    GetMetric SM_CXHTHUMB, "SM_CXHTHUMB", "Horizontal scroll thumb width"
    GetMetric SM_CXICON, "SM_CXICON", "Icon width"
    GetMetric SM_CYICON, "SM_CYICON", "Icon height"
    GetMetric SM_CXCURSOR, "SM_CXCURSOR", "Cursor width"
    GetMetric SM_CYCURSOR, "SM_CYCURSOR", "Cursor height"
    GetMetric SM_CYMENU, "SM_CYMENU", "Menu bar height"
    GetMetric SM_CXFULLSCREEN, "SM_CXFULLSCREEN", "Full screen client area width"
    GetMetric SM_CYFULLSCREEN, "SM_CYFULLSCREEN", "Full screen client area height"
    GetMetric SM_CYKANJIWINDOW, "SM_CYKANJIWINDOW", "Kanji window height"
    GetMetric SM_MOUSEPRESENT, "SM_MOUSEPRESENT", "Mouse present flag"
    GetMetric SM_CYVSCROLL, "SM_CYVSCROLL", "Vertical scroll arrow height"
    GetMetric SM_CXHSCROLL, "SM_CXHSCROLL", "Horizontal scroll arrow width"
    GetMetric SM_DEBUG, "SM_DEBUG", "Debug version flag"
    GetMetric SM_SWAPBUTTON, "SM_SWAPBUTTON", "Mouse buttons swapped flag"
    GetMetric SM_CXMIN, "SM_CXMIN", "Minimum window width"
    GetMetric SM_CYMIN, "SM_CYMIN", "Minimum window height"
    GetMetric SM_CXSIZE, "SM_CXSIZE", "Minimize/Maximize icon width"
    GetMetric SM_CYSIZE, "SM_CYSIZE", "Minimize/Maximize icon height"
    GetMetric SM_CXFRAME, "SM_CXFRAME", "Window frame width"
    GetMetric SM_CYFRAME, "SM_CYFRAME", "Window frame height"
    GetMetric SM_CXMINTRACK, "SM_CXMINTRACK", "Minimum window tracking width"
    GetMetric SM_CYMINTRACK, "SM_CYMINTRACK", "Minimum window tracking height"
    GetMetric SM_CXDOUBLECLK, "SM_CXDOUBLECLK", "Double click x tolerance (3.1)"
    GetMetric SM_CYDOUBLECLK, "SM_CYDOUBLECLK", "Double click y tolerance (3.1)"
    GetMetric SM_CXICONSPACING, "SM_CXICONSPACING", "Horizontal icon spacing (3.1)"
    GetMetric SM_CYICONSPACING, "SM_CYICONSPACING", "Vertical icon spacing (3.1)"
    GetMetric SM_MENUDROPALIGNMENT, "SM_MENUDROPALIGNMENT", "Left or right menu drop (3.1)"
    GetMetric SM_PENWINDOWS, "SM_PENWINDOWS", "Pen extensions installed (3.1)"
    GetMetric SM_DBCSENABLED, "SM_DBCSENABLED", "DBCS version of USER32 installed"
    GetMetric SM_CMOUSEBUTTONS, "SM_CMOUSEBUTTONS", "Number of buttons on mouse"
End Sub

