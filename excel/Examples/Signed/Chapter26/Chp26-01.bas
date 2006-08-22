Attribute VB_Name = "Listing_26_1"
Option Explicit
'
' Listing 26.1. A simple example of conditional compilation.
'
#Const DEPT = "Marketing"

Sub Main()
    #If DEPT = "Marketing" Then
        MsgBox "Welcome to the Marketing application!"
    #ElseIf DEPT = "Accounting" Then
        MsgBox "Welcome to the Accounting application!"
    #Else
        MsgBox "Welcome to the application!"
    #End If
End Sub

