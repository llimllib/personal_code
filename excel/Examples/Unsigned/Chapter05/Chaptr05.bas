Attribute VB_Name = "Chapter05"
' Listing 5.1.
' Calculates the gross margin: (sales-expenses)/sales
'
Function GrossMargin2()
    
    Dim totalSales
    Dim totalExpenses
    
    totalSales = Application.Sum(Range("Sales"))
    totalExpenses = Application.Sum(Range("Expenses"))
        
    If totalSales <> 0 Then
        GrossMargin2 = (totalSales - totalExpenses) / totalSales
    End If

End Function

' Listing 5.2. A procedure that uses If...Then...Else.
'
Function FutureValue(Rate, Nper, Pmt, Frequency)

    If Frequency = "Monthly" Then
        FutureValue = FV(Rate / 12, Nper * 12, Pmt / 12)
    Else
        FutureValue = FV(Rate / 4, Nper * 4, Pmt / 4)
    End If

End Function


' Listing 5.3. A procedure that uses the And operator
' to perform multiple logical tests.
'
Function FutureValue2(Rate, Nper, Pmt, Frequency)

    If Frequency <> "Monthly" And Frequency <> "Quarterly" Then
        MsgBox "The Frequency argument must be either " & _
               """Monthly"" or ""Quarterly""!"
        Exit Function
    End If
    
    If Frequency = "Monthly" Then
        FutureValue2 = FV(Rate / 12, Nper * 12, Pmt / 12)
    Else
        FutureValue2 = FV(Rate / 4, Nper * 4, Pmt / 4)
    End If

End Function

' Listing 5.4. A procedure that uses multiple
' If...Then...Else statements.
'
Function FutureValue3(Rate, Nper, Pmt, Frequency)

    If Frequency = "Monthly" Then
        FutureValue3 = FV(Rate / 12, Nper * 12, Pmt / 12)
    ElseIf Frequency = "Quarterly" Then
        FutureValue3 = FV(Rate / 4, Nper * 4, Pmt / 4)
    Else
        MsgBox "The Frequency argument must be either " & _
               """Monthly"" or ""Quarterly""!"
    End If

End Function

' Listing 5.5. A procedure that uses Select Case to
' test multiple values.
'
Function FutureValue4(Rate, Nper, Pmt, Frequency)

    Select Case Frequency
        Case "Monthly"
            FutureValue4 = FV(Rate / 12, Nper * 12, Pmt / 12)
        Case "Quarterly"
            FutureValue4 = FV(Rate / 4, Nper * 4, Pmt / 4)
        Case Else
            MsgBox "The Frequency argument must be either " & _
                   """Monthly"" or ""Quarterly""!"
    End Select

End Function

' Listing 5.6. A procedure that uses Select Case to
' convert a raw test score into a letter grade.
'
Function LetterGrade(rawScore As Integer) As String

    Select Case rawScore
        Case Is < 0
            LetterGrade = "ERROR! Score less than 0!"
        Case Is < 50
            LetterGrade = "F"
        Case Is < 60
            LetterGrade = "D"
        Case Is < 70
            LetterGrade = "C"
        Case Is < 80
            LetterGrade = "B"
        Case Is <= 100
            LetterGrade = "A"
        Case Else
            LetterGrade = "ERROR! Score greater than 100!"
    End Select

End Function

' Listing 5.7 A function that accepts a color name as a
' string and returns the corresponding RGB value.
'
Function VBAColor(colorName As String) As Long
    
    Select Case LCase(Trim(colorName))
        Case "black"
            VBAColor = RGB(0, 0, 0)
        Case "white"
            VBAColor = RGB(255, 255, 255)
        Case "gray"
            VBAColor = RGB(192, 192, 192)
        Case "dark gray"
            VBAColor = RGB(128, 128, 128)
        Case "red"
            VBAColor = RGB(255, 0, 0)
        Case "dark red"
            VBAColor = RGB(128, 0, 0)
        Case "green"
            VBAColor = RGB(0, 255, 0)
        Case "dark green"
            VBAColor = RGB(0, 128, 0)
        Case "blue"
            VBAColor = RGB(0, 0, 255)
        Case "dark blue"
            VBAColor = RGB(0, 0, 128)
        Case "yellow"
            VBAColor = RGB(255, 255, 0)
        Case "dark yellow"
            VBAColor = RGB(128, 128, 0)
        Case "magenta"
            VBAColor = RGB(255, 0, 255)
        Case "dark magenta"
            VBAColor = RGB(128, 0, 128)
        Case "cyan"
            VBAColor = RGB(0, 255, 255)
        Case "dark cyan"
            VBAColor = RGB(0, 128, 128)
    End Select
    
End Function

' Use this procedure to test the VBAColor function. It sets
' the font color for the activer cell.
'
Sub ColorTester()
    ActiveCell.Font.Color = VBAColor("red")
End Sub

' Listing 5.8. A function procedure that uses IIf to
' test for a faulty Pentium chip.
'
Function FlawedPentium() As Boolean
    FlawedPentium = IIf((4195835 - (4195835 / 3145727) * 3145727), True, False)
End Function

' The following procedure tests the FlawedPentium function.
'
Sub FlawedPentiumTest()
    If FlawedPentium Then
        MsgBox "Flawed Pentium alert!"
    Else
        MsgBox "Your Pentium is cool!"
    End If
End Sub

' Listing 5.9. A function procedure that uses the Choose
' function to select from a list of values.
'
Function WeekdayName(weekdayNum As Integer) As String
    WeekdayName = Choose(weekdayNum, "Sunday", "Monday", _
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
End Function

' Use this procedure to test the WeekdayName function.
'
Sub WeekdayNameTest()
    MsgBox "Today is " & WeekdayName(WeekDay(Now))
End Sub

' Listing 5.10. A procedure that uses the Switch function
' to convert a test score into a letter grade.
'
Function LetterGrade2(rawScore As Integer) As String
    LetterGrade2 = Switch( _
        rawScore < 0, "ERROR! Score less than 0!", _
        rawScore < 50, "F", _
        rawScore < 60, "D", _
        rawScore < 70, "C", _
        rawScore < 80, "B", _
        rawScore <= 100, "A", _
        rawScore > 100, "ERROR! Score greater than 100!")
End Function

' Listing 5.11. A procedure that uses a Do...Loop to process
' cells until it encounters a blank cell.
'
Sub BigNumbers()

    Dim rowNum As Integer, colNum As Integer, currCell As Range
    rowNum = ActiveCell.Row                              'Initialize row #
    colNum = ActiveCell.Column                           'Initialize column #
    Set currCell = ActiveSheet.Cells(rowNum, colNum)     'Get first cell

    Do While currCell.Value <> ""                        'Do while not empty
        If IsNumeric(currCell.Value) Then                'If it's a number,
            If currCell.Value >= 1000 Then               'and it's a big one,
                currCell.Font.Color = VBAColor("magenta") 'color font magenta
            End If
        End If
        rowNum = rowNum + 1                              'Increment row #
        Set currCell = ActiveSheet.Cells(rowNum, colNum) 'Get next cell
    Loop

End Sub

' Listing 5.12. A simple For...Next loop.
'
Sub LoopTest()

    Dim counter

    For counter = 1 To 10
        'Display the message
        Application.StatusBar = "Counter value: " & counter
        ' Wait for 1 second
        Application.Wait Now + TimeValue("00:00:01")
    Next counter

    Application.StatusBar = False

End Sub

' Use the following procedure to pause program execution
' for "delay" seconds.
'
Sub VBAWait(delay As Integer)
    Dim startTime As Long
    startTime = Timer
    Do While Timer - startTime < delay
        DoEvents
    Loop
End Sub

' Listing 5.13. A Sub procedure that uses For Each...Next to
' loop through a selection and convert each cell to proper case.
'
Sub ConvertToProper()

    Dim cellObject As Object

    For Each cellObject In Selection
        cellObject.Value = Application.Proper(cellObject)
    Next

End Sub

' Listing 5.14. In this version of the BigNumbers procedure,
' the Do...Loop is terminated with the Exit Do statement if
' the current cell isn't a number.
'
Sub BigNumbers2()

    Dim rowNum As Integer, colNum As Integer, currCell As Range
    rowNum = ActiveCell.Row                              'Initialize row #
    colNum = ActiveCell.Column                           'Initialize column #
    Set currCell = ActiveSheet.Cells(rowNum, colNum)     'Get first cell

    Do While currCell.Value <> ""                        'Do while not empty

        If IsNumeric(currCell.Value) Then                'If it's a number,
            If currCell.Value >= 1000 Then               'and it's a big one,
                currCell.Font.Color = RGB(255, 0, 255)   'color font magenta
            End If
        Else                                             'If it's not,
            Exit Do                                      'exit the loop
        End If
        rowNum = rowNum + 1                              'Increment row #
        Set currCell = ActiveSheet.Cells(rowNum, colNum) 'Get next cell

    Loop

End Sub

