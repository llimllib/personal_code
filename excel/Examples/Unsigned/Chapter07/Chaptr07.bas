Attribute VB_Name = "Chapter07"
Option Explicit
Dim currCell As String  ' The module-level variable for Listing 7.4

' Listing 7.1. Toggles the active window's gridlines on and off.
'
Sub ToggleGridlines()
Attribute ToggleGridlines.VB_ProcData.VB_Invoke_Func = " \n14"

    With ActiveWindow
        .DisplayGridlines = Not .DisplayGridlines
    End With
    
End Sub

' Listing 7.2. A procedure that tests the Evaluate function.
'
Sub EvaluateTest()
    Dim columnLetter As String
    Dim rowNumber As String
    Dim cellAddress As String
    '
    ' Activate the "Test Data" worksheet
    '
    Worksheets("Test Data").Activate
    '
    ' Get the value in cell A1
    '
    columnLetter = [A1].Value
    '
    ' Get the value in cell B1
    '
    rowNumber = [B1].Value
    '
    ' Concatenate the two values and then display the message
    '
    cellAddress = columnLetter & rowNumber
    MsgBox "The value in cell " & cellAddress & " is " & Evaluate(cellAddress)
End Sub

' Listing 7.3. Procedures that set and reset a key
' combination using the OnKey method.
'
' SetKey()
' Sets up Ctrl-Delete to delete everything from a cell
'
Sub SetKey()
    Application.OnKey _
        Key:="^{Del}", _
        Procedure:="Chaptr07.xls!DeleteAll"
End Sub

' DeleteAll()
' This procedure runs whever Ctrl-Delete is pressed
'
Sub DeleteAll()
    Selection.Clear
End Sub

' ResetKey()
' Resets Ctrl-Delete to its normal behavior
'
Sub ResetKey()
    Application.OnKey _
        Key:="^{Del}"
End Sub
Sub test()
Application.CalculateFull
End Sub

    
' Listing 7.4. Procedures that set the OnRepeat
' and OnUndo properties.
'
Sub BoldAndItalic()

    With ActiveCell
        .Font.Bold = True
        .Font.Italic = True
        currCell = .Address
    End With

    Application.OnRepeat _
        Text:="&Repeat Bold and Italic", _
        Procedure:="BoldAndItalic"

    Application.OnUndo _
        Text:="&Undo Bold and Italic", _
        Procedure:="UndoBoldAndItalic"
End Sub

Sub UndoBoldAndItalic()

    With Range(currCell).Font
        .Bold = False
        .Italic = False
    End With

End Sub

' Listing 7.5. A Calculate event handler that monitors the
' GrossMargin cell.
'
Private Sub Worksheet_Calculate()
    With Range("GrossMargin")
        If IsNumeric(.Value) Then
            If .Value < 0.2 Then
                .Select
                MsgBox "Gross Margin is below 20%!"
            End If
        End If
    End With
End Sub

' Listing 7.6. A Change event handler that ensures the user
' enters only numeric values in Column B.
'
Private Sub Worksheet_Change(ByVal Target As Excel.Range)
    With Target
        '
        ' Make sure we're in Column B
        '
        If .Column = 2 And (.Row <> 16 And .Row <> 17) Then
            '
            ' The value must be a number
            '
            If Not IsNumeric(.Value) Then
                MsgBox "Please enter a number in cell " & _
                    .Address & "!"
                .Select
            End If
        End If
    End With
End Sub

' Listing 7.7. A procedure that uses the Rows method
' to insert a row before the last row of a range.
'
Sub InsertRangeRow(rangeObject As Range)
Attribute InsertRangeRow.VB_ProcData.VB_Invoke_Func = " \n14"

    Dim totalRows As Integer, lastRow As Integer
    
    With rangeObject
        totalRows = .Rows.Count         ' Total rows in the range
        lastRow = .Rows(totalRows).Row  ' Last row number
        .Rows(lastRow).Insert           ' Insert before last row
    End With
    
End Sub

Sub InsertTest()
    InsertRangeRow ThisWorkbook.Worksheets("Sheet1").Range("Test")
End Sub

' Listing 7.8. A procedure that uses the Offset method
' to concatenates two strings.
'
Sub ConcatenateStrings()
Attribute ConcatenateStrings.VB_ProcData.VB_Invoke_Func = " \n14"

    Dim string1$, string2$
    
    ' Store the contents of the cell 2 to the left of the active cell
    string1$ = ActiveCell.Offset(0, -2)
        
    ' Store the contents of the cell 1 to the left of the active cell
    string2$ = ActiveCell.Offset(0, -1)
        
    ' Enter combined strings (separated by a space) into active cell
    ActiveCell.Value = string1$ & " " & string2$
    
End Sub

' Listing 7.9. A procedure that uses Resize to adjust a
' range name.
'
Sub InsertAndRedefineName()
Attribute InsertAndRedefineName.VB_ProcData.VB_Invoke_Func = " \n14"
    With ThisWorkbook.Worksheets("Test Data")
        InsertRangeRow .Range("Test")
        With .Range("Test")
            Names.Add _
                Name:="Test", _
                RefersTo:=.Resize(.Rows.Count + 1)
        End With
        .Range("Test").Select
    End With
End Sub

Sub ToggleAddIn()
    With ThisWorkbook
        .IsAddin = Not .IsAddin
    End With
End Sub

Function CalcGrossMargin()

    Dim totalSales
    Dim totalExpenses

    With Worksheets("2000 Budget")
        totalSales = Application.Sum(.Range("Sales"))
        totalExpenses = Application.Sum(.Range("Expenses"))
    End With

    If totalSales <> 0 Then
        CalcGrossMargin = (totalSales - totalExpenses) / totalSales
    End If
    '
    ' Recalculate whenever the sheet recalculates
    '
    Application.Volatile
End Function

Function CalcNetMargin(fixedCosts)

    Dim totalSales
    Dim totalExpenses
    
    With Worksheets("2000 Budget")
        totalSales = Application.Sum(.Range("Sales"))
        totalExpenses = Application.Sum(.Range("Expenses"))
    End With
        
    If totalSales <> 0 Then
        CalcNetMargin = (totalSales - totalExpenses - fixedCosts) / totalSales
    End If
    '
    ' Recalculate whenever the sheet recalculates
    '
    Application.Volatile
End Function
