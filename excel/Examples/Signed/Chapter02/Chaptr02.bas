Attribute VB_Name = "Chapter02"
' Module-level declarations go at the top
' of the module and before any procedures.
'
Dim totSales2
Dim totExpenses2

' Listing 2.1.
' Calculates the gross margin: (sales-expenses)/sales
'
Function GrossMargin()
    '
    ' Declarations
    '
    Dim totalSales
    Dim totalExpenses
    '
    ' Code
    '
    totalSales = Application.Sum(Range("Sales"))
    totalExpenses = Application.Sum(Range("Expenses"))
        
    GrossMargin = (totalSales - totalExpenses) / totalSales
End Function

' Listing 2.2. Procedure-level scope demonstration.
'
Sub Procedure1()
    Dim message
    '
    ' All statements in Procedure1 can access the
    ' "message" variable because they're in the scope:
    '
    message = "I'm in the scope!"
    MsgBox message
End Sub

Sub Procedure2()
    '
    ' The statements in Procedure2 can't access the
    ' "message" variable because they aren't in the scope:
    '
    MsgBox message
End Sub

' Listing 2.3. A procedure that calls two functions to calculate
' margin values. The two functions use the same variables.
'
Sub CalculateMargins()
    Range("GrossMarg").Value = GrossMarginCalc
    Range("NetMarg").Value = NetMarginCalc(Range("FixedCosts").Value)
End Sub

Function GrossMarginCalc()
    Dim totSales
    Dim totExpenses
    
    totSales = Application.Sum(Range("Sales"))
    totExpenses = Application.Sum(Range("Expenses"))
        
    GrossMarginCalc = (totSales - totExpenses) / totSales
End Function

Function NetMarginCalc(fixedCosts)
    Dim totSales2
    Dim totExpenses
    
    totSales = Application.Sum(Range("Sales"))
    totExpenses = Application.Sum(Range("Expenses"))
    
    NetMarginCalc = (totSales - totExpenses - fixedCosts) / totSales
End Function

' Listing 2.4. When you declare variables at the top of a module,
' they acquire module-level scope and so are available to every
' procedure in the module.
'
Sub CalculateMargins2()
    Range("GrossMarg").Value = GrossMarginCalc2
    Range("NetMarg").Value = NetMarginCalc2(Range("FixedCosts").Value)
End Sub

Function GrossMarginCalc2()
    totSales2 = Application.Sum(Range("Sales"))
    totExpenses2 = Application.Sum(Range("Expenses"))
        
    GrossMarginCalc2 = (totSales2 - totExpenses2) / totSales2
End Function

Function NetMarginCalc2(fixedCosts)
    NetMarginCalc2 = (totSales2 - totExpenses2 - fixedCosts) / totSales2
End Function

' Listing 2.5. Passing a variable by reference.
'
Sub Procedure3()
    Dim message
    message = "Pass it on!"
    Procedure4 message
    MsgBox message
End Sub

Sub Procedure4(message)
    MsgBox message
    message = "Thanks for the variable!"
End Sub

' Listing 2.6. Passing a variable by value.
'
Sub Procedure5()
    Dim message
    message = "Pass it on!"
    Procedure6 message
    MsgBox message
End Sub

Sub Procedure6(ByVal message)
    MsgBox message
    message = "Thanks for the variable!"
End Sub

' Listing 2.7. Static variable example.
'
Sub StaticTest()
    StaticProcedure
    StaticProcedure
End Sub

Sub StaticProcedure()
    Static staticVar
    Dim regularVar
    
    staticVar = staticVar + 5
    regularVar = regularVar + 5
    MsgBox "staticVar = " & staticVar & " and regularVar = " & regularVar
End Sub

