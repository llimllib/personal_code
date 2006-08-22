Attribute VB_Name = "Chapter01"
' HypotenuseLength()
' Calculates the length of a right triangle's
' hypotenuse given the other two sides: x and y
'
Function HypotenuseLength(x, y)

    HypotenuseLength = Sqr(x ^ 2 + y ^ 2)
    
End Function

' Listing 1.1. An example Sub procedure.
'
Sub EnterLoanData()
    Range("IntRate").Value = 0.08
    Range("Term").Value = 10
    Range("Principal").Value = 10000
    Range("Payment").Formula = "=PMT(IntRate/12, Term*12, Principal)"
End Sub

'Listing 1.2. An example Function procedure.
'
Function CalcNetMargin(fixedCosts)
    totalSales = Application.Sum(Range("Sales"))
    totalExpenses = Application.Sum(Range("Expenses"))
    CalcNetMargin = (totalSales - totalExpenses - fixedCosts) / totalSales
End Function




