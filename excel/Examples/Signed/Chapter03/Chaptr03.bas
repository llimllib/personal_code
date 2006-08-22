Attribute VB_Name = "Chapter03"
'Listing 3.1. A first attempt at calculating the pre-tax cost.
'
Function PreTaxCost(totalCost As Currency, taxRate As Single) As Currency
    PreTaxCost = totalCost / 1 + taxRate
End Function

' Listing 3.2. The correct way to calculate the pre-tax cost.
'
Function PreTaxCost2(totalCost As Currency, taxRate As Single) As Currency
    PreTaxCost2 = totalCost / (1 + taxRate)
End Function

' RandomFilename()
' Generates a random 8-digit string, suitable for use as the
' primary part of a temporary filename.
'
Function RandomFilename() As String
    Randomize
    RandomFilename = Int((99999999 - 1000000) * Rnd + 10000000)
End Function

' Listing 3.3. A procedure that uses a few string functions.
' The fullString argument should be a string value that includes
' both the first and last name.
'
Function ExtractLastName(fullName As String) As String
    Dim spacePos As Integer
    spacePos = InStr(fullName, " ")
    ExtractLastName = Mid$(fullName, _
                           spacePos + 1, _
                           Len(fullName) - spacePos)
End Function

' Use this procedure to test ExtractFileName.
'
Sub TestIt()
    MsgBox ExtractLastName("Millicent Peeved")
End Sub

' Listing 3.4. A Function procedure that uses various date
' functions to calculate a person's age.
'
Function CalculateAge(birthDate As Date) As Byte
    Dim birthdayNotPassed As Boolean
    birthdayNotPassed = CDate(Month(birthDate) & "/" & _
                           Day(birthDate) & "/" & _
                           Year(Now)) > Now
    CalculateAge = Year(Now) - Year(birthDate) + birthdayNotPassed
End Function

' Use this procedure to test CalculateAge.
'
Sub TestIt2()
    MsgBox CalculateAge(#10/21/1959#)
End Sub

Sub PhoneFormat()
    phoneNum = 2135556543
    MsgBox "The phone number is " & Format(phoneNum, "(000) 000-0000")
End Sub

Sub SSNFormat()
    ssn = 123456789
    MsgBox "The social security number is " & Format(ssn, "000-00-0000")
End Sub

Sub DegreeForamt()
    temp = 98.6
    MsgBox "The temperature is " & Format(temp, "#,##0.0" & Chr(176) & "F")
End Sub

Sub DateFormat()
    thismoment = Now
    MsgBox "It's now " & Format(thismoment, "h:nn AM/PM") & " on " & _
       Format(thismoment, "dddd, mmmm d")
End Sub

