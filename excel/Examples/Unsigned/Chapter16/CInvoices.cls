VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "CInvoices"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

' Data member
Private m_Invoices As New Collection
'
' Returns the CInvoices.Count property
'
Public Property Get Count() As Long
    Count = m_Invoices.Count
End Property
'
' This function is a wrapper for the Add method
'
Public Function Add(CustAcct As String, Amt As Currency) As CInvoice
    Dim newInvoice As New CInvoice
    With newInvoice
        .CustomerAccount = CustAcct
        .Amount = Amt
        m_Invoices.Add Item:=newInvoice, Key:=CStr(.ID)
    End With
    Set Add = newInvoice
End Function
'
' This function is a wrapper for the Item method
'
Public Function Item(Index As Variant) As CInvoice
    Set Item = m_Invoices.Item(Index)
End Function
'
' This procedure is a wrapper for the Remove method
'
Public Sub Delete(Index As Variant)
    m_Invoices.Remove Index
End Sub

