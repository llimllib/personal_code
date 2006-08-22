VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "CInvoice"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
'
' Declare the data members
'
Private m_ID As Long
Private m_CustAcct As String
Private m_Amount As Currency
Private m_InvDate As Date
Private m_DueDate As Date
Private m_PmtDate As Date
Private m_Overdue As Boolean
'
' Returns the current value of the CInvoice.ID property
'
Public Property Get ID() As Long
    ID = m_ID
End Property
'
' Returns the current value of the CInvoice.CustomerAccount property
'
Public Property Get CustomerAccount() As String
    CustomerAccount = m_CustAcct
End Property
'
' Returns the current value of the CInvoice.Amount property
'
Public Property Get Amount() As Currency
    Amount = m_Amount
End Property
'
' Returns the current value of the CInvoice.InvoiceDate property
'
Public Property Get InvoiceDate() As Date
    InvoiceDate = m_InvDate
End Property
'
' Returns the current value of the CInvoice.DueDate property
'
Public Property Get DueDate() As Date
    DueDate = m_DueDate
End Property
'
' Returns the current value of the CInvoice.PaymentDate property
'
Public Property Get PaymentDate() As Date
    PaymentDate = m_PmtDate
End Property
'
' Returns the current value of the CInvoice.Overdue property
'
Public Property Get Overdue() As Boolean
    Overdue = m_Overdue
End Property
'
' Changes the value of the CInvoice.CustomerAccount property
'
Public Property Let CustomerAccount(CustAcct As String)
    m_CustAcct = CustAcct
End Property
'
' Changes the value of the CInvoice.Amount property
'
Public Property Let Amount(Amt As Currency)
    If Amt < 0 Then
        Amt = 0
        MsgBox "Can't enter a negative amount!" & _
            Chr(13) & "$0.00 will be used, instead."
    End If
    m_Amount = Amt
End Property
'
' Changes the value of the CInvoice.InvoiceDate property
'
Public Property Let InvoiceDate(InvDate As Date)
    If InvDate < Date Then
        InvDate = Date
        MsgBox "Can't set an invoice date in the past!" & _
            Chr(13) & "Today's date will be used, instead."
    End If
    m_InvDate = InvDate
    m_DueDate = m_InvDate + 30
End Property
'
' Changes the value of the CInvoice.PaymentDate property
'
Public Property Let PaymentDate(PmtDate As Date)
    m_PmtDate = PmtDate
    m_Overdue = False
End Property
'
' Defines the CInvoice.Update method.
'
Public Sub Update()
    If m_DueDate < Date And m_PmtDate = 0 Then
        m_Overdue = True
    End If
End Sub
'
' This procedure runs when the class Initialize event fires
'
Private Sub Class_Initialize()
    Randomize
    m_ID = Int((999999 - 100000) * Rnd + 100000)
    m_InvDate = Date
    m_DueDate = m_InvDate + 30
End Sub


