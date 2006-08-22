VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "CCustomer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private m_CustAcct As String
Private m_CustName As String
Private m_CustAddress As String
Private m_CustInvoices As CInvoices

Public Property Get Account() As String
    Account = m_CustAcct
End Property
Public Property Let Account(CustAcct As String)
    m_CustAcct = CustAcct
End Property

Public Property Get Name() As String
    Name = m_CustName
End Property
Public Property Let Name(CustName As String)
    m_CustName = CustName
End Property

Public Property Get Address() As String
    Address = m_CustAddress
End Property
Public Property Let Address(CustAddress As String)
    m_CustAddress = CustAddress
End Property

Public Property Get Invoices() As CInvoices
    Set Invoices = m_CustInvoices
End Property

Public Property Set Invoices(InvoicesObject As CInvoices)
    Set m_CustInvoices = InvoicesObject
End Property

