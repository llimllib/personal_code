VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "CEvents"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Public WithEvents WordEvents As Application
Attribute WordEvents.VB_VarHelpID = -1

Private Sub WordEvents_DocumentBeforeClose(ByVal Doc As Document, ByVal Cancel As Boolean)
    
End Sub
Private Sub WordEvents_DocumentBeforePrint(ByVal Doc As Document, ByVal Cancel As Boolean)
    
End Sub
Private Sub WordEvents_DocumentBeforeSave(ByVal Doc As Document, SaveAsUI As Boolean, ByVal Cancel As Boolean)
    
End Sub
Private Sub WordEvents_DocumentChange()
    
End Sub
Private Sub WordEvents_DocumentOpen(ByVal Doc As Document)
    
End Sub
Private Sub WordEvents_NewDocument(ByVal Doc As Document)
    
End Sub
Private Sub WordEvents_Quit()
    
End Sub
Private Sub WordEvents_WindowActivate(ByVal Doc As Document, ByVal Wn As Window)
    
End Sub
Private Sub WordEvents_WindowBeforeDoubleClick(ByVal Sel As Selection, ByVal Cancel As Boolean)
    
End Sub
Private Sub WordEvents_WindowBeforeRightClick(ByVal Sel As Selection, ByVal Cancel As Boolean)
    
End Sub
Private Sub WordEvents_WindowDeactivate(ByVal Doc As Document, ByVal Wn As Window)
    
End Sub
Private Sub WordEvents_SelectionChange(ByVal Sel As Selection)
    
End Sub

