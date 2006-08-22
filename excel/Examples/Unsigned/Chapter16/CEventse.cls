VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "CEvents"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public WithEvents XLEvents As Application
Attribute XLEvents.VB_VarHelpID = -1

Private Sub XLEvents_NewWorkbook(ByVal Wb As Workbook)

End Sub
Private Sub XLEvents_SheetActivate(ByVal Sh As Object)

End Sub
Private Sub XLEvents_SheetBeforeDoubleClick(ByVal Sh As Object, ByVal Target As Range, Cancel As Boolean)

End Sub
Private Sub XLEvents_SheetBeforeRightClick(ByVal Sh As Object, ByVal Target As Range, Cancel As Boolean)

End Sub
Private Sub XLEvents_SheetCalculate(ByVal Sh As Object)

End Sub
Private Sub XLEvents_SheetChange(ByVal Sh As Object, ByVal Target As Range)

End Sub
Private Sub XLEvents_SheetDeactivate(ByVal Sh As Object)

End Sub
Private Sub XLEvents_SheetFollowHyperlink(ByVal Sh As Object, ByVal Target As Hyperlink)

End Sub
Private Sub XLEvents_SheetSelectionChange(ByVal Sh As Object, ByVal Target As Range)

End Sub
Private Sub XLEvents_WindowActivate(ByVal Wb As Workbook, ByVal Wn As Window)

End Sub
Private Sub XLEvents_WindowDeactivate(ByVal Wb As Workbook, ByVal Wn As Window)

End Sub
Private Sub XLEvents_WindowResize(ByVal Wb As Workbook, ByVal Wn As Window)

End Sub
Private Sub XLEvents_WorkbookActivate(ByVal Wb As Workbook)

End Sub
Private Sub XLEvents_WorkbookAddinInstall(ByVal Wb As Workbook)

End Sub
Private Sub XLEvents_WorkbookAddinUninstall(ByVal Wb As Workbook)

End Sub
Private Sub XLEvents_WorkbookBeforeClose(ByVal Wb As Workbook, Cancel As Boolean)

End Sub
Private Sub XLEvents_WorkbookBeforePrint(ByVal Wb As Workbook, Cancel As Boolean)

End Sub
Private Sub XLEvents_WorkbookBeforeSave(ByVal Wb As Workbook, ByVal SaveAsUI As Boolean, Cancel As Boolean)

End Sub
Private Sub XLEvents_WorkbookDeactivate(ByVal Wb As Workbook)

End Sub
Private Sub XLEvents_WorkbookNewSheet(ByVal Wb As Workbook, ByVal Sh As Object)

End Sub
Private Sub XLEvents_WorkbookOpen(ByVal Wb As Workbook)

End Sub

