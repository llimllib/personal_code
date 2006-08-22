Attribute VB_Name = "DBMaintainer"
Option Explicit
'
' This global variable holds the project's
' currently open database
'
Global db As Database

Sub Main()
    Dim dbObject As Database
    frmDBMaintainer.Show
    Set frmDBMaintainer = Nothing
    Set db = Nothing
    '
    ' To be safe, make sure there are no open Database objects
    '
    For Each dbObject In Workspaces(0).Databases
        dbObject.Close
    Next 'dbObject
End Sub

Sub OpenDB(dbFile As String)
    Dim frm As UserForm
    Dim prop As Property
    Dim isReplica As Boolean
    
    Set db = OpenDatabase(dbFile)
    Set frm = frmDBMaintainer
    With db
        frm.lblDatabase.Caption = .Name
        frm.lblOpen.Caption = "Yes"
        frm.lblConnect.Caption = .Connect
        isReplica = IsAReplica(.Name)
        If isReplica Then
            If .ReplicaID = .DesignMasterID Then
                frm.lblReplica.Caption = isReplica & " (Design Master)"
            Else
                frm.lblReplica.Caption = isReplica & " (Replica)"
            End If
        Else
            frm.lblReplica.Caption = isReplica
        End If
        frm.lblUpdatable.Caption = .Updatable
        frm.lblVersion.Caption = .Version
    End With
End Sub

Sub CloseDB(dbFile As String)
    Dim frm As UserForm
    Set frm = frmDBMaintainer
    With db
        frm.lblDatabase.Caption = "None"
        frm.lblOpen.Caption = ""
        frm.lblConnect.Caption = ""
        frm.lblReplica.Caption = ""
        frm.lblUpdatable.Caption = ""
        frm.lblVersion.Caption = ""
        .Close
    End With
    Set db = Nothing
End Sub

Function IsAReplica(dbName As String) As Boolean
    On Error GoTo ErrorHandler
    Dim replicaResult As Boolean
    replicaResult = False
    '
    ' Need to open the database
    '
    With OpenDatabase(dbName)
        If .Properties("Replicable") = "T" Then
            '
            ' Replicable property is "T" for replicas and
            ' design masters
            '
            replicaResult = True
        Else
            '
            ' Replicable property is "F" for non-replicated
            '
            replicaResult = False
        End If

ReturnResult:
        .Close
        IsAReplica = replicaResult
        Exit Function
    End With
    
ErrorHandler:
    '
    ' We'll branch here if the database has no Replicable
    ' property, which is the same as saying the database
    ' was never made replicable. Therefore, False will be returned.
    '
    Resume ReturnResult
End Function
