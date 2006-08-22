Attribute VB_Name = "Security"
Option Explicit
'
' Listing 19.7. A procedure that creates a new user account,
' appends the user to the Users collection.
'
Sub AddNewUser()
    Dim ws As Workspace
    Dim newUser As User
    '
    ' Set up the secure default workspace
    '
    With DBEngine
        .SystemDB = "C:\Windows\System32\Secure.mdw"
        .DefaultUser = "Administrator"
        .DefaultPassword = "shhhhh"
        Set ws = Workspaces(0)
    End With
    '
    ' Create the user and set their properties
    '
    Set newUser = ws.CreateUser( _
        Name:="Biff", _
        PID:="Biff123", _
        Password:="DoNotTell")
    '
    ' Add the user to the Users collection
    '
    ws.Users.Append newUser
End Sub
'
' Listing 19.8. A procedure that creates a new group.
'
Sub AddGroup()
    Dim ws As Workspace
    Dim grp As Group
    '
    ' Set up the secure default workspace
    '
    With DBEngine
        .SystemDB = "C:\Windows\System32\Secure.mdw"
        .DefaultUser = "Administrator"
        .DefaultPassword = "shhhhh"
        Set ws = Workspaces(0)
    End With
    '
    ' Create the new group and append it
    '
    Set grp = ws.CreateGroup( _
        Name:="Marketing", _
        PID:="mktg1234")
    ws.Groups.Append grp
End Sub
'
' Listing 19.9. A procedure that creates a new user account,
' appends the user to the Users collection, and assigns the
' user to the Marketing group.
'
Sub AddNewUser2()
    Dim ws As Workspace
    Dim newUser As User
    Dim grp As Group
    '
    ' Set up the secure default workspace
    '
    With DBEngine
        .SystemDB = "C:\Windows\System32\Secure.mdw"
        .DefaultUser = "Administrator"
        .DefaultPassword = "shhhhh"
        Set ws = Workspaces(0)
    End With
    '
    ' Create the user and set their properties
    '
    Set newUser = ws.CreateUser("Alphonse")
    newUser.PID = "Al123"
    newUser.Password = "WhoKnows"
    '
    ' Add the user to the Users collection
    '
    ws.Users.Append newUser
    '
    ' Create an object for the Users group and then add
    ' this object to the new user's Groups collection
    '
    Set grp = newUser.CreateGroup("Marketing")
    newUser.Groups.Append grp
End Sub
'
' Listing 19.10. A procedure that sets various permissions.
'
Sub SetPermissions()
    Dim db As Database
    Dim ws As Workspace
    '
    ' Set up the secure default workspace
    '
    With DBEngine
        .SystemDB = "C:\Windows\System32\Secure.mdw"
        .DefaultUser = "Administrator"
        .DefaultPassword = "shhhhh"
        Set ws = Workspaces(0)
    End With
    '
    ' Open a secure version of the Northwind database
    '
    Set db = ws.OpenDatabase("C:\Program Files\Microsoft Office\Access\Secure Northwind.mdb")
    '
    ' Set permissions on Customers table for user "Biff"
    '
    With db.Containers("Tables").Documents("Customers")
        .UserName = "Biff"
        .Permissions = dbSecInsertData + _
                       dbSecReplaceData + _
                       dbSecDeleteData
    End With
    '
    ' Set permissions on all Report objects for "Marketing" group
    '
    With db.Containers("Reports")
        .UserName = "Marketing"
        .Permissions = dbSecFullAccess
    End With
    db.Close
End Sub
