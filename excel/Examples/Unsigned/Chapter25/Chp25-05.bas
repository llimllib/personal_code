Attribute VB_Name = "Listing_25_05"
Option Explicit
'
' Listing 25.5. A procedure that determines the number of bytes available on a disk.
'
Declare Function GetDiskFreeSpace Lib "kernel32" Alias "GetDiskFreeSpaceA" (ByVal lpRootPathName As String, lpSectorsPerCluster As Long, lpBytesPerSector As Long, lpNumberOfFreeClusters As Long, lpTotalNumberOfClusters As Long) As Long

Sub GetBytesFree()
    Dim lpSectorsPerCluster As Long
    Dim lpBytesPerSector As Long
    Dim lpNumberOfFreeClusters As Long
    Dim lpTotalNumberOfClusters As Long
    Dim retval As Long
    
    retval = GetDiskFreeSpace("A:\", lpSectorsPerCluster, lpBytesPerSector, lpNumberOfFreeClusters, lpTotalNumberOfClusters)
    If retval <> 0 Then
        Debug.Print "Sectors per cluster: "; lpSectorsPerCluster
        Debug.Print "Bytes per sector: "; lpBytesPerSector
        Debug.Print "Free clusters: "; lpNumberOfFreeClusters
        Debug.Print "Bytes free: "; lpSectorsPerCluster * lpBytesPerSector * lpNumberOfFreeClusters
    End If
End Sub

