''' <summary>Provides an enumerable (for-each) collection of entries in a Simple DNS Plus raw log file (.sdraw)</summary>
''' <remarks>Rather then reading the whole log file intro memory at once, it is read sequentially as you move through the collection.
''' This means that a file handle remains open as long the RawLog object exists.</remarks>
Public Class RawLog
  Implements IEnumerable(Of Request)

  Private _FileName As String

  Private Sub New()
    REM private = prevent access 
  End Sub

  ''' <summary>Opens a raw log file and creates an instances of the RawLog object</summary>
  ''' <param name="fileName">The full path and file name of the raw log file (.sdraw)</param>
  Public Sub New(ByVal fileName As String)
    _FileName = fileName
  End Sub

  ''' <summary>Returns an enumerator for the entries in the log file</summary>
  Public Function GetEnumerator() As System.Collections.Generic.IEnumerator(Of Request) Implements System.Collections.Generic.IEnumerable(Of Request).GetEnumerator
    Return New myEnumerator(_FileName)
  End Function

  ''' <summary>Returns an enumerator for the entries in the log file</summary>
  Public Function GetEnumerator1() As System.Collections.IEnumerator Implements System.Collections.IEnumerable.GetEnumerator
    Return GetEnumerator()
  End Function

  Private Class myEnumerator
    Implements IEnumerator(Of Request)

    Private logFile As System.IO.FileStream
    Private buf(1023) As Byte
    Private fLen As Integer
    Private ip4ba(3) As Byte, ip6ba(15) As Byte
    Private curEntry As Request

    Friend Sub New(ByVal fileName As String)
      logFile = New System.IO.FileStream(fileName, _
         IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite, 4096, IO.FileOptions.SequentialScan)
      fLen = CInt(logFile.Length)
    End Sub

    Public ReadOnly Property Current() As Request Implements System.Collections.Generic.IEnumerator(Of Request).Current
      Get
        Return curEntry
      End Get
    End Property
    Public ReadOnly Property Current1() As Object Implements System.Collections.IEnumerator.Current
      Get
        Return curEntry
      End Get
    End Property

    Public Function MoveNext() As Boolean Implements System.Collections.IEnumerator.MoveNext
      If logFile Is Nothing Then Return False
      If logFile.Position >= fLen Then GoTo markNoMore

      Try
        logFile.Read(buf, 0, 10)
      Catch ex As Exception
        GoTo markNoMore
      End Try

      curEntry = New Request
      curEntry.Time = New TimeSpan(0, 0, (CInt(buf(0)) << 16) + (CInt(buf(1)) << 8) + buf(2))
      curEntry.OpCode = (buf(3) >> 3) And CByte(15)
      curEntry.RD = ((buf(3) And 1) = 1)
      curEntry.QType = (CUShort(buf(5)) << 8) + buf(6)
      curEntry.QClass = (CUShort(buf(7)) << 8) + buf(8)

      Dim i As Integer = buf(9) + 1 ' qname length
      Try
        logFile.Read(buf, 0, i + 1)
      Catch ex As Exception
        GoTo markNoMore
      End Try

      curEntry.QName = DomainName.FromBytes(buf, 0, i)

      i = buf(i) ' source ip lenght
      Try
        If i = 4 Then
          logFile.Read(ip4ba, 0, 4)
          curEntry.FromIP = New System.Net.IPAddress(ip4ba)
        ElseIf i = 16 Then
          logFile.Read(ip6ba, 0, 16)
          curEntry.FromIP = New System.Net.IPAddress(ip6ba)
        End If
      Catch ex As Exception
        GoTo markNoMore
      End Try

      Return True

markNoMore:
      If logFile IsNot Nothing Then logFile.Close() : logFile = Nothing
      Return False
    End Function

    Public Sub Reset() Implements System.Collections.IEnumerator.Reset
      If logFile IsNot Nothing Then logFile.Close() : logFile = Nothing
    End Sub

    Private disposedValue As Boolean = False    ' To detect redundant calls
    ' IDisposable
    Protected Overridable Sub Dispose(ByVal disposing As Boolean)
      If Not Me.disposedValue Then
        If disposing Then
          ' free other state (managed objects).
          If logFile IsNot Nothing Then logFile.Close() : logFile = Nothing
        End If

        ' free your own state (unmanaged objects).
        '  set large fields to null.
      End If
      Me.disposedValue = True
    End Sub
#Region " IDisposable Support "
    ' This code added by Visual Basic to correctly implement the disposable pattern.
    Public Sub Dispose() Implements IDisposable.Dispose
      ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
      Dispose(True)
      GC.SuppressFinalize(Me)
    End Sub
#End Region
  End Class

  ''' <summary>An object representing a single DNS request in a Simple DNS Plus raw log file</summary>
  Public Class Request
    ''' <summary>The IP address that the DNS request came from</summary>
    Public FromIP As System.Net.IPAddress
    ''' <summary>Time request was received since midnight</summary>
    Public Time As TimeSpan
    ''' <summary>The DNS request OpCode</summary>
    Public OpCode As Byte
    ''' <summary>The DNS request RD (Recursion Desired) flag</summary>
    Public RD As Boolean
    ''' <summary>The requested domain name</summary>
    Public QName As DomainName
    ''' <summary>The query type (numeric)</summary>
    Public QType As UShort
    ''' <summary>the query class (numeric)</summary>
    Public QClass As UShort

    ''' <summary>The query type (alphanumeric)</summary>
    Public ReadOnly Property QTypeName() As String
      Get
        Return RecTypeName(QType)
      End Get
    End Property

    ''' <summary>the query class (alphanumeric)</summary>
    Public ReadOnly Property QClassName() As String
      Get
        Return RecClassName(QClass)
      End Get
    End Property

    Friend Sub New()
      REM no access
    End Sub

  End Class

End Class
