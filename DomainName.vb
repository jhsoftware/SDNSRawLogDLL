''' <summary>Immutable object representing a domain name</summary>
<DebuggerDisplay("{DebuggerValue}")> _
Public Class DomainName
  Implements IComparable(Of DomainName)
  Implements IEquatable(Of DomainName)

  REM this must be first - otherwise we get error for constant values below
  Private Shared ParseBuf(320) As Byte

  ''' <summary>Constant for the root domain name</summary>
  Public Shared ReadOnly Root As DomainName = DomainName.FromBytes(New Byte() {0}, 0, 1)

  Private Bytes As Byte()

  ''' <summary>For Visual Studio debugger visualization only</summary>
  Public ReadOnly Property DebuggerValue() As String
    Get
      Return ToString()
    End Get
  End Property

  Private Sub New()
    REM not creatable
  End Sub

  ''' <summary>Compares this domain name with another domain names for equality</summary>
  ''' <param name="other">The domain name to comare to</param>
  Public Overloads Function Equals(ByVal other As DomainName) As Boolean Implements System.IEquatable(Of DomainName).Equals
    Return (Me = other)
  End Function

  ''' <summary>Calculates hash code</summary>
  Public Overrides Function GetHashCode() As Integer
    REM "One-at-a-time hash"
#If DEBUG Then
    REM **** slower overflow checks version ****
    Dim hash As Long = 0
    For i As Integer = 0 To Bytes.Length - 1
      hash = (hash + Bytes(i)) And &HFFFFFFFFL
      hash = (hash + (hash << 10)) And &HFFFFFFFFL
      hash = hash Xor (hash >> 6)
    Next
    hash = (hash + (hash << 3)) And &HFFFFFFFFL
    hash = hash Xor (hash >> 11)
    hash = (hash + (hash << 15)) And &HFFFFFFFFL
    If hash < &H80000000L Then Return CInt(hash) Else _
      Return -(&H7FFFFFFF - CInt(hash And &H7FFFFFFFL)) - 1
#Else
    REM **** faster no overflow checks version ****
    Dim hash As UInteger = 0
    For i As Integer = 0 To Bytes.Length - 1
      hash += Bytes(i)
      hash += (hash << 10)
      hash = hash Xor (hash >> 6)
    Next
    hash += (hash << 3)
    hash = hash Xor (hash >> 11)
    hash += (hash << 15)
    If hash < &H80000000L Then Return CInt(hash) Else _
      Return -(&H7FFFFFFF - CInt(hash And &H7FFFFFFFL)) - 1
#End If
  End Function

  ''' <summary>String representation of domain name (not native characters)</summary>
  Overrides Function ToString() As String
    If Bytes.Length = 0 Then Return "."
    Dim p As Integer = 0
    Dim i As Integer
    Dim cc As Integer ' current char value
    Dim segLen As Integer
    Dim rv(Bytes.Length - 2) As Char
    Do
      segLen = Bytes(p)
      For i = 0 To segLen - 1
        cc = Bytes(p + i + 1)
        If cc > 31 AndAlso cc < 127 Then
          rv(p + i) = ChrW(cc)
        Else
          rv(p + i) = "?"c
        End If
      Next
      p += segLen + 1
      If p >= Bytes.Length Then Return rv
      rv(p - 1) = "."c
    Loop
  End Function

  ''' <summary>Returns the number of domain name segments (dot-to-dot)</summary>
  Public Function SegmentCount() As Integer
    Dim p As Integer = 0
    SegmentCount = 0
    While p < Bytes.Length
      SegmentCount += 1
      p += Bytes(p) + 1
    End While
  End Function

  ''' <summary>Checks if a domain name ends with another</summary>
  ''' <param name="ewDom">Ends-with domain name</param>
  ''' <remarks>Only checks full domain name segments (dot-to-dot)</remarks>
  Public Function EndsWith(ByVal ewDom As DomainName) As Boolean
    If Bytes Is ewDom.Bytes Then Return True
    If ewDom.Bytes.Length = 0 Then Return True
    Dim p As Integer = 0
    While Bytes.Length - p > ewDom.Bytes.Length
      p += Bytes(p) + 1
    End While
    If Bytes.Length - p < ewDom.Bytes.Length Then Return False
    For i As Integer = 0 To ewDom.Bytes.Length - 1
      If Bytes(p + i) <> ewDom.Bytes(i) Then Return False
    Next
    Return True
  End Function

  ''' <summary>Extracts a domain name from a byte array - No trailing zero is expected</summary>
  ''' <param name="ba">The byte array to extract from</param>
  ''' <param name="FromPos">The position in the buffer to start at</param>
  ''' <param name="Length">The lenght of the domain name bytes</param>
  Public Shared Function FromBytesNT(ByVal ba() As Byte, ByVal FromPos As Integer, ByVal Length As Integer) As DomainName
    Dim rv As New DomainName
    ReDim rv.Bytes(Length - 1)
    Dim p As Integer = FromPos
    Dim segLen As Byte
    Dim i As Integer
    Dim toPos As Integer = 0
    Do
      If p - FromPos = Length Then Return rv
      segLen = ba(p)
      If segLen = 0 Or segLen > 63 Then Throw New ArgumentException
      If p + segLen - FromPos >= Length Then Throw New ArgumentException
      rv.Bytes(toPos) = segLen
      For i = 1 To segLen
        Select Case ba(p + i)
          Case 65 To 90 ' A-Z
            rv.Bytes(toPos + i) = ba(p + i) + CByte(32)
          Case Else
            rv.Bytes(toPos + i) = ba(p + i)
        End Select
      Next
      p += segLen + 1
      toPos += segLen + 1
    Loop
  End Function

  ''' <summary>Extracts a domain name from a byte array - Domain name in buffer much have trailing zero</summary>
  ''' <param name="ba">The byte array to extract from</param>
  ''' <param name="FromPos">The position in the buffer to start at</param>
  ''' <param name="Length">The lenght of the domain name bytes</param>
  Public Shared Function FromBytes(ByVal ba() As Byte, ByVal FromPos As Integer, ByVal Length As Integer) As DomainName
    If Length < 1 Then Throw New ArgumentException
    If ba(FromPos + Length - 1) <> 0 Then Throw New ArgumentException
    Return FromBytesNT(ba, FromPos, Length - 1)
  End Function

  ''' <summary>Copies to network byte representation of the domain name to a byte array</summary>
  ''' <param name="toBA">The byte array to copy to</param>
  ''' <param name="toPos">The position in the byte array to copy to</param>
  Public Sub CopyBytesTo(ByVal toBA() As Byte, ByVal toPos As Integer)
    Bytes.CopyTo(toBA, toPos)
    toBA(toPos + Bytes.Length) = 0
  End Sub

  ''' <summary>Network byte representation of the domain name</summary>
  Public Function GetBytes() As Byte()
    Dim rv(Bytes.Length) As Byte
    Bytes.CopyTo(rv, 0)
    rv(Bytes.Length) = 0
    Return rv
  End Function

  ''' <summary>Network byte representation of the domain name without trailing zero</summary>
  Public Function GetBytesNT() As Byte()
    Dim rv(Bytes.Length - 1) As Byte
    Bytes.CopyTo(rv, 0)
    Return rv
  End Function

  ''' <summary>Compares two domain names for equality</summary>
  ''' <param name="d1">First domain name</param>
  ''' <param name="d2">Second domain name</param>
  Public Shared Operator =(ByVal d1 As DomainName, ByVal d2 As DomainName) As Boolean
    If d1.Bytes Is d2.Bytes Then Return True
    If d1.Bytes.Length <> d2.Bytes.Length Then Return False
    For i As Integer = 0 To d1.Bytes.Length - 1
      If d1.Bytes(i) <> d2.Bytes(i) Then Return False
    Next
    Return True
  End Operator

  ''' <summary>Compares two domain names for inequality</summary>
  ''' <param name="d1">First domain name</param>
  ''' <param name="d2">Second domain name</param>
  Public Shared Operator <>(ByVal d1 As DomainName, ByVal d2 As DomainName) As Boolean
    Return (Not d1 = d2)
  End Operator

  ''' <summary>Concatenates two domain names</summary>
  ''' <param name="d1">First domain name</param>
  ''' <param name="d2">Second domain name</param>
  Public Shared Operator &(ByVal d1 As DomainName, ByVal d2 As DomainName) As DomainName
    Dim rv As New DomainName
    ReDim rv.Bytes(d1.Bytes.Length + d2.Bytes.Length - 1)
    Array.Copy(d1.Bytes, rv.Bytes, d1.Bytes.Length)
    Array.Copy(d2.Bytes, 0, rv.Bytes, d1.Bytes.Length, d2.Bytes.Length)
    Return rv
  End Operator

  ''' <summary>Compares one domain name to another</summary>
  ''' <param name="other">The domain name to compare to</param>
  Public Function CompareTo(ByVal other As DomainName) As Integer Implements System.IComparable(Of DomainName).CompareTo
    If Bytes Is other.Bytes Then Return 0
    Dim p As Integer = 0
    Dim meL, otL, i, cp As Integer
    Do
      REM at the end of either one?
      If p >= Bytes.Length OrElse p >= other.Bytes.Length Then
        Return Bytes.Length.CompareTo(other.Bytes.Length)
      End If
      meL = Bytes(p)
      otL = other.Bytes(p)
      REM Compare data
      For i = 1 To Math.Min(meL, otL)
        cp = Bytes(p + i).CompareTo(other.Bytes(p + i))
        If cp <> 0 Then Return cp
      Next
      REM Compare length
      cp = meL.CompareTo(otL)
      If cp <> 0 Then Return cp
      REM Next segment
      p += meL + 1
    Loop
  End Function

  ''' <summary>Attempts to parse a domain name from a string</summary>
  ''' <param name="s">The string to parse from</param>
  ''' <param name="Result">The resulting domain name</param>
  ''' <returns>Boolean indicating if the domain name was parsed succesfully</returns>
  Public Shared Function TryParse(ByVal s As String, ByRef Result As DomainName) As Boolean
    s = s.ToLower.Replace(" ", "")
    If s = "." Then Result = Root : Return True
    REM remove leading dots
    While s.Length > 0 AndAlso s(0) = "."
      s = s.Substring(1)
    End While
    REM remove trailing dots
    While s.Length > 0 AndAlso s(s.Length - 1) = "."
      s = s.Substring(0, s.Length - 1)
    End While
    REM remove double-dots
    While s.IndexOf("..") >= 0
      s = s.Replace("..", ".")
    End While
    If s.Length = 0 Then Return False

    Dim cc As Integer  ' current character ascii value
    Dim p As Integer = 0 ' current possition
    Dim pbp As Integer = 0 ' parse buffer pos
    Dim SegLen As Integer ' current segment length

    While p < s.Length
      SegLen = 0
      While p + SegLen < s.Length
        cc = AscW(s.Chars(p + SegLen))
        If cc = 46 Then Exit While
        If cc < 33 OrElse cc = 127 Then Return False
        If cc >= 128 Then Return False ' Non-ASCII
        SegLen += 1
      End While
      If SegLen > 63 Then Return False
      If pbp + SegLen + 1 > 255 Then Return False
        ParseBuf(pbp) = CByte(SegLen)
        System.Text.ASCIIEncoding.ASCII.GetBytes(s, p, SegLen, ParseBuf, pbp + 1)
        p += SegLen + 1
        pbp += SegLen + 1
    End While
    Result = New DomainName
    ReDim Result.Bytes(pbp - 1)
    Array.Copy(ParseBuf, Result.Bytes, pbp)
    Return True
  End Function

  ''' <summary>Parses a domain name from a string</summary>
  ''' <param name="s">The string to parse from</param>
  ''' <returns>The resulting domain name</returns>
  ''' <exception cref="ArgumentException">If the string could not be parsed into a domain name</exception>
  Public Shared Function Parse(ByVal s As String) As DomainName
    Dim rv As DomainName = Nothing
    If Not TryParse(s, rv) Then Throw New ArgumentException("Invalid domain name")
    Return rv
  End Function

  ''' <summary>Returns a domain name's parent name</summary>
  Public Function Parent() As DomainName
    If Bytes.Length = 0 Then Return DomainName.Root
    Dim rv As New DomainName
    ReDim rv.Bytes(Bytes.Length - Bytes(0) - 2)
    Array.Copy(Bytes, Bytes(0) + 1, rv.Bytes, 0, rv.Bytes.Length)
    Return rv
  End Function

  ''' <summary>Returns specific segments (dot-to-dot) of a domain name</summary>
  ''' <param name="index">The first segment to retreive (zero based)</param>
  ''' <param name="count">The number of segments to retrieve</param>
  Public Function GetSegments(ByVal index As Integer, ByVal count As Integer) As DomainName
    If count <= 0 Then Return DomainName.Root
    Dim i, p1, p2, nl As Integer
    For i = 1 To index
      p1 += Bytes(p1) + 1
    Next
    p2 = p1
    For i = 1 To count
      p2 += Bytes(p2) + 1
    Next
    nl = p2 - p1
    GetSegments = New DomainName
    ReDim GetSegments.Bytes(nl - 1)
    Array.Copy(Bytes, p1, GetSegments.Bytes, 0, nl)
  End Function

End Class


