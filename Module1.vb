Module Module1
  Private RTypeAlpha() As String

  Friend Function BytesToUInt32(ByVal FromBA() As Byte, ByRef FromPos As Integer) As UInt32
    BytesToUInt32 = (CUInt(FromBA(FromPos)) << 24) Or _
                    (CUInt(FromBA(FromPos + 1)) << 16) Or _
                    (CUInt(FromBA(FromPos + 2)) << 8) Or _
                    FromBA(FromPos + 3)
    FromPos += 4
  End Function

  Friend Function BytesToInt32(ByVal FromBA() As Byte, ByRef FromPos As Integer) As Int32
    BytesToInt32 = ((FromBA(FromPos) And &H7F) << 24) Or _
                   (CInt(FromBA(FromPos + 1)) << 16) Or _
                   (CInt(FromBA(FromPos + 2)) << 8) Or _
                   FromBA(FromPos + 3)
    FromPos += 4
  End Function

  Private Sub InitRecTypes()
    ReDim RTypeAlpha(255)

    AddRecType(1, "A", "Host Address")
    AddRecType(2, "NS", "Name Server")
    AddRecType(3, "MD", "Mail Destination")
    AddRecType(4, "MF", "Mail Forwarder")
    AddRecType(5, "CNAME", "Alias")
    AddRecType(6, "SOA", "Start Of Authority")
    AddRecType(7, "MB", "Mailbox")
    AddRecType(8, "MG", "Mail Group Member")
    AddRecType(9, "MR", "Mail Rename")
    AddRecType(10, "NULL", "Null")
    AddRecType(11, "WKS", "Well Known Service")
    AddRecType(12, "PTR", "Pointer")
    AddRecType(13, "HINFO", "Host Information")
    AddRecType(14, "MINFO", "Mailbox or Mail List Information")
    AddRecType(15, "MX", "Mail Exchange")
    AddRecType(16, "TXT", "Text Strings")
    AddRecType(17, "RP", "Responsible Person")
    AddRecType(18, "AFSDB", "AFS Data Base Location")
    AddRecType(19, "X25", "X.25 PSDN Address")
    AddRecType(20, "ISDN", "ISDN Address")
    AddRecType(21, "RT", "Route Through")
    AddRecType(22, "NSAP", "NSAP Address")
    AddRecType(23, "NSAP-PTR", "NSAP Pointer")
    AddRecType(24, "SIG", "Security Signature")
    AddRecType(25, "KEY", "Security Key")
    AddRecType(26, "PX", "X.400 Mail Mapping Information")
    AddRecType(27, "GPOS", "Geographical Position")
    AddRecType(28, "AAAA", "IPv6 Host Address")
    AddRecType(29, "LOC", "Location Information")
    AddRecType(30, "NXT", "Next Domain")
    AddRecType(31, "EID", "Endpoint Identifier")
    AddRecType(32, "NIMLOC", "Nimrod Locator")
    AddRecType(33, "SRV", "Server Selection")
    AddRecType(34, "ATMA", "ATM Address")
    AddRecType(35, "NAPTR", "Naming Authority Pointer")
    AddRecType(36, "KX", "Key Exchanger")
    AddRecType(37, "CERT", "Certificate")
    AddRecType(38, "A6", "IPv6 Prefix/Suffix")
    AddRecType(39, "DNAME", "Name Redirection")
    AddRecType(40, "SINK", "Sink")
    AddRecType(41, "OPT", "EDNS0 Options")
    AddRecType(42, "APL", "Address Prefixes List")
    AddRecType(43, "DS", "Delegation Signer")
    AddRecType(44, "SSHFP", "SSH Key Fingerprint")
    AddRecType(45, "IPSECKEY", "IPSec Key")
    AddRecType(46, "RRSIG", "RRset Signature")
    AddRecType(47, "NSEC", "Next Secure")
    AddRecType(48, "DNSKEY", "DNSSEC Key")
    AddRecType(49, "DHCID", "DHCP Information")
    AddRecType(50, "NSEC3", "Next Secure 3")
    AddRecType(51, "NSEC3PARAM", "NSEC3 Parameters")
    AddRecType(52, "TLSA", "TLSA certificate association")
    AddRecType(55, "HIP", "Host Identity Protocol")
    AddRecType(59, "CDS", "Child DS")
    AddRecType(60, "CDNSKEY", "Child DNS key")
    AddRecType(62, "CSYNC", "Child-To-Parent Synchronization")

    AddRecType(99, "SPF", "Sender Policy Framework")
    AddRecType(100, "UINFO", "UINFO")
    AddRecType(101, "UID", "UID")
    AddRecType(102, "GID", "GID")
    AddRecType(103, "UNSPEC", "UNSPEC")

    AddRecType(249, "TKEY", "Transaction Key")
    AddRecType(250, "TSIG", "Transaction Signature")
    AddRecType(251, "IXFR", "Incremental Transfer")
    AddRecType(252, "AXFR", "Zone Transfer")
    AddRecType(253, "MAILB", "Mailbox-related RRs")
    AddRecType(254, "MAILA", "Mail Agent RRs")
    AddRecType(255, "*", "Any records")
  End Sub

  Private Sub AddRecType(ByVal tNum As UInt16, ByVal tAlpha As String, ByVal longName As String)
    If tNum < 256 Then RTypeAlpha(tNum) = tAlpha
  End Sub

  Friend Function RecTypeName(ByVal rtNum As UShort) As String
    If RTypeAlpha Is Nothing Then InitRecTypes()
    If rtNum < 256 Then
      If String.IsNullOrEmpty(RTypeAlpha(rtNum)) Then Return "TYPE" & rtNum
      Return RTypeAlpha(rtNum)
    End If
    If rtNum = 257 Then Return "CAA"
    If rtNum = 65357 Then Return "ALIAS"
    If rtNum = 32768 Then Return "TA"
    If rtNum = 32766 Then Return "DLV"
    Return "TYPE" & rtNum
  End Function

  Friend Function RecClassName(ByVal rcNum As Integer) As String
    Select Case rcNum
      Case 1
        Return "IN"
      Case 2
        Return "CS"
      Case 3
        Return "CH"
      Case 4
        Return "HS"
      Case 254
        Return "None"
      Case 255
        Return "Any"
      Case Else
        Return "CLASS" & rcNum
    End Select
  End Function


End Module
