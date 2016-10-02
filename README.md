# Simple DNS Plus - Raw Log Library

A .NET library (.dll) which provides programmatic access (read-only) to raw log files (.sdraw) from [Simple DNS Plus](http://simpledns.com) v. 5.0 and later.

This can be used for extracting and filtering DNS request information for statistics and other purposes.

The source code also serves as an example of how to access raw log files programmatically.

Simple DNS Plus raw log files are enabled in the Options dialog / Logging / Log Files section.
The raw log files are then found in the Simple DNS Plus log files directory (See Options dialog / Logging section). 

This library replaces the "SDNSFileLib.dll" library previously included with the Simple DNS Plus program installation.

### Compatibility

- This library can be referenced from any type of project under .NET Framework 2.0 (including .NET 3.0 / 3.5).
- It can be used from any .NET language (C#, VB.NET, F#, etc.).
- It can be used with Visual Studio as well as other programming environments supporting .NET 2.0.
- It can be re-compiled for later .NET versions without changing anything.

### Download / Installation

Download the latest binary from <https://github.com/jhsoftware/SDNSRawLogDLL/releases> and unzip the file.

### Usage

You can access log entries by instantiating a RawLog object, which is an enumeration (access with for-each) over the log entry items in the raw log file.

### Sample code (VB.NET)

```
Imports JHSoftware.SDNSRawLog

Module Module1
  Sub Main()
    Dim log As New RawLog(Now.AddDays(-1).ToString("yyyyMMdd") & ".sdraw")
    For Each entry As RawLog.Request In log
      Console.WriteLine(entry.QName.ToString & " " & entry.QTypeName)
    Next
  End Sub
End Module
```

### Source code / Git repository

See <https://github.com/jhsoftware/SDNSRawLogDLL>

Contributions are most welcome. 

Fork the repository, create a branch, commit your changes, push, and submit a pull request.
Or just e-mail us the changes :-)

### License

See the [LICENSE](LICENSE.md) file