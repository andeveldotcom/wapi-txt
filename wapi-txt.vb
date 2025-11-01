Imports System.Diagnostics
Imports System.IO
Imports System.Net
Imports System.Net.Http
Imports System.Reflection
Imports System.Security
Imports System.Security.Cryptography
Imports System.Text
Imports System.Text.RegularExpressions
Imports System.Threading
Imports System.Xml
Imports System.Xml.Schema
Imports System.Xml.Serialization
Imports System.Xml.XPath
Imports System.Xml.Xsl

Public Class Check

    ''' <summary>
    ''' This function checks if the provided record name is valid. It first checks if the length of the record name is within the acceptable range (1-255 characters). 
    ''' If the length is valid, it then checks each sub-record of the domain to ensure they are also valid. 
    ''' The function returns a boolean value indicating whether the record name is valid or not.
    ''' </summary>
    Public Shared Function IsValidRecordName(ByVal idnString As String) As Boolean
        If idnString.Length > 255 OrElse idnString.Length < 1 Then
            Return False
        Else
            Dim subRecords = New Domain(idnString)
            Dim test = True
            For i = 1 To subRecords.MaxLevel
                test = IsValidSubRecordName(subRecords.Level(i).Label)
                If Not test Then Return False
            Next
            Return test
        End If


    End Function
    ''' <summary>
    ''' This function checks if the provided sub-record name is valid. It checks for the length of the string, 
    ''' ensures it contains only valid characters, and checks that it does not contain consecutive hyphens or underscores, 
    ''' or start or end with a hyphen. Returns true if the sub-record name is valid, false otherwise.
    ''' </summary>
    Public Shared Function IsValidSubRecordName(ByVal idnString As String) As Boolean
        If idnString.Length > 63 OrElse idnString.Length < 1 Then
            Return False

        ElseIf idnString.Contains("--") OrElse idnString.Contains("__") OrElse idnString.StartsWith("-") OrElse idnString.EndsWith("-") Then
            Return False
        ElseIf Not ContainOnlyValidCharacters(ContentType.SubRecord, idnString) Then
            Return False
        Else
            Return True
        End If
    End Function
    ''' <summary>
    ''' This function checks if the given string is a valid label. A valid label is a string that has a length between 1 and 63 characters, contains only valid characters, and does not start or end with a hyphen or contain two consecutive hyphens.
    ''' </summary>
    ''' <param name="idnString">The string to be checked.</param>
    ''' <returns>Returns a boolean value indicating whether the string is a valid label or not.</returns>
    Public Shared Function IsValidLabel(ByVal idnString As String) As Boolean
        If idnString.Length > 63 OrElse idnString.Length < 1 Then
            Return False
        ElseIf idnString.Contains("--") OrElse idnString.StartsWith("-") OrElse idnString.EndsWith("-") Then
            Return False
        ElseIf Not ContainOnlyValidCharacters(ContentType.Label, idnString) Then
            Return False
        Else
            Return True
        End If
    End Function
    ''' <summary>
    ''' Checks if the provided string value is valid. A valid string is one that has a length between 1 and 5120 characters and contains only valid characters as defined by the ContainOnlyValidCharacters function.
    ''' </summary>
    ''' <param name="value">The string value to be checked.</param>
    ''' <returns>Returns a boolean value indicating whether the string is valid or not.</returns>
    Public Shared Function IsValidTxtValue(ByVal value As String) As Boolean
        If value.Length > 5120 OrElse value.Length < 1 Then
            Return False
        Else
            Return ContainOnlyValidCharacters(ContentType.TxtValue, value)
        End If
    End Function
    ''' <summary>
    ''' This function checks if the provided domain name is valid or not. It first checks if the length of the domain name is within the acceptable range (4-255 characters). 
    ''' Then it creates a new Domain object and checks if the domain has at least two levels. 
    ''' It also checks if the second level of the domain has a valid name (not containing a dash and having at least two characters). 
    ''' Finally, it checks if each level of the domain has a valid label. 
    ''' If all these conditions are met, the function returns true, indicating that the domain name is valid. Otherwise, it returns false.
    ''' </summary>
    Public Shared Function IsValidDomain(ByVal domainName As String) As Boolean
        If domainName.Length > 255 OrElse domainName.Length < 4 Then
            Return False
        Else
            Dim d = New Domain(domainName)
            If d.MaxLevel < 2 Then
                Return False
            ElseIf d.Level(1).FullName.Length < 2 OrElse d.Level(1).FullName.Contains("-") Then
                Return False
            ElseIf domainName.Contains("..") Then
                Return False
            Else
                Dim test = True
                For i = 1 To d.MaxLevel
                    test = IsValidLabel(d.Level(i).Label)
                Next
                Return test
            End If
        End If
    End Function
    Enum ContentType
        TxtValue
        DomainName
        SubRecord
        Label
    End Enum
    ''' <summary>
    ''' This function checks if the input string contains only valid characters based on the content type. 
    ''' It uses a list of valid characters from a resource file and adds additional valid characters based on the content type.
    ''' </summary>
    ''' <param name="contentType">A string that specifies the type of content. It can be "TxtValue", "DomainName", or "SubRecord".</param>
    ''' <param name="inputString">The string to be checked for valid characters.</param>
    ''' <returns>
    ''' Returns a boolean value. If the input string contains only valid characters, it returns True. Otherwise, it returns False.
    ''' </returns>
    Public Shared Function ContainOnlyValidCharacters(contentType As ContentType, inputString As String) As Boolean
        Dim validChars As String = ResourceLoader.GetStringResource("idna-chars.txt")
        Dim allowedCodesArray As String() = validChars.Split({vbCrLf, vbLf, vbCr}, StringSplitOptions.RemoveEmptyEntries)
        Dim allowedCodesArrayList As New ArrayList(allowedCodesArray)
        If contentType = ContentType.TxtValue Then
            Return True
        Else
            allowedCodesArrayList.Add("002D") ' dash
        End If
        If contentType = ContentType.DomainName Then
            allowedCodesArrayList.Add("002E") ' dot
        ElseIf contentType = ContentType.SubRecord Then
            allowedCodesArrayList.Add("005F") ' underscore
        End If

        For Each c As Char In inputString
            Dim charCode As Integer = Char.ConvertToUtf32(c, 0)
            Dim charCodeHex As String = charCode.ToString("X4")
            If Not allowedCodesArrayList.Contains(charCodeHex) Then
                Return False
            End If
        Next
        Return True
    End Function
End Class

Public Class WapiCredentials
    Public Login As String
    Public Password As String
End Class

Public Class WapiResponse
    Public Code As String
    Public Result As String
End Class

Public Class Config
    Public CredentialsFile As String
End Class
Public Class DomainLevel
    Public FullName As String
    Public Label As String
End Class
''' <summary>
''' Class for representing domain name and its levels.
''' </summary>
Public Class Domain
    Private ReadOnly _levelDict As New Dictionary(Of Integer, DomainLevel)

    ''' <summary>
    '''Initializes a new instance of the Domain class based on the specified domain name.
    ''' </summary>
    ''' <param name="domainName">The domain name that is to be represented by this instance.</param>
    Public Sub New(ByVal domainName As String)
        Dim splitName = domainName.Split("."c)
        OriginalName = domainName
        MaxLevel = splitName.Length
        For i = 1 To splitName.Length
            Dim domLevel As New DomainLevel With {
                    .FullName = String.Join("."c, splitName.Skip(splitName.Length - i)),
                    .Label = splitName.ElementAt(splitName.Length - i)}
            _levelDict.Add(i, domLevel)
        Next
    End Sub

    ''' <summary>
    ''' Gets the part of the domain name for the specified level.
    ''' </summary>
    ''' <param name="lvl">The level number for which the part of the domain name should be obtained.</param>
    ''' <returns>The part of the domain name for the specified level or an empty string if the level does not exist.</returns>
    Public ReadOnly Property Level(lvl As Integer) As DomainLevel
        Get
            If _levelDict.ContainsKey(lvl) Then
                Return _levelDict(lvl)
            Else
                Return Nothing
            End If
        End Get
    End Property

    ''' <summary>
    ''' Gets the original domain name that was used when initializing the instance.
    ''' </summary>
    ''' <returns>The original domain name.</returns>
    Public ReadOnly Property OriginalName As String

    ''' <summary>
    ''' Gets the maximum level of the domain name.
    ''' </summary>
    ''' <returns>The maximum level of the domain name.</returns>
    Public ReadOnly Property MaxLevel As Integer
End Class


Module Configuration

    Public AppFolder = AppContext.BaseDirectory
    Public AppConfig As New Config With {.CredentialsFile = Path.Combine(AppFolder, "wapi-txt.sec")}
    Public WapiCredentials As New WapiCredentials

    ''' <summary>
    ''' Transforms an input XML document using an XSLT stylesheet.
    ''' </summary>
    ''' <param name="inputXml">The input XML document to be transformed.</param>
    ''' <returns>The transformed XML document.</returns>
    Public Function TransformXmlWithXslt(inputXml As XDocument) As XDocument

        Dim xsltString As String = ResourceLoader.GetStringResource("parameters.xslt")
        Dim xslt As New XslCompiledTransform()

        Using reader As XmlReader = XmlReader.Create(New StringReader(xsltString))
            xslt.Load(reader)
        End Using

        Dim outStream As New StringWriter()
        Using writer As XmlWriter = XmlWriter.Create(outStream)
            xslt.Transform(inputXml.CreateReader(), writer)
        End Using

        Dim resultXml As XDocument = XDocument.Parse(outStream.ToString())

        Return resultXml
    End Function

    Private Sub ValidationCallBack(sender As Object, e As ValidationEventArgs)
        Close($"Validation Error: {e.Message}")
    End Sub
    Private Sub Serializer_UnknownNode(sender As Object, e As XmlNodeEventArgs)
        Close($"Unknown Node: {e.Name}	{e.Text}")
    End Sub

    Private Sub Serializer_UnknownAttribute(sender As Object, e As XmlAttributeEventArgs)
        Dim attr As XmlAttribute = e.Attr
        Close($"Unknown attribute {attr.Name}='{attr.Value}'")
    End Sub
    ''' <summary>
    ''' Reads credentials from a file and stores them in the Configuration.WapiCredentials property.
    ''' For .NET Framework (NET462) the file is decrypted using DPAPI.
    ''' For .NET 8 (NET8_0) the file is read as plain XML (no decryption).
    ''' </summary>
    Sub ReadCredentialsFromFile()
#If NET462 Then
        Dim xmlText = DecryptDataFromFile(DataProtectionScope.LocalMachine, AppConfig.CredentialsFile)
#ElseIf NET8_0 Then
         Dim xmlText As String = ""
         If Not File.Exists(AppConfig.CredentialsFile) Then
             Close($"Credentials file ({AppConfig.CredentialsFile}) does not exist.")
         End If
         Try
             xmlText = File.ReadAllText(AppConfig.CredentialsFile, Encoding.UTF8)
         Catch ex As Exception
             Close($"Loading credentials file failed: {ex.Message}")
         End Try
#End If
        Try
            Using reader As New StringReader(xmlText)
                Dim serializer As New XmlSerializer(GetType(WapiCredentials))
                WapiCredentials = DirectCast(serializer.Deserialize(reader), WapiCredentials)
            End Using

        Catch ex As Exception
            Close($"Parsing data from {AppConfig.CredentialsFile} failed: {ex.Message}")
        End Try

    End Sub
    ''' <summary>
    ''' Parses And processes a configuration XML file.
    ''' </summary>
    ''' <param name="filePath">Optional file path of the XML file. If Not provided, a default file path Is used.</param>

    Sub ParseAndProcessConfigXml(Optional filePath As String = Chr(0))
        Dim cfg As Config
        Dim noFilePathDefined As Boolean = False
        If filePath = Chr(0) Or filePath = "" Then
            filePath = AppFolder & "wapi-txt.cfg"
            noFilePathDefined = True
        End If
        Try
            If File.Exists(filePath) Then
                Dim settings As New XmlReaderSettings With {
                     .XmlResolver = New XmlUrlResolver,
                     .DtdProcessing = DtdProcessing.Parse,
                     .ValidationType = ValidationType.DTD,
                     .NameTable = New NameTable,
                     .IgnoreWhitespace = True
                 }
                AddHandler settings.ValidationEventHandler, AddressOf ValidationCallBack
                Dim sr1 As New StreamReader(filePath)

                Dim dtd As String = ResourceLoader.GetStringResource("config.dtd")
                Dim xml As String = sr1.ReadToEnd
                Dim concatenated As String = dtd & System.Environment.NewLine & xml

                sr1.Close()

                Dim reader As XmlReader = XmlReader.Create(New StringReader(concatenated), settings)
                Dim serializer As New XmlSerializer(GetType(Config))

                AddHandler serializer.UnknownNode, AddressOf Serializer_UnknownNode
                AddHandler serializer.UnknownAttribute, AddressOf Serializer_UnknownAttribute

                cfg = CType(serializer.Deserialize(reader), Config)

                AppConfig = cfg
            Else
                If Not noFilePathDefined Then
                    Close($"Configuration file ({filePath}) does not exist.")

                End If
            End If

        Catch ex As Exception
            Close($"Check of configuration file failed: {ex.Message}")
        End Try
    End Sub
End Module

Module ResourceLoader
    ' Loads text resources from assembly manifest or falls back to Resources folder on disk
    Public Function GetStringResource(resourceFileName As String) As String
        If String.IsNullOrEmpty(resourceFileName) Then Return String.Empty
        Dim asm = Assembly.GetExecutingAssembly()
        Dim resNames = asm.GetManifestResourceNames()
        ' Try to find resource by file name suffix (case-insensitive)
        Dim match = resNames.FirstOrDefault(Function(n) n.EndsWith(resourceFileName, StringComparison.OrdinalIgnoreCase))
        If match IsNot Nothing Then
            Using s = asm.GetManifestResourceStream(match)
                If s IsNot Nothing Then
                    Using sr As New StreamReader(s, Encoding.UTF8)
                        Return sr.ReadToEnd()
                    End Using
                End If
            End Using
        End If

        ' Fallback: try to read from Resources folder next to app
        Dim filePath = Path.Combine(AppContext.BaseDirectory, "Resources", resourceFileName)
        If File.Exists(filePath) Then
            Return File.ReadAllText(filePath, Encoding.UTF8)
        End If

        Return String.Empty
    End Function


End Module


Module App

    Public Enum Result
        Ok
        Failed
    End Enum

    Private ReadOnly EnumToStringMap As New Dictionary(Of Result, String) From {
         {Result.Ok, "OK"},
         {Result.Failed, "Failed"}
      }

    Public Function ResultText(res As Result) As String
        Return EnumToStringMap(res)
    End Function

    Function RecordRow(message As String, result As Result) As String
        Return (message).PadRight(Math.Max(80 - ResultText(result).Length, message.Length + 3), "."c) & ResultText(result)
    End Function

    ' Unified severity enum used throughout the code; mapped to platform specifics inside WriteToLog
    Public Enum LogSeverity
        Information
        Warning
        [Error]
    End Enum

    ' Returns application version string. Prefers AssemblyInformationalVersion, falls back to assembly version or file version.
    Public Function GetAppVersion() As String
        Try
            Dim assembliesToTry As New List(Of Assembly) From {
            Assembly.GetEntryAssembly(),
            Assembly.GetExecutingAssembly()
        }

            For Each asm In assembliesToTry
                If asm Is Nothing Then Continue For

                ' 1) informational version
                Dim infoAttr = asm.GetCustomAttribute(Of Reflection.AssemblyInformationalVersionAttribute)()
                If infoAttr IsNot Nothing AndAlso Not String.IsNullOrEmpty(infoAttr.InformationalVersion) Then
                    Return infoAttr.InformationalVersion
                End If

                ' 2) assembly version
                If asm.GetName() IsNot Nothing AndAlso asm.GetName().Version IsNot Nothing Then
                    Return asm.GetName().Version.ToString()
                End If

                ' 3) file version from assembly location (may be empty for single-file/trimming)
                Try
                    If Not String.IsNullOrEmpty(asm.Location) Then
                        Dim vi = FileVersionInfo.GetVersionInfo(asm.Location)
                        If Not String.IsNullOrEmpty(vi.FileVersion) Then
                            Return vi.FileVersion
                        End If
                    End If
                Catch
                    ' ignore
                End Try
            Next

            ' 4) fallback to current process main module file version
            Try
                Dim procFile = Process.GetCurrentProcess().MainModule?.FileName
                If Not String.IsNullOrEmpty(procFile) Then
                    Dim viProc = FileVersionInfo.GetVersionInfo(procFile)
                    If Not String.IsNullOrEmpty(viProc.FileVersion) Then
                        Return viProc.FileVersion
                    End If
                End If
            Catch
                ' ignore
            End Try
        Catch
            ' ignore
        End Try

        Return "unknown"
    End Function

    ''' <summary>
    ''' Closes the application and writes an error message to the console or a specified output.
    ''' </summary>
    ''' <param name="errMessage">The error message to be written.</param>
    ''' <param name="toConsole">Optional. The output to write the error message to. If not specified, the error message will be written to the console.</param>
    Public Sub Close(errMessage As String, Optional toConsole As String = Chr(0))
        If toConsole = Chr(0) Then
            Console.Error.WriteLine(errMessage)
        Else
            Console.Error.WriteLine(toConsole)
        End If
        WriteToLog(errMessage, LogSeverity.Error)
#If DEBUG Then
        Console.WriteLine("Press any key...")
        Console.ReadKey()
#End If

        Environment.Exit(1)
    End Sub




#If NET462 Then
    Sub WriteToLog(eventMessage As String, eventType As LogSeverity)
        Try
            Dim entryType As EventLogEntryType = EventLogEntryType.Information
            Select Case eventType
                Case LogSeverity.Error
                    entryType = EventLogEntryType.Error
                Case LogSeverity.Warning
                    entryType = EventLogEntryType.Warning
                Case LogSeverity.Information
                    entryType = EventLogEntryType.Information
            End Select

            Dim eventLog = New EventLog("Application") With {
                .Source = "Application"
            }

            eventLog.WriteEntry($"WAPI-TXT: {eventMessage}", entryType)

        Catch ex As Exception
            Console.Error.WriteLine($"[Logging]: {ex.Message}")
            Environment.Exit(1)
        End Try
    End Sub

#ElseIf NET8_0 Then

    Sub WriteToLog(eventMessage As String, eventType As LogSeverity)
        Try
            Dim priority As String = "info"
            Select Case eventType
                Case LogSeverity.Error
                    priority = "err"
                Case LogSeverity.Warning
                    priority = "warning"
                Case LogSeverity.Information
                    priority = "info"
            End Select

            Dim loggerArgs As String = $"-p user.{priority} ""WAPI-TXT: {eventMessage}"""

            Try
                Dim psi As New System.Diagnostics.ProcessStartInfo("logger", loggerArgs) With {
                    .RedirectStandardOutput = True,
                    .RedirectStandardError = True,
                    .UseShellExecute = False,
                    .CreateNoWindow = True
                }
                Using proc As System.Diagnostics.Process = System.Diagnostics.Process.Start(psi)
                    If proc IsNot Nothing Then
                        proc.WaitForExit(2000)
                    End If
                End Using
            Catch ex As Exception
                Console.Error.WriteLine($"[Logging][logger fallback]: {ex.Message}")
                Console.Error.WriteLine($"WAPI-TXT: {eventMessage}")
            End Try

        Catch ex As Exception
            Console.Error.WriteLine($"[Logging]: {ex.Message}")
            Environment.Exit(1)
        End Try
    End Sub

#End If

End Module


Module WapiTxt
    ReadOnly Conn As New Wapi
    Dim _xmlData As XDocument


    ''' <summary>
    ''' This function parses the command line arguments and converts them into an XML document. 
    ''' It distinguishes between options (arguments starting with "--"), commands ("create" or "delete") and other arguments.
    ''' In case of an exception, the application is closed with an error message.
    ''' </summary>
    ''' <param name="args">An array of command line arguments.</param>
    ''' <returns>
    ''' An XDocument that represents the parsed command line arguments. Each argument is represented as an XElement.
    ''' Options are represented as "option-{optionName} commands as "command-{commandName} and other arguments as "arg".
    ''' </returns>
    Public Function ParseParameters(args As String()) As XDocument
        Dim xmlArgs As New XDocument(New XElement("arguments"))
        Try
            For i = 0 To args.GetUpperBound(0)
                If args(i).StartsWith("--") Then
                    Dim xe As New XElement($"option-{args(i).Substring(2)}", New XAttribute("id", $"option-{args(i).Substring(2)}"))
                    xmlArgs.Root.Add(xe)
                ElseIf args(i) = "create" Or args(i) = "delete" Then
                    Dim xe As New XElement($"command-{args(i)}", New XAttribute("id", $"command-{args(i)}"))
                    xmlArgs.Root.Add(xe)
                Else
                    Dim xe As New XElement("arg") With {
                        .Value = args(i)
                    }
                    xmlArgs.Root.Add(xe)
                End If
            Next
        Catch ex As Exception
            Close($"[Args] Wrong paramets: {ex.Message}")
        End Try
        Return xmlArgs
    End Function

    Sub ParamsValidationEventHandler(sender As Object, e As ValidationEventArgs)
        If e.Severity = XmlSeverityType.Error Then
            Close("Validation Error: " & e.Message, "Parameters validation failed")
        Else
            WriteToLog($"Parameters validation error: {e.Message}", LogSeverity.Warning)
        End If
    End Sub

    ''' <summary>
    ''' This subroutine validates the parameters passed in the xmlArgs parameter. It transforms the XML using XSLT, 
    ''' then validates the transformed XML against a schema. If the XML is not valid, an exception is thrown and the application is closed.
    ''' </summary>
    ''' <param name="xmlArgs">An XDocument that contains the parameters to be validated.</param>
    Public Sub ValidateParameters(xmlArgs As XDocument)
        Dim xmlTransArgs = TransformXmlWithXslt(xmlArgs)
        Dim xsdText As String = ResourceLoader.GetStringResource("parameters.xsd")
        Dim schema As XmlSchema = XmlSchema.Read(New StringReader(xsdText), AddressOf ParamsValidationEventHandler)

        Dim settings As New XmlReaderSettings()
        settings.Schemas.Add(schema)
        settings.ValidationType = ValidationType.Schema
        settings.ValidationFlags = XmlSchemaValidationFlags.ReportValidationWarnings

        AddHandler settings.ValidationEventHandler, AddressOf ParamsValidationEventHandler

        Try
            Using reader As XmlReader = XmlReader.Create(New StringReader(xmlTransArgs.ToString()), settings)
                While reader.Read()
                End While
            End Using
        Catch ex As Exception
            Close("[Parameters] Parameters are not valid.")
        End Try
    End Sub
    Sub Main(args As String())
        WriteToLog("Application start.", LogSeverity.Information)
        If args.Length < 1 Then
            WriteHelpToOutput()
        Else


            Dim xmlArgs As XDocument = ParseParameters(args)
            ValidateParameters(xmlArgs)


            Dim configFile As String = ""
            Dim optHelp As Boolean = xmlArgs.Descendants("option-help").Any()
            Dim optVersion As Boolean = xmlArgs.Descendants("option-version").Any()
            Dim optEula As Boolean = xmlArgs.Descendants("option-EULA").Any()
            Dim optSet As Boolean = xmlArgs.Descendants("option-set").Any()
            Dim optConfig As Boolean = xmlArgs.Descendants("option-config").Any()
            Dim cmdCreate As Boolean = xmlArgs.Descendants("command-create").Any()
            Dim cmdDelete As Boolean = xmlArgs.Descendants("command-delete").Any()
            Dim optNoCheck As Boolean = xmlArgs.Descendants("option-no-check").Any()
            Dim optLetsSecure As Boolean = xmlArgs.Descendants("option-lets-secure").Any()

            If optConfig Then
                configFile = xmlArgs.XPathSelectElement("//option-config/following-sibling::arg[1]")?.Value
            End If

            If cmdCreate Or cmdDelete Then
                ParseAndProcessConfigXml(configFile)


                Dim domainName As String = xmlArgs.XPathSelectElement("//command-create/following-sibling::arg[1] | //command-delete/following-sibling::arg[1]")?.Value
                Dim recordName As String = xmlArgs.XPathSelectElement("//command-create/following-sibling::arg[2] | //command-delete/following-sibling::arg[2]")?.Value
                Dim recordValue As String = xmlArgs.XPathSelectElement("//command-create/following-sibling::arg[3] | //command-delete/following-sibling::arg[3]")?.Value

                If Not optNoCheck Then
                    If Not Check.IsValidDomain(domainName) Then Close($"[Invalid input] Domain name is not valid: {domainName}")
                    If Not Check.IsValidRecordName(recordName) Then Close($"[Invalid input] Record name is not valid: {recordName}")
                    If Not Check.IsValidTxtValue(recordValue) Then Close($"[Invalid input] Record value is not valid: {recordValue}")
                End If
                If optLetsSecure Then
                    If Not recordName.Contains("_acme-challenge") Then
                        Close("Record name should contain '_acme-challenge' substring.")
                    End If
                End If

                Dim myDomain As New Domain(domainName)
                Dim foundDomain As String = ""
                ReadCredentialsFromFile()
                Conn.SetAuth(Configuration.WapiCredentials.Login, Configuration.WapiCredentials.Password)
                Dim domains = Conn.GetDomains.Result
                Dim responseDomains As WapiResponse = CheckResult(domains)
                If responseDomains.Result = "OK" Then
                    For i = myDomain.MaxLevel To 2 Step -1
                        If OneActiveDomainExist(domains, myDomain.Level(i).FullName) Then
                            foundDomain = myDomain.Level(i).FullName
                            Exit For
                        End If
                    Next

                    If foundDomain.Length > 1 Then
                        Console.WriteLine(RecordRow("Domain " + foundDomain + " is active", Result.Ok))
                    Else
                        Close(RecordRow("Domain found.", Result.Failed))

                    End If
                    If Right(recordName, foundDomain.Length + 1) = "." & foundDomain Then recordName = Left(recordName, recordName.Length - foundDomain.Length - 1)
                    If cmdCreate Then
                        Dim addRow = Conn.CreateTxtRecord(foundDomain, recordName, recordValue).Result
                        Dim responseAddRecord As WapiResponse = CheckResult(addRow)
                        If responseAddRecord.Result = "OK" Then
                            Console.WriteLine(RecordRow("DNS record " + recordName + " created", Result.Ok))
                        Else
                            Close($"[Add record] Request failed: {responseAddRecord.Code} : {responseAddRecord.Result}")

                        End If
                    ElseIf cmdDelete Then
                        Dim records = Conn.GetRecords(foundDomain).Result
                        Dim responseGetRecords As WapiResponse = CheckResult(records)
                        If responseGetRecords.Result = "OK" Then
                            Console.WriteLine(RecordRow("Records from domain " + foundDomain + " retrieved", Result.Ok))
                            Dim recordId As String = GetRecordId(records, recordName, recordValue)
                            If recordId <> "" Then
                                Console.WriteLine(RecordRow("DNS record " + recordName + " (" + recordId + ") founded", Result.Ok))
                                Dim deleteRow As String = Conn.DeleteTxtRecord(foundDomain, recordId).Result
                                Dim responseDeleteRow As WapiResponse = CheckResult(deleteRow)
                                If responseDeleteRow.Result = "OK" Then
                                    Console.WriteLine(RecordRow("DNS record " + recordName + " (" + recordId + ") deleted", Result.Ok))
                                Else
                                    Close($"[Delete row] Request failed: {responseGetRecords.Code} : {responseGetRecords.Result}")
                                End If
                            Else
                                Close("[Get record ID] Record is missing or it is not unique.")

                            End If
                        Else
                            Close($"[Get records] Request failed: {responseGetRecords.Code} : {responseGetRecords.Result}")
                        End If
                    Else
                        Close("[Parameters] Wrong command.")

                    End If

                Else
                    Close($"[Get domains] Request failed: {responseDomains.Code} : {responseDomains.Result}")
                End If
                Dim commit = Conn.CommitChanges(foundDomain).Result
                Dim responseCommitChanges As WapiResponse = CheckResult(commit)
                If responseCommitChanges.Result = "OK" Then
                    Console.WriteLine(RecordRow($"Commit changes for domain {foundDomain}.", Result.Ok))
                Else
                    Close($"[Commit changes] Request failed: {responseCommitChanges.Code} : {responseCommitChanges.Result}")
                End If

            ElseIf optHelp Then
                WriteHelpToOutput()
            ElseIf optEula Then
                WriteEulaToOutput()

            ElseIf optVersion Then
                Console.WriteLine(GetAppVersion())

            ElseIf optSet Then
                ParseAndProcessConfigXml(configFile)
                Dim toSet As String = xmlArgs.XPathSelectElement("//option-set/following-sibling::arg[1]")?.Value
                If toSet = "wapi_credentials" Then
                    Console.WriteLine("You can set your password to WAPI interface now.")
                    Console.WriteLine()
                    Console.Write("Enter WAPI login: ")

                    Dim login As String = Console.ReadLine()

                    Dim password As String = ReadPassword()
                    Dim credentials As New WapiCredentials With {.Login = login, .Password = password}
                    If YesOrNo($"Do you want to store your WAPI credentials to file ({AppConfig.CredentialsFile})?") Then
                        Dim xmlString As String
                        Dim serializer As New XmlSerializer(GetType(WapiCredentials))
                        Using stringWriter As New StringWriter()
                            serializer.Serialize(stringWriter, credentials)
                            xmlString = stringWriter.ToString()
                        End Using
#If NET462 Then
                        EncryptDataToFile(xmlString, DataProtectionScope.LocalMachine, AppConfig.CredentialsFile)
#ElseIf NET8_0 Then
                        SaveDataToFile(xmlString, AppConfig.CredentialsFile)
#End If

                        Console.WriteLine($"Credentials was saved to file {AppConfig.CredentialsFile}.")
                        Const waitTime As Integer = 5

                        Dim endTime As DateTime = DateTime.Now.AddSeconds(waitTime)

                        Console.WriteLine($"Program will be closed within {waitTime} seconds or after pressing any key...")
                        While DateTime.Now < endTime
                            If Console.KeyAvailable Then
                                Console.ReadKey(True)
                                Exit While
                            End If
                            Thread.Sleep(100)
                        End While

                        Console.WriteLine("Program closed.")
                    End If
                Else
                    Close("[Parameters]: Wrong argument.")
                End If
            Else
                Close("[Parameters]: Wrong argument.")
            End If

        End If
#If DEBUG Then
        Console.WriteLine("Press any key...")
        Console.ReadKey()
#End If
    End Sub




    Function ReadPassword() As String
        Dim cki As ConsoleKeyInfo
        Dim password As String
        Dim isPasswordValid As Boolean

        Do
            password = "" ' Reset password
            Dim prompt As String = "Enter WAPI password: "
            Console.Write(prompt)

            Dim currentPosition As Integer = 0

            Do
                cki = Console.ReadKey(intercept:=True)

                Select Case cki.Key
                    Case ConsoleKey.Enter
                        Exit Do
                    Case ConsoleKey.Backspace
                        If currentPosition > 0 Then
                            currentPosition -= 1
                            password = password.Remove(currentPosition, 1)
                        End If

                    Case ConsoleKey.Delete
                        If currentPosition < password.Length Then
                            password = password.Remove(currentPosition, 1)
                        End If
                    Case ConsoleKey.LeftArrow
                        If currentPosition > 0 Then
                            currentPosition -= 1
                        End If
                    Case ConsoleKey.RightArrow
                        If currentPosition < password.Length Then
                            currentPosition += 1
                        End If
                    Case ConsoleKey.Home
                        currentPosition = 0
                    Case ConsoleKey.End
                        currentPosition = password.Length
                    Case Else
                        If Not Char.IsControl(cki.KeyChar) Then
                            password = password.Insert(currentPosition, cki.KeyChar.ToString())
                            currentPosition += 1
                        End If
                End Select
            Loop

            Console.WriteLine() ' Adds a new line for better readability

            Dim passwordValidationResult = ValidatePassword(password)
            If passwordValidationResult.Count = 0 Then
                isPasswordValid = True
            Else
                Console.WriteLine() ' Adds a new line for better readability
                Console.Error.WriteLine("Your password does not meet the following requirements:")
                For Each row In passwordValidationResult
                    Console.WriteLine(row)
                Next
                Console.WriteLine()
                isPasswordValid = Not YesOrNo("Do you want to enter a different one?",
                                              "",
                                              "OK, the provided password will be used regardless of its insufficient complexity.")
                Console.WriteLine()
            End If
        Loop Until isPasswordValid

        Return password
    End Function


    ''' <summary>
    ''' This function validates a password based on the provided parameters. It checks the length of the password, 
    ''' the number of uppercase and lowercase letters, the number of numeric characters, and the number of special characters. 
    ''' If the password does not meet the requirements, it adds the issues to a list and returns this list.
    ''' </summary>
    ''' <param name="pwd">The password to validate.</param>
    ''' <param name="minLength">The minimum length of the password. Default is 8.</param>
    ''' <param name="maxLength">The maximum length of the password. Default is 15.</param>
    ''' <param name="numUpper">The minimum number of uppercase letters. Default is 1.</param>
    ''' <param name="numLower">The minimum number of lowercase letters. Default is 1.</param>
    ''' <param name="numNumbers">The minimum number of numeric characters. Default is 1.</param>
    ''' <param name="numSpecial">The minimum number of special characters. Default is 1.</param>
    ''' <returns>
    ''' A list of strings, each string is an issue with the password. If the list is empty, the password meets all the requirements.
    ''' </returns>
    Function ValidatePassword(pwd As String,
    Optional ByVal minLength As Integer = 8,
    Optional ByVal maxLength As Integer = 15,
    Optional ByVal numUpper As Integer = 1,
    Optional ByVal numLower As Integer = 1,
    Optional ByVal numNumbers As Integer = 1,
    Optional ByVal numSpecial As Integer = 1) As List(Of String)
        Dim passwordIssues As New List(Of String)
        Dim upper As New Regex("[A-Z]")
        Dim lower As New Regex("[a-z]")
        Dim number As New Regex("[0-9]")
        Const pattern As String = "[" & "=!""#()+,-./:;<=>?@[\]^_`{|}~*]"
        Dim special As New Regex(pattern)

        If pwd.Length < minLength Then passwordIssues.Add($"- Must be at least {minLength} character{(If(minLength > 1, "s", ""))} long.")
        If pwd.Length > maxLength Then passwordIssues.Add($"- Must not exceed {maxLength} character{(If(maxLength > 1, "s", ""))} in length.")
        If upper.Matches(pwd).Count < numUpper Then passwordIssues.Add($"- Must contain at least {numUpper} uppercase letter{(If(numUpper > 1, "s", ""))}.")
        If lower.Matches(pwd).Count < numLower Then passwordIssues.Add($"- Must contain at least {numLower} lowercase letter{(If(numLower > 1, "s", ""))}.")
        If number.Matches(pwd).Count < numNumbers Then passwordIssues.Add($"- Must contain at least {numNumbers} numeric character{(If(numNumbers > 1, "s", ""))}.")
        If special.Matches(pwd).Count < numSpecial Then passwordIssues.Add($"- Must contain at least {numSpecial} of the following special character{(If(numSpecial > 1, "s", ""))}: =!""#()+,-./:;<=>?@[]^_`{{|}}~*")
        Return passwordIssues
    End Function



    ''' <summary>
    ''' This function prompts the user with a question and expects a Yes or No response. 
    ''' It loops until a valid response is received. If the response is Yes, it prints a custom message if provided and returns True. 
    ''' If the response is No, it prints a custom message if provided and returns False.
    ''' </summary>
    ''' <param name="prompt">The question to prompt the user with.</param>
    ''' <param name="yes">Optional custom message to print if the response is Yes.</param>
    ''' <param name="no">Optional custom message to print if the response is No.</param>
    ''' <returns>Boolean value indicating whether the response was Yes (True) or No (False).</returns>
    Public Function YesOrNo(prompt As String, Optional yes As String = "", Optional no As String = "") As Boolean
        Dim response As Char
        Do
            Console.Write(prompt & " (Y/N) ")
            response = Char.ToUpper(Console.ReadKey().KeyChar)
            Console.WriteLine()
        Loop While response <> "Y"c And response <> "N"c

        If response = "Y"c Then
            If yes.Length > 0 Then Console.WriteLine(yes)
            Return True
        Else
            If no.Length > 0 Then Console.WriteLine(no)
            Return False
        End If
    End Function
    Public Sub WriteHelpToOutput()
#If NET462 Then
        Dim h As String = ResourceLoader.GetStringResource("help-win.txt")
#ElseIf NET8_0 Then
        Dim h As String = ResourceLoader.GetStringResource("help-linux.txt")
#End If

        Console.Write(h)
    End Sub
    Public Sub WriteEulaToOutput()
        Console.Write(ResourceLoader.GetStringResource("eula.txt"))
    End Sub
    ''' <summary>
    ''' This function checks if there is exactly one active domain in the provided XML string. 
    ''' It takes two parameters: 'domains' which is an XML string containing domain data, and 'domain' which is the name of the domain to check.
    ''' It returns a boolean value indicating whether exactly one active domain with the provided name exists in the XML string.
    ''' </summary>
    Public Function OneActiveDomainExist(domains As String, domain As String) As Boolean
        _xmlData = XDocument.Parse(domains)

        Dim childList As IEnumerable(Of XElement)
        childList = From de In _xmlData.Root.<data>.Elements("domain")
                    Where de.<name>.Value = domain AndAlso de.<status>.Value = "active"
                    Select de

        If childList.Count = 1 Then
            Return True
        Else
            Return False
        End If

    End Function
    Public Function GetRecordId(records As String, name As String, value As String) As String
        _xmlData = XDocument.Parse(records)

        Dim childList As IEnumerable(Of XElement)
        childList = From ve In _xmlData.Root.<data>.Elements("row")
                    Where ve.<name>.Value = name AndAlso ve.<rdata>.Value = value AndAlso ve.<rdtype>.Value = "TXT"
                    Select ve


        If childList.Count = 1 Then

            Return childList(0).<ID>.Value
        Else
            Return ""
        End If

    End Function
    ''' <summary>
    ''' This function checks the result of a WapiResponse. It parses the result into an XML document and checks for the presence of "code" and "result" elements.
    ''' If both elements are present, it returns a new WapiResponse with the values of these elements. If not, it returns a new WapiResponse with a default error message.
    ''' </summary>
    ''' <param name="result">The result string to be checked, expected to be in XML format.</param>
    ''' <returns>
    ''' A WapiResponse object with either the parsed "code" and "result" values from the input string, or a default error message.
    ''' </returns>
    Public Function CheckResult(result As String) As WapiResponse
        If String.IsNullOrWhiteSpace(result) Then
            Return New WapiResponse With {.Code = "0", .Result = "No response or empty result."}
        End If

        Try
            _xmlData = XDocument.Parse(result)
        Catch ex As Exception
            Return New WapiResponse With {.Code = "0", .Result = $"Invalid XML response: {ex.Message}"}
        End Try

        Dim childList = (From ed In _xmlData.Root.Descendants()
                         Where ed.Name.LocalName = "code" Or ed.Name.LocalName = "result"
                         Select ed).ToList()

        If childList.Count = 2 Then
            Return New WapiResponse With {.Code = childList(0).Value, .Result = childList(1).Value}
        Else
            Return New WapiResponse With {.Code = "0", .Result = "Unexpected error."}
        End If
    End Function

    REM wapi.ClientPrepare()

    Class Wapi
        Private _auth As String
        Private _user As String

        ReadOnly _baseAddress As New Uri("https://api.wedos.com/")
        ReadOnly _cookieContainer As New CookieContainer()
        ReadOnly _handler As New HttpClientHandler() With {.CookieContainer = _cookieContainer}
        ReadOnly _client As New HttpClient(_handler) With {.BaseAddress = _baseAddress}
        ''' <summary>
        ''' This function generates a hash value for a given input string using the specified hash algorithm. 
        ''' It first converts the input string into bytes using UTF8 encoding, then computes the hash. 
        ''' The resulting hash byte array is then converted into a hexadecimal string and returned.
        ''' </summary>
        ''' <param name="hashAlgorithm">The hash algorithm to use for generating the hash.</param>
        ''' <param name="input">The input string to generate the hash for.</param>
        ''' <returns>A hexadecimal string representation of the hash value.</returns>
        Private Function GetHash(ByVal hashAlgorithm As HashAlgorithm, ByVal input As String) As String
            Dim data As Byte() = hashAlgorithm.ComputeHash(Encoding.UTF8.GetBytes(input))
            Dim sBuilder As New StringBuilder()
            For i As Integer = 0 To data.Length - 1
                sBuilder.Append(data(i).ToString("x2"))
            Next
            Return sBuilder.ToString()
        End Function

        ''' <summary>
        ''' This subroutine sets the authentication for a user. It takes the login and password as parameters, 
        ''' hashes the password using SHA1, concatenates the login, hashed password and current hour, 
        ''' and then hashes this string again to create the final authentication hash. 
        ''' This hash is then stored in the _auth variable and the login is stored in the _user variable.
        ''' </summary>
        ''' <param name="login">The login of the user.</param>
        ''' <param name="password">The password of the user.</param>
        Public Sub SetAuth(ByVal login As String, ByVal password As String)
            Dim sha1 As SHA1 = SHA1.Create()
            Dim passwordHashString = GetHash(sha1, password)
            Dim authString = login & passwordHashString & Date.Now.ToString("HH")
            Dim authHashString = GetHash(sha1, authString)
            _auth = authHashString
            _user = login
        End Sub

        Public Async Function GetDomains() As Task(Of String)
            Try
                Return Await SendRequest(_auth, "dns-domains-list")
            Catch e As Exception
                Console.WriteLine(e.Message)
                Return ""
            End Try
        End Function
        Public Async Function GetRecords(domain As String) As Task(Of String)
            Try
                Dim domainEsc = SecurityElement.Escape(domain)
                Return Await SendRequest(_auth, "dns-rows-list", $"<domain>{domainEsc}</domain>")
            Catch e As Exception
                Console.WriteLine(e.Message)
                Return ""
            End Try
        End Function
        Public Async Function CommitChanges(domain As String) As Task(Of String)
            Try
                Dim domainEsc = SecurityElement.Escape(domain)
                Return Await SendRequest(_auth, "dns-domain-commit", $"<name>{domainEsc}</name>")
            Catch e As Exception
                Console.WriteLine(e.Message)
                Return ""
            End Try
        End Function
        Public Async Function CreateTxtRecord(domain As String, name As String, value As String) As Task(Of String)
            Try
                Dim domainEsc = SecurityElement.Escape(domain)
                Dim nameEsc = SecurityElement.Escape(name)
                Dim valueEsc = SecurityElement.Escape(value)
                Return Await SendRequest(_auth, "dns-row-add", $"<domain>{domainEsc}</domain>
                  <name>{nameEsc}</name>
                  <ttl>300</ttl>
                  <type>TXT</type>
                  <rdata>{valueEsc}</rdata>")
            Catch e As Exception
                Console.Error.WriteLine(e.Message)
                Return ""
            End Try
        End Function
        Public Async Function DeleteTxtRecord(domain As String, id As String) As Task(Of String)
            Try
                Dim domainEsc = SecurityElement.Escape(domain)
                Dim idEsc = SecurityElement.Escape(id)
                Return Await SendRequest(_auth, "dns-row-delete", $"<domain>{domainEsc}</domain>
                  <row_id>{idEsc}</row_id>")
            Catch e As Exception
                Console.Error.WriteLine(e.Message)
                Return ""
            End Try
        End Function
        Private Async Function GetRequestByteLength(data As FormUrlEncodedContent) As Task(Of Integer)
            Dim encodedString As String = Await data.ReadAsStringAsync()
            Dim encoding As Encoding = Encoding.UTF8
            Dim unesco = Uri.UnescapeDataString(encodedString)
            If unesco.Length >= 8 AndAlso unesco.StartsWith("request=", StringComparison.Ordinal) Then
                unesco = unesco.Remove(0, 8)
            End If
            Dim a = encoding.GetByteCount(unesco)
            Return a
        End Function
        Private Async Function SendRequest(auth As String, requestCommand As String, Optional requestData As String = "") As Task(Of String)

            Const xmlDeclaration As String = "<?xml version=""1.0"" encoding=""UTF-8""?>"
            Dim content As String

            content = $"<request><user>{_user}</user><auth>{auth}</auth><command>{requestCommand}</command>"
            If requestData.Length > 0 Then content += $"<data>{requestData}</data>"
            content += "</request>"
            Dim xe = XElement.Parse(content)
            content = xe.ToString(SaveOptions.DisableFormatting)
            content = xmlDeclaration & content

            Dim source = {New KeyValuePair(Of String, String)("request", content)}
            Dim data As New FormUrlEncodedContent(source)

            Dim dataLengthInBytes As Integer = Await GetRequestByteLength(data)
            If dataLengthInBytes > 1024 Then
                Close("[Request]: Input data is too long.")
            End If
            Try
                Dim response As HttpResponseMessage = Await _client.PostAsync("/wapi/xml", data)
                response.EnsureSuccessStatusCode()

                Dim byt As Byte() = Await response.Content.ReadAsByteArrayAsync()
                Dim ret As String = Encoding.UTF8.GetString(byt)
                Return ret

            Catch ex As Exception
                Console.WriteLine(ex.Message)
                Return ""
            End Try
        End Function
    End Class

End Module




Module SecretProtection
#If NET462 Then

    Sub EncryptDataToFile(ByVal stringToProtect As String, ByVal scope As DataProtectionScope, ByVal fileName As String)
        If stringToProtect Is Nothing OrElse stringToProtect.Length <= 0 Then Throw New ArgumentNullException("StringToProtect")
        If fileName Is Nothing Then Throw New ArgumentNullException("fileName")
        Dim bytesToProtect As Byte() = Encoding.ASCII.GetBytes(stringToProtect)
        Dim entropy As Byte() = Encoding.ASCII.GetBytes("yQw0gTRxNuJ1/Qf5dN2v5KwdXQ1KSXaB")
        Dim encryptedData As Byte() = ProtectedData.Protect(bytesToProtect, entropy, scope)
        Try
            File.WriteAllBytes(fileName, encryptedData)
        Catch ex As Exception
            Close($"Save data to file failed: {ex.Message}")

        End Try
    End Sub 'EncryptDataToFile


    Function DecryptDataFromFile(ByVal scope As DataProtectionScope, ByVal fileName As String) As String
        Dim entropy As Byte() = Encoding.ASCII.GetBytes("yQw0gTRxNuJ1/Qf5dN2v5KwdXQ1KSXaB")
        Dim bytesToUnprotect As Byte()
        Dim unencryptedData As Byte() = New Byte(0) {}
        Try
            bytesToUnprotect = File.ReadAllBytes(fileName)
            unencryptedData = ProtectedData.Unprotect(bytesToUnprotect, entropy, scope)
        Catch ex As Exception
            Close($"Load data from file failed: {ex.Message}")

        End Try
        If unencryptedData Is Nothing OrElse unencryptedData.Length = 0 Then
            Close("No data.")

        End If
        Return Encoding.ASCII.GetString(unencryptedData)
    End Function 'DecryptDataFromStream 

#ElseIf NET8_0 Then

    Sub SaveDataToFile(ByVal xmlString As String, ByVal fileName As String)
        Try
            File.WriteAllText(fileName, xmlString, Encoding.UTF8)
        Catch ex As Exception
            Close($"Saving credentials to file failed: {ex.Message}")
        End Try
    End Sub
    Function LoadDataFromFile(ByVal fileName As String) As String
        Try
            Return File.ReadAllText(fileName, Encoding.UTF8)
        Catch ex As Exception
            Close($"Load data from file failed: {ex.Message}")
            Return ""
        End Try
    End Function 'DecryptDataFromStream 

#End If
End Module 'SecretProtection

Module Module1
    ''' <summary>
    ''' Startup wrapper used by the project when the startup object is set to "wapi_txt.Module1".
    ''' It forwards the command line arguments (excluding the executable path) to `WapiTxt.Main`.
    ''' </summary>
    Sub Main()
        Dim cmdArgs As String() = Environment.GetCommandLineArgs()
        Dim argsArray As String()
        If cmdArgs.Length > 1 Then
            ReDim argsArray(cmdArgs.Length - 2)
            Array.Copy(cmdArgs, 1, argsArray, 0, cmdArgs.Length - 1)
        Else
            argsArray = New String() {}
        End If

        WapiTxt.Main(argsArray)
    End Sub
End Module































