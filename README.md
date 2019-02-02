# R Clipboard Daemon
==============================================================================
	ClipD.R (R Clipboard Deamon) 
		- using a custom simple fairly safe server client clipboard communication protocol.
		(SFSSCC = cc323f83-592b-4f20-a0d4-48b7c5c51a30)
------------------------------------------------------------------------------
Uses:
	ClipD.R 
	VBA.Module:basRClipboardDeamon

==============================================================================
Communicating between applications over the clipboard has many disadvantages, but can be very fast. 
The user should allway understand that using the clipboard durring application data transfer could break things.
The client and server service should be written in such a maner to identify when information is comming 
to each and take action on the clipboard as fast as possible to minimize user interaction with the clipboard.

This method should only be used over writing to the file system is speed is the priority, and communications via 
sockets/shared memory is prevented due to platform, technology, or security constrainsts.

Protocol:
We are using a UUID for this protocol of "'cc323f83-592b-4f20-a0d4-48b7c5c51a30' followed by the 'message', 'from', 'to' all 
seperated by periods'.' to the clipboard. The from and to are unique identifier for the running application usually 
just the PID. MS Excel can have many workbooks open under the same PID so we append the workbook name to it's PID with spaces and periods removed.

Our Custom Communication protocol:
--------------------------------------------------------------
Nomenclature:
	clip_d UUDI.Message.From.To
Nomenclature Descrption:
	"UUID for SFSSCC"."Message For Actions To Be Performed With Incoming Data"."From's PID"."To PID"

Example:
--------------------------------------------------------------
Excell needs the R scrip to process data in the clipboard and return the results to the clipboard

	cc323f83-592b-4f20-a0d4-48b7c5c51a30.Att.11108RCTQSMonteCarloThroughputTool_LPxlsb.any_R
	cc323f83-592b-4f20-a0d4-48b7c5c51a30.Ack.112.11108RCTQSMonteCarloThroughputTool_LPxlsb
	cc323f83-592b-4f20-a0d4-48b7c5c51a30.Post(NamedProcess).11108RCTQSMonteCarloThroughputTool_LPxlsb.112
	cc323f83-592b-4f20-a0d4-48b7c5c51a30.Ack.112.11108RCTQSMonteCarloThroughputTool_LPxlsb
	[Data written to clipboard by excel client, R-Server with PID 112 reads the data and replys back with]
	cc323f83-592b-4f20-a0d4-48b7c5c51a30.Ack.112.11108RCTQSMonteCarloThroughputTool_LPxlsb
	cc323f83-592b-4f20-a0d4-48b7c5c51a30.Post.112.11108RCTQSMonteCarloThroughputTool_LPxlsb
	cc323f83-592b-4f20-a0d4-48b7c5c51a30.Ack.11108RCTQSMonteCarloThroughputTool_LPxlsb.112
	[Results written to clipboard by R-Server, Excel reads data from clipboard and replys]
	cc323f83-592b-4f20-a0d4-48b7c5c51a30.Ack.11108RCTQSMonteCarloThroughputTool_LPxlsb.any_R_Client.Connect

Messages:
Att = Give me your full attention (and PID/Name)
Ack = Acknowledged 
Post(NamedProcess) =  I'm about to write to the clipboard, process data by named procedure
Post = I'm about to write to the clipboard


UUID for SFSSCC.R PID.Action
cc323f83-592b-4f20-a0d4-48b7c5c51a30


We will be using unique application/vba process name with
==============================================================
In R we get this sessions PID with:
--------------------------------------------------------------
	Sys.getpid()

In Excel we get this sessions PID and workbook name with:
--------------------------------------------------------------
Option Explicit

Private Declare Function GetCurrentProcessId Lib "kernel32" () As LongPtr

Public Function ThisWorkbookPid() As String
    ThisWorkbookPidAndName = GetCurrentProcessId
End Function

Public Function ThisWorkbookPidAndName() As String
    ThisWorkbookPidAndName = ThisWorkbookPid & Replace(Replace(ThisWorkbook.name, " ", vbNullString), ".", vbNullString)
End Function

We will be using UUIDs for unique identifiers, with hyphens without braces.
==============================================================
Also read:
	https://www.r-bloggers.com/a-million-ways-to-connect-r-and-excel/
	http://alandgraf.blogspot.com/2013/02/copying-data-from-excel-to-r-and-back_24.html
	https://www.johndcook.com/blog/r_excel_clipboard/
	https://www.r-bloggers.com/copying-data-from-excel-to-r-and-back/

In R we can readfrom and write to the clipboard using
--------------------------------------------------------------
library(clipr)
read_clip_tbl()
read_clip()
write_clip()

==============================================================
Generating UUIDs, and GUIDs
--------------------------------------------------------------
In R we can use
--------------------------------------------------------------
	require(uuid)
	UUIDgenerate(FALSE) # Random based UUID 
	UUIDgenerate(TRUE) # Time based UUID
--------------------------------------------------------------
In VBA we will use this function:
--------------------------------------------------------------
Public Function CreateGUIDv4( _
    Optional fIncludeBraces As Boolean = False, _
    Optional fIncludeHyphens As Boolean = True) _
As String
    'Modified by jeremy.gerdes@navy.mil from https://stackoverflow.com/a/46474125
    'This creates a V4 (pseudo-random number generated) GUID/UUID per RFC 4122 section 4.4
    'Alternatively we can use this formula:
    '=CONCATENATE(DEC2HEX(RANDBETWEEN(0,4294967295),8),"-",DEC2HEX(RANDBETWEEN(0,65535),4),"-",DEC2HEX(RANDBETWEEN(16384,20479),4),"-",DEC2HEX(RANDBETWEEN(32768,49151),4),"-",DEC2HEX(RANDBETWEEN(0,65535),4),DEC2HEX(RANDBETWEEN(0,4294967295),8))
    Do While Len(CreateGUIDv4) < 32
        If Len(CreateGUIDv4) = 16 Then
            '17th character holds version information
            CreateGUIDv4 = CreateGUIDv4 & Hex$(8 + CInt(Rnd * 3))
        End If
        CreateGUIDv4 = CreateGUIDv4 & Hex$(CInt(Rnd * 15))
    Loop
    Dim strLeftBrace As String
    Dim strRightBrace As String
    Dim strHyphen As String
    If fIncludeBraces Then
        strLeftBrace = "{"
        strRightBrace = "}"
    Else
        strLeftBrace = vbNullString
        strRightBrace = vbNullString
    End If
    If fIncludeHyphens Then
        strHyphen = "-"
    Else
        strHyphen = vbNullString
    End If
    CreateGUIDv4 = _
    strLeftBrace & Mid(CreateGUIDv4, 1, 8) & _
    strHyphen & Mid(CreateGUIDv4, 9, 4) & _
    strHyphen & Mid(CreateGUIDv4, 13, 4) & _
    strHyphen & Mid(CreateGUIDv4, 17, 4) & _
    strHyphen & Mid(CreateGUIDv4, 21, 12) & strRightBrace
End Function

in SQL use the function NewID()
--------------------------------------------------------------
INSERT INTO table_name (ID,Column1,Column2,Column3)
VALUES (NewID(),value1,value2,value3)


Thoughs on Namespace model 
==============================================================
For using the clipboard as a signal to perform work between MS Office products and R, 
this works best as a server client relationship. Each application will be continually or occaisionally 
checking the clipboard for specific values, 

We are essentially building an api that uses the clipboard as the data stream using a request-response messaging paggern
For verb noun adjective apis, read https://www.wordsapi.com/#try free tier is 2,500 calls daily

We could potentially write clients that understood human language commands using 'noun, preposition, verb, preposition, adjective' like
	client.application.r.is.ready.to.recieve
	server.application.msexcell.is.ready.to.send
but that's probably overkill, we are better served by sticking to simpler models like the REST verbs of:
	GET,PUT,POST,DELETE
Read:
	https://stackoverflow.com/questions/2001773/understanding-rest-verbs-error-codes-and-authentication/2022938#2022938
