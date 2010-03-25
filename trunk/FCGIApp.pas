{
FCGIApp unit implements, in my opinion, the best behavior for Web applications: statefull, multi-threaded, blocking and non-multiplexed connection.
This is a native and full Object Pascal implementation that doesn't depend on DLLs or external libraries.
This unit is based on <extlink http://www.fastcgi.com/devkit/doc/fcgi-spec.html>FastCGI specs</extlink>, read it for more details.
The initial state in a <link TFCGIApplication, FastCGI application> is a listening socket, through which it accepts connections from a Web server.
After a FastCGI application <link TFCGIApplication.Run, accepts a connection on its listening socket>,
a <link TFCGIThread> is <link TFCGIThread.Create, created> that executes the FCGI protocol to <link TFCGIThread.ReadRequestHeader, receive> and <link TFCGIThread.SendResponse, send> data.
As the actual Web paradigm is based on non-related requests, FCGIApp uses a Cookie to relate requests of a same browser session.
This cookie is a <link TFCGIThread.SetCurrentFCGIThread, GUID that is associated> to actual <link TFCGIThread, Thread> address.
In this way a statefull and multi-thread behavior is provided.
-Limitations and architectural decisions:-
1. Multiplexing is not supported. Multiplexing don't works with Apache anyway. Indeed Apache don't supports Filter role.
2. Only Sockets is supported, because is more flexible providing the ability to run applications remotely, named pipes is not.
   So IIS is not natively supported, use <link CGIGateway.dpr> instead.
3. Only Responder role is implemented.
4. Event-driven paradigm is not supported, instead FCGIApp uses extensively multi-thread approach.
Author: Wanderlan Santos dos Anjos (wanderlan.anjos@gmail.com)
Date: apr-2008
License: BSD<extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
unit FCGIApp;

// directives for config file support
{.$DEFINE HAS_CONFIG}
{$IFDEF HAS_CONFIG}
  {.$DEFINE CONFIG_MUST_EXIST}  // directive to make config file becomes mandatory
{$ENDIF}

interface

uses
  {$IFNDEF MSWINDOWS}cthreads,{$ENDIF}
  {$IFDEF HAS_CONFIG}IniFiles,{$ENDIF}
  BlockSocket, SysUtils, SyncObjs, Classes, ExtPascalUtils;

type
  // FastCGI record types, i.e. the general function that the record performs
  TRecType = (rtBeginRequest = 1, rtAbortRequest, rtEndRequest, rtParams, rtStdIn, rtStdOut, rtStdErr, rtData, rtGetValues, rtGetValuesResult, rtUnknown);
  // FastCGI roles, only Responder role is supported in this FCGIApp version
  TRole = (rResponder = 1, rAuthorizer, rFilter);
  // FastCGI level status code
  TProtocolStatus = (psRequestComplete, psCantMPXConn, psOverloaded, psUnknownRole, psBusy);
  // HTTP request methods
  TRequestMethod = (rmGet, rmPost, rmHead, rmPut, rmDelete);

  {$M+}
  {
  Each browser session generates a TFCGIThread. On first request it is <link TFCGIThread.Create, created> and a Cookie is associated with it.
  On subsequent requests this <link TFCGIThread.SetCurrentFCGIThread, Cookie is read to recover the original thread address>.
  Each request <link TFCGIThread.Execute, is interpreted as a FastCGI record and executed according> to its <link TRecType, record type>.
  }
  TFCGIThread = class(TThread)
  private
    FRequestID : word; // FastCGI request ID for this thread
    FRole : TRole; // FastCGI Thread role
    FRequestMethod : TRequestMethod; // Current HTTP request method
    FGarbageCollector : TStringList; // Object list to free when the thread to end
    FSocket : TBlockSocket; // Current socket for current FastCGI request
    FGarbage,
    FKeepConn : boolean; // Not used
    FUploadMark,
    FScriptName,
    FFileUploaded,
    FFileUploadedFullName : AnsiString; // HTTP response header @see SetResponseHeader, SetCookie, SendResponse, Response
    FRequestHeader,
    FQuery,
    FCookie : TStringList;
    FLastAccess : TDateTime;
    FBrowser : TBrowser;
    function GetRequestHeader(Name: string): string;
    function CompleteRequestHeaderInfo(Buffer : AnsiString; I : integer) : boolean;
    function GetCookie(Name: string): string;
    function GetQuery(Name: string) : string;
    function GetQueryAsDouble(Name: string): double;
    function GetQueryAsInteger(Name: string): integer;
    function GetQueryAsTDateTime(Name: string) : TDateTime;
    function GetQueryAsBoolean(Name: string): boolean;
    procedure GarbageCollector(FreeGarbage : boolean);
    procedure WriteUploadFile(Buffer : AnsiString);
    function GetWebServer: string;
  protected
    FResponseHeader : AnsiString; // HTTP response header @see SetResponseHeader, SetCookie, SendResponse, Response
    FRequest, FPathInfo : string;
    FIsAjax,
    FIsUpload,
    FIsDownload,
    NewThread : boolean; // True if is the first request of a thread
    class function URLDecode(Encoded: string): string;
    class function URLEncode(Decoded: string): string;
    procedure AddParam(var S : string; Param: array of string);
    procedure ReadRequestHeader(var RequestHeader : TStringList; Stream : AnsiString; const Cookies : TStringList = nil);
    procedure ReadBeginRequest(var FCGIHeader; Content : AnsiString);
    procedure GetValues(Content : string);
    function HandleRequest(pRequest : AnsiString) : AnsiString;
    function BeforeHandleRequest : boolean; virtual;
    function SetCurrentFCGIThread : boolean;
    procedure AfterHandleRequest; virtual;
    procedure OnError(Msg, Method, Params : string); virtual;
    procedure OnNotFoundError; virtual;
    procedure AfterThreadConstruction; virtual;
    procedure BeforeThreadDestruction; virtual;
    procedure SetPaths; virtual;
    procedure DownloadContentType(Name : string); virtual;
    procedure Refresh;
  public
    BrowserCache  : boolean; // If false generates 'cache-control:no-cache' in HTTP header, default is false
    Response      : string;  // Response string
    ContentType   : string;  // HTTP content-type header, default is 'text/html'
    UploadPath    : string;  // Upload path below document root. Default value is '/uploads'
    MaxUploadSize : integer; // Max size of upload file. Default is MaxLongint(2GB)
    property Role : TRole read FRole; // FastCGI role for the current request
    property Request : string read FRequest; // Request body string
    property PathInfo : string read FPathInfo; // Path info string for the current request
    property LastAccess : TDateTime read FLastAccess; // Last TDateTime access of this thread
    property RequestMethod : TRequestMethod read FRequestMethod; // HTTP request method for the current request
    property RequestHeader[Name : string] : string read GetRequestHeader; // Returns HTTP headers read in the current request
    property Cookie[Name : string] : string read GetCookie; // Returns HTTP cookies read in the current request
    property Query[Name : string] : string read GetQuery; // Returns HTTP query info parameters read in the current request as a string
    property QueryAsBoolean[Name : string] : boolean read GetQueryAsBoolean; // Returns HTTP query info parameters as a boolean
    property QueryAsInteger[Name : string] : integer read GetQueryAsInteger; // Returns HTTP query info parameters as an integer
    property QueryAsDouble[Name : string] : double read GetQueryAsDouble; // Returns HTTP query info parameters as a double
    property QueryAsTDateTime[Name : string] : TDateTime read GetQueryAsTDateTime; // Returns HTTP query info parameters as a TDateTime
    property Queries : TStringList read FQuery; // Returns all HTTP queries as list to ease searching
    property FileUploaded : AnsiString read FFileUploaded; // Last uploaded file
    property FileUploadedFullName : AnsiString read FFileUploadedFullName; // Last uploaded file
    property IsAjax : boolean read FIsAjax; // Tests if execution is occurring in an AJAX request
    property IsUpload : boolean read FIsUpload;
    property IsDownload : boolean read FIsDownload;
    property ScriptName : AnsiString read FScriptName;
    property Browser : TBrowser read FBrowser; // Browser in use in this session
    property WebServer : string read GetWebServer; // WebServer in use in this session
    constructor Create(NewSocket : integer); virtual;
    destructor Destroy; override;
    procedure AddToGarbage(const Name : string; Obj: TObject);
    procedure DeleteFromGarbage(Obj : TObject);
    function FindObject(Name : string) : TObject;
    function ExistsReference(Name : string) : boolean;
    procedure SendResponse(S : AnsiString; pRecType : TRecType = rtStdOut);
    procedure Execute; override;
    procedure SendEndRequest(Status: TProtocolStatus = psRequestComplete);
    procedure SetResponseHeader(Header : AnsiString);
    procedure Alert(Msg : string); virtual;
    procedure SetCookie(Name, Value : string; Expires : TDateTime = 0; Domain : string = ''; Path : string = ''; Secure : boolean = false);
    function  MethodURI(AMethodName : string) : string; overload;
    function  MethodURI(AMethodName : TExtProcedure) : string; overload;
    procedure DownloadBuffer(Name, Buffer : AnsiString; pContentType : string = '');
    procedure Terminate; reintroduce;
  published
    procedure Home; virtual; abstract; // Default method to be called by <link TFCGIThread.HandleRequest, HandleRequest>
    procedure Logout; virtual;
    procedure Shutdown; virtual;
    {$IFDEF HAS_CONFIG}
    procedure Reconfig; virtual;
    {$ENDIF}
  end;
  {$M-}
  TFCGIThreadClass = class of TFCGIThread; // Thread class type to create when a new request arrives

  {
  Statefull and multi-thread behavior for FastCGI applications. This class has a garbage collector that frees idle threads.
  The initial state in a FastCGI application is a listening socket, through which it accepts connections from a Web server.
  After a FastCGI application <link TFCGIApplication.Run, accepts a connection on its listening socket>,
  a <link TFCGIThread> is <link TFCGIThread.Create, created> that executes the FCGI protocol to <link TFCGIThread.ReadRequestHeader, receive> and <link TFCGIThread.SendResponse, send> data.
  }
  TFCGIApplication = class
  private
    FExeName        : string;
    Threads         : TStringList;
    FThreadsCount   : integer;
    AccessThreads   : TCriticalSection;
    FCGIThreadClass : TFCGIThreadClass;
    // Configurable options
    Port        : word;
    MaxConns    : integer;
    MaxIdleTime : TDateTime;
    WebServers  : TStringList;
    procedure GarbageThreads;
  public
    Terminated : boolean; // Set to true to terminate the application
    GarbageNow : boolean; // Set to true to trigger the garbage colletor
    Shutdown   : boolean; // Set to true to shutdown the application after the last thread to end, default is false
    Title      : string;  // Application title used by <link TExtThread.AfterHandleRequest, AfterHandleRequest>
    Icon       : string;  // Icon to show in Browser
    Password   : string;  // Password to be informed in Browser URL, as a query parameter, to Shutdown and Reconfig methods. Default password is extpascal
    HasConfig  : boolean; // True if this application was compiled to have a configuration file
    {$IFDEF HAS_CONFIG}
    Config : TIniFile; // Config file handle
    function ReadConfig : boolean;
    procedure Reconfig(AReload : boolean = true);
    {$ENDIF}
    property ExeName : string read FExeName;
    procedure Run(OwnerThread : TThread = nil);
    function CanConnect(Address : string) : boolean;
    function GetThread(I : integer) : TFCGIThread;
    function ThreadsCount : integer;
    function ReachedMaxConns : boolean;
    procedure OnPortInUseError; virtual;
    constructor Create(pTitle : string; pFCGIThreadClass : TFCGIThreadClass; pPort : word = 2014; pMaxIdleMinutes : word = 30;
      pShutdownAfterLastThreadDown : boolean = false; pMaxConns : integer = 1000);
    destructor Destroy; override;
  end;

var
  Application : TFCGIApplication; // FastCGI application object

threadvar
  CurrentFCGIThread : TFCGIThread; // Current FastCGI thread address assigned by <link TFCGIThread.SetCurrentFCGIThread, SetCurrentFCGIThread> method

implementation

uses
  StrUtils, Math, ExtPascal;

type
  // FastCGI header
  TFCGIHeader = packed record
    Version : byte;     // FastCGI protocol version, ever constant 1
    RecType : TRecType; // FastCGI record type
    ID,                 // Is zero if management request else is data request. Used also to determine if the session is being multiplexed
    Len     : word;     // FastCGI record length
    PadLen  : byte;     // Pad length to complete the alignment boundery that is 8 bytes on FastCGI protocol
    Filler  : byte;     // Pad field
  end;

  // <link TRecType, Begin request> record
  TBeginRequest = packed record
    Header  : TFCGIHeader; // FastCGI header
    Filler  : byte;        // Pad field
    Role    : TRole;       // FastCGI role
    KeepConn: boolean;     // Keep connection
    Filler2 : array[1..5] of byte; // Pad field
  end;

{
Converts a Request string into a FastCGI Header
@param Buffer Input buffer to convert
@param FCGIHeader FastCGI header converted from Buffer
@see MoveFromFCGIHeader
}
procedure MoveToFCGIHeader(var Buffer : AnsiChar; var FCGIHeader : TFCGIHeader); begin
  move(Buffer, FCGIHeader, sizeof(TFCGIHeader));
  {$IFNDEF FPC_BIG_ENDIAN}
  FCGIHeader.ID  := swap(FCGIHeader.ID);
  FCGIHeader.Len := swap(FCGIHeader.Len);
  {$ENDIF}
end;

{
Converts a Request string into a FastCGI Header
@param Buffer Input buffer to convert
@param FCGIHeader FastCGI header converted from Buffer
@see MoveToFCGIHeader
}
procedure MoveFromFCGIHeader(FCGIHeader : TFCGIHeader; var Buffer : AnsiChar); begin
  {$IFNDEF FPC_BIG_ENDIAN}
  FCGIHeader.ID  := swap(FCGIHeader.ID);
  FCGIHeader.Len := swap(FCGIHeader.Len);
  {$ENDIF}
  move(FCGIHeader, Buffer, sizeof(TFCGIHeader));
end;

{
Creates a TFCGIThread to handle a new request to be read from the NewSocket parameter
@param NewSocket Socket to read a new request
}
constructor TFCGIThread.Create(NewSocket : integer); begin
  if Application.FThreadsCount < 0 then Application.FThreadsCount := 0;
  inc(Application.FThreadsCount);
  FGarbageCollector := TStringList.Create;
  FGarbageCollector.Sorted := true;
  FSocket := TBlockSocket.Create(NewSocket);
  FRequestHeader := TStringList.Create;
  FRequestHeader.StrictDelimiter := true;
  FQuery := TStringList.Create;
  FQuery.StrictDelimiter := true;
  FQuery.Delimiter := '&';
  FCookie := TStringList.Create;
  FCookie.StrictDelimiter := true;
  FCookie.Delimiter := ';';
  ContentType := 'text/html';
  inherited Create(false);
end;

{
Deletes a TObject from the Thread Garbage Collector
@param Obj TObject to delete
}
procedure TFCGIThread.DeleteFromGarbage(Obj : TObject);
var
  I : Integer;
begin
  I := FGarbageCollector.IndexOfObject(Obj);
  if I >= 0 then TExtObject(FGarbageCollector.Objects[I]).IsChild := true;
end;

{
Finds a TObject in Garbage collector using its JavaScript name
@param Name Object JS name
@return The object reference if exists else returns nil
}
function TFCGIThread.FindObject(Name : string) : TObject;
var
  I : Integer;
begin
  I := FGarbageCollector.IndexOf(Name);
  if I >= 0 then
    Result := FGarbageCollector.Objects[I]
  else
    Result := nil;
end;

{
Tests if a JSObject was explicitly declared
@param Name Object JS name ("dirty" name)
@return True if declared
}
function TFCGIThread.ExistsReference(Name : string) : boolean; begin
  Result := FGarbageCollector.IndexOf(AnsiReplaceStr(Name, IdentDelim, '')) <> -1;
end;

{
Frees all objects associated for this thread.
@param FreeGarbage If True free garbage object itself.
}
procedure TFCGIThread.GarbageCollector(FreeGarbage : boolean);
var
  I : integer;
begin
  with FGarbageCollector do begin
    for I := Count-1 downto 0 do
      try
        if (Objects[I] <> nil) and not TExtObject(Objects[I]).IsChild then TExtObject(Objects[I]).Free;
      except end;
    if FreeGarbage then Free;
  end;
end;

// Destroys the TFCGIThread invoking the Thread Garbage Collector to free the associated objects
destructor TFCGIThread.Destroy; begin
  GarbageCollector(true);
  FRequestHeader.Free;
  FQuery.Free;
  FCookie.Free;
  dec(Application.FThreadsCount);
  {$IFDEF MSWINDOWS}inherited;{$ENDIF} // Collateral effect of Unix RTL FPC bug
end;

{
Appends or cleans HTTP response header. The HTTP response header is sent using <link TFCGIThread.SendResponse, SendResponse> method.
@param Header Use '' to clean response header else Header parameter is appended to response header
}
procedure TFCGIThread.SetResponseHeader(Header : AnsiString); begin
  if Header = '' then
    FResponseHeader := ''
  else
    FResponseHeader := FResponseHeader + Header + ^M^J;
end;

// Terminates the TFCGIThread, calls <link TFCGIThread.Logout, Logout> method and terminates the application
procedure TFCGIThread.Shutdown; begin
  if Query['password'] = Application.Password then begin
    Logout;
    SendResponse('SHUTDOWN: Service is temporarily shutdown for maintenance. Please, try again after a few moments.');
    Application.Terminated := true;
  end;
end;

procedure TFCGIThread.DownloadBuffer(Name, Buffer : AnsiString; pContentType : string = ''); begin
  if pContentType = '' then
    DownloadContentType(Name)
  else
    ContentType := pContentType;
  FResponseHeader := 'content-disposition:attachment;filename="' + ExtractFileName(Name) + '"'^M^J;
  Response    := Buffer;
  FIsDownload := true;
end;

procedure TFCGIThread.DownloadContentType(Name: string); begin end;

procedure TFCGIThread.Terminate; begin
  inherited;
  CurrentFCGIThread.ReturnValue := 1; // Current thread is sleeping
end;

{
Sets a cookie in HTTP response header
@param Name Cookie name
@param Value Cookie value
@param Expires Cookie expiration date. If zero or not specified, the cookie will expire when the user's session ends.
@param Domain Sets this cookie only if Domain parameter matches against the tail of the fully qualified domain name of the host.
If not specified assumes the current Domain request.
@param Path Sets this cookie only if Path parameter matches against the initial part of pathname component of the URL.
If not specified assumes the current pathname request.
@param Secure If true the cookie will only be transmitted if the communications channel with the host is a secure one (HTTPS only).
The default is false.
}
procedure TFCGIThread.SetCookie(Name, Value: string; Expires: TDateTime; Domain, Path: string; Secure: boolean); begin
  SetResponseHeader('set-cookie:' + Name + '=' + Value + ';' +
    IfThen(Expires <> 0, ' expires=' + FormatDateTime('ddd, dd-mmm-yyyy hh:nn:ss', Expires) + ' GMT;', '') +
    IfThen(Domain <> '', ' domain=' + Domain + ';', '') + IfThen(Path <> '', ' path=' + Path + ';', '') + IfThen(Secure, ' secure', ''))
end;

{
Sends a FastCGI response record to the Web Server. Puts the HTTP header in front of response, generates the FastCGI header and sends using sockets.
@param S String to format using FastCGI protocol
@param pRecType FastCGI record type
@see MoveFromFCGIHeader
@see TBlockSocket.SendString
}
procedure TFCGIThread.SendResponse(S : AnsiString; pRecType : TRecType = rtStdOut);
const
  MAX_BUFFER = 65536 - sizeof(TFCGIHeader);
var
  FCGIHeader : TFCGIHeader;
  Buffer : AnsiString;
  I : integer;
begin
  if pRecType = rtStdOut then begin
    if FRequestMethod = rmHead then S := '';
    FResponseHeader := FResponseHeader + 'content-type:' + ContentType + ^M^J;
    if not BrowserCache and not IsDownload then FResponseHeader := FResponseHeader + 'cache-control:no-cache'^M^J;
    S := FResponseHeader + ^M^J + S;
    FResponseHeader := '';
    ContentType := 'text/html';
  end;
  fillchar(FCGIHeader, sizeof(FCGIHeader), 0);
  with FCGIHeader do begin
    Version := 1;
    ID := IfThen(pRecType in [rtGetValuesResult, rtUnknown], 0, FRequestID);
    RecType := pRecType;
    I := 1;
    repeat
      Len := IfThen((length(S)-I+1) <= MAX_BUFFER, length(S)-I+1, MAX_BUFFER);
      PadLen := Len mod 8; 
      SetLength(Buffer, sizeof(TFCGIHeader) + Len + PadLen);
      MoveFromFCGIHeader(FCGIHeader, Buffer[1]);
      move(S[I], Buffer[sizeof(TFCGIHeader) + 1], Len);
      inc(I, Len);
      FSocket.SendString(Buffer);
    until I > length(S);
  end;
end;

{
Sends an end request record to the Web Server and ends this thread.
@param Status Status of request. Default is psRequestComplete
}
procedure TFCGIThread.SendEndRequest(Status : TProtocolStatus = psRequestComplete); begin
  if Status <> psRequestComplete then begin
    case Status of
      psCantMPXConn : Alert('Multiplexing is not allowed.');
      psOverloaded  : Alert('Maximum connection limit is ' + IntToStr(Application.MaxConns) + ' and was reached.');
      psUnknownRole : Alert('Unknown FastCGI Role received.');
      psBusy        : ;//Alert('Session is busy, try later.');
    end;
    SendResponse(Response);
  end;
  SendResponse(#0#0#0#0#0#0#0#0, rtEndRequest);
  Terminate;
end;

{
Reads the begin request record from FastCGI request to the thread.
@param FCGIHeader FastCGI Header
@param Content Additional bytes from FastCGI request
}
procedure TFCGIThread.ReadBeginRequest(var FCGIHeader; Content : AnsiString);
var
  BeginRequest : TBeginRequest;
begin
  FLastAccess := Now;
  BeginRequest.Header := TFCGIHeader(FCGIHeader);
  move(Content[1], BeginRequest.Filler, sizeof(BeginRequest)-sizeof(TFCGIHeader));
  if BeginRequest.Role in [rResponder..rFilter] then begin
    FRequestID := BeginRequest.Header.ID;
    FRole      := BeginRequest.Role;
    FKeepConn  := BeginRequest.KeepConn; // can't close socket if true
  end
  else
    SendEndRequest(psUnknownRole);
end;

{
Reads HTTP headers and cookies from FastCGI rtParams record type
@param RequestHeader List of HTTP headers to initialize
@param Stream rtParams record type body
@param Cookies List of HTTP cookies to initialize
}
procedure TFCGIThread.ReadRequestHeader(var RequestHeader : TStringList; Stream : AnsiString; const Cookies : TStringList = nil);
var
  I, Pos : integer;
  Len    : array[0..1] of integer;
  Param  : array[0..1] of AnsiString;
begin
  RequestHeader.Clear;
  if Cookies <> nil then Cookies.Clear;
  Pos := 1;
  while Pos < length(Stream) do begin
    for I := 0 to 1 do begin
      Len[I] := byte(Stream[Pos]);
      if Len[I] > 127 then begin
        Len[I] := ((byte(Stream[Pos]) and $7F) shl 24) + (byte(Stream[Pos+1]) shl 16) + (byte(Stream[Pos+2]) shl 8) + byte(Stream[Pos+3]);
        inc(Pos, 4);
      end
      else
        inc(Pos);
      SetLength(Param[I], Len[I]);
    end;
    if Len[0] > 0 then move(Stream[Pos], Param[0][1], Len[0]);
    inc(Pos, Len[0]);
    if Len[1] > 0 then move(Stream[Pos], Param[1][1], Len[1]);
    inc(Pos, Len[1]);
    if Param[0] = 'HTTP_COOKIE' then begin
      if Cookies <> nil then Cookies.DelimitedText := URLDecode(Param[1])
    end
    else
      RequestHeader.Values[Param[0]] := {$IFDEF MSWINDOWS}UTF8Decode{$ENDIF}(Param[1]);
  end;
end;

{
Decodes a URL encoded string to a normal string
@param Encoded URL encoded string to convert
@return A decoded string
}
class function TFCGIThread.URLDecode(Encoded : string) : string;
var
  I : integer;
begin
  Result := {$IFDEF MSWINDOWS}UTF8Decode{$ENDIF}(Encoded);
  I := pos('%', Result);
  while I <> 0 do begin
    Result[I] := chr(StrToIntDef('$' + copy(Result, I+1, 2), 32));
    Delete(Result, I+1, 2);
    I := pos('%', Result);
  end;
end;

{
Encodes a string to fit in URL encoding form
@param Decoded Normal string to convert
@return An URL encoded string
}
class function TFCGIThread.URLEncode(Decoded: string): string;
const
  Allowed = ['A'..'Z','a'..'z', '*', '@', '.', '_', '-', '0'..'9', '$', '!', '''', '(', ')'];
var
  I : integer;
begin
  Result := '';
  for I := 1 to length(Decoded) do
    if Decoded[I] in Allowed then
      Result := Result + Decoded[I]
    else
      Result := Result + '%' + IntToHex(ord(Decoded[I]), 2);
end;

{
Adds a TObject to the Thread Garbage Collector
@param Name JS name or other object identification
@param Obj TObject to add
}
procedure TFCGIThread.AddToGarbage(const Name : string; Obj : TObject); begin
  FGarbageCollector.AddObject(AnsiReplaceStr(Name, IdentDelim, ''), Obj);
end;

{
Processing to execute after <link TFCGIThread.HandleRequest, HandleRequest> method immediately before to <link TFCGIThread.SendResponse, SendResponse>
@see TExtThread.AfterHandleRequest
}
procedure TFCGIThread.AfterHandleRequest; begin end;

// Override this method to takes some action after the FastCGI thread is created
procedure TFCGIThread.AfterThreadConstruction; begin end;

procedure TFCGIThread.Alert(Msg : string); begin
  Response := 'alert("' + Msg + '");'
end;

// Override this method to takes some action before the FastCGI thread is destroyed
procedure TFCGIThread.BeforeThreadDestruction; begin end;

{
Processing to execute before <link TFCGIThread.HandleRequest, HandleRequest> method.
@see TExtThread.BeforeHandleRequest
@return True if the processing is ok else retuns False and the <link TFCGIThread.HandleRequest, HandleRequest> method will not call the published method indicated by PathInfo.
}
function TFCGIThread.BeforeHandleRequest : boolean; begin Result := true end;

procedure TFCGIThread.WriteUploadFile(Buffer : AnsiString);
var
  F : file;
  I, J, Tam : integer;
begin
  if FileUploaded <> '' then begin
    if MaxUploadSize = 0 then begin
      Response := '{success:true,file:"' + FileUploaded + '"}';
      exit;
    end;
    Assign(F, FileUploadedFullName);
    I := pos(FUploadMark, Buffer);
    case I of
      0 : begin // middle blocks
        Reset(F, 1);
        Seek(F, FileSize(F));
        I := 1;
        Tam := length(Buffer);
      end;
      1 : begin // begin block
        Rewrite(F, 1);
        I := pos(^M^J^M^J, Buffer) + 4;
        J := posex(FUploadMark, Buffer, I);
        if J = 0 then begin
          Tam := length(Buffer) - I + 1;
          Response := '{success:false,file:"' + FileUploaded + '"}'
        end
        else begin // unique block
          Tam := J - I - 2;
          Response := '{success:true,file:"' + FileUploaded + '"}'
        end;
      end;
    else // end block
      Reset(F, 1);
      Seek(F, FileSize(F));
      Tam := I - 3;
      I := 1;
      Response := '{success:true,file:"' + FileUploaded + '"}'
    end;
    if (FileSize(F) + Tam) <= MaxUploadSize then
      Blockwrite(F, Buffer[I], Tam);
    Close(F);
  end;
end;

// Sets FLastAccess, FPathInfo, FRequestMethod and FQuery internal fields
function TFCGIThread.CompleteRequestHeaderInfo(Buffer : AnsiString; I : integer) : boolean;
var
  ReqMet, CT : AnsiString;
  J : integer;
begin
  FLastAccess := Now;
  FPathInfo := FRequestHeader.Values['PATH_INFO'];
  if FPathInfo = '' then  // Windows 2003 Server bug
    FPathInfo := copy(FRequestHeader.Values['SCRIPT_NAME'], length(ScriptName) + 1, 100)
  else
    FPathInfo := copy(FPathInfo, 2, 100);
  ReqMet := FRequestHeader.Values['REQUEST_METHOD'];
  case ReqMet[1] of
    'G' : FRequestMethod := rmGet;
    'P' :
      if ReqMet = 'POST' then
        FRequestMethod := rmPost
      else
        FRequestMethod := rmPut;
    'H' : FRequestMethod := rmHead;
    'D' : FRequestMethod := rmDelete;
  end;
  FQuery.DelimitedText := FRequestHeader.Values['QUERY_STRING'];
  for J := 0 to FQuery.Count-1 do
    FQuery[J] := URLDecode(FQuery[J]);
  FIsUpload := false;
  CT := RequestHeader['CONTENT_TYPE'];
  if pos('multipart/form-data', CT) <> 0 then begin
    FIsUpload := true;
    J := pos('=', CT);
    FUploadMark := '--' + copy(CT, J+1, length(CT));
    {$IFDEF FPC} // Very stranger bug in FPC !!!
    I := pos(FUploadMark, Buffer);
    {$ELSE}
    I := posex(FUploadMark, Buffer, I);
    {$ENDIF}
    I := posex('filename="', Buffer, I);
    J := posex('"', Buffer, I+10);
    FFileUploaded := ExtractFileName(copy(Buffer, I+10, J-I-10));
    if FFileUploaded <> '' then
      FFileUploadedFullName := RequestHeader['DOCUMENT_ROOT'] + UploadPath + '/' + FFileUploaded
    else
      Response := '{success:false,message:"File not informed"}'
  end;
  Result := FIsUpload
end;

{
Adds a pair Name/Value to a FastCGI rtGetValuesResult <link TRecType, record type>
@param S Body of rtGetValuesResult <link TRecType, record type>
@param Param Pair Name/Value
}
procedure TFCGIThread.AddParam(var S : string; Param : array of string);
var
  I, J   : integer;
  Len    : array[0..1] of integer;
  Format : array[0..1] of integer;
begin
  for I := 0 to 1 do begin
    Len[I] := length(Param[I]);
    if Len[I] <= 127 then
      Format[I] := 1
    else
      Format[I] := 4;
  end;
  J := length(S);
  SetLength(S, J + Len[0] + Format[0] + Len[1] + Format[1]);
  inc(J);
  for I := 0 to 1 do begin
    if Format[I] = 1 then
      S[J] := char(Len[I])
    else begin
      S[J]   := char(((Len[I] shr 24) and $FF) + $80);
      S[J+1] := char( (Len[I] shr 16) and $FF);
      S[J+2] := char( (Len[I] shr  8) and $FF);
      S[J+3] := char(  Len[I] and $FF);
    end;
    inc(J, Format[I]);
  end;
  move(Param[0][1], S[J], Len[0]);
  move(Param[1][1], S[J + Len[0]], Len[1]);
end;

function TFCGIThread.GetRequestHeader(Name: string): string; begin
  Result := FRequestHeader.Values[Name]
end;

function TFCGIThread.GetQuery(Name: string) : string;  begin
  Result := FQuery.Values[Name]
end;

function TFCGIThread.GetQueryAsBoolean(Name: string): boolean; begin
  Result := Query[Name] = 'true';
end;

function TFCGIThread.GetQueryAsDouble(Name: string): double; begin
  Result := StrToFloatDef(Query[Name], 0)
end;

function TFCGIThread.GetQueryAsInteger(Name: string): integer; begin
  Result := StrToIntDef(Query[Name], 0)
end;

function TFCGIThread.GetQueryAsTDateTime(Name: string): TDateTime; begin
  Result := StrToFloatDef(Query[Name], 0)
end;

function TFCGIThread.GetCookie(Name: string): string; begin
  Result := FCookie.Values[Name]
end;

{
Handles the FastCGI rtGetValues record type and sends a rtgetValuesResult record type
@param Body of rtGetValues record type
}
procedure TFCGIThread.GetValues(Content : string);
var
  Values : TStringList;
  GetValuesResult : string;
begin
  if Content = '' then exit;
  Values := TStringList.Create;
  ReadRequestHeader(Values, Content);
  GetValuesResult := '';
  if Values.IndexOf('FCGI_MAX_CONNS')  <> -1 then AddParam(GetValuesResult, ['FCGI_MAX_CONNS', IntToStr(Application.MaxConns)]);
  if Values.IndexOf('FCGI_MAX_REQS')   <> -1 then AddParam(GetValuesResult, ['FCGI_MAX_REQS',  IntToStr(Application.MaxConns)]);
  if Values.IndexOf('FCGI_MPXS_CONNS') <> -1 then AddParam(GetValuesResult, ['FCGI_MPXS_CONNS', '0']);
  Values.Free;
  SendResponse(GetValuesResult, rtGetValuesResult);
end;

function TFCGIThread.GetWebServer: string; begin
  Result := RequestHeader['Server_Software'];
  if Result = '' then Result := 'Embedded';
end;

{
Handles "method not found" error. Occurs when PathInfo not matches a published method declared in this thread. Can be overrided in descendent thread class
@see HandleRequest
}
procedure TFCGIThread.OnNotFoundError; begin
  Alert('Method: ''' + PathInfo + ''' is not found');
end;

// Handles errors raised in the method called by PathInfo in <link TFCGIThread.HandleRequest, HandleRequest> method. Occurs when PathInfo not matches a published method declared in this thread. Can be overrided in descendent thread class
procedure TFCGIThread.OnError(Msg, Method, Params : string); begin
  Alert(Msg + '\non Method: ' + Method + '\nParams: ' + Params);
end;

// Ends current Browser session and triggers the Garbage Collector
procedure TFCGIThread.Logout; begin
  Response := 'window.close();';
  FGarbage := true;
end;

{
Returns the URI address of a Method. Doesn't test if Method name is invalid.
@param AMethodName Method name.
}
function TFCGIThread.MethodURI(AMethodName : string) : string; begin
  Result := ScriptName + AMethodName;
end;

{
Returns the URI address of a Method. If method is not published raises an exception
@param AMethodName Method reference
}
function TFCGIThread.MethodURI(AMethodName : TExtProcedure) : string; begin
  Result := CurrentFCGIThread.MethodName(@AMethodName);
  if Result <> '' then
    Result := MethodURI(Result)
  else
    raise Exception.Create('MethodURI: Method is not published');
end;

{
Sets the context of current thread to the context of associated session using a cookie (<b>FCGIThread</b>).
When a browser session sends its first request this method associates the current browser session, this first thread, to a new cookie (<b>FCGIThread</b>),
whose value is a <extlink http://en.wikipedia.org/wiki/GUID>GUID</extlink>.
In subsequent requests this cookie is the key to find the browser session, i.e. the original <link TFCGIThread, Thread>.
In this way a statefull and multi-thread behavior is provided.
@return False if it fails to find the session associated with the cookie, for example if the session already expired.
}
function TFCGIThread.SetCurrentFCGIThread : boolean;
var
  Thread : string;
  GUID : TGUID;
  I : integer;
  Temp : TStringList;
begin
  Result := true;
  Application.AccessThreads.Enter;
  I := Application.Threads.IndexOf(Cookie['FCGIThread']);
  if I = -1 then begin
    I := Application.Threads.IndexOf(Cookie[' FCGIThread']);
    if I <> -1 then SetCookie('FCGIThread', Cookie[' FCGIThread']);
  end;
  if I = -1 then begin
    NewThread := true;
    if Application.ReachedMaxConns then begin
      SendEndRequest(psOverloaded);
      Result := false;
    end
    else begin
      CreateGUID(GUID);
      Thread := GUIDToString(GUID);
      SetCookie('FCGIThread', Thread);
      Application.Threads.AddObject(Thread, Self);
      AfterThreadConstruction;
    end;
    FScriptName := FRequestHeader.Values['SCRIPT_NAME'];
    if FScriptName[length(FScriptName)] <> '/' then FScriptName := FScriptName + '/';
  end
  else begin
    CurrentFCGIThread := TFCGIThread(Application.Threads.Objects[I]);
    if CurrentFCGIThread.ReturnValue = 1 then begin // Current thread is sleeping
      CurrentFCGIThread.ReturnValue := 0; // Wakeup it
      Temp := Explode(FRequestHeader.Delimiter, FRequestHeader.DelimitedText);
      CurrentFCGIThread.FRequestHeader.Assign(Temp);
      Temp.Free;
      Temp := Explode(FCookie.Delimiter, FCookie.DelimitedText);
      CurrentFCGIThread.FCookie.Assign(Temp);
      Temp.Free;
      CurrentFCGIThread.NewThread := false;
      CurrentFCGIThread.FResponseHeader := '';
      CurrentFCGIThread.ContentType := 'text/html';
      Application.Threads.AddObject('0', Self);
    end
    else begin // Current thread is busy
      Result := false;
      SendEndRequest(psBusy);
    end;
  end;
  Application.AccessThreads.Leave;
end;

procedure TFCGIThread.SetPaths; begin end;

{
The thread main loop.<p>
On receive a request, each request, on its execution cycle, does:
  * <link MoveToFCGIHeader, Reads its FCGI header>
  * Depending on <link TRecType, record type> does:
    * <link TFCGIThread.ReadBeginRequest, Starts a request> or
    * <link TFCGIThread.Logout, Aborts the request> or
    * <link TFCGIThread.SendEndRequest, Ends the request> or
    * <link TFCGIThread.ReadRequestHeader, Reads HTTP headers> or
    * <link TFCGIThread.HandleRequest, Handles the request> with these internal steps:
      * <link TFCGIThread.BeforeHandleRequest, BeforeHandleRequest>
      * The <link TFCGIThread.HandleRequest, HandleRequest> own method
      * <link TFCGIThread.AfterHandleRequest, AfterHandleRequest>
}
procedure TFCGIThread.Execute;
var
  FCGIHeader : TFCGIHeader;
  Buffer, Content : AnsiString;
  I : integer;
begin
  CurrentFCGIThread := Self;
  FRequest := '';
  try
    if Application.CanConnect(FSocket.GetHostAddress) then
      repeat
        if FSocket.WaitingData > 0 then begin
          Buffer := FSocket.RecvPacket;
          if FSocket.Error <> 0 then
            Terminate
          else begin
            I := 1;
            while I <= length(Buffer) do begin
              MoveToFCGIHeader(Buffer[I], FCGIHeader);
              if (FRequestID <> 0) and (FCGIHeader.ID <> 0) and (FCGIHeader.ID <> FRequestID) then
                SendEndRequest(psCantMPXConn)
              else begin
                inc(I, sizeof(FCGIHeader));
                Content := copy(Buffer, I, FCGIHeader.Len);
                case FCGIHeader.RecType of
                  rtBeginRequest : ReadBeginRequest(FCGIHeader, Content);
                  rtAbortRequest : Logout;
                  rtGetValues    : GetValues(Content);
                  rtParams, rtStdIn, rtData :
                    if Content = '' then begin
                      if FCGIHeader.RecType = rtParams then begin
                        ReadRequestHeader(FRequestHeader, FRequest, FCookie);
                        if SetCurrentFCGIThread then begin
                          FIsUpload := CurrentFCGIThread.CompleteRequestHeaderInfo(Buffer, I);
                          MaxUploadSize := CurrentFCGIThread.MaxUploadSize;
                          if FIsUpload then begin
                            FFileUploaded         := CurrentFCGIThread.FFileUploaded;
                            FFileUploadedFullName := CurrentFCGIThread.FFileUploadedFullName;
                            Response              := CurrentFCGIThread.Response;
                            FUploadMark           := CurrentFCGIThread.FUploadMark;
                          end;
                        end
                        else
                          break;
                      end
                      else begin
                        if pos(IISDelim, FRequest) = length(FRequest) then delete(FRequest, length(fRequest), 1); // IIS bug
                        CurrentFCGIThread.FIsUpload := FIsUpload;
                        CurrentFCGIThread.Response  := Response;
                        Response        := CurrentFCGIThread.HandleRequest(FRequest);
                        FResponseHeader := CurrentFCGIThread.FResponseHeader;
                        ContentType     := CurrentFCGIThread.ContentType;
                        FGarbage        := CurrentFCGIThread.FGarbage;
                        FIsDownload     := CurrentFCGIThread.FIsDownload;
                        if (Response <> '') or (RequestMethod in [rmGet, rmHead]) then SendResponse(Response);
                        SendEndRequest;
                      end;
                      FRequest := '';
                    end
                    else
                      if IsUpLoad then
                        WriteUploadFile(Content)
                      else
                        FRequest := FRequest + Content;
                else
                  SendResponse(char(FCGIHeader.RecType), rtUnknown);
                  Buffer := '';
                  sleep(200);
                  break;
                end;
              end;
              inc(I, FCGIHeader.Len);
            end;
          end;
        end
        else
          sleep(5);
      until Terminated
    else
      Terminate;
  except
    on E : Exception do begin
      Content := E.ClassName + ': ' + E.Message + ' at ' + IntToStr(integer(ExceptAddr));
      SendResponse(Content);
      SendResponse(Content, rtStdErr);
      SendEndRequest;
    end;
  end;
  FSocket.Free;
  if FGarbage then begin
    CurrentFCGIThread.FLastAccess := 0;
    FLastAccess := 0;
    Application.GarbageNow := true;
  end;
  {$IFNDEF MSWINDOWS}EndThread(0){$ENDIF} // Unix RTL FPC bug
end;

{
Calls Garbage collector. Optionally used to Refresh the Home page, when user press F5 on browser
@example <code>
if not NewThread then begin
  Refresh;
  EditorGrid := nil;
  DataStore := nil;
end;</code>
}
procedure TFCGIThread.Refresh; begin
  GarbageCollector(false);
end;

{
Calls the published method indicated by PathInfo. Before calls <link TFCGIThread.BeforeHandleRequest, BeforeHandleRequest> method and after calls <link TFCGIThread.AfterHandleRequest, AfterHandleRequest> method.
If PathInfo is null then <link TFCGIThread.Home, Home> method will be called.
The published method will use the Request as input and the Response as output.
<link TFCGIThread.OnError, OnError> method is called if an exception is raised in published method.
<link TFCGIThread.OnNotFoundError, OnNotFoundError> method is called if the published method is not declared in this thread.
@param pRequest Request body assigned to FRequest field or to <link TFCGIThread.Query, Query> array if FRequestMethod is <link TRequestMethod, rmPost>, it is the input to the published method
@return Response body to <link TFCGIThread.SendResponse, send>
}
function TFCGIThread.HandleRequest(pRequest : AnsiString) : AnsiString;
//{DOM-IGNORE-BEGIN
type
  MethodCall = procedure of object;
//DOM-IGNORE-END}
var
  PageMethod : TMethod;
  MethodCode : pointer;
  I : integer;
begin
  if (FRequestMethod = rmPost) and (pos('=', pRequest) <> 0) then begin
    FQuery.DelimitedText := pRequest;
    for I := 0 to FQuery.Count - 1 do
      FQuery[I] := URLDecode(FQuery[I]);
  end
  else
    FRequest := pRequest;
  if not IsUpload then Response := '';
  FIsDownload := false;
  if Browser = brUnknown then
    FBrowser := TBrowser(RCaseOf(RequestHeader['HTTP_USER_AGENT'], ['MSIE', 'Firefox', 'Chrome', 'Safari', 'Opera', 'Konqueror'])+1);
  if BeforeHandleRequest then
    try
      if PathInfo = '' then
        Home
      else
        if not IsUpload or (pos('success:true', Response) <> 0) then begin
          MethodCode := MethodAddress(PathInfo);
          if MethodCode <> nil then begin
            PageMethod.Code := MethodCode;
            PageMethod.Data := Self;
            MethodCall(PageMethod); // Call published method
          end
          else
            OnNotFoundError;
        end;
    except
      on E : Exception do OnError(E.Message, PathInfo, pRequest)
    end;
  AfterHandleRequest;
  if IsDownload or (IsUpload and (Browser = brIE)) then
    Result := Response
  else
    Result := {$IFDEF MSWINDOWS}UTF8Encode{$ENDIF}(Response);
end;

{$IFDEF HAS_CONFIG}

// Calls <link Application.Reconfig> if the password is right
procedure TFCGIThread.Reconfig; begin
  if Query['password'] = Application.Password then begin
    Application.Reconfig;
    SendResponse('RECONFIG: Application configurations are being re-read and reapplied.');
  end;
end;

{
Reads FastCGI port and Password from the configuration file
@return True if the config file exists
}
function TFCGIApplication.ReadConfig : boolean;
var
  ConfigFile : string;
begin
  Result := false;
  ConfigFile := ChangeFileExt(ParamStr(0), {$IFDEF MSWINDOWS}'.ini'{$ELSE}'.conf'{$ENDIF});
  if FileExists(ConfigFile) then
    try
      Config := TINIFile.Create(ConfigFile);
      // changing below options requires restart
      Port := Config.ReadInteger('FCGI', 'Port', Port);
      Password := Config.ReadString('FCGI', 'Password', Password);
      Result := true;
    except end;
end;

{
Reads MaxIdleTime, MaxConns, Shutdown and WServers fields from the config file
@param AReload If true reload from disk
}
procedure TFCGIApplication.Reconfig(AReload : boolean = true);
var
  H, M, S, MS : word;
  ConfigFile, WServers : string;
begin
  if HasConfig then begin
    // force refresh in-memory data
    if AReload then begin
      ConfigFile := Config.FileName;
      Config.Free;
      Sleep(100);
      Config := TINIFile.Create(ConfigFile);
    end;
    DecodeTime(MaxIdleTime, H, M, S, MS);
    MaxIdleTime := EncodeTime(0, Config.ReadInteger('FCGI', 'MaxIdle', M), 0, 0);
    MaxConns    := Config.ReadInteger('FCGI', 'MaxConn', MaxConns);
    Shutdown    := Config.ReadBool('FCGI', 'AutoOff', Shutdown);
    WServers    := Config.ReadString('FCGI', 'InServers', '');
    if WServers <> '' then begin
      if WebServers = nil then WebServers := TStringList.Create;
      WebServers.DelimitedText := WServers;
    end;
  end;
end;
{$ENDIF}

// Frees a TFCGIApplication
destructor TFCGIApplication.Destroy; begin
  {$IFDEF HAS_CONFIG}Config.Free;{$ENDIF}
  Threads.Free;
  AccessThreads.Free;
  WebServers.Free;
  inherited;
end;

{
Creates a FastCGI application instance.
@param pTitle Application title used by <link TExtThread.AfterHandleRequest, AfterHandleRequest>
@param pFCGIThreadClass Thread class type to create when a new request arrives
@param pPort TCP/IP port used to comunicate with the Web Server, default is 2014
@param pMaxIdleMinutes Minutes of inactivity before the end of the thread, releasing it from memory, default is 30 minutes
@param pShutdownAfterLastThreadDown If true Shutdown the application after the last thread to end, default is false. Good for commercial CGI hosting.
@param pMaxConns Maximum accepted connections, default is 1000
}
constructor TFCGIApplication.Create(pTitle : string; pFCGIThreadClass : TFCGIThreadClass; pPort : word = 2014; pMaxIdleMinutes : word = 30;
  pShutdownAfterLastThreadDown : boolean = false; pMaxConns : integer = 1000);
var
  WServers : string;
begin
  Title := pTitle;
  FCGIThreadClass := pFCGIThreadClass;
  Port := pPort;
  Threads := TStringList.Create;
  AccessThreads := TCriticalSection.Create;
  MaxIdleTime := EncodeTime(0, pMaxIdleMinutes, 0, 0);
  Shutdown := pShutdownAfterLastThreadDown;
  MaxConns := pMaxConns;
  WServers := GetEnvironmentVariable('FCGI_WEB_SERVER_ADDRS');
  if WServers <> '' then begin
    WebServers := TStringList.Create;
    WebServers.DelimitedText := WServers;
  end;
  Password := 'extpascal';
  FExeName := ExtractFileName(ParamStr(0));
  {$IFDEF HAS_CONFIG}
  HasConfig := ReadConfig;
  Reconfig(false);
  {$ENDIF}
end;

{
Tests if Address parameter is an IP address in WebServers list
@param Address IP address to find
@return True if Address is in WebServers list
}
function TFCGIApplication.CanConnect(Address : string) : boolean; begin
  Result := (WebServers = nil) or (WebServers.IndexOf(Address) <> -1)
end;

{
Tests if MaxConns (max connections), default is 1000, was reached
@return True if was reached
}
function TFCGIApplication.ReachedMaxConns : boolean; begin
  Result := Threads.Count >= MaxConns
end;

// Thread Garbage Collector. Frees all expired threads
procedure TFCGIApplication.GarbageThreads;
var
  I : integer;
  Thread : TFCGIThread;
begin
  for I := Threads.Count-1 downto 0 do begin
    Thread := TFCGIThread(Threads.Objects[I]);
    if (Now - Thread.LastAccess) > MaxIdleTime then begin
      AccessThreads.Enter;
      try
        if Threads.Strings[I] <> '0' then Thread.BeforeThreadDestruction;
        Thread.Free
      except end;
      Threads.Delete(I);
      AccessThreads.Leave;
    end;
  end;
end;

{
Returns the Ith thread
@param I Index of the thread to return
}
function TFCGIApplication.GetThread(I : integer) : TFCGIThread; begin
  Result := TFCGIThread(Threads.Objects[I])
end;

{
Handles "Port #### already in use" error. Occurs when the port is already in use for another service or application.
Can be overrided in descendent thread class. It shall be overrided if the application is a service.
@see Create
@see Run
}
procedure TFCGIApplication.OnPortInUseError; begin
  writeln('Port: ', Port, ' already in use.');
  sleep(10000);
end;

type
  THackThread = class(TThread); // Internal hack

{
The application main loop. Listens a socket port, through which it accepts connections from a Web server.
For each connection a <link TFCGIThread> is <link TFCGIThread.Create, created> that executes the FCGI protocol
to <link TFCGIThread.ReadRequestHeader, receive> and <link TFCGIThread.SendResponse, send> data.
@param OwnerThread Optional parameter to use with a service thread, see ExtPascalSamples.pas.
When this thread is terminated the application is terminated too.
}
procedure TFCGIApplication.Run(OwnerThread : TThread = nil);
var
  NewSocket, I : integer;
begin
  {$IFDEF HAS_CONFIG}
  {$IFDEF CONFIG_MUST_EXIST}
  if not HasConfig then begin
    writeln('Config: Required configuration file is not found.');
    sleep(10000);
    exit;
  end;
  {$ENDIF}
  if HasConfig then begin
    if not Config.ReadBool('FCGI', 'Enabled', true) then begin
      writeln('Config: Application is being disabled by config file.');
      sleep(10000);
      exit;
    end;
  end;
  {$ENDIF}
  I := 0;
  FThreadsCount := -1;
  with TBlockSocket.Create do begin
    Bind(Port, 1000);
    if Error = 0 then
      repeat
        NewSocket := Accept(250);
        if NewSocket <> 0 then FCGIThreadClass.Create(NewSocket);
        if ((I mod 40) = 0) or GarbageNow then begin // A garbage for each 10 seconds
          GarbageThreads;
          GarbageNow := false;
          I := 0;
        end;
        inc(I);
      until Terminated or (Shutdown and (ThreadsCount = 0)) or ((OwnerThread <> nil) and THackThread(OwnerThread).Terminated)
    else
      OnPortInUseError;
    Free;
  end;
end;

// Returns the number of active threads
function TFCGIApplication.ThreadsCount : integer; begin
  Result := FThreadsCount
end;

initialization

finalization
  if Application <> nil then Application.Free
end.
