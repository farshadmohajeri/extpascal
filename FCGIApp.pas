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

interface

uses
  {$IFNDEF MSWINDOWS}cthreads,{$ENDIF}
  BlockSocket, SysUtils, SyncObjs, Classes{$IFDEF FPC}, ExtPascalUtils{$ENDIF};

type
  // FastCGI record types, i.e. the general function that the record performs
  TRecType = (rtBeginRequest = 1, rtAbortRequest, rtEndRequest, rtParams, rtStdIn, rtStdOut, rtStdErr, rtData, rtGetValues, rtGetValuesResult, rtUnknown);
  // FastCGI roles, only Responder role is supported in this FCGIApp version
  TRole = (rResponder = 1, rAuthorizer, rFilter);
  // FastCGI level status code
  TProtocolStatus = (psRequestComplete, psCantMPXConn, psOverloaded, psUnknownRole);
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
    FRequest, FPathInfo : string;
    FRequestMethod : TRequestMethod; // Current HTTP request method
    FGarbageCollector : TStringList; // Object list to free when the thread to end
    FSocket : TBlockSocket; // Current socket for current FastCGI request
    FGarbage,
    FKeepConn : boolean; // Not used
    FResponseHeader : string; // HTTP response header @see SetResponseHeader, SetCookie, SendResponse, Response
    FRequestHeader,
    FQuery,
    FCookie : TStringList;
    FLastAccess : TDateTime;
    function GetRequestHeader(Name: string): string;
    procedure CompleteRequestHeaderInfo;
    function GetCookie(Name: string): string;
    function GetQuery(Name: string) : string;
    function GetQueryAsDouble(Name: string): double;
    function GetQueryAsInteger(Name: string): integer;
    function GetQueryAsTDateTime(Name: string) : TDateTime;
    function GetQueryAsBoolean(Name: string): boolean;
  protected
    NewThread : boolean; // True if is the first request of a thread
    class function URLDecode(Encoded: string): string;
    class function URLEncode(Decoded: string): string;
    procedure AddParam(var S: string; Param: array of string);
    procedure ReadRequestHeader(var RequestHeader : TStringList; Stream : string; const Cookies : TStringList = nil);
    procedure ReadBeginRequest(var FCGIHeader; Content : string);
    procedure GetValues(Content : string);
    function HandleRequest(pRequest : string) : string;
    function BeforeHandleRequest : boolean; virtual;
    function SetCurrentFCGIThread : boolean;
    procedure AfterHandleRequest; virtual;
    procedure OnError(Msg, Method, Params : string); virtual;
    procedure OnNotFoundError; virtual;
    procedure AfterThreadConstruction; virtual;
    procedure BeforeThreadDestruction; virtual;
  public
    BrowserCache : boolean;// If false generates 'cache-control:no-cache' in HTTP header, default is false
    Response     : string; // Response string
    ContentType  : string; // HTTP content-type header, default is 'text/html'
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
    constructor Create(NewSocket : integer); virtual;
    destructor Destroy; override;
    procedure AddToGarbage(const Name : string; Obj: TObject);
    procedure DeleteFromGarbage(Obj : TObject);
    function FindObject(Name : string) : TObject;
    procedure SendResponse(S : string; pRecType : TRecType = rtStdOut);
    procedure Execute; override;
    procedure SendEndRequest(Status: TProtocolStatus = psRequestComplete);
    procedure SetResponseHeader(Header : string);
    procedure SetCookie(Name, Value : string; Expires : TDateTime = 0; Domain : string = ''; Path : string = ''; Secure : boolean = false);
  published
    procedure Home; virtual; abstract; // Default method to be called by <link TFCGIThread.HandleRequest, HandleRequest>
    procedure Logout; virtual;
    procedure Shutdown; virtual;
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
    WebServers : TStringList;
    FCGIThreadClass : TFCGIThreadClass;
    Port : word;
    MaxConns, FThreadsCount : integer;
    Threads : TStringList;
    MaxIdleTime : TDateTime;
    AccessThreads : TCriticalSection;
    procedure GarbageThreads;
  public
    Terminated : boolean; // Set to true to terminate the application
    GarbageNow : boolean; // Set to true to trigger the garbage colletor
    Shutdown   : boolean; // Set to true to shutdown the application after the last thread to end, default is false
    Title      : string;  // Application title used by <link TExtThread.AfterHandleRequest, AfterHandleRequest>
    Icon       : string;  // Icon to show in Browser
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
  StrUtils;

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
procedure MoveToFCGIHeader(var Buffer : char; var FCGIHeader : TFCGIHeader); begin
  move(Buffer, FCGIHeader, sizeof(TFCGIHeader));
  FCGIHeader.ID  := swap(FCGIHeader.ID);
  FCGIHeader.Len := swap(FCGIHeader.Len);
end;

{
Converts a Request string into a FastCGI Header
@param Buffer Input buffer to convert
@param FCGIHeader FastCGI header converted from Buffer
@see MoveToFCGIHeader
}
procedure MoveFromFCGIHeader(FCGIHeader : TFCGIHeader; var Buffer : char); begin
  FCGIHeader.ID  := swap(FCGIHeader.ID);
  FCGIHeader.Len := swap(FCGIHeader.Len);
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
  I: Integer;
begin
  I := FGarbageCollector.IndexOfObject(Obj);
  if I >= 0 then
    FGarbageCollector.Delete(I);
end;

// Finds a TObject in Garbage collector using its JavaScript name
function TFCGIThread.FindObject(Name : string): TObject;
var
  I: Integer;
begin
  I := FGarbageCollector.IndexOf(Name);
  if I >= 0 then
    Result := FGarbageCollector.Objects[I]
  else
    Result := nil;
end;

// Destroys the TFCGIThread invoking the Thread Garbage Collector to free the associated objects
destructor TFCGIThread.Destroy;
var
  I : integer;
begin
  with FGarbageCollector do begin
    for I := 0 to Count-1 do
      try TObject(Objects[I]).Free except end;
    Free;
  end;
  FRequestHeader.Free;
  FQuery.Free;
  FCookie.Free;
  dec(Application.FThreadsCount);
  inherited;
end;

{
Appends or cleans HTTP response header. The HTTP response header is sent using <link TFCGIThread.SendResponse, SendResponse> method.
@param Header Use '' to clean response header else Header parameter is appended to response header
}
procedure TFCGIThread.SetResponseHeader(Header : string); begin
  if Header = '' then
    FResponseHeader := ''
  else
    FResponseHeader := FResponseHeader + Header + ^M^J;
end;

// Terminates the TFCGIThread calls <link TFCGIThread.Logout, Logout> method
procedure TFCGIThread.Shutdown; begin
  if Query['password'] = 'pitinnu' then begin
    Logout;
    Application.Terminated := true
  end;
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
procedure TFCGIThread.SendResponse(S : string; pRecType : TRecType = rtStdOut);
var
  FCGIHeader : TFCGIHeader;
  Buffer : string;
begin
  if pRecType = rtStdOut then begin
    if FRequestMethod = rmHead then S := '';
    FResponseHeader := FResponseHeader + 'content-type:' + ContentType + ^M^J;
    if not BrowserCache then FResponseHeader := FResponseHeader + 'cache-control:no-cache'^M^J;
    S := FResponseHeader + ^M^J + S;
    FResponseHeader := '';
  end;
  fillchar(FCGIHeader, sizeof(FCGIHeader), 0);
  with FCGIHeader do begin
    Version := 1;
    if pRecType in [rtGetValuesResult, rtUnknown] then
      ID := 0
    else
      ID := FRequestID;
    RecType := pRecType;
    Len := length(S);
    PadLen := 7 - ((Len + 7) and 7);
    SetLength(Buffer, sizeof(FCGIHeader) + Len + PadLen);
  end;
  MoveFromFCGIHeader(FCGIHeader, Buffer[1]);
  if S <> '' then move(S[1], Buffer[sizeof(FCGIHeader) + 1], length(S));
  FSocket.SendString(Buffer);
end;

{
Sends an end request record to the Web Server and ends this thread.
@param Status Status of request. Default is psRequestComplete
}
procedure TFCGIThread.SendEndRequest(Status : TProtocolStatus = psRequestComplete); begin
  if Status <> psRequestComplete then begin
    case Status of
      psCantMPXConn : Response := 'Multiplexing is not allowed.';
      psOverloaded  : Response := 'Maximum connection limit is ' + IntToStr(Application.MaxConns) + ' and was reached.';
      psUnknownRole : Response := 'Unknown FastCGI Role received.';
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
procedure TFCGIThread.ReadBeginRequest(var FCGIHeader; Content : string);
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
procedure TFCGIThread.ReadRequestHeader(var RequestHeader : TStringList; Stream : string; const Cookies : TStringList = nil);
var
  I, Pos : integer;
  Len    : array[0..1] of integer;
  Param  : array[0..1] of string;
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
      RequestHeader.Values[Param[0]] := UTF8Decode(Param[1]);
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
  Result := UTF8Decode(Encoded);
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
  FGarbageCollector.AddObject(AnsiReplaceStr(Name, '_', ''), Obj);
end;

{
Processing to execute after <link TFCGIThread.HandleRequest, HandleRequest> method immediately before to <link TFCGIThread.SendResponse, SendResponse>
@see TExtThread.AfterHandleRequest
}
procedure TFCGIThread.AfterHandleRequest; begin end;

// Override this method to takes some action after the FastCGI thread is created
procedure TFCGIThread.AfterThreadConstruction; begin end;

// Override this method to takes some action before the FastCGI thread is destroyed
procedure TFCGIThread.BeforeThreadDestruction; begin end;

{
Processing to execute before <link TFCGIThread.HandleRequest, HandleRequest> method.
@see TExtThread.BeforeHandleRequest
@return True if the processing is ok else retuns False and the <link TFCGIThread.HandleRequest, HandleRequest> method will not call the published method indicated by PathInfo.
}
function TFCGIThread.BeforeHandleRequest : boolean; begin Result := true end;

procedure TFCGIThread.CompleteRequestHeaderInfo;
var
  ReqMet : string;
begin
  FLastAccess := Now;
  FPathInfo := FRequestHeader.Values['PATH_INFO'];
  if FPathInfo <> '' then FPathInfo := copy(FPathInfo, 2, 100);
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
  FQuery.DelimitedText := URLDecode(FRequestHeader.Values['QUERY_STRING']);
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

{
Handles "method not found" error. Occurs when PathInfo not matches a published method declared in this thread. Can be overrided in descendent thread class
@see HandleRequest
}
procedure TFCGIThread.OnNotFoundError; begin
  Response := 'alert("Method: ''' + PathInfo + ''' not found");';
end;

// Handles errors raised in the method called by PathInfo in <link TFCGIThread.HandleRequest, HandleRequest> method. Occurs when PathInfo not matches a published method declared in this thread. Can be overrided in descendent thread class
procedure TFCGIThread.OnError(Msg, Method, Params : string); begin
  Response := 'alert("' + Msg + '\non Method: ' + Method + '\nParams: ' + Params + '");'
end;

// Ends current Browser session and triggers the Garbage Collector
procedure TFCGIThread.Logout; begin
  Response := 'window.close();';
  FGarbage := true;
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
    end
  end
  else begin
    CurrentFCGIThread := TFCGIThread(Application.Threads.Objects[I]);
    CurrentFCGIThread.FRequestHeader.Assign(FRequestHeader);
    CurrentFCGIThread.FCookie.Assign(FCookie);
    CurrentFCGIThread.NewThread := false;
    Application.Threads.AddObject('0', Self);
  end;
  Application.AccessThreads.Leave;
end;

{
The thread main loop.<p>
On receive a request, each request, on its execution cycle, do:
  * <link MoveToFCGIHeader, Reads its FCGI header>
  * Depending on <link TRecType, record type> do:
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
  Buffer, Content : string;
  I : integer;
begin
  CurrentFCGIThread := Self;
  FRequest := '';
  try
    if Application.CanConnect(FSocket.GetHostAddress) then
      repeat
        if FSocket.WaitingData > 0 then begin
          Buffer := FSocket.RecvString;
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
                        if SetCurrentFCGIThread then
                          CurrentFCGIThread.CompleteRequestHeaderInfo
                        else
                          break;
                      end
                      else begin
                        Response := CurrentFCGIThread.HandleRequest(FRequest);
                        FResponseHeader := CurrentFCGIThread.FResponseHeader;
                        FGarbage := CurrentFCGIThread.FGarbage;
                        if (Response <> '') or (RequestMethod in [rmGet, rmHead]) then SendResponse(Response);
                        SendEndRequest;
                      end;
                      FRequest := '';
                    end
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
          sleep(200);
      until Terminated
    else
      Terminate;
  except
    on E : Exception do begin
      SendResponse(E.Message);
      SendResponse(E.Message, rtStdErr);
      SendEndRequest;
    end;
  end;
  FSocket.Free;
  if (not NewThread) or FGarbage then begin
    if FGarbage then
      CurrentFCGIThread.FLastAccess := 0
    else
      FLastAccess := 0;
    Application.GarbageNow := true;
  end;
  {$IFNDEF MSWINDOWS}EndThread(0){$ENDIF} // Unix RTL FPC bug
end;

{
Calls the published method indicated by PathInfo. Before calls <link TFCGIThread.BeforeHandleRequest, BeforeHandleRequest> method and after calls <link TFCGIThread.AfterHandleRequest, AfterHandleRequest> method.
If PathInfo is null then <link TFCGIThread.Home, Home> method will be called.
The published method will use the FRequest as input and the Response as output.
<link TFCGIThread.OnError, OnError> method is called if an exception is raised in published method.
<link TFCGIThread.OnNotFoundError, OnNotFoundError> method is called if the published method is not declared in this thread.
@param pRequest Request body assigned to FRequest field or to <link TFCGIThread.Query, Query> array if FRequestMethod is <link TRequestMethod, rmPost>, it is the input to the published method
@return Response body to <link TFCGIThread.SendResponse, send>
}
function TFCGIThread.HandleRequest(pRequest : string) : string;
//{DOM-IGNORE-BEGIN
type
  MethodCall = procedure of object;
//DOM-IGNORE-END}
var
  PageMethod : TMethod;
  MethodCode : pointer;
begin
  if (FRequestMethod = rmPost) and (pos('=', pRequest) <> 0) then
    FQuery.DelimitedText := URLDecode(pRequest)
  else
    FRequest := pRequest;
  Response := '';
  if BeforeHandleRequest then
    try
      if PathInfo = '' then
        Home
      else begin
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
  Result := UTF8Encode(Response);
end;

// Frees a TFCGIApplication
destructor TFCGIApplication.Destroy; begin
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
@param pShutdownAfterLastThreadDown If true Shutdown the application after the last thread to end, default is false
@param pMaxConns Maximum accepted connections
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
  if ParamCount = 1 then
    WServers := ParamStr(1)
  else
    WServers := GetEnvironmentVariable('FCGI_WEB_SERVER_ADDRS');
  if WServers <> '' then begin
    WebServers := TStringList.Create;
    WebServers.DelimitedText := WServers;
  end;
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
  writeln('Port: ', Port, ' already in use.'^M^J'Press ENTER.');
  readln;
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
  I := 0;
  FThreadsCount := -1;
  with TBlockSocket.Create do begin
    Bind(Port, 100);
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

end.
