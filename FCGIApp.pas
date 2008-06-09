unit FCGIApp;
// Author: Wanderlan Santos dos Anjos (wanderlan.anjos@gmail.com) at 04/2008
// License: LGPLv3
// This FCGIApp unit implements, in my opinion, the best behaviour for Web applications: statefull, multi-threaded, blocking and non-multiplexed connection.
// Multiplexing don't works with Apache anyway. Indeed Apache don't supports Filter role.

interface

uses
  {$IFNDEF MSWINDOWS}cthreads,{$ENDIF}
  BlockSocket, SysUtils, SyncObjs, Classes, ExtPascalUtils;

type
  TRecType = (rtBeginRequest = 1, rtAbortRequest, rtEndRequest, rtParams, rtStdIn, rtStdOut, rtStdErr, rtData, rtGetValues, rtGetValuesResult, rtUnknown);
  TRole = (rResponder = 1, rAuthorizer, rFilter);
  TProtocolStatus = (psRequestComplete, psCantMPXConn, psOverloaded, psUnknownRole);
  TRequestMethod = (rmGet, rmPost, rmHead, rmPut, rmDelete);
  {$M+}
  TFCGIThread = class(TThread)
  private
    FRequestID : word;
    FRole : TRole;
    FRequest, FPathInfo : string;
    FRequestMethod : TRequestMethod;
    function GetRequestHeader(Name: string): string;
    function GetQuery(Name: string): string;
    procedure CompleteRequestHeaderInfo;
    function GetCookie(Name: string): string;
    function SetCurrentFCGIThread : boolean;
  protected
    FSocket : TBlockSocket;
    FKeepConn, NewThread : boolean;
    FResponseHeader : string;
    FRequestHeader, FQuery, FCookie : TStringList;
    FLastAccess : TDateTime;
    class function URLDecode(Encoded: string): string;
    class function URLEncode(Decoded: string): string;
    procedure AddParam(var S: string; Param: array of string);
    procedure ReadRequestHeader(var RequestHeader : TStringList; Stream : string; const Cookies : TStringList = nil);
    procedure ReadBeginRequest(var FCGIHeader; Content : string);
    procedure GetValues(Content : string);
    function HandleRequest(pRequest : string) : string;
    procedure NotFoundError; virtual;
    function BeforeHandleRequest : boolean; virtual;
    procedure AfterHandleRequest; virtual;
    procedure OnError(Msg, Method, Params : string); virtual;
  public
    BrowserCache : boolean;
    Response, ContentType : string;
    property Role : TRole read FRole;
    property Request : string read FRequest;
    property PathInfo : string read FPathInfo;
    property LastAccess : TDateTime read FLastAccess;
    property RequestMethod : TRequestMethod read FRequestMethod;
    property RequestHeader[Name : string] : string read GetRequestHeader;
    property Query[Name : string] : string read GetQuery;
    property Cookie[Name : string] : string read GetCookie;
    constructor Create(NewSocket : integer); virtual;
    destructor Destroy; override;
    procedure SendResponse(S : string; pRecType : TRecType = rtStdOut);
    procedure Execute; override;
    procedure SendEndRequest(Status: TProtocolStatus = psRequestComplete);
    procedure SetResponseHeader(Header : string);
    procedure SetCookie(Name, Value : string; Expires : TDateTime = 0; Domain : string = ''; Path : string = ''; Secure : boolean = false);
  published
    procedure Home; virtual; abstract;
    procedure Logout;
    procedure Shutdown;
  end;
  {$M-}
  TFCGIThreadClass = class of TFCGIThread;

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
    Terminated, GarbageNow, Shutdown : boolean;
    Title : string;
    constructor Create(pTitle : string; pFCGIThreadClass : TFCGIThreadClass; pPort : word = 2014; pMaxIdleMinutes : word = 30;
      pShutdownAfterLastThreadDown : boolean = false; pMaxConns : integer = 1000);
    destructor Destroy; override;
    procedure Run;
    function CanConnection(Address : string) : boolean;
    function GetThread(I : integer) : TFCGIThread;
    function ThreadsCount : integer;
    function ReachedMaxConns : boolean;
  end;

var
  Application : TFCGIApplication;

threadvar
  CurrentFCGIThread : TFCGIThread;

implementation

uses
  StrUtils;

type
  TFCGIHeader = packed record
    Version : byte; // 1
    RecType : TRecType;
    ID, Len : word;
    PadLen  : byte;
    Filler  : byte;
  end;

  TBeginRequest = packed record
    Header  : TFCGIHeader;
    Filler  : byte;
    Role    : TRole;
    KeepConn: boolean; // Keep connection
    Filler2 : array[1..5] of byte;
  end;

procedure MoveToFCGIHeader(var Buffer : char; var FCGIHeader : TFCGIHeader); begin
  move(Buffer, FCGIHeader, sizeof(TFCGIHeader));
  FCGIHeader.ID  := swap(FCGIHeader.ID);
  FCGIHeader.Len := swap(FCGIHeader.Len);
end;

procedure MoveFromFCGIHeader(FCGIHeader : TFCGIHeader; var Buffer : char); begin
  FCGIHeader.ID  := swap(FCGIHeader.ID);
  FCGIHeader.Len := swap(FCGIHeader.Len);
  move(FCGIHeader, Buffer, sizeof(TFCGIHeader));
end;

constructor TFCGIThread.Create(NewSocket : integer); begin
  if Application.FThreadsCount < 0 then Application.FThreadsCount := 0;
  inc(Application.FThreadsCount);
  FSocket := TBlockSocket.Create(NewSocket);
  FRequestHeader := TStringList.Create;
  FRequestHeader.StrictDelimiter := true;
  FQuery := TStringList.Create;
  FQuery.StrictDelimiter := true;
  FQuery.Delimiter := '&';
  FCookie := TStringList.Create;
  FCookie.StrictDelimiter := true;
  FCookie.Delimiter := ';';
  ContentType  := 'text/html';
  FreeOnTerminate := true;
  inherited Create(false);
end;

destructor TFCGIThread.Destroy; begin
  dec(Application.FThreadsCount);
  FRequestHeader.Free;
  FQuery.Free;
  FCookie.Free;
  inherited;
end;

procedure TFCGIThread.SetResponseHeader(Header : string); begin
  if Header = '' then
    FResponseHeader := ''
  else
    FResponseHeader := FResponseHeader + Header + ^M^J;
end;

procedure TFCGIThread.Shutdown; begin
  Logout;
  Application.Terminated := true
end;

procedure TFCGIThread.SetCookie(Name, Value: string; Expires: TDateTime; Domain, Path: string; Secure: boolean); begin
  SetResponseHeader('Set-Cookie: ' + Name + '=' + Value + ';' +
    IfThen(Expires <> 0, ' expires=' + FormatDateTime('ddd, dd-mmm-yyyy hh:nn:ss', Expires) + ' GMT;', '') +
    IfThen(Domain <> '', ' domain=' + Domain + ';', '') + IfThen(Path <> '', ' path=' + Path + ';', '') + IfThen(Secure, ' secure', ''))
end;

procedure TFCGIThread.SendResponse(S : string; pRecType : TRecType = rtStdOut);
var
  FCGIHeader : TFCGIHeader;
  Buffer : string;
begin
  if pRecType = rtStdOut then begin
    if FRequestMethod = rmHead then S := '';
    FResponseHeader := FResponseHeader + 'content-type: ' + ContentType + ^M^J;
    if not BrowserCache then FResponseHeader := FResponseHeader + 'Cache-Control: no-cache'^M^J;
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
      RequestHeader.Values[Param[0]] := Param[1];
  end;
end;

class function TFCGIThread.URLDecode(Encoded : string) : string;
var
  I : integer;
begin
  Result := Encoded;
  I := pos('%', Result);
  while I <> 0 do begin
    Result[I] := chr(StrToIntDef('$' + copy(Result, I+1, 2), 32));
    Delete(Result, I+1, 2);
    I := pos('%', Result);
  end;
end;

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

procedure TFCGIThread.AfterHandleRequest; begin end;

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

function TFCGIThread.GetQuery(Name: string): string;  begin
  Result := FQuery.Values[Name]
end;

function TFCGIThread.GetCookie(Name: string): string; begin
  Result := FCookie.Values[Name]
end;

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

procedure TFCGIThread.NotFoundError; begin
  SetResponseHeader('Status: 405 Method not found');
  Response := 'Method not found'
end;

procedure TFCGIThread.OnError(Msg, Method, Params : string); begin
  Response := Msg
end;

function TFCGIThread.HandleRequest(pRequest : string) : string;
type
  MethodCall = procedure of object;
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
    if PathInfo = '' then
      Home
    else begin
      MethodCode := MethodAddress(PathInfo);
      if MethodCode <> nil then begin
        PageMethod.Code := MethodCode;
        PageMethod.Data := Self;
        try
          MethodCall(PageMethod); // Call published method
        except
          on E : Exception do OnError(E.Message, PathInfo, pRequest)
        end;
      end
      else
        NotFoundError;
    end;
  AfterHandleRequest;
  Result := Response;
end;

procedure TFCGIThread.Logout; begin
  SendEndRequest;
//  sleep(0);
  FLastAccess := 0;
  Application.GarbageNow := true;
end;

function TFCGIThread.SetCurrentFCGIThread : boolean;
var
  Thread : string;
  GUID : TGUID;
  I : integer;
begin
  Result := true;
  Thread := Cookie['FCGIThread'];
  Application.AccessThreads.Enter;
  I := Application.Threads.IndexOf(Thread);
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
      FreeOnTerminate := false;
    end
  end
  else begin
    CurrentFCGIThread := TFCGIThread(Application.Threads.Objects[I]);
    CurrentFCGIThread.FRequestHeader.Assign(FRequestHeader);
    CurrentFCGIThread.FCookie.Assign(FCookie);
    CurrentFCGIThread.NewThread := false;
  end;
  Application.AccessThreads.Leave;
end;

procedure TFCGIThread.Execute;
var
  FCGIHeader : TFCGIHeader;
  Buffer, Content : string;
  I : integer;
begin
  CurrentFCGIThread := Self;
  FRequest := '';
  try
    if Application.CanConnection(FSocket.GetHostAddress) then
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
end;

{ TFCGIApplication }

function TFCGIApplication.CanConnection(Address: string): boolean; begin
  Result := (WebServers = nil) or (WebServers.IndexOf(Address) <> -1)
end;

function TFCGIApplication.ReachedMaxConns : boolean; begin
  Result := Threads.Count >= MaxConns
end;

constructor TFCGIApplication.Create(pTitle : string; pFCGIThreadClass : TFCGIThreadClass; pPort : word = 2014;
  pMaxIdleMinutes : word = 30; pShutdownAfterLastThreadDown : boolean = false; pMaxConns : integer = 1000);
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

destructor TFCGIApplication.Destroy; begin
  Threads.Free;
  AccessThreads.Free;
  WebServers.Free;
  inherited;
end;

procedure TFCGIApplication.GarbageThreads;
var
  I : integer;
  Thread : TFCGIThread;
begin
  for I := Threads.Count-1 downto 0 do begin
    Thread := TFCGIThread(Threads.Objects[I]);
    if (Now - Thread.LastAccess) > MaxIdleTime then begin
      try Thread.Free except end;
      AccessThreads.Enter;
      Threads.Delete(I);
      AccessThreads.Leave;
    end;
  end;
end;

function TFCGIApplication.GetThread(I: integer): TFCGIThread; begin
  Result := TFCGIThread(Threads.Objects[I])
end;

procedure TFCGIApplication.Run;
var
  NewSocket, I : integer;
begin
  I := 0;
  FThreadsCount := -1;
  with TBlockSocket.Create do begin
    Bind(Port, 100);
    repeat
      NewSocket := Accept(250);
      if NewSocket <> 0 then FCGIThreadClass.Create(NewSocket);
      if ((I mod 120) = 0) or GarbageNow then begin // A garbage for each 30 seconds
        GarbageThreads;
        GarbageNow := false;
        I := 0;
      end;
      inc(I);
    until Terminated or (Shutdown and (ThreadsCount = 0));
    Free;
  end;
end;

function TFCGIApplication.ThreadsCount: integer; begin
  Result := FThreadsCount
end;

end.
