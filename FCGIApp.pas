unit FCGIApp;
// Author: Wanderlan Santos dos Anjos (wanderlan.anjos@gmail.com) at 04/2008
// License: LGPLv3
// This FCGIApp unit implements, in my opinion, the best behaviour for Web applications: statefull, multi-threaded, blocking and non-multiplexed connection.
// Multiplexing don't works with Apache anyway. Indeed Apache don't supports Filter role.

interface

uses
  Classes, BlockSocket;

const
  MaxConns = '1000';

type
  TRecType = (rtBeginRequest = 1, rtAbortRequest, rtEndRequest, rtParams, rtStdIn, rtStdOut, rtStdErr, rtData, rtGetValues, rtGetValuesResult, rtUnknown);
  TRole = (rResponder = 1, rAuthorizer, rFilter);
  TProtocolStatus = (psRequestComplete, psCantMPXConn, psOverloaded, psUnknownRole);
  TRequestMethod = (rmGet, rmPost, rmHead, rmPut);
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
    function URLDecode(Encoded: string): string;
  protected
    FSocket : TBlockSocket;
    FKeepConn : boolean;
    FResponse, FResponseHeader : string;
    FRequestHeader, FQuery : TStringList;
    procedure AddParam(var S: string; Param: array of string);
    procedure ReadParamList(var ParamList : TStringList; Stream : string);
    procedure ReadBeginRequest(var Header; Content : string);
    procedure GetValues(Content : string);
    procedure HandleRequest;
    procedure DoIdle; virtual;
    function NotFoundError : string; virtual;
  public
    BrowserCache : boolean;
    ContentType : string;
    property RequestID : word read FRequestID;
    property Role : TRole read FRole;
    property Request : string read FRequest;
    property Response : string read FResponse;
    property PathInfo : string read FPathInfo;
    property RequestMethod : TRequestMethod read FRequestMethod;
    property RequestHeader[Name : string] : string read GetRequestHeader;
    property Query[Name : string] : string read GetQuery;
    constructor Create(NewSocket : integer);
    destructor Destroy; override;
    procedure SendRecord(S : string; pRecType : TRecType = rtStdOut);
    procedure Execute; override;
    procedure SendEndRequest(Status: TProtocolStatus = psRequestComplete; DoTerminate : boolean = false);
    procedure SetResponseHeader(Header : string);
  published
    procedure Home; virtual; abstract;
  end;
  {$M-}
  TFCGIThreadClass = class of TFCGIThread;

  TFCGIApplication = class
  private
    WebServers : TStringList;
    FCGIThreadClass : TFCGIThreadClass;
    Port : word;
  public
    Terminated : boolean;
    constructor Create(pFCGIThreadClass : TFCGIThreadClass; pPort : word = 2014);
    destructor Destroy; override;
    procedure Run;
    function CanConnection(Address : string) : boolean;
  end;

var
  Application : TFCGIApplication;
  FCGIThread  : TFCGIThread;

implementation

uses
  SysUtils;

type
  THeader = packed record
    Version : byte; // 1
    RecType : TRecType;
    ID, Len : word;
    PadLen  : byte;
    Filler  : byte;
  end;

  TBeginRequest = packed record
    Header  : THeader;
    Filler  : byte;
    Role    : TRole;
    KeepConn: boolean; // Keep connection
    Filler2 : array[1..5] of byte;
  end;

procedure MoveToHeader(var Buffer : char; var Header : THeader); begin
  move(Buffer, Header, sizeof(THeader));
  Header.ID  := swap(Header.ID);
  Header.Len := swap(Header.Len);
end;

procedure MoveFromHeader(Header : THeader; var Buffer : char); begin
  Header.ID  := swap(Header.ID);
  Header.Len := swap(Header.Len);
  move(Header, Buffer, sizeof(THeader));
end;

constructor TFCGIThread.Create(NewSocket : integer); begin
  FSocket := TBlockSocket.Create(NewSocket);
  FRequestHeader := TStringList.Create;
  FQuery := TStringList.Create;
  FQuery.Delimiter := '&';
  BrowserCache := true;
  ContentType  := 'text/html';
  FreeOnTerminate := true;
  inherited Create(false);
end;

destructor TFCGIThread.Destroy; begin
  FRequestHeader.Free;
  FQuery.Free;
  FSocket.Free;
  inherited;
end;

procedure TFCGIThread.DoIdle; begin end;

procedure TFCGIThread.SetResponseHeader(Header : string); begin
  if Header = '' then
    FResponseHeader := ''
  else
    FResponseHeader := FResponseHeader + Header + ^M^J;
end;

procedure TFCGIThread.SendRecord(S : string; pRecType : TRecType = rtStdOut);
var
  Header : THeader;
  Buffer : string;
begin
  if pRecType = rtStdOut then begin
    if FRequestMethod = rmHead then S := '';
    FResponseHeader := FResponseHeader + 'content-type: ' + ContentType + ^M^J;
    if not BrowserCache then FResponseHeader := FResponseHeader + 'Cache-Control: no-cache'^M^J;
    S := FResponseHeader + ^M^J + S;
  end;
  fillchar(Header, sizeof(Header), 0);
  with Header do begin
    Version := 1;
    ID := RequestID;
    RecType := pRecType;
    Len := length(S);
    PadLen := 7 - ((Len + 7) and 7);
    SetLength(Buffer, sizeof(Header) + Len + PadLen);
  end;
  MoveFromHeader(Header, Buffer[1]);
  if S <> '' then move(S[1], Buffer[sizeof(Header) + 1], length(S));
  FSocket.SendString(Buffer);
end;

procedure TFCGIThread.SendEndRequest(Status : TProtocolStatus = psRequestComplete; DoTerminate : boolean = false); begin
  SendRecord(#0#0#0 + char(Status) + char(Status) + #0#0#0, rtEndRequest);
  if (Status <> psRequestComplete) or DoTerminate then Terminate;
end;

procedure TFCGIThread.ReadBeginRequest(var Header; Content : string);
var
  BeginRequest : TBeginRequest;
begin
  BeginRequest.Header := THeader(Header);
  move(Content[1], BeginRequest.Filler, sizeof(BeginRequest)-sizeof(THeader));
  if BeginRequest.Role in [rResponder..rFilter] then begin
    FRequestID := BeginRequest.Header.ID;
    FRole      := BeginRequest.Role;
    FKeepConn  := BeginRequest.KeepConn; // can't close socket if true
  end
  else
    SendEndRequest(psUnknownRole);
end;

procedure TFCGIThread.ReadParamList(var ParamList : TStringList; Stream : string);
var
  I, Pos : integer;
  Len    : array[0..1] of integer;
  Param  : array[0..1] of string;
begin
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
    ParamList.Values[Param[0]] := Param[1];
  end;
end;

function TFCGIThread.URLDecode(Encoded : string) : string;
var
  I : integer;
begin
  Result := Encoded;
  I := pos('%', Result);
  while I <> 0 do begin
    Result[I] := chr(StrToIntDef(copy(Result, I+1, 2), 32));
    Delete(Result, I+1, 2);
    I := pos('%', Result);
  end;
end;

procedure TFCGIThread.CompleteRequestHeaderInfo;
var
  ReqMet : string;
begin
  FPathInfo := FRequestHeader.Values['PATH_INFO'];
  if FPathInfo <> '' then FPathInfo := copy(FPathInfo, 2, 100);
  ReqMet := FRequestHeader.Values['REQUEST_METHOD'];
  case ReqMet[1] of
    'G' : FRequestMethod := rmGet;
    'P' :
      if ReqMet = 'POST' then
        FRequestMethod := rmPost
      else
        FRequestMethod := rmPut
  else
    FRequestMethod := rmHead;
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

procedure TFCGIThread.GetValues(Content : string);
var
  Values : TStringList;
  GetValuesResult : string;
begin
  if Content = '' then exit;
  Values := TStringList.Create;
  ReadParamList(Values, Content);
  GetValuesResult := '';
  if Values.IndexOf('FCGI_MAX_CONNS')  <> -1 then AddParam(GetValuesResult, ['FCGI_MAX_CONNS', MaxConns]);
  if Values.IndexOf('FCGI_MAX_REQS')   <> -1 then AddParam(GetValuesResult, ['FCGI_MAX_REQS',  MaxConns]);
  if Values.IndexOf('FCGI_MPXS_CONNS') <> -1 then AddParam(GetValuesResult, ['FCGI_MPXS_CONNS', '0']);
  Values.Free;
  SendRecord(GetValuesResult, rtGetValuesResult);
end;

function TFCGIThread.NotFoundError : string; begin
  Result := '404 - Method not found'
end;

procedure TFCGIThread.HandleRequest;
type
  MethodCall = procedure of object;
var
  PageMethod : TMethod;
  MethodCode : pointer;
begin
  if PathInfo = '' then
    Home
  else begin
    MethodCode := MethodAddress(PathInfo);
    if MethodCode <> nil then begin
      PageMethod.Code := MethodCode;
      PageMethod.Data := Self;
      if (FRequestMethod = rmPost) and (pos('=', FResponse) <> 0) then FQuery.DelimitedText := URLDecode(FRequest);
      MethodCall(PageMethod); // Call published method
    end
    else
      FResponse := NotFoundError;
  end;
  if FResponse <> '' then begin
    SendRecord(FResponse);
    FResponse := '';
  end;
  SendEndRequest;
end;

procedure TFCGIThread.Execute;
var
  Header : THeader;
  Buffer, Content : string;
  I, SleepTime : integer;
begin
  FCGIThread := Self;
  FRequest := ''; SleepTime := 1;
  try
    if Application.CanConnection(FSocket.GetHostAddress) then
      repeat
        if FSocket.WaitingData > 0 then begin
          Buffer := FSocket.RecvString;
          if FSocket.SocketError <> 0 then begin
            Terminate;
            exit;
          end;
          I := 1; SleepTime := 1;
          while I <= length(Buffer) do begin
            MoveToHeader(Buffer[I], Header);
            if (RequestID <> 0) and (Header.ID <> 0) and (Header.ID <> RequestID) then
              SendEndRequest(psCantMPXConn)
            else begin
              inc(I, sizeof(Header));
              Content := copy(Buffer, I, Header.Len);
              case Header.RecType of
                rtBeginRequest : ReadBeginRequest(Header, Content);
                rtAbortRequest : SendEndRequest(psRequestComplete, true);
                rtGetValues    : GetValues(Content);
                rtParams, rtStdIn, rtData :
                  if Content = '' then begin
                    if Header.RecType = rtParams then begin
                      ReadParamList(FRequestHeader, FRequest);
                      CompleteRequestHeaderInfo;
                    end
                    else
                      HandleRequest;
                    FRequest := '';
                  end
                  else
                    FRequest := FRequest + Content;
              else
                SendRecord(char(Header.RecType), rtUnknown);
                Buffer := '';
                sleep(20);
                break;
              end;
            end;
            inc(I, Header.Len);
          end;
        end
        else begin
          sleep(SleepTime);
          if SleepTime < 256 then inc(SleepTime);
          DoIdle;
        end;
      until Terminated
    else
      Terminate;
  except
    on E : Exception do begin
      SendRecord(E.Message);
      SendRecord(E.Message, rtStdErr);
      SendEndRequest;
    end;
  end;
end;

{ TFCGIApplication }

function TFCGIApplication.CanConnection(Address: string): boolean; begin
  Result := (WebServers = nil) or (WebServers.IndexOf(Address) <> -1)
end;

constructor TFCGIApplication.Create(pFCGIThreadClass : TFCGIThreadClass; pPort : word = 2014);
var
  WS : string;
begin
  FCGIThreadClass := pFCGIThreadClass;
  Port := pPort;
  WS := GetEnvironmentVariable('FCGI_WEB_SERVER_ADDRS');
  if WS <> '' then begin
    WebServers := TStringList.Create;
    WebServers.DelimitedText := WS;
  end;
end;

destructor TFCGIApplication.Destroy; begin
  WebServers.Free;
  inherited;
end;

procedure TFCGIApplication.Run;
var
  NewThread : integer;
begin
  with TBlockSocket.Create do begin
    Bind(Port, 100);
    repeat
      NewThread := Accept(256);
      if NewThread <> 0 then FCGIThreadClass.Create(NewThread);
    until Terminated;
  end;
end;

end.
