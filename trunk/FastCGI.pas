unit FastCGI; // This FastCGI unit implements: application peer, multi-threaded, blocking and non-multiplexed behaviour only

interface

uses
  Classes, BlockSocket;

const
  MaxConns = '1000';

type
  TRecType = (rtBeginRequest = 1, rtAbortRequest, rtEndRequest, rtParams, rtStdIn, rtStdOut, rtStrErr, rtData, rtGetValues, rtGetValuesResult, rtUnknown);
  TRole = (rResponder = 1, rAuthorizer, rFilter);
  TProtocolStatus = (psRequestComplete, psCantMPXConn, psOverloaded, psUnknownRole);

  TFastCGIThread = class(TThread)
  protected
    FRequestID : integer;
    FRole : TRole;
    FSocket : TBlockSocket;
    FKeepConn : boolean;
    procedure AddParam(var S: string; Param: array of string);
    procedure ReadParams(Variables : TStringList; Params: string);
    procedure ReadBeginRequest(Buffer: string);
    procedure GetValues(Params: string);
  public
    Variables : TStringList;
    property RequestID : integer read FRequestID;
    property Role : TRole read FRole;
    constructor Create(NewSocket : integer);
    destructor Destroy; override;
    procedure SendRecord(S : string; pRecType : TRecType = rtStdOut);
    procedure Execute; override;
    procedure SendEndRequest(Status: TProtocolStatus = psRequestComplete);
  end;

  TFastCGIApplication = class
  private
    WebServers : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
    function CanConnection(Address : string) : boolean;
  end;

var
  FastCGIApplication : TFastCGIApplication;

implementation

uses
  SysUtils;

type
  THeader = packed record
    Version : byte; // 1
    RecType : TRecType;
    ID, Len : integer;
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

constructor TFastCGIThread.Create(NewSocket : integer); begin
  FSocket := TBlockSocket.Create(NewSocket);
  Variables := TStringList.Create;
  FreeOnTerminate := true;
  inherited Create(false);
end;

destructor TFastCGIThread.Destroy; begin
  Variables.Free;
  FSocket.Free;
  inherited;
end;

procedure TFastCGIThread.SendRecord(S : string; pRecType : TRecType = rtStdOut);
var
  Header : THeader;
  Buffer : string;
begin
  fillchar(Header, sizeof(Header), 0);
  with Header do begin
    Version := 1;
    ID := RequestID;
    RecType := pRecType;
    Len := length(S);
    PadLen := 7 - ((Len + 7) and 7);
    SetLength(Buffer, sizeof(Header) + Len + PadLen);
  end;
  move(Header, Buffer[1], sizeof(Header));
  move(S[1], Buffer[sizeof(Header) + 1], length(S));
  FSocket.SendString(Buffer);
end;

procedure TFastCGIThread.SendEndRequest(Status : TProtocolStatus = psRequestComplete); begin
  SendRecord(#0#0#0 + char(Status) + char(Status), rtEndRequest);
  Terminate;
end;

procedure TFastCGIThread.ReadBeginRequest(Buffer : string);
var
  BeginRequest : TBeginRequest;
begin
  move(Buffer[1], BeginRequest, sizeof(BeginRequest));
  if BeginRequest.Role in [rResponder..rFilter] then begin
    FRequestID := BeginRequest.Header.ID;
    FRole      := BeginRequest.Role;
    FKeepConn  := BeginRequest.KeepConn; // can't close socket if true
  end
  else
    SendEndRequest(psUnknownRole);
end;

procedure TFastCGIThread.ReadParams(Variables : TStringList; Params : string);
var
  I, Pos : integer;
  Len    : array[0..1] of Int64;
  Param  : array[0..1] of string;
begin
  Pos := 1;
  while Pos < length(Params) do begin
    for I := 0 to 1 do begin
      Len[I] := byte(Params[Pos]);
      if Len[I] > 127 then begin
        move(Params[Pos], Len[I], 4);
        inc(Pos, 4);
      end
      else
        inc(Pos);
      SetLength(Param[I], Len[I]);
    end;
    move(Params[Pos], Param[0], Len[0]);
    inc(Pos, Len[0]);
    move(Params[Pos], Param[1], Len[1]);
    inc(Pos, Len[1]);
    if Variables.IndexOf(Param[0]) = -1 then Variables.Add(Param[0]);
    Variables.Values[Param[0]] := Param[1];
  end;
end;

procedure TFastCGIThread.AddParam(var S : string; Param : array of string);
var
  I, J   : integer;
  Len    : array[0..1] of Int64;
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
    else
      move(Len[I], S[J], Format[I]);
    inc(J, Format[I]);
  end;
  move(Param[0][1], S[J], Len[0]);
  move(Param[1][1], S[J + Len[0]], Len[1]);
end;

procedure TFastCGIThread.GetValues(Params : string);
var
  I : integer;
  Variables : TStringList;
  GetValuesResult : string;
begin
  if Params = '' then exit;
  Variables := TStringList.Create;
  ReadParams(Variables, Params);
  GetValuesResult := '';
  for I := 0 to Variables.Count-1 do begin
    if Variables.Names[I] = 'FCGI_MAX_CONNS'  then AddParam(GetValuesResult, ['FCGI_MAX_CONNS', MaxConns]);
    if Variables.Names[I] = 'FCGI_MAX_REQS'   then AddParam(GetValuesResult, ['FCGI_MAX_REQS',  MaxConns]);
    if Variables.Names[I] = 'FCGI_MPXS_CONNS' then AddParam(GetValuesResult, ['FCGI_MPXS_CONNS', '0']);
  end;
  Variables.Free;
  SendRecord(GetValuesResult, rtGetValuesResult);
end;

procedure TFastCGIThread.Execute;
var
  Header : THeader;
  Buffer : string;
begin
  try
    if FastCGIApplication.CanConnection(FSocket.GetHostAddress) then
      while not Terminated and FSocket.CanRead(200) do begin
        Buffer := FSocket.RecvString;
        if FSocket.SocketError = 0 then begin
          move(Buffer[1], Header, sizeof(Header));
          if (RequestID <> 0) and (Header.ID <> 0) and (Header.ID <> RequestID) then
            SendEndRequest(psCantMPXConn)
          else
            case Header.RecType of
              rtBeginRequest : ReadBeginRequest(Buffer);
              rtAbortRequest : SendEndRequest;
              rtParams       : ReadParams(Variables, copy(Buffer, sizeof(Header) + 1, Header.Len));
              rtStdIn,
              rtData         : ;//Result := copy(Buffer, sizeof(Header) + 1, Header.Len);
              rtGetValues    : GetValues(copy(Buffer, sizeof(Header) + 1, Header.Len));
            else
              SendRecord(char(Header.RecType), rtUnknown);
            end;
        end
        else
          FSocket.Purge;
      end
    else
      Terminate;
  except
    on E : Exception do begin
      SendRecord(E.Message);
      SendRecord(E.Message, rtStrErr);
    end;
  end;
end;

{ TFastCGIApplication }

function TFastCGIApplication.CanConnection(Address: string): boolean; begin
  Result := (WebServers = nil) or (WebServers.IndexOf(Address) <> -1)
end;

constructor TFastCGIApplication.Create;
var
  WS : string;
begin
  WS := GetEnvironmentVariable('FCGI_WEB_SERVER_ADDRS');
  if WS <> '' then begin
    WebServers := TStringList.Create;
    WebServers.DelimitedText := WS;
  end;
end;

destructor TFastCGIApplication.Destroy; begin
  WebServers.Free;
  inherited;
end;

procedure TFastCGIApplication.Run;
var
  NewThread : integer;
begin
  try
    with TBlockSocket.Create do
      try
        Bind(0, 10);
        repeat
          NewThread := Accept(200);
          if NewThread <> 0 then TFastCGIThread.Create(NewThread);
        until Terminated;
      finally
        FreeThreads;
        Free;
      end;
  finally
    fThreadListLock.Free;
    FreeAndNil(fThreadList);
  end;
end;

end.
