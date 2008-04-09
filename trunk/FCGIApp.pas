unit FCGIApp;
// This FCGIApp unit implements, in my opinion, the best behaviour for Web applications: multi-threaded, blocking and non-multiplexed behaviour.
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

  TFCGIThread = class(TThread)
  protected
    FRequestID : word;
    FRole : TRole;
    FSocket : TBlockSocket;
    FKeepConn : boolean;
    procedure AddParam(var S: string; Param: array of string);
    procedure ReadParamList(var ParamList : TStringList; Stream : string);
    procedure ReadBeginRequest(var Header; Content : string);
    procedure GetValues(Content : string);
  public
    Params : TStringList;
    property RequestID : word read FRequestID;
    property Role : TRole read FRole;
    constructor Create(NewSocket : integer);
    destructor Destroy; override;
    procedure SendRecord(S : string; pRecType : TRecType = rtStdOut);
    procedure Execute; override;
    procedure ProcessMessage(Message : string); virtual; abstract;
    procedure DoIdle; virtual;
    procedure SendEndRequest(Status: TProtocolStatus = psRequestComplete);
  end;
  TFCGIThreadClass = class of TFCGIThread;

  TFCGIApplication = class
  private
    WebServers : TStringList;
    FCGIThreadClass : TFCGIThreadClass;
    Response : string;
  public
    Terminated : boolean;
    constructor Create(pFCGIThreadClass : TFCGIThreadClass; pResponse : string);
    destructor Destroy; override;
    procedure Run;
    function CanConnection(Address : string) : boolean;
  end;

var
  Application : TFCGIApplication;

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

procedure MoveToHeader(var Buffer; var Header : THeader); begin
  move(Buffer, Header, sizeof(THeader));
  Header.ID  := swap(Header.ID);
  Header.Len := swap(Header.Len);
end;

procedure MoveFromHeader(Header : THeader; var Buffer); begin
  Header.ID  := swap(Header.ID);
  Header.Len := swap(Header.Len);
  move(Header, Buffer, sizeof(THeader));
end;

constructor TFCGIThread.Create(NewSocket : integer); begin
  FSocket := TBlockSocket.Create(NewSocket);
  Params  := TStringList.Create;
  FreeOnTerminate := true;
  inherited Create(false);
end;

destructor TFCGIThread.Destroy; begin
  Params.Free;
  FSocket.Free;
  inherited;
end;

procedure TFCGIThread.DoIdle; begin end;

procedure TFCGIThread.SendRecord(S : string; pRecType : TRecType = rtStdOut);
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
  MoveFromHeader(Header, Buffer[1]);
  move(S[1], Buffer[sizeof(Header) + 1], length(S));
  FSocket.SendString(Buffer);
  if (S <> '') and (pRecType in [rtStdOut, rtStdErr]) then SendRecord('', pRecType);
end;

procedure TFCGIThread.SendEndRequest(Status : TProtocolStatus = psRequestComplete); begin
  SendRecord(#0#0#0 + char(Status) + char(Status), rtEndRequest);
  Terminate;
end;

procedure TFCGIThread.ReadBeginRequest(var Header; Content : string);
var
  BeginRequest : TBeginRequest;
begin
  BeginRequest.Header := THeader(Header);
  move(Content[1], BeginRequest.Filler, sizeof(BeginRequest)-sizeof(Header));
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
    move(Stream[Pos], Param[0][1], Len[0]);
    inc(Pos, Len[0]);
    move(Stream[Pos], Param[1][1], Len[1]);
    inc(Pos, Len[1]);
    if ParamList.IndexOf(Param[0]) = -1 then ParamList.Add(Param[0]);
    ParamList.Values[Param[0]] := Param[1];
  end;
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

procedure TFCGIThread.Execute;
var
  Header : THeader;
  Buffer, Stream, Content : string;
  I : integer;
begin
  Stream := '';
  try
    if Application.CanConnection(FSocket.GetHostAddress) then
      repeat
        if FSocket.CanRead(200) and (FSocket.WaitingData > 0) then begin
          Buffer := FSocket.RecvString;
          if FSocket.SocketError <> 0 then begin
            FSocket.Purge;
            continue;
          end;
          I := 1;
          while I <= length(Buffer) do begin
            MoveToHeader(Buffer[I], Header);
            if (RequestID <> 0) and (Header.ID <> 0) and (Header.ID <> RequestID) then
              SendEndRequest(psCantMPXConn)
            else begin
              inc(I, sizeof(Header));
              Content := copy(Buffer, I, Header.Len);
              case Header.RecType of
                rtBeginRequest : ReadBeginRequest(Header, Content);
                rtAbortRequest : SendEndRequest;
                rtGetValues    : GetValues(Content);
                rtParams, rtStdIn, rtData:
                  if Content = '' then begin
                    if Header.RecType = rtParams then
                      ReadParamList(Params, Stream)
                    else begin
                      ProcessMessage(Stream);
                      if Application.Response <> '' then begin
                        SendRecord(Application.Response);
                        Application.Response := '';
                      end;
                    end;
                    Stream := '';
                  end
                  else
                    Stream := Stream + Content;
              else
                SendRecord(char(Header.RecType), rtUnknown);
              end;
            end;
            inc(I, Header.Len);
          end
        end
        else
          DoIdle;
      until Terminated
    else
      Terminate;
  except
    on E : Exception do begin
      SendRecord(E.Message);
      SendRecord(E.Message, rtStdErr);
    end;
  end;
end;

{ TFCGIApplication }

function TFCGIApplication.CanConnection(Address: string): boolean; begin
  Result := (WebServers = nil) or (WebServers.IndexOf(Address) <> -1)
end;

constructor TFCGIApplication.Create(pFCGIThreadClass : TFCGIThreadClass; pResponse : string);
var
  WS : string;
begin
  FCGIThreadClass := pFCGIThreadClass;
  Response := pResponse;
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
    Bind(2014, 10);
    repeat
      NewThread := Accept(200);
      if NewThread <> 0 then FCGIThreadClass.Create(NewThread);
    until Terminated;
  end;
end;

end.
