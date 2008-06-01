unit BlockSocket;

interface

uses
  {$IFDEF FPC}Sockets{$ELSE}SocketsDelphi{$ENDIF};

type
  TBlockSocket = class
  private
    Socket : TSocket;
		RemoteSin : TInetSockAddr;
  public
    constructor Create(S : TSocket = 0);
		destructor Destroy; override;
    procedure Bind(Porta, BackLog : word);
		function Accept(Timeout : integer) : integer;
    procedure Connect(Host : string; Porta : Word);
		procedure Purge;
		procedure Close;
		function RecvString : string;
    procedure SendString(const Data: string);
    function WaitingData : cardinal;
    function CanRead(Timeout: Integer): Boolean;
    function Error : integer;
    function GetHostAddress : string;
  end;

implementation

uses
  SysUtils{$IFDEF MSWINDOWS}, Windows, {$IFDEF FPC}WinSock2{$ELSE}WinSockDelphi{$ENDIF}{$ELSE}, cthreads, BaseUnix, TermIO{$ENDIF};

{$IFDEF UNIX}{$DEFINE IOCtlSocket:=fpIOCtl}{$DEFINE FD_Zero:=fpFD_Zero}{$DEFINE FD_Set:=fpFD_Set}{$DEFINE Select:=fpSelect}{$ENDIF}

procedure TBlockSocket.Bind(Porta, BackLog : word); begin
  with RemoteSin do begin
    Sin_Family := AF_INET;
    Sin_Addr.s_Addr := 0;
    Sin_Port   := htons(Porta);
  end;
  fpBind(Socket, @RemoteSin, sizeof(RemoteSin));
  fpListen(Socket, BackLog);
end;

procedure TBlockSocket.Connect(Host : string; Porta : Word); begin
  with RemoteSin do begin
    Sin_Family := AF_INET;
    Sin_Addr   := StrToHostAddr(Host);
    Sin_Port   := htons(Porta);
  end;
  fpConnect(Socket, @RemoteSin, sizeof(RemoteSin));
end;

constructor TBlockSocket.Create(S : integer = 0); begin
	if S = 0 then
  	Socket := fpSocket(AF_INET, SOCK_STREAM, 0)
	else
		Socket := S;
end;

destructor TBlockSocket.Destroy; begin
  Close;
	inherited;
end;

function TBlockSocket.GetHostAddress: string;
var
  Tam : integer;
  Addr: SockAddr;
begin
  Tam := sizeof(Addr);
  fpGetSockName(Socket, @Addr, @Tam);
  Result := NetAddrToStr(Addr.Sin_Addr);
end;

procedure TBlockSocket.Close; begin
  CloseSocket(Socket);
end;

function TBlockSocket.Accept(Timeout : integer) : integer;
var
	Tam : integer;
begin
  if CanRead(Timeout) then begin
    sleep(0);
    Tam := sizeof(RemoteSin);
    Result := fpAccept(Socket, @RemoteSin, @Tam)
  end
  else
    Result := 0;
end;

function TBlockSocket.WaitingData : cardinal;
var
  Tam : dword;
begin
	if IOCtlSocket(Socket, FIONREAD, @Tam) <> 0 then
    Result := 0
  else
    Result := Tam;
end;

function TBlockSocket.CanRead(Timeout: Integer): Boolean;
var
  FDS: TFDSet;
  TimeV: TTimeVal;
begin
  FD_Zero(FDS);
  FD_Set(Socket, FDS);
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  Result := Select(Socket + 1, @FDS, nil, nil, @TimeV) > 0;
end;

procedure TBlockSocket.Purge;
var
	Tam : cardinal;
	Buffer : pointer;
begin
  Tam := WaitingData;
	if Tam = 0 then exit;
	getmem(Buffer, Tam);
	fpRecv(Socket, Buffer, Tam, 0);
	freemem(Buffer, Tam);
end;

function TBlockSocket.RecvString : string;
var
	Tam : integer;
begin
  Tam := WaitingData;
  SetLength(Result, Tam);
  fpRecv(Socket, @Result[1], Tam, 0);
end;

procedure TBlockSocket.SendString(const Data: string); begin
	fpSend(Socket, @Data[1], length(Data), 0);
end;

function TBlockSocket.Error : integer; begin
  Result := SocketError
end;

end.
