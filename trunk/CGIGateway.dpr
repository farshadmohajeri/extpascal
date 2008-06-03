program CGIGateway;

{$IFDEF MSWINDOWS}{$APPTYPE CONSOLE}{$ENDIF}

uses
  SysUtils, BlockSocket, {$IFDEF FPC}Process{$ELSE}ProcessDelphi{$ENDIF};

procedure AddParam(var S : string; Param : array of string);
var
  I, J   : integer;
  Len    : array[0..1] of integer;
  Format : array[0..1] of integer;
  Tam    : word;
begin
  for I := 0 to 1 do begin
    Len[I] := length(Param[I]);
    if Len[I] <= 127 then
      Format[I] := 1
    else
      Format[I] := 4;
  end;
  Tam := Len[0] + Format[0] + Len[1] + Format[1];
  S := S + #1#4#0#1 + chr(hi(Tam)) + chr(lo(Tam)) + #0#0; // FCGI Params header
  J := length(S);
  SetLength(S, J + Tam);
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

function EnvVariables : string;
const
  EnvVar : array[0..38] of string = (
    'QUERY_STRING', 'PATH_INFO', 'REQUEST_METHOD', 'HTTP_COOKIE', 'HTTP_ACCEPT_LANGUAGE', 'SCRIPT_NAME', 'DOCUMENT_ROOT', //6 only essential
    'CONTENT_LENGTH', 'REQUEST_URI', 'SCRIPT_FILENAME', 'SERVER_ADMIN', //10
    'HTTP_USER_AGENT', 'HTTP_HOST', 'HTTP_ACCEPT', 'HTTP_ACCEPT_CHARSET', 'HTTP_ACCEPT_ENCODING', 'HTTP_KEEP_ALIVE', 'HTTP_CONNECTION', 'HTTP_REFERER', //18
    'AUTH_TYPE', 'CONTENT_TYPE', 'PATH_TRANSLATED',
    'REMOTE_ADDR', 'REMOTE_HOST', 'REMOTE_PORT', 'REMOTE_IDENT', 'REMOTE_USER',
    'SERVER_ADDR', 'SERVER_NAME', 'SERVER_PORT', 'SERVER_SIGNATURE', 'SERVER_SOFTWARE', 'SERVER_PROTOCOL', 'GATEWAY_INTERFACE',
    'HTTP_IF_MODIFIED_SINCE', 'PATH', 'SystemRoot', 'COMSPECC', 'WINDIR');
var
  I : integer;
  Value : string;
begin
  Result := #1#1#0#1#0#8#0#0#0#1#0#0#0#0#0#0; // FCGI Begin Request
  for I := 0 to 6 (*high(EnvVar)*) do begin
    Value := GetEnvironmentVariable(EnvVar[I]);
    if Value <> '' then AddParam(Result, [EnvVar[I], Value]);
  end;
  Result := Result + #1#4#0#1#0#0#0#0; // FCGI End Params
end;

const
  Host = '127.0.0.1';
  Port = 2014;

var
  Socket : TBlockSocket;

procedure TalkFCGI;
var
  Request : string;
  Tam : word;
begin
  with Socket do begin
    Request := '';
    if GetEnvironmentVariable('REQUEST_METHOD') = 'POST' then Read(Request);
    Tam := length(Request);
    SendString(EnvVariables + #1#5#0#1 + chr(hi(Tam)) + chr(lo(Tam)) + #0#0 + Request);
    Request := '';
    CanRead(3000);
    repeat
      Request := Request + RecvString;
    until WaitingData = 0;
    if length(Request) > 8 then
      writeln(copy(Request, 9, (byte(Request[5]) shl 8) + byte(Request[6])))
    else
      writeln;
  end;
end;

begin
  Socket := TBlockSocket.Create;
  with Socket do begin
    Connect(Host, Port);
    if Error = 0 then
      TalkFCGI
    else
      with TProcess.Create(nil) do begin
        CommandLine := 'C:/Trabalho/ExtPascal/ExtPascalSamples.exe';//GetEnvironmentVariable('SCRIPT_FILENAME');
        Options := [poNoConsole];
        try
          Execute;
          sleep(10);
          Connect(Host, Port);
          if Error = 0 then
            TalkFCGI
          else
            writeln('FastCGI application (', CommandLine, ') not connect at port ', Port);
        except
          writeln(CommandLine, ' not found.');
        end;
      end;
    Free;
  end;
end.
