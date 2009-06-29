{
It is used to run an ExtPascal application on a Web Server that not provides FastCGI protocol but that provides CGI.
-Steps:-
1. The Browser requests a <link FCGIApp.pas, FCGI application> through CGI gateway (for example ExtPascalSamples.cgi).
2. The CGI gateway tries to connect to associated <link FCGIApp.pas, FCGI application> counterpart.
3. If the <link FCGIApp.pas, FCGI application> isn't running, the CGI gateway fires up the <link FCGIApp.pas, FCGI application>.
4. The <link FCGIApp.pas, FCGI application> processes request from CGI gateway and returns the result.
5. The CGI gateway forward <link FCGIApp.pas, FCGI application> result to the browser. The communication is done through CGI standard protocol.
6. If there's no request over than <link TFCGIApplication.Create, MaxIdleMinutes>, the <link FCGIApp.pas, FCGI application> terminates itself.
<image cgigateway>
Author: Wanderlan Santos dos Anjos, wanderlan.anjos@gmail.com
Date: jul-2008
License: <extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
program CGIGateway;

{$IFDEF MSWINDOWS}{$APPTYPE CONSOLE}{$ENDIF}

uses
  SysUtils, BlockSocket
  {$IFNDEF MSWINDOWS} // Posix systems
    , BaseUnix;
  {$ELSE}
    {$IFNDEF FPC} // Delphi
      , ShellAPI;
    {$ELSE}
      ; // FPC Windows optimization, ShellAPI generate a greater .exe
      function ShellExecute(hWnd: integer; Operation, FileName, Parameters,
        Directory: PChar; ShowCmd: Integer): integer; stdcall; external 'shell32.dll' name 'ShellExecuteA';
    {$ENDIF}
  {$ENDIF}

const
  Host = '127.0.0.1'; // Host IP address, default is '127.0.0.1' (localhost)
  Port = 2014;        // Socket port to comunicate with FastCGI application. Change this if necessary.

{
Adds a pair Name/Value to a <link FCGIApp.pas, FastCGI> <link TRecType, rtGetValuesResult> record type
@param S Body of <link TRecType, rtGetValuesResult> record type
@param Param Pair Name/Value
}
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

// Writes a log message to the browser screen
procedure Log(Msg : string);
const
	First : boolean = true;
begin
  if First then begin
  	writeln('Content-Type:text/plain'#10#13);
  	First := false;
  end;
  writeln(Msg);
end;

// Returns the environment variables encapsulated using FastCGI protocol
function EnvVariables : string;
const
  EnvVar : array[0..39] of string = (
    'QUERY_STRING', 'PATH_INFO', 'REQUEST_METHOD', 'HTTP_COOKIE', 'HTTP_ACCEPT_LANGUAGE', 'SCRIPT_NAME', 'DOCUMENT_ROOT', 'HTTP_X_REQUESTED_WITH', //7 only essential
    'CONTENT_LENGTH', 'REQUEST_URI', 'SCRIPT_FILENAME', 'SERVER_ADMIN', //11
    'HTTP_USER_AGENT', 'HTTP_HOST', 'HTTP_ACCEPT', 'HTTP_ACCEPT_CHARSET', 'HTTP_ACCEPT_ENCODING', 'HTTP_KEEP_ALIVE', 'HTTP_CONNECTION', 'HTTP_REFERER', //19
    'AUTH_TYPE', 'CONTENT_TYPE', 'PATH_TRANSLATED',
    'REMOTE_ADDR', 'REMOTE_HOST', 'REMOTE_PORT', 'REMOTE_IDENT', 'REMOTE_USER',
    'SERVER_ADDR', 'SERVER_NAME', 'SERVER_PORT', 'SERVER_SIGNATURE', 'SERVER_SOFTWARE', 'SERVER_PROTOCOL', 'GATEWAY_INTERFACE',
    'HTTP_IF_MODIFIED_SINCE', 'PATH', 'SystemRoot', 'COMSPECC', 'WINDIR');
var
  I : integer;
  Value : string;
begin
  Result := #1#1#0#1#0#8#0#0#0#1#0#0#0#0#0#0; // FCGI Begin Request
  for I := 0 to high(EnvVar) do begin
    Value := GetEnvironmentVariable(EnvVar[I]);
    if Value <> '' then AddParam(Result, [EnvVar[I], Value]);
  end;
  Result := Result + #1#4#0#1#0#0#0#0; // FCGI End Params
end;

var
  Socket : TBlockSocket; // Block socket object

{
Reads the data from Web Server using CGI protocol and
writes the same data to FastCGI application using FastCGI protocol
}
procedure TalkFCGI;
var
  Request : string;
  Tam : word;
begin
  with Socket do begin
    Request := '';
    if GetEnvironmentVariable('REQUEST_METHOD') = 'POST' then Read(Request);
    Tam := length(Request);
    if Request <> '' then Request := Request + #1#5#0#1#0#0#0#0;
    SendString(EnvVariables + #1#5#0#1 + chr(hi(Tam)) + chr(lo(Tam)) + #0#0 + Request);
    Request := '';
    CanRead(3000);
    repeat
      Request := Request + RecvString;
    until WaitingData = 0;
    if length(Request) > 8 then
      writeln(copy(Request, 9, (byte(Request[5]) shl 8) + byte(Request[6])));
  end;
end;

{
Executes a program file
@param Prog Executable file with path
@return True if succeded else False
}
function Exec(Prog : string) : boolean;
{$IFNDEF MSWINDOWS}
var
  ArgV : array of pchar;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := ShellExecute(0, nil, pchar(Prog), nil, nil, 0) > 31
{$ELSE}
  Result := false;
	case fpFork of
		-Maxint..-1 : Result := false;
    0 : begin
      FpSetSid; // set process as session leader
      // re-fork in order to enable session leader process get exited
      if fpFork = 0 then begin
        FpChDir(ExtractFilePath(Prog)); // make sure process path
        FpUMask(0); // reset umask
        // close all std
        FpClose(2);
        FpClose(1);
        FpClose(0);
        // open new std point to /dev/null
        FpOpen('/dev/null', O_RDWR);
        FpDup2(0, 1);
        FpDup2(0, 2);
        // run fcgi
        SetLength(ArgV, 2);
        ArgV[0] := pchar(Prog);
        ArgV[1] := nil;
        FpExecv(Prog, ArgV);
      end;
      FpExit(0);
    end
  else
  	Result := true
  end;
{$ENDIF}
end;

var
  FCGIApp : string; // FastCGI program file name. The extension is '.exe' on Windows and '.fcgi' on Posix platforms
begin
  Socket := TBlockSocket.Create;
  Socket.Connect(Host, Port);
  if Socket.Error = 0 then
    TalkFCGI
  else begin
  	Socket.Free;
    FCGIApp := ChangeFileExt(paramstr(0), {$IFDEF MSWINDOWS}'.exe'{$ELSE}'.fcgi'{$ENDIF});
    if Exec(FCGIApp) then begin
      sleep(1000);
    	Socket := TBlockSocket.Create;
      Socket.Connect(Host, Port);
      if Socket.Error = 0 then
        TalkFCGI
      else
        Log('CGIGateway: FastCGI application (' + FCGIApp + ') not connect at port ' + IntToStr(Port));
    end
    else
      Log('CGIGateway: ' + FCGIApp + ' not found or has no execute permission.');
  end;
  try Socket.Free; except end;
end.
