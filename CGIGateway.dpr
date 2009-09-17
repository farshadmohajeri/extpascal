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

-Format for optional configuration file (.INI)-
To use configuration file define the conditional symbol HAS_CONFIG.
Must have a section named [FCGI]. See sample file ExtPascalSamples.ini.
If the configuration file is not found then the value is taken from default.

Below are the supported options:

* Enabled: boolean     - to enable or disable FCGI service
* Execute: boolean     - to allow or disallow CGI to execute FCGI service (only on localhost)
* Name: file name      - FCGI executable file name to load by CGI (only on localhost)
* Host: IP string      - host location of the FCGI service
* Port: socket number  - socket port of the FCGI service
* Home: path string    - path for HOME of the FCGI service
* MaxConn: integer     - max connections allowed to FCGI service
* MaxIdle: integer     - max idle time before time-out (in minutes)
* AutoOff: boolean     - auto-shutdown FCGI service after all child threads finish
* ExtPath: string      - path to ExtJS library
* ImagePath: string    - path to image collection
* ExtTheme: string     - Ext's theme selection
* ExtBuild: string     - Ext's build file name
* Password: string     - password required to shutdown and reconfigure application
* InServers: strings   - list of allowed incoming remote hosts (comma delimited strings)
* Timeout: integer     - milliseconds before timeout in TalkFCGI

Below are the conditional compiler directive available:

* HAS_CONFIG           - to enable or disable file configuration support
* CONFIG_MUST_EXIST    - to make configuration file as mandatory (required)

Author: Wanderlan Santos dos Anjos, wanderlan.anjos@gmail.com
Date: Jul-2008
License: <extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
program CGIGateway;

{$IFDEF MSWINDOWS}{$APPTYPE CONSOLE}{$ENDIF}

// force HAS_CONFIG flag if config file is mandatory
{$IFDEF CONFIG_MUST_EXIST}{$DEFINE HAS_CONFIG}{$ENDIF}

uses
  SysUtils, BlockSocket
  {$IFDEF HAS_CONFIG} // Configuration file
    , StrUtils, IniFiles
  {$ENDIF}
  {$IFNDEF MSWINDOWS} // Posix systems
    ,  Unix, BaseUnix;
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
  Host : string = '127.0.0.1'; // Host IP address, default is '127.0.0.1' (localhost)
  Port : word   = 2014;        // Socket port to comunicate with FastCGI application. Change this if necessary.
  IISDelim      = '`';         // For IIS bug
  {$IFDEF HAS_CONFIG}
  ConfigSection = 'FCGI';      // Configuration section to be read
  {$ENDIF}
  
var
  Socket     : TBlockSocket;   // Block socket object
  FCGIApp    : string;         // FastCGI program file name. The extension is '.exe' on Windows and '.fcgi' on Posix platforms
  {$IFDEF HAS_CONFIG}
  Config     : TIniFile;       // Optional configuration file to control application behavior
  ConfigFile : string;         // Configuration file name
  {$ENDIF}

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

{
Returns the environment variables encapsulated using FastCGI protocol
@param EnvName Optional environment variable name to override
@param EnvValue Optional value to override
}
function EnvVariables(EnvName : string = ''; EnvValue : string = '') : string;
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
  // Result := #1#1#0#1#0#8#0#0#0#1#0#0#0#0#0#0; // FCGI Begin Request (redundant!)
  for I := 0 to high(EnvVar) do begin
    // override default value
    if EnvVar[I] = EnvName then
      Value := EnvValue
    else
      Value := GetEnvironmentVariable(EnvVar[I]);
    {$IFDEF HAS_CONFIG}
    if I = 1 then // override PATH_INFO
      if ConfigFile <> '' then begin
        // override HOME path
        if (Value = '') or (Value = '/') then Value := '/' + Config.ReadString(ConfigSection, 'Home', '');
        // force shutdown request
        if not Config.ReadBool(ConfigSection, 'Enabled', true) then Value := '/shutdown';
      end;
    {$ENDIF}
    if Value <> '' then AddParam(Result, [EnvVar[I], Value]);
  end;
  Result := Result + #1#4#0#1#0#0#0#0; // FCGI End Params
end;

{
Sends Environment variables to FastCGI application using FastCGI protocol.
Receives the response and returns this response to Web Server
@param EnvVars Environment variables to send
}
procedure TalkFCGI(EnvVars : string);
var
  Request : string;
  Tam : word;
  R : char;
begin
  with Socket do begin
    Request := '';
    if GetEnvironmentVariable('REQUEST_METHOD') = 'POST' then
      repeat
        read(R);
        if R = IISDelim then break; // for IIS bug
        Request := Request + R;
      until seekeof(Input);
    Tam := length(Request);
    if Request <> '' then Request := Request + #1#5#0#1#0#0#0#0;
    SendString(#1#1#0#1#0#8#0#0#0#1#0#0#0#0#0#0 +  // FCGI begin request
               EnvVars + #1#5#0#1 + chr(hi(Tam)) + chr(lo(Tam)) + #0#0 + Request);
{$IFDEF HAS_CONFIG}
    if ConfigFile <> '' then
      Request := RecvString(Config.ReadInteger(ConfigSection, 'Timeout', 10000))
    else
{$ENDIF}    
    Request := RecvString(10000);  {timeout after 10 seconds}
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
  {$IFDEF HAS_CONFIG}
  Result := true;
  if ConfigFile <> '' then
    if not Config.ReadBool(ConfigSection, 'Execute', true) then exit;
  {$ENDIF}
  // allow execution only on local machine
  Result := false;
  if (Host = 'localhost') or (Host = '127.0.0.1') then begin
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
end;

{$IFDEF HAS_CONFIG}

{
Reads FCGIApp name, Host and Port from configuration file
@param AFileName Configuration file name
@return True if AFileName exists
}
function ReadConfig(AFileName : string) : boolean; 
var
  s: string;
begin
  Result := false;
  if FileExists(AFileName) then begin
    Config  := TINIFile.Create(AFileName);
    s := Config.ReadString(ConfigSection, 'Name', '');
    FCGIApp := IfThen(s = '', FCGIApp, s);
    s := Config.ReadString(ConfigSection, 'Host', '');
    Host    := IfThen(s = '', Host, s);
    Port    := Config.ReadInteger(ConfigSection, 'Port', Port);
    Result  := true;
  end;
end;

{
Forces shutdown any running FCGI instances through request
@return True if sent request
}
function ShutdownFCGI : boolean; 
var
  s: string;
begin
  Result := false;
  if (ConfigFile <> '') and not Config.ReadBool(ConfigSection, 'Enabled', true) then 
  begin
    s := Config.ReadString(ConfigSection, 'Password', ''); 
    s := IfThen(s = '', 'extpascal'); // default password
    TalkFCGI(EnvVariables('QUERY_STRING', 'password=' + s));
    Result := true;
  end;
end;

{
Kills shutdown any running FCGI instances through system call
@return True if called the kill command
}
function KillFCGI : boolean; begin
  Result := false;
  if (ConfigFile <> '') and not Config.ReadBool(ConfigSection, 'Enabled', true) then begin
    if (Host = 'localhost') or (Host = '127.0.0.1') then begin
      {$IFNDEF MSWINDOWS}
      fpSystem('killall '+ ExtractFileName(FCGIApp));
      {$ELSE}
      { TODO : windows way to force kill an app }
      {$ENDIF}
    end;
    Result := true;
  end;
end;

{$ENDIF}

var
  I : integer;
begin
  FCGIApp := ChangeFileExt(ExtractFileName(ParamStr(0)), {$IFDEF MSWINDOWS}'.exe'{$ELSE}'.fcgi'{$ENDIF});
  {$IFDEF HAS_CONFIG}
  ConfigFile := ChangeFileExt(ParamStr(0), {$IFDEF MSWINDOWS}'.ini'{$ELSE}'.conf'{$ENDIF});
   {Note Delphi TIniFile.Create requires full path to file, so base on
     location of CGIGateway executable file.}
  if not ReadConfig(ConfigFile) then begin
    ConfigFile := ''; // indicates no config found
    {$IFDEF CONFIG_MUST_EXIST}
    Log('CONFIG ERROR: Configuration file is not found or has no read permission.');
    Config.Free;
    exit;
    {$ENDIF}
  end;
  {$ENDIF}
  Socket := TBlockSocket.Create;
  Socket.Connect(Host, Port);
  if Socket.Error = 0 then begin
    {$IFDEF HAS_CONFIG}
    if not ShutdownFCGI then
    {$ENDIF}
    TalkFCGI(EnvVariables);
  end
  else begin
    Socket.Close;
  	Socket.Free;
    {$IFDEF HAS_CONFIG}
    if KillFCGI then
      Log('OUT OF SERVICE: Application is temporarily disabled or being updated. Please, try again after a few moments. ')
    else
    {$ENDIF}
    if Exec(FCGIApp) then begin
    	Socket := TBlockSocket.Create;
      for I := 1 to 3 do begin
        sleep(1000 * I);
        Socket.Connect(Host, Port);
        if Socket.Error = 0 then break;
      end;
      if Socket.Error = 0 then
        TalkFCGI(EnvVariables)
      else
        Log('CGI GATEWAY ERROR: Unable to access application '+ FCGIApp +' at '+ Host + ':' + IntToStr(Port));
    end
    else
      Log('CGI GATEWAY ERROR: '+ FCGIApp +' is not found or has no execute permission.');
  end;
  try 
    {$IFDEF HAS_CONFIG}Config.Free;{$ENDIF}
    Socket.Close;
    Socket.Free;
  except end;
end.
