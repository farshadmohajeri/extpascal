unit epGDB;

interface

uses
  Process, Classes;

type
  TGDBFormat = (gfDefault, gfTexto, gfDecimal, gfHexadecimal, gfPonteiro, gfCaractere, gfBinario, gfTipo, gfDumpTexto, gfDumpDecimal, gfDumpHexa);
  TWatchKind = (wkWrite, wkRead, wkReadWrite);
  TTraceKind = (tkCustom, tkWatches, tkSelf, tkLocals, tkCallStack, tkThreads, tkModules);
  TTraceKindSet = set of TTraceKind;
  TGDBCallBack = procedure(Path, S : string; Thread : TThread);

  TGDB = class
  private
    Process : TProcess;
    OutputList : TStringList;
    TargetID : integer;
    Synchronize, HasWatchDog, HasSelf : boolean;
    TraceFile, fPath : string;
    CallBack : TGDBCallBack;
    Thread   : TThread;
    function SetDebugpointParams(Condition: string; PassCount: integer; Trace: string; TraceKind : TTraceKindSet) : string;
    function ReplaceAll(S: string; Old, New: array of string): string;
    function StripInitial(S: string; Initial: array of string): string;
    function StripResponse(S: string): string;
    function FormatEvaluate(Expression, Out: string): string;
    function FormatExpression(E: string): string;
    procedure Trace(var Msg: string);
    procedure SaveConfig;
    procedure RestoreConfig;
  public
    property Path : string read fPath;
    constructor Create(pPath, pExe : string; pCallBack : TGDBCallBack; pThread : TThread);
    destructor Destroy; override;
    function Output : string;
    function Command(Cmd : string; Wait : boolean = true) : string;
    function RunCommand(Cmd: string): string;
    function Evaluate(Expression : string; Format : TGDBFormat = gfDefault; Len : integer = 10) : string;
    function Modify(Variable, Expression : string) : string;
    function AddBreakpoint(Source: string; Line: integer; Condition: string = ''; PassCount: integer = 0;
                           Trace : string = ''; TraceKind : TTraceKindSet = []) : string; overload;
    function AddBreakpoint(Source: string; Condition: string = ''; PassCount: integer = 0;
                           Trace : string = ''; TraceKind : TTraceKindSet = []) : string; overload;
    function AddWatchpoint(Expression : string; Kind: TWatchKind; Condition: string = ''; PassCount: integer = 0;
                           Trace : string = ''; TraceKind : TTraceKindSet = []) : string;
    procedure DeleteDebugpoint(Number: string);
    procedure DisableDebugpoint(Number: string);
    procedure EnableDebugpoint(Number: string);
    function AddWatch(Expression : string; Format : TGDBFormat = gfDefault) : string;
    procedure DeleteWatch(Number: string);
    function StepInto : string;
    function StepOver : string;
    procedure Pause;
    procedure Reset;
    function Run : string;
    function RunToCursor(Source: string; Line: integer) : string; overload;
    function RunToCursor(Source: string) : string; overload;
    function BackToCursor (Source: string; Line: integer) : string; overload;
    function BackToCursor (Source: string) : string; overload;
    function RunUntilReturn : string;
    function ShowCallStack: string;
    function ShowDebugpoints: string;
    function ShowLocals: string;
    function ShowModules: string;
    function ShowThreads: string;
    function ShowWatches: string;
    function ShowTrace: string;
  end;

function PosExR(Sub : char; S : string; I : integer): integer;

implementation

uses
  SysUtils, StrUtils, DateUtils, {$IFDEF MSWindows}Windows{$ELSE}BaseUnix{$ENDIF};

constructor TGDB.Create(pPath, pExe: string; pCallBack : TGDBCallBack; pThread : TThread); // testar compilações com dwarf2/3 e stabs+
var
  GDB : string;
begin
  fPath := ExtractFilePath(pExe);
  GDB := pPath + 'gdb';
  if not FileExists(GDB + '.exe') then
    raise Exception.Create('Depurador: '+ GDB + ' não pôde ser encontrado.')
  else begin
    inherited Create;
    Process := TProcess.Create(nil);
    with Process do begin
      TraceFile := pExe + '.trc';
      CommandLine := GDB + ' -silent ' + pExe;
      Options := [poUsePipes, poStderrToOutPut, poNoConsole];
      Execute;
      if not Running then
        raise Exception.Create('Erro ao iniciar: '+ GDB + ': ' + Self.Output)
      else begin
        OutputList := TStringList.Create;
        CallBack   := pCallBack;
        Thread     := pThread;
        RestoreConfig;
        Reset;
      end;
    end;
  end;
end;

destructor TGDB.Destroy; begin
  if Process <> nil then begin
    SaveConfig;
    Command('kill', false);
    Command('quit', false);
    Process.Terminate(0);
    Process.Free;
    OutputList.Free;
  end;
  inherited
end;

procedure TGDB.SaveConfig;
var
  ConfigFile : text;
begin
  Assign(ConfigFile, ChangeFileExt(TraceFile, '.dbg'));
  Rewrite(ConfigFile);
  Writeln(ConfigFile, Command('info breakpoints')); // save all breakpoints, watchpoints and traces
  Writeln(ConfigFile, Command('info display'));     // save all watches
  Close(ConfigFile);
end;

function Find(A : array of string; S : string) : integer; begin
  for Result := 0 to high(A) do
    if pos(A[Result], S) <> 0 then exit;
  Result := -1;
end;

procedure TGDB.RestoreConfig;
var
  ConfigFile : text;
  Cfg, Line, Cmd : string;
  Display : boolean;
  I : integer;
begin
  Cfg := ChangeFileExt(TraceFile, '.dbg');
  if FileExists(Cfg) then begin
    Assign(ConfigFile, Cfg);
    System.Reset(ConfigFile);
    Display := false;
    readln(ConfigFile);
    while not SeekEof(ConfigFile) do begin
      readln(ConfigFile, Line);
      if Line[1] in ['0'..'9'] then
        if Display then begin
          I := LastDelimiter('/', Line);
          if I <= 0 then I := LastDelimiter(' '#9, Line);
          Command('display ' + trim(copy(Line, I, length(Line))), false)
        end
        else begin
          I := LastDelimiter(' '#9, Line);
          case Find(['breakpoint', 'read watchpoint', 'acc watchpoint'], Line) of
            0 : Cmd := 'break';
            1 : Cmd := 'rwatch';
            2 : Cmd := 'awatch'
          else
            Cmd := 'watch'
          end;
          Command(Cmd + ' ' + trim(copy(Line, I, length(Line))))
        end
      else
        if not Display then
          case Find(['Auto-display', 'stop only', 'ignore next', 'breakpoint already'], Line) of
            0 : Display := true;
            1 : begin
              I := pos(' if ', Line);
              Command('condition $bpnum ' + copy(Line, I+4, Length(Line)), false)
            end;
            2, 3 : ;
          else
            Command('commands $bpnum', false);
            while pos('p "|"', Line) = 0 do begin
              Command(trim(Line), false);
              readln(ConfigFile, Line)
            end;
            readln(ConfigFile);
            Command('p "|"', false);
            Command('continue', false);
            Command('end', false);
          end
    end;
    ShowWatches; // discard output
    Close(ConfigFile);
  end;
end;

function TGDB.Command(Cmd : string; Wait : boolean = true) : string;
var
  S : integer;
  T : TDateTime;
begin
  Pause;
  S := Process.Output.Size;
  Cmd := Cmd + ^M^J;
  Process.Input.WriteBuffer(Cmd[1], length(Cmd));
  sleep(1);
  write('>>>', Cmd);//
  if Wait then begin
    T := Now;
    while (Process.Output.Size = S) and (MilliSecondsBetween(Now, T) < 200) do sleep(10);
    Synchronize := MilliSecondsBetween(Now, T) > 200;
  end;
  Result := Output;
  write('<<<', Result);//
end;

type
  WatchDog = class(TThread)
    fGDB : TGDB;
    procedure Execute; override;
    constructor Create(pGDB : TGDB);
  end;

constructor WatchDog.Create(pGDB: TGDB); begin
  pGDB.HasWatchDog := true;
  fGDB := pGDB;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure WatchDog.Execute; begin
  inherited;
  with fGDB do begin
    repeat
      sleep(10);
      Synchronize := Process.Output.Size = 0;
    until not Synchronize;
    if @CallBack <> nil then CallBack(Path, Output, Thread);
    Synchronize := false;
    HasWatchDog := false;
  end;
end;

function TGDB.RunCommand(Cmd : string) : string; begin
  Result := Command(Cmd);
  Synchronize := Result = '';
  if Synchronize and not HasWatchDog then WatchDog.Create(Self);
end;

procedure TGDB.Reset;
var
  S : string;
  I : integer;
begin
  SysUtils.DeleteFile(TraceFile);
  Command('start'); // Command('set case-sensitive off'); não funciona por bug do GDB
  Command('set prompt', false);
  Command('set confirm off', false);
  Command('set height 0', false);
  Command('set width 0', false);
  Command('set print address off', false);
  Command('set print pretty on', false);
  Command('set print object on', false);
  Command('set language pascal');
  Command('set editing off');
  Command('set unwindonsignal on'); // break FPC_RAISEEXCEPTION
  S := Command('info program');
  I := pos('child thread ', S);
  if I <> 0 then TargetID := StrToIntDef(copy(S, I + 13, posex('.', S, I) - (I + 13)), 0);
  if TargetID = 0 then raise Exception.Create('Erro ao iniciar: '+ Process.CommandLine + ': ' + S);
end;

procedure TGDB.Pause;
{$IFDEF MSWindows} // Não funciona com Windows 95, 98 e ME
var
  hProcess, hThread, hMod : THandle;
  ThreadID : cardinal;
const
  DebugBreak : pointer = nil;
  CreateRemoteThread: function(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer; dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall = nil;
begin
  if Synchronize then begin
    if DebugBreak = nil then begin
      hMod := GetModuleHandle(kernel32);
      if hMod <> 0 then begin
        DebugBreak := GetProcAddress(hMod, 'DebugBreak');
        CreateRemoteThread := GetProcAddress(hMod, 'CreateRemoteThread');
      end;
    end;
    hProcess := OpenProcess(PROCESS_CREATE_THREAD or PROCESS_QUERY_INFORMATION or PROCESS_VM_OPERATION or PROCESS_VM_WRITE or PROCESS_VM_READ, False, TargetID);
    if hProcess <> 0 then begin
      if @CreateRemoteThread <> nil then begin
        hThread := CreateRemoteThread(hProcess, nil, 0, DebugBreak, nil, 0, ThreadID);
        if hThread <> 0 then CloseHandle(hThread);
      end;
      CloseHandle(hProcess);
    end;
{$ELSE}
begin
  if Synchronize then begin
    fpKill(TargetID, SIGINT);
{$ENDIF}
    Synchronize := false;
    ShowWatches; // force and discard output
  end;
end;

function TGDB.StripResponse(S : string) : string;
var
  I, J : integer;
begin
  Result := S;
  I := pos('$', Result);
  while I <> 0 do begin
    if (I <> 0) and (Result[I+1] in ['0'..'9']) then begin
      J := posex(' = ', Result, I);
      if J <> 0 then delete(Result, I, J-I+3);
    end;
    I := posex('$', Result, I+1);
  end;
end;

function TGDB.ReplaceAll(S : string; Old, New : array of string) : string;
var
  I : integer;
begin
  Result := S;
  for I := 0 to high(Old) do
    Result := StringReplace(Result, Old[I], New[I], [rfReplaceAll, rfIgnoreCase]);
end;

function TGDB.StripInitial(S : string; Initial : array of string) : string;
var
  I, J, K : integer;
begin
  Result := S;
  HasSelf := false;
  for I := 0 to high(Initial) do begin
    J := pos(Initial[I], Result);
    if (J <> 0) and ((J = 1) or (Result[J-1] in [^M, ^J])) then begin
      if Initial[I] = 'this = ' then HasSelf := true;
      K := posex(^M, Result, J);
      if K = 0 then
        K := length(S)
      else
        K := K - J;
      delete(Result, J, K);
      Result := trim(Result);
    end;
  end;
end;

function PosExR(Sub : char; S : string; I : integer): integer; begin
  for Result := I downto 1 do
    if S[Result] = Sub then exit;
  Result := -1
end;

procedure TGDB.Trace(var Msg : string);
var
  Trc : text;
  I : integer;
begin
  Assign(Trc, TraceFile); // Grava trace no arquivo Exe.trc
  if FileExists(TraceFile) then
    Append(Trc)
  else
    Rewrite(Trc);
  I := PosExR('|', Msg, length(Msg));
  writeln(Trc, DateTimeToStr(Now), ': ', trim(copy(Msg, 1, I-2)));
  Msg := TrimLeft(copy(Msg, I+2, length(Msg)));
  Close(Trc);
end;

function TGDB.Output: string; begin
  Result := '';
  while Process.Output.Size <> 0 do begin // Process.Output tem um buffer limitado < 1040 bytes
    OutputList.LoadFromStream(Process.Output);
    sleep(1); // coloca caracteres pendentes no Output
    Result := Result + StripResponse(OutputList.Text);
    Result := ReplaceAll(Result, ['{', '}', '0x00', '0x0', '0x', '__', ', this=', 'this=', ', $vmt=', ', vmt=', '(ANSISTRING) '^M, '(ANSISTRING) ', 'this->', 'type = '],
                                 ['(', ')', 'nil',  'nil', '$',  '.',  '',        '',      '',        '',       '''''',            '',               '',      '']);
    Result := StripInitial(Result, ['No symbol t', 'No local', 'No arg', 'There are no', 'No break', 'Cannot', '"finish" ', '$vmt = ', 'vmt = ',
                           '$result = ', 'result = ', 'RESULT = ', 'this = ', 'type = ']);
    if Process.Output.Size <> 0 then sleep(10); // se houver caracteres pendentes dá mais tempo
  end;
  if pos('''|''', Result) <> 0 then Trace(Result);
end;

function TGDB.FormatEvaluate(Expression, Out : string) : string;
var
  I, J : integer;
  C : string;
begin
  Result := Out;
  if Out <> '' then begin
    if pos(#9, Result) <> 0 then begin // Dump
      Result := ReplaceAll(Result, [#9, ' #', ' '''], [' ', '#', '''']);
      I := pos('$', Result);
      while I <> 0 do begin
        J := posex(':', Result, I);
        if J <> 0 then begin
          delete(Result, I, J-I+1);
          I := pos(^M, Result);
        end
        else begin
          Result := copy(Result, 2, length(Result));
          break
        end;
      end;
    end;
    I := pos('members of ', Result);
    if I <> 0 then begin
      J := posex(':', Result, I);
      C := copy(Result, I+11, J-I-11);
      Result := Expression + ' : ' + C + ' = ' + copy(Result, J + 1, length(Result));
    end;
  end;
end;

function LastDelimiterR(Sub, S : string): integer;
var
  I : integer;
begin
  for Result := length(S) downto 1 do
    for I := 1 to length(Sub) do
      if S[Result] = Sub[I] then exit;
  Result := -1
end;

function OpToFunc(E, Op, Func : string) : string;
var
  I, J, K, P : integer;
  Change : boolean;
begin
  Result := E;
  I := pos(Op, Result);
  while I <> 0 do begin
    Change := false; J := length(Result); K := 1;
    for P := I+length(Op) to length(Result) do
      case Result[P] of
        ' ' : ;
        '''': begin
          J := PosEx('''', Result, P+1);
          Change := true;
          break;
        end;
      else
        J := LastDelimiter(' )', copy(Result, P, length(Result)));
        if J <= 0 then
          J := length(Result)
        else
          inc(J, P);
        break;
      end;
    for P := I-1 downto 1 do
      case Result[P] of
        ' ' : ;
        '''': begin
          K := PosExR('''', Result, P-1);
          Change := true;
          break;
        end;
      else
        K := LastDelimiterR(' (', copy(Result, 1, P-1));
        if K = -1 then K := 0;
        break;
      end;
    if Change then begin
      P := length(Op);
      delete(Result, I, P);
      insert(',', Result, I);
      inc(J, P);
      insert(')', Result, J);
      insert(Func + '(', Result, K+1);
    end;
    I := PosEx(Op, Result, I+1);
  end;
end;

function TGDB.FormatExpression(E : string) : string; begin
  Result := OpToFunc(E, '=', 'equal');
  Result := OpToFunc(Result, '<>', 'diff');
  Result := ReplaceAll(Result, ['''', '<>'], ['"', ' <> ']);
end;

function TGDB.Evaluate(Expression : string; Format : TGDBFormat = gfDefault; Len : integer = 10): string;
var
  C : string;
begin
  case Format of
    gfDefault :     C := 'p'; // print
    gfCaractere :   C := 'p/c';
    gfTexto :       C := 'p/s';
    gfDecimal :     C := 'p/u';
    gfHexadecimal : C := 'p/x';
    gfBinario :     C := 'p/t';
    gfPonteiro :    C := 'p/a';
    gfTipo :        C := 'pt';
    gfDumpTexto :   C := 'x/' + IntToStr(Len) + 'cb'; // examine
    gfDumpDecimal : C := 'x/' + IntToStr(Len) + 'ub';
    gfDumpHexa :    C := 'x/' + IntToStr(Len) + 'xb';
  end;
  Result := FormatEvaluate(Expression, Command(C + ' ' + FormatExpression(Expression)));
end;

function TGDB.Modify(Variable, Expression : string) : string; begin
  Result := Command('set var ' + Variable + ' := ' + FormatExpression(Expression), false);
end;

function TGDB.SetDebugpointParams(Condition : string; PassCount : integer; Trace : string; TraceKind : TTraceKindSet) : string; begin
  if Condition <> '' then Command('condition $bpnum ' + FormatExpression(Condition), false);
  if PassCount <> 0  then Command('ignore $bpnum ' + IntToStr(PassCount), false);
  if TraceKind <> [] then begin
    Command('commands $bpnum', false);
    if tkModules in TraceKind then Command('info share', false);
    if tkThreads in TraceKind then Command('info threads', false);
    if tkCallStack in TraceKind then Command('info stack', false);
    if tkLocals in TraceKind then begin
      Command('info args', false);
      Command('info locals', false);
    end;
    if tkSelf in TraceKind then begin
      Command('printf "\n%s = ", "Self"', false);
      Command('p Self', false);
    end;
    if tkWatches in TraceKind then Command('display', false);
    if tkCustom in TraceKind then Command('printf ' + FormatExpression(Trace), false);
    Command('p "|"', false); // marca fim do trace
    Command('continue', false);
    Command('end', false);
  end;
  Result := Output
end;

function TGDB.AddBreakpoint(Source : string; Line : integer; Condition : string = ''; PassCount : integer = 0;
                            Trace : string = ''; TraceKind : TTraceKindSet = []) : string; begin
  Result := Command('break ' + Source + ':' + IntToStr(Line));
  Result := Result + SetDebugpointParams(Condition, PassCount, Trace, TraceKind);
end;

function TGDB.AddBreakpoint(Source : string; Condition : string = ''; PassCount : integer = 0;
                            Trace : string = ''; TraceKind : TTraceKindSet = []) : string; begin
  Result := Command('break ' + Source);
  Result := Result + SetDebugpointParams(Condition, PassCount, Trace, TraceKind);
end;

procedure TGDB.DeleteDebugpoint(Number : string); begin
  Command('delete ' + Number, false); // Se number = '' deleta todos
end;

procedure TGDB.EnableDebugpoint(Number : string); begin
  Command('enable ' + Number, false);
end;

procedure TGDB.DisableDebugpoint(Number : string); begin
  Command('disable ' + Number, false); // Se number = '' desabilita todos
end;

function TGDB.AddWatchpoint(Expression : string; Kind: TWatchKind; Condition : string = ''; PassCount : integer = 0;
                            Trace : string = ''; TraceKind : TTraceKindSet = []) : string;
var
  K : string;
begin
  case Kind of
    wkWrite     : K := '';
    wkRead      : K := 'r';
    wkReadWrite : K := 'a';
  end;
  Result := Command(K + 'watch ' + Expression);
  Result := Result + SetDebugpointParams(Condition, PassCount, Trace, TraceKind);
end;

function TGDB.AddWatch(Expression : string; Format : TGDBFormat = gfDefault) : string;
var
  C : string;
begin
  case Format of
    gfCaractere :   C := '/c';
    gfTexto :       C := '/s';
    gfDecimal :     C := '/u';
    gfHexadecimal : C := '/x';
    gfBinario :     C := '/t';
    gfPonteiro :    C := '/a';
  else
    C := '';
  end;
  Result := Command('display' + C + ' ' + FormatExpression(Expression));
  Synchronize := false;
end;

procedure TGDB.DeleteWatch(Number : string); begin
  Command('delete display ' + Number, false);
end;

function TGDB.StepInto : string; begin
  Result := RunCommand('step');
end;

function TGDB.StepOver : string; begin
  Result := RunCommand('next');
end;

function TGDB.Run : string; begin
  Result := RunCommand('continue');
end;

function TGDB.RunToCursor(Source : string; Line : integer) : string; begin
  Command('tbreak ' + Source + ':' + IntToStr(Line));
  Result := Run;
end;

function TGDB.RunToCursor(Source : string) : string; begin
  Command('tbreak ' + Source);
  Result := Run;
end;

function TGDB.BackToCursor(Source : string; Line : integer) : string; begin
  Command('tbreak ' + Source + ':' + IntToStr(Line));
  Result := Command('jump ' + Source + ':' + IntToStr(Line));
end;

function TGDB.BackToCursor(Source : string) : string; begin
  Command('tbreak ' + Source);
  Result := Command('jump ' + Source);
end;

function TGDB.RunUntilReturn : string; begin
  Result := RunCommand('finish');
end;

function TGDB.ShowLocals : string;
var
  S : string;
begin
  Result := Command('info args');
  if HasSelf then begin
    S := Evaluate('Self');
    if S <> '' then Result := Result + ^M^J + 'Self = ' + S;
  end;
  Result := Result + Command('info locals');
end;

function TGDB.ShowWatches : string; begin
  Result := Command('display');
  Synchronize := false;
end;

function TGDB.ShowDebugpoints : string; begin
  Result := Command('info breakpoints');
end;

function TGDB.ShowCallStack : string; begin
  Result := Command('info stack');
end;

function TGDB.ShowModules : string; begin
  Result := Command('info share');
end;

function TGDB.ShowThreads : string; begin
  Result := Command('info threads');
end;

function TGDB.ShowTrace : string;
var
  Trc : file;
begin
  if FileExists(TraceFile) then begin
    Assign(Trc, TraceFile);
    System.Reset(Trc, 1);
    SetLength(Result, FileSize(Trc));
    BlockRead(Trc, Result[1], length(Result));
    Close(Trc)
  end
  else
    Result := '';
end;

end.
