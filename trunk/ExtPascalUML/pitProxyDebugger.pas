unit pitProxyDebugger;

interface

implementation

uses
  //pitProxyServer,
  pitCommon, pitGDB, pitGenerator, pitThread;

type
  TUnpProxyServer = class(TProxyServer);

procedure InvokeDebugger(ProxyServer : TProxyServer; Cmd : TProxyCommand); begin
  with TUnpProxyServer(ProxyServer), fDecoder, fEncoder, TGDB(Browser.Thread.fGDB) do
    case Cmd of
      cmdEvaluate :         WriteString(Evaluate(ReadString, TGDBFormat(byte(ReadSize)), ReadSize));
      cmdModify :           WriteString(Modify(ReadString, ReadString));
      cmdAddBreakpoint :    WriteString(AddBreakpoint(ModelToSource(Path, ReadString), ReadString, ReadSize, ReadString, TTraceKindSet(Byte(ReadSize))));
      cmdAddWatchpoint :    WriteString(AddWatchpoint(ReadString, TWatchKind(byte(ReadSize)), ReadString, ReadSize, ReadString, TTraceKindSet(Byte(ReadSize))));
      cmdDeleteDebugpoint : DeleteDebugpoint(ReadString);
      cmdDisableDebugpoint: DisableDebugpoint(ReadString);
      cmdEnableDebugpoint : EnableDebugpoint(ReadString);
      cmdAddWatch :         WriteString(AddWatch(ReadString, TGDBFormat(byte(ReadSize))));
      cmdDeleteWatch :      DeleteWatch(ReadString);
      cmdStepInto :         WriteString(SourceToModel(Path, StepInto));
      cmdStepOver :         WriteString(StepOver);
      cmdPause :            Pause;
      cmdReset :            Reset;
      cmdRun :              WriteString(SourceToModel(Path, Run));
      cmdRunToCursor :      WriteString(SourceToModel(Path, RunToCursor(ModelToSource(Path, ReadString))));
      cmdBackToCursor :     WriteString(SourceToModel(Path, BackToCursor(ModelToSource(Path, ReadString))));
      cmdRunUntilReturn :   WriteString(SourceToModel(Path, RunUntilReturn));
      cmdShowCallStack :    WriteString(ShowCallStack);
      cmdShowDebugpoints :  WriteString(ShowDebugpoints);
      cmdShowLocals :       WriteString(ShowLocals);
      cmdShowModules :      WriteString(ShowModules);
      cmdShowThreads :      WriteString(ShowThreads);
      cmdShowWatches :      WriteString(ShowWatches);
      cmdShowTrace :        WriteString(ShowTrace);
    end;
end;

begin
  ProxyDebugger := InvokeDebugger;
end.

