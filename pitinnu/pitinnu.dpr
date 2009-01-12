program pitinnu;

{$IFDEF MSWINDOWS}{$APPTYPE CONSOLE}{$ENDIF}

uses
  FCGIApp, ExtPascal, Classes, SysUtils, {$IFDEF MSWINDOWS}Services,{$ENDIF}
  pitPrevalence, pitUtils, pitThread, pitModel, pitinnuModel, pitServer;//, pitSupportGDB, pitProxyDebugger;

{$IFDEF MSWINDOWS}
type
  TServiceThread = class(TThread)
    procedure Execute; override;
  end;

procedure TServiceThread.Execute; begin
  Application.Run(Self)
end;
{$ENDIF}

begin
  DateSeparator := '/'; ShortDateFormat := 'd/M/yyyy';
  TimeSeparator := ':'; ShortTimeFormat := 'hh:mm';
  FileMode      := fmShareDenyWrite + fmOpenReadWrite;
{$IFDEF MSWINDOWS}
  Service := TService.Create(ServerName, PrevVersion + ' - ' + GetEnvironment);
  with Service do try
    if Install then
      writeln('Service installed')
    else if Uninstall then
      writeln('Service uninstalled')
    else begin
      Prevalence  := TPrevalence.Create(ServerName);
      Application := TFCGIApplication.Create(ServerName + ' ' + PrevVersion + ' - ' + GetEnvironment, TpitThread, StrToInt(GetIniParameter('Connection', 'Port', '2016')));
      NoService   := not Exists;
      Recover;
      if Exists then
        Run([TServiceThread.Create(true)])
      else
        Application.Run;
      Snapshot;
    end;
  except
    on E : Exception do ReportEventLog(EventError, 1, E.Message);
  end;
{$ELSE}
  Prevalence  := TPrevalence.Create(ServerName);
  Application := TFCGIApplication.Create(ServerName + ' ' + PrevVersion + ' - ' + GetEnvironment, TpitThread, StrToInt(GetIniParameter('Connection', 'Port', '2016')));
  Recover;
  Application.Run;
  Snapshot;
{$ENDIF}
end.
