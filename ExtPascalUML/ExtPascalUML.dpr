program ExtPascalUML;

uses
  FCGIApp, ExtPascal, ExtPascalUtils, SysUtils, Classes, {$IFDEF MSWINDOWS}Services,{$ENDIF}
  epPrevalence, epUtils, epThread, epModel, epServer, epSupportGDB;

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
  Service := TService.Create(ServerName, ExtPascalVersion + ' - ' + GetEnvironment);
  with Service do try
    if Install then
      writeln('Service installed')
    else if Uninstall then
      writeln('Service uninstalled')
    else begin
      Prevalence  := TPrevalence.Create(ServerName);
      Application := TFCGIApplication.Create(ServerName + ' ' + ExtPascalVersion + ' - ' + GetEnvironment, TepThread, StrToInt(GetIniParameter('Connection', 'Port', '2015')));
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
  Application := TFCGIApplication.Create(ServerName + ' ' + ExtPascalVersion + ' - ' + GetEnvironment, TepThread, StrToInt(GetIniParameter('Connection', 'Port', '2015')));
  Recover;
  Application.Run;
  Snapshot;
{$ENDIF}
end.
