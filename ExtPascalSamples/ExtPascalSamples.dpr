{$A1,B-,C-,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V-,W-,X+,Y+,Z1}
program ExtPascalSamples;

{$IF Defined(MSWINDOWS) and not Defined(WebServer)}
  {$DEFINE DEFAULT}
{$IFEND}

uses
  SysUtils, Classes,
  {$IFNDEF WebServer}FCGIApp,{$ELSE}IdExtHTTPServer,{$ENDIF}
  {$IFDEF DEFAULT}Services,{$ENDIF} ExtPascalUtils, Session;

{$IFDEF DEFAULT}
{$R UAC.res} // For Windows Vista services
type
  TServiceThread = class(TThread)
    procedure Execute; override;
  end;

procedure TServiceThread.Execute; begin
  Application.Run(Self)
end;
{$ENDIF}

const
  ServerName = 'ExtPascalSamples';
  Port       = 2014;

begin
  DateSeparator := '/'; ShortDateFormat := 'd/M/yyyy';
  TimeSeparator := ':'; ShortTimeFormat := 'hh:mm';
  FileMode      := fmShareDenyWrite + fmOpenReadWrite;
  DecimalSeparator := '.';
{$IFDEF DEFAULT}
  Service := TService.Create(ServerName, ExtPascalVersion);
  with Service do try
    if Install then
      writeln('Service installed')
    else if Uninstall then
      writeln('Service uninstalled')
    else begin
      Application := TFCGIApplication.Create(ServerName + ' ' + ExtPascalVersion, TSession, Port);
      Application.Icon := 'ExtPascal.ico';
      if Exists then
        Run([TServiceThread.Create(true)])
      else
        Application.Run;
    end;
  except
    on E : Exception do ReportEventLog(EventError, 1, E.Message);
  end;
{$ELSE}
  {$IFNDEF WebServer}
  Application := TFCGIApplication.Create(ServerName + ' ' + ExtPascalVersion, TSessao, Port);
  {$ELSE}
  Application := TIdExtApplication.Create('ExtPascal Samples ' + ExtPascalVersion, TSamples, 80, 5);
  {$ENDIF}
  Application.Icon := 'ExtPascal.ico';
  Application.Run;
{$ENDIF}
end.
