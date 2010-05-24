{$A1,B-,C-,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V-,W-,X+,Y+,Z1}
program Blaise;

{$IF Defined(MSWINDOWS) and not Defined(WebServer)}
  {$DEFINE DEFAULT}
{$IFEND}

uses
  {$IFNDEF WebServer}FCGIApp,{$ELSE}IdExtHTTPServer,{$ENDIF}
  {$IFDEF DEFAULT}Services,{$ENDIF}
  SysUtils, Classes, Session;

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

begin
{$IFDEF DEFAULT}
  Service := TService.Create(ServerName);
  with Service do try
    if Install then
      writeln('Service installed')
    else if Uninstall then
      writeln('Service uninstalled')
    else begin
      Application := CreateWebApplication(ServerName, TSession);
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
  Application := CreateWebApplication(ServerName, TSession);
  Application.Icon := 'ExtPascal.ico';
  Application.Run;
{$ENDIF}
end.
