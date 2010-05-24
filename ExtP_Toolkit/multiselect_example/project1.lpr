program project1;

uses
{$IFNDEF WebServer}
  FCGIApp,
{$ELSE}
 {$IFNDEF MSWINDOWS}
  CThreads,
 {$ENDIF}
  IdExtHTTPServer,
{$ENDIF}
  AppThread, Unit1;

{$IFNDEF FPC}
 {$IFNDEF WebServer}
  {$APPTYPE CONSOLE}
 {$ENDIF}
{$ENDIF}

const
  Port = 2014;
  MaxIdleMinutes = 5;


begin
{$IFNDEF WebServer}
  Application := TFCGIApplication.Create('MyApp', TAppThread, Port, MaxIdleMinutes);
{$ELSE}
  Application := TIdExtApplication.Create('MyApp', TAppThread, 80, MaxIdleMinutes);
{$ENDIF}
  Application.Run;
end.

