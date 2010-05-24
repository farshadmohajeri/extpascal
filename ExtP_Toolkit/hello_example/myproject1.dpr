program myproject1;

uses
  FCGIApp, AppThread, mainunit, aboutunit;

{$IFNDEF FPC}
 {$APPTYPE CONSOLE}
{$ENDIF}

const
  Port = 2014;
  MaxIdleMinutes = 5;


begin
  Application := TFCGIApplication.Create('MyApp', TAppThread, Port, MaxIdleMinutes);
  Application.Run;
end.

