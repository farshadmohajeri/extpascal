program fishfacts;

uses
  {$IFNDEF WebServer}FCGIApp,{$ELSE}IdExtHTTPServer,{$ENDIF}
  AppThread, mainform;

begin
  Application := CreateWebApplication('Fish Facts Demo', TAppThread);
  Application.Icon := 'ExtPascal.ico';
  Application.Run;
end.

