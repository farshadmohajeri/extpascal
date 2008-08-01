//Disabling CGI Directive ExtPascal Can Be used within an embedded Indy based WebServer
{$DEFINE CGI}
{$IFDEF CGI}
  {$IFDEF MSWINDOWS}{$APPTYPE CONSOLE}{$DEFINE SERVICE}{$ENDIF}
{$ENDIF}
