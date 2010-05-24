unit Session;

interface
{
AmyEditor

1. Folding
2. Bookmark
3. LastChanges
4. Split views
5. Word Completion
6. Themes
7. Indentation
8. Collaboration
9. Templates
10. Bundles
}
uses
  ExtPascal, IDE;

const
  ServerName = 'Blaise 2010';
  Build = ' - Server on ' + {$IFNDEF FPC}'Windows - i386 - compiled by Delphi'{$ELSE}
    {$I %FPCTARGETOS%} + ' - ' + {$I %FPCTARGETCPU%} + ' - compiled by FreePascal ' + {$I %FPCVersion%} + '(' + {$I %FPCDATE%} + ')' {$ENDIF};

type
  TSession = class(TExtThread)
  private
    IDE : TIDE;
  published
    procedure Home; override;
  end;

function SelfSession : TSession;

implementation

uses
  {$IFNDEF WebServer}FCGIApp;{$ELSE}IdExtHTTPServer;{$ENDIF}

function SelfSession : TSession; begin
  Result := TSession(CurrentWebSession);
end;

procedure TSession.Home; begin
  SetLibrary('/codepress/Ext.ux.CodePress');
  Theme := 'gray';
  if IDE = nil then
    IDE := TIDE.Create
  else
    IDE.Create;
  IDE.Show;
end;

end.
