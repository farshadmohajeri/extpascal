unit AppThread;

interface

uses
  ExtPascal,
  mainunit,
  aboutunit;

type
  TAppThread = class(TExtThread)
  public
    MainWindow : TMainWindow;
    AboutWindow : TAboutWindow;
  published
    procedure Home; override;
  end;

function CurrentThread : TAppThread;


implementation

uses
{$IFNDEF WebServer}
  FCGIApp;
{$ELSE}
  IdExtHTTPServer;
{$ENDIF}

function CurrentThread : TAppThread;
begin
  Result := TAppThread(CurrentFCGIThread);
end;


procedure TAppThread.Home;
begin
  MainWindow := TMainWindow.Create;
  MainWindow.Show;
end;


end.
