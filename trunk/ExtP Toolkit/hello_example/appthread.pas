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
    procedure MainWindow_ExtButton1Click;
    procedure AboutWindow_ExtButton1Click;
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


procedure TAppThread.MainWindow_ExtButton1Click;
begin
  MainWindow.ExtButton1Click;
end;


procedure TAppThread.AboutWindow_ExtButton1Click;
begin
  AboutWindow.ExtButton1Click;
end;


end.
