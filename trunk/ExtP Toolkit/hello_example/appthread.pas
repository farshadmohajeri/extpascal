unit AppThread;

interface

uses
  ExtPascal;

type
  TAppThread = class(TExtThread)
  published
    procedure Home; override;
    procedure MainWindow_ExtButton1Click;
    procedure AboutWindow_ExtButton1Click;
  end;

function CurrentThread : TAppThread;

implementation

uses
  FCGIApp,
  mainunit,
  aboutunit;

function CurrentThread : TAppThread;
begin
  Result := TAppThread(CurrentFCGIThread);
end;

procedure TAppThread.Home;
begin
  if MainWindow = nil then
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
