unit AppThread;

interface

uses
  ExtPascal,
  gridunit;

type
  TAppThread = class(TExtThread)
  public
    GridWindow : TGridWindow;
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
  GridWindow := TGridWindow.Create;
  GridWindow.Show;
end;


end.
