unit AppThread;

interface

uses
  ExtPascal,
  mainform;

type
  TAppThread = class(TExtThread)
  public
    EditWindow : TEditWindow;
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
  Result := TAppThread(CurrentWebSession);
end;

procedure TAppThread.Home;
begin
  EditWindow := TEditWindow.Create;
  EditWindow.Show;
end;

end.
