unit AppThread;

interface

uses
  ExtPascal,
  unit1;

type
  TAppThread = class(TExtThread)
  public
    ExtWindow1 : TExtWindow1;
  published
    procedure Home; override;
    procedure ExtWindow1_ExtButton1Click;
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
  SetLibrary(ExtPath + '/examples/ux/ux-all', True, True);
  ExtWindow1 := TExtWindow1.Create;
  ExtWindow1.Show;
end;


procedure TAppThread.ExtWindow1_ExtButton1Click;
begin
  ExtWindow1.ExtButton1Click;
end;


end.
