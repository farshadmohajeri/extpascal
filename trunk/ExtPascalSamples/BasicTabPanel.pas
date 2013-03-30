unit BasicTabPanel;

interface

uses
  Ext;

type
  TBasicTabPanel = class(TExtWindow)
    constructor Create;
  end;

implementation

uses
  ExtPascalUtils, Session;

constructor TBasicTabPanel.Create; begin
  inherited;
  SelfSession.SetCodePress;
  Title  := 'Hello Dialog';
  Layout := laFit;
  Plain  := true;
  Width  := 500;
  Height := 300;
  CloseAction := 'hide';
  with TExtTabPanel.AddTo(Items) do begin
    ActiveTab := 0;
    with TExtPanel.AddTo(Items) do begin
      Title := 'Hello World 1';
      Html  := 'Hello...';
    end;
    with TExtPanel.AddTo(Items) do begin
      Title := 'Hello World 2';
      Html  := '...World';
    end;
  end;
  with TExtButton.AddTo(Buttons) do begin
    Text     := 'Submit';
    Disabled := true;
  end;
  with TExtButton.AddTo(Buttons) do begin
    Text    := 'Close';
    Handler := JSFunction('window.close()');// try this: Handler := Window.Close; for another effect.
  end;
  SelfSession.AddShowSourceButton(Buttons, 'BasicTabPanel');
end;

end.
