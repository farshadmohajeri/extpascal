program TestExtPascal;

{$APPTYPE CONSOLE}

uses
  FCGIApp, ExtPascal, Ext, ExtGlobal, ExtData, ExtForm, ExtGrid;
  //ExtUtil, ExtAir, ExtDD, ExtLayout, ExtMenu, ExtState, ExtTree;

type
  TTestExtPascal = class(TExtThread)
    Janela : ExtWindow;
    procedure Home; override;
  end;

{ TTestExtPascal }

procedure TTestExtPascal.Home; begin
  Janela := ExtWindow.Create;
  with Janela do begin
    Title := 'Hello Dialog';
    Layout := 'fit';
    Width := 500;
    Height := 300;
    CloseAction := 'hide';
    Plain := true;
    with ExtTabPanel.AddTo(Items) do begin
      ActiveTab := 'tab1';
      with ExtPanel.AddTo(Items) do begin
        Id := 'tab1';
        Title := 'Hello World 1';
        Html := 'Hello...';
      end;
      with ExtPanel.AddTo(Items) do begin
        Title := 'Hello World 2';
        Html := '...World';
      end;
    end;
    with ExtButton.AddTo(Buttons) do begin
      Text := 'Submit';
      Disabled := true;
    end;
    with ExtButton.AddTo(Buttons) do begin
      Text := 'Close';
      Handler := _Function.JSFunction('', Janela.JSName + '.hide()');
    end;
    Show;
  end;
end;

begin
  Application := TFCGIApplication.Create('Test ExtPascal 0.7.1', TTestExtPascal);
  Application.Run;
end.
