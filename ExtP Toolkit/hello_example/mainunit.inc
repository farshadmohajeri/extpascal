  Id := 'MainWindow';
  Layout := 'absolute';
  Border := False;
  Plain := True;
  Height := 334;
  Width := 408;
  OnEsc := ExtGlobal.Ext.EmptyFn;
  Title := 'Hello';

  with TExtPanel.AddTo(Items) do
    begin
    Layout := 'absolute';
    Border := False;
    BodyStyle := 'background:transparent';
    X := 152;
    Height := 38;
    Y := 192;
    Width := 101;
    ExtButton1 := TExtButton.Create;
    with ExtButton1.AddTo(Items) do
      begin
      Id := 'ExtButton1';
      MinWidth := 101;
      Text := 'Click Me';
      On('click', Ajax(CurrentThread.MainWindow_ExtButton1Click));
      end;
    end;

