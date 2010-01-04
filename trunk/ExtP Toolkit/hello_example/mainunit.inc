  Id := 'MainWindow';
  Layout := lyAbsolute;
  Height := 334;
  Width := 408;
  OnEsc := JSFunction('');
  Title := 'Hello';

  ExtButton1 := TExtButton.Create;
  with ExtButton1.AddTo(Items) do
    begin
    Id := 'ExtButton1';
    TooltipType := 'title';
    X := 152;
    Height := 38;
    Y := 192;
    Width := 101;
    Text := 'Click Me';
    On('click', Ajax(CurrentThread.MainWindow_ExtButton1Click));
    end;

