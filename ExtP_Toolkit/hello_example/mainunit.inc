  Id := 'MainWindow';
  Height := 334;
  Width := 408;
  OnEsc := JSFunction('');
  Title := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('Hello');
  Layout := lyAbsolute;

  ExtButton1 := TExtButton.Create;
  with ExtButton1.AddTo(Items) do
    begin
    Id := 'MainWindow_ExtButton1';
    X := 152;
    Height := 38;
    Y := 192;
    Width := 101;
    Text := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('Click Me');
    On('click', Ajax(ExtButton1Click));
    TooltipType := 'title';
    end;

