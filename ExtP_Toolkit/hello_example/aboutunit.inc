  Id := 'AboutWindow';
  Height := 287;
  Width := 358;
  Modal := True;
  Title := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('About');
  Layout := lyAbsolute;

  ExtButton1 := TExtButton.Create;
  with ExtButton1.AddTo(Items) do
    begin
    Id := 'AboutWindow_ExtButton1';
    X := 128;
    Height := 33;
    Y := 168;
    Width := 103;
    Text := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('Close');
    On('click', Ajax(ExtButton1Click));
    TooltipType := 'title';
    end;

