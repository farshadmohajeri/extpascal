  Id := 'AboutWindow';
  Layout := lyAbsolute;
  Height := 287;
  Width := 358;
  Modal := True;
  Title := 'About';

  with TExtPanel.AddTo(Items) do
    begin
    Layout := lyAbsolute;
    Border := False;
    BodyStyle := 'background:transparent';
    X := 128;
    Height := 33;
    Y := 168;
    Width := 103;
    ExtButton1 := TExtButton.Create;
    with ExtButton1.AddTo(Items) do
      begin
      Id := 'ExtButton1';
      MinWidth := 103;
      Text := 'Close';
      On('click', Ajax(CurrentThread.AboutWindow_ExtButton1Click));
      end;
    end;

