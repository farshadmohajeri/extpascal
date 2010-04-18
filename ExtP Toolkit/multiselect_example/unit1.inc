  Id := 'ExtWindow1';
  Height := 334;
  Width := 408;
  OnEsc := JSFunction('');
  Title := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('MultiSelect Example');
  Layout := lyAbsolute;

  ExtFormLabel2 := TExtFormLabel.Create;
  with ExtFormLabel2.AddTo(Items) do
    begin
    Id := 'ExtWindow1_ExtFormLabel2';
    X := 136;
    Height := 31;
    Y := 256;
    Width := 233;
    Text := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('Nothing selected yet');
    end;

  ExtPanel1 := TExtPanel.Create;
  with ExtPanel1.AddTo(Items) do
    begin
    Id := 'ExtWindow1_ExtPanel1';
    X := 17;
    Height := 222;
    Y := 12;
    Width := 367;
    Layout := lyAbsolute;

    ExtFormLabel1 := TExtFormLabel.Create;
    with ExtFormLabel1.AddTo(Items) do
      begin
      Id := 'ExtWindow1_ExtFormLabel1';
      X := 27;
      Height := 17;
      Y := 15;
      Width := 320;
      Text := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('Select an item from the list and click Go');
      end;

    ExtUxFormMultiSelect1 := TExtUxFormMultiSelect.Create;
    with ExtUxFormMultiSelect1.AddTo(Items) do
      begin
      Id := 'ExtWindow1_ExtUxFormMultiSelect1';
      X := 14;
      Height := 152;
      Y := 42;
      Width := 332;
      StoreArray := JSArray(
       '"Item 1", ' +
       '"Item 2", ' +
       '"Item 3", ' +
       '"Item 4", ' +
       '"Item 5", ' +
       '"Item 6", ' +
       '"Item 7", ' +
       '"Item 8", ' +
       '"Item 9", ' +
       '"Item 10", ' +
       '"Item 11", ' +
       '"Item 12", ' +
       '"Item 13", ' +
       '"Item 14", ' +
       '"Item 15", ' +
       '"Item 16", ' +
       '"Item 17", ' +
       '"Item 18", ' +
       '"Item 19", ' +
       '"Item 20"');
      end;
    end;

  ExtButton1 := TExtButton.Create;
  with ExtButton1.AddTo(Items) do
    begin
    Id := 'ExtWindow1_ExtButton1';
    X := 40;
    Height := 27;
    Y := 252;
    Width := 76;
    Text := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('Go');
    On('click', Ajax(ExtButton1Click));
    TooltipType := 'title';
    end;

