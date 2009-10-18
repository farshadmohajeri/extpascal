  Id := 'ExtWindow1';
  Layout := lyAbsolute;
  Height := 334;
  Width := 408;
  OnEsc := JSFunction('');
  Title := 'MultiSelect Example';

  ExtFormLabel2 := TExtFormLabel.Create;
  with ExtFormLabel2.AddTo(Items) do
    begin
    Id := 'ExtFormLabel2';
    X := 136;
    Height := 31;
    Y := 256;
    Width := 233;
    Text := 'Nothing selected yet';
    end;

  ExtPanel1 := TExtPanel.Create;
  with ExtPanel1.AddTo(Items) do
    begin
    Id := 'ExtPanel1';
    Layout := lyAbsolute;
    X := 17;
    Height := 222;
    Y := 12;
    Width := 367;

    ExtFormLabel1 := TExtFormLabel.Create;
    with ExtFormLabel1.AddTo(Items) do
      begin
      Id := 'ExtFormLabel1';
      X := 27;
      Height := 17;
      Y := 15;
      Width := 320;
      Text := 'Select an item from the list and click Go';
      end;

    ExtUxFormMultiSelect1 := TExtUxFormMultiSelect.Create;
    with ExtUxFormMultiSelect1.AddTo(Items) do
      begin
      Id := 'ExtUxFormMultiSelect1';
      StoreArray := JSArray('');
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

  with TExtPanel.AddTo(Items) do
    begin
    Layout := lyAbsolute;
    Border := False;
    BodyStyle := 'background:transparent';
    X := 40;
    Height := 27;
    Y := 252;
    Width := 76;
    ExtButton1 := TExtButton.Create;
    with ExtButton1.AddTo(Items) do
      begin
      Id := 'ExtButton1';
      MinWidth := 76;
      Text := 'Go';
      On('click', Ajax(CurrentThread.ExtWindow1_ExtButton1Click));
      end;
    end;

