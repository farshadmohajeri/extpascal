  Id := 'GridWindow';
  Height := 434;
  Width := 658;
  OnEsc := JSFunction('');
  Border := False;
  Plain := True;
  Title := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('Grid Example');
  Layout := lyAbsolute;

  EditGrid := TExtGridEditorGridPanel.Create;
  with EditGrid.AddTo(Items) do
    begin
    Id := 'GridWindow_EditGrid';
    X := 24;
    Height := 304;
    Y := 24;
    Width := 602;
    AutoExpandColumn := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('GridWindow_CommonCol');
    ClicksToEdit := 1;
    Frame := True;
    Title := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('Edit Plants?');
    Store := TExtDataStore.Create;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'GridWindow_CommonCol';
      Width := 220;
      end;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'GridWindow_LightCol';
      Width := 130;
      end;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'GridWindow_PriceCol';
      Width := 70;
      end;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'GridWindow_AvailDateCol';
      Width := 95;
      end;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'GridWindow_IndoorCol';
      Width := 55;
      end;
    end;
  with TExtGridColumn(EditGrid.Columns[0]) do
    begin
    CommonCol := TExtFormTextField.Create;
    Editor := CommonCol;
    with CommonCol do
     begin
    DataIndex := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('common');
    Header := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('Common Name');
     end;
    end;
  with TExtGridColumn(EditGrid.Columns[1]) do
    begin
    LightCol := TExtFormComboBox.Create;
    Editor := LightCol;
    with LightCol do
     begin
    DataIndex := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('light');
    Header := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('Light');
    StoreArray := JSArray(
     '"Shade", ' +
     '"Mostly Shady", ' +
     '"Sun or Shade", ' +
     '"Mostly Sunny", ' +
     '"Sunny"');
    TriggerAction := 'all';
     end;
    end;
  with TExtGridColumn(EditGrid.Columns[2]) do
    begin
    PriceCol := TExtFormNumberField.Create;
    Editor := PriceCol;
    with PriceCol do
     begin
    DataIndex := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('price');
    Header := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('Price');
     end;
    end;
  with TExtGridColumn(EditGrid.Columns[3]) do
    begin
    AvailDateCol := TExtFormDateField.Create;
    Editor := AvailDateCol;
    with AvailDateCol do
     begin
    DataIndex := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('availDate');
    Header := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('Available');
     end;
    end;
  with TExtGridColumn(EditGrid.Columns[4]) do
    begin
    IndoorCol := TExtFormCheckbox.Create;
    Editor := IndoorCol;
    with IndoorCol do
     begin
    DataIndex := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('indoor');
    Header := {$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}('Indoor?');
     end;
    end;

