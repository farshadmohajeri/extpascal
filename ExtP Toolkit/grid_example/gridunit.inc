  Id := 'GridWindow';
  Height := 434;
  Width := 658;
  OnEsc := JSFunction('');
  Border := False;
  Plain := True;
  Title := 'Grid Example';
  Layout := lyAbsolute;

  EditGrid := TExtGridEditorGridPanel.Create;
  with EditGrid.AddTo(Items) do
    begin
    Id := 'EditGrid';
    X := 24;
    Height := 304;
    Y := 24;
    Width := 602;
    AutoExpandColumn := 'CommonCol';
    ClicksToEdit := 1;
    Frame := True;
    Title := 'Edit Plants?';
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'CommonCol';
      Width := 220;
      end;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'LightCol';
      Width := 130;
      end;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'PriceCol';
      Width := 70;
      end;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'AvailDateCol';
      Width := 95;
      end;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'IndoorCol';
      Width := 55;
      end;
    end;
  with TExtGridColumn(EditGrid.Columns[0]) do
    begin
    CommonCol := TExtFormTextField.Create;
    Editor := CommonCol;
    with CommonCol do
     begin
    DataIndex := 'common';
    Header := 'Common Name';
     end;
    end;
  with TExtGridColumn(EditGrid.Columns[1]) do
    begin
    LightCol := TExtFormComboBox.Create;
    Editor := LightCol;
    with LightCol do
     begin
    DataIndex := 'light';
    Header := 'Light';
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
    DataIndex := 'price';
    Header := 'Price';
     end;
    end;
  with TExtGridColumn(EditGrid.Columns[3]) do
    begin
    AvailDateCol := TExtFormDateField.Create;
    Editor := AvailDateCol;
    with AvailDateCol do
     begin
    DataIndex := 'availDate';
    Header := 'Available';
     end;
    end;
  with TExtGridColumn(EditGrid.Columns[4]) do
    begin
    IndoorCol := TExtFormCheckbox.Create;
    Editor := IndoorCol;
    with IndoorCol do
     begin
    DataIndex := 'indoor';
    Header := 'Indoor?';
     end;
    end;

