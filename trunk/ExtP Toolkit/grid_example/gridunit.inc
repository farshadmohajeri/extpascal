  Id := 'GridWindow';
  Layout := lyAbsolute;
  Height := 434;
  Width := 658;
  OnEsc := JSFunction('');
  Border := False;
  Plain := True;
  Title := 'Grid Example';

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
  with TExtGridColumn(EditGrid.Columns[0]) do  //CommonCol
    begin
    Editor := TExtFormTextField.Create;
    DataIndex := 'common';
    Header := 'Common Name';
    end;
  with TExtGridColumn(EditGrid.Columns[1]) do  //LightCol
    begin
    Editor := TExtFormComboBox.Create;
    DataIndex := 'light';
    Header := 'Light';
    end;
  with TExtGridColumn(EditGrid.Columns[2]) do  //PriceCol
    begin
    Editor := TExtFormNumberField.Create;
    DataIndex := 'price';
    Header := 'Price';
    end;
  with TExtGridColumn(EditGrid.Columns[3]) do  //AvailDateCol
    begin
    Editor := TExtFormDateField.Create;
    DataIndex := 'availDate';
    Header := 'Available';
    end;
  with TExtGridColumn(EditGrid.Columns[4]) do  //IndoorCol
    begin
    Editor := TExtFormCheckbox.Create;
    DataIndex := 'indoor';
    Header := 'Indoor?';
    end;

