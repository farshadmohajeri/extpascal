  Id := 'EditWindow';
  Height := 554;
  Width := 631;
  OnEsc := JSFunction('');
  Maximizable := True;
  Title := 'Fish Facts';
  Layout := lyAbsolute;

  EditGrid := TExtGridEditorGridPanel.Create;
  with EditGrid.AddTo(Items) do
    begin
    Id := 'EditGrid';
    X := 8;
    Height := 318;
    Y := 168;
    Width := 600;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'SpeciesNoCol';
      Width := 70;
      end;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'CategoryCol';
      Width := 105;
      end;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'CommonNameCol';
      Width := 210;
      end;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'SpeciesNameCol';
      Width := 280;
      end;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'LengthCmCol';
      Width := 70;
      end;
    with TExtGridColumn.AddTo(Columns) do
      begin
      Id := 'LengthInCol';
      Width := 70;
      end;
    end;

  NotesMemo := TExtFormTextArea.Create;
  with NotesMemo.AddTo(Items) do
    begin
    Id := 'NotesMemo';
    X := 272;
    Height := 150;
    Y := 8;
    Width := 336;
    end;

  PictureLabel := TExtFormLabel.Create;
  with PictureLabel.AddTo(Items) do
    begin
    Id := 'PictureLabel';
    X := 8;
    Height := 150;
    Y := 8;
    Width := 250;
    end;
  with TExtGridColumn(EditGrid.Columns[0]) do
    begin
    SpeciesNoCol := TExtFormNumberField.Create;
    Editor := SpeciesNoCol;
    with SpeciesNoCol do
     begin
    DataIndex := 'Species_No';
    Header := 'Species No';
     end;
    end;
  with TExtGridColumn(EditGrid.Columns[1]) do
    begin
    CategoryCol := TExtFormTextField.Create;
    Editor := CategoryCol;
    with CategoryCol do
     begin
    DataIndex := 'Category';
    Header := 'Category';
     end;
    end;
  with TExtGridColumn(EditGrid.Columns[2]) do
    begin
    CommonNameCol := TExtFormTextField.Create;
    Editor := CommonNameCol;
    with CommonNameCol do
     begin
    DataIndex := 'Common_Name';
    Header := 'Common Name';
     end;
    end;
  with TExtGridColumn(EditGrid.Columns[3]) do
    begin
    SpeciesNameCol := TExtFormTextField.Create;
    Editor := SpeciesNameCol;
    with SpeciesNameCol do
     begin
    DataIndex := 'Species_Name';
    Header := 'Species Name';
     end;
    end;
  with TExtGridColumn(EditGrid.Columns[4]) do
    begin
    LengthCmCol := TExtFormNumberField.Create;
    Editor := LengthCmCol;
    with LengthCmCol do
     begin
    DataIndex := 'Length_Cm';
    Header := 'Length (cm)';
     end;
    end;
  with TExtGridColumn(EditGrid.Columns[5]) do
    begin
    LengthInCol := TExtFormNumberField.Create;
    Editor := LengthInCol;
    with LengthInCol do
     begin
    DataIndex := 'Length_In';
    Header := 'Length (in)';
     end;
    end;

