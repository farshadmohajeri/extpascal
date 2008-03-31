program TestExtPascal;

{$APPTYPE CONSOLE}

uses
  Classes, ExtPascal, Ext, ExtGlobal, ExtUtil, ExtAir, ExtData, ExtDD, ExtForm, ExtLayout, ExtMenu, ExtState, ExtTree, ExtGrid;

var
  W : ExtWindow;
  Connection : ExtDataConnection;
  DataRecord : ExtDataRecord;
  Fields : ArrayOfExtDataField;
begin
  W := ExtWindow.Create;
  with W do begin
    Layout := 'fit';
    Width := 300;
    Height:= 150;
    Closable := false;
    Resizable := false;
    Plain := true;
    Items := 'dsdsd'; // Verificar
    Show;
  end;
  with ExtButton.Create do begin
    RenderTo := 'button1-div';
    Text := 'Button 1';
    Handler := _Function.JSFunction('', 'alert("You clicked the button 1");');
    WriteBrowser;
  end;
  Connection := ExtDataConnection.Create;
  with Connection do begin
    Url := 'docs/samples/data.txt';
    Method := 'GET';
  end;
  SetLength(Fields, 3);
  Fields[0].Create.Name := 'status';
  Fields[1].Create.Name := 'report';
  Fields[2].Create.Name := 'duration';
  DataRecord := ExtDataRecord.Create(Fields);
  with ExtGridGridPanel.Create do begin
    RenderTo := 'grid-div';
    Title := 'my Grid';
    Width := 300;
    Height := 200;
    Frame := true;
    Store := ExtDataStore.Create;
    with Store do begin
      AutoLoad := true;
      Proxy := ExtDataHttpProxy.Create(Connection);
      Reader := ExtDataJsonReader.Create(nil, DataRecord);
      with ExtDataJsonReader(Reader) do begin
        Root := 'rows';
        TotalProperty := 'totalCount';
      end;
    end;
    SetLengthColumns(3);
    with Columns[0] do begin Header := '';         Width := 30;  Sortable := true; DataIndex := 'status';   Id := DataIndex; end;
    with Columns[1] do begin Header := 'Name';     Width := 160; Sortable := true; DataIndex := 'report';   Id := DataIndex; end;
    with Columns[2] do begin Header := 'Duration'; Width := 70;  Sortable := true; DataIndex := 'duration'; Id := DataIndex; end;
    WriteBrowser;
    Free;
  end;
  with ExtFormFormPanel.Create do begin
    LabelWidth := 80;
    Frame := true;
    Title := 'Login';
    Width := 230;
    DefaultType := 'textfield';
		MonitorValid := true;
    SetLengthButtons(1);
    Buttons[0].Text := 'Login';
  end;
end.

