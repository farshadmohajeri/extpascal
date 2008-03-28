program TestExtPascal;

{$APPTYPE CONSOLE}

uses
  Classes, ExtPascal, Ext, ExtGlobal, ExtUtil, ExtAir, ExtData, ExtDD, ExtForm, ExtLayout, ExtMenu, ExtState, ExtTree, ExtGrid;

var
  W : ExtWindow;
  Connection : ExtDataConnection;
begin
  W := ExtWindow.Create;
  with W do begin
    layout := 'fit';
    width := 300;
    height:= 150;
    closable := false;
    resizable := false;
    plain := true;
    items := 'dsdsd'; // Verificar
    show;
  end;
  with ExtButton.Create do begin
    renderTo := 'button1-div';
    text := 'Button 1';
    handler := _Function.JSFunction('', 'alert("You clicked the button 1");');
    WriteBrowser;
  end;
  Connection := ExtDataConnection.Create;
  with Connection do begin
    url := 'docs/samples/data.txt';
    method := 'GET';
  end;
  with ExtGridGridPanel.Create do begin
    renderTo := 'grid-div';
    title := 'my Grid';
    width := 300;
    height := 200;
    frame := true;
    store := ExtDataStore.Create;
    with Store do begin
      autoLoad := true;
      proxy := ExtDataHttpProxy.Create(Connection);
      reader := ExtDataJsonReader.Create;
      with ExtDataJsonReader(reader) do begin
        root := 'rows';
        totalProperty := 'totalCount';
      end;
    end;
    SetLengthColumns(3);
    with columns[0] do begin header := ''; width := 1212; sortable := true; dataIndex := 'status'; id := dataIndex; end;
    with columns[1] do begin header := ''; width := 1212; sortable := true; dataIndex := 'status'; id := dataIndex; end;
    with columns[2] do begin header := ''; width := 1212; sortable := true; dataIndex := 'status'; id := dataIndex; end;
    WriteBrowser
  end;
end.

