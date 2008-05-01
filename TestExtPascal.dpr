program TestExtPascal;

{$APPTYPE CONSOLE}

uses
  FCGIApp, SysUtils, strutils, ExtPascal, Ext, ExtGlobal, ExtData, ExtForm, ExtGrid;
  //ExtUtil, ExtAir, ExtDD, ExtLayout, ExtMenu, ExtState, ExtTree;

type
  TTestExtPascal = class(TExtThread)
  public
    I : integer;
    Connection : ExtDataConnection;
    DataRecord : ExtDataRecord;
    Fields : ArrayOfExtDataField;
    Login : ExtFormFormPanel;
    TabActions, ActionPanel : ExtPanel;
    TabPanel : ExtTabPanel;
  published
    procedure Home; override;
    procedure Animal;
  end;

procedure TTestExtPascal.Animal; begin
  Response := IntToStr(int64(Self)) + ': ' + Query['color'] + '; ' + Query['type'] + '; ' + IntToStr(I);
  inc(I);
end;

procedure TTestExtPascal.Home; begin
  inherited;
  with ExtButton.Create do begin
    RenderTo := 'content';
    Handler := _Function.JSFunction('', 'alert("You clicked the button 1");');
    Text := 'Button 1';
  end;

  Login := ExtFormFormPanel.Create;
  with Login do begin
    LabelWidth := 80;
    Frame := true;
    Title := 'Login';
    Width := 230;
    DefaultType := 'textfield';
    MonitorValid := true;
    SetLengthButtons(1);
    Buttons[0].Text := 'Login';
    SetLengthItems(2, ExtFormTextField);
    with ExtFormTextField(Items[0]) do begin FieldLabel := 'UserName'; Name := 'loginUserName'; AllowBlank := false end;
    with ExtFormTextField(Items[1]) do begin FieldLabel := 'Password'; Name := 'loginPassword'; AllowBlank := false; InputType := 'password' end;
  end;
  with ExtWindow.Create do begin
    Title := JSName;
    Layout := 'fit';
    Width := 300;
    Height:= 150;
    Closable := false;
    Resizable := false;
    Plain := true;
    SetLengthItems(1, NoCreate);
    Items[0] := Login;
    Show;
  end;

  Connection := ExtDataConnection.Create;
  with Connection do begin
    Url := 'docs/samples/data.txt';
    Method := 'GET';
  end;
  SetLength(Fields, 3, ExtDataField);
  Fields[0].Name := 'status';
  Fields[1].Name := 'report';
  Fields[2].Name := 'duration';
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
    Free;
  end;

  TabActions := ExtPanel.Create;
  with TabActions do begin
    Frame := true;
    Title := 'Actions';
    Collapsible:= true;
    ContentEl := 'actions';
    TitleCollapse := true;
  end;
  ActionPanel := ExtPanel.Create;
  with ActionPanel do begin
    Id := 'action-panel';
    Collapsible := true;
    Width := 340;
    Border := false;
    BaseCls := 'x-plain';
    SetLengthItems(1, NoCreate);
    Items[0] := TabActions;
    Region := 'west';
    Split := true;
    CollapseMode := 'mini';
    MinWidth := 150;
  end;
  TabPanel := ExtTabPanel.Create;
  with TabPanel do begin
    DeferredRender := false;
    AutoScroll := true;
    ActiveTab := 'tab1'; // variant
    Region := 'center';
    Title := 'Main';
    Closable := false;
    Margins := '0 4 4 0';
    SetLengthItems(2);
    with Items[0] do begin Id := 'tab1'; ContentEl := 'tabs'; Title := 'Button';     Closable := false; AutoScroll := true end;
    with Items[1] do begin Id := 'tab2'; ContentEl := 'tabs'; Title := 'Grid Panel'; Closable := false; AutoScroll := true end;
  end;
  with ExtViewPort.Create do begin
    Layout := 'border';
    SetLengthItems(2, NoCreate);
    Items[0] := ActionPanel;
    Items[1] := TabPanel;
  end;
  with TabPanel do begin
    Title := 'New Tab';
    IconCls := 'tabs';
    AutoLoad := ExtFormAction.Create;
    ExtFormAction(AutoLoad).Url := 'TestExtPascal.exe';
    Closable := true;
  end;
end;

begin
  Application := TFCGIApplication.Create('Test ExtPascal 1.0', TTestExtPascal);
  Application.Run;
end.
