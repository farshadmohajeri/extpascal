unit EditableGrid;

interface

uses
  Ext, ExtGrid, ExtData;

type
  TEditableGrid = class(TExtGridEditorGridPanel)
  private
    Plant : TExtDataRecord;
    DataStore : TExtDataStore;
    Grid : TExtGridEditorGridPanel;
  public
    constructor Create;
  published
    procedure AddPlant;
  end;

implementation

uses
  Session, ExtPascal, ExtForm, ExtUtil;

constructor TEditableGrid.Create;
var
  Data : TExtObjectList;
begin
  inherited;
  SelfSession.SetCodePress;
  //  if Grid <> nil then Grid.Free;
  ExtQuickTips.Init(true);
  Data := TExtObjectList.Create;
  // the "name" below matches the tag name to read, except "availDate", which is mapped to the tag "availability"
  with TExtDataField.AddTo(Data) do begin Name := 'common';    TypeJS := 'string' end;
  with TExtDataField.AddTo(Data) do begin Name := 'botanical'; TypeJS := 'string' end;
  TExtDataField.AddTo(Data).Name := 'light';
  with TExtDataField.AddTo(Data) do begin Name := 'price'; TypeJS := 'float' end; // automatic date conversions
  with TExtDataField.AddTo(Data) do begin Name := 'availDate'; Mapping := 'availability'; TypeJS := 'date'; DateFormat := 'm/d/Y' end;
  with TExtDataField.AddTo(Data) do begin Name := 'indoor'; TypeJS := 'bool' end;
  // this could be inline, but we want to define the Plant record, type so we can add records dynamically
  Plant := TExtDataRecord.Create(Data);
  // create the Data Store
  DataStore := TExtDataStore.Create;
  with DataStore do begin
    URL := '/ext/examples/grid/plants.xml'; // Don´t use CacheFly here!
    // the return will be XML, so lets set up a reader, records will have a "plant" tag
    Reader := TExtDataXmlReader.Create(JSObject('record:"plant"'), Plant);
    SortInfo := JSObject('field:"common", direction:"ASC"');
  end;
  // create the editor grid
  Grid := TExtGridEditorGridPanel.Create;
  with Grid do begin
    Store  := DataStore;
    Width  := 600;
    Height := 300;
    Title  := 'Edit Plants?';
    Frame  := true;
    RenderTo := 'body';
    ClicksToEdit := 1;
    AutoExpandColumn := 'common';
    with TExtGridColumn.AddTo(Columns) do begin
      Id := 'common';
      Header := 'Common Name';
      Width  := 220;
      DataIndex := 'common';
      Editor := TExtFormTextField.Create;
      TExtFormTextField(Editor).AllowBlank := false;
    end;
    with TExtGridColumn.AddTo(Columns) do begin
      Header := 'Light';
      Width  := 130;
      DataIndex := 'light';
      Editor := TExtFormComboBox.Create;
      with TExtFormComboBox(Editor) do begin
        StoreArray := JSArray('"Shade", "Mostly Shady", "Sun or Shade", "Mostly Sunny", "Sunny"');
        TypeAhead  := true;
        ListClass  := 'x-combo-list-small';
        TriggerAction := 'all';
      end;
    end;
    with TExtGridColumn.AddTo(Columns) do begin
      Header := 'Price';
      Width  := 70;
      Align  := alRight;
      DataIndex := 'price';
      Renderer := 'usMoney';
      Editor := TExtFormNumberField.Create;
      with TExtFormNumberField(Editor) do begin
        MaxValue      := 100000;
        AllowBlank    := false;
        AllowNegative := false;
      end;
    end;
    with TExtGridColumn.AddTo(Columns) do begin
      Header    := 'Available';
      Width     := 95;
      DataIndex := 'availDate';
      RendererExtFunction := ExtUtilFormat.Date('%0', 'm/d/Y'); // %0..%9 get event parameters
      Editor    := TExtFormDateField.Create;
      with TExtFormDateField(Editor) do begin
        Format           := 'm/d/Y';
        MinValueString   := '01/01/2006';
        DisabledDays     := JSArray('0, 6');
        DisabledDaysText := 'Plants are not available on the weekends'
      end;
    end;
    with TExtGridColumn.AddTo(Columns) do begin
      Header    := 'Indoor?';
      DataIndex := 'indoor';
      Width     := 55;
      Editor    := TExtFormCheckbox.Create;
      RendererExtFunction := JSFunction('v', 'return "<div class=''x-grid3-check-col"+(v?"-on":"")+"''></div>";');
    end;
    with TExtButton.AddTo(TBarArray) do begin
      Text    := 'Add Plant using AJAX!';
      Handler := Ajax(AddPlant);
    end;
    with TExtButton.AddTo(TBarArray) do begin
      Text    := 'Logout';
      Handler := Ajax(SelfSession.Logout);
    end;
    SelfSession.AddShowSourceButton(Buttons, 'EditableGrid');
  end;
  DataStore.Load(nil);
end;

procedure TEditableGrid.AddPlant; begin 
  Grid.StopEditing;
  with DataStore do
    Insert(0, JSArray('new ' + Plant.JSName + '({common:"New Plant 1",light:"Mostly Shade",price:0,availDate:(new Date()).clearTime(),indoor:false})'));
  Grid.StartEditing(0, 0);
end;

end.
