unit gridunit; 

interface

uses
  SysUtils, Classes, 
{$IFDEF UseRuntime}
  Ext, ExtPascal, ExtPascalUtils, ExtForm, 
  ExtData, ExtGrid, ExtUtil, ExtAir, ExtDd, 
  ExtLayout, ExtMenu, ExtState, ExtTree; 

type
  {$M+}
  TExtPanel_Tab = TExtPanel; 
  TExtFormTextField_Grid = TExtFormTextField; 
  TExtFormNumberField_Grid = TExtFormNumberField; 
  TExtFormDateField_Grid = TExtFormDateField; 
  TExtFormTimeField_Grid = TExtFormTimeField; 
  TExtFormCheckbox_Grid = TExtFormCheckbox; 
  TExtFormComboBox_Grid = TExtFormComboBox; 
  {$M-}

{$ELSE}
  ExtP_Design_Ctrls, ExtP_Design_Grid;
{$ENDIF}

type
  TGridWindow = class(TExtWindow)
    EditGrid: TExtGridEditorGridPanel;
    CommonCol: TExtFormTextField_Grid;
    LightCol: TExtFormComboBox_Grid;
    PriceCol: TExtFormNumberField_Grid;
    AvailDateCol: TExtFormDateField_Grid;
    IndoorCol: TExtFormCheckbox_Grid;
  private
  public
    DataStore : TExtDataStore;
    Plant : TExtDataRecord;
    constructor Create; 
    procedure Show; 
  end; 

implementation

uses
  AppThread; 

constructor TGridWindow.Create;
var
  Data : TExtObjectList;
begin

   {Note: Code and comments below are taken directly from ExtPascalSamples.dpr.

    Since Store is passed in grid's JavaScript constructor, need to create
     Store before creating grid.}

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

    URL := CurrentThread.ExtPath + '/examples/grid/plants.xml';

    // the return will be XML, so lets set up a reader, records will have a "plant" tag

    Reader := TExtDataXmlReader.Create(JSObject('record:"plant"'), Plant);

    SortInfo := JSObject('field:"common", direction:"ASC"');
  end;


  inherited; 
{$IFDEF UseRuntime}
 {$I gridunit.inc}
{$ENDIF}

  TExtGridColumn(EditGrid.Columns[4]).RendererExtFunction :=  //IndoorCol
   JSFunction('v', 'return "<div class=''x-grid3-check-col"+(v?"-on":"")+"''></div>";');

  EditGrid.Store := DataStore;
  DataStore.Load(nil);

end; 

procedure TGridWindow.Show;
begin
  inherited Show; 
end; 


end.

