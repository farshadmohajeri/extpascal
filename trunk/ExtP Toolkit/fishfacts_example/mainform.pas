unit mainform;

interface

uses
  SysUtils, Classes, 
{$IFDEF UseRuntime}
  Ext, ExtPascal, ExtPascalUtils, ExtForm, 
  ExtData, ExtGrid, ExtUtil, ExtAir, ExtDd, 
  ExtLayout, ExtMenu, ExtDirect, ExtState, ExtTree, 
  ExtUxForm; 

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
  TEditWindow = class(TExtWindow)
    EditGrid: TExtGridEditorGridPanel;
    SpeciesNoCol: TExtFormNumberField_Grid;
    CategoryCol: TExtFormTextField_Grid;
    CommonNameCol: TExtFormTextField_Grid;
    SpeciesNameCol: TExtFormTextField_Grid;
    LengthCmCol: TExtFormNumberField_Grid;
    LengthInCol: TExtFormNumberField_Grid;
    NotesMemo: TExtFormTextArea;
    PictureLabel: TExtFormLabel;
  private
  public
    DataDir : string;
    DataStore : TExtDataStore;
    DataRecord : TExtDataRecord;
    RowSelect : TExtGridRowSelectionModel;
    constructor Create;
    procedure Show;
    procedure RowSelectOnRowselect(This : TExtGridRowSelectionModel; RowIndex : Integer; R : TExtDataRecord);
  end;

implementation

uses
  AppThread;

constructor TEditWindow.Create;
var
  DataFields : TExtObjectList;
begin
  DataDir := '/fishfacts_data';
   {Below Web server's htdocs/Documents. With embedded Web server,
     this would be below executable's folder.}

   {Define XML reader's data fields. Each field corresponds
     to an element in the repeating element ("ROW").}
  DataFields := TExtObjectList.Create;
  with TExtDataField.AddTo(DataFields) do
    begin
    Name := 'Species_No';
    TypeJS := 'int';
    end;
  with TExtDataField.AddTo(DataFields) do
    begin
    Name := 'Category';
    TypeJS := 'string';
    end;
  with TExtDataField.AddTo(DataFields) do
    begin
    Name := 'Common_Name';
    TypeJS := 'string';
    end;
  with TExtDataField.AddTo(DataFields) do
    begin
    Name := 'Species_Name';
    TypeJS := 'string';
    end;
  with TExtDataField.AddTo(DataFields) do
    begin
    Name := 'Length_Cm';
    TypeJS := 'float';
    end;
  with TExtDataField.AddTo(DataFields) do
    begin
    Name := 'Length_In';
    TypeJS := 'float';
    end;
  with TExtDataField.AddTo(DataFields) do
    begin
    Name := 'Notes';
    TypeJS := 'string';
    end;
  DataRecord := TExtDataRecord.Create(DataFields);

   {Before creating grid (in .inc code), create anything that will be
     passed in grid's JavaScript constructor.}
  DataStore := TExtDataStore.Create;
  with DataStore do
    begin
    URL := DataDir + '/biolife.xml';
    Reader := TExtDataXmlReader.Create(JSObject('record:"ROW"'), DataRecord);
    SortInfo := JSObject('field:"Species_No", direction:"ASC"');
    end;

  RowSelect := TExtGridRowSelectionModel.Create(JSObject('singleSelect:true'));
//  RowSelect.SingleSelect := True;  //done this way creates invalid JS

  inherited;
{$IFDEF UseRuntime}
 {$I mainform.inc}
{$ENDIF}

   {Window is resizable, but don't let it get smaller than designed size}
  MinHeight := Height;
  MinWidth := Width;

   {When window is resized or maximized, adjust size of grid so it's
     anchored to right and bottom of window. Adjust width of notes
     memo so it's anchored to right size of window. Note that changing
     only width of notes memo (setWidth) didn't seem to work right.}
  On('resize',
     JSFunction('thisObj, newWidth, newHeight',
                EditGrid.JSName + '.setSize(newWidth - (' + JSName +
                 '.initialConfig.width - ' + EditGrid.JSName +
                 '.initialConfig.width), newHeight - (' + JSName +
                 '.initialConfig.height - ' + EditGrid.JSName +
                 '.initialConfig.height));' +
                NotesMemo.JSName + '.setSize(newWidth - (' + JSName +
                 '.initialConfig.width - ' + NotesMemo.JSName +
                 '.initialConfig.width), ' +
                 NotesMemo.JSName + '.initialConfig.height);'));

//  RowSelect.OnRowselect := RowSelectOnRowselect;
     //This results in TExtThread.HandleEvent runtime error, so just use
     // a JavaScript function instead:

   {Handle row selection change in browser with JS function.
    Update memo control with Notes field data from row's passed record. 
    Also update picture. Assume image file name is same as Common_Name field
     value. Could probably use update() but that requires Ext JS 3.1.0.
     Using setText() works fine with false passed in encode param so it
     treats text as HTML.
    Note that if a picture doesn't change, just set, for example,
     PictureLabel.Html in config rather than in event handler.}
  RowSelect.On('rowSelect',
                JSFunction('selModel, rowIndex, rec',
                           NotesMemo.JSName + '.setValue(rec.get("Notes"));' +
                           PictureLabel.JSName +
                            '.setText("<img src=\''' + DataDir +
                            '/" + rec.get("Common_Name") + ".gif' +
                            '\'' width=\''250\'' height=\''150\'' />", false);'));

  DataStore.On('load', JSFunction(RowSelect.JSName + '.selectFirstRow();'));
   //Note that selecting row doesn't always focus row and calling 
   // EditGrid.JSName + '.focus();' doesn't help much.

  EditGrid.SelModel := RowSelect;
  EditGrid.Store := DataStore;
  DataStore.Load(nil);
end;  {TEditWindow.Create}

procedure TEditWindow.Show;
begin
  inherited Show;
end;

procedure TEditWindow.RowSelectOnRowselect(This : TExtGridRowSelectionModel; RowIndex : Integer; R : TExtDataRecord);
 //This handler not currently used.
begin
//  NotesMemo.Value := R.Get('Notes').JSString('get', []);
end;

end.

