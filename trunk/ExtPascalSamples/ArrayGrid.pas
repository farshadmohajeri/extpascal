unit ArrayGrid;

interface

uses
  Ext;

type
  TArrayGrid = class(TExtGridPanel)
  private
    RowSelect : TExtSelectionRowModel;
    procedure RowSelectOnRowselect(This : TExtSelectionRowModel; RowIndex : Integer; R : TExtDataRecord);
  public
    constructor Create;
  end;

implementation

uses
  SysUtils, Session, ExtPascal;

constructor TArrayGrid.Create;
var
  DataStore  : TExtDataArrayStore;
  ColorValue : TExtFunction;
begin
  inherited;
  SelfSession.SetCodePress;
  // Statefull !!!
  ExtStateManager.SetProvider(TExtStateCookieProvider.Create);
  // create the data store
  DataStore := TExtDataArrayStore.Create;
  with DataStore do begin
    TExtDataField.AddTo(Fields).Name := 'company';
    with TExtDataField.AddTo(Fields) do begin Name := 'price';     TypeJS := 'float' end;
    with TExtDataField.AddTo(Fields) do begin Name := 'change';    TypeJS := 'float' end;
    with TExtDataField.AddTo(Fields) do begin Name := 'pctchange'; TypeJS := 'float' end;
    with TExtDataField.AddTo(Fields) do begin Name := 'lastchange';TypeJS := 'date'; DateFormat := 'Y/m/d' end;
    Data := JSArray(
      '["3m Co",71.72,0.02,0.03,"2009/01/02"],' +
      '["Alcoa Inc",29.01,0.42,1.47,"2009/01/02"],' +
      '["Altria Group Inc",83.81,0.28,0.34,"2009/01/02"],' +
      '["American Express Company",52.55,0.01,0.02,"2009/01/02"],' +
      '["American International Group, Inc.",64.13,0.31,0.49,"2009/01/02"],' +
      '["AT&T Inc.",31.61,-0.48,-1.54,"2009/01/02"],' +
      '["Boeing Co.",75.43,0.53,0.71,"2009/01/02"],' +
      '["Caterpillar Inc.",67.27,0.92,1.39,"2009/01/02"],' +
      '["Citigroup, Inc.",49.37,0.02,0.04,"2009/01/02"],' +
      '["E.I. du Pont de Nemours and Company",40.48,0.51,1.28,"2009/01/02"],' +
      '["Exxon Mobil Corp",68.1,-0.43,-0.64,"2009/01/02"],' +
      '["General Electric Company",34.14,-0.08,-0.23,"2009/01/02"],' +
      '["General Motors Corporation",30.27,1.09,3.74,"2009/01/02"],' +
      '["Hewlett-Packard Co.",36.53,-0.03,-0.08,"2009/01/02"],' +
      '["Honeywell Intl Inc",38.77,0.05,0.13,"2009/01/02"],' +
      '["Intel Corporation",19.88,0.31,1.58,"2009/01/02"],' +
      '["International Business Machines",81.41,0.44,0.54,"2009/01/02"],' +
      '["Johnson & Johnson",64.72,0.06,0.09,"2009/01/02"],' +
      '["JP Morgan & Chase & Co",45.73,0.07,0.15,"2009/01/02"],' +
      '["McDonald\"s Corporation",36.76,0.86,2.40,"2009/01/02"],' +
      '["Merck & Co., Inc.",40.96,0.41,1.01,"2009/01/02"],' +
      '["Microsoft Corporation",25.84,0.14,0.54,"2009/01/02"],' +
      '["Pfizer Inc",27.96,0.4,1.45,"2009/01/02"],' +
      '["The Coca-Cola Company",45.07,0.26,0.58,"2009/01/02"],' +
      '["The Home Depot, Inc.",34.64,0.35,1.02,"2009/01/02"],' +
      '["The Procter & Gamble Company",61.91,0.01,0.02,"2009/01/02"],' +
      '["United Technologies Corporation",63.26,0.55,0.88,"2009/01/02"],' +
      '["Verizon Communications",35.57,0.39,1.11,"2009/01/02"],' +
      '["Wal-Mart Stores, Inc.",45.45,0.73,1.63,"2009/01/02"]');
  end;
  Store := DataStore;
  ColorValue := JSFunction('V', 'if(V>0){return "<span style=''color:green''>" + V + "</span>";}else ' +
    'if(V<0){return "<span style=''color:red''>" + V + "</span>";}' +
    'return V;');
  with TExtGridColumn.AddTo(Columns) do begin
    Id        := 'company';
    Text      := 'Company';
    Width     := 160;
    Sortable  := true;
    DataIndex := Id;
  end;
  with TExtGridColumn.AddTo(Columns) do begin
    Text      := 'Price';
    Width     := 75;
    Sortable  := true;
    DataIndex := 'price';
    RendererString := 'usMoney';
  end;
  with TExtGridColumn.AddTo(Columns) do begin
    Text      := 'Change';
    Width     := 75;
    Sortable  := true;
    DataIndex := 'change';
    Renderer  := ColorValue;
  end;
  with TExtGridColumn.AddTo(Columns) do begin
    Text      := '% Change';
    Width     := 75;
    Sortable  := true;
    DataIndex := 'pctchange';
    Renderer  := ColorValue;
  end;
  with TExtGridColumn.AddTo(Columns) do begin
    Text      := 'Last Updated';
    Width     := 85;
    Sortable  := true;
    DataIndex := 'lastchange';
    Renderer  := ExtUtilFormat.Date('%0', 'm/d/Y'); // %0..%9 get event parameters
  end;
  with TExtButton.AddTo(TBar) do begin
    Text    := 'Show modal dialog using Ajax';
    Handler := Ajax(SelfSession.ShowLayoutWindow);
  end;
  SelfSession.AddShowSourceButton(Buttons, 'ArrayGrid');
  StripeRows := true;
  Height     := 350;
  Width      := 600;
  Title      := 'Array Grid';
  RenderTo   := 'body';
  Frame      := true;
  AutoExpandColumn := 'company';
  TExtGridRowSelectionModel(GetSelectionModel).SelectFirstRow;
  RowSelect := TExtGridRowSelectionModel.Create;
  SelModel  := RowSelect;
  RowSelect.OnRowSelect := RowSelectOnRowselect;
end;

procedure TArrayGrid.RowSelectOnRowselect(This : TExtGridRowSelectionModel; RowIndex : Integer; R : TExtDataRecord); begin
  ExtMessageBox.Alert('You selected the record number', IntToStr(RowIndex));
end;

end.
