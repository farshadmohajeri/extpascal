// Generated by GenPkg 0.0.8 at 19:27:20 on 2010-01-03

unit ExtP_Design_Ctrls;

{ExtPascal basic design controls}

{$IFDEF LCL}
 {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, {$IFDEF LCL} LResources, {$ENDIF} Controls, StdCtrls, ExtCtrls, ComCtrls, Forms;

type

  TExtEvent=procedure of object;


  TCustomExtWindow = class(TCustomForm)
  end;

  TExtWindow = class(TCustomExtWindow)
  private
    FBorder : Boolean;
    FMaximizable : Boolean;
    FMaximized : Boolean;
    FModal : Boolean;
    FPlain : Boolean;
    FResizable : Boolean;
  protected
    function GetTitle : string;
    procedure SetTitle(ATitle : string);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Border : Boolean read FBorder write FBorder default True;
    property Height;
    property Maximizable : Boolean read FMaximizable write FMaximizable default False;
    property Maximized : Boolean read FMaximized write FMaximized default False;
    property Modal : Boolean read FModal write FModal default False;
    property Name;
    property Plain : Boolean read FPlain write FPlain default False;
    property Resizable : Boolean read FResizable write FResizable default True;
    property Title : string read GetTitle write SetTitle;
    property Width;
  end;


{$IFDEF LCL}
  TCustomExtButton = class(TCustomButton)
{$ELSE}
  TCustomExtButton = class(TButtonControl)
{$ENDIF}
  end;

  TExtButton = class(TCustomExtButton)
  private
    FClick : TExtEvent;
  protected
    function GetDisabled : Boolean;
    procedure SetDisabled(ADisabled : Boolean);
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
    function GetMinWidth : Integer;
    procedure SetMinWidth(AMinWidth : Integer);
    function GetText : string;
    procedure SetText(AText : string);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Disabled : Boolean read GetDisabled write SetDisabled default False;
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property Hint;
    property Left;
    property MinWidth : Integer read GetMinWidth write SetMinWidth;
    property Name;
    property Text : string read GetText write SetText;
    property Top;
    property Width;
    property Click : TExtEvent read FClick write FClick;
  end;


  TCustomExtFormLabel = class(TCustomLabel)
  end;

  TExtFormLabel = class(TCustomExtFormLabel)
  private
  protected
    function GetAutoWidth : Boolean;
    procedure SetAutoWidth(AAutoWidth : Boolean);
    function GetDisabled : Boolean;
    procedure SetDisabled(ADisabled : Boolean);
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
    function GetText : string;
    procedure SetText(AText : string);
    function GetTextAlign : TAlignment;
    procedure SetTextAlign(ATextAlign : TAlignment);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property AutoWidth : Boolean read GetAutoWidth write SetAutoWidth default False;
    property Disabled : Boolean read GetDisabled write SetDisabled default False;
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property Left;
    property Name;
    property Text : string read GetText write SetText;
    property TextAlign : TAlignment read GetTextAlign write SetTextAlign default taLeftJustify;
    property Top;
    property Width;
  end;


  TCustomExtPanel = class(TCustomGroupBox)
  end;

  TExtPanel = class(TCustomExtPanel)
  private
  protected
    function GetDisabled : Boolean;
    procedure SetDisabled(ADisabled : Boolean);
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
    function GetTitle : string;
    procedure SetTitle(ATitle : string);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Disabled : Boolean read GetDisabled write SetDisabled default False;
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property Left;
    property Name;
    property Title : string read GetTitle write SetTitle;
    property Top;
    property Width;
  end;


  TCustomExtFormTextField = class(TCustomEdit)
  end;

  TExtFormTextField = class(TCustomExtFormTextField)
  private
    FChange : TExtEvent;
  protected
    function GetDisabled : Boolean;
    procedure SetDisabled(ADisabled : Boolean);
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
    function GetValue : string;
    procedure SetValue(AValue : string);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Disabled : Boolean read GetDisabled write SetDisabled default False;
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property Left;
    property Name;
    property ReadOnly;
    property Top;
    property Value : string read GetValue write SetValue;
    property Width;
    property Change : TExtEvent read FChange write FChange;
  end;


  TCustomExtFormNumberField = class(TCustomEdit)
  end;

  TExtFormNumberField = class(TCustomExtFormNumberField)
  private
    FChange : TExtEvent;
  protected
    function GetDisabled : Boolean;
    procedure SetDisabled(ADisabled : Boolean);
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
    function GetValue : string;
    procedure SetValue(AValue : string);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Disabled : Boolean read GetDisabled write SetDisabled default False;
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property Left;
    property Name;
    property ReadOnly;
    property Top;
    property Value : string read GetValue write SetValue;
    property Width;
    property Change : TExtEvent read FChange write FChange;
  end;


  TCustomExtFormDateField = class(TCustomEdit)
  end;

  TExtFormDateField = class(TCustomExtFormDateField)
  private
    FChange : TExtEvent;
  protected
    function GetDisabled : Boolean;
    procedure SetDisabled(ADisabled : Boolean);
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
    function GetValue : string;
    procedure SetValue(AValue : string);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Disabled : Boolean read GetDisabled write SetDisabled default False;
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property Left;
    property Name;
    property ReadOnly;
    property Top;
    property Value : string read GetValue write SetValue;
    property Width;
    property Change : TExtEvent read FChange write FChange;
  end;


  TCustomExtFormTimeField = class(TCustomEdit)
  end;

  TExtFormTimeField = class(TCustomExtFormTimeField)
  private
    FChange : TExtEvent;
  protected
    function GetDisabled : Boolean;
    procedure SetDisabled(ADisabled : Boolean);
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
    function GetValue : string;
    procedure SetValue(AValue : string);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Disabled : Boolean read GetDisabled write SetDisabled default False;
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property Left;
    property Name;
    property ReadOnly;
    property Top;
    property Value : string read GetValue write SetValue;
    property Width;
    property Change : TExtEvent read FChange write FChange;
  end;


  TCustomExtFormTextArea = class(TCustomMemo)
  end;

  TExtFormTextArea = class(TCustomExtFormTextArea)
  private
  protected
    function GetDisabled : Boolean;
    procedure SetDisabled(ADisabled : Boolean);
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
    function GetValue : TStrings;
    procedure SetValue(AValue : TStrings);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Disabled : Boolean read GetDisabled write SetDisabled default False;
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property Left;
    property Name;
    property ReadOnly;
    property Top;
    property Value : TStrings read GetValue write SetValue;
    property Width;
  end;


  TCustomExtFormHtmlEditor = class(TCustomMemo)
  end;

  TExtFormHtmlEditor = class(TCustomExtFormHtmlEditor)
  private
  protected
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
    function GetValue : TStrings;
    procedure SetValue(AValue : TStrings);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property Left;
    property Name;
    property Top;
    property Value : TStrings read GetValue write SetValue;
    property Width;
  end;


  TCustomExtFormCheckbox = class(TCustomCheckBox)
  end;

  TExtFormCheckbox = class(TCustomExtFormCheckbox)
  private
    FCheck : TExtEvent;
  protected
    function GetBoxLabel : string;
    procedure SetBoxLabel(ABoxLabel : string);
    function GetDisabled : Boolean;
    procedure SetDisabled(ADisabled : Boolean);
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property BoxLabel : string read GetBoxLabel write SetBoxLabel;
    property Checked;
    property Disabled : Boolean read GetDisabled write SetDisabled default False;
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property Left;
    property Name;
    property Top;
    property Width;
    property Check : TExtEvent read FCheck write FCheck;
  end;


  TCustomExtFormComboBox = class(TCustomComboBox)
  end;

  TExtFormComboBox = class(TCustomExtFormComboBox)
  private
    FChange : TExtEvent;
    FEditable : Boolean;
  protected
    function GetDisabled : Boolean;
    procedure SetDisabled(ADisabled : Boolean);
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
    function GetStoreArray : TStrings;
    procedure SetStoreArray(AStoreArray : TStrings);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Disabled : Boolean read GetDisabled write SetDisabled default False;
    property Editable : Boolean read FEditable write FEditable default True;
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property StoreArray : TStrings read GetStoreArray write SetStoreArray;
    property Left;
    property Name;
    property Top;
    property Width;
    property Change : TExtEvent read FChange write FChange;
  end;


  TCustomExtFormRadioGroup = class(TCustomRadioGroup)
  end;

  TExtFormRadioGroup = class(TCustomExtFormRadioGroup)
  private
    FChange : TExtEvent;
  protected
    function GetColumnsNumber : Integer;
    procedure SetColumnsNumber(AColumnsNumber : Integer);
    function GetDisabled : Boolean;
    procedure SetDisabled(ADisabled : Boolean);
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property ColumnsNumber : Integer read GetColumnsNumber write SetColumnsNumber default 1;
    property Disabled : Boolean read GetDisabled write SetDisabled default False;
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property Items;
    property Left;
    property Name;
    property Top;
    property Width;
    property Change : TExtEvent read FChange write FChange;
  end;


  TCustomExtUxFormMultiSelect = class(TCustomListBox)
  end;

  TExtUxFormMultiSelect = class(TCustomExtUxFormMultiSelect)
  private
    FDelimiter : Char;
  protected
    function GetDisabled : Boolean;
    procedure SetDisabled(ADisabled : Boolean);
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
    function GetStoreArray : TStrings;
    procedure SetStoreArray(AStoreArray : TStrings);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Delimiter : Char read FDelimiter write FDelimiter default ',';
    property Disabled : Boolean read GetDisabled write SetDisabled default False;
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property StoreArray : TStrings read GetStoreArray write SetStoreArray;
    property Left;
    property Name;
    property Top;
    property Width;
  end;


{$IFDEF LCL}
  TCustomExtPanel_Tab = class(TCustomPage)
{$ELSE}
  TCustomExtPanel_Tab = class(TTabSheet)
{$ENDIF}
  end;

  TExtPanel_Tab = class(TCustomExtPanel_Tab)
  private
  protected
    function GetTitle : string;
    procedure SetTitle(ATitle : string);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Name;
    property Title : string read GetTitle write SetTitle;
  end;


{$IFDEF LCL}
  TCustomExtTabPanel = class(TCustomNotebook)
{$ELSE}
  TCustomExtTabPanel = class(TPageControl)
{$ENDIF}
  end;

  TExtTabPanel = class(TCustomExtTabPanel)
  private
  protected
    function GetActiveTab : TExtPanel_Tab;
    procedure SetActiveTab(AActiveTab : TExtPanel_Tab);
    function GetDisabled : Boolean;
    procedure SetDisabled(ADisabled : Boolean);
    function GetHidden : Boolean;
    procedure SetHidden(AHidden : Boolean);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property ActiveTab : TExtPanel_Tab read GetActiveTab write SetActiveTab;
    property Disabled : Boolean read GetDisabled write SetDisabled default False;
    property Height;
    property Hidden : Boolean read GetHidden write SetHidden default False;
    property Left;
    property Name;
    property Top;
    property Width;
  end;


procedure Register;


implementation

 {TExtWindow}

constructor TExtWindow.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Height := 300;
  Width := 400;
  Border := True;
  Maximizable := False;
  Maximized := False;
  Modal := False;
  Plain := False;
  Resizable := True;
end;

destructor TExtWindow.Destroy;
begin
  inherited Destroy;
end;

function TExtWindow.GetTitle : string;
begin
  Result := Caption;
end;

procedure TExtWindow.SetTitle(ATitle : string);
begin
  if Caption <> ATitle then
    Caption := ATitle;
end;


 {TExtButton}

constructor TExtButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Disabled := False;
  Hidden := False;
end;

destructor TExtButton.Destroy;
begin
  inherited Destroy;
end;

function TExtButton.GetDisabled : Boolean;
begin
  Result := not Enabled;
end;

procedure TExtButton.SetDisabled(ADisabled : Boolean);
begin
  if Enabled = ADisabled then
    Enabled := not ADisabled;
end;

function TExtButton.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtButton.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;

function TExtButton.GetMinWidth : Integer;
begin
  Result := Width;
end;

procedure TExtButton.SetMinWidth(AMinWidth : Integer);
begin
  if Width <> AMinWidth then
    Width := AMinWidth;
end;

function TExtButton.GetText : string;
begin
  Result := Caption;
end;

procedure TExtButton.SetText(AText : string);
begin
  if Caption <> AText then
    Caption := AText;
end;


 {TExtFormLabel}

constructor TExtFormLabel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  AutoWidth := False;
  Disabled := False;
  Hidden := False;
  TextAlign := taLeftJustify;
end;

destructor TExtFormLabel.Destroy;
begin
  inherited Destroy;
end;

function TExtFormLabel.GetAutoWidth : Boolean;
begin
  Result := AutoSize;
end;

procedure TExtFormLabel.SetAutoWidth(AAutoWidth : Boolean);
begin
  if AutoSize <> AAutoWidth then
    AutoSize := AAutoWidth;
end;

function TExtFormLabel.GetDisabled : Boolean;
begin
  Result := not Enabled;
end;

procedure TExtFormLabel.SetDisabled(ADisabled : Boolean);
begin
  if Enabled = ADisabled then
    Enabled := not ADisabled;
end;

function TExtFormLabel.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtFormLabel.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;

function TExtFormLabel.GetText : string;
begin
  Result := Caption;
end;

procedure TExtFormLabel.SetText(AText : string);
begin
  if Caption <> AText then
    Caption := AText;
end;

function TExtFormLabel.GetTextAlign : TAlignment;
begin
  Result := Alignment;
end;

procedure TExtFormLabel.SetTextAlign(ATextAlign : TAlignment);
begin
  if Alignment <> ATextAlign then
    Alignment := ATextAlign;
end;


 {TExtPanel}

constructor TExtPanel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Disabled := False;
  Hidden := False;
end;

destructor TExtPanel.Destroy;
begin
  inherited Destroy;
end;

function TExtPanel.GetDisabled : Boolean;
begin
  Result := not Enabled;
end;

procedure TExtPanel.SetDisabled(ADisabled : Boolean);
begin
  if Enabled = ADisabled then
    Enabled := not ADisabled;
end;

function TExtPanel.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtPanel.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;

function TExtPanel.GetTitle : string;
begin
  Result := Caption;
end;

procedure TExtPanel.SetTitle(ATitle : string);
begin
  if Caption <> ATitle then
    Caption := ATitle;
end;


 {TExtFormTextField}

constructor TExtFormTextField.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LCL}
  AutoSize := False;
{$ENDIF}
  Disabled := False;
  Hidden := False;
  ReadOnly := False;
end;

destructor TExtFormTextField.Destroy;
begin
  inherited Destroy;
end;

function TExtFormTextField.GetDisabled : Boolean;
begin
  Result := not Enabled;
end;

procedure TExtFormTextField.SetDisabled(ADisabled : Boolean);
begin
  if Enabled = ADisabled then
    Enabled := not ADisabled;
end;

function TExtFormTextField.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtFormTextField.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;

function TExtFormTextField.GetValue : string;
begin
  Result := Text;
end;

procedure TExtFormTextField.SetValue(AValue : string);
begin
  if Text <> AValue then
    Text := AValue;
end;


 {TExtFormNumberField}

constructor TExtFormNumberField.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LCL}
  AutoSize := False;
{$ENDIF}
  Disabled := False;
  Hidden := False;
  ReadOnly := False;
end;

destructor TExtFormNumberField.Destroy;
begin
  inherited Destroy;
end;

function TExtFormNumberField.GetDisabled : Boolean;
begin
  Result := not Enabled;
end;

procedure TExtFormNumberField.SetDisabled(ADisabled : Boolean);
begin
  if Enabled = ADisabled then
    Enabled := not ADisabled;
end;

function TExtFormNumberField.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtFormNumberField.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;

function TExtFormNumberField.GetValue : string;
begin
  Result := Text;
end;

procedure TExtFormNumberField.SetValue(AValue : string);
begin
  if Text <> AValue then
    Text := AValue;
end;


 {TExtFormDateField}

constructor TExtFormDateField.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LCL}
  AutoSize := False;
{$ENDIF}
  Disabled := False;
  Hidden := False;
  ReadOnly := False;
end;

destructor TExtFormDateField.Destroy;
begin
  inherited Destroy;
end;

function TExtFormDateField.GetDisabled : Boolean;
begin
  Result := not Enabled;
end;

procedure TExtFormDateField.SetDisabled(ADisabled : Boolean);
begin
  if Enabled = ADisabled then
    Enabled := not ADisabled;
end;

function TExtFormDateField.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtFormDateField.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;

function TExtFormDateField.GetValue : string;
begin
  Result := Text;
end;

procedure TExtFormDateField.SetValue(AValue : string);
begin
  if Text <> AValue then
    Text := AValue;
end;


 {TExtFormTimeField}

constructor TExtFormTimeField.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LCL}
  AutoSize := False;
{$ENDIF}
  Disabled := False;
  Hidden := False;
  ReadOnly := False;
end;

destructor TExtFormTimeField.Destroy;
begin
  inherited Destroy;
end;

function TExtFormTimeField.GetDisabled : Boolean;
begin
  Result := not Enabled;
end;

procedure TExtFormTimeField.SetDisabled(ADisabled : Boolean);
begin
  if Enabled = ADisabled then
    Enabled := not ADisabled;
end;

function TExtFormTimeField.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtFormTimeField.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;

function TExtFormTimeField.GetValue : string;
begin
  Result := Text;
end;

procedure TExtFormTimeField.SetValue(AValue : string);
begin
  if Text <> AValue then
    Text := AValue;
end;


 {TExtFormTextArea}

constructor TExtFormTextArea.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Disabled := False;
  Hidden := False;
  ReadOnly := False;
end;

destructor TExtFormTextArea.Destroy;
begin
  inherited Destroy;
end;

function TExtFormTextArea.GetDisabled : Boolean;
begin
  Result := not Enabled;
end;

procedure TExtFormTextArea.SetDisabled(ADisabled : Boolean);
begin
  if Enabled = ADisabled then
    Enabled := not ADisabled;
end;

function TExtFormTextArea.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtFormTextArea.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;

function TExtFormTextArea.GetValue : TStrings;
begin
  Result := Lines;
end;

procedure TExtFormTextArea.SetValue(AValue : TStrings);
begin
  if Lines <> AValue then
    Lines := AValue;
end;


 {TExtFormHtmlEditor}

constructor TExtFormHtmlEditor.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Hidden := False;
end;

destructor TExtFormHtmlEditor.Destroy;
begin
  inherited Destroy;
end;

function TExtFormHtmlEditor.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtFormHtmlEditor.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;

function TExtFormHtmlEditor.GetValue : TStrings;
begin
  Result := Lines;
end;

procedure TExtFormHtmlEditor.SetValue(AValue : TStrings);
begin
  if Lines <> AValue then
    Lines := AValue;
end;


 {TExtFormCheckbox}

constructor TExtFormCheckbox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LCL}
  AutoSize := False;
{$ENDIF}
  Disabled := False;
  Hidden := False;
end;

destructor TExtFormCheckbox.Destroy;
begin
  inherited Destroy;
end;

function TExtFormCheckbox.GetBoxLabel : string;
begin
  Result := Caption;
end;

procedure TExtFormCheckbox.SetBoxLabel(ABoxLabel : string);
begin
  if Caption <> ABoxLabel then
    Caption := ABoxLabel;
end;

function TExtFormCheckbox.GetDisabled : Boolean;
begin
  Result := not Enabled;
end;

procedure TExtFormCheckbox.SetDisabled(ADisabled : Boolean);
begin
  if Enabled = ADisabled then
    Enabled := not ADisabled;
end;

function TExtFormCheckbox.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtFormCheckbox.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;


 {TExtFormComboBox}

constructor TExtFormComboBox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Disabled := False;
  Editable := True;
  Hidden := False;
end;

destructor TExtFormComboBox.Destroy;
begin
  inherited Destroy;
end;

function TExtFormComboBox.GetDisabled : Boolean;
begin
  Result := not Enabled;
end;

procedure TExtFormComboBox.SetDisabled(ADisabled : Boolean);
begin
  if Enabled = ADisabled then
    Enabled := not ADisabled;
end;

function TExtFormComboBox.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtFormComboBox.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;

function TExtFormComboBox.GetStoreArray : TStrings;
begin
  Result := Items;
end;

procedure TExtFormComboBox.SetStoreArray(AStoreArray : TStrings);
begin
  if Items <> AStoreArray then
    Items := AStoreArray;
end;


 {TExtFormRadioGroup}

constructor TExtFormRadioGroup.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ColumnsNumber := 1;
  Disabled := False;
  Hidden := False;
end;

destructor TExtFormRadioGroup.Destroy;
begin
  inherited Destroy;
end;

function TExtFormRadioGroup.GetColumnsNumber : Integer;
begin
  Result := Columns;
end;

procedure TExtFormRadioGroup.SetColumnsNumber(AColumnsNumber : Integer);
begin
  if Columns <> AColumnsNumber then
    Columns := AColumnsNumber;
end;

function TExtFormRadioGroup.GetDisabled : Boolean;
begin
  Result := not Enabled;
end;

procedure TExtFormRadioGroup.SetDisabled(ADisabled : Boolean);
begin
  if Enabled = ADisabled then
    Enabled := not ADisabled;
end;

function TExtFormRadioGroup.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtFormRadioGroup.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;


 {TExtUxFormMultiSelect}

constructor TExtUxFormMultiSelect.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Delimiter := ',';
  Disabled := False;
  Hidden := False;
end;

destructor TExtUxFormMultiSelect.Destroy;
begin
  inherited Destroy;
end;

function TExtUxFormMultiSelect.GetDisabled : Boolean;
begin
  Result := not Enabled;
end;

procedure TExtUxFormMultiSelect.SetDisabled(ADisabled : Boolean);
begin
  if Enabled = ADisabled then
    Enabled := not ADisabled;
end;

function TExtUxFormMultiSelect.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtUxFormMultiSelect.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;

function TExtUxFormMultiSelect.GetStoreArray : TStrings;
begin
  Result := Items;
end;

procedure TExtUxFormMultiSelect.SetStoreArray(AStoreArray : TStrings);
begin
  if Items <> AStoreArray then
    Items := AStoreArray;
end;


 {TExtPanel_Tab}

constructor TExtPanel_Tab.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;

destructor TExtPanel_Tab.Destroy;
begin
  inherited Destroy;
end;

function TExtPanel_Tab.GetTitle : string;
begin
  Result := Caption;
end;

procedure TExtPanel_Tab.SetTitle(ATitle : string);
begin
  if Caption <> ATitle then
    Caption := ATitle;
end;


 {TExtTabPanel}

constructor TExtTabPanel.Create(AOwner : TComponent);
begin
{$IFDEF LCL}
  PageClass := TExtPanel_Tab;
{$ENDIF}
  inherited Create(AOwner);
  Disabled := False;
  Hidden := False;
end;

destructor TExtTabPanel.Destroy;
begin
  inherited Destroy;
end;

function TExtTabPanel.GetActiveTab : TExtPanel_Tab;
begin
{$IFDEF LCL}
  Result := TExtPanel_Tab(inherited ActivePageComponent);
{$ENDIF}
end;

procedure TExtTabPanel.SetActiveTab(AActiveTab : TExtPanel_Tab);
begin
{$IFDEF LCL}
  ActivePageComponent := AActiveTab;
{$ENDIF}
end;

function TExtTabPanel.GetDisabled : Boolean;
begin
  Result := not Enabled;
end;

procedure TExtTabPanel.SetDisabled(ADisabled : Boolean);
begin
  if Enabled = ADisabled then
    Enabled := not ADisabled;
end;

function TExtTabPanel.GetHidden : Boolean;
begin
  Result := not Visible;
end;

procedure TExtTabPanel.SetHidden(AHidden : Boolean);
begin
  if Visible = AHidden then
    Visible := not AHidden;
end;


procedure Register;
begin
  RegisterComponents('ExtPascal',
                     [TExtButton,
                      TExtFormLabel,
                      TExtPanel,
                      TExtFormTextField,
                      TExtFormNumberField,
                      TExtFormDateField,
                      TExtFormTimeField,
                      TExtFormTextArea,
                      TExtFormHtmlEditor,
                      TExtFormCheckbox,
                      TExtFormComboBox,
                      TExtFormRadioGroup,
                      TExtUxFormMultiSelect,
                      TExtTabPanel]);
  RegisterNoIcon([TExtPanel_Tab]);
end;


{$IFDEF LCL}
initialization
{$I extp_glyphs.lrs}
{$ENDIF}

end.
