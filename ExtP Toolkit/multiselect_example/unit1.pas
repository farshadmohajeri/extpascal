unit Unit1; 

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
  ExtP_Design_Ctrls; 
{$ENDIF}

type
  TExtWindow1 = class(TExtWindow)
    ExtPanel1: TExtPanel;
    ExtFormLabel1: TExtFormLabel;
    ExtUxFormMultiSelect1: TExtUxFormMultiSelect;
    ExtButton1: TExtButton;
    ExtFormLabel2: TExtFormLabel;
    procedure ExtButton1Click;
  private
  public
    constructor Create; 
    procedure Show; 
    procedure ExtButton1OnClick(This : TExtButton; E : TExtEventObjectSingleton);
  end; 

implementation

uses
  AppThread; 

constructor TExtWindow1.Create; 
begin
  inherited; 
{$IFDEF UseRuntime}
 {$I unit1.inc}
{$ENDIF}

// Note that there are at least two other ways of handling the click event and
//  passing data to the event handler method on the server, as indicated here
//  in the commented out code.

(*
   //Event set by converter (in .inc) doesn't pass any data, so add an event
   // that passes list box's selected items. Ignore converter's event below.
  ExtButton1.On('click', Ajax(ExtButton1Click,
                              ['Selected', ExtUxFormMultiSelect1.GetValue]));
*)

(*
  ExtButton1.PurgeListeners;  //Disable converter-generated event.
  ExtButton1.OnClick := ExtButton1OnClick;
*)


   //If items not set in IDE, can set them here like this:
//  ExtUxFormMultiSelect1.StoreArray := JSArray('"Item 1", "Item 2"');

  ExtUxFormMultiSelect1.Delimiter := #9;  //Default comma could apear in item
  ExtUxFormMultiSelect1.MaxSelections := 1;
end;

procedure TExtWindow1.Show; 
begin
  inherited Show; 
end;

procedure TExtWindow1.ExtButton1Click;
var
  ItemStr : string;
begin
(*  //This is used with On('click' above.
  if CurrentThread.Queries.IndexOfName('Selected') < 0 then  {Not event we want?}
    Exit;

  ItemStr := CurrentThread.Query['Selected'];
*)

  ItemStr := ExtUxFormMultiSelect1.JSString(ExtUxFormMultiSelect1.GetValue);

  if ItemStr = '' then
    begin
    ExtMessageBox.Alert('Error', 'You must select an item.');
    Exit;
    end;

  if Pos(ExtUxFormMultiSelect1.Delimiter, ItemStr) > 0 then
    begin
    ExtMessageBox.Alert('Error', 'You can only select one item.');
    Exit;
    end;

  ExtFormLabel2.Text := 'You selected ' + ItemStr;
end;

procedure TExtWindow1.ExtButton1OnClick(This : TExtButton; E : TExtEventObjectSingleton);
begin
  ExtButton1Click;
end;

end.

