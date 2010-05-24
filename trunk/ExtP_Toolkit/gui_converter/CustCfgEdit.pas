unit CustCfgEdit;

{
  Custom config file editor for GUI converter of Delphi or
   Lazarus forms to ExtPascal code files.

  Author:     Phil Hess.
  Copyright:  Copyright (C) 2010 Phil Hess. All rights reserved.
  License:    Modified LGPL.
}

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, {$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  IniFiles, Fm2Base;

type
  TCustConfigEditor = class(TForm)
    UnknownGroupBox: TGroupBox;
    UnknownListBox: TListBox;
    UnknownHelpMemo: TMemo;

    MappingGroupBox: TGroupBox;
    MapToLbl: TLabel;
    MapToComboBox: TComboBox;
    FormUnitLbl: TLabel;
    FormUnitComboBox: TComboBox;
    PropsLbl: TLabel;
    PropsHelpLbl: TLabel;
    PropsMemo: TMemo;

    SaveBtn: TButton;
    CloseBtn: TButton;

    procedure UnknownListBoxClick(Sender: TObject);
    procedure MapToComboBoxChange(Sender: TObject);
    procedure FormUnitComboBoxChange(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
  private
    FmFileNames     : TStringList;
    CustCfgFileName : string;
    CustCfgF        : TMemIniFile;
    CurClassItem    : Integer;
    CurClassName    : string;
    HasChanged      : Boolean;
    procedure UpdateFormCaption;
    procedure RefreshList;
    procedure GetPropMappings;
  public
    NewCustCfgFileName : string;
    procedure LoadEditor(      UnmappedClasses  : TStringList;
                               CfgClasses       : TStringList;
                               AFmFileNames     : TStringList;
                         const ACustCfgFileName : string);
  end;

var
  CustConfigEditor: TCustConfigEditor;

implementation

{$IFNDEF LCL}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

procedure TCustConfigEditor.LoadEditor(      UnmappedClasses  : TStringList;
                                             CfgClasses       : TStringList;
                                             AFmFileNames     : TStringList;
                                       const ACustCfgFileName : string);
var
  ClassNum  : Integer;
  FormNum   : Integer;
  ClassName : string;
begin
  FmFileNames := AFmFileNames;
  CustCfgFileName := ACustCfgFileName;
  if FileExists(CustCfgFileName) then
    CustCfgF := TMemIniFile.Create(CustCfgFileName)
  else  {Create has to have a file name, so give it a dummy one}
  {$IFDEF MSWINDOWS}
    CustCfgF := TMemIniFile.Create('NUL');
  {$ELSE}
    CustCfgF := TMemIniFile.Create('/dev/null');
  {$ENDIF}
  UpdateFormCaption;

  for ClassNum := 0 to UnmappedClasses.Count-1 do
    begin
    ClassName := UnmappedClasses.Strings[ClassNum];
    if UnmappedClasses.Objects[ClassNum] <> nil then  {Okay to map class?}
      UnknownListBox.Items.AddObject(ClassName, UnmappedClasses.Objects[ClassNum])
    else  {Ignored class}
      UnknownListBox.Items.Add(ClassName + ' [Ignored]')
    end;

  MapToComboBox.Items.Add('');  {So selection can be blanked}
  for ClassNum := 0 to CfgClasses.Count-1 do
    MapToComboBox.Items.Add(CfgClasses.Strings[ClassNum]);

  FormUnitComboBox.Items.Add('');  {So selection can be blanked}
  for FormNum := 0 to FmFileNames.Count-1 do
    FormUnitComboBox.Items.Add(
     ExtractFileNameWithoutExt(FmFileNames.Strings[FormNum]));

  RefreshList;
  CurClassItem := -1;
  if UnknownListBox.Items.Count > 0 then
    begin
    UnknownListBox.ItemIndex := 0;
    UnknownListBoxClick(nil);
    end
  else  {Can't do anything if no unmapped classes, so just disable controls}
    begin
    MapToComboBox.Enabled := False;
    FormUnitComboBox.Enabled := False;
    PropsMemo.Enabled := False;
    end;
  HasChanged := False;
end;


procedure TCustConfigEditor.UpdateFormCaption;
var
  HyphenPos : Integer;
begin
  HyphenPos := Pos(' - ', Caption);
  if HyphenPos = 0 then
    HyphenPos := Length(Caption) + 1;
  Caption := Copy(Caption, 1, HyphenPos-1) + ' - ';
  if CustCfgFileName = '' then
    Caption := Caption + '[New config file]'
  else
    Caption := Caption + ExtractFileName(CustCfgFileName);  
end;


procedure TCustConfigEditor.RefreshList;
var
  ClassNum   : Integer;
  ClassName  : string;
  CfgPropVal : string;
begin
  for ClassNum := 0 to UnknownListBox.Items.Count-1 do
    begin
    ClassName := UnknownListBox.Items.Strings[ClassNum];
    if Pos('(', ClassName) > 0 then
      ClassName := Trim(Copy(ClassName, 1, Pos('(', ClassName)-1)); 
    if UnknownListBox.Items.Objects[ClassNum] = TObject(1) then  {Inherited form?}
      begin
      CfgPropVal := CustCfgF.ReadString(ClassName, 'UnitName', '');
      if FormUnitComboBox.Items.IndexOf(CfgPropVal) <= 0 then
        ClassName := ClassName + ' (*)';
      end
    else if UnknownListBox.Items.Objects[ClassNum] = TObject(2) then  {Non-form class?}    
      begin
      CfgPropVal := CustCfgF.ReadString(ClassName, 'Class', '');
      if MapToComboBox.Items.IndexOf(CfgPropVal) <= 0 then
        ClassName := ClassName + ' (*)';
      end;
    UnknownListBox.Items.Strings[ClassNum] := ClassName;
    end;
end;


procedure TCustConfigEditor.GetPropMappings;
var
  PropIdx  : Integer;
  PropStr  : string;
  EqualPos : Integer;
begin
  if CurClassItem >= 0 then
    begin  {Check if any property mappings in memo box}
    for PropIdx := 0 to PropsMemo.Lines.Count-1 do
      begin
      PropStr := Trim(PropsMemo.Lines.Strings[PropIdx]);
      if PropStr <> '' then
        begin
        EqualPos := Pos('=', PropStr);
        if EqualPos = 0 then
          CustCfgF.WriteString(CurClassName, PropStr, '')
        else if EqualPos > 1 then  {Note ignoring lines that start with "="}   
{$IFNDEF LCL}  {VCL and LCL WriteString don't work the same}
          CustCfgF.WriteString(CurClassName, 
                               Trim(Copy(PropStr, 1, Pred(EqualPos))),
                               Trim(Copy(PropStr, Succ(EqualPos), MaxInt)));
{$ELSE}  {Above loses blank keys, so just write key and value together}
          CustCfgF.WriteString(CurClassName, PropStr, '');
{$ENDIF}
        end;
      end;
    end;
end;


procedure TCustConfigEditor.UnknownListBoxClick(Sender: TObject);
var
  SaveChanged : Boolean;
  CfgPropVal  : string;
  ClassProps  : TStringList;
  PropIdx     : Integer;
begin
  if UnknownListBox.ItemIndex = CurClassItem then
    Exit;
  GetPropMappings;

  CurClassItem := UnknownListBox.ItemIndex;
  CurClassName := UnknownListBox.Items.Strings[CurClassItem];
  if Pos('(', CurClassName) > 0 then
    CurClassName := Trim(Copy(CurClassName, 1, Pos('(', CurClassName)-1));

  if PropsMemo.Modified then
    HasChanged := True;
  SaveChanged := HasChanged;

  if UnknownListBox.Items.Objects[CurClassItem] = TObject(1) then
    begin  {Inherited form}
    MapToComboBox.Enabled := False;
    MapToComboBox.ItemIndex := -1;
    FormUnitComboBox.Enabled := True;
    CfgPropVal := CustCfgF.ReadString(CurClassName, 'UnitName', '');
    FormUnitComboBox.ItemIndex := FormUnitComboBox.Items.IndexOf(CfgPropVal);
    PropsMemo.Enabled := True;
    end
  else if UnknownListBox.Items.Objects[CurClassItem] = TObject(2) then
    begin  {Non-form class}
    FormUnitComboBox.Enabled := False;
    FormUnitComboBox.ItemIndex := -1;
    MapToComboBox.Enabled := True;
    CfgPropVal := CustCfgF.ReadString(CurClassName, 'Class', '');
    MapToComboBox.ItemIndex := MapToComboBox.Items.IndexOf(CfgPropVal);
    PropsMemo.Enabled := True;
    end
  else
    begin  {Ignored class}
    MapToComboBox.Enabled := False;
    MapToComboBox.ItemIndex := -1;
    FormUnitComboBox.Enabled := False;
    FormUnitComboBox.ItemIndex := -1;
    PropsMemo.Enabled := False;
    end;
  HasChanged := SaveChanged;  {Restore}

  PropsMemo.Lines.Clear;
  if UnknownListBox.Items.Objects[CurClassItem] <> nil then
    begin
    ClassProps := TStringList.Create;
    CustCfgF.ReadSectionValues(CurClassName, ClassProps);
    if ClassProps.Count > 0 then
      begin  {Note that LCL reads and write comments (;) with INI files}
      for PropIdx := 0 to ClassProps.Count-1 do
        begin
        if (not SameText(ClassProps.Names[PropIdx], 'Class')) and
           (not SameText(ClassProps.Names[PropIdx], 'UnitName')) then
          PropsMemo.Lines.Add(ClassProps.Strings[PropIdx]);
        end;
      end;
    ClassProps.Free;
    end;
  PropsMemo.Modified := False;
end;


procedure TCustConfigEditor.MapToComboBoxChange(Sender: TObject);
begin
  if MapToComboBox.ItemIndex < 0 then
    Exit;
  if MapToComboBox.ItemIndex > 0 then
    CustCfgF.WriteString(CurClassName, 'Class', 
                         MapToComboBox.Items.Strings[MapToComboBox.ItemIndex])
  else
    CustCfgF.DeleteKey(CurClassName, 'Class');
  RefreshList;
  HasChanged := True;
end;


procedure TCustConfigEditor.FormUnitComboBoxChange(Sender: TObject);
var
  FmF   : TextFile;
  InStr : string;
begin
  if FormUnitComboBox.ItemIndex < 0 then
    Exit;
  if FormUnitComboBox.ItemIndex > 0 then
    begin
    if not FileExists(FmFileNames.Strings[FormUnitComboBox.ItemIndex-1]) then
      Exit;
    AssignFile(FmF, FmFileNames.Strings[FormUnitComboBox.ItemIndex-1]);
    Reset(FmF);
    ReadLn(FmF, InStr);
    CloseFile(FmF);  
    InStr := Trim(Copy(InStr, Pos(':', InStr)+1, MaxInt));
    CustCfgF.WriteString(CurClassName, 'Class', InStr);
    CustCfgF.WriteString(CurClassName, 'UnitName',
                         FormUnitComboBox.Items.Strings[FormUnitComboBox.ItemIndex]);
    end
  else
    begin
    CustCfgF.DeleteKey(CurClassName, 'Class');
    CustCfgF.DeleteKey(CurClassName, 'UnitName');
    end; 
  RefreshList;
  HasChanged := True;
end;


procedure TCustConfigEditor.SaveBtnClick(Sender: TObject);
var
  SaveDlg : TSaveDialog;
begin
  SaveDlg := TSaveDialog.Create(Application);
  try
    SaveDlg.Filter :=
     'Config File (*' + CfgFileExt + ')|*' + CfgFileExt;
    SaveDlg.DefaultExt := Copy(CfgFileExt, 2, MaxInt);
    SaveDlg.Options :=
     [ofOverwritePrompt,ofPathMustExist,ofHideReadOnly,ofNoReadOnlyReturn];
    SaveDlg.Title := 'Save Custom Config File';
    if CustCfgFileName <> '' then
      SaveDlg.FileName := CustCfgFileName
    else
      SaveDlg.FileName := 'custconfig' + CfgFileExt;
    if not SaveDlg.Execute then
      Exit;
    if not SameText(ExtractFileExt(SaveDlg.FileName), CfgFileExt) then
      begin
      MessageDlg('Select a custom config file (' + CfgFileExt + ').', 
                 mtError, [mbOK], 0);
      Exit;
      end;
    GetPropMappings;
    CustCfgF.Rename(SaveDlg.FileName, False);
    CustCfgF.UpdateFile;
    CustCfgFileName := SaveDlg.FileName;
    if CustCfgFileName <> NewCustCfgFileName then
      NewCustCfgFileName := CustCfgFileName;
    UpdateFormCaption;
    HasChanged := False;
    PropsMemo.Modified := False;
  finally
    SaveDlg.Free;
    end;
end;


procedure TCustConfigEditor.CloseBtnClick(Sender: TObject);
begin
  if HasChanged or PropsMemo.Modified then
    begin
    case MessageDlg('Save edits to custom config file?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes :
        begin
        SaveBtnClick(Sender);
        if HasChanged or PropsMemo.Modified then  {Didn't save?}
          Exit;
        end;
      mrNo : Close;
      mrCancel : Exit;
      end;
    end;
  Close;
end;


end.
