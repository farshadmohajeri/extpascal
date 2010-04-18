unit MainFrm;

{
  Frontend form that collects information to be used in converting
   Delphi or Lazarus forms to ExtPascal code files.

  Note that Delphi form files (.dfm) must be text files.
   
  Author:     Phil Hess.
  Copyright:  Copyright (C) 2010 Phil Hess. All rights reserved.
  License:    Modified LGPL.
}

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, {$ENDIF}
  SysUtils, Classes, Graphics, Forms, Dialogs, Controls, StdCtrls, ExtCtrls,
  Fm2Base, Fm2ExtP, CustCfgEdit;

const
  ProgName          = 'FormsToExtPascal';
  ProgVers          = '0.1.8';     {Keep in sync with command-line converter}
  CmdLineFolderName = 'fmtoextp';  {Command-line converter's folder}
  CmdLineCvtName    = 'fmtoextp';  {Command-line converter's executable name}
  CfgFileName       = CmdLineCvtName + CfgFileExt;  {Standard config file}

type
  TMainForm = class(TForm)
    InputGroupBox: TGroupBox;
    ProjectLbl: TLabel;
    ProjectBtn: TButton;
    MoreFormsLbl: TLabel;
    MoreFormsBtn: TButton;
    FormsListBox: TListBox;
    FormsListLbl: TLabel;
    CustCfgLbl: TLabel;
    CustCfgBtn: TButton;
    CustCfgEditBtn: TButton;
    CustCfgFileNameLbl: TLabel;

    OutputGroupBox: TGroupBox;
    ProgramLbl: TLabel;
    ProgramBtn: TButton;
    ProgramFileNameLbl: TLabel;
    AddSuffixCheckBox: TCheckBox;
    SuffixEdit: TEdit;
    UpdateFormsCheckBox: TCheckBox;
    AppTitleLbl: TLabel;
    AppTitleEdit: TEdit;
    PortLbl: TLabel;
    PortEdit: TEdit;
    MaxIdleLbl: TLabel;
    MaxIdleEdit: TEdit;

    LoadBtn: TButton;
    SaveBtn: TButton;
    ClearBtn: TButton;
    ConvertBtn: TButton;

    procedure FormCreate(Sender: TObject);
    procedure ProjectBtnClick(Sender: TObject);
    procedure MoreFormsBtnClick(Sender: TObject);
    procedure CustCfgBtnClick(Sender: TObject);
    procedure CustCfgEditBtnClick(Sender: TObject);
    procedure ProgramBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure ConvertBtnClick(Sender: TObject);    
  private
    ScriptExt       : string;
    ScriptTerm      : string;
    Converter       : TFormConverterExtPascal;
    ProjInFileName  : string;  {Input "project" file}
    FmFileNames     : TStringList;
    CustCfgFileName : string;
    ProgOutFileName : string;  {Output "program" file}
    function QuoteAndReplaceHome(const FilePath       : string;
                                       LetShellExpand : Boolean) : string;
    function GetCfgFileName : string;
    function GetCmdLineCvtName : string;
    procedure CheckTargetPath;
    procedure ResetForm;
    procedure ReadForm;
    procedure SetForm;
    function LoadScriptFile(const FilePath : string;
                              var ErrMsg   : string) : Boolean;
    function SaveScriptFile(const FilePath : string;
                              var ErrMsg   : string) : Boolean;
    function FormIsComplete : Boolean;
  public
  end;

var
  MainForm: TMainForm; 

implementation

{$IFNDEF LCL}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

procedure TMainForm.FormCreate(Sender: TObject);
var
  ErrMsg : string;
begin
  FmFileNames := TStringList.Create;

{$IFDEF MSWINDOWS}
  ScriptExt := '.bat';
  ScriptTerm := 'Batch';
{$ELSE}
  ScriptExt := '.sh';
  ScriptTerm := 'Script';
{$ENDIF}
  ResetForm;
  if ParamCount > 0 then
    begin
    if SameText(ExtractFileExt(ParamStr(1)), ScriptExt) then
      begin
      if not LoadScriptFile(ParamStr(1), ErrMsg) then
        MessageDlg(ParamStr(1) + #10#10 + ErrMsg, mtError, [mbOK], 0);
      end;
    end; 
end;


procedure TMainForm.ProjectBtnClick(Sender: TObject);
var
  SelectProjDlg   : TOpenDialog;
  FileExt         : string;
  FileName        : string;
  ProjSrcFileName : string;
  FileNum         : Integer;
begin
  if (FormsListBox.Items.Count > 0) and
     (MessageDlg('This will reset the currently selected forms.', 
                 mtWarning, [mbOK, mbCancel], 0) <> mrOK) then
    Exit;

  SelectProjDlg := TOpenDialog.Create(Application);
  try
    SelectProjDlg.Filter :=
     'Projects/Forms (*.dpr; *.lpi; *.dfm; *.lfm)|*.dpr; *.lpi; *.dfm; *.lfm';
    SelectProjDlg.Options := [ofFileMustExist,ofPathMustExist,ofHideReadOnly];
    SelectProjDlg.Title := 'Select Project or Main Form';
    SelectProjDlg.FileName := ProjInFileName;
    if not SelectProjDlg.Execute then
      Exit;
    FileExt := ExtractFileExt(SelectProjDlg.FileName);
    if (not SameText(FileExt, '.dpr')) and
       (not SameText(FileExt, '.lpi')) and
       (not SameText(FileExt, '.dfm')) and
       (not SameText(FileExt, '.lfm')) then
      begin
      MessageDlg('Select a Delphi or Lazarus project or form file.', 
                 mtError, [mbOK], 0);
      Exit;
      end;
    FileName := SelectProjDlg.FileName;
  finally
    SelectProjDlg.Free;
    end;

  FmFileNames.Clear;
  if SameText(FileExt, '.dfm') or SameText(FileExt, '.lfm') then
    begin
    FmFileNames.Add(FileName);
    ProjInFileName := '';
    ProgOutFileName := '';
    end
  else
    begin
    if not Converter.GetFormFiles(FileName, FmFileNames, ProjSrcFileName) then
      begin
      MessageDlg(Converter.ErrMsg, mtError, [mbOK], 0);
      Exit;
      end;
    ProjInFileName := FileName;
     {Use project's source file name or input project file name as default}
    if ProjSrcFileName <> '' then
      ProgOutFileName := ProjSrcFileName
    else if SameText(FileExt, '.dpr') then
      ProgOutFileName := FileName;  
    end;

  FormsListBox.Clear;
  for FileNum := 0 to FmFileNames.Count-1 do
    begin
    FormsListBox.Items.Add(ExtractFileName(FmFileNames[FileNum]));
    if ProjInFileName <> '' then
      FormsListBox.Items.Strings[FormsListBox.Items.Count-1] :=
       FormsListBox.Items.Strings[FormsListBox.Items.Count-1] +
       '  [' + ExtractFileName(ProjInFileName) + ']';
    end;
    
  MoreFormsBtn.Enabled := FormsListBox.Items.Count > 0;
  CustCfgEditBtn.Enabled := MoreFormsBtn.Enabled;
end;


procedure TMainForm.MoreFormsBtnClick(Sender: TObject);
var
  SelectFormDlg : TOpenDialog;
  FileExt       : string;
  FileNum       : Integer;
begin
  SelectFormDlg := TOpenDialog.Create(Application);
  try
    SelectFormDlg.Filter :=
     'Forms (*.dfm; *.lfm)|*.dfm; *.lfm';
    SelectFormDlg.Options := [ofFileMustExist,ofPathMustExist,ofHideReadOnly,
                              ofAllowMultiSelect];
    SelectFormDlg.Title := 'Select Form(s)';
    if not SelectFormDlg.Execute then
      Exit;
    for FileNum := 0 to SelectFormDlg.Files.Count-1 do
      begin
      FileExt := ExtractFileExt(SelectFormDlg.Files[FileNum]);
      if SameText(FileExt, '.dfm') or
         SameText(FileExt, '.lfm') then
        begin       
        FmFileNames.Add(SelectFormDlg.Files[FileNum]);
        FormsListBox.Items.Add(ExtractFileName(SelectFormDlg.Files[FileNum]));
        end;
      end;
  finally
    SelectFormDlg.Free;
    end;
end;


procedure TMainForm.CustCfgBtnClick(Sender: TObject);
var
  SelectIniDlg : TOpenDialog;
begin
  SelectIniDlg := TOpenDialog.Create(Application);
  try
    SelectIniDlg.Filter :=
     'Config Files (*' + CfgFileExt + ')|*' + CfgFileExt;
    SelectIniDlg.Options := [ofFileMustExist,ofPathMustExist,ofHideReadOnly];
    SelectIniDlg.Title := 'Select Custom Config File';
    SelectIniDlg.FileName := CustCfgFileName;
    if not SelectIniDlg.Execute then
      Exit;
    if not SameText(ExtractFileExt(SelectIniDlg.FileName), CfgFileExt) then
      begin
      MessageDlg('Select a custom config file (' + CfgFileExt + ').', 
                 mtError, [mbOK], 0);
      Exit;
      end;
    if SameText(ExtractFileName(SelectIniDlg.FileName), CfgFileName) then
      begin
      MessageDlg('Don''t select the standard config file.',
                 mtError, [mbOK], 0);
      Exit;
      end; 
    CustCfgFileName := SelectIniDlg.FileName;
    CustCfgFileNameLbl.Caption := '[' + ExtractFileName(CustCfgFileName) + ']';
  finally
    SelectIniDlg.Free;
    end;
end;


procedure TMainForm.CustCfgEditBtnClick(Sender: TObject);
var
  UnmappedClasses : TStringList;
  CfgClasses      : TStringList;
begin
  ReadForm;
                                        
  UnmappedClasses := TStringList.Create;
  CfgClasses := TStringList.Create;
  try
    if not Converter.GetUnmappedClasses(UnmappedClasses, CfgClasses) then
      begin
      MessageDlg(Converter.ErrMsg, mtError, [mbOK], 0);
      Exit;
      end;
      
    CustConfigEditor := TCustConfigEditor.Create(Application);
    try
      CustConfigEditor.LoadEditor(
       UnmappedClasses, CfgClasses, Converter.FmFileNames, CustCfgFileName);
      CustConfigEditor.ShowModal;
      if CustConfigEditor.NewCustCfgFileName <> '' then
        begin  {Custom config file was saved under different name, so switch}
        CustCfgFileName := CustConfigEditor.NewCustCfgFileName;
        CustCfgFileNameLbl.Caption := '[' + ExtractFileName(CustCfgFileName) + ']';
        end;
    finally
      CustConfigEditor.Free;
    end;
  finally
    UnmappedClasses.Free;
    CfgClasses.Free;
  end;
end;


procedure TMainForm.ProgramBtnClick(Sender: TObject);
var
  SelectProgDlg : TSaveDialog;
  FileExt       : string;
begin
  SelectProgDlg := TSaveDialog.Create(Application);
  try
    SelectProgDlg.Filter :=
     'Program File (*.dpr; *.lpr)|*.dpr; *.lpr';
    SelectProgDlg.DefaultExt := 'dpr';
    SelectProgDlg.Options :=
     [ofPathMustExist,ofHideReadOnly,ofNoReadOnlyReturn];
    SelectProgDlg.Title := 'Program File To Create';
    SelectProgDlg.FileName := ProgOutFileName;
    if not SelectProgDlg.Execute then
      Exit;
    FileExt := ExtractFileExt(SelectProgDlg.FileName);
    if (not SameText(FileExt, '.dpr')) and
       (not SameText(FileExt, '.lpr')) then
      begin
      MessageDlg('Select a Delphi or Lazarus program file.', 
                 mtError, [mbOK], 0);
      Exit;
      end;
    ProgOutFileName := SelectProgDlg.FileName;
    ProgramFileNameLbl.Caption := '[' + ExtractFileName(ProgOutFileName) + ']';
    CheckTargetPath;
  finally
    SelectProgDlg.Free;
    end;
end;


procedure TMainForm.LoadBtnClick(Sender: TObject);
var
  SelectScriptDlg : TOpenDialog;
  FileExt         : string;
  FileName        : string;
  ErrMsg          : string;
begin
  SelectScriptDlg := TOpenDialog.Create(Application);
  try
    SelectScriptDlg.Filter :=
     ScriptTerm + ' Files (*' + ScriptExt + ')|*' + ScriptExt;
    SelectScriptDlg.Options := [ofFileMustExist,ofPathMustExist,ofHideReadOnly];
    SelectScriptDlg.Title := 'Select ' + ScriptTerm + ' File';
    if not SelectScriptDlg.Execute then
      Exit;
    FileExt := ExtractFileExt(SelectScriptDlg.FileName);
    if not SameText(FileExt, ScriptExt) then
      begin
      MessageDlg('Select a ' + LowerCase(ScriptTerm) + ' file.', 
                 mtError, [mbOK], 0);
      Exit;
      end;
    FileName := SelectScriptDlg.FileName;
  finally
    SelectScriptDlg.Free;
    end;
  
  if not LoadScriptFile(FileName, ErrMsg) then
    MessageDlg(FileName + #10#10 + ErrMsg, mtError, [mbOK], 0);
end;


procedure TMainForm.SaveBtnClick(Sender: TObject);
var
  SelectScriptDlg : TSaveDialog;
  FileExt         : string;
  FileName        : string;
  ErrMsg          : string;
begin
  if not FormIsComplete then
    Exit;

  SelectScriptDlg := TSaveDialog.Create(Application);
  try
    SelectScriptDlg.Filter :=
     ScriptTerm + ' File (*' + ScriptExt + ')|*' + ScriptExt;
    SelectScriptDlg.DefaultExt := Copy(ScriptExt, 2, MaxInt);
    SelectScriptDlg.Options :=
     [ofOverwritePrompt,ofPathMustExist,ofHideReadOnly,ofNoReadOnlyReturn];
    SelectScriptDlg.Title := 'Save as ' + ScriptTerm + ' File';
    SelectScriptDlg.FileName :=  
     ExtractFilePath(ProgOutFileName) + 'convert' + ScriptExt;
      {Note not using DefaultExt since may or may not have period}
    if not SelectScriptDlg.Execute then
      Exit;
    FileExt := ExtractFileExt(SelectScriptDlg.FileName);
    if not SameText(FileExt, ScriptExt) then
      begin
      MessageDlg('Select a ' + LowerCase(ScriptTerm) + ' file.', 
                 mtError, [mbOK], 0);
      Exit;
      end;
    FileName := SelectScriptDlg.FileName;
  finally
    SelectScriptDlg.Free;
    end;

  if not SaveScriptFile(FileName, ErrMsg) then
    MessageDlg(FileName + #10#10 + ErrMsg, mtError, [mbOK], 0);

end;


procedure TMainForm.ClearBtnClick(Sender: TObject);
begin
  ResetForm;
end;


procedure TMainForm.ConvertBtnClick(Sender: TObject);
begin
  if not FormIsComplete then
    Exit;
  if MessageDlg('Converted files will be created in:'#10#10 +
                ExcludeTrailingPathDelimiter(ExtractFilePath(ProgOutFileName)),
                mtConfirmation, [mbOK, mbCancel], 0) <> mrOK then
    Exit;

  ReadForm;

  if Converter.ConvertForms then
    MessageDlg('Conversion successful!', mtInformation, [mbOK], 0)
  else
    MessageDlg(Converter.ErrMsg, mtError, [mbOK], 0);
end;


function TMainForm.QuoteAndReplaceHome(const FilePath       : string;
                                             LetShellExpand : Boolean) : string;
 {On non-Windows platforms, if FilePath starts with user's home
   directory, replace it with tilde to shorten path and make it 
   more general and anonymous.
  Note that shell won't expand tilde in quoted path, so don't
   replace if LetShellExpand is True and path must be quoted
   because of embedded space.}
begin
  Result := FilePath;
{$IFNDEF MSWINDOWS}
  if ((not LetShellExpand) or (Pos(' ', Result) = 0)) and
     (GetUserDir <> '') and
     (Copy(Result, 1, Length(GetUserDir)) = GetUserDir) then
    Result := '~/' + Copy(Result, Length(GetUserDir)+1, MaxInt);
{$ENDIF}
  if Pos(' ', Result) > 0 then
    Result := '"' + Result + '"';
end;


function TMainForm.GetCfgFileName : string;
 {Return likely path to standard config file based on
   location of GUI converter.}
begin
  Result := ExtractFilePath(ParamStr(0));  {Path to GUI converter}
  if FileExists(Result + CfgFileName) then  {Is config file with GUI converter?}
    Result := Result + CfgFileName 
  else  {Assume config file is relative to GUI converter}
    begin
  {$IFDEF DARWIN}  {Back up to folder where .app is located}
    Result := StripLastDir(StripLastDir(StripLastDir(Result)));
  {$ENDIF}
    Result := StripLastDir(Result);  {Move up one folder level}
    Result := Result + CmdLineFolderName + PathDelim + CfgFileName;
     {Move down one level into expected folder} 
    end;
end;


function TMainForm.GetCmdLineCvtName : string;
 {Return likely path to command line converter based on
   location of GUI converter.}
begin
  Result := ExtractFilePath(ParamStr(0));  {Path to GUI converter}
{$IFDEF MSWINDOWS}
  if FileExists(Result + CmdLineCvtName + '.exe') then
{$ELSE}
  if FileExists(Result + CmdLineCvtName) then
{$ENDIF}
    begin  {With GUI converter}
    Result := Result + CmdLineCvtName;  {Don't worry about extension}
    Exit;
    end;
    
  {Assume relative to GUI converter}
{$IFDEF DARWIN}  {Back up to folder where .app is located}
  Result := StripLastDir(StripLastDir(StripLastDir(Result)));
{$ENDIF}
  Result := StripLastDir(Result) + CmdLineFolderName + PathDelim + CmdLineCvtName;
end;


procedure TMainForm.CheckTargetPath;
 {If converted project's target path is same as input project's
   path, check add-suffix box and disable box so can't be unchecked
   since can't convert in same folder with same program and unit 
   file names as input files.}
var
  TargetPath : string;
begin
  AddSuffixCheckBox.Enabled := True;
  if ProgOutFileName = '' then
    Exit;
  if (ProjInFileName <> '') or (FmFileNames.Count > 0) then
    begin 
    if ProjInFileName <> '' then
      TargetPath := ExtractFilePath(ProjInFileName)
    else
      TargetPath := ExtractFilePath(FmFileNames[0]);
    if TargetPath = ExtractFilePath(ProgOutFileName) then
      begin
      AddSuffixCheckBox.Checked := True;
      AddSuffixCheckBox.Enabled := False;
      end;
    end;      
end;


procedure TMainForm.ResetForm;
 {Create new converter object and clear form.}
begin
  Converter.Free;
  Converter := TFormConverterExtPascal.Create;

  ProjInFileName := '';
  FormsListBox.Clear;
  FmFileNames.Clear;
  MoreFormsBtn.Enabled := False;  {Disable until project or main form selected}
  CustCfgFileName := '';
  CustCfgEditBtn.Enabled := False;
  CustCfgFileNameLbl.Caption := '';

  ProgOutFileName := '';
  ProgramFileNameLbl.Caption := '';
  AddSuffixCheckBox.Checked := True;
  CheckTargetPath;
  SuffixEdit.Text := Converter.NameSuffix;
  UpdateFormsCheckBox.Checked := True;
  AppTitleEdit.Text := 'My App';
  PortEdit.Text := IntToStr(Converter.PortNum);
  MaxIdleEdit.Text := IntToStr(Converter.MaxIdleMinutes);
end;


procedure TMainForm.ReadForm;
 {Set converter properties from form data.}
begin
  Converter.CfgFileName := GetCfgFileName;
  Converter.FmFileNames.Clear;
  Converter.FmFileNames.AddStrings(FmFileNames);
  Converter.CustCfgFileName := CustCfgFileName;
  Converter.ProgFileName := ProgOutFileName;
  Converter.Options := [];
  if AddSuffixCheckBox.Checked then
    Converter.Options := Converter.Options + [opFmToExtP_AddSuffixToName];
  Converter.NameSuffix := SuffixEdit.Text;
  if UpdateFormsCheckBox.Checked then
    Converter.Options := Converter.Options + [opFmToExtP_UpdatePasFiles];
  Converter.AppTitle := AppTitleEdit.Text;
  Converter.MaxIdleMinutes := StrToInt(MaxIdleEdit.Text);
  Converter.PortNum := StrToInt(PortEdit.Text);
  Converter.GeneratorName := ProgName;
  Converter.GeneratorVersion := ProgVers;
end;


procedure TMainForm.SetForm;
 {Set form data from converter properties.}
var
  FileNum : Integer;
begin
   {Assume ResetForm previously called to clear list box, etc.}

   {Assume Converter has been loaded}
  FmFileNames.Clear;
  FmFileNames.AddStrings(Converter.FmFileNames);
  for FileNum := 0 to FmFileNames.Count-1 do
    begin
    FormsListBox.Items.Add(ExtractFileName(FmFileNames[FileNum]));
    if ProjInFileName <> '' then
      FormsListBox.Items.Strings[FormsListBox.Items.Count-1] :=
       FormsListBox.Items.Strings[FormsListBox.Items.Count-1] +
       '  [' + ExtractFileName(ProjInFileName) + ']';
    end;   
  MoreFormsBtn.Enabled := True;
  CustCfgEditBtn.Enabled := True;
  CustCfgFileName := Converter.CustCfgFileName;
  if CustCfgFileName <> '' then
    CustCfgFileNameLbl.Caption := '[' + ExtractFileName(CustCfgFileName) + ']';

  ProgOutFileName := Converter.ProgFileName;
  ProgramFileNameLbl.Caption := 
   '[' + ExtractFileName(ProgOutFileName) + ']';
  AddSuffixCheckBox.Checked := opFmToExtP_AddSuffixToName in Converter.Options;
  CheckTargetPath;
  SuffixEdit.Text := Converter.NameSuffix;
  UpdateFormsCheckBox.Checked := opFmToExtP_UpdatePasFiles in Converter.Options;
  AppTitleEdit.Text := Converter.AppTitle;
  PortEdit.Text := IntToStr(Converter.PortNum);
  MaxIdleEdit.Text := IntToStr(Converter.MaxIdleMinutes);
end;


function TMainForm.LoadScriptFile(const FilePath : string;
                                    var ErrMsg   : string) : Boolean;
 {Load converter from script file and set form data.}
var
  IsBatch     : Boolean;
  InF         : TextFile;
  FoundCvt    : Boolean;
  InStr       : string;
  CmdParams   : array of string;
  PrevCharPos : Integer;
  InQuotes    : Boolean;
  PrevChar    : Char;
  CharPos     : Integer;
begin
  Result := False;
  if not FileExists(FilePath) then
    begin
    ErrMsg := 'File does not exist.';
    Exit;
    end;
    
  IsBatch := SameText(ExtractFileExt(FilePath), '.bat');  
    
  AssignFile(InF, FilePath);
  Reset(InF);
  
   {Find first line containing command-line converter}
  FoundCvt := False;
  repeat
    ReadLn(InF, InStr);
    if not ((IsBatch and SameText(Copy(InStr, 1, 4), 'rem ')) or
            ((not IsBatch) and (Copy(InStr, 1, 1) = '#'))) then {Not comment?}
      begin
      if Pos(LowerCase(CmdLineCvtName), LowerCase(InStr)) > 0 then
        FoundCvt := True;  {Assume this is the converter command line}
      end;
  until FoundCvt or Eof(InF);
  CloseFile(InF);
  if not FoundCvt then
    begin
    ErrMsg := 'Can''t find command-line converter in file.'; 
    Exit;
    end; 
    
  ResetForm;

   {Parse file's command line}
  InStr := Trim(InStr);
  SetLength(CmdParams, 0);
  InQuotes := False;
  PrevCharPos := 1;
  PrevChar := ' ';  {Set to something}
  InStr := InStr + ' ';  {To get last param on command line}
  for CharPos := 1 to Length(InStr) do
    begin
    if Copy(InStr, CharPos, 1) = '"' then
      begin
      InQuotes := not InQuotes;
      PrevChar := '"';
      end
    else if Copy(InStr, CharPos, 1) = ' ' then
      begin
      if (not InQuotes) and (PrevChar <> ' ') then
        begin
        SetLength(CmdParams, Length(CmdParams)+1);
        CmdParams[High(CmdParams)] := 
         Trim(AnsiDequotedStr(
               Trim(Copy(InStr, PrevCharPos, CharPos-PrevCharPos)), '"'));
        PrevCharPos := CharPos;
        end;
      PrevChar := ' ';
      end
    else
      PrevChar := InStr[CharPos];
    end;  

  if not Converter.ParseCmdLine(CmdParams, ProjInFileName) then
    begin
    ErrMsg := Converter.ErrMsg;
    Exit;
    end;

  SetForm;
  Result := True;
end;


function TMainForm.SaveScriptFile(const FilePath : string;
                                    var ErrMsg   : string) : Boolean;
 {Create script file from form data.}
var
  OutF    : TextFile;
  FileNum : Integer;
begin
  Result := False;

  AssignFile(OutF, FilePath);
  try 
    Rewrite(OutF);
  except
    on EInOutError do
      begin
      ErrMsg := 'Unable to create file.';
      Exit;
      end;
  end;

  Write(OutF, QuoteAndReplaceHome(GetCmdLineCvtName, True), ' ');
  if ProjInFileName <> '' then
    Write(OutF, QuoteAndReplaceHome(ProjInFileName, False), ' ');
  for FileNum := 0 to FormsListBox.Items.Count-1 do
    begin
    if Pos(' [', FormsListBox.Items.Strings[FileNum]) = 0 then
      Write(OutF, QuoteAndReplaceHome(FmFileNames[FileNum], False), ' ');
    end;
  Write(OutF, QuoteAndReplaceHome(ProgOutFileName, False), ' ');
  if CustCfgFileName <> '' then
    Write(OutF, '-c=', QuoteAndReplaceHome(CustCfgFileName, False), ' ');
  if AddSuffixCheckBox.Checked then
    Write(OutF, '-e="', SuffixEdit.Text, '" ');
  Write(OutF, '-m=', MaxIdleEdit.Text, ' ');
  Write(OutF, '-p=', PortEdit.Text, ' ');
  Write(OutF, '-t="', AppTitleEdit.Text, '"');
  if UpdateFormsCheckBox.Checked then
    Write(OutF, ' -u');
  WriteLn(OutF);
  CloseFile(OutF);
  Result := True;
end;


function TMainForm.FormIsComplete : Boolean;
 {Return True if form data is complete; otherwise display
   error message.}
var
  MaxIdle : Integer;
  PortNum : Integer;
begin
  Result := False;
  
  if FmFileNames.Count = 0 then
    begin
    MessageDlg('You must select at least one form file to convert.', 
               mtError, [mbOK], 0);
    Exit;
    end;

  if ProgramFileNameLbl.Caption = '' then
    begin
    MessageDlg('You must select a program output file.', mtError, [mbOK], 0);
    Exit;
    end;

  if AddSuffixCheckBox.Checked and
     (Trim(SuffixEdit.Text) = '') then
    begin
    MessageDlg('You must specify a suffix to use.', mtError, [mbOK], 0);
    ActiveControl := SuffixEdit;
    Exit;
    end;

  MaxIdle := StrToIntDef(MaxIdleEdit.Text, -1);
  if (MaxIdle < 1) or (MaxIdle > 65535) then
    begin
    MessageDlg('Invalid maximum idle minutes.', mtError, [mbOK], 0);
    ActiveControl := MaxIdleEdit;
    Exit;
    end;
    
  PortNum := StrToIntDef(PortEdit.Text, -1);
  if (PortNum < 0) or (PortNum > 65535) then
    begin
    MessageDlg('Invalid TCP/IP port number.', mtError, [mbOK], 0);
    ActiveControl := PortEdit;
    Exit;
    end;
    
  Result := True;
end;


end.

