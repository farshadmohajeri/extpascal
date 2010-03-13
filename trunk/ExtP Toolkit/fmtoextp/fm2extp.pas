unit Fm2ExtP;

{
  Unit that converts Delphi or Lazarus form design files (.dfm or .lfm)
   to Pascal files that can be compiled against the ExtPascal units.

  Note that Delphi form files (.dfm) must be text files.
   
  Author:     Phil Hess.
  Copyright:  Copyright (C) 2009 Phil Hess. All rights reserved.
  License:    Modified LGPL.
}

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF} 
{$R+,Q+}

interface

uses
  SysUtils,
  Classes,
  IniFiles;

type
  TFmToExtPOption  = (opFmToExtP_AddExtToName);
  TFmToExtPOptions = set of TFmToExtPOption;

function ConvertFormToExtP(const CfgFileName      : string;
                           const FmFileNames      : array of string;
                           const PrjFileName      : string;
                           const Options          : TFmToExtPOptions;
                           const GeneratorName    : string;
                           const GeneratorVersion : string;
                             var ErrMsg           : string) : Boolean;


const
  CfgFileExt        = '.ini';    {Extension for file with same name as program
                                   containing component and property mappings}
  DelProjSrcFileExt = '.dpr';    {Delphi project source code file extension
                                   ("program" file)}
  LazProjSrcFileExt = '.lpr';    {Lazarus project source code file extension
                                   ("program" file)}
  LazProjInfFileExt = '.lpi';    {Lazarus project information flle extension
                                   (an XML file)}
  PasFileExt        = '.pas';    {Pascal source code file extension}
  PasAltFileExt     = '.pp';     {Pascal source code alternate file extension
                                   occasionally used for some reason with Laz}
  IncFileExt        = '.inc';    {Pascal source code include file extension}                                 
  DelFormFileExt    = '.dfm';    {Delphi form design file extension}
  LazFormFileExt    = '.lfm';    {Lazarus form design file extension}
  
  NameSuffixExt     = '_ext';    {Add this to names of files with
                                   opFmToExtP_AddExtToName option}


implementation


function ConvertValue(const ValStr       : string;
                      const FmPropName   : string;
                      const ExtClassName : string;
                      const ExtPropName  : string) : string;
 {Convert form property value to ExtPascal property value.}
var
  PrevAmp : Boolean;
  CurChar : Char;
  CharIdx : Integer;
begin
  Result := '';
  if Copy(ValStr, 1, 1) = '''' then  {Quoted string?}
    begin
    PrevAmp := False;
    CharIdx := 0;
    while CharIdx < Length(ValStr) do
      begin
      Inc(CharIdx);
      CurChar := ValStr[CharIdx];
      if CurChar = '&' then
        begin
        if PrevAmp then  {Escaped?}
          begin
          Result := Result + CurChar;
          PrevAmp := False
          end
        else
          PrevAmp := True;
        end 
      else if CurChar = '''' then
        begin
        Result := Result + CurChar;
        if Copy(ValStr, CharIdx+1, 4) = '#39''' then
          CharIdx := CharIdx + 3;
        PrevAmp := False;
        end
      else  {Not ampersand or apostrophe} 
        begin
        Result := Result + CurChar;
        PrevAmp := False;
        end;  
      end;
    end
    
  else if SameText(ValStr, 'taLeftJustify') then
    Result := '''text-align:left'''
  else if SameText(ValStr, 'taCenter') then
    Result := '''text-align:center'''
  else if SameText(ValStr, 'taRightJustify') then
    Result := '''text-align:right'''

  else if SameText(ValStr, 'csDropDown') then
    Result := 'True'  {Editable}
  else if SameText(ValStr, 'csDropDownList') then
    Result := 'False'  {Editable}
  else if SameText(ValStr, 'csSimple') then
    Result := 'True'  {Editable}

  else if SameText(FmPropName, 'Enabled') and 
          SameText(ExtPropName, 'Disabled') then
    begin  {Convert Enabled to Disabled}
    if SameText(ValStr, 'True') then
      Result := 'False'
    else
      Result := 'True';
    end

  else if SameText(FmPropName, 'Visible') and 
          SameText(ExtPropName, 'Hidden') then
    begin  {Convert Visible to Hidden}
    if SameText(ValStr, 'True') then
      Result := 'False'
    else
      Result := 'True';
    end

  else if SameText(ExtPropName, 'ActiveTab') then
    Result := '''' + ValStr + ''''
     {Convert to string}

  else  {Not a quoted string}
    Result := ValStr;

end;  {ConvertValue}


function BlankStr(Count : Integer) : string;
begin
  Result := StringOfChar(' ', Count);
end;  {BlankStr}


function GetFmPropName(const InStr : string) : string;
 {Extract form property name from input string.}
begin
  Result := Trim(Copy(InStr, 1, Pos('=', InStr)-1));
end;  {GetFmPropName}


function GetFmPropVal(const InStr : string) : string;
 {Extract form property value from input string.}
begin
  Result := Trim(Copy(InStr, Pos('=', InStr)+1, MaxInt));
end;  {GetFmPropVal}


function GetFmClassName(const FormClassName : string) : string;
 {Return form component's ancestor.
  FormClassName is the form's class name from
   the form design file.}
begin
   {Since ancestor class not stored in form design file,
     try to determine it from form class name.}
  if Pos('window', LowerCase(FormClassName)) > 0 then  {TExtWindow descendant?}
    Result := 'TExtWindow'
  else  {Assume it's a VCL/LCL form descendant}
    Result := 'TForm'; 
end;  {GetFmClassName}


function GetExtPropName(const ValStr : string) : string;
var
  ColonPos : Integer;
begin
  ColonPos := Pos(':', ValStr);
  if ColonPos = 0 then
    ColonPos := Succ(Length(ValStr));
  Result := Trim(Copy(ValStr, 1, Pred(ColonPos)));
end;  {GetExtPropName}


procedure ParseProp(const PropSpec    : string;
                      var DesignProp  : string;
                      var InvertVal   : Boolean;
                      var PropType    : string;
                      var PropDefault : string;
                      var PropInit    : string);
var
  TempStr : string;
begin
  TempStr := Trim(Copy(PropSpec, Pos(':', PropSpec)+1, MaxInt));
  DesignProp := Trim(Copy(TempStr, 1, Pos(':', TempStr)-1));
  InvertVal := False;
  if Copy(DesignProp, 1, 1) = '!' then  {Reverse meaning of property?}
    begin
    DesignProp := Copy(DesignProp, 2, MaxInt);
    InvertVal := True;
    end;
  PropType := Trim(Copy(TempStr, Pos(':', TempStr)+1, MaxInt));
  PropDefault := '';
  PropInit := '';
  if Pos(':', PropType) > 0 then  {Has default value?}
    begin
    PropDefault := Trim(Copy(PropType, Pos(':', PropType)+1, MaxInt)); 
    PropType := Trim(Copy(PropType, 1, Pos(':', PropType)-1));
    if Pos(':', PropDefault) > 0 then  {Has initial value?}
      begin
      PropInit := Trim(Copy(PropDefault, Pos(':', PropDefault)+1, MaxInt)); 
      PropDefault := Trim(Copy(PropDefault, 1, Pos(':', PropDefault)-1));
      end;
    end;
end;  {ParseProp}


function IsEventProp(const PropSpec : string) : Boolean;
var
  DesignProp  : string;
  InvertVal   : Boolean;
  PropType    : string;
  PropDefault : string;
  PropInit    : string;
begin
  Result := False;
  ParseProp(PropSpec, DesignProp, InvertVal, PropType, PropDefault, PropInit);
  if (Length(PropType) > 5) and
     SameText('Event', Copy(PropType, Length(PropType)-4, 5)) then
    Result := True;
end;  {IsEventProp} 


function CreateOutputFile(  var OutF            : TextFile;
                          const FileName        : string;
                                UseNullIfExists : Boolean;
                            var ErrMsg          : string) : Boolean;
 {Create output file. If UseNullIfExists and file with specified
   name already exists, create file on null device instead.}
begin
  Result := False;
  if FileExists(FileName) and UseNullIfExists then
{$IFDEF MSWINDOWS}  {Just discard output so don't overwrite user edits}
    AssignFile(OutF, 'NUL')
{$ELSE}
    AssignFile(OutF, '/dev/null')
{$ENDIF}
  else
    AssignFile(OutF, FileName);

  try 
    Rewrite(OutF);
    Result := True;
  except
    on EInOutError do
      begin
      ErrMsg := 'Unable to create file: ' + FileName;
      end;
    end;
end;  {CreateOutputFile}


function ConvertFormToExtP(const CfgFileName      : string;
                           const FmFileNames      : array of string;
                           const PrjFileName      : string;
                           const Options          : TFmToExtPOptions;
                           const GeneratorName    : string;
                           const GeneratorVersion : string;
                             var ErrMsg           : string) : Boolean;
 {Using mappings in CfgFileName, converts FmFileNames file(s) to
   PrjFileName ("program" file) and units based on FmFileNames forms. 
   If error, returns False and error message in ErrMsg.
  If GeneratorName is non-blank, GeneratorName and GeneratorVersion
   are indicated in comments at the top of output code file for 
   documentation.}

const
  PortDef           = 2014; {Default port for generated apps}
  MaxIdleMinutesDef = 5;    {Default number of idle minutes before apps quit}

  MaxNestedObjs     = 20;   {Maximum depth of nested controls on form}
  IndentInc         = 2;    {Indent code this many spaces per "with" level}

var
  CfgFileObj        : TMemIniFile;
  FmFileVar         : TextFile;
  PasFileVar        : TextFile;
  ThrdFileVar       : TextFile;
  IncFileVar        : TextFile;
  ControlProps      : TStringList;
  DefaultProps      : TStringList;
  ClassProps        : TStringList;
  GridColIndexes    : TStringList;
  JsLibs            : TStringList;
  TargetPath        : string;
  ProgName          : string;
  ThreadClassName   : string;
  ThreadFileName    : string;
  ThrdFileExists    : Boolean;
  ThrdIsOkay        : Boolean;
  FormNum           : Integer;
  UsesNum           : Integer;
  UsesName          : string;
  InStr             : string;
  ObjLevel          : Integer;
  ObjName           : string;
  FormObjName       : string;
  FormClassName     : string;
  FmClassName       : string;
  FmPropName        : string;
  FmPropVal         : string;
  ExtClassName      : string;
  ExtPropName       : string;
  DefPropIdx        : Integer;
  DefPropStr        : string;
  UnitName          : string;
  UnitFileName      : string;
  IncFileName       : string;
  IncIsOkay         : Boolean;
  IsDfm             : Boolean;
  FormHeight        : Integer;
  FormWidth         : Integer;
  CheckDefaults     : Boolean;
  IgnoreObj         : array [1..MaxNestedObjs] of Boolean;
  ItemStrDone       : Boolean;
  ItemStrCnt        : Integer;
  GridColIdx        : Integer;
  ColDataDone       : Boolean;
  ColWidth          : string;
  JsLibIdx          : Integer;

begin
  Result := False;
  ErrMsg := 'Unexpected error';
  
  if not FileExists(CfgFileName) then
    begin
    ErrMsg := 'Can''t load program configuration file ' + CfgFileName;
    Exit;
    end;
  
  for FormNum := 0 to High(FmFileNames) do
    begin
    if not FileExists(FmFileNames[FormNum]) then
      begin
      ErrMsg := 'Form file does not exist: ' + FmFileNames[FormNum];
      Exit;
      end;
    end;

   {Create Pascal program file}
  if not CreateOutputFile(PasFileVar, PrjFileName, True, ErrMsg) then
    Exit;

  TargetPath := ExtractFilePath(PrjFileName);
  ProgName := ExtractFileName(PrjFileName);
  ProgName := Copy(ProgName, 1,
                   Length(ProgName) - Length(ExtractFileExt(ProgName)));

  ThreadClassName := 'AppThread';
  ThreadFileName := TargetPath + LowerCase(ThreadClassName);
  ThreadFileName := ThreadFileName + PasFileExt;
  ThreadClassName := 'T' + ThreadClassName;

  Write(PasFileVar, '// Generated');
  if GeneratorName <> '' then
    Write(PasFileVar, ' by ', GeneratorName, ' ', GeneratorVersion);
  WriteLn(PasFileVar, 
          FormatDateTime('" at "hh:nn:ss" on "yyyy-mm-dd" //"', Now));
  WriteLn(PasFileVar); 

  WriteLn(PasFileVar, 'program ', ProgName, ';');
  WriteLn(PasFileVar);
  WriteLn(PasFileVar, 'uses');
  WriteLn(PasFileVar, '{$IFNDEF WebServer}');
  WriteLn(PasFileVar, '  FCGIApp,');
  WriteLn(PasFileVar, '{$ELSE}');
  WriteLn(PasFileVar, ' {$IFNDEF MSWINDOWS}');
  WriteLn(PasFileVar, '  CThreads,');
  WriteLn(PasFileVar, ' {$ENDIF}');
  WriteLn(PasFileVar, '  IdExtHTTPServer,');
  WriteLn(PasFileVar, '{$ENDIF}');
  WriteLn(PasFileVar, '  ', Copy(ThreadClassName, 2, MaxInt), ';');
  WriteLn(PasFileVar);
  WriteLn(PasFileVar, '{$IFNDEF FPC}');
  WriteLn(PasFileVar, ' {$IFNDEF WebServer}');
  WriteLn(PasFileVar, '  {$APPTYPE CONSOLE}');
  WriteLn(PasFileVar, ' {$ENDIF}');
  WriteLn(PasFileVar, '{$ENDIF}');
  WriteLn(PasFileVar);
  WriteLn(PasFileVar, 'const');
  WriteLn(PasFileVar, '  Port = ', PortDef, ';');
  WriteLn(PasFileVar, '  MaxIdleMinutes = ', MaxIdleMinutesDef, ';');
  WriteLn(PasFileVar);
  WriteLn(PasFileVar, 'begin');
  WriteLn(PasFileVar, '{$IFNDEF WebServer}');
  WriteLn(PasFileVar, '  Application := TFCGIApplication.Create(''',
                      ProgName, ''', ', ThreadClassName, 
                      ', Port, MaxIdleMinutes);');
  WriteLn(PasFileVar, '{$ELSE}');
  WriteLn(PasFileVar, '  Application := TIdExtApplication.Create(''',
                      ProgName, ''', ', ThreadClassName, 
                      ', 80, MaxIdleMinutes);');
  WriteLn(PasFileVar, '{$ENDIF}');
  WriteLn(PasFileVar, '  Application.Run;');
  WriteLn(PasFileVar, 'end.');
  CloseFile(PasFileVar);


   {Check to see if any form files have been modified, meaning
     a new thread file is needed}
  ThrdFileExists := FileExists(ThreadFileName);
  ThrdIsOkay := ThrdFileExists;
  for FormNum := 0 to High(FmFileNames) do
    begin
    if ThrdIsOkay and
       (FileAge(ThreadFileName) < FileAge(FmFileNames[FormNum])) then
      ThrdIsOkay := False;
    end;

   {Create Pascal thread unit file}
  if not CreateOutputFile(ThrdFileVar, ThreadFileName, ThrdIsOkay, ErrMsg) then
    Exit;

  WriteLn(ThrdFileVar, 'unit ', Copy(ThreadClassName, 2, MaxInt), ';');
  WriteLn(ThrdFileVar);
  WriteLn(ThrdFileVar, 'interface');
  WriteLn(ThrdFileVar);
  WriteLn(ThrdFileVar, 'uses');
  WriteLn(ThrdFileVar, '  ExtPascal,');
  for UsesNum := 0 to High(FmFileNames) do
    begin
    UsesName := ExtractFileName(FmFileNames[UsesNum]);
    UsesName := Copy(UsesName, 1,
                     Length(UsesName) - Length(ExtractFileExt(UsesName))); 
    if opFmToExtP_AddExtToName in Options then
      UsesName := UsesName + NameSuffixExt;
    Write(ThrdFileVar, '  ', UsesName);
    if UsesNum < High(FmFileNames) then
      WriteLn(ThrdFileVar, ',')
    else
      WriteLn(ThrdFileVar, ';'); 
    end;
  WriteLn(ThrdFileVar);
  WriteLn(ThrdFileVar, 'type');
  WriteLn(ThrdFileVar, '  ', ThreadClassName, ' = class(TExtThread)');
  WriteLn(ThrdFileVar, '  public');
  for UsesNum := 0 to High(FmFileNames) do
    begin
    AssignFile(FmFileVar, FmFileNames[UsesNum]);
    Reset(FmFileVar);
    ReadLn(FmFileVar, InStr);
    InStr := Trim(InStr);
    FormObjName := Copy(InStr, 8, Pos(':', InStr)-8);
    FormClassName := Trim(Copy(InStr, Pos(':', InStr)+2, MaxInt));
    WriteLn(ThrdFileVar, '    ', FormObjName, ' : ', FormClassName, ';');
    CloseFile(FmFileVar);
    end;
  WriteLn(ThrdFileVar, '  published');
  WriteLn(ThrdFileVar, '    procedure Home; override;');


   {Load configuration file}
  CfgFileObj := TMemIniFile.Create(CfgFileName);
  ControlProps := TStringList.Create;
  CfgFileObj.ReadSectionValues('TControl', ControlProps);
  DefaultProps := TStringList.Create;
  ClassProps := TStringList.Create;
  GridColIndexes := TStringList.Create;
  JsLibs := TStringList.Create;

   {Now generate Pascal form units that create ExtPascal windows (forms)}
  for FormNum := 0 to High(FmFileNames) do
    begin
     {Create Pascal form unit file}
    UnitName := ExtractFileName(FmFileNames[FormNum]);
    UnitName := Copy(UnitName, 1,
                     Length(UnitName) - Length(ExtractFileExt(UnitName)));
    if opFmToExtP_AddExtToName in Options then
      UnitName := UnitName + NameSuffixExt;
    UnitFileName := TargetPath + UnitName + PasAltFileExt;  {See if alt used}
    if not FileExists(UnitFileName) then  {Okay to assume normal extension?}
      UnitFileName := ChangeFileExt(UnitFileName, PasFileExt);
    IncFileName := TargetPath + UnitName + IncFileExt;
    if not CreateOutputFile(PasFileVar, UnitFileName, True, ErrMsg) then
      Exit;
    if not FileExists(IncFileName) then
      IncIsOkay := False
    else  {If form hasn't been modified, don't generate .inc file}
      IncIsOkay := FileAge(IncFileName) >= FileAge(FmFileNames[FormNum]);
    if not CreateOutputFile(IncFileVar, IncFileName, IncIsOkay, ErrMsg) then
      Exit;
      
    WriteLn(PasFileVar, 'unit ', UnitName, ';');
    WriteLn(PasFileVar);
    WriteLn(PasFileVar, 'interface');
    WriteLn(PasFileVar);
    WriteLn(PasFileVar, 'uses');
    WriteLn(PasFileVar, '  SysUtils, Classes,');
    WriteLn(PasFileVar, '  Ext, ExtPascal, ExtPascalUtils, ExtForm,');
    WriteLn(PasFileVar, '  ExtData, ExtGrid, ExtUtil, ExtAir, ExtDd,'); 
    WriteLn(PasFileVar, '  ExtLayout, ExtMenu, ExtDirect, ExtState, ExtTree,');
    WriteLn(PasFileVar, '  ExtUxForm;');
    WriteLn(PasFileVar);
//    WriteLn(PasFileVar, '{$M+}');  //Shouldn't be needed anymore
    WriteLn(PasFileVar);
    WriteLn(PasFileVar, 'type');

     {Scan form for objects that need to be declared}
    AssignFile(FmFileVar, FmFileNames[FormNum]);
    Reset(FmFileVar);
    IsDfm := SameText(ExtractFileExt(FmFileNames[FormNum]), DelFormFileExt);
    FormHeight := 0;
    FormWidth := 0;
    ObjLevel := 0;
    while not Eof(FmFileVar) do  
      begin
      ReadLn(FmFileVar, InStr);
      InStr := Trim(InStr);
      if SameText(Copy(InStr, 1, 7), 'object ') then  {Found object?}
        begin
        Inc(ObjLevel);
        ObjName := Copy(InStr, 8, Pos(':', InStr)-8);
        FmClassName := Trim(Copy(InStr, Pos(':', InStr)+2, MaxInt));
        if ObjLevel = 1 then  {Is form?}
          begin
          FormObjName := ObjName;
          FormClassName := FmClassName;
          FmClassName := GetFmClassName(FormClassName);
          WriteLn(PasFileVar, '  ', FormClassName, ' = class(TExtWindow)');
          end
        else
          begin
          CfgFileObj.ReadSectionValues(FmClassName, ClassProps);
          if ClassProps.Count > 0 then  {Object's class is mapped?}
            begin
            ExtClassName := ClassProps.Values['Class'];
            WriteLn(PasFileVar, '    ', ObjName, ' : ', ExtClassName, ';'); 
            if (ClassProps.IndexOfName('SetLibrary') >= 0) and  {Ux library?}
               (JsLibs.IndexOfName(ExtClassName) < 0) then
              JsLibs.Add(ExtClassName + '=' + ClassProps.Values['SetLibrary']);
            end;
          end;
        end
      else if SameText(InStr, 'end') then  {Found end of object?}
        Dec(ObjLevel)
      else  {Found property}
        begin
        if ObjLevel = 1 then  {Is form?}
          begin
           {Note: With Lazarus, Height and Width are client height/width,
             not form height/width, so increase TExtWindow's Height and
             Width so it has at least that much client area for worst
             case form (designed with Lazarus on Windows, with its 4 pixel
             thick border on left, right and bottom and 30 pixel high title 
             bar). Not a great solution, but don't really want to change 
             meaning of Lazarus form's Height and Width.}
          FmPropName := GetFmPropName(InStr);
          FmPropVal := GetFmPropVal(InStr);
          if SameText(FmPropName, 'Height') or
             SameText(FmPropName, 'ClientHeight') then
            begin
            FormHeight := StrToInt(FmPropVal);
            if SameText(FmPropName, 'Height') and IsDfm then
              Dec(FormHeight, 0)
            else
              Inc(FormHeight, 34);
            end
          else if SameText(FmPropName, 'Width') or
                  SameText(FmPropName, 'ClientWidth') then
            begin
            FormWidth := StrToInt(FmPropVal);
            if SameText(FmPropName, 'Width') and IsDfm then
              Inc(FormWidth, 0)
            else
              Inc(FormWidth, 8);
            end
          end;
        end;
      end;  {while not Eof}
    CloseFile(FmFileVar);

     {Scan form file for event handlers that can be mapped and
       add their declarations to TExtWindow descendent class}
    Reset(FmFileVar);
    ObjLevel := 0;
    while not Eof(FmFileVar) do  
      begin
      ReadLn(FmFileVar, InStr);
      InStr := Trim(InStr);
      if SameText(Copy(InStr, 1, 7), 'object ') then  {Found object?}
        begin
        Inc(ObjLevel);
        ObjName := Copy(InStr, 8, Pos(':', InStr)-8);
        if ObjLevel = 1 then  {Is form?}
          FmClassName := GetFmClassName(FormClassName)
        else  {Object on form}
          FmClassName := Trim(Copy(InStr, Pos(':', InStr)+2, MaxInt));
        CfgFileObj.ReadSectionValues(FmClassName, ClassProps);
        end
      else if SameText(InStr, 'end') then  {Found end of object?}
        Dec(ObjLevel)
      else  {Found property}
        begin
        FmPropName := GetFmPropName(InStr);
        FmPropVal := GetFmPropVal(InStr);
        if (SameText(Copy(FmPropName, 1, 2), 'On')) or
           (IsEventProp(ClassProps.Values[FmPropName])) then  {Event property?}
          begin
          ExtPropName := GetExtPropName(ClassProps.Values[FmPropName]);
          if ExtPropName <> '' then  {Is event mapped?}
            begin
            Write(PasFileVar, '    procedure ', ObjName);
            if SameText(Copy(FmPropName, 1, 2), 'On') then
              Write(PasFileVar, Copy(FmPropName, 3, MaxInt))
            else
              Write(PasFileVar, FmPropName);
            WriteLn(PasFileVar, ';');

            Write(ThrdFileVar, '    procedure ', FormObjName, '_', ObjName);
            if SameText(Copy(FmPropName, 1, 2), 'On') then
              Write(ThrdFileVar, Copy(FmPropName, 3, MaxInt))
            else
              Write(ThrdFileVar, FmPropName);
            WriteLn(ThrdFileVar, ';');
            end;
          end;
        end;
      end;  {while not Eof}
    CloseFile(FmFileVar);
    
    WriteLn(PasFileVar, '  private');
    WriteLn(PasFileVar, '  public');
    WriteLn(PasFileVar, '    constructor Create;');
    WriteLn(PasFileVar, '    procedure Show;');
    WriteLn(PasFileVar, '  end;');
    WriteLn(PasFileVar);
    WriteLn(PasFileVar);
    WriteLn(PasFileVar, 'implementation');
    WriteLn(PasFileVar);
    WriteLn(PasFileVar, 'uses');
    Write(PasFileVar, '  ', Copy(ThreadClassName, 2, MaxInt));
    if FormNum = 0 then  {Main form?}
      begin
      for UsesNum := 1 to High(FmFileNames) do
        begin
        WriteLn(PasFileVar, ',');
        UsesName := ExtractFileName(FmFileNames[UsesNum]);
        UsesName := Copy(UsesName, 1,
                         Length(UsesName) - Length(ExtractFileExt(UsesName))); 
        if opFmToExtP_AddExtToName in Options then
          UsesName := UsesName + NameSuffixExt;
        Write(PasFileVar, '  ', UsesName); 
        end;
      end;
    WriteLn(PasFileVar, ';');   
    WriteLn(PasFileVar);
    WriteLn(PasFileVar, 'constructor ', FormClassName, '.Create;');
    WriteLn(PasFileVar, 'begin');
    WriteLn(PasFileVar, '  inherited;');
    WriteLn(PasFileVar, '{$I ', ExtractFileName(IncFileName), '}');
    WriteLn(PasFileVar, 'end;  {', FormClassName, '.Create}');

     {Now generate the actual code that creates ExtPascal components}
    Reset(FmFileVar);
    GridColIndexes.Clear;
    ObjLevel := 0;
    CheckDefaults := False;
    while not Eof(FmFileVar) do  
      begin
      ReadLn(FmFileVar, InStr);
      InStr := Trim(InStr);

      if (SameText(Copy(InStr, 1, 7), 'object ') or
          SameText(InStr, 'end')) and  {New object or end of current object?}
         CheckDefaults then  {And haven't already checked?}  
        begin  {Do anything here to finish current object's properties}
         {If any default properties were not set in form for current object, 
           set them now.}
        CheckDefaults := False;
        for DefPropIdx := 0 to DefaultProps.Count-1 do  {Default properties}
          begin
          DefPropStr := DefaultProps.Strings[DefPropIdx];
          if SameText(FmClassName + '.', 
                      Copy(DefPropStr, 1, Length(FmClassName)+1)) then
            begin
            ExtPropName := Copy(DefPropStr, 
                                Length(FmClassName)+2, 
                                Pos('=', DefPropStr) - Length(FmClassName) - 2);
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)), 
                                ExtPropName, ' := ', 
                                Copy(DefPropStr, Pos('=', DefPropStr)+1, MaxInt), 
                                ';');
            end;
          end;
        end;

      if SameText(Copy(InStr, 1, 7), 'object ') then  {Found object?}
        begin
        Inc(ObjLevel);
        ObjName := Copy(InStr, 8, Pos(':', InStr)-8);
        if ObjLevel = 1 then  {Is form?}
          FmClassName := GetFmClassName(FormClassName)
        else  {Object on form}
          FmClassName := Trim(Copy(InStr, Pos(':', InStr)+2, MaxInt));
        CfgFileObj.ReadSectionValues(FmClassName, ClassProps);
        
        if (Length(FmClassName) > 5) and
           SameText(Copy(FmClassName, Length(FmClassName)-4, 5), '_Grid') then
          begin  {Column editor control column model already created with grid}
          if GridColIndexes.Values[ObjName] = '' then  {Not part of a grid?}
            begin
            IgnoreObj[ObjLevel] := True;
            ClassProps.Clear;
            end
          else
            begin
            IgnoreObj[ObjLevel] := False;
            ExtClassName := ClassProps.Values['Class'];
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel-1)),
                                'with TExtGridColumn(' +
                                GridColIndexes.Values[ObjName] + '.Columns[' +
                                IntToStr(Integer(GridColIndexes.Objects[
                                                  GridColIndexes.IndexOfName(ObjName)])) +
//                                ']) do  //', ObjName);
                                ']) do');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)), 'begin');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                                ObjName, ' := ', ExtClassName, '.Create;');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                                'Editor := ', ObjName, ';');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                                'with ', ObjName, ' do');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                                ' begin');
             {Note outputting two with statements so can reference either
               column model or column editor properties naturally.}
            end;
          end
        
        else if ClassProps.Count > 0 then  {Object's class is mapped?}
          begin
          IgnoreObj[ObjLevel] := False;
          ExtClassName := ClassProps.Values['Class'];
          if ObjLevel > 1 then
            WriteLn(IncFileVar);  {For readability}

          if ObjLevel > 1 then  {Object on form?}
            begin
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel-1)), ObjName, 
                                ' := ', ExtClassName, '.Create;');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel-1)), 
                                'with ', ObjName, '.AddTo(Items) do');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)), 'begin');
            end;

          WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)), 
                              'Id := ''', ObjName, ''';');

          if SameText(ExtClassName, 'TExtGridEditorGridPanel') and
            ((not SameText(FmClassName, 'TExtGridEditorGridPanel')) and
             (not SameText(FmClassName, 'TOvcTable'))) then
            begin  {Create dummy data store and column model objects}
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                                'Store := TExtDataStore.Create;');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                                'with TExtGridColumn.AddTo(Columns) do');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel+1)),
                                'begin');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel+1)),
                                'Id := ''', ObjName, '_Col1'';');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel+1)),
                                'Editor := TExtFormTextField.Create;');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel+1)),
                                'Header := ''', ObjName, ''';');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel+1)),
                                'Width := 100;');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel+1)),
                                'end;');
            end;
            
          if ObjLevel = 1 then  {Is form?}
            begin
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                                'Height := ', FormHeight, ';');
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                                'Width := ', FormWidth, ';');
            if FormNum = 0 then  {Is main form?}
              WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                                  'OnEsc := JSFunction('''');')
               {Don't want Escape key blanking page}
            else if SameText(FmClassName, 'TForm') then  {Not TExtWindow?}
              WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                                  'Modal := True;');
               {No way to specify Modal with TForm; just assume other
                 forms will be modal.}
            end;
          end
        else  {Object's class not mapped}
          begin
          IgnoreObj[ObjLevel] := True;
          WriteLn(IncFileVar);
          WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel-1)), 
                              '{', FmClassName, ' not mapped}');
          end;

        CheckDefaults := True;
         {Reload defaults in case any were deleted with previous object} 
        CfgFileObj.ReadSectionValues('Defaults', DefaultProps);
        end  {is object}

      else if SameText(InStr, 'end') then  {Found end of object?}
        begin
        if ObjLevel = 1 then  {Is form?}
          WriteLn(IncFileVar)
        else if not IgnoreObj[ObjLevel] then  {Object's class is mapped?}
          begin
          if (Length(FmClassName) > 5) and
            SameText(Copy(FmClassName, Length(FmClassName)-4, 5), '_Grid') then
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)), ' end;');
             {Need extra end for second with statement used with grid column}
          WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)), 'end;');
          end;
        Dec(ObjLevel);
        end  {is end of object}

      else  {Found property}
        begin
        FmPropName := GetFmPropName(InStr);
        FmPropVal := GetFmPropVal(InStr);
        if FmPropVal = '' then  {String begins on next line?}
          begin
          repeat
            ReadLn(FmFileVar, InStr);
            InStr := Trim(InStr);
            if Copy(InStr, Length(InStr)-2, 3) = ''' +' then
              FmPropVal := FmPropVal + Copy(InStr, 1, Length(InStr)-3)
            else
              FmPropVal := FmPropVal + Copy(InStr, 2, MaxInt);
          until Copy(InStr, Length(InStr), 1) <> '+'; 
          end;
        ExtPropName := '';
        if (ClassProps.IndexOfName(FmPropName) < 0) or  {Not mapped in class?}
           (GetExtPropName(ClassProps.Values[FmPropName]) <> '') then  {Or non-blank prop?}
          begin  {Okay to output property if mapped}
          if SameText(ClassProps.Values['IsControl'], 'True') then
            ExtPropName := ControlProps.Values[FmPropName];  {Try ancestor's}
          if ExtPropName = '' then
            ExtPropName := GetExtPropName(ClassProps.Values[FmPropName]);  {Use class's}  
          end;
        if ExtPropName <> '' then  {Is property mapped?}
          begin
          if (SameText(Copy(FmPropName, 1, 2), 'On')) or
             (IsEventProp(ClassProps.Values[FmPropName])) then {Event property?}
            begin
            Write(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                              'On(''', ExtPropName, ''', Ajax(CurrentThread.',
                              FormObjName, '_', ObjName);
            if SameText(Copy(FmPropName, 1, 2), 'On') then
              Write(IncFileVar, Copy(FmPropName, 3, MaxInt))
            else
              Write(IncFileVar, FmPropName);
            WriteLn(IncFileVar, '));');
            end

          else if SameText(FmPropName, 'Items.Strings') or
                  SameText(FmPropName, 'StoreArray.Strings') or
                  SameText(FmPropName, 'Lines.Strings') or
                  SameText(FmPropName, 'Value.Strings') then
            begin  {Read item strings and convert}
            Write(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                  ExtPropName, ' := ');
            if SameText(ExtClassName, 'TExtFormComboBox') or
               SameText(ExtClassName, 'TExtUxFormMultiSelect') then               
              WriteLn(IncFileVar, 'JSArray(');
            ItemStrCnt := 0;
            ItemStrDone := False;
            repeat
              ReadLn(FmFileVar, InStr);
              InStr := Trim(InStr);
              if Copy(InStr, Length(InStr), 1) = ')' then
                begin
                InStr := Copy(InStr, 1, Length(InStr)-1);
                ItemStrDone := True;
                end;
              if InStr <> '' then  {Lazarus puts ) on its own line}
                begin
                InStr := ConvertValue(InStr, '', '', '');
                if SameText(ExtClassName, 'TExtFormComboBox') or
                   SameText(ExtClassName, 'TExtUxFormMultiSelect') then
                  begin
                  if ItemStrCnt > 0 then
                    WriteLn(IncFileVar, ', '' +');
                  Write(IncFileVar, BlankStr(IndentInc*(ObjLevel)+1), 
                                    '''"', Copy(InStr, 2, Length(InStr)-2), '"');
                  end
                else if SameText(ExtClassName, 'TExtFormTextArea') or
                        SameText(ExtClassName, 'TExtFormHtmlEditor') then
                  begin
                  if ItemStrCnt > 0 then
                    WriteLn(IncFileVar, ' + ''\n'' +');  {Start new line}
                  Write(IncFileVar, BlankStr(IndentInc*(ObjLevel)+1),
                                    InStr);
                  end
                else if SameText(ExtClassName, 'TExtFormRadioGroup') then
                  begin
                  WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                                      'with TExtFormRadio.AddTo(Items) do');
                  WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel+1)),
                                      'begin');
                  WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel+1)),
                                      'Name := ''', ObjName, ''';');  {Group}
                  WriteLn(IncFilevar, BlankStr(IndentInc*(ObjLevel+1)),
                                      'BoxLabel := ', InStr, ';');
                  WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel+1)),
                                      'end;');
                  end;
                Inc(ItemStrCnt);
                end;
            until ItemStrDone;
            if not SameText(ExtClassName, 'TExtFormRadioGroup') then
              begin
              if SameText(ExtClassName, 'TExtFormComboBox') or
                 SameText(ExtClassName, 'TExtUxFormMultiSelect') then
                Write(IncFileVar, ''')');
              WriteLn(IncFileVar, ';');
              end;
            end

          else  {Normal property}
            begin
            WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)), 
                                ExtPropName, ' := ', 
                                ConvertValue(FmPropVal, FmPropName,
                                             ExtClassName, ExtPropName), 
                                ';');
            end;

          DefPropIdx :=
           DefaultProps.IndexOfName(FmClassName + '.' + ExtPropName);
          if DefPropIdx >= 0 then  {Won't need this default for class?}
            DefaultProps.Delete(DefPropIdx);
          end  {Property is mapped}
          
        else if SameText(ExtClassName, 'TExtGridEditorGridPanel') and
                SameText(FmPropName, 'ColData') then
          begin  {TOvcTable column defs reached, so special code required}
          GridColIdx := 0;
          ColDataDone := False;
          repeat  {Assume each column def is on 4 lines; width is first line}
            ReadLn(FmFileVar, ColWidth);
            ColWidth := Trim(ColWidth);
            if ColWidth = ')' then  {Lazarus puts ) on its own line}
              ColDataDone := True;
            if not ColDataDone then
              begin  
              ReadLn(FmFileVar, InStr);  {Skip}
              ReadLn(FmFileVar, InStr);  {Skip}
              ReadLn(FmFileVar, InStr);  {Column editor object}
              InStr := Trim(InStr);
              if Copy(InStr, Length(InStr), 1) = ')' then
                begin
                InStr := Copy(InStr, 1, Length(InStr)-1);
                ColDataDone := True;
                end;
              InStr := Copy(InStr, Pos('.', InStr)+1, MaxInt);  {Strip form}
              InStr := Copy(InStr, 1, Length(InStr)-1);  {Trim trailing quote}
              WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel)),
                                  'with TExtGridColumn.AddTo(Columns) do');
              WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel+1)),
                                  'begin');
              WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel+1)),
                                  'Id := ''', InStr, ''';');
              WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel+1)),
                                  'Width := ', ColWidth, ';');
              WriteLn(IncFileVar, BlankStr(IndentInc*(ObjLevel+1)),
                                  'end;');
              GridColIndexes.AddObject(InStr + '=' + ObjName,
                                       TObject(GridColIdx));
               {Save order of grid columns for use later when actual column
                 controls are encountered. Note assuming column controls
                 always come after grid where they're used.}
              Inc(GridColIdx);
              end;
          until ColDataDone;
          end;  {TOvcTable column defs}

        end;  {is property}

      end;  {while not Eof}
    CloseFile(FmFileVar);    
    
    WriteLn(PasFileVar);
    WriteLn(PasFileVar);
    WriteLn(PasFileVar, 'procedure ', FormClassName, '.Show;');
    WriteLn(PasFileVar, 'begin');
    WriteLn(PasFileVar, '  inherited Show;');
    WriteLn(PasFileVar, 'end;');
    WriteLn(PasFileVar);
    WriteLn(PasFileVar);

     {Now generate stub event handlers for form} 
    Reset(FmFileVar);
    ObjLevel := 0;
    while not Eof(FmFileVar) do  
      begin
      ReadLn(FmFileVar, InStr);
      InStr := Trim(InStr);
      if SameText(Copy(InStr, 1, 7), 'object ') then  {Found object?}
        begin
        Inc(ObjLevel);
        ObjName := Copy(InStr, 8, Pos(':', InStr)-8);
        if ObjLevel = 1 then  {Is form?}
          FmClassName := GetFmClassName(FormClassName)
        else  {Object on form}
          FmClassName := Trim(Copy(InStr, Pos(':', InStr)+2, MaxInt));
        CfgFileObj.ReadSectionValues(FmClassName, ClassProps);
        end
      else if SameText(InStr, 'end') then  {Found end of object?}
        Dec(ObjLevel)
      else  {Found property}
        begin
        FmPropName := GetFmPropName(InStr);
        FmPropVal := GetFmPropVal(InStr);
        if (SameText(Copy(FmPropName, 1, 2), 'On')) or
           (IsEventProp(ClassProps.Values[FmPropName])) then  {Event property?}
          begin
          ExtPropName := GetExtPropName(ClassProps.Values[FmPropName]);
          if ExtPropName <> '' then  {Is event mapped?}
            begin
            Write(PasFileVar, 'procedure ', FormClassName, '.', ObjName);
            if SameText(Copy(FmPropName, 1, 2), 'On') then
              Write(PasFileVar, Copy(FmPropName, 3, MaxInt))
            else
              Write(PasFileVar, FmPropName);
            WriteLn(PasFileVar, ';');
            WriteLn(PasFileVar, 'begin');
            WriteLn(PasFileVar, 'end;');
            WriteLn(PasFileVar);
            WriteLn(PasFileVar);
            end;
          end;
        end;
      end;  {while not Eof}
    WriteLn(PasFileVar, 'end.');
    CloseFile(PasFileVar);
    CloseFile(IncFileVar);
    CloseFile(FmFileVar);

    end;  {for FormNum}

  WriteLn(ThrdFileVar, '  end;');
  WriteLn(ThrdFileVar);
  WriteLn(ThrdFileVar, 'function CurrentThread : ', ThreadClassName, ';');
  WriteLn(ThrdFileVar);
  WriteLn(ThrdFileVar);
  WriteLn(ThrdFileVar, 'implementation');
  WriteLn(ThrdFileVar);
  WriteLn(ThrdFileVar, 'uses');
  WriteLn(ThrdFileVar, '{$IFNDEF WebServer}');
  WriteLn(ThrdFileVar, '  FCGIApp;');
  WriteLn(ThrdFileVar, '{$ELSE}');
  WriteLn(ThrdFileVar, '  IdExtHTTPServer;');
  WriteLn(ThrdFileVar, '{$ENDIF}');
  WriteLn(ThrdFileVar);
  WriteLn(ThrdFileVar, 'function CurrentThread : ', ThreadClassName, ';');
  WriteLn(ThrdFileVar, 'begin');
  WriteLn(ThrdFileVar, '  Result := ', ThreadClassName, '(CurrentFCGIThread);');
  WriteLn(ThrdFileVar, 'end;');
  WriteLn(ThrdFileVar);
  WriteLn(ThrdFileVar);

   {Now generate thread handlers that call form handlers} 
  for FormNum := 0 to High(FmFileNames) do
    begin
    AssignFile(FmFileVar, FmFileNames[FormNum]);
    Reset(FmFileVar);
    ObjLevel := 0;
    while not Eof(FmFileVar) do  
      begin
      ReadLn(FmFileVar, InStr);
      InStr := Trim(InStr);
      if SameText(Copy(InStr, 1, 7), 'object ') then  {Found object?}
        begin
        Inc(ObjLevel);
        ObjName := Copy(InStr, 8, Pos(':', InStr)-8);
        FmClassName := Trim(Copy(InStr, Pos(':', InStr)+2, MaxInt));
        if ObjLevel = 1 then  {Is form?}
          begin
          FormObjName := ObjName;
          FormClassName := FmClassName;
          FmClassName := GetFmClassName(FormClassName);
          if FormNum = 0 then
            begin
            WriteLn(ThrdFileVar, 'procedure ', ThreadClassName, '.Home;');
            WriteLn(ThrdFileVar, 'begin');
            for JsLibIdx := 0 to JsLibs.Count-1 do
              WriteLn(ThrdFileVar, '  SetLibrary(ExtPath + ' +
                                   JsLibs.ValueFromIndex[JsLibIdx], ');');
            WriteLn(ThrdFileVar, '  ', FormObjName, ' := ', FormClassName, '.Create;');
            WriteLn(ThrdFileVar, '  ', FormObjName, '.Show;');
            WriteLn(ThrdFileVar, 'end;');
            WriteLn(ThrdFileVar);
            WriteLn(ThrdFileVar);
            end;
          end
        else  {Object on form}
          FmClassName := Trim(Copy(InStr, Pos(':', InStr)+2, MaxInt));
        CfgFileObj.ReadSectionValues(FmClassName, ClassProps);
        end
      else if SameText(InStr, 'end') then  {Found end of object?}
        Dec(ObjLevel)
      else  {Found property}
        begin
        FmPropName := GetFmPropName(InStr);
        FmPropVal := GetFmPropVal(InStr);
        if (SameText(Copy(FmPropName, 1, 2), 'On')) or
           (IsEventProp(ClassProps.Values[FmPropName])) then  {Event property?}
          begin
          ExtPropName := GetExtPropName(ClassProps.Values[FmPropName]);
          if ExtPropName <> '' then  {Is event mapped?}
            begin
            Write(ThrdFileVar, 'procedure ', ThreadClassName, '.', 
                               FormObjName, '_', ObjName);
            if SameText(Copy(FmPropName, 1, 2), 'On') then
              Write(ThrdFileVar, Copy(FmPropName, 3, MaxInt))
            else
              Write(ThrdFileVar, FmPropName);
            WriteLn(ThrdFileVar, ';');
            WriteLn(ThrdFileVar, 'begin');
            Write(ThrdFileVar, '  ', FormObjName, '.', ObjName);
            if SameText(Copy(FmPropName, 1, 2), 'On') then
              Write(ThrdFileVar, Copy(FmPropName, 3, MaxInt))
            else
              Write(ThrdFileVar, FmPropName);
            WriteLn(ThrdFileVar, ';');
            WriteLn(ThrdFileVar, 'end;');
            WriteLn(ThrdFileVar);
            WriteLn(ThrdFileVar);
            end;
          end;
        end;
      end;  {while not Eof}
    end;  {for FormNum}

  WriteLn(ThrdFileVar, 'end.');
  CloseFile(ThrdFileVar);

  CfgFileObj.Free;
  ControlProps.Free;
  DefaultProps.Free;
  ClassProps.Free;
  GridColIndexes.Free;
  JsLibs.Free;

   {Create little custom config file. This is a way to signal to FPC to
     compile with ExtPascal runtime units, not Extp_Design_Ctrls unit.}
  if not CreateOutputFile(IncFileVar, TargetPath + 'extpascal.cfg', True, 
                          ErrMsg) then
    Exit;
  WriteLn(IncFileVar, 
          '#  Tell FPC to compile with ExtPascal runtime units, rather');
  WriteLn(IncFileVar, 
          '#   than ExtP_Design_Ctrls unit that Lazarus IDE sees.');
  WriteLn(IncFileVar, 
          '-dUseRuntime');
  WriteLn(IncFileVar);
  WriteLn(IncFileVar, 
          '#  You can put other FPC switches in this file, although it''s');
  WriteLn(IncFileVar,
          '#   probably better to put them in the .lpi file. However, don''t');
  WriteLn(IncFileVar,
          '#   put the above define in the .lpi file or it will confuse');
  WriteLn(IncFileVar,
          '#   Lazarus when working with the ExtPascal design controls.');
  WriteLn(IncFileVar);
  CloseFile(IncFileVar);

  ErrMsg := '';
  Result := True;
end;  {ConvertFormToExtP}


end.

