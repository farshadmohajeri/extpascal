unit Fm2ExtP;

{
  Class that converts Delphi or Lazarus form design files (.dfm or .lfm)
   to Pascal files that can be compiled against the ExtPascal units.

  Note that Delphi form files (.dfm) must be text files.
   
  Author:     Phil Hess.
  Copyright:  Copyright (C) 2009-2010 Phil Hess. All rights reserved.
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
  IniFiles,
  Fm2Base;

type
  TFmToExtPOption  = (opFmToExtP_AddSuffixToName,
                      opFmToExtP_ReformatForLaz);
  TFmToExtPOptions = set of TFmToExtPOption;

  TFormConverterExtPascal = class(TFormConverterBase)
  private
    FPortNum        : Integer;  {Port for generated apps}
    FMaxIdleMinutes : Integer;  {Number of idle minutes before app quits}
    FOptions        : TFmToExtPOptions;  {Conversion options}
    FNameSuffix     : string;   {Add to file names with AddSuffixToName option}

  protected
    function ConvertValue(const ValStr       : string;
                          const FmPropName   : string;
                          const ExtClassName : string;
                          const ExtPropName  : string) : string;
    function GetAncestorName(const FormClassName : string) : string;
    function GetExtPropName(const ValStr : string) : string;
    procedure ReadClassProps(    CfgF        : TMemIniFile;
                                 CustCfgF    : TMemIniFile;
                             var FmClassName : string;
                                 ClassProps  : TStringList);
    function IsEventProp(const PropSpec : string) : Boolean;
    function IsGridColumn(const FmClassName : string) : Boolean;
    function Convert : Boolean;

  public
    property PortNum : Integer read FPortNum write FPortNum;
    property MaxIdleMinutes : Integer read FMaxIdleMinutes write FMaxIdleMinutes;
    property Options : TFmToExtPOptions read FOptions write FOptions;
    property NameSuffix : string read FNameSuffix write FNameSuffix;

    constructor Create;
    function ConvertForms : Boolean;
  end;


implementation

constructor TFormConverterExtPascal.Create;
begin
  inherited;
  PortNum := 2014;
  MaxIdleMinutes := 5;
end;

function TFormConverterExtPascal.ConvertValue(const ValStr       : string;
                                              const FmPropName   : string;
                                              const ExtClassName : string;
                                              const ExtPropName  : string) : string;
 {Convert form property value to ExtPascal property value.}

type
  THtmlColorRec = record
    Name  : string;
    Value : string;
    end;
const
   {HTML/CSS color equivalents to Delphi color constants.
     See http://www.w3.org/TR/REC-CSS2/ui.html#system-colors
    Colors like clBtnFace differ by platform and by user preferences on
     a particular computer. RGB values below were generated on a Windows
     computer with "Windows XP style" and "Default (blue)" color scheme.}
  HtmlColors : array [0..51] of THtmlColorRec =
   ((Name: 'clBlack'; Value: 'Black'),  //#000000
    (Name: 'clMaroon'; Value: 'Maroon'),  //#800000
    (Name: 'clGreen'; Value: 'Green'),  //#008000
    (Name: 'clOlive'; Value: 'Olive'),  //#808000
    (Name: 'clNavy'; Value: 'Navy'),  //#000080
    (Name: 'clPurple'; Value: 'Purple'),  //#800080
    (Name: 'clTeal'; Value: 'Teal'),  //#008080
    (Name: 'clGray'; Value: 'Gray'),  //#808080
    (Name: 'clSilver'; Value: 'Silver'),  //#C0C0C0
    (Name: 'clRed'; Value: 'Red'),  //#FF0000
    (Name: 'clLime'; Value: 'Lime'),  //#00FF00
    (Name: 'clYellow'; Value: 'Yellow'),  //#FFFF00
    (Name: 'clBlue'; Value: 'Blue'),  //#0000FF
    (Name: 'clFuchsia'; Value: 'Fuchsia'),  //#FF00FF
    (Name: 'clAqua'; Value: 'Aqua'),  //#00FFFF
    (Name: 'clWhite'; Value: 'White'),  //#FFFFFF
    (Name: 'clMoneyGreen'; Value: '#C0DCC0'),
    (Name: 'clSkyBlue'; Value: '#A6CAF0'),
    (Name: 'clCream'; Value: '#FFFBF0'),
    (Name: 'clMedGray'; Value: '#A0A0A4'),
    (Name: 'clActiveBorder'; Value: 'ActiveBorder'),  //#D4D0C8
    (Name: 'clActiveCaption'; Value: 'ActiveCaption'),  //#0054E3
    (Name: 'clAppWorkSpace'; Value: 'AppWorkspace'),  //#808080
    (Name: 'clBackground'; Value: 'Background'),  //#A6CAF0
    (Name: 'clBtnFace'; Value: 'ButtonFace'),  //#ECE9D8
    (Name: 'clBtnHighlight'; Value: 'ButtonHighlight'),  //#FFFFFF
    (Name: 'clBtnShadow'; Value: 'ButtonShadow'),  //#ACA899
    (Name: 'clBtnText'; Value: 'ButtonText'),  //#000000
    (Name: 'clCaptionText'; Value: 'CaptionText'),  //#FFFFFF
    (Name: 'clDefault'; Value: 'Black'),  //#000000
    (Name: 'clGradientActiveCaption'; Value: 'ActiveCaption'),  //#3D95FF
    (Name: 'clGradientInactiveCaption'; Value: 'InactiveCaption'),  //#9DB9EB
    (Name: 'clGrayText'; Value: 'GrayText'),  //#ACA899
    (Name: 'clHighlight'; Value: 'Highlight'),  //#316AC5
    (Name: 'clHighlightText'; Value: 'HighlightText'),  //#FFFFFF
    (Name: 'clHotLight'; Value: 'Navy'),  //#000080
    (Name: 'clInactiveBorder'; Value: 'InactiveBorder'),  //#D4D0C8
    (Name: 'clInactiveCaption'; Value: 'InactiveCaption'),  //#7A96DF
    (Name: 'clInactiveCaptionText'; Value: 'InactiveCaptionText'),  //#D8E4F8
    (Name: 'clInfoBk'; Value: 'InfoBackground'),  //#FFFFE1
    (Name: 'clInfoText'; Value: 'InfoText'),  //#000000
    (Name: 'clMenu'; Value: 'Menu'),  //#FFFFFF
    (Name: 'clMenuBar'; Value: 'ButtonFace'),  //#ECE9D8
    (Name: 'clMenuHighlight'; Value: 'Highlight'),  //#316AC5
    (Name: 'clMenuText'; Value: 'MenuText'),  //#000000
    (Name: 'clNone'; Value: 'White'),  //#FFFFFF
    (Name: 'clScrollBar'; Value: 'ScrollBar'),  //#D4D0C8
    (Name: 'cl3DDkShadow'; Value: 'ThreeDDarkShadow'),  //#716F64
    (Name: 'cl3DLight'; Value: 'ThreeDLightShadow'),  //#F1EFE2
    (Name: 'clWindow'; Value: 'Window'),  //#FFFFFF
    (Name: 'clWindowFrame'; Value: 'WindowFrame'),  //#000000
    (Name: 'clWindowText'; Value: 'WindowText'));  //#000000
var
  PrevAmp  : Boolean;
  CurChar  : Char;
  CharIdx  : Integer;
  ColorIdx : Integer;
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
(*  This works with, say, Title property, but not with Tooltip.
        if (Copy(ValStr, CharIdx+1, 1) = '#') and
           (Copy(ValStr, CharIdx+5, 1) = '''') then  {Upper ASCII char?}
          begin  {Convert to HTML encoding}
          Result := Result + '&' + Copy(ValStr, CharIdx+1, 4) + ';';
          CharIdx := CharIdx + 5;
          end
        else
*)
          begin
          Result := Result + CurChar;
          if Copy(ValStr, CharIdx+1, 4) = '#39''' then
            CharIdx := CharIdx + 3;
          end;
        PrevAmp := False;
        end
      else  {Not ampersand or apostrophe} 
        begin
        Result := Result + CurChar;
        PrevAmp := False;
        end;  
      end;

     {See if quoted string needs to be converted}
    if SameText(FmPropName, 'Font.Name') then
      Result := '"font-family":"' + Copy(Result, 2, Length(Result)-2) + '"';
    end
    
  else if Pos('color', LowerCase(FmPropName)) > 0 then  {Color property?}
    begin
    if SameText(Copy(ValStr, 1, 2), 'cl') then  {Color constant name?}
      begin  {Get HTML color value}
      for ColorIdx := Low(HtmlColors) to High(HtmlColors) do
        begin
        if ValStr = HtmlColors[ColorIdx].Name then
          begin
          Result := HtmlColors[ColorIdx].Value;
          Break;
          end;
        end;
      end
    else if (Copy(ValStr, 1, 1) = '$') and
            (Length(ValStr) = 9) then  {Color hex RGB value?}
      begin  {Convert to HTML color value}
      Result := '#' + Copy(ValStr, 8, 2) +  {Red}
                      Copy(ValStr, 6, 2) +  {Green}
                      Copy(ValStr, 4, 2);   {Blue}
      end;
    if Result <> '' then
      begin
      if SameText(FmPropName, 'Font.Color') then
        Result := '"color":"' + Result + '"'
      else if SameText(FmPropName, 'Color') then
        Result := '"background-color":"' + Result + '"';
      end;
    end

  else if SameText(ValStr, 'taLeftJustify') then
    Result := '"text-align":"left"'
  else if SameText(ValStr, 'taCenter') then
    Result := '"text-align":"center"'
  else if SameText(ValStr, 'taRightJustify') then
    Result := '"text-align":"right"'

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

  else if Copy(ValStr, 1, 1) = '[' then  {Set?}
    begin
    if SameText(FmPropName, 'Font.Style') then
      begin
      Result := '';
      if Pos('fsitalic', LowerCase(ValStr)) > 0 then
        Result := Result + '"font-style":"italic"';
      if Pos('fsbold', LowerCase(ValStr)) > 0 then
        begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + '"font-weight":"bold"';
        end;
      end;
    end

  else  {Must be a number}
    begin
    Result := ValStr;
    if SameText(FmPropName, 'Font.Height') then
      begin
      if Copy(Result, 1, 1) = '-' then  {Line leading not included?}
        Delete(Result, 1, 1);
         {TODO: Should reduce font size if positive (line leading
                included in size), but by how much?}
      Result := '"font-size":"' + Result + 'px"';
      end;
    end;
end;  {ConvertValue}


function TFormConverterExtPascal.GetAncestorName(const FormClassName : string) : string;
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
end;  {GetAncestorName}


function TFormConverterExtPascal.GetExtPropName(const ValStr : string) : string;
var
  ColonPos : Integer;
begin
  ColonPos := Pos(':', ValStr);
  if ColonPos = 0 then
    ColonPos := Succ(Length(ValStr));
  Result := Trim(Copy(ValStr, 1, Pred(ColonPos)));
end;  {GetExtPropName}


procedure TFormConverterExtPascal.ReadClassProps(    CfgF        : TMemIniFile;
                                                     CustCfgF    : TMemIniFile;
                                                 var FmClassName : string;
                                                     ClassProps  : TStringList);
 {Read property definitions for FmClassName into ClassProps.
  CfgF contains class defs from standard config file.
  If not nil, CustCfgF contains class defs from custom config file.
  A custom config file can be used to define its own custom classes,
   as well as substitute similar standard classes for custom classes
   and/or override some of the standard properties.}
var
  SubClassName : string;
  PropIdx      : Integer;
begin
  if Assigned(CustCfgF) then  {First look for class def in custom config file?}
    begin
    SubClassName := CustCfgF.ReadString(FmClassName, 'Class', '');
    if SubClassName <> '' then  {Class defined in custom config file?}
      begin
      if CfgF.ValueExists(SubClassName, 'Class') then  {And in standard?}
        begin
        CfgF.ReadSectionValues(SubClassName, ClassProps);
        for PropIdx := 0 to ClassProps.Count-1 do
          begin  {Check if any props overridden in custom config file}
          if (not SameText(ClassProps.Names[PropIdx], 'Class')) and
             CustCfgF.ValueExists(FmClassName, ClassProps.Names[PropIdx]) then
            ClassProps.Strings[PropIdx] := 
             CustCfgF.ReadString(FmClassName, ClassProps.Names[PropIdx], '') + '=';
             {Note apparent bug in Delphi: if assign blank string to Values[],
               removes string with that key from list. FPC works as expected.
               Workaround here is just to assign key-value pair with blank 
               value directly to string in list.}
          end;
        FmClassName := SubClassName;  {Replace with class to use}
        end
      else  {Not defined in standard config file, so use custom class def}
        CustCfgF.ReadSectionValues(FmClassName, ClassProps);
      end
    else  {Not defined in custom config file, so use standard class def}
      CfgF.ReadSectionValues(FmClassName, ClassProps);
    end
  else  {No custom config file, so look for class def in standard config file}
    CfgF.ReadSectionValues(FmClassName, ClassProps)
end;  {ReadClassProps}


function TFormConverterExtPascal.IsEventProp(const PropSpec : string) : Boolean;
var
  DesignProp  : string;
  InvertVal   : Boolean;
  PropType    : string;
  PropDefault : string;
begin
  Result := False;
  ParseProp(PropSpec, DesignProp, InvertVal, PropType, PropDefault);
  if (Length(PropType) > 5) and
     SameText('Event', Copy(PropType, Length(PropType)-4, 5)) then
    Result := True;
end;  {IsEventProp}


function TFormConverterExtPascal.IsGridColumn(const FmClassName : string) : Boolean;
begin
  Result := ((Length(FmClassName) > 5) and
             SameText(Copy(FmClassName, Length(FmClassName)-4, 5), '_Grid')) or
            SameText(Copy(FmClassName, 1, 6), 'TOvcTC') or
            SameText(Copy(FmClassName, 1, 6), 'TO32TC');
end;


function TFormConverterExtPascal.Convert : Boolean;
 {Using mappings in CfgFileName, converts FmFileNames form file(s) to
   PrjFileName ("program" file) and units based on FmFileNames. 
   If error, returns False and error message in ErrMsg property.
  If GeneratorName is non-blank, GeneratorName and GeneratorVersion
   are indicated in comments at the top of output code file for 
   documentation.}

const
  MaxNestedObjs     = 20;   {Maximum depth of nested controls on form}
var
  FmF               : TextFile;
  PasF              : TextFile;
  ThrdF             : TextFile;
  IncF              : TextFile;
  FormNum           : Integer;
  TargetPath        : string;
  ProgName          : string;
  ThreadClassName   : string;
  ThreadFileName    : string;
  ThrdFileExists    : Boolean;
  ThrdIsOkay        : Boolean;
  UsesNum           : Integer;
  UsesName          : string;
  InStr             : string;
  ObjIsInherited    : Boolean;
  FormObjName       : string;
  FormClassName     : string;
  FormIsInherited   : Boolean;
  CfgF              : TMemIniFile;
  CustCfgF          : TMemIniFile;
  ControlProps      : TStringList;
  DefaultProps      : TStringList;
  ClassProps        : TStringList;
  CustProps         : TStringList;
  GridColIndexes    : TStringList;
  JsLibs            : TStringList;
  UnitName          : string;
  UnitFileName      : string;
  IncFileName       : string;
  IncIsOkay         : Boolean;
  IsDfm             : Boolean;
  FormHeight        : Integer;
  FormWidth         : Integer;
  ObjLevel          : Integer;
  ObjName           : string;
  FmClassName       : string;
  ExtClassName      : string;
  FmPropName        : string;
  FmPropVal         : string;
  ExtPropName       : string;
  CheckDefaults     : Boolean;
  StyleProps        : string;
  DefPropIdx        : Integer;
  DefPropStr        : string;
  IgnoreObj         : array [1..MaxNestedObjs] of Boolean;
  ItemStrCnt        : Integer;
  GridColIdx        : Integer;
  ColWidth          : string;
  TableName         : string;
  JsLibIdx          : Integer;

begin
  Result := False;

  if not InputFilesOkay then
    Exit;

   {Create Pascal program file}
  if not CreateOutputFile(PasF, PrjFileName, True) then
    Exit;

  TargetPath := ExtractFilePath(PrjFileName);
  ProgName := ExtractFileName(PrjFileName);
  ProgName := Copy(ProgName, 1,
                   Length(ProgName) - Length(ExtractFileExt(ProgName)));

  ThreadClassName := 'AppThread';
  ThreadFileName := TargetPath + LowerCase(ThreadClassName);
  ThreadFileName := ThreadFileName + PasFileExt;
  ThreadClassName := 'T' + ThreadClassName;

  OutputDateTime(PasF);

  WriteLn(PasF, 'program ', ProgName, ';');
  WriteLn(PasF);
  WriteLn(PasF, 'uses');
  WriteLn(PasF, '{$IFNDEF WebServer}');
  WriteLn(PasF, '  FCGIApp,');
  WriteLn(PasF, '{$ELSE}');
  WriteLn(PasF, ' {$IFNDEF MSWINDOWS}');
  WriteLn(PasF, '  CThreads,');
  WriteLn(PasF, ' {$ENDIF}');
  WriteLn(PasF, '  IdExtHTTPServer,');
  WriteLn(PasF, '{$ENDIF}');
  WriteLn(PasF, '  ', Copy(ThreadClassName, 2, MaxInt), ';');
  WriteLn(PasF);
  WriteLn(PasF, '{$IFNDEF FPC}');
  WriteLn(PasF, ' {$IFNDEF WebServer}');
  WriteLn(PasF, '  {$APPTYPE CONSOLE}');
  WriteLn(PasF, ' {$ENDIF}');
  WriteLn(PasF, '{$ENDIF}');
  WriteLn(PasF);
  WriteLn(PasF, 'const');
  WriteLn(PasF, '  Port = ', PortNum, ';');
  WriteLn(PasF, '  MaxIdleMinutes = ', MaxIdleMinutes, ';');
  WriteLn(PasF);
  WriteLn(PasF, 'begin');
  WriteLn(PasF, '{$IFNDEF WebServer}');
  WriteLn(PasF, '  Application := TFCGIApplication.Create(''',
                ProgName, ''', ', ThreadClassName, 
                ', Port, MaxIdleMinutes);');
  WriteLn(PasF, '{$ELSE}');
  WriteLn(PasF, '  Application := TIdExtApplication.Create(''',
                ProgName, ''', ', ThreadClassName, 
                ', 80, MaxIdleMinutes);');
  WriteLn(PasF, '{$ENDIF}');
  WriteLn(PasF, '  Application.Run;');
  WriteLn(PasF, 'end.');
  CloseFile(PasF);


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
  if not CreateOutputFile(ThrdF, ThreadFileName, ThrdIsOkay) then
    Exit;

  WriteLn(ThrdF, 'unit ', Copy(ThreadClassName, 2, MaxInt), ';');
  WriteLn(ThrdF);
  WriteLn(ThrdF, 'interface');
  WriteLn(ThrdF);
  WriteLn(ThrdF, 'uses');
  WriteLn(ThrdF, '  ExtPascal,');
  for UsesNum := 0 to High(FmFileNames) do
    begin
    UsesName := ExtractFileName(FmFileNames[UsesNum]);
    UsesName := Copy(UsesName, 1,
                     Length(UsesName) - Length(ExtractFileExt(UsesName))); 
    if opFmToExtP_AddSuffixToName in Options then
      UsesName := UsesName + NameSuffix;
    Write(ThrdF, '  ', UsesName);
    if UsesNum < High(FmFileNames) then
      WriteLn(ThrdF, ',')
    else
      WriteLn(ThrdF, ';'); 
    end;
  WriteLn(ThrdF);
  WriteLn(ThrdF, 'type');
  WriteLn(ThrdF, '  ', ThreadClassName, ' = class(TExtThread)');
  WriteLn(ThrdF, '  public');
  for UsesNum := 0 to High(FmFileNames) do
    begin
    AssignFile(FmF, FmFileNames[UsesNum]);
    Reset(FmF);
    ReadFormLine(FmF, InStr);
    FormObjName := GetObjName(InStr, FormIsInherited);
    FormClassName := GetClassName(InStr);
    WriteLn(ThrdF, '    ', FormObjName, ' : ', FormClassName, ';');
    CloseFile(FmF);
    end;
  WriteLn(ThrdF, '  published');
  WriteLn(ThrdF, '    procedure Home; override;');


   {Load configuration file}
  CfgF := TMemIniFile.Create(CfgFileName);

  if CustCfgFileName = '' then
    CustCfgF := nil
  else
    CustCfgF := TMemIniFile.Create(CustCfgFileName);

  ControlProps := TStringList.Create;
  CfgF.ReadSectionValues('TControl', ControlProps);
  DefaultProps := TStringList.Create;
  ClassProps := TStringList.Create;
  CustProps := TStringList.Create;
  GridColIndexes := TStringList.Create;
  JsLibs := TStringList.Create;


   {Now generate Pascal form units that create ExtPascal windows (forms)}
  for FormNum := 0 to High(FmFileNames) do
    begin
     {Create Pascal form unit file}
    UnitName := ExtractFileName(FmFileNames[FormNum]);
    UnitName := Copy(UnitName, 1,
                     Length(UnitName) - Length(ExtractFileExt(UnitName)));
    if opFmToExtP_AddSuffixToName in Options then
      UnitName := UnitName + NameSuffix;
    UnitFileName := TargetPath + UnitName + PasAltFileExt;  {See if alt used}
    if not FileExists(UnitFileName) then  {Okay to assume normal extension?}
      UnitFileName := ChangeFileExt(UnitFileName, PasFileExt);
    IncFileName := TargetPath + UnitName + IncFileExt;
    if not CreateOutputFile(PasF, UnitFileName, True) then
      Exit;
    if not FileExists(IncFileName) then
      IncIsOkay := False
    else  {If form hasn't been modified, don't generate .inc file}
      IncIsOkay := FileAge(IncFileName) >= FileAge(FmFileNames[FormNum]);
    if not CreateOutputFile(IncF, IncFileName, IncIsOkay) then
      Exit;
      
    WriteLn(PasF, 'unit ', UnitName, ';');
    WriteLn(PasF);
    WriteLn(PasF, 'interface');
    WriteLn(PasF);
    WriteLn(PasF, 'uses');
    WriteLn(PasF, '  SysUtils, Classes,');
    WriteLn(PasF, '  Ext, ExtPascal, ExtPascalUtils, ExtForm,');
    WriteLn(PasF, '  ExtData, ExtGrid, ExtUtil, ExtAir, ExtDd,'); 
    WriteLn(PasF, '  ExtLayout, ExtMenu, ExtDirect, ExtState, ExtTree,');
    Write(PasF, '  ExtUxForm');

     {Scan form for objects that need to be declared}
    AssignFile(FmF, FmFileNames[FormNum]);
    Reset(FmF);
    IsDfm := SameText(ExtractFileExt(FmFileNames[FormNum]), DelFormFileExt);
    FormHeight := 0;
    FormWidth := 0;
    ObjLevel := 0;
    while not Eof(FmF) do  
      begin
      ReadFormLine(FmF, InStr);
      if IsObject(InStr) then  {Found object?}
        begin
        Inc(ObjLevel);
        ObjName := GetObjName(InStr, ObjIsInherited);
        FmClassName := GetClassName(InStr);
        if ObjLevel = 1 then  {Is form?}
          begin
          FormObjName := ObjName;
          FormClassName := FmClassName;
          FormIsInherited := ObjIsInherited;
          FmClassName := GetAncestorName(FormClassName);
          if Assigned(CustCfgF) then
            CustCfgF.ReadSectionValues(FormClassName, CustProps);
          if FormIsInherited then  {Add to uses?}
            Write(PasF, ', ', CustProps.Values['UnitName']);
          WriteLn(PasF, ';');  {Finish uses}
          WriteLn(PasF);
          WriteLn(PasF);
          WriteLn(PasF, 'type');
          Write(PasF, '  ', FormClassName, ' = class(');
          if CustProps.Values['Class'] = '' then
            WriteLn(PasF, 'TExtWindow)')
          else
            WriteLn(PasF, CustProps.Values['Class'], ')');
          end
        else if not ObjIsInherited then  {Okay to declare object?}
          begin
          ReadClassProps(CfgF, CustCfgF, FmClassName, ClassProps);
          ExtClassName := ClassProps.Values['Class'];
          if ExtClassName <> '' then  {Object's class is mapped?}
            begin
            WriteLn(PasF, '    ', ObjName, ' : ', ExtClassName, ';'); 
            if (ClassProps.IndexOfName('SetLibrary') >= 0) and  {Ux library?}
               (JsLibs.IndexOfName(ExtClassName) < 0) then
              JsLibs.Add(ExtClassName + '=' + ClassProps.Values['SetLibrary']);
            end;
          end;
        end
      else if IsEnd(InStr) then  {Found end of object?}
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
    CloseFile(FmF);


     {Scan form file for event handlers that can be mapped and
       add their declarations to TExtWindow descendent class}
    Reset(FmF);
    ObjLevel := 0;
    while not Eof(FmF) do  
      begin
      ReadFormLine(FmF, InStr);
      if IsObject(InStr) then  {Found object?}
        begin
        Inc(ObjLevel);
        ObjName := GetObjName(InStr, ObjIsInherited);
        if ObjLevel = 1 then  {Is form?}
          FmClassName := GetAncestorName(FormClassName)
        else  {Object on form}
          FmClassName := GetClassName(InStr);
        ReadClassProps(CfgF, CustCfgF, FmClassName, ClassProps);
        end
      else if IsEnd(InStr) then  {Found end of object?}
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
            Write(PasF, '    procedure ', ObjName);
            if SameText(Copy(FmPropName, 1, 2), 'On') then
              Write(PasF, Copy(FmPropName, 3, MaxInt))
            else
              Write(PasF, FmPropName);
            WriteLn(PasF, ';');

            Write(ThrdF, '    procedure ', FormObjName, '_', ObjName);
            if SameText(Copy(FmPropName, 1, 2), 'On') then
              Write(ThrdF, Copy(FmPropName, 3, MaxInt))
            else
              Write(ThrdF, FmPropName);
            WriteLn(ThrdF, ';');
            end;
          end;
        end;
      end;  {while not Eof}
    CloseFile(FmF);
    

    WriteLn(PasF, '  private');
    WriteLn(PasF, '  public');
    WriteLn(PasF, '    constructor Create;');
    WriteLn(PasF, '    procedure Show;');
    WriteLn(PasF, '  end;');
    WriteLn(PasF);
    WriteLn(PasF);
    WriteLn(PasF, 'implementation');
    WriteLn(PasF);
    WriteLn(PasF, 'uses');
    Write(PasF, '  ', Copy(ThreadClassName, 2, MaxInt));
    if FormNum = 0 then  {Main form?}
      begin
      for UsesNum := 1 to High(FmFileNames) do
        begin
        UsesName := ExtractFileName(FmFileNames[UsesNum]);
        UsesName := Copy(UsesName, 1,
                         Length(UsesName) - Length(ExtractFileExt(UsesName))); 
        if opFmToExtP_AddSuffixToName in Options then
          UsesName := UsesName + NameSuffix;
        if (not FormIsInherited) or
           (not SameText(UsesName, CustProps.Values['UnitName'])) then
          begin
          WriteLn(PasF, ',');
          Write(PasF, '  ', UsesName);
          end;
        end;
      end;
    WriteLn(PasF, ';');   
    WriteLn(PasF);
    WriteLn(PasF, 'constructor ', FormClassName, '.Create;');
    WriteLn(PasF, 'begin');
    WriteLn(PasF, '  inherited;');
    WriteLn(PasF, '{$I *.inc}');
    WriteLn(PasF, 'end;  {', FormClassName, '.Create}');


     {Now generate the actual code that creates ExtPascal components}
    Reset(FmF);
    GridColIndexes.Clear;
    ObjLevel := 0;
    CheckDefaults := False;
    StyleProps := '';
    while not Eof(FmF) do  
      begin
      ReadFormLine(FmF, InStr);

      if (IsObject(InStr) or IsEnd(InStr)) and  {New object or end of current object?}
         CheckDefaults then  {And haven't already checked?}  
        begin  {Do anything here to finish current object's properties}
         {If any default properties were not set in form for current object, 
           set them now.}
        if StyleProps <> '' then
          begin
          WriteLn(IncF, IndentStr(ObjLevel),
                  'JSCode(''style:{', StyleProps, '}'');');
          StyleProps := '';
          end;
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
            WriteLn(IncF, IndentStr(ObjLevel), 
                          ExtPropName, ' := ', 
                          Copy(DefPropStr, Pos('=', DefPropStr)+1, MaxInt), 
                          ';');
            end;
          end;
        end;

      if IsObject(InStr) then  {Found object?}
        begin
        Inc(ObjLevel);
        ObjName := GetObjName(InStr, ObjIsInherited);
        if ObjLevel = 1 then  {Is form?}
          FmClassName := GetAncestorName(FormClassName)
        else  {Object on form}
          FmClassName := GetClassName(InStr);
        ReadClassProps(CfgF, CustCfgF, FmClassName, ClassProps);
        ExtClassName := ClassProps.Values['Class'];
        
        if (ExtClassName <> '') and  {Object's class is mapped?}
           IsGridColumn(FmClassName) then  {And it's a grid cell editor?}
          begin  {Column editor control column model already created with grid}
          if GridColIndexes.Values[ObjName] = '' then  {Not part of a grid?}
            begin
            IgnoreObj[ObjLevel] := True;
            ClassProps.Clear;
            end
          else
            begin
            IgnoreObj[ObjLevel] := False;
            WriteLn(IncF, IndentStr(ObjLevel-1),
                          'with TExtGridColumn(' +
                          GridColIndexes.Values[ObjName] + '.Columns[' +
                          IntToStr(Integer(GridColIndexes.Objects[
                                            GridColIndexes.IndexOfName(ObjName)])) +
//                          ']) do  //', ObjName);
                          ']) do');
            WriteLn(IncF, IndentStr(ObjLevel), 'begin');
            WriteLn(IncF, IndentStr(ObjLevel),
                          ObjName, ' := ', ExtClassName, '.Create;');
            WriteLn(IncF, IndentStr(ObjLevel),
                          'Editor := ', ObjName, ';');
            WriteLn(IncF, IndentStr(ObjLevel),
                          'with ', ObjName, ' do');
            WriteLn(IncF, IndentStr(ObjLevel),
                          ' begin');
             {Note outputting two with statements so can reference either
               column model or column editor properties naturally.}
            end;
          end
        
        else if ExtClassName <> '' then  {Object's class is mapped?}
          begin
          IgnoreObj[ObjLevel] := False;
          ExtClassName := ClassProps.Values['Class'];
          if ObjLevel > 1 then
            WriteLn(IncF);  {For readability}

          if ObjLevel > 1 then  {Object on form?}
            begin
            if not ObjIsInherited then  {Not created in ancestor?}
              WriteLn(IncF, IndentStr(ObjLevel-1), ObjName, 
                            ' := ', ExtClassName, '.Create;');
            WriteLn(IncF, IndentStr(ObjLevel-1), 
                          'with ', ObjName, '.AddTo(Items) do');
            WriteLn(IncF, IndentStr(ObjLevel), 'begin');
            end;

          WriteLn(IncF, IndentStr(ObjLevel), 
                        'Id := ''', ObjName, ''';');

          if SameText(ExtClassName, 'TExtGridEditorGridPanel') and
            ((not SameText(FmClassName, 'TExtGridEditorGridPanel')) and
             (not SameText(FmClassName, 'TOvcTable'))) then
            begin  {Create dummy data store and column model objects}
            WriteLn(IncF, IndentStr(ObjLevel),
                          'Store := TExtDataStore.Create;');
            WriteLn(IncF, IndentStr(ObjLevel),
                          'with TExtGridColumn.AddTo(Columns) do');
            WriteLn(IncF, IndentStr(ObjLevel+1),
                          'begin');
            WriteLn(IncF, IndentStr(ObjLevel+1),
                          'Id := ''', ObjName, '_Col1'';');
            WriteLn(IncF, IndentStr(ObjLevel+1),
                          'Editor := TExtFormTextField.Create;');
            WriteLn(IncF, IndentStr(ObjLevel+1),
                          'Header := ''', ObjName, ''';');
            WriteLn(IncF, IndentStr(ObjLevel+1),
                          'Width := 100;');
            WriteLn(IncF, IndentStr(ObjLevel+1),
                          'end;');
            end;
            
          if ObjLevel = 1 then  {Is form?}
            begin
            WriteLn(IncF, IndentStr(ObjLevel),
                          'Height := ', FormHeight, ';');
            WriteLn(IncF, IndentStr(ObjLevel),
                          'Width := ', FormWidth, ';');
            if FormNum = 0 then  {Is main form?}
              WriteLn(IncF, IndentStr(ObjLevel),
                            'OnEsc := JSFunction('''');')
               {Don't want Escape key blanking page}
            else if SameText(FmClassName, 'TForm') then  {Not TExtWindow?}
              WriteLn(IncF, IndentStr(ObjLevel),
                            'Modal := True;');
               {No way to specify Modal with TForm; just assume other
                 forms will be modal.}
            end;
          end  {Object's class is mapped}

        else if FmClassName = 'TOvcTCColHead' then
          begin  {More special code to convert column header names}
          IgnoreObj[ObjLevel] := True;
          FmPropVal := '';
          repeat
            ReadFormLine(FmF, InStr);
            if SameText(GetFmPropName(InStr), 'Headings.Strings') then
              FmPropVal := GetFmPropVal(InStr);
          until SameText(GetFmPropName(InStr), 'Table') or
                SameText(GetFmPropName(InStr), 'Left');
          if (FmPropVal <> '') and SameText(GetFmPropName(InStr), 'Table') then
            begin
            TableName := GetFmPropVal(InStr);
            WriteLn(IncF);
            ItemStrCnt := 0;
            repeat
              InStr := ConvertValue(CopyAndTrimToLF(FmPropVal), '', '', '');
              for GridColIdx := 0 to GridColIndexes.Count-1 do
                begin
                if (Pos('=' + TableName,
                        GridColIndexes.Strings[GridColIdx]) > 0) and
                   (TObject(ItemStrCnt) = 
                    GridColIndexes.Objects[GridColIdx]) then
                  WriteLn(IncF, IndentStr(ObjLevel-1),
                          'TExtGridColumn(', TableName, '.Columns[',
                          ItemStrCnt, ']).Header := ', InStr, ';');
                end;
              Inc(ItemStrCnt);
            until FmPropVal = '';
            WriteLn(IncF);
            end;
          end  {TOvcTCColHead}

        else  {Object's class is not mapped}
          begin
          IgnoreObj[ObjLevel] := True;
          WriteLn(IncF);
          WriteLn(IncF, IndentStr(ObjLevel-1), 
                        '{', FmClassName, ' not mapped}');
          end;

        CheckDefaults := True;
         {Reload defaults in case any were deleted with previous object} 
        CfgF.ReadSectionValues('Defaults', DefaultProps);
        end  {is object}

      else if IsEnd(InStr) then  {Found end of object?}
        begin
        if ObjLevel = 1 then  {Is form?}
          WriteLn(IncF)
        else if not IgnoreObj[ObjLevel] then  {Object's class is mapped?}
          begin
          if IsGridColumn(FmClassName) then
            WriteLn(IncF, IndentStr(ObjLevel), ' end;');
             {Need extra end for second with statement used with grid column}
          WriteLn(IncF, IndentStr(ObjLevel), 'end;');
          end;
        Dec(ObjLevel);
        end  {is end of object}

      else  {Found property}
        begin
        FmPropName := GetFmPropName(InStr);
        FmPropVal := GetFmPropVal(InStr);

        ExtPropName := '';
        if (ClassProps.IndexOfName(FmPropName) < 0) or  {Not mapped at all in class?}
           (GetExtPropName(ClassProps.Values[FmPropName]) <> '') then  {Or non-blank mapping?}
          begin  {Okay to output property if mapped}
          if SameText(FmPropName, 'Font.Color') or
             SameText(FmPropName, 'Font.Height') or
             SameText(FmPropName, 'Font.Name') or
             SameText(FmPropName, 'Font.Style') then
            ExtPropName := 'Style'  {Map individual font props to Style}
          else
            begin
            if SameText(ClassProps.Values['IsControl'], 'True') then
              ExtPropName := ControlProps.Values[FmPropName];  {Try ancestor's}
            if ExtPropName = '' then
              ExtPropName := GetExtPropName(ClassProps.Values[FmPropName]);  {Use class's}  
            end;
          end;

        if ExtPropName <> '' then  {Is property mapped?}
          begin
          if (SameText(Copy(FmPropName, 1, 2), 'On')) or
             (IsEventProp(ClassProps.Values[FmPropName])) then {Event property?}
            begin
            Write(IncF, IndentStr(ObjLevel),
                        'On(''', ExtPropName, ''', Ajax(CurrentThread.',
                        FormObjName, '_', ObjName);
            if SameText(Copy(FmPropName, 1, 2), 'On') then
              Write(IncF, Copy(FmPropName, 3, MaxInt))
            else
              Write(IncF, FmPropName);
            WriteLn(IncF, '));');
            end

          else if SameText(FmPropName, 'Items.Strings') or
                  SameText(FmPropName, 'StoreArray.Strings') or
                  SameText(FmPropName, 'Lines.Strings') or
                  SameText(FmPropName, 'Value.Strings') then
            begin  {Convert item strings}
            ItemStrCnt := 0;
            if SameText(ExtClassName, 'TExtFormComboBox') or
               SameText(ExtClassName, 'TExtUxFormMultiSelect') then               
              begin
              WriteLn(IncF, IndentStr(ObjLevel),
                      ExtPropName, ' := JSArray(');
              repeat
                InStr := ConvertValue(CopyAndTrimToLF(FmPropVal), '', '', '');
                if ItemStrCnt > 0 then
                  WriteLn(IncF, ', '' +');
                Write(IncF, IndentStr(ObjLevel), 
                            ' ''"', Copy(InStr, 2, Length(InStr)-2), '"');
                Inc(ItemStrCnt);
              until FmPropVal = '';
              WriteLn(IncF, ''');');
              end
            else if SameText(ExtClassName, 'TExtFormTextArea') or
                    SameText(ExtClassName, 'TExtFormHtmlEditor') then
              begin
              Write(IncF, IndentStr(ObjLevel),
                    ExtPropName, ' := ');
              repeat
                InStr := ConvertValue(CopyAndTrimToLF(FmPropVal), '', '', '');
                if ItemStrCnt > 0 then
                  begin
                  if SameText(ExtClassName, 'TExtFormTextArea') then
                    WriteLn(IncF, ' + ''\n'' +')  {Start new line in code/text}
                  else
                    WriteLn(IncF, ' +');  {Start new line only in code}
                     {Note assuming HTML shouldn't have line breaks unless
                       <BR> or <P> entered by user in editor}
                  Write(IncF, IndentStr(ObjLevel), ' ');
                  end;
                Write(IncF, InStr);
                Inc(ItemStrCnt);
              until FmPropVal = '';
              WriteLn(IncF, ';');
              end
            else if SameText(ExtClassName, 'TExtFormRadioGroup') then
              begin
              repeat
                InStr := ConvertValue(CopyAndTrimToLF(FmPropVal), '', '', '');
                WriteLn(IncF, IndentStr(ObjLevel),
                              'with TExtFormRadio.AddTo(Items) do');
                WriteLn(IncF, IndentStr(ObjLevel+1),
                              'begin');
                WriteLn(IncF, IndentStr(ObjLevel+1),
                              'Name := ''', ObjName, ''';');  {Group}
                WriteLn(IncF, IndentStr(ObjLevel+1),
                              'BoxLabel := ', InStr, ';');
                WriteLn(IncF, IndentStr(ObjLevel+1),
                              'end;');
              until FmPropVal = '';
              end;
            end

          else if SameText(ExtPropName, 'Style') then
            begin
            FmPropVal := ConvertValue(FmPropVal, FmPropName,
                                      ExtClassName, ExtPropName);
            if FmPropVal <> '' then
              begin
              if StyleProps <> '' then
                StyleProps := StyleProps + ',';
              StyleProps := StyleProps + FmPropVal;
              end;
            end

          else  {Normal property}
            begin
            FmPropVal := ConvertValue(FmPropVal, FmPropName,
                                      ExtClassName, ExtPropName);
            if FmPropVal <> '' then
              begin
              if Copy(FmPropVal, 1, 1) = '''' then
                FmPropVal := 
                 '{$IFNDEF MSWINDOWS}AnsiToUTF8{$ENDIF}(' + FmPropVal + ')';
              WriteLn(IncF, IndentStr(ObjLevel), 
                            ExtPropName, ' := ', FmPropVal, ';');
              end;
            end;

          DefPropIdx :=
           DefaultProps.IndexOfName(FmClassName + '.' + ExtPropName);
          if DefPropIdx >= 0 then  {Won't need this default for class?}
            DefaultProps.Delete(DefPropIdx);
          end  {Property is mapped}
          
        else if SameText(ExtClassName, 'TExtGridEditorGridPanel') and
                SameText(FmPropName, 'ColData') then
          begin  {TOvcTable column defs reached, so special code required}
          WriteLn(IncF, IndentStr(ObjLevel),
                        'Store := TExtDataStore.Create;');
           {Note need dummy data store or grid malfunctions as is}
          GridColIdx := 0;
          repeat  {Assume each column def is on 4 lines; width is first line}
            ColWidth := CopyAndTrimToLF(FmPropVal);
            CopyAndTrimToLF(FmPropVal);
            if CopyAndTrimToLF(FmPropVal) = 'True' then  {Column has obj?}
              begin
              InStr := CopyAndTrimToLF(FmPropVal);
              InStr := Copy(InStr, Pos('.', InStr)+1, MaxInt);  {Strip form}
              InStr := Copy(InStr, 1, Length(InStr)-1);  {Trim trailing quote}
              WriteLn(IncF, IndentStr(ObjLevel),
                            'with TExtGridColumn.AddTo(Columns) do');
              WriteLn(IncF, IndentStr(ObjLevel+1),
                            'begin');
              WriteLn(IncF, IndentStr(ObjLevel+1),
                            'Id := ''', InStr, ''';');
              WriteLn(IncF, IndentStr(ObjLevel+1),
                            'Width := ', ColWidth, ';');
              WriteLn(IncF, IndentStr(ObjLevel+1),
                            'end;');
              GridColIndexes.AddObject(InStr + '=' + ObjName,
                                       TObject(GridColIdx));
               {Save order of grid columns for use later when actual column
                 controls are encountered. Note assuming column controls
                 always come after grid where they're used.}
              Inc(GridColIdx);
              end;
          until FmPropVal = '';
          end;  {TOvcTable column defs}

        end;  {is property}

      end;  {while not Eof}
    CloseFile(FmF);    
    
    WriteLn(PasF);
    WriteLn(PasF);
    WriteLn(PasF, 'procedure ', FormClassName, '.Show;');
    WriteLn(PasF, 'begin');
    WriteLn(PasF, '  inherited Show;');
    WriteLn(PasF, 'end;');
    WriteLn(PasF);
    WriteLn(PasF);


     {Now generate stub event handlers for form} 
    Reset(FmF);
    ObjLevel := 0;
    while not Eof(FmF) do  
      begin
      ReadFormLine(FmF, InStr);
      if IsObject(InStr) then  {Found object?}
        begin
        Inc(ObjLevel);
        ObjName := GetObjName(InStr, ObjIsInherited);
        if ObjLevel = 1 then  {Is form?}
          FmClassName := GetAncestorName(FormClassName)
        else  {Object on form}
          FmClassName := GetClassName(InStr);
        ReadClassProps(CfgF, CustCfgF, FmClassName, ClassProps);
        end
      else if IsEnd(InStr) then  {Found end of object?}
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
            Write(PasF, 'procedure ', FormClassName, '.', ObjName);
            if SameText(Copy(FmPropName, 1, 2), 'On') then
              Write(PasF, Copy(FmPropName, 3, MaxInt))
            else
              Write(PasF, FmPropName);
            WriteLn(PasF, ';');
            WriteLn(PasF, 'begin');
            WriteLn(PasF, 'end;');
            WriteLn(PasF);
            WriteLn(PasF);
            end;
          end;
        end;
      end;  {while not Eof}
    WriteLn(PasF, 'end.');
    CloseFile(PasF);
    CloseFile(IncF);
    CloseFile(FmF);

    end;  {for FormNum}


  WriteLn(ThrdF, '  end;');
  WriteLn(ThrdF);
  WriteLn(ThrdF, 'function CurrentThread : ', ThreadClassName, ';');
  WriteLn(ThrdF);
  WriteLn(ThrdF);
  WriteLn(ThrdF, 'implementation');
  WriteLn(ThrdF);
  WriteLn(ThrdF, 'uses');
  WriteLn(ThrdF, '{$IFNDEF WebServer}');
  WriteLn(ThrdF, '  FCGIApp;');
  WriteLn(ThrdF, '{$ELSE}');
  WriteLn(ThrdF, '  IdExtHTTPServer;');
  WriteLn(ThrdF, '{$ENDIF}');
  WriteLn(ThrdF);
  WriteLn(ThrdF, 'function CurrentThread : ', ThreadClassName, ';');
  WriteLn(ThrdF, 'begin');
  WriteLn(ThrdF, '  Result := ', ThreadClassName, '(CurrentFCGIThread);');
  WriteLn(ThrdF, 'end;');
  WriteLn(ThrdF);
  WriteLn(ThrdF);

   {Now generate published thread handlers that call form event handlers} 
  for FormNum := 0 to High(FmFileNames) do
    begin
    AssignFile(FmF, FmFileNames[FormNum]);
    Reset(FmF);
    ObjLevel := 0;
    while not Eof(FmF) do  
      begin
      ReadFormLine(FmF, InStr);
      if IsObject(InStr) then  {Found object?}
        begin
        Inc(ObjLevel);
        ObjName := GetObjName(InStr, ObjIsInherited);
        FmClassName := GetClassName(InStr);
        if ObjLevel = 1 then  {Is form?}
          begin
          FormObjName := ObjName;
          FormClassName := FmClassName;
          FmClassName := GetAncestorName(FormClassName);
          if FormNum = 0 then
            begin
            WriteLn(ThrdF, 'procedure ', ThreadClassName, '.Home;');
            WriteLn(ThrdF, 'begin');
            for JsLibIdx := 0 to JsLibs.Count-1 do
              WriteLn(ThrdF, '  SetLibrary(ExtPath + ' +
                             JsLibs.ValueFromIndex[JsLibIdx], ');');
            WriteLn(ThrdF, '  ', FormObjName, ' := ', FormClassName, '.Create;');
            WriteLn(ThrdF, '  ', FormObjName, '.Show;');
            WriteLn(ThrdF, 'end;');
            WriteLn(ThrdF);
            WriteLn(ThrdF);
            end;
          end;
        ReadClassProps(CfgF, CustCfgF, FmClassName, ClassProps);
        end
      else if IsEnd(InStr) then  {Found end of object?}
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
            Write(ThrdF, 'procedure ', ThreadClassName, '.', 
                         FormObjName, '_', ObjName);
            if SameText(Copy(FmPropName, 1, 2), 'On') then
              Write(ThrdF, Copy(FmPropName, 3, MaxInt))
            else
              Write(ThrdF, FmPropName);
            WriteLn(ThrdF, ';');
            WriteLn(ThrdF, 'begin');
            Write(ThrdF, '  ', FormObjName, '.', ObjName);
            if SameText(Copy(FmPropName, 1, 2), 'On') then
              Write(ThrdF, Copy(FmPropName, 3, MaxInt))
            else
              Write(ThrdF, FmPropName);
            WriteLn(ThrdF, ';');
            WriteLn(ThrdF, 'end;');
            WriteLn(ThrdF);
            WriteLn(ThrdF);
            end;
          end;
        end;
      end;  {while not Eof}
    end;  {for FormNum}

  WriteLn(ThrdF, 'end.');
  CloseFile(ThrdF);

  CfgF.Free;
  CustCfgF.Free;
  ControlProps.Free;
  DefaultProps.Free;
  ClassProps.Free;
  CustProps.Free;
  GridColIndexes.Free;
  JsLibs.Free;


  if opFmToExtP_ReformatForLaz in Options then
    begin
      {If run from Laz IDE, create little custom config file. This is a way to
        signal to FPC to compile with runtime unit, not design controls unit.}
    if not CreateOutputFile(IncF, TargetPath + 'extpascal.cfg', True) then
      Exit;
    WriteLn(IncF, 
            '#  Tell FPC to compile with ExtPascal runtime units, rather');
    WriteLn(IncF, 
            '#   than ExtP_Design_Ctrls unit that Lazarus IDE sees.');
    WriteLn(IncF, 
            '-dUseRuntime');
    WriteLn(IncF);
    WriteLn(IncF, 
            '#  You can put other FPC switches in this file, although it''s');
    WriteLn(IncF,
            '#   probably better to put them in the .lpi file. However, don''t');
    WriteLn(IncF,
            '#   put the above define in the .lpi file or it will confuse');
    WriteLn(IncF,
            '#   Lazarus when working with the ExtPascal design controls.');
    WriteLn(IncF);
    CloseFile(IncF);
    end;

  ErrMsg := '';
  Result := True;

end;  {Convert}


function TFormConverterExtPascal.ConvertForms : Boolean;
begin
  Result := False;
  ErrMsg := 'Unexpected conversion error';
  try
    Result := Convert;
  except on E: Exception do
    ErrMsg := ErrMsg + #13#10 + E.Message;
  end;
end;  {ConvertForms}


end.

