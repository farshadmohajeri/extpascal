unit Fm2Base;

{
  Base class for converters of Delphi or Lazarus form design files (.dfm
   or .lfm) to something else.

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
  Classes;

type
  TFormConverterBase = class
  private
    FIndentInc        : Integer;       {Indent this many spaces per nesting level}
    FCfgFileName      : string;        {Converter's configuration file}
    FCustCfgFileName  : string;        {User's custom configuration file}
    FFmFileNames      : TStringList;   {Form design files to convert}
    FProgFileName     : string;        {Pascal program file to create}
    FGeneratorName    : string;        {Name of program using converter class}
    FGeneratorVersion : string;        {Version of program using converter class}
    FErrMsg           : string;        {Any error message from conversion}

  protected
     {Methods for working with form files}
    function IsObject(const InStr : string) : Boolean;
    function GetObjName(const InStr       : string;
                          var IsInherited : Boolean) : string;
    function GetClassName(const InStr : string) : string;
    function IsEnd(const InStr : string) : Boolean;
    function GetFmPropName(const InStr : string) : string;
    function GetFmPropVal(const InStr : string) : string;
    function CopyAndTrimToLF(var PropVal : string) : string;
    procedure ReadFormLine(var FmF   : TextFile;
                           var InStr : string);

     {Methods for working with config files}
    procedure ParsePropDef(const PropSpec    : string;
                             var DesignProp  : string;
                             var InvertVal   : Boolean;
                             var PropType    : string;
                             var PropDefault : string);
    function StripPropDef(const ValStr : string) : string;
    function InputFilesOkay : Boolean;

     {Methods for creating output files}
    function CreateOutputFile(  var OutF            : TextFile;
                              const FileName        : string;
                                    UseNullIfExists : Boolean) : Boolean;
    procedure OutputDateTime(  var OutF : TextFile);
    function IndentStr(LevelNum : Integer) : string;

  public
    property IndentInc : Integer read FIndentInc write FIndentInc;
    property CfgFileName : string read FCfgFileName write FCfgFileName;
    property CustCfgFileName : string read FCustCfgFileName write FCustCfgFileName;
    property FmFileNames : TStringList read FFmFileNames write FFmFileNames;
    property ProgFileName : string read FProgFileName write FProgFileName;
    property GeneratorName : string read FGeneratorName write FGeneratorName;
    property GeneratorVersion : string read FGeneratorVersion write FGeneratorVersion;
    property ErrMsg : string read FErrMsg write FErrMsg;

    constructor Create;
    destructor Destroy; override;
    function GetFormFiles(const ProjFileName    : string;
                                FormFileNames   : TStringList;
                            var ProjSrcFileName : string) : Boolean;
  end;


const  {Various file extensions}
  CfgFileExt        = '.ini';    {Extension for file with same name as program
                                   containing component and property mappings}
  DelProjSrcFileExt = '.dpr';    {Delphi project source code file
                                   ("program" file)}
  LazProjSrcFileExt = '.lpr';    {Lazarus project source code file
                                   ("program" file)}
  DelOptionsFileExt = '.dof';    {Delphi project options file}
  LazProjInfFileExt = '.lpi';    {Lazarus project information flle
                                   (an XML file)}
  PasFileExt        = '.pas';    {Pascal source code file}
  PasAltFileExt     = '.pp';     {Pascal source code alternate file
                                   occasionally used for some reason with Laz}
  IncFileExt        = '.inc';    {Pascal source code include file}                                 
  DelFormFileExt    = '.dfm';    {Delphi form design file}
  LazFormFileExt    = '.lfm';    {Lazarus form design file}


 {Standalone helper functions}
function ExtractFileNameWithoutExt(const FileName : string) : string;

function ExpandFileNameWithCase(const FileName : string) : string;

function StripLastDir(const FilePath : string) : string;


implementation

constructor TFormConverterBase.Create;
begin
  inherited;
  IndentInc := 2;
  FFmFileNames := TStringList.Create;
end;


destructor TFormConverterBase.Destroy;
begin
  FFmFileNames.Free;
  inherited;
end;


function TFormConverterBase.GetFormFiles(const ProjFileName    : string;
                                               FormFileNames   : TStringList;
                                           var ProjSrcFileName : string) : Boolean;
 {Open project file and look for form units to convert.}
var
  ProjF      : TextFile;
  FileExt    : string;
  InUses     : Boolean;
  InBegin    : Boolean;
  InStr      : string;
  MainFmNum  : Integer;
  FmNum      : Integer;
  SaveFmName : string;
  InUnits    : Boolean;
  QuotePos   : Integer;
begin
  Result := False;
  AssignFile(ProjF, ProjFileName);
  try
    Reset(ProjF);
  except
    on EInOutError do
      begin
      ErrMsg := 'Error: Can''t open project file ' + ProjFileName;
      Exit;
      end;
    end;

  ProjSrcFileName := '';
  FileExt := ExtractFileExt(ProjFileName);
  if SameText(FileExt, DelProjSrcFileExt) then {Delphi project src file?}
    begin
    InUses := False;
    InBegin := False;
    while not Eof(ProjF) do
      begin
      ReadLn(ProjF, InStr);
      if SameText(Copy(Trim(InStr), 1, 4), 'uses') then
        InUses := True
      else if SameText(Copy(Trim(InStr), 1, 5), 'begin') then
        begin
        InUses := False;
        InBegin := True;
        end
      else if InUses and (Pos(''' {', InStr) > 0) and
              (Pos(':', InStr) = 0) then {Found form unit?}
        begin  {Note checking to make sure not Application: CoClass}
        FormFileNames.Add(Copy(InStr, Pos('''', InStr)+1, MaxInt));
          {Add everything to list (file name + form object name
            in comment) - will fix up later}
        end
      else if InBegin and (Pos('.CREATEFORM', UpperCase(InStr)) > 0) then
        begin  {Assume first CreateForm is for main form}
        InBegin := False;
        InStr := UpperCase(Trim(Copy(InStr, Pos(',', InStr)+1, MaxInt)));
        if Pos(');', InStr) > 0 then
          InStr := Trim(Copy(InStr, 1, Pos(');', InStr)-1));
        InStr := '{' + InStr + '}';  {Look for this to find main form}
        MainFmNum := 0;
        for FmNum := 0 to FormFileNames.Count-1 do
          begin
          if Pos(InStr, UpperCase(FormFileNames[FmNum])) > 0 then
            MainFmNum := FmNum;  {Found main form}
          FormFileNames[FmNum] :=  {Strip out object name and change ext}
           ExtractFilePath(ProjFileName) + 
           ChangeFileExt(Copy(FormFileNames[FmNum], 1,
                              Pos('''', FormFileNames[FmNum])-1),
                         DelFormFileExt);
          end;
        if MainFmNum > 0 then  {Main form wasn't first form in uses?}
          begin  {Swap so main form is first in list}
          SaveFmName := FormFileNames[MainFmNum];
          for FmNum := MainFmNum downto 1 do
            FormFileNames[FmNum] := FormFileNames[FmNum-1];
          FormFileNames[0] := SaveFmName;
          end;
        end;
      end;  {while not Eof}
    end  {Delphi project file}
          
  else if SameText(FileExt, LazProjInfFileExt) then {Laz project info file?}
    begin  {Open project file and look for form units to convert}
    InUnits := False;
    while not Eof(ProjF) do
      begin
      ReadLn(ProjF, InStr);
      if Copy(Trim(InStr), 1, 6) = '<Units' then
        InUnits := True
      else if Copy(Trim(InStr), 1, 8) = '</Units>' then
        InUnits := False
      else if InUnits and (Copy(Trim(InStr), 1, 9) = '<Filename') then
        begin  {Found unit file name}
        QuotePos := Pos('"', InStr);
        if QuotePos > 0 then
          begin
          InStr := Copy(InStr, Succ(QuotePos), MaxInt);
          QuotePos := Pos('"', InStr);
          if QuotePos > 0 then
            begin
            InStr := Copy(InStr, 1, Pred(QuotePos));
            if (Pos(PathDelim, InStr) = 0) and
               (ExtractFilePath(ProjFileName) <> '') then
              InStr := ExtractFilePath(ProjFileName) + InStr;
               {If file name has no path, make relative to project file}
            if (SameText(ExtractFileExt(InStr), PasFileExt) or
                SameText(ExtractFileExt(InStr), PasAltFileExt)) and
               FileExists(ChangeFileExt(InStr, LazFormFileExt)) then
              begin  {Pascal unit with form design file, so add to list}
              FormFileNames.Add(ChangeFileExt(InStr, LazFormFileExt));
              end
            else if SameText(ExtractFileExt(InStr), DelProjSrcFileExt) or
                    SameText(ExtractFileExt(InStr), LazProjSrcFileExt) then
              ProjSrcFileName := InStr;  {Project source code file}
            end;
          end;
        end;
      end;  {while not Eof}
    end;  {Lazarus project file}
  Result := True;
end;  {GetFormFiles}


function TFormConverterBase.IsObject(const InStr : string) : Boolean;
 {Return True if string is an object declaration.}
begin
  Result := SameText(Copy(InStr, 1, 7), 'object ') or
            SameText(Copy(InStr, 1, 10), 'inherited ');
end;


function TFormConverterBase.GetObjName(const InStr       : string;
                                         var IsInherited : Boolean) : string;
 {Return object name from object declaration and
   whether object is inherited.}
begin
  if SameText(Copy(InStr, 1, 7), 'object ') then
    begin
    Result := Copy(InStr, 8, Pos(':', InStr)-8);
    IsInherited := False;
    end
  else
    begin
    Result := Copy(InStr, 11, Pos(':', InStr)-11);
    IsInherited := True;
    end;
end;


function TFormConverterBase.GetClassName(const InStr : string) : string;
 {Return class name from object declaration.}
begin
  Result := Trim(Copy(InStr, Pos(':', InStr)+1, MaxInt));
end;


function TFormConverterBase.IsEnd(const InStr : string) : Boolean;
 {Return True if string is end of object declaration.}
begin
  Result := SameText(InStr, 'end');
end;


function TFormConverterBase.GetFmPropName(const InStr : string) : string;
 {Extract form property name from string.}
begin
  Result := Trim(Copy(InStr, 1, Pos('=', InStr)-1));
end;


function TFormConverterBase.GetFmPropVal(const InStr : string) : string;
 {Extract form property value from string.}
begin
  Result := Trim(Copy(InStr, Pos('=', InStr)+1, MaxInt));
end;


function TFormConverterBase.CopyAndTrimToLF(var PropVal : string) : string;
 {Return substring up to first line feed or end of string,
   deleting through first line feed or end of string.}
var
  LFPos : Integer;
begin
  LFPos := Pos(#10, PropVal);
  if LFPos = 0 then
    LFPos := Succ(Length(PropVal));
  Result := Trim(Copy(PropVal, 1, Pred(LFPos)));
  PropVal := Copy(PropVal, Succ(LFPos), MaxInt);
end;


procedure TFormConverterBase.ReadFormLine(var FmF   : TextFile;
                                          var InStr : string);
 {Read line from form. If a multi-line property, read
   additional lines and concatenate.}
var
  FmPropName     : string;
  FmPropVal      : string;
  DataDone       : Boolean;
  FirstLineOfStr : Boolean;
begin
  ReadLn(FmF, InStr);
  InStr := Trim(InStr);
  if IsObject(InStr) or IsEnd(InStr) then
    Exit;

  FmPropName := GetFmPropName(InStr);
  FmPropVal := GetFmPropVal(InStr);
  if FmPropVal = '' then  {String begins on next line?}
    begin
    repeat  {Until reach a line that does not end with "+"}
      ReadLn(FmF, InStr);
      InStr := Trim(InStr);
      if Copy(InStr, Length(InStr)-2, 3) = ''' +' then
        begin
        if FmPropVal = '' then
          FmPropVal := Copy(InStr, 1, Length(InStr)-3)
        else
          FmPropVal := FmPropVal + Copy(InStr, 2, Length(InStr)-4);
        end
      else
        FmPropVal := FmPropVal + Copy(InStr, 2, MaxInt);
    until Copy(InStr, Length(InStr), 1) <> '+';
    InStr := FmPropName + '=' + FmPropVal;
    end

  else if FmPropVal = '(' then  {Data items until ')' reached?}
    begin
    FmPropVal := '';
    DataDone := False;
    FirstLineOfStr := True;
    repeat  {Until reach a line that ends with ")"}
      ReadLn(FmF, InStr);
      InStr := Trim(InStr);
      if InStr = ')' then  {Lazarus puts ) on its own line}
        DataDone := True;
      if (not DataDone) and (InStr <> '') then
        begin
        if Copy(InStr, Length(InStr), 1) = ')' then
          begin
          InStr := Copy(InStr, 1, Length(InStr)-1);
          DataDone := True;
          end;
        if Copy(InStr, Length(InStr)-2, 3) = ''' +' then
          begin
          if FirstLineOfStr then
            FmPropVal := FmPropVal + Copy(InStr, 1, Length(InStr)-3)
          else
            FmPropVal := FmPropVal + Copy(InStr, 2, Length(InStr)-4);
          FirstLineOfStr := False;
          end
        else  {Separate data items with line feed}
          begin
          if FirstLineOfStr then
            FmPropVal := FmPropVal + InStr + #10
          else
            FmPropVal := FmPropVal + Copy(InStr, 2, MaxInt) + #10;
          FirstLineOfStr := True;
          end;
        end;
    until DataDone;
    InStr := FmPropName + '=' + FmPropVal;
    end

  else if (Copy(FmPropVal, 1, 1) = '<') or 
          (Copy(FmPropVal, 1, 1) = '{') then
    begin  {Not sure what to do with this kind of potentially multi-line 
             value, so just read to end of value and concatenate lines}
    FmPropVal := Copy(FmPropVal, 2, MaxInt);
    if (Copy(FmPropVal, Length(FmPropVal), 1) = '>') or 
       (Copy(FmPropVal, Length(FmPropVal), 1) = '}') then  {Only one line?}
      FmPropVal := Copy(FmPropVal, 1, Length(FmPropVal)-1)
    else  {Value spans more than one line}
      begin
      DataDone := False;
      repeat
        ReadLn(FmF, InStr);
        InStr := Trim(InStr);
        if InStr <> '' then
          begin
          if (Copy(InStr, Length(InStr), 1) = '>') or
             (Copy(InStr, Length(InStr), 1) = '}') then  {End of value?}
            begin
            InStr := Copy(InStr, 1, Length(InStr)-1);
            DataDone := True;
            end;
          if FmPropVal <> '' then
            FmPropVal := FmPropVal + #10;  {Separate lines with line feed}
          FmPropVal := FmPropVal + InStr;  
          end;
      until DataDone;
      end;
    InStr := FmPropName + '=' + FmPropVal;
    end;       
end;  {ReadFormLine}


procedure TFormConverterBase.ParsePropDef(const PropSpec    : string;
                                            var DesignProp  : string;
                                            var InvertVal   : Boolean;
                                            var PropType    : string;
                                            var PropDefault : string);
 {If property is for a design control, parse the extra definition
   information following the mapped-to property name.}
var
  TempStr : string;
begin
  if Pos(':', PropSpec) = 0 then
    begin
    DesignProp := '';
    InvertVal := False;
    PropType := '';
    PropDefault := '';
    Exit;
    end;
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
  if Pos(':', PropType) > 0 then  {Has default value?}
    begin
    PropDefault := Trim(Copy(PropType, Pos(':', PropType)+1, MaxInt)); 
    PropType := Trim(Copy(PropType, 1, Pos(':', PropType)-1));
    end;
end;  {ParsePropDef}


function TFormConverterBase.StripPropDef(const ValStr : string) : string;
 {If property is for a design control, strip the extra definition
   information and return only the mapped-to property name.}
var
  ColonPos : Integer;
begin
  ColonPos := Pos(':', ValStr);
  if ColonPos = 0 then
    Result := ValStr
  else
    Result := Trim(Copy(ValStr, 1, Pred(ColonPos)));
end;  {StripPropDef}


function TFormConverterBase.InputFilesOkay : Boolean;
 {Return True if input files exist; otherwise, set ErrMsg.}
var
  FormNum : Integer;
begin
  Result := False;
  if CfgFileName = '' then
    begin
    ErrMsg := 'Configuration file is not specified.';
    Exit;
    end;

  if not FileExists(CfgFileName) then
    begin
    ErrMsg := 'Can''t load configuration file ' + CfgFileName;
    Exit;
    end;
  
  if (CustCfgFileName <> '') and
     (not FileExists(CustCfgFileName)) then
    begin
    ErrMsg := 'Can''t load custom configuration file ' + CustCfgFileName;
    Exit;
    end;
  
  if FmFileNames.Count = 0 then
    begin
    ErrMsg := 'No form files have been specified.';
    Exit;
    end;

  for FormNum := 0 to FmFileNames.Count-1 do
    begin
    if FmFileNames[FormNum] = '' then
      begin
      ErrMsg := 'A form file name is blank.';
      Exit;
      end;
    if not FileExists(FmFileNames[FormNum]) then
      begin
      ErrMsg := 'Form file does not exist: ' + FmFileNames[FormNum];
      Exit;
      end;
    end;
  Result := True;
end;  {InputFilesOkay}


function TFormConverterBase.CreateOutputFile(  var OutF            : TextFile;
                                             const FileName        : string;
                                                   UseNullIfExists : Boolean) : Boolean;
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


procedure TFormConverterBase.OutputDateTime(var OutF : TextFile);
 {Output date-time comment(s).}
begin
  Write(OutF, '  // Generated');
  if GeneratorName <> '' then
    Write(OutF, ' by ', GeneratorName, ' ', GeneratorVersion);
  WriteLn(OutF, FormatDateTime('" at "hh:nn:ss" on "yyyy-mm-dd" //"', Now));
  WriteLn(OutF); 
end;


function TFormConverterBase.IndentStr(LevelNum : Integer) : string;
 {Return number of spaces to indent for indicated nesting level.}
begin
  Result := StringOfChar(' ', IndentInc*LevelNum);
end;



 {Standalone helper functions}
function ExtractFileNameWithoutExt(const FileName : string) : string;
 {Return file name base without its extension.}
begin
  Result := ExtractFileName(FileName);
  Result := Copy(Result, 1, Length(Result)-Length(ExtractFileExt(Result)));
end;


function ExpandFileNameWithCase(const FileName : string) : string;
 {Return file name expanded and with correct case.}
{$IFNDEF FPC}
var
  MatchFound : TFilenameCaseMatch;
begin
  Result := ExpandFileNameCase(FileName, MatchFound);
{$ELSE}
begin
  Result := ExpandFileName(FileName);
{$ENDIF}
end;


function StripLastDir(const FilePath : string) : string;
 {Strip last directory name from file path.}
var
  CharPos : Integer;
begin
  Result := FilePath;
  for CharPos := Length(Result)-1 downto 1 do
    begin
    if Copy(Result, CharPos, 1) = PathDelim then
      begin
      Result := Copy(Result, 1, CharPos);
      Exit;
      end;
    end;
end;


end.
