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
  TFmFileNames = array of string;

  TFormConverterBase = class
  private
    FIndentInc        : Integer;       {Indent this many spaces per nesting level}
    FCfgFileName      : string;        {Converter's configuration file}
    FCustCfgFileName  : string;        {User's custom configuration file}
    FFmFileNames      : TFmFileNames;  {Form design files to convert}
    FPrjFileName      : string;        {Pascal program file to create}
    FGeneratorName    : string;        {Name of program using converter class}
    FGeneratorVersion : string;        {Version of program using converter class}
    FErrMsg           : string;        {Any error message from conversion}

  protected
    function IsObject(const InStr : string) : Boolean;
    function GetObjName(const InStr       : string;
                          var IsInherited : Boolean) : string;
    function GetClassName(const InStr : string) : string;
    function IsEnd(const InStr : string) : Boolean;
    function GetFmPropName(const InStr : string) : string;
    function GetFmPropVal(const InStr : string) : string;
    function CopyAndTrimToLF(var PropVal : string) : string;
    procedure ParseProp(const PropSpec    : string;
                          var DesignProp  : string;
                          var InvertVal   : Boolean;
                          var PropType    : string;
                          var PropDefault : string);
    procedure ReadFormLine(var FmF   : TextFile;
                           var InStr : string);
    function InputFilesOkay : Boolean;

    function CreateOutputFile(  var OutF            : TextFile;
                              const FileName        : string;
                                    UseNullIfExists : Boolean) : Boolean;
    procedure OutputDateTime(  var OutF : TextFile);
    function IndentStr(LevelNum : Integer) : string;

  public
    property IndentInc : Integer read FIndentInc write FIndentInc;
    property CfgFileName : string read FCfgFileName write FCfgFileName;
    property CustCfgFileName : string read FCustCfgFileName write FCustCfgFileName;
    property FmFileNames : TFmFileNames read FFmFileNames write FFmFileNames;
    property PrjFileName : string read FPrjFileName write FPrjFileName;
    property GeneratorName : string read FGeneratorName write FGeneratorName;
    property GeneratorVersion : string read FGeneratorVersion write FGeneratorVersion;
    property ErrMsg : string read FErrMsg write FErrMsg;

    constructor Create;
    function GetFormFiles(const ProjFileName    : string;
                            var FormFileNames   : TFmFileNames;
                            var ProjSrcFileName : string) : Boolean;
  end;


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


implementation

constructor TFormConverterBase.Create;
begin
  inherited;
  IndentInc := 2;
end;


function TFormConverterBase.GetFormFiles(const ProjFileName    : string;
                                           var FormFileNames   : TFmFileNames;
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
        SetLength(FormFileNames, High(FormFileNames)+2);
        FormFileNames[High(FormFileNames)] :=
         Copy(InStr, Pos('''', InStr)+1, MaxInt);
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
        for FmNum := 0 to High(FormFileNames) do
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
              SetLength(FormFileNames, High(FormFileNames)+2);
              FormFileNames[High(FormFileNames)] :=
               ChangeFileExt(InStr, LazFormFileExt);
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
begin
  Result := SameText(Copy(InStr, 1, 7), 'object ') or
            SameText(Copy(InStr, 1, 10), 'inherited ');
end;


function TFormConverterBase.GetObjName(const InStr       : string;
                                         var IsInherited : Boolean) : string;
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
begin
  Result := Trim(Copy(InStr, Pos(':', InStr)+1, MaxInt));
end;


function TFormConverterBase.IsEnd(const InStr : string) : Boolean;
begin
  Result := SameText(InStr, 'end');
end;


function TFormConverterBase.GetFmPropName(const InStr : string) : string;
 {Extract form property name from input string.}
begin
  Result := Trim(Copy(InStr, 1, Pos('=', InStr)-1));
end;


function TFormConverterBase.GetFmPropVal(const InStr : string) : string;
 {Extract form property value from input string.}
begin
  Result := Trim(Copy(InStr, Pos('=', InStr)+1, MaxInt));
end;


function TFormConverterBase.CopyAndTrimToLF(var PropVal : string) : string;
 {Return substring up to first line feed or end of string and
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


procedure TFormConverterBase.ParseProp(const PropSpec    : string;
                                         var DesignProp  : string;
                                         var InvertVal   : Boolean;
                                         var PropType    : string;
                                         var PropDefault : string);
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
  if Pos(':', PropType) > 0 then  {Has default value?}
    begin
    PropDefault := Trim(Copy(PropType, Pos(':', PropType)+1, MaxInt)); 
    PropType := Trim(Copy(PropType, 1, Pos(':', PropType)-1));
    end;
end;  {ParseProp}


procedure TFormConverterBase.ReadFormLine(var FmF   : TextFile;
                                          var InStr : string);
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
    end;
end;  {ReadFormLine}


function TFormConverterBase.InputFilesOkay : Boolean;
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
  
  if Length(FmFileNames) = 0 then
    begin
    ErrMsg := 'No form files have been specified.';
    Exit;
    end;

  for FormNum := 0 to High(FmFileNames) do
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
begin
  Result := StringOfChar(' ', IndentInc*LevelNum);
end;


end.
