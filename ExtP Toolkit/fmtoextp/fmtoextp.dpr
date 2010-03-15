program FmToExtP;

{
  Console app that converts Delphi or Lazarus form design files (.dfm or .lfm) 
   to Pascal files that can be compiled against the ExtPascal units.

  Note that Delphi form files (.dfm) must be text files.

  Component and property mappings are read from fmtoextp.ini.
   
  Author:     Phil Hess.
  Copyright:  Copyright (C) 2009 Phil Hess. All rights reserved.
  License:    Modified LGPL.
}

{$IFDEF FPC}
 {$MODE Delphi}
{$ELSE}
 {$APPTYPE CONSOLE}
{$ENDIF}
{$R+,Q+}

uses
  SysUtils,
  Fm2ExtP;

const
  ConverterName     = 'FmToExtP';
  ConverterVersion  = '0.1.5';
  
var
  CfgFileName     : string;
  FmFileNames     : array of string;
  ProjSrcFileName : string;
  Options         : TFmToExtPOptions;
  AddExtToName    : Boolean;
  ReformatErrMsg  : Boolean;
  ParamNum        : Integer;
  FileExt         : string;
{$IFNDEF FPC}
  MatchFound      : TFilenameCaseMatch;
{$ENDIF}
  ProjFileVar     : TextFile;
  ProjFileName    : string;
  InStr           : string;
  InUses          : Boolean;
  InBegin         : Boolean;
  MainFmNum       : Integer;
  FmNum           : Integer;
  SaveFmName      : string;
  InUnits         : Boolean;
  QuotePos        : Integer;
  ErrMsg          : string;  

const
  FpcErrLastLine = 'Fatal: Form conversion aborted';
   {If -r switch included, reformat any error message to start with this so
     that Lazarus will think it's a Free Pascal error and display it.}

begin
   {Base configuration file name on executable file location and program name}
  CfgFileName := 
   ExtractFilePath(ParamStr(0)) + LowerCase(ConverterName) + CfgFileExt;

   
  if (ParamCount = 1) and (ParamStr(1) = '-r') then
    begin  {Assume run from IDE, so generate special error message}
    WriteLn(FpcErrLastLine, '. No input file specified -- have you ' +
            'saved and named your project (Save All)?');
    Halt;
    end;

  if ParamCount = 0 then  {List program syntax and exit?}
    begin
    WriteLn(ConverterName, ', version ', ConverterVersion,
            ' - converts Delphi or Lazarus form files to ExtPascal.');
    WriteLn;
    WriteLn('Usage: ', LowerCase(ConverterName), ' [mainform|projectfile] [otherforms] [programfile] [switches]');
    WriteLn('          (where form is .dfm or .lfm, project is .dpr or .lpi)');
    WriteLn('Switches:');
    WriteLn('  -e  Add "', NameSuffixExt, '" to names of all converted files.');
    WriteLn('  -r  Reformat any error message so Lazarus will display it.');
    WriteLn;
    WriteLn('Examples:');
    WriteLn('  ', LowerCase(ConverterName), ' hellomain.dfm helloabout.dfm myproj\hello.dpr -- creates hello.dpr,');
    WriteLn('    appthread.pas, hellomain.pas, hellomain.inc, helloabout.pas and');
    WriteLn('    helloabout.inc in myproj folder from hellomain.dfm and helloabout.dfm.');
    WriteLn;
    WriteLn('  ', LowerCase(ConverterName), ' hello.lpi -e -- creates hello', NameSuffixExt, '.lpr, etc. from project .lfm forms');
    WriteLn('    in same folder, adding "', NameSuffixExt, '" to file names to make them unique.');
    WriteLn;
    WriteLn('Notes:');
    WriteLn('  ', ConverterName, ' will look for its configuration data in:');
    WriteLn('    ', CfgFileName);
    WriteLn;
    WriteLn('  The generated Pascal files require open-source ExtPascal units to compile.'); 
    Halt;
    end;

  SetLength(FmFileNames, 0);
  ProjSrcFileName := '';
  Options := [];
  AddExtToName := False;
  ReformatErrMsg := False;
  ProjFileName := '';

   {Get names of input form file(s) and Pascal output file from command line}
  for ParamNum := 1 to ParamCount do
    begin
    if Copy(ParamStr(ParamNum), 1, 1) = '-' then  {Switch on command line?}
      begin
      if ParamStr(ParamNum) = '-e' then
        begin
        AddExtToName := True;
        Include(Options, opFmToExtP_AddExtToName);
        end
      else if ParamStr(ParamNum) = '-r' then
        ReformatErrMsg := True;
      end

    else  {File name on command line}
      begin
      FileExt := ExtractFileExt(ParamStr(ParamNum));
      if SameText(FileExt, DelFormFileExt) or 
         SameText(FileExt, LazFormFileExt) then  {Form design file?}
        begin
        SetLength(FmFileNames, High(FmFileNames)+2);
{$IFNDEF FPC}
        FmFileNames[High(FmFileNames)] :=
         ExpandFileNameCase(ParamStr(ParamNum), MatchFound);
{$ELSE}
        FmFileNames[High(FmFileNames)] :=
         ExpandFileName(ParamStr(ParamNum));
{$ENDIF}
        end

      else if SameText(FileExt, LazProjSrcFileExt) then  {Project output file?}
        ProjSrcFileName := ParamStr(ParamNum)

      else if SameText(FileExt, DelProjSrcFileExt) and
              (Length(FmFileNames) > 0) then  {Project output, not input file?}
        ProjSrcFileName := ParamStr(ParamNum)

      else if SameText(FileExt, DelProjSrcFileExt) or {Delphi project src file?}
              SameText(FileExt, LazProjInfFileExt) then {Laz project info file?}
        begin  {Open project file and look for form units to convert}
        ProjFileName := ParamStr(ParamNum);
        AssignFile(ProjFileVar, ProjFileName);
        try
          Reset(ProjFileVar);
        except
          on EInOutError do
            begin
            if ReformatErrMsg then
              Write(FpcErrLastLine, ' - ');
            WriteLn('Error: Can''t open project file ' + ProjFileName);
            Halt;
            end;
          end;

        if SameText(FileExt, DelProjSrcFileExt) then {Delphi project src file?}
          begin
          InUses := False;
          InBegin := False;
          while not Eof(ProjFileVar) do
            begin
            ReadLn(ProjFileVar, InStr);
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
              SetLength(FmFileNames, High(FmFileNames)+2);
              FmFileNames[High(FmFileNames)] :=
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
              for FmNum := 0 to High(FmFileNames) do
                begin
                if Pos(InStr, UpperCase(FmFileNames[FmNum])) > 0 then
                  MainFmNum := FmNum;  {Found main form}
                FmFileNames[FmNum] :=  {Strip out object name and change ext}
                 ExtractFilePath(ProjFileName) + 
                 ChangeFileExt(Copy(FmFileNames[FmNum], 1,
                                    Pos('''', FmFileNames[FmNum])-1),
                               DelFormFileExt);
                end;
              if MainFmNum > 0 then  {Main form wasn't first form in uses?}
                begin  {Swap so main form is first in list}
                SaveFmName := FmFileNames[MainFmNum];
                for FmNum := MainFmNum downto 1 do
                  FmFileNames[FmNum] := FmFileNames[FmNum-1];
                FmFileNames[0] := SaveFmName;
                end;
              end;
            end;
          end  {Delphi project file}
          
        else if SameText(FileExt, LazProjInfFileExt) then {Laz project info file?}
          begin  {Open project file and look for form units to convert}
          ProjFileName := ParamStr(ParamNum);
          AssignFile(ProjFileVar, ProjFileName);
          try
            Reset(ProjFileVar);
          except
            on EInOutError do
              begin
              if ReformatErrMsg then
                Write(FpcErrLastLine, ' - ');
              WriteLn('Error: Can''t open project file ' + ProjFileName);
              Halt;
              end;
            end;
          InUnits := False;
          while not Eof(ProjFileVar) do
            begin
            ReadLn(ProjFileVar, InStr);
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
                    SetLength(FmFileNames, High(FmFileNames)+2);
                    FmFileNames[High(FmFileNames)] :=
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
        end;  {Delphi or Laz project file}
      end;  {File name on command line}
    end;  {for ParamNum}
    
  if High(FmFileNames) < 0 then
    begin
    if ReformatErrMsg then
      Write(FpcErrLastLine, ' - ');
    WriteLn('Error: No project or form input file specified.');
    Halt;
    end;
    
   {Base project source code file name on main form file name or Lazarus
     project info file name if not specified on command line or encountered
     in Lazarus project info file.}
  if ProjSrcFileName = '' then
    begin
    if ProjFileName = '' then
      begin
      ProjSrcFileName :=
       Copy(FmFileNames[0], 1, 
            Length(FmFileNames[0]) - Length(ExtractFileExt(FmFileNames[0])));
      if AddExtToName then
        ProjSrcFileName := ProjSrcFileName + NameSuffixExt;
      ProjSrcFileName := ProjSrcFileName + DelProjSrcFileExt;
      end
    else
      begin
      ProjSrcFileName := 
       Copy(ProjFileName, 1, 
            Length(ProjFileName) - Length(ExtractFileExt(ProjFileName)));
      if AddExtToName then
        ProjSrcFileName := ProjSrcFileName + NameSuffixExt;
      ProjSrcFileName := ProjSrcFileName + LazProjSrcFileExt; 
      end;
    end
  else
    begin
    if AddExtToName then
      ProjSrcFileName := 
       Copy(ProjSrcFileName, 1, 
            Length(ProjSrcFileName) - Length(ExtractFileExt(ProjSrcFileName))) +
       NameSuffixExt + ExtractFileExt(ProjSrcFileName);
    end;
  
  if ConvertFormToExtP(CfgFileName, FmFileNames, ProjSrcFileName, Options,
                       ConverterName, ConverterVersion, ErrMsg) then
    begin
    WriteLn('Converted by ', ConverterName, ': ', ProjSrcFileName);
    end
  else
    begin
    if ReformatErrMsg then
      Write(FpcErrLastLine, ' - ');
    WriteLn(ErrMsg);
    end;
end.
