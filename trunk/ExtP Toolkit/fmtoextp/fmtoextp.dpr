program FmToExtP;

{
  Console app that converts Delphi or Lazarus form design files (.dfm or .lfm) 
   to Pascal files that can be compiled against the ExtPascal units.

  Note that the Delphi form files (.dfm) must be text files.

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
  ConverterVersion  = '0.1.1';
  
var
  CfgFileName     : string;
  FmFileNames     : array of string;
  ProjSrcFileName : string;
  Options         : TFmToExtPOptions;
  AddExtToName    : Boolean;
  ReformatErr     : Boolean;
  ParamNum        : Integer;
  FileExt         : string;
{$IFNDEF FPC}
  MatchFound      : TFilenameCaseMatch;
{$ENDIF}
  ProjInfFileVar  : TextFile;
  ProjInfFileName : string;
  InUnits         : Boolean;
  InStr           : string;
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
    WriteLn;
    WriteLn('Switches:');
    WriteLn('  -e  Add "', NameSuffixExt, '" to names of all files created.');
    WriteLn('  -r  Reformat any error message so Lazarus will display it.');
    WriteLn;
    WriteLn('Examples:');
    WriteLn('  ', LowerCase(ConverterName), ' hellomain.dfm helloabout.dfm myproj\hello.dpr -- creates');
    WriteLn('    hello.dpr, appthread.pas, hellomain.pas, hellomain.inc,');
    WriteLn('    helloabout.pas and helloabout.inc in myproj folder from');
    WriteLn('    hellomain.dfm and helloabout.dfm forms.');
    WriteLn;
    WriteLn('  ', LowerCase(ConverterName), ' hello.lpi -e -- creates hello', NameSuffixExt, '.lpr, etc. from project .lfm forms');
    WriteLn('    in same folder, adding "', NameSuffixExt, '" to all file names to make them unique.');
    WriteLn;
    WriteLn('Notes:');
    WriteLn('  ', ConverterName, ' will look for its configuration data in:');
    WriteLn('    ', CfgFileName);
    WriteLn;
    WriteLn('  The resulting Pascal files require open-source ExtPascal units to compile.'); 
    Halt;
    end;

  SetLength(FmFileNames, 0);
  ProjSrcFileName := '';
  Options := [];
  AddExtToName := False;
  ReformatErr := False;
  ProjInfFileName := '';

   {Get names of input form file(s) and Pascal output file from command line}
  for ParamNum := 1 to ParamCount do
    begin
    if Copy(ParamStr(ParamNum), 1, 1) = '-' then  {Switch on command line?}
      begin
      if ParamStr(ParamNum) = '-e' then
        begin
        AddExtToName := True;
        Include(Options, opFmToExtP_AddExtToName);
        end;
      if ParamStr(ParamNum) = '-r' then
        ReformatErr := True;
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

      else if SameText(FileExt, DelProjSrcFileExt) or
              SameText(FileExt, LazProjSrcFileExt) then  {Project output file?}
        ProjSrcFileName := ParamStr(ParamNum)

      else if SameText(FileExt, LazProjInfFileExt) then {Laz project info file?}
        begin  {Open project file and look for form units to convert}
        ProjInfFileName := ParamStr(ParamNum);
        AssignFile(ProjInfFileVar, ProjInfFileName);
        try
          Reset(ProjInfFileVar);
        except
          on EInOutError do
            begin
            if ReformatErr then
              Write(FpcErrLastLine, ' - ');
            WriteLn('Error: Can''t open project file ' + ProjInfFileName);
            Halt;
            end;
          end;
        InUnits := False;
        while not Eof(ProjInfFileVar) do
          begin
          ReadLn(ProjInfFileVar, InStr);
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
                   (ExtractFilePath(ProjInfFileName) <> '') then
                  InStr := ExtractFilePath(ProjInfFileName) + InStr;
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
      end;  {File name on command line}
    end;  {for ParamNum}
    
  if High(FmFileNames) < 0 then
    begin
    if ReformatErr then
      Write(FpcErrLastLine, ' - ');
    WriteLn('Error: No project or form input file specified.');
    Halt;
    end;
    
   {Base project source code file name on main form file name or Lazarus
     project info file name if not specified on command line or encountered
     in Lazarus project info file.}
  if ProjSrcFileName = '' then
    begin
    if ProjInfFileName = '' then
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
       Copy(ProjInfFileName, 1, 
            Length(ProjInfFileName) - Length(ExtractFileExt(ProjInfFileName)));
      if AddExtToName then
        ProjSrcFileName := ProjSrcFileName + NameSuffixExt;
      ProjSrcFileName := ProjSrcFileName + LazProjSrcFileExt; 
      end;
    end;
  
  if ConvertFormToExtP(CfgFileName, FmFileNames, ProjSrcFileName, Options,
                       ConverterName, ConverterVersion, ErrMsg) then
    begin
    WriteLn('Converted by ', ConverterName, ': ', ProjSrcFileName);
    end
  else
    begin
    if ReformatErr then
      Write(FpcErrLastLine, ' - ');
    WriteLn(ErrMsg);
    end;
end.
