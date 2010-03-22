program FmToExtP;

{
  Console app that converts Delphi or Lazarus form design files (.dfm or .lfm) 
   to Pascal files that can be compiled against the ExtPascal units.

  Note that Delphi form files (.dfm) must be text files.

  Component and property mappings are read from fmtoextp.ini.
   
  Author:     Phil Hess.
  Copyright:  Copyright (C) 2009-2010 Phil Hess. All rights reserved.
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
  Fm2Base,
  Fm2ExtP;

const
  ConverterName    = 'FmToExtP';
  ConverterVersion = '0.1.6';
  NameSuffix       = '_ext';

var
  CfgFileName      : string;
  CustCfgFileName  : string;
  FmFileNames      : TFmFileNames;
  ProjSrcFileName  : string;
  Options          : TFmToExtPOptions;
  AddSuffixToNames : Boolean;
  ReformatErrMsg   : Boolean;
  ParamNum         : Integer;
  FileExt          : string;
{$IFNDEF FPC}
  MatchFound       : TFilenameCaseMatch;
{$ENDIF}
  ProjFileName     : string;
  Converter        : TFormConverterExtPascal;

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
    WriteLn('(where form is .dfm or .lfm, project is .dpr or .lpi, program is .dpr or .lpr)');
    WriteLn;
    WriteLn('Switches:');
    WriteLn('  -c= Use custom configuration file in addition to default.');
    WriteLn('  -e  Add "', NameSuffix, '" to names of all converted files.');
    WriteLn('  -r  Reformat any error message so Lazarus will display it.');
    WriteLn;
    WriteLn('Examples:');
    WriteLn('  ', LowerCase(ConverterName), ' hellomain.dfm helloabout.dfm myproj\hello.dpr -- creates hello.dpr,');
    WriteLn('    appthread.pas, hellomain.pas, hellomain.inc, helloabout.pas and');
    WriteLn('    helloabout.inc in myproj folder from hellomain.dfm and helloabout.dfm.');
    WriteLn;
    WriteLn('  ', LowerCase(ConverterName), ' hello.lpi -e -- creates hello', NameSuffix, '.lpr, etc. from project .lfm forms');
    WriteLn('    in same folder, adding "', NameSuffix, '" to file names to make them unique.');
    WriteLn;
    WriteLn('Notes:');
    WriteLn('  ', ConverterName, ' will look for its configuration data in:');
    WriteLn('    ', CfgFileName);
    WriteLn;
    WriteLn('  The generated Pascal files require open-source ExtPascal units to compile.'); 
    Halt;
    end;

  Converter := TFormConverterExtPascal.Create;
  SetLength(FmFileNames, 0);
  ProjSrcFileName := '';
  Options := [];
  AddSuffixToNames := False;
  ReformatErrMsg := False;
  CustCfgFileName := '';
  ProjFileName := '';

   {Get names of input form file(s) and Pascal output file from command line}
  for ParamNum := 1 to ParamCount do
    begin
    if Copy(ParamStr(ParamNum), 1, 1) = '-' then  {Switch on command line?}
      begin
      if ParamStr(ParamNum) = '-e' then
        begin
        AddSuffixToNames := True;
        Include(Options, opFmToExtP_AddSuffixToName);
        end
      else if ParamStr(ParamNum) = '-r' then
        begin
        ReformatErrMsg := True;
        Include(Options, opFmToExtP_ReformatForLaz);
        end
      else if Copy(ParamStr(ParamNum), 1, 3) = '-c=' then
        CustCfgFileName := Copy(ParamStr(ParamNum), 4, MaxInt);
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
        begin
        ProjFileName := ParamStr(ParamNum);
        if not Converter.GetFormFiles(ProjFileName, FmFileNames, 
                                      ProjSrcFileName) then
          begin
          if ReformatErrMsg then
            Write(FpcErrLastLine, ' - ');
          WriteLn(Converter.ErrMsg);
          Converter.Free;
          Halt;
          end;
        end;
      end;  {File name on command line}
    end;  {for ParamNum}
    
  if High(FmFileNames) < 0 then
    begin
    if ReformatErrMsg then
      Write(FpcErrLastLine, ' - ');
    WriteLn('Error: No project or form input file specified.');
    Converter.Free;
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
      if AddSuffixToNames then
        ProjSrcFileName := ProjSrcFileName + NameSuffix;
      ProjSrcFileName := ProjSrcFileName + DelProjSrcFileExt;
      end
    else
      begin
      ProjSrcFileName := 
       Copy(ProjFileName, 1, 
            Length(ProjFileName) - Length(ExtractFileExt(ProjFileName)));
      if AddSuffixToNames then
        ProjSrcFileName := ProjSrcFileName + NameSuffix;
      ProjSrcFileName := ProjSrcFileName + LazProjSrcFileExt; 
      end;
    end
  else
    begin
    if AddSuffixToNames then
      ProjSrcFileName := 
       Copy(ProjSrcFileName, 1, 
            Length(ProjSrcFileName) - Length(ExtractFileExt(ProjSrcFileName))) +
       NameSuffix + ExtractFileExt(ProjSrcFileName);
    end;
  
  Converter.CfgFileName := CfgFileName;
  Converter.CustCfgFileName := CustCfgFileName;
  Converter.FmFileNames := FmFileNames;
  Converter.PrjFileName := ProjSrcFileName;
  Converter.Options := Options;
  Converter.NameSuffix := NameSuffix;
  Converter.GeneratorName := ConverterName;
  Converter.GeneratorVersion := ConverterVersion;
  if Converter.ConvertForms then
    begin
    WriteLn('Converted by ', ConverterName, ': ', ProjSrcFileName);
    end
  else
    begin
    if ReformatErrMsg then
      Write(FpcErrLastLine, ' - ');
    WriteLn(Converter.ErrMsg);
    end;
  Converter.Free;
end.
