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
  ConverterVersion = '0.1.7';

var
  CfgFileName      : string;
  Converter        : TFormConverterExtPascal;
  CmdParams        : array of string;
  ParamNum         : Integer;
  ProjInFileName   : string;

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
    WriteLn('Usage: ', LowerCase(ConverterName), ' [projectfile|mainform] [otherforms] [programfile] [switches]');
    WriteLn(' (where input project is .dpr or .lpi, input forms are .dfm or .lfm,');
    WriteLn('  and output program is .dpr or .lpr)');
    WriteLn;
    WriteLn('Switches:');
    WriteLn('  -c=  Specify custom configuration file to use in addition to standard file.');
    WriteLn('  -e   Add "', DefExtPNameSuffix, '" to names of all converted files.');
    WriteLn('  -e=  Specify suffix to add to names of all converted files.');
    WriteLn('  -m=  Specify maximum idle minutes for session threads (default ', DefExtPMaxIdleMinutes, ').');
    Writeln('  -p=  Specify application''s TCP/IP port number (default ', DefExtPPortNum, ').');
    WriteLn('  -r   Reformat any error message so Lazarus will display it.');
    WriteLn('  -t=  Specify app''s title for browser title bar (default is program name).');
    WriteLn('  -u   Update previously converted Pascal form units'' class declarations.');
    WriteLn;
    WriteLn('Examples:');
    WriteLn('  ', LowerCase(ConverterName), ' hellomain.dfm helloabout.dfm myproj\hello.dpr -- creates hello.dpr,');
    WriteLn('    appthread.pas, hellomain.pas, hellomain.inc, helloabout.pas and');
    WriteLn('    helloabout.inc in myproj folder from hellomain.dfm and helloabout.dfm.');
    WriteLn;
    WriteLn('  ', LowerCase(ConverterName), ' hello.lpi -e -- creates hello', DefExtPNameSuffix, '.lpr, etc. from project .lfm forms');
    WriteLn('    in same folder, adding "', DefExtPNameSuffix, '" to file names to make them unique.');
    WriteLn;
    WriteLn('Notes:');
    WriteLn('  ', ConverterName, ' will look for its standard configuration data in:');
    WriteLn('    ', CfgFileName);
    WriteLn;
    WriteLn('  The generated Pascal files require open-source ExtPascal units to compile.'); 
    Halt;
    end;

  Converter := TFormConverterExtPascal.Create;

   {Get names of input form files and Pascal output file from command line}
  SetLength(CmdParams, ParamCount+1);
  for ParamNum := 0 to ParamCount do
    CmdParams[ParamNum] := ParamStr(ParamNum);
  if not Converter.ParseCmdLine(CmdParams, ProjInFileName) then
    begin
    if opFmToExtP_ReformatForLaz in Converter.Options then
      Write(FpcErrLastLine, ' - ');
    WriteLn(Converter.ErrMsg);
    Converter.Free;
    Halt;
    end;
      
  Converter.CfgFileName := CfgFileName;
  Converter.GeneratorName := ConverterName;
  Converter.GeneratorVersion := ConverterVersion;
  if Converter.ConvertForms then
    begin
    WriteLn('Conversion successful to ', 
            ExcludeTrailingPathDelimiter(
             ExtractFilePath(Converter.ProgFileName)));
    end
  else
    begin
    if opFmToExtP_ReformatForLaz in Converter.Options then
      Write(FpcErrLastLine, ' - ');
    WriteLn(Converter.ErrMsg);
    end;
  Converter.Free;
end.
