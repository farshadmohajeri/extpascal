unit ExtP_Proj_Intf;

{
  Adds ExtPascal application and form to Lazarus File | New list
   for creating projects that use ExtP_Ctrls design controls.
  
  Author:     Phil Hess.
  Copyright:  Copyright (C) 2009 Phil Hess. All rights reserved.
  License:    Modified LGPL.
}  

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Controls, Forms, Dialogs,
  LazIDEIntf, ProjectIntf, FormEditingIntf, Project,
  ExtP_Design_Ctrls;

type
  TExtPApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function DoInitDescriptor: TModalResult; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  TFileDescPascalUnitWithExtWindow = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function CreateSource(const Filename     : string;
                          const SourceName   : string;
                          const ResourceName : string): string; override;
    function GetInterfaceUsesSection: string; override;
    function GetInterfaceSource(const Filename     : string;
                                const SourceName   : string;
                                const ResourceName : string): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetImplementationSource(const Filename     : string;
                                     const SourceName   : string;
                                     const ResourceName : string): string; override;
  end;

var
  ProjectDescriptorExtPApplication: TExtPApplicationDescriptor;
  FileDescriptorExtWindow: TFileDescPascalUnitWithExtWindow;
  
procedure Register;


implementation

procedure Register;
begin
  FileDescriptorExtWindow := TFileDescPascalUnitWithExtWindow.Create;
  RegisterProjectFileDescriptor(FileDescriptorExtWindow);
  ProjectDescriptorExtPApplication := TExtPApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorExtPApplication);
  FormEditingHook.RegisterDesignerBaseClass(TExtWindow);
end;


 {TExtPApplicationDescriptor}

constructor TExtPApplicationDescriptor.Create;
begin
  inherited Create;
  Name := 'ExtPascal Application';
end;

function TExtPApplicationDescriptor.GetLocalizedName: string;
begin
  Result := 'ExtPascal Application';
end;

function TExtPApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result := 'An ExtPascal GUI Web application.'#13#10#13#10 +
            'To compile your app, you will need the open-source ' +
            'ExtPascal units:'#13#10#13#10 +
            'http://code.google.com/p/extpascal';
end;

function TExtPApplicationDescriptor.DoInitDescriptor: TModalResult;
begin
  MessageDlg('Welcome to ExtPascal for Lazarus!'#10#10 +
             'Before you compile your ExtPascal app, be sure to check the ' +
             'project''s paths:'#10#10 +
             '(1)  Choose Project | Compiler Options, then on the Paths tab, ' +
             'check the path to the ExtPascal units in the Other Unit Files ' +
             'box. By default it''s set to one level up from the design ' +
             'control package.'#10#10 +
             '(2)  Also in Compiler Options, on the Compilation tab, check ' +
             'the path to the FmToExtP converter in the Execute Before ' +
             'Command box. By default it''s set to the same level as the ' +
             'design control package, as distributed.'#10#10 +
             'Tips:'#10#10 +
             ' - With both paths, you can use the $(Home) macro to avoid ' +
             'hardwiring the path to a particular user folder.'#10#10 +
             '- When designing your app''s forms, only add controls from the ' +
             'palette''s ExtPascal tab.',
             mtInformation, [mbOK], 0);
  Result := mrOK;
end;

function TExtPApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  NewSource: string;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile := AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject := True;
  AProject.AddFile(MainFile, False);
  AProject.MainFileID := 0;

  NewSource := 'program Project1;' + LineEnding +
               LineEnding +
               'uses' + LineEnding +
               '{$IFNDEF WebServer}' + LineEnding +
               '  FCGIApp,' + LineEnding +
               '{$ELSE}' + LineEnding +
               ' {$IFNDEF MSWINDOWS}' + LineEnding +
               '  CThreads,' + LineEnding +
               ' {$ENDIF}' + LineEnding +
               '  IdExtHTTPServer,' + LineEnding +
               '{$ENDIF}' + LineEnding +
               '  AppThread;' + LineEnding +
               LineEnding +
               '{$IFNDEF FPC}' + LineEnding +
               ' {$IFNDEF WebServer}' + LineEnding +
               '  {$APPTYPE CONSOLE}' + LineEnding +
               ' {$ENDIF}' + LineEnding +
               '{$ENDIF}' + LineEnding +
               LineEnding +
               'const' + LineEnding +
               '  Port = 2014;' + LineEnding +
               '  MaxIdleMinutes = 5;' + LineEnding +
               LineEnding + LineEnding +
               'begin' + LineEnding +
               '{$IFNDEF WebServer}' + LineEnding + 
               '  Application := TFCGIApplication.Create(''MyApp'', ' +
                  'TAppThread, Port, MaxIdleMinutes);' + LineEnding +
               '{$ELSE}' + LineEnding + 
               '  Application := TIdExtApplication.Create(''MyApp'', ' +
                  'TAppThread, 80, MaxIdleMinutes);' + LineEnding +
               '{$ENDIF}' + LineEnding +
               '  Application.Run;' + LineEnding +
               'end.' + LineEnding +
               LineEnding;
   {Note using placeholder "MyApp" for application title rather than something 
     like "project1" since user will likely save with different file/program 
     name.
    Application.Title appears to be used only in TExtThread.AfterHandleRequest
     for generating HTML.
    Similarly, assuming thread unit and class names are always the same since
     this code is generated before converter creates thread unit. They can
     be renamed as long as this code and each form unit's implementation uses
     is edited to reflect new unit/class names. Renaming of thread unit is
     required if you want more than one project in the same folder.}

  AProject.MainFile.SetSourceText(NewSource);

  AProject.AddPackageDependency('ExtP_Ctrls');

  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements,
                                      pfMainUnitHasTitleStatement,
                                      pfLRSFilesInOutputDirectory];
//   Leave next box checked since Laz ignores it anyway and if it's not
//    checked Laz doesn't rename unit in project uses when save unit
//    file with a different name.
//  AProject.Flags := AProject.Flags - [pfMainUnitHasUsesSectionForAllUnits];

   {Note that in order to reference TProject, need Project unit in uses.
     This requires path to IDE, CodeTools and SynEdit units in package file.}
  TProject(AProject).UseAppBundle := False;
  TProject(AProject).Resources.XPManifest.UseManifest := False;
  TProject(AProject).Resources.ProjectIcon.SetStream(nil);

   {Set path to ExtPascal units - assume it's one level up from
     control package.}
  if AProject.LazCompilerOptions.OtherUnitFiles = '' then
    AProject.LazCompilerOptions.OtherUnitFiles := 
     '$PKGDIR(ExtP_Ctrls)' + PathDelim + '..' + PathDelim;
     
   {This eliminates most LCL and widgetset code when compiling app.
     Note that Laz IDE might warn about a missing "nogui" folder
     when close compiler options dialog if this widgetset has not
     been built yet.}
  AProject.LazCompilerOptions.LCLWidgetType := 'nogui';

   {This config file will be created by converter and signals to FPC to
     compile with ExtPascal runtime units, not Extp_Design_Ctrls unit.}
  AProject.LazCompilerOptions.CustomConfigFile := True;
  AProject.LazCompilerOptions.ConfigFilePath := 'extpascal.cfg';

   {Set these compiler options per ExtPascal requirements}
  AProject.LazCompilerOptions.SyntaxMode := 'delphi';
  AProject.LazCompilerOptions.CStyleOperators := True;
  AProject.LazCompilerOptions.AllowLabel := True;
  AProject.LazCompilerOptions.CPPInline := True;
  AProject.LazCompilerOptions.CStyleMacros := True;
  AProject.LazCompilerOptions.UseAnsiStrings := True;
  AProject.LazCompilerOptions.UseLineInfoUnit := True;
  
   {Always a good idea to start out with checks on}
  AProject.LazCompilerOptions.RangeChecks := True;
  AProject.LazCompilerOptions.OverflowChecks := True;
  AProject.LazCompilerOptions.StackChecks := True;

  AProject.LazCompilerOptions.OptimizationLevel := 0;
  AProject.LazCompilerOptions.Win32GraphicApp := False;
  
  TProject(AProject).CompilerOptions.ExecuteBefore.Command :=
   '"$PKGDIR(ExtP_Ctrls)' + PathDelim + '..' + PathDelim + 
   'fmtoextp' + PathDelim + 'fmtoextp$ExeExt()' +
   '" "$Project(InfoFile)" -r';
   {Include path to converter - assume its folder is at same level
     as control package, as distributed.}

  TProject(AProject).CompilerOptions.ExecuteBefore.ScanForFPCMessages := True;
   {Converter's -r switch coupled with "Scan for FPC messages" checked
     will cause compilation to halt if converter finds an error, for
     example no input file specified, etc.}
  TProject(AProject).CompilerOptions.ExecuteBefore.ShowAllMessages := True;
   {Checking "Show all messages" will display converter completion message.}
  
  Result := mrOK;
end;

function TExtPApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
//  if AProject=nil then ;
  LazarusIDE.DoNewEditorFile(FileDescriptorExtWindow, '', '',
                             [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  Result := mrOK;
end;



 {TFileDescPascalUnitWithExtWindow}

constructor TFileDescPascalUnitWithExtWindow.Create;
begin
  inherited Create;
  Name := 'ExtPascal Form';
  ResourceClass := TExtWindow;
end;

function TFileDescPascalUnitWithExtWindow.CreateSource(const Filename     : string;
                                                       const SourceName   : string;
                                                       const ResourceName : string): string;
begin
  Result := 'unit ' + SourceName + ';' + LineEnding +
            LineEnding +
            'interface' + LineEnding +
            LineEnding +
            'uses' + LineEnding +
            '  ' + GetInterfaceUsesSection + LineEnding +
            LineEnding +
            GetInterfaceSource(Filename, SourceName, ResourceName) +
            'implementation' + LineEnding +
            LineEnding + 
            GetImplementationSource(Filename, SourceName, ResourceName) +
            'end.' + LineEnding +
            LineEnding;
end;
                                                       
function TFileDescPascalUnitWithExtWindow.GetInterfaceUsesSection: string;
begin
  Result := 'SysUtils, Classes,' + LineEnding +
            '{$IFDEF UseRuntime}' + LineEnding +
            '  Ext, ExtPascal, ExtPascalUtils, ExtForm,' + LineEnding +
            '  ExtData, ExtGrid, ExtUtil, ExtAir, ExtDd,' + LineEnding +
            '  ExtLayout, ExtMenu, ExtDirect, ExtState, ExtTree,' + LineEnding +
            '  ExtUxForm;' + LineEnding +
            LineEnding +
            'type' + LineEnding +
            '  {$M+}' + LineEnding +
            '  TExtPanel_Tab = TExtPanel;' + LineEnding +
            '  TExtFormTextField_Grid = TExtFormTextField;' + LineEnding +
            '  TExtFormNumberField_Grid = TExtFormNumberField;' + LineEnding +
            '  TExtFormDateField_Grid = TExtFormDateField;' + LineEnding +
            '  TExtFormTimeField_Grid = TExtFormTimeField;' + LineEnding +
            '  TExtFormCheckbox_Grid = TExtFormCheckbox;' + LineEnding +
            '  TExtFormComboBox_Grid = TExtFormComboBox;' + LineEnding +
            '  {$M-}' + LineEnding +
            LineEnding +
            '{$ELSE}' + LineEnding +
            '  ExtP_Design_Ctrls;' + LineEnding +
            '{$ENDIF}';
end;

function TFileDescPascalUnitWithExtWindow.GetInterfaceSource(const Filename     : string;
                                                             const SourceName   : string;
                                                             const ResourceName : string): string;
begin
  Result := //'{$M+}' + LineEnding +  {So members at top without specified visibility are published}
            //LineEnding +
            'type' + LineEnding +
            '  T' + ResourceName + ' = class(' + ResourceClass.ClassName + ')' + LineEnding +
            '  private' + LineEnding +
            '  public' + LineEnding +
            '    constructor Create;' + LineEnding +
            '    procedure Show;' + LineEnding +
            '  end;' + LineEnding +
//            LineEnding +
            //'{$M-}' + LineEnding +
            //LineEnding + 
//            'var' + LineEnding +  //Declared in thread class now, not global.
//            '  ' + ResourceName + ': T' + ResourceName + ';' + LineEnding +
            LineEnding;
end;

function TFileDescPascalUnitWithExtWindow.GetLocalizedName: string;
begin
  Result := 'ExtPascal Form';
end;

function TFileDescPascalUnitWithExtWindow.GetLocalizedDescription: string;
begin
  Result := 'ExtPascal Form'#13#10#13#10 +
            'Use this to create a new "form" based on an ' +
            'ExtPascal TExtWindow.'#13#10#13#10 +
            'Tip: When designing your form, only add controls from the ' +
            'palette''s ExtPascal tab.';
end;

function TFileDescPascalUnitWithExtWindow.GetImplementationSource(
                                           const Filename     : string;
                                           const SourceName   : string;
                                           const ResourceName : string): string;
begin
  Result := 'uses' + LineEnding +
            '  AppThread;' + LineEnding +
            LineEnding +
            'constructor T' + ResourceName + '.Create;' + LineEnding +
            'begin' + LineEnding +
            '  inherited;' + LineEnding +
            '{$IFDEF UseRuntime}' + LineEnding +  {So Laz IDE doesn't complain}
            ' {$I ' + LowerCase(SourceName) + '.inc}' + LineEnding +
            '{$ENDIF}' + LineEnding +
            'end;' + LineEnding +
            LineEnding +
            'procedure T' + ResourceName + '.Show;' + LineEnding +
            'begin' + LineEnding +
            '  inherited Show;' + LineEnding +
            'end;' + LineEnding +
            LineEnding;
end;


end.

