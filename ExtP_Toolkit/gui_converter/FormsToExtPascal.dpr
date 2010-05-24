program FormsToExtPascal;

uses
{$IFDEF LCL}
  Interfaces,
{$ENDIF}
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  CustCfgEdit in 'CustCfgEdit.pas' {CustConfigEditor};

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

