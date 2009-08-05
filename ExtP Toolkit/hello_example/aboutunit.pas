unit aboutunit;

interface

uses
  SysUtils, Classes, 
{$IFDEF UseRuntime}
  Ext, ExtPascal, ExtPascalUtils, ExtForm, 
  ExtGlobal, ExtData, ExtGrid, ExtUtil, ExtAir, ExtDd, 
  ExtLayout, ExtMenu, ExtState, ExtTree
{$ELSE}
  ExtP_Design_Ctrls
{$ENDIF}; 

type

  { TAboutWindow }

  TAboutWindow = class(TExtWindow)
    ExtButton1: TExtButton;
    procedure ExtButton1Click;
  private
  public
    constructor Create; 
    procedure Show; 
  end; 

var
  AboutWindow: TAboutWindow;

implementation

uses
  AppThread; 

procedure TAboutWindow.ExtButton1Click;
begin
  Close;
end;

constructor TAboutWindow.Create;
begin
  inherited; 
{$IFDEF UseRuntime}
 {$I aboutunit.inc}
{$ENDIF}
end; 

procedure TAboutWindow.Show;
begin
  inherited Show; 
end; 

end.
