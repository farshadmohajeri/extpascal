unit AdvancedTabs;

interface

uses
  Ext;

type
  TAdvancedTabs = class(TExtTabPanel)
  private
    TabIndex : integer;
    procedure HandleExtButtonClick(This: TExtButton; E: TExtEventObjectSingleton);
  public
    procedure AddTab;
    constructor Create;
  end;

implementation

uses
  SysUtils, Session;

procedure TAdvancedTabs.AddTab; begin // Ajax
  inc(TabIndex);
  with TExtPanel.AddTo(Items) do begin
    Title    := 'New Tab ' + IntToStr(TabIndex);
    IconCls  := 'tabs';
    Html     := 'Tab Body ' + IntToStr(TabIndex) + '<br/><br/>blahblah';
    Closable := true;
    if SelfSession.IsAjax then Show;
    Free;
  end;
  // Tabs.ActiveTabNumber := TabIndex-1;
end;

constructor TAdvancedTabs.Create;
var
  I : integer;
begin
  inherited;
  with SelfSession do begin
    SetCodePress;
    SetStyle('.new-tab{background-image:url(' + ExtPath + '/examples/feed-viewer/images/new_tab.gif) !important}');
    SetStyle('.tabs{background:url(' + ExtPath + '/examples/desktop/images/tabs.gif)}');
  end;
  with TExtButton.Create do begin
    RenderTo := 'body';
    Text     := 'Add Tab using AJAX!';
    IconCls  := 'new-tab';
    Handler  := Ajax(SelfSession.AddTab);
    OnClick  := HandleExtButtonClick; // Delphi style event handler
  end;
  RenderTo        := 'body';
  ActiveTabNumber := 0;
  ResizeTabs      := true; // turn on tab resizing
  MinTabWidth     := 115;
  TabWidth        := 135;
  Width           := 600;
  Height          := 150;
  Defaults        := JSObject('autoScroll:true');
  EnableTabScroll := true;
  for I := 1 to 7 do AddTab;
  SelfSession.AddShowSourceButton(Buttons, 'AdvancedTabs');
end;

procedure TAdvancedTabs.HandleExtButtonClick(This: TExtButton; E: TExtEventObjectSingleton); begin
  ExtMessageBox.Alert('alert', 'event handled successfully');
end;

end.
