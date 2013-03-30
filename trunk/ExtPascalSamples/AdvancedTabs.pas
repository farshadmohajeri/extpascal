unit AdvancedTabs;

interface

uses
  Ext;

type
  TAdvancedTabs = class(TExtTabPanel)
  private
    TabIndex, Tabs : integer;
    procedure HandleExtButtonClick(This: TExtButton; E: TExtDirectEvent);
  public
    constructor Create;
  published
    procedure AddTab;
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
    if SelfSession.IsAjax then begin
      Show;
      inc(Tabs);
    end;
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
    Handler  := Ajax(AddTab);
    OnClick  := HandleExtButtonClick; // Delphi style event handler
  end;
  RenderTo    := 'body';
  ActiveTab   := 0;
  Resizable   := true; // turn on tab resizing
  MinTabWidth := 115;
  Width       := 600;
  Height      := 150;
  AutoScroll  := true;
  if Tabs = 0 then
    Tabs := 7
  else
    TabIndex := 0;
  for I := 1 to Tabs do AddTab;
  SelfSession.AddShowSourceButton(Buttons, 'AdvancedTabs');
end;

procedure TAdvancedTabs.HandleExtButtonClick(This: TExtButton; E: TExtDirectEvent); begin
  ExtMessageBox.Alert('alert', 'event handled successfully');
end;

end.
