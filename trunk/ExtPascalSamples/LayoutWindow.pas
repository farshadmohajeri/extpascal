unit LayoutWindow;

interface

uses
  Ext;

type
  TLayoutWindow = class(TExtWindow)
    constructor Create;
    procedure ExtTabPanelOnTabchange(AThis : TExtTabPanel; NewTab, OldTab : TExtPanel);
  end;

implementation

uses
  Session, ExtPascalUtils;

constructor TLayoutWindow.Create;
var
  Tabs : TExtTabPanel;
  Nav  : TExtPanel;
begin
  inherited;
  SelfSession.SetCodePress;
  Tabs := TExtTabPanel.AddTo(Items);
  with Tabs do begin
    Region   := reCenter;
    Margin   := 3;
    Defaults := JSObject('autoScroll:true');
    ActiveTab := 0;
    OnTabChange := ExtTabPanelOnTabchange;
    with TExtPanel.AddTo(Items) do begin
      Title := 'Bogus Tab';
      Html  := 'Blah blah blah';
    end;
    with TExtPanel.AddTo(Items) do begin
      Title := 'Another Tab';
      Html  := 'Blah blah blah';
    end;
    with TExtPanel.AddTo(Items) do begin
      Title := 'Closable Tab';
      Html  := 'Blah blah blah';
      Closable := true;
    end;
  end;
  Nav := TExtPanel.Create;
  with Nav do begin
    Title       := 'Navigation';
    Region      := reWest;
    Split       := true;
    Width       := 200;
    Collapsible := true;
    MarginString:= SetMargins(3, 0, 3, 3);
  end;
  Title    := 'Layout Window';
  Closable := true;
  Width    := 600;
  Height   := 350;
  Plain    := true;
  Layout   := laBorder;
  Modal    := true;
  Nav.AddTo(Items);
  SelfSession.AddShowSourceButton(Buttons, 'LayoutWindow');
end;

procedure TLayoutWindow.ExtTabPanelOnTabchange(AThis : TExtTabPanel; NewTab, OldTab : TExtPanel); begin
   ExtMessageBox.Alert('Active Tab is', NewTab.Title);
end;

end.
