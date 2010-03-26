unit BorderLayout;

interface

uses
  Ext;

type
  TBorderLayout = class(TExtViewport)
  private
    procedure SelectNodeEventBrowserSide;
  public
    procedure SelectNodeEventServerSide;  // Ajax
    constructor Create;
  end;

implementation

uses
  ExtPascalUtils, Session, ExtTree, ExtGrid;

constructor TBorderLayout.Create;
var
 Tree : TExtTreeTreePanel;
 Node : TExtTreeTreeNode;
begin
  inherited;
  with SelfSession do begin
    SetCodePress;
    SetStyle('html,body{font:normal 12px verdana;margin:0;padding:0;border:0 none;overflow:hidden;height:100%}' +
      'p{margin:5px}' +
      '.settings{background:url(' + ExtPath + '/examples/shared/icons/fam/folder_wrench.png)}' +
      '.nav{background:url(' + ExtPath + '/examples/shared/icons/fam/folder_go.png)}');
  end;
  Tree := TExtTreeTreePanel.Create;
  Tree.Border := false;
  //set root node
  Tree.Root := TExtTreeTreeNode.Create;
  with Tree.Root do begin
    Text := 'Root';
    AllowChildren := True;
    Expandable := True;
    Expanded := True;
    Leaf := False;
    on('click', JSFunction(SelectNodeEventBrowserSide));
  end;
  //set child node
  Node := TExtTreeTreeNode.Create;
  with Node do begin
    Text := 'child0';
    on('click', Ajax(SelfSession.SelectNodeEventServerSide, ['Name', '%0.text']));
  end;
  Tree.Root_.AppendChild(Node);

  Layout := lyBorder;
  with TExtPanel.AddTo(Items) do begin
    Region := rgNorth;
    Height := 64;
    Frame  := true;
    Html   := '<p>north - generally for menus, toolbars and/or advertisements</p>';
    SelfSession.AddShowSourceButton(TbarArray, 'BorderLayout');
  end;
  with TExtPanel.AddTo(Items) do begin
    Region  := rgSouth;
    Html    := '<p>south - generally for informational stuff, also could be for status bar</p>';
    Split   := true;
    Height  := 100;
    Title   := 'South';
    Margins := SetMargins(0);
    MinSize := 100;
    MaxSize := 200;
    Collapsible := true;
  end;
  with TExtPanel.AddTo(Items) do begin
    Region  := rgEast;
    Split   := true;
    Height  := 100;
    Title   := 'East Side';
    Margins := SetMargins(0, 5);
    Width   := 225;
    Layout  := lyFit;
    MinSize := 175;
    MaxSize := 400;
    Collapsible := true;
    with TExtTabPanel.AddTo(Items) do begin
      Border := false;
      TabPosition := 'bottom';
      ActiveTabNumber := 1;
      with TExtPanel.AddTo(Items) do begin
        Html  := '<p>A TabPanel component can be a region.</p>';
        Title := 'A Tab';
        AutoScroll := true;
      end;
      with TExtGridPropertyGrid.AddTo(Items) do begin
        Title    := 'Property Grid';
        Closable := true;
        Source   := JSObject('"(name)":"Property Grid",grouping:false,autoFitColumns:true,productionQuality:false,' +
          'created:new Date(Date.parse("10/15/2006")),tested:false,version:.01,borderWidth:1');
      end;
    end;
  end;
  with TExtPanel.AddTo(Items) do begin
    Region  := rgWest;
    Id      := 'west-panel';
    Split   := true;
    Width   := 200;
    Title   := 'West';
    Margins := SetMargins(0, 0, 0, 5);
    Layout  := lyAccordion;
    MinSize := 175;
    MaxSize := 400;
    Collapsible  := true;
    LayoutConfig := JSObject('animate:true');
    with TExtPanel.AddTo(Items) do begin
      Title   := 'Navigation';
      Html    := '<p>Hi. I''m the west panel.</p>';
      Border  := false;
      IconCls := 'nav';
      Tree.AddTo(Items);
    end;
    with TExtPanel.AddTo(Items) do begin
      Title   := 'Settings';
      Html    := '<p>Some settings in here.</p>';
      Border  := false;
      IconCls := 'settings';
    end;
  end;
  with TExtTabPanel.AddTo(Items) do begin
    Region := rgCenter;
    DeferredRender  := false;
    ActiveTabNumber := 0;
    with TExtPanel.AddTo(Items) do begin
      Title      := 'Close Me';
      Html       := '<p><b>Done reading me? Close me by clicking the X in the top right corner.</b></p>';
      Closable   := true;
      AutoScroll := true
    end;
    with TExtPanel.AddTo(Items) do begin
      Title      := 'Center Panel';
      Html       := '<p>The center panel automatically grows to fit the remaining space in the container that isn''t taken up by the border regions.</p>';
      AutoScroll := true
    end;
  end;
end;

procedure TBorderLayout.SelectNodeEventBrowserSide; begin
  ExtMessageBox.Alert('Browser Side', '%0.text')
end;

procedure TBorderLayout.SelectNodeEventServerSide; begin
  ExtMessageBox.Alert('Server Side', SelfSession.Query['Name']);
end;

end.
