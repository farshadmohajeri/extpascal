unit BorderLayout;

interface

uses
  Ext;

type
  TBorderLayout = class(TExtContainerViewport)
  private
    procedure SelectNodeEventBrowserSide;
  public
    constructor Create;
  published
    procedure SelectNodeEventServerSide; // Ajax
  end;

implementation

uses
  ExtPascalUtils, Session;

constructor TBorderLayout.Create;
var
 Tree : TExtTreePanel;
 Node : TExtDataNodeInterface;
begin
  inherited;
  with SelfSession do begin
    SetCodePress;
    SetStyle('html,body{font:normal 12px verdana;margin:0;padding:0;border:0 none;overflow:hidden;height:100%}' +
      'p{margin:5px}' +
      '.settings{background:url(' + ExtPath + '/examples/shared/icons/fam/folder_wrench.png)}' +
      '.nav{background:url(' + ExtPath + '/examples/shared/icons/fam/folder_go.png)}');
  end;
  Tree := TExtTreePanel.Create;
  Tree.Border := 0;
  //set root node
  Node := TExtDataNodeInterface.Create;
  with Node do begin
    Text := 'Root';
    Root := True;
    Expandable := True;
    Expanded := True;
    Leaf := False;
    AddListener('click', JSFunction(SelectNodeEventBrowserSide));
  end;
  Tree.Root := Node;
  //set child node
  Node := TExtDataNodeInterface.Create;
  with Node do begin
    Text := 'child0';
    AddListener('click', Ajax(SelectNodeEventServerSide, ['Name', '%0.text']));
  end;
  Tree.Root.AppendChild(Node);

  Layout := laBorder;
  with TExtPanel.AddTo(Items) do begin
    Region := reNorth;
    Height := 64;
    Frame  := true;
    Html   := '<p>north - generally for menus, toolbars and/or advertisements</p>';
    SelfSession.AddShowSourceButton(Tbar, 'BorderLayout');
  end;
  with TExtPanel.AddTo(Items) do begin
    Region  := reSouth;
    Html    := '<p>south - generally for informational stuff, also could be for status bar</p>';
    Split   := true;
    Height  := 100;
    Title   := 'South';
    Margin  := 0;
    MinSize := 100;
    MaxSize := 200;
    Collapsible := true;
  end;
  with TExtPanel.AddTo(Items) do begin
    Region  := reEast;
    Split   := true;
    Height  := 100;
    Title   := 'East Side';
    MarginString := SetMargins(0, 5);
    Width   := 225;
    Layout  := laFit;
    MinSize := 175;
    MaxSize := 400;
    Collapsible := true;
    with TExtTabPanel.AddTo(Items) do begin
      Border := 0;
      TabPosition := taBottom;
      ActiveTab := 1;
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
    Region  := reWest;
    Id      := 'west-panel';
    Split   := true;
    Width   := 200;
    Title   := 'West';
    MarginString := SetMargins(0, 0, 0, 5);
    Layout  := laAccordion;
    MinSize := 175;
    MaxSize := 400;
    Collapsible  := true;
    AnimCollapse := true;
    with TExtPanel.AddTo(Items) do begin
      Title   := 'Navigation';
      Html    := '<p>Hi. I''m the west panel.</p>';
      Border  := 0;
      IconCls := 'nav';
      Tree.AddTo(Items);
    end;
    with TExtPanel.AddTo(Items) do begin
      Title   := 'Settings';
      Html    := '<p>Some settings in here.</p>';
      Border  := 0;
      IconCls := 'settings';
    end;
  end;
  with TExtTabPanel.AddTo(Items) do begin
    Region := reCenter;
    DeferredRender  := false;
    ActiveTab := 0;
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
