unit IDE;

interface

uses
  Ext;

type
  TIDE = class(TExtViewport)
    constructor Create;
  published
    procedure BuildMenu;
  end;

implementation

uses
  ExtPascalUtils, Session, ExtTree, ExtGrid;

procedure TIDE.BuildMenu;
const
  Menus : array[1..6] of record
    Option : string;
    Level : integer;
    Proc : TExtProcedure;
  end = (
    (Option : 'File'; Level : 0), (Option : 'Edit'; Level : 0), (Option : 'Search'; Level : 0), (Option : 'View'; Level : 0),
    (Option : 'Refactor'; Level : 0), (Option : 'Project'; Level : 0));
var
  I, J, K, MaxMenu : integer;
  Tasks, Options : TStringList;
  Item : TExtMenuItem;
  Menu : array of record
    Option : string;
    Menu   : TExtMenuMenu;
  end;
begin
  Menu := TExtMenuMenu.Create;
  for I := 1 to high(Menus) do begin
    Options.DelimitedText := Tasks[I];
    for J := 0 to Options.Count-1 do
      if Menu[J].Option <> Options[J] then begin
        for K := J+1 to MaxMenu do begin
          Menu[K].Option := '';
          Menu[K].Menu   := nil;
        end;
        Menu[J].Option := Options[J];
        if Menu[J].Menu = nil then begin
          Menu[J].Menu := TExtMenuMenu.Create;
          Item.Menu := Menu[J].Menu;
        end;
        Item := TExtMenuItem.AddTo(Menu[J].Menu.Items);
        Item.Text := Options[J];
      end;
  end;
  with TaskMenu do begin
    AddSeparator;
    Item := TExtMenuItem.Create;
    with Item do begin
      Text    := 'Sair';
      Handler := Ajax(Logout);
      IconCls := 'exit';
    end;
    AddItem(Item);
  end;
end;

constructor TIDE.Create;
var
  Tree : TExtTreeTreePanel;
  Node : TExtTreeTreeNode;
begin
  inherited;
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
  end;
  //set child node
  Node := TExtTreeTreeNode.Create;
  with Node do begin
    Text := 'child0';
  end;
  Tree.Root_.AppendChild(Node);
  Layout := lyBorder;
  with TExtPanel.AddTo(Items) do begin
    //Title       := ServerName;// + Build + ' - Web Server is ' + SelfSession.WebServer;
    AutoWidth  := true;
    BuildMenu(TBarArray);
    TExtToolbarSeparator.AddTo(TBarArray);
    Region := rgNorth;
    Height := 64;
    Frame  := true;
    Collapsible := true;
  end;
  with TExtPanel.AddTo(Items) do begin
    Region  := rgSouth;
    Split   := true;
    Height  := 100;
    Title   := 'Messages';
    Margins := SetMargins(0);
    MinSize := 100;
    MaxSize := 400;
    Collapsible := true;
  end;
  with TExtPanel.AddTo(Items) do begin
    Region  := rgWest;
    Id      := 'ObjectInspector';
    Split   := true;
    Width   := 200;
    Margins := SetMargins(0, 0, 0, 5);
    Layout  := lyAccordion;
    MinSize := 175;
    MaxSize := 400;
    Collapsible  := true;
    LayoutConfig := JSObject('animate:true');
    with TExtPanel.AddTo(Items) do begin
      Title   := 'Object Inspector';
      Border  := false;
      IconCls := 'nav';
      with TExtTabPanel.AddTo(Items) do begin
        Border := false;
        ActiveTabNumber := 0;
        with TExtPanel.AddTo(Items) do begin
          AutoScroll := true;
          Title      := 'Properties';
          with TExtGridPropertyGrid.AddTo(Items) do begin
            //Closable := true;
            Source   := JSObject('"(name)":"Property Grid",grouping:false,autoFitColumns:true,productionQuality:false,' +
              'created:new Date(Date.parse("10/15/2006")),tested:false,version:.01,borderWidth:1');
          end;
        end;
        with TExtPanel.AddTo(Items) do begin
          AutoScroll := true;
          Title      := 'Events';
          with TExtGridPropertyGrid.AddTo(Items) do begin
            Closable := true;
            Source   := JSObject('"(name)":"Property Grid",grouping:false,autoFitColumns:true,productionQuality:false,' +
              'created:new Date(Date.parse("10/15/2006")),tested:false,version:.01,borderWidth:1');
          end;
        end;
      end;
    end;
    with TExtPanel.AddTo(Items) do begin
      Title   := 'Project Manager';
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

end.
