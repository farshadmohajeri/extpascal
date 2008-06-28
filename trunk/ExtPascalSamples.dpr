program ExtPascalSamples;

{$IFDEF MSWINDOWS}{$APPTYPE CONSOLE}{$DEFINE SERVICE}{$ENDIF}

uses
  FCGIApp, ExtPascal, SysUtils, {$IFDEF SERVICE} Classes, Services,{$ENDIF}
  Ext, ExtGlobal, ExtData, ExtForm, ExtGrid, ExtUtil, ExtAir, ExtDD, ExtLayout, ExtMenu, ExtState, ExtTree;

type
  TSamples = class(TExtThread)
  private
  public
    Tabs : TExtTabPanel;
    TabIndex : integer;
    Grid : TExtGridEditorGridPanel;
    DataStore : TExtDataStore;
    Plant : TExtDataRecord;
  published
    procedure Home; override;
    procedure BasicTabPanel;
    procedure MessageBoxes;
    procedure Layout;
    procedure AdvancedTabs;
    procedure AddTab; // Ajax
    procedure BorderLayout;
    procedure ArrayGrid;
    procedure EditableGrid;
    procedure AddPlant; // Ajax
    procedure ReadButtonAjax;
    procedure ReadButtonJS;
  end;

procedure TSamples.Home;
const
  Examples : array[0..6] of record
    Name, Proc, Gif, Desc : string
  end = (
    (Name: 'Basic TabPanel'; Proc: 'BasicTabPanel'; Gif: 'window';        Desc: 'Simple Hello World window that contains a basic TabPanel.'),
    (Name: 'Message Boxes';  Proc: 'MessageBoxes';  Gif: 'msg-box';       Desc: 'Different styles include confirm, alert, prompt, progress, wait and also support custom icons. Calling events passing parameters using AJAX or browser side logic'),
    (Name: 'Layout Window';  Proc: 'Layout';        Gif: 'window-layout'; Desc: 'A window containing a basic BorderLayout with nested TabPanel.'),
    (Name: 'Advanced Tabs';  Proc: 'AdvancedTabs';  Gif: 'tabs-adv';      Desc: 'Advanced tab features including tab scrolling, adding tabs programmatically using AJAX and a context menu plugin.'),
    (Name: 'Border Layout';  Proc: 'BorderLayout';  Gif: 'border-layout'; Desc: 'A complex BorderLayout implementation that shows nesting multiple components and sub-layouts.'),
    (Name: 'Array Grid';     Proc: 'ArrayGrid';     Gif: 'grid-array';    Desc: 'A basic read-only grid loaded from local array data that demonstrates the use of custom column renderer functions.<br/>And a simple modal dialog invoked using AJAX.'),
    (Name: 'Editable Grid';  Proc: 'EditableGrid';  Gif: 'grid-edit';     Desc: 'An editable grid loaded from XML that shows multiple types of grid editors as well adding new custom data records using AJAX.')
  );
var
  I : integer;
begin
  // Theme := 'gray';
  SetStyle('img:hover{border:1px solid blue}');
  with TExtPanel.Create do begin
    Title       := 'ExtPascal Samples';
    RenderTo    := 'body';
    Width       := 400;
    Floating    := true;
    Collapsible := true;
    SetPosition(300, 0);
    for I := 0 to high(Examples) do
      with Examples[I], TExtPanel.AddTo(Items) do begin
        Title := Name;
        Frame := true;
        Html  := '<table><td><a href=' + RequestHeader['SCRIPT_NAME'] + '/' + Proc + ' target=_blank>'+
          '<img src=' + ExtPath + '/examples/shared/screens/' + Gif + '.gif /></a><td/><td>' + Desc + '</td></table>';
        Collapsible := true;
      end;
    //Free;
  end;
end;

procedure TSamples.Layout;
var
  Tabs : TExtTabPanel;
  Nav  : TExtPanel;
begin
  Tabs := TExtTabPanel.Create;
  with Tabs do begin
    Region   := 'center';
    Margins  := '3 3 3 0';
    Defaults := JSObject('autoScroll:true');
    ActiveTabNumber := 0;
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
    Region      := 'west';
    Split       := true;
    Width       := 200;
    Collapsible := true;
    Margins     := '3 0 3 3';
  end;
  with TExtWindow.Create do begin
    Title    := 'Layout Window';
    Closable := true;
    Width    := 600;
    Height   := 350;
    Plain    := true;
    Layout   := 'border';
    Modal    := true;
    Nav.AddTo(Items);
    Tabs.AddTo(Items);
    Show;
    //Free;
  end;
end;

procedure TSamples.AddTab; begin // Ajax
  inc(TabIndex);
  with TExtPanel.AddTo(Tabs.Items) do begin
    Title    := 'New Tab ' + IntToStr(TabIndex);
    IconCls  := 'tabs';
    Html     := 'Tab Body ' + IntToStr(TabIndex) + '<br/><br/>blahblah';
    Closable := true;
    if IsAjax then Show;
    Free;
  end;
  // Tabs.ActiveTabNumber := TabIndex-1;
end;

procedure TSamples.AdvancedTabs;
var
  I : integer;
begin
  SetStyle('.new-tab{background-image:url(' + ExtPath + '/examples/feed-viewer/images/new_tab.gif) !important}');
  SetStyle('.tabs{background:url(' + ExtPath + '/examples/desktop/images/tabs.gif)}');
  SetLibrary(ExtPath + '/examples/tabs/TabCloseMenu.js');
  with TExtButton.Create do begin
    RenderTo := 'body';
    Text     := 'Add Tab using AJAX!';
    IconCls  := 'new-tab';
    Handler  := Ajax(AddTab);
    Free;
  end;
  Tabs := TExtTabPanel.Create;
  with Tabs do begin
    RenderTo        := 'body';
    ActiveTabNumber := 0;
    ResizeTabs      := true; // turn on tab resizing
    MinTabWidth     := 115;
    TabWidth        := 135;
    Width           := 600;
    Height          := 150;
    Defaults        := JSObject('autoScroll:true');
    EnableTabScroll := true;
    Plugins         := JSObject('', 'Ext.ux.TabCloseMenu');
    for I := 1 to 7 do AddTab;
  end;
end;

procedure TSamples.ArrayGrid;
var
  DataStore  : TExtDataSimpleStore;
  ColorValue : TExtFunction;
begin
  // Statefull !!!
  ExtStateManager.SetProvider(TExtStateCookieProvider.Create);
  // create the data store
  DataStore := TExtDataSimpleStore.Create;
  with DataStore do begin
    TExtDataField.AddTo(Fields).Name := 'company';
    with TExtDataField.AddTo(Fields) do begin Name := 'price';     Type_ := 'float' end;
    with TExtDataField.AddTo(Fields) do begin Name := 'change';    Type_ := 'float' end;
    with TExtDataField.AddTo(Fields) do begin Name := 'pctchange'; Type_ := 'float' end;
    with TExtDataField.AddTo(Fields) do begin Name := 'lastchange';Type_ := 'date'; DateFormat := 'n/j h:ia' end;
    Data := JSArray(
      '["3m Co",71.72,0.02,0.03,"9/1 12:00am"],' +
      '["Alcoa Inc",29.01,0.42,1.47,"9/1 12:00am"],' +
      '["Altria Group Inc",83.81,0.28,0.34,"9/1 12:00am"],' +
      '["American Express Company",52.55,0.01,0.02,"9/1 12:00am"],' +
      '["American International Group, Inc.",64.13,0.31,0.49,"9/1 12:00am"],' +
      '["AT&T Inc.",31.61,-0.48,-1.54,"9/1 12:00am"],' +
      '["Boeing Co.",75.43,0.53,0.71,"9/1 12:00am"],' +
      '["Caterpillar Inc.",67.27,0.92,1.39,"9/1 12:00am"],' +
      '["Citigroup, Inc.",49.37,0.02,0.04,"9/1 12:00am"],' +
      '["E.I. du Pont de Nemours and Company",40.48,0.51,1.28,"9/1 12:00am"],' +
      '["Exxon Mobil Corp",68.1,-0.43,-0.64,"9/1 12:00am"],' +
      '["General Electric Company",34.14,-0.08,-0.23,"9/1 12:00am"],' +
      '["General Motors Corporation",30.27,1.09,3.74,"9/1 12:00am"],' +
      '["Hewlett-Packard Co.",36.53,-0.03,-0.08,"9/1 12:00am"],' +
      '["Honeywell Intl Inc",38.77,0.05,0.13,"9/1 12:00am"],' +
      '["Intel Corporation",19.88,0.31,1.58,"9/1 12:00am"],' +
      '["International Business Machines",81.41,0.44,0.54,"9/1 12:00am"],' +
      '["Johnson & Johnson",64.72,0.06,0.09,"9/1 12:00am"],' +
      '["JP Morgan & Chase & Co",45.73,0.07,0.15,"9/1 12:00am"],' +
      '["McDonald\"s Corporation",36.76,0.86,2.40,"9/1 12:00am"],' +
      '["Merck & Co., Inc.",40.96,0.41,1.01,"9/1 12:00am"],' +
      '["Microsoft Corporation",25.84,0.14,0.54,"9/1 12:00am"],' +
      '["Pfizer Inc",27.96,0.4,1.45,"9/1 12:00am"],' +
      '["The Coca-Cola Company",45.07,0.26,0.58,"9/1 12:00am"],' +
      '["The Home Depot, Inc.",34.64,0.35,1.02,"9/1 12:00am"],' +
      '["The Procter & Gamble Company",61.91,0.01,0.02,"9/1 12:00am"],' +
      '["United Technologies Corporation",63.26,0.55,0.88,"9/1 12:00am"],' +
      '["Verizon Communications",35.57,0.39,1.11,"9/1 12:00am"],' +
      '["Wal-Mart Stores, Inc.",45.45,0.73,1.63,"9/1 12:00am"]');
  end;
  with TExtGridGridPanel.Create do begin
    Store := DataStore;
    ColorValue := JSFunction('V', 'if(V>0){return "<span style=''color:green''>" + V + "</span>";}else ' +
      'if(V<0){return "<span style=''color:red''>" + V + "</span>";}' +
      'return V;');
    with TExtGridColumnModel.AddTo(Columns) do begin
      Id        := 'company';
      Header    := 'Company';
      Width     := 160;
      Sortable  := true;
      DataIndex := Id;
    end;
    with TExtGridColumnModel.AddTo(Columns) do begin
      Header    := 'Price';
      Width     := 75;
      Sortable  := true;
      DataIndex := 'price';
      RendererString := 'usMoney';
    end;
    with TExtGridColumnModel.AddTo(Columns) do begin
      Header    := 'Change';
      Width     := 75;
      Sortable  := true;
      DataIndex := 'change';
      Renderer  := ColorValue;
    end;
    with TExtGridColumnModel.AddTo(Columns) do begin
      Header    := '% Change';
      Width     := 75;
      Sortable  := true;
      DataIndex := 'pctchange';
      Renderer  := ColorValue;
    end;
    with TExtGridColumnModel.AddTo(Columns) do begin
      Header    := 'Last Updated';
      Width     := 85;
      Sortable  := true;
      DataIndex := 'lastchange';
      Renderer  := ExtUtilFormat.Date('%0', 'm/d/Y'); // %0..%9 handle internal parameters to events
    end;
    with TExtButton.AddTo(TBarArray) do begin
      Text    := 'Show modal dialog using Ajax';
      Handler := Ajax(Self.Layout);
    end;
    StripeRows := true;
    Height     := 350;
    Width      := 600;
    Title      := 'Array Grid';
    RenderTo   := 'body';
    Frame      := true;
    AutoExpandColumn := 'company';
    TExtGridRowSelectionModel(GetSelectionModel).SelectFirstRow;
    Free;
  end;
end;

procedure TSamples.BasicTabPanel;
var
  Window : TExtWindow;
begin
  Window := TExtWindow.Create;
  with Window do begin
    Title  := 'Hello Dialog';
    Layout := 'fit';
    Plain  := true;
    Width  := 500;
    Height := 300;
    CloseAction := 'hide';
    with TExtTabPanel.AddTo(Items) do begin
      ActiveTabNumber := 0;
      with TExtPanel.AddTo(Items) do begin
        Title := 'Hello World 1';
        Html  := 'Hello...';
      end;
      with TExtPanel.AddTo(Items) do begin
        Title := 'Hello World 2';
        Html  := '...World';
      end;
    end;
    with TExtButton.AddTo(Buttons) do begin
      Text     := 'Submit';
      Disabled := true;
    end;
    with TExtButton.AddTo(Buttons) do begin
      Text    := 'Close';
      Handler := JSFunction('window.close()');// try this: Handler := Window.Close; for another effect.
    end;
    Show;
//    Free;
  end;
end;

procedure TSamples.BorderLayout; begin
  SetStyle('html,body{font:normal 12px verdana;margin:0;padding:0;border:0 none;overflow:hidden;height:100%}' +
	  'p{margin:5px}' +
    '.settings{background:url(' + ExtPath + '/examples/shared/icons/fam/folder_wrench.png)}' +
    '.nav{background:url(' + ExtPath + '/examples/shared/icons/fam/folder_go.png)}');
  with TExtViewport.Create do begin
    Layout := 'border';
    with TExtPanel.AddTo(Items) do begin
      Region := 'north';
      Height := 32;
      Frame  := true;
      Html   := '<p>north - generally for menus, toolbars and/or advertisements</p>';
    end;
    with TExtPanel.AddTo(Items) do begin
      Region  := 'south';
      Html    := '<p>south - generally for informational stuff, also could be for status bar</p>';
      Split   := true;
      Height  := 100;
      Title   := 'South';
      Margins := '0 0 0 0';
      MinSize := 100;
      MaxSize := 200;
      Collapsible := true;
    end;
    with TExtPanel.AddTo(Items) do begin
      Region  := 'east';
      Split   := true;
      Height  := 100;
      Title   := 'East Side';
      Margins := '0 5 0 0';
      Width   := 225;
      Layout  := 'fit';
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
      Region  := 'west';
      Id      := 'west-panel';
      Split   := true;
      Width   := 200;
      Title   := 'West';
      Margins := '0 0 0 5';
      Layout  := 'accordion';
      MinSize := 175;
      MaxSize := 400;
      Collapsible  := true;
      LayoutConfig := JSObject('animate:true');
      with TExtPanel.AddTo(Items) do begin
        Title   := 'Navigation';
        Html    := '<p>Hi. I''m the west panel.</p>';
        Border  := false;
        IconCls := 'nav';
      end;
      with TExtPanel.AddTo(Items) do begin
        Title   := 'Settings';
        Html    := '<p>Some settings in here.</p>';
        Border  := false;
        IconCls := 'settings';
      end;
    end;
    with TExtTabPanel.AddTo(Items) do begin
      Region := 'center';
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
    Free;
  end;
end;

procedure TSamples.AddPlant; begin // Ajax method
  Grid.StopEditing;
  with DataStore do
    Insert(0, JSArray('new ' + Plant.JSName + '({common:"New Plant 1",light:"Mostly Shade",price:0,availDate:(new Date()).clearTime(),indoor:false})'));
  Grid.StartEditing(0, 0);
end;

procedure TSamples.EditableGrid;
var
  Data : TExtObjectList;
begin
//  if Grid <> nil then Grid.Free;
  ExtQuickTips.Init;
  Data := TExtObjectList.Create;
  // the "name" below matches the tag name to read, except "availDate", which is mapped to the tag "availability"
  with TExtDataField.AddTo(Data) do begin Name := 'common';    Type_ := 'string' end;
  with TExtDataField.AddTo(Data) do begin Name := 'botanical'; Type_ := 'string' end;
  TExtDataField.AddTo(Data).Name := 'light';
  with TExtDataField.AddTo(Data) do begin Name := 'price'; Type_ := 'float' end; // automatic date conversions
  with TExtDataField.AddTo(Data) do begin Name := 'availDate'; Mapping := 'availability'; Type_ := 'date'; DateFormat := 'm/d/Y' end;
  with TExtDataField.AddTo(Data) do begin Name := 'indoor'; Type_ := 'bool' end;
  // this could be inline, but we want to define the Plant record, type so we can add records dynamically
  Plant := TExtDataRecord.Create(Data);
  // create the Data Store
  DataStore := TExtDataStore.Create;
  with DataStore do begin
    URL := ExtPath + '/examples/grid/plants.xml';
    // the return will be XML, so lets set up a reader, records will have a "plant" tag
    Reader := TExtDataXmlReader.Create(JSObject('record:"plant"'), Plant);
    SortInfo := JSObject('field:"common", direction:"ASC"');
  end;
  // create the editor grid
  Grid := TExtGridEditorGridPanel.Create;
  with Grid do begin
    Store  := DataStore;
    Width  := 600;
    Height := 300;
    Title  := 'Edit Plants?';
    Frame  := true;
    RenderTo := 'body';
    ClicksToEdit := 1;
    AutoExpandColumn := 'common';
    with TExtGridColumnModel.AddTo(Columns) do begin
      Id := 'common';
      Header := 'Common Name';
      Width  := 220;
      DataIndex := 'common';
      Editor := TExtFormTextField.Create;
      TExtFormTextField(Editor).AllowBlank := false;
    end;
    with TExtGridColumnModel.AddTo(Columns) do begin
      Header := 'Light';
      Width  := 130;
      DataIndex := 'light';
      Editor := TExtFormComboBox.Create;
      with TExtFormComboBox(Editor) do begin
        StoreArray := JSArray('"Shade", "Mostly Shady", "Sun or Shade", "Mostly Sunny", "Sunny"');
        TypeAhead  := true;
        ListClass  := 'x-combo-list-small';
        TriggerAction := 'all';
      end;
    end;
    with TExtGridColumnModel.AddTo(Columns) do begin
      Header := 'Price';
      Width  := 70;
      Align  := 'right';
      DataIndex := 'price';
      RendererString := 'usMoney';
      Editor := TExtFormNumberField.Create;
      with TExtFormNumberField(Editor) do begin
        MaxValue      := 100000;
        AllowBlank    := false;
        AllowNegative := false;
      end;
    end;
    with TExtGridColumnModel.AddTo(Columns) do begin
      Header    := 'Available';
      Width     := 95;
      DataIndex := 'availDate';
      Renderer  := JSFunction('v', 'return v?v.dateFormat("M d, Y"):"";');
      Editor    := TExtFormDateField.Create;
      with TExtFormDateField(Editor) do begin
        Format := 'm/d/y';
        MinValueString   := '01/01/06';
        DisabledDays     := JSArray('0, 6');
        DisabledDaysText := 'Plants are not available on the weekends'
      end;
    end;
    with TExtGridColumnModel.AddTo(Columns) do begin
      Header    := 'Indoor?';
      DataIndex := 'indoor';
      Width     := 55;
      Editor    := TExtFormCheckbox.Create;
      Renderer  := JSFunction('v', 'return "<div class=''x-grid3-check-col"+(v?"-on":"")+"''></div>";');
    end;
    with TExtButton.AddTo(TBarArray) do begin
      Text    := 'Add Plant using AJAX!';
      Handler := Ajax(AddPlant);
    end;
    with TExtButton.AddTo(TBarArray) do begin
      Text    := 'Logout';
      Handler := Ajax(Logout);
    end;
  end;
  DataStore.Load(nil);
end;

procedure TSamples.ReadButtonAjax; begin
  ExtMessageBox.Alert('AJAX: Button clicked', 'You clicked the "' + Query['ButtonID'] + '" button')
end;

procedure TSamples.ReadButtonJS; begin
  ExtMessageBox.Alert('Browser Side: Button clicked', 'You clicked the "%0" button')
end;

procedure TSamples.MessageBoxes;
var
  ShowConfig : TExtShowConfig;
begin
  with TExtPanel.Create do begin
    Title    := 'Message Boxes';
    Width    := 700;
    RenderTo := 'body';
    Frame    := true;
    with TExtButton.AddTo(Buttons) do begin
      Text    := 'Alert Dialog';
      Handler := ExtMessageBox.Alert('Status', 'Changes saved succesfully', Ajax(ReadButtonAjax, ['ButtonID', '%0']));
    end;
    with TExtButton.AddTo(Buttons) do begin
      Text    := 'Confirm Message';
      Handler := ExtMessageBox.Confirm('Confirm', 'Are you sure?', JSFunction(ReadButtonJS));
    end;
    with TExtButton.AddTo(Buttons) do begin
      Text    := 'Prompt Dialog';
      Handler := ExtMessageBox.Prompt('Name', 'Please enter your name:', JSFunction(ReadButtonJS));
    end;
    with TExtButton.AddTo(Buttons) do begin
      Id   := 'ButtonMultiline';
      Text := 'Multi-line prompt dialog';
      ShowConfig := TExtShowConfig.Create;
      with ShowConfig do begin
        Title     := 'Address';
        Msg       := 'Please enter your address:';
        Width     := 300;
        Buttons   := ExtMessageBox.OKCANCEL;
        Multiline := true;
        AnimEl    := 'ButtonMultiline';
        Fn        := JSFunction(ReadButtonJS);
      end;
      Handler := ExtMessageBox.Show(ShowConfig);
      ShowConfig.Free;
    end;
    with TExtButton.AddTo(Buttons) do begin
      Id   := 'Yes/No/Cancel Dialog';
      Text := Id;
      ShowConfig := TExtShowConfig.Create;
      with ShowConfig do begin
        Title   := 'Save Changes?';
        Msg     := 'You are closing a tab that has unsaved changes.<br/>Would you like to save your changes?';
        Icon    := ExtMessageBox.QUESTION;
        Buttons := ExtMessageBox.YESNOCANCEL;
        AnimEl  := Id;
        Fn      := Ajax(ReadButtonAjax, ['ButtonID', '%0']);
      end;
      Handler := ExtMessageBox.Show(ShowConfig);
      ShowConfig.Free;
    end;
    with TExtButton.AddTo(Buttons) do begin
      Id   := 'Progress Dialog';
      Text := Id;
      ShowConfig := TExtShowConfig.Create;
      with ShowConfig do begin
        Title    := 'Please wait';
        Width    := 300;
        Progress := true;
        Msg      := 'Loading items...';
        Wait     := true;
        AnimEl   := Id;
        ProgressText := 'Loading...';
        WaitConfig   := TExtProgressWaitConfig.Create;
        with WaitConfig do begin
          Duration  := 5000;
          Interval  := 500;
          Increment := 11;
          Fn        := ExtMessageBox.Alert('Ok', 'Items loaded.');
          Free;
        end;
      end;
      Handler := ExtMessageBox.Show(ShowConfig);
      ShowConfig.Free;
    end;
    SetStyle('.x-window-dlg .ext-mb-download{background:transparent ' +
      'url(' + ExtPath + '/examples/message-box/images/download.gif) no-repeat top left; height:46px}');
    with TExtButton.AddTo(Buttons) do begin
      Id   := 'Wait Dialog';
      Text := Id;
      ShowConfig := TExtShowConfig.Create;
      with ShowConfig do begin
        Msg    := 'Saving your data, please wait...';
        Width  := 300;
        Wait   := true;
        Icon   := 'ext-mb-download';
        AnimEl := Id;
        ProgressText := 'Saving...';
        WaitConfig   := TExtProgressWaitConfig.Create;
        with WaitConfig do begin
          Duration := 5000;
          Interval := 500;
          Fn       := ExtMessageBox.Hide;
          Free;
        end;
      end;
      Handler := ExtMessageBox.Show(ShowConfig);
      ShowConfig.Free;
    end;
    //Free;
  end;
end;

{$IFNDEF SERVICE}
begin
  Application := TFCGIApplication.Create('ExtPascal Samples 0.8.8', TSamples, 2014, 5);
  Application.Run;
{$ELSE}
type
  TServiceThread = class(TThread)
    procedure Execute; override;
  end;

procedure TServiceThread.Execute; begin
  Application.Run(Self)
end;

begin
  Service := TService.Create('ExtPascal', 'ExtPascal Samples 0.8.8');
  with Service do try
    if Install then
      writeln('Service installed')
    else if Uninstall then
      writeln('Service uninstalled')
    else begin
      Application := TFCGIApplication.Create('ExtPascal Samples 0.8.8', TSamples, 2014, 5);
      if Exists then
        Run([TServiceThread.Create(true)])
      else
        Application.Run;
    end;
  except
    on E : Exception do ReportEventLog(EventError, 1, E.Message);
  end;
{$ENDIF}
end.
