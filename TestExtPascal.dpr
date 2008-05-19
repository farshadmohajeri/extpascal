program TestExtPascal;

{$APPTYPE CONSOLE}

uses
  SysUtils, FCGIApp, ExtPascal, Ext, ExtGlobal, ExtData, ExtForm, ExtGrid,
  ExtUtil, ExtAir, ExtDD, ExtLayout, ExtMenu, ExtState, ExtTree;

type
  TSamples = class(TExtThread)
  public
    Tabs : ExtTabPanel;
    TabIndex : integer;
  published
    procedure Home; override;
    procedure BasicTabPanel;
    procedure MessageBoxes;
    procedure Layout;
    procedure AdvancedTabs;
    procedure AddTab;
  end;

procedure TSamples.Home;
const
  Examples : array[0..3] of record
    Name, Proc, Gif, Desc : string
  end = (
    (Name: 'Basic TabPanel'; Proc: 'BasicTabPanel'; Gif: 'window'; Desc: 'Simple Hello World window that contains a basic TabPanel.'),
    (Name: 'Message Boxes';  Proc: 'MessageBoxes';  Gif: 'msg-box'; Desc: 'Different styles include confirm, alert, prompt, progress, wait and also support custom icons.'),
    (Name: 'Layout Window';  Proc: 'Layout';        Gif: 'window-layout'; Desc: 'A window containing a basic BorderLayout with nested TabPanel.'),
    (Name: 'Advanced Tabs';  Proc: 'AdvancedTabs';  Gif: 'tabs-adv'; Desc: 'Advanced tab features including tab scrolling, adding tabs programmatically and a context menu plugin.')
  );
var
  I : integer;
begin
  // Theme := 'gray';
  SetStyle('img:hover{border:1px solid blue}');
  with ExtPanel.Create do begin
    Title       := 'ExtPascal Samples';
    RenderTo    := 'content';
    Width       := 400;
    floating    := true;
    Collapsible := true;
    SetPosition(300, 50);
    for I := 0 to high(Examples) do
      with Examples[I], ExtPanel.AddTo(Items) do begin
        Title := Name;
        Frame := true;
        Collapsible := true;
        Html := '<center><a href=/extpascal/testextpascal.exe/' + Proc + ' target=_blank>'+
          '<img src=/ext-2.1/examples/shared/screens/' + Gif + '.gif /><p>' + Desc + '</a></center>';
      end;
    Free;
  end;
end;

procedure TSamples.Layout;
var
  Tabs : ExtTabPanel;
  Nav  : ExtPanel;
begin
  Tabs := ExtTabPanel.Create;
  with Tabs do begin
    Region := 'center';
    Margins:= '3 3 3 0';
    ActiveTabNumber := 0;
    AddJS('defaults:{autoScroll:true}');
    with ExtPanel.AddTo(Items) do begin
      Title := 'Bogus Tab';
      Html := 'Blah blah blah';
    end;
    with ExtPanel.AddTo(Items) do begin
      Title := 'Another Tab';
      Html := 'Blah blah blah';
    end;
    with ExtPanel.AddTo(Items) do begin
      Title := 'Closable Tab';
      Html := 'Blah blah blah';
      Closable := true;
    end;
  end;
  Nav := ExtPanel.Create;
  with Nav do begin
    Title := 'Navigation';
    Region := 'west';
    Split := true;
    Width := 200;
    Collapsible := true;
    Margins := '3 0 3 3';
  end;
  with ExtWindow.Create do begin
    Title := 'Layout Window';
    Closable := true;
    Width := 600;
    Height := 350;
    Plain := true;
    Layout := 'border';
    Nav.AddTo(Items);
    Tabs.AddTo(Items);
    Show;
    Free;
  end;
end;

procedure TSamples.AddTab; begin
  inc(TabIndex);
  with ExtPanel.AddTo(Tabs.Items) do begin
    Title := 'New Tab ' + IntToStr(TabIndex);
    IconCls := 'tabs';
    Html := 'Tab Body ' + IntToStr(TabIndex) + '<br/><br/>blahblah';
    Closable := true;
  end;
end;

procedure TSamples.AdvancedTabs;
var
  I : integer;
begin
  SetStyle('.new-tab{background-image:url(/ext-2.1/examples/feed-viewer/images/new_tab.gif) !important}');
  SetStyle('.tabs{background-image:url(/ext-2.1/examples/desktop/images/tabs.gif ) !important}');
  with ExtButton.Create do begin
    RenderTo := 'content';
    Text := 'Add Tab';
    IconCls := 'new-tab';
    Handler := _Function(Ajax('AddTab'));
  end;
  Tabs := ExtTabPanel.Create;
  with Tabs do begin
    RenderTo := 'content';
    ActiveTabNumber := 0;
    ResizeTabs := true; // turn on tab resizing
    MinTabWidth:= 115;
    TabWidth := 135;
    EnableTabScroll :=true;
    Width := 600;
    Height := 250;
    for I := 1 to 7 do AddTab;
    // defaults: {autoScroll:true},
    // plugins: new Ext.ux.TabCloseMenu()
  end;
end;

procedure TSamples.BasicTabPanel;
var
  Window : ExtWindow;
begin
  Window := ExtWindow.Create;
  with Window do begin
    Title  := 'Hello Dialog';
    Layout := 'fit';
    Plain  := true;
    Width  := 500;
    Height := 300;
    CloseAction := 'hide';
    with ExtTabPanel.AddTo(Items) do begin
      ActiveTabNumber := 0;
      with ExtPanel.AddTo(Items) do begin
        Title := 'Hello World 1';
        Html  := 'Hello...';
      end;
      with ExtPanel.AddTo(Items) do begin
        Title := 'Hello World 2';
        Html  := '...World';
      end;
    end;
    with ExtButton.AddTo(Buttons) do begin
      Text     := 'Submit';
      Disabled := true;
    end;
    with ExtButton.AddTo(Buttons) do begin
      Text    := 'Close';
      Handler := Window.Close;
    end;
    Show;
    Free;
  end;
end;

procedure TSamples.MessageBoxes;
var
  ShowConfig : ExtShowConfig;
begin
  with ExtPanel.Create do begin
    Title    := 'Message Boxes';
    Width    := 700;
    RenderTo := 'content';
    Frame    := true;
    with ExtButton.AddTo(Buttons) do begin
      Text    := 'Alert Dialog';
      Handler := ExtMessageBox.Alert('Status', 'Changes saved succesfully');
    end;
    with ExtButton.AddTo(Buttons) do begin
      Text    := 'Confirm Message';
      Handler := ExtMessageBox.Confirm('Confirm', 'Are you sure?');
    end;
    with ExtButton.AddTo(Buttons) do begin
      Text    := 'Prompt Dialog';
      Handler := ExtMessageBox.Prompt('Name', 'Please enter your name:');
    end;
    with ExtButton.AddTo(Buttons) do begin
      Id   := 'ButtonMultiline';
      Text := 'Multi-line prompt dialog';
      ShowConfig := ExtShowConfig.Create;
      with ShowConfig do begin
        Title     := 'Address';
        Msg       := 'Please enter your address:';
        Width     := 300;
        Buttons   := ExtMessageBox.OKCANCEL;
        Multiline := true;
        AnimEl    := 'ButtonMultiline';
      end;
      Handler := ExtMessageBox.Show(ShowConfig);
      ShowConfig.Free;
    end;
    with ExtButton.AddTo(Buttons) do begin
      Id   := 'Yes/No/Cancel Dialog';
      Text := Id;
      ShowConfig := ExtShowConfig.Create;
      with ShowConfig do begin
        Title   := 'Save Changes?';
        Msg     := 'You are closing a tab that has unsaved changes.<p>Would you like to save your changes?';
        Icon    := ExtMessageBox.QUESTION;
        Buttons := ExtMessageBox.YESNOCANCEL;
        AnimEl  := Id;
      end;
      Handler := ExtMessageBox.Show(ShowConfig);
      ShowConfig.Free;
    end;
    with ExtButton.AddTo(Buttons) do begin
      Id   := 'Progress Dialog';
      Text := Id;
      ShowConfig := ExtShowConfig.Create;
      with ShowConfig do begin
        Title   := 'Please wait';
        Width   := 300;
        Progress:= true;
        Msg     := 'Loading items...';
        Wait    := true;
        AnimEl  := Id;
        ProgressText := 'Loading...';
        WaitConfig := ExtProgressWaitConfig.Create;
        with WaitConfig do begin
          Duration  := 5000;
          Interval  := 500;
          Increment := 11;
          Fn := ExtMessageBox.Alert('Ok', 'Items loaded.');
          Free;
        end;
      end;
      Handler := ExtMessageBox.Show(ShowConfig);
      ShowConfig.Free;
    end;
    SetStyle('.x-window-dlg .ext-mb-download{background:transparent ' +
      'url(/ext-2.1/examples/message-box/images/download.gif) no-repeat top left; height:46px}');
    with ExtButton.AddTo(Buttons) do begin
      Id   := 'Wait Dialog';
      Text := Id;
      ShowConfig := ExtShowConfig.Create;
      with ShowConfig do begin
        Msg    := 'Saving your data, please wait...';
        Width  := 300;
        Wait   := true;
        Icon   := 'ext-mb-download';
        AnimEl := Id;
        ProgressText := 'Saving...';
        WaitConfig := ExtProgressWaitConfig.Create;
        with WaitConfig do begin
          Duration := 5000;
          Interval := 500;
          Fn := ExtMessageBox.Hide;
          Free;
        end;
      end;
      Handler := ExtMessageBox.Show(ShowConfig);
      ShowConfig.Free;
    end;
    Free;
  end;
end;

begin
  Application := TFCGIApplication.Create('ExtPascal Samples 0.8.1', TSamples);
  Application.Run;
end.
