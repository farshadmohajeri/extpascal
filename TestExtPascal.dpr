program TestExtPascal;

{$APPTYPE CONSOLE}

uses
  FCGIApp, ExtPascal, Ext, ExtGlobal, ExtData, ExtForm, ExtGrid,
  ExtUtil, ExtAir, ExtDD, ExtLayout, ExtMenu, ExtState, ExtTree;

type
  TSamples = class(TExtThread)
    procedure Home; override;
    procedure BasicTabPanel;
    procedure MessageBoxes;
  end;

procedure TSamples.BasicTabPanel;
var
  Window : ExtWindow;
begin
  Window := ExtWindow.Create;
  with Window do begin
    Title := 'Hello Dialog';
    Layout := 'fit';
    Width := 500;
    Height := 300;
    CloseAction := 'hide';
    Plain := true;
    with ExtTabPanel.AddTo(Items) do begin
      ActiveTab := 'tab1';
      with ExtPanel.AddTo(Items) do begin
        Id := 'tab1';
        Title := 'Hello World 1';
        Html := 'Hello...';
      end;
      with ExtPanel.AddTo(Items) do begin
        Title := 'Hello World 2';
        Html := '...World';
      end;
    end;
    with ExtButton.AddTo(Buttons) do begin
      Text := 'Submit';
      Disabled := true;
    end;
    with ExtButton.AddTo(Buttons) do begin
      Text := 'Close';
      Handler := _Function.JSFunction('', Window.JSName + '.hide()');
    end;
    Show;
    free;
  end;
end;

procedure TSamples.Home;
const
  Examples : array[0..1] of record
    Name, Proc, Gif : string
  end = (
    (Name: 'Basic TabPanel'; Proc: 'BasicTabPanel'; Gif: 'window'),
    (Name: 'Message Boxes';  Proc: 'MessageBoxes';  Gif: 'msg-box')
  );
var
  I : integer;
begin
  SetStyle('img:hover{border:1px solid blue}');
  with ExtPanel.Create do begin
    RenderTo := 'content';
    Title := 'ExtPascal Samples';
    Collapsible := true;
    Width := 400;
    floating := true;
    for I := 0 to high(Examples) do
      with Examples[I], ExtPanel.AddTo(Items) do begin
        Title := Name;
        Collapsible := true;
        Frame := true;
        //AddJS(JSName + '.on("click", "window.open();");');
        html := '<center><a href=/extpascal/testextpascal.exe/' + Proc + ' target=_blank>'+
          '<img src=/ext-2.1/examples/shared/screens/' + Gif + '.gif /></a></center>';
      end;
    SetPosition(300, 50);
  end;
(*
    var catalog = [{
        title: 'Combination Samples',
        samples: [{
            text: 'Feed Viewer 2.0',
            url: 'feed-viewer/view.html',
            icon: 'feeds.gif',
            desc: 'RSS 2.0 feed reader sample application that features a swappable reader panel layout.'
        },{
            text: 'Simple Tasks 2.0',
            url: 'http://extjs.com/blog/2008/02/24/tasks2/',
            icon: 'air.gif',
            desc: 'Complete personal task management application sample that runs on <a href="http://labs.adobe.com/technologies/air/" target="_blank">Adobe AIR</a>.'
        },{
            text: 'Simple Tasks',
            url: 'tasks/tasks.html',
            icon: 'tasks.gif',
            desc: 'Personal task management application sample that uses <a href="http://gears.google.com" target="_blank">Google Gears</a> for data storage.'
        },{
            text: 'Image Organizer',
            url: 'organizer/organizer.html',
            icon: 'organizer.gif',
            desc: 'DataView and TreePanel sample that demonstrates dragging data items from a DataView into a TreePanel.'
        },{
            text: 'Web Desktop',
            url: 'desktop/desktop.html',
            icon: 'desktop.gif',
            desc: 'Demonstrates how one could build a desktop in the browser using Ext components including a module plugin system.'
        }]
    },{
        title: 'Grids',
        samples: [{
            text: 'Basic Array Grid',
            url: 'grid/array-grid.html',
            icon: 'grid-array.gif',
            desc: 'A basic read-only grid loaded from local array data that demonstrates the use of custom column renderer functions.'
        },{
            text: 'Editable Grid',
            url: 'grid/edit-grid.html',
            icon: 'grid-edit.gif',
            desc: 'An editable grid loaded from XML that shows multiple types of grid ediors as well as defining custom data records.'
        },{
            text: 'XML Grid',
            url: 'grid/xml-grid.html',
            icon: 'grid-xml.gif',
            desc: 'A simple read-only grid loaded from XML data.'
        },{
            text: 'Paging',
            url: 'grid/paging.html',
            icon: 'grid-paging.gif',
            desc: 'A grid with paging, cross-domain data loading and custom- rendered expandable row bodies.'
        },{
            text: 'Grouping',
            url: 'grid/grouping.html',
            icon: 'grid-grouping.gif',
            desc: 'A basic grouping grid showing collapsible data groups that can be customized via the "Group By" header menu option.'
        },{
            text: 'Live Group Summary',
            url: 'grid/totals.html',
            icon: 'grid-summary.gif',
            desc: 'Advanced grouping grid that allows cell editing and includes custom dynamic summary calculations.'
        },{
            text: 'Grid Plugins',
            url: 'grid/grid3.html',
            icon: 'grid-plugins.gif',
            desc: 'Multiple grids customized via plugins: expander rows, checkbox selection and row numbering.'
        },{
            text: 'Grid Filtering',
            url: 'grid-filtering/grid-filter.html',
            icon: 'grid-filter.gif',
            desc: 'Grid plugins providing custom data filtering menus that support various data types.'
        },{
            text: 'Grid From Markup',
            url: 'grid/from-markup.html',
            icon: 'grid-from-markup.gif',
            desc: 'Custom GridPanel extension that can convert a plain HTML table into a dynamic grid at runtime.'
        },{
            text: 'Grid Data Binding (basic)',
            url: 'grid/binding.html',
            icon: 'grid-data-binding.gif',
            desc: 'Data binding a grid to a detail preview panel via the grid\'s RowSelectionModel.'
        },{
            text: 'Grid Data Binding (advanced)',
            url: 'grid/binding-with-classes.html',
            icon: 'grid-data-binding.gif',
            desc: 'Refactoring the basic data binding example to use a class-based application design model.'
        }]
    },{
        title: 'Tabs',
        samples: [{
            text: 'Basic Tabs',
            url: 'tabs/tabs.html',
            icon: 'tabs.gif',
            desc: 'Basic tab functionality including autoHeight, tabs from markup, Ajax loading and tab events.'
        },{
            text: 'Advanced Tabs',
            url: 'tabs/tabs-adv.html',
            icon: 'tabs-adv.gif',
            desc: 'Advanced tab features including tab scrolling, adding tabs programmatically and a context menu plugin.'
        }]
    },{
        title: 'Windows',
        samples: [{
            text: 'Hello World',
            url: 'window/hello.html',
            icon: 'window.gif',
            desc: 'Simple "Hello World" window that contains a basic TabPanel.'
        },{
            text: 'MessageBox',
            url: 'message-box/msg-box.html',
            icon: 'msg-box.gif',
            desc: 'Different styles include confirm, alert, prompt, progress and wait and also support custom icons.'
        },{
            text: 'Layout Window',
            url: 'window/layout.html',
            icon: 'window-layout.gif',
            desc: 'A window containing a basic BorderLayout with nested TabPanel.'
        }]
    },{
        title: 'Trees',
        samples: [{
            text: 'Drag and Drop Reordering',
            url: 'tree/reorder.html',
            icon: 'tree-reorder.gif',
            desc: 'A TreePanel loaded asynchronously via a JSON TreeLoader that shows drag and drop with container scroll.'
        },{
            text: 'Multiple trees',
            url: 'tree/two-trees.html',
            icon: 'tree-two.gif',
            desc: 'Drag and drop between two different sorted TreePanels.'
        },{
            text: 'Column Tree',
            url: 'tree/column-tree.html',
            icon: 'tree-columns.gif',
            desc: 'A custom TreePanel implementation that demonstrates extending an existing component.'
        }]
    },{
        title: 'Layout Managers',
        samples: [{
            text: 'Layout Browser',
            url: 'layout-browser/layout-browser.html',
            icon: 'layout-browser.gif',
            desc: 'Includes examples for each standard Ext layout, several custom layouts and combination examples.'
        },{
            text: 'Border Layout',
            url: 'layout/complex.html',
            icon: 'border-layout.gif',
            desc: 'A complex BorderLayout implementation that shows nesting multiple components and sub-layouts.'
        },{
            text: 'Anchor Layout',
            url: 'form/anchoring.html',
            icon: 'anchor.gif',
            desc: 'A simple example of anchoring form fields to a window for flexible form resizing.'
        },{
            text: 'Portal Demo',
            url: 'portal/portal.html',
            icon: 'portal.gif',
            desc: 'A page layout using several custom extensions to provide a web portal interface.'
        }]
    },{
        title: 'ComboBox',
        samples: [{
            text: 'Basic ComboBox',
            url: 'form/combos.html',
            icon: 'combo.gif',
            desc: 'Basic combos, combos rendered from markup and customized list layout to provide item tooltips.'
        },{
            text: 'ComboBox Templates',
            url: 'form/forum-search.html',
            icon: 'combo-custom.gif',
            desc: 'Customized combo with template-based list rendering, remote loading and paging.'
        }]
    },{
        title: 'Forms',
        samples: [{
            text: 'Dynamic Forms',
            url: 'form/dynamic.html',
            icon: 'form-dynamic.gif',
            desc: 'Various example forms showing collapsible fieldsets, column layout, nested TabPanels and more.'
        },{
            text: 'Ajax with XML Forms',
            url: 'form/xml-form.html',
            icon: 'form-xml.gif',
            desc: 'Ajax-loaded form fields from remote XML data and remote field validation on submit.'
        },{
            text: 'Custom Search Field',
            url: 'form/custom.html',
            icon: 'form-custom.gif',
            desc: 'A TriggerField search extension combined with an XTemplate for custom results rendering.'
        },{
            text: 'Binding a Grid to a Form',
            url: 'form/form-grid.html',
            icon: 'form-grid-binding.gif',
            desc: 'A grid embedded within a FormPanel that automatically loads records into the form on row selection.'
        },{
            text: 'Advanced Validation',
            url: 'form/adv-vtypes.html',
            icon: 'form-adv-vtypes.gif',
            desc: 'Relational form field validation using custom vtypes.'
        }]
    },{
        title: 'Toolbars and Menus',
        samples: [{
            text: 'Basic Toolbar',
            url: 'menu/menus.html',
            icon: 'toolbar.gif',
            desc: 'Toolbar and menus that contain various components like date pickers, color pickers, sub-menus and more.'
        },{
            text: 'Ext Actions',
            url: 'menu/actions.html',
            icon: 'toolbar-actions.gif',
            desc: 'Bind the same behavior to multiple buttons, toolbar and menu items using the Ext.Action class.'
        }]
    },{
        title: 'Templates and DataView',
        samples: [{
            text: 'Templates',
            url: 'core/templates.html',
            icon: 'templates.gif',
            desc: 'A simple example of rendering views from templates bound to data objects.'
        },{
            text: 'DataView',
            url: 'view/data-view.html',
            icon: 'data-view.gif',
            desc: 'A basic DataView with custom plugins for editable labels and drag selection of items.'
        },{
            text: 'DataView (advanced)',
            url: 'view/chooser.html',
            icon: 'chooser.gif',
            desc: 'A more customized DataView supporting sorting and filtering with multiple templates.'
        }]
    },{
        title: 'Miscellaneous',
        samples: [{
            text: 'Slider',
            url: 'slider/slider.html',
            icon: 'slider.gif',
            desc: 'A slider component that supports vertical mode, snapping, tooltips, customized styles and more.'
        },{
            text: 'Custom Drag and Drop',
            url: 'dd/dragdropzones.html',
            icon: 'dd-zones.gif',
            desc: 'Enabling drag and drop between a DataView and a grid using DragZone and DropZone extensions.'
        },{
            text: 'QuickTips',
            url: 'simple-widgets/qtips.html',
            icon: 'qtips.gif',
            desc: 'Various tooltip and quick tip configuration options including Ajax loading and mouse tracking.'
        },{
            text: 'Progress Bar',
            url: 'simple-widgets/progress-bar.html',
            icon: 'progress.gif',
            desc: 'A basic progress bar component shown in various configurations and with custom styles.'
        },{
            text: 'Panels',
            url: 'panel/panels.html',
            icon: 'panel.gif',
            desc: 'A basic collapsible panel example.'
        },{
            text: 'Resizable',
            url: 'resizable/basic.html',
            icon: 'resizable.gif',
            desc: 'Examples of making any element resizable with various configuration options.'
        },{
            text: 'Spotlight',
            url: 'core/spotlight.html',
            icon: 'spotlight.gif',
            desc: 'A utility for masking everything except a single element on the page to visually highlight it.'
        },{
            text: 'Localization (static)',
            url: 'locale/dutch-form.html',
            icon: 'locale-dutch.gif',
            desc: 'Demonstrates fully localizing a form by including a custom locale script.'
        },{
            text: 'Localization (dynamic)',
            url: 'locale/multi-lang.html',
            icon: 'locale-switch.gif',
            desc: 'Dynamically render various Ext components in different locales by selecting from a locale list.'
        }]
    }];

*)
end;

procedure TSamples.MessageBoxes; begin
  with ExtPanel.Create do begin
    RenderTo := 'content';
    Title := 'Message Boxes';
    Width := 300;
    Floating := true;
    SetPosition(300, 50);
    with ExtButton.AddTo(Buttons) do begin
      Text := 'Confirm Message';
      //handler := nil; // dá erro
      Handler := _Function(ExtMessageBox.Confirm('Confirm', 'Are you sure?'));
    end;
    with ExtButton.AddTo(Buttons) do begin
      Text := 'Prompt Dialog';
      Handler := _Function(ExtMessageBox.Prompt('Name', 'Please enter your name:'));
    end;
    with ExtButton.AddTo(Buttons) do begin
      Text := 'Alert Dialog';
      Handler := _Function(ExtMessageBox.Alert('Status', 'Changes saved succesfully'));
    end;
  end;
end;

begin
  Application := TFCGIApplication.Create('ExtPascal Samples 0.7.1', TSamples);
  Application.Run;
end.
