{
Basic classes for JavaScript and Ext JS translating from Object Pascal.
Associates semantic concepts of JavaScript and Ext JS for Object Pascal, such as: Function, Object, List of Objects and Ajax Method.
It's the heart of the automatic translation method from Object Pascal to JavaScript, that I call "Self-translating".
It takes advantage of the fact that JavaScript and Object Pascal are structurally similar languages,
where there is almost one to one parity between its syntax and semantic structures.
-ExtPascal is composed of four main components:-
1. The <color red>Parser</color> (<link ExtToPascal.dpr>) able to scan Ext JS documentation in HTML format and to create the <color red>Wrapper</color>.
2. The <color red>Wrapper</color> programmatically created by Parser. It's in fact a set of units (twelve in Ext JS 2.1), which has the definition of all Ext JS classes, properties, methods and events.
3. The <color red>Self-translating</color> engine, ExtPascal unit. It's triggered when using the <color red>Wrapper</color>, ie by creating objects, assigning properties and events, and invoking methods.
4. The <color red><link FCGIApp.pas FastCGI></color>) multithread environment. It implements FastCGI protocol using TCP/IP Sockets and statefull, keep-alive, multithread web application behaviour.
<image extpascal>
1. The <color red>Parser</color> read the HTML documentation of Ext JS,
2. Read ExtFixes.txt file to fix documentation faults and omissions, and
3. Generate the <color red>Wrapper</color>.
4. With the application running, a browser session do a HTTP request to the Web Server.
5. The Web Server do a FastCGI request to the application that create a <color red>thread</color> to handle the request.
6. The <color red>thread</color> create ExtObjects, set properties and call methods from <color red>Wrapper</color> Units.
7. For each these tasks the <color red>Self-translating</color> is invoked
8. Generating JavaScript code that uses Ext JS classes.
9. At end of request processing, the <color red>thread</color> read and format all JS generated
10. And send the response to browser session. New requests can be done begining from step 4.

So the translating is not focused on JavaScript language, but to access widget frameworks made in JavaScript.
In this way the use of (X)HTML, JavaScript and CSS is minimum.
Indeed the <color red>Parser</color> can be adapted to read the documentation of another JavaScript framework, Dojo for example.

ExtPascal has one optional fifth component the <link CGIGateway.dpr>.

Author: Wanderlan Santos dos Anjos, wanderlan.anjos@gmail.com
Date: jun-2008
License: <extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
unit ExtPascal;

interface

uses
  FCGIApp;

const
  ExtPath = '/ext'; // Instalation path of Ext JS framework, below the your Web server document root

type
  TArrayOfString  = array of string;
  TArrayOfInteger = array of Integer;
  TExtObjectList  = class;
  TExtFunction    = class;
  TExtProcedure   = procedure of object; // Defines a procedure than can be called by a <link TExtObject.Ajax, AJAX> request

  {
  Defines an user session opened in a browser. Each session is a FastCGI thread that owns additional JavaScript and Ext JS resources
  as: theme, language, Ajax, error messages using Ext look, JS libraries and CSS.
  The <color red>"Self-translating"</color> is implemented in this class in <link JSCode> method.
  }
  TExtThread = class(TFCGIThread)
  private
    Style, Libraries, FLanguage : string;
    Sequence : cardinal;
    FIsAjax  : boolean;
    procedure RelocateVar(JS, JSName : string; I : integer);
    procedure RemoveJS(JS : string);
    function GetStyle: string;
  protected
    function BeforeHandleRequest : boolean; override;
    procedure AfterHandleRequest; override;
    procedure OnError(Msg, Method, Params : string); override;
    function GetSequence : string;
    function JSConcat(PrevCommand, NextCommand : string) : string;
  public
    HTMLQuirksMode : boolean; // Defines the (X)HTML DocType. True to Transitional (Quirks mode) or false to Strict. Default is false.
    Theme : string; // Sets or gets Ext JS installed theme, default '' that is Ext Blue theme
    property Language : string read FLanguage; // Actual language for this session, reads HTTP_ACCEPT_LANGUAGE header
    property IsAjax : boolean read FIsAjax; // Tests if execution is occuring in an AJAX request
    procedure JSCode(JS : string; JSName : string = ''; Owner : string = ''); // Self-translating main procedure
    procedure SetStyle(pStyle : string = ''); // Sets or resets stylesheet rules for this session
    procedure SetLibrary(pLibrary : string = ''); // Sets or resets additional JavaScript libraries, beyond of Ext JS standard libraries
    procedure ErrorMessage(Msg : string; Action : string = ''); overload;
    procedure ErrorMessage(Msg : string; Action : TExtFunction); overload;
  end;

  {
  Ancestor of all classes and components of Ext JS framework.
  Each TExtObject has the capability to self-translate to JavaScript during the program execution.
  When a property is assigned or a method is called the <link TExtThread.JSCode, Self-translating> enter in action
  translating these Object Pascal commands to JavaScript.
  }
  TExtObject = class
  private
    FJSName : string;
    function WriteFunction(Command : string): string;
  protected
    JSCommand : string; // Last command written in Response
    function VarToJSON(A : array of const)     : string; overload;
    function VarToJSON(Exts : TExtObjectList)  : string; overload;
    function VarToJSON(Strs : TArrayOfString)  : string; overload;
    function VarToJSON(Ints : TArrayOfInteger) : string; overload;
    function IsParent(CName : string): boolean;
    function ExtractJSCommand(Command : string) : string;
    procedure CreateVar(JS : string);
    procedure CreateVarAlt(JS : string);
    procedure CreateJSName;
    procedure InitDefaults; virtual;
  public
    constructor CreateInternal(Owner : TExtObject; Attribute : string);
    constructor Create(Owner : TExtObject = nil);
    constructor CreateSingleton(Attribute : string = '');
    constructor AddTo(List : TExtObjectList);
    destructor Destroy; override;
    procedure DeleteFromGarbage;
    function JSClassName : string; virtual;
    function JSArray(JSON : string) : TExtObjectList;
    function JSObject(JSON : string; ObjectConstructor : string = '') : TExtObject;
    function JSFunction(Params, Body : string) : TExtFunction; overload;
    procedure JSFunction(Name, Params, Body : string); overload;
    function JSFunction(Body : string) : TExtFunction; overload;
    function JSFunction(Method: TExtProcedure) : TExtFunction; overload;
    procedure JSCode(JS : string; pJSName : string = ''; pOwner : string = '');
    function Ajax(Method : TExtProcedure) : TExtFunction; overload;
    function Ajax(Method : TExtProcedure; Params : array of const) : TExtFunction; overload;
    property JSName : string read FJSName; // JS variable name to this object, it's created automatically when the object is created
  end;

  {
  Basic class descending of TExtObject that defines a JavaScript function. All functions and procedures of Ext JS framework are converted to Pascal functions
  where its return is this class. With this all converted functions by <link ExtPascal.pas, Wrapper> could be assigned to event handlers.
  }
  TExtFunction = class(TExtObject);

  // List of TExtObjects. The <link ExtPascal.pas, Wrapper> convey the JavaScript Array type to this class
  TExtObjectList = class(TExtFunction)
  private
    FObjects : array of TExtObject;
    Attribute, JSName : string;
    Owner : TExtObject;
    function GetFObjects(I : integer) : TExtObject;
  public
    property Objects[I : integer] : TExtObject read GetFObjects; default; // Returns the Ith object in the list, start with 0. @see GetFObjects
    constructor CreateSingleton(pAttribute : string);
    constructor Create(pOwner : TExtObject = nil; pAttribute : string = '');
    destructor Destroy; override;
    procedure Add(Obj : TExtObject);
    function Count : integer;
  end;

{$IFNDEF DOC}
  {
  Classes that can not be documented.
  They are usually JS basic classes without reference in Ext JS documentation by omission or fault.
  }
  THTMLElement = class(TExtObject);
  TStyleSheet = class(TExtObject);
  TRegExp = type string;
  TCSSRule = class(TExtObject);
  TXMLDocument = class(TExtObject);
  TNodeList = class(TExtObject);
  TExtDataNode = class(TExtObject);
  TRegion = type string;
  TNativeMenu = TExtObject;
  Tel = type string; // doc fault
  TEvent = class(TExtObject);
  TEventObject = TEvent;
  THTMLNode = TExtObject;
  TConstructor = class(TExtObject);
  TExtLibRegion = class(TExtObject); //doc fault
  TvisMode = Integer; // doc fault
  TThe = TExtObject; // doc fault
  TThis = TExtObject; // doc fault
  TairNativeMenu = TExtObject;
  TX = TExtObject; // doc fault
  TN1 = TExtObject; // doc fault
  TN2 = TExtObject; // doc fault
  TLayout = TExtObject; // Poor hierarchy definition
  TId = TExtObject; // doc fault
  TiPageX = TExtObject; // doc fault
  TiPageY = TExtObject; // doc fault
  TExtGridGrid = TExtObject; // doc fault
  TTreeSelectionModel = TExtObject; // doc fault
  TSelectionModel = TExtObject; // doc fault
  TDataSource = TExtObject; // doc fault
{$ENDIF}

implementation

uses
  SysUtils, StrUtils, Math, ExtPascalUtils;

{ TExtThread }

{
Removes identificated JS commands from response.
Used internally by Self-Translating mechanism to repositioning JS commands.
@param JS JS command with sequence identifier.
@see ExtractJSCommand
}
procedure TExtThread.RemoveJS(JS : string);
var
  I : integer;
begin
  I := pos(JS, Response);
  if I <> 0 then delete(Response, I, length(JS))
end;

{
Adds/Removes an user JS library to be used in current response.
Common requests does reset for user libraries and user style.
In opposite the AJAX requests does not reset and becomes part or rest of the same request.
@param pLibrary JS library with Path based on Web server document root.
If pLibrary is '' then all user JS libraries to this session will be removed from response.
@example <code>SetLibrary('');</code>
@example <code>SetLibrary(<link ExtPath> + '/examples/tabs/TabCloseMenu.js');</code>
}
procedure TExtThread.SetLibrary(pLibrary : string); begin
  if pLibrary = '' then
    Libraries := ''
  else
    Libraries := Libraries + '<script src=' + pLibrary + '></script>';
end;

(*
Adds/Removes an user stylesheet to be used in current response.
Common requests does reset for user libraries and user style.
In opposite the AJAX requests does not reset and becomes part or rest of the same request.
@param pStyle Styles to apply upon HTML or Ext elements in this response using CSS notation.
If pStyle is '' then all user styles to this session will be removed from response.
@example <code>SetStyle('');</code>
@example <code>SetStyle('img:hover{border:1px solid blue}');</code>
*)
procedure TExtThread.SetStyle(pStyle : string); begin
  if pStyle = '' then
    Style := ''
  else
    Style := Style + pStyle
end;

// Returns all styles in use in current response
function TExtThread.GetStyle : string; begin
  if Style = '' then
    Result := ''
  else
    Result := '<style>' + Style + '</style>';
end;

{
Shows an error message in browser session using Ext JS style.
@param Msg Message text, can to use HTML to formating text.
@param Action Optional action that will be executed after user to click Ok button. Could be JavaScript or ExtPascal commands
@example <code>ErrorMessage('User not found.');</code>
@example <code>ErrorMessage('Context not found.<br/>This Window will be reloaded to fix this issue.', 'window.location.reload()');</code>
}
procedure TExtThread.ErrorMessage(Msg : string; Action : string = ''); begin
  JSCode('Ext.Msg.show({title:"Error",msg:"' + Msg + '",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK' +
    IfThen(Action = '', '', ',fn:function(){' + Action + '}') + '});');
end;

{
Shows an error message in browser session using Ext JS style.
@param Msg Message text, can to use HTML to formating text.
@param Action Optional action that will be executed after user to click Ok button. Could be JavaScript or ExtPascal commands
@example <code>ErrorMessage('Illegal operation.<br/>Click OK to Shutdown.', Ajax(Shutdown));</code>
}
procedure TExtThread.ErrorMessage(Msg : string; Action : TExtFunction); begin
  JSCode('Ext.Msg.show({title:"Error",msg:"' + Msg + '",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK' +
    ',fn:function(){' + Action.ExtractJSCommand(Action.JSCommand) + '}});');
end;

{
Occurs when an exception is raised during the execution of method that handle the request (PATH_INFO).
Display error message with exception message, method name and method params.
@param Msg Exception message
@param Method Method name invoked by Browser (PATH_INFO) or thru AJAX request
@param Params Method params list
@exception TAccessViolation If current request is AJAX the session can be fixed reloading the page.
}
procedure TExtThread.OnError(Msg, Method, Params : string); begin
  if IsAjax and (pos('Access violation', Msg) <> 0) then
    Msg := Msg + '<br/><b>Reloading this page (F5) perhaps fix this error.</b>';
  ErrorMessage(Msg + '<br/>Method: ' + Method + IfThen(Params = '', '', '<br/>Params: ' + Params));
end;

{
Self-translating main procedure. Translates Object Pascal code to JavaScript code.
Self-translating (ST) is not a compiler. It's a minimalist (very small, ultra quick an dirty) approach that imitates an online interpreter.
You code in Object Pascal and when the program runs it automatically generates the corresponding JavaScript code.

But there is an essential limitation, it does not create business rules or sophisticated behaviour in JavaScript.
So it does not interpret "IF", "WHILE", "CASE", "TRY", etc commands, but "IF", "WHILE", etc but realizes a conditional code generation
on Server side as ASP and JSP does it. ST is used to create objects and widgets, to set properties and events, to call methods.
It's analogous to Delphi .dfm file role: to describe a GUI.
There are additional facilities to invoke Server side logic using <link TExtObject.Ajax, AJAX>, to define small <link TExtObject.JSFunction, functions> in JavaScript and
to use <link TExtThread.SetLibrary, large JS libraries>. It's enough to create powerful GUIs.
The rest (business rules, database access, etc) should be done in Object Pascal on Server side.

Basic work:
JS commands are appended to Response.
JS attributes are found in Response using yours JSName attribute and setted in place.
If not found, JS attributes are appended to Response.

@param JS JS commands or assigning of attributes or events
@param JSName Optional current JS Object name
}
procedure TExtThread.JSCode(JS : string; JSName : string = ''; Owner : string = '');
var
  I : integer;
begin
  if JS[length(JS)] = ';' then // Command
    I := length(Response) + 1
  else begin // set attribute
    I := pos('/*' + JSName + '*/', Response);
    if I = 0 then begin
      ErrorMessage('Config Option: ' + JS + '<br/>is refering a previous request,' +
        '<br/>it''s not allowed in AJAX request or JS handler.<br/>Use equivalent Public Property or Method instead.');
      exit;
    end
    else
      if not(Response[I-1] in ['{', '[', '(', ';']) then JS := ',' + JS;
  end;
  insert(JS, Response, I);
  if (pos('O_', JS) <> 0) and (pos('O_', JSName) <> 0) then begin
    if Owner <> '' then JSName := Owner;
    RelocateVar(JS, JSName, I+length(JS));
  end;
end;

{
Forces that the JS Object declaration (var) occurs before of its use: method call, get/set attributes, etc,
relocating the declaration to a previous position in Response.
@param JS Command or attribute that uses the JS Object.
@param JSName Current JS Object.
@param I Position in Response string where the JS command ends.
}
procedure TExtThread.RelocateVar(JS, JSName : string; I : integer);
var
  VarName, VarBody : string;
  J, K : integer;
begin
  J := LastDelimiter(':,', JS);
  if J <> 0 then
    VarName := copy(JS, J+1, posex('_', JS, J+3)-J)
  else
    VarName := JS;
  J := posex('/*' + VarName + '*/', Response, I);
  if J > I then begin
    K := pos('/*' + JSName + '*/', Response);
    K := posex(';', Response, K)+1;
    J := posex(';', Response, J);
    VarBody := copy(Response, K, J-K+1);
    J := LastDelimiter('|', VarBody)+1;
    VarBody := copy(VarBody, J, length(VarBody));
    delete(Response, K+J-1, length(VarBody)+1);
    insert(VarBody, Response, pos('var ' + JSName + '=new', Response));
  end;
end;

{
Concats two JS commands only to translate nested Object Pascal typecasts as:
@param PrevCommand Command already present in Response that will be concatenated with NextCommand
@param NextCommand Command that will be concatenated with PrevCommand.
@example <code>
TExtGridRowSelectionModel(GetSelectionModel).SelectFirstRow;
// It's usually could be translated to:
O1.getSelectionModel;
O1.selectFirstRow;
// instead of:
O1.getSelectionModel.selectFirstRow;</code>
}
function TExtThread.JSConcat(PrevCommand, NextCommand : string) : string;
var
  I , J : integer;
begin
  J := pos(PrevCommand, Response);
  I := pos('.', NextCommand);
  if (I <> 0) and (J <> 0) then begin
    NextCommand := copy(NextCommand, I, length(NextCommand));
    Result := copy(PrevCommand, 1, length(PrevCommand)-1) + NextCommand;
    delete(Response, J + length(PrevCommand)-1, 1);
    insert(NextCommand, Response, J + length(PrevCommand))
  end
  else
    Result := PrevCommand;
end;

{
Does tasks related to the Request que ocorrem antes da chamada do m�todo invocada pelo Browser (PATH-INFO)
1. Detects the browser language.
2. If that language has corresponding JS resource file in framework uses it, for example: '/ext/source/locale/ext-lang-?????.js',
3. Else uses the default language (english).
4. Tests if is an AJAX request.
5. If not is AJAX request resets Sequence, Style and Libraries.
6. Tests if cookies are enabled.

@return false if Cookies are disable or if is Ajax executing the first thread request else returns true.
}
function TExtThread.BeforeHandleRequest : boolean;
var
  I : integer;
begin
  Result := true;
  if FLanguage = '' then begin // Set language
    FLanguage := RequestHeader['HTTP_ACCEPT_LANGUAGE'];
    I := LastDelimiter('-', FLanguage);
    if I <> 0 then begin
      FLanguage := copy(FLanguage, I-2, 2) + '_' + Uppercase(copy(FLanguage, I+1, 2));
      if not FileExists(RequestHeader['DOCUMENT_ROOT'] + ExtPath + '/source/locale/ext-lang-' + FLanguage + '.js') then
        FLanguage := copy(FLanguage, 1, 2)
    end;
  end;
  Response := '';
  FIsAjax  := Query['Ajax'] = '1';
  if not IsAjax then begin
    Sequence  := 0;
    Style     := '';
    Libraries := '';
  end
  else
    if Cookie['FCGIThread'] = '' then begin
      ErrorMessage('This web application requires Cookies enabled to AJAX works.');
      Result := false;
    end
    else
      if NewThread then begin
        ErrorMessage('Session expired or lost.<br/>A new session will be created now.', 'window.location.reload()');
        Result := false;
      end
end;

{
Does tasks after Request processing.
1. Extracts Comments.
2. Sets:
  2.1. HTML body,
  2.2. Title,
  2.3. Charset,
  2.4. ExtJS CSS,
  2.5. ExtJS libraries,
  2.6. ExtJS theme,
  2.7. ExtJS language,
  2.8. Additional user styles,
  2.9. Additional user libraries,
  2.9. ExtJS invoke and
  2.10. Handlers for AJAX response
}
procedure TExtThread.AfterHandleRequest;
var
  I, J : integer;
begin
  I := pos('/*', Response);
  while I <> 0 do begin // Extracts comments
    J := PosEx('*/', Response, I);
    delete(Response, I, J - I + 2);
    I := PosEx('/*', Response, I);
  end;
  Response := AnsiReplaceStr(AnsiReplaceStr(Response, '|', ''), '_', ''); // Extracts aux chars
  if not IsAjax then
    Response := IfThen(HTMLQuirksMode, '<!docttype html public><html>',
      '<?xml version=1.0?><!doctype html public "-//W3C//DTD XHTML 1.0 Strict//EN"><html xmlns=http://www.w3org/1999/xthml>') +
      '<title>' + Application.Title + '</title>' +
      '<meta http-equiv="content-type" content="charset=utf-8">' +
      '<link rel=stylesheet href=' + ExtPath + '/resources/css/ext-all.css />' +
      '<script src=' + ExtPath + '/adapter/ext/ext-base.js></script>' +
      '<script src=' + ExtPath + '/ext-all.js></script>' +
      IfThen(Theme = '', '', '<link rel=stylesheet href=' + ExtPath + '/resources/css/xtheme-' + Theme + '.css />') +
      IfThen(FLanguage = 'en', '', '<script src=' + ExtPath + '/source/locale/ext-lang-' + FLanguage + '.js></script>') +
      GetStyle + Libraries +
      '<script>Ext.onReady(function(){' +
      'Ext.BLANK_IMAGE_URL="' + ExtPath + '/resources/images/default/s.gif";'+
      'function AjaxSuccess(response){eval(response.responseText);};' +
      'function AjaxFailure(){Ext.Msg.show({title:"Error",msg:"Server unavailable, try later.",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});};' +
      Response + '});</script><body><div id=body></div><noscript>This web application requires JavaScript enabled</noscript></body></html>';
end;

{
Returns a unique numeric sequence to identify a JS object, list or attribute in this session.
This sequence will be used by Self-translating process imitating a Symbol table entrance.
}
function TExtThread.GetSequence : string; begin
  Result := IntToStr(Sequence);
  inc(Sequence);
end;

{ ExtObjectList }

{
Creates a TExtObjectList instance.
@param pOwner TExtObject that owns this list
@param pAttribute JS attribute name in TExtObject to this list
}
constructor TExtObjectList.Create(pOwner : TExtObject = nil; pAttribute : string = ''); begin
  Attribute := pAttribute;
  Owner     := pOwner;
  JSName    := 'O_' + TExtThread(CurrentFCGIThread).GetSequence + '_';
end;

{
Creates a singleton TExtObjectList instance, used usually by Parser only.
@param pAttribute JS attribute name in TExtObject to this list in the form 'Owner.attribute'
}
constructor TExtObjectList.CreateSingleton(pAttribute : string); begin
  Attribute := pAttribute;
end;

// Frees this list and all objects linked in it
destructor TExtObjectList.Destroy;
var
  I : integer;
begin
  for I := 0 to length(FObjects)-1 do try FObjects[I].Free except end;
  inherited;
end;

{
Adds a <link TExtObject> in this list and generates the corresponding JS code in the Response.
@param Obj <link TExtObject> to add in the list
}
procedure TExtObjectList.Add(Obj : TExtObject);
var
  ListAdd, Response, OwnerName : string;
begin
  Obj.DeleteFromGarbage;
  if length(FObjects) = 0 then
    if Owner <> nil then
      Owner.JSCode(Attribute + ':[/*' + JSName + '*/]', Owner.JSName)
    else
      TExtThread(CurrentFCGIThread).JSCode('var ' + JSName + '=[/*' + JSName + '*/];');
  SetLength(FObjects, length(FObjects) + 1);
  FObjects[high(FObjects)] := Obj;
  Response := CurrentFCGIThread.Response;
  if Owner <> nil then
    OwnerName := Owner.JSName
  else
    OwnerName := '';
  if pos(Obj.JSName, Response) = 0 then begin
    if (pos(JSName, Response) = 0) and TExtThread(CurrentFCGIThread).IsAjax and (OwnerName <> '') then
      ListAdd := 'var ' + Obj.JSName + '=' + OwnerName + '.add(%s);'
    else
      ListAdd := '%s';
    if Attribute = 'items' then // Generalize it more if necessary
      ListAdd := Format(ListAdd, ['new ' + Obj.JSClassName + '({/*' + Obj.JSName + '*/})'])
    else
      ListAdd := Format(ListAdd, ['{/*' + Obj.JSName + '*/}']);
    Obj.JSCode(ListAdd, JSName, OwnerName);
  end
  else
    Obj.JSCode(Obj.JSName, JSName, OwnerName);
end;

{
Returns the Ith object in the list, start with 0.
@param I Position in list
@return <link TExtObject>
}
function TExtObjectList.GetFObjects(I : integer): TExtObject; begin
  Result := FObjects[I]
end;

// Returns the number of Objects in the list
function TExtObjectList.Count: integer; begin
  Result := length(FObjects)
end;

{ ExtObject }

// Set an unique <link JSName> using <link TExtThread.GetSequence>
procedure TExtObject.CreateJSName; begin
  FJSName := 'O_' + TExtThread(CurrentFCGIThread).GetSequence + '_';
end;

{
Creates a singleton TExtObject instance, used usually by Parser only.
@param Attribute JS attribute name in TExtObject to this list in the form 'Owner.attribute'
}
constructor TExtObject.CreateSingleton(Attribute : string = ''); begin
  if Attribute = '' then
    FJSName := JSClassName
  else
    FJSName := Attribute;
  InitDefaults;
end;

{
Used by <link Create> to initialize the JSName, to <link TFCGIThread.AddToGarbage, add to Garbage Collector>
and to generate <link JSCode, JS code>
@param JS JS constructor for the JS <color red>new</color> command
@see CreateVarAlt
}
procedure TExtObject.CreateVar(JS : string); begin
  CurrentFCGIThread.AddToGarbage(Self);
  CreateJSName;
  insert('/*' + JSName + '*/', JS, length(JS)-IfThen(pos('});', JS) <> 0, 2, 1));
  JSCode('|var ' + JSName + '=new ' + JS)
end;

{
Alternate create constructor, it is an ExtJS fault
@see CreateVar
}
procedure TExtObject.CreateVarAlt(JS : string); begin
  CurrentFCGIThread.AddToGarbage(Self);
  CreateJSName;
  insert('/*' + JSName + '*/', JS, length(JS)-IfThen(pos('});', JS) <> 0, 2, 1));
  JSCode('|var ' + JSName + '= ' + JS)
end;


// <link TFCGIThread.DeleteFromGarbage, Removes object from Garbage Collector> if is not in a Garbage Collector call
procedure TExtObject.DeleteFromGarbage; begin
  if CurrentFCGIThread <> nil then CurrentFCGIThread.DeleteFromGarbage(Self);
end;

// <link DeleteFromGarbage, Removes object from Garbage Collector> and frees it
destructor TExtObject.Destroy; begin
  try
    DeleteFromGarbage;
    inherited;
  except end;
end;

{
Creates a TExtObject and generate corresponding JS code using <link JSCode, Self-translating>
@param Owner Optional parameter used internally by <link JSObject> and <link JSArray> only
}
constructor TExtObject.Create(Owner : TExtObject = nil); begin
  if Owner = nil then CreateVar(JSClassName + '({});');
end;

{
Used by Parser to build <link InitDefaults> methods used to initialize public JS properties in a TExtObject
@param Owner TExtObject where this property is declared
@param Attribute Public JS property name
}
constructor TExtObject.CreateInternal(Owner : TExtObject; Attribute : string); begin
  FJSName := Owner.JSName + '.' + Attribute;
end;

// Returns 'Object' that is the default class name for Ext JS objects
function TExtObject.JSClassName: string; begin
  Result := 'Object'
end;

{
Tests if a class name is parent of this object
@param CName Class name with "T" prefix
@return true if CName is parent of this object and false if not
}
function TExtObject.IsParent(CName : string) : boolean;
var
  Cls : TClass;
begin
  if (CName <> '') and (CName[1] = 'T') then begin
    Result := true;
    Cls    := ClassType;
    while Cls.ClassName <> 'TExtFunction' do begin
      if Cls.ClassName = CName then exit;
      Cls := Cls.ClassParent
    end;
  end;
  Result := false;
end;

{
Starts Self-translating mechanism invoking <link TExtThread.JSCode>.
Invokes <link TExtThread.JSConcat> if identify a nested typecast
@param JS JS commands or declarations
@param pJSName Optional, by default is the <link TExtObject.JSName>, when used by <link TExtObjectList.Add> is the <link TExtObjectList.JSName>
@param pOwner Optional, used by <link TExtObjectList.Add> only to pass the TExtObject owner list
}
procedure TExtObject.JSCode(JS : string; pJSName : string = ''; pOwner : string = ''); begin
  if JS <> '' then begin
    if (JS[length(JS)] = ';') and (pos('var ', JS) <> 1) then begin
      if (JSCommand <> '') and (pJSName <> '') and not IsParent(pJSName) then begin
        JSCommand := TExtThread(CurrentFCGIThread).JSConcat(JSCommand, JS);
        exit;
      end;
      JSCommand := JS;
    end
    else
      JSCommand := '';
    if pJSName = '' then pJSName := JSName;
    TExtThread(CurrentFCGIThread).JSCode(JS, pJSName, pOwner);
  end;
end;

{
Adds this object in a list.
If called as constructor creates the object before adds it to the list.
@param List An instanciated <link TExtObjectList>
}
constructor TExtObject.AddTo(List : TExtObjectList); begin
  if JSName = '' then begin
    CreateJSName;
    InitDefaults;
  end;
  List.Add(Self);
end;

{
Virtual method overrided by Parser generated code to initialize public JS properties in a <link TExtObject>
@see CreateInternal
}
procedure TExtObject.InitDefaults; begin end;

{
Generates JS code to declare a inline JS Array.
@param JSON JavaScript Object Notation, the body of Array declaration
@return <link TExtObjectList> to be used in assigns
}
function TExtObject.JSArray(JSON : string) : TExtObjectList; begin
  Result := TExtObjectList(TExtObject.Create(Self));
  TExtObject(Result).FJSName := '[' + JSON + ']';
end;

{
Generates JS code to declare a inline generic JS object.
It is necessary in 3 cases:

1. When the Ext JS documentation informs that the attribute is an object without any particular type (Object),
as JavaScript language is almost typeless it happens eventually. That would be equivalent to the type Variant of VB or Delphi.
Examples includes data records.

2. There are omissions in the documentation and attribute actually belongs to a specific class, in this case use the JSObject method,
do a typecast or declare in <link ExtFixes.txt, ExtFixes.txt> file, this allows to register in the Wrapper the omissions of the
documentation or the framework.

3. There are omissions in the framework, ie should be a specific class. Read its attributes in description contained
in the documentation and declare them in <link ExtFixes.txt, ExtFixes.txt> for the Wrapper to recognize them or use JSObject method.

@param JSON JavaScript Object Notation, the body of JS object declaration
@return <link TExtObject> to be used in assigns
}
function TExtObject.JSObject(JSON : string; ObjectConstructor : string = '') : TExtObject; begin
  Result := TExtObject.Create(Self);
  if ObjectConstructor = '' then
    Result.FJSName := '{' + JSON + '}'
  else
    Result.FJSName := 'new ' + ObjectConstructor + '({' + JSON + '})'
end;

{
Generates JS code to declare an anonymous JS function with parameters
@param Params JS Parameters separated by commas
@param Body JS commands for JS function
@return <link TExtFunction> to use in event handlers
}
function TExtObject.JSFunction(Params, Body : string) : TExtFunction; begin
  Result := TExtFunction.Create(Self);
  Result.FJSName := 'function(' + Params + '){' + Body + '}';
end;

{
Generates JS code to declare an anonymous JS function without parameters
@param Body JS commands for JS function
@return <link TExtFunction> to use in event handlers
}
function TExtObject.JSFunction(Body : string) : TExtFunction; begin
  Result := JSFunction('', Body);
end;

{
Declares a named JS function with parameters.
@param Name JS function name
@param Params JS Parameters separated by commas
@param Body JS commands for JS function
}
procedure TExtObject.JSFunction(Name, Params, Body : string); begin
  JSCode('function ' + Name + '(' + Params + '){' + Body + '};');
end;

{
Generates an anonymous JS function from an Object Pascal procedure to use in event handlers.
To get event parameters use %0, %1 until %9 place holders.
@param Method Object Pascal procedure declared on the <link TExtThread> or in a <link TExtObject>
@return <link TExtFunction> to use in event handlers
@example <code>
procedure TSamples.ReadButtonJS; begin
  ExtMessageBox.Alert('Browser Side: Button clicked', 'You clicked the "%0" button')
end;

procedure TSamples.MessageBoxes; begin
  with TExtPanel.Create do begin
    Title    := 'Message Boxes';
    Width    := 700;
    RenderTo := 'body';
    Frame    := true;
    with TExtButton.AddTo(Buttons) do begin
      Text    := 'Confirm Message';
      Handler := ExtMessageBox.Confirm('Confirm', 'Are you sure?', JSFunction(ReadButtonJS));
    end;
  end;
</code>
@example
<code>
procedure TSamples.SelectNodeEventBrowserSide; begin
  ExtMessageBox.Alert('Browser Side', '%0.text')
end;

procedure TSamples.BorderLayout;
var
 Tree : TExtTreeTreePanel;
 Root, Node : TExtTreeTreeNode;
begin
  Tree := TExtTreeTreePanel.Create;
  Tree.Border := false;
  //set root node
  Root := TExtTreeTreeNode.Create;
  with Root do begin
    Text := 'Root';
    AllowChildren := True;
    Expandable := True;
    Expanded := True;
    Leaf := False;
    on('click', JSFunction(SelectNodeEventBrowserSide));
  end;
: : : : : : :</code>
}
function TExtObject.JSFunction(Method : TExtProcedure) : TExtFunction;
var
  CurrentResponse : string;
begin
  Result := TExtFunction(Self);
  with TExtThread(CurrentFCGIThread) do begin
    CurrentResponse := Response;
    Response := '';
    Method;
    JSCommand := Response;
    Response  := CurrentResponse;
    JSCode(JSCommand);
  end;
end;

{
Invokes an Object Pascal published procedure in AJAX mode.
To get event parameters use %0, %1 until %9 place holders.
@param Method Published procedure to invoke
@return <link TExtFunction> to use in event handlers
@example <code>
procedure TSamples.AddTab; begin // published method
  inc(TabIndex);
  with TExtPanel.AddTo(Tabs.Items) do begin
    Title    := 'New Tab ' + IntToStr(TabIndex);
    IconCls  := 'tabs';
    Html     := 'Tab Body ' + IntToStr(TabIndex) + '<br/><br/>blahblah';
    Closable := true;
    if IsAjax then Show;
    Free;
  end;
end;

procedure TSamples.AdvancedTabs;
var
  I : integer;
begin
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
end;</code>
}
function TExtObject.Ajax(Method : TExtProcedure) : TExtFunction; begin
  Result := Ajax(Method, []);
end;

{
Invokes an Object Pascal published procedure with parameters in AJAX mode.
To get event parameters use %0, %1 until %9 place holders.
@param Method Published procedure to invoke
@param Params Array of Parameters, each parameter is a pair: Name, Value.
To get them on server side use <link TFCGIThread.Query> array property in AJAX method.
@return <link TExtFunction> to use in event handlers
@example <code>
procedure TSamples.CheckLogin; begin
  if true (*user account verification should be done here*) then
    with TExtWindow.Create do begin
      Title    := 'Login';
      Width    := 380;
      Height   := 140;
      Plain    := true;
      Layout   := 'fit';
      Closable := false;
      with TExtPanel.AddTo(Items) do begin
        Border    := false;
        BodyStyle := 'padding: 5px 8px';
        HTML      := 'Welcome, ' + Query['UserName'] + '.<br/>Password: ' + Query['Password'];
      end;
      Show;
    end
  else
    ExtMessageBox.Alert('Unknown', 'User is not known.');
end;

procedure TSamples.Login;
var
  UserName, Password : TExtFormTextField;
begin
  FormLogin := TExtWindow.Create;
  with FormLogin do begin
    Title    := 'Login';
    Width    := 380;
    Height   := 140;
    Plain    := true;
    Layout   := 'fit';
    Closable := false;
    with TExtFormFormPanel.AddTo(Items) do begin
      LabelWidth  := 70;
      Border      := false;
      XType       := 'form';
      ButtonAlign := 'right';
      BodyStyle   := 'padding: 10px 15px';
      DefaultType := 'textfield';
      Defaults    := JSObject('width: 250');
      UserName    := TExtFormTextField.Create;
      with UserName.AddTo(Items) do begin
        Name       := 'user';
        FieldLabel := 'Username';
        InputType  := 'textfield';
      end;
      Password := TExtFormTextField.Create;
      with Password.AddTo(Items) do begin
        Name       := 'pass';
        FieldLabel := 'Password';
        InputType  := 'password';
      end;
      with TExtButton.AddTo(Buttons) do begin
        Text    := 'LOGIN';
        Handler := Ajax(CheckLogin, ['UserName', UserName.GetValue, 'Password', Password.GetValue]);
      end;
    end;
    Show;
  end;
end;
</code>
}
function TExtObject.Ajax(Method : TExtProcedure; Params : array of const) : TExtFunction;
var
  MetName, lParams : string;
  I : integer;
begin
  Result  := TExtFunction(Self);
  MetName := CurrentFCGIThread.MethodName(@Method);
  if MetName <> '' then begin
    lParams := 'Ajax=1';
    for I := 0 to high(Params) do
      with Params[I] do
        if Odd(I) then
          case VType of
            vtAnsiString : lParams := lParams + string(VAnsiString);
            vtString     : lParams := lParams + VString^;
            vtObject     : lParams := lParams + '"+' + ExtractJSCommand(TExtObject(VObject).JSCommand) + '+"';
            vtInteger    : lParams := lParams + IntToStr(VInteger);
            vtBoolean    : lParams := lParams + IfThen(VBoolean, 'true', 'false');
            vtExtended   : lParams := lParams + FloatToStr(VExtended^);
            vtVariant    : lParams := lParams + string(VVariant^)
          end
        else
          case VType of
            vtAnsiString : lParams := lParams + '&' + string(VAnsiString) + '=';
            vtString     : lParams := lParams + '&' + VString^ + '=';
          else
            JSCode('Ext.Msg.show({title:"Error",msg:"Ajax method: ' + MetName +
              ' has an invalid parameter name in place #' + IntToStr(I+1) + '",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});');
            exit;
          end;
    JSCode('Ext.Ajax.request({url:"' + CurrentFCGIThread.RequestHeader['SCRIPT_NAME'] + '/' + MetName +
      '",params:"' + lParams + '",success:AjaxSuccess,failure:AjaxFailure});');
  end
  else
    JSCode('Ext.Msg.show({title:"Error",msg:"Ajax method not published.",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});');
end;

{
Encapsulates JS commands in an anonymous JS function, find %0..%9 place holders and declares respective event parameters
@param Command JS command to convert to JS function
@return An anonymous JS function with optional event parameters declared
}
function TExtObject.WriteFunction(Command : string) : string;
var
  I, J   : integer;
  Params : string;
begin
  Params := '';
  J := -1;
  I := pos('%', Command);
  while I <> 0 do begin
    if Command[I+1] in ['0'..'9'] then begin
      Command[I] := 'P';
      insert('+"', Command, FirstDelimiter('"'' ', Command, I));
      insert('"+', Command, I);
      inc(J);
    end;
    I := posex('%', Command, I);
  end;
  for I := 0 to J do begin
    Params := 'P' + IntToStr(I);
    if I <> J then Params := Params + ','
  end;
  I := LastDelimiter(';', copy(Command, 1, length(Command)-1));
  if (I = 0) or (I = length(Command)) and (pos('return ', Command) <> 1) then
    Command := 'return ' + Command
  else
    insert('return ', Command, I+1);
  Result := 'function(' + Params + '){' + Command + '}';
end;

{
Extracts JS command from Response
@param Command JS
@return JS command without the last char ';'
@see TExtThread.RemoveJS
}
function TExtObject.ExtractJSCommand(Command : string) : string; begin
  Result := Command;
  TExtThread(CurrentFCGIThread).RemoveJS(Result);
  SetLength(Result, length(Result)-1);
end;

{
Converts an array of const to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
@param A Array of anytype variables
@return JSON representation of array
}
function TExtObject.VarToJSON(A : array of const) : string;
var
  I : integer;
  Command : string;
begin
  Result := '';
  I := 0;
  while I <= high(A) do begin
    with A[I] do
      case VType of
        vtObject: begin
          if VObject <> nil then begin
            Command := TExtObject(VObject).JSCommand;
            if (Command <> '') and A[I+1].VBoolean then begin
              Result := Result + WriteFunction(Command);
              TExtThread(CurrentFCGIThread).RemoveJS(Command);
            end
            else
              Result := Result + TExtObject(VObject).JSName
          end
          else
            if Result = '' then
              Result := 'null'
            else begin
              inc(I, 2);
              continue;
            end;
          inc(I);
        end;
        vtAnsiString: Result := Result + '"' + AnsiReplaceStr(string(VAnsiString), '"', '''') + '"';
        vtString:     Result := Result + '"' + AnsiReplaceStr(VString^, '"', '''') + '"';
        vtInteger:    Result := Result + IntToStr(VInteger);
        vtBoolean:    Result := Result + IfThen(VBoolean, 'true', 'false');
        vtExtended:   Result := Result + FloatToStr(VExtended^);
        vtVariant:    Result := Result + string(VVariant^)
      end;
    if I < high(A) then Result := Result + ',';
    inc(I);
  end;
  if (Result <> '') and (Result[length(Result)] = ',') then delete(Result, length(Result), 1);
end;

{
Converts a <link TExtObjectList> to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
@param Exts An TExtObjectList to convert
@return JSON representation of Exts
}
function TExtObject.VarToJSON(Exts : TExtObjectList): string; begin
  if Exts.ClassName = 'TExtObjectList' then
    Result := Exts.JSName
  else
    Result := TExtObject(Exts).JSName
end;

{
Converts an <link TArrayOfString, array of strings> to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
@param Strs An <link TArrayOfString, array of strings> to convert
@return JSON representation of Strs
}
function TExtObject.VarToJSON(Strs : TArrayOfString): string;
var
  I : integer;
begin
  Result := '[';
  for I := 0 to high(Strs) do begin
    Result := Result + '"' + AnsiReplaceStr(Strs[I], '"', '''') + '"';
    if I < high(Strs) then Result := Result + ',';
  end;
  Result := Result + ']'
end;

{
Converts an <link TArrayOfInteger, array of integers> to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
@param Ints An <link TArrayOfInteger, array of integers> to convert
@return JSON representation of Ints
}
function TExtObject.VarToJSON(Ints : TArrayOfInteger): string;
var
  I : integer;
begin
  Result := '[';
  for I := 0 to high(Ints) do begin
    Result := Result + IntToStr(Ints[I]);
    if I < high(Ints) then Result := Result + ',';
  end;
  Result := Result + ']'
end;

end.
