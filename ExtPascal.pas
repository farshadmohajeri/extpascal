{
Classes to JavaScript and Ext JS translating from Object Pascal.
Associates semantic concepts of JavaScript and Ext JS for Object Pascal, such as: Function, Object, List of Objects and Ajax Method.
It's the heart of the automatic translation method from Object Pascal to JavaScript, that I call "Self-translating".
It takes advantage of the fact that JavaScript and Object Pascal are structurally similar languages,
where there is almost one to one parity between its syntax and semantic structures.
-ExtPascal is composed of four main components:-
1. The <color red>Parser</color> (<link ExtToPascal.dpr>) able to scan Ext JS documentation in HTML format and to create the <color red>Wrapper</color>.
2. The <color red>Wrapper</color> programmatically created by Parser. It's in fact a set of units (twelve in Ext JS 2.1), which has the definition of all Ext JS classes, properties, methods and events.
3. The <color red>Self-translating</color> engine, <link ExtPascal.pas, ExtPascal unit>. It's triggered when using the <color red>Wrapper</color>, ie by creating objects, assigning properties and events, and invoking methods.
4. The <link FCGIApp.pas, FastCGI> multithread environment. It implements FastCGI protocol using TCP/IP Sockets and statefull, keep-alive, multithread web application behavior.
<image extpascal>
1. The <color red>Parser</color> reads the HTML documentation of Ext JS,
2. Reads ExtFixes.txt file to fix documentation faults and omissions, and
3. Generates the <color red>Wrapper</color>.
4. With the application running, a browser session does a HTTP request to the Web Server.
5. The Web Server does a FastCGI request to the application that creates a <color red>thread</color> to handle the request.
6. The <color red>thread</color> creates ExtObjects, set properties and call methods from <color red>Wrapper</color> Units.
7. For each these tasks the <color red>Self-translating</color> is invoked
8. Generating JavaScript code that uses Ext JS classes.
9. At end of request processing, the <color red>thread</color> reads and formats all JS generated
10. And sends the response to browser session. New requests can be done begining from step 4.
So the translating is not focused on JavaScript language, but to access widget frameworks made in JavaScript.
In this way the use of (X)HTML, JavaScript and CSS is minimum.
Indeed the <color red>Parser</color> can be adapted to read the documentation of another JavaScript framework, Dojo for example.

ExtPascal has one optional fifth component the <link CGIGateway.dpr>.

Author: Wanderlan Santos dos Anjos, wanderlan.anjos@gmail.com
Date: jun-2008
License: <extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}

unit ExtPascal;

//@@Overview
//<copy ExtPascal.pas>

// Enabling SERVICE directive ExtPascal application can be used as a Windows Service,
// In command line use -INSTALL to install the service and - UNINSTALL to uninstall the service
{$IFDEF MSWINDOWS}
{.$DEFINE SERVICE}
{$ENDIF}

// Enabling WebServer directive ExtPascal can be used within an embedded Indy based WebServer
{$IF Defined(MSWINDOWS) and not Defined(SERVICE)}
{.$DEFINE WebServer}
{$IFEND}

// Uses ext-all-debug.js and break line at ";" char to facilitate JS debugging
{.$DEFINE DEBUGJS}

interface

uses
  {$IFNDEF WebServer}FCGIApp{$ELSE}IdExtHTTPServer{$ENDIF}, Classes;

const
  ExtPath = '/ext'; // Installation path of Ext JS framework, below the your Web server document root

type
  TArrayOfString  = array of string;
  TArrayOfInteger = array of Integer;
  TExtObjectList  = class;
  TExtFunction    = class;
  TExtProcedure   = procedure of object; // Defines a procedure than can be called by a <link TExtObject.Ajax, AJAX> request

  {
  Defines an user session opened in a browser. Each session is a FastCGI thread that owns additional JavaScript and Ext JS resources
  as: theme, language, Ajax, error messages using Ext look, JS libraries and CSS.
  The <color red>"Self-translating"</color> is implemented in this class in <link TExtObject.JSCode, JSCode> method.
  }
  TExtThread = class({$IFNDEF WebServer}TFCGIThread{$ELSE}TIdExtSession{$ENDIF})
  private
    Style, Libraries, FLanguage : string;
    JSReturns : TStringList;
    Sequence  : cardinal;
    FIsAjax   : boolean;
    FIsIE     : boolean;
    procedure RelocateVar(JS, JSName : string; I : integer);
    function GetStyle: string;
  protected
    procedure RemoveJS(JS : string);
    function BeforeHandleRequest : boolean; override;
    procedure AfterHandleRequest; override;
    procedure OnError(Msg, Method, Params : string); override;
    function GetSequence : string;
    function JSConcat(PrevCommand, NextCommand : string) : string;
  public
    HTMLQuirksMode : boolean; // Defines the (X)HTML DocType. True to Transitional (Quirks mode) or false to Strict. Default is false.
    Theme : string; // Sets or gets Ext JS installed theme, default '' that is Ext Blue theme
    property Language : string read FLanguage write FLanguage; // Actual language for this session, reads HTTP_ACCEPT_LANGUAGE header
    property IsAjax : boolean read FIsAjax; // Tests if execution is occurring in an AJAX request
    property IsIE : boolean read FIsIE; // Tests if session is using IE
    procedure JSCode(JS : string; JSName : string = ''; Owner : string = '');
    procedure SetStyle(pStyle : string = '');
    procedure SetLibrary(pLibrary : string = '');
    procedure ErrorMessage(Msg : string; Action : string = ''); overload;
    procedure ErrorMessage(Msg : string; Action : TExtFunction); overload;
    {$IFDEF WebServer}
    procedure InitSessionDefs; override;
    {$ENDIF}
  published
    procedure HandleEvent; virtual;
  end;

  {
  Ancestor of all classes and components of Ext JS framework.
  Each TExtObject has the capability to self-translate to JavaScript during the program execution.
  When a property is assigned or a method is called the <link TExtThread.JSCode, Self-translating> enter in action
  translating these Object Pascal commands to JavaScript.
  }
  TExtObject = class
  private
    function WriteFunction(Command : string): string;
  protected
    FJSName   : string;  // Internal JavaScript name generated automatically by <link TExtObject.CreateJSName, CreateJSName>
    JSCommand : string;  // Last command written in Response
    Created   : boolean; // Tests if object already created
    function ConfigAvailable(JSName : string) : boolean;
    function ExtractJSCommand : string;
    function IsParent(CName : string): boolean;
    function VarToJSON(A : array of const)     : string; overload;
    function VarToJSON(Exts : TExtObjectList)  : string; overload;
    function VarToJSON(Strs : TArrayOfString)  : string; overload;
    function VarToJSON(Ints : TArrayOfInteger) : string; overload;
    function ParamAsInteger(ParamName : string) : integer;
    function ParamAsDouble(ParamName : string) : double;
    function ParamAsBoolean(ParamName : string) : boolean;
    function ParamAsString(ParamName : string) : string;
    function ParamAsTDateTime(ParamName : string) : TDateTime;
    function ParamAsObject(ParamName : string) : TExtObject;
    procedure CreateVar(JS : string);
    procedure CreateVarAlt(JS : string);
    procedure CreateJSName;
    procedure InitDefaults; virtual;
    function Ajax(MethodName : string; Params : array of const; IsEvent : boolean) : TExtFunction; overload;
    procedure HandleEvent(const AEvtName: string); virtual;
  public
    constructor CreateInternal(Owner : TExtObject; Attribute : string);
    constructor Create(Owner : TExtObject = nil);
    constructor CreateSingleton(Attribute : string = '');
    constructor AddTo(List : TExtObjectList);
    constructor Init(Method : TExtFunction); overload;
    constructor Init(Command : string); overload;
    destructor Destroy; override;
    function DestroyJS : TExtFunction; virtual;
    procedure Free(CallDestroyJS : boolean = false);
    procedure Delete;
    procedure DeleteFromGarbage;
    function JSClassName : string; virtual;
    function JSArray(JSON : string) : TExtObjectList;
    function JSObject(JSON : string; ObjectConstructor : string = '') : TExtObject;
    function JSFunction(Params, Body : string) : TExtFunction; overload;
    procedure JSFunction(Name, Params, Body : string); overload;
    function JSFunction(Body : string) : TExtFunction; overload;
    function JSFunction(Method: TExtProcedure; Silent : boolean = false) : TExtFunction; overload;
    function JSExpression(Expression : string; MethodsValues : array of const) : integer; overload;
    function JSExpression(Method : TExtFunction) : integer; overload;
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
    property Objects[I : integer] : TExtObject read GetFObjects; default; // Returns the Ith object in the list, start with 0.
    constructor CreateSingleton(pAttribute : string);
    constructor Create(pOwner : TExtObject = nil; pAttribute : string = '');
    destructor Destroy; override;
    procedure Add(Obj : TExtObject);
    function Count : integer;
  end;

//(*DOM-IGNORE-BEGIN
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
  TExtEventObject = TEventObject;
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
//DOM-IGNORE-END*)

implementation

uses
  SysUtils, StrUtils, Math, ExtPascalUtils, ExtUtil;

const
  DeclareJS    = '/*var*/ '; // Declare JS objects as global
  CommandDelim = #3;         // Internal JS command delimiter
  IdentDelim   = #4;         // Internal JS identifier delimiter

{ TExtThread }

{
Removes identificated JS commands from response.
Used internally by Self-Translating mechanism to repositioning JS commands.
@param JS JS command with sequence identifier.
@see TExtObject.ExtractJSCommand
}
procedure TExtThread.RemoveJS(JS : string);
var
  I : integer;
begin
  I := RPos(JS, Response);
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
Adds/Removes a user stylesheet to be used in current response.
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
    ',fn:function(){' + Action.ExtractJSCommand + '}});');
end;

{
Occurs when an exception is raised during the execution of method that handles the request (PATH_INFO).
Display error message with exception message, method name and method params.
@param Msg Exception message
@param Method Method name invoked by Browser (PATH_INFO) or thru AJAX request
@param Params Method params list
@exception TAccessViolation If current request is AJAX the session can be fixed reloading the page.
}
procedure TExtThread.OnError(Msg, Method, Params : string); begin
  Response := '';
  if IsAjax and (pos('Access violation', Msg) <> 0) then
    Msg := Msg + '<br/><b>Reloading this page (F5) perhaps fix this error.</b>';
  ErrorMessage(Msg + '<br/>Method: ' + IfThen(Method = '', 'Home', Method) + IfThen(Params = '', '', '<br/>Params: ' + Params));
end;

{
Self-translating main procedure. Translates Object Pascal code to JavaScript code.
Self-translating (ST) is not a compiler. It's a minimalist (very small, ultra quick and dirty) approach that imitates an online interpreter.
You code in Object Pascal and when the program runs it automatically generates the corresponding JavaScript code.

But there is an essential limitation; it does not create business rules or sophisticated behavior in JavaScript.
So it does not interpret "IF", "WHILE", "CASE", "TRY", etc commands, but "IF", "WHILE", etc but realizes a conditional code generation
on Server side as ASP and JSP does it. ST is used to create objects and widgets, to set properties and events, to call methods.
It's analogous to Delphi .dfm file role: to describe a GUI.
There are additional facilities to invoke Server side logic using <link TExtObject.Ajax, AJAX>, to define small <link TExtObject.JSFunction, functions> in JavaScript and
to use <link TExtThread.SetLibrary, large JS libraries>. It's enough to create powerful GUIs.
The rest (business rules, database access, etc) should be done in Object Pascal on Server side.

Basic work:
* JS commands are appended to Response.
* JS attributes are found in Response using yours JSName attribute and setted in place.
* If not found, JS attributes are appended to Response.
@param JS JS commands or assigning of attributes or events
@param JSName Optional current JS Object name
}
procedure TExtThread.JSCode(JS : string; JSName : string = ''; Owner : string = '');
var
  I : integer;
begin
  if JS[length(JS)] = ';' then // Command
    I := length(Response) + 1
  else  // set attribute
    if JSName = '' then begin
      ErrorMessage('Missing '';'' in command: ' + JS);
      exit;
    end
    else begin
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
  if (pos('O' + IdentDelim, JS) <> 0) and (pos('O' + IdentDelim, JSName) <> 0) then begin
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
    VarName := copy(JS, J+1, posex(IdentDelim, JS, J+3)-J)
  else
    VarName := JS;
  J := posex('/*' + VarName + '*/', Response, I);
  if J > I then begin
    K := pos('/*' + JSName + '*/', Response);
    K := posex(';', Response, K)+1;
    J := posex(';', Response, J);
    VarBody := copy(Response, K, J-K+1);
    J := LastDelimiter(CommandDelim, VarBody)+1;
    VarBody := copy(VarBody, J, length(VarBody));
    delete(Response, K+J-1, length(VarBody));
    insert(VarBody, Response, pos(DeclareJS + JSName + '=new', Response));
  end;
end;

{
Concats two JS commands only to translate nested Object Pascal typecasts as:
@param PrevCommand Command already present in Response that will be concatenated with NextCommand
@param NextCommand Command that will be concatenated with PrevCommand.
@return The JS commands concatenated
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
  J := RPos(PrevCommand, Response);
  I := Pos('.', NextCommand);
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
Does tasks related to the Request that occur before the method call invoked by Browser (PATH-INFO)
1. Detects the browser language.
2. If that language has corresponding JS resource file in framework uses it, for example: '/ext/source/locale/ext-lang-?????.js',
3. Else uses the default language (English).
4. Tests if is an AJAX request.
5. If not is AJAX request resets Sequence, Style and Libraries.
6. Tests if cookies are enabled.
@return False if Cookies are disable or if is Ajax executing the first thread request else returns true.
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
  FIsIE    := pos('MSIE', RequestHeader['HTTP_USER_AGENT']) <> 0;;
  FIsAjax  := RequestHeader['HTTP_X_REQUESTED_WITH'] = 'XMLHttpRequest';
  if not IsAjax then begin
    //Sequence  := 0;
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
      end;
  JSReturns := TStringList.Create;
end;

{$IFDEF WebServer}
procedure TExtThread.InitSessionDefs; begin
  inherited;
  Sequence := 0;
end;
{$ENDIF}

// Calls events using Delphi style
procedure TExtThread.HandleEvent;
var
  Obj : TExtObject;
begin
  if Query['IsEvent'] = '1' then begin
    Obj := TExtObject(FindObject(Query['Obj']));
    if not Assigned(Obj) then
      OnError('Object not found in session list. It could be timed out, refresh page and try again', 'HandleEvent', '')
    else
      Obj.HandleEvent(Query['Evt']);
  end
  else
  	OnError('This method only can be called by internal objects', 'HandleEvent', '');
end;

{
Does tasks after Request processing.
1. Extracts Comments, auxiliary delimiters, and sets:
2. HTML body,
3. Title,
4. Charset,
5. ExtJS CSS,
6. ExtJS libraries,
7. ExtJS theme,
8. ExtJS language,
9. Additional user styles,
10. Additional user libraries,
11. ExtJS invoke and
12. Handlers for AJAX response
}
procedure TExtThread.AfterHandleRequest;

  procedure HandleJSReturns;
  var
    I : integer;
  begin
    if JSReturns.Count <> 0 then
      for I := 0 to JSReturns.Count-1 do
        Response := AnsiReplaceStr(Response, JSReturns.Names[I], JSReturns.ValueFromIndex[I]);
    JSReturns.Free;
  end;

var
  I, J : integer;
begin
  I := pos('/*', Response);
  while I <> 0 do begin // Extracts comments
    J := PosEx('*/', Response, I);
    delete(Response, I, J - I + 2);
    I := PosEx('/*', Response, I);
  end;
  HandleJSReturns;
  Response := AnsiReplaceStr(AnsiReplaceStr(Response, CommandDelim, ''), IdentDelim, ''); // Extracts aux delimiters
  if not IsAjax then
    Response := IfThen(HTMLQuirksMode, '<!docttype html public><html>',
      '<?xml version=1.0?><!doctype html public "-//W3C//DTD XHTML 1.0 Strict//EN"><html xmlns=http://www.w3org/1999/xthml>') +
      IfThen(Application.Icon = '', '', '<link rel="shortcut icon" href="' + {$IFDEF VER2_3_1}ShortString{$ENDIF}(Application.Icon) + '"/>') +
      '<title>' + Application.Title + '</title>' +
      '<meta http-equiv="content-type" content="charset=utf-8">' +
      '<link rel=stylesheet href=' + ExtPath + '/resources/css/ext-all.css />' +
      '<script src=' + ExtPath + '/adapter/ext/ext-base.js></script>' +
      '<script src=' + ExtPath + '/ext-all' + {$IFDEF DEBUGJS}'-debug'+{$ENDIF} '.js></script>' +
      IfThen(Theme = '', '', '<link rel=stylesheet href=' + ExtPath + '/resources/css/xtheme-' + Theme + '.css />') +
      IfThen(FLanguage = 'en', '', '<script src=' + ExtPath + '/source/locale/ext-lang-' + FLanguage + '.js></script>') +
      GetStyle + Libraries +
      '<script>Ext.onReady(function(){' +
      'Ext.BLANK_IMAGE_URL="' + ExtPath + '/resources/images/default/s.gif";TextMetrics=Ext.util.TextMetrics.createInstance("body");'+
      'function AjaxSuccess(response){eval(response.responseText)};' +
      'function AjaxFailure(){Ext.Msg.show({title:"Error",msg:"Server unavailable, try later.",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});};' +
      Response + '});</script><body><div id=body></div><noscript>This web application requires JavaScript enabled</noscript></body></html>';
  {$IFDEF DEBUGJS}
  Response := AnsiReplaceStr(Response, ';', ';'^M^J)
  {$ENDIF}
end;

{
Returns a unique numeric sequence to identify a JS object, list or attribute in this session.
This sequence will be used by Self-translating process imitating a Symbol table entrance.
}
function TExtThread.GetSequence : string; begin
  Result := IntToHex(Sequence, 1);
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
  Created   := true;
  if CurrentFCGIThread <> nil then
    JSName := 'O' + IdentDelim + TExtThread(CurrentFCGIThread).GetSequence + IdentDelim;
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
  for I := 0 to high(FObjects) do try FObjects[I].Free except end;
  SetLength(FObjects, 0);
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
  if length(FObjects) = 0 then
    if Owner <> nil then begin
      if pos('/*' + Owner.JSName + '*/', CurrentFCGIThread.Response) <> 0 then
        Owner.JSCode(Attribute + ':[/*' + JSName + '*/]', Owner.JSName)
    end
    else
      TExtThread(CurrentFCGIThread).JSCode(DeclareJS + JSName + '=[/*' + JSName + '*/];');
  SetLength(FObjects, length(FObjects) + 1);
  FObjects[high(FObjects)] := Obj;
  Response := CurrentFCGIThread.Response;
  if Owner <> nil then
    OwnerName := Owner.JSName
  else
    OwnerName := '';
  if not Obj.Created or (pos(JSName, Response) = 0) then begin
    if TExtThread(CurrentFCGIThread).IsAjax and (OwnerName <> '') then
      if not Obj.Created then
        if pos(JSName, Response) = 0 then
          ListAdd := DeclareJS + Obj.JSName + '=' + OwnerName + '.add(%s);'
        else
          ListAdd := '%s'
      else
        ListAdd := OwnerName + '.add(' + Obj.JSName + ');'
    else
      ListAdd := '%s';
    if Attribute = 'items' then // Generalize it more if necessary
      ListAdd := Format(ListAdd, ['new ' + Obj.JSClassName + '({/*' + Obj.JSName + '*/})'])
    else
      ListAdd := Format(ListAdd, ['{/*' + Obj.JSName + '*/}']);
  end
  else
    if pos(Obj.JSName + '.clone', Obj.JSCommand) = 1 then
      ListAdd := obj.ExtractJSCommand
    else
      ListAdd := Obj.JSName;
  Obj.Created := true;
  Obj.JSCode(ListAdd, JSName, OwnerName);
end;

{
Returns the Ith object in the list, starts with 0.
@param I Position in list
@return <link TExtObject>
}
function TExtObjectList.GetFObjects(I : integer): TExtObject; begin
  if (I >= 0) and (I <= high(FObjects)) then
    Result := FObjects[I]
  else
    Result := nil
end;

// Returns the number of Objects in the list
function TExtObjectList.Count: integer; begin
  Result := length(FObjects)
end;

{ ExtObject }

// Set an unique <link TExtObject.JSName, JSName> using <link TExtThread.GetSequence, GetSequence>
procedure TExtObject.CreateJSName; begin
  FJSName := 'O' + IdentDelim + TExtThread(CurrentFCGIThread).GetSequence + IdentDelim;
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
When assign value to a config property, check if it is in creating process.
If not then config property code will redirect to a relationed method if it exists.
@param JSName Objects name to be searched in generated script
@return true if the JSName object has a configuration in this request, false if not
@example <code>
//doesn't matter if you are into Create block or assign to previous ajax created object
//O1.title will be mapped to O1.setTitle('new title', ''); by ExtToPascal wrapper
O1.title = 'new title';
</code>
}
function TExtObject.ConfigAvailable(JSName : string) : boolean; begin
  Result := pos('/*' + JSName + '*/', TExtThread(CurrentFCGIThread).Response) <> 0;
end;

{
Used by <link TExtObject.Create, Create> to initialize the JSName, to <link TFCGIThread.AddToGarbage, add to Garbage Collector>
and to generate <link TExtObject.JSCode, JS code>
@param JS JS constructor for the JS <color red>new</color> command
@see CreateVarAlt
}
procedure TExtObject.CreateVar(JS : string); begin
  CreateJSName;
  CurrentFCGIThread.AddToGarbage(JSName, Self);
  insert('/*' + JSName + '*/', JS, length(JS)-IfThen(pos('});', JS) <> 0, 2, 1));
  Created := true;
  JSCode(CommandDelim + DeclareJS + JSName + '=new ' + JS);
  JSCode(JSName + '.nm="' + JSName + '";');
end;

{
Alternate create constructor, it is an ExtJS fault
@see CreateVar
}
procedure TExtObject.CreateVarAlt(JS : string); begin
  CreateJSName;
  CurrentFCGIThread.AddToGarbage(JSName, Self);
  insert('/*' + JSName + '*/', JS, length(JS)-IfThen(pos('});', JS) <> 0, 2, 1));
  Created := true;
  JSCode(CommandDelim + DeclareJS + JSName + '= ' + JS);
  JSCode(JSName + '.nm="' + JSName + '";');
end;

// Deletes JS object from Browser memory
procedure TExtObject.Delete; begin
  JSCode('delete ' + JSName + ';')
end;

// <link TFCGIThread.DeleteFromGarbage, Removes object from Garbage Collector> if is not in a Garbage Collector call
procedure TExtObject.DeleteFromGarbage; begin
  if CurrentFCGIThread <> nil then CurrentFCGIThread.DeleteFromGarbage(Self);
end;

// Calls Ext JS <b>destroy()</b> method if it exists else calls the JS <b>delete</b> command
function TExtObject.DestroyJS : TExtFunction; begin
  Delete;
  Result := TExtFunction(Self)
end;

// <link TExtObject.DeleteFromGarbage, Removes object from Garbage Collector> and frees it
destructor TExtObject.Destroy; begin
  try
    DeleteFromGarbage;
    inherited;
  except end;
end;

{
Creates a TExtObject and generate corresponding JS code using <link TExtObject.JSCode, Self-translating>
@param Owner Optional parameter used internally by <link TExtObject.JSObject, JSObject> and <link TExtObject.JSArray, JSArray> only
}
constructor TExtObject.Create(Owner : TExtObject = nil); begin
  if Owner = nil then CreateVar(JSClassName + '({});');
end;

{
Used by Parser to build <link TExtObject.InitDefaults, InitDefaults> methods used to initialize public JS properties in a TExtObject
@param Owner TExtObject where this property is declared
@param Attribute Public JS property name
}
constructor TExtObject.CreateInternal(Owner : TExtObject; Attribute : string); begin
  FJSName := Owner.JSName + '.' + Attribute;
  Created := true;
end;

// Returns 'Object' that is the default class name for Ext JS objects
function TExtObject.JSClassName: string; begin
  Result := 'Object'
end;

{
Tests if a class name is parent of this object
@param CName Class name with "T" prefix
@return True if CName is parent of this object and false if not
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
Starts Self-translating mechanism invoking <link TExtThread.JSCode, JSCode>.
Invokes <link TExtThread.JSConcat, JSConcat> if identify a nested typecast
@param JS JS commands or declarations
@param pJSName Optional, by default is the <link TExtObject.JSName, JSName>, when used by <link TExtObjectList.Add> is the TExtObjectList.JSName
@param pOwner Optional, used by <link TExtObjectList.Add> only to pass the TExtObject owner list
}
procedure TExtObject.JSCode(JS : string; pJSName : string = ''; pOwner : string = ''); begin
  if JS <> '' then begin
    if (pos('.nm="', JS) = 0) and (JS[length(JS)] = ';') and not(pos(DeclareJS, JS) in [1, 2]) then begin
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

// Inits a JS Object with a <link TExtFunction>
constructor TExtObject.Init(Method : TExtFunction); begin
  CreateJSName;
  Created := true;
  JSCode(CommandDelim + DeclareJS + JSName + '=' + Method.ExtractJSCommand + ';');
end;

constructor TExtObject.Init(Command: string); begin
  CreateJSName;
  CurrentFCGIThread.AddToGarbage(JSName, Self);
  Created := true;
  JSCode(CommandDelim + DeclareJS + JSName + '=' + Command + ';');
end;

procedure TExtObject.InitDefaults; begin end;

{
Generates JS code to declare an inline JS Array.
@param JSON JavaScript Object Notation, the body of Array declaration
@return <link TExtObjectList> to be used in assigns
}
function TExtObject.JSArray(JSON : string) : TExtObjectList; begin
  Result := TExtObjectList(TExtObject.Create(Self));
  TExtObject(Result).FJSName := '[' + JSON + ']';
end;

{
Generates JS code to declare an inline generic JS object.
It is necessary in 3 cases:
1. When the Ext JS documentation informs that the attribute is an object without any particular type (Object),
   as JavaScript language is almost typeless it happens eventually. That would be equivalent to the type Variant of VB or Delphi.
   Examples include data records.
2. There are omissions in the documentation and attribute actually belongs to a specific class, in this case use the JSObject method,
   do a typecast or declare in ExtFixes.txt file, this allows to register in the Wrapper the omissions of the
   documentation or the framework.
3. There are omissions in the framework, ie should be a specific class. Read its attributes in description contained
   in the documentation and declare them in ExtFixes.txt for the Wrapper to recognize them or use JSObject method.
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
Lets use a JS expression to set a ExtJS property or parameter.   <link TExtFunction, ExtJS function> in ExtJS properties and parameters.
The expression will be called in the browser side and should returns an integer.
@param Expression JS Expression using or not Delphi Format specifiers: (%s, %d and etc), use %s for Methods and %d for integers
@param MethodsValues Array of Methods or Integer values to use in Expression
@return Integer Internal and fake value to return
@example <code>
Grid := TExtGridEditorGridPanel.Create;
with Grid do begin
  Width  := JSExpression(Panel.GetInnerWidth);
  Height := JSExpression(Panel.GetInnerHeight);
end;
FormWidth := 40;
with TExtWindow.Create do
  Width := JSExpression('%s * %d', [ExtUtilTextMetrics.GetWidth('g'), FormWidth]);
  // or Width := JSExpression('Ext.util.TextMetrics.getWidth("g") * ' + IntToStr(FormWidth), []);
</code>
}
function TExtObject.JSExpression(Expression : string; MethodsValues : array of const) : integer;
var
  Mark, Command : string;
  I : integer;
begin
  with TExtThread(CurrentFCGIThread) do begin
    Result := StrToInt('-$7' + GetSequence + '7');
    Mark   := IntToStr(Result);
    for I := 0 to high(MethodsValues) do
      with MethodsValues[I] do
        if VType = vtObject then begin
          Command     := TExtFunction(VObject).ExtractJSCommand; // FPC idiosincrasy
          VAnsiString := pointer(Command);
          VType       := vtAnsiString;
        end;
    JSReturns.Values[Mark] := Format(Expression, MethodsValues);
  end;
end;

function TExtObject.JSExpression(Method : TExtFunction) : integer; begin
  Result := JSExpression('%s', [Method]);
end;

// Must be implemented on extended classes where events can be handled
procedure TExtObject.HandleEvent(const AEvtName: string); begin end;

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
@param Silent Discards JS exceptions and returns immediately
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
function TExtObject.JSFunction(Method : TExtProcedure; Silent : boolean = false) : TExtFunction;
var
  CurrentResponse : string;
begin
  Result := TExtFunction(Self);
  with TExtThread(CurrentFCGIThread) do begin
    CurrentResponse := Response;
    Response := '';
    Method;
    if Silent then
      JSCommand := 'try{' + Response + '}catch(e){};'
    else
      JSCommand := Response;
    Response  := CurrentResponse;
    JSCode(JSCommand);
  end;
end;

{
Invokes an Object Pascal published procedure in AJAX mode.
To get event parameters use %0, %1 until %9 place holders.<p>
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
To get event parameters use %0, %1 until %9 place holders.<p>
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
  MetName : string;
begin
  MetName := CurrentFCGIThread.MethodName(@Method);
  if MetName <> '' then
    Result := Ajax(MetName, Params, false)
  else begin
    Result  := TExtFunction(Self);
    JSCode('Ext.Msg.show({title:"Error",msg:"Ajax method not published.",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});');
  end;
end;

// Internal Ajax generation handler treating IsEvent, when is true HandleEvent will be invoked instead published methods
function TExtObject.Ajax(MethodName : string; Params : array of const; IsEvent : boolean) : TExtFunction;
var
  lParams : string;
  I : integer;
begin
  Result := TExtFunction(Self);
  lParams := 'Ajax=1';
  if IsEvent then begin
    lParams := lParams + '&IsEvent=1&Obj=' + JSName + '&Evt=' + MethodName;
    MethodName := 'HandleEvent';
  end;
  for I := 0 to high(Params) do
    with Params[I] do
      if Odd(I) then
        case VType of
          vtAnsiString : lParams := lParams + '"+' + string(VAnsiString) + '+"';
          vtString     : lParams := lParams + '"+' + VString^ + '+"';
          vtObject     : begin
            lParams := lParams + '"+' + TExtObject(VObject).ExtractJSCommand + '+"';
            TExtObject(VObject).JSCommand := '';
          end;
          vtInteger    : lParams := lParams + IntToStr(VInteger);
          vtBoolean    : lParams := lParams + IfThen(VBoolean, 'true', 'false');
          vtExtended   : lParams := lParams + FloatToStr(VExtended^);
          vtVariant    : lParams := lParams + string(VVariant^);
          vtChar       : lParams := lParams + '"+' + VChar + '+"';
        end
      else
        case VType of
          vtAnsiString : lParams := lParams + '&' + string(VAnsiString) + '=';
          vtString     : lParams := lParams + '&' + VString^ + '=';
          vtChar       : lParams := lParams + '&' + VChar + '=';
        else
          JSCode('Ext.Msg.show({title:"Error",msg:"Ajax method: ' + MethodName +
            ' has an invalid parameter name in place #' + IntToStr(I+1) + '",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});');
          exit;
        end;
  JSCode('Ext.Ajax.request({url:"' + CurrentFCGIThread.RequestHeader['SCRIPT_NAME'] + '/' + MethodName +
    '",params:"' + lParams + '",success:AjaxSuccess,failure:AjaxFailure});');
end;

{
Encapsulates JS commands in an anonymous JS function, find %0..%9 place holders and declares respective event parameters
@param Command JS command to convert to JS function
@return The code for an anonymous JS function with optional event parameters declared
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
      J := max(J, StrToInt(Command[I+1]));
    end;
    I := posex('%', Command, I);
  end;
  for I := 0 to J do begin
    Params := Params + 'P' + IntToStr(I);
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
Extracts <link TExtObject.JSCommand, JSCommand> from Response and resets JSCommand
@return JSCommand without the last char ';'
@see TExtThread.RemoveJS
}
function TExtObject.ExtractJSCommand : string; begin
  Result := JSCommand;
  TExtThread(CurrentFCGIThread).RemoveJS(Result);
  SetLength(Result, length(Result)-1);
  JSCommand := ''
end;

{
Frees TExtObject if object is not destroyed. Automatically calls Free for all components that it does reference.
Free is successful even if called repeatedly to the same object.
@param CallDestroyJS Calls <link TExtObject.DestroyJS, DestroyJS> if true to destroy or delete the JS object too
}
procedure TExtObject.Free(CallDestroyJS : boolean = false); begin
  if (Self <> nil) and Created then begin
    if CallDestroyJS then DestroyJS;
    Destroy;
    Created := false;
  end;
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
        vtAnsiString: Result := Result + StrToJS(string(VAnsiString));
        vtString:     Result := Result + StrToJS(VString^);
        vtInteger:    Result := Result + IntToStr(VInteger);
        vtBoolean:    Result := Result + IfThen(VBoolean, 'true', 'false');
        vtExtended:   Result := Result + FloatToStr(VExtended^);
        vtVariant:    Result := Result + string(VVariant^)
      end;
    if I < high(A) then Result := Result + ',';
    inc(I);
  end;
  if (Result <> '') and (Result[length(Result)] = ',') then System.Delete(Result, length(Result), 1);
end;

{
Converts a <link TExtObjectList> to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
@param Exts An TExtObjectList to convert
@return JSON representation of Exts
}
function TExtObject.VarToJSON(Exts : TExtObjectList) : string; begin
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
function TExtObject.VarToJSON(Strs : TArrayOfString) : string;
var
  I : integer;
begin
  Result := '[';
  for I := 0 to high(Strs) do begin
    Result := Result + StrToJS(Strs[I]);
    if I < high(Strs) then Result := Result + ',';
  end;
  Result := Result + ']'
end;

{
Converts an <link TArrayOfInteger, array of integers> to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
@param Ints An <link TArrayOfInteger, array of integers> to convert
@return JSON representation of Ints
}
function TExtObject.VarToJSON(Ints : TArrayOfInteger) : string;
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

function TExtObject.ParamAsInteger(ParamName : string) : integer; begin
  Result := StrToIntDef(CurrentFCGIThread.Query[ParamName], 0);
end;

function TExtObject.ParamAsDouble(ParamName : string) : double; begin
  Result := StrToFloatDef(CurrentFCGIThread.Query[ParamName], 0);
end;

function TExtObject.ParamAsBoolean(ParamName : string) : boolean; begin
  Result := CurrentFCGIThread.Query[ParamName] = 'true';
end;

function TExtObject.ParamAsString(ParamName : string) : string; begin
  Result := CurrentFCGIThread.Query[ParamName];
end;

function TExtObject.ParamAsTDateTime(ParamName : string) : TDateTime; begin
  Result := ParamAsDouble(ParamName);
end;

function TExtObject.ParamAsObject(ParamName : string) : TExtObject; begin
  Result := TExtObject(CurrentFCGIThread.FindObject(CurrentFCGIThread.Query[ParamName]));
end;

begin
  ExtUtilTextMetrics.FJSName := 'TextMetrics';
end.
