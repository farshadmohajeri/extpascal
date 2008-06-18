unit ExtPascal;
{
Basic classes for JavaScript and Ext JS translating from Object Pascal.
Associates semantic concepts of JavaScript and Ext JS for Object Pascal, such as: Function, Object, List of Objects and Ajax Method.
It's the heart of the automatic translation method from Object Pascal to JavaScript, that I call "Self-translating".
It takes advantage of the fact that JavaScript and Object Pascal are structurally similar languages,
where there is almost one to one parity between its syntax and semantic structures.

ExtPascal é composto por três componentes principais:

1. The Parser @[link ExtToPascal] able to scan Ext JS documentation in HTML format and to create the Wrapper.
2. The Wrapper programmatically created by Parser, is in fact a set of units (twelve in Ext JS 2.1)
   which has the definition of all Ext JS classes, properties, methods and events.
3. The Self-translating engine, this unit. It's triggered when using the Wrapper,
   ie by creating objects, assigning properties and events, and invoking methods.

@[image c:\trabalho\extpascal\images\extpascal.gif]

1. The Parser read the HTML documentation of Ext JS and
2. Generate the Wrapper.
3. With the application on-line a browser session to do a request.
4. The application create ExtObjects, set properties and call methods from Wrapper Units.
5. For each these tasks the "Self-translating" is invoked
6. Generating JavaScript code that uses Ext JS classes.
7. At end of request processing, the application read and format all JS generated
8. And send the response to browser session. New requests can be done begining of step 3.

So the translation is not focused on JavaScript language, but to access widget frameworks made in JavaScript.
In this way the use of (X)HTML, JavaScript and CSS is minimum.
Indeed the Parser can be adapted to read the documentation of another JavaScript framework, Dojo for example.
}
interface

uses
  FCGIApp;

const
  ExtPath = '/ext'; // Instalation path of Ext JS framework, below the your Web server document root

type
  TArrayOfString  = array of string;
  TArrayOfInteger = array of Integer;

  TExtObjectList = class;
  TExtFunction   = class;
  TExtPublishedMethod = procedure of object; // Tipo que define uma procedure que pode ser chamada num @[link TExtObject.AJAX] request

  {
  Representa uma sessão de usuário aberta em um browser. Cada sessão é uma thread FastCGI que possui recursos adicionais do
  JavaScript e do Ext JS, tais como: tema, língua, Ajax, mensagens de erro no padrão Ext, definição de bibliotecas JS e
  regras de estilo (CSS) da sessão.
  O "Self-translating" está implementado nesta classe no método @[link JSCode].
  }
  TExtThread = class(TFCGIThread)
  private
    Style, Libraries, FLanguage : string;
    Sequence : cardinal;
    FIsAjax  : boolean;
    procedure RelocateVar(JS, JSName : string; I: integer);
    procedure RemoveJS(JS : string);
    function GetStyle: string;
  protected
    function BeforeHandleRequest : boolean; override;
    procedure AfterHandleRequest; override;
    procedure OnError(Msg, Method, Params : string); override;
    function GetSequence : string;
    function JSConcat(PrevCommand, NextCommand : string) : string;
  public
    Theme : string; // Set or get Ext JS installed theme
    property Language : string read FLanguage; // Actual language for this session, read HTTP_ACCEPT_LANGUAGE header
    property IsAjax : boolean read FIsAjax; // Test if execution is occuring in an AJAX request
    procedure JSCode(JS : string; JSName : string = ''); // Self-translating main procedure
    procedure SetStyle(pStyle : string = ''); // Set or reset stylesheet rules for this session
    procedure SetLibrary(pLibrary : string = ''); // Set or reset aditional JavaScript libraries, além das necessárias pelo Ext JS
    procedure ErrorMessage(Msg : string; Action : string = ''); overload;
    procedure ErrorMessage(Msg : string; Action : TExtFunction); overload;
  end;

  {
  Classe pai de todas as classes e componentes do framework Ext JS.
  Cada TExtObject tem a capacidade de se auto traduzir para JavaScript durante sua programação.
  Ou seja, quando uma propriedade é atribuída ou um método é chamado o "Self-translating" entra em ação
  traduzindo esses comandos Objet Pascal em JavaScript. Para isso o método @[link TExtThread.JSCode] é invocado cada vez
  }
  TExtObject = class
  private
    FJSName : string;
    function WriteFunction(Command: string): string;
  protected
    JSCommand : string;
    function VarToJSON(A : array of const)     : string; overload;
    function VarToJSON(Exts : TExtObjectList)  : string; overload;
    function VarToJSON(Strs : TArrayOfString)  : string; overload;
    function VarToJSON(Ints : TArrayOfInteger) : string; overload;
    function IsParent(CName : string): boolean;
    function ExtractJSCommand(pJSCommand : string) : string;
    procedure CreateVar(JS : string);
    procedure CreateVarAlt(JS : string);
    procedure CreateJSName;
  protected
    procedure InitDefaults; virtual;
  public
    constructor Create(Owner : TExtObject = nil);
    constructor CreateSingleton(pJSName : string = '');
    constructor AddTo(List : TExtObjectList);
    function JSClassName : string; virtual;
    function JSArray(JSON : string) : TExtObjectList;
    function JSObject(JSON : string; ObjectConstructor : string = '') : TExtObject;
    function JSFunction(Params, Body : string) : TExtFunction; overload;
    procedure JSFunction(Name, Params, Body : string); overload;
    function JSFunction(Body : string): TExtFunction; overload;
    function JSFunction(Method: TExtPublishedMethod) : TExtFunction; overload;
    procedure JSCode(JS : string; pJSName : string = ''); // Método de ligação
    function Ajax(Method : TExtPublishedMethod) : TExtFunction; overload;
    function Ajax(Method : TExtPublishedMethod; Params : array of const) : TExtFunction; overload;
    property JSName : string read FJSName;
  end;

  {
  Classe básica que define uma função JavaScript. Todas as funções/procedures do framework Ext JS são convertidas para functions Pascal
  cujo retorno pertence a esta classe. Com isso todas as funções convertidas pelo @[link ExtPascal Wrapper] podem ser atribuídas a
  tratadores de eventos.
  }
  TExtFunction = class(TExtObject);

  {
  Lista de TExtObjects. O @[link ExtPascal Wrapper] converte o tipo Array do JavaScript para esta classe
  }
  TExtObjectList = class(TExtFunction)
  private
    FObjects : array of TExtObject;
    Attribute, JSName : string;
    Owner : TExtObject;
    function GetFObjects(I: integer): TExtObject;
  public
    property Objects[I : integer] : TExtObject read GetFObjects; default;
    constructor CreateSingleton(pJSName: string);
    constructor Create(pOwner : TExtObject = nil; pAttribute : string = '');
    destructor Destroy; override;
    procedure Add(Obj : TExtObject);
    function Count : integer;
  end;

{$IFNDEF DELPHIDOC}
  // Classes que não devem ser documentadas.
  // Elas são normalmente classes básicas do JavaScript que não são referenciadas na documentação do Ext JS, por omissão ou por falha
  THTMLElement = class(TExtObject);
  TStyleSheet = class(TExtObject);
  TRegExp = class(TExtObject);
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
  TExtLibRegion = TRegion; //doc fault
  TvisMode = Integer; // doc fault
  TThe = TExtObject; // doc fault
  TThis = TExtObject; // doc fault
  TairNativeMenu = TExtObject;
  TX = TExtObject; // doc fault
  TN1 = TExtObject; // doc fault
  TN2 = TExtObject; // doc fault
  TLayout = TExtObject; // Poor hierarchy definition
  TId = TExtObject;// doc fault
  TiPageX = TExtObject; // doc fault
  TiPageY = TExtObject; // doc fault
  TExtGridGrid = TExtObject; // doc fault
  TTreeSelectionModel = TExtObject; // doc fault
  TSelectionModel = TExtObject; // doc fault
  TDataSource = TExtObject; // doc fault
{$ENDIF}

implementation

uses
  SysUtils, StrUtils, Math;

{ TExtThread }

{
Remove identificated JS command from response.
Used internally by Sel-Translating mechanism to repositioning JS commands.
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
Add/Remove a user JS library to be used in current response.
Common requests fazem reset das user libraries e das user style.
Por outro lado os AJAX requests não fazem reset e são considerados parte ou continuação do mesmo request.
@param pLibrary JS library with Path based on Web server document root.
If pLibrary is '' then all user JS libraries to this session will be removed from response.
@example SetLibrary('');
@example SetLibrary(@[link ExtPath] + '/examples/tabs/TabCloseMenu.js');
}
procedure TExtThread.SetLibrary(pLibrary: string); begin
  if pLibrary = '' then
    Libraries := ''
  else
    Libraries := Libraries + '<script src=' + pLibrary + '></script>';
end;

(*
Add/Remove a user stylesheet to be used in current response.
Common requests fazem reset das user libraries e das user style.
Por outro lado os AJAX requests não fazem reset e são considerados parte ou continuação do mesmo request.
@param pStyle Styles to apply upon HTML or Ext elements in this response using CSS notation.
If pStyle is '' then all user styles to this session will be removed from response.
@example SetStyle('');
@example SetStyle('img:hover{border:1px solid blue}');
*)
procedure TExtThread.SetStyle(pStyle: string); begin
  if pStyle = '' then
    Style := ''
  else
    Style := Style + pStyle
end;

{
Returns all styles in use in current response
}
function TExtThread.GetStyle : string; begin
  if Style = '' then
    Result := ''
  else
    Result := '<style>' + Style + '</style>';
end;

{
Show an error message in browser session using Ext JS style.
@param Msg Message text, can to use HTML to formating text.
@param Action Optional action that will be executed after user to click Ok button. Could be JavaScript or ExtPascal commands
@example ErrorMessage('User not found.');
@example ErrorMessage('Context not found.<br/>This Window will be reloaded to fix this issue.', 'window.location.reload()');
}
procedure TExtThread.ErrorMessage(Msg : string; Action : string = ''); begin
  JSCode('Ext.Msg.show({title:"Error",msg:"' + Msg + '",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK' +
    IfThen(Action = '', '', ',fn:function(){' + Action + '}') + '});');
end;

{
Show an error message in browser session using Ext JS style.
@param Msg Message text, can to use HTML to formating text.
@param Action Optional action that will be executed after user to click Ok button. Could be JavaScript or ExtPascal commands
@example ErrorMessage('Illegal operation.<br/>Click OK to Shutdown.', Ajax(Shutdown));
}
procedure TExtThread.ErrorMessage(Msg : string; Action : TExtFunction); begin
  JSCode('Ext.Msg.show({title:"Error",msg:"' + Msg + '",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK' +
    ',fn:function(){' + Action.ExtractJSCommand(Action.JSCommand) + '}});');
end;

{
Occurs when an exception is raised during the execution of method that handle the request (PATH_INFO).
Display error message with exception message, method name and method params.
@exception TAccessViolation If current request is AJAX a sessão pode ser recuperada com um reload da página.
@param Msg Exception message
@param Method Method name invoked by Browser (PATH_INFO) or thru AJAX request
@param Params Method params
}
procedure TExtThread.OnError(Msg, Method, Params : string); begin
  if IsAjax and (pos('Access violation', Msg) <> 0) then
    Msg := Msg + '<br/><b>Reloading this page (F5) perhaps fix this error.</b>';
  ErrorMessage(Msg + '<br/><hr/>Method: ' + Method + IfThen(Params = '', '', '<br/>Params: ' + Params));
end;

{
Self-translating main procedure. Translate Object Pascal code to JavaScript code.
Self-translating (ST) is not a compiler. It's a minimalist (very small, ultra quick an dirty) approach that imitates an online interpreter.
You program in Object pascal and when the program runs it automatically generates the corresponding JavaScript code.

But thre is an essential limitation, it does not create business rules or sophisticated behaviour in JavaScript.
So it is not interpret "IF", "WHILE", "CASE", "TRY", etc commands, but "IF", "WHILE", etc realize a conditional code generation
on Server side as ASP and JSP do it. ST is used to create objects and widgets, to set properties and events, to call methods.
It's analogous to Delphi .dfm file role: to describe a GUI.
There are aditional facilities to invoke Server side logic using @[link AJAX], to define small @[link JSFunction functions] in JavaScript and
to use @[link SetLibrary large JS libraries]. It's enough to create powerful GUIs.
The rest (business rules, database access, etc) should be done in Object Pascal on Server side.

Basic work:
JS commands are appended to Response.
JS attributes are found in Response using yours JSName attribute and setted in place.
If not found, JS attributes are appended to Response.

@param JS JS commands or assigning of attributes or events
@param JSName Optional current JS Object name
}
procedure TExtThread.JSCode(JS : string; JSName : string = '');
var
  I : integer;
begin
  if JS[length(JS)] = ';' then // Command
    I := length(Response) + 1
  else begin // set attribute
    I := pos('/*' + JSName + '*/', Response);
    if I = 0 then begin
      if IsAjax then JS := JSName + '.' + AnsiReplaceStr(JS, ':', '=') + ';';
      I := length(Response) + 1;
    end;
    if (I > 1) and not(Response[I-1] in ['{', '[', '(', ';']) then JS := ',' + JS;
  end;
  insert(JS, Response, I);
  if (pos('O_', JS) <> 0) and (pos('O_', JSName) <> 0) then
    RelocateVar(JS, JSName, I+length(JS));
end;

{
Garante que a declaração de um Objeto JS (var) ocorre antes da sua utilização; Chamada de métodos, uso de atributos, etc.
Se isso não ocorrer realoca a declaração para uma posição física anterior à sua utilização.
@param JS Comando ou atribuição que utiliza o JS Object.
@param JSName JS Object corrente.
@param I Posição física no Response onde o JS comando termina.
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
    K := pos('var ' + VarName + '=new', Response);
    J := posex(';', Response, J);
    VarBody := copy(Response, K, J-K+1+length(VarName));
    delete(Response, K, length(VarBody));
    insert(VarBody, Response, pos('var ' + JSName + '=new', Response));
  end;
end;

{
Concatena dois comandos JS exclusivamente para traduzir typecasts aninhados em Object Pascal como:
@[sample TExtGridRowSelectionModel(GetSelectionModel).SelectFirstRow;]
Que normalmente seria traduzido para:
@sample[O1.getSelectionModel;
O1.selectFirstRow;]
ao invés de:
@[sample O1.getSelectionModel.selectFirstRow;]
@param PrevCommand Comando já presente no Response que será concatenado com o próximo comando
@param NextCommand Comando a ser concatenado com o anterior.
}
function TExtThread.JSConcat(PrevCommand, NextCommand: string): string;
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
Realiza tarefas relacionada ao Request que ocorrem antes da chamada do método invocada pelo Browser (PATH-INFO)
1. Determina a linguagem do browser
2. Verifica se essa linguagem tem arquivo correspondente no Ext JS '/ext/source/locale/ext-lang-?????.js'
3. Se não tem usa a língua default (inglês) se existe usa essa língua.
4. Verifica se é um request AJAX.
5. If not is AJAX reset Sequence, Style and Libraries
6. Verifica se os cookies estão ativados.

@return Devolve false se Cookies estão desativados ou se é Ajax executando no primeiro request de uma thread.
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
      ErrorMessage('This web application requires Cookies enabled to Ajax works.');
      Result := false;
    end
    else
      if NewThread then begin
        ErrorMessage('Context not found.<br/>This Window will be reloaded to fix this issue.', 'window.location.reload()');
        Result := false;
      end
end;

{
Realiza tarefas após o processamento do Request.
1. Extract Comments.
2. Set:
  2.1. HTML body,
  2.2. Title,
  2.3. Charset,
  2.4. ExtJS CSS,
  2.5. ExtJS libraries,
  2.6. ExtJS theme,
  2.7. ExtJS language,
  2.8. Aditional user styles,
  2.9. Aditional user libraries,
  2.9. ExtJS invoke and
  2.10. Handlers for AJAX response
}
procedure TExtThread.AfterHandleRequest;
var
  I, J : integer;
begin
  I := pos('/*', Response);
  while I <> 0 do begin // Extract comments
    J := PosEx('*/', Response, I);
    delete(Response, I, J - I + 2);
    I := PosEx('/*', Response, I);
  end;
  Response := AnsiReplaceStr(Response, '_', '');
  if not IsAjax then begin
    Response := '<!doctype html public><html><title>' + Application.Title + '</title>' +
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
      Response +
      '});</script><body><div id=body></div></body></html>';
  end;
end;

{
Return a unique numeric sequence to identify a JS object, list or attribute in this session.
This sequence will be used by Self-translating process imitating a Symbol table entrance.
}
function TExtThread.GetSequence: string; begin
  Result := IntToStr(Sequence);
  inc(Sequence);
end;

{ ExtObjectList }

constructor TExtObjectList.Create(pOwner : TExtObject = nil; pAttribute : string = ''); begin
  Attribute := pAttribute;
  Owner     := pOwner;
  JSName    := 'O_' + TExtThread(CurrentFCGIThread).GetSequence + '_';
end;

constructor TExtObjectList.CreateSingleton(pJSName : string); begin
  Attribute := JSName;
end;

destructor TExtObjectList.Destroy;
var
  I : integer;
begin
  for I := 0 to length(FObjects)-1 do FObjects[I].Free;
  inherited;
end;

procedure TExtObjectList.Add(Obj : TExtObject);
var
  ListAdd, Response : string;
begin
  if length(FObjects) = 0 then
    if Owner <> nil then
      Owner.JSCode(Attribute + ':[/*' + JSName + '*/]', Owner.JSName)
    else
      TExtThread(CurrentFCGIThread).JSCode('var ' + JSName + '=[/*' + JSName + '*/];');
  SetLength(FObjects, length(FObjects) + 1);
  FObjects[high(FObjects)] := Obj;
  Response := TExtThread(CurrentFCGIThread).Response;
  if pos(Obj.JSName, Response) = 0 then begin
    if (pos(JSName, Response) = 0) and TExtThread(CurrentFCGIThread).IsAjax then
      ListAdd := 'var ' + Obj.JSName + '=' + Owner.JSName + '.add(%s);'
    else
      ListAdd := '%s';
    if Attribute = 'items' then // Generalize it more if necessary
      ListAdd := Format(ListAdd, ['new ' + Obj.JSClassName + '({/*' + Obj.JSName + '*/})'])
    else
      ListAdd := Format(ListAdd, ['{/*' + Obj.JSName + '*/}']);
    Obj.JSCode(ListAdd, JSName);
  end
  else
    Obj.JSCode(Obj.JSName, JSName);
end;

function TExtObjectList.GetFObjects(I: integer): TExtObject; begin
  Result := FObjects[I]
end;

function TExtObjectList.Count: integer; begin
  Result := length(FObjects)
end;

{ ExtObject }

procedure TExtObject.CreateJSName; begin
  FJSName := 'O_' + TExtThread(CurrentFCGIThread).GetSequence + '_';
end;

constructor TExtObject.CreateSingleton(pJSName : string = ''); begin
  InitDefaults;
  if pJSName = '' then
    FJSName := JSClassName
  else
    FJSName := pJSName;
end;

procedure TExtObject.CreateVar(JS : string); begin
  CreateJSName;
  insert('/*' + JSName + '*/', JS, length(JS)-IfThen(pos('});', JS) <> 0, 2, 1));
  JSCode('var ' + JSName + '=new ' + JS)
end;

// Alternate create constructor, it is an ExtJS fault
procedure TExtObject.CreateVarAlt(JS : string); begin
  CreateJSName;
  insert('/*' + JSName + '*/', JS, length(JS)-IfThen(pos('});', JS) <> 0, 2, 1));
  JSCode('var ' + JSName + '= ' + JS)
end;

{
Create a TExtObject and generate corresponding JS code using "Self-translating"
@param Owner Optional
}
constructor TExtObject.Create(Owner : TExtObject = nil); begin
  if Owner = nil then CreateVar(JSClassName + '({});')
end;

{
Returns 'Object' that is the default class name for Ext JS objects
}
function TExtObject.JSClassName: string; begin
  Result := 'Object'
end;

{
Test if a class name is parent of this object
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

procedure TExtObject.JSCode(JS : string; pJSName : string = ''); begin 
  if JS <> '' then begin
    if (JS[length(JS)] = ';') and (pos('var ', JS) <> 1) then begin
      if (JSCommand <> '') and (pJSName <> '') and  not IsParent(pJSName) then begin
        JSCommand := TExtThread(CurrentFCGIThread).JSConcat(JSCommand, JS);
        exit;
      end;
      JSCommand := '/*' + TExtThread(CurrentFCGIThread).GetSequence + '*/' + JS;
      JS := JSCommand
    end
    else
      JSCommand := '';
    if pJSName = '' then pJSName := JSName;
    TExtThread(CurrentFCGIThread).JSCode(JS, pJSName);
  end;
end;

constructor TExtObject.AddTo(List : TExtObjectList); begin
  if JSName = '' then begin
    InitDefaults;
    CreateJSName;
  end;
  List.Add(Self);
end;

procedure TExtObject.InitDefaults; begin end;

function TExtObject.JSArray(JSON: string): TExtObjectList; begin
  Result := TExtObjectList(TExtObject.Create(Self));
  TExtObject(Result).FJSName := '[' + JSON + ']';
end;

function TExtObject.JSObject(JSON : string; ObjectConstructor : string = ''): TExtObject; begin
  Result := TExtObject.Create(Self);
  if ObjectConstructor = '' then
    Result.FJSName := '{' + JSON + '}'
  else
    Result.FJSName := 'new ' + ObjectConstructor + '({' + JSON + '})'
end;

function TExtObject.JSFunction(Params, Body : string) : TExtFunction; begin
  Result := TExtFunction.Create(Self);
  Result.FJSName := 'function(' + Params + '){' + Body + '}';
end;

function TExtObject.JSFunction(Body : string) : TExtFunction; begin
  Result := JSFunction('', Body);
end;

procedure TExtObject.JSFunction(Name, Params, Body : string); begin
  JSCode('function ' + Name + '(' + Params + '){' + Body + '};');
end;

function TExtObject.JSFunction(Method : TExtPublishedMethod) : TExtFunction;
var
  CurrentResponse : string;
begin
  Result := TExtFunction(Self);
  with TExtThread(CurrentFCGIThread) do begin
    CurrentResponse := Response;
    Response := '';
    Method;
    JSCommand := Response;
    Response := CurrentResponse;
    JSCode(JSCommand);
  end;
end;

function TExtObject.Ajax(Method: TExtPublishedMethod): TExtFunction; begin
  Result := Ajax(Method, []);
end;

function TExtObject.Ajax(Method: TExtPublishedMethod; Params: array of const): TExtFunction;
var
  MetName, lParams : string;
  I : integer;
begin
  Result  := TExtFunction(Self);
  MetName := TExtThread(CurrentFCGIThread).MethodName(@Method);
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
    JSCode('Ext.Ajax.request({url:"' + TExtThread(CurrentFCGIThread).RequestHeader['SCRIPT_NAME'] + '/' + MetName +
      '",params:"' + lParams + '",success:AjaxSuccess,failure:AjaxFailure});');
  end
  else
    JSCode('Ext.Msg.show({title:"Error",msg:"Ajax method not published.",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});');
end;

function TExtObject.WriteFunction(Command : string) : string;
var
  I, J : integer;
  Params : string;
begin
  Params := '';
  J := -1;
  I := pos('%', Command);
  while I <> 0 do begin
    if Command[I+1] in ['0'..'9'] then begin
      Command[I] := 'P';
      delete(Command, I+2, 1);
      delete(Command, I-1, 1);
      inc(J);
    end;
    I := posex('%', Command, I);
  end;
  for I := 0 to J do begin
    Params := 'P' + IntToStr(I);
    if I <> J then Params := Params + ','
  end;
  I := LastDelimiter(';', copy(Command, 1, length(Command)-1));
  if I = 0 then
    Command := 'return ' + Command
  else
    insert('return ', Command, I+1);
  Result := 'function(' + Params + '){' + Command + '}';
end;

function TExtObject.ExtractJSCommand(pJSCommand : string) : string;
var
  I : integer;
begin
  Result := pJSCommand;
  TExtThread(CurrentFCGIThread).RemoveJS(Result);
  I := pos('*/', Result);
  if I <> 0 then Result := copy(Result, I+2, length(Result)-I-2)
end;

function TExtObject.VarToJSON(A : array of const): string;
var
  I : integer;
begin
  Result := '';
  for I := 0 to high(A) do begin
    with A[I] do
      case VType of
        vtObject:
          if VObject <> nil then
            if TExtObject(VObject).JSCommand = '' then
              Result := Result + TExtObject(VObject).JSName
            else begin
              Result := Result + WriteFunction(TExtObject(VObject).JSCommand);
              TExtThread(CurrentFCGIThread).RemoveJS(TExtObject(VObject).JSCommand);
            end
          else
            if Result = '' then
              Result := 'null'
            else
              continue;
        vtAnsiString: Result := Result + '"' + string(VAnsiString) + '"';
        vtString:     Result := Result + '"' + VString^ + '"';
        vtInteger:    Result := Result + IntToStr(VInteger);
        vtBoolean:    Result := Result + IfThen(VBoolean, 'true', 'false');
        vtExtended:   Result := Result + FloatToStr(VExtended^);
        vtVariant:    Result := Result + string(VVariant^)
      end;
    if I < high(A) then Result := Result + ',';
  end;
  if (Result <> '') and (Result[length(Result)] = ',') then delete(Result, length(Result), 1);
end;

function TExtObject.VarToJSON(Exts : TExtObjectList): string; begin
  if Exts.ClassName = 'TExtObjectList' then
    Result := Exts.JSName
  else
    Result := TExtObject(Exts).JSName
end;

function TExtObject.VarToJSON(Strs : TArrayOfString): string;
var
  I : integer;
begin
  Result := '[';
  for I := 0 to high(Strs) do begin
    Result := Result + '"' + Strs[I] + '"';
    if I < high(Strs) then Result := Result + ',';
  end;
  Result := Result + ']'
end;

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
