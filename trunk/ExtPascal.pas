unit ExtPascal;

interface

uses
  FCGIApp;

const
  ExtPath = '/ext';

type
  TExtThread = class(TFCGIThread)
  private
    Style, Libraries, FLanguage : string;
    Sequence : cardinal;
    FIsAjax  : boolean;
    procedure RelocateVar(JS, JSName, Owner: string; I: integer);
    procedure RemoveJS(JS : string);
    function GetStyle: string;
  protected
    function BeforeHandleRequest : boolean; override;
    procedure AfterHandleRequest; override;
    procedure OnError(Msg, Method, Params : string); override;
    function GetSequence : string;
    function JSConcat(OldCommand, NewCommand : string) : string;
  public
    Theme : string;
    property Language : string read FLanguage;
    property IsAjax : boolean read FIsAjax;
    procedure JSCode(JS : string; JSName : string = ''; Owner : string = '');
    procedure SetStyle(pStyle : string = '');
    procedure SetLibrary(pLibrary : string = '');
    procedure ErrorMessage(Msg : string; Handler: string = '');
  end;

  TArrayOfString  = array of string;
  TArrayOfInteger = array of Integer;

  TExtObjectList = class;
  TExtFunction = class;
  TExtAjaxMethod = procedure of object;

  TExtObject = class
  private
    FJSName : string;
  protected
    JSCommand : string;
    function VarToJSON(A : array of const) : string; overload;
    function VarToJSON(Exts : TExtObjectList) : string; overload;
    function VarToJSON(Strs : TArrayOfString) : string; overload;
    function VarToJSON(Ints : TArrayOfInteger) : string; overload;
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
    function JSFunction(Body: string): TExtFunction; overload;
    procedure JSCode(JS : string; pJSName : string = ''; Owner : string = '');
    function Ajax(Method : TExtAjaxMethod; Params: string = '') : TExtFunction;
    property JSName : string read FJSName;
  end;

  TExtFunction = class(TExtObject);// end;

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

{$IFNDEF FPDOC}
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
  TClass = class(TExtObject);
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

procedure TExtThread.RemoveJS(JS: string);
var
  I : integer;
begin
  I := pos(JS, Response);
  if I <> 0 then delete(Response, I, length(JS))
end;

procedure TExtThread.SetLibrary(pLibrary: string); begin
  if pLibrary = '' then
    Libraries := ''
  else
    Libraries := Libraries + '<script src=' + pLibrary + '></script>';
end;

procedure TExtThread.SetStyle(pStyle: string); begin
  if pStyle = '' then
    Style := ''
  else
    Style := Style + pStyle
end;

function TExtThread.GetStyle : string; begin
  if Style = '' then
    Result := ''
  else
    Result := '<style>' + Style + '</style>';
end;

procedure TExtThread.ErrorMessage(Msg : string; Handler : string = ''); begin
  JSCode('Ext.Msg.show({title:"Error",msg:"' + Msg + '",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK' +
    IfThen(Handler = '', '', ',fn:function(){' + Handler + '}') + '});');
end;

procedure TExtThread.OnError(Msg, Method, Params : string); begin
  if IsAjax and (pos('Access violation', Msg) <> 0) then
    Msg := Msg + '<br/><b>Reloading this page (F5) perhaps fix this error.</b>';
  ErrorMessage(Msg + '<br/><hr/>Method: ' + Method + IfThen(Params = '', '', '<br/>Params: ' + Params));
end;

// Self-translating
procedure TExtThread.JSCode(JS : string; JSName : string = ''; Owner : string = '');
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
  if (JSName <> '') and (pos('O_', JS) <> 0) and (pos('O_', JSName) <> 0) then
    RelocateVar(JS, JSName, Owner, I+length(JS));
end;

procedure TExtThread.RelocateVar(JS, JSName, Owner : string; I : integer);
var
  VarName, VarBody : string;
  J, K : integer;
begin
  J := LastDelimiter(':,', JS);
  if J <> 0 then
    VarName := copy(JS, J+1, posex('_', JS, J+3)-J)
  else
    exit;
  J := posex('/*' + VarName + '*/', Response, I);
  if J > I then begin
    K := pos('var ' + VarName + '=new', Response);
    J := posex(';', Response, J);
    VarBody := copy(Response, K, J-K+1+length(VarName));
    delete(Response, K, length(VarBody));
    if JSName[1] in ['0'..'9'] then JSName := Owner;
    insert(VarBody, Response, pos('var ' + JSName + '=new', Response));
  end;
end;

function TExtThread.JSConcat(OldCommand, NewCommand: string): string;
var
  I , J : integer;
begin
  J := pos(OldCommand, Response);
  I := pos('.', NewCommand);
  if (I <> 0) and (J <> 0) then begin
    NewCommand := copy(NewCommand, I, length(NewCommand));
    Result := copy(OldCommand, 1, length(OldCommand)-1) + NewCommand;
    delete(Response, J + length(OldCommand)-1, 1);
    insert(NewCommand, Response, J + length(OldCommand))
  end
  else
    Result := OldCommand;
end;

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

// Extract Comments, set: HTML body, title, charset, ExtJS CSS, ExtJS libraries, ExtJS theme, ExtJS language, user styles, ExtJS invoke and Ajax response
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
      '<meta http-equiv="Content-Type" content="charset=utf-8">' +
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
    Obj.JSCode(Obj.JSName, JSName, Owner.JSName)
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

procedure TExtObject.CreateVarAlt(JS: string); begin // Alternate create constructor, ExtJS fault
  CreateJSName;
  insert('/*' + JSName + '*/', JS, length(JS)-IfThen(pos('});', JS) <> 0, 2, 1));
  JSCode('var ' + JSName + '= ' + JS)
end;

constructor TExtObject.Create(Owner : TExtObject = nil); begin
  if Owner = nil then CreateVar(JSClassName + '({});')
end;

function TExtObject.JSClassName: string; begin
  Result := 'Object'
end;

procedure TExtObject.JSCode(JS : string; pJSName : string = ''; Owner : string = ''); begin
  if JS <> '' then begin
    if (JS[length(JS)] = ';') and (pos('var ', JS) <> 1) then begin
      if (JSCommand <> '') and (pJSName <> '') and (pJSName <> Classname) then begin
        JSCommand := TExtThread(CurrentFCGIThread).JSConcat(JSCommand, JS);
        exit;
      end;
      JSCommand := '/*' + TExtThread(CurrentFCGIThread).GetSequence + '*/' + JS;
      JS := JSCommand
    end
    else
      JSCommand := '';
    if pJSName = '' then pJSName := JSName;
    TExtThread(CurrentFCGIThread).JSCode(JS, pJSName, Owner);
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

function TExtObject.Ajax(Method : TExtAjaxMethod; Params : string = ''): TExtFunction;
var
  MetName : string;
begin
  Result  := TExtFunction(Self);
  MetName := TExtThread(CurrentFCGIThread).MethodName(@Method);
  if MetName <> '' then begin
    if Params <> '' then
      if pos('&', Params) <> 0 then
        Params := '&' + TExtThread.URLEncode(Params)
      else
        Params := '&' + TExtThread.URLEncode(AnsiReplaceStr(Params, ',', '&'));
    JSCode('Ext.Ajax.request({url:"' + TExtThread(CurrentFCGIThread).RequestHeader['SCRIPT_NAME'] + '/' + MetName +
      '",params:"Ajax=1' + Params + '",success:AjaxSuccess,failure:AjaxFailure});');
  end
  else
    JSCode('Ext.Msg.show({title:"Error",msg:"Ajax method not published.",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});');
end;

function WriteFunction(Command : string) : string;
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
  Result := 'function(' + Params + '){return ' + Command + '}';
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
