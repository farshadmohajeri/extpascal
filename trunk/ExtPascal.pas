unit ExtPascal;

interface

uses
  Classes, FCGIApp;

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
    procedure BeforeHandleRequest; override;
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
  end;

  ArrayOfString  = array of string;
  ArrayOfInteger = array of Integer;

  ExtObjectList = class;
  ExtFunction = class;

  ExtObject = class
  private
    FJSName : string;
  protected
    JSCommand : string;
    function VarToJSON(A : array of const) : string; overload;
    function VarToJSON(Exts : ExtObjectList) : string; overload;
    function VarToJSON(Strs : ArrayOfString) : string; overload;
    function VarToJSON(Ints : ArrayOfInteger) : string; overload;
    procedure CreateVar(JS : string);
    procedure CreateJSName;
  protected
    procedure InitDefaults; virtual;
  public
    constructor Create(Owner : ExtObject = nil);
    constructor CreateSingleton(pJSName : string = '');
    constructor AddTo(List : ExtObjectList);
    function JSClassName : string; virtual;
    function JSArray(JSON : string) : ExtObjectList;
    function JSObject(JSON : string; ObjectConstructor : string = '') : ExtObject;
    function JSFunction(Params, Body : string) : ExtFunction; overload;
    procedure JSFunction(Name, Params, Body : string); overload;
    function JSFunction(Body: string): ExtFunction; overload;
    procedure JSCode(JS : string; pJSName : string = ''; Owner : string = '');
    function Ajax(Method : string; Params: string = '') : ExtFunction;
    property JSName : string read FJSName;
  end;

  ExtObjectList = class
  private
    FObjects : array of ExtObject;
    Attribute, Mark : string;
    Owner : ExtObject;
    function GetFObjects(I: integer): ExtObject;
  public
    property Objects[I : integer] : ExtObject read GetFObjects; default;
    constructor CreateSingleton(JSName: string);
    constructor Create(pOwner : ExtObject; pAttribute : string);
    destructor Destroy; override;
    procedure Add(Obj : ExtObject);
    function Count : integer;
  end;

  ExtFunction = class(ExtObject) end;
  HTMLElement = class(ExtObject) end;
  StyleSheet = class(ExtObject) end;
  RegExp = class(ExtObject) end;
  CSSRule = class(ExtObject) end;
  XMLDocument = class(ExtObject) end;
  NodeList = class(ExtObject) end;
  Region = type string;
  NativeMenu = ExtObject;
  el = type string; // doc fault
  Event = class(ExtObject) end;
  EventObject = Event;
  HTMLNode = ExtObject;
  _Constructor = class(ExtObject) end;
  _Class = class(ExtObject) end;
  ExtLibRegion = Region; //doc fault
  visMode = Integer; // doc fault
  The = ExtObject; // doc fault
  This = ExtObject; // doc fault
  airNativeMenu = ExtObject;
  X = ExtObject; // doc fault
  N1 = ExtObject; // doc fault
  N2 = ExtObject; // doc fault
  Layout = ExtObject; // Poor hierarchy definition
  Id = ExtObject;// doc fault
  iPageX = ExtObject; // doc fault
  iPageY = ExtObject; // doc fault
  ExtGridGrid = ExtObject; // doc fault
  TreeSelectionModel = ExtObject; // doc fault
  SelectionModel = ExtObject; // doc fault
  DataSource = ExtObject; // doc fault

function Extract(Delims : array of string; S : string; var Matches : TStringList) : boolean; // Mimics preg_match php function
function Explode(Delim : char; S : string) : TStringList; // Mimics explode php function

implementation

uses
  SysUtils, StrUtils;

{ TExtThread }

procedure TExtThread.RelocateVar(JS, JSName, Owner : string; I : integer);
var
  VarName, VarBody : string;
  J, K : integer;
begin
  if pos('O_', JS) = 0 then exit;
  J := LastDelimiter(':,', JS);
  if J <> 0 then
    VarName := copy(JS, J+1, posex('_', JS, J+3)-J)
  else
    exit;
  J := posex('/*' + VarName + '*/', Response, I);
  if J > I then begin
    K := pos('var ' + VarName + '=new', Response);
    VarBody := copy(Response, K, J-K+7+length(VarName));
    delete(Response, K, length(VarBody));
    if JSName[1] in ['0'..'9'] then JSName := Owner;
    insert(VarBody, Response, pos('var ' + JSName + '=new', Response));
  end;
end;

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

procedure TExtThread.OnError(Msg, Method, Params : string); begin
  if IsAjax and (pos('Access violation', Msg) <> 0) then
    Msg := Msg + '<br/><b>Reloading this page (F5) perhaps fix this error.</b>';
  JSCode('Ext.Msg.show({title:"Error",msg:"' + Msg + '<br/><hr/>Method: ' + Method +
    IfThen(Params = '', '', '<br/>Params: ' + Params) + '",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});');
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
      if IsAjax then JS := JSName + '.' + AnsiReplaceText(JS, ':', '=') + ';';
      I := length(Response) + 1;
    end;
    if (I > 1) and not(Response[I-1] in ['{', '[', '(', ';']) then JS := ',' + JS;
  end;
  insert(JS, Response, I);
  if JSName <> '' then RelocateVar(JS, JSName, Owner, I+length(JS));
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

procedure TExtThread.BeforeHandleRequest;
var
  I : integer;
begin
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
  end;
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

constructor ExtObjectList.Create(pOwner : ExtObject; pAttribute : string); begin
  Attribute := pAttribute;
  Owner     := pOwner;
  Mark      := TExtThread(CurrentFCGIThread).GetSequence;
end;

constructor ExtObjectList.CreateSingleton(JSName : string); begin
  Attribute := JSName;
end;

destructor ExtObjectList.Destroy;
var
  I : integer;
begin
  for I := 0 to length(FObjects)-1 do FObjects[I].Free;
  inherited;
end;

procedure ExtObjectList.Add(Obj : ExtObject);
var
  ListAdd : string;
begin
  if length(FObjects) = 0 then Owner.JSCode(Attribute + ':[/*' + Mark + '*/]', Owner.JSName);
  SetLength(FObjects, length(FObjects) + 1);
  FObjects[high(FObjects)] := Obj;
  if pos(Obj.JSName, TExtThread(CurrentFCGIThread).Response) = 0 then begin
    if TExtThread(CurrentFCGIThread).IsAjax then
      ListAdd := 'var ' + Obj.JSName + '=' + Owner.JSName + '.add(%s);'
    else
      ListAdd := '%s';
    if pos(Obj.Classname + '.', 'ExtTabPanel.ExtGridPropertyGrid.') <> 0 then // Generalize it if necessary
      ListAdd := Format(ListAdd, ['new ' + Obj.JSClassName + '({/*' + Obj.JSName + '*/})'])
    else
      ListAdd := Format(ListAdd, ['{/*' + Obj.JSName + '*/}']);
    Obj.JSCode(ListAdd, Mark);
  end
  else
    Obj.JSCode(Obj.JSName, Mark, Owner.JSName)
end;

function ExtObjectList.GetFObjects(I: integer): ExtObject; begin
  Result := FObjects[I]
end;

function ExtObjectList.Count: integer; begin
  Result := length(FObjects)
end;

{ ExtObject }

procedure ExtObject.CreateJSName; begin
  FJSName := 'O_' + TExtThread(CurrentFCGIThread).GetSequence + '_';
end;

constructor ExtObject.CreateSingleton(pJSName : string = ''); begin
  InitDefaults;
  if pJSName = '' then
    FJSName := JSClassName
  else
    FJSName := pJSName;
end;

procedure ExtObject.CreateVar(JS : string); begin
  CreateJSName;
  if pos('{}', JS) <> 0 then
    JS := AnsiReplaceStr(JS, '{}', '{/*' + JSName + '*/}')
  else
    JS := AnsiReplaceStr(JS, '()', '(/*' + JSName + '*/)');
  JSCode('var ' + JSName + '=new ' + JS);
end;

constructor ExtObject.Create(Owner : ExtObject = nil); begin
  if Owner = nil then CreateVar(JSClassName + '({});')
end;

function ExtObject.JSClassName: string; begin
  Result := 'Object'
end;

procedure ExtObject.JSCode(JS : string; pJSName : string = ''; Owner : string = ''); begin
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

constructor ExtObject.AddTo(List : ExtObjectList); begin
  if JSName = '' then begin
    InitDefaults;
    CreateJSName;
  end;
  List.Add(Self);
end;

procedure ExtObject.InitDefaults; begin end;

function ExtObject.JSArray(JSON: string): ExtObjectList; begin
  Result := ExtObjectList(ExtObject.Create(Self));
  ExtObject(Result).FJSName := '[' + JSON + ']';
end;

function ExtObject.JSObject(JSON : string; ObjectConstructor : string = ''): ExtObject; begin
  Result := ExtObject.Create(Self);
  if ObjectConstructor = '' then
    Result.FJSName := '{' + JSON + '}'
  else
    Result.FJSName := 'new ' + ObjectConstructor + '({' + JSON + '})'
end;

function ExtObject.JSFunction(Params, Body : string) : ExtFunction; begin
  Result := ExtFunction.Create(Self);
  Result.FJSName := 'function(' + Params + '){' + Body + '}';
end;

function ExtObject.JSFunction(Body : string) : ExtFunction; begin
  Result := JSFunction('', Body);
end;

procedure ExtObject.JSFunction(Name, Params, Body : string); begin
  JSCode('function ' + Name + '(' + Params + '){' + Body + '};');
end;

function ExtObject.Ajax(Method : string; Params: string = ''): ExtFunction; begin
  Result := ExtFunction(Self);
  if Params <> '' then
    if pos('&', Params) <> 0 then
      Params := '&' + TExtThread.URLEncode(Params)
    else
      Params := '&' + TExtThread.URLEncode(AnsiReplaceText(Params, ',', '&'));
  JSCode('Ext.Ajax.request({url:"' + TExtThread(CurrentFCGIThread).RequestHeader['SCRIPT_NAME'] + '/' + Method +
    '",params:"Ajax=1' + Params + '",success:AjaxSuccess,failure:AjaxFailure});');
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

function ExtObject.VarToJSON(A : array of const): string;
var
  I : integer;
begin
  Result := '';
  for I := 0 to high(A) do begin
    with A[I] do
      case VType of
        vtInteger: Result := Result + IntToStr(VInteger);
        vtAnsiString:
          if string(VAnsiString) <> '' then
            Result := Result + '"' + string(VAnsiString) + '"'
          else
            continue;
        vtObject:
          if VObject <> nil then
            if ExtObject(VObject).JSCommand = '' then
              Result := Result + ExtObject(VObject).JSName
            else begin
              Result := Result + WriteFunction(ExtObject(VObject).JSCommand);
              TExtThread(CurrentFCGIThread).RemoveJS(ExtObject(VObject).JSCommand);
            end
          else
            if Result = '' then
              Result := 'null'
            else
              continue;
        vtBoolean: Result := Result + IfThen(VBoolean, 'true', 'false');
        vtString:
          if VString^ <> '' then
            Result := Result + '"' + VString^ + '"'
          else
            continue;
        vtExtended: Result := Result + FloatToStr(VExtended^);
        vtVariant:
          if string(VVariant^) <> '' then
            Result := Result + string(VVariant^)
          else
            continue
      end;
    if I < high(A) then Result := Result + ',';
  end;
  if (Result <> '') and (Result[length(Result)] = ',') then delete(Result, length(Result), 1);
end;

function ExtObject.VarToJSON(Exts : ExtObjectList): string;
var
  I : integer;
begin
  Result := '[';
  for I := 0 to Exts.Count-1 do begin
    Result := Result + Exts[I].JSName;
    if I < (Exts.Count-1) then Result := Result + ',';
  end;
  Result := Result + ']';
end;

function ExtObject.VarToJSON(Strs : ArrayOfString): string;
var
  I : integer;
begin
  Result := '';
  for I := 0 to high(Strs) do begin
    Result := Result + '"' + Strs[I] + '"';
    if I < high(Strs) then Result := Result + ',';
  end;
end;

function ExtObject.VarToJSON(Ints : ArrayOfInteger): string;
var
  I : integer;
begin
  Result := '';
  for I := 0 to high(Ints) do begin
    Result := Result + IntToStr(Ints[I]);
    if I < high(Ints) then Result := Result + ',';
  end;
end;

// Mimics preg_match php function
function Extract(Delims : array of string; S : string; var Matches : TStringList) : boolean;
var
  I, J : integer;
begin
  Result := false;
  if Matches <> nil then Matches.Clear;
  J := 1;
  for I := 0 to high(Delims) do begin
    J := posex(Delims[I], S, J);
    if J = 0 then
      exit
    else
      inc(J, length(Delims[I]));
  end;
  J := 1;
  for I := 0 to high(Delims)-1 do begin
    J := posex(Delims[I], S, J);
    inc(J, length(Delims[I]));
    Matches.Add(trim(copy(S, J, posex(Delims[I+1], S, J)-J)));
  end;
  Result := true
end;

// Mimics explode php function
function Explode(Delim : char; S : string) : TStringList;
var
  I : integer;
begin
  Result := TStringList.Create;
  Result.StrictDelimiter := true;
  Result.Delimiter := Delim;
  Result.DelimitedText := S;
  for I := 0 to Result.Count-1 do Result[I] := trim(Result[I]);
end;

end.
