unit ExtPascal;

interface

uses
  Classes, FCGIApp;

type
  TExtThread = class(TFCGIThread)
  private
    Style, FLanguage : string;
    Sequence : cardinal;
    FIsAjax : boolean;
    procedure RelocateVar(JS, JSName, Owner: string; I: integer);
    procedure RemoveJS(JS : string);
    function GetStyle: string;
  protected
    procedure BeforeHandleRequest; override;
    procedure AfterHandleRequest; override;
    procedure OnError(Msg, Method, Params : string); override;
    function GetSequence : string;
  public
    LongJSNames : boolean;
    Theme : string;
    ExtPath : string;
    property Language : string read FLanguage;
    property IsAjax : boolean read FIsAjax;
    procedure AddJS(JS : string; JSName : string = ''; Owner : string = '');
    procedure SetStyle(pStyle : string = '');
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
    procedure CreateVar(JS : string);
    function VarToJSON(A : array of const) : string; overload;
    function VarToJSON(Exts : ExtObjectList) : string; overload;
    function VarToJSON(Strs : ArrayOfString) : string; overload;
    function VarToJSON(Ints : ArrayOfInteger) : string; overload;
    procedure CreateJSName;
  protected
    procedure InitDefaults; virtual;
  public
    constructor Create(Owner : ExtObject = nil);
    constructor CreateSingleton(pJSName : string);
    constructor AddTo(List : ExtObjectList);
    function JSArray(JSON : string) : ExtObject;
    function JSObject(JSON : string) : ExtObject;
    function JSFunction(Params, Body : string) : ExtFunction; overload;
    procedure JSFunction(Name, Params, Body : string); overload;
    function Ajax(Method : string; Params: string = '') : ExtFunction;
    procedure AddJS(JS : string; pJSName : string = ''; Owner : string = '');
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
  J := LastDelimiter(':,', JS);
  if J <> 0 then
    VarName := copy(JS, J+1, length(JS))
  else
    VarName := JS;
  J := pos('/*' + VarName + '*/', Response);
  if J > I then begin
    K := pos('var ' + VarName + '=new', Response);
    VarBody := copy(Response, K, J-K+7+length(VarName));
    delete(Response, K, J-K+7+length(VarName));
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

procedure TExtThread.SetStyle(pStyle: string); begin
  if pStyle = '' then
    Style := ''
  else
    Style := Style + ' ' + pStyle
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
  AddJS('Ext.Msg.show({title:"Error",msg:"' + Msg + '<br/><hr/>Method: ' + Method +
    IfThen(Params = '', '', '<br/>Params: ' + Params) + '",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});');
end;

// Self-translating
procedure TExtThread.AddJS(JS : string; JSName : string = ''; Owner : string = '');
var
  I : integer;
begin
  if JS[length(JS)] = ';' then // Command
    I := pos('/**/', Response)
  else begin // set attribute
    I := pos('/*' + JSName + '*/', Response);
    if I = 0 then begin
      if IsAjax then JS := JSName + '.' + AnsiReplaceText(JS, ':', '=') + ';';
      I := pos('/**/', Response);
    end;
    if (I > 1) and not(Response[I-1] in ['{', '[', '(', ';']) then JS := ',' + JS;
  end;
  insert(JS, Response, I);
  if JSName <> '' then RelocateVar(JS, JSName, Owner, I);
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
      if not FileExists(RequestHeader['DOCUMENT_ROOT'] + '/ext-2.1/source/locale/ext-lang-' + FLanguage + '.js') then
        FLanguage := copy(FLanguage, 1, 2)
    end;
  end;
  FIsAjax  := Query['Ajax'] = '1';
  Response := '/**/'
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
    Response := '<!DOCTYPE html><html><title>' + Application.Title + '</title>' +
      '<meta http-equiv="Content-Type" content="charset=utf-8">' +
      '<link rel=stylesheet href=/ext-2.1/resources/css/ext-all.css />' +
      '<script src=/ext-2.1/adapter/ext/ext-base.js></script>' +
      '<script src=/ext-2.1/ext-all.js></script>' +
      IfThen(Theme = '', '', '<link rel=stylesheet href=/ext-2.1/resources/css/xtheme-' + Theme + '.css />') +
      IfThen(FLanguage = 'en', '', '<script src=/ext-2.1/source/locale/ext-lang-' + FLanguage + '.js></script>') +
      GetStyle +
      '<script>Ext.onReady(function(){' +
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
  if length(FObjects) = 0 then Owner.AddJS(Attribute + ':[/*' + Mark + '*/]', Owner.JSName);
  SetLength(FObjects, length(FObjects) + 1);
  FObjects[high(FObjects)] := Obj;
  if pos(Obj.JSName, TExtThread(CurrentFCGIThread).Response) = 0 then begin
    if TExtThread(CurrentFCGIThread).IsAjax then
      ListAdd := 'var ' + Obj.JSName + '=' + Owner.JSName + '.add(%s);'
    else
      ListAdd := '%s';
    if Obj.Classname = 'ExtTabPanel' then // Generalize it if necessary
      ListAdd := Format(ListAdd, ['new Ext.TabPanel({/*' + Obj.JSName + '*/})'])
    else
    if Obj.Classname = 'ExtGridPropertyGrid' then // Generalize it if necessary
      ListAdd := Format(ListAdd, ['new Ext.grid.PropertyGrid({/*' + Obj.JSName + '*/})'])
    else
      ListAdd := Format(ListAdd, ['{/*' + Obj.JSName + '*/}']);
    Obj.AddJS(ListAdd, Mark);
  end
  else
    Obj.AddJS(Obj.JSName, Mark, Owner.JSName)
end;

function ExtObjectList.GetFObjects(I: integer): ExtObject; begin
  Result := FObjects[I]
end;

function ExtObjectList.Count: integer; begin
  Result := length(FObjects)
end;

{ ExtObject }

procedure ExtObject.CreateJSName; begin
  with TExtThread(CurrentFCGIThread) do
    if LongJSNames then
      FJSName := Self.ClassName + GetSequence
    else
      FJSName := 'O' + GetSequence;
end;

constructor ExtObject.CreateSingleton(pJSName : string); begin
  InitDefaults;
  FJSName := pJSName;
end;

procedure ExtObject.CreateVar(JS : string); begin
  CreateJSName;
  if pos('{}', JS) <> 0 then
    JS := AnsiReplaceStr(JS, '{}', '{/*' + JSName + '*/}')
  else
    JS := AnsiReplaceStr(JS, '()', '(/*' + JSName + '*/)');
  AddJS('var ' + JSName + '=new ' + JS);
end;

constructor ExtObject.Create(Owner : ExtObject = nil); begin
  if Owner = nil then CreateVar('Object({});')
end;

procedure ExtObject.AddJS(JS : string; pJSName : string = ''; Owner : string = ''); begin
  if JS <> '' then begin
    if (JS[length(JS)] = ';') and (pos('var ', JS) <> 1) then begin
      JSCommand := '/*' + TExtThread(CurrentFCGIThread).GetSequence + '*/' + JS;
      JS := JSCommand
    end
    else
      JSCommand := '';
    if pJSName = '' then pJSName := JSName;
    TExtThread(CurrentFCGIThread).AddJS(JS, pJSName, Owner);
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

function ExtObject.JSArray(JSON: string): ExtObject; begin
  Result := ExtObject.Create(Self);
  Result.FJSName := '[' + JSON + ']';
end;

function ExtObject.JSObject(JSON: string): ExtObject; begin
  Result := ExtObject.Create(Self);
  Result.FJSName := '{' + JSON + '}';
end;

function ExtObject.JSFunction(Params, Body : string) : ExtFunction; begin
  Result := ExtFunction.Create(Self);
  Result.FJSName := 'function(' + Params + '){' + Body + '}';
end;

procedure ExtObject.JSFunction(Name, Params, Body : string); begin
  AddJS('function ' + Name + '(' + Params + '){' + Body + '};');
end;

function ExtObject.Ajax(Method : string; Params: string = ''): ExtFunction; begin
  Result := ExtFunction(Self);
  if Params <> '' then
    if pos('&', Params) <> 0 then
      Params := '&' + TExtThread.URLEncode(Params)
    else
      Params := '&' + TExtThread.URLEncode(AnsiReplaceText(Params, ',', '&'));
  AddJS('Ext.Ajax.request({url:"' + TExtThread(CurrentFCGIThread).RequestHeader['SCRIPT_NAME'] + '/' + Method +
    '",params:"Ajax=1' + Params + '",success:AjaxSuccess,failure:AjaxFailure});');
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
              Result := Result + 'function(){' + ExtObject(VObject).JSCommand + '}';
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
