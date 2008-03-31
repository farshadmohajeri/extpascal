unit ExtPascal;

interface

type
  ArrayOfString  = array of string;
  ArrayOfInteger = array of Integer;

  ExtObject = class;
  ArrayOfExtObject = array of ExtObject;
  TExtObjectClass = class of ExtObject;

  ExtObject = class
  protected
    JSON : string;
    procedure AddJSON(S : string);
    function VarToJSON(A : array of const) : string; overload;
    function VarToJSON(Exts : ArrayOfExtObject) : string; overload;
    function VarToJSON(Strs : ArrayOfString) : string; overload;
    function VarToJSON(Ints : ArrayOfInteger) : string; overload;
    function ToJSON : string;
    procedure SetLength(var A : ArrayOfExtObject; ExtObjectClass : TExtObjectClass; NewLength : Integer);
  public
    procedure WriteBrowser(S : string = '');
    procedure JSVar(V : string; Value : string = '');
    constructor Create(pJSON : string = '');
    constructor JSFunction(Params, Body : string);
  end;

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

implementation

uses
  SysUtils, StrUtils, Variants;

{ ExtObject }

procedure ExtObject.AddJSON(S : string); begin
  JSON := JSON + S;
end;

constructor ExtObject.Create(pJSON: string); begin
  JSON := pJSON
end;

constructor ExtObject.JSFunction(Params, Body: string); begin
  JSON := 'function (' + Params + '){' + Body + '}'
end;

procedure ExtObject.JSVar(V: string; Value : string = ''); begin
  AddJSON('var ' + V + '=' + IfThen(Value = '', JSON, Value));
end;

procedure ExtObject.SetLength(var A: ArrayOfExtObject; ExtObjectClass: TExtObjectClass; NewLength : Integer);
var
  I, OldLen : integer;
begin
  OldLen := high(A) + 1;
  for I := NewLength to high(A) do A[I].Free;
  System.SetLength(A, NewLength);
  for I := OldLen to high(A) do A[I] := ExtObjectClass.Create;
end;

function ExtObject.ToJSON: string; begin
  Result := ''
end;

function ExtObject.VarToJSON(A : array of const): string;
var
  I : integer;
begin
  Result := '';
  for I := 0 to high(A) do begin
    with A[I] do
      case VType of
        vtInteger:    Result := Result + IntToStr(VInteger);
        vtBoolean:    Result := Result + IfThen(VBoolean, 'true', 'false');
        vtExtended:   Result := Result + FloatToStr(VExtended^);
        vtString:     Result := Result + '"' + VString^ + '"';
        vtObject:     Result := Result + IfThen(VObject <> nil, ExtObject(VObject).ToJSON, '{}');
        vtAnsiString: Result := Result + '"' + string(VAnsiString) + '"';
        vtVariant:    Result := Result + string(VVariant^);
      end;
    if I < high(A) then Result := Result + ', ';
  end;
end;

function ExtObject.VarToJSON(Exts : ArrayOfExtObject): string;
var
  I : integer;
begin
  Result := '';
  for I := 0 to high(Exts) do begin
    Result := Result + Exts[I].ToJSON;
    if I < high(Exts) then Result := Result + ', ';
  end;
end;

function ExtObject.VarToJSON(Strs : ArrayOfString): string;
var
  I : integer;
begin
  Result := '';
  for I := 0 to high(Strs) do begin
    Result := Result + '"' + Strs[I] + '"';
    if I < high(Strs) then Result := Result + ', ';
  end;
end;

function ExtObject.VarToJSON(Ints : ArrayOfInteger): string;
var
  I : integer;
begin
  Result := '';
  for I := 0 to high(Ints) do begin
    Result := Result + IntToStr(Ints[I]);
    if I < high(Ints) then Result := Result + ', ';
  end;
end;

procedure ExtObject.WriteBrowser(S: string); begin
  if S <> '' then AddJSON(S);
end;

end.
