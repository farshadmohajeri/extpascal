unit ExtPascal;

interface

uses
  Classes, FCGIApp;

type
  TExtThread = class(TFCGIThread)
  public
    procedure AddJSON(S : string);
  end;

  ArrayOfString  = array of string;
  ArrayOfInteger = array of Integer;

  ExtObject = class;
  ArrayOfExtObject = array of ExtObject;
  TExtObjectClass = class of ExtObject;

  ExtObject = class
  protected
    procedure AddJSON(S : string);
    function VarToJSON(A : array of const) : string; overload;
    function VarToJSON(Exts : ArrayOfExtObject) : string; overload;
    function VarToJSON(Strs : ArrayOfString) : string; overload;
    function VarToJSON(Ints : ArrayOfInteger) : string; overload;
    function ToJSON : string;
    procedure SetLength(var A : ArrayOfExtObject; ExtObjectClass : TExtObjectClass; NewLength : Integer);
    function IfOtherClass(B : Boolean; DefaultClass, OtherClass : TExtObjectClass) : TExtObjectClass;
  public
    procedure JSVar(V : string; Value : string);
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

const
  NoCreate = pointer(1);

procedure SetLength(var Arr; NewLength : Integer; ExtObjectClass: TExtObjectClass = nil);
function Extract(Delims : array of string; S : string; var Matches : TStringList) : boolean; // Mimics preg_match php function
function Explode(Delim : char; S : string) : TStringList; // Mimics explode php function

implementation

uses
  SysUtils, StrUtils;

{ TExtThread }

procedure TExtThread.AddJSON(S : string); begin
  Response := Response + S
end;

{ ExtObject }

procedure ExtObject.AddJSON(S : string); begin
  TExtThread(CurrentFCGIThread).AddJSON(S);
end;

constructor ExtObject.Create(pJSON : string); begin
  AddJSON(pJSON)
end;

function ExtObject.IfOtherClass(B: Boolean; DefaultClass, OtherClass : TExtObjectClass): TExtObjectClass; begin
  if B then
    Result := DefaultClass
  else
    Result := OtherClass
end;

constructor ExtObject.JSFunction(Params, Body: string); begin
  AddJSON('function (' + Params + '){' + Body + '}')
end;

procedure ExtObject.JSVar(V: string; Value : string); begin
  AddJSON('var ' + V + '=' + Value);
end;

procedure ExtObject.SetLength(var A: ArrayOfExtObject; ExtObjectClass: TExtObjectClass; NewLength : Integer);
var
  I, OldLen : integer;
begin
  OldLen := high(A) + 1;
  if ExtObjectClass <> NoCreate then for I := NewLength to high(A) do A[I].Free;
  System.SetLength(A, NewLength);
  if ExtObjectClass <> NoCreate then for I := OldLen to high(A) do A[I] := ExtObjectClass.Create;
end;

procedure SetLength(var Arr; NewLength : Integer; ExtObjectClass: TExtObjectClass = nil);
var
  A : ArrayOfExtObject absolute Arr;
  I, OldLen : integer;
begin
  OldLen := high(A) + 1;
  if ExtObjectClass <> nil then for I := NewLength to high(A) do A[I].Free;
  System.SetLength(A, NewLength);
  if ExtObjectClass <> nil then for I := OldLen to high(A) do A[I] := ExtObjectClass.Create;
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
