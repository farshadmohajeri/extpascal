program ExtToPascal; {$APPTYPE CONSOLE}

uses
  SysUtils, StrUtils, Classes, Math, ExtPascal;

function FixReserved(S : string) : string;
const
  Reserved = '.and.array.as.asm.begin.case.class.const.constructor.destructor.destroy.dispinterface.div.do.downto.else.end.except.exports.'+
    'file.finalization.finally.for.function.goto.if.implementation.in.inherited.initialization.inline.interface.is.label.library.'+
    'mod.nil.not.object.of.or.out.packed.procedure.program.property.raise.record.repeat.resourcestring.set.shl.shr.string.then.'+
    'threadvar.to.try.type.unit.until.uses.var.while.with.xor.';
begin
  if pos('.' + lowercase(S) + '.', Reserved) = 0 then
    Result := S
  else
    Result := '_' + S;
end;

function FixIdent(Ident : string) : string;
var
  I : integer;
begin
  Result := '';
  for I := 1 to length(Ident) do
    if Ident[I] in ['0'..'9', 'A'..'Z', 'a'..'z', '_'] then
      if I = 1 then
        Result := UpCase(Ident[1])
      else
        Result := Result + Ident[I]
    else
      if I < length(Ident) then Ident[I+1] := UpCase(Ident[I+1]);
  Result := FixReserved(Result);
end;

const ArrayOf = 'ArrayOf';

function FixType(Ident : string) : string;
var
  T : string;
  I : integer;
begin
  T := LowerCase(Ident);
  if T = 'string'    then begin Result := 'string';           exit end else
  if T = 'number'    then begin Result := 'Integer';          exit end else
  if T = 'boolean'   then begin Result := 'Boolean';          exit end else
  if T = 'object'    then begin Result := 'ExtObject';        exit end else
  if T = 'date'      then begin Result := 'TDateTime';        exit end else
  if T = 'float'     then begin Result := 'Double';           exit end else
  if T = 'mixed'     then begin Result := 'string';           exit end else
  if T = 'array'     then begin Result := 'ArrayOfExtObject'; exit end else
  if T = 'int'       then begin Result := 'Integer';          exit end else
  if T = 'object...' then begin Result := 'ArrayOfExtObject'; exit end
  else begin
    I := LastDelimiter('/[:', Ident);
    if I <> 0 then begin
      Result := FixType(copy(Ident, 1, I-1)); // alternative types: choose the first option
      if Ident[I] <> '/' then Result := ArrayOf + Result;
      exit;
    end;
  end;
  Result := FixIdent(Ident);
end;

type
  TUnit = class
    Name, UsesList : string;
    Classes : TStringList;
    constructor Create(pName : string);
    function InitUsesList : string;
    procedure ReviewTypes;
  end;

  TClass = class
    Name, JSName, Parent, UnitName : string;
    Defaults, Arrays : boolean;
    Properties, Methods : TStringList;
    constructor Create(pName : string; pParent : string = ''; pUnitName : string = '');
    function InheritLevel : integer;
  end;

  TProp = class
    Name, JSName, Typ, Default : string;
    Static : boolean;
    constructor Create(pName : string; pType : string; pStatic : boolean; pDefault : string = '');
  end;

  TMethod = class
    Name, JSName, Return : string;
    Params : TStringList;
		Static, Overload : boolean;
	  constructor Create(pName, pReturn : string; pParams : TStringList; pStatic, pOverload : boolean); overload;
	  constructor Create(pName, pJSName, pReturn : string; pParams : TStringList; pOverload : boolean); overload;
    function CreateOverloadParams(P : integer; NewType : string) : TStringList;
  end;

  TParam = class
	  Name, Typ : string;
	  Optional : boolean;
    constructor Create(pName, pType : string; pOptional : boolean);
  end;

var
  AllClasses, Units : TStringList;

constructor TUnit.Create(pName : string); begin
  Name    := FixIdent(pName);
  Classes := TStringList.Create;
end;

function TUnit.InitUsesList: string;
var
  I, J : integer;
  UName : string;
begin
  if Name = 'ExtGlobal' then
    Result := ''
  else
    Result := ', ExtGlobal';
  for I := 0 to Classes.Count-1 do
    with TClass(Classes.Objects[I]) do
      if Parent <> '' then begin
        J := AllClasses.IndexOf(Parent);
        if J <> -1 then begin
          UName := TClass(AllClasses.Objects[J]).UnitName;
          if (UName <> UnitName) and (pos(', ' + UName + ',', Result + ',') = 0) then
            Result := Result + ', ' + UName
        end;
      end;
end;

procedure TUnit.ReviewTypes;

  procedure InsertNamespace(var Typ : string);
  const
    Types = '.string.Integer.Boolean.Double.TDateTime.ExtObject.ArrayOfExtObject.Void._Function.Event.HTMLElement.RegExp.';
  var
    T : string;
    I, J : integer;
  begin
    if (Typ <> '') and (pos('.' + Typ + '.', Types) = 0) then begin
      T := Typ;
      if (Name <> 'Ext') and (pos('Ext', T) = 1) and (pos('Ext,', UsesList + ',') = 0) then begin // uses Ext?
        I := AllClasses.IndexOf(T);
        if (I <> -1) and (TClass(AllClasses.Objects[I]).UnitName = 'Ext') then begin
          UsesList := UsesList + ', Ext';
          exit;
        end;
      end;
      J := pos(ArrayOf, T);
      if J = 1 then T := copy(T, length(ArrayOf)+1, length(T));
      if T[1] = '_' then T := copy(T, 2, length(T));
      for I := 0 to Units.Count-1 do
        if AllClasses.IndexOf(Units[I] + T) <> -1 then begin
          Typ := Units[I] + T;
          if J = 1 then Typ := ArrayOf + Typ;
          if (Name = 'Ext') and (Units[I] = 'ExtLayout') then exit; // Exception to resolve circular reference
          if (Units[I] <> Name) and (pos(Units[I] + ',', UsesList + ',') = 0) then UsesList := UsesList + ', ' + Units[I];
          exit;
        end;
    end;
  end;

var
  I, J, K : integer;
begin
  writeln(Name);
  UsesList := InitUsesList;
  for I := 0 to Classes.Count-1 do
    with TClass(Classes.Objects[I]) do begin
      for J := 0 to Properties.Count-1 do
        InsertNamespace(TProp(Properties.Objects[J]).Typ);
      for J := 0 to Methods.Count-1 do
        with TMethod(Methods.Objects[J]) do begin
          InsertNamespace(Return);
          for K := 0 to Params.Count-1 do
            InsertNamespace(TParam(Params.Objects[K]).Typ);
        end;
    end;
end;

constructor TClass.Create(pName, pParent, pUnitName : string); begin
  Name       := FixIdent(pName);
  JSName     := pName;
  Parent     := FixIdent(pParent);
  UnitName   := FixIdent(pUnitName);
  Properties := TStringList.Create;
  Methods    := TStringList.Create;
end;

function TClass.InheritLevel : integer;
var
  P : string;
begin
  Result := 0;
  P := Parent;
  while P <> '' do begin
    inc(Result);
    P := TClass(AllClasses.Objects[AllClasses.IndexOf(P)]).Parent;
  end;
end;

constructor TProp.Create(pName : string; pType : string; pStatic : boolean; pDefault : string = ''); begin
  Name    := FixIdent(pName);
  JSName  := pName;
  Static  := pStatic;
  Typ     := FixType(pType);
  Default := pDefault;
end;

constructor TParam.Create(pName, pType : string; pOptional : boolean); begin
  Name     := FixIdent(pName);
  Typ      := pType;
  Optional := pOptional;
end;

constructor TMethod.Create(pName, pReturn : string; pParams : TStringList; pStatic, pOverload : boolean); begin
  Name     := FixIdent(pName);
  JSName   := pName;
  Return   := pReturn;
  Params   := pParams;
  Static   := pStatic;
  Overload := pOverload;
end;

constructor TMethod.Create(pName, pJSName, pReturn : string; pParams : TStringList; pOverload : boolean);
var
  I : integer;
begin
  Name     := pName;
  JSName   := pJSName;
  Return   := pReturn;
  Params   := pParams;
  Overload := pOverload;
  I := LastDelimiter('.', Name);
  if I <> 0 then Name := copy(Name, I+1, length(Name));
  Static := I <> 0;
end;

function TMethod.CreateOverloadParams(P : integer; NewType : string): TStringList;
var
  I, J : integer;
begin
  Result := TStringList.Create;
  for I := 0 to Params.Count-1 do
    with TParam(Params.Objects[I]) do begin
      Result.AddObject(Name, TParam.Create(Name, IfThen(I = P, FixType(NewType), Typ), Optional));
      if (I = P) and Optional then // Remove unnecessary optional params
        for J := P downto 0 do TParam(Result.Objects[J]).Optional := false;
    end;
end;

procedure DoOverloads(Cls : TClass; Method : TMethod);
var
  Types : TStringList;
  I, J : integer;
begin
  Cls.Methods.AddObject(Method.Name, Method);
  with Method do begin
    for I := 0 to Params.Count-1 do
      with TParam(Params.Objects[I]) do
        if pos('/', Typ) <> 0 then begin
          Overload := true;
          Types := Explode('/', Typ);
          Typ   := FixType(Types[0]);
          for J := 1 to Types.Count-1 do
            DoOverloads(Cls, TMethod.Create(Method.JSName, Return, CreateOverloadParams(I, Types[J]), Static, true));
          Types.Free;
        end
        else
          Typ := FixType(Typ);
    if pos('/', Return) <> 0 then begin
      Overload := true;
      Types  := Explode('/', Return);
      Return := FixType(Types[0]);
      for J := 1 to Types.Count-1 do
        DoOverloads(Cls, TMethod.Create(Method.JSName, Types[J], Params, Static, true));
      Types.Free;
    end
    else
      Return := FixType(Return);
  end;
end;

procedure SetDefault(Prop : TProp; Def : string; var Match : TStringList);
var
  I : integer;
begin
  Def := AnsiReplaceText(Def, '"', '''');
  if (Def = '') or (Def = '''''') then exit;
  with Prop do begin
    if Default <> '' then exit;
    if (Typ = 'Boolean') and (pos('true', Def) <> 0) then Default := 'true' else
    if (Typ = 'string') and Extract(['''', ''''], Def, Match) then begin
      if Match[0] <> '' then Default := '''' + Match[0] + ''''
    end else
    if Typ = 'Integer' then begin
      for I := 1 to length(Def) do
        if Def[I] in ['0'..'9'] then Default := Default + Def[I];
    end
      else
        if Typ = 'Double' then begin
          for I := 1 to length(Def) do
            if Def[I] in ['0'..'9', '.'] then Default := Default + Def[I];
          if (Default <> '') and (Default[1] = '.') then Default := '0' + Default;
        end
  end;
end;

function Unique(S : string; List : TStringList) : string; begin
  Result := S;
  while List.IndexOf(Result) <> -1 do Result := Result + '_'
end;

var
  State : (Initial, InClass, InProperties, InMethods);

procedure LoadElements(Line : string);
const
  CurClass : TClass  = nil;
  CurProp  : TProp   = nil;
  CurMethod: TMethod = nil;
  PropName : string  = '';
  MetName  : string  = '';
var
  PackName, Arg, Return, JSName : string;
  Matches, Params, Args : TStringList;
  Package : TUnit;
  I : integer;
begin
  Matches := TStringList.Create;
  try
    case State of
      Initial :
        if Extract(['<h1>Class', '</h1>'], Line, Matches) then begin
          CurClass := TClass.Create(Matches[0]);
          State := InClass;
        end;
      InClass :
        if Extract(['Package:', '<td class="hd-info">', '<'], Line, Matches) then begin
          if CurClass.Name = 'Ext' then begin // Pascal requires this exception: move Ext class to Unit class and rename Ext class to _Ext
            PackName := 'Ext';
            CurClass.Name := '_Ext';
          end
          else begin
            PackName := FixIdent(Matches[1]);
            if pos('Ext', PackName) <> 1 then PackName := 'Ext' + PackName;
          end;
          CurClass.UnitName := PackName;
          I := Units.IndexOf(PackName);
          if I = -1 then begin
            Package := TUnit.Create(PackName);
            Units.AddObject(PackName, Package)
          end
          else
            Package := TUnit(Units.Objects[I]);
          Package.Classes.AddObject(PackName, CurClass)
        end
        else
          if Extract(['Extends:', '<a ext:cls="', '"'], Line, Matches) then
            CurClass.Parent := FixIdent(Matches[1])
          else
            if (pos('<h2>Config Options</h2>', Line) + pos('<h2>Public Properties</h2>', Line)) <> 0 then State := InProperties;
      InProperties :
        if Extract(['<b>', '</b>', ':', '<div class="mdesc">'], Line, Matches) then begin
          PropName := Matches[0];
          I := LastDelimiter('.', PropName);
          if I <> 0 then PropName := copy(PropName, I+1, length(PropName)); //Static
          if (PropName <> 'config') and (CurClass.Properties.IndexOf(PropName) = -1)  then begin // config object are deleted, sugar coding non sweet to Pascal
            CurProp := TProp.Create(PropName, Matches[2], I <> 0);
            CurClass.Properties.AddObject(FixIdent(PropName), CurProp);
          end;
        end
        else
          if Extract(['(defaults to', ')'], Line, Matches) or Extract(['(default to', ')'], Line, Matches) then begin
            SetDefault(CurProp, Matches[0], Matches);
            if (CurProp.Default <> '') and not CurClass.Defaults then begin
              CurClass.Defaults := true;
              CurClass.Methods.AddObject('Create', TMethod.Create('Create', '', TStringList.Create, false, false));
            end;
          end
          else
            if (PropName <> 'config') and Extract(['<td class="msource">', '</td>'], Line, Matches) then begin
              if Matches[0][1] = '<' then
                with CurClass.Properties do Delete(IndexOf(FixIdent(PropName))); // delete property inherited
            end
            else
              if pos('<h2>Public Methods</h2>', Line) <> 0 then
                State := InMethods
              else
                if pos('<static>', lowercase(Line)) <> 0 then CurProp.Static := true;
      InMethods :
          if Extract(['<b>', '</b>', ':', '<div class="mdesc">'], Line, Matches) or
             Extract(['<b>', '</b>', ')', '<div class="mdesc">'], Line, Matches) then begin
            JSName  := Matches[0];
            MetName := Unique(FixIdent(JSName), CurClass.Properties);
            MetName := Unique(MetName, CurClass.Methods);
            if pos('Init', MetName) = 1 then exit; // delete init functions, usualy private
            Return := FixType(Matches[2]);
            if Return = '' then begin
              MetName := 'Create';
              if CurClass.Defaults then exit; // already have Create
            end;
            if pos('Instance', Return) > 1 then Return := copy(Return, 1, length(Return) - length('Instance')); // doc fault
            Params := Explode(',', Matches[1]);
            Args   := TStringList.Create;
            for I := 0 to Params.Count-1 do
              if Extract(['<code>', ' ', '</code>'], Params[I], Matches) then begin
                if pos('etc', Matches[1]) = 1 then begin // variable parameter list
                  Matches[0] := ArrayOf + FixType(Matches[0]);
                  Arg := Args[0];
                  Arg[length(Arg)] := 's';
                  Matches[1] := Arg;
                  Args.Clear;
                end
                else
                  if not IsValidIdent(Matches[1]) and (Matches[1][1] = '{') then begin // documentation fault
                    Arg := copy(Matches[1], 2, length(Matches[1])-2);
                    Matches[1] := Matches[0];
                    Matches[0] := Arg;
                  end;
                if Matches[1] <> 'config' then begin// config object are deleted, sugar coding non pratical in Pascal
                  Matches[1] := Unique(FixIdent(Matches[1]), Args);
                  Args.AddObject(Matches[1], TParam.Create(Matches[1], Matches[0], (Return = '') or
                    ((pos('>[', Params[I]) <> 0) and (pos(ArrayOf, Matches[0]) = 0))));
                end;
              end;
            Params.Free;
            CurMethod := TMethod.Create(MetName, JSName, Return, Args, false);
            DoOverloads(CurClass, CurMethod);
          end
          else
            if Extract(['<td class="msource">', '</td>'], Line, Matches) then begin
              if Matches[0][1] = '<' then
                with CurClass.Methods do begin
                  I := IndexOf(MetName);
                  while I <> -1 do begin
                    Delete(I);
                    I := IndexOf(MetName); // delete method inherited and overloads
                  end;
                end;
            end
            else
              if pos('<static>', lowercase(Line)) <> 0 then
                CurMethod.Static := true
              else
                if pos('<h2>Public Events</h2>', Line) <> 0 then begin
                  AllClasses.AddObject(CurClass.Name, CurClass);
                  State := Initial;
                end;
    end;
  finally
    Matches.Free;
  end;
end;

function FixHtml(H : string) : string;
var
  I, J : integer;
begin
  Result := H;
  I := pos('&', Result);
  while I <> 0 do begin
    J := posex(';', Result, I);
    if J = 0 then exit;
    delete(Result, I, J-I+1);
    I := pos('&', Result);
  end;
  I := pos('|', Result);
  if I <> 0 then Result := AnsiReplaceText(Result, '|', '/');
end;

procedure ReadHtml(FileName : string);
var
  Html : text;
  Line : string;
begin
  writeln(FileName);
  assign(Html, FileName);
  reset(Html);
  State := Initial;
  repeat
    readln(Html, Line);
    LoadElements(FixHtml(Line));
  until SeekEOF(Html);
  close(Html);
end;

procedure LoadFixes;
var
  Fixes : text;
  Fix : string;
  Fields, Params : TStringList;
  I, J, K : integer;
  NewClass : TClass;
begin
  if FileExists('ExtFixes.txt') then begin
    assign(Fixes, 'ExtFixes.txt');
    reset(Fixes);
    repeat
      readln(Fixes, Fix);
      Fields := Explode(',', Fix);
      I := AllClasses.IndexOf(Fields[0]);
      if I <> -1 then begin
        with TClass(AllClasses.Objects[I]) do
          if Fields.Count = 5 then begin // props
            J := Properties.IndexOf(Fields[1]);
            if J = -1 then // Add
              Properties.AddObject(Fields[1], TProp.Create(Fields[1], Fields[2], lowercase(Fields[3]) = 'true', Fields[4]))
            else // Update
              with TProp(Properties.Objects[J]) do begin
                Typ := Fields[2];
                Static := lowercase(Fields[3]) = 't';
                Default := Fields[4]
              end;
          end
          else begin // methods
            J := Methods.IndexOf(Fields[1]);
            if J = -1 then begin // Add
              Params := TStringList.Create;
              Methods.AddObject(Fields[1], TMethod.Create(Fields[1], Fields[2], Params, lowercase(Fields[3]) = 'true', lowercase(Fields[4]) = 'true'));
              for K := 0 to ((Fields.Count-4) div 3)-1 do
                Params.AddObject(Fields[K*3+5], TParam.Create(Fields[K*3+5], Fields[K*3+6], lowercase(Fields[K*3+7]) = 'true'))
            end
            else // Update
              with TMethod(Methods.Objects[J]) do begin
                Return := Fields[2];
                Static := lowercase(Fields[3]) = 'true';
                Overload := lowercase(Fields[4]) = 'true';
                for K := 0 to ((Fields.Count-4) div 3)-1 do
                  with TParam(Params.Objects[K]) do begin
                    Typ := Fields[K*3+6];
                    Optional := lowercase(Fields[K*3+7]) = 'true';
                  end;
              end;
          end;
      end
      else begin// Create new Class
        NewClass := TClass.Create(Fields[0], Fields[1], Fields[2]);
        AllClasses.AddObject(Fields[0], NewClass);
        TUnit(Units.Objects[Units.IndexOf(Fields[2])]).Classes.AddObject(Fields[0], NewClass)
      end;
      Fields.Free;
    until SeekEOF(Fixes);
    close(Fixes);
  end;
end;

function Tab(I : integer = 1) : string;
var
  J : integer;
begin
  Result := '  ';
  for J := 2 to I do Result := Result + '  '
end;

function WriteOptional(Optional : boolean; Typ : string; Equal : string = ' = ') : string; begin
  if Optional then begin
    if Typ = 'string'       then Result := ''''''  else
    if Typ = 'Integer'      then Result := '0'     else
    if Typ = 'Boolean'      then Result := 'false' else
    if Typ = 'Double'       then Result := '0.0'   else
    if Typ = 'TDateTime'    then Result := '0'     else
    if Typ = 'Region'       then Result := ''''''  else
    if Typ = 'ExtLibRegion' then Result := ''''''  else
    Result := 'nil';
    Result := Equal + Result;
  end
  else
    Result := ''
end;

var
  Pas : text;

procedure WriteParams(Params : TStringList);
var
  I  : integer;
  Op : boolean;
begin
  Op := false;
  for I := 0 to Params.Count-1 do begin
    with TParam(Params.Objects[I]) do begin
      if Optional then Op := true;
      write(Pas, Name, ' : ', Typ, WriteOptional(Op, Typ));
    end;
    if I <> Params.Count-1 then write(Pas, '; ');
  end;
end;

function WriteMethodSignature(Method : TMethod; Defaults : boolean; pClassName : string = '') : boolean;
var
  T : string;
begin
  with Method do begin
    if pClassName <> '' then begin
      pClassName := pClassName + '.';
      T := ''
    end
    else
      T := Tab(2);
    if Return = '' then begin
//      if (Params.Count = 0) and not Defaults then exit;
      write(Pas, T, 'constructor ', pClassName, 'Create')
    end
    else
      write(Pas, T, IfThen(Static, 'class ', ''), IfThen(Return = 'Void', 'procedure ', 'function '), pClassName, Name);
    if Params.Count <> 0 then begin
      write(Pas, '(');
      WriteParams(Params);
      write(Pas, ')');
    end;
    write(Pas, IfThen((Return = 'Void') or (Return = ''), ';', ' : ' + Return + ';'), IfThen(Overload and (pClassName = ''), ' overload;', ''),
      IfThen(pClassName = '', ^M^J, ''));
  end;
  Result := true;
end;

procedure WriteClassType(Cls : TClass);

  procedure WriteArrayType(T : string);
  const
    GlobalTypes : string = '.ArrayOfExtObject.ArrayOfstring.ArrayOfInteger.';
  begin
    if pos(ArrayOf, T) <> 0 then begin // Pascal requires proper type instead of "array of" as return type
      if pos('.' + T + '.', GlobalTypes) = 0 then begin
        writeln(Pas, Tab, T, ' = array of ', copy(T, length(ArrayOf)+1, length(T)), ';'^M^J);
        GlobalTypes := GlobalTypes + T + '.';
      end;
    end;
  end;

var
  I, J : integer;
  ArrayType : string;
begin
  with Cls do begin
    for I := 0 to Properties.Count-1 do // Write array types
      WriteArrayType(TProp(Properties.Objects[I]).Typ);
    for I := 0 to Methods.Count-1 do // Write array types
      with TMethod(Methods.Objects[I]) do begin
        WriteArrayType(Return);
        for J := 0 to Params.Count-1 do
          WriteArrayType(TParam(Params.Objects[J]).Typ);
      end;
    writeln(Pas, Tab, Name, ' = class(', IfThen(Parent = '', 'ExtObject', Parent), ')');
    if Properties.Count > 0 then writeln(Pas, Tab, 'private');
    for I := 0 to Properties.Count-1 do // Write private fields
      with TProp(Properties.Objects[I]) do begin
        if not Static then
          writeln(Pas, Tab(2), 'F', Name, ' : ', Typ, ';', IfThen(Default <> '', ' // ' + Default, ''));
      end;
    for I := 0 to Properties.Count-1 do // Write Set procedures
      with TProp(Properties.Objects[I]) do begin
        if not Static then
          if pos(ArrayOf, Typ) = 1 then begin
            ArrayType := copy(Typ, length(ArrayOf)+1, length(Typ));
            writeln(Pas, Tab(2), 'function  GetF', Name, '(I : Word) : ', ArrayType, ';');
            writeln(Pas, Tab(2), 'procedure SetF', Name, '(I : Word; Value : ', ArrayType, ');');
          end
          else
            writeln(Pas, Tab(2), 'procedure SetF', Name, '(Value : ', Typ, ');');
      end;
    if Properties.Count > 0 then writeln(Pas, Tab, 'public');
    for I := 0 to Properties.Count-1 do // Write properties
      with TProp(Properties.Objects[I]) do begin
        if not Static then
          if pos(ArrayOf, Typ) = 1 then begin
            ArrayType := copy(Typ, length(ArrayOf)+1, length(Typ));
            writeln(Pas, Tab(2), 'property ', Name, '[I : Word] : ', ArrayType, ' read GetF', Name, ' write SetF', Name, ';');
          end
          else
            writeln(Pas, Tab(2), 'property ', Name, ' : ', Typ, ' read F', Name, ' write SetF', Name, ';');
      end;
    for I := 0 to Properties.Count-1 do // Write class properties
      with TProp(Properties.Objects[I]) do
        if Static then
          writeln(Pas, Tab(2), 'class function ', Name, ' : ', Typ, ';');
    if Defaults then writeln(Pas, Tab(2), 'procedure Init;');
    for I := 0 to Methods.Count-1 do // Write methods
      WriteMethodSignature(TMethod(Methods.Objects[I]), Defaults);
    for I := 0 to Properties.Count-1 do // Write SetLength Methods
      with TProp(Properties.Objects[I]) do
        if not Static and (pos(ArrayOf + 'Ext', Typ) = 1) then begin
          Arrays := true;
          writeln(Pas, Tab(2), 'procedure SetLength', Name, '(NewLength : Word; OtherClass : TExtObjectClass = nil);');
        end;
    if Arrays then writeln(Pas, Tab(2), 'destructor Destroy; override;');
    writeln(Pas, Tab, 'end;'^M^J);
  end;
end;

function ParamsToJSON(Params : TStringList; Config : boolean = true) : string;

  procedure OptionalParam(Param : TParam); begin
    with Param do
      if Optional then
        Result := Result + ' + IfThen(' + Name + WriteOptional(Optional, Typ) + ', '''', '','')'
      else
        Result := Result + ' + '',''';
  end;

var
  I : integer;
  InitCommonParam : boolean;
begin
  if Params.Count <> 0 then begin
    InitCommonParam := true;
    Result := '(''';
    for I := 0 to Params.Count-1 do
      with TParam(Params.Objects[I]) do begin
        if pos(ArrayOf, Typ) <> 1 then begin
          if InitCommonParam then begin
            Result := Result + ' + VarToJSON([';
            InitCommonParam := false;
          end
          else
            if I > 0 then Result := Result + ', ';
          Result := Result + Name;
        end
        else begin
          if not InitCommonParam then begin
            Result := Result + '])';
            OptionalParam(TParam(Params.Objects[I]));
          end;
          if pos(ArrayOf + 'Ext', Typ) = 1 then
            Result := Result + ' + VarToJSON(ArrayOfExtObject(' + Name + '))'
          else
            Result := Result + ' + VarToJSON(' + Name + ')';
          if I < (Params.Count-1) then OptionalParam(TParam(Params.Objects[I+1]));
          InitCommonParam := true;
        end;
      end;
    if not InitCommonParam then Result := Result + '])';
    Result := Result + ' + '');';
  end
  else
    Result := IfThen(Config, '({});', '();');
end;

function SortByInheritLevel(List : TStringList; I, J : integer) : integer; begin
  Result := TClass(List.Objects[I]).InheritLevel - TClass(List.Objects[J]).InheritLevel
end;

procedure WriteUnits;
var
  I, J, K, L : integer;
  CName, CJSName, ArrayType, DestructorBody : string;
begin
  for I := 0 to Units.Count-1 do
    with TUnit(Units.Objects[I]) do begin
      ReviewTypes;
      assign(Pas, Name + '.pas');
      rewrite(Pas);
      writeln(Pas, 'unit ', Name, ';'^M^J);
      writeln(Pas, 'interface'^M^J^M^J'uses'^M^J, Tab, 'StrUtils, ExtPascal', UsesList, ';'^M^J);
      writeln(Pas, 'type');
      Classes.CustomSort(SortByInheritLevel);
      for J := 0 to Classes.Count-1 do // forward classes
        writeln(Pas, Tab, TClass(Classes.Objects[J]).Name, ' = class;');
      if Units[I] = 'Ext' then // Exception, this workaround resolve circular reference in Ext Unit
        writeln(Pas, Tab, 'ExtFormField = ExtBoxComponent;'^M^J, Tab, 'ExtLayoutContainerLayout = ExtObject;'^M^J, Tab, 'ExtMenuCheckItem = ExtComponent;');
      writeln(Pas);
      for J := 0 to Classes.Count-1 do
        WriteClassType(TClass(Classes.Objects[J]));
      writeln(Pas, 'implementation'^M^J);
      for J := 0 to Classes.Count-1 do
        with TClass(Classes.Objects[J]) do begin
          CName   := Name;
          CJSName := JSName;
          for K := 0 to Properties.Count-1 do // Write Set procedures implementation
            with TProp(Properties.Objects[K]) do begin
              if not Static then begin
                if pos(ArrayOf, Typ) = 1 then begin
                  ArrayType := copy(Typ, length(ArrayOf)+1, length(Typ));
                  writeln(Pas, 'function ', CName, '.GetF', Name, '(I : Word) : ', ArrayType, '; begin');
                  writeln(Pas, Tab, 'Result := F', Name, '[I];');
                  writeln(Pas, 'end;'^M^J);
                  writeln(Pas, 'procedure ', CName, '.SetF', Name, '(I : Word; Value : ', ArrayType, '); begin');
                  writeln(Pas, Tab, 'F', Name, '[I] := Value;');
                  writeln(Pas, Tab, 'AddJS(Value.JSName, JSName + ''.' + JSName + ''');');
                end
                else begin
                  writeln(Pas, 'procedure ', CName, '.SetF', Name, '(Value : ', Typ, '); begin');
                  writeln(Pas, Tab, 'F', Name, ' := Value;');
                  writeln(Pas, Tab, 'AddJS(''', JSName, ':'' + VarToJSON([Value]));');
                end;
                writeln(Pas, 'end;'^M^J);
              end;
            end;
          for K := 0 to Properties.Count-1 do // Write class properties implementation
            with TProp(Properties.Objects[K]) do
              if Static then begin
                writeln(Pas, 'class function ', CName, '.', Name, ' : ', Typ, '; begin');
                //writeln(Pas, Tab, 'AddJS(''', CName + '.' + Name + '({});'');');
                writeln(Pas, Tab, 'Result', WriteOptional(true, Typ, ' := '));
                writeln(Pas, 'end;'^M^J);
              end;
          if Defaults then begin // write Init method
            writeln(Pas, 'procedure ', CName, '.Init; begin');
            writeln(Pas, Tab, 'inherited;');
            for L := 0 to Properties.Count-1 do
              with TProp(Properties.Objects[L]) do
                if Default <> '' then writeln(Pas, Tab, 'F', Name, ' := ', Default, ';');
            writeln(Pas, 'end;'^M^J);
          end;
          for K := 0 to Methods.Count-1 do // Write methods
            if WriteMethodSignature(TMethod(Methods.Objects[K]), Defaults, Name) then begin
              writeln(Pas, ' begin');
              with TMethod(Methods.Objects[K]) do
                if Return = '' then begin // Write constructors
                  writeln(Pas, Tab, 'Init;');
                  writeln(Pas, Tab, 'CreateVar(''', CJSName, ParamsToJSON(Params), ''')');
                end
                else begin // Write class and instance methods
                  writeln(Pas, Tab, 'AddJS(', IfThen(Static, '''' + CJSName + '.', 'JSName' + ' + ''.'), JSName, ParamsToJSON(Params, false), ''');');
                  if Return <> 'Void' then writeln(Pas, Tab, 'Result', WriteOptional(true, Return, ' := '));
                end;
              writeln(Pas, 'end;'^M^J);
            end;
          DestructorBody := '';
          for K := 0 to Properties.Count-1 do // Write SetLength Methods
            with TProp(Properties.Objects[K]) do
              if not Static and (pos(ArrayOf + 'Ext', Typ) = 1) then begin
                writeln(Pas, 'procedure ', CName, '.SetLength', Name, '(NewLength : Word; OtherClass : TExtObjectClass); begin');
                writeln(Pas, Tab, 'SetLength(ArrayOfExtObject(F', Name, '), IfOtherClass(OtherClass = nil, ',
                  copy(Typ, length(ArrayOf)+1, length(Typ)), ', OtherClass), NewLength, ''', JSName,''')');
                DestructorBody := DestructorBody + Tab + 'SetLength' + Name + '(0);'^M^J;
                writeln(Pas, 'end;'^M^J);
              end;
          if Arrays then begin // Write destructor
            writeln(Pas, 'destructor ', CName, '.Destroy; begin');
            writeln(Pas, Tab, 'inherited;');
            write(Pas, DestructorBody);
            writeln(Pas, 'end;'^M^J);
          end;
        end;
      write(Pas, 'end.');
      close(Pas);
    end;
end;

var
  F : TSearchrec;
  T : tdatetime;
begin
  if FindFirst(paramstr(1) + '\*.html', faAnyFile, F) = 0 then begin
    AllClasses := TStringList.Create;
    Units := TStringList.Create;
    T := now;
    writeln('Reading HTML files...');
    repeat
  		ReadHtml(paramstr(1) + '\' + F.Name)
    until FindNext(F) <> 0;
    writeln('Writing Unit files...');
    FindClose(F);
    LoadFixes;
    WriteUnits;
    writeln(AllClasses.Count, ' ExtJS classes wrapped to Object Pascal at ', formatdatetime('ss.zzz', Now-T), ' seconds');
  end
  else
    writeln('ExtJS HTML files not found at ' + paramstr(1) + '\*.html'); readln;
end.
