program Ext2Pascal;

uses
  SysUtils, StrUtils, Classes, Math;

function FixReserved(S : string) : string;
const
  Reserved = '.and.array.as.asm.begin.case.class.const.constructor.create.destructor.destroy.dispinterface.div.do.downto.else.end.except.exports.'+
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
      Result := Result + Ident[I]
    else
      if I < length(Ident) then Ident[I+1] := UpCase(Ident[I+1]);
  Result := FixReserved(Result);
end;

function FixType(Ident : string) : string;
var
  T : string;
  I : integer;
begin
  T := LowerCase(Ident);
  if T = 'string'    then begin Result := 'string';             exit end else
  if T = 'number'    then begin Result := 'Integer';            exit end else
  if T = 'boolean'   then begin Result := 'Boolean';            exit end else
  if T = 'object'    then begin Result := 'ExtObject';          exit end else
  if T = 'date'      then begin Result := 'TDateTime';          exit end else
  if T = 'float'     then begin Result := 'Double';             exit end else
  if T = 'mixed'     then begin Result := 'Variant';            exit end else
  if T = 'array'     then begin Result := 'ArrayOfVariant';     exit end else
  if T = 'int'       then begin Result := 'Integer';            exit end else
  if T = 'object...' then begin Result := 'array of ExtObject'; exit end else
  if pos('array of ', T) = 1 then begin Result := Ident;        exit end
  else begin
    I := LastDelimiter('/[:', Ident);
    if I <> 0 then begin
      if Ident[I] = '/' then
        Result := 'string'
      else
        Result := 'array of ' + FixType(copy(Ident, 1, I-1));
      exit;
    end;
  end;
  Result := FixIdent(Ident);
end;

type
  TUnitDef = class
    Name : string;
    Classes : TStringList;
    constructor Create(pName : string);
    function UsesList : string;
    procedure ReviewTypes;
  end;

  TClassDef = class
    Name, Parent, UnitName : string;
    Properties, Methods : TStringList;
    constructor Create(pName : string);
    function InheritLevel : integer;
  end;

  TPropDef = class
    Name, Default, Typ : string;
    Static : boolean;
    constructor Create(pName : string; pStatic : boolean; pType : string);
  end;

  TMethodDef = class
    Name, Return : string;
    Params : TStringList;
		Static, Overload : boolean;
	  constructor Create(pName, pReturn : string; pParams : TStringList; pStatic, pOverload : boolean);
    function CreateOverloadParams(P : integer; NewType : string) : TStringList;
  end;

  TParamDef = class
	  Name, Typ : string;
	  Optional : boolean;
    constructor Create(pName, pType : string; pOptional : boolean);
  end;

var
  AllClasses, Units : TStringList;

constructor TUnitDef.Create(pName : string); begin
  Name := FixIdent(pName);
  Classes := TStringList.Create;
end;

function TUnitDef.UsesList: string;
var
  I, J : integer;
  UName : string;
begin
  if Name = 'ExtGlobal' then
    Result := ''
  else
    Result := ', ExtGlobal';
  for I := 0 to Classes.Count-1 do
    with TClassDef(Classes.Objects[I]) do
      if Parent <> '' then begin
        J := AllClasses.IndexOf(Parent);
        if J <> 0 then begin
          UName := TClassDef(AllClasses.Objects[J]).UnitName;
          if (UName <> UnitName) and (pos(', ' + UName + ',', Result + ',') = 0) then
            Result := Result + ', ' + UName
        end;
      end;
end;

procedure TUnitDef.ReviewTypes;

  procedure InsertNamespace(var Typ : string);
  var
    T : string;
    I : integer;
  begin
    if Typ <> '' then begin
      T := Typ;
      if T[1] = '_' then T := copy(T, 2, length(T));
(*      if Classes.IndexOf(Name + T) <> -1 then
        Typ := Name + T
      else*)
        for I := 0 to Units.Count-1 do
          if AllClasses.IndexOf(Units[I] + T) <> -1 then
            Typ := Units[I] + T;
    end;
  end;

var
  I, J, K : integer;
begin
  writeln(Name);
  for I := 0 to Classes.Count-1 do
    with TClassDef(Classes.Objects[I]) do begin
      for J := 0 to Properties.Count-1 do
        InsertNamespace(TPropDef(Properties.Objects[J]).Typ);
      for J := 0 to Methods.Count-1 do
        with TMethodDef(Methods.Objects[J]) do begin
          InsertNamespace(Return);
          for K := 0 to Params.Count-1 do
            InsertNamespace(TParamDef(Params.Objects[K]).Typ);
        end;
    end;
end;

constructor TClassDef.Create(pName : string); begin
  Name := FixIdent(pName);
  Properties := TStringList.Create;
  Methods := TStringList.Create;
end;

function TClassDef.InheritLevel : integer;
var
  P : string;
begin
  Result := 0;
  P := Parent;
  while P <> '' do begin
    inc(Result);
    P := TClassDef(AllClasses.Objects[AllClasses.IndexOf(P)]).Parent;
  end;
end;

constructor TPropDef.Create(pName : string; pStatic : boolean; pType : string); begin
  Name   := FixIdent(pName);
  Static := pStatic;
  Typ    := FixType(pType);
end;

constructor TParamDef.Create(pName, pType : string; pOptional : boolean); begin
  Name := FixIdent(pName);
  Typ  := pType;
  Optional := pOptional;
end;

constructor TMethodDef.Create(pName, pReturn : string; pParams : TStringList; pStatic, pOverload : boolean); begin
  Name     := FixIdent(pName);
  Return   := pReturn;
  Params   := pParams;
  Static   := pStatic;
  Overload := pOverload;
end;

function TMethodDef.CreateOverloadParams(P : integer; NewType : string): TStringList;
var
  I : integer;
begin
  Result := TStringList.Create;
  for I := 0 to Params.Count-1 do
    with TParamDef(Params.Objects[I]) do
      Result.AddObject(Name, TParamDef.Create(Name, IfThen(I = P, FixType(NewType), Typ), Optional));
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
function Explode(Delim : char; S : string) : TStringList; begin
  Result := TStringList.Create;
  Result.StrictDelimiter := true;
  Result.Delimiter := Delim;
  Result.DelimitedText := S;
end;

procedure DoOverloads(Cls : TClassDef; Method : TMethodDef);
var
  Types : TStringList;
  I, J : integer;
begin
  Cls.Methods.AddObject(Method.Name, Method);
  with Method do begin
    for I := 0 to Params.Count-1 do
      with TParamDef(Params.Objects[I]) do
        if pos('/', Typ) <> 0 then begin
          Overload := true;
          Types := Explode('/', Typ);
          Typ   := FixType(Types[0]);
          for J := 1 to Types.Count-1 do
            DoOverloads(Cls, TMethodDef.Create(Method.Name, Return, CreateOverloadParams(I, Types[J]), Static, true));
          Types.Free;
        end
        else
          Typ := FixType(Typ);
    if pos('/', Return) <> 0 then begin
      Overload := true;
      Types  := Explode('/', Return);
      Return := FixType(Types[0]);
      for J := 1 to Types.Count-1 do
        DoOverloads(Cls, TMethodDef.Create(Method.Name, Types[J], Params, Static, true));
      Types.Free;
    end
    else
      Return := FixType(Return);
  end;
end;

procedure SetDefault(Prop : TPropDef; Def : string; var Match : TStringList);
var
  I : integer;
begin
  Def := AnsiReplaceText(Def, '"', '''');
  if (Def = '') or (Def = '''''') then exit;
  with Prop do begin
    if Default <> '' then exit;
    if (Typ = 'Boolean') and (pos('true', Def) <> 0) then Default := 'true' else
    if ((Typ = 'string') or (Typ = 'Variant')) and Extract(['''', ''''], Def, Match) then begin
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

var
  State : (Initial, InClass, InProperties, InMethods);

procedure LoadElements(Line : string);
const
  CurClass : TClassDef = nil;
  CurProp  : TPropDef  = nil;
  PropName : string    = '';
  MetName  : string    = '';
var
  PackName, Arg, Return : string;
  Matches, Params, Args : TStringList;
  Package : TUnitDef;
  I : integer;
begin
  Matches := TStringList.Create;
	case State of
    Initial :
      if Extract(['<h1>Class', '</h1>'], Line, Matches) then begin
				CurClass := TClassDef.Create(Matches[0]);
				State := InClass;
			end;
    InClass :
      if Extract(['Package:', '<td class="hd-info">', '<'], Line, Matches) then begin
        if CurClass.Name = 'Ext' then begin
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
          Package := TUnitDef.Create(PackName);
          Units.AddObject(PackName, Package)
        end
        else
          Package := TUnitDef(Units.Objects[I]);
        Package.Classes.AddObject(CurClass.Name, CurClass)
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
        if (PropName <> 'config') and (CurClass.Properties.IndexOf(PropName) = -1)  then begin // config object are deleted, sugar coding non pratical in Pascal
          CurProp := TPropDef.Create(PropName, I <> 0, Matches[2]);
          CurClass.Properties.AddObject(FixIdent(PropName), CurProp);
        end;
      end
			else
  			if Extract(['(defaults to', ')'], Line, Matches) then
          SetDefault(CurProp, Matches[0], Matches)
        else
          if (PropName <> 'config') and Extract(['<td class="msource">', '</td>'], Line, Matches) then begin
            if Matches[0][1] = '<' then
              with CurClass.Properties do Delete(IndexOf(FixIdent(PropName))); // delete property inherited
          end
          else
            if pos('<h2>Public Methods</h2>', Line) <> 0 then State := InMethods;
    InMethods :
				if Extract(['<b>', '</b>', ':', '<div class="mdesc">'], Line, Matches) or
           Extract(['<b>', '</b>', ')', '<div class="mdesc">'], Line, Matches) then begin
					MetName:= Matches[0];
					Params := Explode(',', Matches[1]);
          Return := FixType(Matches[2]);
          if pos('Instance', Return) > 1 then Return := copy(Return, 1, length(Return) - length('Instance')); // doc fault
          Args   := TStringList.Create;
					for I := 0 to Params.Count-1 do
						if Extract(['<code>', ' ', '</code>'], Params[I], Matches) then begin
							if pos('etc', Matches[1]) = 1 then begin // variable parameter list
							  Matches[0] := 'array of ' + FixType(Matches[0]);
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
              if Matches[1] <> 'config' then // config object are deleted, sugar coding non pratical in Pascal
							  Args.AddObject(FixIdent(Matches[1]), TParamDef.Create(Matches[1], Matches[0], (pos('>[', Params[I]) <> 0) and (pos('array of ', Matches[0]) = 0)));
            end;
          Params.Free;
          I := LastDelimiter('.', MetName);
          if I <> 0 then MetName := copy(MetName, I+1, length(MetName));
  				DoOverloads(CurClass, TMethodDef.Create(MetName, Return, Args, I <> 0, false));
				end
				else
          if Extract(['<td class="msource">', '</td>'], Line, Matches) then begin
            if Matches[0][1] = '<' then
              with CurClass.Methods do begin
                I := IndexOf(FixIdent(MetName));
                while I <> -1 do begin
                  Delete(I);
                  I := IndexOf(FixIdent(MetName)); // delete method inherited and overloads
                end;
              end;
          end
          else
            if pos('<h2>Public Events</h2>', Line) <> 0 then begin
              AllClasses.AddObject(CurClass.Name, CurClass);
              State := Initial;
            end;
  end;
  Matches.Free;
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

function Tab(I : integer = 1) : string;
var
  J : integer;
begin
  Result := '  ';
  for J := 2 to I do Result := Result + '  '
end;

function WriteOptional(Optional : boolean; Typ : string) : string; begin
  if Optional then begin
    if Typ = 'string'    then Result := ' = '''''  else
    if Typ = 'Integer'   then Result := ' = 0'     else
    if Typ = 'Boolean'   then Result := ' = false' else
    if Typ = 'Double'    then Result := ' = 0.0'   else
    if Typ = 'TDateTime' then Result := ' = 0'     else
    if Typ = 'Variant'   then Result := ''  else ###########
    Result := ' = nil';
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
    with TParamDef(Params.Objects[I]) do begin
      if Optional then Op := true;
      write(Pas, Name, ' : ', Typ, WriteOptional(Op, Typ));
    end;
    if I <> Params.Count-1 then write(Pas, '; ');
  end;
end;

function WriteMethodSignature(Method : TMethodDef; pClassName : string = '') : boolean;
var
  T : string;
begin
  Result := false;
  with Method do begin
    if pClassName <> '' then begin
      pClassName := pClassName + '.';
      T := ''
    end
    else
      T := Tab(2);
    if Return = '' then begin
      if Params.Count = 0 then exit;
      write(Pas, T, 'constructor ', pClassName, 'Create')
    end
    else
      write(Pas, T, IfThen(Static, 'class ', ''), IfThen(Return = 'void', 'procedure ', 'function '), pClassName, Name);
    if Params.Count <> 0 then begin
      write(Pas, '(');
      WriteParams(Params);
      write(Pas, ')');
    end;
    write(Pas, IfThen((Return = 'void') or (Return = ''), ';', ' : ' + Return + ';'), IfThen(Overload and (pClassName = ''), ' overload;', ''),
      IfThen(pClassName = '', ^M^J, ''));
  end;
  Result := true;
end;

procedure WriteClassType(Cls : TClassDef);
const
  GlobalTypes : string = '';
var
  I : integer;
begin
  with Cls do begin
    for I := 0 to Methods.Count-1 do
      with TMethodDef(Methods.Objects[I]) do
        if pos('array of ', Return) <> 0 then begin // Pascal requires proper type instead of "array of" as return type
          if pos(Return + '.', GlobalTypes) = 0 then begin
            writeln(Pas, Tab, FixIdent(Return), ' = ', Return, ';'^M^J);
            GlobalTypes := GlobalTypes + Return + '.';
          end;
          Return := FixIdent(Return);
        end;
    writeln(Pas, Tab, Name, ' = class(', IfThen(Parent = '', 'ExtObject', Parent), ')');
    for I := 0 to Properties.Count-1 do
      with TPropDef(Properties.Objects[I]) do
        writeln(Pas, Tab(2), IfThen(Static, 'class function ', ''), Name, ' : ', Typ, ';', IfThen(Default <> '', ' // ' + Default, ''));
    for I := 0 to Methods.Count-1 do
      WriteMethodSignature(TMethodDef(Methods.Objects[I]));
    writeln(Pas, Tab, 'end;'^M^J);
  end;
end;

function SortByInheritLevel(List : TStringList; I, J : integer) : integer; begin
  Result := TClassDef(List.Objects[I]).InheritLevel - TClassDef(List.Objects[J]).InheritLevel
end;

procedure WriteUnits;
var
  I, J, K : integer;
  CName : string;
begin
  for I := 0 to Units.Count-1 do
    with TUnitDef(Units.Objects[I]) do begin
      assign(Pas, Name + '.pas');
      rewrite(Pas);
      writeln(Pas, 'unit ', Name, ';'^M^J);
      writeln(Pas, 'interface'^M^J^M^J'uses'^M^J, Tab, 'ExtPascal', UsesList, ';'^M^J);
      writeln(Pas, 'type');
      ReviewTypes;
      Classes.CustomSort(SortByInheritLevel);
      for J := 0 to Classes.Count-1 do // forward classes
        writeln(Pas, Tab, TClassDef(Classes.Objects[J]).Name, ' = class;');
      writeln(Pas);
      for J := 0 to Classes.Count-1 do
        WriteClassType(TClassDef(Classes.Objects[J]));
      writeln(Pas, 'implementation'^M^J);
      for J := 0 to Classes.Count-1 do
        with TClassDef(Classes.Objects[J]) do begin
          CName := Name;
          for K := 0 to Properties.Count-1 do
            with TPropDef(Properties.Objects[K]) do
              if Static then begin
                writeln(Pas, 'class function ', CName, '.', Name, ' : ', Typ, '; begin');
                writeln(Pas, Tab, 'writeln;');
                writeln(Pas, 'end;'^M^J);
              end;
          for K := 0 to Methods.Count-1 do
            if WriteMethodSignature(TMethodDef(Methods.Objects[K]), Name) then begin
              writeln(Pas, ' begin');
              // Quando de classe lembrar de mandar o nome da classe quando mandar o json, idem para propriedade
              writeln(Pas, Tab, 'writeln;');
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
    repeat
  		ReadHtml(paramstr(1) + '\' + F.Name)
    until FindNext(F) <> 0;
    FindClose(F);
    WriteUnits;
    writeln(AllClasses.Count, ' ExtJS classes wrapped to Object Pascal.', formatdatetime('ss:zzz', Now-t));
  end
  else
    writeln('ExtJS HTML files not found at ' + paramstr(1) + '\*.html'); readln;
end.
