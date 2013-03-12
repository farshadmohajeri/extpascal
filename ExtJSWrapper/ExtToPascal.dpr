{
ExtToPascal is a parser that scans Ext JS sources and that creates a Object Pascal wrapper upon Ext classes and widgets
Author: Wanderlan Santos dos Anjos (wanderlan.anjos@gmail.com)
Date: apr-2008, mar-2013 (Ext JS 4.x)
License: BSD<extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
program ExtToPascal;

{$IFDEF FPC}{$MACRO ON}{$MODE DELPHI}{$ENDIF}
{$IFDEF MSWINDOWS}{$APPTYPE CONSOLE}{$ENDIF}
{$J+}

uses
  SysUtils, StrUtils, Classes, Math, ExtPascalUtils
//  , Ext
  ;

{.$DEFINE USES_PUBLISHED}

const
  AP = '''';

function FixReserved(S : string) : string;
const
  Reserved = '.and.array.as.asm.begin.case.class.const.constructor.destructor.destroy.dispinterface.div.do.downto.else.end.except.exports.'+
    'false.file.finalization.finally.for.function.goto.if.implementation.in.inherited.initialization.inline.interface.is.label.library.'+
    'mod.nil.not.object.of.or.out.packed.procedure.program.property.raise.record.repeat.resourcestring.result.set.shl.shr.string.then.'+
    'threadvar.to.try.true.type.unit.until.uses.var.while.with.xor.';
begin
  if pos('.' + lowercase(S) + '.', Reserved) = 0 then
    Result := S
  else
    Result := S + 'JS';
end;

function FixIdent(Ident : string; IsType : boolean = false) : string;
var
  I, J : integer;
  Words : array[0..9] of integer;
begin
  Result := '';
  if Ident = '' then exit;
  Words[0] := 1;
  J := 0;
  for I := 1 to length(Ident) do
    if Ident[I] in ['0'..'9', 'A'..'Z', 'a'..'z', '_'] then
      if I = 1 then
        Result := UpCase(Ident[1])
      else
        Result := Result + Ident[I]
    else
      if (I <> 1) and (I <> length(Ident)) and (Ident[I] in ['(', '[', '{', ')', ']', '}', '=']) then
        break
      else
        if I < length(Ident) then begin
          inc(J);
          Words[J]   := I;
          Ident[I+1] := UpCase(Ident[I+1]);
        end;
  if Result = '' then exit;
  // Remove final dup word
  if (J > 0) and (copy(Result, Words[J-1], Words[J] - Words[J-1]-1) = copy(Result, Words[J]-1, Length(Result))) then
    delete(Result, Words[J]-1, length(Result));
  if IsType then begin
    if Result <> '' then Result := 'T' + Result;
    Result := AnsiReplaceText(Result, 'MethodConstructor', '');
    Result := AnsiReplaceText(Result, 'cfghideMode', '');
  end
  else
    Result := FixReserved(Result);
end;

function FixType(Ident : string) : string;
var
  I : integer;
begin
  if Ident <> '' then
    case CaseOf(LowerCase(Ident), ['string', 'number', 'integer', 'object', 'boolean', 'function', 'mixed',
                'array', 'object...', 'date', 'float', 'int', 'bool', 'double', 'object.', 'string...']) of
      0, 6, 15 : Result := 'String';
      1, 2, 11 : Result := 'Integer';
      3, 14    : Result := 'TExtObject';
      4, 12    : Result := 'Boolean';
      5        : Result := 'TExtFunction';
      7, 8     : Result := 'TExtObjectList';
      9        : Result := 'TDateTime';
      10, 13   : Result := 'Double';
    else
      if pos('mixedcollection', lowercase(Ident)) <> 0 then
        Result := 'TExtObjectList'
      else begin
        I := pos('/', Ident);
        if I = 0 then I := LastDelimiter('[:', Ident);
        if I <> 0 then begin
          if Ident[I] <> '/' then begin
            Result := FixType(copy(Ident, 1, I-1)); // for alternative types at methods' return choose first option
            if (Result <> 'Integer') and (Result <> 'string') then
              Result := 'TExtObjectList'
            else
              Result := 'TArrayOf' + Result;
          end
          else
            Result := Ident;
          exit;
        end;
        if pos('TExt', Ident) = 1 then
          Result := Ident
        else
          Result := FixIdent(Ident, true);
      end;
    end
  else
    Result := ''
end;

function FixJSName(JSName : string) : string; begin
  Result := AnsiReplaceStr(JSName, '_', '')
end;

type
  TUnit = class
    Name, UsesList : string;
    Classes : TStringList;
    constructor Create(pName : string);
    destructor Destroy; override;
    function InitUsesList : string;
  end;

  TClass = class
    Name, JSName, Parent, UnitName, SimpleName : string;
    Singleton, Defaults, Arrays, Objects, AltCreate : boolean;
    Properties, Methods, Events : TStringList;
    constructor Create(pName : string; pParent : string = ''; pUnitName : string = '');
    procedure AddCreate;
    destructor Destroy; override;
  private
    function InheritLevel: integer;
  end;

  TProp = class
    Name, JSName, Typ, Default : string;
    Static, Config, Enum : boolean;
    constructor Create(pName, pJSName, pType : string; pStatic, pConfig : boolean; pDefault : string = '');
  end;

  TMethod = class
    Name, JSName, Return : string;
    Params : TStringList;
		Static, Overload : boolean;
	  constructor Create(pName, pReturn : string; pParams : TStringList; pStatic, pOverload : boolean); overload;
	  constructor Create(pName, pJSName, pReturn : string; pParams : TStringList; pStatic, pOverload : boolean); overload;
    function CreateOverloadParams(P : integer; NewType : string) : TStringList;
    destructor Destroy; override;
  end;

  TParam = class
	  Name, Typ : string;
	  Optional  : boolean;
    constructor Create(pName, pType : string; pOptional : boolean);
    destructor Destroy; override;
  end;

procedure FreeStringList(var L : TStringList);
var
  I : Integer;
begin
  for I := 0 to L.Count-1 do L.Objects[I].Free;
  L.Free;
end;

var
  AllClasses, Units : TStringList;

constructor TUnit.Create(pName : string); begin
  Name    := FixIdent(pName);
  Classes := TStringList.Create;
end;

destructor TUnit.Destroy; begin
  FreeAndNil(Classes); // AllClasses frees class instances.
  inherited;
end;

function TUnit.InitUsesList: string;
var
  I, J : integer;
  UName : string;
begin
  Result := '';
  for I := 0 to Classes.Count-1 do
    with TClass(Classes.Objects[I]) do
      if Parent <> '' then begin
        J := AllClasses.IndexOf(Parent);
        if J <> -1 then begin
          UName := TClass(AllClasses.Objects[J]).UnitName;
          if (Self.Name = 'Ext') and (UName = 'ExtData') then continue; // remove circular reference
          if (UName <> UnitName) and (pos(', ' + UName + ',', Result + ',') = 0) then
            Result := Result + ', ' + UName
        end;
      end;
end;

constructor TClass.Create(pName, pParent, pUnitName : string); begin
  Name       := FixIdent(pName, true);
  SimpleName := Copy(pName, LastDelimiter('.', pName) + 1, MaxInt);
  JSName     := FixJSName(pName);
  Parent     := FixIdent(pParent, true);
  UnitName   := FixIdent(pUnitName);
  Properties := TStringList.Create;
  Methods    := TStringList.Create;
  Events     := TStringList.Create;
  if pos('Ext.form.Action', JSName) <> 0 then JSName := ''; // Anonymous classes
end;

destructor TClass.Destroy; begin
  FreeStringList(Properties);
  FreeStringList(Methods);
  FreeStringList(Events);
  inherited;
end;

procedure TClass.AddCreate; begin
  if not Singleton and (Methods.IndexOf('Create') = -1) then
    Methods.AddObject('Create', TMethod.Create('Create', '', TStringList.Create, false, false));
end;

function TClass.InheritLevel : integer;
var
  P : string;
  I : integer;
begin
  Result := 0;
  P := Parent;
  while P <> '' do begin
    I := AllClasses.IndexOf(P);
    if I = -1 then exit;
    inc(Result);
    P := TClass(AllClasses.Objects[I]).Parent;
  end;
end;

function IsUpper(S : string) : boolean;
var
  I : integer;
begin
  Result := true;
  for I := 1 to length(S) do
    if S[I] in ['a'..'z'] then begin
      Result := false;
      exit;
    end;
end;

function DefValue(Typ : string) : string; begin
  case CaseOf(Typ, ['string', 'Region', 'TExtLibRegion', 'integer', 'double', 'TDateTime', 'boolean']) of
    0..2 : Result := '''''';
    3..5 : Result := '0';
    6    : Result := 'false';
  else
    Result := 'nil'
  end;
end;

constructor TProp.Create(pName, pJSName, pType : string; pStatic, pConfig : boolean; pDefault : string = ''); begin
  Name    := FixIdent(pName);
  JSName  := FixJSName(pJSName);
  Static  := pStatic or IsUpper(pName);
  Config  := pConfig;
  Typ     := FixType(pType);
  Default := pDefault;
end;

var
  FCParams, TCParams : Integer;

constructor TParam.Create(pName, pType : string; pOptional : boolean); begin
  Inc(FCParams);
  Inc(TCParams);
  Name     := FixIdent(IfThen(pName = '', 'Param', pName));
  Typ      := pType;
  Optional := pOptional;
end;

destructor TParam.Destroy; begin
  Dec(FCParams);
  inherited;
end;

constructor TMethod.Create(pName, pReturn : string; pParams : TStringList; pStatic, pOverload : boolean); begin
  Name   := FixIdent(pName);
  JSName := FixJSName(pName);
  Params := pParams;
  Static := pStatic;
  Return := pReturn;
  Overload := pOverload;
end;

constructor TMethod.Create(pName, pJSName, pReturn : string; pParams : TStringList; pStatic, pOverload : boolean);
var
  I : integer;
begin
  Name   := pName;
  JSName := FixJSName(pJSName);
  Params := pParams;
  Return := pReturn;
  Static := pStatic;
  Overload := pOverload;
  I := LastDelimiter('.', Name);
  if I <> 0 then Name := copy(Name, I+1, length(Name));
  if not Static then Static := I <> 0;
end;

destructor TMethod.Destroy; begin
  FreeStringList(Params);
  inherited;
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
  if Method.Name = 'Create' then begin
    I := Cls.Methods.IndexOf('Create');
    if I <> -1 then begin
      TMethod(Cls.Methods.Objects[I]).Overload := True;
      Method.Overload := True;
    end;
  end;
  Cls.Methods.AddObject(Method.Name, Method);
  with Method do
    for I := 0 to Params.Count-1 do
      with TParam(Params.Objects[I]) do
        if pos('/', Typ) <> 0 then begin
          Overload := true;
          Types := Explode('/', Typ);
          Typ   := FixType(Types[0]);
          for J := 1 to Types.Count-1 do
            if FixType(Types[J]) <> FixType(Types[J-1]) then // Discard Duplicates
              DoOverloads(Cls, TMethod.Create(Method.Name, Return, CreateOverloadParams(I, Types[J]), Static, true));
          Types.Free;
        end;
end;

procedure SetDefault(Prop : TProp; Def : string; var Match : TStringList);
var
  I : integer;
begin
  Def := trim(AnsiReplaceText(Def, '"', AP));
  if (Def = '') or (Def = '''''') then exit;
  with Prop do begin
    if Default <> '' then exit;
    case CaseOf(Typ, ['boolean', 'string', 'integer', 'double']) of
      0 : if pos('true', Def) <> 0 then Default := 'true';
      1 : begin
        if pos('...', Def) <> 0 then begin
          I := pos('(default', Def);
          if I <> 0 then Def := copy(Def, I+7, length(Def));
        end;
        if Extract(['''', ''''], Def, Match) and (Match[0] <> '') then Default := '''' + Match[0] + '''';
      end;
      2 :
        for I := 1 to length(Def) do
          if Def[I] in ['0'..'9'] then
            Default := Default + Def[I]
          else
            if Default <> '' then exit;
      3 : begin
        for I := 1 to length(Def) do
          if Def[I] in ['0'..'9', '.'] then
            Default := Default + Def[I]
          else
            break;
        if (Default <> '') and (Default[1] = '.') then Default := '0' + Default;
      end;
    end;
  end;
end;

function Unique(S : string; List : TStringList) : string; begin
  Result := S;
  while List.IndexOf(Result) <> -1 do Result := Result + '_'
end;

procedure LoadElements(Line : string);
const
  CurClass  : TClass      = nil;
  CurProp   : TProp       = nil;
  CurMethod : TMethod     = nil;
  PropName  : string      = '';
  MetName   : string      = '';
  PropTypes : TStringList = nil;
  Config    : boolean     = false;
var
  State : (Initial, InClass, InProperties, InMethods);
  PackName, Return, JSName, Params : string;
  Matches, Args : TStringList;
  IsEvent, Optional, Static, Singleton: boolean;
  Package : TUnit;
  I, Ci   : integer;
begin
  if Before('@private',   '*/', Line, false) or
     Before('@protected', '*/', Line, false) or
     Before('@ignore',    '*/', Line, false) or
     Before('@abstract',  '*/', Line, false) then exit;
  Ci := -1;
  State := Initial;
  Matches := TStringList.Create;
  while true do begin
    case State of
      Initial : begin
        JSName := '';
        if pos('Ext.define(''Ext.util.Observable''', Line) <> 0 then
        writeln;
        Singleton := Before('@singleton', '*/', Line, false);
        if Before('@define', '*/', Line, false) then
          if Extract(['@define ', ' '], Line, Matches) then
            JSName := Matches[0];
        Extract(['/**', '*/'], Line, Matches);
        if (JSName <> '') or
           Extract(['Ext.define(' + AP, AP], Line, Matches) or
           Extract(['Ext.define(' + '"', '"'], Line, Matches) then begin
          if JSName = '' then JSName := Matches[0];
          Ci := AllClasses.IndexOf(JSName);
          if Ci = -1 then
            CurClass := TClass.Create(JSName)
          else
            CurClass := TClass(AllClasses.Objects[Ci]);
          State := InClass;
          if Singleton then CurClass.Name := CurClass.Name + 'Singleton';
          CurClass.Singleton := Singleton;
          continue;
        end
        else
          break;
      end;
      InClass : begin
        PackName := 'Ext';
        CurClass.UnitName := PackName;
        I := Units.IndexOf(PackName);
        if I = -1 then begin
          Package := TUnit.Create(PackName);
          Units.AddObject(PackName, Package)
        end
        else
          Package := TUnit(Units.Objects[I]);
        if Ci = -1 then
          Package.Classes.AddObject(PackName, CurClass);
        if Extract(['extend: ' + AP, AP], Line, Matches) then
          CurClass.Parent := FixIdent(Matches[0], true);
        if AllClasses.IndexOf(CurClass.Name) = -1 then
          AllClasses.AddObject(CurClass.Name, CurClass);
        State := InProperties;
        continue;
      end;
      InProperties : begin
        if Before('@hide',      '*/', Line) or
           Before('@private',   '*/', Line) or
           Before('@protected', '*/', Line) then continue;
        Config := Extract(['@cfg {', '} ', ' '], Line, Matches);
        // To do Extract(['@property ', ' ', '@type ', ' '], Line, Matches) PropName = Matches[0]; PropType = Matches[2]
        if Config or Extract(['@property {', '} ', ' '], Line, Matches) then begin
          PropName := Matches[1];
          Static := false;
          I := pos('=', PropName);
          if I <> 0 then
            PropName := copy(PropName, 1, I-1)
          else
            if pos('.', PropName) <> 0 then continue; // doc fault
          if FixIdent(PropName) = '' then continue; // Discard properties nameless
          if Before('@static', '*/', Line, false) then Static := true;
          if IsUppercase(PropName) then Static := true;
          PropName := Unique(FixIdent(PropName), CurClass.Properties);
          if not Static then PropName[1] := LowerCase(PropName)[1];
          if pos('__', Unique(FixIdent(PropName), CurClass.Properties)) <> 0 then begin
            State := InMethods;
            continue; // Discard duplicates
          end;
          PropTypes := Explode('/', Matches[0]);
          I := FirstDelimiter(' <>', PropTypes[0]);
          if I <> 0 then // doc fault
            PropTypes.DelimitedText := copy(PropTypes[0], 1, I-1);
          for I := 0 to PropTypes.Count-1 do
            if I = 0 then begin
              CurProp := TProp.Create(PropName, PropName, PropTypes[I], Static, Config);
              CurClass.Properties.AddObject(FixIdent(PropName), CurProp);
            end
            else
              if CurClass.Properties.IndexOf(FixIdent(PropName + FixType(PropTypes[I]))) = -1 then
                CurClass.Properties.AddObject(FixIdent(PropName + FixType(PropTypes[I])),
                                              TProp.Create(PropName + FixType(PropTypes[I]), PropName, PropTypes[I], Static, Config));
          //if Extract([PropName, ':', ','], Line, Matches) then begin
          if (Before('Default to',  '*/', Line, false) and Extract(['Default to',  '.'], Line, Matches)) or
             (Before('Defaults to', '*/', Line, false) and Extract(['Defaults to', '.'], Line, Matches)) then begin
            SetDefault(CurProp, Matches[0], Matches);
            if (CurProp.Default <> '') and not CurClass.Defaults then begin
              CurClass.Defaults := true;
              CurClass.AddCreate;
            end;
          end;
          FreeAndNil(PropTypes);
          Extract([' ', '*/'], Line, Matches);
          continue;
        end;
        for I := 0 to CurClass.Properties.Count-1 do
          with TProp(CurClass.Properties.Objects[I]) do
            if not Static then
              if Typ = 'TExtObjectList' then begin
                CurClass.Arrays := true;
                CurClass.AddCreate;
              end
              else
                if (pos('TExt', Typ) = 1) and (Typ <> 'TExtFunction') then begin
                  CurClass.Objects := true;
                  CurClass.AddCreate;
                end;
        State := InMethods;
        continue;
      end;
      InMethods : begin
        if Before('@private', '*/', Line) or Before('@protected', '*/', Line) or
           Before('@template', '*/', Line) then continue;
        IsEvent := Extract(['@event ', ' ', '*/'], Line, Matches);
        if IsEvent or Extract(['/**', '*/', ': function('], Line, Matches) then begin
          JSName := IfThen(IsEvent, Matches[0], Matches[1]);
          if (length(JSName) > 30) or (FixIdent(JSName) = '') or (JSName = 'destroy') then continue; // Doc fault
          if JSName = 'create' then CurClass.AltCreate := true;
          Static := pos('@static', IfThen(IsEvent, Matches[1], Matches[0])) <> 0;
          if IsEvent then begin
            MetName := Unique('On' + FixIdent(JSName), CurClass.Properties);
            if MetName <> Unique(MetName, CurClass.Events) then continue;
          end
          else begin
            MetName := Unique(FixIdent(JSName), CurClass.Properties);
            if MetName <> Unique(MetName, CurClass.Methods) then continue;
            if MetName <> 'Create' then MetName := Unique(MetName, CurClass.Methods);
            if pos(MetName, 'ConstructorJS.Create.') <> 0 then begin
              MetName := 'Create';
              Return := ''
            end
            else
              Return := 'TExtFunction';
            if pos('Instance', Return) > 1 then Return := copy(Return, 1, length(Return) - length('Instance')); // doc fault
          end;
          Args := TStringList.Create;
          Params := IfThen(IsEvent, Matches[1], Matches[0]);
          while pos('@param', Params) <> 0 do begin
            if not Extract(['@param', '{', '} ', ' ', ' '], Params, Matches) then
              break;
            Optional := pos('[', Matches[2]) = 1;
            if Matches.Count = 4 then
              Optional := Optional or (pos('(optional)', LowerCase(Matches[3])) <> 0);
            Matches[2] := Unique(FixIdent(Matches[2]), Args);
            if IsEvent and SameText(Matches[2], 'This') then
              Matches[1] := CurClass.Name;
            Args.AddObject(Matches[2], TParam.Create(Matches[2], FixType(Matches[1]), Optional));
          end;
          CurMethod := TMethod.Create(MetName, JSName, Return, Args, Static, false);
          if IsEvent then
            CurClass.Events.AddObject(CurMethod.Name, CurMethod)
          else
            DoOverloads(CurClass, CurMethod);
          continue;
        end;
        break;
      end;
    end;
  end;
  Matches.Free;
end;

procedure ReadJS(FileName : string);
var
  JS : text;
  L, Line : string;
begin
  writeln(copy(FileName, LastDelimiter('/', FileName) + 1, 100));
  assign(JS, FileName);
  reset(JS);
  Line := '';
  repeat
    readln(JS, L);
    Line := Line + ' ' + trim(L)
  until SeekEOF(JS);
  LoadElements(Line);
  close(JS);
end;

procedure LoadFixes;
var
  Fixes : text;
  Fix, FixesFile : string;
  Fields, Params : TStringList;
  I, J, K  : integer;
  NewClass : TClass;
begin
  FixesFile := 'ExtFixes.txt';
  writeln('Loading custom fixes...');
  writeln('  > ' + FixesFile);
  if FileExists(FixesFile) then begin
    assign(Fixes, FixesFile);
    reset(Fixes);
    repeat
      readln(Fixes, Fix);
      if (Fix <> '') and (Fix[1] in ['A'..'Z', 'a'..'z', '_']) then begin // else comments
        Fields := Explode(',', Fix);
        try
          I := AllClasses.IndexOf('T' + Fields[0]);
          if I <> -1 then
            with TClass(AllClasses.Objects[I]) do
              if pos('(', Fields[2]) = 1 then begin // Enums
                J := Properties.IndexOf(Fields[1]);
                if J <> -1 then
                  with TProp(Properties.Objects[J]) do begin
                    Enum := true;
                    if pos(')', Fields[2]) = 0 then begin
                      Typ  := '';
                      for K := 2 to Fields.Count-1 do begin
                        Typ := Typ + Fields[K];
                        if K <> Fields.Count-1 then
                          Typ := Typ + ', '
                      end
                    end
                    else
                      Typ := FixIdent(Fields[2], true);
                  end;
              end
              else
                if Fields.Count = 6 then // props
                  if lowercase(Fields[5]) = 'forceadd' then
                    Properties.AddObject(Fields[1] + Fields[2],
                      TProp.Create(Fields[1] + Fields[2], Fields[1], Fields[2], lowercase(Fields[3]) = 'true', lowercase(Fields[4]) = 'true', ''))
                  else begin
                    J := Properties.IndexOf(Fields[1]);
                    if J = -1 then // Add
                      Properties.AddObject(Fields[1],
                        TProp.Create(Fields[1], Fields[1], Fields[2], lowercase(Fields[3]) = 'true', lowercase(Fields[4]) = 'true', Fields[5]))
                    else // Update
                      with TProp(Properties.Objects[J]) do begin
                        Typ     := FixType(Fields[2]);
                        Static  := lowercase(Fields[3]) = 'true';
                        Config  := lowercase(Fields[4]) = 'true';
                        Default := Fields[5]
                      end
                  end
                else begin // Methods or Events
                  if SameText(Fields[2], 'Event') then begin // Events
                    J := Events.IndexOf('On' + Fields[1]);
                    if J = -1 then begin // Add
                      Params := TStringList.Create;
                      Events.AddObject('On' + Fields[1], TMethod.Create('On' + Fields[1], '', Params, false, false));
                      for K := 0 to ((Fields.Count-2) div 2)-1 do
                        Params.AddObject(Fields[K*2+3], TParam.Create(Fields[K*2+3], FixType(Fields[K*2+4]), false));
                    end
                    else // Update
                      with TMethod(Events.Objects[J]) do begin
                        For k:=0 to Params.Count-1 do
                          Params.Objects[k].Free;
                        Params.Clear;
                        for K := 0 to ((Fields.Count-2) div 2)-1 do
                          Params.AddObject(Fields[K*2+3], TParam.Create(Fields[K*2+3], FixType(Fields[K*2+4]), false));
                      end;
                  end
                  else begin
                    if Fields.Count < 5 then begin
                      writeln(^M^J'*** WARNING: Class ', Fields[0], ' already exists. ***'^M^J);
                      Fields.Free;
                      continue;
                    end;
                    J := Methods.IndexOf(Fields[1]);
                    if J = -1 then begin // Add
                      Params := TStringList.Create;
                      Methods.AddObject(Fields[1], TMethod.Create(Fields[1], FixType(Fields[2]), Params, lowercase(Fields[3]) = 'true', lowercase(Fields[4]) = 'true'));
                      for K := 0 to ((Fields.Count-4) div 3)-1 do
                        Params.AddObject(Fields[K*3+5], TParam.Create(Fields[K*3+5], FixType(Fields[K*3+6]), lowercase(Fields[K*3+7]) = 'true'))
                    end
                    else // Update
                      with TMethod(Methods.Objects[J]) do begin
                        Return   := FixType(Fields[2]);
                        Static   := lowercase(Fields[3]) = 'true';
                        Overload := lowercase(Fields[4]) = 'true';
                        Params.Clear;
                        for K := 0 to ((Fields.Count-4) div 3)-1 do
                          Params.AddObject(Fields[K*3+5], TParam.Create(Fields[K*3+5], FixType(Fields[K*3+6]), lowercase(Fields[K*3+7]) = 'true'))
                      end;
                  end;
                end
          else begin // Create new Class
            I := Units.IndexOf(Fields[2]);
            if I <> -1 then begin
              NewClass := TClass.Create(Fields[0], Fields[1], Fields[2]);
              NewClass.JSName := Fields[3];
              AllClasses.AddObject(NewClass.Name, NewClass);
              TUnit(Units.Objects[I]).Classes.AddObject(NewClass.Name, NewClass)
            end
            else
              writeln('Unit: ', Fields[2], ' not found. Fix record: ', Fix);
          end;
        finally
          Fields.Free;
        end;
      end;
    until SeekEOF(Fixes);
    close(Fixes);
  end
  else
    writeln(FixesFile + ' file not found.');
end;

procedure FixEvents;

  procedure ChangeSimpleClassToFinalClass(var ATypTo : string);
  var
    I : Integer;
    TypFrom : string;
  begin
    if SameText(ATypTo, 'TError') then
      ATypTo := 'string'
    else
      if pos('/', ATypTo) > 0 then
        ATypTo := FixType(copy(ATypTo, 1, pos('/', ATypTo)-1))
      else
        if (CaseOf(ATypTo, ['integer', 'string', 'boolean', 'double', 'TDateTime', 'TExtObject', 'TExtObjectList', 'TArray', 'TExtObject']) = -1) and
           (AllClasses.IndexOf(ATypTo) < 0) then begin
          TypFrom := copy(ATypTo, 2, Maxint);
          for I := 0 to AllClasses.Count - 1 do
            if SameText(TClass(AllClasses.Objects[I]).SimpleName, TypFrom) then begin
              ATypTo := TClass(AllClasses.Objects[I]).Name;
              exit;
            end;
        end;
  end;

var
  I, J, K: Integer;
begin
  for I := 0 to AllClasses.Count - 1 do
    with TClass(AllClasses.Objects[I]) do
      if Parent <> '' then
        for J := 0 to Events.Count - 1 do
          with TMethod(Events.Objects[J]) do
            for K := 0 to Params.Count - 1 do
              with TParam(Params.Objects[K]) do
                ChangeSimpleClassToFinalClass(Typ);
end;

procedure FixHeritage;
var
  P : string;
  I, J : integer;
  C : TClass;
begin
  for I := 0 to AllClasses.Count - 1 do begin
    C := TClass(AllClasses.Objects[I]);
    P := C.Parent;
    write(I, ^M);
    while P <> '' do begin
      J := AllClasses.IndexOf(P);
      if J = -1 then begin
        writeln('Parent: ', P, ' not found at class: ', C.Name);
        C.Parent := '';
        break;
      end;
      C := TClass(AllClasses.Objects[J]);
      if P = C.Parent then begin
        writeln('Parent: ', P, ' is the same at class: ', C.Name, ' and its descendent' );
        C.Parent := '';
        break
      end
      else
        P := C.Parent;
    end;
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
  if Optional then
    Result := Equal + DefValue(Typ)
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
  if Params.Count <> 0 then begin
    write(Pas, '(');
    Op := false;
    for I := 0 to Params.Count-1 do begin
      with TParam(Params.Objects[I]) do begin
        if Optional then Op := true;
        write(Pas, Name, ' : ', Typ, WriteOptional(Op, Typ));
      end;
      if I <> Params.Count-1 then write(Pas, '; ');
    end;
    write(Pas, ')');
  end;
end;

function WriteMethodSignature(Method : TMethod; pClassName : string = '') : boolean;
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
    if Name = 'Create' then
      write(Pas, T, 'constructor ', pClassName, 'Create')
    else
      write(Pas, T, IfThen(Static, 'class ', ''), IfThen(Return = 'TVoid', 'procedure ', 'function '), pClassName, Name);
    WriteParams(Params);
    write(Pas, IfThen((Return = 'TVoid') or (Return = ''), ';', ' : ' + Return + ';'), IfThen(Overload and (pClassName = ''), ' overload;', ''));
    if pClassName = '' then begin
      if Name = 'DestroyJS' then
        if Params.Count = 0 then
          write(Pas, ' override;')
        else
          write(Pas, ' reintroduce;');
      writeln(Pas);
    end;
  end;
  Result := true;
end;

procedure WriteClassType(Cls : TClass);
var
  I : integer;
  HasEnum : boolean;
begin
  with Cls do begin
    if Events.Count > 0 then begin
      writeln(Pas, Tab, '// Procedural types for events ', Name);
      for I := 0 to Events.Count - 1 do
        with TMethod(Events.Objects[I]) do begin
          write(Pas, Tab, Cls.Name, Name, ' = procedure');
          WriteParams(Params);
          writeln(Pas, ' of object;');
        end;
      writeln(Pas);
    end;
    // Write Enumerations
    HasEnum := false;
    for I := 0 to Properties.Count-1 do
      with TProp(Properties.Objects[I]) do
        if Enum and (Typ[1] = '(') then begin
          if not HasEnum then writeln(Pas, Tab, '// Enumerated types for properties');
          HasEnum := true;
          writeln(Pas, Tab, Cls.Name, Name, ' = ', Typ, ';');
          Typ := Cls.Name + Name;
        end;
    if HasEnum then writeln(Pas);
    writeln(Pas, Tab, Name, ' = class(', IfThen(Parent = '', 'TExtFunction', Parent), ')');
    if (Properties.Count > 0) or (Events.Count > 0) then writeln(Pas, Tab, 'private');
    // Write private fields
    for I := 0 to Properties.Count-1 do
      with TProp(Properties.Objects[I]) do
        if not Static then writeln(Pas, Tab(2), 'F', Name, ' : ', Typ, ';', IfThen(Default <> '', ' // ' + Default, ''));
    for I := 0 to Events.Count-1 do
      with TMethod(Events.Objects[I]) do
        writeln(Pas, Tab(2), 'F', Name, ' : ', Cls.Name, Name, ';');
    // Write Set procedures
    for I := 0 to Properties.Count-1 do
      with TProp(Properties.Objects[I]) do
        if not Static then writeln(Pas, Tab(2), 'procedure SetF', Name, '(Value : ', Typ, ');');
    for I := 0 to Events.Count-1 do
      with TMethod(Events.Objects[I]) do
         writeln(Pas, Tab(2), 'procedure SetF', Name, '(Value : ', Cls.Name, Name, ');');
    if Defaults or Arrays or Objects or (Events.Count > 0) then writeln(Pas, Tab, 'protected');
    if Defaults or Arrays or Objects then writeln(Pas, Tab(2), 'procedure InitDefaults; override;');
    if Events.Count > 0 then writeln(Pas, Tab(2), 'procedure HandleEvent(const AEvtName: string); override;');
    writeln(Pas, Tab, 'public');
    writeln(Pas, Tab(2), 'function JSClassName : string; override;');
    // Write class properties
    for I := 0 to Properties.Count-1 do
      with TProp(Properties.Objects[I]) do
        if Static then writeln(Pas, Tab(2), 'class function ', Name, ' : ', Typ, ';');
    if Singleton then
      for I := 0 to Methods.Count-1 do
        with TMethod(Methods.Objects[I]) do
          if (Return + 'Singleton') = Cls.Name then Return := Return + 'Singleton';
    writeln(Pas, Tab(2), '{$IFDEF FPC}constructor AddTo(List : TExtObjectList);{$ENDIF}');
    // Write methods
    for I := 0 to Methods.Count-1 do
      WriteMethodSignature(TMethod(Methods.Objects[I]));
    if Arrays or Objects then writeln(Pas, Tab(2), 'destructor Destroy; override;');
    {$IFDEF USES_PUBLISHED}if (Properties.Count > 0) or (Events.Count > 0) then writeln(Pas, Tab, 'published');{$ENDIF}
    // Write properties
    for I := 0 to Properties.Count-1 do
      with TProp(Properties.Objects[I]) do
        if not Static then writeln(Pas, Tab(2), 'property ', Name, ' : ', Typ, ' read F', Name, ' write SetF', Name, ';');
    // Write events
    for I := 0 to Events.Count-1 do
      with TMethod(Events.Objects[I]) do
        writeln(Pas, Tab(2), 'property ', Name, ' : ', Cls.Name, Name, ' read F', Name, ' write SetF', Name, ';');
    writeln(Pas, Tab, 'end;'^M^J);
  end;
end;

// Add boolean additional param to identify TExtFunction param type
function AddBoolParam(Type_ : string) : string; begin
  Result := '';
  if (Type_ <> '') and (Type_[1] = 'T') and (pos('TArrayOf', Type_) = 0) and
     (pos('.' + Type_ + '.', '.TDateTime.TRegExp.TRegion.Tel.TVisMode.') = 0) then
    if Type_ = 'TExtFunction' then
      Result := ', true'
    else
      Result := ', false';
end;

function ParamsToJSON(Params : TStringList; Config : boolean = true) : string;
var
  I : integer;
  InitCommonParam : boolean;
begin
  if Params.Count <> 0 then begin
    InitCommonParam := true;
    Result := '(''';
    for I := 0 to Params.Count-1 do
      with TParam(Params.Objects[I]) do begin
        if Typ <> 'TExtObjectList' then begin
          if InitCommonParam then begin
            Result := Result + ' + VarToJSON([';
            InitCommonParam := false;
          end
          else
            if I > 0 then Result := Result + ', ';
          Result := Result + IfThen(Typ = 'TRegExp', '#3 + ', '') + Name + AddBoolParam(Typ);
        end
        else begin
          if not InitCommonParam then Result := Result + ']) + '',''';
          Result := Result + ' + VarToJSON(' + Name + ')';
          if I < (Params.Count-1) then Result := Result + ' + '',''';
          InitCommonParam := true;
        end;
      end;
    if not InitCommonParam then Result := Result + '])';
    Result := Result + ' + '');';
  end
  else
    Result := IfThen(Config, '({});', '();');
end;

function SortByInheritLevel(List : Classes.TStringList; I, J : integer) : integer; begin
  Result := TClass(List.Objects[I]).InheritLevel - TClass(List.Objects[J]).InheritLevel
end;

procedure DeclareSingletons(pUnit : TUnit);
var
  I : integer;
  HasSingleton : boolean;
begin
  HasSingleton := false;
  with pUnit do
    for I := 0 to Classes.Count-1 do
      with TClass(Classes.Objects[I]) do begin
        if Singleton then begin
          if not HasSingleton then begin
            writeln(Pas, 'var');
            HasSingleton := true
          end;
          writeln(Pas, Tab, copy(Name, 2, length(Name) - length('Singleton') - 1), ' : ', Name, ';');
        end;
      end;
  if HasSingleton then writeln(Pas);
end;

procedure CreateSingletons(pUnit : TUnit);
var
  I : integer;
  HasSingleton : boolean;
begin
  HasSingleton := false;
  with pUnit do
    for I := 0 to Classes.Count-1 do
      with TClass(Classes.Objects[I]) do
        if Singleton then begin
          if not HasSingleton then begin
            writeln(Pas, 'initialization');
            HasSingleton := true
          end;
          writeln(Pas, Tab, copy(Name, 2, length(Name) - length('Singleton') - 1), ' := ', Name, '.CreateSingleton;');
        end;
end;

procedure FreeSingletons(pUnit : TUnit);
var
  I : integer;
  HasSingleton : boolean;
begin
  HasSingleton := false;
  with pUnit do
    for I := Classes.Count-1 downto 0 do
      with TClass(Classes.Objects[I]) do
        if Singleton then begin
          if not HasSingleton then begin
            writeln(Pas);
            writeln(Pas, 'finalization');
            HasSingleton := true
          end;
          writeln(Pas, Tab, copy(Name, 2, length(Name) - length('Singleton') - 1), '.Destroy;');
        end;
end;

procedure WriteUnits;

  procedure WriteEventParamsAdapter(Event : TMethod);
  var
    I : Integer;
    First : boolean;
  begin
    write(Pas, '[');
    First := true;
    for I := 0 to Event.Params.Count - 1 do
      with TParam(Event.Params.Objects[I]) do
        if not AnsiEndsText('Singleton', Name) then begin
          write(Pas, IfThen(not First, ',', ''), '''', Name, '''');
          First := false;
          // Simple types
          if CaseOf(Typ, ['integer', 'string', 'boolean', 'double', 'TDateTime']) <> -1 then
            write(Pas, ', ''%', I, '''')
          else // Objects, just get the name, server will convert in an object
            write(Pas, ', ''%', I, '.nm''')
        end;
    write(Pas, ']');
  end;

  procedure WriteEventParamsConverter(Event : TMethod);
  var
    I : Integer;
  begin
    write(Pas, '(');
    for I := 0 to Event.Params.Count - 1 do begin
      if I > 0 then write(Pas, ', ');
      with TParam(Event.Params.Objects[I]) do
        if AnsiEndsText('Singleton', Typ) then
          write(Pas, Copy(Typ, 2, length(Typ) - 10)) // Gets Singleton object
        else // Simple types
          if CaseOf(Typ, ['integer', 'string', 'boolean', 'double', 'TDateTime']) <> -1 then
            write(Pas, 'ParamAs', Typ, '(''', Name, ''')')
          else // Objects, just get the name, server will convert in an object
            write(Pas, Typ, '(ParamAsObject(''', Name, '''))');
    end;
    write(Pas, ')');
  end;

var
  I, J, K, M : integer;
  CName, CJSName, BoolParam, RegExParam : string;
begin
  for I := 0 to Units.Count-1 do
    with TUnit(Units.Objects[I]) do begin
      assign(Pas, Name + '.pas');
      rewrite(Pas);
      writeln(Pas, 'unit ', Name, ';'^M^J);
      writeln(Pas, '// Generated by ExtToPascal v.', ExtPascalVersion, ', at ', DateTimeToStr(Now));
      writeln(Pas, '// from "', paramstr(1), ^M^J);
      writeln(Pas, 'interface'^M^J^M^J'uses'^M^J, Tab, 'StrUtils, ExtPascal, ExtPascalUtils', UsesList, ';'^M^J);
      if Name = 'Ext' then begin
        writeln(Pas, 'const');
        writeln(Pas, Tab, 'SourcePath = ''/src'';'^M^J);
      end;
      {$IFDEF USES_PUBLISHED}writeln(Pas, '{$M+}');{$ENDIF}
      writeln(Pas, 'type');
      Classes.CustomSort(SortByInheritLevel);
      for J := 0 to Classes.Count-1 do // forward classes
        writeln(Pas, Tab, TClass(Classes.Objects[J]).Name, ' = class;');
      writeln(Pas, Tab, 'TExtDataRecord = TExtDataModel;');
      writeln(Pas);
      for J := 0 to Classes.Count-1 do
        WriteClassType(TClass(Classes.Objects[J]));
      DeclareSingletons(TUnit(Units.Objects[I]));
      writeln(Pas, 'implementation'^M^J);
      for J := 0 to Classes.Count-1 do
        with TClass(Classes.Objects[J]) do begin
          CName   := Name;
          CJSName := JSName;
          for K := 0 to Properties.Count-1 do // Write Set procedures implementation
            with TProp(Properties.Objects[K]) do begin
              if not Static then begin
                writeln(Pas, 'procedure ', CName, '.SetF', Name, '(Value : ', Typ, '); begin');
                writeln(Pas, Tab, 'F', Name, ' := Value;');
                RegExParam := IfThen(LowerCase(Typ) = 'tregexp', '#3 +', '');
                BoolParam  := AddBoolParam(Typ);
                if (BoolParam = ', false') and not Enum and (RegExParam = '') then writeln(Pas, Tab, 'Value.DeleteFromGarbage;');
                if Config then begin
                  // If there is an alternative method, and its parameters are
                  // compatible, implement an workaround to reconfig objects created in a previous request
                  M := Methods.IndexOf('Set' + Name);
                  if (M <> -1) and (TMethod(Methods.Objects[M]).Params.Count > 0) and
                    (SameText(Typ, TParam(TMethod(Methods.Objects[M]).Params.Objects[0]).Typ)) then begin
                    writeln(Pas, Tab, 'if not ConfigAvailable(JSName) then');
                    with TMethod(Methods.Objects[M]) do begin
                      write(Pas, Tab(2), Name, '(Value');
                      for M := 1 to Params.Count - 1 do
                        if TParam(Params.Objects[M]).Optional then
                          break
                        else
                          Write(Pas, ', ', DefValue(TParam(Params.Objects[M]).Typ));
                      writeln(Pas, ')');
                      writeln(Pas, Tab, 'else');
                    end;
                    write(Pas, Tab);
                  end;
                  if not Enum then
                    writeln(Pas, Tab, 'JSCode(''', JSName, ':'' + ',
                      IfThen(pos('TArrayOf', Typ) = 0, 'VarToJSON([' + RegExParam + 'Value' + BoolParam + ']', 'ArrayToJSON(' + RegExParam + 'Value'), '));')
                  else
                    writeln(Pas, Tab, 'JSCode(''', JSName, ':"'' + EnumToJSString(TypeInfo(' + Typ + '), ord(Value)) + ''"'');');
                end
                else
                  if not Enum then
                    writeln(Pas, Tab, 'JSCode(JSName + ''.', JSName, '='' + ',
                      IfThen(pos('TArrayOf', Typ) = 0, 'VarToJSON([' + RegExParam + 'Value' + BoolParam + ']', 'ArrayToJSON(' + RegExParam + 'Value'), ') + '';'');')
                  else
                    writeln(Pas, Tab, 'JSCode(JSName + ''.', JSName, '="'' + EnumToJSString(TypeInfo(' + Typ + '), ord(Value)) + ''";'');');
                writeln(Pas, 'end;'^M^J);
              end;
            end;

            for K := 0 to Events.Count-1 do // Write Set procedures implementation
              with TMethod(Events.Objects[K]) do
                if not Static then begin
                  writeln(Pas, 'procedure ', CName, '.SetF', Name, '(Value : ', CName, Name, '); begin');
                  writeln(Pas, Tab, 'if Assigned(F', Name, ') then');
                  writeln(Pas, Tab(2), 'JSCode(JSName+'+'''.events ["'+JSName+'"].listeners=[];'');');
                  writeln(Pas, Tab, 'if Assigned(Value) then');
                  write(Pas, Tab(2), 'On(''', JSName, ''', Ajax(''', JSName, ''', ');
                  WriteEventParamsAdapter(TMethod(Events.Objects[K]));
                  writeln(Pas, ', true));');
                  writeln(Pas, Tab, 'F', Name, ' := Value;');
                  writeln(Pas, 'end;'^M^J);
                end;
          writeln(Pas, 'function ' + Name + '.JSClassName : string; begin');
          writeln(Pas, Tab, 'Result := ''' + JSName + ''';');
          writeln(Pas, 'end;'^M^J);
          for K := 0 to Properties.Count-1 do // Write class properties implementation
            with TProp(Properties.Objects[K]) do
              if Static then begin
                write(Pas, 'class function ', CName, '.', Name, ' : ', Typ, ';');
                if WriteOptional(true, Typ, '') = 'nil' then begin
                  writeln(Pas, ^M^J'const');
                  writeln(Pas, Tab, 'l', Name, ' : ', Typ, ' = nil;');
                  writeln(Pas, 'begin');
                  writeln(Pas, Tab, 'if l', Name, ' = nil then l', Name, ' := ', Typ, '.CreateSingleton(''' + CJSName + '.', JSName, ''');');
                  writeln(Pas, Tab, 'Result := l', Name);
                end
                else begin
                  writeln(Pas, ' begin');
                  writeln(Pas, Tab, 'Result := ', IfThen(Default <> '', Default, WriteOptional(true, Typ, '')));
                end;
                writeln(Pas, 'end;'^M^J);
              end;
          if Defaults or Arrays or Objects then begin // write Init method
            writeln(Pas, 'procedure ', CName, '.InitDefaults; begin');
            writeln(Pas, Tab, 'inherited;');
            for K := 0 to Properties.Count-1 do
              with TProp(Properties.Objects[K]) do
                if not Static then
                  if (Default <> '') and not Enum then
                    writeln(Pas, Tab, 'F', Name, ' := ', Default, ';')
                  else
                    if Typ = 'TExtObjectList' then
                      writeln(Pas, Tab, 'F', Name, ' := TExtObjectList.Create(Self, ''', JSName, ''');')
                    else
                      if (pos('TExt', Typ) = 1) and (Typ <> 'TExtFunction') and not Enum then
                        writeln(Pas, Tab, 'F', Name, ' := ', Typ, '.CreateInternal(Self, ''', JSName, ''');');
            writeln(Pas, 'end;'^M^J);
          end;
          writeln(Pas, '{$IFDEF FPC}constructor ' + CName + '.AddTo(List : TExtObjectList);begin inherited end;{$ENDIF}'^M^J);
          for K := 0 to Methods.Count-1 do // Write methods
            if WriteMethodSignature(TMethod(Methods.Objects[K]), Name) then begin
              writeln(Pas, ' begin');
              with TMethod(Methods.Objects[K]) do
                if Return = '' then begin // Write constructors
                  if AltCreate then // ExtJS fault
                    writeln(Pas, Tab, 'CreateVarAlt(JSClassName + ''.create', ParamsToJSON(Params), ''');')
                  else begin
                    if (Params.Count = 1) and (TParam(Params.Objects[0]).Name = 'Config') and (TParam(Params.Objects[0]).Typ = 'TExtObject') then
                      writeln(Pas, Tab, 'if Config = nil then CreateVar(JSClassName + ''({});'') else');
                    writeln(Pas, Tab, 'CreateVar(JSClassName + ''', ParamsToJSON(Params), ''');');
                  end;
                  writeln(Pas, Tab, 'InitDefaults;');
                end
                else begin // Write class and instance methods
                  writeln(Pas, Tab, 'JSCode(', IfThen(Static, 'JSClassName', 'JSName'), ' + ''.', JSName, ParamsToJSON(Params, false),
                    ''', ''' + CName + ''');');
                  if Return <> 'TVoid' then
                    writeln(Pas, Tab, 'Result := Self;')
                end;
              writeln(Pas, 'end;'^M^J);
            end;
          if Arrays or Objects then begin // Write destructor
            writeln(Pas, 'destructor ', CName, '.Destroy; begin');
            writeln(Pas, Tab, 'try');
            for K := 0 to Properties.Count-1 do
              with TProp(Properties.Objects[K]) do
                if not Static and not Enum and (pos('TExt', Typ) = 1) and (Typ <> 'TExtFunction') then
                  writeln(Pas, Tab(2), 'F' + Name + '.Free;');
            writeln(Pas, Tab, 'except end;');
            writeln(Pas, Tab, 'inherited;');
            writeln(Pas, 'end;'^M^J);
          end;
          if Events.Count > 0 then begin // write Event handler
            writeln(Pas, 'procedure ', CName, '.HandleEvent(const AEvtName : string); begin');
            writeln(Pas, Tab, 'inherited;');
            for K := 0 to Events.Count - 1 do
              with TMethod(Events.Objects[K]) do begin
                if K > 0 then
                  write(Pas, Tab, 'else ')
                else
                  write(Pas, Tab);
                writeln(Pas, 'if (AEvtName = ''', JSName, ''') and Assigned(F', Name, ') then');
                write(Pas, Tab(2), 'F', Name);
                WriteEventParamsConverter(TMethod(Events.Objects[K]));
                if K = Events.Count - 1 then
                  writeln(Pas, ';')
                else
                  writeln(Pas);
              end;
            writeln(Pas, 'end;'^M^J);
          end;
        end;
      CreateSingletons(TUnit(Units.Objects[I]));
      FreeSingletons(TUnit(Units.Objects[I]));
      write(Pas, 'end.');
      close(Pas);
    end;
end;

var
  Files : integer;

procedure WrapJSDir(const ADir : String);
var
  F : TSearchrec;
begin
  if FindFirst(ADir + '/*.js', faAnyFile, F) = 0 then begin
    repeat
      write('  > ');
      ReadJS(ADir + '/' + F.Name);
      Inc(Files);
    until FindNext(F) <> 0;
    FindClose(F);
  end;
end;

var
  T : TDateTime;

procedure DoTree(Tree : string);
var
  F : TSearchRec;
begin
  WrapJSDir(Tree);
  try
    if FindFirst(Tree + '/*', faDirectory, F) = 0 then begin
      while pos('.', F.Name) <> 0 do
        if FindNext(F) <> 0 then exit;
      repeat
        if pos('.', F.Name) = 0 then DoTree(Tree + '/' + F.Name);
      until FindNext(F) <> 0;
    end;
  finally
    FindClose(F)
  end;
end;

begin
  if (ParamCount = 0) or (ParamCount > 1) or (ParamStr(1) = '-h') or (ParamStr(1) = '/?') then begin
    Writeln('Usage: ', ChangeFileExt(ExtractFileName(ParamStr(0)), ''), ' inputdir');
    Writeln('where');
    Writeln('  inputdir directory with Ext JS 4.x sources');
    Halt(1);
  end;
  T := now;
  AllClasses := TStringList.Create;
  try
    AllClasses.Sorted := true;
    Units := TStringList.Create;
    try
      Units.Sorted := true;
      writeln('ExtToPascal - Ext JS classes to Pascal unit wrapper, version ', ExtPascalVersion);
      writeln('(c) 2008-2013 by Wanderlan Santos dos Anjos, BSD license');
      writeln('http://extpascal.googlecode.com/'^M^J);
      writeln('Reading Ext JS source files...');
      Files := 0;
      DoTree(AnsiReplaceStr(ParamStr(1), '\', '/'));
      if Files <> 0 then begin
        writeln(Files, ' .js files found.');
        writeln('Fixing event prototypes...');
        FixHeritage;
        FixEvents;
//        LoadFixes;
        writeln('Writing unit file(s)...');
        WriteUnits;
        writeln(AllClasses.Count, ' Ext JS classes wrapped.');
        writeln('Unit file generated.');
        writeln(FormatDateTime('ss.zzz', Now-T), ' seconds elapsed.');
        writeln('Done. Press Enter.');
      end
      else begin
        writeln('Ext JS source files not found at ' + ParamStr(1) + '/*.js');
        writeln('Aborted! Press Enter.');
      end;
      readln;
    finally
      FreeStringList(Units);
    end;
  finally
    FreeStringList(AllClasses);
    Flush(Output);
  end;
end.

