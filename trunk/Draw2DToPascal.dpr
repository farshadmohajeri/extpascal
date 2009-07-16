{
Draw2DToPascal is a parser that scans Draw2D documentation, in HTML format, and that creates a Object Pascal wrapper upon Draw2D classes and widgets
Author: Wanderlan Santos dos Anjos (wanderlan.anjos@gmail.com)
Date: jul-2009
License: BSD<extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
program Draw2DToPascal;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}
{$IFDEF MSWINDOWS}{$APPTYPE CONSOLE}{$ENDIF}

uses
  SysUtils, StrUtils, Classes, Math, ExtPascalUtils;

{.$DEFINE USESPUBLISHED}

function FixReserved(S : string) : string;
const
  Reserved = '.and.array.as.asm.begin.case.class.const.constructor.destructor.destroy.dispinterface.div.do.downto.else.end.except.exports.'+
    'false.file.finalization.finally.for.function.goto.if.implementation.in.inherited.initialization.inline.interface.is.label.library.'+
    'mod.nil.not.object.of.or.out.packed.procedure.program.property.raise.record.repeat.resourcestring.set.shl.shr.string.then.'+
    'threadvar.to.try.true.type.unit.until.uses.var.while.with.xor.';
begin
  if pos('.' + lowercase(S) + '.', Reserved) = 0 then
    Result := S
  else
    Result := S + 'JS';
end;

function FixIdent(Ident : string; IsType : boolean = false) : string;
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
      if (I <> 1) and (I <> length(Ident)) and (Ident[I] in ['(', '[', '{', ')', ']', '}']) then
        break
      else
        if I < length(Ident) then Ident[I+1] := UpCase(Ident[I+1]);
  if IsType then begin
    if (Result <> '') and (pos('TExt', Result) = 0) then Result := 'T' + Result
  end
  else
    Result := FixReserved(Result);
end;

function FixType(Ident : string) : string;
var
  I : integer;
begin
  if Ident <> '' then
    case CaseOf(Ident, ['string', 'number', 'integer', 'object', 'boolean', 'function', 'mixed', 'array', 'object...', 'date', 'float', 'int']) of
      0, 6     : Result := 'string';
      1, 2, 11 : Result := 'Integer';
      3        : Result := 'TExtObject';
      4        : Result := 'Boolean';
      5        : Result := 'TExtFunction';
      7, 8     : Result := 'TExtObjectList';
      9        : Result := 'TDateTime';
      10       : Result := 'Double';
    else
      if pos('mixedcollection', lowercase(Ident)) <> 0 then
        Result := 'TExtObjectList'
      else begin
        I := LastDelimiter('/[:', Ident);
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
  TClass = class
    Name, JSName, Parent, SimpleName : string;
    Arrays, Objects : boolean;
    Properties, Methods, Events : TStringList;
    constructor Create(pName : string; pParent : string = '');
    function InheritLevel : integer;
    procedure AddCreate;
  end;

  TProp = class
    Name, JSName, Typ, Default : string;
    Static, Enum : boolean;
    constructor Create(pName, pJSName, pType : string; pStatic : boolean);
  end;

  TMethod = class
    Name, JSName, Return : string;
    Params : TStringList;
		Static : boolean;
	  constructor Create(pName, pReturn : string; pParams : TStringList; pStatic : boolean); overload;
	  constructor Create(pName, pJSName, pReturn : string; pParams : TStringList; pStatic : boolean); overload;
  end;

  TParam = class
	  Name, Typ : string;
	  Optional  : boolean;
    constructor Create(pName, pType : string; pOptional : boolean);
  end;

var
  AllClasses : TStringList;

constructor TClass.Create(pName, pParent : string); begin
  Name       := FixIdent(pName, true);
  SimpleName := Copy(pName, LastDelimiter('.', pName) + 1, MaxInt);
  JSName     := FixJSName(pName);
  Parent     := FixIdent(pParent, true);
  Properties := TStringList.Create;
  Methods    := TStringList.Create;
  Events     := TStringList.Create;
end;

procedure TClass.AddCreate; begin
  if Methods.IndexOf('Create') = -1 then
    Methods.AddObject('Create', TMethod.Create('Create', '', TStringList.Create, false));
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
    if I = -1 then begin
      writeln('Parent: ', P, ' not found at class: ', Name);
      exit;
    end;
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
  case CaseOf(Typ, ['string', 'integer', 'double', 'TDateTime', 'boolean']) of
    0    : Result := '''''';
    1..3 : Result := '0';
    4    : Result := 'false';
  else
    Result := 'nil'
  end;
end;

constructor TProp.Create(pName, pJSName, pType : string; pStatic : boolean); begin
  Name    := FixIdent(pName);
  JSName  := FixJSName(pJSName);
  Static  := pStatic or IsUpper(pName);
  Typ     := FixType(pType);
end;

constructor TParam.Create(pName, pType : string; pOptional : boolean); begin
  Name     := FixIdent(IfThen(pName = '', 'Param', pName));
  Typ      := pType;
  Optional := pOptional;
end;

constructor TMethod.Create(pName, pReturn : string; pParams : TStringList; pStatic : boolean); begin
  Name     := FixIdent(pName);
  JSName   := FixJSName(pName);
  Params   := pParams;
  Static   := pStatic;
  if pReturn <> '' then Return := 'TExtFunction';
end;

constructor TMethod.Create(pName, pJSName, pReturn : string; pParams : TStringList; pStatic : boolean);
var
  I : integer;
begin
  Name   := pName;
  JSName := FixJSName(pJSName);
  Params := pParams;
  Static := pStatic;
  if pReturn <> '' then Return := 'TExtFunction';
  I := LastDelimiter('.', Name);
  if I <> 0 then Name := copy(Name, I+1, length(Name));
  Static := I <> 0;
end;

function Unique(S : string; List : TStringList) : string; begin
  Result := S;
  while List.IndexOf(Result) <> -1 do Result := Result + '_'
end;

function ClearHRef(S : string) : string;
var
  Match : TStringList;
begin
  if pos('<a', lowercase(S)) <> 0 then begin
    Match := TStringList.Create;
    Extract(['">', '</'], S, Match);
    Result := Match[0];
    if pos('draw2d.', Result) = 1 then delete(Result, 1,  7);
    Match.Free;
  end
  else
    Result := S;
end;

procedure LoadElements(Line : string);
const
  CurClass  : TClass      = nil;
  CurProp   : TProp       = nil;
  CurMethod : TMethod     = nil;
  PropName  : string      = '';
  MetName   : string      = '';
  PropTypes : TStringList = nil;
  Params    : TStringList = nil;
var
  State : (Initial, InClass, InProperties, InConstructor, InMethods);
  Return, JSName, Param, ParamType : string;
  Matches, Args : TStringList;
  Static  : boolean;
  I : integer;
begin
  State := Initial;
  Matches := TStringList.Create;
  while true do begin
    case State of
      Initial :
        if Extract(['<TITLE>draw2d.', '</TITLE>'], Line, Matches) then begin
          CurClass := TClass.Create(ClearHRef(Matches[0]));
          State    := InClass;
          continue;
        end;
      InClass : begin
        if Extract(['extends <a href=''draw2d.', '.html'], Line, Matches) then
          CurClass.Parent := FixIdent(Matches[0], true);
        if Before('END METHOD SUMMARY', '<CODE>', Line) then
          break
        else
          if Before('END CONSTRUCTOR SUMMARY', '<CODE>', Line) then
            State := InMethods
          else
            if Before('END FIELD SUMMARY', '<CODE>', Line) then
              State := InConstructor
            else
              State := InProperties;
        continue;
      end;
      InProperties :
        if Extract(['<CODE>', '</CODE>', '<B>', '</B>'], Line, Matches) then begin
          PropName := ClearHRef(Matches[2]);
          Static   := pos('static', Matches[0]) <> 0;
          if IsUppercase(PropName) then Static := true;
          CurProp := TProp.Create(PropName, PropName, Matches[0], Static);
          CurClass.Properties.AddObject(FixIdent(PropName), CurProp);
          if (PropTypes <> nil) and (PropName <> 'config') and Extract(['<td class="msource">', '</td>'], Line, Matches) then begin
            if Matches[0][1] = '<' then
              with CurClass.Properties do
                for I := 0 to PropTypes.Count-1 do
                  Delete(IndexOf(FixIdent(PropName + IfThen(I = 0, '', PropTypes[I])))); // delete inherited properties
            FreeAndNil(PropTypes);
          end;
          if Before('END FIELD SUMMARY', '<CODE>', Line) then begin
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
            State := InConstructor;
          end;
          continue;
        end;
      InConstructor :
        if Extract(['<CODE>', '<B>', '</B>(', ')</CODE>'], Line, Matches) then begin
          JSName  := ClearHRef(Matches[1]);
          MetName := 'Create';
          Params  := Explode(',', Matches[2]);
          Args    := TStringList.Create;
          for I := 0 to Params.Count-1 do begin
            Param := Params[I];
            if Extract(['<', '>'], Param, Matches) then
              ParamType := Matches[0]
            else
              ParamType := 'object';
            Param := Unique(FixIdent(Param), Args);
            Args.AddObject(Param, TParam.Create(Param, FixType(ParamType), false));
          end;
          Params.Free;
          CurClass.Methods.AddObject('Create', TMethod.Create(MetName, JSName, '', Args, false));
          State := InMethods;
          continue;
        end;
      InMethods :
        if Extract(['<CODE>', '</CODE>', '<CODE>', '<B>', '</B>(', ')</CODE>'], Line, Matches) then begin
          JSName  := ClearHRef(Matches[3]);
          MetName := Unique(FixIdent(JSName), CurClass.Properties);
          MetName := Unique(MetName, CurClass.Methods);
          Return  := ClearHRef(Matches[0]);
          Static  := pos('static', Return) <> 0;
          Params  := Explode(',', Matches[4]);
          Args    := TStringList.Create;
          for I := 0 to Params.Count-1 do begin
            Param := Params[I];
            if Extract(['<', '>'], Param, Matches) then
              ParamType := Matches[0]
            else
              ParamType := 'object';
            Param := Unique(FixIdent(Param), Args);
            Args.AddObject(Param, TParam.Create(Param, FixType(ParamType), false));
          end;
          Params.Free;
          CurMethod := TMethod.Create(MetName, JSName, Return, Args, Static);
          if pos('on', CurMethod.Name) = 1 then
            CurClass.Events.AddObject(CurMethod.Name, CurMethod)
          else
            CurClass.Methods.AddObject(CurMethod.Name, CurMethod);
          if Before('END METHOD SUMMARY', '<CODE>', Line) then break;
          continue;
        end;
    end;
    break;
  end;
  AllClasses.AddObject(CurClass.Name, CurClass);
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
    if (J-I) > 15 then J := I+1;
    delete(Result, I, J-I);
    Result[I] := ' ';
    I := pos('&', Result);
  end;
  I := pos('|', Result);
  if I <> 0 then Result := AnsiReplaceText(Result, '|', '/');
end;

procedure ReadHtml(FileName : string);
var
  Html : text;
  L, Line : string;
begin
  writeln(FileName);
  assign(Html, FileName);
  reset(Html);
  Line := '';
  repeat
    readln(Html, L);
    Line := Line + trim(L)
  until SeekEOF(Html);
  LoadElements(FixHtml(Line));
  close(Html);
end;

procedure LoadFixes;
var
  Fixes : text;
  Fix, FixesFile : string;
  Fields, Params : TStringList;
  I, J, K  : integer;
  NewClass : TClass;
begin
  exit;
  FixesFile := 'Draw2DFixes.txt';
  writeln('Reading ' + FixesFile);
  if FileExists(FixesFile) then begin
    assign(Fixes, FixesFile);
    reset(Fixes);
    repeat
      readln(Fixes, Fix);
      if (Fix <> '') and (Fix[1] in ['A'..'Z', 'a'..'z', '_']) then begin // else comments
        Fields := Explode(',', Fix);
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
                    TProp.Create(Fields[1] + Fields[2], Fields[1], Fields[2], lowercase(Fields[3]) = 'true'))
                else begin
                  J := Properties.IndexOf(Fields[1]);
                  if J = -1 then // Add
                    Properties.AddObject(Fields[1],
                      TProp.Create(Fields[1], Fields[1], Fields[2], lowercase(Fields[3]) = 'true'))
                  else // Update
                    with TProp(Properties.Objects[J]) do begin
                      Typ     := FixType(Fields[2]);
                      Static  := lowercase(Fields[3]) = 'true';
                      Default := Fields[5]
                    end
                end
              else begin // Methods or Events
                if SameText(Fields[2], 'Event') then begin // Events
                  J := Events.IndexOf('on' + Fields[1]);
                  if J = -1 then begin // Add
                    Params := TStringList.Create;
                    Events.AddObject('on' + Fields[1], TMethod.Create('on' + Fields[1], '', Params, false));
                    for K := 0 to ((Fields.Count-2) div 2)-1 do
                      Params.AddObject(Fields[K*2+3], TParam.Create(Fields[K*2+3], FixType(Fields[K*2+4]), false));
                  end
                  else // Update
                    with TMethod(Events.Objects[J]) do begin
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
                    Methods.AddObject(Fields[1], TMethod.Create(Fields[1], FixType(Fields[2]), Params, lowercase(Fields[3]) = 'true'));
                    for K := 0 to ((Fields.Count-4) div 3)-1 do
                      Params.AddObject(Fields[K*3+5], TParam.Create(Fields[K*3+5], FixType(Fields[K*3+6]), lowercase(Fields[K*3+7]) = 'true'))
                  end
                  else // Update
                    with TMethod(Methods.Objects[J]) do begin
                      Return := FixType(Fields[2]);
                      Static := lowercase(Fields[3]) = 'true';
                      Params.Clear;
                      for K := 0 to ((Fields.Count-4) div 3)-1 do
                        Params.AddObject(Fields[K*3+5], TParam.Create(Fields[K*3+5], FixType(Fields[K*3+6]), lowercase(Fields[K*3+7]) = 'true'))
                    end;
                end;
              end
        else begin // Create new Class
          I := AllClasses.IndexOf(Fields[2]);
          if I <> -1 then begin
            NewClass := TClass.Create(Fields[0], Fields[1]);
            NewClass.JSName := Fields[3];
            AllClasses.AddObject(NewClass.Name, NewClass);
            AllClasses.AddObject(NewClass.Name, NewClass)
          end
          else
            writeln('Unit: ', Fields[2], ' not found. Fix record: ', Fix);
        end;
        Fields.Free;
      end;
    until SeekEOF(Fixes);
    close(Fixes);
  end
  else begin
    writeln(FixesFile + ' file not found. Generation aborted.'^M^J'Press enter.');
    readln;
  end;
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
    if Return = '' then
      write(Pas, T, 'constructor ', pClassName, 'Create')
    else
      write(Pas, T, IfThen(Static, 'class ', ''), IfThen(Return = 'TVoid', 'procedure ', 'function '), pClassName, Name);
    WriteParams(Params);
    write(Pas, IfThen((Return = 'TVoid') or (Return = ''), ';', ' : ' + Return + ';'), '');
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
    // Write Enumerateds
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
    if Arrays or Objects or (Events.Count > 0) then writeln(Pas, Tab, 'protected');
    if Arrays or Objects then writeln(Pas, Tab(2), 'procedure InitDefaults; override;');
    if Events.Count > 0 then writeln(Pas, Tab(2), 'procedure HandleEvent(const AEvtName: string); override;');
    writeln(Pas, Tab, 'public');
    writeln(Pas, Tab(2), 'function JSClassName : string; override;');
    // Write class properties
    for I := 0 to Properties.Count-1 do
      with TProp(Properties.Objects[I]) do
        if Static then writeln(Pas, Tab(2), 'class function ', Name, ' : ', Typ, ';');
    writeln(Pas, Tab(2), '{$IFDEF FPC}constructor AddTo(List : TExtObjectList);{$ENDIF}');
    // Write methods
    for I := 0 to Methods.Count-1 do
      WriteMethodSignature(TMethod(Methods.Objects[I]));
    if Arrays or Objects then writeln(Pas, Tab(2), 'destructor Destroy; override;');
    {$IFDEF USESPUBLISHED}if (Properties.Count > 0) or (Events.Count > 0) then writeln(Pas, Tab, 'published');{$ENDIF}
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
  J, K : integer;
  CName, CJSName, BoolParam, RegExParam : string;
begin
  assign(Pas, 'Draw2D.pas');
  rewrite(Pas);
  writeln(Pas, 'unit Draw2D;'^M^J);
  writeln(Pas, '// Generated by ExtToPascal v.', ExtPascalVersion, ', at ', DateTimeToStr(Now));
  writeln(Pas, '// from ', paramstr(1), ^M^J);
  writeln(Pas, 'interface'^M^J^M^J'uses'^M^J, Tab, 'StrUtils, ExtPascal, ExtPascalUtils;'^M^J);
  {$IFDEF USESPUBLISHED}writeln(Pas, '{$M+}');{$ENDIF}
  writeln(Pas, 'type');
  AllClasses.CustomSort(SortByInheritLevel);
  for J := 0 to AllClasses.Count-1 do // forward classes
    writeln(Pas, Tab, TClass(AllClasses.Objects[J]).Name, ' = class;');
  writeln(Pas);
  for J := 0 to AllClasses.Count-1 do
    WriteClassType(TClass(AllClasses.Objects[J]));
  writeln(Pas, 'implementation'^M^J);
  for J := 0 to AllClasses.Count-1 do
    with TClass(AllClasses.Objects[J]) do begin
      CName   := Name;
      CJSName := JSName;
      for K := 0 to Properties.Count-1 do // Write Set procedures implementation
        with TProp(Properties.Objects[K]) do begin
          if not Static then begin
            writeln(Pas, 'procedure ', CName, '.SetF', Name, '(Value : ', Typ, '); begin');
            writeln(Pas, Tab, 'F', Name, ' := Value;');
            RegExParam := IfThen(Typ = 'TRegExp', '#3 +', '');
            BoolParam  := AddBoolParam(Typ);
            if (BoolParam = ', false') and not Enum then writeln(Pas, Tab, 'Value.DeleteFromGarbage;');
            if not Enum then
              writeln(Pas, Tab, 'JSCode(JSName + ''.', JSName, '='' + VarToJSON(',
                IfThen(pos('TArrayOf', Typ) = 0, '[' + RegExParam + 'Value' + BoolParam + ']', RegExParam + 'Value'), ') + '';'');')
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
              write(Pas, Tab(2), 'Un(''', JSName, ''', Ajax(''', JSName, ''', ');
              WriteEventParamsAdapter(TMethod(Events.Objects[K]));
              writeln(Pas, ', true));');
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
      if Arrays or Objects then begin // write Init method
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
              writeln(Pas, Tab, 'CreateVar(JSClassName + ''', ParamsToJSON(Params), ''');');
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
  write(Pas, 'end.');
  close(Pas);
end;

var
  F : TSearchrec;
  T : tdatetime;
  P : string;
begin
  P := AnsiReplaceStr(paramstr(1), '\', '/');
  if FindFirst(P + '/draw2d.*.html', faAnyFile, F) = 0 then begin
    AllClasses := TStringList.Create;
    writeln('Draw2DToPascal wrapper, version ', ExtPascalVersion);
    writeln('(c) 2009 by Wanderlan Santos dos Anjos, BSD license'^M^J);
    writeln('Reading HTML files...');
    T := now;
    repeat
  		ReadHtml(P + '/' + F.Name)
    until FindNext(F) <> 0;
    FindClose(F);
    writeln('Fixing Event Prototypes...');
    FixEvents;
    LoadFixes;
    writeln('Writing Unit files...');
    WriteUnits;
    writeln(AllClasses.Count, ' Ext JS classes wrapped to Object Pascal at ', FormatDateTime('ss.zzz', Now-T), ' seconds');
  end
  else
    writeln('Ext JS HTML files not found at ' + P + '/*.html');
  writeln('Press enter.');
  readln;
end.
