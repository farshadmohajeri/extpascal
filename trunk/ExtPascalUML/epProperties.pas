{$I epDirectives.inc}

unit epProperties;

interface

uses
  Classes, TypInfo, epObjectList, epCommon;

type
  TExtraRTTI = packed record
    Mask, Alias, Hint, OldName, Group, PropOrder, View : string;
    ViewFormat : T_ViewFormat;
    TargetClass : TClass;
    Target : word;
    Constraints : TConstraints;
    HasPropString : T_HasPropString;
    DependencyLists : TList;
    SortRequest : integer;
  end;
  PExtraRTTI = ^TExtraRTTI;

  TMethodRTTI = packed record
    Name, Alias, Mask, Hint : string;
    Stereotype : T_Stereotype;
    Kind       : TMethodKind;
    TypeInfo   : PTypeInfo;
    Params : packed array of record
      Name, Mask, Hint : string;
      Default   : variant;
      Semantics : TParamFlag;
      TypeInfo  : PTypeInfo;
    end;
  end;
  PMethodRTTI = ^TMethodRTTI;

  TUnidirect = packed record
    TargetList      : TObject;
    TargetField     : string;
    TargetConstraints : TConstraints;
  end;

  TArrayExtraRTTI = array of PExtraRTTI;

  TProperties = packed class
  private
    FClass      : TClass;
    FProperties : PPropList;
    FExtraRTTI  : TArrayExtraRTTI;
    FMethods    : array of PMethodRTTI;
    FPackage    : string;
    function GetTypeName(Prop : integer): string;
    function GetType(Prop : integer): TTypeKind;
    function GetParentClass(Prop : integer): TClass;
    function GetOrdType(Prop: integer): TOrdType;
    function GetFloatType(Prop: integer): TFloatType;
    function GetMaxValue(Prop: integer): integer;
    function GetMinValue(Prop: integer): integer;
    function GetMask(Prop: integer): string;
    function GetHint(Prop: integer): string;
    function GetAlias(Prop: integer): string;
    procedure CopyParentProperties(ParentClass : TClass; var aExtraRTTI: array of PExtraRTTI);
    procedure CopyParentMethods(ParentClass: TClass; var aMethods: array of PMethodRTTI);
    function GetMethod(Met: integer): PMethodRTTI;
    function GetIsAssociation(Prop: integer): boolean;
    function GetTarget(Prop: integer): word;
    function GetTargetClass(Prop: integer): TClass;
    function GetOldName(Prop: integer): string;
    function GetGroup(Prop: integer): string;
    function GetClass(Prop: integer): TClass;
    function GetIsDerived(Prop: integer): boolean;
    procedure NewRTTI(Index: integer);
  public
    Unidirect : array of TUnidirect;
    ListIndex, PropCount, MethodCount : word;
    constructor Create(pClass: TClass; pListIndex : word);
    procedure AddMetadata(Index : integer; pAlias, pHint : string; pMask : string = ''); overload;
    procedure AddMetadata(Index : integer; pAlias, pHint : string; pTargetClass : TClass; TargetField : string); overload;
    procedure AddOverride(pFieldOverride, pAlias, pHint, pMask, pGroup : string; pConstraints : TConstraints);
    procedure AddConstraints(Index: integer; pConstraints : TConstraints);
    procedure InheritMetadata(pMethodCount : integer = 0);
    procedure AddClassTags(pTag, pValue: string);
    procedure AddOldName(Index: integer; pOldName: string);
    procedure AddGroup(Index: integer; pGroup: string);
    procedure AddView(pView: string);
    procedure AddUnidirectional(pTargetClass : TClass; pTargetField : string; Constraints : TConstraints);
    procedure AddMethod(Index : integer; pName, pAlias, pMask, pHint : string; pKind : TMethodKind; ParamCount : integer; pTypeInfo : PTypeInfo; pStereotype : T_Stereotype);
    procedure AddParam(Index : integer; pName, pMask, pHint : string; pDefault : variant; pSemantics : TParamFlag; pTypeInfo : PTypeInfo);
    procedure AddDependencyLists(pProp, pList : PtrInt);
    function InConstraints(Prop : integer; Constraint : TConstraint) : boolean;
    function UnidirectInConstraints(Index : integer; Constraint : TConstraint): boolean;
    function CheckConstraint(Transient : TTransient; Prop : integer; OldImage : TTransient = nil; CompleteCheck : boolean = true) : string;
    function MethodByName(pMethodName : string) : PMethodRTTI;
    function MethodIndexByName(pMethodName : string) : integer;
    function PropByName(pPropName : string) : integer;
    function CallCheck(Transient : TTransient; CheckMethod: string; var Message: string): boolean;
    function PropsToJSON(Instance : TObject) : string;
    property Properties : PPropList read FProperties write FProperties;
    property TypeNameProp[Prop : integer] : string read GetTypeName;
    property TypeProp[Prop : integer] : TTypeKind read GetType;
    property ClassProp[Prop : integer] : TClass read GetClass;
    property ParentClassProp[Prop : integer] : TClass read GetParentClass;
    property IsAssociation[Prop : integer] : boolean read GetIsAssociation;
    property IsDerived[Prop : integer] : boolean read GetIsDerived;
    property OrdTypeProp[Prop : integer] : TOrdType read GetOrdType;
    property FloatTypeProp[Prop : integer] : TFloatType read GetFloatType;
    property MaxValueProp[Prop : integer] : integer read GetMaxValue;
    property MinValueProp[Prop : integer] : integer read GetMinValue;
    property MaskProp[Prop : integer] : string read GetMask;
    property AliasProp[Prop : integer] : string read GetAlias;
    property HintProp[Prop : integer] : string read GetHint;
    property OldName[Prop : integer] : string read GetOldName;
    property Group[Prop : integer] : string read GetGroup;
    property TargetProp[Prop : integer] : word read GetTarget;
    property TargetClassProp[Prop : integer] : TClass read GetTargetClass;
    property Method[Met : integer] : PMethodRTTI read GetMethod;
    property Package : string read FPackage;
    property ExtraRTTI : TArrayExtraRTTI read FExtraRTTI;
  end;

procedure SetPropInfoValue(Instance: TObject; const PropInfo: PPropInfo; const Value: Variant);
function GetPropInfoValue(Instance: TObject; const PropInfo: PPropInfo) : Variant;

implementation

uses
  RTLConsts, SysUtils, SysConst, Variants, Math, ExtPascalUtils, epPrevalence, epUtils, StrUtils;

constructor TProperties.Create(pClass : TClass; pListIndex : word);
var
  Tipos : TTypeKinds;
begin
  FClass := pClass;
  ListIndex := pListIndex;
  Tipos := [tkInteger, tkInt64, tkChar, tkEnumeration, {$IFDEF FPC}tkBool,{$ENDIF}tkFloat, tkString, tkSet, tkClass, tkLString];
  // tkUnknown, tkMethod, tkWChar, tkWString, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray
  PropCount := GetPropList(pClass.ClassInfo, Tipos, nil, false);
  getmem(FProperties, PropCount * sizeof(pointer));
  GetPropList(pClass.ClassInfo, Tipos, FProperties, false);
end;

function TProperties.GetTypeName(Prop : integer): string; begin
  Result := Properties[Prop].PropType^{$IFNDEF FPC}^{$ENDIF}.Name;
end;

function TProperties.GetType(Prop : integer): TTypeKind;
var
  C : TClass;
begin
  Result := Properties[Prop].PropType^{$IFNDEF FPC}^{$ENDIF}.Kind;
  if Result = tkClass then begin
    C := GetTypeData(Properties[Prop].PropType{$IFNDEF FPC}^{$ENDIF}).ClassType;
    if not C.InheritsFrom(TTransient) and not C.InheritsFrom(TObjectList) then
      Result := tkUnknown;
  end;
end;

function TProperties.GetIsDerived(Prop: integer): boolean; begin
  Result := (Properties[Prop].SetProc = nil) and not IsAssociation[Prop]
end;

function TProperties.GetParentClass(Prop : integer): TClass; begin
  if GetType(Prop) = tkClass then
    Result := GetTypeData(Properties[Prop].PropType{$IFNDEF FPC}^{$ENDIF}).ClassType.ClassParent
  else
    Result := nil;
end;

function TProperties.GetClass(Prop : integer): TClass; begin
  if GetType(Prop) = tkClass then
    Result := GetTypeData(Properties[Prop].PropType{$IFNDEF FPC}^{$ENDIF}).ClassType
  else
    Result := nil;
end;

function TProperties.GetOrdType(Prop : integer): TOrdType; begin
  Result := GetTypeData(Properties[Prop].PropType{$IFNDEF FPC}^{$ENDIF}).OrdType
end;

function TProperties.GetFloatType(Prop : integer): TFloatType; begin
  Result := GetTypeData(Properties[Prop].PropType{$IFNDEF FPC}^{$ENDIF}).FloatType
end;

function TProperties.GetMaxValue(Prop : integer): integer; begin
  if IsAssociation[Prop] then
    Result := TAssociationClass(GetClass(Prop)).MaxConstraint
  else
    Result := GetTypeData(Properties[Prop].PropType{$IFNDEF FPC}^{$ENDIF}).MaxValue;
end;

function TProperties.GetMinValue(Prop : integer): integer; begin
  if IsAssociation[Prop] then
    Result := TAssociationClass(GetClass(Prop)).MinConstraint
  else
    Result := GetTypeData(Properties[Prop].PropType{$IFNDEF FPC}^{$ENDIF}).MinValue
end;

function TProperties.GetAlias(Prop: integer): string; begin
  Result := '';
  if (Prop < length(FExtraRTTI)) and (FExtraRTTI[Prop] <> nil) then
    Result := FExtraRTTI[Prop].Alias;
  if (Result = '') or (lowerCase(Result) = 'hidden') then
    if Prop = 0 then
      Result := copy(FClass.ClassName, 2, 100)
    else
      Result := Properties[Prop].Name
end;

function TProperties.GetMethod(Met: integer): PMethodRTTI; begin
  Result := nil;
  if Met in [0..MethodCount] then
    Result := FMethods[Met];
end;

function TProperties.MethodByName(pMethodName: string): PMethodRTTI;
const
  PrevMethodName : string = '';
  PrevMethod     : PMethodRTTI = nil;
  PrevClass      : TClass = nil;
var
  I : integer;
begin
  if (PrevClass = FClass) and (PrevMethodName = pMethodName) then
    Result := PrevMethod
  else begin
    PrevMethodName := pMethodName;
    PrevClass      := FClass;
    for I := 0 to MethodCount-1 do
      if FMethods[I].Name = pMethodName then begin
        Result := FMethods[I];
        PrevMethod := Result;
        exit;
      end;
    Result := nil;
    PrevMethod := nil;
  end;
end;

function TProperties.MethodIndexByName(pMethodName: string): integer; begin
  for Result := 0 to MethodCount-1 do
    if FMethods[Result].Name = pMethodName then begin
      exit;
    end;
  Result := -1;
end;

function TProperties.GetHint(Prop: integer): string; begin
  Result := '';
  if (Prop < length(FExtraRTTI)) and (FExtraRTTI[Prop] <> nil) then Result := FExtraRTTI[Prop].Hint;
end;

function TProperties.GetMask(Prop: integer): string; begin
  Result := '';
  if (Prop < length(FExtraRTTI)) and (FExtraRTTI[Prop] <> nil) then Result := FExtraRTTI[Prop].Mask;
end;

function TProperties.GetOldName(Prop: integer): string; begin
  Result := '';
  if (Prop < length(FExtraRTTI)) and (FExtraRTTI[Prop] <> nil) then Result := FExtraRTTI[Prop].OldName
end;

function TProperties.GetGroup(Prop: integer): string; begin
  Result := '';
  if (Prop < length(FExtraRTTI)) and (FExtraRTTI[Prop] <> nil) then Result := FExtraRTTI[Prop].Group
end;

function TProperties.GetTarget(Prop: integer): word; begin
  Result := 0;
  if (Prop < length(FExtraRTTI)) and (FExtraRTTI[Prop] <> nil) then Result := FExtraRTTI[Prop].Target;
end;

function TProperties.GetTargetClass(Prop: integer): TClass; begin
  Result := nil;
  if (Prop < length(FExtraRTTI)) and (FExtraRTTI[Prop] <> nil) then Result := FExtraRTTI[Prop].TargetClass;
end;

function TProperties.InConstraints(Prop : integer; Constraint : TConstraint): boolean; begin
  Result := false;
  if (Prop < length(FExtraRTTI)) and (FExtraRTTI[Prop] <> nil) then Result := Constraint in FExtraRTTI[Prop].Constraints
end;

function TProperties.UnidirectInConstraints(Index : integer; Constraint : TConstraint): boolean; begin
  Result := Constraint in Unidirect[Index].TargetConstraints;
end;

procedure TProperties.CopyParentProperties(ParentClass : TClass; var aExtraRTTI : array of PExtraRTTI);
var
  I : integer;
begin
  if ParentClass = TPrevalent then exit;
  with Prevalence.Metadata(ParentClass.ClassName) do
    for I := 0 to PropCount-1 do
      aExtraRTTI[I] := FExtraRTTI[I];
end;

procedure TProperties.CopyParentMethods(ParentClass : TClass; var aMethods : array of PMethodRTTI);
var
  I : integer;
begin
  if ParentClass = TPrevalent then exit;
  with Prevalence.Metadata(ParentClass.ClassName) do
    for I := 0 to MethodCount - 1 do
      aMethods[I] := FMethods[I];
end;

var
  LastMethod : integer = -1;

procedure TProperties.InheritMetadata(pMethodCount : integer = 0); begin
  if length(FExtraRTTI) = 0 then begin
    setlength(FExtraRTTI, PropCount);
    CopyParentProperties(FClass.ClassParent, FExtraRTTI);
  end;
  if length(FMethods) = 0 then begin
    if FClass.ClassParent <> TPrevalent then
      LastMethod := Prevalence.Metadata(FClass.ClassParent.ClassName).MethodCount-1
    else
      LastMethod := -1;
    MethodCount := pMethodCount + LastMethod + 1;
    setlength(FMethods, MethodCount);
    CopyParentMethods(FClass.ClassParent, FMethods)
  end;
end;

procedure TProperties.AddMetadata(Index : integer; pAlias, pHint : string; pMask : string = ''); begin
  if Index > 0 then Index := PropCount - Index;
  NewRTTI(Index);
  with FExtraRTTI[Index]^ do begin
    Alias := pAlias;
    Hint  := pHint;
    OldName := '';
    Group := '';
    PropOrder := '';
    View := '';
    Constraints := [];
    DependencyLists := TList.Create;
    if Index = 0 then begin
      FPackage := pMask;
      HasPropString := _hpNONE;
      SortRequest := 1;
    end
    else
      Mask := pMask;
  end;
end;

procedure TProperties.AddMetadata(Index : integer; pAlias, pHint : string; pTargetClass : TClass; TargetField : string); begin
  if Index <> 0 then Index := PropCount - Index;
  NewRTTI(Index);
  with FExtraRTTI[Index]^ do begin
    Alias       := pAlias;
    SortRequest := 1;
    TargetClass := pTargetClass;
    try
      if TargetField <> '' then Target := GetPropInfo(TargetClass, TargetField).NameIndex;
    except
      raise EExtP.CreateFmt('Classe %s não tem o atributo %s. Possível erro no modelo.', [TargetClass.ClassName, TargetField])
    end;
    Hint := pHint;
  end;
end;

procedure TProperties.AddOverride(pFieldOverride, pAlias, pHint, pMask, pGroup : string; pConstraints : TConstraints);
var
  I : integer;
  OldRTTI : PExtraRTTI;
begin
  for I := 0 to PropCount-1 do
    if FProperties[I].Name = pFieldOverride then begin
      OldRTTI := FExtraRTTI[I];
      New(FExtraRTTI[I]);
      with FExtraRTTI[I]^ do begin
        if OldRTTI = nil then begin
          Alias := pAlias;
          Mask := pMask;
          Hint := pHint;
          Group := pGroup;
          OldName := '';
          PropOrder := '';
          View := '';
          ViewFormat := _vfGrid;
          Constraints := pConstraints;
        end
        else begin
          Alias := ifthen((pAlias <> '') or (OldRTTI = nil), pAlias, OldRTTI.Alias);
          Mask :=  ifthen((pMask <> '')  or (OldRTTI = nil), pMask,  OldRTTI.Mask);
          Hint :=  ifthen((pHint <> '')  or (OldRTTI = nil), pHint,  OldRTTI.Hint);
          Group := ifthen((pGroup <> '') or (OldRTTI = nil), pGroup, OldRTTI.Group);
          OldName := OldRTTI.OldName;
          PropOrder := OldRTTI.PropOrder;
          View := OldRTTI.View;
          ViewFormat := OldRTTI.ViewFormat;
          TargetClass := OldRTTI.TargetClass;
          Target := OldRTTI.Target;
          DependencyLists := OldRTTI.DependencyLists;
          if pConstraints = [] then
            Constraints := OldRTTI.Constraints
          else
            Constraints := pConstraints + Constraints;
        end;
      end;
      exit;
    end;
end;

procedure TProperties.NewRTTI(Index : integer); begin
  New(FExtraRTTI[Index]);
  fillchar(FExtraRTTI[Index]^, sizeof(FExtraRTTI[Index]^), 0);
end;

procedure TProperties.AddConstraints(Index: integer; pConstraints: TConstraints); begin
  FExtraRTTI[PropCount - Index].Constraints := pConstraints
end;

procedure TProperties.AddClassTags(pTag, pValue: string); begin
  if pTag = 'PropOrder' then FExtraRTTI[0].PropOrder := pValue;
  if pTag = 'ViewFormat' then
    if pValue = 'grid' then
      FExtraRTTI[0].ViewFormat := _vfGrid
    else
      if pValue = 'card' then FExtraRTTI[0].ViewFormat := _vfCard;
end;

procedure TProperties.AddOldName(Index : integer; pOldName : string); begin
  if Index = 0 then
    FExtraRTTI[0].OldName := pOldName
  else begin
    NewRTTI(PropCount - Index);
    FExtraRTTI[PropCount - Index].OldName := pOldName;
  end;
end;

procedure TProperties.AddGroup(Index: integer; pGroup: string); begin
  if Index = 0 then
    FExtraRTTI[0].Group := pGroup
  else
    FExtraRTTI[PropCount - Index].Group := pGroup;
end;

procedure TProperties.AddView(pView: string); begin
  AddToStr(FExtraRTTI[0].View, pView);
end;

procedure TProperties.AddUnidirectional(pTargetClass: TClass; pTargetField : string; Constraints : TConstraints); begin
  setlength(Unidirect, length(Unidirect) + 1);
  with Unidirect[length(Unidirect)-1] do begin
    TargetList := Prevalence.PrevalentLists(pTargetClass.ClassName + 'List');
    if TargetList = nil then TargetList := Prevalence.AbstractLists(pTargetClass.ClassName + 'List');
    TargetField := pTargetField;
    TargetConstraints := Constraints;
  end;
end;

procedure TProperties.AddMethod(Index: integer; pName, pAlias, pMask, pHint: string; pKind: TMethodKind; ParamCount: integer; pTypeInfo : PTypeInfo; pStereotype : T_Stereotype);
var
  Method : PMethodRTTI;
begin
  New(Method);
  inc(LastMethod);
  FMethods[LastMethod] := Method;
  Method.Name := pName;
  Method.Mask := pMask;
  Method.Hint := pHint;
  Method.Kind := pKind;
  Method.TypeInfo := pTypeInfo;
  Method.Stereotype := pStereotype;
  if pAlias = '' then
    Method.Alias := Method.Name
  else
    Method.Alias := pAlias;
  setlength(Method.Params, ParamCount);
end;

procedure TProperties.AddParam(Index: integer; pName, pMask, pHint : string; pDefault : variant; pSemantics : TParamFlag; pTypeInfo : PTypeInfo); begin
  with FMethods[LastMethod].Params[Index] do begin
    Name      := pName;
    Mask      := pMask;
    Hint      := pHint;
    Default   := pDefault;
    Semantics := pSemantics;
    TypeInfo  := pTypeInfo;
  end;
end;

procedure TProperties.AddDependencyLists(pProp, pList : PtrInt);
var
  Index : integer;
begin
  if pProp > high(FExtraRTTI) then exit;
  if FExtraRTTI[pProp] = nil then begin
    NewRTTI(pProp);
    FExtraRTTI[pProp].DependencyLists := TList.Create;
    Index := -1;
  end
  else begin
    if FExtraRTTI[pProp].DependencyLists = nil then
      FExtraRTTI[pProp].DependencyLists := TList.Create;
    Index := FExtraRTTI[pProp].DependencyLists.IndexOf(pointer(pList));
  end;
  if Index = -1 then FExtraRTTI[pProp].DependencyLists.Add(pointer(pList));
end;

function TProperties.GetIsAssociation(Prop: integer): boolean; begin
  Result := (Properties[Prop].PropType^{$IFNDEF FPC}^{$ENDIF}.Kind = tkClass) and
            GetTypeData(Properties[Prop].PropType{$IFNDEF FPC}^{$ENDIF}).ClassType.InheritsFrom(TAssociation)
end;

procedure SetPropInfoValue(Instance: TObject; const PropInfo: PPropInfo; const Value: Variant);

 function RangedValue(const AMin, AMax: Int64): Int64; begin
    if not (PropInfo.PropType^{$IFNDEF FPC}^{$ENDIF}.Kind in [tkChar, tkWChar]) then
      Result := Value
    else
      if VarToStr(Value) <> '' then
        Result := Ord(VarToStr(Value)[1])
      else
        Result := 32;
    if InMoveCorresponding then exit;
    if (Result < AMin) or (Result > AMax) then raise ERangeError.CreateRes(@SRangeError);
  end;

var
  K : integer;
  TypeData: PTypeData;
  DynArray: Pointer;
begin
  case PropInfo.PropType^{$IFNDEF FPC}^{$ENDIF}.Kind of
    tkInteger, tkChar, tkWChar: begin
      TypeData := GetTypeData(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF});
      if TypeData^.MinValue < TypeData^.MaxValue then
        SetOrdProp(Instance, PropInfo, RangedValue(TypeData^.MinValue, TypeData^.MaxValue))
      else // Unsigned type
        SetOrdProp(Instance, PropInfo, RangedValue(LongWord(TypeData^.MinValue), LongWord(TypeData^.MaxValue)));
    end;
    tkString, tkLString:
      if Instance is TPrevalent then
        with TPrevalent(Instance).Metadata do begin
          K := PropByName(PropInfo.Name);
          if (ExtraRTTI[K] <> nil) and ((lowerCase(ExtraRTTI[K].Mask) = 'image') or (lowerCase(ExtraRTTI[K].Mask) = 'editor')) then
            SetStrProp(Instance, PropInfo, VarToStr(Value))
          else
            SetStrProp(Instance, PropInfo, trim(VarToStr(Value)));
        end;
    tkEnumeration:
      if VarType(Value) = varString then
        SetEnumProp(Instance, PropInfo, VarToStr(Value))
      else
        if VarType(Value) = varBoolean then
          // Need to map variant boolean values -1,0 to 1,0
          SetOrdProp(Instance, PropInfo, Abs(Trunc(Integer(Value))))
        else begin
          TypeData := GetTypeData(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF});
          SetOrdProp(Instance, PropInfo, RangedValue(TypeData^.MinValue, TypeData^.MaxValue));
        end;
    {$IFDEF FPC}
    tkBool : SetOrdProp(Instance, PropInfo, Abs(Trunc(Integer(Value))));
    {$ENDIF}
    tkInt64: begin
      TypeData := GetTypeData(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF});
      SetInt64Prop(Instance, PropInfo, RangedValue(TypeData^.MinInt64Value, TypeData^.MaxInt64Value));
    end;
    tkFloat: SetFloatProp(Instance, PropInfo, Value);
    tkSet: SetOrdProp(Instance, PropInfo, Value);
    tkWString: SetWideStrProp(Instance, PropInfo, VarToWideStr(Value));
    tkVariant: SetVariantProp(Instance, PropInfo, Value);
    tkDynArray: begin
      DynArrayFromVariant(DynArray, Value, PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF});
      SetOrdProp(Instance, PropInfo, PtrInt(DynArray));
    end;
  else
    raise EPropertyError.CreateResFmt(@SInvalidPropertyType, [PropInfo.PropType^{$IFNDEF FPC}^{$ENDIF}.Name]);
  end;
end;

function GetPropInfoValue(Instance: TObject; const PropInfo: PPropInfo) : Variant; begin
  case PropInfo^.PropType^{$IFNDEF FPC}^{$ENDIF}.Kind of
    tkInteger, tkChar, tkWChar, tkClass: Result := GetOrdProp(Instance, PropInfo);
    tkString, tkLString: Result := GetStrProp(Instance, PropInfo);
    tkEnumeration:
      if GetTypeData(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF})^.BaseType{$IFNDEF FPC}^{$ENDIF} = TypeInfo(Boolean) then
        Result := Boolean(GetOrdProp(Instance, PropInfo))
      else
        Result := GetOrdProp(Instance, PropInfo);
    {$IFDEF FPC}
    tkBool: Result := Boolean(GetOrdProp(Instance, PropInfo));
    {$ENDIF}
    tkFloat: Result := GetFloatProp(Instance, PropInfo);
    tkInt64: Result := GetInt64Prop(Instance, PropInfo);
    tkSet:   Result := GetOrdProp(Instance, PropInfo);
    tkMethod: Result := PropInfo^.PropType^.Name;
    tkWString: Result := GetWideStrProp(Instance, PropInfo);
    tkVariant: Result := GetVariantProp(Instance, PropInfo);
    tkDynArray: DynArrayToVariant(Result, Pointer(GetOrdProp(Instance, PropInfo)), PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF});
  else
    raise EPropertyError.CreateResFmt(@SInvalidPropertyType, [PropInfo.PropType^{$IFNDEF FPC}^{$ENDIF}.Name]);
  end;
end;

function TProperties.PropsToJSON(Instance: TObject) : string;
var
  I, J, MaxVal, Val : integer;
begin
  Result := '{';
  for I := 0 to PropCount-1 do begin
    Result := Result + Properties[I].Name + ':';
    case Properties[I]^.PropType^{$IFNDEF FPC}^{$ENDIF}.Kind of
      tkInteger, tkClass : Result := Result + IntToStr(GetOrdProp(Instance, Properties[I]));
      tkString, tkLString: Result := Result + StrToJS(GetStrProp(Instance, Properties[I]));
      tkChar, tkWChar : Result := Result + StrToJS(char(GetOrdProp(Instance, Properties[I])));
      tkEnumeration:
        if GetTypeData(Properties[I]^.PropType{$IFNDEF FPC}^{$ENDIF})^.BaseType{$IFNDEF FPC}^{$ENDIF} = TypeInfo(Boolean) then
          Result := Result + IfThen(Boolean(GetOrdProp(Instance, Properties[I])), 'true', 'false')
        else
          Result := Result + IntToStr(GetOrdProp(Instance, Properties[I]));
      tkSet : begin
        MaxVal := GetTypeData(GetTypeData(Properties[I]^.PropType{$IFNDEF FPC}^{$ENDIF}).CompType{$IFNDEF FPC}^{$ENDIF}).MaxValue;
        Val := GetOrdProp(Instance, Properties[I]);
        Result := Result + '"';
        for J := 0 to MaxVal do
          if (trunc(power(2, J)) and Val) <> 0 then
            Result := Result + IntToStr(J) + ',';
        if Result[length(Result)] = ',' then
          delete(Result, length(Result), 1);
        Result := Result + '"';
      end;
      {$IFDEF FPC}
      tkBool: Result := Result + IfThen(Boolean(GetOrdProp(Instance, Properties[I])), 'true', 'false');
      {$ENDIF}
      tkFloat:
        case GetDateTimeType(TypeNameProp[I]) of
          dtNone : Result := Result + FloatToStr(GetFloatProp(Instance, Properties[I]));
          dtDate : Result := Result + '"' + FormatDateTime('dd/mm/yyyy', GetFloatProp(Instance, Properties[I])) + '"';
          dtTime : Result := Result + '"' + FormatDateTime('hh:nn:ss', GetFloatProp(Instance, Properties[I])) + '"';
          dtDateTime : Result := Result + '"' + FormatDateTime('dd/mm/yyyy, hh:nn:ss', GetFloatProp(Instance, Properties[I])) + '"';
        end;
      tkInt64: Result := Result + IntToStr(GetInt64Prop(Instance, Properties[I]));
      tkMethod: Result := Result + Properties[I]^.PropType^.Name;
      tkWString: Result := Result + StrToJS(GetWideStrProp(Instance, Properties[I]));
      tkVariant: Result := Result + StrToJS(GetVariantProp(Instance, Properties[I]));
    else
      raise EPropertyError.CreateResFmt(@SInvalidPropertyType, [Properties[I].PropType^{$IFNDEF FPC}^{$ENDIF}.Name]);
    end;
    if I <> PropCount-1 then Result := Result + ',';
  end;
  Result := Result + '}';
end;

function TProperties.PropByName(pPropName: string): integer;
const
  PrevPropName : string  = '';
  PrevProp     : integer = -1;
  PrevClass    : TClass  = nil;
begin
  if pPropName = 'ID' then begin
    PrevClass := FClass;
    Result := 0;
    PrevProp := 0;
    PrevPropName := pPropName;
    exit;
  end;
  if (PrevClass = FClass) and (PrevPropName = pPropName) then
    Result := PrevProp
  else begin
    PrevPropName := pPropName;
    if (PrevClass = FClass) then begin //primeiro procura a partir da última posição
      for Result := PrevProp+1 to PropCount-1 do
        if AnsiCompareText(Properties[Result].Name, pPropName) = 0 then begin
          PrevProp := Result;
          exit;
        end;
    end
    else
      PrevClass := FClass;
    for Result := 2 to PropCount-1 do
      if AnsiCompareText(Properties[Result].Name, pPropName) = 0 then begin
        PrevProp := Result;
        exit;
      end;
    Result := -1;
    PrevProp := -1;
  end;
end;

type
  TUnPTransaction = class(TTransaction);

function TProperties.CallCheck(Transient : TTransient; CheckMethod : string; var Message : string) : boolean;
type
  TCallCheck = function(var Msg : string) : boolean of object;
var
  Check : TMethod;
  S: string;
begin
  try
    Check.Code := Transient.MethodAddress(CheckMethod);
    if Check.Code = nil then
      Result := true
    else begin
      Check.Data := Transient;
      Result     := TCallCheck(Check)(Message)
    end;
  except
    on E: Exception do begin
      S := Trim(Transient.GetIdentification);
      if S <> '' then S := ': ' + S;
      raise Exception.Create('Erro "' + E.Message + '" executando ' + CheckMethod + ' de ' + Transient.ClassName + S);
    end;
  end;
end;

function TProperties.CheckConstraint(Transient : TTransient; Prop: integer; OldImage : TTransient = nil; CompleteCheck : boolean = true): string;

  procedure AddError(Message, PropName : string); begin
    Result := Result + Format('%s do(a) %s(%s): %s. ' + #13#10,
      [PropName, TPrevalent(Transient).GetClassAlias, IfThen(Transient <> nil, Transient.GetIdentification, '0'), Message])
  end;

var
  S : string;
  Obj : TPrevalent;
  Ass : TAssociation;
begin
  Result := '';
  if InConstraints(Prop, NOTNULL) then
    if (TypeProp[Prop] <> tkClass) or ((TypeProp[Prop] = tkClass) and CompleteCheck) then
      if IsAssociation[Prop] then begin
        if TAssociation(GetObjectProp(Transient, Properties[Prop])).Count = 0 then
          AddError('Deve ser informado(a)', AliasProp[Prop])
      end
      else begin
        S := '';
        if (TypeProp[Prop] = tkClass) and (OldImage <> nil) and (OldImage.ThreadID = integer(GetFCGIThreadID)) then begin
          Obj := TPrevalent(GetObjectProp(OldImage, Properties[Prop]));
          if (Obj = nil) or (Obj.ThreadID < 0) then S := 'OK';
        end;
        if S = '' then S := GetPropInfoValue(Transient, Properties[Prop]);
        if (S = '') or (S = '0') or (S = #0) then AddError('Deve ser informado(a)', AliasProp[Prop]);
      end;
  if IsAssociation[Prop] then begin
    Ass := TAssociation(GetObjectProp(Transient, Properties[Prop]));
    if Ass.Count < Ass.MinConstraint then
      AddError('Quantidade de elementos na associacao inferior ao mínimo permitido('+ inttoStr(Ass.MinConstraint)+')' , AliasProp[Prop]);
    if Ass.Count > Ass.MaxConstraint then
      AddError('Quantidade de elementos na associacao superior ao máximo permitido('+ inttoStr(Ass.MaxConstraint)+')' , AliasProp[Prop]);
  end;
  if TypeProp[Prop] in [tkString, tkLString] then begin
    S := GetPropInfoValue(Transient, Properties[Prop]);
    if length(S) > 65535 then
      raise EExtP.CreateFmt('%s do(a) %s(%s): Tem %d caracteres, o que excede o máximo permitido de 65535.',
        [AliasProp[Prop], TPrevalent(Transient).GetClassAlias, Transient.GetIdentification, length(S)]);
  end;
  if InConstraints(Prop, CHECK) then
    if not CallCheck(Transient, 'Check' + Properties[Prop].Name, S) then AddError(S, AliasProp[Prop])
end;

end.
