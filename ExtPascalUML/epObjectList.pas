{$I epDirectives.inc}

unit epObjectList;

interface

uses Classes, SyncObjs, epCommon;

const
  MAXLIST = (MAXINT div sizeof(TObject))-1;

type
  {$M+}
  TTransient = class
  private
    destructor InternalDestroy;
  protected
    FID, FThreadID : integer; // ** Eco
    FPermissionView : T_ViewPermissionSet;
    function PrimaryKey : Integer;
    procedure InternalFree; virtual;
    procedure DelCreates; virtual;
  public
    destructor Destroy; override;
    procedure Delete; virtual;
    procedure Add; virtual;
    function InSession : boolean;
    function GetIdentification: string; virtual;
    function GetPermissionView: T_ViewPermissionSet; virtual;
    property ThreadID : integer read FThreadID write FThreadID;
  published
    property ID : Integer read FID write FID;
    property PermissionView : T_ViewPermissionSet read GetPermissionView;
  end;
  {$M-}
  TObjectArray = array[0..MAXLIST] of TTransient;
  PObjectArray = ^TObjectArray;

  TListProp  = (lpPrimary, lpSorted, lpDuplicates, lpUnique, lpTransient, lpAbstract);
  TListProps = set of TListProp;
  TListType  = (ltString, ltWord, ltInteger, ltLongint, ltInt64, ltDouble, ltDateTime, ltDate, ltTime, ltCurrency);
  TCondition = function(pObject : TTransient) : boolean of object;

  TTransientClass = class of TTransient;

  TObjectList = packed class
  private
    FObjects  : PObjectArray;
    FCapacity : integer;
    FObjectListIndex : word;
    function GetDuplicates : boolean;
    function GetPrimary    : boolean;
    function GetSorted     : boolean;
    function GetTransient  : boolean;
    function GetAbstract   : boolean;
    function GetHidden     : boolean;
    function GetKeyStr(pObject : TTransient) : string;
    function GetKeyInt(pObject : TTransient) : integer;
    function GetKeyI64(pObject : TTransient) : int64;
    function GetKeyDbl(pObject : TTransient) : double;
    function GetKeyWord(pObject: TTransient) : word;
    function GetObject(Index : integer) : TTransient;
    function GetInternalObject(Index : integer) : TTransient;
    procedure Grow;
    procedure QuickSortStr(L, R : integer);
    procedure QuickSortInt(L, R : integer);
    procedure QuickSortI64(L, R : integer);
    procedure QuickSortDbl(L, R : integer);
    procedure QuickSortWord(L, R : integer);
    procedure FastQuickSort(pObject : TTransient);
    procedure SetSorted(Value : boolean);
    function CheckSortStr(L, R : integer) : boolean;
    function CheckSortInt(L, R : integer) : boolean;
    function CheckSortI64(L, R : integer) : boolean;
    function CheckSortDbl(L, R : integer) : boolean;
    function CheckSortWord(L, R : integer) : boolean;
    procedure PutObject(Index : integer; const Value : TTransient);
    procedure FixDuplicates(pObject: TTransient; I: integer);
  protected
    FCount : integer;
    ListProps: TListProps; // ** Eco
    KeyCode  : pointer;
    ListType : TListType;
    FSortVersion : integer;
    LockList : TCriticalSection; // ** Eco
    function InView(pObject : TTransient) : boolean; virtual;
    function InSession(pObject : TTransient) : boolean; virtual;
    function PriorInNear(Index : integer; Key : variant; IsString : boolean = false): TTransient;
    function PriorNextNear(Key : string; var Index: integer; OnlyInSession : boolean = true): boolean; overload;
    function PriorNextNear(Key : integer; var Index: integer; OnlyInSession : boolean = true): boolean; overload;
    function PriorNextNear(Key : word; var Index: integer; OnlyInSession : boolean = true): boolean; overload;
    function PriorNextNear(Key : int64; var Index: integer; OnlyInSession : boolean = true): boolean; overload;
    function PriorNextNear(Key : double; var Index: integer; OnlyInSession : boolean = true): boolean; overload;
    function PriorNear(var Index: integer; OnlyInSession : boolean = true; OnlyDown : boolean = false): boolean;
    function NextNear(var Index: integer; OnlyInSession : boolean = true): boolean;
    procedure Insert(Index: integer; pObject: TTransient);
    procedure VerifySortVersion; virtual;
    procedure Clear;
    class function GetKeyCode : pointer; virtual;
    class function GetListType: TListType; virtual;
    class function GetObjectClass : TTransientClass; virtual;
  public
    ObjectClass : TTransientClass;
    function FindNear(const Value : string;  var Index : integer; OnlyInSession : boolean = true) : boolean; overload;
    function FindNear(const Value : integer; var Index : integer; OnlyInSession : boolean = true) : boolean; overload;
    function FindNear(const Value : word;    var Index : integer; OnlyInSession : boolean = true) : boolean; overload;
    function FindNear(const Value : int64;   var Index : integer; OnlyInSession : boolean = true) : boolean; overload;
    function FindNear(const Value : double;  var Index : integer; OnlyInSession : boolean = true) : boolean; overload;
    function FindNear(const Value : TTransient; var Index : integer; OnlyInSession : boolean = true) : boolean; overload;
    function IndexOf(const Value : string)  : integer; overload;
    function IndexOf(const Value : integer) : integer; overload;
    function IndexOf(const Value : word)    : integer; overload;
    function IndexOf(const Value : int64)   : integer; overload;
    function IndexOf(const Value : double)  : integer; overload;
    function IndexOf(const Value : TTransient) : integer; overload;
    procedure Sort; overload;
    procedure Sort(pObject : TTransient); overload;
    function CheckSort : boolean;
    procedure SetCapacity(NewCapacity: integer);
    procedure SetObjectListIndex(NewIndex : word);
    property Objects[Index: integer]: TTransient read GetObject write PutObject;
    property InternalObjects[Index: integer]: TTransient read GetInternalObject write PutObject;
    constructor Create(pListProps : TListProps = []);
    destructor Destroy; override;
    function GetKey(pObject : TTransient) : string;
    function Add(const pObject : TTransient) : integer;
    function Delete(pObject : TTransient) : boolean; overload;
    procedure Delete(Index : integer; DelObject : boolean = true); overload;
    procedure Populate(ObjectList : TObjectList);
    procedure ForceSorted;
    function Find(Key : string) : TTransient; overload;
    function Find(Key : integer): TTransient; overload;
    function Find(Key : word)   : TTransient; overload;
    function Find(Key : int64)  : TTransient; overload;
    function Find(Key : double) : TTransient; overload;
    function Find(Key : TTransient) : TTransient; overload;
    function Find(Key : variant): TTransient; overload;
    function First : TTransient;
    function Last : TTransient;
    function Next(var pObject : TTransient): boolean;
    function Prior(var pObject : TTransient): boolean;
    function Near(Key : integer): TTransient; overload;
    function Near(Key : word): TTransient; overload;
    function Near(Key : int64): TTransient; overload;
    function Near(Key : double) : TTransient; overload;
    function Near(Key : string) : TTransient; overload;
    function Near(Key : variant) : TTransient; overload;
    function Scan(Attribute: string; Value: variant; Initial: TTransient = nil): TTransient;
    function Sum(Attribute : string; Initial : TTransient = nil; Condition : TCondition = nil; FullScan : boolean = true) : double;
    function Min(Attribute : string; Initial : TTransient = nil; Condition : TCondition = nil; FullScan : boolean = true) : double;
    function Max(Attribute : string; Initial : TTransient = nil; Condition : TCondition = nil; FullScan : boolean = true) : double;
    function Avg(Attribute : string; Initial : TTransient = nil; Condition : TCondition = nil; FullScan : boolean = true) : double;
    function CreateView : TObjectList; overload;
    function CreateView(Value: variant): TObjectList; overload;
    function CreateView(LowValue, HighValue: variant): TObjectList; overload;
    function CreateView(LowValue, HighValue : variant; var MoreObjects : boolean; MaxObjects : integer = MAXINT) : TObjectList; overload;
    function IsAssociation : boolean; virtual;
    property IsDuplicates : boolean read GetDuplicates;
    property IsPrimary : boolean read GetPrimary;
    property IsTransient : boolean read GetTransient;
    property IsAbstract : boolean read GetAbstract;
    property IsHidden : boolean read GetHidden;
    property Count : integer read FCount;
    property Capacity : integer read FCapacity;
    property IsSorted : boolean read GetSorted write SetSorted;
    property SortVersion : integer read FSortVersion write FSortVersion;
    property ObjectListIndex : word read FObjectListIndex;
  end;

  TProtectedObjectList = packed class(TObjectList)
  protected
    destructor InternalDestroy; virtual;
    procedure InternalFree; virtual;
  public
    destructor Destroy; override;
  end;

threadvar
  TerminateSession : boolean;
  CallStateMachineCount : word;//indica que a transacao não é originária do browser
  ListInCheck : word; //indica quando se está verificando chave em uma lista. É o número da lista ou NONELIST
  InUpdate : boolean;
  InMoveCorresponding : boolean;
  InSort : boolean;
  CreatedViews: TList;
  IsRunningStateMachine : boolean;

function ValidObject(O : TObject): boolean;

implementation

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Math, TypInfo, Variants, StrUtils, DateUtils,
  epUtils, epPrevalence, epProperties;

constructor TObjectList.Create(pListProps : TListProps = []); begin
  KeyCode     := GetKeyCode;
  ListType    := GetListType;
  ObjectClass := GetObjectClass;
  LockList    := SyncObjs.TCriticalSection.Create;
  ListProps   := pListProps + [lpSorted];
  if GetKeyCode = @TTransient.PrimaryKey then include(ListProps, lpPrimary);
end;

type
  TObjectListClass = class of TObjectList;
  
function TObjectList.CreateView : TObjectList; begin
  Result := TObjectListClass(ClassType).Create(ListProps + [lpTransient]);
  if Assigned(CreatedViews) then CreatedViews.Add(Result);
  Result.KeyCode := GetKeyCode;
  Result.ListType := GetListType;
  Result.ObjectClass := ObjectClass;
end;

function TObjectList.CreateView(Value : variant) : TObjectList;
var
  Dummy : boolean;
begin
  Result := CreateView(Value, Value, Dummy);
end;

function TObjectList.CreateView(LowValue, HighValue : variant) : TObjectList;
var
  Dummy : boolean;
begin
  Result := CreateView(LowValue, HighValue, Dummy);
end;

function TObjectList.CreateView(LowValue, HighValue : variant; var MoreObjects : boolean; MaxObjects : integer = MAXINT) : TObjectList;
var
  Obj : TTransient;
  I, KeyInt, HighInt, LowInt : integer;
  KeyI64, HighI64, LowI64 : Int64;
  KeyDbl : double;
  StrLow, StrHigh, KeyStr, KeyStrHigh : string;
begin
  if MaxObjects < 1 then raise EExtP.CreateFmt('Erro: Quantidade de objetos solicitada(%d) deve ser maior que zero. ', [MaxObjects]);
  MoreObjects := false;
  Obj := Near(LowValue);
  Result := CreateView;
  if Obj = nil then exit;
  I := 0;
  case ListType of
    ltString : begin
      while (I < MaxObjects) and (Obj <> nil) do begin
        StrLow := varToStr(LowValue);
        StrHigh := varToStr(HighValue);
        KeyStr := GetKeyStr(Obj);
        KeyStrHigh := copy(GetKeyStr(Obj), 1, length(StrHigh));
        if (CompareText(KeyStr, StrLow) >= 0) and (CompareText(keyStrHigh, StrHigh) <= 0) then begin
          Result.Add(Obj);
          Next(Obj);
          inc(I);
        end
        else
          break;
      end;
      if I = MaxObjects then MoreObjects := Next(Obj) and ((CompareText(KeyStr, StrLow) >= 0) and (CompareText(keyStrHigh, StrHigh) <= 0));
    end;
    ltLongint,
    ltInteger : begin
      KeyInt := 0;
      LowInt := varToInt(LowValue);
      HighInt := varToInt(HighValue);
      while (I < MaxObjects) and (Obj <> nil) do begin
        KeyInt := GetKeyInt(Obj);
        if (KeyInt >= LowInt) and (KeyInt <= HighInt) then begin
          Result.Add(Obj);
          Next(Obj);
          inc(I);
        end
        else
          break;
      end;
      if I = MaxObjects then MoreObjects := Next(Obj) and (KeyInt >= LowInt) and (KeyInt <= HighInt);
    end;
    ltInt64,
    ltCurrency : begin
      KeyI64 := 0;
      LowI64 := varToI64(LowValue);
      HighI64 := varToI64(HighValue);
      while (I < MaxObjects) and (Obj <> nil) do begin
        KeyI64 := GetKeyI64(Obj);
        if (KeyI64 >= LowI64) and (KeyI64 <= HighI64) then begin
          Result.Add(Obj);
          Next(Obj);
          inc(I);
        end
        else
          break;
      end;
      if I = MaxObjects then MoreObjects := Next(Obj) and (KeyI64 >= LowI64) and (KeyI64 <= HighI64);
    end;
    ltWord : begin
      KeyInt := 0;
      LowInt := varToInt(LowValue);
      HighInt := varToInt(HighValue);
      while (I < MaxObjects) and (Obj <> nil) do begin
        KeyInt := GetKeyWord(Obj);
        if (KeyInt >= LowInt) and (KeyInt <= HighInt) then begin
          Result.Add(Obj);
          Next(Obj);
          inc(I);
        end
        else
          break;
      end;
      if I = MaxObjects then MoreObjects := Next(Obj) and (KeyInt >= varToWord(LowValue)) and (KeyInt <= varToWord(HighValue));
    end;
    ltDouble,
    ltDateTime,
    ltDate,
    ltTime : begin
      KeyDbl := 0;
      while (I < MaxObjects) and (Obj <> nil) do begin
        KeyDbl := GetKeyDbl(Obj);
        if (KeyDbl >= varToDbl(LowValue)) and (KeyDbl <= varToDbl(HighValue)) then begin
          Result.Add(Obj);
          Next(Obj);
          inc(I);
        end
        else
          break;
      end;
      if I = MaxObjects then MoreObjects := Next(Obj) and (KeyDbl >= varToDbl(LowValue)) and (KeyDbl <= varToDbl(HighValue));
    end;
  end;

end;

destructor TObjectList.Destroy; begin
  if Assigned(CreatedViews) then CreatedViews.Remove(Self);
  Clear;
  if LockList <> nil then LockList.Free;
  inherited
end;

class function TObjectList.GetObjectClass : TTransientClass; begin
  Result := TTransient
end;

class function TObjectList.GetKeyCode : pointer; begin
  Result := @TTransient.PrimaryKey
end;

class function TObjectList.GetListType : TListType; begin
  Result := ltInteger
end;

procedure TObjectList.Populate(ObjectList : TObjectList);
var
  I : integer;
begin
  if Self is ObjectList.ClassType then
    for I := 0 to ObjectList.FCount-1 do
      Add(ObjectList.Objects[I])
  else
    raise EExtP.CreateFmt('Incompatible classes %s and %s at Populate', [ClassName, ObjectList.ClassType]);
end;

procedure TObjectList.Clear; begin
  if FCount <> 0 then InterLockedExchange(FCount, 0);
  LockList.Enter;
  try
    SetCapacity(0);
  finally
    LockList.Leave;
  end;
end;

procedure TObjectList.SetCapacity(NewCapacity: integer); begin
  FCapacity := Math.Min(NewCapacity, MAXLIST);
  ReallocMem(FObjects, FCapacity * SizeOf(TObject));
end;

procedure TObjectList.SetObjectListIndex(NewIndex : word); begin
  FObjectListIndex := NewIndex;
end;

procedure TObjectList.SetSorted(Value: Boolean); begin
  if IsSorted <> Value then
    if Value then begin
      include(ListProps, lpSorted);
      Sort;
    end
    else
      exclude(ListProps, lpSorted);
end;

procedure TObjectList.ForceSorted; begin
  include(ListProps, lpSorted);
end;

function TObjectList.IsAssociation : boolean; begin
  Result := false;
end;

function TObjectList.GetDuplicates: boolean; begin
  Result := lpDuplicates in ListProps
end;

function TObjectList.GetPrimary: boolean; begin
  Result := lpPrimary in ListProps
end;

function TObjectList.GetSorted: boolean; begin
  Result := lpSorted in ListProps
end;

function TObjectList.GetTransient : boolean; begin
  Result := lpTransient in ListProps
end;

function TObjectList.GetAbstract : boolean; begin
  Result := lpAbstract in ListProps
end;

function TObjectList.GetHidden : boolean; begin
  Result := lpAbstract in ListProps
end;

function TObjectList.Max(Attribute: string; Initial: TTransient = nil; Condition: TCondition = nil; FullScan: boolean = true): double;
var
  Obj : TTransient;
begin
  if Initial = nil then
    Obj := First
  else
    Obj := Initial;
  Result := -MAXINT;
  while Obj <> nil do begin
    if (@Condition = nil) or Condition(Obj) then begin
      if GetPropValue(Obj, Attribute) > Result then Result := GetPropValue(Obj, Attribute)
    end
    else
      if not FullScan then exit;
    Next(Obj)
  end;
end;

function TObjectList.Min(Attribute: string; Initial: TTransient = nil; Condition: TCondition = nil; FullScan: boolean = true): double;
var
  Obj : TTransient;
begin
  if Initial = nil then
    Obj := First
  else
    Obj := Initial;
  Result := MAXINT;
  while Obj <> nil do begin
    if (@Condition = nil) or Condition(Obj) then begin
      if GetPropValue(Obj, Attribute) < Result then Result := GetPropValue(Obj, Attribute)
    end
    else
      if not FullScan then exit;
    Next(Obj)
  end;
end;

function TObjectList.Sum(Attribute: string; Initial: TTransient = nil; Condition: TCondition = nil; FullScan: boolean = true): double;
var
  Obj : TTransient;
begin
  if Initial = nil then
    Obj := First
  else
    Obj := Initial;
  Result := 0;
  while Obj <> nil do begin
    if (@Condition = nil) or Condition(Obj) then
      Result := Result + GetPropValue(Obj, Attribute)
    else
      if not FullScan then exit;
    Next(Obj)
  end;
end;

function TObjectList.Avg(Attribute: string; Initial: TTransient = nil; Condition: TCondition = nil; FullScan: boolean = true): double;
var
  Obj : TTransient;
  I : integer;
begin
  if Initial = nil then
    Obj := First
  else
    Obj := Initial;
  Result := 0;
  I := 0;
  while Obj <> nil do begin
    if (@Condition = nil) or Condition(Obj) then begin
      Result := Result + GetPropValue(Obj, Attribute);
      inc(I);
    end
    else
      if not FullScan then exit;
    Next(Obj)
  end;
  if I <> 0 then Result := Result / I;
end;

function TObjectList.Scan(Attribute: string; Value : variant; Initial: TTransient = nil): TTransient;
var
  S : string;
begin
  if Initial = nil then
    Result := First
  else
    Result := Initial;
  if TVarData(Value).VType = VarString then begin
    S := Value;
    while Result <> nil do begin
      if SameText(GetStrProp(Result, Attribute), S) then exit;
      Next(Result)
    end;
  end
  else
    while Result <> nil do begin
      if GetPropValue(Result, Attribute) = Value then exit;
      Next(Result)
    end;
end;

function TObjectList.GetObject(Index: integer): TTransient; begin
  if (Index < 0) or (Index >= FCount) then raise EExtP.CreateFmt('List: "%s". Out of range access, tried to access element %d', [ClassName, Index]);
  Result := FObjects[Index];
end;

function TObjectList.GetInternalObject(Index: integer): TTransient; begin
  if (Index < 0) or (Index >= FCount) then raise EExtP.CreateFmt('List: "%s". Out of range access, tried to access element %d', [ClassName, Index]);
  try
    Result := FObjects[Index];
  except
    Result := nil
  end;
end;

procedure TObjectList.PutObject(Index: integer; const Value: TTransient); begin
  if IsSorted and not Prevalence.InEvolve then raise EExtP.CreateFmt('List: "%s". Direct attribution: "%d" is not allowed in a sorted list. Key: "%s"', [ClassName, Index, GetKey(Value)]);
  if (Index < 0) or (Index >= FCount) then raise EExtP.CreateFmt('List: "%s". Out of range attribution, tried to access element: "%d". Key: "%s"', [ClassName, Index, GetKey(Value)]);
  {$IFDEF CPU64}
  InterLockedExchange(pointer(FObjects[Index]), pointer(Value));
  {$ELSE}
  InterLockedExchange(integer(FObjects[Index]), integer(Value));
  {$ENDIF}
end;

procedure TObjectList.Grow;
var
  Delta: integer;
begin
  if FCapacity > 64 then
    Delta := Math.Min(FCapacity div 5, 1024)
  else
    if FCapacity > 8  then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TObjectList.Insert(Index: integer; pObject: TTransient); begin
  if FCount >= MAXLIST then raise EExtP.CreateFmt('Insert not allowed. List: "%s" reach %d objects!', [ClassName, MAXLIST]);
  if pObject = nil then raise EExtP.CreateFmt('Insert not allowed because object is nil. List : "%s".', [ClassName]);
  if Index = -1 then Index := 0;
  LockList.Enter;
  try
    if FCount = FCapacity then Grow;
    if FCount > Index then Move(FObjects[Index], FObjects[Index + 1], (FCount - Index) * SizeOf(TObject));
    FObjects[Index] := pObject;
    inc(FCount);
    if not Prevalence.InEvolve and (ClassName <> 'TUpdateList') and not IsAssociation then pObject.DelCreates;
  finally
    LockList.Leave;
  end;
end;

function TObjectList.Delete(pObject : TTransient) : boolean;
var
  Index : integer;
begin
  if FCount = 0 then begin
    Result := false;
    exit;
  end;
  Index := IndexOf(pObject);
  Result := Index <> -1;
  if not Result then begin
    Index := 0;
    while (FObjects[Index] <> pObject) and (Index < FCount) do inc(Index);
    Result := Index < FCount;
  end;
  if Result then Delete(Index);
end;

procedure TObjectList.Delete(Index : integer; DelObject : boolean = true); begin
  if Index <> -1 then
    try
      LockList.Enter;
      if DelObject and not Prevalence.InEvolve and (ClassName <> 'TUpdateList') and not IsAssociation then FObjects[Index].DelCreates;
      dec(FCount);
      if Index < FCount then Move(FObjects[Index + 1], FObjects[Index], (FCount - Index) * SizeOf(TObject));
    finally
      LockList.Leave;
    end;
end;

function TObjectList.GetKeyStr(pObject : TTransient) : string;
type
  TCallKeyMethod = function : string of object;
var
  Key : TMethod;
begin
  Key.Code := KeyCode;
  Key.Data := pObject;
  Result   := TCallKeyMethod(Key)
end;

function TObjectList.GetKeyWord(pObject : TTransient) : word;
type
  TCallKeyMethod = function : word of object;
var
  Key : TMethod;
begin
  Key.Code := KeyCode;
  Key.Data := pObject;
  Result   := TCallKeyMethod(Key)
end;

function TObjectList.GetKeyInt(pObject : TTransient) : integer;
type
  TCallKeyMethod = function : integer of object;
var
  Key : TMethod;
begin
  Key.Code := KeyCode;
  Key.Data := pObject;
  Result   := TCallKeyMethod(Key)
end;

function TObjectList.GetKeyI64(pObject: TTransient): int64;
type
  TCallKeyMethod = function : int64 of object;
var
  Key : TMethod;
begin
  Key.Code := KeyCode;
  Key.Data := pObject;
  Result   := TCallKeyMethod(Key)
end;

function TObjectList.GetKeyDbl(pObject: TTransient): double;
type
  TCallKeyMethod = function : double of object;
var
  Key : TMethod;
begin
  Key.Code := KeyCode;
  Key.Data := pObject;
  Result   := TCallKeyMethod(Key)
end;

function TObjectList.GetKey(pObject: TTransient) : string; begin
  if pObject = nil then raise Exception.Create('Internal Error: GetKey not allowed. Object is nil.');
  try
    case ListType of
      ltLongint,
      ltInteger : Result := IntToStr(GetKeyInt(pObject));
      ltInt64   : Result := IntToStr(GetKeyI64(pObject));
      ltWord    : Result := IntToStr(GetKeyWord(pObject));
      ltCurrency: Result := CurrToStr(GetKeyI64(pObject));
      ltDouble  : Result := FloatToStr(GetKeyDbl(pObject));
      ltDateTime: Result := DateTimeToStr(GetKeyDbl(pObject));
      ltDate    : Result := DateToStr(GetKeyDbl(pObject));
      ltTime    : Result := TimeToStr(GetKeyDbl(pObject));
      ltString  : Result := GetKeyStr(pObject);
    end;
  except
    if IsRunningStateMachine and (pObject.FThreadID = 0) then
      raise Exception.CreateFmt('Chave inválida ou nula. Classe: %s, Chave: %s.', [pObject.ClassName, pObject.MethodName(KeyCode)])
    else
      if ListInCheck <> NONELIST then raise;
  end;
end;

function TObjectList.Add(const pObject: TTransient) : integer;
var
  Ok : boolean;
begin
  if pObject = nil then raise Exception.Create('Erro interno: Add não permitido. Objeto é nulo.');
  if (IsPrimary and not IsAssociation and not IsTransient) or not IsSorted or Prevalence.InEvolve then
    Result := FCount
  else begin
    Ok := false;
    try
      case ListType of
        ltLongint,
        ltInteger : Ok := FindNear(GetKeyInt(pObject), Result, false);
        ltInt64,
        ltCurrency: Ok := FindNear(GetKeyI64(pObject), Result, false);
        ltWord    : Ok := FindNear(GetKeyWord(pObject), Result, false);
        ltDouble,
        ltDate,
        ltTime,
        ltDateTime: Ok := FindNear(GetKeyDbl(pObject), Result, false);
      else
        Ok := FindNear(GetKeyStr(pObject), Result, false);
      end;
    except
      Result := FCount;
    end;
    if Ok and not IsDuplicates then
      raise Exception.CreateFmt('Inclusão para chave já existente não é permitida. Classe: %s, Chave: %s = "%s".',
        [pObject.ClassName, pObject.MethodName(KeyCode), GetKey(pObject)]);
  end;
  Insert(Result, pObject);
end;

function TObjectList.InView(pObject : TTransient) : boolean; begin
  Result := true
end;

function TObjectList.InSession(pObject : TTransient) : boolean; begin
  Result := pObject.InSession
end;

function TObjectList.PriorNextNear(Key : string; var Index : integer; OnlyInSession : boolean = true) : boolean;
var
  I, Initial : integer;
begin
  Result := false;
  if Index <> -1 then begin
    if Index > FCount - 1 then exit;
    if (FCount > 0) and (Index < FCount) then begin
      Initial := Index;
      for I := Index downto 0 do
        if TerminateSession or ((not OnlyInSession or (OnlyInSession and InSession(FObjects[I]))) and InView(FObjects[I])) then begin
          if CompareText(Key, GetKey(FObjects[I])) <> 0 then
            if Initial <> Index then
              exit
            else
              break;
          Index := I;
          Result := true;
        end;
      if Result then exit;
      inc(Index);
      Initial := Index;
      for I := Index to FCount-1 do
        if TerminateSession or ((not OnlyInSession or (OnlyInSession and InSession(FObjects[I]))) and InView(FObjects[I])) then begin
          if CompareText(Key, GetKey(FObjects[I])) <> 0 then
            if Initial <> Index then
              exit
            else
              break;
          Index := I;
          Result := true;
        end;
      if Result then exit;
    end;
    Index := -1;
  end;
end;

function TObjectList.PriorNextNear(Key : integer; var Index : integer; OnlyInSession : boolean = true) : boolean;
var
  I, Initial : integer;
begin
  Result := false;
  if Index <> -1 then begin
    if Index > FCount - 1 then exit;
    if (FCount > 0) and (Index < FCount) then begin
      Initial := Index;
      for I := Index downto 0 do
        if TerminateSession or ((not OnlyInSession or (OnlyInSession and InSession(FObjects[I]))) and InView(FObjects[I])) then begin
          if Key <> GetKeyInt(FObjects[I]) then
            if Initial <> Index then
              exit
            else
              break;
          Index := I;
          Result := true;
        end;
      if Result then exit;
      inc(Index);
      Initial := Index;
      for I := Index to FCount-1 do
        if TerminateSession or ((not OnlyInSession or (OnlyInSession and InSession(FObjects[I]))) and InView(FObjects[I])) then begin
          if Key <> GetKeyInt(FObjects[I]) then
            if Initial <> Index then
              exit
            else
              break;
          Index := I;
          Result := true;
        end;
      if Result then exit;
    end;
    Index := -1;
  end;
end;

function TObjectList.PriorNextNear(Key : word; var Index : integer; OnlyInSession : boolean = true) : boolean;
var
  I, Initial : integer;
begin
  Result := false;
  if Index <> -1 then begin
    if Index > FCount - 1 then exit;
    if (FCount > 0) and (Index < FCount) then begin
      Initial := Index;
      for I := Index downto 0 do
        if TerminateSession or ((not OnlyInSession or (OnlyInSession and InSession(FObjects[I]))) and InView(FObjects[I])) then begin
          if Key <> GetKeyWord(FObjects[I]) then
            if Initial <> Index then
              exit
            else
              break;
          Index := I;
          Result := true;
        end;
      if Result then exit;
      inc(Index);
      Initial := Index;
      for I := Index to FCount-1 do
        if TerminateSession or ((not OnlyInSession or (OnlyInSession and InSession(FObjects[I]))) and InView(FObjects[I])) then begin
          if Key <> GetKeyWord(FObjects[I]) then
            if Initial <> Index then
              exit
            else
              break;
          Index := I;
          Result := true;
        end;
      if Result then exit;
    end;
    Index := -1;
  end;
end;

function TObjectList.PriorNextNear(Key : int64; var Index : integer; OnlyInSession : boolean = true) : boolean;
var
  I, Initial : integer;
begin
  Result := false;
  if Index <> -1 then begin
    if Index > FCount - 1 then exit;
    if (FCount > 0) and (Index < FCount) then begin
      Initial := Index;
      for I := Index downto 0 do
        if TerminateSession or ((not OnlyInSession or (OnlyInSession and InSession(FObjects[I]))) and InView(FObjects[I])) then begin
          if Key <> GetKeyI64(FObjects[I]) then
            if Initial <> Index then
              exit
            else
              break;
          Index := I;
          Result := true;
        end;
      if Result then exit;
      inc(Index);
      Initial := Index;
      for I := Index to FCount-1 do
        if TerminateSession or ((not OnlyInSession or (OnlyInSession and InSession(FObjects[I]))) and InView(FObjects[I])) then begin
          if Key <> GetKeyI64(FObjects[I]) then
            if Initial <> Index then
              exit
            else
              break;
          Index := I;
          Result := true;
        end;
      if Result then exit;
    end;
    Index := -1;
  end;
end;

function TObjectList.PriorNextNear(Key : double; var Index : integer; OnlyInSession : boolean = true) : boolean;
var
  I, Initial : integer;
begin
  Result := false;
  if Index <> -1 then begin
    if Index > FCount - 1 then exit;
    if (FCount > 0) and (Index < FCount) then begin
      Initial := Index;
      for I := Index downto 0 do
        if TerminateSession or ((not OnlyInSession or (OnlyInSession and InSession(FObjects[I]))) and InView(FObjects[I])) then begin
          if Key <> GetKeyDbl(FObjects[I]) then
            if Initial <> Index then
              exit
            else
              break;
          Index := I;
          Result := true;
        end;
      if Result then exit;
      inc(Index);
      Initial := Index;
      for I := Index to FCount-1 do
        if TerminateSession or ((not OnlyInSession or (OnlyInSession and InSession(FObjects[I]))) and InView(FObjects[I])) then begin
          if Key <> GetKeyDbl(FObjects[I]) then
            if Initial <> Index then
              exit
            else
              break;
          Index := I;
          Result := true;
        end;
      if Result then exit;
    end;
    Index := -1;
  end;
end;

function TObjectList.PriorNear(var Index : integer; OnlyInSession : boolean = true; OnlyDown : boolean = false) : boolean;
var
  I : integer;
begin
  if Index <> -1 then begin
    Result := true;
    if Index > FCount - 1 then exit;
    if (FCount > 0) and (Index < FCount) then begin
      for I := Index downto 0 do
        if TerminateSession or ((not OnlyInSession or (OnlyInSession and InSession(FObjects[I]))) and InView(FObjects[I])) then begin
          Index := I;
          exit;
        end;
      if not OnlyDown then begin
        inc(Index);
        for I := Index to FCount-1 do
          if TerminateSession or ((not OnlyInSession or (OnlyInSession and InSession(FObjects[I]))) and InView(FObjects[I])) then begin
            Index := I;
            exit;
          end;
      end;
    end;
    Index := -1;
  end;
  Result := false;
end;

function TObjectList.NextNear(var Index : integer; OnlyInSession : boolean = true) : boolean;
var
  I : integer;
begin
  if Index <> -1 then begin
    Result := true;
    for I := Index to FCount-1 do
      if TerminateSession or ((not OnlyInSession or (OnlyInSession and InSession(FObjects[I]))) and InView(FObjects[I])) then begin
        Index := I;
        exit;
      end;
    Index  := -1;
  end;
  Result := false;
end;

function TObjectList.FindNear(const Value : string; var Index : integer; OnlyInSession : boolean = true) : boolean;
var
  H, I, C: integer;
begin
  if FCount = 0 then begin
    Result := false;
    Index := -1;
    exit;
  end;
  VerifySortVersion;
  H := FCount - 1;
  Result := false;
  Index := 0;
  while Index <= H do begin
    I := (Index + H) shr 1;
    C := CompareText(GetKeyStr(FObjects[I]), Value);
    if C < 0 then
      Index := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := true;
        Index := I;
      end;
    end;
  end;
  if not Result then exit;
  Result := PriorNextNear(Value, Index, OnlyInSession);
end;

function TObjectList.FindNear(const Value : integer; var Index : integer; OnlyInSession : boolean = true) : boolean;
var
  H, I, V: integer;
begin
  if FCount = 0 then begin
    Result := false;
    Index := -1;
    exit;
  end;
  VerifySortVersion;
  H := FCount - 1;
  Result := false;
  Index := 0;
  while Index <= H do begin
    I := (Index + H) shr 1;
    V := GetKeyInt(FObjects[I]);
    if V < Value then
      Index := I + 1
    else begin
      H := I - 1;
      if V = Value then begin
        Result := true;
        Index := I;
      end;
    end;
  end;
  if not Result then exit;
  Result := PriorNextNear(Value, Index, OnlyInSession);
end;

function TObjectList.FindNear(const Value : word; var Index : integer; OnlyInSession : boolean = true) : boolean;
var
  H, I : integer;
  V : word;
begin
  if FCount = 0 then begin
    Result := false;
    Index := -1;
    exit;
  end;
  VerifySortVersion;
  H := FCount - 1;
  Result := false;
  Index := 0;
  while Index <= H do begin
    I := (Index + H) shr 1;
    V := GetKeyWord(FObjects[I]);
    if V < Value then
      Index := I + 1
    else begin
      H := I - 1;
      if V = Value then begin
        Result := true;
        Index := I;
      end;
    end;
  end;
  if not Result then exit;
  Result := PriorNextNear(Value, Index, OnlyInSession);
end;

function TObjectList.FindNear(const Value: int64; var Index: integer; OnlyInSession : boolean = true) : boolean;
var
  H, I : integer;
  V : int64;
begin
  if FCount = 0 then begin
    Result := false;
    Index := -1;
    exit;
  end;
  VerifySortVersion;
  H := FCount - 1;
  Result := false;
  Index := 0;
  while Index <= H do begin
    I := (Index + H) shr 1;
    V := GetKeyI64(FObjects[I]);
    if V < Value then
      Index := I + 1
    else begin
      H := I - 1;
      if V = Value then begin
        Result := true;
        Index := I;
      end;
    end;
  end;
  if not Result then exit;
  Result := PriorNextNear(Value, Index, OnlyInSession);
end;

function TObjectList.FindNear(const Value: double; var Index: integer; OnlyInSession : boolean = true) : boolean;
var
  H, I : integer;
  V : double;
begin
  if FCount = 0 then begin
    Result := false;
    Index := -1;
    exit;
  end;
  VerifySortVersion;
  H := FCount - 1;
  Result := false;
  Index := 0;
  while Index <= H do begin
    I := (Index + H) shr 1;
    V := GetKeyDbl(FObjects[I]);
    if V < Value then
      Index := I + 1
    else begin
      H := I - 1;
      if V = Value then begin
        Result := true;
        Index := I;
      end;
    end;
  end;
  if not Result then exit;
  Result := PriorNextNear(Value, Index, OnlyInSession);
end;

function TObjectList.FindNear(const Value: TTransient; var Index: integer; OnlyInSession : boolean = true) : boolean; begin
  if Value <> nil then
    case ListType of
      ltLongint,
      ltInteger : Result := FindNear(GetKeyInt(Value), Index, OnlyInSession);
      ltInt64,
      ltCurrency: Result := FindNear(GetKeyI64(Value), Index, OnlyInSession);
      ltWord    : Result := FindNear(GetKeyWord(Value), Index, OnlyInSession);
      ltDouble,
      ltDate,
      ltTime,
      ltDateTime: Result := FindNear(GetKeyDbl(Value), Index, OnlyInSession);
    else
      Result := FindNear(GetKeyStr(Value), Index, OnlyInSession);
    end
  else begin
    Result := false;
    Index := -1
  end;
end;

function TObjectList.IndexOf(const Value : string): integer; begin
  if not FindNear(Value, Result) then Result := -1
end;

function TObjectList.IndexOf(const Value : integer): integer; begin
  if not FindNear(Value, Result) then Result := -1
end;

function TObjectList.IndexOf(const Value : word): integer; begin
  if not FindNear(Value, Result) then Result := -1
end;

function TObjectList.IndexOf(const Value: int64): integer; begin
  if not FindNear(Value, Result) then Result := -1
end;

function TObjectList.IndexOf(const Value: double): integer; begin
  if not FindNear(Value, Result) then Result := -1
end;

function TObjectList.IndexOf(const Value : TTransient): integer;
var
  Temp,
  LastIndex : integer;
begin
  if Value <> nil then begin
    case ListType of
      ltLongint,
      ltInteger : Result := IndexOf(GetKeyInt(Value));
      ltInt64,
      ltCurrency: Result := IndexOf(GetKeyI64(Value));
      ltWord    : Result := IndexOf(GetKeyWord(Value));
      ltDouble,
      ltDate,
      ltTime,
      ltDateTime: Result := IndexOf(GetKeyDbl(Value));
    else
      Result := IndexOf(GetKeyStr(Value));
    end;
    if (Result <> -1) and (Value <> FObjects[Result]) then begin
      Temp := Result;
      inc(Result);
      while NextNear(Result) and (Result < FCount) do begin
        if Value = FObjects[Result] then exit;
        if GetKey(FObjects[Result]) <> GetKey(Value) then break;
        inc(Result);
      end;
      Result := Temp;
      dec(Result);
      LastIndex := -1;
      while PriorNear(Result) do begin
        if Value = FObjects[Result] then exit;
        if GetKey(FObjects[Result]) <> GetKey(Value) then break;
        dec(Result);
        if LastIndex = Result then break;
        LastIndex := Result;
      end;
      Result := -1;
    end;
  end
  else
    Result := -1;
end;

function TObjectList.Find(Key : integer): TTransient;
var
  I : integer;
begin
  I := IndexOf(Key);
  if I >= 0 then
    Result := Objects[I]
  else
    Result := nil;
end;

function TObjectList.Find(Key : word): TTransient;
var
  I : integer;
begin
  I := IndexOf(Key);
  if I >= 0 then
    Result := Objects[I]
  else
    Result := nil
end;

function TObjectList.Find(Key : int64): TTransient;
var
  I : integer;
begin
  I := IndexOf(Key);
  if I >= 0 then
    Result := Objects[I]
  else
    Result := nil
end;

function TObjectList.Find(Key : double): TTransient;
var
  I : integer;
begin
  I := IndexOf(Key);
  if I >= 0 then
    Result := Objects[I]
  else
    Result := nil
end;

function TObjectList.Find(Key : string): TTransient;
var
  I : integer;
begin
  I := IndexOf(Key);
  if I >= 0 then
    Result := Objects[I]
  else
    Result := nil
end;

function TObjectList.Find(Key : TTransient): TTransient;
var
  I : integer;
begin
  I := IndexOf(Key);
  if I >= 0 then
    Result := Objects[I]
  else
    Result := nil
end;

function TObjectList.Find(Key : variant): TTransient;
var
  I : integer;
begin
  with TVarData(Key) do
    case ListType of
      ltLongint,
      ltInteger : I := IndexOf(VInteger);
      ltInt64,
      ltCurrency: I := IndexOf(VInt64);
      ltWord    : I := IndexOf(VWord);
      ltDouble,
      ltDate,
      ltTime,
      ltDateTime: I := IndexOf(VDouble);
    else
      I := IndexOf(VarToStr(Key));
    end;
  if I >= 0 then
    Result := Objects[I]
  else
    Result := nil
end;

function TObjectList.First: TTransient;
var
  I : integer;
begin
  Result := nil;
  VerifySortVersion;
  if FCount > 0 then begin
    I := 0;
    if NextNear(I) then Result := Objects[I];
  end;
end;

function TObjectList.Last: TTransient;
var
  I : integer;
begin
  Result := nil;
  VerifySortVersion;
  if FCount > 0 then begin
    I := FCount-1;
    if PriorNear(I) then Result := Objects[I];
  end;
end;

// Wander Elimina ponteiros duplicados
procedure TObjectList.FixDuplicates(pObject : TTransient; I : integer); begin
  Trace('Correção automática. Lista: ' + ClassName + ' com objeto duplicado ' + pObject.GetIdentification);
  Delete(I, false);
end;

function TObjectList.Next(var pObject: TTransient): boolean;
var
  I : integer;
begin
  if FCount = 1 then begin
    pObject := nil;
    Result := false;
    exit;
  end;
  if FindNear(pObject, I, false) then begin
    while pObject <> FObjects[I] do inc(I);
    inc(I);
  end;
  if (I < FCount) and NextNear(I) then begin
    if pObject = Objects[I] then begin
      FixDuplicates(pObject, I);
      Next(pObject);
    end;
    pObject := Objects[I];
  end
  else
    pObject := nil;
  Result := Assigned(pObject);
{
  I := IndexOf(pObject);
  pObject := nil;
  if (I >= 0) and (I < FCount) then begin
    inc(I);
    if NextNear(I) then
      pObject := Objects[I];
  end
  else
    pObject := nil;
  Result := Assigned(pObject);
}
end;

function TObjectList.Prior(var pObject: TTransient): boolean;
var
  I : integer;
begin
  if FCount = 1 then begin
    pObject := nil;
    Result := false;
    exit;
  end;
  if FindNear(pObject, I, false) then begin
    while pObject <> FObjects[I] do inc(I);
    dec(I);
  end;
  if (I >= 0) and PriorNear(I, true, true) then begin
    if pObject = Objects[I] then begin
      FixDuplicates(pObject, I);
      Prior(pObject);
    end;
    pObject := FObjects[I]
  end
  else
    pObject := nil;
  Result := Assigned(pObject);
{
  I := IndexOf(pObject);
  pObject := nil;
  if I > 0 then begin
    dec(I);
    if PriorNear(I) then
      pObject := Objects[I];
  end;
  Result := Assigned(pObject);
}
end;

function TObjectList.PriorInNear(Index : integer; Key : variant; IsString : boolean = false): TTransient;
var
  //I : integer;
  KeyStr: string;
  AuxObj: TTransient;
begin
{  Result := Objects[Index];
  I := Index - 1;
  while true do
    if PriorNear(I) then begin
      if (I < 0) or (I >= Index) then exit;
      if IsString then begin
        if not AnsiStartsText(Key , GetKeyStr(Objects[I])) then exit
      end else
        if GetKey(Objects[I]) <> VarToStr(Key) then exit;
      Result := Objects[I];
      dec(I);
    end else
      exit;}
  // Walter!!  Nota: poderia ter apenas o objeto como parâmetro!!
  Result := Objects[Index];
  KeyStr := VarToStr(Key);
  repeat
    AuxObj := Result;
    Prior(AuxObj);
    if (AuxObj = nil) or (IsString and not AnsiStartsText(KeyStr , GetKeyStr(AuxObj))) or (not IsString and (GetKey(AuxObj) <> KeyStr)) then
      break;
    Result := AuxObj;
  until false;
end;

function TObjectList.Near(Key : integer): TTransient;
var
  I : integer;
begin
  if (not FindNear(Key, I) and (I >= FCount)) or (I < 0) then
    Result := nil
  else
    Result := PriorInNear(I, Key);
end;

function TObjectList.Near(Key : word): TTransient;
var
  I : integer;
begin
  if (not FindNear(Key, I) and (I >= FCount)) or (I < 0) then
    Result := nil
  else
    Result := PriorInNear(I, Key);
end;

function TObjectList.Near(Key : int64): TTransient;
var
  I : integer;
begin
  if (not FindNear(Key, I) and (I >= FCount)) or (I < 0) then
    Result := nil
  else
    Result := PriorInNear(I, Key);
end;

function TObjectList.Near(Key : double): TTransient;
var
  I : integer;
begin
  if (not FindNear(Key, I) and (I >= FCount)) or (I < 0) then
    Result := nil
  else
    Result := PriorInNear(I, Key);
end;

function TObjectList.Near(Key : string): TTransient;
var
  I: integer;
begin
  if (not FindNear(Key, I) and (I >= FCount)) or (I < 0) then
    Result := nil
  else
    Result := PriorInNear(I, Key, true);
end;

function TObjectList.Near(Key : variant) : TTransient; begin
  with TVarData(Key) do
    case ListType of
      ltLongint,
      ltInteger : Result := Near(VInteger);
      ltInt64,
      ltCurrency: Result := Near(VInt64);
      ltWord    : Result := Near(VWord);
      ltDouble,
      ltDate,
      ltTime,
      ltDateTime: Result := Near(VDouble);
    else
      Result := Near(VarToStr(Key));
    end;
end;

procedure TObjectList.QuickSortStr(L, R: integer);
var
  I, J, P : integer;
  Temp    : TTransient;
  Key : string;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Key:= GetKeyStr(FObjects[P]);
      while CompareText(GetKeyStr(FObjects[I]), Key) < 0 do inc(I);
      while CompareText(GetKeyStr(FObjects[J]), Key) > 0 do dec(J);
      if I < J then begin
        Temp        := FObjects[I];
        FObjects[I] := FObjects[J];
        FObjects[J] := Temp;
        if P = I then
          P := J
        else
          if P = J then P := I;
        inc(I);
        dec(J);
      end
      else
        if I = J then begin
          inc(I);
          dec(J);
          break;
        end;
    until I > J;
    if L < J then QuickSortStr(L, J);
    L := I;
  until I >= R;
end;

procedure TObjectList.QuickSortInt(L, R: integer);
var
  I, J,
  P, Key : integer;
  Temp   : TTransient;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Key:= GetKeyInt(FObjects[P]);
      while GetKeyInt(FObjects[I]) < Key do inc(I);
      while GetKeyInt(FObjects[J]) > Key do dec(J);

      if I < J then begin
        Temp        := FObjects[I];
        FObjects[I] := FObjects[J];
        FObjects[J] := Temp;
        if P = I then P := J
        else
        if P = J then P := I;
        inc(I);
        dec(J);
      end
      else
      if I = J then begin
        inc(I);
        dec(J);
        break;
      end;
    until I > J;
    if L < J then QuickSortInt(L, J);
    L := I;
  until I >= R;
end;

procedure TObjectList.QuickSortWord(L, R: integer);
var
  I, J, P : integer;
  Temp    : TTransient;
  Key     : word;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Key:= GetKeyWord(FObjects[P]);
      while GetKeyWord(FObjects[I]) < Key do inc(I);
      while GetKeyWord(FObjects[J]) > Key do dec(J);
      if I < J then begin
        Temp        := FObjects[I];
        FObjects[I] := FObjects[J];
        FObjects[J] := Temp;
        if P = I then
          P := J
        else
          if P = J then P := I;
        inc(I);
        dec(J);
      end
      else
        if I = J then begin
          inc(I);
          dec(J);
          break;
        end;
    until I > J;
    if L < J then QuickSortWord(L, J);
    L := I;
  until I >= R;
end;

procedure TObjectList.QuickSortI64(L, R: integer);
var
  I, J, P : integer;
  Temp    : TTransient;
  Key     : int64;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Key:= GetKeyI64(FObjects[P]);
      while GetKeyI64(FObjects[I]) < Key do inc(I);
      while GetKeyI64(FObjects[J]) > Key do dec(J);
      if I < J then begin
        Temp        := FObjects[I];
        FObjects[I] := FObjects[J];
        FObjects[J] := Temp;
        if P = I then
          P := J
        else
          if P = J then P := I;
        inc(I);
        dec(J);
      end
      else
        if I = J then begin
          inc(I);
          dec(J);
          break;
        end;
    until I > J;
    if L < J then QuickSortI64(L, J);
    L := I;
  until I >= R;
end;

procedure TObjectList.QuickSortDbl(L, R: integer);
var
  I, J, P : integer;
  Temp    : TTransient;
  Key     : double;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Key:= GetKeyDbl(FObjects[P]);
      while GetKeyDbl(FObjects[I]) < Key do inc(I);
      while GetKeyDbl(FObjects[J]) > Key do dec(J);
      if I < J then begin
        Temp        := FObjects[I];
        FObjects[I] := FObjects[J];
        FObjects[J] := Temp;
        if P = I then
          P := J
        else
          if P = J then P := I;
        inc(I);
        dec(J);
      end
      else
        if I = J then begin
          inc(I);
          dec(J);
          break;
        end;
    until I > J;
    if L < J then QuickSortDbl(L, J);
    L := I;
  until I >= R;
end;

function TObjectList.CheckSortStr(L, R: integer) : boolean;
var
  I, J, P : integer;
  Key     : string;
begin
  I := L;
  J := R;
  P := (L + R) shr 1;
  Key:= GetKeyStr(FObjects[P]);
  while CompareText(GetKeyStr(FObjects[I]), Key) < 0 do inc(I);
  while CompareText(GetKeyStr(FObjects[J]), Key) > 0 do dec(J);
  if (I < J) and (GetKeyStr(FObjects[I]) <> GetKeyStr(FObjects[J])) then begin
    Sort;
    Result := false;
  end
  else
    Result := true;
end;

function TObjectList.CheckSortInt(L, R: integer) : boolean;
var
  I, J,
  P, Key : integer;
begin
  I := L;
  J := R;
  P := (L + R) shr 1;
  Key:= GetKeyInt(FObjects[P]);
  while GetKeyInt(FObjects[I]) < Key do inc(I);
  while GetKeyInt(FObjects[J]) > Key do dec(J);
  if (I < J) and (GetKeyInt(FObjects[I]) <> GetKeyInt(FObjects[J])) then begin
    Sort;
    Result := false;
  end
  else
    Result := true;
end;

function TObjectList.CheckSortWord(L, R: integer) : boolean;
var
  I, J, P : integer;
  Key     : word;
begin
  I := L;
  J := R;
  P := (L + R) shr 1;
  Key:= GetKeyWord(FObjects[P]);
  while GetKeyWord(FObjects[I]) < Key do inc(I);
  while GetKeyWord(FObjects[J]) > Key do dec(J);
  if (I < J) and (GetKeyWord(FObjects[I]) <> GetKeyWord(FObjects[J])) then begin
    Sort;
    Result := false;
  end
  else
    Result := true;
end;

function TObjectList.CheckSortI64(L, R: integer) : boolean;
var
  I, J, P : integer;
  Key     : int64;
begin
  I := L;
  J := R;
  P := (L + R) shr 1;
  Key:= GetKeyI64(FObjects[P]);
  while GetKeyI64(FObjects[I]) < Key do inc(I);
  while GetKeyI64(FObjects[J]) > Key do dec(J);
  if (I < J) and (GetKeyI64(FObjects[I]) <> GetKeyI64(FObjects[J])) then begin
    Sort;
    Result := false;
  end
  else
    Result := true;
end;

function TObjectList.CheckSortDbl(L, R: integer) : boolean;
var
  I, J, P : integer;
  Key     : double;
begin
  I := L;
  J := R;
  P := (L + R) shr 1;
  Key:= GetKeyDbl(FObjects[P]);
  while GetKeyDbl(FObjects[I]) < Key do inc(I);
  while GetKeyDbl(FObjects[J]) > Key do dec(J);
  if (I < J) and (GetKeyDbl(FObjects[I]) <> GetKeyDbl(FObjects[J])) then begin
    Sort;
    Result := false;
  end
  else
    Result := true;
end;

function TObjectList.CheckSort : boolean; begin
  Result := true;
  if FCount > 1 then
    try
      LockList.Enter;
      InSort := true;
      case ListType of
        ltLongint,
        ltInteger : Result := CheckSortInt(0, FCount - 1);
        ltInt64,
        ltCurrency: Result := CheckSortI64(0, FCount - 1);
        ltWord    : Result := CheckSortWord(0, FCount - 1);
        ltDouble,
        ltDate,
        ltTime,
        ltDateTime: Result := CheckSortDbl(0, FCount - 1);
      else
        Result := CheckSortStr(0, FCount - 1);
      end;
    finally
      LockList.Leave;
      InSort := false;
    end;
end;

procedure TObjectList.VerifySortVersion; begin
end;

procedure TObjectList.Sort; begin
  if (ListInCheck = NONELIST) and (FCount < 2) then exit;
  if FCount > 0 then
    try
      try
        LockList.Enter;
        InSort := true;
        case ListType of
          ltLongint,
          ltInteger : QuickSortInt(0, FCount - 1);
          ltInt64,
          ltCurrency: QuickSortI64(0, FCount - 1);
          ltWord    : QuickSortWord(0, FCount - 1);
          ltDouble,
          ltDate,
          ltTime,
          ltDateTime: QuickSortDbl(0, FCount - 1);
        else
          QuickSortStr(0, FCount - 1)
        end;
      except
      end;
    finally
      LockList.Leave;
      InSort := false;
    end;
end;

procedure TObjectList.FastQuickSort(pObject : TTransient);
var
  I : integer;
begin
  if FCount = 1 then exit;
  try
    I := IndexOf(pObject);
  except
    I := -1;
  end;
  if I = -1 then begin
    I := 0;
    while (pObject <> FObjects[I]) and (I < FCount) do inc(I);
    if I <> FCount then Sort;
  end
  else begin
    Delete(I);
    Add(pObject);
  end;
end;

procedure TObjectList.Sort(pObject : TTransient); begin
  if FCount > 1 then
    try
      LockList.Enter;
      InSort := true;
      FastQuickSort(pObject);
    finally
      LockList.Leave;
      InSort := false;
    end;
end;

{TProtectedObjectList}

destructor TProtectedObjectList.Destroy; begin
  if not IsTransient then raise EExtP.CreateFmt('Operação de Free não é permitida em listas da prevalência (%s)', [ClassName])
end;

destructor TProtectedObjectList.InternalDestroy; begin
  inherited Destroy;
end;

procedure TProtectedObjectList.InternalFree; begin
  if Self <> nil then InternalDestroy;
end;

{ TTransient }

procedure TTransient.Add; begin end;

procedure TTransient.Delete; begin
  InternalFree;
end;

destructor TTransient.Destroy; begin
  raise EExtP.CreateFmt('Operação de Free não é permitida em Objetos Prevalents (%s)', [ClassName])
end;

function TTransient.InSession: boolean; begin // Não considera os deletados
  Result := (FThreadID = 0) or (FThreadID = integer(GetFCGIThreadID))
end;

destructor TTransient.InternalDestroy; begin
  inherited Destroy;
end;

function ValidObject(O : TObject): boolean; begin
  Result := (O <> nil) and (Int64(pointer(O)^) <> $7777777777777777)
end;

procedure TTransient.InternalFree; begin
  if Self <> nil then begin
    InternalDestroy;
    Int64(pointer(Self)^) := $7777777777777777;
  end;
end;

procedure TTransient.DelCreates; begin end;

{Retorna o @[link .ID] como chave primária. É usado pela lista primária da classe para acessar e classificar a lista.
Esta chave não aceita duplicates. Não há necessidade de chamar este método diretamente.}
function TTransient.GetIdentification: string; begin
  Result := IntToStr(FID)
end;

function TTransient.GetPermissionView: T_ViewPermissionSet; begin
  Result := [_vpShow, _vpModify, _vpDelete]
end;

function TTransient.PrimaryKey: Integer; begin
  Result := FID
end;

end.
