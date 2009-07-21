{$I epDirectives.inc}{$define checksort}

unit epPrevalence;

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ELSE}Unix,{$ENDIF}
  Classes, TypInfo, SyncObjs, epUtils, epObjectList, epProperties, epCommon;

const
  PrevVersion = '1.0.1';
  PrevalenceVersion = PrevVersion + ' - ' + {$IFNDEF FPC}'Win32 - i386 - powered by Delphi'{$ELSE}
    {$I %FPCTARGETOS%} + ' - ' + {$I %FPCTARGETCPU%} + ' - powered by FPC ' + {$I %FPCVersion%} + ' - ' + {$I %FPCDATE%} {$ENDIF};

type
  TTransactionStream = class;
  TPrevalentList = packed class;

  TPrevalent = packed class(TTransient)
  private
    FVersion   : byte;
    FListIndex : word; // ** Eco
    procedure EvolveClass(var NameTarget : string; Source : integer; var Evolve : TEvolveRecord; Index : integer);
    function EvolveAssociation(var NameTarget : string; var Evolve : TEvolveRecord; Index : integer) : TObject;
    function GetEvolveProperty(var NameTarget: string): PPropInfo;
    function GetIsTransient : boolean;
    procedure HandleReflectionsForDelete(CascadeObject : TPrevalent = nil; InReflection : boolean = false);
    procedure HandleReflectionsForUpdate(NewImage, PropObject, PrevObject : TPrevalent; Prop : integer; Stream : TTransactionStream; Props : TProperties; InRollback : boolean = false);
    procedure HandleReflectionsForInsert;
    procedure SetAssociationToNil;
    procedure SetTargetToNil(PropObject: TPrevalent; Target: integer; CascadeObject : TPrevalent = nil; InReflection : boolean = false);
    function  GetFirstProperty: string;
  protected
    procedure New; virtual;
    procedure ResolveDependencyLists(List : TPrevalentList; NewImage : TPrevalent); overload;
    procedure ResolveDependencyLists(Index : word; NewImage : TPrevalent); overload;
    procedure MoveCorresponding(Target : TPrevalent);
    procedure MoveSameClass(Target : TPrevalent);
    procedure UpdateLog(Index : word; NewImage : TPrevalent; Stream : TTransactionStream; InRollback : boolean = false);
    procedure EfectiveDelete;
    procedure DeleteInReflection;
    procedure DoCheckConstraints(CompleteCheck : boolean = true; OldImage : TPrevalent = nil);
    procedure MergeDependecyLists(var List : TList; const Prop : integer); overload;
    procedure MergeDependecyLists(var List : TList); overload;
    procedure DelCreates; override;
    procedure InternalFree; override;
    function FixPropObject(Obj: TObject; Prop: integer): TObject;
  public
    constructor Create(DoNew : boolean = true);
    procedure Add; override;
    procedure Delete; override;
    procedure DuplicateObject(Target : TPrevalent);
    function CopyObjectCascade(Excepts : array of string) : TPrevalent; overload;
    function CopyObjectCascade : TPrevalent; overload;
    function  GetIdentification: string; override;
    function  GetClassAlias: string;
    function  Equal(Target : TPrevalent) : boolean;
    function  GetProperty(Prop : integer) : variant;
    procedure SetProperty(Prop : integer; Value : variant);
    class function CallMethod(pMethodName : string; Params : TMethodParams; const SelfParam: pointer) : variant; overload;
    function  CallMethod(pMethodName : string; Params : TMethodParams) : variant; overload;
    function  Metadata : TProperties;
    function  CheckPropConstraint(Transient : TTransient; Prop: integer; OldImage : TTransient = nil; CompleteCheck : boolean = true): string;
    function  GetPermissionView: T_ViewPermissionSet; override;
    procedure SetDependencyLists(Prop : word);
    property  IsTransient : boolean read GetIsTransient;
    property  Version  : byte read FVersion;
    property  ListIndex : word read FListIndex;
  end;

  TPrevalentClass = class of TPrevalent;

  TPrevalentList = class(TProtectedObjectList)
  private
    ListIndex,
    PrevalentsIndex     : word;
    FAutoIncInfo        : PPropInfo;
    Family,
    DependencyListAssoc : TList;
    IsManaged,
    PendingFastAdd      : boolean;
    NextManaged         : TPrevalentList;
    FilterCode          : pointer; // ** Eco
    FastAdds            : byte;
    LastRecovered       : TPrevalent;
    procedure SetFastAdd(const Value: boolean);
    function  GetFastAdd : boolean;
    procedure CheckCascade(Prevalent: TPrevalent; Origin : integer = -1);
    procedure DeleteCascade(Prevalent : TPrevalent);
    procedure WriteMetadata(Stream : TMemoryStream);
    procedure SetCapacity(NewCapacity : integer);
    procedure SetLastRecovered(Index : integer);
  protected
    property FastAdd : boolean read GetFastAdd write SetFastAdd;
    function InView(pObject : TTransient) : boolean; override;
    procedure VerifySortVersion; override;
  public
    destructor  Destroy; override;
    constructor Create(pListProps : TListProps = [lpPrimary]; Filter : string = ''; AutoIncField : string = '');
    procedure Add(Prevalent : TPrevalent; CompleteCheck : boolean = true); overload;
    procedure Delete(Prevalent : TPrevalent; CascadeObject : TPrevalent = nil; InReflection : boolean = false); overload;
    function InFilter(Prevalent : TPrevalent) : boolean;
    function FindInFamily(ID: integer): TPrevalent;
    function FindKeyInFamily(Value: variant): TPrevalent;
    function ScanInFamily(Attribute: string; Value: variant): TPrevalent; overload;
    function ScanInFamily(Attribute : string; Prevalent : TPrevalent; IsAssociation : boolean = false) : TPrevalent; overload;
    procedure Sort;
  end;

  TAssociation = packed class(TProtectedObjectList)
  private
    TransObjs : TList;
    procedure AddTransObj(pPrevalent : TPrevalent; IsAdd, IsReflect : boolean);
    function FindTransObj(pPrevalent : TPrevalent) : boolean;
    function IsInAssociation(pPrevalent : TPrevalent) : boolean;
    function GetCount : integer;
    function GetOwner : TPrevalent;
  protected
    Owner     : TPrevalent;
    PropIndex : byte; // ** Eco
    destructor InternalDestroy; override;
    function InView(pObject : TTransient) : boolean; override;
    function InSession(pPrevalent : TTransient) : boolean; override;
    function GetTargetProp : integer;
    procedure VerifySortVersion; override;
  public
    class function MinConstraint : integer; virtual;
    class function MaxConstraint : integer; virtual;
    constructor Create(pOwner : TPrevalent; OwnerField: string; isTransient : boolean = false);
    procedure Add(Prevalent: TPrevalent; InReflection : boolean = false; IsRollback : boolean = false); reintroduce;
    procedure Delete(Prevalent: TPrevalent; CascadeObject : TPrevalent = nil; InReflection : boolean = false; InRollBack : boolean = false); overload;
    procedure SetDependencyLists;
    procedure Clear;
    function IsAssociation : boolean; override;
    property Count : integer read GetCount;
    property AssocOwner : TPrevalent read GetOwner;
    procedure Sort;
  end;

  TAssociationClass = class of TAssociation;

  TTransactionStream = class(TMemoryStream)
  private
    UpdateObject : TPrevalent;
    DirectAccess,
    GetLastImage,
    DontUpdateLog: boolean;
    procedure WriteProperty (Prevalent : TPrevalent; Props : TProperties; Prop : word; IsSnapShot : boolean = false);
    procedure WritePrevalent(Prevalent : TPrevalent; IsSnapShot : boolean = false);
  end;

  TPrevalentStream = class(TFileStream)
    Position : integer;
    Version : single;
    function Read(var Buffer; Count: integer): integer; reintroduce;
  private
    FBufferReaded : array[0..65535] of byte;
    LenBufferReaded, PosBufferReaded : integer;
    procedure ReadVersion;
    procedure WriteVersion;
    function ReadFromStandard(var Buffer; Count: integer): integer;
    procedure ReadProperty(Prevalent: TPrevalent; Props: TProperties; Prop: word; IsSnapShot: boolean = false);
    function ReadPrevalentEvolve(Prevalent : TPrevalent; Evolve : TEvolveRecord) : TPrevalent;
    procedure ReadPrevalentUpdate(Prevalent: TPrevalent);
  protected
    procedure LoadHeaderFile(var HeaderFile : THeaderFile);
    function ReadPrevalent(Prevalent : TPrevalent; IsSnapShot : boolean = false) : TPrevalent;
    procedure ReadAuditPrevalentUpdate(Prevalent: TPrevalent; IsByPass : boolean);
    procedure CopyFrom(Transaction : TMemoryStream);
    constructor Create(const FileName: string; Mode: Word);
  public
    destructor Destroy; override;
  end;

  TIsolationLevel = (ReadUncommited, ReadCommited, RepeatableRead, Serializable);

  TPrevalentImages = record
    NewImage : TPrevalent;
    Stream   : TTransactionStream;
  end;

  TPrevalence = packed class
  private
    FVersion,
    FSnapShotVersion : single;
    FRecoveredLists : integer;
    FStartupDate    : TDateTime;
    CriticalSection : TCriticalSection;
    FAbstractLists,
    FamilyList      : TStringList;
    FileName        : string;
    LogFile,
    AuditFile       : TPrevalentStream;
    FForceFlush,
    InRecover,
    InRecoverSnapShot,
    PartialRecover,
    InWaitSnapShot,
    ManagedLists    : boolean;
    GlobalTransactionLevel : integer;
    function GetStream : TTransactionStream;
    procedure Log(Operation: T_Operation; Association : TAssociation; Prevalent : TPrevalent); overload;
    procedure LoadDependencyLists;
    procedure VerifySecondaryLists;
    procedure ResolveIDs;
    procedure RecoverLog;
    procedure IncTransactionLevel;
    procedure DecTransactionLevel;
    procedure SetForceFlush(const Value: boolean);
    procedure Evolve(Stream : TPrevalentStream);
    procedure AdjustListIndex;
    procedure AddAbstractClass(AbstractClass: TTransientClass; AbstractList : TPrevalentList);
    property RecoveredLists : integer read FRecoveredLists write FRecoveredLists;
  protected
    procedure Log(Operation : T_Operation; Prevalent : TPrevalent; ImmediateWrite : boolean = false); overload;
  public
    InEvolve : boolean;
    EvolveData  : TEvolveData;
    FPrevalentLists,
    Prevalents : TStringList;
    function PrevalentLists(ListIndex : word) : TPrevalentList; overload;
    function PrevalentLists(ListName : string) : TPrevalentList; overload;
    function AbstractLists(ListName : string) : TPrevalentList;
    constructor Create(pFileName : string);
    destructor Destroy; override;
    procedure BeginThread;
    procedure EndThread;
    procedure Recover;
    procedure BeginTransaction(pIsolationLevel : TIsolationLevel = ReadCommited);
    function CheckTransaction : string;
    procedure EndTransaction(Force : boolean = false);
    procedure RollBack(Msg : string = '');
    procedure ClearTransaction;
    procedure CheckSortList(Trace : boolean);
    procedure CheckModel(AndClean : boolean = false; pClass : TClass = nil);
    procedure SnapShot;
    function FindInUpdate(Prevalent: TPrevalent) : TPrevalent;
    function GetImages(Prevalent: TPrevalent): TPrevalentImages;
    function GetNewImage(Prevalent: TPrevalent; Prop : integer = -1): TPrevalent;
    function Metadata(pClassName : string) : TProperties;
    property ForceFlush : boolean read FForceFlush write SetForceFlush;
    property Version : single read FVersion;
    property SnapShotVersion : single read FSnapShotVersion;
    property StartupDate : TDateTime read FStartupDate;
    property IsInRecover : boolean read InRecover;
    property IsInRecoverSnapShot : boolean read InRecoverSnapShot;
    property IsPartialRecover : boolean read PartialRecover;
  end;

  TOnSetRunningMethod = procedure(const MethodName : string) of object;

  TUpdatedPrevalent = class(TTransient)
    OldImage, NewImage, LastImage : TPrevalent;
    function ByOldImage : PtrInt;
  end;

  TUpdateList = class(TProtectedObjectList)
    constructor Create;
    class function GetKeyCode : pointer; override;
    class function GetObjectClass: TTransientClass; override;
  end;

  TTransaction = class
  private
    Level          : integer;
    IsolationLevel : TIsolationLevel;
    InsDels,
    Associations,
    Creates        : TList;
    function BeginUpdate(Prevalent : TPrevalent) : TUpdatedPrevalent;
    procedure AddInsDels(Prevalent : TPrevalent);
  protected
    Updates        : TUpdateList;
    Stream         : TTransactionStream;
    AuditStream    : TMemoryStream;
    MapeamentoAuditLog : TList;
  public
    destructor Destroy; override;
  end;

threadvar
  CheckConstraints : boolean;
  ThreadTrans      : TTransaction;

type
  TProcedure = procedure;

var
  Prevalence         : TPrevalence; // Objeto singleton que faz referência a toda a prevalência (ObjectSpace). @see TPrevalence
  OnSetRunningMethod : TOnSetRunningMethod = nil;
  DirSnapShot, DirLog, DirBackup, DirAudit : string;

function  Recover  : boolean;
function  SnapShot : boolean;
procedure BeginTransaction(pIsolationLevel : TIsolationLevel = ReadCommited);
procedure EndTransaction(Force : boolean = false);
procedure RollBack;
procedure FixReference(var Reference : TTransient; TransientClass : TTransientClass);

implementation

uses
  Variants, SysUtils, StrUtils, DateUtils, Math, {$IFDEF SERVICE}Services,{$ENDIF} epServer;

type
  TInsDels = packed record
    IsAdd : boolean;
    Obj   : TPrevalent;
  end;
  PInsDels = ^TInsDels;

  TTransObj = packed record
    ThreadID  : integer;
    IsReflect : boolean;
    Prevalent : TPrevalent;
  end;
  PTransObj = ^TTransObj;

procedure FlushFileBuffers(Handle : integer); begin
{$IFDEF MSWINDOWS}
  Windows.FlushFileBuffers(Handle)
{$ELSE}
  fpFSync(Handle)
{$ENDIF}
end;

procedure GetDirectories; begin
  DirSnapShot := GetIniParameter('Directory', 'SnapShot', '');
  if (DirSnapShot <> '') and not DirectoryExists(DirSnapShot) then CreateDir(copy(DirSnapShot, 1, length(DirSnapShot) - 1));
  DirLog := GetIniParameter('Directory', 'Log', '');
  if (DirLog <> '') and not DirectoryExists(DirLog) then CreateDir(copy(DirLog, 1, length(DirLog) - 1));
  DirBackup := GetIniParameter('Directory', 'Backup', '');
  if (DirBackup <> '') and not DirectoryExists(DirBackup) then CreateDir(copy(DirBackup, 1, length(DirBackup) - 1));
  DirAudit := GetIniParameter('Directory', 'Audit', '');
  if (DirAudit <> '') and not DirectoryExists(DirAudit) then CreateDir(copy(DirAudit, 1, length(DirAudit) - 1));
end;

function GetFileExtension(Data : TDateTime) : string; begin
{$IFDEF DEBUG}
  Result := '_Safe';
{$ELSE}
  Result := '_' + FormatDateTime('yyyymmdd_hhnnss', Data);
{$ENDIF}
end;

procedure RenameFiles(OldName , NewName : string); begin
  if FileExists(NewName) then DeleteFile(NewName);
  RenameFile(OldName, NewName);
end;

function GetVersion : single;
var
  S : string;
  P : integer;
begin
  P := 1;
  S := ExtractFromStr(PrevVersion, P, '.');
  Result := StrToInt(S) * 10000;
  S := ExtractFromStr(PrevVersion, P, '.');
  if S <> '' then
    Result := Result + StrToInt(S) * 100
  else begin
    S := ExtractFromStr(PrevVersion, P, '.');
    if S <> '' then Result := Result + StrToInt(S);
  end;
end;

{ TPrevalent }

function TPrevalent.GetIsTransient: boolean; begin
  Result := Prevalence.PrevalentLists(FListIndex).IsTransient
end;

procedure TPrevalent.New; begin end;

constructor TPrevalent.Create(DoNew : boolean = true); begin
  if ThreadTrans = nil then raise EExtP.Create('Erro: Transação sem BeginTransaction.');
  try
    inherited Create;
    FListIndex := word(Prevalence.FPrevalentLists.IndexOf(ClassName + 'List'));
    if DoNew then New;
    if not Prevalence.IsInRecover and DoNew and (ClassName <> 'TUpdateList') then
      ThreadTrans.Creates.Add(Self);
  except
    on E : Exception do
      raise EExtP.CreateFmt('Erro na criação de Objeto: %s', [E.Message])
  end;
end;

procedure TPrevalent.Add; begin
  Prevalence.PrevalentLists(FListIndex).Add(Self);
end;

procedure TPrevalent.Delete; begin
  Prevalence.PrevalentLists(FListIndex).Delete(Self, nil, false);
end;

procedure TPrevalent.DeleteInReflection; begin
  Prevalence.PrevalentLists(FListIndex).Delete(Self, nil, true);
end;

function TPrevalent.GetEvolveProperty(var NameTarget : string) : PPropInfo;
var
  I, Prop : integer;
begin
  if NameTarget <> '' then begin
    Result := GetPropInfo(Self, NameTarget);
    with Metadata do
      if Result = nil then begin // Mudou o Nome ou foi removida
        for I := 2 to PropCount-1 do
          if NameTarget = OldName[I] then begin
            NameTarget := Properties[I].Name;
            if IsDerived[I] then
              NameTarget := ''
            else
              Result := GetPropInfo(Self, NameTarget);
            exit
          end;
          NameTarget := ''; // Propriedade removida
      end
      else begin
        Prop := PropByName(NameTarget);
        if (Prop > 0) and IsDerived[Prop] then begin
          Result := nil;
          NameTarget := '';
        end;
      end;
  end
  else
    Result := nil;
end;

procedure TPrevalent.EvolveClass(var NameTarget : string; Source : integer; var Evolve : TEvolveRecord; Index : integer);
var
  TargetProp : TObject;
begin
  with Evolve.Props[Index] do begin
    if PropInfo = nil then PropInfo := GetEvolveProperty(NameTarget);
    if PropInfo <> nil then begin
      if PropInfo.PropType^{$IFNDEF FPC}^{$ENDIF}.Kind <> tkClass then exit;
      TargetProp := GetObjectProp(Self, PropInfo);
      if TargetProp = nil then
        SetOrdProp(Self, PropInfo, Source);// Reference/Association --> Reference
    end;
  end;
end;

function TPrevalent.EvolveAssociation(var NameTarget : string; var Evolve : TEvolveRecord; Index : integer) : TObject; begin
  Result := nil;
  with Evolve.Props[Index] do begin
    if PropInfo = nil then PropInfo := GetEvolveProperty(NameTarget);
    if PropInfo <> nil then begin
      if PropInfo.PropType^{$IFNDEF FPC}^{$ENDIF}.Kind <> tkClass then exit;
      Result := GetObjectProp(Self, PropInfo);
    end;
  end;
end;

procedure TPrevalent.MoveCorresponding(Target : TPrevalent);
var
  I : integer;
  TargetPropInfo : PPropInfo;
  TargetTypeInfo : PTypeInfo;
begin
  with Prevalence.GetStream do try
    InMoveCorresponding := true;
    DirectAccess := true;
    DontUpdateLog:= true;
    Target.FID := FID;
    Target.FVersion := FVersion;
    TargetTypeInfo := PTypeInfo(Target.ClassType.ClassInfo);
    with Metadata do begin
      for I := 2 to PropCount-1 do
        if not IsDerived[I] then
          try
            TargetPropInfo := GetPropInfo(TargetTypeInfo, Properties[I].Name);
            if TargetPropInfo <> nil then begin
              if (ExtraRTTI[0].HasPropString = _hpNONE) and (TypeProp[I] in [tkString, tkLString]) then ExtraRTTI[0].HasPropString := _hpHAVE;
              case TypeProp[I] of
                tkClass : SetObjectProp(Target, TargetPropInfo, GetObjectProp(Self, Properties[I]));
                tkChar, tkWChar, tkSet{$IFDEF FPC}, tkBool{$ENDIF} : SetOrdProp(Target, TargetPropInfo, GetPropInfoValue(Self, Properties[I]));
              else
                SetPropInfoValue(Target, TargetPropInfo, GetPropInfoValue(Self, Properties[I])); // Normal attribute
              end;
            end;
          except
            on E : Exception do
              raise EExtP.CreateFmt('Internal Error in MoveCorresponding, source: %s.%s, target: %s.%s; %s: %s',
                [GetClassAlias, AliasProp[I], Target.GetClassAlias, AliasProp[I], E.ClassName, E.Message])
          end;
      if ExtraRTTI[0].HasPropString = _hpNONE then ExtraRTTI[0].HasPropString := _hpHAVENT;
    end;
  finally
    DirectAccess := false;
    DontUpdateLog:= false;
    InMoveCorresponding := false;
  end;
end;

procedure TPrevalent.MoveSameClass(Target : TPrevalent);
var
  I : integer;
begin
  with Prevalence.GetStream do try
    InMoveCorresponding := true;
    DirectAccess := true;
    DontUpdateLog:= true;
    Target.FID := FID;
    Target.FVersion := FVersion;
    with Metadata do begin
      for I := 2 to PropCount-1 do
        if not IsDerived[I] then
          try
            if (ExtraRTTI[0].HasPropString = _hpNONE) and (TypeProp[I] in [tkString, tkLString]) then ExtraRTTI[0].HasPropString := _hpHAVE;
            case TypeProp[I] of
              tkClass : SetObjectProp(Target, Properties[I], GetObjectProp(Self, Properties[I]));
              tkChar, tkWChar, tkSet{$IFDEF FPC}, tkBool{$ENDIF} : SetOrdProp(Target, Properties[I], GetPropInfoValue(Self, Properties[I]));
            else
              SetPropInfoValue(Target, Properties[I], GetPropInfoValue(Self, Properties[I])); // Normal attribute
            end;
          except
            on E : Exception do
              raise EExtP.CreateFmt('Internal Error in MoveCorresponding, source: %s.%s, target: %s.%s; %s: %s',
                [GetClassAlias, AliasProp[I], Target.GetClassAlias, AliasProp[I], E.ClassName, E.Message])
          end;
      if ExtraRTTI[0].HasPropString = _hpNONE then ExtraRTTI[0].HasPropString := _hpHAVENT;
    end;
  finally
    DirectAccess := false;
    DontUpdateLog:= false;
    InMoveCorresponding := false;
  end;
end;

procedure TPrevalent.DuplicateObject(Target : TPrevalent); begin
  if not (Metadata.ExtraRTTI[0].HasPropString in [_hpNONE, _hpHAVE]) then
    move(pointer(Self)^, pointer(Target)^, Target.InstanceSize)
  else
  if ClassType <> Target.ClassType then
    MoveCorresponding(Target)
  else
    MoveSameClass(Target);
end;

function TPrevalent.CopyObjectCascade(Excepts : array of string) : TPrevalent;
var
  ListOriginal,
  ListCopia : TList;
  function IsInExcepts(pObj : TPrevalent; pPropName : string = '') : boolean;
  var
    I : integer;
  begin
    if (pObj.ClassType = Self.ClassType) and (pObj <> Self) then begin
      Result := true;
      exit;
    end;
    Result := false;
    for I:= 0 to high(Excepts)-1 do
      if pPropName = '' then begin
        if pos('.', Excepts[I]) = 0 then begin
          if pObj.ClassName = 'T' + copy(Excepts[I], 0, pos('.', Excepts[I])-1) then begin
            Result := true;
            exit;
          end;
        end
      end
      else //a exceção é de uma propriedade
        if (pObj.ClassName = 'T' + copy(Excepts[I], 0, pos('.', Excepts[I])-1)) and (pPropName = copy(Excepts[I], pos('.', Excepts[I])+1, MAXINT)) then begin
          Result := true;
          exit;
        end;
  end;

  function InternalCopyCascade(pPrevalent : TPrevalent) : TPrevalent;
  var
    I, Pos : integer;
    fPropInfo : PPropInfo;
    fAssociation,
    fNewAssociation : TAssociation;
    fTransient : TTransient;
    fPrevalent  : TPrevalent;
  begin
    Pos := ListOriginal.IndexOf(pPrevalent);
    if Pos <> -1 then begin
      Result := ListCopia[Pos];
      exit;
    end;
    Pos := ListCopia.IndexOf(pPrevalent);
    if Pos <> -1 then begin
      Result := ListCopia[Pos];
      exit;
    end;
    if IsInExcepts(pPrevalent) then begin
      Result := pPrevalent;
      ListOriginal.Add(pPrevalent);
      ListCopia.Add(Result);
      exit;
    end
    else
      Result := TPrevalentClass(pPrevalent.ClassType).Create;
    ListOriginal.Add(pPrevalent);
    ListCopia.Add(Result);
    with pPrevalent.Metadata do begin
      for I := 2 to PropCount-1 do
        if not IsDerived[I] then begin
          fPropInfo := GetPropInfo(pPrevalent, Properties[I].Name);
          if TypeProp[I] <> tkClass then begin//propriedade normal
            if not IsInExcepts(pPrevalent, Properties[I].Name) then
              SetPropInfoValue(Result, fPropInfo, GetPropInfoValue(pPrevalent, Properties[I]))
          end
          else begin
            if not IsAssociation[I] then begin//referencia
              fPrevalent := TPrevalent(GetObjectProp(pPrevalent, Properties[I]));
              if fPrevalent <> nil then
                if IsInExcepts(pPrevalent, Properties[I].Name) then
                  SetObjectProp(Result, fPropInfo, fPrevalent)
                else
                  SetObjectProp(Result, fPropInfo, InternalCopyCascade(fPrevalent));
            end
            else begin
              fAssociation := TAssociation(GetObjectProp(pPrevalent, Properties[I]));
              fNewAssociation := TAssociation(GetObjectProp(Result, Properties[I]));
              fTransient := fAssociation.First;
              while Assigned(fTransient) do begin
                if IsInExcepts(pPrevalent, Properties[I].Name) then
                  fNewAssociation.Add(TPrevalent(fTransient))
                else
                  fNewAssociation.Add(InternalCopyCascade(TPrevalent(fTransient)));
                fAssociation.Next(fTransient);
              end;
            end;
          end;
        end;
    end;
    Result.Add;
  end;
begin
  try
    ListOriginal := TList.Create;
    ListCopia := TList.Create;
    Result := InternalCopyCascade(Self);
  finally
    ListOriginal.Free;
    ListCopia.Free;
  end;
end;

function TPrevalent.CopyObjectCascade : TPrevalent; begin
  Result := CopyObjectCascade([]);
end;

function TPrevalent.Equal(Target : TPrevalent) : boolean;
var
  I : integer;
begin
  Result := false;
  if ClassType <> Target.ClassType then exit;
  with Metadata do
    for I := 2 to PropCount-1 do
      if (TypeProp[I] <> tkClass) and (GetPropInfoValue(Self, Properties[I]) <> GetPropInfoValue(Target, Properties[I])) then exit;
  Result := true;
end;

function TPrevalent.CheckPropConstraint(Transient : TTransient; Prop: integer; OldImage : TTransient = nil; CompleteCheck : boolean = true): string; begin
  if CheckConstraints then
    with Metadata do
      Result := CheckConstraint(Self, Prop, OldImage, CompleteCheck);
end;

procedure TPrevalent.DoCheckConstraints(CompleteCheck : boolean = true; OldImage : TPrevalent = nil);
var
  Prop, I : integer;
  S, Errors : string;
  List, PriList : TPrevalentList;
begin
  Errors  := '';
  PriList := Prevalence.PrevalentLists(FListIndex);
  List    := PriList.NextManaged;
  while (List <> nil) and (List <> PriList) do begin
    with List do
      if InFilter(Self) then begin
        S := GetKey(Self);
        if S <> '' then
          if not IsDuplicates then // Testa Unique
            if OldImage <> nil then begin
              if (S <> GetKey(OldImage)) then begin
                Sort;
                I := TObjectList(List).IndexOf(Self); //%% Pular os não commitados para nível 2 e 3
                if ((I < Count-1) and (GetKey(List.InternalObjects[I+1]) = S)) or
                   ((I > 0) and (GetKey(List.InternalObjects[I-1]) = S)) then
                  Errors := format('Modificação para chave já existente não e permitida. Classe: %s, Chave: %s = "%s".', [GetClassAlias, MethodName(KeyCode), S]) + #13
              end
            end
      end;
    List := List.NextManaged;
  end;
  with Metadata do
    for Prop := 2 to PropCount-1 do
      Errors := Errors + CheckPropConstraint(Self, Prop, OldImage, CompleteCheck);
  if trim(Errors) <> '' then
    raise Exception.Create(copy(Errors, 1, length(Errors)-2));
end;

procedure TPrevalent.SetAssociationToNil;
var
  I : integer;
begin
  with Metadata do
    for I := 2 to PropCount-1 do
      if (TypeProp[I] = tkClass) and IsAssociation[I] then
        SetObjectProp(Self, Properties[I], nil);
end;

procedure TPrevalent.SetTargetToNil(PropObject : TPrevalent; Target : integer; CascadeObject : TPrevalent = nil; InReflection : boolean = false);

  procedure CheckKeyLinks(pObject : TPrevalent);
  var
    I, J, SaveFThreadID : integer;
    IndexList : PtrInt;
    Props : TProperties;
  begin
    if InReflection then exit;
    Props := TPrevalent(pObject).Metadata;
    //-> verifica se o pObject pode ser excluido da classe
    for J := 2 to Props.PropCount - 1 do
      if (Props.ExtraRTTI[J] <> nil) and (Props.ExtraRTTI[J].DependencyLists <> nil) then
        for I := 0 to Props.ExtraRTTI[J].DependencyLists.Count -1 do begin
          IndexList := PtrInt(Props.ExtraRTTI[J].DependencyLists[I]);
          with TPrevalentList(Prevalence.FPrevalentLists.Objects[IndexList]) do begin
            if ObjectClass.ClassName <> pObject.ClassName then begin
              SaveFThreadID := pObject.FThreadID;
              try
                pObject.FThreadID := -GetFCGIThreadID;
                ListInCheck := IndexList;
                Sort;
                pObject.FThreadID := SaveFThreadID;
                ListInCheck := NONELIST;
              except
                pObject.FThreadID := SaveFThreadID;
                ListInCheck := NONELIST;
                raise EExtP.CreateFmt('Remoção não permitida. %s: %s é referenciada pela chave %s.', [Self.GetClassAlias, GetFirstProperty, GetClassAlias]);
              end;
            end;
          end;
        end;
  end;

var
  TargetObject : TObject;
begin
  with TPrevalent(PropObject).Metadata do begin
    TargetObject := GetObjectProp(PropObject, Properties[Target]);
    if (TargetObject = nil) or (CascadeObject = PropObject) then exit;
    if TargetObject is TPrevalent then
      with Prevalence.GetStream do begin
        CheckKeyLinks(TPrevalent(TargetObject));
        if PropObject.ThreadID >= 0 then begin
          DontUpdateLog := true;
          SetOrdProp(PropObject, Properties[Target], 0);
          DontUpdateLog := false;
        end;
      end
    else
      TAssociation(TargetObject).Delete(Self, CascadeObject, InReflection);
  end;
end;

procedure TPrevalent.HandleReflectionsForDelete(CascadeObject : TPrevalent = nil; InReflection : boolean = false);
var
  I, K, L : integer;
  PropObject  : TObject;
  TargetField : string;
  AssocObj  : TTransient;
  TargetObj : TPrevalent;
  TargetClass : TClass;
begin
  with Metadata do begin
    for I := PropCount-1 downto 2 do
      if TypeProp[I] = tkClass then
        if TargetProp[I] <> 0 then begin
          PropObject := GetObjectProp(Self, Properties[I]);
          if PropObject <> nil then
            if PropObject is TPrevalent then begin
              if InConstraints(I, COMPOSITE) then TPrevalent(PropObject).DeleteInReflection;
              SetTargetToNil(TPrevalent(PropObject), TargetProp[I], CascadeObject, InReflection);
            end
            else
              with TAssociation(PropObject) do
               if not InConstraints(I, COMPOSITE) then begin
                 AssocObj := First;
                 while Assigned(AssocObj) do begin
                   SetTargetToNil(TPrevalent(AssocObj), TargetProp[I], CascadeObject);
                   Next(AssocObj);
                 end;
               end;
        end;
    for I := 0 to length(Unidirect)-1 do
      with TPrevalentList(Unidirect[I].TargetList) do begin
        TargetField := Unidirect[I].TargetField;
        TargetClass := ObjectClass;
        for K := Family.Count-1 downto 0 do
          with TPrevalentList(Family[K]) do
            if ObjectClass.InheritsFrom(TargetClass) then
              for L := Count-1 downto 0 do begin
                TargetObj := TPrevalent(InternalObjects[L]);
                if TargetObj <> nil then begin
                  PropObject := GetObjectProp(TargetObj, TargetField);
                  if PropObject <> nil then
                    if PropObject is TPrevalent then begin
                      if PropObject = Self then SetOrdProp(TargetObj, TargetField, 0)
                    end
                    else
                      with TAssociation(PropObject) do
                        if IndexOf(Self) <> -1 then Delete(Self, CascadeObject)
                end;
              end;
      end;
  end;
end;

procedure TPrevalent.HandleReflectionsForUpdate(NewImage, PropObject, PrevObject : TPrevalent; Prop : integer; Stream : TTransactionStream; Props : TProperties; InRollback : boolean = false);

  procedure ReflectTarget(PropObject, PrevObject : TPrevalent; Target : integer);
  var
    TargetObject : TObject;
  begin
    if Target = 0 then exit;
    if (PrevObject <> nil) and (PropObject <> PrevObject) and not Prevalence.InRecover then
      SetTargetToNil(PrevObject, Target, nil, true);
    if (PropObject <> nil) and (PropObject.ID > -1) then
      with TPrevalent(PropObject).Metadata do begin
        TargetObject := GetObjectProp(PropObject, Properties[Target]);
        if (TargetObject = nil) or (TargetObject is TPrevalent) then
          SetObjectProp(PropObject, Properties[Target], Self)
        else
          with TAssociation(TargetObject) do begin
            if GetKey(Self) <> GetKey(NewImage) then Sort;
            Add(Self, true)
          end;
      end;
  end;

begin
  with Props do
    if (TypeProp[Prop] = tkClass) and ((PropObject = nil) or not IsAssociation[Prop]) then
      ReflectTarget(PropObject, PrevObject, TargetProp[Prop]);
end;

procedure TPrevalent.HandleReflectionsForInsert;

  procedure ReflectTarget(PropObject : TPrevalent; Target : integer);
  var
    TargetObject : TObject;
  begin
    if (Target = 0) or (PropObject.FID = 0) then exit;
    with ThreadTrans.Stream do
      if PropObject <> nil then begin
        with TPrevalent(PropObject).Metadata do begin
          TargetObject := GetObjectProp(PropObject, Properties[Target]);
          if (TargetObject = nil) or (TargetObject is TPrevalent) then
            SetObjectProp(PropObject, Properties[Target], Self)
          else
            with TAssociation(TargetObject) do begin
              Sort;
              if not IsInAssociation(Self) then Add(Self, true)
            end
        end;
        DontUpdateLog:=False;
      end;
  end;

var
  I, J, Prop  : integer;
  PropObject  : TPrevalent;
  TargetField : string;
begin
  with Metadata, ThreadTrans.Stream do
    for Prop := PropCount-1 downto 2 do
      if TypeProp[Prop] = tkClass then begin
        DirectAccess := True;
        PropObject := TPrevalent(GetObjectProp(Self, Properties[Prop]));
        DirectAccess := false;
        if (PropObject <> nil) or (PropObject is TPrevalent) then
          ReflectTarget(PropObject, TargetProp[Prop]);
        for I := 0 to length(Unidirect)-1 do
          with TPrevalentList(Unidirect[I].TargetList) do begin
            TargetField := Unidirect[I].TargetField;
            for J := Count-1 downto 0 do begin
              PropObject := TPrevalent(GetObjectProp(InternalObjects[J], TargetField));
              if (PropObject = nil) or (PropObject is TPrevalent) then
                break
              else
                with TAssociation(PropObject) do
                  if not IsPrimary or IsTransient then
                    Sort
                  else
                    break;
            end;
          end;
      end;
end;

function TPrevalent.FixPropObject(Obj : TObject; Prop : integer) : TObject; begin
  Result := Obj;
  if Obj = nil then exit;
  try
    if Obj is TPrevalent then;
  except
    Result := nil;
    if ThreadTrans <> nil then SetObjectProp(Self, Metadata.Properties[Prop], nil);
  end;
end;

function TPrevalent.GetProperty(Prop: integer): variant;
var
  aObject : TObject;
begin
  with Metadata do
    if TypeProp[Prop] <> tkClass then begin
      try
        Result := GetPropInfoValue(Self, Properties[Prop]);
      except
        Result := 0
      end;
      if (VarType(Result) and varTypeMask) = varBoolean then
        if Result then
          Result := 1
        else
          Result := 0;
      if TypeProp[Prop] = tkChar then Result := char(integer(Result))
    end
    else begin
      aObject := FixPropObject(GetObjectProp(Self, Properties[Prop]), Prop);
      if (aObject <> nil) and (aObject is TPrevalent) then
        Result := TPrevalent(aObject).ID
      else
        Result := 0
    end;
end;

function TPrevalent.GetPermissionView : T_ViewPermissionSet; begin
  Result := T_ViewPermissionSet(GetViewPerm(Self))
end;

function TPrevalent.GetIdentification: string; begin
  Result := GetFirstProperty;
end;

function TPrevalent.GetClassAlias: string; begin
  try Result := Metadata.AliasProp[0]; except end;
  if Result = '' then Result := ClassName;
end;

procedure TPrevalent.SetDependencyLists(Prop : word); begin
  if (Self = nil) or Prevalence.IsInRecoverSnapShot or (ListInCheck = NONELIST) then exit;
  Metadata.AddDependencyLists(Prop, ListInCheck);
end;

function TPrevalent.GetFirstProperty : string;
var
  Prop: integer;
  DT: TDateTime;
  V: variant;
  DateTimeType: TDateTimeType;

  function GetSetDisplayValue(const V: LongWord): string;
  var
    AndV: LongWord;
    I: integer;
    PropType: {$IFNDEF FPC}PPTypeInfo{$ELSE}PTypeInfo{$ENDIF};
  begin
    AndV   := 1;
    Result := '';
    PropType := Metadata.Properties[Prop].PropType;
    for I := 0 to GetTypeData(GetTypeData(PropType{$IFNDEF FPC}^{$ENDIF}).CompType{$IFNDEF FPC}^{$ENDIF}).MaxValue do begin
      if V and AndV <> 0 then
        AddToStr(Result, AdjustEnumerationOrSet(GetEnumName(GetTypeData(PropType{$IFNDEF FPC}^{$ENDIF}).CompType{$IFNDEF FPC}^{$ENDIF}, I)), ';');
      AndV := AndV shl 1;
    end;
  end;

begin
  with Metadata do
    for Prop := 2 to PropCount - 1 do
      if TypeProp[Prop] <> tkClass then begin
        V := GetProperty(Prop);
        case TypeProp[Prop] of
          tkEnumeration{$IFDEF FPC}, tkBool{$ENDIF}: Result := AdjustEnumerationOrSet(GetEnumName(Properties[Prop].PropType{$IFNDEF FPC}^{$ENDIF}, V));
          tkFloat       : begin
            DateTimeType := GetDateTimeType(TypeNameProp[Prop]);
            if DateTimeType = dtNone then
              Result := V
            else begin
              DT := V;
              if DT <> 0 then
                case DateTimeType of
                  dtDate    : Result := DateToStr(DT);
                  dtTime    : Result := TimeToStr(DT);
                  dtDateTime: Result := DateTimeToStr(DT);
                end
              else
                Result := '';
            end;
          end;
          tkSet : Result := GetSetDisplayValue(V);
          else
            Result := V;
        end;
        exit;
      end
      else
        break;
  Result := '';
end;

procedure TPrevalent.MergeDependecyLists(var List : TList; const Prop : integer);
var
  I, L : PtrInt;
begin
  with Metadata do begin
    if (ExtraRTTI[Prop] <> nil) and (ExtraRTTI[Prop].DependencyLists <> nil) then
      for I := 0 to ExtraRTTI[Prop].DependencyLists.Count -1 do begin
        L := PtrInt(ExtraRTTI[Prop].DependencyLists[I]);
        if List.IndexOf(pointer(L)) = -1 then
          List.Add(pointer(L));
      end;
  end;
end;

procedure TPrevalent.MergeDependecyLists(var List : TList);
var
  Prop : integer;
begin
  with Metadata do
    for Prop := 2 to PropCount-1 do
      MergeDependecyLists(List, Prop);
end;

function ExtractMask(V : variant) : variant;
var
  I    : integer;
  I64  : int64;
  S, R : string;
begin
  I64 := StrToInt64Def(V, -MAXINT);
  if I64 = -MAXINT then begin
    S := V; R := '';
    if (S <> '') and (S[1] in ['0'..'9', '-', '+']) then
      R := S[1];
    for I := 2 to length(S) do
      if S[I] in ['0'..'9'] then
        R := R + S[I];
    Result := R;
  end
  else
    Result := V;
end;

procedure TPrevalent.SetProperty(Prop: integer; Value: variant);
var
  Prevalent : TPrevalent;
  PrevalentList : TPrevalentList;
begin
  with Metadata do
    if not IsDerived[Prop] then
      if TypeProp[Prop] <> tkClass then begin
        case TypeProp[Prop] of
          tkInteger, tkInt64 : Value := ExtractMask(Value);
          tkSet : Value := Integer(Value);
        end;
        SetPropInfoValue(Self, Properties[Prop], Value)
      end
      else begin
        PrevalentList := Prevalence.PrevalentLists(TypeNameProp[Prop] + 'List');
        if PrevalentList = nil then
          PrevalentList := Prevalence.AbstractLists(TypeNameProp[Prop] + 'List');
        if Assigned(PrevalentList.Family) then
          Prevalent := TPrevalent(PrevalentList.FindInFamily(Value))
        else
          Prevalent := TPrevalent(PrevalentList.Find(Value));
        SetObjectProp(Self, Properties[Prop], Prevalent);
      end;
end;

function TPrevalent.Metadata: TProperties; begin
  try
    Result := TProperties(Prevalence.Prevalents.Objects[TPrevalentList(Prevalence.FPrevalentLists.Objects[FListIndex]).PrevalentsIndex]);
  except
    raise Exception.CreateFmt('A classe %s é abstrata e por isso não pode ser instanciada.', [GetClassAlias]);
  end;
end;

{ TPrevalentList }

constructor TPrevalentList.Create(pListProps : TListProps = [lpPrimary]; Filter : string = ''; AutoIncField : string = '');

  procedure SetFamily;
  var
    Parent : TClass;
    I : integer;
  begin
    Parent := ObjectClass.ClassParent;
    if Parent = TPrevalent then
      Family := TList.Create
    else begin
      while Parent.ClassParent <> TPrevalent do
        Parent := Parent.ClassParent;
      I := Prevalence.FPrevalentLists.IndexOf(Parent.ClassName + 'List');
      if I <> -1 then
        Family := Prevalence.PrevalentLists(I).Family
      else begin
        I := Prevalence.FAbstractLists.IndexOf(Parent.ClassName + 'List');
        if I <> -1 then
          Family := Prevalence.AbstractLists(Parent.ClassName + 'List').Family;
      end;
      if Family = nil then begin
        I := Prevalence.FamilyList.IndexOf(Parent.ClassName + 'List');
        if I <> -1 then
          Family := TList(Prevalence.FamilyList.Objects[I])
        else begin
          Family := TList.Create;
          Prevalence.FamilyList.AddObject(Parent.ClassName + 'List', Family);
        end;
      end;
      Parent := ObjectClass.ClassParent;
      while Parent.ClassParent <> TPrevalent do begin
        if (Prevalence.FPrevalentLists.IndexOf(Parent.ClassName + 'List') = -1) and
           (Prevalence.FamilyList.IndexOf(Parent.ClassName + 'List') = -1) then
          Prevalence.FamilyList.AddObject(Parent.ClassName + 'List', Family);
        Parent := Parent.ClassParent;
      end;
    end;
    Family.Add(Self);
  end;

var
  PrimaryList : TPrevalentList;
begin
  inherited Create(pListProps);
  if IsPrimary then begin
    FAutoIncInfo := GetPropInfo(ObjectClass, AutoIncField);
    if (FAutoIncInfo <> nil) and (FAutoIncInfo.PropType^{$IFNDEF FPC}^{$ENDIF}.Kind <> tkInteger) then
      raise EExtP.CreateFmt('Internal Error: Autoincrement field: "%s" in Class: "%s" must be a 32 or 16 bit integer type', [AutoIncField, ObjectClass.ClassName]);
  end;
  if Filter <> '' then
    FilterCode := ObjectClass.MethodAddress(Filter);
  IsManaged := Prevalence.ManagedLists and not IsAbstract;
  DependencyListAssoc := TList.Create;
  with Prevalence do begin
    ListIndex := word(FPrevalentLists.IndexOf(Self.ClassName));
    if IsManaged then
      if ListIndex = NONELIST then
        if IsPrimary then begin
          ListIndex := FPrevalentLists.AddObject(Self.ClassName, Self);
          if Prevalents.IndexOf(ObjectClass.ClassName) = -1 then
            Prevalents.AddObject(ObjectClass.ClassName, TProperties.Create(ObjectClass, Self.ListIndex));
          SetFamily;
        end
        else begin
          ListIndex := Prevalents.IndexOf(ObjectClass.ClassName);
          if ListIndex = NONELIST then
            raise EExtP.CreateFmt('Internal Error: Secondary list: "%s" must be created after the Primary list for this class: "%s"', [Self.ClassName, ObjectClass.ClassName])
          else begin // Cria lista encadeada de listas gerenciadas automaticamente
            FPrevalentLists.AddObject(Self.ClassName, Self);
            ListIndex   := Metadata(ObjectClass.ClassName).ListIndex;
            PrimaryList := PrevalentLists(ListIndex);
            Family      := PrimaryList.Family;
            if PrimaryList.NextManaged = nil then
              Self.NextManaged := PrimaryList
            else
              Self.NextManaged := PrimaryList.NextManaged;
            PrimaryList.NextManaged := Self;
          end;
        end
      else
        raise EExtP.CreateFmt('Internal Error: Duplicate creation of list "%s" for the same class "%s"', [Self.ClassName, ObjectClass.ClassName])
    else begin
      if IsAbstract then begin
        SetFamily;
        Prevalence.AddAbstractClass(ObjectClass, Self);
      end;
      NextManaged := nil;
    end;
  end;
end;

destructor TPrevalentList.Destroy; begin
  Clear;
  inherited;
end;

procedure TPrevalentList.Add(Prevalent: TPrevalent; CompleteCheck : boolean = true);

  procedure VerifySort(List : TPrevalentList; Index : integer);
  var
    Key : string;
  begin
    if not Prevalence.InRecoverSnapShot then
      with List do begin
        Key := GetKey(Prevalent);
        if Key = '' then
          Sort
        else
          if not Prevalence.InRecover then
            if (Index > 1) and (Index < FCount) and (InternalObjects[Index] = Prevalent) then
              if CompareText(GetKey(InternalObjects[Index-1]), Key) > 0 then
                Sort
              else
                if (Index < FCount-1) and (CompareText(GetKey(InternalObjects[Index+1]), Key) < 0) then Sort;
      end
  end;

var
  PriList, List : TPrevalentList;
  NextAutoInc, I : integer;
begin
  if Prevalent = nil then raise Exception.Create('Add não permitido. Objeto é nulo.');
  if ThreadTrans = nil then
    raise EExtP.Create('Erro: Tentativa de Add sem o correspondente BeginTransaction.');
  if not Prevalence.InRecover and (Prevalent.FID <> 0) and not IsTransient then exit;
  if IsManaged then
    PriList := Prevalence.PrevalentLists(ListIndex)
  else
    PriList := Self;
  if not Prevalence.InRecover and IsManaged then begin
    NextAutoInc := 0;
    LockList.Enter;
    try
      for I := 0 to Family.Count-1 do with TPrevalentList(Family[I]) do
        if Count > 0 then
          NextAutoInc := Math.Max(TPrevalent(InternalObjects[Count-1]).ID, NextAutoInc);
      Prevalent.ID := NextAutoInc + 1;
      if PriList.FAutoIncInfo <> nil then begin
        NextAutoInc := 0;
        if Count > 0 then
          NextAutoInc := GetOrdProp(InternalObjects[Count-1], PriList.FAutoIncInfo);
        SetOrdProp(Prevalent, PriList.FAutoIncInfo, NextAutoInc + 1);
      end;
    finally
      LockList.Leave;
    end;
    Prevalent.FThreadID := GetFCGIThreadID;
    ThreadTrans.AddInsDels(Prevalent);
  end;
  List := PriList;
  repeat // Inclui nas Listas Gerenciadas
    with List do
      if InFilter(Prevalent) then begin
        if IsPrimary then
          TObjectList(List).Add(Prevalent)
        else
          VerifySort(List, TObjectList(List).Add(Prevalent));
        if IsPrimary and IsManaged then
          Prevalence.Log(_opAdd, Prevalent);
      end;
    List := List.NextManaged;
  until (List = nil) or (List = PriList);
  if not Prevalence.InRecover and IsManaged and not InUpdate then
    Prevalent.HandleReflectionsForInsert;
end;

procedure TPrevalentList.CheckCascade(Prevalent : TPrevalent; Origin : integer = -1); // Wander

  procedure RaiseError(Papel : string; Target : TPrevalent = nil); begin
    if Target = nil then
      raise EExtP.CreateFmt('Não é possível remover %s: %s, pois possui um(a) %s, que é uma agregação.',
        [Prevalent.GetClassAlias, Prevalent.GetFirstProperty, Papel])
    else
      raise EExtP.CreateFmt('Não é possível remover %s: %s, pois %s: %s precisa de %s',
        [Prevalent.GetClassAlias, Prevalent.GetFirstProperty, Target.GetClassAlias, Target.GetFirstProperty, Papel])
  end;

  procedure CheckUnidirect;
  var
    I : integer;
    TargetObj : TPrevalent;
  begin
    with Prevalent.Metadata do
      for I := 0 to length(Unidirect)-1 do
        with Unidirect[I] do begin
          if UnidirectInConstraints(I, SHARED) or UnidirectInConstraints(I, NOTNULL) then begin
            TargetObj := TPrevalentList(TargetList).ScanInFamily(TargetField, Prevalent, UnidirectInConstraints(I, ASSOCIATIONCONSTRAINT));
            if (TargetObj <> nil) and TargetObj.InSession then RaiseError(TargetField, TargetObj);
          end;
        end;
  end;

  procedure CheckTarget(TargetPrevalent : TPrevalent; Target : integer);
  var
    TargetObj : TPrevalent;
  begin
    if TargetPrevalent.InSession then
      with TPrevalent(TargetPrevalent).Metadata do
        if InConstraints(Target, NOTNULL) then
          if not IsAssociation[Target] then begin
            if Prevalent = GetObjectProp(TargetPrevalent, Properties[Target]) then RaiseError(AliasProp[Target], TargetPrevalent);
          end
          else begin
            TargetObj := TPrevalent(GetObjectProp(TargetPrevalent, Properties[Target]));
            if TargetObj <> nil then
              with TAssociation(TargetObj) do
                if (Count = 1) and (InternalObjects[0] = Prevalent) then RaiseError(AliasProp[Target], TargetPrevalent);
          end;
  end;

var
  I, J : integer;
  Child : TPrevalent;
  PropObject : TObject;
begin
  if Prevalent.InSession then begin
    CheckUnidirect;
    with Prevalent.Metadata do
      for I := 2 to PropCount-1 do
        if TypeProp[I] = tkClass then
          if Origin <> TargetProp[I] then begin
            PropObject := GetObjectProp(Prevalent, Properties[I]);
            if PropObject <> nil then begin
              if InConstraints(I, SHARED) then
                if IsAssociation[I] then begin
                  if TAssociation(PropObject).Count > 0 then RaiseError(AliasProp[I]);
                end
                else
                  RaiseError(AliasProp[I]);
              if InConstraints(I, COMPOSITE) then
                if IsAssociation[I] then
                  with TAssociation(PropObject) do
                    for J := 0 to Count-1 do begin
                      Child := TPrevalent(InternalObjects[J]);
                      Prevalence.PrevalentLists(Child.FListIndex).CheckCascade(Child, I);
                    end
                else
                  Prevalence.PrevalentLists(TPrevalent(PropObject).FListIndex).CheckCascade(TPrevalent(PropObject), I)
              else
                if PropObject is TPrevalent then
                  CheckTarget(TPrevalent(PropObject), TargetProp[I])
                else
                  with TAssociation(PropObject) do
                    for J := 0 to Count-1 do
                      CheckTarget(TPrevalent(InternalObjects[J]), TargetProp[I])
            end;
          end;
  end;
end;

procedure TPrevalentList.DeleteCascade(Prevalent : TPrevalent);
var
  I, J  : integer;
  Child : TPrevalent;
begin
  with Prevalent.Metadata do
    for I := PropCount-1 downto 2 do
      if TypeProp[I] = tkClass then
        if InConstraints(I, COMPOSITE) then
          if IsAssociation[I] then
            with TAssociation(GetObjectProp(Prevalent, Properties[I])) do
              for J := Count-1 downto 0 do begin
                Child := TPrevalent(InternalObjects[J]);
                if Child.InSession and InSession(Child) then // Wander
                  Prevalence.PrevalentLists(Child.FListIndex).Delete(Child, Prevalent);
              end
          else begin
            Child := TPrevalent(GetObjectProp(Prevalent, Properties[I]));
            if Child <> nil then
              Prevalence.PrevalentLists(Child.FListIndex).Delete(Child, Prevalent);
          end;
end;

procedure TPrevalentList.Delete(Prevalent : TPrevalent; CascadeObject : TPrevalent = nil; InReflection : boolean = false);
var
  List : TPrevalentList;
  I : integer;
begin
   if ThreadTrans = nil then raise EExtP.Create('Erro: Tentativa de Delete sem o correspondente BeginTransaction.');
  if IsManaged then
    List := Prevalence.PrevalentLists(ListIndex)
  else
    List := Self;
  with List do
    if InFilter(Prevalent) then begin
      I := IndexOf(Prevalent);
      if I >= 0 then
        if IsManaged then begin
          if IsPrimary then begin
            if not (Prevalence.InRecover or (CascadeObject <> nil)) then CheckCascade(Prevalent);
            if CascadeObject = nil then CascadeObject := Prevalent;
            ThreadTrans.Stream.DontUpdateLog:= true;
            Prevalent.HandleReflectionsForDelete(CascadeObject, InReflection);
            DeleteCascade(Prevalent);
            ThreadTrans.Stream.DontUpdateLog:= false;
          end;
          if Prevalence.InRecover and (IndexOf(Prevalent) >= 0) then
            Prevalent.EfectiveDelete
          else begin
            Prevalent.FThreadID := -GetFCGIThreadID;
            ThreadTrans.AddInsDels(Prevalent);
          end;
          if IsPrimary and (CascadeObject = Prevalent) then
            Prevalence.Log(_opDelete, Prevalent);
        end
        else
          Prevalent.EfectiveDelete
    end;
end;

function TPrevalentList.FindInFamily(ID : integer) : TPrevalent;
var
  I : integer;
begin
  Result := nil;
  for I := Family.Count-1 downto 0 do
    with TPrevalentList(Family[I]) do
      if (Count <> 0) and ObjectClass.InheritsFrom(Self.ObjectClass) then begin
        Result := TPrevalent(Find(ID));
        if Result <> nil then exit;
      end;
end;

function TPrevalentList.FindKeyInFamily(Value : variant) : TPrevalent;
var
  I : integer;
  List  : TPrevalentList;
  Sufix : string;
begin
  Result := nil;
  Sufix  := copy(ClassName, length(ObjectClass.ClassName) + 1, 100);
  for I := Family.Count-1 downto 0 do begin
    List := Family[I];
    if (List.Count <> 0) and List.ObjectClass.InheritsFrom(ObjectClass) then begin
      List := Prevalence.PrevalentLists(LeftStr(List.ClassName, length(List.ClassName) - 4) + Sufix);
      if List <> nil then begin
        Result := TPrevalent(List.Find(Value));
        if Result <> nil then exit;
      end;
    end;
  end;
end;

function TPrevalentList.ScanInFamily(Attribute : string; Value : variant) : TPrevalent;
var
  I, J : integer;
begin
  Result := nil;
  for I := Family.Count-1 downto 0 do
    with TPrevalentList(Family[I]) do
      if ObjectClass.InheritsFrom(Self.ObjectClass) then
        try
          for J := 0 to Count-1 do
            if GetPropValue(InternalObjects[J], Attribute) = Value then begin
              Result := TPrevalent(InternalObjects[J]);
              exit;
            end;
        except end;
end;

function TPrevalentList.ScanInFamily(Attribute : string; Prevalent : TPrevalent; IsAssociation : boolean = false) : TPrevalent;
var
  I, J : integer;
  Assoc : TAssociation;
begin
  Result := nil;
  for I := Family.Count-1 downto 0 do
    with TPrevalentList(Family[I]) do
      if ObjectClass.InheritsFrom(Self.ObjectClass) then
        try
          if not IsAssociation then begin
            for J := 0 to Count-1 do
              if GetObjectProp(InternalObjects[J], Attribute) = Prevalent then begin
                Result := TPrevalent(InternalObjects[J]);
                exit;
              end;
          end
          else
            for J := 0 to Count-1 do begin
              Assoc := TAssociation(GetObjectProp(InternalObjects[J], Attribute));
              if (Assoc <> nil) and (Assoc is TAssociation) then begin
                Result := TPrevalent(Assoc.Find(Prevalent));
                if Result <> nil then exit;
              end;
            end;
        except end;
end;

function TPrevalentList.InFilter(Prevalent : TPrevalent) : boolean;
type
  TCallFilter = function : boolean of object;
var
  Filter : TMethod;
begin
  if FilterCode = nil then
    Result := true
  else begin
    Filter.Code := FilterCode;
    Filter.Data := Prevalent;
    Result      := TCallFilter(Filter)
  end
end;

procedure CallClassMethod(P : pointer);
type
  TCallClassMethod = procedure of object;
var
  ClassMethod : TMethod;
begin
  ClassMethod.Code := P;
  ClassMethod.Data := nil;
  TCallClassMethod(ClassMethod)
end;

procedure TPrevalentList.SetFastAdd(const Value: boolean);
const
  ListIndexAnt : integer = -1;
var
  I : integer;
  List : TPrevalentList;
begin
  if not Prevalence.InRecover then exit;
  if not Value then FastAdds := 0;
  if IsSorted <> not Value then begin
    if Prevalence.InRecover and not Value then
      ForceSorted
    else
      IsSorted := not Value;
    if IsPrimary then
      List := NextManaged
    else
      List := Self;
    if List = nil then exit;
    PendingFastAdd := true;
    while true do begin
      if not List.IsAbstract then
        try
          List.IsSorted := not Value;
        except
          if Prevalence.IsInRecover then begin
            if not Prevalence.IsInRecoverSnapShot then begin
              if List.ListIndex <> ListIndexAnt then begin
                ListIndexAnt := List.ListIndex;
                Trace('***** ERRO: NÃO FOI POSSÍVEL ORDENAR A LISTA <%s> da Classe <%s>.', [List.ClassName, TPrevalent(List.InternalObjects[0]).GetClassAlias]);
                Trace('  POSSÍVEL PROBLEMA EM CHAVE ESTRANGEIRA');
                Trace('  ACERTE OS OBJETOS INCONSISTENTES ANTES DE TORNAR OPERACIONAL O SISTEMA.');
                Trace('    Relação dos objetos inconsistentes:');
                ListInCheck := List.ObjectListIndex;
                try
                  for I := 0 to List.Count -1 do
                    try
                      List.GetKey(List.InternalObjects[I]);
                    except
                      Trace(' ID: %d Identificador: %s.',[List.InternalObjects[I].ID, List.InternalObjects[I].GetIdentification]);
                    end;
                finally
                  ListInCheck := NONELIST;
                end;
              end
              else
                ListIndexAnt := List.ListIndex;
            end
            else
              raise;
          end
          else
            raise EExtP.CreateFmt('***** ERRO: NÃO FOI POSSÍVEL ORDENAR A LISTA %s. POSSÍVEL PROBLEMA EM CHAVE ESTRANGEIRA. *****', [List.ClassName]);
        end;
      List.PendingFastAdd := false;
      List := List.NextManaged;
      if not IsPrimary then
        break
      else
        if (List = nil) or (List = Self) then break;
    end;
    PendingFastAdd := false;
  end;
end;

procedure TPrevalentList.VerifySortVersion; begin
  if Count < 2 then exit;
  if TObjectList(Self) is TUpdateList then exit;
  if not IsPrimary or IsTransient then
    with Prevalence.Metadata(ObjectClass.ClassName) do begin
      if SortVersion <> ExtraRTTI[0].SortRequest then begin
        TObjectList(Self).Sort;
        SortVersion := ExtraRTTI[0].SortRequest;
      end;
{$IFDEF CHECKSORT}
      if not CheckSort then
        Trace('Correção automática. Lista desordenada após o VerifySortVersion: ' + Self.ClassName +
          ' SortVersion:' + IntToStr(SortVersion) + ' SortRequest:' + IntToStr(ExtraRTTI[0].SortRequest));
{$ENDIF}
    end;
end;

procedure TPrevalentList.Sort; begin
  if not IsPrimary or IsTransient then inc(TPrevalent(InternalObjects[0]).Metadata.ExtraRTTI[0].SortRequest)
end;

function TPrevalentList.InView(pObject : TTransient) : boolean; begin
  if Prevalence.InRecover or (CallStateMachineCount > 0) then
    Result := true
  else
    Result := T_ViewPermissionSet(GetViewPerm(pObject)) <> [];
end;

function TPrevalentList.GetFastAdd: boolean; begin
  Result := not IsSorted
end;

procedure TPrevalentList.SetCapacity(NewCapacity: integer);
var
  List : TPrevalentList;
begin
  inherited;
  List := NextManaged;
  while (List <> nil) and (List <> Self) do begin
    if not List.IsAbstract then TObjectList(List).SetCapacity(NewCapacity);
    List := List.NextManaged
  end;
end;

procedure TPrevalentList.WriteMetadata(Stream : TMemoryStream);
var
  I,
  Index   : integer;
  Tam     : byte;
  Kind    : TTypeKind;
  SubKind,
  Props,
  Meths   : word;
  S       : string;
  fClas,
  fObj    : TObject;
  TargetList : TPrevalentList;
begin
  with Stream, Prevalence, Metadata(ObjectClass.ClassName) do begin
    S   := ObjectClass.ClassName;
    Tam := length(S);
    Write(Tam, sizeof(Tam));
    Write(S[1], Tam);
    fClas := Get_Classe(copy(S, 2, MAXINT));
    if fClas = nil then Index := -1 else Index := TPrevalent(fClas).ID;
    Write(Index, sizeof(Index));
    Props := PropCount;
    Write(Props, sizeof(Props));
    for I := 0 to PropCount-1 do begin
      Tam := length(Properties[I].Name);
      Write(Tam, sizeof(Tam));
      Write(Properties[I].Name[1], Tam);
      fObj := Get_Propriedade(fClas, Properties[I].Name);
      if fObj = nil then Index := -1 else Index := TPrevalent(fObj).ID;
      Write(Index, sizeof(Index));
      Kind := TypeProp[I];
      Write({$IFNDEF FPC}Kind{$ELSE}KindFPC2Delphi[Kind]{$ENDIF}, sizeof(Kind));
      case TypeProp[I] of
        tkInteger, tkSet, tkEnumeration{$IFDEF FPC}, tkBool{$ENDIF} : SubKind := word(OrdTypeProp[I]);
        tkFloat   : SubKind := word(FloatTypeProp[I]);
        tkClass   :
          if IsAssociation[I] then
            SubKind := NONELIST
          else begin
            TargetList := PrevalentLists(TypeNameProp[I] + 'List');
            if TargetList <> nil then
              SubKind := TargetList.ListIndex
            else
              SubKind := 0
          end;
        else
          SubKind := 0
      end;
      if IsDerived[I] then SubKind := NONELIST;
      Write(SubKind, sizeof(SubKind));
    end;
    S := Package;
    Tam := length(S);
    Write(Tam, sizeof(Tam));
    Write(S[1], Tam);
    Meths := MethodCount;
    Write(Meths, sizeof(Meths));
    for I := 0 to MethodCount - 1 do begin
      S := Method[I].Name;
      Tam := length(S);
      Write(Tam, sizeof(Tam));
      Write(S[1], Tam);
      fObj := Get_Metodo(fClas, Method[I].Name);
      if fObj = nil then Index := NONELIST else Index := TPrevalent(fObj).ID;
      Write(Index, sizeof(Index));
    end;
  end;
end;

{ TPrevalence }

constructor TPrevalence.Create(pFileName : string); begin
  inherited Create;
  FVersion        := GetVersion;
  FStartupDate    := Now;
  FileName        := pFileName;
  CriticalSection := SyncObjs.TCriticalSection.Create;
  ManagedLists    := true;
  CheckConstraints:= true;
  ListInCheck     := NONELIST;
  FPrevalentLists := TStringList.Create;
  with FPrevalentLists do begin
    Sorted     := false;
    Duplicates := DupError;
  end;
  FAbstractLists := TStringList.Create;
  with FAbstractLists do begin
    Sorted     := true;
    Duplicates := DupError;
  end;
  Prevalents := TStringList.Create;
  with Prevalents do begin
    Sorted     := true;
    Duplicates := DupError;
  end;
  FamilyList := TStringList.Create;
  with FamilyList do begin
    Sorted     := true;
    Duplicates := DupError;
  end;
end;

destructor TPrevalence.Destroy;
var
  I : Integer;
begin
  try for I := 0 to high(Finalizations) do CallClassMethod(Finalizations[I]); except end;
  CriticalSection.Free;
  LogFile.Free;
  if GetAuditory then AuditFile.Free;
  FPrevalentLists.Free;
  FAbstractLists.Free;
  Prevalents.Free;
  FamilyList.Free;
  inherited;
end;

procedure TPrevalence.BeginThread; begin
  ThreadTrans           := nil;
  CallStateMachineCount := 0;
  ListInCheck           := NONELIST;
  CheckConstraints      := true;
  TerminateSession      := false;
  InUpdate              := false;
  InSort                := false;
  InMoveCorresponding   := false;
  IsRunningStateMachine := false;
end;

procedure TPrevalence.EndThread; begin
  ClearTransaction;
end;

procedure TPrevalentList.SetLastRecovered(Index: integer); begin
  if (LastRecovered = nil) or (LastRecovered.FListIndex <> ListIndex) or (LastRecovered.FID <> Index) then
    LastRecovered := TPrevalent(FindInFamily(Index));
end;

procedure ReportStart; begin
  {$IFDEF SERVICE}if Service <> nil then Service.ReportStart{$ENDIF}
end;

procedure ReportStop; begin
  {$IFDEF SERVICE}if Service <> nil then Service.ReportStop{$ENDIF}
end;

procedure TPrevalence.RecoverLog;
var
  ListIndex,
  TargetList    : word;
  PropIndex     : byte;
  Index, T      : integer;
  Operation     : T_Operation;
  ActualList,
  PrevalentList : TPrevalentList;
  Association   : TAssociation;
  Resposta      : string;
begin
  if (Version <> SnapShotVersion) and (LogFile.Size > 4) then begin
    EExtP.CreateFmt('Recover do Log não permitido. Houve alteração de versão.', [], true);
    exit;
  end;
  T := 0;
  with LogFile do
    try
      ReadVersion;
      BeginTransaction;
      while Read(Operation, sizeof(Operation)) > 0 do begin
        inc(T);
        if (T mod 10000) = 0 then begin
          ReportStart;
          Trace('.....Transações aplicadas: %d', [T]);
        end;
        Read(ListIndex, sizeof(ListIndex));
        ActualList := PrevalentLists(ListIndex);
        if ActualList = nil then begin
          if NoService then begin
            Resposta := 'N';
            System.write('Log corrompido. Entrar no ar com log incompleto(S/N)? '); readln(Resposta);
            if upcase(Resposta[1]) <> 'N' then exit;
          end;
          raise EExtP.CreateFmt('Falha durante Recover do Log. Log corrompido.', [], true);
        end;
        with ActualList do
          case Operation of
            _opAdd : begin
              LastRecovered := ReadPrevalent(TPrevalentClass(ObjectClass).Create);
              Add(LastRecovered);
            end;
            _opUpdate : begin
              Read(Index, sizeof(Index));
              SetLastRecovered(Index);
              ReadPrevalentUpdate(LastRecovered);
            end;
            _opAddAssociation, _opDeleteAssociation : begin
              Read(Index, sizeof(Index));
              SetLastRecovered(Index);
              Read(PropIndex, sizeof(PropIndex));//índice da propriedade
              with LastRecovered.Metadata do
                Association := TAssociation(GetObjectProp(LastRecovered, Properties[PropIndex]));
              if Operation = _opAddAssociation then begin
                Read(TargetList, sizeof(TargetList));
                Read(Index, sizeof(Index));
                with PrevalentLists(TargetList) do begin
                  SetLastRecovered(Index);
                  Association.Add(LastRecovered);
                end;
              end
              else begin
                Read(Index, sizeof(Index));
                PrevalentList := PrevalentLists(Association.ObjectClass.ClassName + 'List');
                if PrevalentList <> nil then begin
                  PrevalentList.SetLastRecovered(Index);
                  Association.Delete(PrevalentList.LastRecovered)
                end
                else begin
                  PrevalentList := AbstractLists(Association.ObjectClass.ClassName + 'List');
                  Association.Delete(TPrevalent(PrevalentList.FindInFamily(Index)));
                end;
              end
            end;
            _opDelete : begin
              Read(Index, sizeof(Index));
              SetLastRecovered(Index);
              Delete(LastRecovered);
              LastRecovered := nil;
            end;
          end;
      end;
    finally
      Trace('.....Transações aplicadas: %d', [T]);
      ReportStart;
      EndTransaction;
    end;
end;

procedure TPrevalence.Evolve(Stream : TPrevalentStream);
var
  I, J, ListCount,
  TamMetadados  : integer;
  Tam           : byte;
  PropCount,
  MethodCount   : word;
  PrevalentList : TPrevalentList;

  procedure ChangeClassName;
  var
    I, J : integer;
    Bo : boolean;
    Metadado : TProperties;
  begin
    for I := 0 to RecoveredLists - 1 do // Verifica mudança de nome das classes e remoção de classes
      with EvolveData[I] do
        if ClassName <> '' then begin
          PrevalentList := PrevalentLists(ClassName + 'List');
          if PrevalentList = nil then begin
            Bo := false;
            for J := 0 to FPrevalentLists.Count-1 do begin
              Metadado := Metadata(PrevalentLists(J).ObjectClass.ClassName);
              if 'T'+ Metadado.OldName[0] = ClassName then begin
                ClassName := PrevalentLists(J).ObjectClass.ClassName;
                Bo := true;
              end;
            end;
            if not Bo then ClassName := '*Removida';
          end
        end
        else
          ClassName := '*Removida';
  end;

begin
  with Stream do begin
    Read(FRecoveredLists, sizeof(FRecoveredLists));
    Read(TamMetadados, sizeof(TamMetadados));
    setlength(EvolveData, RecoveredLists);
    for J := 0 to RecoveredLists-1 do
      with EvolveData[J] do begin
        Read(Tam, sizeof(Tam));
        setlength(ClassName, Tam);
        Read(ClassName[1], Tam);
        Read(ClassID, sizeof(ClassID));
        Read(PropCount, sizeof(PropCount));
        setlength(Props, PropCount);
        for I := 0 to PropCount-1 do
          with Props[I] do begin
            Read(Tam, sizeof(Tam));
            setlength(Name, Tam);
            Read(Name[1], Tam);
            Read(ID, sizeof(ID));
            Read(Kind, sizeof(Kind));
            {$IFDEF FPC}
            Kind := TTypeKind(KindDelphi2FPC[Kind]);
            {$ENDIF}
            Read(SubKind, sizeof(SubKind));
            PropInfo := nil;
          end;
        Read(Tam, sizeof(Tam));
        setlength(Package, Tam);
        Read(Package[1], Tam);
        Read(MethodCount, sizeof(MethodCount));
        setlength(Methods, MethodCount);
        for I := 0 to MethodCount -1 do
          with Methods[I] do begin
            Read(Tam, sizeof(Tam));
            setlength(Name, Tam);
            Read(Name[1], Tam);
            Read(ID, sizeof(ID));
          end;
      end;
    ChangeClassName;
    for I := 0 to RecoveredLists - 1 do
      with EvolveData[I] do
        if (ClassName <> '') or (ClassID <> NONELIST) then begin
          PrevalentList := PrevalentLists(ClassName + 'List');
          if (PrevalentList <> nil) and (ClassName <> '*Removida') then //carrega so os prevalents
            with PrevalentList do begin
              Read(ListCount, sizeof(ListCount));//tamanho da lista
              if ListCount > 0 then begin
                Trace('%s: %d', [ClassName, ListCount]);
                SetCapacity(ListCount);
                if ListCount > 0 then ReportStart;
                for J := 0 to ListCount - 1 do begin
                  if (J mod 10000) = 0 then ReportStart;
                  Add(ReadPrevalentEvolve(TPrevalentClass(ObjectClass).Create, EvolveData[I]));
                end;
              end;
            end
          else begin //Pula classes removidas
            Read(ListCount, sizeof(ListCount)); //tamanho da lista
            for J := 0 to ListCount - 1 do begin
              if (J mod 10000) = 0 then ReportStart;
              ReadPrevalentEvolve(nil, EvolveData[I]);
            end;
          end;
        end;
  end;
end;

procedure TPrevalence.LoadDependencyLists;
var
  I : integer;
  List : TPrevalentList;
begin
  try
    for I := 0 to FPrevalentLists.Count -1 do begin
      List := Prevalence.PrevalentLists(I);
      if not List.IsAbstract then begin
        ListInCheck := I;
        if (List.Count > 0) and not List.IsPrimary then
          try
            TObjectList(List).GetKey(List.InternalObjects[0]);
          except
          end;
      end;
    end;
  finally
    ListInCheck := NONELIST;
  end;
end;

procedure TPrevalence.VerifySecondaryLists;
var
  I, J, L : integer;
  List : TPrevalentList;
  Metadados : TProperties;

  procedure SortAssociation(PropIndex : integer);
  var
    K : integer;
    PropObject : TObject;
  begin
    for K := 0 to List.Count-1 do begin
      PropObject := GetObjectProp(List.InternalObjects[K], Metadados.Properties[PropIndex]);
      if PropObject is TAssociation then TAssociation(PropObject).Sort;
    end;
  end;

begin
  LoadDependencyLists;
  for I := 0 to FPrevalentLists.Count-1 do
    with PrevalentLists(I) do
      if IsPrimary then
        if Count > 0 then begin
          ReportStart;
          with Metadata(ObjectClass.ClassName) do
            for L := 2 to PropCount-1 do
              if (ExtraRTTI[L] <> nil) and (ExtraRTTI[L].DependencyLists <> nil) then
                for J := 0 to ExtraRTTI[L].DependencyLists.Count - 1 do
                  PrevalentLists(PtrInt(ExtraRTTI[L].DependencyLists[J])).PendingFastAdd := true;
        end;
  for I := 0 to FPrevalentLists.Count-1 do
    with PrevalentLists(I) do
      if PendingFastAdd then begin
        ReportStart;
        try
          Sort;
          PendingFastAdd := false;
        except end;
        ReportStart;
      end;
  with Prevalence do
    for I := 0 to FPrevalentLists.Count - 1 do begin
      List := PrevalentLists(I);
      if List.IsPrimary and (List.Count > 0) then begin
        Metadados := Metadata(List.ObjectClass.ClassName);
        with List, Metadados do
          for J := PropCount-1 downto 2 do
            if TypeProp[J] = tkClass then
              if IsAssociation[J] then SortAssociation(J);
      end;
    end;
end;

procedure TPrevalence.ResolveIDs;
var
  I, J, K, L, Refs : integer;
  ObjID : PtrInt;
  TargetClassName  : string;
  Prevalent : TPrevalent;
  fAssociacao : TAssociation;
  References : array[0..100] of record
    Assoc : boolean;
    List  : TPrevalentList;
    Prop  : PPropInfo;
    Index : word;
  end;

  procedure LoadReferences;
  var
    J : integer;
  begin
    with PrevalentLists(I) do
      with Metadata(ObjectClass.ClassName) do begin
        Refs := 0;
        for J := PropCount-1 downto 2 do
          if TypeProp[J] = tkClass then begin
            References[Refs].Prop  := Properties[J];
            References[Refs].Assoc := IsAssociation[J];
            if References[Refs].Assoc then
              TargetClassName := TAssociation(GetObjectProp(InternalObjects[0], References[Refs].Prop)).ObjectClass.ClassName + 'List'
            else
              TargetClassName := TypeNameProp[J] + 'List';
            References[Refs].List := PrevalentLists(TargetClassName);
            References[Refs].Index := J;
            if References[Refs].List = nil then
              References[Refs].List := AbstractLists(TargetClassName);
            inc(Refs);
          end;
      end;
  end;

begin
  for I := 0 to FPrevalentLists.Count-1 do
    with PrevalentLists(I) do
      if IsPrimary then
        if Count > 0 then begin
          ReportStart;
          LoadReferences;
          for J := 0 to Count-1 do
            for K := 0 to Refs-1 do with References[K] do
              if Assoc then begin
                fAssociacao := TAssociation(GetObjectProp(InternalObjects[J], Prop));
                with fAssociacao do begin
                  for L := Count-1 downto 0 do begin
                    ObjID := PtrInt(InternalObjects[L]);
                    Prevalent := List.FindInFamily(ObjID);
                    if Prevalent = nil then begin
                      Delete(L);
                      Trace('Evolve -> Não carregado objeto de ID %d na associação: %s', [ObjId, ClassName]);
                    end
                    else
                      InternalObjects[L] := Prevalent;
                  end;
                  Sort;
                end
              end
              else begin
                ObjID := GetOrdProp(InternalObjects[J], Prop);
                Prevalent := List.FindInFamily(ObjID);
                if Prevalent = nil then begin
                  if ObjID <> 0 then begin
                    SetObjectProp(InternalObjects[J], Prop, nil);
                    Trace('Evolve -> Não carregada referência do objeto de ID %d na Classe: %s', [ObjId, ClassName]);
                  end;
                end
                else
                  SetObjectProp(InternalObjects[J], Prop, Prevalent);
              end;
        end;
end;

procedure TPrevalence.AdjustListIndex;
var
  PrevalentsIndex, List, I : integer;
  PrevList : TPrevalentList;
  Name : string;
begin
  FPrevalentLists.Sorted := true;
  for List := 0 to FPrevalentLists.Count - 1 do PrevalentLists(List).ListIndex := NONELIST;
  for List := 0 to FPrevalentLists.Count - 1 do begin
    PrevList := PrevalentLists(List);
    if PrevList.ListIndex = NONELIST then begin
      Name := PrevList.ObjectClass.ClassName;
      PrevalentsIndex := Prevalents.IndexOf(Name);
      Name := Name + 'List';
      for I := List to FPrevalentLists.Count - 1 do
        if Name = PrevalentLists(I).ClassName then begin
          repeat
            PrevList.ListIndex := I;
            PrevList.PrevalentsIndex := PrevalentsIndex;
            PrevList := PrevList.NextManaged;
          until (PrevList = nil) or (PrevList = PrevalentLists(I));
          break;
        end;
    end;
  end;
  for List := 0 to FPrevalentLists.Count - 1 do TObjectList(PrevalentLists(List)).SetObjectListIndex(List);
end;

procedure TPrevalence.Recover;
var
  I : integer;
  EvolveTime,
  LogTime,
  RecoverTime : TDateTime;
  SnapShotFile : TPrevalentStream;
  SnapShotFileName : string;
  List  : TPrevalentList;
  SearchRec : TSearchRec;
  SnapShotDate : integer;
  Loop : boolean;
begin
  Trace('----- Início de Recover ----- Prevalência versão %s', [PrevalenceVersion]);
  RecoverTime := Now;
  try
    for I := 0 to high(RecoverInits) do
      RecoverInits[I];
    AdjustListIndex;
    GetDirectories;
    ManagedLists     := false;
    CheckConstraints := false;
    InRecover        := true;
    ThreadTrans      := nil;
    if FileExists(DirSnapShot + FileName + '.new') then DeleteFile(DirSnapShot + FileName + '.new');//snapshot anterior preso
    FSnapShotVersion := GetVersion;
    SnapShotFileName := DirSnapShot + FileName + '.dat';
    if FileExists(SnapShotFileName) then begin // Recover SnapShot
      InEvolve := true;
      InRecoverSnapShot := true;
      BeginTransaction;
      SnapShotFile := TPrevalentStream.Create(SnapShotFileName, fmOpenRead or fmShareDenyWrite);
      with SnapShotFile do begin
        ReadVersion;
        FSnapShotVersion := Version;
        Trace('Recuperando a partir do SnapShot %s (Versão %.0f)', [SnapShotFileName, SnapShotVersion]);
        EvolveTime := Now;
        Evolve(SnapShotFile);
        Free;
      end;
      ResolveIDs;
      EndTransaction;
      InRecoverSnapShot := false;
      InEvolve := false;
      Trace('Tempo de Evolve: '+ inttostr(MilliSecondsBetween(Now, EvolveTime)));
      Trace('Fim da recuperação a partir do SnapShot %s', [SnapShotFileName]);
    end;
    LogTime := Now;
    if UpperCase(GetIniParameter('Server', 'Environment', 'Development')) <> 'DEVELOPMENT' then begin
      if FindFirst(SnapShotFileName, faAnyFile, SearchRec) = 0 then
        SnapShotDate := SearchRec.Time
      else
        SnapShotDate := 0;
      Loop := FindFirst(DirBackup + FileName + '_*_*.log', faAnyFile, SearchRec) = 0;
      while Loop do begin
        if SnapShotDate < SearchRec.Time then begin
          LogFile.Free;
          LogFile := TPrevalentStream.Create(SearchRec.Name, fmOpenReadWrite or fmShareDenyWrite);
          Trace('Aplicando o Log %s', [DirBackup + SearchRec.Name]);
          RecoverLog;
        end;
        Loop := FindNext(SearchRec) = 0;
      end;
      FindClose(SearchRec);
    end;
    if FileExists(DirLog + FileName + '.log') then
      Trace('Aplicando o Log %s', [DirLog + FileName + '.log'])
    else begin
      LogFile := TPrevalentStream.Create(DirLog + FileName + '.log', fmCreate);
      with LogFile do begin
        WriteVersion;
        Free;
      end;
    end;
    LogFile := TPrevalentStream.Create(DirLog + FileName + '.log', fmOpenReadWrite or fmShareDenyWrite);
    RecoverLog;
    InEvolve := false;
    InRecover := false;
    VerifySecondaryLists;
    CheckConstraints := true;
    try
      for I := 0 to FPrevalentLists.Count -1 do begin
        List := Prevalence.PrevalentLists(I);
        ListInCheck := List.ObjectListIndex;
        if (List.Count > 0) and not List.IsPrimary then
          try
            TObjectList(List).GetKey(List.InternalObjects[0]);
          except
          end;
      end;
    finally
      ListInCheck := NONELIST;
    end;
    Trace('Fim da Recuperacao a partir de Log. Tempo: ' + IntToStr(MilliSecondsBetween(Now, LogTime)));
  except
    on E : Exception do begin
      PartialRecover := true;
      raise EExtP.CreateFmt('Erro no Recover do Log ou do Snapshot: %s', [E.Message], true);
    end
  end;
  EvolveSeguranca;
  if GetAuditory then
    if not FileExists(DirSnapShot + FileName + '.dat') then
      SnapShot
    else begin
      AuditFile := TPrevalentStream.Create(DirAudit + FileName + '.aud', fmOpenReadWrite or fmShareDenyWrite);
      AuditFile.Seek(Auditfile.Size, soFromBeginning)
    end;
  try
    Trace('Initializations');
    for I := 0 to high(Initializations) do CallClassMethod(Initializations[I]);
    Trace('Fim das Initializations');
  except
    on E : Exception do begin
      PartialRecover := true;
      raise EExtP.CreateFmt('Erro nas Initializations do modelo: %s', [E.Message], true);
    end
  end;
  Trace('----- Fim de Recover -----. Tempo: ' + inttostr(MilliSecondsBetween(Now, RecoverTime)));
  if Version <> SnapShotVersion then begin
    Trace('Houve troca de versão. Disparando SnapShot...');
    Snapshot;
  end;
  PartialRecover := false;
  Trace('<\END>');
end;

procedure TPrevalence.SnapShot;
var
  HowLists, TamMetadados : integer;
  List, Obj, ListCount : integer;
  Stream, MetadadosStream : TTransactionStream;
  Extensao : string;
  DataHora : TDateTime;
  SnapShotFile : TPrevalentStream;
begin
  try
    CriticalSection.Enter;
    if GlobalTransactionLevel > 0 then begin
      InWaitSnapShot := true;
      raise EExtP.CreateFmt('Há %d transações em aberto. SnapShot agendado.', [GlobalTransactionLevel]);
    end
    else
      InWaitSnapShot := false;
    Stream := TTransactionStream.Create;
    SnapShotfile := nil;
    DataHora := Now;
    try
      try
        if FileExists(DirSnapShot + FileName + '.new') then
           DeleteFile(DirSnapShot + FileName + '.new');
        SnapShotFile := TPrevalentStream.Create(DirSnapShot + FileName + '.new', fmCreate);
        SnapShotFile.Free;
        SnapShotFile := TPrevalentStream.Create(DirSnapShot + FileName + '.new', fmOpenReadWrite + fmShareDenyWrite);
        Trace('Início do Snapshot.');
        with SnapShotFile do begin
          WriteVersion;
          HowLists := 0;
          for List := 0 to FPrevalentLists.Count - 1 do
            with PrevalentLists(List) do
              if IsPrimary and not IsAbstract{and not IsTransient} then inc(HowLists);
          Stream.Write(HowLists, sizeof(HowLists)); // Quantas listas
          MetadadosStream := TTransactionStream.Create;
          for List := 0 to FPrevalentLists.Count - 1 do
            with PrevalentLists(List) do
              if IsPrimary and not IsAbstract{and not IsTransient} then WriteMetadata(MetadadosStream);
          TamMetadados := MetadadosStream.Position;
          ReportStop;
          Stream.Write(TamMetadados, sizeof(TamMetadados));
          ReportStop;
          CopyFrom(Stream);
          CopyFrom(MetadadosStream);
          MetadadosStream.Free;
          for List := 0 to FPrevalentLists.Count - 1 do
            with PrevalentLists(List) do
              if IsPrimary and not IsAbstract{and not IsTransient} then begin
                if IsTransient then
                  ListCount := 0
                else
                  ListCount := Count;
                Stream.Write(ListCount, sizeof(ListCount)); // tamanho da lista
                if ListCount > 0 then
                  Trace('Snapshot -> Classe: %s Qtd: %d', [ClassName, ListCount]);
                for Obj := 0 to ListCount - 1 do begin
                  Stream.WritePrevalent(TPrevalent(InternalObjects[Obj]), true);
                  if Stream.Position > 65535 then begin
                    CopyFrom(Stream);// Otimização com buffer de 64K
                    sleep(0);
                  end;
                end;
                if Stream.Position > 65535 then
                  CopyFrom(Stream);
                ReportStop;
              end;
          CopyFrom(Stream);
          FlushFileBuffers(Handle);
          Free;
        end;
        Extensao := GetFileExtension(DataHora);
        //SnapShot
        RenameFiles(DirSnapShot + FileName + '.dat', DirBackup + FileName + Extensao + '.dat');
        RenameFiles(DirSnapShot + FileName + '.new', DirSnapShot + FileName + '.dat');
        Trace('Fim do Snapshot.');
        //Auditoria
        if GetAuditory then begin
          if AuditFile <> nil then begin
            FlushFileBuffers(AuditFile.Handle);
            AuditFile.Free;
          end;
          RenameFile(DirAudit + FileName + '.aud' , DirBackup + FileName + Extensao + '.aud');
          FileSetDate(AuditFile.Handle, DateTimeToFileDate(DataHora));
          AuditFile := TPrevalentStream.Create(DirAudit + FileName + '.aud', fmCreate);
          AuditFile.Free;
          AuditFile := TPrevalentStream.Create(DirAudit + FileName + '.aud', fmOpenReadWrite or fmShareDenyWrite);
        end;
        //Log
        FlushFileBuffers(LogFile.Handle);
        LogFile.Free;
        RenameFiles(DirLog + FileName + '.log' , DirBackup + FileName + Extensao + '.log');
        FileSetDate(LogFile.Handle, DateTimeToFileDate(DataHora));
        LogFile := TPrevalentStream.Create(DirLog + FileName + '.log', fmCreate);
        LogFile.Free;
        LogFile := TPrevalentStream.Create(DirLog + FileName + '.log', fmOpenReadWrite or fmShareDenyWrite);
        LogFile.WriteVersion;
        if GetAuditory then begin
          if FileExists(DirBackup + FileName + Extensao + '.aud') then
             AtualizaControleArquivo(FileName + Extensao + '.aud', DataHora, _casArquivado);
          AtualizaControleArquivo(FileName + '.aud', DataHora, _casAtivo);
        end;
      except
        on E : Exception do begin
          if FileExists(DirSnapShot + FileName + '.new') then begin
            SnapShotFile.Free;
            DeleteFile(DirSnapShot + FileName + '.new');
          end;
          raise EExtP.CreateFmt('Erro (%s) na execução do SnapShot. Operação abortada.',[E.Message]);
        end;
      end;
    finally
      Stream.Free;
    end;
  finally
    CriticalSection.Leave;
  end;
end;

procedure TPrevalence.IncTransactionLevel; begin
  if ThreadTrans <> nil then begin
    inc(ThreadTrans.Level);
    CriticalSection.Enter;
    inc(GlobalTransactionLevel);
    CriticalSection.Leave;
  end;
end;

procedure TPrevalence.DecTransactionLevel; begin
  if ThreadTrans <> nil then begin
    dec(ThreadTrans.Level);
    CriticalSection.Enter;
    dec(GlobalTransactionLevel);
    CriticalSection.Leave;
    if (GlobalTransactionLevel = 0) and InWaitSnapShot then begin
      SnapShot;
      InWaitSnapShot := false;
    end;
  end;
end;

procedure TPrevalence.BeginTransaction(pIsolationLevel : TIsolationLevel = ReadCommited); begin
  if ThreadTrans = nil then begin
    ThreadTrans := TTransaction.Create;
    with ThreadTrans do begin
      Level := 0;
      IncTransactionLevel;
      Stream := TTransactionStream.Create;
      Creates := TList.Create;
      if GetAuditory then begin
        AuditStream := TTransactionStream.Create;
        MapeamentoAuditLog := TList.Create;
      { Auditoria
      1. opBeginTransaction
        a) find na classe Aud_Login se não encontrar:
          - grava opLogin(usuário, nome, máquina, perfil)
          - recupera a posição e insere na Aud_Login
        b) grava opBeginTransaction(data e posição do usuário)}
      	if not(InRecover (* or InAuditMode*)) then GravaAuditoria(_opBeginTransaction);
      end;
      IsolationLevel := pIsolationLevel;
    end;
  end
  else
    IncTransactionLevel;
end;

procedure TPrevalent.EfectiveDelete;
var
  I : integer;
  List, PriList : TPrevalentList;
begin
  PriList := Prevalence.PrevalentLists(FListIndex);
  List    := PriList;
  repeat // Exclui das Listas Gerenciadas
    try
      TObjectList(List).Delete(Self);
    except
      for I := 0 to List.Count - 1 do
        if TObjectList(List).InternalObjects[I] = Self then begin
          TObjectList(List).Delete(I);
          break;
        end;
    end;
    List := List.NextManaged;
  until (List = nil) or (List = PriList);
  if PriList.IsManaged then InternalFree;
end;

procedure EfectiveUpdates(IsRollBack : boolean = false);
var
  I, J : integer;
  OldValue : variant;
  List : TList;
begin
  with ThreadTrans do
    if Updates <> nil then begin
      List := TList.Create;
      try
        for I := 0 to Updates.Count-1 do
          with TUpdatedPrevalent(Updates.InternalObjects[I]) do
            if not IsRollback then begin
              NewImage.DuplicateObject(OldImage);
              inc(OldImage.FVersion);
            end
            else
              with OldImage.Metadata do
                for J := 2 to PropCount-1 do
                  if (TypeProp[J] <> tkClass) or ((TypeProp[J] = tkClass) and not IsAssociation[J]) then begin
                    Stream.DirectAccess := true;
                    OldValue := GetPropInfoValue(OldImage, Properties[J]);
                    Stream.DirectAccess := false;
                    if IsDerived[J] or (OldValue <> GetPropInfoValue(NewImage, Properties[J])) then
                       OldImage.MergeDependecyLists(List, J);
                  end;
        for J := 0 to List.Count-1 do
          Prevalence.PrevalentLists(PtrInt(List[J])).Sort;
      finally
        List.Clear;
        FreeAndNil(List);
      end;
    end;
end;

procedure CheckKeyPendencies;
var
  I : integer;
  Prevalent : TPrevalent;
  PriList,
  List  : TPrevalentList;
begin
  with ThreadTrans do begin
    if InsDels = nil then exit;
    for I := 0 to InsDels.Count - 1 do
      if PInsDels(InsDels[I]).IsAdd and (PInsDels(InsDels[I]).Obj.FThreadID > 0) then begin
        Prevalent := Prevalence.GetNewImage(PInsDels(InsDels[I]).Obj);
        PriList := Prevalence.PrevalentLists(Prevalent.ListIndex);
        List := PriList;
        repeat
          with List do
            if not IsPrimary and IsManaged then begin
              ListInCheck := List.ObjectListIndex;
              try
                try
                  GetKey(Prevalent);
                except
                  if not Prevalence.InRecover then
                    raise EExtP.CreateFmt('%s: %s. Inclusão com a chave %s incompleta.',
                      [Prevalent.GetClassAlias, List.ClassName, Prevalent.GetIdentification])
                end;
              finally
                ListInCheck := NONELIST;
              end;
            end;
          List := List.NextManaged;
        until (List = nil) or (List = PriList);
      end;
  end;
end;

procedure EfectiveInsDels(IsRollBack : boolean = false);
var
  I : Integer;

  procedure SearchAndNilObj;
  var
    J : Integer;
  begin
    with ThreadTrans do begin
      if Updates <> nil then
        for J := Updates.Count-1 downto 0 do
          with TUpdatedPrevalent(Updates.InternalObjects[J]), PInsDels(InsDels[I])^ do
            if (OldImage = Obj) or (NewImage = Obj) then Updates.Delete(J);
      for J := I+1 to InsDels.Count-1 do
        with PInsDels(InsDels[I])^ do
          if Obj = PInsDels(InsDels[J]).Obj then PInsDels(InsDels[J]).Obj := nil;
    end;
  end;

begin
  with ThreadTrans do
    if InsDels <> nil then begin
      for I := 0 to InsDels.Count-1 do
        with PInsDels(InsDels[I])^ do
          if Obj <> nil then
            if not IsRollback then //EndTransaction
              if IsAdd then begin
                Obj.FThreadID := 0; // Commit do Insert
                Obj.FVersion  := 1;
              end
              else begin
                try
                  Obj.EfectiveDelete; // Commit do Delete
                  SearchAndNilObj;
                except
                end;
              end
            else //Rollback
              if IsAdd then begin
                if Obj.ThreadID > 0 then Obj.EfectiveDelete; // Rollback do Insert
                SearchAndNilObj;
              end
              else //artifício para minimizar a possibilidade de invasão de memória em transações concorrentes
                if (Obj.FID > 0) and (abs(Obj.FThreadID) = integer(GetFCGIThreadID)) then Obj.FThreadID := 0; // Rollback do Delete
      for I := InsDels.Count-1 downto 0 do begin
        Dispose(PInsDels(InsDels[I]));
        InsDels.Delete(I);
      end;
    end;
end;

procedure EfectiveAssociations(IsRollBack : boolean = false);
var
  I, J, MyThread : integer;
begin
  with ThreadTrans do
    if Associations <> nil then begin
      MyThread := GetFCGIThreadID;
      for I := 0 to Associations.Count-1 do
        with TAssociation(Associations[I]) do
          if TransObjs <> nil then begin
            for J := 0 to TransObjs.Count-1 do
              with PTransObj(TransObjs[J])^ do
                if abs(ThreadID) = MyThread then begin
                  if not IsRollBack then begin
                    if ThreadID < 0 then
                      Delete(Prevalent, nil, IsReflect, IsRollback)
                    else
                      Add(Prevalent, IsReflect, IsRollback);
                  end
                  else //Rollback
                    if ThreadID > 0 then
                      Delete(Prevalent, nil, IsReflect, IsRollback)
                    else
                      Add(Prevalent, IsReflect, IsRollback);
                  ThreadID := 0;
                end;
            for J := TransObjs.Count-1 downto 0 do
              if PTransObj(TransObjs[J]).ThreadID = 0 then begin
                Dispose(PTransObj(TransObjs[J]));
                TransObjs.Delete(J);
              end;
            if TransObjs.Count = 0 then
              FreeAndNil(TransObjs);
          end;
      for I := Associations.Count-1 downto 0 do
        Associations.Delete(I);
    end;
end;

procedure CheckTransactionConstraints;
var
  I : integer;
begin
  with ThreadTrans do begin
    if InsDels <> nil then
      for I := 0 to InsDels.Count-1 do
        if PInsDels(InsDels[I]).IsAdd and (PInsDels(InsDels[I]).Obj.FThreadID > 0) then
          PInsDels(InsDels[I]).Obj.DoCheckConstraints;
    if Updates <> nil then begin
      for I := 0 to Updates.Count-1 do
        with TUpdatedPrevalent(Updates.InternalObjects[I]) do
          if NewImage.Version <> OldImage.Version then
            raise EExtP.CreateFmt('%s:%s foi atualizado por outra sessão. Transação desfeita.',
              [OldImage.GetClassAlias, OldImage.GetFirstProperty]);
      Stream.DirectAccess := true;
      try
        for I := 0 to Updates.Count-1 do
          with TUpdatedPrevalent(Updates.InternalObjects[I]) do
            if OldImage.FThreadID >= 0 then NewImage.DoCheckConstraints(true, OldImage);
      finally
        Stream.DirectAccess := false;
      end;
    end;
  end;
end;

function TPrevalence.CheckTransaction : string; begin
  try
    if CheckConstraints then CheckTransactionConstraints;
    TerminateSession := true;
    try
      CheckKeyPendencies;
    finally
      TerminateSession := false;
    end;
    Result := '';
  except
    on E : Exception do begin
      RollBack;
      Result := E.Message;
    end;
  end;
end;

procedure TPrevalence.EndTransaction(Force : boolean = false);
var
  Error : string;
begin
  DecTransactionLevel;
  if ThreadTrans = nil then
    if Force then
      exit
    else
      raise EExtP.Create('EndTransaction sem o BeginTransaction correspondente.');
   with ThreadTrans do begin
    if not Force and (Level > 0) then exit;
    CriticalSection.Enter;
    try
      Error := CheckTransaction;
      if Error <> '' then begin
        IncTransactionLevel;
        raise EExtP.Create(Error);
      end;
      while Level > 0 do DecTransactionLevel;
      TerminateSession := true;
      try
        EfectiveUpdates;
        EfectiveAssociations;
        EfectiveInsDels;
        if GetAuditory and (AuditFile <> nil) then begin
          CorrigeMapeamentoAuditLog(AuditFile.Size, LogFile.Size);
          AuditFile.CopyFrom(AuditStream);
          if ForceFlush then FlushFileBuffers(AuditFile.Handle);
        end;
        LogFile.CopyFrom(Stream);
        if ForceFlush then FlushFileBuffers(LogFile.Handle);
      finally
        FreeAndNil(ThreadTrans);
      end;
    finally
      TerminateSession := false;
      CriticalSection.Leave;
    end;
  end;
end;

procedure TPrevalence.RollBack(Msg : string = ''); begin
  if (ListInCheck <> NONELIST) or InRecover then
    raise Exception.Create('');
  if ThreadTrans <> nil then try
    CriticalSection.Enter;
    TerminateSession := true;
    EfectiveAssociations(true);
    EfectiveInsDels(true);
    EfectiveUpdates(true);
  finally
    TerminateSession := false;
    while ThreadTrans.Level > 0 do DecTransactionLevel;
    FreeAndNil(ThreadTrans);
    CriticalSection.Leave;
  end;
  if Msg = '' then begin
  	if ExceptObject <> nil then
      if ExceptObject is EAbort then
        Abort
      else
        raise EExtP.Create(Exception(ExceptObject).Message)
  end
  else
    if pos('EAbort', Msg) <> 0 then
      exit
    else
      raise EExtP.Create(Msg)
end;

procedure TPrevalence.ClearTransaction; begin
  try Rollback except end;
end;

procedure TPrevalent.ResolveDependencyLists(List : TPrevalentList; NewImage : TPrevalent);

  procedure ResolveDependencyAssociation;
  var
    J, K, ClassIndex, ClassListIndex : integer;
    ClassProps: TProperties;
    AssocList : TAssociation;
    ClassList : TPrevalentList;
  begin
    with List do
      for J := 0 to DependencyListAssoc.Count -1 do begin
        ClassIndex := PtrInt(DependencyListAssoc[J]);
        ClassProps := TProperties(Prevalence.Prevalents.Objects[ClassIndex]);
        ClassListIndex := Prevalence.FPrevalentLists.IndexOf(Prevalence.Prevalents[ClassIndex] + 'List');
        ClassList := Prevalence.PrevalentLists(ClassListIndex);
        if ClassList.Count > 0 then
          for K := ClassProps.PropCount -1 downto 2 do
            if ClassProps.TypeProp[K] = tkClass then
              if ClassProps.IsAssociation[K] then begin
                AssocList := TAssociation(GetObjectProp(ClassList.InternalObjects[0], ClassProps.Properties[K]));
                if AssocList.GetKeyCode = List.GetKeyCode then AssocList.Sort;
              end;
      end;
  end;

begin
  if InMoveCorresponding or (NewImage.FID <= 0) then exit;
  List.Sort;
  ResolveDependencyAssociation;
end;

procedure TPrevalent.ResolveDependencyLists(Index : word; NewImage : TPrevalent);
var
  I : integer;
begin
  with NewImage.Metadata do
    if (ExtraRTTI[Index] <> nil) and (ExtraRTTI[Index].DependencyLists <> nil) then
      for I := 0 to ExtraRTTI[Index].DependencyLists.Count -1 do
        ResolveDependencyLists(Prevalence.PrevalentLists(PtrInt(ExtraRTTI[Index].DependencyLists[I])), NewImage);
end;

procedure TPrevalent.UpdateLog(Index : word; NewImage : TPrevalent; Stream : TTransactionStream; InRollback : boolean = false);
var
  Operation : T_Operation;
  Props : TProperties;
  PropObject, PrevObject : TPrevalent;
begin
  if not Prevalence.InRecoverSnapShot then
    with Stream do
      if InRollback or (not DirectAccess and not DontUpdateLog) then begin
        ResolveDependencyLists(Index, Self);
        Props := Metadata;
        if (Props.TypeProp[Index] = tkClass) then begin
          Stream.GetLastImage := true;
          PrevObject := TPrevalent(GetObjectProp(Self, Props.Properties[Index]));
          Stream.GetLastImage := false;
          PropObject := TPrevalent(GetObjectProp(NewImage, Props.Properties[Index]));
          DontUpdateLog := True;
          HandleReflectionsForUpdate(NewImage, PropObject, PrevObject, Index, Stream, Props, InRollback);
          DontUpdateLog := False;
        end
        else
          if Props.Unidirect <> nil then begin
            DontUpdateLog := True;
            HandleReflectionsForUpdate(NewImage, nil, nil, Index, Stream, Props);
            DontUpdateLog := False;
          end;
        if not InRollback and not Prevalence.InRecover and (FID > 0) and not Prevalence.PrevalentLists(FListIndex).IsTransient {and not InAuditMode} then begin
          if UpdateObject <> Self then begin
            UpdateObject := Self;
            Operation := _opUpdate;
            Write(Operation, sizeof(Operation));
            if GetAuditory then GravaAuditoria(Operation, NewImage);
            Write(FListIndex, sizeof(FListIndex));
            Write(FID, sizeof(FID));
          end
          else
            if Position > 2 then Position := Position - sizeof(Index); // Volta o EndUpdate
          Write(Index, sizeof(Index));
          WriteProperty(Self, Props, Index);
          Index := ENDUPDATE;
          Write(Index, sizeof(Index));
        end;
      end;
end;

procedure TPrevalence.Log(Operation: T_Operation; Prevalent: TPrevalent; ImmediateWrite : boolean = false);
var
  ImmediateStream : TTransactionStream;
  Prop  : word;
  Props : TProperties;
begin
  if not InRecover and not TerminateSession and not PrevalentLists(Prevalent.FListIndex).IsTransient {and not InAuditMode} then
    if ImmediateWrite then begin // para sequence
      ImmediateStream := TTransactionStream.Create;
      try
        with ImmediateStream do begin
          UpdateObject := nil;
          Write(Operation, sizeof(Operation));
          Write(Prevalent.FListIndex, sizeof(Prevalent.FListIndex));
          if Operation = _opAdd then
            WritePrevalent(Prevalent)
          else begin
            Write(Prevalent.ID, sizeof(integer));
            Props := Prevalent.Metadata;
            for Prop := 2 to Props.PropCount-1 do begin
              Write(Prop, sizeof(Prop));
              WriteProperty(Prevalent, Props, Prop);
            end;
            Prop := ENDUPDATE;
            Write(Prop, sizeof(Prop));
          end;
          CriticalSection.Enter;
          LogFile.CopyFrom(ImmediateStream);
          CriticalSection.Leave;
        end;
      finally
        ImmediateStream.Free;
      end;
    end
    else
      with ThreadTrans, Stream do begin
        UpdateObject := nil;
        Write(Operation, sizeof(Operation));
        if GetAuditory then GravaAuditoria(Operation, Prevalent);
        Write(Prevalent.FListIndex, sizeof(Prevalent.FListIndex));
        if Operation = _opAdd then
          WritePrevalent(Prevalent)
        else
          Write(Prevalent.ID, sizeof(integer));
      end;
end;

procedure TPrevalence.Log(Operation: T_Operation; Association : TAssociation; Prevalent : TPrevalent); begin
  if not InRecover and not TerminateSession and not Association.IsTransient {and not InAuditMode} then
    with Association, ThreadTrans, Stream do begin
      UpdateObject := nil;
      Write(Operation, sizeof(Operation));
      Write(Owner.FListIndex, sizeof(Owner.FListIndex));
      Write(Owner.ID, sizeof(integer));
      Write(PropIndex, sizeof(PropIndex));
      if Operation = _opAddAssociation then Write(Prevalent.FListIndex, sizeof(Prevalent.FListIndex));
      Write(Prevalent.ID, sizeof(integer));
    end;
end;

function TPrevalence.Metadata(pClassName : string) : TProperties;
const
  ClassName : string = '';
  Prop      : TProperties = nil;
var
  I : Integer;
begin
  if pClassName = ClassName then
    Result := Prop
  else begin
    with Prevalents do begin
      I := IndexOf(pClassName);
      if I >= 0 then
        Result := TProperties(Objects[I])
      else
        Result := nil;
    end;
    Prop := Result;
    ClassName := pClassName;
  end;
end;

procedure TPrevalence.AddAbstractClass(AbstractClass : TTransientClass; AbstractList : TPrevalentList); begin
  if Prevalents.IndexOf(AbstractClass.ClassName) = -1 then begin
    Prevalents.AddObject(AbstractClass.ClassName, TProperties.Create(AbstractClass, 0));
    FAbstractLists.AddObject(AbstractClass.ClassName + 'List', AbstractList);
  end;
end;

function TPrevalence.PrevalentLists(ListIndex: word): TPrevalentList; begin
  if ListIndex < FPrevalentLists.Count then
    Result := TPrevalentList(FPrevalentLists.Objects[ListIndex])
  else
    Result := nil;
end;

function TPrevalence.PrevalentLists(ListName : string): TPrevalentList;
var
  I : integer;
begin
  I := FPrevalentLists.IndexOf(ListName);
  if I >= 0 then
    Result := TPrevalentList(FPrevalentLists.Objects[I])
  else
    Result := nil
end;

function TPrevalence.AbstractLists(ListName : string): TPrevalentList;
var
  I : integer;
begin
  I := FAbstractLists.IndexOf(ListName);
  if I >= 0 then
    Result := TPrevalentList(FAbstractLists.Objects[I])
  else
    Result := nil
end;

procedure TPrevalence.SetForceFlush(const Value: boolean); begin
  FForceFlush := Value;
  FlushFileBuffers(LogFile.Handle);
  if GetAuditory then FlushFileBuffers(AuditFile.Handle);
end;

function TPrevalence.FindInUpdate(Prevalent: TPrevalent) : TPrevalent; begin
  Result := nil;
  if ThreadTrans = nil then exit;
  with ThreadTrans do
    if (Updates = nil) or (Updates.Count = 0) then
      Result := nil
    else
      Result := TPrevalent(Updates.Find(PtrInt(Prevalent)));
end;

function TPrevalence.GetImages(Prevalent : TPrevalent) : TPrevalentImages; begin
  if InRecoverSnapShot then
    Result.NewImage := Prevalent
  else
    if ThreadTrans <> nil then begin
      if InRecover or (Prevalent.FID = 0) or ThreadTrans.Stream.DirectAccess then
        Result.NewImage := Prevalent
      else
        Result.NewImage := ThreadTrans.BeginUpdate(Prevalent).NewImage;
      Result.Stream := ThreadTrans.Stream
    end
    else
      raise EExtP.Create('Erro: Atualização sem BeginTransaction.')
end;

function TPrevalence.GetNewImage(Prevalent : TPrevalent; Prop : integer = -1) : TPrevalent;
var
  UpdatedPrevalent : TUpdatedPrevalent;
begin
  if (ListInCheck = NONELIST) and (not Prevalence.InRecover) and (Prevalent = nil) then
    raise EExtP.Create('Tentativa de uso de objeto inexistente.');
  if Prop <> -1 then Prevalent.SetDependencyLists(Prop);
  if Prevalence.IsInRecoverSnapShot then begin
    Result := Prevalent;
    exit;
  end;
  if (ThreadTrans = nil) or ThreadTrans.Stream.DirectAccess or (ThreadTrans.Updates = nil) or (ThreadTrans.Updates.Count = 0) then
    Result := Prevalent
  else begin
    UpdatedPrevalent := TUpdatedPrevalent(ThreadTrans.Updates.Find(PtrInt(Prevalent)));
    if UpdatedPrevalent = nil then
      Result := Prevalent
    else
      if ThreadTrans.Stream.GetLastImage then
        Result := UpdatedPrevalent.LastImage
      else
        Result := UpdatedPrevalent.NewImage;
  end;
  if (ListInCheck <> NONELIST) and ((Result <> nil) and (Result.FThreadID < 0{Arlon: deve testar apenas a minha sessão?})) then Result := nil;
end;

function TPrevalence.GetStream: TTransactionStream; begin
  if ThreadTrans <> nil then
    Result := ThreadTrans.Stream
  else
    raise EExtP.Create('Erro: Atualização sem BeginTransaction.')
end;

procedure TPrevalence.CheckSortList(Trace : boolean); begin
{$IFNDEF NEWXSC}
//--> retirar esta procedure quando for implantada a nova versão de segurança
{$ENDIF}
end;

procedure TPrevalence.CheckModel(AndClean : boolean = false; pClass : TClass = nil);
var
  I, J, K : integer;
  Prop : word;
  Ret : string;
  Error : boolean;
begin
  J := 0;
  if AndClean then
    Trace('*** Início do CheckModel com Limpeza de inconsistências ***')
  else
    Trace('*** Início do CheckModel ***');
  for I := 0 to FPrevalentLists.Count - 1 do
    with PrevalentLists(I) do begin
      if IsAbstract or not IsPrimary then continue;
      if (pClass <> nil) and (pClass <> ObjectClass) then continue;
      Trace('.Verificando: Classe: %s (Qtd:%d)', [ObjectClass.ClassName, FCount]);
      for K := FCount-1 downto 0 do begin
        Error := false;
        with TPrevalent(InternalObjects[K]).Metadata do begin
          for Prop := 2 to PropCount-1 do begin
            Ret := CheckConstraint(InternalObjects[K], Prop);
            if Ret <> '' then begin
              Ret := StringReplace(Ret, #13#10, '', [rfReplaceAll]);
              Trace('...Erro: ID: %d Inconsistência: %s', [TPrevalent(InternalObjects[K]).FID, Ret]);
              Error := true;
            end;
          end;
        end;
        if Error then begin
          if AndClean then begin
            BeginTransaction;
            TPrevalent(InternalObjects[K]).Delete;
            Trace('..Objeto Apagado!');
            EndTransaction;
          end;
          inc(J);
        end;
      end;
    end;
  if AndClean then begin
    Trace('*** Fim do CheckModel com Limpeza de inconsistências ***');
    Trace('*** %d objetos apagados ***',[J]);
  end
  else begin
    Trace('*** Fim do CheckModel ***');
    Trace('*** %d objetos inconsistentes ***',[J]);
  end;
end;

{ TPrevalentStream }

constructor TPrevalentStream.Create(const FileName: string; Mode: Word); begin
  inherited Create(FileName, Mode);
end;

destructor TPrevalentStream.Destroy; begin
  FlushFileBuffers(Handle);
  inherited;
end;

function TPrevalentStream.Read(var Buffer; Count: integer): integer; begin
  Result := ReadFromStandard(Buffer, Count);
end;

function TPrevalentStream.ReadFromStandard(var Buffer; Count: integer): integer;
var
  PosTarget : integer;
  Buf : array[0..65535] of byte absolute Buffer;
begin
  PosTarget := 0;
  if (Count + PosBufferReaded) > LenBufferReaded then begin
    if LenBufferReaded <> 0 then begin
      PosTarget := LenBufferReaded - PosBufferReaded;
      if PosTarget <> 0 then begin
        move(FBufferReaded[PosBufferReaded], Buf, PosTarget);
        inc(Position, PosTarget);
        dec(Count, PosTarget);
      end;
    end;
    LenBufferReaded := inherited Read(FBufferReaded, sizeof(FBufferReaded));
    if LenBufferReaded < Count then Count := LenBufferReaded;
    PosBufferReaded := 0;
  end;
  move(FBufferReaded[PosBufferReaded], Buf[PosTarget], Count);
  inc(PosBufferReaded, Count); inc(Position, Count);
  Result := Count + PosTarget;
end;

procedure TPrevalentStream.CopyFrom(Transaction : TMemoryStream);
var
  Len : integer;
begin
  if Transaction.Position = 0 then exit;
  Position := Size;
  Len := Transaction.Position;
  Transaction.Position := 0;
  inherited CopyFrom(Transaction, Len);
  Transaction.Position := 0;
end;

procedure TPrevalentStream.ReadVersion; begin
  ReadFromStandard(Version, sizeof(Version));
end;

procedure TPrevalentStream.WriteVersion; begin
  Version := GetVersion;
  Write(Version, sizeof(Version));
end;

procedure TPrevalentStream.ReadProperty(Prevalent : TPrevalent; Props : TProperties; Prop : word; IsSnapShot : boolean = false);
var
  I, Index,
  PropInteger  : integer;
  PropByte     : byte;
  PropWord     : word;
  PropInt64    : Int64;
  PropString   : string;
  PropDouble   : double;
  PropCurrency : currency;
  PropSingle   : single;
  PropExtended : extended;
  PropObject   : TObject;
  PropList     : word;
begin
  with Props do
    if not IsDerived[Prop] then
      case TypeProp[Prop] of
        tkInteger, tkEnumeration, tkChar, tkSet, tkWChar :
          case OrdTypeProp[Prop] of
            otSLong, otULong : begin
              Read(PropInteger, sizeof(PropInteger));
              SetOrdProp(Prevalent, Properties[Prop], PropInteger);
            end;
            otSByte, otUByte : begin
              Read(PropByte, sizeof(PropByte));
              SetOrdProp(Prevalent, Properties[Prop], PropByte);
            end;
            otSWord, otUWord : begin
              Read(PropWord, sizeof(PropWord));
              SetOrdProp(Prevalent, Properties[Prop], PropWord);
            end;
          end;
        {$IFDEF FPC}
        tkBool : begin
          Read(PropByte, sizeof(PropByte));
          SetOrdProp(Prevalent, Properties[Prop], PropByte);
        end;
		    {$ENDIF}
        tkInt64 : begin
          Read(PropInt64, sizeof(PropInt64));
          SetInt64Prop(Prevalent, Properties[Prop], PropInt64);
        end;
        tkFloat :
          case FloatTypeProp[Prop] of
            ftCurr : begin
              Read(PropCurrency, sizeof(PropCurrency));
              SetFloatProp(Prevalent, Properties[Prop], PropCurrency);
            end;
            ftDouble, ftComp : begin
              Read(PropDouble, sizeof(PropDouble));
              SetFloatProp(Prevalent, Properties[Prop], PropDouble);
            end;
            ftSingle : begin
              Read(PropSingle, sizeof(PropSingle));
              SetFloatProp(Prevalent, Properties[Prop], PropSingle);
            end;
            ftExtended : begin
              Read(PropExtended, sizeof(PropExtended));
              SetFloatProp(Prevalent, Properties[Prop], PropExtended);
            end;
          end;
        tkLString, tkString : begin
          Read(PropWord, sizeof(PropWord));
          SetLength(PropString, PropWord);
          Read(PropString[1], PropWord);
          SetStrProp(Prevalent, Properties[Prop], PropString);
        end;
        tkClass : begin
          PropObject := GetObjectProp(Prevalent, Properties[Prop]);
          if (PropObject = nil) or (PropObject is TPrevalent) then begin
            Read(PropList, sizeof(PropList));
            if PropList <> NONELIST then begin
              Read(PropInteger, sizeof(PropInteger));
              if PropInteger < 1 then begin // Insere objeto novo
                Read(PropExtended, 3); //seek(3, soFromCurrent); // Bypassa opAdd e ListIndex
                PropObject := TPrevalentClass(Prevalence.PrevalentLists(PropList).ObjectClass).Create;
                with Prevalence.PrevalentLists(PropList) do begin
                  LastRecovered := ReadPrevalent(TPrevalent(PropObject));//arlon
                  Add(LastRecovered);
                end;
                if IsSnapShot then
                  SetOrdProp(Prevalent, Properties[Prop], TPrevalent(PropObject).ID)
                else begin
                  SetObjectProp(Prevalent, Properties[Prop], PropObject);
                  Prevalent.UpdateLog(Prop, Prevalent, Prevalence.GetStream); //arlon
                end
              end
              else
                if IsSnapShot then
                  SetOrdProp(Prevalent, Properties[Prop], PropInteger)
                else
                  with Prevalence.PrevalentLists(PropList) do begin
                    SetObjectProp(Prevalent, Properties[Prop], Find(PropInteger));
                    Prevalent.UpdateLog(Prop, Prevalent, Prevalence.GetStream); //arlon
                  end;
            end
            else
              SetOrdProp(Prevalent, Properties[Prop], 0)
          end
          else
            if IsSnapShot and (PropObject is TAssociation) then begin
              Read(PropInteger, sizeof(PropInteger));
              if PropInteger > 0 then begin
                TAssociation(PropObject).SetCapacity(PropInteger);
                for I := 0 to PropInteger-1 do begin
                  Read(Index, sizeof(Index));
                  TObjectList(PropObject).Add(TTransient(Index));
                end;
              end;
            end;
        end;
      end;
end;

function TPrevalentStream.ReadPrevalentEvolve(Prevalent : TPrevalent; Evolve : TEvolveRecord) : TPrevalent;
var
  I, J, Index,
  PropInteger  : integer;
  PropByte     : byte;
  PropWord     : word;
  PropInt64    : Int64;
  PropString   : string;
  PropDouble   : double;
  PropCurrency : currency;
  PropSingle   : single;
  PropExtended : extended;
  PropList     : word;
  Obj : TObject;
begin
  Result := Prevalent;
  with Evolve do
    for I := 0 to length(Props)-1 do with Props[I] do try
      if (Kind <> tkClass) and (SubKind = NONELIST) then continue; //atributo derivado
      case Kind of
        tkLString, tkString : begin
          Read(PropWord, sizeof(PropWord));
          if PropWord = 0 then continue;
          SetLength(PropString, PropWord);
          Read(PropString[1], PropWord);
          if Prevalent <> nil then begin
            if PropInfo = nil then PropInfo := Prevalent.GetEvolveProperty(Name);
            if PropInfo <> nil then SetStrProp(Prevalent, PropInfo, PropString);
          end;
        end;
        tkInteger, tkEnumeration, tkChar, tkSet, tkWChar :
          case TOrdType(SubKind) of
            otSLong, otULong : begin
              Read(PropInteger, sizeof(PropInteger));
              if Prevalent <> nil then begin
                if PropInfo = nil then PropInfo := Prevalent.GetEvolveProperty(Name);
                if PropInfo <> nil then SetOrdProp(Prevalent, PropInfo, PropInteger);
              end;
            end;
            otSByte, otUByte : begin
              Read(PropByte, sizeof(PropByte));
              if Prevalent <> nil then begin
                if PropInfo = nil then PropInfo := Prevalent.GetEvolveProperty(Name);
                if PropInfo <> nil then SetOrdProp(Prevalent, PropInfo, PropByte);
              end;
            end;
            otSWord, otUWord : begin
              Read(PropWord, sizeof(PropWord));
              if Prevalent <> nil then begin
                if PropInfo = nil then PropInfo := Prevalent.GetEvolveProperty(Name);
                if PropInfo <> nil then SetOrdProp(Prevalent, PropInfo, PropWord);
              end;
            end;
          end;
        {$IFDEF FPC}
		    tkBool : begin
          Read(PropByte, sizeof(PropByte));
          if Prevalent <> nil then begin
            if PropInfo = nil then PropInfo := Prevalent.GetEvolveProperty(Name);
            if PropInfo <> nil then SetOrdProp(Prevalent, PropInfo, PropByte);
          end;
		    end;
		    {$ENDIF}
        tkInt64 : begin
          Read(PropInt64, sizeof(PropInt64));
          if Prevalent <> nil then begin
            if PropInfo = nil then PropInfo := Prevalent.GetEvolveProperty(Name);
            if PropInfo <> nil then SetInt64Prop(Prevalent, PropInfo, PropInt64);
          end;
        end;
        tkFloat :
          case TFloatType(SubKind) of
            ftCurr : begin
              Read(PropCurrency, sizeof(PropCurrency));
              if Prevalent <> nil then begin
                if PropInfo = nil then PropInfo := Prevalent.GetEvolveProperty(Name);
                if PropInfo <> nil then SetFloatProp(Prevalent, PropInfo, PropCurrency);
              end;
            end;
            ftDouble, ftComp : begin
              Read(PropDouble, sizeof(PropDouble));
              if Prevalent <> nil then begin
                if PropInfo = nil then PropInfo := Prevalent.GetEvolveProperty(Name);
                if PropInfo <> nil then SetFloatProp(Prevalent, PropInfo, PropDouble);
              end;
            end;
            ftSingle : begin
              Read(PropSingle, sizeof(PropSingle));
              if Prevalent <> nil then begin
                if PropInfo = nil then PropInfo := Prevalent.GetEvolveProperty(Name);
                if PropInfo <> nil then SetFloatProp(Prevalent, PropInfo, PropSingle);
              end;
            end;
            ftExtended : begin
              Read(PropExtended, sizeof(PropExtended));
              if Prevalent <> nil then begin
                if PropInfo = nil then PropInfo := Prevalent.GetEvolveProperty(Name);
                if PropInfo <> nil then SetFloatProp(Prevalent, PropInfo, PropExtended);
              end;
            end;
          end;
        tkClass :
          if SubKind <> NONELIST then begin // Referência
            Read(PropList, sizeof(PropList));
            if PropList <> NONELIST then begin
              Read(PropInteger, sizeof(PropInteger));
              if Prevalent <> nil then
                Prevalent.EvolveClass(Name, PropInteger, Evolve, I)
            end
            else
              if Prevalent <> nil then Prevalent.EvolveClass(Name, 0, Evolve, I)
          end
          else begin // Associação
            Read(PropInteger, sizeof(PropInteger)); // Quantidade de elementos da associação
            if PropInteger > 0 then begin
              if ClassName = '*Removida' then
                Obj := nil
              else
                Obj := Prevalent.EvolveAssociation(Name, Evolve, I);
              if Obj is TAssociation then
                with TObjectList(Obj) do begin
                  SetCapacity(PropInteger);
                  for J := 0 to PropInteger-1 do begin
                    Read(Index, sizeof(Index));
                    Add(TTransient(Index)) // Reference/Association --> Association
                  end;
                end
              else begin
                if Prevalent <> nil then Prevalent.EvolveClass(Name, 0, Evolve, I); //mudou para referência;
                for J := 0 to PropInteger-1 do //pula os elementos da associacao
                  Read(Index, sizeof(Index));
              end;
            end;
          end;
        end;
      except
        on E : Exception do begin
          Trace('Erro durante o Evolve do modelo: %s:%s (Atributo:%s) Mensagem: %s:%s', [Prevalent.ClassName, Prevalent.GetFirstProperty, Name, E.ClassName, E.Message]);
          raise;
        end;
      end;
end;

function TPrevalentStream.ReadPrevalent(Prevalent: TPrevalent; IsSnapShot : boolean) : TPrevalent;
var
  Prop  : word;
  Props : TProperties;
begin
  Props := Prevalent.Metadata;
  for Prop := 0 to Props.PropCount-1 do
    ReadProperty(Prevalent, Props, Prop, IsSnapShot);
  Result := Prevalent;
end;

procedure TPrevalentStream.ReadPrevalentUpdate(Prevalent: TPrevalent);
var
  Prop  : word;
  Props : TProperties;
begin
  if Prevalent = nil then exit;
  Props := Prevalent.Metadata;
  Read(Prop, sizeof(Prop));
  while Prop <> ENDUPDATE do begin
    ReadProperty(Prevalent, Props, Prop);
    Read(Prop, sizeof(Prop));
  end;
end;

procedure TPrevalentStream.ReadAuditPrevalentUpdate(Prevalent: TPrevalent; IsByPass : boolean);
var
  Prop  : word;
begin
  if Prevalent <> nil then begin
    Read(Prop, sizeof(Prop));
    while Prop <> ENDUPDATE do
      Read(Prop, sizeof(Prop));
  end;
end;

procedure TPrevalentStream.LoadHeaderFile(var HeaderFile : THeaderFile);
var
  I, J, Q, K, Pos,
  RecoveredLists,
  TamMetadados  : integer;
  Tam,
  PropCount     : word;
begin
  Pos := 4;
  inc(Pos, Read(HeaderFile.Version, sizeof(HeaderFile.Version)));
  inc(Pos, Read(HeaderFile.Date, sizeof(HeaderFile.Date)));
  inc(Pos, Read(RecoveredLists, sizeof(RecoveredLists)));
  inc(Pos, Read(TamMetadados, sizeof(TamMetadados)));
  inc(TamMetadados, Pos);
  setlength(HeaderFile.HeaderData, RecoveredLists);
  J := 0;
  while Pos < TamMetadados do begin
    with HeaderFile.HeaderData[J] do begin
      inc(Pos, Read(Tam, sizeof(Tam)));
      setlength(PackageName, Tam);
      inc(Pos, Read(PackageName[1], Tam));
      inc(Pos, Read(ListIndex, sizeof(ListIndex)));
      inc(Pos, Read(Tam, sizeof(Tam)));
      setlength(ClassName, Tam);
      inc(Pos, Read(ClassName[1], Tam));
      inc(Pos, Read(Tam, sizeof(Tam)));
      setlength(ClassAlias, Tam);
      inc(Pos, Read(ClassAlias[1], Tam));
      inc(Pos, Read(PropCount, sizeof(PropCount)));
      setlength(Props, PropCount);
      for I := 0 to PropCount - 1 do
        with Props[I] do begin
          inc(Pos, Read(PropIndex, sizeof(PropIndex)));
          inc(Pos, Read(Tam, sizeof(Tam)));
          setlength(PropName, Tam);
          inc(Pos, Read(PropName[1], Tam));
          inc(Pos, Read(Tam, sizeof(Tam)));
          setlength(PropAlias, Tam);
          inc(Pos, Read(PropAlias[1], Tam));
          inc(Pos, Read(Tam, sizeof(Tam)));
          setlength(PropMask, Tam);
          inc(Pos, Read(PropMask[1], Tam));
          inc(Pos, Read(PropKind, sizeof(PropKind)));
          {$IFDEF FPC}PropKind := TTypeKind(KindDelphi2FPC[PropKind]);{$ENDIF}
          inc(Pos, Read(PropSubKind, sizeof(PropSubKind)));
          if PropKind in [tkSet, tkEnumeration{$IFDEF FPC}, tkBool{$ENDIF}] then begin
            inc(Pos, Read(Q, sizeof(Q)));
            setlength(Enums, Q);
            for K := 0 to Q -1 do with Enums[K] do begin
              inc(Pos, Read(EnumValor, sizeof(EnumValor))); //valor
              inc(Pos, Read(Tam, sizeof(Tam)));
              setlength(EnumName, Tam);
              inc(Pos, Read(EnumName[1], Tam));
            end;
          end;
        end;
      inc(J);
    end;
  end;
end;

{ TTransactionStream }
procedure TTransactionStream.WriteProperty(Prevalent : TPrevalent; Props : TProperties; Prop : word; IsSnapShot : boolean = false);
var
  I,
  PropInteger   : integer;
  PropByte      : byte;
  PropWord      : word;
  PropInt64     : Int64;
  PropString    : string;
  PropDouble    : double;
  PropCurrency  : currency;
  PropSingle    : single;
  PropExtended  : extended;
  PropObject    : TObject;
  PropPrevalent : TPrevalent;
  PropAssoc     : TAssociation;
  PropList      : word;
begin
  with Props do
    if not IsDerived[Prop] then
      case TypeProp[Prop] of
        tkLString, tkString : begin
          PropString := GetStrProp(Prevalent, Properties[Prop]);
          PropWord   := length(PropString);
          Write(PropWord, sizeof(PropWord));
          Write(PropString[1], PropWord);
        end;
        tkInteger, tkEnumeration, tkChar, tkSet, tkWChar :
          case OrdTypeProp[Prop] of
            otSLong, otULong : begin
              PropInteger := GetOrdProp(Prevalent, Properties[Prop]);
              Write(PropInteger, sizeof(PropInteger));
            end;
            otSByte, otUByte : begin
              PropByte := GetOrdProp(Prevalent, Properties[Prop]);
              Write(PropByte, sizeof(PropByte));
            end;
            otSWord, otUWord : begin
              PropWord := GetOrdProp(Prevalent, Properties[Prop]);
              Write(PropWord, sizeof(PropWord));
            end;
          end;
        {$IFDEF FPC}
		    tkBool : begin
          PropByte := GetOrdProp(Prevalent, Properties[Prop]);
          Write(PropByte, sizeof(PropByte));
		    end;
		    {$ENDIF}
        tkInt64 : begin
          PropInt64 := GetInt64Prop(Prevalent, Properties[Prop]);
          Write(PropInt64, sizeof(PropInt64));
        end;
        tkFloat :
          case FloatTypeProp[Prop] of
            ftCurr : begin
              PropCurrency := GetFloatProp(Prevalent, Properties[Prop]);
              Write(PropCurrency, sizeof(PropCurrency));
            end;
            ftDouble, ftComp : begin
              PropDouble := GetFloatProp(Prevalent, Properties[Prop]);
              Write(PropDouble, sizeof(PropDouble));
            end;
            ftSingle : begin
              PropSingle := GetFloatProp(Prevalent, Properties[Prop]);
              Write(PropSingle, sizeof(PropSingle));
            end;
            ftExtended : begin
              PropExtended := GetFloatProp(Prevalent, Properties[Prop]);
              Write(PropExtended, sizeof(PropExtended));
            end;
          end;
        tkClass :
          with Prevalence do begin
            PropObject := Prevalent.FixPropObject(GetObjectProp(Prevalent, Properties[Prop]), Prop);
            if PropObject = nil then begin
              PropList := NONELIST;
              Write(PropList, sizeof(PropList));
            end
            else
            if PropObject is TPrevalent then begin
              if Prevalence.PrevalentLists(PropObject.ClassName + 'List').IsTransient then begin
                PropList := NONELIST;
                Write(PropList, sizeof(PropList));
              end
              else begin
                PropList := TPrevalent(PropObject).FListIndex;
                Write(PropList, sizeof(PropList));
                Write(TPrevalent(PropObject).ID, sizeof(Integer));
                if TPrevalent(PropObject).ID < 1 then// Usado para referência cruzada (dual)
                  PrevalentLists(PropList).Add(TPrevalent(PropObject));
              end
            end
            else
              if IsSnapShot and (PropObject is TAssociation) then begin
                PropAssoc   := TAssociation(PropObject);
                if PropAssoc.IsTransient then
                  PropInteger := 0
                else
                  PropInteger := PropAssoc.Count;
                Write(PropInteger, sizeof(PropInteger));
                for I := 0 to PropInteger-1 do begin
                  PropPrevalent := TPrevalent(PropAssoc.InternalObjects[I]);
                  Write(PropPrevalent.ID, sizeof(Integer));
                end;
              end;
          end;
      end;
end;

procedure TTransactionStream.WritePrevalent(Prevalent: TPrevalent; IsSnapShot : boolean = false);
var
  Prop  : word;
  Props : TProperties;
begin
  Props := Prevalent.Metadata;
  for Prop := 0 to Props.PropCount-1 do
    WriteProperty(Prevalent, Props, Prop, IsSnapShot);
end;

{ TAssociation }

constructor TAssociation.Create(pOwner : TPrevalent; OwnerField : string; IsTransient : boolean = false);
var
  I, Index : integer;
  OwnerListIndex : PtrInt;
  List : TPrevalentList;
begin
  Owner     := pOwner;
  PropIndex := GetPropInfo(Owner, OwnerField).NameIndex;
  inherited Create([lpDuplicates]);
  if not IsPrimary then begin
    //procura a lista que gera dependência
    for I := 0 to Prevalence.FPrevalentLists.Count -1 do begin
      List := Prevalence.PrevalentLists(I);
      if List.GetKeyCode = Self.GetKeyCode then begin
        OwnerListIndex := Prevalence.Prevalents.IndexOf(pOwner.ClassName);
        Index := List.DependencyListAssoc.IndexOf(pointer(OwnerListIndex));
        if Index = -1 then List.DependencyListAssoc.Add(pointer(OwnerListIndex));
      end;
    end;
  end;
  if IsTransient then ListProps := ListProps + [lpTransient];
end;

function TAssociation.GetTargetProp : integer; begin
  Result := Owner.Metadata.TargetProp[PropIndex]
end;

procedure TAssociation.VerifySortVersion; begin
  if FCount < 2 then exit;
  with Owner.Metadata do begin
    if SortVersion <> ExtraRTTI[PropIndex].SortRequest then begin
      TObjectList(Self).Sort;
      SortVersion := ExtraRTTI[PropIndex].SortRequest;
    end;
{$IFDEF CHECKSORT}
    if not CheckSort then
      Trace('Correção automática. Associação desordenada após o VerifySortVersion: ' + Self.ClassName +
        ' SortVersion:' + IntToStr(SortVersion) + ' SortRequest:' + IntToStr(ExtraRTTI[PropIndex].SortRequest));
{$ENDIF}
  end;
end;

procedure TAssociation.Sort; begin
  inc(Owner.Metadata.ExtraRTTI[PropIndex].SortRequest);
end;

procedure TAssociation.AddTransObj(pPrevalent : TPrevalent; IsAdd, IsReflect : boolean);
var
  TransObj : PTransObj;
begin
  with ThreadTrans do
    if Associations = nil then begin
      Associations := TList.Create;
      Associations.Add(Self);
    end
    else
      with Associations do
        if Count > 1 then begin
          if (Items[Count-1] <> Self) and (Items[Count-2] <> Self) then Add(Self);
        end
        else
          if Items[Count-1] <> Self then Add(Self);
  if TransObjs = nil then TransObjs := TList.Create;
  New(TransObj);
  TransObj.ThreadID  := GetFCGIThreadID;
  TransObj.Prevalent := pPrevalent;
  TransObj.IsReflect := IsReflect;
  if not IsAdd then TransObj.ThreadID := -TransObj.ThreadID;
  TransObjs.Add(TransObj);
end;

function TAssociation.FindTransObj(pPrevalent : TPrevalent) : boolean;
var
  I, T : integer;
begin
  Result := false;
  if TransObjs = nil then exit;
  T := GetFCGIThreadID;
  for I := TransObjs.Count-1 downto 0 do
    with PTransObj(TransObjs[I])^ do
      if Prevalent = pPrevalent then begin
        if abs(ThreadID) = T then begin
          Result := ThreadID >= 0;
          if Result then Result := Prevalent.ThreadID >= 0;
        end
        else
          Result := ThreadID <= 0;
        exit;
      end;
  Result := true;
end;

destructor TAssociation.InternalDestroy; begin
  TransObjs.Free;
  inherited;
end;

function TAssociation.InView(pObject : TTransient) : boolean; begin
  if Prevalence.InRecover or (CallStateMachineCount > 0) then
    Result := true
  else
    Result := T_ViewPermissionSet(GetViewPerm(pObject)) <> [];
end;

function TAssociation.IsInAssociation(pPrevalent : TPrevalent) : boolean;
var
  Key : string;
  I, Index : integer;
begin
  Result := false;
  if Prevalence.InRecoverSnapShot then exit;
  if FindNear(pPrevalent, Index, false) then begin
    if IsPrimary then begin
      Result := true;
      exit;
    end;
    Key := GetKey(InternalObjects[Index]);
    for I := Index to FCount-1 do begin
      if Key <> GetKey(InternalObjects[I]) then break;
      if pPrevalent = InternalObjects[I] then begin
        Result := true;
        exit;
      end;
    end;
  end;
end;

procedure TAssociation.SetDependencyLists;
var
  Prop : word;
  PropName : string;
begin
  if ListInCheck = NONELIST then exit;
  PropName := copy(ClassName, length(Owner.ClassName) + 1, MaxInt);
  System.Delete(PropName, pos('Association', PropName), MaxInt);
  Prop := Owner.Metadata.PropByName(PropName);
  Owner.SetDependencyLists(Prop);
end;

function TAssociation.InSession(pPrevalent : TTransient) : boolean; begin
  if TransObjs = nil then
    Result := true
  else
    Result := FindTransObj(TPrevalent(pPrevalent));
end;

function TAssociation.GetCount: integer;
var
  I, T : integer;
begin
  Result := FCount;
  if TransObjs <> nil then begin
    T := GetFCGIThreadID;
    for I := 0 to TransObjs.Count-1 do
      if (PTransObj(TransObjs[I]).ThreadID <> T) and not (TerminateSession and (PTransObj(TransObjs[I]).ThreadID = -T)) then
        if not PTransObj(TransObjs[I]).IsReflect then dec(Result);
  end;
end;

function TAssociation.GetOwner: TPrevalent; begin
  Result := Owner;
end;

function TAssociation.IsAssociation : boolean; begin
  Result := true;
end;

procedure TAssociation.Clear;
var
  Obj : TTransient;
begin
  Obj := Last;
  while Obj <> nil do begin
    Delete(Obj);
    Obj := Last
  end;
end;

procedure TAssociation.Add(Prevalent: TPrevalent; InReflection : boolean = false; IsRollback : boolean = false);
var
  PrevalentPropObj : TObject;
  Target : integer;
begin
  if ThreadTrans = nil then
    raise EExtP.Create('Erro: Tentativa de inclusão em associação sem o correspondente BeginTransaction.');
  if Prevalent = nil then
    raise EExtP.CreateFmt('Erro: Tentativa de inclusão de objeto nil na associação %s.', [ClassName]);
  if Prevalence.InRecover or TerminateSession or (Count < MaxConstraint) then begin
    if Owner.FID = 0 then
      Prevalence.PrevalentLists(Owner.FListIndex).Add(Owner, false);
    with Prevalent do begin
      if FID = 0 then
        try
          Prevalence.PrevalentLists(Prevalent.FListIndex).Add(Prevalent, false);
        except
          on E : Exception do
            raise Exception.CreateFmt('Erro interno: ' + E.Message + #13'Erro em %s.Add %s: %s',
              [Owner.Metadata.Properties[PropIndex].Name, ClassName, GetFirstProperty]);
        end;
      if not InReflection then with Metadata do begin // Reflect association
        Target := GetTargetProp;
        if Target <> 0 then begin
          PrevalentPropObj := GetObjectProp(Prevalent, Properties[Target]);
          if (PrevalentPropObj is TAssociation) and Owner.ClassType.InheritsFrom(TAssociation(PrevalentPropObj).ObjectClass) then
            with TAssociation(PrevalentPropObj) do
              if TerminateSession or (Count < MaxConstraint) then
                if TerminateSession or Prevalence.InRecover then begin
                  if not IsInAssociation(Self.Owner) then TObjectList(PrevalentPropObj).Add(Self.Owner)
                end
                else
                  if IsInAssociation(Self.Owner) then begin
                    if not InSession(Self.Owner) then AddTransObj(Self.Owner, true, false);
                  end
                  else begin
                    AddTransObj(Self.Owner, true, false);
                    TObjectList(PrevalentPropObj).Add(Self.Owner);
                  end
              else
                raise EExtP.CreateFmt('Máximo de %d objetos na associação %s foi alcançado', [MaxConstraint, ClassName])
          else
            if Owner.ClassType.InheritsFrom(ClassProp[Target]) then
              with Prevalence.GetStream do begin
                DontUpdateLog := true;
                SetObjectProp(Prevalent, Properties[Target], Owner);
                DontUpdateLog := false;
              end;
        end;
        Prevalence.Log(_opAddAssociation, Self, Prevalent);
      end;
      if Prevalence.InRecover then
        inherited Add(Prevalent)
      else
        if TerminateSession then begin
          if not IsInAssociation(Prevalent) then inherited Add(Prevalent);
        end
        else
          if IsInAssociation(Prevalent) then begin
            if not Self.InSession(Prevalent) then AddTransObj(Prevalent, true, false);
          end
          else begin
            AddTransObj(Prevalent, true, false);
            inherited Add(Prevalent);
          end;
    end;
    if IsRollback or IsTransient then Sort;
  end
  else
    if inherited Find(Prevalent) = nil then
      raise EExtP.CreateFmt('Máximo de %d objetos na associação %s foi alcançado', [MaxConstraint, ClassName]);
end;

procedure TAssociation.Delete(Prevalent : TPrevalent; CascadeObject : TPrevalent = nil; InReflection : boolean = false; InRollBack : boolean = false);
var
  PrevalentPropObj : TObject;
  Target : integer;
begin
  if ThreadTrans = nil then
    raise EExtP.Create('Erro: Tentativa de Delete em associação sem o correspondente BeginTransaction.');
  if IndexOf(Prevalent) = -1 then exit;
  if TerminateSession or Prevalence.InRecover or InReflection or (Count > MinConstraint) then begin
    if CascadeObject <> Prevalent then
      with Prevalent.Metadata do begin // Reflect association
        Target := GetTargetProp;
        if Target <> 0 then begin
          PrevalentPropObj := GetObjectProp(Prevalent, Properties[Target]);
          if (PrevalentPropObj is TAssociation) and Owner.ClassType.InheritsFrom(TAssociation(PrevalentPropObj).ObjectClass) then
            with TAssociation(PrevalentPropObj) do begin
              if TerminateSession or Prevalence.InRecover then begin
                if IsInAssociation(Self.Owner) then TObjectList(PrevalentPropObj).Delete(Self.Owner)
              end
              else begin
                if IsInAssociation(Self.Owner) then begin
                  if InSession(Self.Owner) then AddTransObj(Self.Owner, false, true);
                end
                else
                  AddTransObj(Self.Owner, false, true);
              end
          end
          else if Owner.ClassType.InheritsFrom(ClassProp[Target]) then
            with Prevalence.GetStream do begin
              DontUpdateLog := true;
              if InConstraints(Target, COMPOSITE) then begin
                Prevalent.DeleteInReflection
              end
              else begin
                if (Prevalent.FThreadID >= 0) and (PrevalentPropObj <> nil) and not InReflection and not Prevalence.InRecover then
                  with TPrevalent(PrevalentPropObj) do
                    if not InRollback and (Metadata.InConstraints(Target, COMPOSITE) or Prevalent.Metadata.InConstraints(Target, NOTNULL)) then
                      Prevalent.DeleteInReflection
                    else
                      SetTargetToNil(Prevalent, Target, CascadeObject, true);
              end;
              DontUpdateLog := false;
            end;
        end;
      end;
    if TerminateSession or Prevalence.InRecover then begin
      if IsInAssociation(Prevalent) then inherited Delete(Prevalent)
    end
    else
      if IsInAssociation(Prevalent) then begin
        if InSession(Prevalent) then AddTransObj(Prevalent, false, InReflection);
      end
      else
        AddTransObj(Prevalent, false, InReflection);
    if (CascadeObject = nil) then Prevalence.Log(_opDeleteAssociation, Self, Prevalent);
  end
  else
    if TerminateSession then
      raise EExtP.CreateFmt('Mínimo de %d objetos na associação %s foi alcançado', [MinConstraint, ClassName])
end;

class function TAssociation.MaxConstraint: integer; begin
  Result := MAXINT;
end;

class function TAssociation.MinConstraint: integer; begin
  Result := 0
end;

class function TPrevalent.CallMethod(pMethodName : string; Params : TMethodParams; const SelfParam: pointer) : variant;
type
  TCallProcedure = procedure(Params : TMethodParams) of object;
  TCallFunction  = function (Params : TMethodParams) : variant of object;
var
  PMethod    : TMethod;
  MetAddress : pointer;
  Method     : PMethodRTTI;
const
  PrevMetAddress : pointer = pointer(1);
  PrevMet : PMethodRTTI = nil;
begin
  if Assigned(OnSetRunningMethod) then OnSetRunningMethod(ClassName + '.' + pMethodName);
  try
    MetAddress := MethodAddress(pMethodName + '_Int');
    if MetAddress = PrevMetAddress then
      Method := PrevMet
    else begin
      if MetAddress = nil then raise Exception.CreateFmt('Internal Error: Missing operation: %s.%s', [copy(ClassName, 2, 100), pMethodName]);
      Method  := Prevalence.Metadata(ClassName).MethodByName(pMethodName);
      PrevMet := Method;
      PrevMetAddress := MetAddress;
    end;
    if length(Method.Params) <> length(Params) then
      raise Exception.CreateFmt('Internal Error: Parameters mismatch in operation: %s.%s', [copy(ClassName, 2, 100), pMethodName]);
    try
      PMethod.Code := MetAddress;
      PMethod.Data := SelfParam;
      if Method.TypeInfo <> nil then
        Result := TCallFunction(PMethod)(Params)
      else begin
        VarClear(Result);
        TCallProcedure(PMethod)(Params);
      end;
    except
      on EAbort do raise;
      on E : Exception do raise Exception.CreateFmt('%s'^M^J'Runtime error on executing operation: %s.%s', [E.Message, copy(ClassName, 2, 100), pMethodName]);
    end;
  finally
    if Assigned(OnSetRunningMethod) then OnSetRunningMethod('');
  end;
end;

function TPrevalent.CallMethod(pMethodName: string; Params: TMethodParams) : variant; begin
  Result := CallMethod(pMethodName, Params, Self);
end;

{ TTransaction }

function TTransaction.BeginUpdate(Prevalent: TPrevalent) : TUpdatedPrevalent; begin
  if Updates = nil then Updates := TUpdateList.Create;
  Result := TUpdatedPrevalent(Updates.Find(PtrInt(Prevalent)));
  if Result = nil then begin
    Result := TUpdatedPrevalent.Create;
    with Result do begin
      FThreadID:= GetFCGIThreadID;
      OldImage := Prevalent;
      NewImage := TPrevalentClass(Prevalent.ClassType).Create(false);
      Prevalent.DuplicateObject(NewImage);
      LastImage := TPrevalentClass(Prevalent.ClassType).Create(false);
      Prevalent.DuplicateObject(LastImage);
    end;
    Updates.Add(Result);
  end
  else
    if not TerminateSession then Result.NewImage.DuplicateObject(Result.LastImage);
end;

destructor TTransaction.Destroy;
var
  I : integer;
begin
  Stream.UpdateObject := nil;
  Stream.Clear;
  FreeAndNil(Stream);
  if GetAuditory then FreeAndNil(AuditStream);
  try
    if Updates <> nil then begin
      for I := Updates.Count-1 downto 0 do begin
        TUpdatedPrevalent(Updates.InternalObjects[I]).NewImage.SetAssociationToNil;
        try TUpdatedPrevalent(Updates.InternalObjects[I]).NewImage.InternalFree; except end;
        TUpdatedPrevalent(Updates.InternalObjects[I]).LastImage.SetAssociationToNil;
        try TUpdatedPrevalent(Updates.InternalObjects[I]).LastImage.InternalFree; except end;
        TUpdatedPrevalent(Updates.InternalObjects[I]).OldImage := nil;
        try TUpdatedPrevalent(Updates.InternalObjects[I]).InternalFree; except end;
        Updates.Delete(I);
      end;
      Updates.Clear;
      Updates.InternalFree;
      Updates := nil;
    end;
    if Creates <> nil then begin
      for I := Creates.Count-1 downto 0 do begin
        if TPrevalentList(Prevalence.FPrevalentLists.Objects[TPrevalent(Creates[I]).FListIndex]).Find(TTransient(Creates[I]).ID) = nil then begin
          Trace(inttoStr(I) + ' ' + TTransient(Creates[I]).ClassName);
          Trace(inttoStr(I) + ' ID ' + inttostr(TTransient(Creates[I]).ID));
          Trace('Objeto criado e não inserido. Classe: ' + TTransient(Creates[I]).ClassName);
          try TPrevalent(Creates[I]).InternalFree; except end;
        end;
      end;
      Creates.Clear;
      FreeAndNil(Creates);
    end;
    if InsDels <> nil then begin
      InsDels.Clear;
      FreeAndNil(InsDels);
    end;
    if Associations <> nil then begin
      Associations.Clear;
      FreeAndNil(Associations);
    end;
  except
    on E : Exception do Trace('Erro no Free da Transaction: %s', [E.Message]);
  end;
  inherited;
end;

procedure TTransaction.AddInsDels(Prevalent : TPrevalent);
var
  I : integer;
  ObjInsDels : PInsDels;
begin
  if TerminateSession then exit;
  if InsDels = nil then
    InsDels := TList.Create
  else
    for I := 0 to InsDels.Count - 1 do
      if PInsDels(InsDels[I]).Obj = Prevalent then begin
        PInsDels(InsDels[I]).IsAdd := Prevalent.ThreadID >= 0;
        exit;
      end;
  New(ObjInsDels);
  ObjInsDels.IsAdd := Prevalent.ThreadID >= 0;
  ObjInsDels.Obj   := Prevalent;
  InsDels.Add(ObjInsDels);
end;

procedure TPrevalent.DelCreates;
var
  I : integer;
begin
  if Prevalence.InRecover or (ThreadTrans = nil) then exit;
  with ThreadTrans do begin
    for I := Creates.Count-1 downto 0 do
      if Creates[I] = Self then begin
         Creates.Delete(I);
         exit;
      end;
  end;
end;

procedure TPrevalent.InternalFree; begin
  DelCreates;
  inherited;
end;

{ TUpdatedPrevalent }

function TUpdatedPrevalent.ByOldImage: PtrInt; begin
  Result := PtrInt(OldImage)
end;

procedure BeginTransaction(pIsolationLevel : TIsolationLevel = ReadCommited); begin
  Prevalence.BeginTransaction(pIsolationLevel)
end;

procedure EndTransaction(Force : boolean = false); begin
  Prevalence.EndTransaction(Force)
end;

procedure RollBack; begin
  Prevalence.RollBack
end;

function Recover : boolean; begin
  Result := true;
  try
    Prevalence.Recover
  except
    Result := false
  end;
end;

function SnapShot : boolean; begin
  Result := true;
  try
    Prevalence.SnapShot
  except
    Result := false
  end;
end;

procedure FixReference(var Reference : TTransient; TransientClass : TTransientClass); begin
  try
    if not Prevalence.IsInRecover and ((Reference = nil) or (Reference is TransientClass)) then; // só testa
  except
    Reference := nil
  end
end;

{ TUpdateList }

constructor TUpdateList.Create; begin
  inherited Create([lpSorted]);
end;

class function TUpdateList.GetKeyCode: pointer;  begin
  Result := @TUpdatedPrevalent.ByOldImage
end;

class function TUpdateList.GetObjectClass: TTransientClass;  begin
  Result := TUpdatedPrevalent
end;

end.
