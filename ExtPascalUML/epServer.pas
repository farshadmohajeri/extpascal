{$I epDirectives.inc}

unit epServer;

interface

uses
  SysUtils, Classes, epCommon, epObjectList, epThread;

  procedure CarregaSeguranca;
  procedure EvolveSeguranca;
  function Get_Classe(Nome : string) : TObject;
  function Get_Propriedade(Classe : TObject; Nome : string) : TObject; overload;
  function Get_Propriedade(Classe : TObject; pID : integer) : TObject; overload;
  procedure Add_Propriedade(Classe, Propriedade : TObject);
  function Get_Metodo(Classe : TObject; Nome : string) : TObject; overload;
  function Get_Metodo(Classe : TObject; pID : integer) : TObject; overload;
  procedure Add_Metodo(Classe, Metodo : TObject);
  procedure InicializaClassesOcupacao;
  function CalculaOcupacaoMemoriaLista(pObj : TObject) : double;
  function GetProfiles(UserName, LogonSrv : string) : String;
  function GetClassePerm(pPackage, pClasse : String) : T_ClassPermissionSet;
  function GetPropriedadePerm(pPackage, pClasse, pPropriedade : String) : T_PropPermission;
  function GetMetodoPerm(pPackage, pClasse, pMetodo : String; pGrupo : string = '') : boolean;
  procedure GetMetodoProfiles(pMetodo : TObject; var Profiles : TObjectList);
  function GetViewPerm(pObject : TObject) : T_ViewPermissionSet;
  function AuthenticateUser(Thr : TepThread; Key : String) : Boolean;
  procedure AssociaServicoDesigner(pNome : string);
  procedure DesassociaServicoDesigner(pNome : string; DoDelete : boolean = true);
  procedure Login(Thr : TepThread);
  procedure LogOff(Thr : TepThread);
  procedure KillSession(Thr : TepThread);
  function GetSecurity : boolean;
  function GetEnvironment : string;
  function GetShowID : boolean;
  function GetShowAll : boolean;
  function CreateExport(pModelNome : string; pClasse : TObject) : TObject;
  function CreateImportDelimitado(pModelNome : string; pClasse : TObject) : TObject;
  function CreateImportPosicional(pModelNome : string; pClasse : TObject) : TObject;
  procedure Import(pObject : TObject); overload;
  procedure Import(Nome : string); overload;
  procedure Export(pObject : TObject); overload;
  procedure Export(Nome : string); overload;
  function GetAuditory : boolean;
  procedure AtualizaControleArquivo(pFileName : string; pData : TDateTime; pStatus : T_Aud_ArquivoStatus);
  procedure GravaAuditoria(pOperation : T_Operation; pObj : TObject = nil);
  procedure CorrigeMapeamentoAuditLog(pAuditFilePosition, pLogFilePosition : integer);
  procedure GerarPesquisaAuditoria(pCri : TObject);
{$IFNDEF AUDITORIA}
  function RecuperaObjetoAuditoria(pObj : TObject) : TObject;
{$ELSE}
  procedure RecuperaObjetoAuditoria(pObj : TObject);
{$ENDIF}

implementation

uses
  StrUtils, Typinfo, Variants, DateUtils, {$IFDEF MSWINDOWS}WinSvc, Services,{$ENDIF}
  epProperties, epModel, epPrevalence, epUtils;
  
type
  TUnPObjectList = class(TObjectList);
  TUnPPrevalent = class(TPrevalent);
  TUnPTransaction = class(TTransaction);
  TUnPPrevalentStream = class(TPrevalentStream);

function GetEnvironment : string;
const
  Environment : string = '';
begin
  if Environment = '' then Environment := GetIniParameter('Server', 'Environment', 'Development');
  Result := Environment;
end;

function GetShowID : boolean;
const
  ShowID : boolean = false;
  Cached : boolean = false;
begin
  if not Cached then ShowID := GetIniParameter('Browser', 'ShowID', 'FALSE') = 'TRUE';
  Result := ShowID;
end;

function GetShowAll : boolean;
const
  ShowAll : boolean = false;
  Cached  : boolean = false;
begin
  if not Cached then ShowAll := GetIniParameter('Browser', 'ShowAll', 'FALSE') = 'TRUE';
  Result := ShowAll;
end;

const
  GRPSECADM = 'epADM';

function Get_Classe(Nome : string) : TObject; begin
  Result := TObject(_ClassePorNomeList.Find(Nome));
end;

function Get_Propriedade(Classe : TObject; Nome : string) : TObject;
var
  fProp : T_Propriedade;
begin
  Result := nil;
  if Classe = nil then exit;
  fProp := T_Classe(Classe).Propriedades.First;
  while Assigned(fProp) do begin
    if fProp.Nome = Nome then begin
      Result := fProp;
      exit;
    end;
    T_Classe(Classe).Propriedades.Next(fProp);
  end;
end;

function Get_Propriedade(Classe : TObject; pID : integer) : TObject;
var
  fProp : T_Propriedade;
begin
  Result := nil;
  if Classe = nil then exit;
  fProp := T_Classe(Classe).Propriedades.First;
  while Assigned(fProp) do begin
    if fProp.ID = pID then begin
      Result := fProp;
      exit;
    end;
    T_Classe(Classe).Propriedades.Next(fProp);
  end;
end;

procedure Add_Propriedade(Classe, Propriedade : TObject); begin
  T_Classe(Classe).Propriedades.Add(T_Propriedade(Propriedade));
end;

function Get_Metodo(Classe : TObject; Nome : string) : TObject;
var
  fMeth : T_Metodo;
begin
  Result := nil;
  if Classe = nil then exit;
  fMeth := T_Classe(Classe).Metodos.First;
  while Assigned(fMeth) do begin
    if fMeth.Nome = Nome then begin
      Result := fMeth;
      exit;
    end;
    T_Classe(Classe).Metodos.Next(fMeth);
  end;
end;

function Get_Metodo(Classe : TObject; pID : integer) : TObject;
var
  fMeth : T_Metodo;
 begin
  Result := nil;
  if Classe = nil then exit;
  fMeth := T_Classe(Classe).Metodos.First;
  while Assigned(fMeth) do begin
    if fMeth.ID = pID then begin
      Result := fMeth;
      exit;
    end;
    T_Classe(Classe).Metodos.Next(fMeth);
  end;
end;

procedure Add_Metodo(Classe, Metodo : TObject); begin
  T_Classe(Classe).Metodos.Add(T_Metodo(Metodo));
end;

{$IFDEF NEWXSC}
procedure CarregaSeguranca; begin
end;

procedure EvolveSeguranca;
var
  Index : integer;
  fCla : T_Classe;
  fPro : T_Propriedade;
  fPkg : T_Package;
  fMet : T_Metodo;
  fEvolveData : TEvolveData;

  procedure VerificaConstraints;
  var
    I : integer;
  begin
    for I := 0 to Prevalence.FPrevalentLists.Count - 1 do
      with Prevalence.PrevalentLists(I) do
        if IsPrimary and not IsAbstract then
          with Prevalence, Metadata(ObjectClass.ClassName) do
            if Package = '_Security' then Prevalence.CheckModel(true, ObjectClass);
  end;

  procedure LimpaListas; begin
    //package
    fPkg := _PackageList.Last;
    while Assigned(fPkg) do begin
      if not fPkg.Atualizado then fPkg.Delete;
      _PackageList.Prior(fPkg);
    end;
    //classe
    fCla := _ClasseList.Last;
    while Assigned(fCla) do begin
      if not fCla.Atualizado then fCla.Delete;
      _ClasseList.Prior(fCla);
    end;
    //propriedade
    fPro := _PropriedadeList.Last;
    while Assigned(fPro) do begin
      if not fPro.Atualizado then fPro.Delete;
      _PropriedadeList.Prior(fPro);
    end;
    //metodo
    fMet := _MetodoList.Last;
    while Assigned(fMet) do begin
      if not fMet.Atualizado then fMet.Delete;
      _MetodoList.Prior(fMet);
    end;
  end;

  procedure LoadInternalEvolveData;
  var
    I, J : integer;

    function ClassPosAndIDInEvolveData(pClassName : string; var pID : integer) : integer;
    var
      K : integer;
    begin
      for K := 0 to length(Prevalence.EvolveData)-1 do with Prevalence.EvolveData[K] do
        if ClassName = pClassName then begin
          Result := K;
          pID := ClassID;
          exit;
        end;
      pID := -1;
      Result := -1;
    end;

    function PropIDInEvolveData(pClassName, pPropName : string) : integer;
    var
      K, P, Dummy : integer;
    begin
      P := ClassPosAndIDInEvolveData(pClassName, Dummy);
      if P <> -1 then
        with Prevalence.EvolveData[P] do begin
          for K := 0 to length(Props)-1 do with Props[K] do
            if Name = pPropName then begin
              Result := ID;
              exit;
            end;
        end;
      Result := -1;
    end;

    function MethIDInEvolveData(pClassName, pMethodName : string) : integer;
    var
      K, P, Dummy : integer;
    begin
      P := ClassPosAndIDInEvolveData(pClassName, Dummy);
      if P <> -1 then
        with Prevalence.EvolveData[P] do
          for K := 0 to length(Methods)-1 do with Methods[K] do
            if Name = pMethodName then begin
              Result := ID;
              exit;
            end;
      Result := -1;
    end;

    procedure LoadProps(pClassName : string);
    var
      P, Pos : integer;
    begin
      Pos := -1;
      with Prevalence.Metadata(pClassName), fEvolveData[J] do
        for P := 0 to PropCount-1 do begin
          if (P = 0) and (fEvolveData[J].Package = '') then fEvolveData[J].Package := Prevalence.Metadata(pClassName).Package;
          setlength(Props, length(Props)+1);
          inc(Pos);
          Props[Pos].Name := Properties[P].Name;
          Props[Pos].ID := PropIDInEvolveData(pClassName, Properties[P].Name);
        end;
    end;

    procedure LoadMeths(pClassName : string);
    var
      P, Pos : integer;
    begin
      Pos := -1;
      with Prevalence.Metadata(pClassName), fEvolveData[J] do
        for P := 0 to MethodCount-1 do begin
          setlength(Methods, length(Methods)+1);
          inc(Pos);
          Methods[Pos].Name := Method[P].Name;
          Methods[Pos].ID := MethIDInEvolveData(pClassName, Method[P].Name);
        end;
    end;

  begin
    J := -1;
    for I := 0 to Prevalence.FPrevalentLists.Count - 1 do
      with Prevalence.PrevalentLists(I) do
        if IsPrimary and not IsAbstract then
          with Prevalence, Metadata(ObjectClass.ClassName) do begin
            setLength(fEvolveData, length(fEvolveData)+1);
            inc(J);
            with fEvolveData[J] do begin
              ClassName := ObjectClass.ClassName;
              ClassPosAndIDInEvolveData(ClassName, ClassID);
              LoadProps(ClassName);
              LoadMeths(ClassName);
            end;
          end;
  end;

  procedure InternalEvolveSecurity(pEvolveData : TEvolveData);
  var
    I, J  : integer;

    function GetPackageNome(pPackage : string) : string; begin
      if pPackage[1] = '_' then
        Result := 'ep'
      else
        Result := pPackage;
    end;

  begin
    for I := 0 to high(pEvolveData) do with pEvolveData[I] do begin
      if ClassName = '' then continue;
      if Package[1] = '_' then
        if CaseOf(Package, ['_Security', '_Auditory']) <> -1 then continue;
      if ClassName = '*Removida' then continue;
      //Classe
      Index := Prevalence.Prevalents.IndexOf(ClassName);
      if ClassID = -1 then
        fCla := _ClassePorIndexList.Find(word(Index))
      else
        fCla := _ClasseList.Find(ClassID);
      if not Assigned(fCla) then begin
        fCla := T_Classe.Create;
        fCla.Add;
      end;
      fCla.Index := Index;
      fCla.Atualizado := true;
      //package
      fPkg := _PackagePorNomeList.Find(GetPackageNome(Package));
      if fPkg = nil then begin
        fPkg := T_Package.Create;
        fPkg.Nome := GetPackageNome(Package);
        fPkg.Add;
      end;
      fPkg.Atualizado := true;
      fPkg.Classes.Add(fCla);
      //Propriedade
      for J:= 0 to length(Props)-1 do with Props[J] do begin
        if ID = -1 then
          fPro := T_Propriedade(Get_Propriedade(fCla, Name))
        else
          fPro := T_Propriedade(Get_Propriedade(fCla, ID));
        Index := Prevalence.Metadata(ClassName).PropByName(Name);
        if Index = -1 then continue;
        if fPro = nil then begin
          fPro := T_Propriedade.Create;
          fPro.Add;
        end;
        fPro.Index := Index;
        fPro.Atualizado := true;
        fPro.Classe := fCla;
      end;
      //Metodo
      for J:= 0 to length(Methods)-1 do with Methods[J] do begin
        if ID = -1 then
          fMet := T_Metodo(Get_Metodo(fCla, Name))
        else
          fMet := T_Metodo(Get_Metodo(fCla, ID));
        Index := Prevalence.Metadata(ClassName).MethodIndexByName(Name);
        if Index = -1 then continue;
        if fMet = nil then begin
          fMet := T_Metodo.Create;
          fMet.Add;
        end;
        fMet.Index := Index;
        fMet.Atualizado := true;
        fMet.Classe := fCla;
      end;
    end;
  end;

begin
  BeginTransaction;
  LoadInternalEvolveData;
  InternalEvolveSecurity(fEvolveData);
  LimpaListas;
  VerificaConstraints;
  EndTransaction;
  setlength(Prevalence.EvolveData, 0);
  setlength(fEvolveData, 0);
end;
{$ELSE}
procedure EvolveSeguranca; begin
end;

procedure CarregaSeguranca;
var
  Pkg      : T_Package;
  Clas     : T_Classe;
  Props    : TProperties;
  I        : integer;
  NewObj   : boolean;
  ClasNome, AliasNome, Package : string;

  procedure CarregaPackage; begin
    Pkg := _PackagePorNomeList.Find(Package);
    if Pkg = nil then begin
      Pkg := T_Package.Create;
      Pkg.Nome := Package;
      _PackageList.Add(Pkg);
    end;
  end;

  procedure CarregaPropriedades;
  var
    Prop : T_Propriedade;
    J    : integer;
    PropNome : string;
  begin
    for J:=0 to Props.PropCount - 1 do begin
      PropNome := Props.Properties[J].Name;
      Prop := T_Propriedade(Get_Propriedade(Clas,PropNome));
      BeginTransaction;
      if Prop = nil then begin
        Prop := T_Propriedade.Create;
        NewObj := true;
      end
      else
        NewObj := false;
      Prop.Nome := PropNome;
      Prop.ReadOnly := ((Props.InConstraints(J,READONLY)) or (PropNome = 'ID'));
      if Props.TypeProp[J] = tkClass then
        if Props.IsAssociation[J] then
          Prop.Tipo := _ptAssociacao
        else
          Prop.Tipo := _ptReferencia
      else
        Prop.Tipo := _ptAtributo;
      Prop.Posicao := J + 1;
      if (J > 0) and (Props.ExtraRTTI[J] <> nil) and (Props.ExtraRTTI[J].Alias <> '') and (lowerCase(Props.ExtraRTTI[J].Alias) <> 'hidden') then
        AliasNome := Props.ExtraRTTI[J].Alias
      else
        AliasNome := '';
      if (AliasNome <> PropNome) and (AliasNome <> '') then
        Prop.AliasNome := AliasNome
      else
        Prop.AliasNome := PropNome;
      if Prop.Classe <> Clas then Prop.Classe := Clas;
      if NewObj then Prop.Add;
      EndTransaction;
    end;
  end;

  procedure CarregaMetodos;
  var
    Met : T_Metodo;
    J   : integer;
    MetNome, AliasNome : string;
  begin
    for J := 0 to Props.MethodCount - 1 do begin
      MetNome := Props.Method[J].Name;
      Met := T_Metodo(Get_Metodo(Clas, MetNome));
      BeginTransaction;
      if Met = nil then begin
        Met := T_Metodo.Create;
        NewObj := true;
      end
      else
        NewObj := false;
      Met.Nome := MetNome;
      if (Props.Method[J] <> nil)  and (Props.Method[J].Alias <> '') then
        AliasNome := Props.Method[J].Alias
      else
        AliasNome := MetNome;
      Met.AliasNome := AliasNome;
      Met.Tipo      := Props.Method[J].Stereotype;
      if Met.Classe <> Clas then Met.Classe := Clas;
      if NewObj then Met.Add;
      EndTransaction;
    end;
  end;

begin
  if GetSecurity then
    try
      with Prevalence do begin
        for I:=0 to Prevalents.Count - 1 do begin
          Props := Metadata(Prevalents[I]);
          if CaseOf(Props.Package, ['_Security', '_Auditory']) <> -1 then continue;
          if Props.Package[1] = '_' then
            Package := 'ep'
          else
            Package := Props.Package;
          BeginTransaction;
          CarregaPackage;
          EndTransaction;
            ClasNome := Prevalents[I];
          Delete(ClasNome, 1, 1);
          Clas := _ClassePorNomeList.Find(ClasNome);
          BeginTransaction;
          if Clas = nil then begin
            Clas := T_Classe.Create;
            NewObj := true;
          end
          else
            NewObj := false;
          Clas.Nome := ClasNome;
          AliasNome := Props.ExtraRTTI[0].Alias;
          if (AliasNome = '') or (lowerCase(AliasNome) = 'hidden') then
            Clas.AliasNome := ClasNome
          else
            Clas.AliasNome := AliasNome;
          if Clas.Package <> Pkg then Clas.Package := Pkg;
          if NewObj then Clas.Add;
          EndTransaction;
          CarregaPropriedades;
          CarregaMetodos;
        end;
      end;
    except
      Rollback;
    end;
end;
{$ENDIF}

//-------------Inicio da Recuperação das permissões
function GetSecurity : boolean;
const
  Security       : boolean = false;
  CachedSecurity : boolean = false;
begin // AUTH_TYPE <> ''
  if not CachedSecurity then begin
    CachedSecurity := true;
    Security := UpperCase(GetIniParameter('Server', 'Security', 'false')) = 'TRUE';
  end;
  Result := Security;
end;

function Get_Grupo(Grupo : string = '') : T_Grupo;
var
 Dom : T_Dominio;
begin
  Result := nil;
  Dom := _DominioPorNomeList.Find(Browser.UserInfo.UserDomain);
  if Dom = nil then
    if lowerCase(GetEnvironment) = 'development' then Dom := _DominioPorNomeList.First;
  if Dom = nil then exit;
  Result := Dom.Grupos.Find(ifthen(Grupo = '', Browser.UserInfo.Profile, Grupo));
  if not ((Result <> nil) and (Result.Ativo) and (Result.ThreadID = 0)) then Result := nil;
end;

function FiltraUserGroups(Grps : string) : string;
var
  P   : integer;
  Str : string;
begin
  Result := '';
  P := 1;
  while P <= length(Grps) do begin
    Str := ExtractFromStr(Grps, P);
    if Str <> GRPSECADM then begin
      if (pos('ep', Str) = 1) and (Get_Grupo(Str) <> nil) then AddToStr(Result, Str);
    end
    else
      AddToStr(Result, Str);
  end;
  if (lowerCase(GetEnvironment) = 'production') and (pos(GRPSECADM, Result) <> 0) then Result := GRPSECADM;
end;

function GetProfiles(UserName, LogonSrv : string) : String;
var
  Profiles: string;
  I: integer;
  Profile: T_Grupo;
  List: TStringList;
  ProfileName: string;
begin
  if GetSecurity then begin
    Profiles := FiltraUserGroups(GetUserGroups(UserName, LogonSrv));
    // Get Aliases
    I := 1;
    List := TStringList.Create;
    try
      while I <= Length(Profiles) do begin
        ProfileName := ExtractFromStr(Profiles, I);
        Profile := _GrupoPorNomeList.Find(ProfileName);
        if Assigned(Profile) then
          List.Add(Profile.Identificador)
        else //if ProfileName = GRPSECADM then
          List.Add(ProfileName);
     end;
     List.Sort;
     Result := '';
     for I := 0 to List.Count - 1 do
       AddToStr(Result, List[I]);
    finally
      List.Free;
    end;
  end
  else
    Result := '*';
end;

function Get_PermissaoPackage(pPackage : string; Grupo : string = '') : T_PermissaoPackage;
var
  Grp : T_Grupo;
  PPkg : T_PermissaoPackage;
  Loop : boolean;
begin
  Result := nil;
  Grp := Get_Grupo(Grupo);
  if Grp = nil then exit;
  PPkg := Grp.Packages.First;
  Loop := (PPkg <> nil);
  while Loop do begin
    if (lowerCase(PPkg.Package.Nome) = lowerCase(pPackage)) and (PPkg.ThreadID = 0) then begin
      Result := PPkg;
      exit;
    end;
    Loop := Grp.Packages.Next(PPkg);
  end;
end;

function Get_PermissaoClasse(PerPkg : T_PermissaoPackage; pClasse : string) : T_PermissaoClasse;
var
  PCla : T_PermissaoClasse;
  Loop : boolean;
begin
  Result := nil;
  PCla := PerPkg.PermissoesClasses.First;
  Loop := (PCla <> nil);
  while Loop do begin
    if (lowerCase(PCla.Classe.Nome) = lowerCase(pClasse)) and (PCla.ThreadID = 0) then begin
      Result := PCla;
      exit;
    end;
    Loop := PerPkg.PermissoesClasses.Next(PCla);
  end;
end;

function Get_PermissaoPropriedade(PerPkg : T_PermissaoPackage; Cla : T_Classe; Pro : T_Propriedade) : T_PermissaoPropriedade;
var
  PPro : T_PermissaoPropriedade;
  Loop : boolean;
begin
  Result := nil;
  PPro := PerPkg.PermissoesPropriedades.First;
  Loop := (PPro <> nil);
  while Loop do begin
    if (lowerCase(PPro.Propriedade.Nome) = lowerCase(Pro.Nome)) and (Pro.ThreadID = 0) then begin
      Result := PPro;
      exit;
    end;
    Loop := PerPkg.PermissoesPropriedades.Next(PPro);
  end;
end;

function Get_PermissaoMetodo(PerPkg : T_PermissaoPackage; Cla : T_Classe; Met : T_Metodo) : T_PermissaoMetodo;
var
  PMet : T_PermissaoMetodo;
  Loop : boolean;
begin
  Result := nil;
  PMet := PerPkg.PermissoesMetodos.First;
  Loop := (PMet <> nil);
  while Loop do begin
    if (lowerCase(PMet.Metodo.Nome) = lowerCase(Met.Nome)) and (lowerCase(PMet.Metodo.Classe.Nome) = lowerCase(Cla.Nome)) and (PMet.ThreadID = 0) then begin
      Result := PMet;
      exit;
    end;
    Loop := PerPkg.PermissoesMetodos.Next(PMet);
  end;
end;

function GetClassePerm(pPackage, pClasse : String) : T_ClassPermissionSet;
var
  PerPkg    : T_PermissaoPackage;
  PerCla    : T_PermissaoClasse;
begin
  Result:=[_cpShow, _cpInsert, _cpModify, _cpDelete];
  if pPackage[1] = '_' then begin
{$IFNDEF DEBUG}
    if pPackage = '_Security' then begin
      if Browser.UserInfo.Profile <> GRPSECADM then Result := []
      else
      if CaseOf(pClasse, ['_Package', '_Classe', '_Propriedade', '_Metodo']) <> -1 then Result:= [_cpShow];
      exit;
    end;
    if pPackage = '_Auditory' then begin
      Result:=[];
      exit;
    end;
    if pPackage = '_Administration' then; //verifica permissões
    if pPackage = '_Workflow'  then begin
      if SameText(pClasse, '_Task') then
        Result:=[_cpInsert, _cpModify, _cpDelete]
      else
        Result:=[];
      exit;
    end;
{$ELSE}
    exit;
{$ENDIF}
  end;
  if not GetSecurity then exit;
  Result := [];
  PerPkg := Get_PermissaoPackage(pPackage);
  if PerPkg <> nil then begin
    Result := T_ClassPermissionSet(PerPkg.PermissaoClasse);
    PerCla := Get_PermissaoClasse(PerPkg, pClasse);
    if PerCla <> nil then Result := T_ClassPermissionSet(PerCla.Permissoes)
  end;
end;

function GetPropriedadePerm(pPackage, pClasse, pPropriedade : String) : T_PropPermission;
var
  PerPkg  : T_PermissaoPackage;
  PerPro  : T_PermissaoPropriedade;
  Cla     : T_Classe;
  Pro     : T_Propriedade;
begin
  Result := _ppModify;
  if pPackage[1] = '_' then begin
{$IFNDEF DEBUG}
    if pPackage = '_Security' then begin
      if Browser.UserInfo.Profile <> GRPSECADM then Result := _ppHide;
      exit;
    end;
    if pPackage = '_Auditory' then begin
      Result := _ppHide;
      exit;
    end;
    if pPackage = '_Administration' then; //verifica permissões
    if pPackage = '_Workflow' then exit;
{$ELSE}
    exit;
{$ENDIF}
  end;
  if not GetSecurity then exit;
  Result := _ppShow;
  PerPkg := Get_PermissaoPackage(pPackage);
  if PerPkg = nil then exit;
  Cla := _ClassePorNomeList.Find(pClasse);
  if Cla = nil then exit;
{$IFDEF NEWXSC}
  Pro := T_Propriedade(Get_Propriedade(Cla, pPropriedade));
{$ELSE}
  Pro := Cla.Propriedades.Find(pPropriedade);
{$ENDIF}
  if Pro = nil then exit;
  if PerPkg <> nil then begin
    Result := T_PropPermission(PerPkg.PermissaoPropriedade);
    PerPro := Get_PermissaoPropriedade(PerPkg, Cla, Pro);
    if PerPro <> nil then Result := T_PropPermission(PerPro.Permissao);
  end;
end;

function GetMetodoPerm(pPackage, pClasse, pMetodo : String; pGrupo : string = '') : boolean;
var
  Package, Grupo : string;
  PerPkg  : T_PermissaoPackage;
  PerMet  : T_PermissaoMetodo;
  Cla     : T_Classe;
  Met     : T_Metodo;
begin
  Result := true;
  if pMetodo = 'GetDelegateUsers' then exit;
  if pGrupo <> '' then
    Grupo := pGrupo
  else
    Grupo := Browser.UserInfo.Profile;
  if pPackage[1] = '_' then begin
{$IFNDEF DEBUG}
    if pPackage = '_Security' then begin
      if Grupo <> GRPSECADM then Result := false;
      exit;
    end;
    if pPackage = '_Auditory' then begin
      Result:=false;
      exit;
    end;
    if pPackage = '_Administration' then Package := 'ep'; //verifica permissões
    if pPackage = '_Workflows' then Package := 'ep'; //verifica permissões
{$ELSE}
    exit;
{$ENDIF}
  end
  else
    Package := pPackage;
  if not GetSecurity then exit;
  Result := false;
  PerPkg := Get_PermissaoPackage(Package, Grupo);
  if PerPkg = nil then exit;
  Cla := _ClassePorNomeList.Find(pClasse);
  if Cla = nil then exit;
{$IFDEF NEWXSC}
  Met := T_Metodo(Get_Metodo(Cla, pMetodo));
{$ELSE}
  Met := Cla.Metodos.Find(pMetodo);
{$ENDIF}
  if (Met = nil) or Met.Bloqueado then exit;
  if PerPkg <> nil then begin
    Result := PerPkg.PermissaoMetodo;
    PerMet := Get_PermissaoMetodo(PerPkg, Cla, Met);
    if PerMet <> nil then Result := PerMet.Permissao;
  end;
end;

procedure GetMetodoProfiles(pMetodo : TObject; var Profiles : TObjectList);
var
  Package, Classe, Metodo : string;
  Grp : T_Grupo;
  Met : T_Metodo;
begin
  try
    Met := T_Metodo(pMetodo);
    Package := Met.Classe.Package.Nome;
    Classe := Met.Classe.Nome;
    Metodo := Met.Nome;
    Grp := _GrupoPorNomeList.First;
    while Grp <> nil do begin
      if GetMetodoPerm(Package, Classe, Metodo, Grp.Nome) then ProFiles.Add(Grp);
      _GrupoPorNomeList.Next(Grp);
    end;
  except
  end;
end;

function MethodFillParams(pClassName, pMethodName, pParams : string) : TMethodParams;
var
  Metadata  : TProperties;
  vMethod   : PMethodRTTI;
  I, Pos, AuxInt: integer;
  S : string;
  V : variant;
  I64 : Int64;
  D   : double;
  AuxSingle : single;
begin
  Result := nil;
  Metadata := Prevalence.Metadata(pClassName);
  vMethod := Metadata.MethodByName(pMethodName);
  if vMethod = nil then
    raise EExtP.CreateFmt('Erro: Visão %s inválida. Não pertence à package %s. Impossível exibir os objetos.', [pMethodName, pClassName]);
  Pos := 1;
  SetLength(Result, Length(vMethod.Params));
  for I := 0 to High(Result) do
    with vMethod.Params[I] do
      try
        S := Trim(ExtractFromStr(pParams, Pos, ','));
        if S = '' then exit;
        case TypeInfo.Kind of
          tkLString, tkWString, tkString : V := AnsiDequotedStr(S, '''');
          tkInteger, tkEnumeration, tkSet{$IFDEF FPC}, tkBool{$ENDIF} : begin
            AuxInt := StrToInt(S);
            case GetTypeData(TypeInfo).OrdType of
              otSLong : V := AuxInt;
              otUWord : V := Word(AuxInt);
              otUByte : V := Byte(AuxInt);
              otULong : V := Cardinal(AuxInt);
              otSWord : V := Smallint(AuxInt);
              otSByte : V := ShortInt(AuxInt);
            end;
          end;
          tkInt64: begin
            I64 := StrToInt64(S);
            V   := I64;
          end;
          tkChar, tkWChar : begin
            S := AnsiDequotedStr(S, '''');
            if S <> '' then
              V := S[1]
            else
              V := ' ';
          end;
          tkFloat:
            case GetTypeData(TypeInfo).FloatType of
              ftDouble : begin
              case GetDateTimeType(TypeInfo.Name) of
                dtDate:     V := StrToDate(S);
                dtTime:     V := StrToTime(S);
                dtDateTime: V := StrToDateTime(S);
              else
                D := StrToFloat(S);
                V := D;
              end;
            end;
              ftCurr     : V := StrToCurr(S);
              ftSingle   :
                begin
                  AuxSingle := StrToFloat(S);
                  V := AuxSingle;
                end;
              ftExtended : V := Extended(StrToFloat(S));
            end;
        end;
        Result[I] := V;
      except
        raise EExtP.Create('Error: invalid parameters in: ' + pParams)
      end;
end;

function GetViewPerm(pObject : TObject) : T_ViewPermissionSet;
var
  PerPkg    : T_PermissaoPackage;
  PerClass  : T_ClassPermissionSet;
  PerView   : T_PermissaoView;
  PkgName,
  ClaNome   : string;
  Loop      : boolean;
  PermTrue,
  PermFalse  : T_ViewPermissionSet;
begin
  Result := [_vpShow, _vpModify, _vpDelete];
  if Prevalence.IsInRecover or not (pObject is TPrevalent) or not GetSecurity then exit;
  PkgName := Prevalence.Metadata(pObject.ClassName).Package;
  if PkgName = '_Security' then exit;
  ClaNome := pObject.ClassName;
  Delete(ClaNome, 1, 1);
  //tratamento específico para pendencias de workflow
  if (CompareText(PkgName, '_Workflow') = 0) and (CompareText(ClaNome, '_Pendency') = 0) then
    with T_Pendency(pObject) do begin
      //delegação
      if Delegate <> nil then
        if (CompareText(Delegate.Nome, Browser.UserInfo.Profile) = 0) and ((Delegate.Nome = '') or
           (CompareText(DelegateUser, Browser.UserInfo.UserFullName) = 0)) then exit;
      //associação
      if AssignedProfile <> nil then
        if CompareText(AssignedProfile.Nome, Browser.UserInfo.Profile) = 0 then
          exit
        else begin
          Result := [];
          exit;
        end
      else
        if GetMetodoPerm(Method.Classe.Package.Nome, Method.Classe.Nome, Method.Nome) then
          exit
        else begin
          Result := [];
          exit;
        end;
    end;
  Result := [];
  PerPkg := Get_PermissaoPackage(PkgName);
  if PerPkg = nil then begin
    Result := [_vpShow];
    exit;
  end;
  PerClass := GetClassePerm(PkgName, ClaNome);
  if (_cpShow in PerClass) or (PerClass = []) then Result := [_vpShow];
  if _cpModify in PerClass then Result := [_vpModify];
  if _cpDelete in PerClass then Result := Result + [_vpShow, _vpDelete];
  if Result = [] then exit;
  if PerPkg.PermissoesViews.Count = 0 then exit;
  PermTrue := [_vpShow, _vpModify, _vpDelete];
  PermFalse := [_vpShow, _vpModify, _vpDelete];
  PerView := PerPkg.PermissoesViews.First;
  Loop := PerView <> nil;
  while Loop do begin
    if not (TPrevalent(pObject).CallMethod(PerView.View.Nome, MethodFillParams(pObject.ClassName, PerView.View.Nome, PerView.Parametro), TPrevalent(pObject))) then begin
      PermFalse := PermFalse * PerView.PermissaoFalso;
      if ((_vpModify in PermFalse) or (_vpDelete in PermFalse)) and not (_vpShow in PermFalse) then include(PermFalse, _vpShow);
    end
    else begin
      PermTrue := PermTrue * PerView.PermissaoVerdadeiro;
      if ((_vpModify in PermTrue) or (_vpDelete in PermTrue)) and not (_vpShow in PermTrue) then include(PermTrue, _vpShow);
    end;
    Loop := PerPkg.PermissoesViews.Next(PerView);
  end;
  Result := Result * PermFalse;
  Result := Result * PermTrue;
  if ((_vpModify in Result) or (_vpDelete in Result)) and not (_vpShow in Result) then include(Result, _vpShow);
end;

function AuthenticateUser(Thr : TepThread; Key : String) : Boolean;
var
  I, P   : Integer;
  User   : T_UsuarioBloqueado;
  Str, Str1, Usr, Hst : String;
begin
  P := 1; I := 0; Str1 := ''; Result := False;
  try
    try
      Usr := UpperCase(Thr.UserInfo.UserName);
      User := _UsuarioBloqueadoPorIdentificadorList.Find(Usr);
      if User <> nil then exit;
//gatilho
      Result := true;
      exit;
      Hst := '';
{
  AddToStr(Str, GetUserName);
  Hst := GetComputerName;
  AddToStr(Str, Hst);
  AddToStr(Str, GetHostIPByName(Hst));
  AddToStr(Str, GetChv(Usr, Srv));
  Hst:= GetLogonSrv(Usr, Srv);
  AddToStr(Str, Hst);
  AddToStr(Str, GetHostIPByName(Hst));
  AddToStr(Str, GetUserDomain(Usr, Srv));
  Result := Str;
  -> nome do usuário
  -> nome do computador local -> verifica se está na minha rede
  -> ip do computador local
  -> servidor de logon
  -> domínio do usuário
}
      I := 1;
      while P <= length(Key) do begin
        Str := UpperCase(ExtractFromStr(Key ,P));
        case I of
          1: if Str <> Usr then exit; //usuário da thread é o mesmo da key
          // não verifica: apenas utiliza para recuperar o IP
          2: if Str <> Thr.UserInfo.ComputerName then exit;
          3: if Str <> GetHostIPByName(Thr.UserInfo.ComputerName) then exit;//sem efeito -> era para testas se o ip é o mesmo
          4: if Str <> GetUserInfo(tuUserChv, Usr, Hst) then exit;//verifica se o userid é o mesmo
          5: begin
            Hst := GetUserInfo(tuLogonSrv, Usr); //verifica se o DC é o mesmo que autenticou o client
            if Str <> Hst then exit;
          end;
          6: if Str <> GetHostIPByName(Hst) then exit;
          7: if Str <> GetUserInfo(tuUserDomain, Usr, Hst) then exit;
          else Break;
        end;
        AddToStr(Str1, Str);
        inc(I);
      end;
      if (lowerCase(Key) = lowerCase(Str1)) and (GetProfiles(Thr.UserInfo.UserName, Thr.UserInfo.LogonSrv) <> '') then Result := true;
    except
    end;
  finally
    if not Result then begin
      EExtP.CreateFmt('*', [], true);
      EExtP.CreateFmt('Recusado Login. Usuário:<%s>. Motivo:<%d>', [Thr.UserInfo.UserName, I], true);
      EExtP.CreateFmt('*', [], true);
    end
    else
      EExtP.CreateFmt('Login com sucesso. Usuário:<%s> Estação:<%s>', [Thr.UserInfo.UserName, Thr.UserInfo.LogonSrv], true);
  end;
end;
//-------------Fim da Recuperação das permissões

procedure AssociaServicoDesigner(pNome : string);
var
  fSessao : T_Sessao;
  fServico : T_Servico;
begin
  fSessao := _SessaoPorThreadList.Find(PtrInt(Browser));
  if fSessao <> nil then begin
    BeginTransaction;
    fServico := T_Servico.Create;
    fServico.Nome := pNome;
    fSessao.Servicos.Add(fServico);
    EndTransaction;
  end;
end;

procedure DesassociaServicoDesigner(pNome : string; DoDelete : boolean = true);
var
  fSessao : T_Sessao;
  fServico : T_Servico;
begin
{$IFDEF MSWINDOWS}
  if NoService then begin
(*//*    if Servidor <> nil then begin
      with Servidor do begin
        Terminate(0);
        while Running do sleep(100);
        free;
      end;
      Servidor := nil;
    end*)
  end
  else
    try
      with TService.Create(pNome) do
        try
          if GetState = SERVICE_RUNNING then begin
            Stop;
            while GetState <> SERVICE_STOPPED do sleep(100);
          end;
        finally
          if DoDelete then Delete;
          Free;
        end;
    except end;
{$ENDIF}
  fSessao := _SessaoPorThreadList.Find(PtrInt(Browser));
  if fSessao <> nil then begin
    fServico := fSessao.Servicos.Find(pNome);
    if fServico <> nil then begin
      BeginTransaction;
      fSessao.Servicos.Delete(fServico);
      EndTransaction;
    end;
  end;
  FreeAndNil(Browser.GDB);
end;

procedure Login(Thr : TepThread);
var
  Sessao : T_Sessao;
begin
{$IFDEF VERSAOFREE}
  if _SessaoList.Count = 3 then
    raise EExtP.Create('Login não permitido. Atingido o número máximo 3 de conexões sem pagamento de licença.');
{$ENDIF}
  try
    BeginTransaction;
    Sessao := T_Sessao.Create;
    Sessao.PThread := Thr;
    _SessaoList.Add(Sessao);
    EndTransaction;
  except
    Rollback;
  end;
end;

procedure LogOff(Thr : TepThread);
var
  fSessao : T_Sessao;
  fServico : T_Servico;
begin
  try
    try
      BeginTransaction;
      fSessao:= _SessaoPorThreadList.Find(PtrInt(Thr));
      if fSessao <> nil then begin
        fServico := fSessao.Servicos.First;
        while Assigned(fServico) do begin
          DesassociaServicoDesigner(fServico.Nome);
          fSessao.Servicos.Next(fServico);
        end;
        _SessaoList.Delete(fSessao);
      end;
      EndTransaction;
    except
      Rollback;
    end;
  except
  end;
end;

procedure KillSession(Thr : TepThread); begin
  try
    Thr.Terminate;
  except
  end;
end;

//-------------Fim das Rotinas de Sessão
function CreateExport(pModelNome : string; pClasse : TObject) : TObject;
var
  Loop    : boolean;
  Obj     : TPrevalent;
  Prop    : T_Propriedade;
  Exp     : T_Exporta;
  ExpPro  : T_ExpPropriedade;
begin
  try
    Result := nil;
    CheckConstraints := false;
    Obj := _ExportaPorNomeList.Find(pModelNome);
    if Obj <> nil then raise Exception.Create('Erro : Modelo ' + pModelNome + ' já existe');
    if pClasse = nil then raise Exception.Create('Erro : Classe não informada');
    try
      Exp := T_Exporta.Create;
      Exp.Nome := pModelNome;
      Exp.Classe := T_Classe(pClasse);
      Prop := Exp.Classe.Propriedades.First;
      Loop := (Prop <> nil);
      while Loop do begin
{$IFDEF NEWXSC}
        if Prop.Index > 2 then begin
          ExpPro := T_ExpPropriedade.Create;
          ExpPro.Elemento := Prop.Index - 2;
{$ELSE}
        if Prop.Posicao > 2 then begin
          ExpPro := T_ExpPropriedade.Create;
          ExpPro.Elemento := Prop.Posicao - 2;
{$ENDIF}
          ExpPro.Propriedade := Prop;
          Exp.Propriedades.Add(ExpPro);
        end;
        Loop := Exp.Classe.Propriedades.Next(Prop);
      end;
      Result := Exp;
    except
      Raise;
    end;
  finally
    CheckConstraints := true;
//*    Browser.Refresh;
  end;
end;

function CreateImportDelimitado(pModelNome : string; pClasse : TObject) : TObject;
var
  Loop    : boolean;
  Obj     : TPrevalent;
  Prop    : T_Propriedade;
  Imp     : T_ImportaDelimitado;
  ImpPro  : T_PropriedadeDelimitado;
begin
  try
    Result := nil;
    CheckConstraints := false;
    Obj := _ImportaDelimitadoPorNomeList.Find(pModelNome);
    if Obj <> nil then raise Exception.Create('Erro : Modelo ' + pModelNome + ' já existe');
    Obj := _ImportaPosicionalPorNomeList.Find(pModelNome);
    if Obj <> nil then raise Exception.Create('Erro : Modelo ' + pModelNome + ' já existe como Posicional');
    if pClasse = nil then raise Exception.Create('Erro : Classe não informada');
    BeginTransaction;
    try
      Imp := T_ImportaDelimitado.Create;
      Imp.Nome := pModelNome;
      Imp.Classe := T_Classe(pClasse);
      Prop := Imp.Classe.Propriedades.First;
      Loop := (Prop <> nil);
      while Loop do begin
{$IFDEF NEWXSC}
        if Prop.Index > 2 then begin
          ImpPro := T_PropriedadeDelimitado.Create;
          ImpPro.Elemento := Prop.Index - 2;
{$ELSE}
        if Prop.Posicao > 2 then begin
          ImpPro := T_PropriedadeDelimitado.Create;
          ImpPro.Elemento := Prop.Posicao - 2;
{$ENDIF}
          ImpPro.Propriedade := Prop;
          Imp.Propriedades.Add(ImpPro);
        end;
        Loop := Imp.Classe.Propriedades.Next(Prop);
      end;
      EndTransaction;
      Result := Imp;
    except
      //Rollback;
      Raise;
    end;
  finally
    CheckConstraints := true;
//*    Browser.Refresh;
  end;
end;

function CreateImportPosicional(pModelNome : string; pClasse : TObject) : TObject;
var
  Loop    : boolean;
  Obj     : TPrevalent;
  Prop    : T_Propriedade;
  Imp     : T_ImportaPosicional;
  ImpPro  : T_PropriedadePosicional;
begin
  try
    Result := nil;
    CheckConstraints := false;
    Obj := _ImportaPosicionalPorNomeList.Find(pModelNome);
    if Obj <> nil then raise Exception.Create('Erro : Modelo ' + pModelNome + ' já existe');
    Obj := _ImportaDelimitadoPorNomeList.Find(pModelNome);
    if Obj <> nil then raise Exception.Create('Erro : Modelo ' + pModelNome + ' já existe como Delimitado');
    if pClasse = nil then raise Exception.Create('Erro : Classe não informada');
    try
      Imp := T_ImportaPosicional.Create;
      Imp.Nome := pModelNome;
      Imp.Classe := T_Classe(pClasse);
      Prop := Imp.Classe.Propriedades.First;
      Loop := (Prop <> nil);
      while Loop do begin
{$IFDEF NEWXSC}
        if Prop.Index > 2 then begin
          ImpPro := T_PropriedadePosicional.Create;
          ImpPro.Elemento := Prop.Index - 2;
{$ELSE}
        if Prop.Posicao > 2 then begin
          ImpPro := T_PropriedadePosicional.Create;
          ImpPro.Elemento := Prop.Posicao - 2;
{$ENDIF}
          ImpPro.Propriedade := Prop;
          Imp.Propriedades.Add(ImpPro);
        end;
        Loop := Imp.Classe.Propriedades.Next(Prop);
      end;
      Result := Imp;
    except
      Raise;
    end;
  finally
    CheckConstraints := true;
//*    Browser.Refresh;
  end;
end;

//Globais do Import
type
  TLayOut = record
    pos,
    posArq,
    qtdPos  : integer;
    ListaElemento   : string;
  end;

var
  ALayout     : array of TLayout;
  NumRegistro,
  posExtract  : integer;
  ChaveAtual  : string;

function ImportRecuperaObj(pClasse, pLista, pKey : string) : TPrevalent;
var
  List    : TPrevalentList;
begin
  List := Prevalence.PrevalentLists(pLista);
  if List = nil then begin
    raise Exception.CreateFmt('Erro : Lista Inexistente: %s', [pLista]);
    exit;
  end;
  case TUnpObjectList(List).ListType of
    ltInteger, ltWord, ltCurrency, ltLongInt: Result := TPrevalent(List.FindKeyInFamily(StrToIntDef(pKey, 0)));
    ltInt64 : Result := TPrevalent(List.FindKeyInFamily(StrToInt64Def(pKey, 0)));
    ltString: Result := TPrevalent(List.FindKeyInFamily(pKey));
    ltDateTime, ltDate, ltTime: Result := TPrevalent(List.FindKeyInFamily(StrToDateTime(pKey)));
    ltDouble : Result := TPrevalent(List.FindKeyInFamily(pKey))
  else
    raise Exception.CreateFmt('Tipo de retorno na Lista %s não previsto', [List.ClassName]);
  end;
end;

procedure ImportSetAssocProp(Obj : TPrevalent; Prop : PPropInfo; Lista, Valor, Delimitador : string);
var
  TargetObj : TPrevalent;
  PropObj   : TObject;
  Target, Str : string;
  P         : integer;
begin
  PropObj := GetObjectProp(Obj, Prop);
  if PropObj is TAssociation then
    Target := TAssociation(PropObj).ObjectClass.ClassName
  else
    Target := Prop.PropType^{$IFNDEF FPC}^{$ENDIF}.Name;
  if PropObj is TAssociation then begin
    P := 1;
    while P <= length(Valor) do begin
      Str := Trim(ExtractFromStr(Valor, P, Delimitador));
      if Str[1] = '[' then
        Delete(Str, 1, 1)
      else
        if Str[length(Str)] = ']' then Delete(Str, length(Str), 1);
      if Str <> '' then begin
        TargetObj := ImportRecuperaObj(Target, {Target + }'T' + Lista + 'List' , Str);
        if TargetObj <> nil then
          if TAssociation(PropObj).Find(TargetObj) <> nil then
            TAssociation(PropObj).Add(TPrevalent(TargetObj))
          else
            Browser.Message(Format('Registro: %d Chave: %s -> ' +
                'Aviso : Objeto ' + Str + ': ' + TPrevalent(TargetObj).GetIdentification  + ' já existe na associação. Inclusão não efetuada.',
                [NumRegistro, ChaveAtual]))
        else
          Browser.Message(Format('Registro: %d Chave: %s -> ' +
            'Aviso : Objeto ' + Str + ' não existe na Lista ' + Lista + '. Inclusão não efetuada.',
                [NumRegistro, ChaveAtual]))
      end;
    end;
  end
  else begin
    TargetObj := ImportRecuperaObj(Target, Target + Lista + 'List' , Str);
    if TargetObj = nil then
      Browser.Message(Format('Registro: %d Chave: %s ->' +
        'Aviso : Objeto ' + Str + ' não existe na Lista ' + Lista + '. Atualização de referência não efetuada.',
                [NumRegistro, ChaveAtual]))
    else
      SetObjectProp(Obj, Prop.Name, TargetObj);
  end;
end;

procedure MontaLayOutPosicional(pObj : T_ImportaPosicional);
var
  Prop  : T_PropriedadePosicional;
  Loop  : boolean;
  I     : integer;
begin
  Prop := pObj.Propriedades.Last;
  Loop := true;
  setLength(ALayout, Prop.Elemento + 1);
  for I := 0 to Prop.Elemento do ALayout[I].pos := -1;
  while Loop do begin
    TUnPPrevalent(Prop).DoCheckConstraints;
    ALayout[Prop.Elemento].pos := Prevalence.Metadata('T' + pObj.Classe.Nome).PropByName(Prop.Propriedade.Nome);
    ALayout[Prop.Elemento].posArq := Prop.Posicao;
    ALayout[Prop.Elemento].qtdPos := Prop.QuantidadePosicoes;
    ALayout[Prop.Elemento].ListaElemento := Prop.ListaElementoChave;
    Loop := pObj.Propriedades.Prior(Prop);
  end;
end;

procedure MontaLayOutDelimitado(pObj : T_ImportaDelimitado);
var
  Prop  : T_PropriedadeDelimitado;
  Loop  : boolean;
  I     : integer;
begin
  Prop := pObj.Propriedades.Last;
  Loop := true;
  setLength(ALayout, Prop.Elemento + 1);
  for I := 0 to Prop.Elemento do ALayout[I].pos := -1;
  while Loop do begin
    TUnPPrevalent(Prop).DoCheckConstraints;
    ALayout[Prop.Elemento].pos := Prevalence.Metadata('T' + pObj.Classe.Nome).PropByName(Prop.Propriedade.Nome);
    ALayout[Prop.Elemento].ListaElemento := Prop.ListaElementoChave;
    Loop := pObj.Propriedades.Prior(Prop);
  end;
end;

procedure Import(pObject : TObject);
var
  Obj     : TPrevalent;
  Arq     : TextFile;
  posEle,
  posKey,
  IgnoraInicio,
  IgnoraFim,
  NumTotalRegistro : integer;
  Arquivo,
  Linha,
  Str,
  ChaveAtual,
  ImpLista,
  ImpClasse    : string;
  NewObj,
  isPosicional : boolean;

  function RecuperaPrincipalKey : string;
  var
    I : integer;
  begin
    I := 0; Result := '';
    posExtract := 1;
    while posExtract <= length(Linha) do begin
      inc(I);
      Result := ExtractFromStr(Linha, posExtract, T_ImportaDelimitado(pObject).Delimitador);
      ChaveAtual := Result;
      if I = posKey then break;
    end;
  end;

begin
  isPosicional := (pObject.ClassName = 'T_ImportaPosicional');
  if isPosicional then begin
    TUnPPrevalent(T_ImportaPosicional(pObject)).DoCheckConstraints;
    ImpClasse := 'T' + T_ImportaPosicional(pObject).Classe.Nome;
    ImpLista := ImpClasse + T_ImportaPosicional(pObject).Lista + 'List';
    Arquivo := T_ImportaPosicional(pObject).Arquivo;
    ShortDateFormat := T_ImportaPosicional(pObject).FormatoDataHora;
    IgnoraInicio := T_ImportaPosicional(pObject).SaltarInicio;
    IgnoraFim := T_ImportaPosicional(pObject).SaltarFim;
    posKey := T_ImportaPosicional(pObject).NumeroElementoChave;
  end
  else begin
    TUnPPrevalent(T_ImportaDelimitado(pObject)).DoCheckConstraints;
    ImpClasse := 'T' + T_ImportaDelimitado(pObject).Classe.Nome;
    ImpLista := ImpClasse + T_ImportaDelimitado(pObject).Lista + 'List';
    Arquivo := T_ImportaDelimitado(pObject).Arquivo;
    ShortDateFormat := T_ImportaDelimitado(pObject).FormatoDataHora;
    IgnoraInicio := T_ImportaDelimitado(pObject).SaltarInicio;
    IgnoraFim := T_ImportaDelimitado(pObject).SaltarFim;
    posKey := T_ImportaDelimitado(pObject).NumeroElementoChave;
  end;
  if isPosicional then
    MontaLayOutPosicional(T_ImportaPosicional(pObject))
  else
    MontaLayOutDelimitado(T_ImportaDelimitado(pObject));
  if not FileExists(Arquivo) then begin
    Browser.Message('Erro : Arquivo ' + Arquivo + ' não existe.');
    exit;
  end;
  try
    assignfile(Arq, Arquivo);
    reset(Arq);
    NumRegistro := 0; NumTotalRegistro :=0;
    while not SeekEOF(Arq) do begin
      readln(Arq, Linha);
      inc(NumTotalRegistro);
    end;
    reset(Arq);
    try
      NumRegistro := 0;
      Prevalence.EndTransaction(true);
      CheckConstraints := false;
      Prevalence.BeginTransaction;
      Browser.Message('Início da importação');
      with Prevalence.Metadata(ImpClasse) do begin
        while not SeekEOF(Arq) do begin
          readln(Arq, Linha);
          inc(NumRegistro);
          if NumRegistro > (NumTotalRegistro - IgnoraFim) then break;;
          if NumRegistro < (IgnoraInicio + 1) then continue;
          if isPosicional then begin
            Str := Trim(copy(Linha, ALayout[posKey].posArq, ALayout[posKey].qtdPos));
            ChaveAtual := Str;
          end
          else
            Str := Trim(RecuperaPrincipalKey);
          if posKey <> 0 then
            Obj := ImportRecuperaObj(ImpClasse, ImpLista , Str)
          else
            Obj := nil;
          if Obj = nil then begin
            Obj := TPrevalentClass(Prevalence.PrevalentLists(ImpLista).ObjectClass).Create;
            NewObj := true;
          end
          else begin
            NewObj := false;
            if isPosicional then begin
              if T_ImportaPosicional(pObject).SeExiste = _iseIgnorar then continue
            end
            else
              if T_ImportaDelimitado(pObject).SeExiste = _iseIgnorar then continue;
          end;
          posExtract := 1; posEle := 0;
          while true do begin
            inc(posEle);
            if isPosicional then begin
              if not (posEle < length(ALayout) - 1) then break
            end else
              if posExtract > length(Linha) then break;
            if ALayout[posEle].pos <> -1 then begin
              Str := '';
              if isPosicional then
                Str := Trim(copy(Linha, ALayout[posEle].posArq, ALayout[posEle].qtdPos))
              else
                Str := Trim(ExtractFromStr(Linha, posExtract, T_ImportaDelimitado(pObject).Delimitador));
              if Str = '' then continue;
              case TypeProp[ALayout[posEle].pos] of
                tkInteger, tkEnumeration, tkSet{$IFDEF FPC}, tkBool{$ENDIF}:
                  SetOrdProp(Obj, Properties[ALayout[posEle].pos], StrToIntDef(Str, 0));
                tkInt64 : SetInt64Prop(Obj, Properties[ALayout[posEle].pos], StrToInt64Def(Str, 0));
                tkClass : begin
                  Str := Str + '|' + Trim(ExtractFromStr(Linha, posExtract, ']'));
                  inc(posExtract);
                  ImportSetAssocProp(Obj, Properties[ALayout[posEle].pos], ALayout[posEle].ListaElemento, Str, T_ImportaDelimitado(pObject).Delimitador);
                end;
                tkChar  : SetOrdProp(Obj, Properties[ALayout[posEle].pos], integer(Str[1]));
                tkFloat :
                  if LeftStr(TypeNameProp[ALayout[posEle].pos], 5) = 'TDate' then
                    SetFloatProp(Obj, Properties[ALayout[posEle].pos], StrToDateTime(Str))
                  else
                    SetPropInfoValue(Obj, Properties[ALayout[posEle].pos], Str);
                tkString, tkLString :
                  SetStrProp(Obj, Properties[ALayout[posEle].pos].Name, Str);
              else
                SetPropInfoValue(Obj, Properties[ALayout[posEle].pos], Str);
              end;
            end;
          end;
          if NewObj then begin
            Obj.Add;
            Browser.Message(Format('Registro: %d Chave: %s -> %s', [NumRegistro, ChaveAtual, 'Incluido']))
          end else
            Browser.Message(Format('Registro: %d Chave: %s -> %s', [NumRegistro, ChaveAtual, 'Atualizado']))
        end;
      end;
      Prevalence.EndTransaction;
    except
      on E : Exception do begin
        Browser.Message(Format('Registro: %d Chave: %s -> %s', [NumRegistro, ChaveAtual, 'Erro : ' + E.Message]));
        Prevalence.RollBack;
      end;
    end;
  finally
    CheckConstraints := true;
    close(Arq);
  end;
  try
    DeleteFile(pChar(ChangeFileExt(Arquivo, '.imported')));
    RenameFile(Arquivo, ChangeFileExt(Arquivo, '.imported'));
  except
    Browser.Message('Aviso: Não foi possível renomear o arquivo');
  end;
  Browser.Message('Fim da importação');
//*  Browser.Refresh;
end;

procedure Import(Nome : string);
var
  ImpObj  : TObject;
begin
  ImpObj:= _ImportaDelimitadoPorNomeList.FindKeyInFamily(Nome);
  if ImpObj = nil then raise Exception.Create('Erro : Modelo de importação informado inexistente');
  Import(ImpObj);
end;

function RecuperaValor(pObj : TObject; pPropNome : string; pTypeProp : TTypeKind) : string;
var
  V : variant;
begin
  V := GetPropValue(pObj, pPropNome, false);
  if pTypeProp in [tkEnumeration{$IFDEF FPC}, tkBool{$ENDIF}] then
    with TVarData(V) do
      if VType = varBoolean then begin
        if VBoolean then Result := '1' else Result := '0';
        exit;
      end;
  Result := Trim(varToStr(V));
end;

procedure Export(pObject : TObject);
var
  I, J, PropIndex, NumRegistro : integer;
  Arq : textfile;
  Str, Linha : string;
  ExpObj : T_Exporta;
  ExpProp : T_ExpPropriedade;
  Props : array of T_ExpPropriedade;
  List  : TPrevalentList;
  ObjCla, AssEle : TPrevalent;
  ObjAss : integer;
begin
  if pObject = nil then begin
    raise Exception.Create('Erro : O Modelo de exportação deve ser informado');
    exit;
  end;
  TUnPPrevalent(T_Exporta(pObject)).DoCheckConstraints;
  ExpObj := T_Exporta(pObject);
  ShortDateFormat := ExpObj.FormatoDataHora;
  SetLength(Props, ExpObj.Propriedades.Count);
  ExpProp := ExpObj.Propriedades.First;
  I := 0;
  while ExpProp <> nil do begin
    TUnPPrevalent(T_ExpPropriedade(ExpProp)).DoCheckConstraints;
    Props[I] := ExpProp;
    ExpObj.Propriedades.Next(ExpProp);
    if ExpProp <> nil then inc(I);
  end;
  List := Prevalence.PrevalentLists('T' + ExpObj.Classe.Nome + 'List');
  ObjCla := TPrevalent(List.First);
  NumRegistro := 0;
  try
    try
      if ObjCla <> nil then begin
        assignfile(Arq, ExpObj.Arquivo);
        rewrite(Arq);
        Linha := '';
      end
      else begin
        Browser.Message('Aviso : Classe para exportação vazia.');
        exit;
      end;
      while ObjCla <> nil do begin
        with Prevalence.Metadata('T' + ExpObj.Classe.Nome) do
          for I := 0 to Length(Props) -1 do begin
            PropIndex := PropByName(Props[I].Propriedade.Nome);
            if TypeProp[PropIndex] <> tkClass then
              Linha := Linha + RecuperaValor(ObjCla, Props[I].Propriedade.Nome, TypeProp[PropIndex]) + '|'
            else begin
              ObjAss := StrToInt(RecuperaValor(ObjCla, Properties[PropIndex].Name, TypeProp[PropIndex]));
              if IsAssociation[PropIndex] then begin
                if TAssociation(ObjAss).Count > 0 then begin
                  AssEle := TPrevalent(TAssociation(ObjAss).First);
                  Str := '[';
                  while AssEle <> nil do begin
                    J := Prevalence.Metadata(AssEle.ClassName).PropByName(Props[I].ListaElementoChave);
                    if J < 0 then begin
                      raise Exception.Create('Erro : Elemento Chave (' + Props[I].ListaElementoChave + ')  informado não foi localizado.');
                      exit;
                    end;
                    Str := Str + RecuperaValor(AssEle, Props[I].ListaElementoChave, TypeProp[J]) + '|';
                    TAssociation(ObjAss).Next(TTransient(AssEle));
                  end;
                  Str[length(Str)] := ']';
                end
                  else
                    Str := '[]';
                Linha := Linha + Str + '|';
              end else
              begin
                J := Prevalence.Metadata(TObject(ObjAss).ClassName).PropByName(Props[I].ListaElementoChave);
                Linha := Linha + RecuperaValor(TObject(ObjAss), Props[I].ListaElementoChave, TypeProp[J]) + '|';
              end;
            end;
          end;
        writeln(Arq, copy(Linha, 1, length(linha) - 1));
        inc(NumRegistro);
        if ExpObj.Log then Browser.Message(Format('Exportado: %d Chave: %s', [NumRegistro, ObjCla.GetIdentification]));
        flush(Arq);
        Linha := '';
        List.Next(TTransient(ObjCla));
      end;
    except
      on E : Exception do begin
        Browser.Message(Format('Erro: %d Chave: %s -> %s', [NumRegistro, TPrevalent(ObjCla).GetIdentification, E.Message]));
        exit;
      end;
    end;
  finally
    try close(Arq) except end;
    Browser.Message(Format('Fim da Exportação. Número de Registros exportados: %d', [NumRegistro]));
  end;
end;

procedure Export(Nome : string);
var
  ExpObj  : T_Exporta;
begin
  ExpObj:= _ExportaPorNomeList.Find(Nome);
  if ExpObj = nil then raise Exception.Create('Erro : Modelo de exportação informado inexistente');
  Export(ExpObj);
end;

//Rotinas de carga de Ocupação
procedure InicializaClassesOcupacao;
var
  I, J  : integer;
  Pkg   : T_OcupacaoPackage;
  Clas  : T_OcupacaoClasse;
  List  : T_OcupacaoLista;
  Assoc : T_OcupacaoAssociacao;
  Props : TProperties;
begin
  exit;
  try
    with Prevalence do
      for I := 0 to Prevalents.Count - 1 do begin
        Props := Metadata(Prevalents[I]);
        BeginTransaction;
        Pkg := _OcupacaoPackagePorNomeList.Find(Props.Package);
        if Pkg = nil then begin
          Pkg := T_OcupacaoPackage.Create;
          Pkg.Package := _PackagePorNomeList.Find(Props.Package);
          Pkg.Add;
        end;
        Clas := _OcupacaoClassePorNomeList.Find(copy(Prevalents[I],2,100));
        if Clas = nil then begin
          Clas := T_OcupacaoClasse.Create;
          Clas.Classe := _ClassePorNomeList.Find(copy(Prevalents[I],2,100));
          Clas.Package := Pkg;
          Clas.Add;
        end;
        List := T_OcupacaoLista.Create;
        List.Classe := Clas;
        List.Indice := I;
        List.Add;
        with Props do
          for J := 0 to PropCount -1 do
            if (TypeProp[J] = tkClass) and IsAssociation[J] then begin
              Assoc := T_OcupacaoAssociacao.Create;
              Assoc.Associacao := Props.Properties[J].Name;
              Assoc.Lista := List;
              Assoc.Add;
            end;
        EndTransaction;
      end;
  except
    Rollback;
  end;
end;

function CalculaOcupacaoMemoriaLista(pObj : TObject) : double;
var
  I, J, Ret : integer;
  Props   : TProperties;
  Target  : TObject;
  Lista   : T_OcupacaoLista;
  Assoc   : T_OcupacaoAssociacao;
begin
  Result := 0;
  exit;
  Ret := 0;
  Lista := T_OcupacaoLista(pObj);
  BeginTransaction;
  Assoc := Lista.Associacoes.First;
  while Assoc <> nil do begin
    Assoc.Count := 0;
    Assoc.Capacity := 0;
    Lista.Associacoes.Next(Assoc);
  end;
  with Prevalence.PrevalentLists(Lista.Indice) do 
    for I := Count-1 downto 0 do
      with Prevalence do begin
        Props := Prevalence.Metadata(Prevalence.Prevalents[Lista.Indice]);
        Assoc := nil;
        with Props do
          for J := 0 to Props.PropCount -1 do begin
            case TypeProp[J] of
              tkString,
              tkLString : inc(Ret, length(GetPropInfoValue(Objects[I], Properties[J])));
              tkClass :
                if IsAssociation[J] then begin
                  Target := TObject(GetObjectProp(Objects[I], Properties[J]));
                  if Assoc = nil then
                    Assoc := Lista.Associacoes.First
                  else
                    Lista.Associacoes.Next(Assoc);
                  if Assoc = nil then Rollback;
                  Assoc.Count := Assoc.Count + TAssociation(Target).Count;
                  Assoc.Capacity := Assoc.Capacity + TAssociation(Target).Capacity;
                  inc(Ret, Assoc.Capacity * sizeof(integer));
                end;
            end;
            case TypeProp[J] of
              tkInteger, tkEnumeration, tkChar, tkSet, tkWChar{$IFDEF FPC}, tkBool{$ENDIF} :
                case OrdTypeProp[J] of
                  otSLong, otULong : inc(Ret, sizeof(Integer));
                  otSByte, otUByte : inc(Ret, sizeof(Byte));
                  otSWord, otUWord : inc(Ret, sizeof(Word));
                end;
              tkInt64 : inc(Ret, sizeof(Int64));
              tkFloat :
                case FloatTypeProp[J] of
                  ftCurr : inc(Ret, sizeof(Currency));
                  ftDouble, ftComp : inc(Ret, sizeof(Double));
                  ftSingle : inc(Ret, sizeof(Single));
                  ftExtended : inc(Ret, sizeof(Extended));
                end;
              tkLString, tkString : inc(Ret, sizeof(Word));
            end;
          end;
      end;
  EndTransaction;
  Result := Ret;// div 1024;
end;

//--------------------------------------------
// rotinas de auditoria
//--------------------------------------------

function GetAuditory : boolean;
const
  Auditory : boolean = false;
  CachedAuditory : boolean = false;
begin
  if not CachedAuditory then begin
    CachedAuditory := true;
    Auditory := UpperCase(GetIniParameter('Server', 'Auditory', 'false')) = 'TRUE';
  end;
  Result := Auditory;
end;

function GetFileName(pArq : T_Aud_Arquivo; Extensao : string = 'aud') : string;
var
  P : integer;
begin
  P := 1;
  if pArq.Status = _casAtivo then
    if Extensao = 'aud' then
      Result := DirAudit + pArq.Nome
    else begin
      Result := Trim(ExtractFromStr(pArq.Nome, P, '.'));
      if Extensao = 'log' then
        Result := DirLog + Result + '.' + Extensao
      else
        Result := DirSnapShot + Result + '.' + Extensao
    end
  else
    if Extensao = 'aud' then
      Result := DirBackup + pArq.Nome
    else
      Result := DirBackup + Trim(ExtractFromStr(pArq.Nome, P, '.')) + '.' + Extensao;

end;

procedure AtualizaControleArquivo(pFileName : string; pData : TDateTime; pStatus : T_Aud_Arquivostatus);
{$IFNDEF AUDITORIA}
begin
end;
{$ELSE}
var
  AudArquivo : T_Aud_Arquivo;
begin
  //é pegar o ativo e alterar para arquivado, trocando Status e Nome. Se o Nome já existir, atualiza a Data
  InAuditMode := true;
  try
    try
      BeginTransaction;
      AudArquivo := _Aud_ArquivoPorNomeList.Find(pFileName);//verifica se o arquivo já existe
      if AudArquivo <> nil then begin
        if AudArquivo.Status = _casAtivo then AudArquivo.Data := pData;
        AudArquivo.Status := pStatus;
      end
      else begin //recupera o ativo
        AudArquivo := _Aud_ArquivoPorStatusList.Find(word(_casAtivo));
        if AudArquivo <> nil then begin
          AudArquivo.Nome := pFileName;
          AudArquivo.Status := pStatus;
        end
        else begin
          AudArquivo := T_Aud_Arquivo.Create;
          with AudArquivo do begin
            Data := pData;
            Nome := pFileName;
            Status := pStatus;
            Add;
          end;
        end;
      end;
      EndTransaction;
    except
      Rollback;
    end;
  finally
    InAuditMode := false;
  end;
end;
{$ENDIF}
{ Como gravar a Auditoria
1. opBeginTransaction
  a) find na classe Aud_Login se não encontrar:
    - grava opLogin(usuário, nome, máquina, perfil)
    - recupera a posição e insere na Aud_Login
  b) grava opBeginTransaction(data e posição do usuário)
2. opAdd e opDelete
  a) grava LI, ID e Identification

Como recuperar o objeto

1. Insert
  a) se o da prevalencia tiver a versão zero é ele
  b) senão: é o do snapshot ou do log
  --> basta gravar o ID e a LI
2. Delete
  a) está no snapshot + complementado com com todos os updates do log até o delete
  --> basta gravar o ID e a LI
3. Update
  a) está no snapshot + complementado com todos os updates do log, até o update em questão
  --> basta continuar gravando o que está sendo gravado hoje
}

procedure GravaStringAuditoria(pString : string);
var
  vSize : word;
begin
  with TUnPTransaction(ThreadTrans).AuditStream do begin
    vSize := length(pString);
    Write(vSize, sizeof(vSize));
    Write(pString[1], vSize);
  end;
end;
procedure GravaBeginTransactionAuditoria;
{$IFNDEF AUDITORIA}
begin
end;
{$ELSE}
var
  vOperation : T_Operation;
  vLog : T_Aud_Login;
  vArquivo : T_Aud_Arquivo;
  vLoginPosition : integer;
  vData : double;
  vUsuario,
  vNome,
  vMaquina,
  vPerfil : string;
  MapAuditLog : P_MapeamentoAuditLog;
begin

  InAuditMode := true;
  try
    if ObjectServer.ConnectionThread <> nil then begin
      vUsuario := Browser.UserName;
      vNome := Browser.UserName;
      vMaquina := Browser.ComputerName;
      vPerfil := Browser.Profile;
    end
    else begin
      vUsuario := 'Internal';
      vNome := 'Internal';
      vMaquina := 'Internal';
      vPerfil := 'Internal';
    end;
    with TUnPTransaction(ThreadTrans) do begin
      vOperation := _opLogin;
      AuditStream.Write(vOperation, sizeof(vOperation));
      GravaStringAuditoria(vUsuario);
      GravaStringAuditoria(vNome);
      GravaStringAuditoria(vMaquina);
      GravaStringAuditoria(vPerfil);
    end;
    vLoginPosition := 0;
    vLog := _Aud_LoginPorUsuarioMaquinaPerfilList.Find(vUsuario+'.'+vMaquina+'.'+vPerfil);
    if vLog <> nil then
      vLoginPosition := vLog.Posicao
    else
      try
        BeginTransaction;
        vLog := T_Aud_Login.Create;
        with vLog do begin
          Usuario := vUsuario;
          Nome := vNome;
          Maquina := vMaquina;
          Perfil := vPerfil;
///está errado
Posicao := vLoginPosition;
          vArquivo := _Aud_ArquivoPorStatusList.Find(word(_casAtivo));
          Arquivo := vArquivo;
          Add;
        end;
        EndTransaction;
      except
        Rollback;
      end;
    with TUnPTransaction(ThreadTrans) do begin
      vOperation := _opBeginTransaction;
      AuditStream.Write(vOperation, sizeof(vOperation));
      // marca a posicao da data da operação para acerto posterior
      New(MapAuditLog);
      MapAuditLog.Operation := _opBeginTransaction;
      MapAuditLog.audPos := AuditStream.Position;
      MapAuditLog.logPos := 0;
      MapeamentoAuditLog.Add(MapAuditLog);
      vData := 0;
      AuditStream.Write(vData, sizeof(vData)); //data
      // marca a posicao do login para acerto posterior
      New(MapAuditLog);
      MapAuditLog.Operation := _opLogin;
      MapAuditLog.audPos := AuditStream.Position;
      MapAuditLog.logPos := 0;
      MapeamentoAuditLog.Add(MapAuditLog);
      AuditStream.Write(vLoginPosition, sizeof(vLoginPosition)); //posicao do login
      if Assigned(ObjectServer) then
        if TUnPBrowser(Browser).RunningMethodName <> '' then
          GravaStringAuditoria(TUnPBrowser(Browser).RunningMethodName) //é para colocar o nome da stm
        else
          GravaStringAuditoria('Browser')
      else
        GravaStringAuditoria('Internal');
    end;
  finally
    InAuditMode := false;
  end;
end;
{$ENDIF}
procedure GravaAuditoria(pOperation : T_Operation; pObj : TObject = nil);
{$IFNDEF AUDITORIA}
begin
end;
{$ELSE}
var
  Prevalent : TPrevalent;
  LogPosition, ID : integer;
  MapAuditLog : P_MapeamentoAuditLog;

  procedure AuditaObjeto; begin
    with TUnPTransaction(ThreadTrans) do begin
      AuditStream.Write(pOperation, sizeof(pOperation));
      New(MapAuditLog);
      MapAuditLog.Operation := pOperation;
      MapAuditLog.audPos := AuditStream.Position;
      LogPosition := Stream.Position;
      MapAuditLog.logPos := LogPosition;
      MapeamentoAuditLog.Add(MapAuditLog);
      AuditStream.Write(LogPosition, sizeof(LogPosition));
      ID := Prevalent.ID;
      AuditStream.Write(ID, sizeof(ID));
      GravaStringAuditoria(Prevalent.GetIdentification);
    end;
  end;

begin
  with TUnPTransaction(ThreadTrans) do begin
    Prevalent := TPrevalent(pObj);
    if pObj <> nil then begin
      if Prevalent.Metadata.Package = '_Auditory' then exit;
    end;
    case pOperation of
      _opBeginTransaction : GravaBeginTransactionAuditoria;
      _opAdd, _opDelete, _opUpdate : AuditaObjeto;
    end;
  end;
end;
{$ENDIF}

procedure CorrigeMapeamentoAuditLog(pAuditFilePosition, pLogFilePosition : integer);
{$IFNDEF AUDITORIA}
begin
end;
{$ELSE}
var
  I : integer;
  MapAuditLog : P_MapeamentoAuditLog;
  NewPosition : integer;
  Data : double;
begin
  with TUnPTransaction(ThreadTrans) do
    for I := MapeamentoAuditLog.Count - 1 downto 0 do begin
      MapAuditLog := MapeamentoAuditLog[I];
      AuditStream.Seek(MapAuditLog.audPos, soBeginning);
      case MapAuditLog.Operation of
        _opBeginTransaction : begin //acerto da data/hora da operação
          Data := Now;
          AuditStream.Write(Data, sizeof(Data));
        end;
        _opLogin : begin //acerto da posicao do login
          NewPosition := MapAuditLog.audPos + pAuditFilePosition;
          AuditStream.Write(NewPosition, sizeof(NewPosition));
        end;
        else begin
          NewPosition := MapAuditLog.logPos + pLogFilePosition;
          AuditStream.Write(NewPosition, sizeof(NewPosition));
        end;
      end;
      AuditStream.Position := AuditStream.Size;
    end;
end;
{$ENDIF}
procedure CarregaHeaderAuditoria(pArq : T_Aud_Arquivo);
{$IFNDEF AUDITORIA}
begin
end;
{$ELSE}
var
  vSingle : single;
  AuditFileName : string;
  I, J : integer;
  Stream  : TUnPPrevalentStream;
  AudClas : T_Aud_Classe;
  AudPkg  : T_Aud_Package;
  AudProp : T_Aud_Propriedade;
  HeaderFile : THeaderFile;
begin
  if pArq.Packages.Count > 0 then exit;
  AuditFileName := GetFileName(pArq);
  Stream := TUnPPrevalentStream(TUnPPrevalentStream.Create(AuditFileName, fmOpenRead or fmShareDenyNone));
  with Stream do begin
    Read(vSingle, sizeof(vSingle));
    TUnPPrevalentStream(Stream).LoadHeaderFile(HeaderFile);
    InAuditMode := true;
    try
      BeginTransaction;
      try
        for I := 0 to high(HeaderFile.HeaderData) - 1 do
          with HeaderFile.HeaderData[I] do begin
            //package
              AudPkg := pArq.Packages.Find(PackageName);
              if AudPkg = nil then begin
                AudPkg := T_Aud_Package.Create;
                AudPkg.Nome := PackageName;
                AudPkg.Arquivo := pArq;
                AudPkg.Add;
              end;
              AudClas := AudPkg.Classes.Find(ListIndex);
              if AudClas = nil then begin
                 AudClas := T_Aud_Classe.Create;
                 AudClas.Nome := ClassName;
                 AudClas.Alias := ClassAlias;
                 AudClas.ListIndex := ListIndex;
                 AudClas.Package := AudPkg;
                 AudClas.Arquivo := pArq;
                 AudClas.Add;
              end;
              for J := 0 to length(Props) - 1 do with Props[J] do begin
                AudProp := AudClas.Propriedades.Find(PropIndex);
                if AudProp = nil then begin
                  AudProp := T_Aud_Propriedade.Create;
                  AudProp.Nome := PropName;
                  AudProp.Alias := PropAlias;
                  AudProp.Mascara := PropMask;
                  AudProp.Tipo := word(PropKind);
                  AudProp.SubTipo := PropSubKind;
                  AudProp.PropIndex := PropIndex;
                  AudProp.Classe := AudClas;
                  AudProp.Add;
                end;
              end;
          end;
      EndTransaction;
      except
        RollBack;
      end;
    finally
      InAuditMode := false;
    end;
  end;
  Stream.Free;
end;
{$ENDIF}

function LeStringAuditoria(AuditStream : TFileStream) : string;
var
  vSize : word;
begin
  with AuditStream do begin
    Read(vSize, sizeof(vSize));
    SetLength(Result, vSize);
    Read(Result[1], vSize);
  end;
end;

{$IFDEF AUDITORIA}
function ReadAuditProperty(Stream : TFileStream; pProp : T_Aud_Propriedade) : variant;
var
  PropInteger  : integer;
  PropByte     : byte;
  PropWord     : word;
  PropInt64    : Int64;
  PropString   : string;
  PropDouble   : double;
  PropCurrency : currency;
  PropSingle   : single;
  PropExtended : extended;
begin
  with Stream, pProp do begin
      case TTypeKind(Tipo) of
        tkInteger, tkEnumeration, tkChar, tkSet, tkWChar{$IFDEF FPC}, tkBool{$ENDIF} :
          case TOrdType(SubTipo) of
            otSLong, otULong : begin
              Read(PropInteger, sizeof(PropInteger));
              Result := PropInteger;
            end;
            otSByte, otUByte : begin
              Read(PropByte, sizeof(PropByte));
              Result := PropByte;
            end;
            otSWord, otUWord : begin
              Read(Result, sizeof(PropWord));
              Result := PropWord;
            end;
          end;
        tkInt64 : begin
          Read(PropInt64, sizeof(PropInt64));
          Result := PropInt64;
        end;
        tkFloat :
          case TFloatType(SubTipo) of
            ftCurr : begin
              Read(PropCurrency, sizeof(PropCurrency));
              Result := PropCurrency;
            end;
            ftDouble, ftComp : begin
              Read(PropDouble, sizeof(PropDouble));
              Result := PropDouble;
            end;
            ftSingle : begin
              Read(PropSingle, sizeof(PropSingle));
              Result := PropSingle;
            end;
            ftExtended : begin
              Read(PropExtended, sizeof(PropExtended));
              Result := PropExtended;
            end;
          end;
        tkLString, tkString : begin
          Read(PropWord, sizeof(PropWord));
          SetLength(PropString, PropWord);
          Read(PropString[1], PropWord);
          Result := PropString;
        end;
        tkClass : begin
          Read(PropWord, sizeof(PropWord));
          if PropWord <> NONELIST then begin
            Read(PropInteger, sizeof(PropInteger));
            Result := PropInteger;
          end
          else
            Result := NONELIST;
        end;
      end;
  end;
end;
{$ENDIF}

procedure CarregaTransacaoAuditoria(pArq : T_Aud_Arquivo; pCri : T_Aud_Criterio);
{$IFNDEF AUDITORIA}
begin
end;
{$ELSE}

type
  TObjectProp = packed record
    Prop      : T_Aud_Propriedade;
    PropValue : variant;
  end;
  PObjectProp = ^TObjectProp;
  TArqLogin  = packed record
    Usuario,
    Nome,
    Maquina,
    Perfil : string;
    Posicao : integer;
  end;

var
  vData : double;
  vSingle : single;
  vPropIndex,
  vListIndex : word;
  vOperacao : T_Operation;
  I,
  vUsuPosicao,
  vIDObjeto,
  vAuditPosition,
  vLogPosition : integer;
  vOrigem,
  vIdentificacaoObjeto,
  AuditFileName,
  LogFileName : string;
  AuditStream,
  LogStream : TFileStream;
  PropItem : PObjectProp;
  PropList : TList;
  arqLogin : TArqLogin;
  v_Login : T_Aud_Login;
  vCla : T_Aud_Classe;
  vOri : T_Aud_Origem;
  vOpe : T_Aud_Operacao;
  vUpd : T_Aud_Update;
  HeaderFile : THeaderFile;

  function InCriterio : boolean; begin
    Result := false;
    if not ((vOperacao in [_opLogin, _opBeginTransaction]) or (pCri.Operacao = _opAll) or (pCri.Operacao = vOperacao)) then exit;
    if not ((pCri.Usuario = '') or (AnsiCompareText(ArqLogin.Usuario, pCri.Usuario) = 0)) then exit;
    if not ((pCri.Classe = '') or (pCri.Classe = vCla.Nome)) then exit;
    if not ((pCri.Package = '') or (pCri.Package = vCla.Package.Nome)) then exit;
    Result := true;
  end;

begin
  AuditFileName := GetFileName(pArq);
  if not FileExists(AuditFileName) then exit;
  LogFileName := GetFileName(pArq , 'log');
  if not FileExists(LogFileName) then exit;
  CarregaHeaderAuditoria(pArq);
  AuditStream := TFileStream.Create(AuditFileName, fmOpenRead or fmShareDenyNone);
  LogStream := TFileStream.Create(LogFileName, fmOpenRead or fmShareDenyNone);
  with AuditStream do begin
    Read(vSingle, sizeof(vSingle));
    TUnPPrevalentStream(AuditStream).LoadHeaderFile(HeaderFile);
    vCla := nil;
    PropList := nil;
    vAuditPosition := -1;
    try
      while Read(vOperacao, sizeof(vOperacao)) > 0 do begin
        case vOperacao of
          _opLogin : begin
            with arqLogin do begin
              arqLogin.Posicao := AuditStream.Position - sizeof(vOperacao);
              Usuario := LeStringAuditoria(AuditStream);
              Nome    := LeStringAuditoria(AuditStream);
              Maquina := LeStringAuditoria(AuditStream);
              Perfil  := LeStringAuditoria(AuditStream);
            end;
            continue;
          end;
          _opBeginTransaction : begin
            Read(vData, sizeof(vData));
            if not ((CompareDateTime(pCri.DataInicio, vData) <= 0) and (CompareDateTime(pCri.DataFim, vData) >= 0)) then break;
            Read(vUsuPosicao, sizeof(vUsuPosicao));
            vOrigem := LeStringAuditoria(AuditStream);
            continue;
          end;
          _opAdd,
          _opDelete,
          _opUpdate : begin
            vAuditPosition := AuditStream.Position - sizeof(vOperacao);
            Read(vLogPosition, sizeof(vLogPosition));
            Read(vIDObjeto, sizeof(vIDObjeto));
            vIdentificacaoObjeto := LeStringAuditoria(AuditStream);
            LogStream.Seek(vLogPosition , soBeginning);
            LogStream.Read(vListIndex, sizeof(vListIndex));
            vCla := pArq.Classes.Find(vListIndex);
            if vOperacao = _opUpdate then begin
              PropList := TList.Create;
              LogStream.Read(vIDObjeto, sizeof(vIDObjeto));
              LogStream.Read(vPropIndex, sizeof(vPropIndex));
              while vPropIndex <> ENDUPDATE do begin
                New(PropItem);
                PropItem.Prop := vCla.Propriedades.Find(vPropIndex);
                PropItem.PropValue := ReadAuditProperty(LogStream, PropItem.Prop);
                PropList.Add(PropItem);
                LogStream.Read(vPropIndex, sizeof(vPropIndex));
              end;
            end;
          end;
          _opAddAssociation : ;
          _opDeleteAssociation : ;
        end;
        if InCriterio then begin
          InAuditMode := true;
          BeginTransaction;
          //cria login
          v_Login := pArq.Logins.Find(arqLogin.Usuario + '.' + arqLogin.Maquina + '.' + arqLogin.Perfil);
          if v_Login = nil then begin
            v_Login := T_Aud_Login.Create;
            with v_Login do begin
              Posicao := arqLogin.Posicao;
              Usuario := arqLogin.Usuario;
              Nome    := arqLogin.Nome;
              Maquina := arqLogin.Maquina;
              Perfil  := arqLogin.Perfil;
              Arquivo := pArq;
              Add;
            end;
          end;
          //cria a origem
          vOri := _Aud_OrigemPorNomeList.Find(vOrigem);
          if vOri = nil then begin
            vOri := T_Aud_Origem.Create;
            vOri.Nome := vOrigem;
            vOri.Add;
          end;
          //cria a operacao
          case vOperacao of
            _opAdd, _opDelete, _opUpdate : begin
              vOpe := T_Aud_Operacao.Create;
              with vOpe do begin
                Operacao := vOperacao;
                Data := vData;
                IdentificacaoObjeto := vIdentificacaoObjeto;
                AuditPosicao := vAuditPosition;
                LogPosicao := vLogPosition;
                IDObjeto := vIDObjeto;
                Criterio := pCri;
                Arquivo := pArq;
                Login := v_Login;
                Classe := vCla;
                Origem := vOri;
                Package := vCla.Package;
                Add;
                //cria o update
                if PropList <> nil then
                  for I := 0 to PropList.Count - 1 do begin
                    PropItem := PropList[I];
                    vUpd := T_Aud_Update.Create;
                    vUpd.Propriedade := PropItem.Prop;
                    vUpd.Valor := PropItem.PropValue;
                    vUpd.Operacao := vOpe;
                    vUpd.Add;
                  end;
                FreeAndNil(PropList);
              end;
            end;
          end;
          EndTransaction;
        end;
      end;
    finally
      InAuditMode := false;
    end;
  end;
  AuditStream.Destroy;
  LogStream.Destroy;
end;
{$ENDIF}

procedure GerarPesquisaAuditoria(pCri : TObject);
{$IFNDEF AUDITORIA}
begin
end;
{$ELSE}
var
  ArqNome : string;
  vArq : T_Aud_Arquivo;

begin
  vArq := _Aud_ArquivoPorDataList.First;
  while vArq <> nil do begin
    if (CompareDateTime(vArq.Data, T_Aud_Criterio(pCri).DataInicio) >= 0) and
       (CompareDateTime(vArq.Data, T_Aud_Criterio(pCri).DataFim) <= 0) then begin
        ArqNome := vArq.Nome;
        CarregaTransacaoAuditoria(vArq, T_Aud_Criterio(pCri));
    end;
    _Aud_ArquivoPorDataList.Next(vArq);
  end;
end;
{$ENDIF}

{ Como recuperar o objeto

1. Insert
  a)   a) se o arquivo de auditoria estiver ativo e verifica na prevalencia, se tiver com a versão zero é ele
  b) senão: é o do snapshot ou do log
  --> basta gravar o ID e a LI
2. Delete
  a) está no snapshot + complementado com com todos os updates do log até o delete
  --> basta gravar o ID e a LI
3. Update
  a) está no snapshot + complementado com todos os updates do log, até o update em questão
  --> basta continuar gravando o que está sendo gravado hoje
}
{$IFDEF AUDITORIA}
type
  TObjectProp = packed record
    PropIndex : word;
    PropValue : variant;
  end;
  PObjectProp = ^TObjectProp;

function ResgataObjetoDoLog(Stream : TFileStream; pObj : T_Aud_Operacao) : TList;
var
  vProp : T_Aud_Propriedade;
  vOper : T_Aud_Operacao;
  PropItem : PObjectProp;
  PropList : TList;

  procedure MontaObjeto(pOper : T_Aud_Operacao); begin
    Stream.Seek((pOper.LogPosicao + sizeof(pOper.ListIndex)), soBeginning);
    vProp := pOper.Classe.Propriedades.First;
    while Assigned(vProp) do begin
      New(PropItem);
      PropItem.PropIndex := vProp.PropIndex;
      PropItem.PropValue := ReadAuditProperty(Stream, vProp);
      PropList.Add(PropItem);
      pOper.Classe.Propriedades.Next(vProp);
    end;
  end;

begin
  PropList := TList.Create;
  vProp := pObj.Classe.Propriedades.Last;
  with Stream do begin
    case pObj.Operacao of
      _opAdd : MontaObjeto(pObj);
      _opDelete : begin
        vOper := _Aud_OperacaoPorDataList.First;
        while Assigned(vOper) do begin
          if (vOper.Arquivo = pObj.Arquivo) and (vOper.Classe.ListIndex = pObj.Classe.ListIndex) and
             (vOper.IDObjeto = pObj.IDObjeto) then
          begin
            Result := ResgataObjetoDoLog(Stream, vOper);
          end;
          _Aud_OperacaoPorDataList.Next(vOper);
          if vOper.Data >= pObj.Data then break;
        end;
      end;
      _opUpdate:;
    end;
  end;
  Result := PropList;
  PropList.Free;
end;

function ResgataObjetoDoSnapShot(pObj : T_Aud_Operacao) : TList;
var
  I, J, ListCount : integer;
  HeaderFile : THeaderFile;
  SnapShotFileName : string;
  SnapShotStream : TFileStream;
  vClas : T_Aud_Classe;
  vProp : T_Aud_Propriedade;
  IsMyClas, IsMyObj : boolean;
  PropItem : PObjectProp;
  PropList : TList;
  PropValue : variant;

begin
  Result := nil;
  IsMyObj := false;
  SnapShotFileName := GetFilename(pObj.Arquivo, 'dat');
  SnapShotStream := TFileStream.Create(SnapShotFileName, fmOpenRead or fmShareDenyNone);
  try
    with SnapShotStream do begin
      Seek(4, soBeginning);
      TUnPPrevalentStream(SnapShotStream).LoadHeaderFile(HeaderFile);
      for I := 0 to high(HeaderFile.HeaderData) - 1 do
        with HeaderFile.HeaderData[I] do begin
          vClas := pObj.Arquivo.Classes.Find(ListIndex);
          IsMyClas := vClas.ListIndex = ListIndex;
          Read(ListCount, sizeof(ListCount));//tamanho da lista
          if ListCount <> 0 then begin
            PropList := TList.Create;
            for J := 0 to ListCount - 1 do begin
              vProp := pObj.Classe.Propriedades.First;
              while Assigned(vProp) do begin
                PropValue := ReadAuditProperty(SnapShotStream, vProp);
                if IsMyClas and (vProp.PropIndex = 0) and (PropValue = pObj.IDObjeto) then IsMyObj := true;
                if IsMyClas and IsMyObj then begin
                  New(PropItem);
                  PropItem.PropIndex := vProp.PropIndex;
                  PropItem.PropValue := PropValue;
                  PropList.Add(PropItem);
                end;
                pObj.Classe.Propriedades.Next(vProp);
              end;
              if IsMyClas and IsMyObj then begin
                Result := PropList;
                PropList.Free;
                exit;
              end;
            end;
          end;
        end;
    end;
  finally
    SnapShotStream.Free;
  end;
end;

function AtualizaImagemObjeto(pObj : T_Aud_Operacao; Prevalent : TPrevalent) : TPrevalent; begin
  Result := nil
end;
{$ENDIF}

{$IFNDEF AUDITORIA}
function ResgataObjetoOriginal(pObj : T_Aud_Operacao) : TObject; begin
  Result := nil;
end;
{$ELSE}
function ResgataObjetoOriginal(pObj : T_Aud_Operacao) : TList;
var
  I : integer;
  PrevalentList : TPrevalentList;
  LogFileName : string;
  LogStream : TFileStream;
  PropItemLog : PObjectProp;
  PropItemSnap : PObjectProp;
  PropListLog : TList;
  PropListSnap : TList;

  function ResgataDoLog : TList; begin
    LogFileName := GetFilename(pObj.Arquivo, 'log');
    if not FileExists(LogFileName) then raise EExtP.CreateFmt('O Arquivo de log <%s> não se encontra em disco.', [LogFileName]);
    LogStream := TFileStream.Create(LogFileName, fmOpenRead or fmShareDenyNone);
    try
      Result := ResgataObjetoDoLog(LogStream, pObj);
    finally
      LogStream.Free;
    end;
  end;

begin
  if (pObj.Data > Prevalence.StartupDate) and (pObj.Operacao = _opAdd) then begin //procura na prevalência
    PrevalentList := Prevalence.PrevalentLists(pObj.Classe.ListIndex);
    TPrevalent(Result) := TPrevalent(PrevalentList.Find(pObj.IDObjeto));
    if (Result <> nil) and (TPrevalent(Result).ThreadID = 0) then
      exit
    else
      Result := ResgataDoLog;
  end
  else begin
    PropListSnap := ResgataObjetoDoSnapShot(pObj);
    PropListLog := ResgataDoLog;
    //merge
    if PropListLog.Count > 0 then begin
      for I := 0 to PropListSnap.Count - 1 do begin
        PropItemLog := PropListLog[I];
        PropItemSnap := PropListSnap[I];
        if PropItemSnap.PropValue <> PropItemLog.PropValue then
          PropItemSnap.PropValue := PropItemLog.PropValue;
      end;
      Result := PropListSnap;
    end
    else
      Result := PropListLog;
  end;
end;
{$ENDIF}

{$IFNDEF AUDITORIA}
function RecuperaObjetoAuditoria(pObj : TObject) : TObject; begin
  Result := nil;
end;
{$ELSE}
procedure RecuperaObjetoAuditoria(pObj : TObject);
var
  vOpe : T_Aud_Operacao;
  vObj : TBrowserParams;
begin
  vOpe := T_Aud_Operacao(pObj);
  with T_Aud_Operacao(pObj) do
    if Operacao in [_opAdd, _opDelete] then ResgataObjetoOriginal(vOpe);
  vObj := TBrowserParams.Create;
  try
    with vObj do begin
      Caption := 'Detalhe de Operacao'#13 +
      '#13 Tipo: ' + GetEnumName(T_Operation, vOpe.Operacao) +
      '#13 Data: ' + FormatDateTime('hh:nn:ss.zzz dd/mm/yyyy', vOpe.Data) +
      '#13 Usuário: ' + upperCase(vOpe.Login.Usuario) + ' com Perfil: ' + upperCase(vOpe.Login.Perfil) +
      '#13 Origem da Operacao: ' + upperCase(vOpe.Origem.Nome) + ' Máquina: ' + upperCase(vOpe.Login.Maquina));
      Add('Detalhe de Operacao', tpString);
    end;
  finally
    vObj.Free;
  end;
end;
{
  vArq := T_Aud_Operacao(pObj).Arquivo;
  AuditFileName := GetFilename(vArq);
  if not FileExists(AuditFileName) then
    raise EExtP.CreateFmt('O Arquivo de auditoria <%s> não se encontra em disco.', [AuditFileName]);

  AuditStream := TFileStream.Create(AuditFileName, fmOpenRead or fmShareDenyNone);
  AuditStream.Seek(T_Aud_Operacao(pObj).AuditPosicao, soBeginning);


  with AuditStream do begin
    while Read(vOperacao, sizeof(vOperacao)) > 0 do
      case vOperacao of
        _opLogin : begin
          vString := LeStringAuditoria(AuditStream); //usuario
          vString := LeStringAuditoria(AuditStream); //nome
          vString := LeStringAuditoria(AuditStream); //maquina
          vString := LeStringAuditoria(AuditStream); //perfil
          continue;
        end;
        _opBeginTransaction : begin
          Read(vDataTrans, sizeof(vDataTrans));
          Read(vInteger, sizeof(vInteger));
          vString := LeStringAuditoria(AuditStream); //origem
          continue;
        end;
        _opAdd,
        _opDelete : begin
          Read(vLogPosicao, sizeof(vLogPosicao));
          Result := ResgataObjetoOriginal(T_Aud_Operacao(pObj), vLogPosicao);
          vString := LeStringAuditoria(AuditStream); //identificacao do objeto
          break;
        end;
        _opUpdate : begin
        end;
        _opAddAssociation : begin
        end;
        _opDeleteAssociation : begin
        end;
      end;
  end;
}
end;
{$ENDIF}
//salvo
{
  I, J, ListCount,
  Index,
  RecoveredLists,
  TamMetadados  : integer;
  Operation     : T_Operation;
  vSingle       : single;
  PropIndex,
  Tam           : byte;
  ListIndex,
  TargetList,
  PropCount     : word;
}

{
  //procura no snapshot
  FileName := copy(AuditFileName, 1, length(AuditFileName) - 4) + '.dat';
  if not FileExists(FileName) then
    raise EExtP.CreateFmt('O Arquivo de SnapShot <%s> não se encontra em disco.', [FileName]);

  Stream := TUnPPrevalentStream(TUnPPrevalentStream.Create(FileName, fmOpenRead or fmShareDenyNone));

  with Stream do begin
    Read(vSingle, sizeof(vSingle));
    Read(RecoveredLists, sizeof(RecoveredLists));
    Read(TamMetadados, sizeof(TamMetadados));
    inc(TamMetadados, Position);
    setlength(SnapShotData, RecoveredLists);
    J := 0;
    while Position < TamMetadados do begin
      with SnapShotData[J] do begin
        Read(Tam, sizeof(Tam));
        setlength(ClassName, Tam);
        Read(ClassName[1], Tam);
        Read(PropCount, sizeof(PropCount));
        setlength(Props, PropCount);
        for I := 0 to PropCount-1 do
          with Props[I] do begin
            Read(Tam, sizeof(Tam));
            setlength(Name, Tam);
            Read(Name[1], Tam);
            Read(Kind, sizeof(Kind));
            Read(SubKind, sizeof(SubKind));
          end;
        inc(J);
      end;
    end;

    for I := 0 to RecoveredLists - 1 do
      with SnapShotData[I] do begin
        PrevalentList := Prevalence.PrevalentLists(ClassName + 'List');
        Read(ListCount, sizeof(ListCount)); // tamanho da lista
        Prevalent := TPrevalentClass(PrevalentList.ObjectClass).Create;
        for J := 0 to ListCount - 1 do begin
          Prevalent := ReadAuditPrevalent(Prevalent, (ClassName = Prevalence.PrevalentLists(pLi).ObjectClass.ClassName));
          if (ClassName = Prevalence.PrevalentLists(pLi).ObjectClass.ClassName) and (TPrevalent(Result).ID = pId) then begin
            TPrevalent(Result) := Prevalent;
            FreeAndNil(Prevalent);
            exit;
          end;
        end;
        FreeAndNil(Prevalent);
      end;
  end;

  //procura no log
  FileName := copy(AuditFileName, 1, length(AuditFileName) - 4) + '.log';
  if not FileExists(FileName) then
    raise EExtP.CreateFmt('O Arquivo de Log <%s> não se encontra em disco.', [FileName]);

  Stream := TUnPPrevalentStream(TUnPPrevalentStream.Create(FileName, fmOpenRead or fmShareDenyNone));

  with Stream do begin
    Read(vSingle, sizeof(vSingle));
    while Read(Operation, sizeof(Operation)) > 0 do begin
      Read(ListIndex, sizeof(ListIndex));
      PrevalentList := Prevalence.PrevalentLists(ListIndex);
      Prevalent := TPrevalentClass(PrevalentList.ObjectClass).Create;
      case Operation of
        _opAdd : begin
          Prevalent := ReadAuditPrevalent(Prevalent, (ListIndex = pLi));
          if (ListIndex = pLi) and (TPrevalent(Result).ID = pId) then begin
            TPrevalent(Result) := Prevalent;
            FreeAndNil(Prevalent);
            case pObj.Operacao of
              _opAdd : begin
                exit;
              end;
              _opUpdate, _opDelete : begin
              end;
              _opAddAssociation, _opDeleteAssociation : begin
              end;
            end;
          end;
        end;
        _opUpdate : begin
          Read(Index, sizeof(Index));
          case pObj.Operacao of
            _opAdd: begin
              ReadAuditPrevalentUpdate(Prevalent, true);
            end;
            _opDelete : begin
              if (ListIndex = pLi) and (Index = pId) then begin
                TPrevalent(Result).DuplicateObject(Prevalent);
                ReadAuditPrevalentUpdate(Prevalent, false);
                Prevalent.DuplicateObject(TPrevalent(Result));
                FreeAndNil(Prevalent);
              end
              else
                ReadAuditPrevalentUpdate(Prevalent, true);
            end;
            _opUpdate : begin
              if (ListIndex = pLi) and (Index = pId) then begin
                TPrevalent(Result).DuplicateObject(Prevalent);
                ReadAuditPrevalentUpdate(Prevalent, false);
                Prevalent.DuplicateObject(TPrevalent(Result));
                FreeAndNil(Prevalent);
              end
              else
                ReadAuditPrevalentUpdate(Prevalent, true);
            end;
            _opAddAssociation, _opDeleteAssociation : begin
            end;
          end;

        end;
        _opAddAssociation, _opDeleteAssociation : begin
            Read(Index, sizeof(Index));
            Read(PropIndex, sizeof(PropIndex)); // índice da propriedade
            if Operation = _opAddAssociation then begin
              Read(TargetList, sizeof(TargetList));
              Read(Index, sizeof(Index));
            end
            else begin
              Read(Index, sizeof(Index));
            end
        end;
        _opDelete : begin
          Read(Index, sizeof(Index));
          if (ListIndex = pLi) and (TPrevalent(Result).ID = pId) then exit;
        end;
      end;
    end;
  end;

  if Result = nil then
    Browser.ShowMessage('Objeto não localizado.');
}
end.
