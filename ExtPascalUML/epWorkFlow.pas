{$I epDirectives.inc}

// Creation started in Feb 08 2006 by Walter Frederico Bauchspiess

unit epWorkFlow;

interface

{.$define DisablePendencies}

uses
  SysUtils, epObjectList, epModel, epPrevalence, epCommon;

type
  T_PendencyPriority = (_pplow, _ppHigh);

  TPendency = class(T_Pendency)
  private
    function  GetProfile: string;
    procedure SetProfile(const Value: string);
    procedure Broadcast;
    function  PermittedProfiles: TObjectList;
    function  FindValidProfile(const ProfileName: string): T_Grupo;
    function GetDelegateProfile: string;
    procedure SetDelegateProfile(const Value: string);
  public
    property  Profile: string read GetProfile write SetProfile;
    procedure ChooseProfile;  // Permits the user to choose the assigned profile
    function  ValidProfile(const ProfileName: string): boolean;
    property  DelegateProfile: string read GetDelegateProfile write SetDelegateProfile;
  end;

  TPendencyList = class(TObjectList)
  private
    function GetProfile: string;
    procedure SetProfile(const Value: string);
    function GetDelegateProfile: string;
    function GetDelegateUser: string;
    procedure SetDelegateProfile(const Value: string);
    procedure SetDelegateUser(const Value: string);
  public
    // Creates a new Pendency
    function New(const Obj: TPrevalent; const RunProc: TObjectProcedure; const Priority: T_PendencyPriority = _ppLow;
      const TimeOut: TDateTime = 0; const RunOnTimeOut: TObjectProcedure = nil): TPendency;
    // Navigation functions
    function First : TPendency;
    function Last : TPendency;
    function Prior(var _Pendency : TPendency) : boolean;
    function Next(var _Pendency : TPendency) : boolean;
    // Sets or Gets profiles of all pendencies
    property Profile: string read GetProfile write SetProfile;
    // Permits the user to choose the assigned profiles of the pendencies
    procedure ChooseProfiles;
    property DelegateProfile: string read GetDelegateProfile write SetDelegateProfile;
    property DelegateUser: string read GetDelegateUser write SetDelegateUser;
  end;

  TJoinPendencyList = class(TPendencyList) // Important: no data should be introduced
  public
    procedure Broadcast;
  end;

  TSequence = class
  private
    function GetValue(const Name: string): integer;
    procedure SetValue(const Name: string; const Value: integer);
  public
    property Value[const Name: string]: integer read GetValue write SetValue; default;
    function Next(const Name: string; const Increment: integer = 1): integer;
  end;

threadvar
  SourcePendency: T_Pendency;

function Sequence: TSequence;
function AddLog(const aText: string): T_Log;

procedure CreatePendency(const Obj: TPrevalent; const RunProc: Pointer; const Priority: T_PendencyPriority = _ppLow;
  const TimeOut: TDateTime = 0; const RunOnTimeOut: Pointer = nil); overload;

procedure CreatePendency(const Obj: TPrevalent; const RunProc: TObjectProcedure; const Priority: T_PendencyPriority = _ppLow;
  const TimeOut: TDateTime = 0; const RunOnTimeOut: TObjectProcedure = nil); overload;

procedure ClearPendencies(const Obj: TPrevalent); overload;
procedure ClearPendencies(const List: TObjectList); overload;
function  EndPendency(const Obj: TPrevalent): boolean; // returns if the flow can continue (there are no other pendencies)
function  ExistPendency(const Obj: TPrevalent): boolean; // Note: only considers Pendencies different than the SourcePendency
procedure KeepPendency;
procedure ExecutePendency(const ID: integer);
procedure ChoosePendencyProfile;
//procedure WorkFlowTest;
function After(const aAfter: double): boolean;
procedure BroadcastDelegatedPendencies;

implementation

uses
  epUtils, epProperties, epServer, epThread;

threadvar
  fKeepPendency: boolean;

type
  TUnpTransaction = class(TTransaction);
  TUnp_Pendency = class(T_Pendency);

var
  fSequence: TSequence;

function Sequence: TSequence; begin
  Result := fSequence;
end;

procedure DoNotifyNewPendency(const pThread: TepThread); begin
  //NotifyNewPendency;
end;

procedure ThreadNotifyNewPendencyByMethod(const Thread: TepThread; Param: pointer; var Continue: boolean); begin
  with T_Metodo(Param) do
    if GetMetodoPerm(Classe.Package.Nome, Classe.Nome, Nome, Thread.UserInfo.Profile) then
      DoNotifyNewPendency(Thread);
end;

procedure ThreadNotifyNewPendencyToProfile(const Thread: TepThread; Param: pointer; var Continue: boolean); begin
  if SameText(Thread.UserInfo.Profile, T_Grupo(Param).Nome) and SameText(Thread.UserInfo.UserDomain, T_Grupo(Param).Dominio.Nome) then
    DoNotifyNewPendency(Thread);
end;

function GetPendencyByID(const ID: integer): T_Pendency; begin
  Inc(CallStateMachineCount); // in order to use security as in a State Machine
  try
    Result := _PendencyList.Find(ID);
  finally
    Dec(CallStateMachineCount);
  end;
end;

{ TPendency }

procedure TPendency.ChooseProfile;
var
  Profiles: TObjectList;
begin
  Profiles := PermittedProfiles;
  try
    if Profiles.Count > 1 then
      AssignedProfile := T_Grupo(Browser.Choose(Profiles, 'Nome', 'Escolha o perfil', 'Escolha o perfil que deverá executar "'
        + MethodAlias + '" em "' + ObjIdentification + '":', [bbFinish]));
  finally
    Profiles.Free;
  end;
end;

function TPendency.FindValidProfile(const ProfileName: string): T_Grupo;
var
  Profiles: TObjectList;
begin
  Profiles := PermittedProfiles;
  try
    Result := T_Grupo(Profiles.Find(ProfileName));
  finally
    Profiles.Free;
  end;
end;

function TPendency.GetProfile: string; begin
  if Assigned(AssignedProfile) then
    Result := AssignedProfile.Nome
  else
    Result := '';
end;

function TPendency.PermittedProfiles: TObjectList; begin
  Result := _GrupoPorNomeList.CreateView;
  GetMetodoProfiles(Method, Result);
end;

procedure TPendency.Broadcast; begin
  if AssignedProfile = nil then
//*    ObjectServer.RunOnAllThreads(ThreadNotifyNewPendencyByMethod, Method)
  else
//*    ObjectServer.RunOnAllThreads(ThreadNotifyNewPendencyToProfile, AssignedProfile);
end;

procedure TPendency.SetProfile(const Value: string); begin
  if Value <> '' then begin
    AssignedProfile := FindValidProfile(Value);
    if AssignedProfile = nil then raise Exception.Create('Invalid profile name: ' + Value);
  end
  else
    AssignedProfile := nil;
end;

function TPendency.ValidProfile(const ProfileName: string): boolean; begin
  Result := FindValidProfile(ProfileName) <> nil;
end;

function TPendency.GetDelegateProfile: string; begin
  if Assigned(Delegate) then
    Result := Delegate.Nome
  else
    Result := '';
end;

procedure TPendency.SetDelegateProfile(const Value: string);
var
  Profile: T_Grupo;
begin
  if Value = '' then begin
    Delegate := nil;
    exit;
  end;
  Profile := _GrupoPorNomeList.Find(Value);
  if Profile = nil then raise Exception.Create('Perfil ''' + Value + ''' inexistente.');
  //if AssociationConstraintDelegate.Find(Profile) = nil then
  //  raise Exception.Create('Não é permitida a delegação para ''' + Value + ''', conforme definido no sistema de segurança.');
  Delegate := Profile;
end;

{ TPendencyList }

procedure TPendencyList.ChooseProfiles;
var
  Pendency: TPendency;
begin
  Pendency := First;
  while Assigned(Pendency) do begin
    Pendency.ChooseProfile;
    Next(Pendency);
  end;
end;

function TPendencyList.First: TPendency; begin
  Result := TPendency(inherited First);
end;

function TPendencyList.GetDelegateProfile: string;
var
  Pendency: TPendency;
begin
  Pendency := First;
  if Assigned(Pendency) then
    Result := Pendency.DelegateProfile
  else
    Result := '';
end;

function TPendencyList.GetDelegateUser: string;
var
  Pendency: TPendency;
begin
  Pendency := First;
  if Assigned(Pendency) then
    Result := Pendency.DelegateUser
  else
    Result := '';
end;

function TPendencyList.GetProfile: string;
var
  Pendency: TPendency;
begin
  Pendency := First;
  if Assigned(Pendency) then
    Result := Pendency.Profile
  else
    Result := '';
end;

function TPendencyList.Last: TPendency; begin
  Result := TPendency(inherited Last);
end;

function FindMethod(const Obj: TPrevalent; const Method: Pointer): T_Metodo;
var
  Classe: T_Classe;
begin
  Classe := _ClassePorNomeList.Find(Copy(Obj.ClassName, 2, MaxInt));
  if Assigned(Classe) then
    Result := T_Metodo(Get_Metodo(Classe, Obj.MethodName(Method)))
  else
    raise Exception.Create('Classe "' + Obj.ClassName + '" da pendência não encontrada');
  if Result = nil then
    raise Exception.Create('Método da pendência não encontrado na classe "' + Obj.ClassName + '".');
end;

function TPendencyList.New(const Obj: TPrevalent; const RunProc: TObjectProcedure; const Priority: T_PendencyPriority = _ppLow;
    const TimeOut: TDateTime = 0; const RunOnTimeOut: TObjectProcedure = nil): TPendency;
begin
  {$ifdef DisablePendencies} exit; {$endif}
  if ((TimeOut = 0) and Assigned(RunOnTimeOut)) or ((TimeOut <> 0) and not Assigned(RunOnTimeOut)) then
    raise Exception.Create ('Invalid TimeOut defined');
  Result := TPendency(T_Pendency.Create);
  try
    //Result.Caption := Caption;
    Result.Method := FindMethod(Obj, @RunProc);
    //Result.Priority := Priority;
    Result.RunObjectID := Obj.ID;
    Result.Add;
    if TimeOut <> 0 then begin
      Result.Timeout := T_PendencyTimeoutTask.Create;
      with Result.Timeout do begin
        Pendency := Result;
        Start := TimeOut;
        Method := FindMethod(Obj, @RunOnTimeOut);
        RunObjectID := Obj.ID;
        Add;
      end;
    end;
    Add(Result);
  except
    TUnp_Pendency(Result).InternalFree;
    raise;
  end;
end;

function TPendencyList.Next(var _Pendency: TPendency): boolean; begin
  Result := inherited Next(TTransient(_Pendency));
end;

function TPendencyList.Prior(var _Pendency: TPendency): boolean; begin
  Result := inherited Prior(TTransient(_Pendency));
end;

procedure TPendencyList.SetDelegateProfile(const Value: string);
var
  Pendency: TPendency;
begin
  Pendency := First;
  while Assigned(Pendency) do begin
    Pendency.DelegateProfile := Value;
    Next(Pendency);
  end;
end;

procedure TPendencyList.SetDelegateUser(const Value: string);
var
  Pendency: TPendency;
begin
  Pendency := First;
  while Assigned(Pendency) do begin
    Pendency.DelegateUser := Value;
    Next(Pendency);
  end;
end;

procedure TPendencyList.SetProfile(const Value: string);
var
  Pendency: TPendency;
begin
  Pendency := First;
  while Assigned(Pendency) do begin
    Pendency.Profile := Value;
    Next(Pendency);
  end;
end;

{ TJoinPendencyList }

procedure TJoinPendencyList.Broadcast;
var
  Pendency: TPendency;
begin
  try
    // Notify all threads of the new pendencies
    Pendency := First;
    while Assigned(Pendency) do begin
      Pendency.Broadcast;
      Next(Pendency);
    end;
  except
  end;
end;

{ End of TPendencyList }

function After(const aAfter: double): boolean; begin
  Result := False;
end;

function AddLog(const aText: string): T_Log; begin
  BeginTransaction;
  try
    Result := T_Log.Create;
    with Result do begin
      DateTime := Now;
      Text := aText;
      Add;
    end;
    EndTransaction;
  except
    Result := nil;  // only to supress warning
    RollBack;
  end;
end;

procedure CreatePendency(const Obj: TPrevalent; const RunProc: Pointer; const Priority: T_PendencyPriority = _ppLow;
  const TimeOut: TDateTime = 0; const RunOnTimeOut: Pointer = nil); overload;
var
  Result: T_Pendency;
  //ProfileName: string;
  //Profile: T_Grupo;
begin
  {$ifdef DisablePendencies} exit; {$endif}
  if ((TimeOut = 0) and Assigned(RunOnTimeOut)) or ((TimeOut <> 0) and not Assigned(RunOnTimeOut)) then
    raise Exception.Create ('Invalid TimeOut defined');
{  ProfileName := ObjectServer.ConnectionThread.fPendencyProfile;
  if ProfileName <> '' then
  begin
    Profile := _GrupoPorNomeList.Find(ProfileName);
    if Profile = nil then
      raise Exception.Create('Profile "' + ProfileName + '" wasn''t found trying to create a pendency');
  end
  else Profile := nil; }
  Result             := T_Pendency.Create;
  //Result.Caption     := Caption;
  Result.Method      := FindMethod(Obj, RunProc);
  //Result.Priority    := Priority;
  Result.RunObjectID := Obj.ID;
  //Result.AssignedProfile := Profile;
  Result.Add;
  if TimeOut <> 0 then begin
    Result.Timeout := T_PendencyTimeoutTask.Create;
    with Result.Timeout do begin
      Pendency := Result;
      Start := TimeOut;
      Method := FindMethod(Obj, RunOnTimeOut);
      RunObjectID := Obj.ID;
      Add;
    end;
  end;
  // Notify all threads that can execute the method
//*  ObjectServer.RunOnAllThreads(ThreadNotifyNewPendencyByMethod, Result.Method);
end;

procedure CreatePendency(const Obj: TPrevalent; const RunProc: TObjectProcedure;
  const Priority: T_PendencyPriority = _ppLow; const TimeOut: TDateTime = 0; const RunOnTimeOut: TObjectProcedure = nil); overload;
begin
  CreatePendency(Obj, @RunProc, Priority, TimeOut, @RunOnTimeOut);
end;

procedure ClearPendencies(const Obj: TPrevalent); overload;
var
  Pendency, Aux: T_Pendency;
  ObjClassName: string;
begin
  {$ifdef DisablePendencies} exit; {$endif}
  if Obj = nil then exit;
  ObjClassName := Copy(Obj.ClassName, 2, MaxInt);
  BeginTransaction;
  try
    Pendency := _PendencyByRunObjectList.Near(ObjClassName + ':' + IntToStr(Obj.ID));
    while Assigned(Pendency) and SameText(Pendency.Method.Classe.Nome, ObjClassName) and (Pendency.RunObjectID = Obj.ID) do begin
      Aux := Pendency;
      _PendencyByRunObjectList.Next(Pendency);
      Aux.Delete;
      if Aux = SourcePendency then SourcePendency := nil;
    end;
    EndTransaction;
  except
    Rollback;
  end;
end;

procedure ClearPendencies(const List: TObjectList); overload;
var
  Obj: TTransient;
begin
  Obj := List.First;
  while Assigned(Obj) do begin
    if Obj is TPrevalent then ClearPendencies(TPrevalent(Obj));
    List.Next(Obj);
  end;
end;

function EndPendency(const Obj: TPrevalent): boolean; begin
  {$ifdef DisablePendencies} Result := True; exit; {$endif}
  Result := not ExistPendency(Obj);
  if not fKeepPendency and Assigned(SourcePendency) then begin
    BeginTransaction;
    try
      SourcePendency.Delete;
      EndTransaction;
      SourcePendency := nil;
    except
      Rollback;
    end;
  end;
  //Result := not ExistPendency(Obj);
end;

function ExistPendency(const Obj: TPrevalent): boolean;
var
  Pendency: T_Pendency;
  ObjClassName: string;
begin
  {$ifdef DisablePendencies} Result := False; exit; {$endif}
  {if Obj = nil then
  begin
    Result := False;
    exit;
  end;}
  ObjClassName := Copy(Obj.ClassName, 2, MaxInt);
  Pendency := _PendencyByRunObjectList.Find(ObjClassName + ':' + IntToStr(Obj.ID));
  Result := Assigned(Pendency);
  if Result and (Pendency = SourcePendency) then begin
    _PendencyByRunObjectList.Next(Pendency);
    Result := Assigned(Pendency) and SameText(Pendency.Method.Classe.Nome, ObjClassName) and (Pendency.RunObjectID = Obj.ID);
  end;
end;

procedure KeepPendency; begin
  fKeepPendency := True;
end;

(*procedure WorkFlowTest;
var
  TipoPessoa,
  PessoaFisica,
  PessoaJuridica,
  Contato,
  Assinatura: TBrowserParams;
  Tela: integer;
  Ok: boolean;
begin
  TUnpBrowser(Browser).WorkFlowBegin;
  Ok := False;
  TipoPessoa := TBrowserParams.Create;
  PessoaFisica := TBrowserParams.Create;
  PessoaJuridica := TBrowserParams.Create;
  Contato := TBrowserParams.Create;
  Assinatura := TBrowserParams.Create;
  try
    with TipoPessoa do
    begin
      Add('Tipo da pessoa', tpEnumeration).Values := 'Física'#13'Jurídica';
      Add('Tipo da conta', tpEnumeration).Values := 'Corrente'#13'Poupança'#13'Investimento'#13'Salário'#13'Poupança salário'#13'Social';
      {with Add('Indicador', tpDouble) do
      begin
        Value := 50;
        Mask := 'meter max=100 high=70 low=30';
      end;}
      Caption := 'Selecione o tipo da pessoa que deseja abrir a conta';
    end;
    with PessoaFisica do
    begin
      Add('Nome', tpString);
      Add('Data de Nascimento', tpDate);
      Add('Identidade', tpString);
      Add('Renda', tpCurr);
      Add('Foto', tpString).Mask := 'Image';
      Caption := 'Favor entrar com os dados pessoais';
    end;
    with PessoaJuridica do
    begin
      Add('Razão social', tpString);
      Add('Nome de fantasia', tpString);
      Add('Data de fundação', tpDate);
      //Add('Identidade', tpString);
      Add('Capital social', tpCurr);
      Caption := 'Favor entrar com os dados da empresa';
    end;
    with Contato do
    begin
      Add('Endereço', tpString);
      Add('Telefone', tpString);
      Caption := 'Favor entrar com os dados para contato';
    end;
    with Assinatura do
    begin
      Add('Assinatura', tpString).Mask := 'Image';
      Caption := 'Favor digitalizar a assinatura';
    end;

    Tela := 1;
    repeat
      case Tela of
        1: case TipoPessoa.Execute of
             bbNext: if TipoPessoa.Param[0].Value = 0 then Tela := 2
                     else Tela := 3;
             else Tela := 0;
           end;
        2: case PessoaFisica.Execute([bbBack, bbNext, bbCancel]) of
             bbNext: Tela := 4;
             bbBack: Tela := 1;
             else Tela := 0;
           end;
        3: case PessoaJuridica.Execute([bbBack, bbNext, bbCancel]) of
             bbNext: Tela := 4;
             bbBack: Tela := 1;
             else Tela := 0;
           end;
        4: case Contato.Execute([bbBack, bbNext, bbCancel]) of
             bbNext: Tela := 5;
             bbBack: if TipoPessoa.Param[0].Value = 0 then Tela := 2
                     else Tela := 3;
             else Tela := 0;
           end;
        5:  case Assinatura.Execute([bbBack, bbNext, bbCancel]) of
             bbNext: Tela := 6;
             bbBack: Tela := 4;
             else Tela := 0;
           end;
        6: begin
             if Browser.Confirm('Confirma abertura da conta?') then
               Browser.ShowMessage('Conta aberta com sucesso!');
             Tela := 0;
             Ok := True;
           end;
      end;
    until Tela = 0;
  finally
    TipoPessoa.Free;
    PessoaFisica.Free;
    PessoaJuridica.Free;
    Contato.Free;
    Assinatura.Free;
  end;
  Browser.WorkFlowEnd;
  if not Ok then Abort;
end; *)

procedure ExecutePendency(const ID: integer);
var
  MustAbort: boolean;
begin
//*  TUnpBrowser(Browser).WorkFlowBegin;
  try
    MustAbort := False;
    BeginTransaction;
    fKeepPendency  := False;
    SourcePendency := GetPendencyByID(ID);
    if Assigned(SourcePendency) then
      try
        if SourcePendency.RunningThread <> nil then
          raise Exception.Create('Esta pendência não pode ser executada pois já se encontra em execução por ' +
            TepThread(SourcePendency.RunningThread).UserInfo.UserName + '.');
//*        SourcePendency.RunningThread := ObjectServer.ConnectionThread;
        try
          if SourcePendency.RunObject <> nil then
//*            RunCommand('T' + SourcePendency.Method.Classe.Nome, SourcePendency.Method.Nome, SourcePendency.RunObjectID)
          else begin
            Browser.MessageDlg('Não foi encontrado o item ao qual esta pendência está ligada.'#13'A pendência será automaticamente apagada.', mtInformation);
            MustAbort := True; // Only to force a refresh
          end;
          if (SourcePendency <> nil) and not fKeepPendency then SourcePendency.Delete;
        finally
          SourcePendency := GetPendencyByID(ID); // Locates again: only for safety reasons: redudancy
          if Assigned(SourcePendency) then SourcePendency.RunningThread := nil;
        end;
      finally
        SourcePendency := nil;
      end
    else
      Browser.MessageDlg('Esta pendência já foi executada por outro usuário, deixando de existir.', mtInformation);
    EndTransaction(true);
    if MustAbort then Abort;  // Only to force a refresh
//*    TUnpBrowser(Browser).WorkFlowEnd;
  except
    on E: Exception do //*TUnpBrowser(Browser).WorkFlowEnd(E);
  end;
end;

procedure BroadcastDelegatedPendencies;
var
  Upd: TUpdatedPrevalent;
begin
  if Assigned(ThreadTrans) then
  with TUnpTransaction(ThreadTrans) do begin
    Upd := Updates.First as TUpdatedPrevalent;
    while Assigned(Upd) do begin
      if Upd.NewImage is T_Pendency then
        with T_Pendency(Upd.NewImage) do
          if Delegate <> nil then // Notify all threads that have the delegated profile
//*            ObjectServer.RunOnAllThreads(ThreadNotifyNewPendencyToProfile, Delegate);
      Updates.Next(TTransient(Upd));
    end;
  end;
end;

procedure ChoosePendencyProfile; begin
  //Browser.Choose(
end;

{ TSequence }

function TSequence.GetValue(const Name: string): integer; begin
  Result := T_Sequence.GetValue(Name);
end;

function TSequence.Next(const Name: string; const Increment: integer): integer; begin
  Result := T_Sequence.Next(Name, Increment);
end;

procedure TSequence.SetValue(const Name: string; const Value: integer); begin
  T_Sequence.SetValue(Name, Value);
end;

initialization
  fSequence := TSequence.Create;
finalization
  fSequence.Free;
end.
