{$I epDirectives.inc}

unit epStateMachine;

interface

uses epCommon;

type
  TProc = procedure({$IFDEF FPC}Dummy,{$ENDIF}Context : pointer); 
  TFunc = function ({$IFDEF FPC}Dummy,{$ENDIF}Context : pointer) : boolean;

  TState = class
  private
    Name    : string;
    Action  : TProc;
    Buttons : TBrowserButtons;
    constructor Create(pName : string; pAction : TProc; pButtons : TBrowserButtons; pID : string = '');
  end;

  TTransition = class
  private
    Name      : string;
    NextState : integer;
    Condition : TFunc;
    constructor Create(pName : string; pCondition : TFunc; pNextState : integer; pID : string = '');
  end;

  TStateMachine = class
  private
    Package,
    ClassName,
    Method      : string;
    State,
    Transition  : integer;
    States      : array of TState;
    Transitions : array of array of TTransition;
    procedure RaiseError(Msg : string);
  public
    constructor Create(pPackage, pClassName, pMethod : string; TransitionsPerState : array of integer);
    procedure SetState(State : integer; Name : string; Action : pointer; Buttons : TBrowserButtons = []; pID : string = '');
    procedure SetTransition(State, Transition : integer; Name : string; Condition : pointer; NextState : integer; pID : string = '');
    procedure Execute(Context : pointer; Wizard : boolean = false);
    function FixButtons(OriginalButtons : TBrowserButtons) : TBrowserButtons;
  end;

implementation

uses
  SysUtils, epThread, epObjectList, epPrevalence, epWorkFlow;

const
  FINAL_STATE = -1;

constructor TStateMachine.Create(pPackage, pClassName, pMethod : string; TransitionsPerState : array of integer);
var
  I : integer;
begin
  Package   := pPackage;
  ClassName := pClassName;
  Method    := pMethod;
  setlength(States, high(TransitionsPerState) + 1);
  setlength(Transitions, high(TransitionsPerState) + 1);
  for I := 0 to high(TransitionsPerState) do
    setlength(Transitions[I], TransitionsPerState[I]);
end;

procedure TStateMachine.SetState(State : integer; Name : string; Action : pointer; Buttons : TBrowserButtons = []; pID : string = ''); begin
  States[State] := TState.Create(Name, TProc(Action), Buttons, pID);
end;

procedure TStateMachine.SetTransition(State, Transition : integer; Name : string; Condition : pointer; NextState : integer; pID : string = ''); begin
  Transitions[State, Transition] := TTransition.Create(Name, TFunc(Condition), NextState, pID);
end;

procedure TStateMachine.RaiseError(Msg : string); begin
  if ExceptObject <> nil then
    if ExceptObject is EAbort then
      Abort
    else
      with Exception(ExceptObject) do
        Prevalence.RollBack('Erro em Máquina de Estado'^M^J + ClassName + ': ' + Message +
                            ^M^J'Máquina: ' + Package + '.T' + Self.ClassName + '.' + Method + ^M^J + Msg)
  else
    Prevalence.RollBack('Erro em Máquina de Estado'^M^J'Máquina: ' + Package +
      '.T' + Self.ClassName + '.' + Method + ^M^J + Msg)
end;

threadvar
  History  : array of array of record
    NState : integer;
    PState : TState;
  end;
  PHistory : array of integer;

procedure TStateMachine.Execute(Context : pointer; Wizard : boolean = false);
var
  lTransition : TTransition;
  LastState   : integer;
  DoCondition : boolean;
  I           : integer;

  procedure IncHistory;
  var
    I : integer;
  begin
    if States[State].Buttons <> [] then begin
      for I := 0 to PHistory[CallStateMachineCount-1]-1 do
        if History[CallStateMachineCount-1, I].NState = State then exit;
      with History[CallStateMachineCount-1, PHistory[CallStateMachineCount-1]] do begin
        NState := State;
        PState := States[State];
      end;
      inc(PHistory[CallStateMachineCount-1])
    end;
  end;

begin
  State := 0; Transition := 0; LastState := 0;
  if Wizard then begin
    setlength(PHistory, CallStateMachineCount);
    setlength(History,  CallStateMachineCount);
    setlength(History[CallStateMachineCount-1], length(States));
    PHistory[CallStateMachineCount-1] := 0;
    IncHistory;
  end;
  try
    try
      if @States[0].Action <> nil then States[0].Action({$IFDEF FPC}nil,{$ENDIF}Context);
    except
      RaiseError('Estado: ' + States[State].Name)
    end;
    repeat
      lTransition := Transitions[State, Transition];
      DoCondition := true;
      try
        if @lTransition.Condition <> nil then DoCondition := lTransition.Condition({$IFDEF FPC}nil,{$ENDIF}Context);
      except
        RaiseError('Transição: ' + Transitions[State, Transition].Name + ', após Estado: ' + States[State].Name);
      end;
      if DoCondition then begin
        LastState := State;
        State := lTransition.NextState;
        if State = FINAL_STATE then break;
        try
          if Wizard then IncHistory;
          if @States[State].Action <> nil then States[State].Action({$IFDEF FPC}nil,{$ENDIF}Context);
          if Wizard then
            while Browser.Action = bbBack do
              if PHistory[CallStateMachineCount-1] > 0 then begin
                dec(PHistory[CallStateMachineCount-1]);
                State := History[CallStateMachineCount-1, PHistory[CallStateMachineCount-1]-1].NState;
                States[State].Action({$IFDEF FPC}nil,{$ENDIF}Context);
              end
              else
                exit;
        except
          RaiseError('Estado: ' + States[State].Name + ', depois da Transição: ' + Transitions[LastState, Transition].Name + ', do Estado: ' + States[LastState].Name);
        end;
        Transition := 0;
      end
      else begin
        inc(Transition);
        if Transition > high(Transitions[State]) then
          RaiseError('Falta transição saindo do Estado: ' + States[State].Name + ' e depois da Transição: ' + Transitions[State, Transition-1].Name);
      end;
    until false;
    if Wizard and (pos('Join', States[LastState].Name) = 0) then
      for I := 0 to High(States) do
        if pos('Join', States[LastState].Name) <> 0 then
        begin
          ClearPendencies(TPrevalent(Context^));
          break;
        end;
  finally
    if Wizard then begin
      setlength(PHistory, CallStateMachineCount-1);
      setlength(History,  CallStateMachineCount-1);
    end
  end;
end;

function TStateMachine.FixButtons(OriginalButtons : TBrowserButtons) : TBrowserButtons; begin
  if (CallStateMachineCount < 2) or (PHistory[0] < 1) then
    Result := OriginalButtons
  else
    if History[0, PHistory[0]-1].PState <> nil then
      Result := History[0, PHistory[0]-1].PState.Buttons
    else
      Result := OriginalButtons;
end;

constructor TState.Create(pName: string; pAction: TProc; pButtons : TBrowserButtons; pID : string = ''); begin
  Name    := pName;
  Action  := pAction;
  Buttons := pButtons;
end;

constructor TTransition.Create(pName: string; pCondition : TFunc; pNextState : integer; pID : string = ''); begin
  Name      := pName;
  Condition := pCondition;
  NextState := pNextState;
end;

end.
