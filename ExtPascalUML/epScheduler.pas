{$I epDirectives.inc}

// Creation started in Feb 03 2006 by Walter Frederico Bauchspiess

unit epScheduler;

interface

uses
  Classes, SysUtils, TypInfo, DateUtils;//, Windows;

implementation

uses
  epUtils, epThread, epModel, epPrevalence, epProperties, epObjectList, epWorkFlow;

const
  SchedulerPoolingInterval = 5000; // 5s precision

type
  TepTask = class(TThread)
  private
    fTask: T_Task;
  protected
    procedure Execute; override;
  end;

  TepScheduler = class(TThread)
  private
    procedure StartScheduledTasks;
    procedure StartTask(const Task: T_Task);
  protected
    procedure Execute; override;
  public
    constructor Create;
    procedure Finish;
  end;

(*var
  Scheduler: TepScheduler;*)

{ TepTask }

procedure TepTask.Execute;
var
  fResult, fRunCommand: string;
  FlgLog: boolean;
  LogObj, AuxLogObj: T_Log;
  fTaskID: integer;
  Obj: TPrevalent;
  RunStart: TDateTime;
begin
  RunStart := Now;
  Prevalence.BeginThread;
  try
    IsRunningStateMachine := True;  // threadvar defined in epObjectList, disables security checks
    Inc(CallStateMachineCount); // in order to ignore security checks - simulate a running state machine
    fRunCommand := fTask.Method.Classe.Nome + '.' + fTask.Method.Nome;
    if fTask.RunObjectID > 0 then fRunCommand := fRunCommand + ' on object ' + IntToStr(fTask.RunObjectID);
    FlgLog := fTask.LogKind <> _rlkNone;
    if (fTask.LogKind = _rlkSingle) and (fTask.Log.Count > 0) then begin // erase previous Log
      BeginTransaction;
      try
        LogObj := fTask.Log.First;
        while Assigned(LogObj) do begin
          AuxLogObj := LogObj;
          fTask.Log.Next(LogObj);
          AuxLogObj.Delete;
        end;
        EndTransaction;
      except
        RollBack;
      end;
    end;
    if FlgLog then begin
      BeginTransaction;
      try
        fTask.Log.Add(AddLog('Started: ' {'Iniciando a execução: '} + fRunCommand));
        EndTransaction;
      except
        Rollback;
      end;
    end;
    fResult := '';
    fTaskID := fTask.ID;
    try
      if fTask is T_PendencyTimeOutTask then begin
        BeginTransaction;
        try
          Obj := fTask.RunObject; //   TPrevalent(Prevalence.PrevalentLists(fTask.Method.Classe.Nome + 'List').Find(fTask.RunObjectID));
          if Assigned(Obj) then CreatePendency(Obj, Obj.MethodAddress(fTask.Method.Nome));
          T_PendencyTimeOutTask(fTask).Pendency.Delete;
          EndTransaction;
        except
          try
            RollBack;
          except
            raise Exception.Create('Error executing timeout');
          end;
        end;
      end
{//*      else
        RunCommand('T' + fTask.Method.Classe.Nome, fTask.Method.Nome, fTask.RunObjectID);
      {if fTask is T_PendencyTimeOutTask then
        SourcePendency := T_PendencyTimeOutTask(fTask).Pendency
      else SourcePendency := nil;
      RunCommand('T' + fTask.Method.Classe.Nome, fTask.Method.Nome, fTask.RunObjectID);}
    except
      on E: Exception do fResult := E.Message;
    end;
    // try to find again: it could have been deleted by the "RunCommand"
    fTask := _TaskList.Find(fTaskID);
    if Assigned(fTask) then
      with fTask do begin
        BeginTransaction;
        try
          RunningThread := nil;
          // discard seconds, to be more precise, and because the smallest interval is 1 minute
          LastRan := RecodeSecond(RunStart, 0);
          //if RunTimes > 0 then
          //  RunTimes := RunTimes - 1;
          EndTransaction;
        except
          try RollBack; except end;
        end;
      end;
    if FlgLog then begin
      BeginTransaction;
      try
        if fResult = '' then
          fTask.Log.Add(AddLog('Finished: ' {'Finalizada a execução: '} + fRunCommand))
        else
          fTask.Log.Add(AddLog('Aborted: ' {'Abortada a execução: '} + fRunCommand + ' Exceção: ' + fResult));
        EndTransaction;
      except
        Rollback;
      end;
    end;
  finally
    Dec(CallStateMachineCount);
    try Prevalence.EndThread; except end;
  end;
end;

{ TepScheduler }

constructor TepScheduler.Create; begin
  inherited Create(True);
  FreeOnTerminate := True;
  Resume;
end;

procedure TepScheduler.Execute; begin
  // Wait for Prevalence start
  while not Terminated and Browser.Suspended do Sleep(200);
  Prevalence.BeginThread;
  try
    IsRunningStateMachine := True;  // threadvar defined in epObjectList, disables security checks
    Inc(CallStateMachineCount); // in order to ignore security checks - simulate a running state machine
    // Process scheduled tasks
    while not Terminated do begin
      try StartScheduledTasks except end;
      Sleep(100);
    end;
  finally
    Dec(CallStateMachineCount);
    try Prevalence.EndThread; except end;
  end;
end;

procedure TepScheduler.Finish; begin
  Terminate;
end;

procedure TepScheduler.StartScheduledTasks;
var
  LocalNow: TDateTime;

  procedure StartList(const List: TPrevalentList);
  var
    Task: T_Task;
  begin
    Task := T_Task(List.First);
    while Assigned(Task) and (Task.NextStart <= LocalNow) do
      with Task do begin
        if Active and not Running and (NextStart > 0) {and (RunTimes <> 0)} then
          try StartTask(Task) except end;
        List.Next(TTransient(Task));
      end;
  end;

begin
  LocalNow := Now;
  StartList(_TaskByNextStartList);
  StartList(_PendencyTimeOutTaskByNextStartList);
end;

procedure TepScheduler.StartTask(const Task: T_Task); begin
  Task.RunningThread := TepTask.Create(True);
  with TepTask(Task.RunningThread) do begin
    fTask := Task;
    FreeOnTerminate := True;
    Resume;
  end;
end;

initialization
//*  Scheduler := TepScheduler.Create;
//*  Scheduler.FreeOnTerminate := True;
//*  Scheduler.Resume;
finalization
//*  Scheduler.Finish;
end.
