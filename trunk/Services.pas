unit Services;
{
Slim Services and EventLog support for Windows
@author Wanderlan Santos dos Anjos
@data 27-jun-2008
@license BSD
}
interface

uses WinSvc, Classes;

{$R *.res}

type
	TFuncBool  = function : boolean;
  TEventType = (EventError = 1, EventWarning = 2, EventInformation = 4);

  TService = class
  private
  	FName       : pchar;
    FDescription,
	  FParamStr   : string;
    FManager,
    FService    : SC_Handle;
    FTimeout,
    FExitCode,
	  FParamCount : integer;
	  FSource     : THandle;
	  FStatus     : TServiceStatus;
	  FStopEvent  : THandle;
    FReportStartStop : boolean;
	  FStatusHandle    : Service_Status_Handle;
    FServiceThreads  : array[0..10] of TThread;
    FMaxThreads	     : integer;
    FServiceBegin,
    FServiceEnd		   : TFuncBool;
    procedure StopNow;
    function GetName : string;
    function ReportNoError(Estado : integer) : boolean;
    function ReportServiceStatus(CurrentState, Win32ExitCode, CheckPoint, WaitHint: integer): boolean;
  public
    constructor Create(ServiceName : string; Description : string = '');
    destructor Destroy; override;
    function GetServiceError: integer;
    function GetServiceErrorMessage: string;
    function GetState : cardinal;
    function  Install : boolean;
    function  Uninstall : boolean;
    procedure Insert(Exec: string);
    procedure Delete;
    function  Run(ServThreads : array of TThread; ServBegin : TFuncBool = nil; ServEnd : TFuncBool = nil) : boolean;
    function  Exists : boolean;
    function  Stop : integer;
    function  Start : integer;
    function  ReportStart : boolean;
    function  ReportStop  : boolean;
    procedure ReportEventLog(EventType : TEventType; EventCode : word; Message : string);
    procedure Reset;
    property Timeout : integer read FTimeout write FTimeout;
    property ExitCode : integer read FExitCode write FExitCode;
    property Name : string read GetName;
    property ParamStr : string read FParamStr;
    property ParamCount : integer read FParamCount;
  end;

var
  Service : TService;

implementation

uses
	Windows, SysUtils, Registry;

function TService.GetName : string; begin
  Result := string(FName);
end;

procedure TService.Reset; begin
  CloseServiceHandle(FService);
  FService := 0;
end;

function TService.Exists : boolean; begin
	Result := FService <> 0;
end;

procedure TService.Insert(Exec : string); begin
  FService := CreateService(FManager, FName, FName, SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, SERVICE_AUTO_START,
                            SERVICE_ERROR_NORMAL, pchar(Exec), nil, nil, nil, nil, nil);
  if not Exists then RaiseLastOSError;
  with TRegistry.Create do begin
    Access  := KEY_ALL_ACCESS;
    RootKey := HKey_Local_Machine;
    OpenKey('\SYSTEM\CurrentControlSet\Services\EventLog\Application\' + FName, true);
    WriteString('EventMessageFile', Exec);
    OpenKey('\SYSTEM\CurrentControlSet\Services\' + FName, true);
    WriteString('Description', FDescription);
    Free;
  end;
end;

function TService.Install : boolean; begin
  if FindCmdLineSwitch('INSTALL', ['-', '/'], true) and (FService = 0) then begin
    Insert(system.ParamStr(0));
    Result := true;
  end
  else
    Result := false
end;

procedure TService.Delete; begin
  if not Exists then RaiseLastOSError;
  if not DeleteService(FService) then RaiseLastOSError;
  with TRegistry.Create do begin
    Access  := KEY_ALL_ACCESS;
    RootKey := HKey_Local_Machine;
    DeleteKey('\SYSTEM\CurrentControlSet\Services\EventLog\Application\' + FName);
    Free;
  end;
end;

function TService.Uninstall : boolean; begin
  if FindCmdLineSwitch('UNINSTALL', ['-', '/'], true) then begin
    Delete;
    Result := true;
  end
  else
    Result := false
end;

function TService.GetServiceError : integer; begin
  Result := GetLastError;
  if Result = 0 then Result := -1
end;

function TService.GetServiceErrorMessage : string; begin
  Result := SysErrorMessage(GetServiceError)
end;

// Pára o Service, devolve 0 se conseguiu pará-lo ou o GetLastError
function TService.Stop : integer; begin
	Result := 0;
  if Exists then begin
		if not WinSvc.ControlService(FService, SERVICE_CONTROL_STOP, FStatus) then Result := GetLastError;
	end
	else
		Result := GetServiceError;
end;

// Inicia o Service, devolve 0 se conseguiu estartá-lo ou o GetLastError
function TService.Start : integer;
const
	Param : pchar = nil;
begin
	Result := 0;
  if FService = 0 then FService := OpenService(FManager, FName, SERVICE_ALL_ACCESS);
	if Exists then begin
	  if not StartServiceA(FService, 0, Param) then Result := GetServiceError;
  end
	else
		Result := GetServiceError;
end;

function TService.GetState : cardinal; begin
  if WinSvc.QueryServiceStatus(FService, FStatus) then
    Result := FStatus.dwCurrentState
  else
    Result := 77;
end;

procedure TService.ReportEventLog(EventType : TEventType; EventCode : word; Message : string);
var
	Mensagem : pchar;
begin
	Mensagem := pchar(Message);
	ReportEvent(FSource, Word(EventType), 1000+EventCode, 0, nil, 1, 0, @Mensagem, nil);
end;

// StopNow pode ser usada dentro do service para parar o Service
procedure TService.StopNow; begin
  SetLastError(0);
  SetEvent(FStopEvent)
end;

function TService.ReportServiceStatus(CurrentState, Win32ExitCode, CheckPoint, WaitHint : integer) : boolean; begin
  SetLastError(0);
	with FStatus do begin
		dwServiceType := SERVICE_WIN32_OWN_PROCESS;
		dwServiceSpecificExitCode := 0;
		// Desabilita requisições até o serviço estar startado
		if CurrentState = SERVICE_START_PENDING then
			dwControlsAccepted := 0
		else
			dwControlsAccepted := SERVICE_ACCEPT_STOP + SERVICE_ACCEPT_PAUSE_CONTINUE;
		dwCurrentState := CurrentState;
		dwCheckPoint 	 := CheckPoint;
		dwWaitHint     := WaitHint;
		if ExitCode = 0 then
			dwWin32ExitCode := Win32ExitCode
		else begin
			dwWin32ExitCode := ERROR_SERVICE_SPECIFIC_ERROR;
			dwServiceSpecificExitCode := ExitCode;
		end;
		// Manda o status do service para o service manager.
		Result := WinSvc.SetServiceStatus(FStatusHandle, FStatus);
		if not Result then StopNow;
	end;
end;

function TService.ReportNoError(Estado : integer) : boolean; begin
	Result := ReportServiceStatus(Estado, NO_ERROR, 0, 0)
end;

function TService.ReportStart : boolean;
const
	ChkPoint : integer = 0;
begin
  Result := false;
  if FReportStartStop and Exists then begin
    inc(ChkPoint);
    Result := ReportServiceStatus(SERVICE_START_PENDING, NO_ERROR, ChkPoint, Timeout);
  end;
end;

function TService.ReportStop : boolean;
const
	ChkPoint : integer = 0;
begin
  Result := false;
  if FReportStartStop and Exists then begin
    inc(ChkPoint);
    Result := ReportServiceStatus(SERVICE_STOP_PENDING, NO_ERROR, ChkPoint, Timeout);
  end;
end;

// Is called by Windows Server Manager
procedure ServController(Comando : integer); stdcall;
var
	I : integer;
begin
  with Service do
    case Comando of
      SERVICE_CONTROL_PAUSE: if FStatus.dwCurrentState = SERVICE_RUNNING then begin
        for I := 0 to FMaxThreads do
          FServiceThreads[I].Suspend;
        ReportNoError(SERVICE_PAUSED);
      end;
      SERVICE_CONTROL_CONTINUE: if FStatus.dwCurrentState = SERVICE_PAUSED then begin
        for I := 0 to FMaxThreads do
          FServiceThreads[I].Resume;
        ReportNoError(SERVICE_RUNNING);
      end;
      SERVICE_CONTROL_STOP: begin
        FReportStartStop := true;
        ReportStop;
        // Request all threads to terminate
        for I := 0 to FMaxThreads do
          FServiceThreads[I].Terminate;
        // Wait to termination and free them
        for I := 0 to FMaxThreads do
          with FServiceThreads[I] do begin
            WaitFor;
            Free;
          end;
        ReportStop;
        StopNow;
      end;
    else
      ReportNoError(SERVICE_RUNNING);
    end;
end;

{
Starta o serviço, informando ao Server Manager cada passo do processo,
depois lança as threads do serviço, espera o evento de finalização e
volta para StartServiceCtrlDispatcher no RunService
}
procedure ServiceMain(ArgC : integer; ArgV : pchar); stdcall;
var
	I : integer;
  InitOk : boolean;
begin
  with Service do begin
    FParamCount := ArgC;
    FParamStr 	:= strpas(ArgV);
    SetLastError(0);
    FStatusHandle := RegisterServiceCtrlHandler(FName, @ServController);
    if FStatusHandle <> 0 then begin
      if ReportStart then begin
        // Cria o evento que irá sinalizar o fim do service
        SetLastError(0);
        FStopEvent := CreateEvent(nil, true, false, nil);
        if FStopEvent <> 0 then begin
          // Roda a rotina de inicialização do Service
          InitOk := true;
          if @FServiceBegin <> nil then InitOk := FServiceBegin;
          if InitOk then begin
            ReportStart;
            FReportStartStop := false;
            // Starta as threads do service
            for I := 0 to FMaxThreads do
              FServiceThreads[I].Resume;
            ReportEventLog(EventInformation, 0, 'Started');
            if ReportNoError(SERVICE_RUNNING) then
              // Espera indefinidamente até o StopEvent ocorrer
              WaitForSingleObject(FStopEvent, INFINITE);
            FReportStartStop := true;
            ReportStop;
            // Desaloca as Threads
            for I := 0 to FMaxThreads do
              FServiceThreads[I].Terminate;
            ReportEventLog(EventInformation, 1, 'Stopped');
            ReportStop;
            SetLastError(0);
            if @FServiceEnd <> nil then FServiceEnd; // Roda a rotina de finalização
          end;
          CloseHandle(FStopEvent);
        end;
      end;
      ReportServiceStatus(SERVICE_STOPPED, GetLastError, 0, 0);
    end;
  end;
end;

{
Chama StartServiceCtrlDispatcher para register a main service thread.
Quando a API retorna, o service foi stopado, então halt.
}
function TService.Run(ServThreads : array of TThread; ServBegin  : TFuncBool = nil; ServEnd : TFuncBool = nil) : boolean;
var
	ServTable : array[0..1] of WinSvc.TServiceTableEntry;
	I : integer;
begin
	FServiceBegin := ServBegin;
	FServiceEnd   := ServEnd;
	FMaxThreads   := high(ServThreads);
	for I := 0 to FMaxThreads do
		FServiceThreads[I] := ServThreads[I];
	fillchar(ServTable, sizeof(ServTable), 0);
	with ServTable[0] do begin
		lpServiceName := FName;
		lpServiceProc := @ServiceMain
	end;
  SetLastError(0);
	Result := WinSvc.StartServiceCtrlDispatcher(ServTable[0]);
end;

constructor TService.Create(ServiceName : string; Description : string = ''); begin
  inherited Create;
  FName        := pchar(ServiceName);
  FDescription := Description;
  FSource      := OpenEventLog(nil, FName);
  FManager     := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if FManager = 0 then RaiseLastOSError;
  FService  := OpenService(FManager, FName, SERVICE_ALL_ACCESS);
  FTimeout  := 20000;
  FReportStartStop := true;
end;

destructor TService.Destroy; begin
  CloseServiceHandle(FService);
	CloseEventLog(FSource);
  CloseServiceHandle(FManager);
end;

end.
