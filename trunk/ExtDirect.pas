unit ExtDirect;

// Generated by ExtToPascal v.0.9.8, at 12/3/2010 16:44:12
// from "C:\Trabalho\ext-3.1.0\docs\output" detected as ExtJS v.3

interface

uses
  StrUtils, ExtPascal, ExtPascalUtils, ExtUtil;

type
  TExtDirectTransaction = class;
  TExtDirectProvider = class;
  TExtDirectJsonProvider = class;
  TExtDirectRemotingProvider = class;
  TExtDirectPollingProvider = class;

  TExtDirectTransaction = class(TExtFunction)
  public
    function JSClassName : string; override;
    {$IFDEF FPC}constructor AddTo(List : TExtObjectList);{$ENDIF}
  end;

  // Procedural types for events TExtDirectProvider
  TExtDirectProviderOnConnect = procedure(Provider : TExtDirectProvider) of object;
  TExtDirectProviderOnData = procedure(Provider : TExtDirectProvider; E : TEvent) of object;
  TExtDirectProviderOnDisconnect = procedure(Provider : TExtDirectProvider) of object;
  TExtDirectProviderOnException = procedure of object;

  TExtDirectProvider = class(TExtUtilObservable)
  private
    FId : String;
    FPriority : Integer;
    FTypeJS : String;
    FConnect : TExtObject;
    FDisconnect : TExtObject;
    FOnConnect : TExtDirectProviderOnConnect;
    FOnData : TExtDirectProviderOnData;
    FOnDisconnect : TExtDirectProviderOnDisconnect;
    FOnException : TExtDirectProviderOnException;
    procedure SetFId(Value : String);
    procedure SetFPriority(Value : Integer);
    procedure SetFTypeJS(Value : String);
    procedure SetFConnect(Value : TExtObject);
    procedure SetFDisconnect(Value : TExtObject);
    procedure SetFOnConnect(Value : TExtDirectProviderOnConnect);
    procedure SetFOnData(Value : TExtDirectProviderOnData);
    procedure SetFOnDisconnect(Value : TExtDirectProviderOnDisconnect);
    procedure SetFOnException(Value : TExtDirectProviderOnException);
  protected
    procedure InitDefaults; override;
    procedure HandleEvent(const AEvtName: string); override;
  public
    function JSClassName : string; override;
    {$IFDEF FPC}constructor AddTo(List : TExtObjectList);{$ENDIF}
    constructor Create;
    function IsConnected : TExtFunction;
    destructor Destroy; override;
    property Id : String read FId write SetFId;
    property Priority : Integer read FPriority write SetFPriority;
    property TypeJS : String read FTypeJS write SetFTypeJS;
    property Connect : TExtObject read FConnect write SetFConnect;
    property Disconnect : TExtObject read FDisconnect write SetFDisconnect;
    property OnConnect : TExtDirectProviderOnConnect read FOnConnect write SetFOnConnect;
    property OnData : TExtDirectProviderOnData read FOnData write SetFOnData;
    property OnDisconnect : TExtDirectProviderOnDisconnect read FOnDisconnect write SetFOnDisconnect;
    property OnException : TExtDirectProviderOnException read FOnException write SetFOnException;
  end;

  TExtDirectJsonProvider = class(TExtDirectProvider)
  public
    function JSClassName : string; override;
    {$IFDEF FPC}constructor AddTo(List : TExtObjectList);{$ENDIF}
  end;

  // Procedural types for events TExtDirectRemotingProvider
  TExtDirectRemotingProviderOnBeforecall = procedure(Provider : TExtDirectRemotingProvider; Transaction : TExtDirectTransaction) of object;
  TExtDirectRemotingProviderOnCall = procedure(Provider : TExtDirectRemotingProvider; Transaction : TExtDirectTransaction) of object;

  TExtDirectRemotingProvider = class(TExtDirectJsonProvider)
  private
    FActions : TExtObject;
    FEnableBuffer : Integer; // 10
    FEnableBufferBoolean : Boolean;
    FEnableUrlEncode : String;
    FMaxRetries : Integer;
    FNamespace : String;
    FNamespaceObject : TExtObject;
    FTimeout : Integer;
    FUrl : String;
    FOnBeforecall : TExtDirectRemotingProviderOnBeforecall;
    FOnCall : TExtDirectRemotingProviderOnCall;
    procedure SetFActions(Value : TExtObject);
    procedure SetFEnableBuffer(Value : Integer);
    procedure SetFEnableBufferBoolean(Value : Boolean);
    procedure SetFEnableUrlEncode(Value : String);
    procedure SetFMaxRetries(Value : Integer);
    procedure SetFNamespace(Value : String);
    procedure SetFNamespaceObject(Value : TExtObject);
    procedure SetFTimeout(Value : Integer);
    procedure SetFUrl(Value : String);
    procedure SetFOnBeforecall(Value : TExtDirectRemotingProviderOnBeforecall);
    procedure SetFOnCall(Value : TExtDirectRemotingProviderOnCall);
  protected
    procedure InitDefaults; override;
    procedure HandleEvent(const AEvtName: string); override;
  public
    function JSClassName : string; override;
    {$IFDEF FPC}constructor AddTo(List : TExtObjectList);{$ENDIF}
    constructor Create;
    destructor Destroy; override;
    property Actions : TExtObject read FActions write SetFActions;
    property EnableBuffer : Integer read FEnableBuffer write SetFEnableBuffer;
    property EnableBufferBoolean : Boolean read FEnableBufferBoolean write SetFEnableBufferBoolean;
    property EnableUrlEncode : String read FEnableUrlEncode write SetFEnableUrlEncode;
    property MaxRetries : Integer read FMaxRetries write SetFMaxRetries;
    property Namespace : String read FNamespace write SetFNamespace;
    property NamespaceObject : TExtObject read FNamespaceObject write SetFNamespaceObject;
    property Timeout : Integer read FTimeout write SetFTimeout;
    property Url : String read FUrl write SetFUrl;
    property OnBeforecall : TExtDirectRemotingProviderOnBeforecall read FOnBeforecall write SetFOnBeforecall;
    property OnCall : TExtDirectRemotingProviderOnCall read FOnCall write SetFOnCall;
  end;

  // Procedural types for events TExtDirectPollingProvider
  TExtDirectPollingProviderOnBeforepoll = procedure(E : TExtDirectPollingProvider) of object;
  TExtDirectPollingProviderOnPoll = procedure(E : TExtDirectPollingProvider) of object;

  TExtDirectPollingProvider = class(TExtDirectJsonProvider)
  private
    FBaseParams : TExtObject;
    FInterval : Integer; // 3000
    FPriority : Integer; // 3
    FUrl : String;
    FUrlFunction : TExtFunction;
    FOnBeforepoll : TExtDirectPollingProviderOnBeforepoll;
    FOnPoll : TExtDirectPollingProviderOnPoll;
    procedure SetFBaseParams(Value : TExtObject);
    procedure SetFInterval(Value : Integer);
    procedure SetFPriority(Value : Integer);
    procedure SetFUrl(Value : String);
    procedure SetFUrlFunction(Value : TExtFunction);
    procedure SetFOnBeforepoll(Value : TExtDirectPollingProviderOnBeforepoll);
    procedure SetFOnPoll(Value : TExtDirectPollingProviderOnPoll);
  protected
    procedure InitDefaults; override;
    procedure HandleEvent(const AEvtName: string); override;
  public
    function JSClassName : string; override;
    {$IFDEF FPC}constructor AddTo(List : TExtObjectList);{$ENDIF}
    constructor Create;
    function Connect : TExtFunction;
    function Disconnect : TExtFunction;
    destructor Destroy; override;
    property BaseParams : TExtObject read FBaseParams write SetFBaseParams;
    property Interval : Integer read FInterval write SetFInterval;
    property Priority : Integer read FPriority write SetFPriority;
    property Url : String read FUrl write SetFUrl;
    property UrlFunction : TExtFunction read FUrlFunction write SetFUrlFunction;
    property OnBeforepoll : TExtDirectPollingProviderOnBeforepoll read FOnBeforepoll write SetFOnBeforepoll;
    property OnPoll : TExtDirectPollingProviderOnPoll read FOnPoll write SetFOnPoll;
  end;

implementation

function TExtDirectTransaction.JSClassName : string; begin
  Result := 'Ext.direct.Transaction';
end;

{$IFDEF FPC}constructor TExtDirectTransaction.AddTo(List : TExtObjectList);begin inherited end;{$ENDIF}

procedure TExtDirectProvider.SetFId(Value : String); begin
  FId := Value;
  JSCode('id:' + VarToJSON([Value]));
end;

procedure TExtDirectProvider.SetFPriority(Value : Integer); begin
  FPriority := Value;
  JSCode('priority:' + VarToJSON([Value]));
end;

procedure TExtDirectProvider.SetFTypeJS(Value : String); begin
  FTypeJS := Value;
  JSCode('typeJS:' + VarToJSON([Value]));
end;

procedure TExtDirectProvider.SetFConnect(Value : TExtObject); begin
  FConnect := Value;
  Value.DeleteFromGarbage;
  JSCode(JSName + '.connect=' + VarToJSON([Value, false]) + ';');
end;

procedure TExtDirectProvider.SetFDisconnect(Value : TExtObject); begin
  FDisconnect := Value;
  Value.DeleteFromGarbage;
  JSCode(JSName + '.disconnect=' + VarToJSON([Value, false]) + ';');
end;

procedure TExtDirectProvider.SetFOnConnect(Value : TExtDirectProviderOnConnect); begin
  if Assigned(FOnConnect) then
    JSCode(JSName+'.events ["connect"].listeners=[];');
  if Assigned(Value) then
    On('connect', Ajax('connect', ['Provider', '%0.nm'], true));
  FOnConnect := Value;
end;

procedure TExtDirectProvider.SetFOnData(Value : TExtDirectProviderOnData); begin
  if Assigned(FOnData) then
    JSCode(JSName+'.events ["data"].listeners=[];');
  if Assigned(Value) then
    On('data', Ajax('data', ['Provider', '%0.nm','E', '%1.nm'], true));
  FOnData := Value;
end;

procedure TExtDirectProvider.SetFOnDisconnect(Value : TExtDirectProviderOnDisconnect); begin
  if Assigned(FOnDisconnect) then
    JSCode(JSName+'.events ["disconnect"].listeners=[];');
  if Assigned(Value) then
    On('disconnect', Ajax('disconnect', ['Provider', '%0.nm'], true));
  FOnDisconnect := Value;
end;

procedure TExtDirectProvider.SetFOnException(Value : TExtDirectProviderOnException); begin
  if Assigned(FOnException) then
    JSCode(JSName+'.events ["exception"].listeners=[];');
  if Assigned(Value) then
    On('exception', Ajax('exception', [], true));
  FOnException := Value;
end;

function TExtDirectProvider.JSClassName : string; begin
  Result := 'Ext.direct.Provider';
end;

procedure TExtDirectProvider.InitDefaults; begin
  inherited;
  FConnect := TExtObject.CreateInternal(Self, 'connect');
  FDisconnect := TExtObject.CreateInternal(Self, 'disconnect');
end;

{$IFDEF FPC}constructor TExtDirectProvider.AddTo(List : TExtObjectList);begin inherited end;{$ENDIF}

constructor TExtDirectProvider.Create; begin
  CreateVar(JSClassName + '({});');
  InitDefaults;
end;

function TExtDirectProvider.IsConnected : TExtFunction; begin
  JSCode(JSName + '.isConnected();', 'TExtDirectProvider');
  Result := Self;
end;

destructor TExtDirectProvider.Destroy; begin
  try
    FConnect.Free;
    FDisconnect.Free;
  except end;
  inherited;
end;

procedure TExtDirectProvider.HandleEvent(const AEvtName : string); begin
  inherited;
  if (AEvtName = 'connect') and Assigned(FOnConnect) then
    FOnConnect(TExtDirectProvider(ParamAsObject('Provider')))
  else if (AEvtName = 'data') and Assigned(FOnData) then
    FOnData(TExtDirectProvider(ParamAsObject('Provider')), TEvent(ParamAsObject('E')))
  else if (AEvtName = 'disconnect') and Assigned(FOnDisconnect) then
    FOnDisconnect(TExtDirectProvider(ParamAsObject('Provider')))
  else if (AEvtName = 'exception') and Assigned(FOnException) then
    FOnException();
end;

function TExtDirectJsonProvider.JSClassName : string; begin
  Result := 'Ext.direct.JsonProvider';
end;

{$IFDEF FPC}constructor TExtDirectJsonProvider.AddTo(List : TExtObjectList);begin inherited end;{$ENDIF}

procedure TExtDirectRemotingProvider.SetFActions(Value : TExtObject); begin
  FActions := Value;
  Value.DeleteFromGarbage;
  JSCode('actions:' + VarToJSON([Value, false]));
end;

procedure TExtDirectRemotingProvider.SetFEnableBuffer(Value : Integer); begin
  FEnableBuffer := Value;
  JSCode('enableBuffer:' + VarToJSON([Value]));
end;

procedure TExtDirectRemotingProvider.SetFEnableBufferBoolean(Value : Boolean); begin
  FEnableBufferBoolean := Value;
  JSCode('enableBuffer:' + VarToJSON([Value]));
end;

procedure TExtDirectRemotingProvider.SetFEnableUrlEncode(Value : String); begin
  FEnableUrlEncode := Value;
  JSCode('enableUrlEncode:' + VarToJSON([Value]));
end;

procedure TExtDirectRemotingProvider.SetFMaxRetries(Value : Integer); begin
  FMaxRetries := Value;
  JSCode('maxRetries:' + VarToJSON([Value]));
end;

procedure TExtDirectRemotingProvider.SetFNamespace(Value : String); begin
  FNamespace := Value;
  JSCode('namespace:' + VarToJSON([Value]));
end;

procedure TExtDirectRemotingProvider.SetFNamespaceObject(Value : TExtObject); begin
  FNamespaceObject := Value;
  Value.DeleteFromGarbage;
  JSCode('namespace:' + VarToJSON([Value, false]));
end;

procedure TExtDirectRemotingProvider.SetFTimeout(Value : Integer); begin
  FTimeout := Value;
  JSCode('timeout:' + VarToJSON([Value]));
end;

procedure TExtDirectRemotingProvider.SetFUrl(Value : String); begin
  FUrl := Value;
  JSCode('url:' + VarToJSON([Value]));
end;

procedure TExtDirectRemotingProvider.SetFOnBeforecall(Value : TExtDirectRemotingProviderOnBeforecall); begin
  if Assigned(FOnBeforecall) then
    JSCode(JSName+'.events ["beforecall"].listeners=[];');
  if Assigned(Value) then
    On('beforecall', Ajax('beforecall', ['Provider', '%0.nm','Transaction', '%1.nm'], true));
  FOnBeforecall := Value;
end;

procedure TExtDirectRemotingProvider.SetFOnCall(Value : TExtDirectRemotingProviderOnCall); begin
  if Assigned(FOnCall) then
    JSCode(JSName+'.events ["call"].listeners=[];');
  if Assigned(Value) then
    On('call', Ajax('call', ['Provider', '%0.nm','Transaction', '%1.nm'], true));
  FOnCall := Value;
end;

function TExtDirectRemotingProvider.JSClassName : string; begin
  Result := 'Ext.direct.RemotingProvider';
end;

procedure TExtDirectRemotingProvider.InitDefaults; begin
  inherited;
  FActions := TExtObject.CreateInternal(Self, 'actions');
  FEnableBuffer := 10;
  FNamespaceObject := TExtObject.CreateInternal(Self, 'namespace');
end;

{$IFDEF FPC}constructor TExtDirectRemotingProvider.AddTo(List : TExtObjectList);begin inherited end;{$ENDIF}

constructor TExtDirectRemotingProvider.Create; begin
  CreateVar(JSClassName + '({});');
  InitDefaults;
end;

destructor TExtDirectRemotingProvider.Destroy; begin
  try
    FActions.Free;
    FNamespaceObject.Free;
  except end;
  inherited;
end;

procedure TExtDirectRemotingProvider.HandleEvent(const AEvtName : string); begin
  inherited;
  if (AEvtName = 'beforecall') and Assigned(FOnBeforecall) then
    FOnBeforecall(TExtDirectRemotingProvider(ParamAsObject('Provider')), TExtDirectTransaction(ParamAsObject('Transaction')))
  else if (AEvtName = 'call') and Assigned(FOnCall) then
    FOnCall(TExtDirectRemotingProvider(ParamAsObject('Provider')), TExtDirectTransaction(ParamAsObject('Transaction')));
end;

procedure TExtDirectPollingProvider.SetFBaseParams(Value : TExtObject); begin
  FBaseParams := Value;
  Value.DeleteFromGarbage;
  JSCode('baseParams:' + VarToJSON([Value, false]));
end;

procedure TExtDirectPollingProvider.SetFInterval(Value : Integer); begin
  FInterval := Value;
  JSCode('interval:' + VarToJSON([Value]));
end;

procedure TExtDirectPollingProvider.SetFPriority(Value : Integer); begin
  FPriority := Value;
  JSCode('priority:' + VarToJSON([Value]));
end;

procedure TExtDirectPollingProvider.SetFUrl(Value : String); begin
  FUrl := Value;
  JSCode('url:' + VarToJSON([Value]));
end;

procedure TExtDirectPollingProvider.SetFUrlFunction(Value : TExtFunction); begin
  FUrlFunction := Value;
  JSCode('url:' + VarToJSON([Value, true]));
end;

procedure TExtDirectPollingProvider.SetFOnBeforepoll(Value : TExtDirectPollingProviderOnBeforepoll); begin
  if Assigned(FOnBeforepoll) then
    JSCode(JSName+'.events ["beforepoll"].listeners=[];');
  if Assigned(Value) then
    On('beforepoll', Ajax('beforepoll', ['E', '%0.nm'], true));
  FOnBeforepoll := Value;
end;

procedure TExtDirectPollingProvider.SetFOnPoll(Value : TExtDirectPollingProviderOnPoll); begin
  if Assigned(FOnPoll) then
    JSCode(JSName+'.events ["poll"].listeners=[];');
  if Assigned(Value) then
    On('poll', Ajax('poll', ['E', '%0.nm'], true));
  FOnPoll := Value;
end;

function TExtDirectPollingProvider.JSClassName : string; begin
  Result := 'Ext.direct.PollingProvider';
end;

procedure TExtDirectPollingProvider.InitDefaults; begin
  inherited;
  FBaseParams := TExtObject.CreateInternal(Self, 'baseParams');
  FInterval := 3000;
  FPriority := 3;
end;

{$IFDEF FPC}constructor TExtDirectPollingProvider.AddTo(List : TExtObjectList);begin inherited end;{$ENDIF}

constructor TExtDirectPollingProvider.Create; begin
  CreateVar(JSClassName + '({});');
  InitDefaults;
end;

function TExtDirectPollingProvider.Connect : TExtFunction; begin
  JSCode(JSName + '.connect();', 'TExtDirectPollingProvider');
  Result := Self;
end;

function TExtDirectPollingProvider.Disconnect : TExtFunction; begin
  JSCode(JSName + '.disconnect();', 'TExtDirectPollingProvider');
  Result := Self;
end;

destructor TExtDirectPollingProvider.Destroy; begin
  try
    FBaseParams.Free;
  except end;
  inherited;
end;

procedure TExtDirectPollingProvider.HandleEvent(const AEvtName : string); begin
  inherited;
  if (AEvtName = 'beforepoll') and Assigned(FOnBeforepoll) then
    FOnBeforepoll(TExtDirectPollingProvider(ParamAsObject('E')))
  else if (AEvtName = 'poll') and Assigned(FOnPoll) then
    FOnPoll(TExtDirectPollingProvider(ParamAsObject('E')));
end;

end.