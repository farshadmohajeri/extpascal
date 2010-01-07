unit ExtState;

// Generated by ExtToPascal v.0.9.8, at 7/1/2010 09:28:35
// from "C:\Trabalho\ext-3.1.0\docs\output" detected as ExtJS v.3

interface

uses
  StrUtils, ExtPascal, ExtPascalUtils, ExtDirect;

type
  TExtStateProvider = class;
  TExtStateManagerSingleton = class;
  TExtStateCookieProvider = class;

  // Procedural types for events TExtStateProvider
  TExtStateProviderOnStatechange = procedure(This : TExtStateProvider; Key : string; Value : string) of object;

  TExtStateProvider = class(TExtFunction)
  private
    FOnStatechange : TExtStateProviderOnStatechange;
    procedure SetFOnStatechange(Value : TExtStateProviderOnStatechange);
  protected
    procedure HandleEvent(const AEvtName: string); override;
  public
    function JSClassName : string; override;
    {$IFDEF FPC}constructor AddTo(List : TExtObjectList);{$ENDIF}
    function Clear(Name : string) : TExtFunction;
    function DecodeValue(Value : string) : TExtFunction;
    function EncodeValue(Value : string) : TExtFunction;
    function Get(Name : string; DefaultValue : string) : TExtFunction;
    function SetJS(Name : string; Value : string) : TExtFunction;
    function On(EventName : string; Handler : TExtFunction) : TExtFunction;
    function Un(EventName : string; Handler : TExtFunction) : TExtFunction;
    property OnStatechange : TExtStateProviderOnStatechange read FOnStatechange write SetFOnStatechange;
  end;

  TExtStateManagerSingleton = class(TExtFunction)
  public
    function JSClassName : string; override;
    {$IFDEF FPC}constructor AddTo(List : TExtObjectList);{$ENDIF}
    function Clear(Name : string) : TExtFunction;
    function Get(Name : string; DefaultValue : string) : TExtFunction;
    function GetProvider : TExtFunction;
    function SetJS(Name : string; Value : string) : TExtFunction;
    function SetProvider(StateProvider : TExtStateProvider) : TExtFunction;
  end;

  TExtStateCookieProvider = class(TExtStateProvider)
  private
    FDomain : string; // 'www'
    FExpires : TDateTime;
    FPath : string; // '/'
    FSecure : Boolean;
    procedure SetFDomain(Value : string);
    procedure SetFExpires(Value : TDateTime);
    procedure SetFPath(Value : string);
    procedure SetFSecure(Value : Boolean);
  protected
    procedure InitDefaults; override;
  public
    function JSClassName : string; override;
    {$IFDEF FPC}constructor AddTo(List : TExtObjectList);{$ENDIF}
    constructor Create;
    property Domain : string read FDomain write SetFDomain;
    property Expires : TDateTime read FExpires write SetFExpires;
    property Path : string read FPath write SetFPath;
    property Secure : Boolean read FSecure write SetFSecure;
  end;

var
  ExtStateManager : TExtStateManagerSingleton;

implementation

procedure TExtStateProvider.SetFOnStatechange(Value : TExtStateProviderOnStatechange); begin
  if Assigned(FOnStatechange) then
    JSCode(JSName+'.events ["statechange"].listeners=[];');
  if Assigned(Value) then
    On('statechange', Ajax('statechange', ['This', '%0.nm','Key', '%1','Value', '%2'], true));
  FOnStatechange := Value;
end;

function TExtStateProvider.JSClassName : string; begin
  Result := 'Ext.state.Provider';
end;

{$IFDEF FPC}constructor TExtStateProvider.AddTo(List : TExtObjectList);begin inherited end;{$ENDIF}

function TExtStateProvider.Clear(Name : string) : TExtFunction; begin
  JSCode(JSName + '.clear(' + VarToJSON([Name]) + ');', 'TExtStateProvider');
  Result := Self;
end;

function TExtStateProvider.DecodeValue(Value : string) : TExtFunction; begin
  JSCode(JSName + '.decodeValue(' + VarToJSON([Value]) + ');', 'TExtStateProvider');
  Result := Self;
end;

function TExtStateProvider.EncodeValue(Value : string) : TExtFunction; begin
  JSCode(JSName + '.encodeValue(' + VarToJSON([Value]) + ');', 'TExtStateProvider');
  Result := Self;
end;

function TExtStateProvider.Get(Name : string; DefaultValue : string) : TExtFunction; begin
  JSCode(JSName + '.get(' + VarToJSON([Name, DefaultValue]) + ');', 'TExtStateProvider');
  Result := Self;
end;

function TExtStateProvider.SetJS(Name : string; Value : string) : TExtFunction; begin
  JSCode(JSName + '.set(' + VarToJSON([Name, Value]) + ');', 'TExtStateProvider');
  Result := Self;
end;

function TExtStateProvider.On(EventName : string; Handler : TExtFunction) : TExtFunction; begin
  JSCode(JSName + '.on(' + VarToJSON([EventName, Handler, true]) + ');', 'TExtStateProvider');
  Result := Self;
end;

function TExtStateProvider.Un(EventName : string; Handler : TExtFunction) : TExtFunction; begin
  JSCode(JSName + '.un(' + VarToJSON([EventName, Handler, true]) + ');', 'TExtStateProvider');
  Result := Self;
end;

procedure TExtStateProvider.HandleEvent(const AEvtName : string); begin
  inherited;
  if (AEvtName = 'statechange') and Assigned(FOnStatechange) then
    FOnStatechange(TExtStateProvider(ParamAsObject('This')), ParamAsstring('Key'), ParamAsstring('Value'));
end;

function TExtStateManagerSingleton.JSClassName : string; begin
  Result := 'Ext.state.Manager';
end;

{$IFDEF FPC}constructor TExtStateManagerSingleton.AddTo(List : TExtObjectList);begin inherited end;{$ENDIF}

function TExtStateManagerSingleton.Clear(Name : string) : TExtFunction; begin
  JSCode(JSName + '.clear(' + VarToJSON([Name]) + ');', 'TExtStateManagerSingleton');
  Result := Self;
end;

function TExtStateManagerSingleton.Get(Name : string; DefaultValue : string) : TExtFunction; begin
  JSCode(JSName + '.get(' + VarToJSON([Name, DefaultValue]) + ');', 'TExtStateManagerSingleton');
  Result := Self;
end;

function TExtStateManagerSingleton.GetProvider : TExtFunction; begin
  JSCode(JSName + '.getProvider();', 'TExtStateManagerSingleton');
  Result := Self;
end;

function TExtStateManagerSingleton.SetJS(Name : string; Value : string) : TExtFunction; begin
  JSCode(JSName + '.set(' + VarToJSON([Name, Value]) + ');', 'TExtStateManagerSingleton');
  Result := Self;
end;

function TExtStateManagerSingleton.SetProvider(StateProvider : TExtStateProvider) : TExtFunction; begin
  JSCode(JSName + '.setProvider(' + VarToJSON([StateProvider, false]) + ');', 'TExtStateManagerSingleton');
  Result := Self;
end;

procedure TExtStateCookieProvider.SetFDomain(Value : string); begin
  FDomain := Value;
  JSCode('domain:' + VarToJSON([Value]));
end;

procedure TExtStateCookieProvider.SetFExpires(Value : TDateTime); begin
  FExpires := Value;
  JSCode('expires:' + VarToJSON([Value]));
end;

procedure TExtStateCookieProvider.SetFPath(Value : string); begin
  FPath := Value;
  JSCode('path:' + VarToJSON([Value]));
end;

procedure TExtStateCookieProvider.SetFSecure(Value : Boolean); begin
  FSecure := Value;
  JSCode('secure:' + VarToJSON([Value]));
end;

function TExtStateCookieProvider.JSClassName : string; begin
  Result := 'Ext.state.CookieProvider';
end;

procedure TExtStateCookieProvider.InitDefaults; begin
  inherited;
  FDomain := 'www';
  FPath := '/';
end;

{$IFDEF FPC}constructor TExtStateCookieProvider.AddTo(List : TExtObjectList);begin inherited end;{$ENDIF}

constructor TExtStateCookieProvider.Create; begin
  CreateVar(JSClassName + '({});');
  InitDefaults;
end;

initialization
  ExtStateManager := TExtStateManagerSingleton.CreateSingleton;

finalization
  ExtStateManager.Destroy;
end.