unit IdExtHTTPServer;

interface

uses Classes, Forms, StdCtrls, IdCustomHTTPServer, IdHTTPServer, IdContext;

const
  DefaultCharset : string = 'utf-8'; // Default charset when a <link TFCGIThread, thread> is created

type
  TIdExtHTTPServer = class;
  {$M+}
  TIdExtSession = class(TIdHTTPSession)
  private
    FCurrentRequest: TIdHTTPRequestInfo;
    FCurrentReponse: TIdHTTPResponseInfo;
    FNewThread: boolean;
    FGarbageCollector : TStringList;
    FCharset: string;
    function GetPathInfo: string;
    function GetRequestHeader(const HeaderName: string): string;
    function GetQuery(const ParamName: string): string;
    function GetCookie(const CookieName: string): string;
  protected
    //Methods to be implemented in your app
    function BeforeHandleRequest : boolean; virtual;
    procedure AfterHandleRequest; virtual;
    procedure OnError(Msg, Method, Params : string); virtual;
    procedure OnNotFoundError; virtual;
  public
    Response: string;

    constructor CreateInitialized(AOwner: TIdHTTPCustomSessionList; const SessionID, RemoteIP: String); override;
    destructor Destroy; override;

    procedure InitSessionDefs; virtual;

    procedure HandleRequest(ARequest: TIdHTTPRequestInfo; AResponse: TIdHTTPResponseInfo);
    procedure AddToGarbage(const Name: string; Obj: TObject);
    procedure DeleteFromGarbage(Obj: TObject); overload;
    procedure DeleteFromGarbage(Name: string); overload;
    function FindObject(Name: string): TObject;

    property PathInfo: string read GetPathInfo;
    property Query[const ParamName: string]: string read GetQuery;
    property RequestHeader[const HeaderName: string]: string read GetRequestHeader;
    property Cookie[const CookieName: string]: string read GetCookie;
    property NewThread : boolean read FNewThread write FNewThread;
    property Charset: string read FCharset write FCharset;
  published
    procedure Home; virtual; abstract;
    procedure TreatObjEvent; virtual; abstract;
    procedure Logout; virtual;
  end;
  {$M-}
  TIdExtSessionClass = class of TIdExtSession;
  TIdExtSessionList = class(TIdHTTPDefaultSessionList)
  private
    FOwner: TIdExtHTTPServer;
  public
    constructor Create(const AOwner: TIdExtHTTPServer); reintroduce;

    function CreateSession(const RemoteIP, SessionID: String): TIdHTTPSession; override;
  end;

  TIdExtHTTPServer = class(TIdHTTPServer)
  private
    FExtSessionClass: TIdExtSessionClass;
  protected
    procedure InitComponent; override;
    procedure DoCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;
  public
    constructor Create(const AOwner: TComponent; const AExtSessionClass: TIdExtSessionClass); reintroduce;
  published
    property ExtSessionClass: TIdExtSessionClass read FExtSessionClass write FExtSessionClass;
  end;

  TIdExtApplication = class
  private
    FServer: TIdExtHTTPServer;
    FMainForm: TForm;
    FListBox: TListBox;
    FIcon: string;

    procedure DoSessionStart(Sender: TIdHTTPSession);
    procedure DoSessionEnd(Sender: TIdHTTPSession);
    procedure DoTerminate(Sender: TObject);
  public
    constructor Create(ATitle : string; ASessionClass : TIdExtSessionClass; APort : word = 80; AMaxIdleMinutes : word = 30;
      AMaxConns : integer = 1000);
    procedure Run;
    function Title: string;
    property Icon: string read FIcon write FIcon;    
  end;

  TMimeExtension = record
    Ext: string;
    MimeType: string;
  end;
  
const
  MIMEExtensions: array[1..175] of TMimeExtension = (
    (Ext: '.gif'; MimeType: 'image/gif'),
    (Ext: '.jpg'; MimeType: 'image/jpeg'),
    (Ext: '.jpeg'; MimeType: 'image/jpeg'),
    (Ext: '.html'; MimeType: 'text/html'),
    (Ext: '.htm'; MimeType: 'text/html'),
    (Ext: '.css'; MimeType: 'text/css'),
    (Ext: '.js'; MimeType: 'text/javascript'),
    (Ext: '.txt'; MimeType: 'text/plain'),
    (Ext: '.xls'; MimeType: 'application/excel'),
    (Ext: '.rtf'; MimeType: 'text/richtext'),
    (Ext: '.wq1'; MimeType: 'application/x-lotus'),
    (Ext: '.wk1'; MimeType: 'application/x-lotus'),
    (Ext: '.raf'; MimeType: 'application/raf'),
    (Ext: '.png'; MimeType: 'image/x-png'),
    (Ext: '.c'; MimeType: 'text/plain'),
    (Ext: '.c++'; MimeType: 'text/plain'),
    (Ext: '.pl'; MimeType: 'text/plain'),
    (Ext: '.cc'; MimeType: 'text/plain'),
    (Ext: '.h'; MimeType: 'text/plain'),
    (Ext: '.talk'; MimeType: 'text/x-speech'),
    (Ext: '.xbm'; MimeType: 'image/x-xbitmap'),
    (Ext: '.xpm'; MimeType: 'image/x-xpixmap'),
    (Ext: '.ief'; MimeType: 'image/ief'),
    (Ext: '.jpe'; MimeType: 'image/jpeg'),
    (Ext: '.tiff'; MimeType: 'image/tiff'),
    (Ext: '.tif'; MimeType: 'image/tiff'),
    (Ext: '.rgb'; MimeType: 'image/rgb'),
    (Ext: '.g3f'; MimeType: 'image/g3fax'),
    (Ext: '.xwd'; MimeType: 'image/x-xwindowdump'),
    (Ext: '.pict'; MimeType: 'image/x-pict'),
    (Ext: '.ppm'; MimeType: 'image/x-portable-pixmap'),
    (Ext: '.pgm'; MimeType: 'image/x-portable-graymap'),
    (Ext: '.pbm'; MimeType: 'image/x-portable-bitmap'),
    (Ext: '.pnm'; MimeType: 'image/x-portable-anymap'),
    (Ext: '.bmp'; MimeType: 'image/x-ms-bmp'),
    (Ext: '.ras'; MimeType: 'image/x-cmu-raster'),
    (Ext: '.pcd'; MimeType: 'image/x-photo-cd'),
    (Ext: '.cgm'; MimeType: 'image/cgm'),
    (Ext: '.mil'; MimeType: 'image/x-cals'),
    (Ext: '.cal'; MimeType: 'image/x-cals'),
    (Ext: '.fif'; MimeType: 'image/fif'),
    (Ext: '.dsf'; MimeType: 'image/x-mgx-dsf'),
    (Ext: '.cmx'; MimeType: 'image/x-cmx'),
    (Ext: '.wi'; MimeType: 'image/wavelet'),
    (Ext: '.dwg'; MimeType: 'image/vnd.dwg'),
    (Ext: '.dxf'; MimeType: 'image/vnd.dxf'),
    (Ext: '.svf'; MimeType: 'image/vnd.svf'),
    (Ext: '.au'; MimeType: 'audio/basic'),
    (Ext: '.snd'; MimeType: 'audio/basic'),
    (Ext: '.aif'; MimeType: 'audio/x-aiff'),
    (Ext: '.aiff'; MimeType: 'audio/x-aiff'),
    (Ext: '.aifc'; MimeType: 'audio/x-aiff'),
    (Ext: '.wav'; MimeType: 'audio/x-wav'),
    (Ext: '.mpa'; MimeType: 'audio/x-mpeg'),
    (Ext: '.abs'; MimeType: 'audio/x-mpeg'),
    (Ext: '.mpega'; MimeType: 'audio/x-mpeg'),
    (Ext: '.mp2a'; MimeType: 'audio/x-mpeg-2'),
    (Ext: '.mpa2'; MimeType: 'audio/x-mpeg-2'),
    (Ext: '.es'; MimeType: 'audio/echospeech'),
    (Ext: '.vox'; MimeType: 'audio/voxware'),
    (Ext: '.lcc'; MimeType: 'application/fastman'),
    (Ext: '.ra'; MimeType: 'application/x-pn-realaudio'),
    (Ext: '.ram'; MimeType: 'application/x-pn-realaudio'),
    (Ext: '.mmid'; MimeType: 'x-music/x-midi'),
    (Ext: '.skp'; MimeType: 'application/vnd.koan'),
    (Ext: '.talk'; MimeType: 'text/x-speech'),
    (Ext: '.mpeg'; MimeType: 'video/mpeg'),
    (Ext: '.mpg'; MimeType: 'video/mpeg'),
    (Ext: '.mpe'; MimeType: 'video/mpeg'),
    (Ext: '.mpv2'; MimeType: 'video/mpeg-2'),
    (Ext: '.mp2v'; MimeType: 'video/mpeg-2'),
    (Ext: '.qt'; MimeType: 'video/quicktime'),
    (Ext: '.mov'; MimeType: 'video/quicktime'),
    (Ext: '.avi'; MimeType: 'video/x-msvideo'),
    (Ext: '.movie'; MimeType: 'video/x-sgi-movie'),
    (Ext: '.vdo'; MimeType: 'video/vdo'),
    (Ext: '.viv'; MimeType: 'video/vnd.vivo'),
    (Ext: '.pac'; MimeType: 'application/x-ns-proxy-autoconfig'),
    (Ext: '.ai'; MimeType: 'application/postscript'),
    (Ext: '.eps'; MimeType: 'application/postscript'),
    (Ext: '.ps'; MimeType: 'application/postscript'),
    (Ext: '.rtf'; MimeType: 'application/rtf'),
    (Ext: '.pdf'; MimeType: 'application/pdf'),
    (Ext: '.mif'; MimeType: 'application/vnd.mif'),
    (Ext: '.t'; MimeType: 'application/x-troff'),
    (Ext: '.tr'; MimeType: 'application/x-troff'),
    (Ext: '.roff'; MimeType: 'application/x-troff'),
    (Ext: '.man'; MimeType: 'application/x-troff-man'),
    (Ext: '.me'; MimeType: 'application/x-troff-me'),
    (Ext: '.ms'; MimeType: 'application/x-troff-ms'),
    (Ext: '.latex'; MimeType: 'application/x-latex'),
    (Ext: '.tex'; MimeType: 'application/x-tex'),
    (Ext: '.texinfo'; MimeType: 'application/x-texinfo'),
    (Ext: '.texi'; MimeType: 'application/x-texinfo'),
    (Ext: '.dvi'; MimeType: 'application/x-dvi'),
    (Ext: '.doc'; MimeType: 'application/msword'),
    (Ext: '.oda'; MimeType: 'application/oda'),
    (Ext: '.evy'; MimeType: 'application/envoy'),
    (Ext: '.gtar'; MimeType: 'application/x-gtar'),
    (Ext: '.tar'; MimeType: 'application/x-tar'),
    (Ext: '.ustar'; MimeType: 'application/x-ustar'),
    (Ext: '.bcpio'; MimeType: 'application/x-bcpio'),
    (Ext: '.cpio'; MimeType: 'application/x-cpio'),
    (Ext: '.shar'; MimeType: 'application/x-shar'),
    (Ext: '.zip'; MimeType: 'application/zip'),
    (Ext: '.hqx'; MimeType: 'application/mac-binhex40'),
    (Ext: '.sit'; MimeType: 'application/x-stuffit'),
    (Ext: '.sea'; MimeType: 'application/x-stuffit'),
    (Ext: '.fif'; MimeType: 'application/fractals'),
    (Ext: '.bin'; MimeType: 'application/octet-stream'),
    (Ext: '.uu'; MimeType: 'application/octet-stream'),
    (Ext: '.exe'; MimeType: 'application/octet-stream'),
    (Ext: '.src'; MimeType: 'application/x-wais-source'),
    (Ext: '.wsrc'; MimeType: 'application/x-wais-source'),
    (Ext: '.hdf'; MimeType: 'application/hdf'),
    (Ext: '.ls'; MimeType: 'text/javascript'),
    (Ext: '.mocha'; MimeType: 'text/javascript'),
    (Ext: '.vbs'; MimeType: 'text/vbscript'),
    (Ext: '.sh'; MimeType: 'application/x-sh'),
    (Ext: '.csh'; MimeType: 'application/x-csh'),
    (Ext: '.pl'; MimeType: 'application/x-perl'),
    (Ext: '.tcl'; MimeType: 'application/x-tcl'),
    (Ext: '.spl'; MimeType: 'application/futuresplash'),
    (Ext: '.mbd'; MimeType: 'application/mbedlet'),
    (Ext: '.swf'; MimeType: 'application/x-director'),
    (Ext: '.pps'; MimeType: 'application/mspowerpoint'),
    (Ext: '.asp'; MimeType: 'application/x-asap'),
    (Ext: '.asn'; MimeType: 'application/astound'),
    (Ext: '.axs'; MimeType: 'application/x-olescript'),
    (Ext: '.ods'; MimeType: 'application/x-oleobject'),
    (Ext: '.opp'; MimeType: 'x-form/x-openscape'),
    (Ext: '.wba'; MimeType: 'application/x-webbasic'),
    (Ext: '.frm'; MimeType: 'application/x-alpha-form'),
    (Ext: '.wfx'; MimeType: 'x-script/x-wfxclient'),
    (Ext: '.pcn'; MimeType: 'application/x-pcn'),
    (Ext: '.ppt'; MimeType: 'application/vnd.ms-powerpoint'),
    (Ext: '.svd'; MimeType: 'application/vnd.svd'),
    (Ext: '.ins'; MimeType: 'application/x-net-install'),
    (Ext: '.ccv'; MimeType: 'application/ccv'),
    (Ext: '.vts'; MimeType: 'workbook/formulaone'),
    (Ext: '.wrl'; MimeType: 'x-world/x-vrml'),
    (Ext: '.vrml'; MimeType: 'x-world/x-vrml'),
    (Ext: '.vrw'; MimeType: 'x-world/x-vream'),
    (Ext: '.p3d'; MimeType: 'application/x-p3d'),
    (Ext: '.svr'; MimeType: 'x-world/x-svr'),
    (Ext: '.wvr'; MimeType: 'x-world/x-wvr'),
    (Ext: '.3dmf'; MimeType: 'x-world/x-3dmf'),
    (Ext: '.ma'; MimeType: 'application/mathematica'),
    (Ext: '.msh'; MimeType: 'x-model/x-mesh'),
    (Ext: '.v5d'; MimeType: 'application/vis5d'),
    (Ext: '.igs'; MimeType: 'application/iges'),
    (Ext: '.dwf'; MimeType: 'drawing/x-dwf'),
    (Ext: '.showcase'; MimeType: 'application/x-showcase'),
    (Ext: '.slides'; MimeType: 'application/x-showcase'),
    (Ext: '.sc'; MimeType: 'application/x-showcase'),
    (Ext: '.sho'; MimeType: 'application/x-showcase'),
    (Ext: '.show'; MimeType: 'application/x-showcase'),
    (Ext: '.ins'; MimeType: 'application/x-insight'),
    (Ext: '.insight'; MimeType: 'application/x-insight'),
    (Ext: '.ano'; MimeType: 'application/x-annotator'),
    (Ext: '.dir'; MimeType: 'application/x-dirview'),
    (Ext: '.lic'; MimeType: 'application/x-enterlicense'),
    (Ext: '.faxmgr'; MimeType: 'application/x-fax-manager'),
    (Ext: '.faxmgrjob'; MimeType: 'application/x-fax-manager-job'),
    (Ext: '.icnbk'; MimeType: 'application/x-iconbook'),
    (Ext: '.wb'; MimeType: 'application/x-inpview'),
    (Ext: '.inst'; MimeType: 'application/x-install'),
    (Ext: '.mail'; MimeType: 'application/x-mailfolder'),
    (Ext: '.pp'; MimeType: 'application/x-ppages'),
    (Ext: '.ppages'; MimeType: 'application/x-ppages'),
    (Ext: '.sgi-lpr'; MimeType: 'application/x-sgi-lpr'),
    (Ext: '.tardist'; MimeType: 'application/x-tardist'),
    (Ext: '.ztardist'; MimeType: 'application/x-ztardist'),
    (Ext: '.wkz'; MimeType: 'application/x-wingz'),
    (Ext: '.iv'; MimeType: 'graphics/x-inventor'));

function FileType2MimeType(const AFileName: string): string;  

var
  Application: TIdExtApplication;

threadvar
  CurrentFCGIThread : TIdExtSession;

implementation
uses
  Controls, StrUtils, SysUtils, IdGlobalProtocols, ExtPascalUtils;

function FileType2MimeType(const AFileName: string): string;
var
  FileExt: string;
  I: Integer;
begin
  Result := 'text/html';
  FileExt := ExtractFileExt(AFileName);
  for I := Low(MIMEExtensions) to High(MIMEExtensions) do
    if SameText(MIMEExtensions[I].Ext, FileExt) then
    begin
      Result := MIMEExtensions[I].MimeType;
      Break;
    end;
end;

{ TIdExtHTTPServer }

constructor TIdExtHTTPServer.Create(const AOwner: TComponent;
  const AExtSessionClass: TIdExtSessionClass);
begin
  ExtSessionClass := AExtSessionClass;
  inherited Create(AOwner);
end;

procedure TIdExtHTTPServer.DoCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  inherited;
  TIdExtSession(ARequestInfo.Session).HandleRequest(ARequestInfo, AResponseInfo);
end;

procedure TIdExtHTTPServer.InitComponent;
begin
  inherited;
  SessionState := True;
  AutoStartSession := True;
  SessionList.Free;
  FSessionList := TIdExtSessionList.Create(Self);
end;

{ TIdExtSessionList }

constructor TIdExtSessionList.Create(const AOwner: TIdExtHTTPServer);
begin
  FOwner := AOwner;
  inherited Create(AOwner);
end;

function TIdExtSessionList.CreateSession(const RemoteIP,
  SessionID: String): TIdHTTPSession;
begin
  Result := FOwner.ExtSessionClass.CreateInitialized(Self, SessionID, RemoteIP);
  SessionList.Add(Result);
end;

{ TIdExtSession }

procedure TIdExtSession.AddToGarbage(const Name: string; Obj: TObject);
begin
  FGarbageCollector.AddObject(AnsiReplaceStr(Name, '_', ''), Obj);
end;

procedure TIdExtSession.AfterHandleRequest;
begin
//
end;

function TIdExtSession.BeforeHandleRequest: boolean;
begin
  Result := True;
end;

constructor TIdExtSession.CreateInitialized(AOwner: TIdHTTPCustomSessionList;
  const SessionID, RemoteIP: String);
begin
  inherited;
  FNewThread := False;
  FGarbageCollector := TStringList.Create;
  FGarbageCollector.Sorted := True;
  InitSessionDefs;  
end;

procedure TIdExtSession.DeleteFromGarbage(Obj: TObject);
var
  I: Integer;
begin
  I := FGarbageCollector.IndexOfObject(Obj);
  if I >= 0 then
    FGarbageCollector.Delete(I);
end;

procedure TIdExtSession.DeleteFromGarbage(Name: string);
var
  I: Integer;
begin
  I := FGarbageCollector.IndexOf(AnsiReplaceStr(Name, '_', ''));
  if I >= 0 then
    FGarbageCollector.Delete(I);
end;

function TIdExtSession.FindObject(Name: string): TObject;
var
  I: Integer;
begin
  I := FGarbageCollector.IndexOf(AnsiReplaceStr(Name, '_', ''));
  if I >= 0 then
    Result := FGarbageCollector.Objects[I]
  else Result := nil;  
end;

destructor TIdExtSession.Destroy;
var
  I: Integer;
begin
  with FGarbageCollector do begin
    for I := 0 to Count-1 do
      try TObject(Objects[I]).Free except end;
    Free;
  end;
  FGarbageCollector := nil;
  inherited;
end;

function TIdExtSession.GetCookie(const CookieName: string): string;
var
  FCookieIndex: Integer;
begin
  FCookieIndex := FCurrentRequest.Cookies.GetCookieIndex(0, CookieName);
  if FCookieIndex >= 0 then
    Result := FCurrentRequest.Cookies[FCookieIndex].CookieText;
end;

function TIdExtSession.GetPathInfo: string;
begin
  Result := FCurrentRequest.Document;
  if (Length(Result) > 0) and (Result[1] = '/') then
    Delete(Result, 1, 1);
end;

function TIdExtSession.GetQuery(const ParamName: string): string;
begin
  Result := FCurrentRequest.Params.Values[ParamName];
end;

function TIdExtSession.GetRequestHeader(const HeaderName: string): string;
begin
  if SameText(HeaderName, 'HTTP_ACCEPT_LANGUAGE') then
    Result := FCurrentRequest.AcceptLanguage
  else
  begin
    Result := FCurrentRequest.RawHeaders.Values[HeaderName];
    if Result = '' then
      Result := FCurrentRequest.CustomHeaders.Values[HeaderName]; 
  end;
end;

procedure TIdExtSession.HandleRequest(ARequest: TIdHTTPRequestInfo;
  AResponse: TIdHTTPResponseInfo);
  function CheckIfFileIsModified(FileName: string): Boolean;
  const
    FCompareDateFmt = 'yyyymmddhhnnss';
  var
    FFileDateTime: TDateTime;
  begin
    Result := True;
    if (ARequest.RawHeaders.Values['if-Modified-Since'] <> '') then
    begin
      FileAge(FileName, FFileDateTime);
      Result := not SameText(FormatDateTime(FCompareDateFmt,
        FFileDateTime), FormatDateTime(FCompareDateFmt,
        StrInternetToDateTime(ARequest.RawHeaders.Values['if-Modified-Since'])));
    end;
  end;
  function TryToServeFile: boolean;
  var
    FileName: string;
    FileDateTime: TDateTime;
  begin
    FileName := ExtractFilePath(ParamStr(0));
    FileName := StringReplace(FileName, ExtractFileDrive(FileName), '', []);
    if (Length(ARequest.Document) > 1) and (ARequest.Document[1]in ['/', '\']) then
      FileName := FileName + Copy(ARequest.Document, 2, MaxInt)
    else FileName := FileName + ARequest.Document;
    FileName := ExpandFilename(FileName);
    if FileExists(FileName) then
    begin
      Result := True;
      if CheckIfFileIsModified(FileName) then
      begin
        AResponse.ContentStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
        AResponse.FreeContentStream := True;
        AResponse.ContentLength := AResponse.ContentStream.Size;
        FileAge(FileName, FileDateTime);
        AResponse.LastModified := FileDateTime;
        AResponse.ContentType := FileType2MimeType(FileName);
      end else AResponse.ResponseNo := 304; //Not Modified, use cache version
    end else Result := false;
  end;
type
  MethodCall = procedure of object;
var
  PageMethod : TMethod;
  MethodCode : pointer;
begin
  CurrentFCGIThread := Self;
  FCurrentRequest := ARequest;
  FCurrentReponse := AResponse;
  if ARequest.Cookies.GetCookieIndex(0, 'FCGIThread') = -1 then
  begin
    with AResponse.Cookies.Add do
    begin
      CookieName := 'FCGIThread';
      Value := SessionID;
      ARequest.Cookies.AddSrcCookie(CookieText);
    end;
  end;
  AResponse.ContentType := 'text/html';  
  Response := '';
  if BeforeHandleRequest then
    if PathInfo = '' then
      Home
    else begin
      MethodCode := MethodAddress(PathInfo);
      if MethodCode <> nil then begin
        PageMethod.Code := MethodCode;
        PageMethod.Data := Self;
        try
          MethodCall(PageMethod); // Call published method
        except
          on E : Exception do OnError(E.Message, PathInfo, FCurrentRequest.QueryParams)
        end;
      end
      else if not TryToServeFile then
        OnNotFoundError;
    end;
  AfterHandleRequest;
  if not Assigned(AResponse.ContentStream) and (Response <> '') and (AResponse.ResponseNo <> 304) then
    FCurrentReponse.ContentText := Response;
end;

procedure TIdExtSession.InitSessionDefs;
begin
  Charset :=  DefaultCharset;
end;

procedure TIdExtSession.Logout;
begin
  Response := 'window.close();';
  FLastTimeStamp := 0;
end;

procedure TIdExtSession.OnError(Msg, Method, Params: string);
begin
  Response := 'alert("' + Msg + '\non Method: ' + Method + '\nParams: ' + Params + '");'
end;

procedure TIdExtSession.OnNotFoundError;
begin
  Response := 'alert("Method: ''' + PathInfo + ''' not found");';
end;

{ TIdExtApplication }

constructor TIdExtApplication.Create(ATitle: string;
  ASessionClass: TIdExtSessionClass; APort, AMaxIdleMinutes: word;
  AMaxConns: integer);
begin
  inherited Create;
  Forms.Application.Initialize;
  Forms.Application.Title := ATitle;
  Forms.Application.CreateForm(TForm, FMainForm);
  FMainForm.OnDestroy := DoTerminate;
  FListBox := TListBox.Create(FMainForm);
  with FListBox do
  begin
    Parent := FMainForm;
    Align := alClient;
  end;
  FServer := TIdExtHTTPServer.Create(FMainForm, ASessionClass);
  FServer.SessionTimeOut := AMaxIdleMinutes;
  FServer.MaxConnections := AMaxConns;
  FServer.ServerSoftware := ATitle;
  FServer.DefaultPort := APort;
  FServer.SessionList.OnSessionStart := DoSessionStart;
  FServer.OnSessionStart := DoSessionStart;
  FServer.SessionList.OnSessionEnd := DoSessionEnd;
  FServer.OnSessionEnd := DoSessionEnd;
end;

procedure TIdExtApplication.DoSessionEnd(Sender: TIdHTTPSession);
begin
  FListBox.Items.Add(Format('Session %0:s closed', [Sender.SessionID]));
end;

procedure TIdExtApplication.DoSessionStart(Sender: TIdHTTPSession);
begin
  FListBox.Items.Add(Format('Session %0:s started from IP: %1:s',
    [Sender.SessionID, Sender.RemoteHost]));
end;

procedure TIdExtApplication.DoTerminate(Sender: TObject);
begin
  FServer.OnSessionStart := nil;
  FServer.SessionList.OnSessionStart := nil;
  FServer.OnSessionEnd := nil;
  FServer.SessionList.OnSessionEnd := nil;
end;

procedure TIdExtApplication.Run;
begin
  FMainForm.Show;
  FServer.Startup;
  Forms.Application.Run;
end;

function TIdExtApplication.Title: string;
begin
  Result := Forms.Application.Title;
end;

end.
