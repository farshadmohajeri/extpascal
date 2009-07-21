{$I epDirectives.inc}

unit epUtils;

interface

uses Classes, SysUtils, TypInfo, IniFiles;

type
  TTipoUserinfo = (tuUserName, tuLocalComputer, tuUserChv, tuUserId, tuLogonSrv, tuUserDomain, tuUserFullName, tuUserFullNameAndUser, tuUserGroups);

  EExtP = class(Exception)
    constructor CreateFmt(Msg : string; Params : array of const; LogError : Boolean = false);
    constructor Create(const Msg: string);
  end;

  TParser = class
  protected
    Line        : string;
    Source      : text;
    ParseInMemo : Boolean;
  public
    constructor Create(FileorLine : string; ParseMemo : Boolean = False); overload;
    destructor Destroy; override;
    function GetNextToken(Start, Finish : string): string;
    function GetToken(Finish : array of string): string;
    function GetIdentifier: string;
    function NextToken(Tokens : array of string): integer;
    function NextTokenInLine(Tokens : array of string): integer;
  end;

  TUF = (__0, AC, AL, AM, AP, BA, CE, DF, ES, FN, GO, MA, MG, MS, MT, PA, PB, PE, PI, PR, RJ, RN, RO, RR, RS, SC, SE, SP, TO_);

  DateTime = TDateTime;       // Alias para TDateTime. Use DateTime no lugar de TDateTime.
  TDate    = type TDateTime;
  TTime    = type TDateTime;
  Date     = TDate;           // Alias para TDate. Use Date no lugar de TDate.
  Time     = TTime;           // Alias para TTime. Use Time no lugar de TTime.

  PGroupUsersInfo = ^TGroupUsersInfo;
  TGroupUsersInfo = record
    grui0_name: PWideChar;
  end;
  PGroupLocalUsersInfo = ^TGroupLocalUsersInfo;
  TGroupLocalUsersInfo = record
    lgrui0_name: PWideChar;
  end;

  PLocalGroup_Members_Info_3 = ^LocalGroup_Members_Info_3;
  LocalGroup_Members_Info_3 = record
    lgrmi3_domainandname: PWideChar;
  end;

  TDateTimeType = (dtNone, dtDate, dtTime, dtDateTime);

  TMethodParams = array of variant;

var
  NoService  : boolean = false;
  ServerName : string;
  RecoverInits    : array of TProcedure;
  Initializations,
  Finalizations   : array of pointer;

const
  OTHERTOKEN = '';
  Parser     : TParser = nil;
  NETAPI32   = 'netapi32.dll';
  MAX_PREFERRED_LENGTH = Cardinal(-1);

//fun??es Gerais
//function MontaValor(Vez : integer) : string;
function CaseOf(S : string; Cases : array of string) : integer; overload;
function CaseOf(S : integer; Cases : array of string) : string; overload;
procedure Trace(Msg : string; Params: array of const); overload;
procedure Trace(Msg : string); overload;
function CobolToCurr(Valor : string; Default : Currency = 0.0) : Currency;
function CobolToDouble(Cobol: array of Char; Size, Decimals: Byte): Double;
procedure DateToCobol(d: TDateTime; var Cobol: array of Char);
function CobolToDate(Cobol: array of Char; const Formato: string = 'yyyymmdd'): TDateTime;
procedure DoubleToCobol(d: Double; var Cobol: array of Char; Size, Decimals: Byte);
procedure IntToCobol(i: Int64; var Cobol: array of Char; Size: Byte);
procedure StringToCobol(s: string; var Cobol: array of Char; Size: Byte);
function VarToWord(const V: Variant): word;
function VarToInt(const V: Variant): integer;
function VarToI64(const V: Variant): int64;
function VarToDbl(const V: Variant): double;
function TimeToStrElapsed(const aTime: TDateTime; const ScientificNotation: Boolean = false): string;
procedure AddToStr(var S: string; const AddS: string; const Delimiter: string = #13);
function CalcularDvModulo10(Numero: Int64): Byte;
function CalcularDvModulo11(Numero: Int64): Byte;
function ExtractFromStr(const S: string; var Pos: Integer; const Delimiters: string = #13#10): string; overload;
function ExtractFromStr(const S: string; const Delimiters: string = #13#10): string; overload;
function LengthRegExp(Rex : string; CountAll : Boolean = true) : integer;
function ImportDate(S : string; I : integer) : TDate;
function ImportInteger(S : string; I, Len : integer) : integer;
function SendMail(De, Para, Subj, Msg : String) : boolean; // Para mais de um destinat?rio separar por v?rgulas, para Msg com mais de uma linha usar #13
function ValidarCpf(CPF: Int64): Boolean;
function ValidarCnpj(CNPJ: Int64): Boolean;
function ValidarPIS(PIS : Int64): Boolean;
function ValidarModulo10(Numero: int64; Size: Byte): Boolean;
function ValidarModulo11(Numero: int64): Boolean;
//Seguran?a
function GetUserInfo(TipoUserInfo : TTipoUserinfo; Usr : String = ''; Srv: String = '') : Variant;
function GetIniParameter(Section, Param : string; Default : string = '') : string;
function GetUserName: string;
function GetComputerName: string;
function GetHostIPByName(Hst : String) : String;
function CheckServer(Srv : String) : Boolean;
function GetUserGroups(Usr : String; Srv : String = '') : String;
function GetProfileUsers(Grp : String) : String;
function GetLoginKey(Usr : String = ''; Srv: String = '') : String;
// Alias para IntToStr
function S(Value : integer; Precision : integer = 0) : string; overload;
function S(Value : Int64; Precision : integer = 0) : string; overload;

function FixedLen(const Value: integer; const Digits: Integer): string; overload;
function FixedLen(const Value: Int64; const Digits: Integer): string; overload;
function FixedLen(const Value: Extended; const IntDigits, FracDigits: Integer): string; overload;
function FixedLen(const Value: string; const Digits: integer): string; overload;
function FixedLenDate(const Value: TDateTime): string;
function FixedLenDateTime(const Value: TDateTime): string;
function FixedLenTime(const Value: TDateTime): string;

function GetDateTimeType(const TypeName: string): TDateTimeType;
function AdjustEnumerationOrSet(E : string) : string;
function CertifyEnum(TypeInfo : PTypeInfo; Value, Default : integer) : integer;
function EnumToStr(const TypeInfo : PTypeInfo; const Value : integer) : string;
function epIniFile: TIniFile;
function CallBatch(Batch : String) : TStringList;
function ExpandMacros(Macros : array of string; S : string) : string;
function GetFCGIThreadID : integer;

implementation

uses //Windows, 
   StrUtils, DateUtils, Math, Process, epCommon, FCGIApp;
{
function NetApiBufferFree(Buffer: Pointer): Integer; stdcall; external NETAPI32 name 'NetApiBufferFree';
function NetUserGetGroups(servername, username: PWideChar; level: Cardinal; var bufptr: Pointer; prefmaxlen: Cardinal; var entriesread, totalentries: Cardinal): Integer; stdcall; external NETAPI32 name 'NetUserGetGroups';
function NetUserGetLocalGroups(servername, username: PWideChar; level: Cardinal; flags: Cardinal; var bufptr: Pointer; prefmaxlen: Cardinal; var entriesread, totalentries: Cardinal): Integer; stdcall; external NETAPI32 name 'NetUserGetLocalGroups';
function NetUserGetInfo(Server : PWideChar; UserName : PWideChar; Level : DWORD; Buffer : Pointer) : LongInt; stdcall; external NETAPI32;
function NetServerGetInfo(const servername: PWChar; level: DWORD; bufptr: Pointer): DWORD; stdcall; external NETAPI32 name 'NetServerGetInfo';
function NetGetDCName(Server, Domain: pWideChar; var DC: pWideChar): LongInt; stdcall; external NETAPI32;
function NetGetAnyDCName(Server, Domain: pWideChar; var DC: pWideChar): LongInt; stdcall; external NETAPI32;
function NetGroupGetUsers(servername, groupname: PWideChar; level: Cardinal; var bufptr: Pointer; prefmaxlen: Cardinal; var entriesread, totalentries, resumeHandle: Cardinal): Integer; stdcall; external NETAPI32 name 'NetGroupGetUsers';
function NetLocalGroupGetMembers(servername, groupname: PWideChar; level: Cardinal; var bufptr: Pointer; prefmaxlen: Cardinal; var entriesread, totalentries, resumeHandle: cardinal): integer;  stdcall; external NETAPI32 name 'NetLocalGroupGetMembers';
}
constructor TParser.Create(FileorLine : string; ParseMemo : Boolean = false); begin
  inherited Create;
  if not ParseMemo then begin
    if not FileExists(FileorLine) then raise EExtP.CreateFmt('Erro no create do Parser. Arquivo: %s n?o existe', [FileorLine], true);
    assign(Source, FileorLine);
    reset(Source);
    ParseInMemo := false;
  end
  else begin
    ParseInMemo := true;
    Line := FileorLine;
  end;
end;

destructor TParser.Destroy; begin
  if not ParseInMemo then try close(Source) except end;
  inherited
end;

function TParser.NextTokenInLine(Tokens : array of string) : integer;
var
  T, I, Max : integer;
begin
  Result := -1;
  Max := MAXINT;
  Line := trimleft(Line);
  for T := 0 to high(Tokens) do begin
    if Tokens[T] = OtherToken then
      I := 1
    else
      I := pos(Uppercase(Tokens[T]), Uppercase(Line));
    if (I > 0) and (I < Max) then begin
      Max := I;
      Result := T;
    end;
  end;
  if Result <> -1 then
    Line := trimleft(copy(Line, Max + length(Tokens[Result]), length(Line)));
end;

function TParser.NextToken(Tokens : array of string) : integer;

  procedure ProcuraToken;
  var
    T, I, Max : integer;
  begin
    Line := trimleft(Line);
    Max := MAXINT;
    for T := 0 to high(Tokens) do begin
      if Tokens[T] = OtherToken then
        I := 1
      else
        I := pos(Uppercase(Tokens[T]), Uppercase(Line));
      if (I > 0) and (I < Max) then begin
        Max := I;
        Result := T;
      end;
    end;
    if Result <> -1 then
      Line := trimleft(copy(Line, Max + length(Tokens[Result]), length(Line)));
  end;

begin
  Result := -1;
  if not ParseInMemo then
    repeat
      while (Line = '') and not EOF(Source) do readln(Source, Line);
      ProcuraToken;
      if Result = -1 then Line := '';
    until (Result <> -1) or EOF(Source)
  else
    ProcuraToken;
end;

function TParser.GetIdentifier : string; begin
  Result := '';
  if not ParseInMemo then
    while (Line = '') and not EOF(Source) do readln(Source, Line);
  Line := trimleft(Line);
  while (Line <> '') and (upcase(Line[1]) in ['A'..'Z', '_', '0'..'9']) do begin
    Result := Result + Line[1];
    delete(Line, 1, 1)
  end;
end;

function TParser.GetToken(Finish : array of string) : string;

  function ReplaceAlias(S : string) : string; begin
    Result := AnsiReplaceText(S,      '&lt;', '<');
    Result := AnsiReplaceText(Result, '&gt;', '>');
    Result := AnsiReplaceText(Result, '&quot;', '"');
  end;

var
  I, J : integer;
begin
  Result := ''; I := 0; J := 0;
  if not ParseInMemo then
    repeat
      if Line <> '' then
        for J := 0 to high(Finish) do begin
          I := pos(Uppercase(Finish[J]), Uppercase(Line));
          if I <> 0 then begin
            Result := Result + ReplaceAlias(copy(Line, 1, I-1));
            break
          end;
        end;
      if I = 0 then begin
        Result := Result + ReplaceAlias(Line);
        readln(Source, Line);
        Result := TrimRight(Result) + ^M^J;
      end;
    until (I <> 0) or EOF(Source)
  else
    for J := 0 to high(Finish) do begin
      I := pos(Uppercase(Finish[J]), Uppercase(Line));
      if I <> 0 then begin
        Result := Result + ReplaceAlias(copy(Line, 1, I-1));
        break;
      end;
    end;
  if I <> 0 then
    Line := trim(copy(Line, I + length(Finish[J]), length(Line)));
end;

function TParser.GetNextToken(Start, Finish : string) : string; begin
  NextToken([Start]);
  Result := GetToken(Finish)
end;

function CaseOf(S : string; Cases : array of string) : integer;
var
  I : integer;
begin
  Result := -1;
  for I := 0 to high(Cases) do
    if UpperCase(S) = UpperCase(Cases[I]) then begin
      Result := I;
      exit;
    end;
end;

function CaseOf(S : integer; Cases : array of string) : string; begin
  if S in [low(Cases)..high(Cases)] then
    Result := Cases[S]
  else
    Result := '';
end;

constructor EExtP.CreateFmt(Msg : string; Params: array of const; LogError : Boolean = false);
var
  ErrorFile : string;
  Error : text;
begin
  if Length(Params) > 0 then Msg := Format(Msg, Params);
  if LogError and (pos('ExtPascalUML', paramstr(0)) = 0) then begin
    ErrorFile := ChangeFileExt(paramstr(0), '.trc');
    assign(Error, ErrorFile);
    if FileExists(ErrorFile) then
      append(Error)
    else
      rewrite(Error);
    if pos('EAbort', Msg) = 0 then begin
      writeln(Error, DateTimeToStr(Now), ': ', Msg);
      if NoService then writeln(DateTimeToStr(Now), ': ', Msg);
    end;
    close(Error);
  end
  else
    inherited Create(Msg);
end;

constructor EExtP.Create(const Msg : string); begin
  CreateFmt(Msg, [])
end;

procedure BlankToZero(var s: string);
var
  i : Integer;
begin
  for i := 1 to length(s) do
    if s[i] = ' ' then s[i] := '0';
end;

procedure Trace(Msg : string; Params: array of const); begin
  EExtP.CreateFmt(Msg, Params, true);
end;

procedure Trace(Msg : string); begin
  EExtP.CreateFmt(Msg, [], true);
end;

function CobolToCurr(Valor : string; Default : Currency = 0.0) : Currency;
var
  Tam : integer;
begin
  Valor := trim(Valor);
  Tam := length(Valor);
  case Valor[Tam] of // Converte o bit zonado do EBCDIC
    'A'..'I' : dec(byte(Valor[Tam]), 16);
    '{' : Valor[Tam] := '0';
    'J'..'R' : begin
      dec(byte(Valor[Tam]), 25);
      Valor := '-' + Valor;
    end;
    '}' : begin
      Valor[Tam] := '0';
      Valor := '-' + Valor;
    end;
  end;
  System.Insert(DecimalSeparator, Valor, length(Valor)-1);
  Result := StrToCurrDef(Valor, Default)
end;

procedure DateToCobol(d: TDateTime; var Cobol: array of Char);
var
  s : string;
begin
  s := FormatDateTime('yyyymmdd', d);
  try
    FillChar(Cobol, 8, ' ');
    Move(s[1], Cobol, Min(8, Length(s)));
 except
 end;
end;

procedure DoubleToCobol(d: Double; var Cobol: array of Char; Size, Decimals: Byte);
var
  s : string;
begin
  try
    Inc(Size);
    Str(d:Size:Decimals, s);
    if Decimals = 0 then
       Delete(s, 1, 1)
    else
       Delete(s, Size-Decimals, 1);
    BlankToZero(s);
    Move(s[1], Cobol, Min(Size, Length(s)));
  except
  end;
end;

function CobolToDate(Cobol: array of Char; const Formato: string = 'yyyymmdd'): TDateTime;
var
  sShortDateFormat : string;
begin
  if StrToIntDef(Cobol, 0) = 0 then begin
    Result := 0;
    exit;
  end;
  // Guarda o formato de data atual
  sShortDateFormat := ShortDateFormat;
  try
    // Data no formato 'yyyymmdd'
    if High(Cobol) = 7 then
      if Formato = 'yyyymmdd' then begin
        ShortDateFormat := 'yyyymmdd';
        Result := StrToDate(Copy(Cobol, 1, 4) + '/' +
        Copy(Cobol, 5, 2) + '/' + Copy(Cobol, 7, 2));
      end
      else
        if Formato = 'ddmmyyyy' then begin
          ShortDateFormat := 'ddmmyyyy';
          Result := StrToDate(Copy(Cobol, 1, 2) + '/' + Copy(Cobol, 3, 2) + '/' + Copy(Cobol, 5, 4));
        end
        // resolve datas invalidas para 31/12/1898
        else
          Result := 0
    // Data no formato 'yymmdd'
    else begin
      ShortDateFormat := 'yymmdd';
      Result := StrToDate(Copy(Cobol, 1, 2) + '/' + Copy(Cobol, 3, 2) + '/' + Copy(Cobol, 5, 2))
    end;
  except
    Result := 0; // resolve datas invalidas para 31/12/1898
  end;
  // Restaura o formato padr?o para datas
  ShortDateFormat := sShortDateFormat;
end;

procedure IntToCobol(i: Int64; var Cobol: array of Char; Size: Byte);
var
  s : string;
begin
  try
    Str(i:Size, s);
    BlankToZero(s);
    Move(s[1], Cobol, Min(Size, Length(s)))
  except
  end;
end;

procedure StringToCobol(s: string; var Cobol: array of Char; Size: Byte); begin
  try
    FillChar(Cobol, Size, ' ');
    Move(s[1], Cobol, Min(Size, Length(s)));
  except
  end;
end;

function CobolToDouble(Cobol: array of Char; Size, Decimals: Byte): Double;
var
  Negativo : Boolean;
  s        : string[20];
begin
  try
    // Copia o array de char para a string
    s := StringOfChar(' ', Size);
    Move(Cobol, s[1], Min(Size, Length(s)));
    // Identifica se o n?mero ? negativo (posi??o mais a esquerda do n?mero)
    Negativo := (s[1] = '-');
    // Se a posi??o "1" contiver um sinal, Copia um "0" sobre o mesmo
    if s[1] in ['+', '-'] then s[1] := '0';
    s[0] := Char(Size);
    Result := StrToFloatDef(s, 0) / Power(10, Decimals);
    if Negativo then Result := -Result;
  except
    Result := 0;
  end;
end;

function ValidarCpf(CPF: Int64): Boolean;
const
  SeqInvalidas = '11111111111,22222222222,33333333333,44444444444,55555555555,66666666666,77777777777,88888888888,99999999999';
var
  Str : string;
  Tot, I : integer;
begin
  Result := False;
  if CPF <= 0 then Exit;
  Str := format('%.11d', [CPF]);
  if Pos(str, SeqInvalidas) > 0 then Exit;
  Tot := 0;
  for I := 2 to 10 do
    inc(Tot, I * (ord(Str[11 - I]) - 48));
  Tot := (Tot * 10) mod 11;
  if Tot = 10 then Tot := 0;
  if not(char(Tot + 48) = Str[10]) then exit;
  Tot := 0;
  for I := 2 to 11 do
    inc(Tot, I * (ord(Str[12 - I]) - 48));
  Tot := (Tot * 10) mod 11;
  if Tot = 10 then Tot := 0;
  Result := char(Tot + 48) = Str[11];
end;

function ValidarCnpj(CNPJ: Int64): Boolean;
const
  SeqInvalidas = '11111111111111,22222222222222,33333333333333,44444444444444,55555555555555,66666666666666,' +
    '77777777777777,88888888888888,99999999999999';
var
  Str : string;
  Total, I, Peso : integer;
begin
  Result := False;
  if CNPJ <= 0 then Exit;
  Str := format('%.14d', [CNPJ]);
  Total := 0;
  Peso  := 2;
  // CNPJs inv?lidos
  if (copy(Str, 1, 8) = '29979036') or       // C?digo de CGC especial
    (StrToIntDef(Copy(Str, 9, 4), 0) = 0) or // Nr. da filial zerada
    (pos(Str, SeqInvalidas) > 0) then        // Sequencias inv?lidas
    exit;
  for I := 1 to 12 do begin
    Inc(Total, Peso * (Ord(Str[13-I]) - 48));
    Inc(Peso);
    if Peso = 10 then Peso := 2;
  end;
  Total := (Total * 10) mod 11;
  if Total = 10 then Total := 0;
  if not (char(Total + 48) = Str[13]) then exit;
  Total := 0;
  Peso  := 2;
  for I := 1 to 13 do begin
    inc(Total, Peso * (ord(Str[14-I])-48));
    inc(Peso);
    if Peso = 10 then Peso:= 2;
  end;
  Total := (Total * 10) mod 11;
  if Total = 10 then Total := 0;
  Result := char(Total+48) = Str[14]
end;

function ValidarPis(PIS : Int64): Boolean;
var
  I, Soma, Mod11, Digito : Integer;
  Str : string;
begin
  Result := false;
  Str    := format('%.11d', [PIS]);
  if length(Str) <> 11 then exit;
  Soma  := 0;
  Mod11 := 2;
  for I := 1 to 10 do begin
    inc(Soma, Mod11 * ord(Str[11-I])-48);
    if Mod11 < 9 then
      inc(Mod11)
    else
      Mod11 := 2;
  end;
  Digito := 11 - (Soma mod 11);
  if Digito > 9 then Digito := 0;
  Result := char(Digito+48) = Str[11];
end;

function ImportDate(S : string; I : integer) : TDate; begin
  Result := StrToDateDef(trim(copy(S, I+6, 2)) + '/' + trim(copy(S, I+4, 2)) + '/' + trim(copy(S, I, 4)), 0)
end;

function ImportInteger(S : string; I, Len : integer) : integer; begin
  Result := StrToIntDef(trim(copy(S, I, Len)), 0)
end;

function LengthRegExp(Rex : string; CountAll : Boolean = true) : integer;
var
  Slash, I : integer;
  N : string;
begin
  Result := 0;
  N := '';
  Slash := 0;
  for I := 1 to length(Rex) do
    case Rex[I] of
      '\' :
        if CountAll and (I < length(Rex)) and (Rex[I+1] in ['d', 'D', 'l', 'f', 'n', 'r', 's', 'S', 't', 'w', 'W']) then inc(Slash);
      ',', '{' : begin
        N := '';
        if Slash > 1 then begin
          inc(Result, Slash);
          Slash := 0;
        end;
      end;
      '}' : begin
        inc(Result, StrToIntDef(N, 0));
        N := '';
        dec(Slash);
      end;
      '0'..'9' : N := N + Rex[I];
      '?' : inc(Slash);
      '*' :
        if not CountAll then begin
          Result := -1;
          exit;
        end;
    end;
  inc(Result, Slash);
end;

function VarToInt(const V: Variant): integer; begin
  if TVarData(V).VType <> varNull then Result := V else Result := 0;
end;

function VarToWord(const V: Variant): word; begin
  if TVarData(V).VType <> varNull then Result := V else Result := 0;
end;

function VarToI64(const V: Variant): int64; begin
  if TVarData(V).VType <> varNull then Result := V else Result := 0;
end;

function VarToDbl(const V: Variant): double; begin
  if TVarData(V).VType <> varNull then Result := V else Result := 0;
end;

function TimeToStrElapsed(const aTime : TDateTime; const ScientificNotation : Boolean = false): string;
// note: maximum length of result = 17 ("00h 00m 00s 000ms")
var
  H, M, S, MSec: Word;

  procedure CheckAddSpace; begin
    if Result <> '' then Result := Result + ' ';
  end;

  procedure TryAdd(const Value : Word; const Suffix : char); begin
    if Value > 0 then begin
      CheckAddSpace;
      Result := Result + IntToStr(Value) + Suffix;
    end;
  end;

begin
  if ScientificNotation then begin
    Result := FormatDateTime('nn:ss:zzz', aTime) + '''''''';
    Result[3] := '''';
    System.Delete(Result, 6, 1);
    System.Insert('''''', Result, 6);
  end
  else begin
    DecodeTime(aTime, H, M, S, MSec);
    Result := '';
    TryAdd(H, 'h');
    TryAdd(M, 'm');
    TryAdd(S, 's');
    if (MSec > 0) or (Result = '') then begin
      CheckAddSpace;
      Result := Result + IntToStr(MSec) + 'ms';
    end;
  end;
end;

procedure AddToStr(var S: string; const AddS: string; const Delimiter: string = #13); begin
  S := S + AddS + Delimiter;
end;

function ExtractFromStr(const S: string; var Pos: Integer; const Delimiters: string = #13#10): string;
var
  I: Integer;

  function IsDelimiter: Boolean; begin
    Result := System.Pos(S[I], Delimiters) > 0;
  end;
  
begin
  I := Pos;
  while (I <= Length(S)) and not IsDelimiter do Inc(I);
  Result := Copy(S, Pos, I - Pos);
  inc(I);
  Pos := I;
end;

function ExtractFromStr(const S: string; const Delimiters: string = #13#10): string;
var
  Pos: integer;
begin
  Pos := 1;
  Result := ExtractFromStr(S, Pos, Delimiters);
end;

function SendMail(De, Para, Subj, Msg : String) : boolean;
var
  Srv  : String;
  MsgS : TStringList;
begin
  Srv := GetIniParameter('Server', 'EMAILSERVER');
  if Srv = '' then begin
    Trace('Servidor de Email n?o definido no arquivo ini');
    Result := false;
    exit;
  end;
  MsgS := TStringList.Create;
  try
    MsgS.Add(Msg);
    MsgS.Add(#13#10#13#10'Enviado por ExtPascalUML');
    //Result := SendTo(De, Para, Subj, Srv, MsgS);
  finally
    MsgS.free
  end;
  Result := true;
end;

// fun??es para _Security - in?cio
function GetUserName: string;
var
  I : cardinal;
begin
  I := 255;
  SetLength(Result, I);
//  if Windows.GetUserName(PChar(Result), I) then
    Result := string(PChar(Result))
//  else
//    Result := '';
end;

function GetComputerName: string; begin
//  I := MAX_COMPUTERNAME_LENGTH + 1;
//  SetLength(Result, I);
//  Windows.GetComputerName(PChar(Result), I);
  Result := '';//string(PChar(Result));
end;

function GetHostIPByName(Hst : String) : String;
//var
//I : Integer;
//  HEnt: pHostEnt;
begin
    while pos('\',Hst) = 1 do delete(Hst, 1, 1);
(*    HEnt := GetHostByName(Pointer(Hst));
    for I := 0 to HEnt^.h_length - 1 do
     Result :=
      Concat(Result,
      IntToStr(Ord(HEnt^.h_addr_list^[I])) + '.');*)
    Result := ''; 
    SetLength(Result, Length(Result) - 1);
end;

const
  DefaultDomain : string = 'nil';

function GetDefaultDomain : string; begin
  if DefaultDomain = 'nil' then DefaultDomain := GetIniParameter('Server', 'DefaultDomain', '');
  Result := DefaultDomain;
end;

function GetUserDomain(Usr, Srv : String) : String;
//var
//  sid           : PSid;
//  peUse         : SID_NAME_USE;
//  sidLen,
//  domainNameLen : DWORD;
//  domainName    : array [0..255] of char;
begin
  Result := GetDefaultDomain;
{  if Result <> '' then exit;
  Result := ''; exit;
  sidLen := 256; domainNameLen := 256;
  GetMem (sid, sidLen);
  if Usr = '' then Usr := GetUserInfo(tuUserName);
  try
    if Srv = '' then begin
      if LookupAccountName(nil, PChar(Usr), sid, sidLen, domainName, domainNameLen, peUse) then
        Result := copy(DomainName, 1, domainNameLen);
    end
    else
      if LookupAccountName(PChar(Srv), PChar(Usr), sid, sidLen, domainName, domainNameLen, peUse) then
        Result := copy(DomainName, 1, domainNameLen);
  finally
    FreeMem(sid)
  end;}
end;

function GetUserId(Usr : String = ''; Srv : String = '') : String;
{type
  TUserInfo20  = record
    usri20_name     : LPWSTR;
    usri20_full_name: LPWSTR;
    usri20_comment  : LPWSTR;
    usri20_flags    : DWORD;
    usri20_user_id  : DWORD;
  end;
var
  Ret : DWORD;
  TheUser   : Array[0..255] Of WideChar;
  TheServer : Array[0..255] Of WideChar;
  MyInfo    : TUserInfo20;
  MyPtr     : Pointer;
  NetResult, T : integer;
}begin{
  MyPtr := nil; Ret := 0;
  if Usr = '' then Usr := GetUserInfo(tuUsername);
  StringToWideChar(Usr, @TheUser, 255);
  if Srv = '' then Srv := GetUserInfo(tuLogonSrv);
  StringToWideChar(Srv, @TheServer, 255);
  for T := 1 to 3 do begin
    NetResult := NetUserGetInfo(@TheServer, @TheUser, 20, @MyPtr);
    if (NetResult = ERROR_SUCCESS) or (NetResult = ERROR_MORE_DATA) then
      break
    else begin
      sleep(300);
      Trace('NetUserGetInfo(%s, %s, ... Erro <%s> Tentativa<%d>', [Srv, Usr, SysErrorMessage(Netresult), T]);
    end;
  end;
  If MyPtr <> nil then begin
    MyInfo := TUserInfo20(MyPtr^);
    Ret := MyInfo.usri20_user_id;
  end;
  Result := IntToStr(Ret);}
  Result := ''
end;

function GetUserFullName(Usr : String = ''; Srv : String = ''; NameAndUser : boolean = false) : String;
{type
  TUserInfo20  = record
    usri20_name     : LPWSTR;
    usri20_full_name: LPWSTR;
    usri20_comment  : LPWSTR;
    usri20_flags    : DWORD;
    usri20_user_id  : DWORD;
  end;
var
  TheUser   : Array[0..255] Of WideChar;
  TheServer : Array[0..255] Of WideChar;
  MyInfo    : TUserInfo20;
  MyPtr     : Pointer;
  DomainName,
  UserName,
  ServerName: string;
  NetResult, T : integer;
}begin{
  MyPtr := nil;
  if Srv = '' then ServerName := GetUserInfo(tuLogonSrv) else ServerName := Srv;
  if pos('\', Usr) > 0 then begin
    UserName := copy(Usr, pos('\', Usr) + 1, MaxInt);
    DomainName := copy(Usr, 1, pos('\', Usr) - 1);
  end
  else begin
    if Usr = '' then UserName := GetUserInfo(tuUsername) else UserName := Usr;
    DomainName := GetUserDomain(UserName, ServerName);
  end;
  StringToWideChar(UserName, @TheUser, 255);
  StringToWideChar(ServerName, @TheServer, 255);
  for T := 1 to 3 do begin
    NetResult := NetUserGetInfo(@TheServer, @TheUser, 20, @MyPtr);
    if (NetResult = ERROR_SUCCESS) or (NetResult = ERROR_MORE_DATA) then
      break
    else begin
      sleep(300);
      Trace('NetUserGetInfo(%s, %s, ... Erro <%s> Tentativa<%d>', [ServerName, UserName, SysErrorMessage(Netresult), T]);
    end;
  end;
  If MyPtr <> nil then begin
    MyInfo:=TUserInfo20(MyPtr^);
    if NameAndUser then
      Result:=  MyInfo.usri20_full_name  + ' (' + DomainName + '\' + UserName + ')'
    else
      Result:= MyInfo.usri20_full_name;
  end
  else}
    Result:='';
end;

function GetLogonSrv(Usr : String = ''; Dom : String = ''; AnyDC : boolean = true) : String;
{var
  TheDomain : array[0..255] Of WideChar;
  MyPtr : Pointer;
  NetResult, T : integer;
  Dev : boolean;
}begin{
  Result := '';
  MyPtr  := nil;
  if Usr = '' then Usr := GetUserInfo(tuUsername);
  if Dom = '' then Dom := GetUserInfo(tuUserDomain);
  StringToWideChar(Dom, @TheDomain, 255);
  Dev := UpperCase(GetIniParameter('Server', 'ENVIRONMENT', 'PRODUCTION')) = 'DEVELOPMENT';
  for T := 1 to 3 do begin
    if Dev then
      //NetResult := NetGetAnyDCName(nil, @TheDomain, PWideChar(MyPtr))
    else
      //NetResult := NetGetDCName(nil, @TheDomain, PWideChar(MyPtr));
    if (NetResult = ERROR_SUCCESS) or (NetResult = ERROR_MORE_DATA) then
      break
    else begin
      sleep(300);
      Trace('NetGetDCName/NetGetAnyDCName(nil, %s, ... Erro <%s> Tentativa<%d>', [Dom, SysErrorMessage(Netresult), T]);
    end;
  end;
  Result := WideCharToString(MyPtr);
  if Result = '' then Result := GetComputerName;
}Result := ''
end;

function GetChv(Usr : String = ''; Srv : String = '') : String; begin
  if Usr = '' then Usr := GetUserInfo(tuUsername);
  RandSeed := GetUserInfo(tuUserId, Usr, Srv);
  Result := IntToStr(Random(MAXINT));
end;

function CheckServer(Srv : String) : Boolean;
var
  TheServer : array[0..255] of WideChar;
  MyPtr : Pointer;
  //NetResult, 
  T : integer;
begin
  MyPtr := nil;
  StringToWideChar(Srv, @TheServer, 255);
  for T := 1 to 3 do begin
{    NetResult := NetServerGetInfo(@TheServer, 101, @MyPtr);
    if (NetResult = ERROR_SUCCESS) or (NetResult = ERROR_MORE_DATA) then
      break
    else begin
      sleep(300);
      Trace('NetServerGetInfo(%s, ... Erro <%s> Tentativa<%d>', [Srv, SysErrorMessage(Netresult), T]);
    end;}
  end;
  Result := MyPtr <> nil
end;

function GetUserGroups(Usr : String; Srv : String = '') : String;
var
//  ServerNameW,
//  UserNameW : WideString;
//  Buffer    : Pointer;
//  NetResult, I, 
  T : integer;
//  Read,
//  Total     : Cardinal;
//  P         : PGroupUsersInfo;
begin
  Result :='';
  if Usr = '' then Usr := GetUsername;
  if Srv = '' then Srv := GetLogonSrv(Usr);
//  ServerNameW := Srv;
//  UserNameW := Usr;
  if UpperCase(GetIniParameter('Server', 'ENVIRONMENT', 'PRODUCTION')) <> 'DEVELOPMENT' then
    for T := 1 to 3 do begin
{      NetResult := NetUserGetGroups(PWideChar(ServerNameW), PWideChar(UserNameW), 0, Buffer, MAX_PREFERRED_LENGTH, Read, Total);
      if (NetResult = ERROR_SUCCESS) or (NetResult = ERROR_MORE_DATA) then
        break
      else begin
        sleep(300);
        Trace('NetUserGetGroups(%s, %s ... Erro <%s> Tentativa<%d>', [ServerNameW, UserNameW, SysErrorMessage(Netresult), T]);
      end;
    end
  else
    for T := 1 to 3 do begin
      NetResult:= NetUserGetLocalGroups(nil, PWideChar(UserNameW), 0, 0, Buffer, MAX_PREFERRED_LENGTH, Read, Total);
      if (NetResult = ERROR_SUCCESS) or (NetResult = ERROR_MORE_DATA) then
        break
      else begin
        sleep(300);
        Trace('NetUserGetLocalGroups(nil, %s... Erro <%s> Tentativa<%d>', [UserNameW, SysErrorMessage(Netresult), T]);
      end;
    end;
  case NetResult of
    ERROR_SUCCESS, ERROR_MORE_DATA:
      try
        P := Buffer;
        for I := 0 to Read - 1 do
          with P^ do begin
            AddToStr(Result, WideCharToString(grui0_name));
            inc(P);
          end;
      finally
        NetApiBufferFree(Buffer);
      end;
    else begin
      Trace('NetUserGetLocalGroups(nil, %s... Erro <%s>', [UserNameW, SysErrorMessage(Netresult)]);
      raise Exception.Create('NetUserGetGroups erro: ' + syserrormessage(Netresult));
    end;}
  end;
end;

function GetProfileUsers(Grp : String) : String;
//var
//  ServerNameW,
//  GroupNameW : WideString;
//  Buffer    : Pointer;
//  NetResult, I, T : integer;
//  Read, Total, ResumeHandle : cardinal;
//  P : PLocalGroup_Members_Info_3;
//  P1 : PGroupUsersInfo;
//  UserName, FullUserName : string;
begin
  Result :='';
//  GroupNameW := Grp;
//  Buffer := nil;
{  ResumeHandle := 0;
  try
    try
      ServerNameW := GetLogonSrv(GetUsername);
      if UpperCase(GetIniParameter('Server', 'ENVIRONMENT', 'PRODUCTION')) <> 'DEVELOPMENT' then begin
        for T := 1 to 3 do {begin
          NetResult := NetGroupGetUsers(PWideChar(ServerNameW), PWideChar(GroupNameW), 0, Buffer, MAX_PREFERRED_LENGTH, Read, Total, ResumeHandle);
          if (NetResult = ERROR_SUCCESS) or (NetResult = ERROR_MORE_DATA) then
            break
          else begin
            sleep(300);
            Trace('NetGroupGetUsers(%s, %s... Erro <%s> Tentativa<%d>', [ServerNameW, GroupNameW, SysErrorMessage(Netresult), T]);
          end;
        end;
        case NetResult of
          ERROR_SUCCESS, ERROR_MORE_DATA: begin
            P1 := Buffer;
            for I := 0 to Read - 1 do
              with P1^ do begin
                UserName := WideCharToString(grui0_name);
                FullUserName := GetUserFullName(UserName, '', true);
                AddToStr(Result, FullUserName);
                inc(P1);
              end;
          end;
        else
          Trace('NetGroupGetUsers(%s, %s... Erro <%s>', [ServerNameW, GroupNameW, SysErrorMessage(Netresult)]);
          Result := '';
        end;
      end
      else begin
        for T := 1 to 3 do begin
          NetResult:= NetLocalGroupGetMembers(PWideChar(ServerNameW), PWideChar(GroupNameW), 3, Buffer, MAX_PREFERRED_LENGTH, Read, Total, ResumeHandle);
          if (NetResult = ERROR_SUCCESS) or (NetResult = ERROR_MORE_DATA) then
            break
          else begin
            sleep(300);
            Trace('NetLocalGroupGetMembers(%s, %s... Erro <%s> Tentativa<%d>', [ServerNameW, GroupNameW, SysErrorMessage(Netresult), T]);
          end;
        end;
        case NetResult of
          ERROR_SUCCESS : begin
            P := Buffer;
            for I := 0 to Read - 1 do
              with P^ do begin
                UserName := WideCharToString(lgrmi3_domainandname);
                FullUserName := GetUserFullName(UserName, '', true);
                AddToStr(Result, FullUserName);
                inc(P);
              end;
          end;
        else
          Trace('NetLocalGroupGetMembers erro: ' + syserrormessage(Netresult), []);
          Result := ''
        end;
      end;
    except
      on E : Exception do Result := '';
    end;
  finally
    NetApiBufferFree(Buffer);
  end;}
end;

function GetUserInfo(TipoUserInfo : TTipoUserinfo; Usr : String = ''; Srv: String = '') : Variant; begin
  Result := '';
  case TipoUserInfo of
    tuUsername:            Result := GetUserName;
    tuLocalComputer:       Result := GetComputerName;
    tuUserChv:             Result := GetChv(Usr, Srv);
    tuUserId:              Result := GetUserId(Usr, Srv);
    tuLogonSrv:            Result := GetLogonSrv(Usr, Srv, false);
    tuUserDomain:          Result := GetUserDomain(Usr, Srv);
    tuUserFullName:        Result := GetUserFullName(Usr, Srv);
    tuUserFullNameAndUser: Result := GetUserFullName(Usr, Srv, true);
    tuUserGroups:          Result := GetUserGroups(Usr, Srv);
  end;
end;

function GetLoginKey(Usr : String = ''; Srv: String = '') : String;
var
  Str, Hst : String;
begin
  Str:='';
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
end;

//fim fun??es de seguran?a

var
  IniFile : TIniFile;

function GetIniParameter(Section, Param : string; Default : string = '') : string; begin
  Result := IniFile.ReadString(Section, Param, Default);
end;

function S(Value : integer; Precision : integer = 0) : string; begin
  Result := Format('%.*d', [Precision, Value])
end;

function S(Value : Int64; Precision : integer = 0) : string; begin
  Result := Format('%.*d', [Precision, Value])
end;

function GetDateTimeType(const TypeName: string): TDateTimeType;
const
  Results: array[-1..5] of TDateTimeType = (dtNone, dtDateTime, dtDateTime, dtDate, dtDate, dtTime, dtTime);
begin
  Result := Results[CaseOf(LowerCase(TypeName), ['datetime', 'tdatetime','date', 'tdate', 'time', 'ttime'])];
end;

function AdjustEnumerationOrSet(E : string) : string;
var
  I : integer;
begin
  Result := '';
  if pos('__', E) <> 0 then exit;
  case CaseOf(E, ['False', 'True']) of
    0 : begin
      Result := 'N?o';
      exit;
    end;
    1 : begin
      Result := 'Sim';
      exit;
    end;
  end;
  for I := 1 to length(E) do
    case E[I] of
      'a'..'z' : if Result <> '' then Result := Result + E[I];
      'A'..'Z', '0'..'9' :
        if (Result <> '') and (E[I-1] in ['a'..'z']) then
          Result := Result + ' ' + E[I]
        else
          Result := Result + E[I];
      '_': if Result <> '' then Result:= Result + ' ';
    else
      Result := Result + E[I];
    end;
end;

function CertifyEnum(TypeInfo : PTypeInfo; Value, Default : integer) : integer;
var
  TypeData : PTypeData;
begin
  TypeData := GetTypeData(TypeInfo);
  if (Value >= TypeData.MinValue) and (Value <= TypeData.MaxValue) then
    if pos('__', GetEnumName(TypeInfo, Value)) = 0 then
      Result := Value
    else
      Result := Default
  else
    Result := Default
end;

function EnumToStr(const TypeInfo : PTypeInfo; const Value : integer) : string; begin
  Result := AdjustEnumerationOrSet(GetEnumName(TypeInfo, Value))
end;

function ValidarModulo10(Numero: int64; Size: Byte): Boolean;
var
  i,
  Total,
  tmp   : Integer;
  sAll,
  s     : ShortString;
begin
  sAll := Format('%.' + IntToStr(Size) +'d', [Numero]);
  s    := Copy(sAll, 1, Length(sAll) - 1);
  Total:= 0;
  for i := 1 to Length(s) do
    if i mod 2 <> 0 then begin
      tmp :=  2 * (Ord(s[Length(s) + 1 - i]) - 48);
      if tmp > 9 then Dec(tmp, 9);
      Inc(Total, tmp);
    end
    else
      Inc(Total, Ord(s[Length(s) + 1 - i]) - 48);
  Total  := 10 - (Total mod 10);
  Result := (IntToStr(Total mod 10) = sAll[Size + 1]);
end;

function CalcularDvModulo11(Numero: Int64): Byte;
var
  i, j,
  Tamanho  : Integer;
  sNumero  : ShortString;
begin
  Result   := 0;
  j        := 2;
  sNumero  := IntToStr(Numero);
  Tamanho  := Length(sNumero);
  for i := Tamanho downto 1 do begin
    Result:=  Result + (Ord(sNumero[i]) - 48) * j;
    if j = 9 then
      j:= 2
    else
      Inc(j);
  end;
  if ((Result mod 11) = 0) or ((Result mod 11) = 1) then
    Result:= 0
  else
    Result:= 11 - (Result mod 11);
end;

function ValidarModulo11(Numero: int64): Boolean;
var
  i, j,
  Size,
  Total : Integer;
  sAll,
  s     : ShortString;
begin
  sAll  := IntToStr(Numero);
  Size  := Length(sAll);
  s     := Copy(sAll, 1, Size - 1);
  Total := 0;
  j     := 2;
  dec(Size);
  for i := Size downto 1 do begin
    Total:=  Total + (Ord(s[i]) - 48) * j;
    if j = 9 then
      j := 2
    else
      inc(j);
  end;
  if ((Total mod 11) = 0) or ((Total mod 11) = 1) then
    Total:= 0
  else
    Total:= 11 - (Total mod 11);
  Result := IntToStr(Total) = sAll[Size + 1];
end;

function CalcularDvModulo10(Numero: Int64): Byte;
var
  i, tmp : Integer;
  s  : ShortString;
begin
  s := IntToStr(Numero);
  Result := 0;
  for i := 1 to Length(s) do
    if i mod 2 <> 0 then begin
      tmp :=  2 * (Ord(s[Length(s) + 1 - i]) - 48);
      if tmp > 9 then Dec(tmp, 9);
      Inc(Result, tmp);
    end
    else
      Inc(Result, Ord(s[Length(s) + 1 - i]) - 48);
   Result  := 10 - (Result mod 10);
end;

procedure FixedLenOverflow; begin
  raise Exception.Create('Tamanho da vari?vel informada maior que o permitido para FixedLen');
end;

function FixedLen(const Value: integer; const Digits: Integer): string; overload;
var
  L: integer;
begin
  Result := IntToStr(Value);
  L := Length(Result);
  if L < Digits then
    Result := StringOfChar('0', Digits - L) + Result
  else
    if L > Digits then FixedLenOverflow;
end;

function FixedLen(const Value: Int64; const Digits: Integer): string; overload;
var
  L: integer;
begin
  Result := IntToStr(Value);
  L := Length(Result);
  if L < Digits then
    Result := StringOfChar('0', Digits - L) + Result
  else
    if L > Digits then FixedLenOverflow;
end;

var
  ExtendedDigitsFormatSettings: TFormatSettings;

function FixedLen(const Value: Extended; const IntDigits, FracDigits: Integer): string; overload;
var
  L, TotalDigits: integer;
begin
  Result := FloatToStrF(Value, ffFixed, IntDigits, FracDigits, ExtendedDigitsFormatSettings);
  L := Length(Result);
  TotalDigits := IntDigits + FracDigits;
  if L < TotalDigits then
    if Result[1] <> '-' then
      Result := StringOfChar('0', TotalDigits - L) + Result
    else
      System.Insert(StringOfChar('0', TotalDigits - L), Result, 2)
  else
    if L > TotalDigits then FixedLenOverflow;
end;

function FixedLen(const Value: string; const Digits: integer): string; overload;
var
  L: integer;
begin
  L := Length(Value);
  if L = Digits then
    Result := Value
  else
    if L < Digits then
      Result := Value + StringOfChar(' ', Digits - L)
  else
    FixedLenOverflow;
end;

function FixedLenDate(const Value: TDateTime): string; begin
  Result := FormatDateTime('yyyymmdd', Value);
end;

function FixedLenDateTime(const Value: TDateTime): string; begin
  Result := FormatDateTime('yyyymmddhhnnss', Value);
end;

function FixedLenTime(const Value: TDateTime): string; begin
  Result := FormatDateTime('hhnnss', Value);
end;

function epIniFile: TIniFile; begin
  Result := IniFile;
end;

function CallBatch(Batch : String) : TStringList;
const
  READ_BYTES = 2048;
var
  I, BytesRead : cardinal;
  Temp : TMemoryStream;
begin
  with TProcess.Create(nil) do begin
    CommandLine := Batch;
    Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    //StartupOptions := [suoUseShowWindow]; ShowWindow := swoHIDE;
    BytesRead := 0;
    Temp := TMemoryStream.Create;
    Execute;
    while Running do begin
      Temp.SetSize(BytesRead + READ_BYTES);
      I := Output.Read(pointer(PtrUInt(Temp.Memory) + BytesRead)^, READ_BYTES);
      if I > 0 then
        inc(BytesRead, I)
      else
        sleep(10);
    end;
    repeat
      Temp.SetSize(BytesRead + READ_BYTES);
      I := Output.Read(pointer(PtrUInt(Temp.Memory) + BytesRead)^, READ_BYTES);
      if I > 0 then inc(BytesRead, I);
    until I <= 0;
    Temp.SetSize(BytesRead);
    Result := TStringList.Create;
    Result.LoadFromStream(Temp);
    Temp.Free;
    free;
  end;
end;

function ExpandMacros(Macros : array of string; S : string) : string;
var
  I, J : integer;
begin
  I := 0;
  Result := S;
  while I < high(Macros) do begin
    repeat
      J := pos(Macros[I], Result);
      if J <> 0 then begin
        delete(Result, J, length(Macros[I]));
        insert(Macros[I+1], Result, J);
      end;
    until J = 0;
    inc(I, 2);
  end;
end;

function GetFCGIThreadID : integer; begin
   Result := abs(integer(CurrentFCGIThread))
end;

initialization
  ExtendedDigitsFormatSettings.DecimalSeparator := #0;
  ChDir(ExtractFilePath(ParamStr(0)));
  IniFile := TIniFile.Create(ServerName + '.ini');
  ServerName := GetIniParameter('Server', 'Title', ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
finalization
  IniFile.Free;
end.
