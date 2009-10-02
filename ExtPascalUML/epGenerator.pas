{$I epDirectives.inc}

unit epGenerator;

interface

const
  GeneratorVersion = '1.0';

procedure GerarObjectServer(Obj : TObject);
function SourceToModel(Path, SourcePos : string) : string;
function ModelToSource(Path, UMLLink : string) : string;

var
  Modelo : string;
  FPAInfluence : double = 1.14;

implementation

uses
  {$IFDEF MSWINDOWS}Windows, WinSvc,{$ENDIF}
  SysUtils, StrUtils, Classes, TypInfo, Math,
  epUtils, epServer, epDesignerServer, epPrevalence, epObjectList, ExtPascalUMLModel, epThread;

type
  TGerador = class
  private
    fModelo : TModelo;
    fClasseList : TLinkPorHerancaNomeList;
    fMetodoList : TMetodoList;
    fIndiceList : TIndiceList;
    TypeCasts,
    MapClass    : TStringList;
    fServidor : TServidor;
    fGeracao : TGeracao;
    fPath, Pas : String;
    Error, Prim : boolean;
    fLink, NomeFonte, Fonte, EscopoAtual, Enumeracoes, UnitName : string;
    PosImplementation, PosMethods : integer;
    T : TDateTime;
    OpcoesAnt : TSetGeracaoOpcoes;
    Compiled : boolean;
    procedure Lin(S : string = '');
    function  nl(I : integer = 1) : string;
    function  Tab(I : integer = 1) : string;
    procedure LinMembro(S : string);
    procedure GerarFonte(Nome : string);
    procedure ClosePas;
    procedure GerarModelo;
    procedure GerarPacote(Pacote : TPacote);
    procedure GerarCabecalhoDeMetodo(pClasse : TClasse; pDeclaracao : boolean; pEscopo : TAtributoEscopo = aePublished);
    function GerarMaquinaEstados(pMetodo : TMetodo) : boolean;
    procedure GerarCreate(pClasse: TClasse);
    procedure GerarMetadata(pClasse : TClasse);
    function GetError(LineNumber, ColNumber : integer; Fundo : string) : string;
    function ShowCompileError(Line : string; TipoErro : integer) : string;
    function Compile(Source: string) : boolean;
    procedure ModelError(Condicao : boolean; S : string);
    function CreateViewClasse(Pacote : TPacote; ComLinks : boolean) : TLinkPorHerancaNomeList;
    function GerarDescricaoMesmaLinha(D : string) : string;
    function GerarMultiLinhas(Tab, Descricao, Comentario : string) : string;
    function GerarDescricaoProximaLinha(Tab, D : string) : string;
    procedure ProgramarJoin(var Join : TEstado);
    procedure GerarCabecalho(Pacote: TPacote);
    procedure GerarMetricas;
    function GerarParametrosDeMetodo(pMetodo: TMetodo; Prototipo: boolean = true; Adicional : string = '') : string;
    procedure GerarInherited(pMetodo: TMetodo; var S: string);
    constructor Create;
    destructor Destroy; override;
    function AddTypeCasts(S : string) : string;
    procedure GerarDescricaoParametros(pMetodo: TMetodo);
    function CallCompiler(Compilation: String; CheckSyntaxOnly : boolean = false): boolean;
    procedure CarregarFonte(F: string);
    function LinkError(S: string): string;
    function PosToLine(I: integer): integer;
    function GetSourcePos(Path, UMLLink : string): string;
  public
    procedure GerarProjeto;
    procedure GerarUnits;
  end;

procedure GerarObjectServer(Obj : TObject);
var G : Tgerador;
begin
  G := TGerador.Create;
  with g do
    try
      T := Now;
      fServidor := ServidorList.First;
      if fServidor.Versao.Path = '' then // No dat inicial deve ter uma versão lá
        fServidor.Versao.Path := GetIniParameter('DIRECTORY', 'pitinnu');
      fGeracao := TGeracao(Obj);
      if fServidor.RaizGeracao = '' then
        fServidor.RaizGeracao := GetIniParameter('DIRECTORY', 'work');
      fPath := fServidor.RaizGeracao;
      if fPath[length(fPath)] <> PathDelim then fPath := fPath + PathDelim;
      //if fGeracao.Ambiente = gaDesenvolvimento then
        fPath := fPath + Browser.UserInfo.UserName + PathDelim;
      fPath := fPath + fGeracao.Modelo.Nome;
      if not DirectoryExists(fPath) and not ForceDirectories(fPath) then
        raise Exception.Create('Não foi possível criar o diretório ' + fPath + ' no servidor. Geração abortada.');
      EndTransaction(true);
      BeginTransaction;
      GerarProjeto;
      GerarUnits;
      Browser.Message(StringOfChar('-', 150));
      Browser.Message('Tempo de Geração: ' + FormatDateTime('ss' + DecimalSeparator + 'zzz', Now - T) + ' segundos');
      if not Error then Compile(Modelo)
    finally
      Rollback;
      Free;
    end;
end;

procedure TGerador.ModelError(Condicao : boolean; S : string); begin
  if Condicao then begin
    Browser.Message(#1'fontcolor FF0000'#1#1'fontbold'#1'Erro: ' + S);
    Error := true;
  end;
end;

function SplitLine(S : string) : string; begin
  Result := StringReplace(S, #13#10, '''#13#10''', [rfReplaceAll, rfIgnoreCase]);
  Result := WrapText(Result, ''' + '#13#10#9#9#9#9'''', [' ', #9, '-', '.', '|'], 90)
end;

function TipoEhPrimitivo(pElementoTipado : TElementoTipado) : boolean; begin
  Result := not (pElementoTipado.Tipo in [ettClasse, ettLista]);
end;

function MapeiaMetodoTipo(pMetodoTipo : TMetodoTipo) : string; begin
  case pMetodoTipo of
    mtWorkflow          : Result := '_stWORKFLOW';
    mtWizard            : Result := '_stWIZARD';
    mtVisaoDeSeguranca  : Result := '_stVIEW';
    mtMaquinaDeEstados  : Result := '_stSTATEMACHINE';
  else
    Result := 'Inválido';
  end;
end;

function GetTipo(pClasse : TClasse; pElementoTipado : TElementoTipado; pIndice : boolean = false) : string; begin
  Result := '';
  if pElementoTipado = nil then begin
    Browser.Message('Impossível recuperar ElementoTipado da classe ' + pClasse.Nome);
    exit;
  end;
  with pElementoTipado do
    if not pIndice then
      case Tipo of
        ettLogico: Result := 'Boolean';
        ettTexto: begin
          if Tamanho = 1 then
            Result := 'Char'
          else
            Result := 'String';
        end;
        ettNumero : begin
          if not Decimais then begin
            if Tamanho <  3 then if Sinalizado then Result := 'ShortInt' else Result := 'Byte' else
            if Tamanho <  5 then if Sinalizado then Result := 'SmallInt' else Result := 'Word' else
            if Tamanho < 10 then if Sinalizado then Result := 'Integer'  else Result := 'Cardinal' else
            if Tamanho < 20 then Result := 'Int64';
          end
          else begin
            if Tamanho <  8 then Result := 'Single' else
            if Tamanho < 16 then Result := 'Double' else
            if Tamanho < 20 then Result := 'Extended';
          end;
        end;
        ettMonetario : Result := 'Currency';
        ettHora: Result := 'Time';
        ettData: Result := 'Date';
        ettDataHora: Result := 'DateTime';
        ettFaixa: Result := 'T' + pClasse.Nome + Nome;
        ettClasse: Result := 'T' + TipoClasse.Nome;
        ettLista: Result := 'T' + Valores + 'List'; 
        ettConjunto: Result := 'T' + 'Set' + pClasse.Nome + Nome;
        ettEnumeracao : Result := 'T' + pClasse.Nome + Nome;
      else //ettOutros
        Result := Valores;
      end
    else
      case Tipo of
        ettLogico: begin
          Result := 'Word';
          Browser.Message('AVISO: Para usar tipo Lógico como índice na Classe ' + pClasse.Nome + ' algum typecast deve ser usado na expressão do índice.');
        end;
        ettTexto: Result := 'String';
        ettNumero : begin
          if not Decimais then begin
            if Tamanho <  5 then if Sinalizado then Result := 'Integer' else Result := 'Word' else
            if Tamanho < 10 then Result := 'Integer' else
            if Tamanho < 20 then Result := 'Int64';
          end
          else
            Result := 'Double'
        end;
        ettMonetario, ettHora, ettData, ettDataHora: Result := 'Double';
        ettFaixa: Result := 'Integer';
        ettClasse: Browser.Message('Tipo Classe não pode ser usado como índice na Classe ' + pClasse.Nome);
        ettLista: Browser.Message('Tipo Lista não pode ser usado como índice na Classe ' + pClasse.Nome);
        ettConjunto: Browser.Message('Tipo Conjunto não pode ser usado como índice na Classe ' + pClasse.Nome);
        ettEnumeracao : begin
          Result := 'Word';
          Browser.Message('AVISO: Para usar enumeração como índice na Classe ' + pClasse.Nome + ' algum typecast deve ser usado na expressão do índice.');
        end
      else //ettOutros
        Browser.Message('Tipo Outros não pode ser usado como índice na Classe ' + pClasse.Nome);
      end
end;

function GetResultDefault(pClasse : TClasse; pElementoTipado : TElementoTipado) : string;
var
  Tipo : string;
begin
  Tipo := GetTipo(pClasse, pElementoTipado);
  if pos(',' + LowerCase(Tipo) + ',' ,',string,char,') <> 0 then
    Result := ''' '''
  else
  if pos(',' + LowerCase(Tipo) + ',' ,',integer,int64,double,word,byte,longint,smallint,shortint,longword,datetime,time,date,cardinal,currency,comp,single,extended,real,') <> 0 then
    Result := '0'
  else
  if pos(LowerCase(Tipo), 'set') <> 0 then
    Result := '[]'
  else
  if pos(',', Tipo) <> 0 then
    Result:=copy(Tipo , 1, pos(',', Tipo) - 1)
  else
    Result := Tipo + '(0)';
end;

function GerarConstraintsAtributos(pAtributo : TAtributo) : string; begin
  Result := '';
  with pAtributo do begin
    if Validacao <> '' then Result := 'CHECK,';
    if (Caracteristicas * [acDerivado, acSomenteLeitura]) <> [] then Result := Result + 'READONLY,';
    if Habilitacao <> '' then Result := Result + 'ENABLED,';
    if Visibilidade <> '' then Result := Result + 'VISIBLE,';
    if acNaoNulo in Caracteristicas then Result := Result + 'NOTNULL,';
    if Result <> '' then Result := copy(Result, 1, length(Result) - 1);
  end;
end;

function LastPos(SubStr, S: string; Pos : integer = 0): Integer;
var
  Len : integer;
begin
  Len := length(SubStr);
  if Pos = 0 then Pos := length(S);
  dec(Pos, Len+1);
  for Pos := Pos downto 1 do
    if copy(S, Pos, Len) = SubStr then begin
      Result := Pos;
      exit;
    end;
  Result := 0;
end;

function TGerador.LinkError(S : string) : string;
var
  I : integer;
begin
  fLink := S;
  I := pos('_', S);
  if I <> 0 then S[I] := '.';
  Result := #1'link ' + S + '|' + S + #1
end;

function ExtraiNomeMetodo(S : string; Inicio, Tam : integer) : string;
var
  I : integer;
begin
  for I := Inicio to Inicio+Tam do
    if S[I] in ['(', ';', ':'] then begin
      Result := trim(copy(S, Inicio, I-Inicio));
      if Result[length(Result)] = '_' then
        Result := copy(Result, 1, length(Result)-1);
      exit;
    end;
  Result := S;
end;

function STrim(S : string) : string; begin
  while length(S) > 0 do
    if S[1] in [' ', #9] then
      delete(S, 1, 1)
    else
      break;
  Result := TrimRight(S);
end;

procedure MarkError(var Linha : string; ColNumber : integer; Fundo : string);
var
  I, P : integer;
begin
  if ((ColNumber <> 0) and (trim(Linha) <> '')) and (ColNumber <= length(Linha)) then begin
    P := 1;
    if IsValidIdent(Linha[ColNumber]) then
      for I := ColNumber to length(Linha) do
        if IsValidIdent(Linha[I]) then
          inc(P)
        else begin
          dec(P);
          break;
        end
    else begin
      for ColNumber := ColNumber-1 downto 1 do
        if IsValidIdent(Linha[ColNumber]) then break;
      for ColNumber := ColNumber-1 downto 1 do begin
        if not IsValidIdent(Linha[ColNumber]) then break;
        inc(P);
      end;
      if ColNumber <> 1 then inc(ColNumber);
    end;
    insert(#1'fontback FFFFFF'#1#1'fontcolor 000000'#1, Linha, ColNumber+P);
    insert(#1'fontcolor FFFFFF'#1#1'fontback ' + Fundo + #1, Linha, ColNumber);
  end;
end;

function TGerador.GetError(LineNumber, ColNumber : integer; Fundo :string) : string;
var
  I, J, P, F, K : integer;
  Linha, NomeMetodo : string;
begin
  J := 0;
  for I := 1 to LineNumber-1 do
    J := posex(#13, Fonte, J+1);
  Linha := copy(Fonte, J+2, posex(#13, Fonte, J+1)-J-2);
  if ColNumber > length(Linha) then begin
    for I := 1 to 2 do
      J := posex(#13, Fonte, J+1);
    Linha := trim(copy(Fonte, J+2, posex(#13, Fonte, J+1)-J-2));
  end;
  MarkError(Linha, ColNumber, Fundo);
  Linha := STrim(Linha);
  if (Linha <> '') and (Linha[1] = '_') then delete(Linha, 1, 1);
  J := posex(#13, Fonte, J+1);
  if J > PosMethods then begin
    P := LastPos('procedure ', Fonte, J);
    F := LastPos('function ', Fonte, J);
    if P > F then
      I := P + 10
    else
      I := F + 9;
    Result := ExtraiNomeMetodo(Fonte, I, 200);
    K := pos('_', Result);
    if K <> 0 then begin
      Result[K] := '.';
      Result := '. ' + LinkError(Result)
    end
    else begin
      K := LastPos('// Cyclom', Fonte, J);
      if K > PosMethods then begin
        P := posex(#13, Fonte, K);
        P := posex(' ', Fonte, P);
        F := posex('_', Fonte, P);
        K := posex('.', Fonte, P);
        F := min(K, F);
      end
      else begin
        P := I;
        F := posex('.', Fonte, P);
        Result := '';
      end;
      J := pos(' do ', Linha);
      if J <> 0 then Linha := copy(Linha, J+4, 200);
      Result := '. ' + LinkError(copy(Fonte, P + 1, F - P - 1) +
                '.' + trim(copy(Fonte, F + 1, min(min(posex(';', Fonte, F+1), posex(':', Fonte, F+1)), posex('(', Fonte, F+1))-F-1)) + '.' + Result)
    end
  end
  else
  if J > PosImplementation then begin
    Result := ' -> ' + Linha;
    exit;
  end
  else begin
    I := LastPos(' = class(', Fonte, J);
    P := LastPos(' T', Fonte, I);
    NomeMetodo := '';
    K := pos('procedure ', Linha);
    if K <> 0 then
      NomeMetodo := '.' + ExtraiNomeMetodo(Linha, K+10, length(Linha))
    else begin
      K := pos('function ', Linha);
      if K <> 0 then
        NomeMetodo := '.' + ExtraiNomeMetodo(Linha, K+9, length(Linha))
      else begin // Atributo
        K := pos(' ', Linha);
        NomeMetodo := '.' + copy(Linha, 1, K-1)
      end;
    end;
    Result := '. ' + LinkError(copy(Fonte, P+2, I-P-2) + NomeMetodo);
  end;
  if Linha <> 'end;' then Result := Result + ' -> ' + Linha;
end;

var
  OldNum : integer = 0;

procedure TGerador.CarregarFonte(F : string);
var
  Arq : file;
begin
  if LastDelimiter('/\:', F) = 0 then F := fPath + PathDelim + F;
  if (F <> NomeFonte) or (Fonte = '') then begin
    NomeFonte := F;
    assign(Arq, NomeFonte);
    reset(Arq, 1);
    SetLength(Fonte, FileSize(Arq));
    blockread(Arq, Fonte[1], length(Fonte));
    close(Arq);
    PosImplementation := pos('implementation'#13#10, Fonte);
    PosMethods := pos('// Methods', Fonte);
  end;
end;

function TGerador.PosToLine(I : integer) : integer;
var
  J : integer;
begin
  J := 0;
  Result := 0;
  repeat
    J := posex(#13, Fonte, J+1);
    inc(Result);
  until J >= I;
end;

function TGerador.GetSourcePos(Path, UMLLink : string) : string;
var
  Pacote, Classe, Metodo, Estado, S : string;
  I : integer;
begin
  try
    I := 1;
    Pacote := ExtractFromStr(UMLLink, I, '.') + '.pas';
    Classe := ExtractFromStr(UMLLink, I, '.');
    Metodo := ExtractFromStr(UMLLink, I, '.');
    Estado := ExtractFromStr(UMLLink, I, '.');
    CarregarFonte(Path + Pacote);
    S := 'procedure ' + Classe + '_' + Metodo;
    I := pos(S, Fonte);
    I := posex(S, Fonte, I + length(S));
    I := posex(' ' + Estado + '_(var ', Fonte, I + length(S));
    Result := Pacote + ':' + IntToStr(PosToLine(I)+2);
  except
    Result := ''
  end;
end;

function TGerador.ShowCompileError(Line : string; TipoErro : integer) : string;
var
  I, J, K, NumLinha, NumColuna : integer;
  Erro, Cor, Fundo : string;
begin
  NumLinha := 0;
  case TipoErro of
    0,4,8 : begin
      Cor := #1'fontcolor FF0000'#1#1'fontbold'#1; // Erro, vermelho
      Fundo := 'FF0000'
    end;
    1,5 : begin
      Cor := #1'fontcolor FF6400'#1; // Aviso, laranja
      Fundo := 'FF6400'
    end;
    2,6 : begin
      Cor := #1'fontcolor 0000FF'#1; // Nota, azul
      Fundo := '0000FF'
    end;
  else
    Cor := #1'fontcolor 008200'#1; // Sugestão, verde
    Fundo := '008200'
  end;
  Result := Cor + Line + #1'fontcolor'#1;
  I := pos('(', Line);
  if I <> 0 then begin
    try
      CarregarFonte(copy(Line, 1, I-1))
    except
      Fonte := '';
      exit;
    end;
    K := pos(')', Line);
    J := pos(',', Line);
    if J = 0 then begin
      NumColuna := 0;
      J := K;
    end
    else begin
      NumColuna := StrToIntDef(copy(Line, J+1, K-J-1), 0);
      J := min(K, J);
    end;
    Result := Cor + copy(Line, K+2, 500) + #1'fontcolor'#1;
    NumLinha := StrToIntDef(copy(Line, I+1, J-I-1), 0);
    if (TipoErro = 8) and (pos('";"', Line) = 0) and (NumColuna = 0) then begin
      Result := '';
      exit;
    end;
    if (NumLinha <> OldNum) or (TipoErro in [0, 4]) then begin
      if NumColuna = 0 then begin
        if pos('Statement expected', Line) <> 0 then dec(NumLinha, 2); // Erro begin/end Delphi
      end
      else
        if (TipoErro = 8) and (pos('";"', Line) <> 0) then dec(NumLinha, 2); // Erro begin/end FPC
      Erro := GetError(NumLinha, NumColuna, Fundo);
      if Erro = '' then
        Result := ''
      else
        Result := Result + Erro;
    end
    else
      Result := ''
  end;
  OldNum := NumLinha;
end;

function InStr(A : array of string; S : string) : integer;
var
  I : integer;
begin
  Result := -1;
  for I := 0 to high(A) do
    if pos(A[I], S) <> 0 then begin
      Result := I;
      exit;
    end;
end;

function RemoveTypeCasts(S: string): string;
var
  I, J, K : integer;
begin
  Result := S;
  I := pos('T_', Result);
  while I <> 0 do begin
    J := posex('(', Result, I);
    K := posex('.', Result, I);
    if J < K then begin
      delete(Result, I, J-I+1);
      J := posex(')', Result);
      if J <> 0 then delete(Result, J, 1);
      J := I;
    end
    else begin
      delete(Result, I+1, 1);
      J := K;
    end;
    I := posex('T_', Result, J+1);
  end;
end;

function TGerador.CallCompiler(Compilation : String; CheckSyntaxOnly : boolean = false) : boolean;
var
  Error : String;
  I, TipoErro : integer;
  Lines : TStringList;
begin
  Result := true;
  OldNum := 0;
  Lines := CallBatch(Compilation + IfThen(CheckSyntaxOnly, '-s -Cn', '')); // -s skip Assembler, -Cn skip Linker);
  for I := 0 to Lines.Count-1 do begin
    TipoErro := InStr(['Error: ', 'Warning: ', 'Note: ', 'Hint: ', 'Erro: ', 'Aviso: ', 'Nota: ', 'Sugestão: ', 'atal: '], Lines[I]);
    if TipoErro <> -1 then begin
      Result := false;
      Error := ShowCompileError(Lines[I], TipoErro);
      if Error <> '' then Browser.Message(RemoveTypeCasts(Error));
    end
    else
      Browser.Message(Lines[I]);
  end;
  Lines.Free;
end;

function TGerador.Compile(Source : string) : boolean;
var
  Compilacao, FPCPath, Compilador : string;
  I : integer;
begin
  {$IFDEF MSWINDOWS}
  CopyFile(pchar(fServidor.Versao.Path + '\pitinnu.res'), pchar(fPath + '\' + Source + '.res'), true);
  {$ENDIF}
  Source := Source + '.dpr';
  FPCPath := copy(fServidor.Versao.Path, 1, length(fServidor.Versao.Path)-3) + 'FreePascal';
  Compilacao := ExpandMacros(['$(PATH)', fPath, '$(pitPATH)', fServidor.Versao.Path, '$(FPCPATH)', FPCPath],
    fGeracao.Compilador.Executavel + ' "$(PATH)"' + PathDelim + Source + ' ' + fGeracao.Compilador.Opcoes);
//  dcc32 -M -Q -H -W -$I+ -$J+ -$A1 -$Y- -$C- -$D- -$L- -$G- -U"$(pitPATH);$(PATH)" -E"$(PATH)"
//  $(FPCPATH)\ppc386.exe -S2dcghimte20 -venw -n -l -O2pPENTIUM4 -CX -XiXs -CfSSE -CpPENTIUM4 -CPPACKSET=1 -Fu$(pitPATH) -Fu$(FPCPATH)
// -Fr$(FPCPATH)\msg\errorptw.msg
  if goDebugger in fGeracao.Opcoes then Compilacao := Compilacao + ' -O- -ghlt -bl -Ciort -dDEBUG';
  if Compiled and (OpcoesAnt <> fGeracao.Opcoes) then Compilacao := Compilacao + ' -B';
  OpcoesAnt := fGeracao.Opcoes;
  Compiled := true;
  Browser.Message(StringOfChar('-', 150));
  Browser.Message('Parando Serviço...');
  I := pos(' "', Compilacao);
  if I = 0 then I := Maxint;
  Compilador := copy(Compilacao, 1, I-1) + '.exe';
  if FileExists(Compilador) or (LowerCase(Compilador) = 'dcc32') then begin
    DesassociaServicoDesigner(Modelo + Browser.UserInfo.UserName);
    Browser.Message('Compilando: ' + Source);
    Result := CallCompiler(Compilacao);
  end
  else begin
    Browser.Message('Compilador não pôde ser encontrado no caminho ' + copy(Compilacao, 1, I-1));
    Result := false;
  end;
end;

function ClassePai(pLink : TLink; Metodo : string = '') : TClasse;
var
  fClasse : TClasse;
begin
  if pLink is TClasse then
    fClasse := TClasse(pLink)
  else
    fClasse := pLink.Classe;
  if fClasse.Pai = nil then
    Result := nil
  else begin
    Result := TClasse(fClasse.Pai.Destino);
    if (Metodo <> '') and (Result <> nil) then
      while Result.Metodos.Find(Metodo) = nil do begin
        Result := TClasse(Result.Pai.Destino);
        if Result = nil then exit
      end;
  end;
end;

function TGerador.CreateViewClasse(Pacote : TPacote; ComLinks : boolean) : TLinkPorHerancaNomeList;
var
  fLink : TLink;
  LinkList : TLinkPorHerancaNomeList;

  procedure AddPacote; begin
    fLink := Pacote.Classes.First;
    while Assigned(fLink) do begin
      if not ComLinks then begin
        if fLink is TClasse then
          LinkList.Add(fLink);
      end
      else
        LinkList.Add(fLink);
      Pacote.Classes.Next(fLink)
    end;
  end;

begin
  LinkList := TLinkPorHerancaNomeList(LinkPorHerancaNomeList.CreateView);
  if Pacote = nil then begin
    Pacote := fModelo.Pacotes.First;
    while Assigned(Pacote) do begin
      AddPacote;
      fModelo.Pacotes.Next(Pacote);
    end;
  end
  else
    AddPacote;
  Result := LinkList;
end;

function GetOriDes(pObj : TObject) : TClasse; begin
  if pObj = nil then
    Result := nil
  else
    if pObj is TClasse then
      Result := TClasse(pObj)
    else
      Result := TLink(pObj).Classe;
end;

function CreateViewAssociacao(pClasse : TClasse) : TAssociacaoList;
var
  fLink : TLink;
  fClasse : TClasse;
  fAssociacao : TAssociacao;

  procedure AddView(pAssoc : TAssociacao); begin
    if Result.Find(pAssoc.ID) = nil then
      Result.Add(pAssoc);
  end;

  procedure CarregaLinks;
  var
    fAssoc : TAssociacao;
  begin
    fLink := fClasse.Links.First;
    while Assigned(fLink) do begin
      fAssoc := fLink.AssociacoesDestino.First;
      while Assigned(fAssoc) do begin
        AddView(fAssoc);
        fLink.AssociacoesDestino.Next(fAssoc);
      end;
      fAssoc := fLink.AssociacoesOrigem.First;
      while Assigned(fAssoc) do begin
        AddView(fAssoc);
        fLink.AssociacoesOrigem.Next(fAssoc);
      end;
      fClasse.Links.Next(fLink);
    end;
  end;

begin
  Result := TAssociacaoList(AssociacaoList.CreateView);
  fClasse := pClasse;
  while Assigned(fClasse) do begin
    fAssociacao := fClasse.AssociacoesDestino.First;
    while Assigned(fAssociacao) do with fAssociacao do begin
      //para retirar duplicidade no autorelacionamento
      if GetOriDes(Origem) <> GetOriDes(Destino) then AddView(fAssociacao);
      fClasse.AssociacoesDestino.Next(fAssociacao);
    end;
    CarregaLinks;
    fClasse := ClassePai(fClasse);
  end;
  fClasse := pClasse;
  while Assigned(fClasse) do begin
    fAssociacao := fClasse.AssociacoesOrigem.First;
    while Assigned(fAssociacao) do begin
      AddView(fAssociacao);
      fClasse.AssociacoesOrigem.Next(fAssociacao);
    end;
    fClasse := ClassePai(fClasse);
  end;
end;

function GetMinPapel(pPapel : TPapel) : integer; begin
  with pPapel do
    if (Multiplicidade = pm0_N) or (Multiplicidade = pm0_1) then Result := 0 else
    if (Multiplicidade = pm1)   or (Multiplicidade = pm1_N) then Result := 1 else
    Result := Minimo;
end;

function GetMaxPapel(pPapel : TPapel) : integer; begin
  with pPapel do
    if (Multiplicidade = pm0_1) or (Multiplicidade = pm1)   then Result := 1 else
    if (Multiplicidade = pm0_N) or (Multiplicidade = pm1_N) then Result := High(Word) else
    Result := Maximo;
end;


function FunctionPoints(const Classe : TClasse) : double;
var
  Simples, Media, Dificil : integer;
begin
  if Classe.Tipo in [ctPrevalente..ctTransiente] then begin
    Simples := 7;
    Media   := 10;
    Dificil := 15;
  end
  else begin // Arquivo externo
    Simples := 5;
    Media   := 7;
    Dificil := 10;
  end;
  case Classe.AssociacoesDestino.Count of
    0..1 :
      if Classe.Atributos.Count < 51 then
        Result := Simples
      else
        Result := Media;
    2..5 :
      case Classe.Atributos.Count of
         0..19 : Result := Simples;
        20..50 : Result := Media;
      else
        Result := Dificil;
      end;
  else
    if Classe.Atributos.Count < 20 then
      Result := Media
    else
      Result := Dificil;
  end;
  Result := Result * FPAInfluence
end;

function Risk(Low, Medium, Value : double) : string; begin
if Value <= Low then
  Result := 'Baixo'
else if Value <= Medium then
  Result := 'MÉDIO'
else
  Result := '*** ALTO ***'
end;

function CountStr(const Substr, S : string) : integer;
var
  I : integer;
begin
  I := 0;
  Result := 0;
  repeat
    inc(I);
    I := PosEx(Substr, S, I);
    if I <> 0 then inc(Result)
  until I = 0;
end;

function CyclomaticComplexity(Metodo : TMetodo) : integer; overload;
var
  Blocks : string;
  Estado : TEstadoBase;
  Transicao : TTransicao;
begin
  Result := 1;
  with Metodo do
    if Acao = '' then begin// Máquina de Estados
      Estado := Estados.First;
      while Estado <> nil do begin
        inc(Result, Estado.Destinos.Count-1);
        if Estado is TEstado then
          inc(Result, CountStr(' and ', TEstado(Estado).Acao));
        Transicao := Estado.Destinos.First;
        while Transicao <> nil do begin
          inc(Result, CountStr(' and ', Transicao.Condicao));
          inc(Result, CountStr(' and ', Transicao.Acao));
          Estado.Destinos.Next(Transicao)
        end;
        Estados.Next(Estado)
      end
    end
    else with TParser.Create(Acao, true) do begin // Código Pascal
      Blocks := '';
      repeat
        case NextTokenInline(['if ', 'while ', 'repeat', 'for ', 'and', 'try', '''', 'case ', 'begin', 'end', ':']) of
            -1 : break;
          0..5 : inc(Result);
             6 : NextTokenInline(['''']);
             7 : Blocks := Blocks + 'C';
             8 : Blocks := Blocks + 'B';
             9 : SetLength(Blocks, length(Blocks)-1);
            10 : if (pos('C', Blocks) <> 0) (*and (Linha[1] <> '=')*) then inc(Result);
        end;
      until false;
      free;
    end;
end;

function CyclomaticComplexity(const Classe : TClasse; var Operations : integer; var RiskOperations : string) : integer; overload;
var
  CC : integer;
  Metodo : TMetodo;
begin
  Result := 0;
  if Classe.Metodos <> nil then begin
    Metodo := Classe.Metodos.First;
    while Metodo <> nil do begin
      CC := CyclomaticComplexity(Metodo);
      if CC > 1 then begin
        if CC > 20 then
          if RiskOperations = 'Nenhum' then
            RiskOperations := Metodo.Nome
          else
            RiskOperations := RiskOperations + ', T' + Classe.Nome + '.' + Metodo.Nome;
        inc(Result, CC);
        inc(Operations);
      end;
      Classe.Metodos.Next(Metodo);
    end;
  end;
end;

function TGerador.GerarDescricaoMesmaLinha(D : string) : string; begin
  Result := trim(D);
  if (Result <> '') and (pos(^M, Result) = 0) then
    Result := '// ' + Result
  else
    Result := '';
end;

function TGerador.GerarMultiLinhas(Tab, Descricao, Comentario : string) : string; begin
  Result := trim(Descricao);
  if (Result <> '') and (pos(^M, Result) <> 0) then
    Result := Tab + Comentario + AnsiReplaceText(Result, ^M^J, ^M^J + Tab + Comentario)
  else
    Result := ''
end;

function TGerador.GerarDescricaoProximaLinha(Tab, D : string) : string; begin
  Result := GerarDescricaoMesmaLinha(D);
  if Result = '' then
    Result := GerarMultiLinhas(Tab, D, '// ');
  if Result <> '' then
    Result := nl + Result
end;

procedure TGerador.GerarUnits;
var
  Pacote : TPacote;
begin
  try
    UnitName:= Modelo + 'Model';
    GerarFonte(fPath + PathDelim + UnitName + '.pas');
    GerarCabecalho(nil);
    GerarModelo;
    ClosePas;
    Pacote := fGeracao.Modelo.Pacotes.First;
    while Pacote <> nil do begin
      GerarFonte(fPath + PathDelim + Pacote.Nome + '.pas');
      GerarPacote(Pacote);
      ClosePas;
      fGeracao.Modelo.Pacotes.Next(Pacote);
    end;
  except
    on E : Exception do begin
      Browser.Message('Erro : ' + E.Message);
      ClosePas;
      Error := true;
    end;
  end;
end;

function TGerador.GerarParametrosDeMetodo(pMetodo : TMetodo; Prototipo : boolean = true; Adicional : string = '') : string;
var
  fParametro : TParametro;
  I : integer;
begin
  Result := IfThen(Prototipo or ((mcDeClasse in pMetodo.Caracteristicas) and (pMetodo.Tipo in [mtWizard, mtWorkflow])), '', 'Self');
  Result := Result + Adicional;
  fParametro := pMetodo.Parametros.First;
  if (fParametro <> nil) and (Result <> '') then
    Result := Result + IfThen(Prototipo, '; ', ', ');
  while Assigned(fParametro) do with fParametro do begin
    I := pos(' ', Nome);
    if I <> 0 then Nome := trim(copy(Nome, I+1, 100));
    if not Prototipo then
      Result := Result + Nome
    else begin
      case Passagem of
        ppEntradaSaida : Result := Result + 'var ';
        ppSaida : Result := Result + 'out ';
      end;
      Result := Result + Nome + ' : ' + GetTipo(pMetodo.Classe, TElementoTipado(fParametro));
      if Inicial <> '' then Result := Result + ' = ' + Inicial;
    end;
    if pMetodo.Parametros.Next(fParametro) then Result := Result + ifthen(Prototipo, '; ', ', ');
  end;
  if Result <> '' then Result := '(' + Result + ')'
end;

procedure TGerador.GerarPacote(Pacote : TPacote);

procedure GerarPrototipo(Metodo : TMetodo);
var
  Adicional : string;
begin
  with Metodo do begin
    if mcDeClasse in Caracteristicas then begin
      if not (Metodo.Tipo in [mtWorkflow, mtWizard]) then
        Adicional := 'Self : TClass' + Classe.Nome
    end
    else
      Adicional := 'Self : T' + Classe.Nome;
    if Retorno = nil then
      Lin('procedure ' + Classe.Nome + '_' + Nome + GerarParametrosDeMetodo(Metodo, true, Adicional) + ';')
    else
      Lin('function ' + Classe.Nome + '_' + Nome + GerarParametrosDeMetodo(Metodo, true, Adicional) + ' : ' + GetTipo(Classe, Retorno) + ';');
  end;
end;

procedure GerarInterface;
var
  Link : TLink;
  Metodo : TMetodo;
begin
  Link := fClasseList.First;
  while Link <> nil do with TClasse(Link) do begin
    Metodo := Metodos.First;
    while Metodo <> nil do begin
      GerarPrototipo(Metodo);
      Metodos.Next(Metodo);
    end;
    fClasseList.Next(Link);
  end;
end;

procedure GerarCodigo(Metodo : TMetodo);
var
  CodigoSemVar : boolean;
  Codigo, WithSelf : string;
begin
  Codigo := AddTypeCasts(Metodo.Acao);
  if Codigo = '' then begin
    Lin('begin end;');
    Browser.Message(#1'fontcolor FF6400'#1'Aviso: Método ' + LinkError(Metodo.Classe.Nome + '.' + Metodo.Nome) + ' sem máquina de estado');
  end
  else
  if (pos('procedure ', Codigo) <> 0) or (pos('function ', Codigo) <> 0) then begin
    Lin('begin end;');
    Browser.Message(#1'fontcolor FF6400'#1'Aviso: Método ' + LinkError(Metodo.Classe.Nome + '.' + Metodo.Nome) + ' procedures aninhadas não são permitidas');
  end
  else begin
    CodigoSemVar := (LowerCase(copy(Codigo, 1, 3)) <> 'var') and (LowerCase(copy(Codigo, 1, 4)) <> 'type') and (LowerCase(copy(Codigo, 1, 5)) <> 'const');
    if mcDeClasse in Metodo.Caracteristicas then
      WithSelf := 'T_Class' + Metodo.Classe.Nome + '(Self)'
    else
      WithSelf := 'T_' + Metodo.Classe.Nome + '(Self)';
    if CodigoSemVar then
      Codigo := 'begin with ' + WithSelf + ' do begin' + nl + Codigo + nl + 'end end;'
    else begin
      insert('begin with ' + WithSelf + ' do ', Codigo, pos('begin', Codigo));
      Codigo := Codigo + ' end;';
    end;
    GerarInherited(Metodo, Codigo);
    Lin(trim(Codigo));
  end;
end;

procedure GerarImplementation;
var
  Link : TLink;
  Metodo : TMetodo;
  CC : integer;
begin
  Lin(nl + 'implementation' + nl);
  Lin('uses TypInfo, StrUtils, DateUtils, Math, MaskUtils, pitCommon, pitStateMachine, pitBrowserProxy' +
    IfThen(Pacote.UnidadesExternas = '', '', ', ' + Pacote.UnidadesExternas) + ';' + nl);
  Lin('type');
  MapClass := TStringList.Create;
  Link := fClasseList.First;
  while Link <> nil do begin
    Lin(Tab + 'T_' + Link.Nome + ' = class(T' + Link.Nome + '); T_Class' + Link.Nome + ' = class of T_' + Link.Nome + ';');
    MapClass.Add(Link.Nome);
    fClasseList.Next(Link);
  end;
  Link := fClasseList.First;
  while Link <> nil do with TClasse(Link) do begin
    Metodo := Metodos.First;
    while Metodo <> nil do with Metodo do begin
      GerarDescricaoParametros(Metodo);
      if goMetrics in fGeracao.Opcoes then
        CC := CyclomaticComplexity(Metodo)
      else
        Cc := 0;
      Lin(GerarDescricaoProximaLinha('', Documentacao + ^M^J'Cyclomatic Complexity: ' + IntToStr(CC) + ', ' + Risk(10, 20, CC)));
      GerarPrototipo(Metodo);
      TypeCasts.Clear;
      TypeCasts.Add('T_' + Link.Nome + '(');
      if not GerarMaquinaEstados(Metodo) then GerarCodigo(Metodo);
      Metodos.Next(Metodo);
    end;
    fClasseList.Next(Link);
  end;
  MapClass.Free;
end;

begin
  UnitName := Pacote.Nome;
  GerarCabecalho(Pacote);
  Lin('// Methods');
  GerarInterface;
  GerarImplementation;
  Lin('end.');
end;

procedure TGerador.GerarMetricas;
var
  CCs, Operations : integer;
  FP, FPs : double;
  RiskClasses, RiskOperations, S : string;
  Classe : TLink;
begin
  Lin('{');
  S := 'Pontos de Função: FPA';
  Lin(S);
  Browser.Message('...' + S);
  FPs := 0;
  RiskClasses := 'Nenhuma';
  Classe := fClasseList.First;
  while Classe <> nil do begin
    if Classe is TClasse then begin
      FP := FunctionPoints(TClasse(Classe));
      if FP > (10 * FPAInfluence) then
        if RiskClasses = 'Nenhuma' then
          RiskClasses := 'T' + Classe.Nome
        else
          RiskClasses := RiskClasses + ', T' + Classe.Nome;
      FPs := FPs + FP;
    end;
    fClasseList.Next(Classe);
  end;
  try
    S := format('Total: %.0f, Média: %.1f, Risco geral: %s, Classes de alto risco: %s',
                [FPs, FPs/fClasseList.Count, Risk(8 * FPAInfluence, 12 * FPAInfluence, FPs/fClasseList.Count), RiskClasses]);
  except
    S := ''
  end;
  Lin(Tab + S);
  Browser.Message('....' + S);
  S := 'Cyclomatic Complexity: v(G)';
  Lin(S);
  Browser.Message('...' + S);
  CCs := 0; Operations := 0;
  RiskOperations := 'Nenhum';
  Classe := fClasseList.First;
  while Classe <> nil do begin
    if Classe is TClasse then inc(CCs, CyclomaticComplexity(TClasse(Classe), Operations, RiskOperations));
    fClasseList.Next(Classe);
  end;
  if Operations = 0 then Operations := 1;
  S := format('Total: %d, Média: %.1f, Risco geral: %s, Métodos de alto risco: %s', [CCs, CCs/Operations, Risk(10, 20, CCs/Operations), RiskOperations]);
  Lin(Tab + S);
  Browser.Message('....' + S);
  Lin('}' + nl);
end;

procedure TGerador.GerarCabecalho(Pacote : TPacote);
var
  fClasse : TLink;
  fAssociacao : TAssociacao;
  Declaracoes : String;
begin
  Lin('{$A1,B-,C-,D-,E-,F-,G+,H+,I-,J+,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V-,W-,X+,Y-,Z1}');
  Lin('{$IFDEF FPC}{$PACKSET 1}{$PACKRECORDS 1}{$Z1}{$ENDIF}');
  Lin('{$IFDEF DEBUG}{$D+,O-,A1,I+,W+,V-,R+,B-,Q+,S+,X+,P+,H+,J+,L+,Y+}{$ENDIF}');
  Lin('unit ' + UnitName + ';');
  Lin('{');
  Lin('Gerado por pitinnu versão ' + GeneratorVersion);
  Lin('Usuário : ' + GetUserName);
  Lin('Máquina : ' + GetComputerName);
  Lin('}' + nl);
  fClasseList := CreateViewClasse(Pacote, true);
  if goMetrics in fGeracao.Opcoes then GerarMetricas;
  Lin('interface' + nl);
  Lin('uses');
  if Pacote = nil then begin
    Declaracoes := '';
    Pacote := fModelo.Pacotes.First;
    while Assigned(Pacote) do begin
      if Pacote.Declaracao <> '' then Declaracoes := Declaracoes + Pacote.Declaracao;
      fModelo.Pacotes.Next(Pacote);
    end;
    Lin(Tab + 'Classes, SysUtils, pitUtils, pitObjectList, pitPrevalence, pitWorkflow;' + nl);
    if Declaracoes <> '' then begin
      Lin('//*** User Declarations ***');
      Lin(Declaracoes);
      Lin('//*************************' + nl);
    end;
  end
  else
    Lin(Tab + 'Classes, SysUtils, pitUtils, pitObjectList, pitPrevalence, pitWorkflow, ' + fModelo.Nome + 'Model;' + nl);
  fClasseList.Free;
  fClasseList := CreateViewClasse(Pacote, false);
  if Pacote <> nil then exit;
  Lin('type');
  fClasse := fClasseList.First;
  while Assigned(fClasse) do begin
    Lin(Tab + 'T' + fClasse.Nome + ' = class; TClass' + fClasse.Nome + ' = class of T' + fClasse.Nome + ';');
    fClasseList.Next(fClasse);
  end;
  fClasseList.Free;
  Lin;
  fAssociacao := AssociacaoList.First;
  while Assigned(fAssociacao) do with fAssociacao do begin
      if fModelo.Pacotes.Find(fAssociacao.Destino.Pacote.Nome) <> nil then begin
        if (PapelDestino <> nil) and (GetMaxPapel(PapelDestino) > 1) then
          Lin(Tab + 'T' + GetOriDes(Origem).Nome + PapelDestino.Nome + 'Association = class;');
        if (PapelOrigem <> nil) and (GetMaxPapel(PapelOrigem) > 1) then
          Lin(Tab + 'T' + GetOriDes(Destino).Nome + PapelOrigem.Nome + 'Association = class;');
      end;
    AssociacaoList.Next(fAssociacao);
  end;
end;

function AcharPrimeiroEstado(Metodo : TMetodo) : TEstadoBase; begin
  Result := Metodo.Estados.First;
  while Result <> nil do
    if (Result is TEstado) and (TEstado(Result).Tipo = etInicial) then begin
      Result := Result.Destinos.First.Destino;
      exit
    end
    else
      Metodo.Estados.Next(Result);
end;

function GerarTimeout(Estado : TEstadoBase) : string;
var
  I : integer;
  Condicao : string;
  Transicao : TTransicao;
begin
  Result := '';
  Transicao := Estado.Destinos.First;
  while Transicao <> nil do begin
    Condicao := lowercase(Transicao.Condicao);
    I := pos('after(', Condicao);
    if I = 0 then I := pos('when(', Condicao);
    if I <> 0 then begin (*
      if Condicao[J] = 'a' then begin
        inc(J, 6);
        Result := ', Now + ';
      end
      else begin
        inc(J, 5);
        Result := ', '
      end;*)
      Transicao.Condicao := 'true';
      EXIT; // ******************
      (*Prox   := ProximoEstado(Operacao, Parser.Transicoes[Estado.Transicoes_[I]].Destino);
      Result := Result + copy(Condicao, J, length(Condicao) - J) + ', ' + Parser.Estados[Operacao.Estados_[Prox].Estado].Nome;
      exit;*)
    end;
    Estado.Destinos.Next(Transicao);
  end;
end;

procedure TGerador.ProgramarJoin(var Join : TEstado); begin
  if Join.Acao = '' then
    Join.Acao := 'Pendencies := nil'^M'if not EndPendency(This) then exit'^M'EndTransaction(true)'^M'Pendencies.Free'^M
  else
  if pos('if not EndPendency(This) then', Join.Acao) = 0 then
    Join.Acao := 'if not EndPendency(This) then exit'^M'EndTransaction(true)'^M'BeginTransaction'^M'try'^M +
        Tab + 'Pendencies := TPendencyList.Create'^M + Tab + 'try'^M + Tab(2) + 'Pendencies.New(This, ' + Join.Metodo.Nome + GerarTimeout(Join) + ')'^M +
        Tab(2) + AddTypeCasts(Join.Acao) + ^M +
        Tab(2) + 'EndTransaction(true)'^M + Tab(2) + 'TJoinPendencyList(Pendencies).Broadcast'^M +
        Tab + 'finally'^M + Tab(2) + 'Pendencies.Free'^M + Tab + 'end'^M'except'^M+ Tab + 'Rollback'^M'end';;
end;

procedure TGerador.GerarInherited(pMetodo : TMetodo; var S : string);
var
  Params : string;
  fParametro : TParametro;
  fClassePai : TClasse;
begin
  if pos('inherited', lowercase(S)) <> 0 then begin
    S := AnsiReplaceText(S, 'inherited ' + pMetodo.Nome, 'inherited');
    Params := '';
    fParametro := pMetodo.Parametros.First;
    while fParametro <> nil do begin
      Params := Params + fParametro.Nome;
      if pMetodo.Parametros.Next(fParametro) then Params := Params + ', ';
    end;
    fClassePai := ClassePai(pMetodo.Classe, pMetodo.Nome);
    ModelError(fClassePai = nil, '"Inherited" inválido, sem classe pai. ' + LinkError(pMetodo.Classe.Nome + '.' + pMetodo.Nome));
    if Params <> '' then Params := ', ' + Params;
    if not Error then S := AnsiReplaceText(S, 'inherited', fClassePai.Nome + '_' + pMetodo.Nome + '(Self' + Params + ')');
  end;
end;

function TGerador.GerarMaquinaEstados(pMetodo : TMetodo) : boolean;
var
  WithReturn, WithThis : string;

  function VerificaTipo(S : string) : boolean; begin
    Result := true;
    if pos('arrayof', lowerCase(StringReplace(S,' ', '' , [rfReplaceAll, rfIgnoreCase]))) = 1 then exit;
    Result := IsValidIdent(S);
  end;

  function InAttributes(S : string) : boolean;
  var
    Atributo : TAtributo;
  begin
    Result := false;
    if not (pMetodo.Tipo in [mtWorkflow, mtWizard]) then begin
      Atributo := pMetodo.Classe.Atributos.First;
      while Atributo <> nil do begin
        if lowercase(Atributo.Nome) = lowercase(S) then begin
          Result := true;
          exit;
        end;
        pMetodo.Classe.Atributos.Next(Atributo);
      end;
    end;
  end;

  procedure GerarContexto;
  var
    Parametro : TParametro;
    Estado    : TEstadoBase;
    Variavel  : TVariavel;
    TipoPar   : string;
  begin
    Lin('type');
    Lin(Tab + 'TContext = record');
    if not (mcDeClasse in pMetodo.Caracteristicas) or (pMetodo.Tipo in [mtWorkflow, mtWizard]) then begin
      Lin(Tab(2) + 'This : T' + pMetodo.Classe.Nome + ';');
      WithThis := 'Context, T_' + pMetodo.Classe.Nome + '(This)';
      TypeCasts.Add('This');
    end
    else begin
      Lin(Tab(2) + 'This : TClass' + pMetodo.Classe.Nome + ';');
      WithThis := 'T_Class' + pMetodo.Classe.Nome + '(Context.This), Context'
    end;
    Parametro := pMetodo.Parametros.First;
    while Parametro <> nil do with Parametro do begin
      ModelError(not IsValidIdent(Nome), 'Nome de parâmetro inválido: ' + Nome + '. ' + LinkError(pMetodo.Classe.Nome + '.' + pMetodo.Nome));
      ModelError(InAttributes(Nome), 'Prática desaconselhável: Parâmetro com o mesmo nome de atributo de classe: ' + Nome + '. ' + LinkError(pMetodo.Classe.Nome + '.' + pMetodo.Nome));
      TipoPar := GetTipo(pMetodo.Classe, Parametro);
      if TipoPar = 'T' + pMetodo.Classe.Nome then TypeCasts.Add(Nome);
      Lin(Tab(2) + Nome + ' : ' + TipoPar + ';');
      pMetodo.Parametros.Next(Parametro);
    end;
    if pMetodo.Retorno <> nil then begin
      TipoPar := GetTipo(pMetodo.Classe, pMetodo.Retorno);
      if TipoPar = 'T' + pMetodo.Classe.Nome then TypeCasts.Add('Return');
      Lin(Tab(2) + 'Return : ' + TipoPar + ';');
    end;
    Estado := AcharPrimeiroEstado(pMetodo);
    if (Estado <> nil) and (Estado is TEstado) then begin
      Variavel := TEstado(Estado).Variaveis.First;
      while Variavel <> nil do begin
        TipoPar := GetTipo(pMetodo.Classe, Variavel);
        if TipoPar = 'T' + pMetodo.Classe.Nome then TypeCasts.Add(Variavel.Nome);
        Lin(Tab(2) + Variavel.Nome + ' : ' + TipoPar + ';');
        TEstado(Estado).Variaveis.Next(Variavel)
      end;
    end;
    Lin(Tab + 'end;');
    Lin('var');
    Lin(Tab + 'Context : TContext;');
  end;

  function EhEstadoInterativo(pEstado : TEstadoBase) : boolean; begin
    Result := (pEstado is TFormulario) or
              (pos('browser.edit',   lowercase(TEstado(pEstado).Acao)) <> 0) or
              (pos('browser.choose', lowercase(TEstado(pEstado).Acao)) <> 0)
  end;

  function UltimoEstadoUtilWizard(Estado : TEstadoBase; pEstadosVisitados : TList) : boolean;
  var
    Prox : TTransicao;
  begin
    Result := true;
    Prox := Estado.Destinos.First;
    while Prox <> nil do begin
      if pEstadosVisitados.IndexOf(pointer(Prox.Destino)) <> -1 then
        break
      else
        if EhEstadoInterativo(Prox.Destino) then begin
          Result := false;
          break;
        end
        else begin
          Result := UltimoEstadoUtilWizard(Prox.Destino, pEstadosVisitados);
          if not Result then break
        end;
      Estado.Destinos.Next(Prox)
    end;
    pEstadosVisitados.Add(pointer(Estado));
  end;

  function PrimeiroEstadoUtilWizard(Estado : TEstadoBase; pEstadosVisitados : TList) : boolean;
  var
    Prox : TTransicao;
  begin
    Result := true;
    Prox := Estado.Origens.First;
    while Prox <> nil do begin
      if pEstadosVisitados.IndexOf(pointer(Prox.Origem)) <> -1 then
        break
      else
        if EhEstadoInterativo(Prox.Origem) then begin
          Result := false;
          exit;
        end
        else begin
          Result := PrimeiroEstadoUtilWizard(Prox.Origem, pEstadosVisitados);
          if not Result then break
        end;
      Estado.Origens.Next(Prox)
    end;
    pEstadosVisitados.Add(pointer(Estado));
  end;

  procedure GerarProceduresEstados(JoinsAdicionais : TList);
  var
    S : string;
    EstadosVisitados : TList;
    fEstadoBase, PrimEstado : TEstadoBase;
    fVariavel : TVariavel;

    function GerarFormatacoes(Formulario : TFormulario) : string;
    var
      F : TFormatacao;
      S : string;
    begin
      Result := '';
      F := Formulario.Formatacoes.First;
      while F <> nil do with F do begin
        case Comando of
          fcMostrar, fcEditar: begin
            if Comando = fcMostrar then
              S := '#' + Propriedade
            else
              S := Propriedade;
            case Layout of
              flOcuparTodaLargura       : Result := Result + '/' + S + '/';
              flIniciarNaMargemEsquerda : Result := Result + '/' + S;
            else
              Result := Result + S;
            end;
          end;
          fcCriarGrupo          : Result := Result + '|' + Titulo + '|';
          fcCriarGrupoColapsavel: Result := Result + '<' + Titulo + '>';
          fcCriarGrupoColapsado : Result := Result + '>' + Titulo + '<';
          fcCriarAba            : Result := Result + '[' + Titulo + ']';
          fcFecharAbas          : Result := Result + '[]';
        end;
        Formulario.Formatacoes.Next(F);
        if F <> nil then Result := Result + ',';
      end;
    end;

    procedure GerarProcedure(fEstadoBase : TEstadoBase);
    var
      TipoVar : string;
    begin
      if fEstadoBase is TEstado then begin
        with TEstado(fEstadoBase) do
          if (Acao <> '') and (Tipo in [etPadrao, etForkV, etForkH]) then begin
            if (Variaveis.Count > 0) and (fEstadoBase <> PrimEstado) then begin
              Lin('procedure ' + Nome + '_(var Context : TContext);');
              Lin('var');
              fVariavel := Variaveis.First;
              while fVariavel <> nil do begin
                TipoVar := GetTipo(Metodo.Classe, fVariavel);
                if TipoVar = 'T' + Metodo.Classe.Nome then TypeCasts.Add(fVariavel.Nome);
                Lin(Tab + fVariavel.Nome + ' : ' + TipoVar + ';');
                Variaveis.Next(fVariavel);
              end;
              Lin('begin');
            end
            else
              if Tipo in [etForkV, etForkH] then begin
                Lin('procedure ' + Nome + '_(var Context : TContext);');
                Lin('var');
                Lin(' Pendencies: TPendencyList;');
                Lin('begin');
                ProgramarJoin(TEstado(fEstadoBase));
              end
              else
                Lin('procedure ' + Nome + '_(var Context : TContext); begin');
            S := TrimRight(AnsiReplaceText(Acao, ^M^J, ';'^M^J + Tab(2)));
            GerarInherited(pMetodo, S);
            S := AddTypeCasts(S);
            if pos(';', S) = 0  then
              Lin(Tab + 'with ' + WithThis + WithReturn + ' do ' + S)
            else begin
              Lin(Tab + 'with ' + WithThis + WithReturn + ' do begin');
              Lin(Tab(2) + S);
              Lin(Tab + 'end;');
            end;
            Lin('end;' + nl);
          end
      end
      else
        if pMetodo.Tipo in [mtWorkflow, mtWizard] then begin
          Lin('procedure ' + fEstadoBase.Nome + '_(var Context : TContext); begin');
          fEstadoBase.Botoes := '';
          EstadosVisitados := TList.Create;
          try
            fEstadoBase.Botoes := '[bbCancel' + IfThen(PrimeiroEstadoUtilWizard(fEstadoBase, EstadosVisitados), '', ', bbBack');
          finally
            EstadosVisitados.Free;
          end;
          EstadosVisitados := TList.Create;
          try
            fEstadoBase.Botoes := fEstadoBase.Botoes + IfThen(UltimoEstadoUtilWizard(fEstadoBase, EstadosVisitados), ', bbFinish', ', bbNext') + ']';
          finally
            EstadosVisitados.Free;
          end;
          Lin(Tab + 'with ' + WithThis + ' do');
          with TFormulario(fEstadoBase) do begin
            if Ajuda <> nil then
              S := SplitLine(Ajuda.Dica)
            else
              S := '';
            Lin(Tab(2) + 'Browser.Edit(' + Objeto + ', ''' +
              WrapText(GerarFormatacoes(TFormulario(fEstadoBase)), ''' + '#13#10'''', [','], 100) +
              ''', ''' + Apelido + ''', ''' + S + ''', StateMachine.FixButtons(' + Botoes + '));');
          end;
          Lin('end;' + nl);
        end;
    end;

  var
    I : integer;

  begin
    PrimEstado := AcharPrimeiroEstado(pMetodo);
    fEstadoBase := pMetodo.Estados.First;
    while fEstadoBase <> nil do begin
      GerarProcedure(fEstadoBase);
      pMetodo.Estados.Next(fEstadoBase);
    end;
    for I := 0 to JoinsAdicionais.Count-1 do
      GerarProcedure(TEstadoBase(JoinsAdicionais[I]));
  end;

  procedure GerarFunctionsTransicoes;
  var
    fEstadoBase : TEstadoBase;
    fTransicao  : TTransicao;
  begin
    with pMetodo do
      fEstadoBase := pMetodo.Estados.First;
      while Assigned(fEstadoBase) do with fEstadoBase do begin
        fTransicao := Destinos.First;
        while Assigned(fTransicao) do with fTransicao do begin
          if (trim(Nome) <> '') and (trim(Condicao) <> '') then begin //Default
            Lin('function ' + Nome + '_(var Context : TContext) : boolean; begin');
            if Acao = '' then
              Lin(Tab + 'with ' + WithThis + WithReturn + ' do Result := ' + AddTypeCasts(Condicao) + ';')
            else begin
              Lin(Tab + 'with ' + WithThis + WithReturn + ' do begin');
              Lin(Tab(2) + 'Result := ' + IfThen(Condicao = '', 'true', AddTypeCasts(Condicao)) + ';');
              if pos(';', Acao) = 0 then
                Lin(Tab(2) + 'if Result then ' + AddTypeCasts(Acao) + ';')
              else begin
                Lin(Tab(2) + 'if Result then begin');
                Lin(Tab(3) + TrimRight(AnsiReplaceText(AddTypeCasts(Acao), ';'^M, ';'^M^J + Tab(3))));
                Lin(Tab(2) + 'end;');
              end;
              Lin(Tab + 'end;');
            end;
            Lin('end;' + nl);
          end;
          Destinos.Next(fTransicao);
        end;
        pMetodo.Estados.Next(fEstadoBase);
      end;
  end;

  function EstadoUtil(Estado : TEstadoBase) : boolean; begin
    Result := (Estado is TFormulario) or ((Estado is TEstado) and not(TEstado(Estado).Tipo in [etInicial, etFinal]))
  end;

  function ProximoEstado(M : TMetodo; E, PrimEstado : TEstadoBase; JoinsAdicionais : TList) : integer;
  var
    I : integer;
    Estado : TEstadoBase;
  begin
    I := 1;
    Result := -1;
    if PrimEstado = E then begin
      Result := 0;
      exit;
    end;
    Estado := M.Estados.First;
    while Estado <> nil do begin
      if (PrimEstado <> Estado) and EstadoUtil(Estado) then begin
        if Estado = E then begin
          if (Estado is TEstado) and (TEstado(Estado).Tipo = etFinal) then exit;
          Result := I;
          exit;
        end;
        inc(I);
      end;
      M.Estados.Next(Estado)
    end;
    for I := 0 to JoinsAdicionais.Count-1 do
      if TEstadoBase(JoinsAdicionais[I]) = E then begin
         Result := I + M.Estados.Count - 2;
         exit
      end;
  end;

  procedure GerarTransicoes(PrimEstado : TEstadoBase; JoinsAdicionais : TList);
  var
    E, T : integer;
    SemFim : boolean;

  procedure SetTransition(Trans : TTransicao); begin
    with Trans do
      Lin(Tab(2) + 'SetTransition(' + IntToStr(E) + ', ' + IntToStr(T) + ', ''' + ifThen((Nome = '') or (Condicao = ''), 'Default', Nome) + ''', ' +
          IfThen((Nome = '') or (Condicao = ''), 'nil', '@' + Nome + '_') + ', ' + IntToStr(ProximoEstado(pMetodo, Destino, PrimEstado, JoinsAdicionais)) + ');');
  end;

  procedure SetTransitions(Estado : TEstadoBase);
  var
    Default, I : Integer;
    fTransicao, TransDefault : TTransicao;
    S : string;
  begin
    T := 0;
    if (Estado is TEstado) and (TEstado(Estado).Tipo in [etForkV, etForkH]) then
      Lin(Tab(2) + 'SetTransition(' + IntToStr(E) + ', ' + IntToStr(T) + ', ''Default'', nil, -1);')
    else begin
      Default := 0;
      fTransicao := Estado.Destinos.First;
      TransDefault := nil;
      while fTransicao <> nil do with fTransicao do begin
        if (Destino is TEstado) and (TEstado(Destino).Tipo in [etFinal, etForkV, etForkH]) then SemFim := false;
        if trim(Condicao) = '' then begin
          inc(Default);
          TransDefault := fTransicao
        end
        else begin
          SetTransition(fTransicao);
          inc(T);
        end;
        Estado.Destinos.Next(fTransicao);
      end;
      if TransDefault <> nil then SetTransition(TransDefault);
      if Default <> 1 then begin
        S := pMetodo.Nome;
        I := pos('_', S);
        if I = 0 then
          S := S + '.' + Estado.Nome
        else
          S := copy(S, 1, I) + Estado.Nome;
        ModelError(true, 'Estado ' + Estado.Nome + ' deve ter uma e só uma transição default. ' + LinkError(pMetodo.Classe.Nome + '.' + S));
      end;
    end;
    inc(E);
  end;

  var
    fEstadoBase : TEstadoBase;
    I : integer;
  begin
    E := 0; SemFim := true;
    if PrimEstado <> nil then SetTransitions(PrimEstado);
    fEstadoBase := pMetodo.Estados.First;
    while fEstadoBase <> nil do begin
      if (fEstadoBase <> PrimEstado) and EstadoUtil(fEstadoBase) then SetTransitions(fEstadoBase);
      pMetodo.Estados.Next(fEstadoBase);
    end;
    for I := 0 to JoinsAdicionais.Count-1 do
      SetTransitions(TEstadoBase(JoinsAdicionais[I]));
    ModelError(SemFim, 'Máquina de Estado sem estado Final. ' + LinkError(pMetodo.Classe.Nome + '.' + pMetodo.Nome));
  end;

  procedure SetState(Estado : TEstadoBase; Start : boolean = false);
  const
    E : integer = 0;
  begin
    if Start then E := 0;
    with Estado do
      Lin(Tab(2) + 'SetState(' + IntToStr(E) + ', ''' + Nome + ''', ' + ifThen((Estado is TEstado) and (TEstado(Estado).Acao = ''), 'nil', '@' + Nome + '_') +
        ifThen(Botoes <> '', ', ' + Botoes, '') + ');');
    inc(E);
  end;

  procedure AcharJoinsAdicionais(var JoinsAdicionais : TList);
  var
    Estado, Destino : TEstadoBase;
    Trans  : TTransicao;
  begin
    JoinsAdicionais := TList.Create;
    Estado := pMetodo.Estados.First;
    while Estado <> nil do begin
      Trans := Estado.Destinos.First;
      while Trans <> nil do begin
        Destino := Trans.Destino;
        if (Destino is TEstado) and (TEstado(Destino).Tipo in [etForkV, etForkH]) and
           (Destino.Metodo <> pMetodo) and (JoinsAdicionais.IndexOf(Destino) = -1) then
          JoinsAdicionais.Add(Destino);
        Estado.Destinos.Next(Trans)
      end;
      pMetodo.Estados.Next(Estado)
    end;
  end;

//inicio da procedure GerarMaquinaEstados
var
  TransicoesPorEstado : string;
  fParametro : TParametro;
  fEstadoBase, PrimEstado : TEstadoBase;
  JoinsAdicionais : TList;
  I : integer;
begin
  with pMetodo do begin
    Result := Estados.Count > 0;
    if Result then begin
      WithReturn := '';
      if (pMetodo.Retorno <> nil) and (not TipoEhPrimitivo(pMetodo.Retorno)) then
        WithReturn := ', Return';
      GerarContexto;
      Lin(Tab + 'StateMachine : TStateMachine;' + nl);
      AcharJoinsAdicionais(JoinsAdicionais);
      GerarProceduresEstados(JoinsAdicionais);
      GerarFunctionsTransicoes;
      Lin('begin');
      Lin(Tab + 'fillchar(Context, sizeof(Context), 0);');
      if not((mcDeClasse in Caracteristicas) and (pMetodo.Tipo in [mtWorkflow, mtWizard])) then
        Lin(Tab + 'Context.This := Self;');
      fParametro := Parametros.First;
      while Assigned(fParametro) do begin
        Lin(Tab + 'Context.' + fParametro.Nome + ' := ' + fParametro.Nome + ';');
        Parametros.Next(fParametro);
      end;
      PrimEstado := AcharPrimeiroEstado(pMetodo);
      if PrimEstado <> nil then
        TransicoesPorEstado := IntToStr(PrimEstado.Destinos.Count) + ', '
      else
        ModelError(true, 'Máquina de Estado sem estado Inicial. ' + LinkError(pMetodo.Classe.Nome + '.' + pMetodo.Nome));
      fEstadoBase := Estados.First;
      while fEstadoBase <> nil do begin
        if (fEstadoBase <> PrimEstado) and EstadoUtil(fEstadoBase) then
          if (fEstadoBase is TEstado) and (TEstado(fEstadoBase).Tipo in [etForkV, etForkH]) then
            TransicoesPorEstado := TransicoesPorEstado + '1, '
          else
            TransicoesPorEstado := TransicoesPorEstado + IntToStr(fEstadoBase.Destinos.Count) + ', ';
        Estados.Next(fEstadoBase);
      end;
      for I := 0 to JoinsAdicionais.Count-1 do
        TransicoesPorEstado := TransicoesPorEstado + '1, ';
      TransicoesPorEstado := copy(TransicoesPorEstado, 1, length(TransicoesPorEstado)-2);
      Lin(Tab + 'StateMachine := TStateMachine.Create(''' + pMetodo.Classe.Pacote.Nome + ''', ''' + pMetodo.Classe.Nome + ''', ''' +  Nome + ''', [' + TransicoesPorEstado + ']);');
      Lin(Tab + 'with StateMachine do begin');
      if PrimEstado <> nil then SetState(PrimEstado, true);
      fEstadoBase := Estados.First;
      while fEstadoBase <> nil do begin
        if (fEstadoBase <> PrimEstado) and EstadoUtil(fEstadoBase) then SetState(fEstadoBase);
        Estados.Next(fEstadoBase);
      end;
      for I := 0 to JoinsAdicionais.Count-1 do
        SetState(TEstadoBase(JoinsAdicionais[I]));
      GerarTransicoes(PrimEstado, JoinsAdicionais);
      JoinsAdicionais.Free;
      Lin(Tab + 'end;');
      Lin(Tab + 'try');
      Lin(Tab(2) + 'inc(CallStateMachineCount);');
      if Tipo = mtMaquinaDeEstados then
        Lin(Tab(2) + 'StateMachine.Execute(@Context);')
      else begin
        Lin(Tab(2) + 'try');
        Lin(Tab(3) + 'BeginTransaction;');
        Lin(Tab(3) + 'StateMachine.Execute(@Context, true);');
        Lin(Tab(3) + 'if Threadtrans <> nil then EndTransaction;');
        Lin(Tab(2) + 'except');
        Lin(Tab(3) + 'Rollback;');
        Lin(Tab(2) + 'end;');
      end;
      fParametro := Parametros.First;
      while Assigned(fParametro) do with fParametro do begin
        if Passagem <> ppEntrada then
          Lin(Tab(2) + Nome + ' := Context.' + Nome + ';');
        Parametros.Next(fParametro);
      end;
      if Retorno <> nil then
        Lin(Tab(2) + 'Result := Context.Return;');
      Lin(Tab + 'finally');
      Lin(Tab(2) + 'dec(CallStateMachineCount);');
      Lin(Tab(2) + 'StateMachine.Free;');
      Lin(Tab + 'end;');
      Lin('end;');
    end;
  end;
end;

procedure TGerador.Lin(S : string = ''); begin
  if not Error then Pas := Pas + S + ^M^J
end;

function TGerador.nl(I : integer = 1) : string;
var
  J : integer;
begin
  Result := '';
  for J := 1 to I do
    Result := Result + ^M^J;
end;

function TGerador.Tab(I : integer = 1) : string;
var
  J : integer;
begin
  Result := '';
  for J := 1 to I do
    Result := Result + '  ';
end;

procedure TGerador.GerarCreate(pClasse : TClasse);
var
  fIndice : TIndice;
  fClasse : TClasse;
begin
  with pClasse do begin
    if Tipo = ctAbstrata then
      Lin(Tab + 'F'+  Nome + 'List := T' + Nome  + 'List.Create([lpAbstract]);')
    else
    if Tipo = ctPrevalente then
      Lin(Tab + 'F'+ Nome + 'List := T' + Nome + 'List.Create;')
    else
      Lin(Tab + 'F'+ Nome + 'List := T' + Nome + 'List.Create([lpTransient]);');
  end;
  fClasse := pClasse;
  while Assigned(fClasse) do begin
    fIndice := fClasse.Indices.First;
    while Assigned(fIndice) do begin
      if pClasse.Tipo <> ctAbstrata then
        Lin(Tab + 'F'+ pClasse.Nome + fIndice.Nome + 'List := T' + pClasse.Nome + fIndice.Nome + 'List.Create' + '(' + ifthen(not fIndice.Unico, '[lpDuplicates]', '[]') + ');');
      fClasse.Indices.Next(fIndice);
    end;
    fClasse := ClassePai(fClasse);
  end;
end;

function isUnidirect(pClasse : TClasse; pAssociacao : TAssociacao) : boolean; begin
  Result := false;
  with pAssociacao do
    if (PapelDestino = nil) or (PapelOrigem = nil) then begin //unidirecional
      if (PapelOrigem <> nil) and (pClasse = GetOriDes(Origem)) and (GetMaxPapel(PapelOrigem) = 1) then Result := true else
      if (PapelDestino <> nil) and (pClasse = GetOriDes(Destino)) and (GetMaxPapel(PapelDestino) = 1) then Result := true;
    end;
end;

procedure TGerador.GerarMetadata(pClasse : TClasse);
var
  fClasse : TClasse;
  fMetodo : TMetodo;
  fIndice : TIndice;
  fAssociacao : TAssociacao;
  fAtributo : TAtributo;
  fParametro : TParametro;
  fRelacionamentos : TAssociacaoList;
  Constraints : string;
  Prop, Ops, P : integer;
  Gera : boolean;
  MethodKind, Semantics, DefaultParam : string;

  function MontaUnidirectConstraints(pPapel : TPapel) : string; begin
    Result := '';
    if pPapel.Agregacao = paFraca then Result := 'shared' else
    if pPapel.Agregacao = paForte then Result := 'composite';
    if GetMinPapel(pPapel) > 0 then
      if Result <> '' then
        Result := Result + ', NOTNULL'
      else
        Result := Result + 'NOTNULL';
  end;

  function MontaAjuda(Ajuda : TAjuda) : string;
  var
    S : string;
  begin
    if Ajuda <> nil then begin
      S := Ajuda.Dica;
      if Ajuda.Texto <> '' then S := S + '|' + Ajuda.Texto;
      Result := SplitLine(S)
    end
    else
      Result := '';
  end;

  procedure MontaConstraintAssociation(pPapel : TPapel);
  var
    C : string;
  begin
    C := '';
    with pPapel do begin
      if GetMinPapel(pPapel) > 0 then C := 'NOTNULL, ';
      if Agregacao = paForte then C := C + 'COMPOSITE, ' else
      if Agregacao = paFraca then C := C + 'SHARED, ';
      if Validacao <> '' then C := C + 'CHECK, ';
      if Habilitacao <> '' then C := C + 'ENABLED, ';
      if Visibilidade <> '' then C := C + 'VISIBLE, ';
      if Constraint <> '' then C := C + 'ASSOCIATIONCONSTRAINT, ';
    end;
    if C <> '' then begin
      C := copy(C, 1, length(C) -2);
      Lin(tab(2) + format('AddConstraints(%d, [%s]);', [Prop, C]));
    end;
  end;

  function InModelo(C : string) : boolean;
  var
    lClasse : TLink;
  begin
    Result := false;
    lClasse := fClasseList.First;
    while lClasse <> nil do begin
      if lClasse.Nome = C then begin
        Result := true;
        exit;
      end;
      fClasseList.Next(lClasse)
    end;
  end;

  procedure GeraUnidirect; begin
    with fAssociacao do begin
      if (PapelDestino = nil) or (PapelOrigem = nil) then begin //unidirecional
        if (PapelOrigem <> nil) and (pClasse = GetOriDes(Origem)) and (GetMaxPapel(PapelOrigem) = 1) and InModelo(GetOriDes(Destino).Nome) then
          Lin(tab(2) + format('AddUnidirectional(T%s, ''%s'', [%s]);', [GetOriDes(Destino).Nome, PapelOrigem.Nome, MontaUnidirectConstraints(PapelOrigem)]))
        else
        if (PapelDestino <> nil) and (pClasse = GetOriDes(Destino)) and (GetMaxPapel(PapelDestino) = 1) and InModelo(GetOriDes(Origem).Nome) then
          Lin(tab(2) + format('AddUnidirectional(T%s, ''%s'', [%s]);', [GetOriDes(Origem).Nome, PapelDestino.Nome, MontaUnidirectConstraints(PapelDestino)]));
      end;
    end;
  end;

  procedure GeraAssociacao;

    function Gera : boolean; begin
      with fAssociacao do
        Result := ((GetOriDes(Destino).Apelido <> '') and (GetOriDes(Destino).Apelido <> GetOriDes(Destino).Nome)) or (GetOriDes(Destino).Ajuda <> nil) or
                  ((GetOriDes(Origem).Apelido <> '') and (GetOriDes(Origem).Apelido <> GetOriDes(Origem).Nome)) or (GetOriDes(Origem).Ajuda <> nil) or
                  ((PapelDestino <> nil)) or ((PapelOrigem <> nil));
    end;

    procedure TrataPapeis(pIsOrigem : boolean);
    var
      S : string;

      function MontaAlias(pPapel : TPapel) : string; begin
        Result := '';
        if pPapel = nil then exit;
        with pPapel do begin
          if (Apelido <> Nome) and (Apelido <> '') then Result := Apelido;
          if Caracteristicas * [pcOculto] <> [] then
            if Result <> '' then begin
              if pos('hidden', lowerCase(Result)) = 0 then Result := 'hidden, ' + Result
            end
            else
              Result := 'hidden';
        end;
      end;

    begin
      with fAssociacao do begin
        S := '';
        if not pIsOrigem then begin
          if (PapelOrigem <> nil) and (pClasse = GetOriDes(Destino)) then begin
            if PapelDestino <> nil then S := PapelDestino.Nome;
            inc(Prop);
            Lin(tab(2) + format('AddMetadata(%d, ''%s'', ''%s'', T%s, ''%s'');', [Prop, MontaAlias(PapelOrigem), MontaAjuda(PapelOrigem.Ajuda),
                GetOriDes(Origem).Nome , S]));
            MontaConstraintAssociation(PapelOrigem);
            if (PapelOrigem <> nil) and (PapelOrigem.NomeAntigo <> '') then Lin(tab(2) + format('AddOldName(%d, ''%s'');', [Prop, PapelOrigem.NomeAntigo]));
          end;
        end
        else
          if (PapelDestino <> nil) and (pClasse = GetOriDes(Origem)) then begin
            if PapelOrigem <> nil then S := PapelOrigem.Nome;
            inc(Prop);
            Lin(tab(2) + format('AddMetadata(%d, ''%s'', ''%s'', T%s, ''%s'');', [Prop, MontaAlias(PapelDestino), MontaAjuda(PapelDestino.Ajuda),
                GetOriDes(Destino).Nome , S]));
            MontaConstraintAssociation(PapelDestino);
          end;
      end;
    end;

  begin
    with fAssociacao do
      if Gera then
        if not isUnidirect(fClasse, fAssociacao) then begin
          TrataPapeis(true);
          TrataPapeis(false);
        end;
  end;

begin
  with pClasse do begin
    Ops := 0;
    fMetodo := Metodos.First;
    while Assigned(fMetodo) do with fMetodo do begin
      if (Escopo = aePublished) and not (mcSobreposto in Caracteristicas) then inc(Ops);
      Metodos.Next(fMetodo);
    end;

    inc(Ops, Indices.Count);
    Gera := (Ops <> 0) or (Atributos.Count > 0) or (AssociacoesDestino.Count > 0) or (AssociacoesOrigem.Count > 0);
    Lin('procedure Init' + Nome + '; begin');
    Lin(tab(1) + 'with Prevalence.Metadata(''T' + Nome + ''') do begin');
    Lin(tab(2) + format('InheritMetadata(%d);', [Ops]));

    Prop := 0;
    Lin(tab(2) + format('AddMetadata(0, ''%s'', ''%s'', ''%s'');', [ifthen(Nome <> Apelido, Apelido, ''),
      MontaAjuda(Ajuda), Pacote.Nome]));
    if PropOrder <> '' then Lin(tab(2) + format('AddClassTags(''PropOrder'', ''%s'');', [SplitLine(PropOrder)]));
    if Apresentacao <> caGrade then
      Lin(tab(2) + 'AddClassTags(''ViewFormat'', ''Card'');');
    if Gera then begin
      if NomeAntigo <> '' then Lin(tab(2) + format('AddOldName(0, ''%s'');', [NomeAntigo]));
      fRelacionamentos := CreateViewAssociacao(pClasse);
      fAssociacao := fRelacionamentos.Last;
      while Assigned(fAssociacao) do with fAssociacao do begin
        GeraAssociacao;
        fRelacionamentos.Prior(fAssociacao);
      end;
      fAssociacao := fRelacionamentos.Last;
      while Assigned(fAssociacao) do with fAssociacao do begin
        GeraUnidirect;
        fRelacionamentos.Prior(fAssociacao);
      end;
      fRelacionamentos.Free;

      fAtributo := Atributos.Last;
      while Assigned(fAtributo) do with fAtributo do begin
        if (Escopo = aePublished) and not (acSobreposto in Caracteristicas) then begin
          inc(Prop);
          Constraints := GerarConstraintsAtributos(fAtributo);
          if ((Apelido <> '') and (Apelido <> Nome)) or (Mascara <> '') or (Ajuda <> nil) or (NomeAntigo <> '') or (Constraints <> '') or (Inicial <> '') then
            Lin(tab(2) + format('AddMetadata(%d, ''%s'', ''%s''' + ifthen(Mascara = '', ');', ', ''%s'');'), [Prop, ifthen(Apelido <> Nome, Apelido, ''),
              MontaAjuda(Ajuda), Mascara]));
          if Constraints <> '' then
            Lin(tab(2) + format('AddConstraints(%d, [%s]);', [Prop, Constraints]));

          if NomeAntigo <> '' then
            Lin(tab(2) + format('AddOldName(%d, ''%s'');', [Prop, NomeAntigo]));
        end;
        Atributos.Prior(fAtributo);
      end;

      fAtributo := pClasse.Atributos.First;
      while Assigned(fAtributo) do with fAtributo do begin
        if acSobreposto in Caracteristicas then begin
          Constraints := GerarConstraintsAtributos(fAtributo);
          Lin(tab(2) + format('AddOverride(''%s'', ''%s'', ''%s'', ''%s'', ''%s'', [%s]);',
              [Nome, Apelido, MontaAjuda(Ajuda), Mascara, '', Constraints]));
        end;
        pClasse.Atributos.Next(fAtributo);
      end;

      fIndiceList := TIndiceList(IndiceList.CreateView);
      fIndice := pClasse.Indices.First;
      while Assigned(fIndice) do with fIndice do begin
        fIndiceList.Add(fIndice);
        pClasse.Indices.Next(fIndice);
      end;

      fIndice := fIndiceList.Last;
      Ops := 0;
      while Assigned(fIndice) do with fIndice do begin
        Lin(tab(2) + format('AddMethod(%d, ''%s'', ''%s'', ''%s'', ''%s'', mkFunction, %d, TypeInfo(%s), _stINDEX);',
          [Ops, Nome, Apelido, Retorno.Mascara, MontaAjuda(Ajuda), 0, GetTipo(pClasse, Retorno, true)]));
        inc(Ops);
        fIndiceList.Prior(fIndice);
      end;

      fIndiceList.Free;
      fMetodoList := TMetodoList(MetodoList.CreateView);
      fMetodo := pClasse.Metodos.First;
      while Assigned(fMetodo) do begin
        fMetodoList.Add(fMetodo);
        pClasse.Metodos.Next(fMetodo);
      end;

      fMetodo := TMetodo(fMetodoList.Last);
      while Assigned(fMetodo) do with fMetodo do begin
        if (Escopo = aePublished) then begin
          if not (mcSobreposto in Caracteristicas) then begin
            if Retorno = nil then begin
              if not (mcDeClasse in Caracteristicas) then
                MethodKind := 'mkProcedure'
              else
                MethodKind := 'mkClassProcedure';
              Lin(Tab(2) + format('AddMethod(%d, ''%s'', ''%s'', ''%s'', ''%s'', %s, %d, nil, %s);',
                [Ops, Nome, IfThen((mcOculto in Caracteristicas) and (pos('hidden', lowercase(Apelido)) = 0), 'Hidden, ', '') + Apelido, '',
                MontaAjuda(Ajuda), MethodKind, Parametros.Count, MapeiaMetodoTipo(Tipo)]));
              inc(Ops);
            end
            else begin
              if not (mcDeClasse in Caracteristicas) then
                MethodKind := 'mkFunction'
              else
                MethodKind := 'mkClassFunction';
              Lin(Tab(2) + format('AddMethod(%d, ''%s'', ''%s'', ''%s'', ''%s'', %s, %d, TypeInfo(%s), %s);',
                [Ops, Nome, IfThen((mcOculto in Caracteristicas) and (pos('hidden', lowercase(Apelido)) = 0), 'Hidden, ', '') + Apelido, Retorno.Mascara,
                MontaAjuda(Ajuda), MethodKind, Parametros.Count, GetTipo(pClasse, Retorno), MapeiaMetodoTipo(Tipo)]));
              inc(Ops);
            end;
            fParametro := Parametros.First;
            P := 0;
            while Assigned(fParametro) do with fParametro do begin
              if Passagem = ppEntrada then Semantics := 'pfConst' else
              if Passagem = ppSaida   then Semantics := 'pfOut' else Semantics := 'pfVar';
              if Inicial = '' then
                DefaultParam := trim(AnsiDequotedStr(GetResultDefault(pClasse, TElementoTipado(fParametro)), ''''))
              else
                DefaultParam := AnsiDequotedStr(Inicial, '''');
              Lin(Tab(3) + format('AddParam(%d, ''%s'', ''%s'', ''%s'', ''%s'', %s, TypeInfo(%s));',
                [P, ifthen(Apelido = '', Nome, Apelido), Mascara, MontaAjuda(Ajuda), DefaultParam, Semantics, GetTipo(pClasse, TElementoTipado(fParametro))]));
              inc(P);
              Parametros.Next(fParametro);
            end;
            if Tipo = mtVisaoDeSeguranca then
              Lin(Tab(2) + format('AddView(''%s'');', [Nome]));
          end;
        end;
        fMetodoList.Prior(fMetodo);
      end;
      fMetodoList.Free;
    end;
    Lin(tab(1) + 'end;');
  end;
  Lin('end;' + nl);
end;

procedure TGerador.GerarFonte(Nome : string); begin
  Browser.Message(StringOfChar('-', 150));
  Browser.Message('Gerando Fonte: ' + Nome);
  NomeFonte := Nome;
  Pas   := '';
  Fonte := '';
  PosMethods := 0;
end;

procedure TGerador.LinMembro(S : string); begin
  if Prim then begin
    Lin(Tab + EscopoAtual);
    Prim := false;
  end;
  Lin(S);
end;

procedure TGerador.GerarCabecalhoDeMetodo(pClasse : TClasse; pDeclaracao : boolean; pEscopo : TAtributoEscopo = aePublished);
var
  fMetodo : TMetodo;
  Diretiva : string;
begin
  if pEscopo = aePrivate then exit;
  fMetodoList := TMetodoList(MetodoList.CreateView);
  fMetodo := pClasse.Metodos.First;
  while Assigned(fMetodo) do begin
    fMetodoList.Add(fMetodo);
    pClasse.Metodos.Next(fMetodo);
  end;

  fMetodo := TMetodo(fMetodoList.First);
  while Assigned(fMetodo) do with fMetodo do begin
    if pDeclaracao then begin
      if (Escopo = pEscopo) or ((Escopo = aePrivate) and (pEscopo = aeProtected)) then begin
        if mcVirtual in Caracteristicas then
          Diretiva := ' virtual;'
        else if mcSobreposto in Caracteristicas then
          Diretiva := ' override;'
        else
          Diretiva := '';
        if Retorno = nil then
          if Nome = 'Create' then
            LinMembro(Tab(2) + 'constructor ' + Nome + GerarParametrosDeMetodo(fMetodo) + ';')
          else
            LinMembro(Tab(2) + ifthen((mcDeClasse in Caracteristicas), 'class ', '') + 'procedure ' + Nome + GerarParametrosDeMetodo(fMetodo) + ';' + Diretiva)
        else
          LinMembro(Tab(2) + ifthen((mcDeClasse in Caracteristicas), 'class ', '') + 'function ' + Nome + GerarParametrosDeMetodo(fMetodo) + ' : ' + GetTipo(pClasse, Retorno) + ';' + Diretiva);
      end;
      if (pEscopo = aePublished) and (Escopo = aePublished) then
        LinMembro(Tab(2) + ifthen((mcDeClasse in Caracteristicas), 'class ', '') + ifthen(Retorno = nil, 'procedure ', 'function ') +
          Nome + '_Int(const Params : TMethodParams)' + ifthen(Retorno = nil, ';', ' : Variant;'));
    end
    else
      if Retorno = nil then
        if Nome = 'Create' then
          LinMembro('constructor T' + fMetodo.Classe.Nome + '.' + Nome + GerarParametrosDeMetodo(fMetodo) + ';')
        else
          LinMembro(ifthen((mcDeClasse in fMetodo.Caracteristicas), 'class ', '') + 'procedure T' + fMetodo.Classe.Nome + '.' + Nome + GerarParametrosDeMetodo(fMetodo) + ';' + Diretiva)
      else
        LinMembro(ifthen((mcDeClasse in fMetodo.Caracteristicas), 'class ', '') + 'function T' + fMetodo.Classe.Nome + '.' + Nome + GerarParametrosDeMetodo(fMetodo) + ' : ' + GetTipo(pClasse, Retorno) + ';' + Diretiva);
    fMetodoList.Next(fMetodo);
  end;
  fMetodoList.Free;
end;

procedure TGerador.GerarModelo;

function VerificaNewDeLinks(pClasse : TClasse) : boolean;
var
  fLink : TLink;
  fAssoc : TAssociacao;
begin
  Result := false;
  fLink := pClasse.Links.First;
  while Assigned(fLink) do begin
    fAssoc := fLink.AssociacoesDestino.First;
    while Assigned(fAssoc) do with fAssoc do begin
      if (PapelDestino <> nil) and (GetOriDes(Origem) = pClasse) and (GetMaxPapel(PapelDestino) > 1) then begin
        Result := true;
        exit;
      end;
      fLink.AssociacoesDestino.Next(fAssoc);
    end;
    fAssoc := fLink.AssociacoesOrigem.First;
    while Assigned(fAssoc) do with fAssoc do begin
      if (PapelOrigem <> nil) and (GetOriDes(Destino) = pClasse) and (GetMaxPapel(PapelOrigem) > 1)  then begin
        Result := true;
        exit;
      end;
      fLink.AssociacoesOrigem.Next(fAssoc);
    end;
    pClasse.Links.Next(fLink);
  end;
end;

procedure NeedNewDestroy(pClasse : TClasse; var NeedNew, NeedDestroy, TemIniciais : boolean);
var
  fAtributo : TAtributo;
  fAssociacao : TAssociacao;
begin
  NeedNew := false;
  NeedDestroy := false;
  TemIniciais := false;
  fAtributo := pClasse.Atributos.First;
  while Assigned(fAtributo) do with fAtributo do begin
    if (Inicial <> '') and not (acDerivado in Caracteristicas) then begin
      NeedNew := true;
      TemIniciais := true;
      break;
    end;
    pClasse.Atributos.Next(fAtributo);
  end;
  if not NeedNew then begin
    NeedNew := VerificaNewDeLinks(pClasse);
    if NeedNew then NeedDestroy := true;
  end;
  if not NeedNew then begin
    fAssociacao := pClasse.AssociacoesDestino.First;
    while Assigned(fAssociacao) do with fAssociacao do begin
      if ((PapelDestino <> nil) and (GetOriDes(Origem) = pClasse) and (GetMaxPapel(PapelDestino) > 1)) then begin
        NeedNew := true;
        NeedDestroy := true;
        break;
      end;
      pClasse.AssociacoesDestino.Next(fAssociacao);
    end;
  end;
  if not NeedNew then begin
    fAssociacao := pClasse.AssociacoesOrigem.First;
    while Assigned(fAssociacao) do with fAssociacao do begin
      if ((PapelOrigem <> nil) and (GetOriDes(Destino) = pClasse) and (GetMaxPapel(PapelOrigem) > 1)) then begin
        NeedNew := true;
        NeedDestroy := true;
        break;
      end;
      pClasse.AssociacoesOrigem.Next(fAssociacao);
    end;
  end;
end;

procedure GerarMembros(const pClasse : TClasse; pEscopo : TAtributoEscopo);

  procedure GeraReferencia(pPrivate : boolean; pAssociacao : TAssociacao); begin
    with pAssociacao do begin
      if pPrivate then begin
        if (PapelOrigem <> nil) and (GetMaxPapel(PapelOrigem) = 1) and (GetOriDes(Destino) = pClasse) then
          LinMembro(Tab(2) + '_' + PapelOrigem.Nome + ' : T' + GetOriDes(Origem).Nome + ';');
        if (PapelDestino <> nil) and (GetMaxPapel(PapelDestino) = 1) and (GetOriDes(Origem) = pClasse) then
          LinMembro(Tab(2) + '_' + PapelDestino.Nome + ' : T' + GetOriDes(Destino).Nome + ';');
      end
      else begin
        if (PapelOrigem <> nil) and (GetMaxPapel(PapelOrigem) = 1) and (GetOriDes(Destino) = pClasse) then begin
          LinMembro(Tab(2) + 'function  Get' + PapelOrigem.Nome + ' : T' + GetOriDes(Origem).Nome + ';');
          if GetMaxPapel(PapelOrigem) = 1 then LinMembro(Tab(2) + 'procedure Set' + PapelOrigem.Nome + '(Value : T' + GetOriDes(Origem).Nome + ');');
        end;
        if (PapelDestino <> nil) and (GetMaxPapel(PapelDestino) = 1) and (GetOriDes(Origem) = pClasse) then begin
          LinMembro(Tab(2) + 'function  Get' + PapelDestino.Nome + ' : T' + GetOriDes(Destino).Nome + ';');
          if GetMaxPapel(PapelDestino) = 1 then LinMembro(Tab(2) + 'procedure Set' + PapelDestino.Nome + '(Value : T' + GetOriDes(Destino).Nome + ');');
        end;
      end;
    end;
  end;

  procedure GerarAssociacoes(pPrivate : boolean);
  var
    fAssociacao : TAssociacao;
    fRelacionamentos : TAssociacaoList;

    procedure Gera; begin
      with fAssociacao do begin
        if (PapelOrigem <> nil) and (GetOriDes(Destino) = pClasse) then
          if pPrivate then begin
            if GetMaxPapel(PapelOrigem) = 1 then
              LinMembro(Tab(2) + '_' + PapelOrigem.Nome + ' : T' + GetOriDes(Origem).Nome + ';')
            else
              LinMembro(Tab(2) + '_' + PapelOrigem.Nome + ' : T' + pClasse.Nome + PapelOrigem.Nome + 'Association;')
          end
          else begin
            if GetMaxPapel(PapelOrigem) = 1 then
              LinMembro(Tab(2) + 'property ' + PapelOrigem.Nome + ' : T' + GetOriDes(Origem).Nome +
                        ' read Get' + PapelOrigem.Nome + ' write Set' + PapelOrigem.Nome + ';')
            else
              LinMembro(Tab(2) + 'property ' + PapelOrigem.Nome + ' : T'  + pClasse.Nome + PapelOrigem.Nome + 'Association' +
                      ' read _' + PapelOrigem.Nome + ' write _' + PapelOrigem.Nome + ';');
          end;

        if (PapelDestino <> nil) and (GetOriDes(Origem) = pClasse) then
          if pPrivate then begin
            if GetMaxPapel(PapelDestino) = 1 then
              LinMembro(Tab(2) + '_' + PapelDestino.Nome + ' : T' + GetOriDes(Destino).Nome + ';')
            else
              LinMembro(Tab(2) + '_' + PapelDestino.Nome + ' : T' + pClasse.Nome + PapelDestino.Nome + 'Association;')
          end
          else begin
            if GetMaxPapel(PapelDestino) = 1 then
              LinMembro(Tab(2) + 'property ' + PapelDestino.Nome + ' : T' + GetOriDes(Destino).Nome +
                        ' read Get' + PapelDestino.Nome + ' write Set' + PapelDestino.Nome + ';')
            else
              LinMembro(Tab(2) + 'property ' + PapelDestino.Nome + ' : T' + pClasse.Nome + PapelDestino.Nome + 'Association' +
                      ' read _' + PapelDestino.Nome + ' write _' + PapelDestino.Nome + ';');
          end;
      end;
    end;

  begin
    fRelacionamentos := CreateViewAssociacao(pClasse);
    fAssociacao := fRelacionamentos.First;
    while Assigned(fAssociacao) do begin
      Gera;
      fRelacionamentos.Next(fAssociacao);
    end;
    fRelacionamentos.Free;
  end;

var
  NeedNew, NeedDestroy, TemIniciais : boolean;
  fAtributo : TAtributo;
  fAssociacao : TAssociacao;
  fRelacionamentos : TAssociacaoList;
  Diretiva : String;

  procedure GerarIndexProperties;
  var
    fIndice : TIndice;
  begin
    if pEscopo <> aePublished then exit;
    fIndiceList := TIndiceList(IndiceList.CreateView);
    fIndice := pClasse.Indices.First;
    while Assigned(fIndice) do begin
      fIndiceList.Add(fIndice);
      pClasse.Indices.Next(fIndice);
    end;

    fIndice := fIndiceList.First;
    while Assigned(fIndice) do with fIndice do begin
      LinMembro(Tab(2) + 'function ' + fIndice.Nome + ' : ' + GetTipo(pClasse, fIndice.Retorno, true) + ';');
      fIndiceList.Next(fIndice);
    end;
    fIndiceList.Free;
  end;

  procedure GeraConstraintsAssociacao;
    procedure GeraFunctionConstraints(Papel : TPapel); begin
      with Papel do begin
        if Validacao <> '' then
          LinMembro(Tab(2) + 'function Check' + Nome + '(var Message : String) : Boolean;');
        if Habilitacao <> '' then
          LinMembro(Tab(2) + 'function Enabled' + Nome + ' : Boolean;');
        if Visibilidade <> '' then
          LinMembro(Tab(2) + 'function Visible' + Nome + ' : Boolean;');
        if Constraint <> '' then
          LinMembro(Tab(2) + 'function AssociationConstraint' + Nome + ' : TObjectList;');
      end;
    end;

  begin
    with fAssociacao do begin
      if (PapelDestino <> nil) and (GetOriDes(Origem) = pClasse) then GeraFunctionConstraints(PapelDestino);
      if (PapelOrigem <> nil) and (GetOriDes(Destino) = pClasse) then GeraFunctionConstraints(PapelOrigem);
    end;
  end;

begin
  EscopoAtual := lowerCase(AdjustEnumerationOrSet(GetEnumName(TypeInfo(TAtributoEscopo), integer(pEscopo))));
  Prim := true;
  fAtributo := pClasse.Atributos.First;
  while Assigned(fAtributo) do with fAtributo do begin
    if (pEscopo <> aePublished) and (Escopo = pEscopo) then
      LinMembro(Tab(2) + Nome + ' : ' + GetTipo(pClasse, TElementoTipado(fAtributo)) + ';');
    pClasse.Atributos.Next(fAtributo);
  end;
  if pEscopo = aePrivate then begin
    fAtributo := pClasse.Atributos.First;
    while Assigned(fAtributo) do with fAtributo do begin
      if not (acDerivado in Caracteristicas) and (Escopo = aePublished) and not (acSobreposto in Caracteristicas) then
        LinMembro(Tab(2) + '_' + Nome + ' : ' + GetTipo(pClasse, TElementoTipado(fAtributo)) + ';');
      pClasse.Atributos.Next(fAtributo);
    end;
    GerarAssociacoes(true);

    fAtributo := pClasse.Atributos.First;
    while Assigned(fAtributo) do with fAtributo do begin
      if (Escopo = aePublished) and not (acSobreposto in Caracteristicas) then begin
        LinMembro(Tab(2) + 'function  Get' + Nome + ' : ' + GetTipo(pClasse, TElementoTipado(fAtributo)) + ';');
        if not (acDerivado in Caracteristicas) then
          LinMembro(Tab(2) + 'procedure Set' + Nome + '(Value : ' + GetTipo(pClasse, TElementoTipado(fAtributo)) + ');');
      end;
      pClasse.Atributos.Next(fAtributo);
    end;

    fRelacionamentos := CreateViewAssociacao(pClasse);
    fAssociacao := fRelacionamentos.First;
    while Assigned(fAssociacao) do with fAssociacao do begin
      GeraReferencia(false, fAssociacao);
      fRelacionamentos.Next(fAssociacao);
    end;
    fRelacionamentos.Free;
  end;

  if pEscopo = aeProtected then begin
    NeedNewDestroy(pClasse, NeedNew, NeedDestroy, TemIniciais);
    if NeedNew then LinMembro(Tab(2) + 'procedure New; override;');
    if NeedDestroy then LinMembro(Tab(2) + 'procedure InternalFree; override;');
  end;

  if pEscopo = aePublished then begin
    fAtributo := pClasse.Atributos.First;
    while Assigned(fAtributo) do with fAtributo do begin
      if (Escopo = aePublished) and not (acSobreposto in Caracteristicas) then begin
        if not (acDerivado in Caracteristicas) then
          LinMembro(Tab(2) + 'property ' + Nome + ' : ' + GetTipo(pClasse, TElementoTipado(fAtributo)) + ' read Get' + Nome + ' write Set' + Nome + ';')
        else
          LinMembro(Tab(2) + 'property ' + Nome + ' : ' + GetTipo(pClasse, TElementoTipado(fAtributo)) + ' read Get' + Nome + ';');
      end;
      pClasse.Atributos.Next(fAtributo);
    end;
    if pClasse.Identificacao <> '' then LinMembro(Tab(2) + 'function GetIdentification : string; override;');
    GerarAssociacoes(false);
  end;

  GerarIndexProperties;
  GerarCabecalhoDeMetodo(pClasse, true, pEscopo);

  if pEscopo = aePublished then begin
    fAtributo := pClasse.Atributos.First;
    while Assigned(fAtributo) do with fAtributo do begin
      if Escopo = aePublished then begin
        if acSobreposto in Caracteristicas then
          Diretiva := ' reintroduce;'
        else
          Diretiva := '';
        if Validacao    <> '' then LinMembro(Tab(2) + 'function Check' + Nome + '(var Message : String) : Boolean;' + Diretiva);
        if Habilitacao  <> '' then LinMembro(Tab(2) + 'function Enabled' + Nome + ' : Boolean;' + Diretiva);
        if Visibilidade <> '' then LinMembro(Tab(2) + 'function Visible' + Nome + ' : Boolean;' + Diretiva);
      end;
      pClasse.Atributos.Next(fAtributo);
    end;
    fRelacionamentos := CreateViewAssociacao(pClasse);
    fAssociacao := fRelacionamentos.First;
    while Assigned(fAssociacao) do with fAssociacao do begin
      GeraConstraintsAssociacao;
      fRelacionamentos.Next(fAssociacao);
    end;
    fRelacionamentos.Free;
  end;
end;

procedure GerarClasseLista(pClasse : TClasse; NomeLista, Tipo, TipoChave, Chave : string; Min : integer = -1; Max : integer = -1);
var
  IsAbstract : boolean;
begin
  if (Tipo = 'TAssociation') and (Max < 2) then exit;
  IsAbstract := (pClasse.Tipo = ctAbstrata) and (Tipo = 'TPrevalentList');
  if IsAbstract then begin
    if NomeLista = pClasse.Nome + 'List' then begin
      Lin;
      Lin(Tab + 'T' + NomeLista + ' = class(' + Tipo + ')');
      Lin(Tab(2) + 'class function GetObjectClass : TTransientClass; override;');
      Lin(Tab + 'end;');
    end
  end
  else begin
    Lin;
      Lin(Tab + 'T' + NomeLista + ' = class(' + Tipo + ')');
      if Min > 0 then Lin(Tab(2) + 'class function MinConstraint : integer; override;');
      if (Max > 0) and (Max <> high(word)) then Lin(Tab(2) + 'class function MaxConstraint : integer; override;');
      Lin(Tab(2) + 'class function GetObjectClass : TTransientClass; override;');
      Lin(Tab(2) + 'procedure Add(' + pClasse.Nome + ' : T' + pClasse.Nome + ');');
      Lin(Tab(2) + 'procedure Delete(' + pClasse.Nome + ' : T' + pClasse.Nome + ');');
      Lin(Tab(2) + 'function First : T' + pClasse.Nome + ';');
      Lin(Tab(2) + 'function Last : T' + pClasse.Nome + ';');
      Lin(Tab(2) + 'function Next(var ' + pClasse.Nome + ' : T' + pClasse.Nome + ') : boolean;');
      Lin(Tab(2) + 'function Prior(var ' + pClasse.Nome + ' : T' + pClasse.Nome + ') : boolean;');
      Lin(Tab(2) + 'function Find(' + TipoChave[1] + ' : ' + TipoChave + ') : T' + pClasse.Nome + ';');
      Lin(Tab(2) + 'function Near(' + TipoChave[1] + ' : ' + TipoChave + ') : T' + pClasse.Nome + ';');
      if Chave <> '' then begin
        Lin(Tab + 'protected');
        Lin(Tab(2) + 'class function GetKeyCode : pointer; override;');
        if lowercase(TipoChave) <> 'integer' then
          Lin(Tab(2) + 'class function GetListType : TListType; override;');
      end;
      Lin(Tab + 'end;');
  end;
end;

procedure GerarLista(const pClasse : TClasse);
var
  fClasse : TClasse;
  fIndice : TIndice;
begin
  GerarClasseLista(pClasse, pClasse.Nome + 'List', 'TPrevalentList', 'Integer', '');
  fClasse := pClasse;
  while Assigned(fClasse) do begin
    fIndiceList := TIndiceList(IndiceList.CreateView);
    fIndice := fClasse.Indices.First;
    while Assigned(fIndice) do with fIndice do begin
      fIndiceList.Add(fIndice);
      fClasse.Indices.Next(fIndice);
    end;
    fIndice := fIndiceList.First;
    while Assigned(fIndice) do with fIndice do begin
      GerarClasseLista(pClasse, pClasse.Nome + Nome + 'List', 'TPrevalentList', GetTipo(pClasse, Retorno, true), Nome);
      fIndiceList.Next(fIndice);
    end;
    fIndiceList.Free;
    fClasse := ClassePai(fClasse);
  end;
end;

procedure GerarClasseAssociacao(const pClasse : TClasse);
var
  fAssociacao : TAssociacao;
  fRelacionamentos : TAssociacaoList;

  procedure Gera; begin
    with fAssociacao do begin
      if (PapelOrigem <> nil) and (pClasse = GetOriDes(Destino)) then
        if PapelOrigem.Ordem <> nil then
          GerarClasseLista(GetOriDes(Origem), GetOriDes(Destino).Nome + PapelOrigem.Nome + 'Association', 'TAssociation',
                           GetTipo(pClasse, PapelOrigem.Ordem.Retorno) , PapelOrigem.Ordem.Nome, GetMinPapel(PapelOrigem), GetMaxPapel(PapelOrigem))
        else
          GerarClasseLista(GetOriDes(Origem), GetOriDes(Destino).Nome + PapelOrigem.Nome + 'Association', 'TAssociation',
                           'Integer' , '', GetMinPapel(PapelOrigem), GetMaxPapel(PapelOrigem));
      if (PapelDestino <> nil) and (pClasse = GetOriDes(Origem)) then
        if PapelDestino.Ordem <> nil then
          GerarClasseLista(GetOriDes(Destino), GetOriDes(Origem).Nome + PapelDestino.Nome + 'Association', 'TAssociation',
                           GetTipo(pClasse, PapelDestino.Ordem.Retorno) , PapelDestino.Ordem.Nome, GetMinPapel(PapelDestino), GetMaxPapel(PapelDestino))
        else
          GerarClasseLista(GetOriDes(Destino), GetOriDes(Origem).Nome + PapelDestino.Nome + 'Association', 'TAssociation',
                           'Integer' , '', GetMinPapel(PapelDestino), GetMaxPapel(PapelDestino));
    end;
  end;

begin
  fRelacionamentos := CreateViewAssociacao(pClasse);
  fAssociacao := fRelacionamentos.First;
  while Assigned(fAssociacao) do begin
    Gera;
    fRelacionamentos.Next(fAssociacao);
  end;
  fRelacionamentos.Free;
end;

function GerarHeranca(const pClasse : TClasse) : string; begin
  if pClasse.Pai <> nil then
    Result := pClasse.Pai.Destino.Nome
  else
    Result := 'Prevalent';
end;

function PrefixoEnumeration(Atributo : string) : string;
var
  I : integer;
begin
  Result := Atributo[1];
  for I := 2 to length(Atributo) do
    if Atributo[I] in ['A'..'Z'] then
      Result := Result + Atributo[I];
  Result := lowercase(Result);
end;

procedure AlteraEnumeracao(pAtributo : TAtributo; var E : string);
var
  L : TStringList;
  I, J, P, MaxOrder, Order : integer;
  Prefixo, Str : string;
begin
  L := TStringList.Create;
  try
    Prefixo := PrefixoEnumeration(pAtributo.Classe.Nome + pAtributo.Nome);
    if pos('=', E) = 0 then begin
      if (pos('..', E) = 0) or (pos(',', E) <> 0) then
        E := Prefixo + stringreplace(stringreplace(E, ' ', '', [rfreplaceall]), ',', ', ' + Prefixo, [rfreplaceall]);
      exit;
    end;
    Order := 0; MaxOrder := 0; P := 1;//descobre o valor da maior ordem
    while P <= length(E) do begin
      Str := ExtractFromStr(E, P, ',');
      J := pos('=', Str);
      if J > 0 then begin
        try
          Order := StrToInt(trim(copy(Str, J+1, 100)));
        except
          Browser.Message('Enumeração incorreta. Classe: ' + pAtributo.Classe.Nome + '. Atributo: ' + pAtributo.Nome + '. Enumeração: ' + E);
          exit;
        end;
        if Order > MaxOrder then MaxOrder := Order;
      end;
    end;
    for I := 0 to MaxOrder do L.Add('__' + IntToStr(I));
    //preenche as ordens com valor informado
    P := 1;
    while P <= length(E) do begin
      Str := ExtractFromStr(E, P, ',');
      J := pos('=', Str);
      if J <> 0 then begin
        Order := StrToInt(trim(copy(Str, J+1, 100)));
        L[Order] := trim(copy(Str, 1, J-1));
      end;
    end;
    //preenche as ordens com os valores não informados
    P := 1;
    while P <= length(E) do begin
      Str := ExtractFromStr(E, P, ',');
      J := pos('=', Str);
      if J = 0 then L.Add(trim(copy(Str, 1, 100)));
    end;
    E := '';
    for I := 0 to L.Count-1 do begin
      if L[I] = '__' + IntToStr(I) then
        E := E + 'E' + intToStr(pAtributo.ID) + trim(L[I]) + ', '
      else
        E := E + Prefixo + trim(L[I]) + ', ';
    end;
    E := copy(E, 1, length(E)-2);
  finally
    for I := 0 to L.Count-1 do
      if not IsValidIdent(trim(L[I])) then
        Browser.Message('Enumeração inválida: ' + L[I] + ' no atributo: ' + pAtributo.Nome);
    L.Free;
  end;
end;

procedure GerarEnumeracoes(const pClasse : TClasse);
var
  Tipo : string;
  First : boolean;
  fAtributo : TAtributo;
  Enum : String;
begin
  First := true;
  fAtributo := pClasse.Atributos.First;
  while Assigned(fAtributo) do begin
    if (fAtributo.Tipo = ettEnumeracao) or (fAtributo.Tipo = ettFaixa) then begin
      if First then begin
        Lin(nl + Tab + '// Enumeration(s) for T' + pClasse.Nome);
        First := false;
      end;
      Tipo := 'T' + pClasse.Nome + fAtributo.Nome;
      Enum := fAtributo.Valores;
      AlteraEnumeracao(fAtributo, Enum);
      if pos(',', fAtributo.Valores) = 0 then
        Lin(Tab + Tipo + ' = ' + Enum + ';')
      else
        Lin(Tab + Tipo + ' = (' + Enum + ');');
    end;
    pClasse.Atributos.Next(fAtributo);
  end;
end;

procedure GerarSets(const pClasse : TClasse);
var
  Tipo : string;
  First : boolean;
  fAtributo : TAtributo;
  Sets : string;
begin
  First := true;
  fAtributo := pClasse.Atributos.First;
  while Assigned(fAtributo) do begin
    if fAtributo.Tipo = ettConjunto then begin
      if First then begin
        Lin(nl + Tab + '// Set(s) for T' + pClasse.Nome);
        First := false;
      end;
      Tipo := 'TSet' + pClasse.Nome + fAtributo.Nome;
      Sets := fAtributo.Valores;
      AlteraEnumeracao(fAtributo, Sets);
      if pos(',', fAtributo.Valores) = 0 then
        Lin(Tab + Tipo + ' = set of ' + Sets + ';')
      else begin
        Lin(Tab + 'T' + pClasse.Nome + fAtributo.Nome + ' = (' + Sets + ');');
        Lin(Tab + Tipo + ' = set of T' + pClasse.Nome + fAtributo.Nome + ';');
        Enumeracoes := Sets + 'T' + pClasse.Nome + fAtributo.Nome + ',';
      end;
    end;
    pClasse.Atributos.Next(fAtributo);
  end;
end;

procedure GerarClasse(const pClasse : TClasse);
var
  CC, Ops : integer;
  FP, CCMean : double;
  RiskOperations : string;
begin
  with pClasse do begin
    Lin(GerarDescricaoProximaLinha(Tab, Documentacao));
    if goMetrics in fGeracao.Opcoes then begin
      FP := FunctionPoints(pClasse);
      Lin(Tab + format('// Pontos de Função: %.1f, %s', [FP, Risk(8, 12, FP)]));
      Ops := 0;
      RiskOperations := 'Nenhum';
      CC := CyclomaticComplexity(pClasse, Ops, RiskOperations);
      if CC <> 0 then begin
        if Ops = 0 then Ops := 1;
        CCMean := CC / Ops;
        Lin(Tab + format('// Cyclomatic Complexity: %d, Média: %.1f, %s', [CC, CCMean, Risk(10, 20, CCMean)]));
      end;
      if RiskOperations <> 'Nenhum' then
        Lin(Tab + '// Operações de Alto Risco : ' + RiskOperations);
    end;
    Lin(Tab + 'T' + Nome + ' = class(T' + GerarHeranca(pClasse) + ')'); // Ver se é filho de alguém
    GerarMembros(pClasse, aePrivate);
    GerarMembros(pClasse, aeProtected);
    GerarMembros(pClasse, aePublic);
    GerarMembros(pClasse, aePublished);
    Lin(Tab + 'end;');
  end;
end;

procedure CalculaPropIndex(pClasse : TClasse);
var
  fClasse : TClasse;
  fAtributo : TAtributo;
  fAssociacao : TAssociacao;
  fRelacionamentos : TAssociacaoList;
  Prop : Word;
begin
  fClasse := ClassePai(pClasse);
  pClasse.PropFamily := 0;
  if fClasse <> nil then inc(pClasse.PropFamily, fClasse.PropFamily);
  if pClasse.PropFamily = 0 then
    Prop := 1 //id e permissionview
  else
    Prop := pClasse.PropFamily;
  fAtributo := pClasse.Atributos.First;
  while Assigned(fAtributo) do
    with fAtributo do begin
      if (Escopo = aePublished) and not (acSobreposto in Caracteristicas) then begin
        inc(Prop);
        PropIndex := Prop
      end
      else
        PropIndex := 0;
      pClasse.Atributos.Next(fAtributo);
    end;

  fRelacionamentos := CreateViewAssociacao(pClasse);
  fAssociacao := fRelacionamentos.First;
  while Assigned(fAssociacao) do
    with fAssociacao do begin
      if (PapelOrigem <> nil) and (GetOriDes(Destino) = pClasse) then begin
        inc(Prop);
        if GetMaxPapel(PapelOrigem) = 1 then
          PropIndex := Prop;
      end;
      if (PapelDestino <> nil) and (GetOriDes(Origem) = pClasse) then begin
        inc(Prop);
        if GetMaxPapel(PapelDestino) = 1 then
          PropIndex := Prop;
      end;
      fRelacionamentos.Next(fAssociacao);
    end;
  fRelacionamentos.Free;
  inc(pClasse.PropFamily, Prop - pClasse.PropFamily);
end;

procedure GerarMetodosClasse(const pClasse : TClasse);
var
  NeedNew, NeedDestroy, TemIniciais : boolean;
  fAtributo : TAtributo;
  fAssociacao : TAssociacao;
  fMetodo : TMetodo;
  fIndice : TIndice;
  fRelacionamentos : TAssociacaoList;

  procedure GeraCreateAssociacao; begin
    with fAssociacao do begin
      if (PapelOrigem <> nil) and (GetOriDes(Destino) = pClasse) then
        if GetMaxPapel(PapelOrigem) > 1 then begin
          Lin(Tab + '_' + PapelOrigem.Nome + ' := T' + GetOriDes(Destino).Nome + PapelOrigem.Nome +
              'Association.Create(Self, ''' + PapelOrigem.Nome + '''' +
            ifthen(((Origem.Classe <> nil) and (Origem.Classe.Tipo = ctTransiente)) or ((Destino.Classe <> nil) and (Destino.Classe.Tipo = ctTransiente)), ', true);', ');'));
        end
        else
          if PapelOrigem.Inicial <> '' then
              Lin(Tab + 'try _' + PapelOrigem.Nome + ' := ' + PapelOrigem.Inicial +  ' except end;');
      if (PapelDestino <> nil) and (GetOriDes(Origem) = pClasse) then
        if GetMaxPapel(PapelDestino) > 1 then begin
          Lin(Tab + '_' + PapelDestino.Nome + ' := T' + GetOriDes(Origem).Nome + PapelDestino.Nome +
              'Association.Create(Self, ''' + PapelDestino.Nome + '''' +
          ifthen(((Destino.Classe <> nil) and (Destino.Classe.Tipo = ctTransiente)) or ((Origem.Classe <> nil) and (Origem.Classe.Tipo = ctTransiente)), ', true);', ');'));
        end
        else
          if PapelDestino.Inicial <> '' then
              Lin(Tab + 'try _' + PapelDestino.Nome + ' := ' + PapelDestino.Inicial +  ' except end;');
    end;
  end;

  procedure GeraDestroyAssociacao; begin
    with fAssociacao do begin
      if (PapelOrigem <> nil) and (GetOriDes(Destino) = pClasse) and (GetMaxPapel(PapelOrigem) > 1) then
        Lin(Tab + 'if _' + PapelOrigem.Nome + ' <> nil then _' + PapelOrigem.Nome + '.InternalFree;');
      if (PapelDestino <> nil) and (GetOriDes(Origem) = pClasse) and (GetMaxPapel(PapelDestino) > 1) then
        Lin(Tab + 'if _' + PapelDestino.Nome + ' <> nil then _' + PapelDestino.Nome + '.InternalFree;');
    end;
  end;

  procedure GeraGetSet(Classe, Campo, Tipo : string; Ordem : integer; Referencia : boolean = false); begin
    Lin(nl + 'function T' + Classe + '.Get' + Campo + ' : ' + Tipo + '; begin' + nl + Tab +
       'Result := ' + 'T' + Classe + '(Prevalence.GetNewImage(Self, ' + IntToStr(Ordem) + '))._' + Campo + ';' + nl +
       IfThen(Referencia, Tab + 'FixReference(TTransient(Result), ' + Tipo + ');' + nl, '') +
       'end;');
    Lin(nl + 'procedure T' + Classe + '.Set' + Campo + '(Value : ' + Tipo + '); begin');
    Lin(Tab + 'if Prevalence.IsInRecover then _' + Campo + ' := Value else begin');
    Lin(Tab(2) + 'if (_' + Campo + ' = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;');
    Lin(Tab(2) + 'with Prevalence.GetImages(Self) do');
    Lin(Tab(3) + 'if T' + Classe + '(NewImage)._' + Campo + ' <> Value then begin');
    Lin(Tab(4) + 'T' + Classe + '(NewImage)._' + Campo + ' := Value;');
    Lin(Tab(4) + 'UpdateLog(' + IntToStr(Ordem) + ', NewImage, Stream)');
    Lin(Tab(3) + 'end;');
    Lin(Tab + 'end;');
    Lin('end;');
  end;

  procedure GeraGetSetAssociacao; begin
    with fAssociacao do begin
      if (PapelOrigem <> nil) and ((pClasse <> GetOriDes(Origem)) or ((pClasse = GetOriDes(Destino)) and (pClasse = GetOriDes(Origem)))) then
        if GetMaxPapel(PapelOrigem) = 1 then with PapelOrigem do
          GeraGetSet(pClasse.Nome, Nome, 'T' + GetOriDes(Origem).Nome, PropIndex, true);
      if (PapelDestino <> nil) and ((pClasse <> GetOriDes(Destino)) or ((pClasse = GetOriDes(Destino)) and (pClasse = GetOriDes(Origem)))) then
        if GetMaxPapel(PapelDestino) = 1 then with PapelDestino do
          GeraGetSet(pClasse.Nome, Nome, 'T' + GetOriDes(Destino).Nome, PropIndex, true);
    end;
  end;

  procedure GeraConstraintsAssociacao;

  procedure GeraChecks(Papel : TPapel; Classe : string); begin
    if GetMaxPapel(Papel) = 1 then begin
      if Papel.Validacao <> '' then begin
        Lin(nl + 'function T' + Classe + '.Check' + Papel.Nome + '(var Message : String) : Boolean; begin');
        Lin(Tab + 'Result := ' + Papel.Validacao + ';');
        Lin(Tab + 'if Result then');
        Lin(Tab(2) + 'Message := ''''');
        Lin(Tab + 'else');
        Lin(Tab(2) + 'Message := ''' + Papel.MensagemValidacao + '''');
        Lin('end;');
      end;
      if Papel.Habilitacao <> '' then begin
        Lin(nl + 'function T' + Classe + '.Enabled' + Papel.Nome + ' : Boolean; begin');
        Lin(Tab + 'Result := ' + Papel.Habilitacao + ';');
        Lin('end;');
      end;
      if Papel.Visibilidade <> '' then begin
        Lin(nl + 'function T' + Classe + '.Visible' + Papel.Nome + ' : Boolean; begin');
        Lin(Tab + 'Result := ' + Papel.Visibilidade + ';');
        Lin('end;');
      end;
    end;
    if Papel.Constraint <> '' then begin
      Lin(nl + 'function T' + Classe + '.AssociationConstraint' + Papel.Nome + ' : TObjectList; begin');
      Lin(Tab + 'Result := ' + Papel.Constraint + ';');
      Lin('end;');
    end;
  end;

  begin
    with fAssociacao do begin
      if (PapelDestino <> nil) and (GetOriDes(Origem) = pClasse) then GeraChecks(PapelDestino, GetOriDes(Origem).Nome);
      if (PapelOrigem <> nil) and (GetOriDes(Destino) = pClasse) then GeraChecks(PapelOrigem, GetOriDes(Destino).Nome)
    end;
  end;

  procedure GerarInterfaceMetodo(Metodo : TMetodo);
  var
    Parametro : TParametro;
    ProcFunc, RetFunc, ResFunc, Params : string;
    I : integer;
  begin
    with Metodo do begin
      if Retorno = nil then begin
        ProcFunc := 'procedure';
        RetFunc  := '';
        ResFunc  := '';
      end
      else begin
        ProcFunc := 'function';
        RetFunc  := ' : variant';
        ResFunc  := 'Result := ';
        if not TipoEhPrimitivo(Retorno) then ResFunc := ResFunc + 'PtrInt('
      end;
      if mcDeClasse in Caracteristicas then begin
        ProcFunc := 'class ' + ProcFunc;
        if Tipo in [mtWorkflow, mtWizard] then
          Params := ''
        else
          Params := '(Self';
      end
      else
        Params := '(Self';
      if Parametros.Count <> 0 then begin
        I := 0;
        if Params = '' then Params := '(';
        Parametro := Parametros.First;
        while Parametro <> nil do begin
          if Params <> '(' then Params := Params + ', ';
          case Parametro.Tipo of
            ettClasse, ettLista : Params := Params + 'TVarData(Params[' + IntToStr(I) + ']).VPointer';
            ettHora, ettData, ettDataHora : Params := Params + 'TVarData(Params[' + IntToStr(I) + ']).VDouble';
          else
            Params := Params + 'Params[' + IntToStr(I) + ']';
          end;
          inc(I);
          Parametros.Next(Parametro);
        end;
      end;
      if Params <> '' then Params := Params + ')';
      if (Retorno <> nil) and not TipoEhPrimitivo(Retorno) then Params := Params + ')';
      Lin(ProcFunc + ' T' + Classe.Nome + '.' + Nome + '_Int(const Params : TMethodParams)' + RetFunc + '; begin ' + ResFunc + Classe.Nome + '_' + Nome + Params + '; end;');
    end;
  end;

var
  Chamada : string;
begin
  NeedNewDestroy(pClasse, NeedNew, NeedDestroy, TemIniciais);
  Lin(nl + '{ T' + pClasse.Nome + ' }');
  if NeedNew then begin
    Lin(nl + 'procedure T' + pClasse.Nome + '.New; begin');
    Lin(Tab + 'inherited;');
    fRelacionamentos := CreateViewAssociacao(pClasse);
    fAssociacao := fRelacionamentos.First;
    while Assigned(fAssociacao) do with fAssociacao do begin
      GeraCreateAssociacao;
      fRelacionamentos.Next(fAssociacao);
    end;
    fRelacionamentos.Free;
    if TemIniciais then begin
      Lin(Tab + 'if Prevalence.IsInRecover then exit;');
      fAtributo := pClasse.Atributos.First;
      while Assigned(fAtributo) do with fAtributo do begin
        if not (acDerivado in Caracteristicas) then begin
          if (fAtributo.Tipo = ettFaixa) and (Inicial = '') then
            Lin(Tab + 'try _' + Nome + ' := ' + trim(copy(Valores, 1, pos('..', Valores)-1)) +  ' except end;');
          if Inicial <> '' then
            if Tipo = ettEnumeracao then begin
              if pos(Inicial, Valores) > 0 then
                Lin(Tab + 'try _' + Nome + ' := ' + PrefixoEnumeration(pClasse.Nome + Nome) + Inicial +  ' except end;')
              else
                Lin(Tab + 'try _' + Nome + ' := ' + Inicial +  ' except end;');
            end
            else
              Lin(Tab + 'try _' + Nome + ' := ' + Inicial +  ' except end;');
        end;
        pClasse.Atributos.Next(fAtributo);
      end;
    end;
    Lin('end;');
  end;

  if NeedDestroy then begin
    Lin(nl + 'procedure T' + pClasse.Nome + '.InternalFree; begin');
    fRelacionamentos := CreateViewAssociacao(pClasse);
    fAssociacao := fRelacionamentos.First;
    while Assigned(fAssociacao) do with fAssociacao do begin
      GeraDestroyAssociacao;
      fRelacionamentos.Next(fAssociacao);
    end;
    fRelacionamentos.Free;
    Lin(Tab + 'inherited;');
    Lin('end;');
  end;
  if pClasse.Identificacao <> '' then begin
    Lin(nl + 'function T' + pClasse.Nome + '.GetIdentification : string; begin');
    Lin(Tab + 'try Result := ' + pClasse.Identificacao + ' except Result := '' '' end;');
    Lin('end;');
  end;
  CalculaPropIndex(pClasse);
  fAtributo := pClasse.Atributos.First;
  while Assigned(fAtributo) do with fAtributo do begin
    if (Escopo = aePublished) and not (acSobreposto in Caracteristicas) then
      if not (acDerivado in Caracteristicas) then
        GeraGetSet(pClasse.Nome, Nome, GetTipo(pClasse, TElementoTipado(fAtributo)), PropIndex)
      else begin
        ModelError(Inicial = '', 'Falta valor inicial no atributo ' + Nome + '. ' + LinkError(pClasse.Nome));
        Lin(nl + 'function T' + pClasse.Nome + '.Get' + Nome + ' : ' + GetTipo(pClasse, TElementoTipado(fAtributo)) + '; begin');
        Lin(Tab + 'try Result := ' + Inicial + ' except Result := ' + GetResultDefault(pClasse, TElementoTipado(fAtributo)) + ' end;');
        Lin('end;');
      end;
    pClasse.Atributos.Next(fAtributo);
  end;
  fRelacionamentos := CreateViewAssociacao(pClasse);
  fAssociacao := fRelacionamentos.First;
  while Assigned(fAssociacao) do with fAssociacao do begin
    if (pClasse = GetOriDes(Origem)) or (pClasse = GetOriDes(Destino)) then
      GeraGetSetAssociacao;
    fRelacionamentos.Next(fAssociacao);
  end;
  fRelacionamentos.Free;

  fIndice := pClasse.Indices.First;
  while fIndice <> nil do with fIndice do begin
    Lin(nl + 'function T' + Classe.Nome + '.' + Nome + ' : ' + GetTipo(pClasse, Retorno, true) + '; begin');
    Lin(Tab + 'Result := ' + Expressao);
    Lin('end;');
    pClasse.Indices.Next(fIndice);
  end;

  fMetodo := pClasse.Metodos.First;
  while Assigned(fMetodo) do with fMetodo do begin
    Chamada := '; begin ' + IfThen(Retorno = nil, '', 'Result := ') + Classe.Nome + '_' + Nome + GerarParametrosDeMetodo(fMetodo, false) + ' end;';
    if Retorno = nil then
      if Nome = 'Create' then
        Lin('constructor T' + Classe.Nome + '.' + Nome + GerarParametrosDeMetodo(fMetodo) + Chamada)
      else
        Lin(ifthen((mcDeClasse in Caracteristicas), 'class ', '') + 'procedure T' + Classe.Nome + '.' + Nome + GerarParametrosDeMetodo(fMetodo) + Chamada)
    else
      Lin(ifthen((mcDeClasse in Caracteristicas), 'class ', '') + 'function T' + Classe.Nome + '.' + Nome + GerarParametrosDeMetodo(fMetodo) + ' : ' + GetTipo(pClasse, Retorno) + Chamada);
    if Escopo = aePublished then GerarInterfaceMetodo(fMetodo);
    pClasse.Metodos.Next(fMetodo);
  end;
  fAtributo := pClasse.Atributos.First;
  while Assigned(fAtributo) do with fAtributo do begin
    if (Escopo = aePublished) then begin
      if Validacao <> '' then begin
        Lin(nl + 'function T' + pClasse.Nome + '.Check' + Nome + '(var Message : String) : Boolean; begin');
        Lin(Tab + 'Result := ' + Validacao + ';');
        Lin(Tab + 'if Result then');
        Lin(Tab(2) + 'Message := ''''');
        Lin(Tab + 'else');
        Lin(Tab(2) + 'Message := ''' + MensagemValidacao + '''');
        Lin('end;');
      end;
      if Habilitacao <> '' then begin
        Lin(nl + 'function T' + pClasse.Nome + '.Enabled' + Nome + ' : Boolean; begin');
        Lin(Tab + 'Result := ' + Habilitacao + ';');
        Lin('end;');
      end;
      if Visibilidade <> '' then begin
        Lin(nl + 'function T' + pClasse.Nome + '.Visible' + Nome + ' : Boolean; begin');
        Lin(Tab + 'Result := ' + Visibilidade + ';');
        Lin('end;');
      end;
    end;
    pClasse.Atributos.Next(fAtributo);
  end;
  fRelacionamentos := CreateViewAssociacao(pClasse);
  fAssociacao := fRelacionamentos.First;
  while Assigned(fAssociacao) do with fAssociacao do begin
    GeraConstraintsAssociacao;
    fRelacionamentos.Next(fAssociacao);
  end;
  fRelacionamentos.Free;
end;

procedure GerarMetodo(Nome, NomeClasse, Tipo, Chave : string; Min : integer = -1; Max: integer = -1);
var
  SetDependency : string;
begin
  ModelError(Tipo = '', 'Não foi possível gerar os métodos da Classe ' + LinkError(NomeClasse) + '. Possível problema no modelo.');
  if Tipo = '' then exit;
  if pos('Association', Nome) > 0 then
    SetDependency := 'SetDependencyLists; '
  else
    SetDependency := '';
  Lin(nl + '{ T' + Nome + ' }');
  if Min > 0 then
    Lin('class function T' + Nome + '.MinConstraint : integer; begin Result := ' + IntToStr(Min) + '; end;');
  if (Max > 0) and (Max <> high(word)) then
    Lin('class function T' + Nome + '.MaxConstraint : integer; begin Result := ' + IntToStr(Max) + '; end;');
  Lin('class function T' + Nome + '.GetObjectClass : TTransientClass; begin Result := T' + NomeClasse + '; end;');
  Lin('procedure T' + Nome + '.Add(' + NomeClasse + ' : T' + NomeClasse + '); begin inherited Add(TPrevalent(' + NomeClasse + ')); end;');
  Lin('procedure T' + Nome + '.Delete(' + NomeClasse + ' : T' + NomeClasse + '); begin inherited Delete(TPrevalent(' + NomeClasse + ')); end;');
  Lin('function T' + Nome + '.First : T' + NomeClasse + '; begin ' + SetDependency + 'Result := T' + NomeClasse + '(inherited First); end;');
  Lin('function T' + Nome + '.Last : T' + NomeClasse + '; begin ' + SetDependency + 'Result := T' + NomeClasse + '(inherited Last); end;');
  Lin('function T' + Nome + '.Next(var ' + NomeClasse + ' : T' + NomeClasse + ') : boolean; begin ' + SetDependency +
    'Result := inherited Next(TTransient(' + NomeClasse + ')); end;');
  Lin('function T' + Nome + '.Prior(var ' + NomeClasse + ' : T' + NomeClasse + ') : boolean; begin ' + SetDependency +
    'Result := inherited Prior(TTransient(' + NomeClasse + ')); end;');
  Lin('function T' + Nome + '.Find(' + Tipo[1] + ' : ' + Tipo + ') : T' + NomeClasse + '; begin Result := T' + NomeClasse + '(inherited Find(' + Tipo[1] + ')); end;');
  Lin('function T' + Nome + '.Near(' + Tipo[1] + ' : ' + Tipo + ') : T' + NomeClasse + '; begin ' + SetDependency + 'Result := T' + NomeClasse + '(inherited Near(' + Tipo[1] + ')); end;');
  if Chave <> '' then
    Lin('class function T' + Nome + '.GetKeyCode : pointer; begin Result := @T' + NomeClasse + '.' + Chave + '; end;');
  if lowercase(Tipo) <> 'integer' then
    Lin('class function T' + Nome + '.GetListType : TListType; begin Result := lt' + Tipo + '; end;');
end;

procedure GerarMetodosLista(const pClasse : TClasse);
var
  fClasse : TClasse;
  fIndice : TIndice;
begin
  if pClasse.Tipo <> ctAbstrata then begin
    GerarMetodo(pClasse.Nome + 'List', pClasse.Nome, 'Integer', '');
    fClasse := pClasse;
    while Assigned(fClasse) do begin
      fIndiceList := TIndiceList(IndiceList.CreateView);
      fIndice := fClasse.Indices.First;
      while Assigned(fIndice) do with fIndice do begin
        fIndiceList.Add(fIndice);
        fClasse.Indices.Next(fIndice);
      end;

      fIndice := fIndiceList.First;
      while Assigned(fIndice) do with fIndice do begin
        GerarMetodo(pClasse.Nome + Nome + 'List', pClasse.Nome, GetTipo(pClasse, Retorno, true), Nome);
        fIndiceList.Next(fIndice);
      end;
      fIndiceList.Free;
      fClasse := ClassePai(fClasse);
    end;
  end
  else begin
    Lin(nl + 'class function T' + pClasse.Nome + 'List.GetObjectClass : TTransientClass; begin');
    Lin(Tab + 'Result := T' + pClasse.Nome);
    Lin('end;');
  end;
end;

procedure GerarMetodosAssociacao(pClasse : TClasse);
var
  fAssociacao : TAssociacao;
  fRelacionamentos : TAssociacaoList;

  procedure Gera; begin
    with fAssociacao do begin
      if (PapelOrigem <> nil) and (pClasse = GetOriDes(Origem)) and (GetMaxPapel(PapelOrigem) > 1) then begin
        if PapelOrigem.Ordem <> nil then
          GerarMetodo(GetOriDes(Destino).Nome + PapelOrigem.Nome + 'Association', pClasse.Nome,
            GetTipo(pClasse, PapelOrigem.Ordem.Retorno), PapelOrigem.Ordem.Nome, GetMinPapel(PapelOrigem), GetMaxPapel(PapelOrigem))
        else
          GerarMetodo(GetOriDes(Destino).Nome + PapelOrigem.Nome + 'Association', pClasse.Nome,
            'Integer', '', GetMinPapel(PapelOrigem), GetMaxPapel(PapelOrigem))
      end;
      if (PapelDestino <> nil) and (pClasse = GetOriDes(Destino)) and (GetMaxPapel(PapelDestino) > 1) then begin
        if PapelDestino.Ordem <> nil then
          GerarMetodo(GetOriDes(Origem).Nome + PapelDestino.Nome + 'Association', pClasse.Nome,
            GetTipo(pClasse, PapelDestino.Ordem.Retorno), PapelDestino.Ordem.Nome, GetMinPapel(PapelDestino), GetMaxPapel(PapelDestino))
        else
          GerarMetodo(GetOriDes(Origem).Nome + PapelDestino.Nome + 'Association', pClasse.Nome,
            'Integer', '', GetMinPapel(PapelDestino), GetMaxPapel(PapelDestino))
      end;
    end;
  end;

begin
  fRelacionamentos := CreateViewAssociacao(pClasse);
  fAssociacao := fRelacionamentos.First;
  while Assigned(fAssociacao) do with fAssociacao do begin
    Gera;
    fRelacionamentos.Next(fAssociacao);
  end;
  fRelacionamentos.Free;
end;

procedure GerarVar(const pClasse : TClasse);
var
  fClasse : TClasse;
  fIndice : TIndice;
begin
  if pClasse.Tipo = ctAbstrata then
      Lin(Tab + 'F' + pClasse.Nome + 'List : TPrevalentList;')
    else begin
      Lin(Tab +  'F' + pClasse.Nome + 'List : T' + pClasse.Nome + 'List;');
      fClasse := pClasse;
      while Assigned(fClasse) do begin
        fIndice := fClasse.Indices.First;
        while Assigned(fIndice) do with fIndice do begin
          Lin(Tab + 'F' + pClasse.Nome + Nome + 'List : T' + pClasse.Nome + Nome + 'List;');
          fClasse.Indices.Next(fIndice);
        end;
        fClasse := ClassePai(fClasse);
      end;
    end;
end;

procedure GerarGetLists(const pClasse : TClasse; OnlyDeclarations : boolean);
var
  fClasse : TClasse;
  fIndice : TIndice;
begin
  if pClasse.Tipo = ctAbstrata then
    Lin('function ' + pClasse.Nome + 'List : TPrevalentList;' + ifthen(not OnlyDeclarations,' begin Result := ' + 'F' + pClasse.Nome + 'List' + ' end;'))
  else begin
    Lin('function ' + pClasse.Nome + 'List : T' + pClasse.Nome + 'List;' + ifthen(not OnlyDeclarations,' begin Result := ' + 'F' + pClasse.Nome + 'List' + ' end;'));
    fIndiceList := TIndiceList(IndiceList.CreateView);
    fClasse := pClasse;
    while Assigned(fClasse) do begin
      fIndice := fClasse.Indices.First;
      while Assigned(fIndice) do with fIndice do begin
        fIndiceList.Add(fIndice);
        fClasse.Indices.Next(fIndice);
      end;
      fClasse := ClassePai(fClasse);
    end;

    fIndice := fIndiceList.First;
    while Assigned(fIndice) do with fIndice do begin
      Lin('function ' + pClasse.Nome + Nome + 'List : T' + pClasse.Nome + Nome + 'List;' + ifthen(not OnlyDeclarations,' begin Result := ' + 'F' + pClasse.Nome + Nome + 'List' + ' end;'));
      fIndiceList.Next(fIndice);
    end;
    fIndiceList.Free;
  end;
end;

var
  Visitados : TList;

procedure MoverSubMaquinas(var Operacao : TMetodo; Join : TEstado; var VisitadosLocais : TList); forward;

procedure MoverCorpoSubMaquina(var Operacao, OperacaoNova : TMetodo; Atual : TEstadoBase; var JoinsVisitados, VisitadosLocais : TList);
var
  Prox : TEstadoBase;
  Transicao : TTransicao;
begin
  if (Atual <> nil) and (Visitados.IndexOf(Atual) = -1) then begin
    Visitados.Add(Atual);
    VisitadosLocais.Add(Atual);
    OperacaoNova.Estados.Add(Atual);
    Transicao := Atual.Destinos.First;
    while Transicao <> nil do begin
      Prox := Transicao.Destino;
      if pos('Join', Prox.Nome) = 1 then begin
        if JoinsVisitados.IndexOf(Prox) = -1 then begin
          JoinsVisitados.Add(Prox);
          OperacaoNova.Estados.Add(Prox);
          MoverSubMaquinas(Operacao, TEstado(Prox), VisitadosLocais);
        end
      end
      else
        MoverCorpoSubMaquina(Operacao, OperacaoNova, TEstadoBase(Prox), JoinsVisitados, VisitadosLocais);
      Atual.Destinos.Next(Transicao)
    end;
  end;
end;

procedure MoverSubMaquinas(var Operacao : TMetodo; Join : TEstado; var VisitadosLocais : TList);
var
  Prox : TEstadoBase;
  Transicao : TTransicao;
  CompletaCodigo : boolean;

  procedure CriarJoin(NovoNome : string);
  var
    AcaoJoin : string;
  begin
    with Join do begin
      if pos('try'^M, Acao) = 0 then begin
        AcaoJoin := Acao;
        Acao := 'if not EndPendency(This) then exit'^M'EndTransaction(true)'^M'BeginTransaction'^M'try'^M +
          Tab + 'Pendencies := TPendencyList.Create'^M + Tab + 'try'^M;
        CompletaCodigo := true;
      end;
      with Transicao do
        if Condicao <> '' then begin
          Join.Acao:= Join.Acao + Tab(2) + 'if ' + Condicao + ' then ' + IfThen(Acao = '', '', 'begin ' + Acao + '; ');
          Condicao := '';
          Nome     := '';
        end;
      if CompletaCodigo then begin
        Acao := Acao + Tab(2) + 'Pendencies.New(This, ' + NovoNome + GerarTimeout(Prox) + ')';
        if AcaoJoin <> '' then
          Acao := Acao + ^M + Tab(2) + AcaoJoin;
        if Transicao.Acao = '' then
          Acao := Acao + ^M
        else
          Acao := Acao + '; end'^M;
      end;
    end;
  end;

var
  NovaOperacao : TMetodo;
  JoinsVisitados, VLocais : TList;
  NovoInicial : TEstado;
  ProxTrans   : TTransicao;
begin
  Transicao := Join.Destinos.First;
  CompletaCodigo := false;
  if Transicao = nil then
    ProgramarJoin(Join)
  else begin
    ProxTrans := Transicao;
    while Transicao <> nil do begin
      Prox := Transicao.Destino;
      Join.Destinos.Next(ProxTrans);
      if (Prox is TEstado) and (TEstado(Prox).Tipo = etFinal) then
        Join.Acao := 'Pendencies := nil'^M'if not EndPendency(This) then exit'^M'EndTransaction(true)'^M'Pendencies.Free'^M
      else
        if Visitados.IndexOf(Prox) = -1 then begin
          NovaOperacao := TMetodo.Create;
          with NovaOperacao do begin
            Apelido := Prox.Apelido;
            Nome  := Operacao.Nome + '_' + Prox.Nome;
            if Apelido = '' then Apelido := Prox.Nome;
            Tipo  := mtWorkflow;
            Caracteristicas := [mcOculto];
            CriarJoin(Nome);
          end;
          Operacao.Classe.Metodos.Add(NovaOperacao);
          NovoInicial := TEstado.Create;
          NovoInicial.Tipo := etInicial;
          NovoInicial.Aparencia := TAparencia.Create;
          NovoInicial.Aparencia.X := 20;
          NovoInicial.Aparencia.Y := 20;
          NovoInicial.Aparencia.H := 20;
          NovoInicial.Aparencia.W := 20;
          NovaOperacao.Estados.Add(NovoInicial);
          Transicao.Origem := NovoInicial;
          Transicao.PontoOrigem := 3;
          JoinsVisitados := TList.Create;
          VLocais := TList.Create;
          MoverCorpoSubMaquina(Operacao, NovaOperacao, Prox, JoinsVisitados, VLocais);
          JoinsVisitados.Free;
          VLocais.Free;
        end
        else
          if VisitadosLocais.IndexOf(Prox) = -1 then // Loop de Fork
           if AcharPrimeiroEstado(Operacao) <> Prox then
              CriarJoin(Operacao.Nome + '_' + Prox.Nome)
            else
              CriarJoin(Operacao.Nome);
      Transicao := ProxTrans;
    end;
    with Join do
      if CompletaCodigo and (Acao <> '') then Acao := Acao + Tab(2) + 'EndTransaction(true)'^M + Tab(2) + 'TJoinPendencyList(Pendencies).Broadcast'^M +
        Tab + 'finally'^M + Tab(2) + 'Pendencies.Free'^M + Tab + 'end'^M'except'^M+ Tab + 'Rollback'^M'end';
  end;
end;

procedure CaminharAteAcharJoin(var Operacao : TMetodo; Atual : TEstadoBase; var VisitadosLocais : TList);
var
  Prox : TEstadoBase;
  Transicao : TTransicao;
begin
  if (Atual <> nil) and (Visitados.IndexOf(Atual) = -1) then begin
    Visitados.Add(Atual);
    VisitadosLocais.Add(Atual);
    Transicao := Atual.Destinos.First;
    while Transicao <> nil do begin
      Prox := Transicao.Destino;
      if pos('Join', Prox.Nome) = 1 then
        MoverSubMaquinas(Operacao, TEstado(Prox), VisitadosLocais)
      else
        CaminharAteAcharJoin(Operacao, Prox, VisitadosLocais);
      Atual.Destinos.Next(Transicao)
    end;
  end;
end;

function TemJoin(Metodo : TMetodo) : boolean;
var
  Estado : TEstadoBase;
begin
  TemJoin := false;
  Estado := Metodo.Estados.First;
  while Estado <> nil do begin
    if (Estado is TEstado) and (TEstado(Estado).Tipo in [etForkV, etForkH]) then begin
      TemJoin := true;
      exit;
    end;
    Metodo.Estados.Next(Estado);
  end;
end;

procedure RefatorarWorkflows(var Classe : TClasse);
var
  VisitadosLocais : TList;
  Metodo : TMetodo;
begin
  Metodo := Classe.Metodos.First;
  while Metodo <> nil do begin
    if (Metodo.Caracteristicas <> [mcOculto]) and (Metodo.Tipo = mtWorkflow) and TemJoin(Metodo) then begin
      Visitados := TList.Create;
      VisitadosLocais := TList.Create;
      CaminharAteAcharJoin(Metodo, AcharPrimeiroEstado(Metodo), VisitadosLocais);
      Visitados.Free;
      VisitadosLocais.Free;
    end;
    Classe.Metodos.Next(Metodo)
  end;
end;

var
  fPacote : TPacote;
  fClasse : TClasse;
  fLink   : TLink;
  Pacotes, Inicializacao, Finalizacao : string;
  I, J : integer;
begin
  //Gerando Enumerações
  fClasseList := CreateViewClasse(nil, false);
  fLink := fClasseList.First;
  while Assigned(fLink) do begin
    if fLink is TClasse then
      GerarEnumeracoes(TClasse(fLink));
    fClasseList.Next(fLink);
  end;
  //Gerando Conjuntos
  fLink := fClasseList.First;
  while Assigned(fLink) do begin
    if fLink is TClasse then
      GerarSets(TClasse(fLink));
    fClasseList.Next(fLink);
  end;
  //Gerando Declarações de Classes Listas e Associações
  fLink := fClasseList.First;
  while Assigned(fLink) do begin
    if fLink is TClasse then
      fClasse := TClasse(fLink)
    else
      fClasse := fLink.Classe;
    RefatorarWorkFlows(fClasse);
    GerarLista(fClasse);
    GerarClasse(fClasse);
    GerarClasseAssociacao(fClasse);
    fClasseList.Next(fLink);
  end;
  Lin(nl + '// Get Lists - Declarations');
  fLink := fClasseList.First;
  while Assigned(fLink) do begin
    if fLink is TClasse then
      fClasse := TClasse(fLink)
    else
      fClasse := fLink.Classe;
    GerarGetLists(fClasse, true);
    fClasseList.Next(fLink);
  end;
  Lin('implementation' + nl);
  Pacotes := '';
  Inicializacao := '';
  Finalizacao := '';
  fPacote := fModelo.Pacotes.First;
  while Assigned(fPacote) do begin
    if fPacote.Inicializacao <> '' then Inicializacao := Inicializacao + fPacote.Inicializacao;
    if fPacote.Finalizacao <> '' then Finalizacao := Finalizacao + fPacote.Finalizacao;
    Pacotes := Pacotes + fPacote.Nome;
    if fModelo.Pacotes.Next(fPacote) then begin
      Pacotes := Pacotes + ', ';
      if (Inicializacao <> '') and (fPacote.Inicializacao <> '') then Inicializacao := Inicializacao + ', ';
      if (Finalizacao <> '') and (fPacote.Finalizacao <> '') then Finalizacao := Finalizacao + ', ';
    end;
  end;
  Lin('uses TypInfo, StrUtils, DateUtils, Math, MaskUtils, pitCommon, pitBrowserProxy, pitEncoder, ' + Pacotes + ';');
  //Gerando variáveis globais
  Lin(nl + '// Global Lists');
  Lin('var');
  fLink := fClasseList.First;
  while Assigned(fLink) do begin
    if fLink is TClasse then
      fClasse := TClasse(fLink)
    else
      fClasse := fLink.Classe;
    GerarVar(fClasse);
    fClasseList.Next(fLink);
  end;
  Lin(nl + '// Get Lists');
  fLink := fClasseList.First;
  while Assigned(fLink) do begin
    if fLink is TClasse then
      fClasse := TClasse(fLink)
    else
      fClasse := fLink.Classe;
    GerarGetLists(fClasse, false);
    fClasseList.Next(fLink);
  end;
  //Gerando iniciais de Classes
  Lin(nl + 'procedure InitLists; begin');
  fLink := fClasseList.First;
  while Assigned(fLink) do begin
    if fLink is TClasse then
      fClasse := TClasse(fLink)
    else
      fClasse := fLink.Classe;
    GerarCreate(fClasse);
    fClasseList.Next(fLink);
  end;
  Lin('end;');
  //Gerando metadados de Classes
  Lin(nl + '// Metadata' + nl);
  fLink := fClasseList.First;
  while Assigned(fLink) do begin
    if fLink is TClasse then
      fClasse := TClasse(fLink)
    else
      fClasse := fLink.Classe;
    GerarMetaData(fClasse);
    fClasseList.Next(fLink);
  end;
  //Gerando init de listas
  Lin('procedure Init' + UnitName + '; begin');
  Lin(tab + 'InitLists;');
  fLink := fClasseList.First;
  while Assigned(fLink) do begin
    if fLink is TClasse then
      fClasse := TClasse(fLink)
    else
      fClasse := fLink.Classe;
    Lin(tab + 'Init' + fClasse.Nome + ';');
    fClasseList.Next(fLink);
  end;
  Lin('end;' + nl);
  //Gerando Métodos
  Lin('// Methods');
  fClasse := TClasse(fClasseList.First);;
  while Assigned(fClasse) do begin
    GerarMetodosClasse(fClasse);
    GerarMetodosLista(fClasse);
    GerarMetodosAssociacao(fClasse);
    fClasseList.Next(TLink(fClasse));
  end;
  fClasseList.Free;
  //Gerando Rotinas finais
  if Inicializacao <> '' then begin
    Lin(nl + 'var');
    Lin(Tab + 'QtdInits : integer;');
  end;
  Lin(nl + 'initialization');
  Lin(Tab + 'SetLength(RecoverInits, length(RecoverInits) + 1);');
  Lin(Tab + 'RecoverInits[high(RecoverInits)] := Init' + UnitName + ';');
  if Inicializacao <> '' then begin
    Lin(Tab + 'QtdInits := length(Initializations);');
    if Inicializacao[length(Inicializacao)] <> ';' then Inicializacao := Inicializacao + ';';
    J := 0;
    for I := 1 to length(Inicializacao) do if Inicializacao[I] = ';' then inc(J);
    Lin(Tab + 'SetLength(Initializations, (QtdInits + 1) * ' + intToStr(J) + ');');
    I := J;
    for J:= 0 to I - 1 do begin
      Lin(Tab + 'Initializations[QtdInits + ' + intToStr(J) + '] := @' + copy(trim(Inicializacao), 1, pos(';', Inicializacao)));
      Delete(Inicializacao, 1, pos(';', Inicializacao));
    end;
  end;
  if Finalizacao <> '' then begin
    if Finalizacao[length(Finalizacao)] <> ';' then Finalizacao := Finalizacao + ';';
    J := 0;
    for I := 1 to length(Finalizacao) do if Finalizacao[I] = ';' then inc(J);
    Lin(Tab + 'SetLength(Finalizations, (length(Finalizations) + 1) * ' + intToStr(J) + ');');
    I := J;
    for J:= 0 to I -1 do begin
      Lin(Tab + 'Finalizations[' + intToStr(J) + '] := @' + copy(trim(Finalizacao), 1, pos(';', Finalizacao)));
      Delete(Finalizacao, 1, pos(';', Finalizacao));
    end;
  end;
  Lin('end.');
end;

procedure TGerador.GerarProjeto;
var
  Units, Source : string;
begin
  fModelo := fGeracao.Modelo;
  Modelo  := fGeracao.Modelo.Nome;
  Source  := Modelo + '.dpr';
  Units := 'SysUtils,' + nl + tab + 'Services, pitModel, pitPrevalence, pitUtils';
  GerarFonte(fPath + PathDelim + Source);
  Lin('program ' + Modelo + ';');
  Lin('{');
  Lin('Gerado por pitinnu');
  lin('Versão Designer: ' + GeneratorVersion);
  lin('Versão Prevalence: ' + fServidor.Versao.Numero);
  Lin('Usuário : ' + Browser.UserInfo.UserName);
  Lin('Máquina : ' + Browser.UserInfo.ComputerName);
  //Lin('Em : ' + FormatDateTime('dd/mm/yyyy hh:m:ss', Now));
  Lin('}' + nl);
  Lin('{$AppType CONSOLE} {$SetPEFlags $0020} // Access more than 2GB; add the /3GB switch to the Boot.ini file');
  Lin(nl + 'uses');
  Units := Units + ', ' + Modelo + 'Model';
  if goDebugger in fGeracao.Opcoes then Units := Units + ', pitSupportGDB';
  Lin(Tab + Units + ';');
  Lin(nl + '{$R ' + Modelo + '.res}');

  Lin(nl + 'procedure StartPrevalenceApplication; begin');
  Lin(Tab + 'try');
  Lin(Tab(2) + 'NoService := true;');
  Lin(Tab(2) + 'Prevalence := TPrevalence.Create(''' + Modelo + ''');');
  Lin(Tab(2) + 'Prevalence.Recover;');
  Lin(Tab(2) + 'ObjectServer := TObjectServer.Create(false);');
  Lin(Tab(2) + 'repeat sleep(100) until false;');
  Lin(Tab + 'except');
  Lin(Tab(2) + 'on E : Exception do pitTrace(''Erro: 7, %s'', [E.Message]);');
  Lin(Tab + 'end;');
  Lin('end;');

  Lin(nl + 'begin');
  Lin(Tab + 'ChDir(ExtractFilePath(ParamStr(0)));');
  Lin(Tab + 'DateSeparator := ''' + DateSeparator + '''; ShortDateFormat := ''' + ShortDateFormat + ''';');
  Lin(Tab + 'TimeSeparator := ''' + TimeSeparator + '''; ShortTimeFormat := ''' + ShortTimeFormat + ''';');
  Lin(Tab + 'FileMode := fmShareDenyWrite + fmOpenReadWrite;');
  Lin(Tab + 'try');
  Lin(Tab(2) + 'Service := TService.Create(''' + Modelo + IfThen(fGeracao.Ambiente = gaDesenvolvimento, Browser.UserInfo.UserName, '')+ ''');');
  Lin(Tab + 'except');
  Lin(Tab(2) + 'StartPrevalenceApplication;');
  Lin(Tab(2) + 'halt;');
  Lin(Tab + 'end;');
  Lin(Tab + 'with Service do try');
  Lin(Tab(2) + 'if Install then');
  Lin(Tab(3) + 'writeln(''Serviço instalado'')');
  Lin(Tab(2) + 'else if Uninstall then');
  Lin(Tab(3) + 'writeln(''Serviço desinstalado'')');
  Lin(Tab(2) + 'else if Exists then begin');
  Lin(Tab(3) + 'Prevalence := TPrevalence.Create(''' + Modelo + ''');');
  Lin(Tab(3) + 'ObjectServer := TObjectServer.Create;');
  Lin(Tab(3) + 'Run([ObjectServer], Recover, SnapShot)');
  Lin(Tab(2) + 'end');
  Lin(Tab(2) + 'else');
  Lin(Tab(3) + 'StartPrevalenceApplication;');
  Lin(Tab(2) + 'Free;');
  Lin(Tab + 'except');
  Lin(Tab(2) + 'on E : Exception do ReportEventLog(EventError, 7, E.Message);');
  Lin(Tab + 'end;');
  Lin('end.');
  ClosePas;
end;

constructor TGerador.Create; begin
  TypeCasts := TStringList.Create;
  inherited;
end;

destructor TGerador.Destroy; begin
  inherited;
  TypeCasts.Free
end;

function TGerador.AddTypeCasts(S: string): string;
var
  I, J : integer;
begin
  Result := S;
  with TypeCasts do
    for I := 1 to Count-1 do begin
      J := pos(lowercase(Strings[I]), lowercase(Result));
      while J <> 0 do begin
        if not IsValidIdent(Result[J-1]) and (Result[J+length(Strings[I])] = '.') then begin
          system.insert(')', Result, J+length(Strings[I]));
          system.insert(Strings[0], Result, J);
        end;
        J := posex(lowercase(Strings[I]), lowercase(Result), J+length(Strings[I]))
      end;
    end;
  with MapClass do
    for I := 0 to Count-1 do begin
      Result := AnsiReplaceText(Result, 'T' + Strings[I] + '.', 'T_' + Strings[I] + '.');
      Result := AnsiReplaceText(Result, 'T' + Strings[I] + '(', 'T_' + Strings[I] + '(');
      Result := AnsiReplaceText(Result, 'T_' + Strings[I] + '.Create', 'T' + Strings[I] + '.Create');
    end;
end;

procedure TGerador.GerarDescricaoParametros(pMetodo: TMetodo);
var
  Parametro : TParametro;
begin
  Parametro := pMetodo.Parametros.First;
  while Parametro <> nil do begin
    if Parametro.Documentacao <> '' then Lin('// ' + Parametro.Nome + ': ' + Parametro.Documentacao);
    pMetodo.Parametros.Next(Parametro)
  end;
end;

procedure TGerador.ClosePas;
var
  Arq : file;
  Fonte : string;
begin
  if FileExists(NomeFonte) then begin
    assign(Arq, NomeFonte);
    reset(Arq, 1);
    SetLength(Fonte, FileSize(Arq));
    blockread(Arq, Fonte[1], length(Fonte));
    close(Arq);
  end
  else
    Fonte := '';
  if Pas <> Fonte then begin
    assign(Arq, NomeFonte);
    rewrite(Arq, 1);
    blockwrite(Arq, Pas[1], length(Pas));
    close(Arq);
  end;
end;

var
  Gerador : TGerador = nil;

function SourceToModel(Path, SourcePos : string) : string;
var
  I, J : integer;
begin
  try
    I := pos('.pas:', SourcePos);
    if I <> 0 then begin
      if Gerador = nil then Gerador := TGerador.Create;
      J := LastDelimiter(' ', copy(SourcePos, 1, I));
      with Gerador do begin
        CarregarFonte(Path + copy(SourcePos, J+1, I-J+3));
        J := posex(^M, SourcePos, I);
        if J = 0 then J := length(SourcePos) + 1;
        GetError(StrToInt(copy(SourcePos, I+5, J-I-5)), 0, '');
        Result := fLink
      end;
    end;
  except
    Result := '';
  end;
end;

function ModelToSource(Path, UMLLink : string) : string; begin
  if Gerador = nil then Gerador := TGerador.Create;
  Result := Gerador.GetSourcePos(Path, UMLLink);
end;

end.
