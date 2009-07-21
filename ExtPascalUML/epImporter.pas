{$I epDirectives.inc}

unit epImporter;

interface

uses epCommon;

threadvar
  XMIFile, Modelo : string;
  Packs : integer;
  Error, GlobalError,
  isInternalPackage: boolean;

type
  TQuebraRec = record
    X, Y : integer;
  end;

  TQuebras = array[0..10] of TQuebraRec;

  TCoordenada = record
    Left, Top, Right, Bottom : integer;
  end;

  TPontoOrigemDestino = record
    SX, SY, EX, EY : integer;
  end;

  TTransicaoRec =  record
    Ordem, NumCl, NumOp : integer;
    Q, Aresta : byte;
    Quebras : TQuebras;
    OrdemOriginal,
    ID, Origem, Destino, Alias, Evento, Condicao, Acao, CorpoAcao : string;
    PontoOrigemDestino : TPontoOrigemDestino;
  end;

  TVariaveisRec = record
    Nome, Tipo, Inicial : string;
  end;

  TEstadoRec = record
    Ordem, PrvID, NumCl, NumOp, V : integer;
    ID, Nome, Variaveis_, Acoes, Sequence, Estereotipo, Alias, Documentacao, Botoes, Tipo : string;
    Variaveis : array [0..255] of TVariaveisRec;
    Coordenada : TCoordenada;
  end;

  THerancaRec = record
    ID, Pai, Filho : string;
    Q, Aresta : byte;
    Quebras : TQuebras;
    PontoOrigemDestino : TPontoOrigemDestino;
  end;

  TEstado_ = record
    Estado, T : integer;
    Remover : boolean;
    Transicoes_ : array[0..20] of integer;
  end;

  TOperacao = record
    Nome, Alias, Tipo, Descricao, Estereotipo, Escopo, Estatico, Diretiva, Codigo, Filtro, Teste, Mascara, StmId : string;
    P, E, PrvID : integer;
    EstereoTipoKind : T_Stereotype;
    Parametros : array[0..15] of record
      Nome, Alias, Mascara, Tipo, Descricao, Escopo, Default : string
    end;
    Estados_ : array[0..60] of TEstado_;
  end;

  TPackageRec = record
    Nome, ID : string;
    PrvID : integer;
  end;

  TClasse_ = record
    ID, Nome, OldName, Documentacao, Alias, Estereotipo, Chave, AutoInc, OwnerPackage, Package, PropOrder, ViewFormat, Identification: string;
    Abstract, Transient : boolean;
    A, O, R, U, Overs, Transients, PrvID : integer;
    Coordenada : TCoordenada;
    Atributos : array of record
      Nome, OldName, Tipo, Alias, Mascara, Descricao, Grupo, Estereotipo, Enumeracao, Check, CheckMessage, Enabled, Visible, Escopo, Inicial, Constraints : string;
      Tamanho, Filler, PropIndex : integer;
      Derivado : boolean;
    end;
    Operacoes : array[0..50] of TOperacao;
    Relacionamentos : array[0..40] of record
      Indice : integer;
      Unidirecional,
      Ponta  : boolean;
    end;
    Overrides : array[0..30] of record
      Nome, Alias, Tipo, Enumeracao, Inicial, Mascara, Descricao, Grupo, Estereotipo, Check, CheckMessage, Enabled, Visible : string;
    end;
  end;

  TPonta = record
    Nome, OldName, Alias, Descricao, Min, Max, Agregacao, Navegacao, Classe, Estereotipo, Check, CheckMessage, Enabled, Visible, Constraints, AssociationConstraint, Grupo, Inicial : string;
    PropIndex : integer;
    Coordenada : TCoordenada;
  end;

  TTagsGerais = record
    TagId : integer;
    TagValue, Id : string;
  end;

  TEnumsRec = record
    Nome, Tipo : string;
  end;

  TAssociacoesRec = record
    ID, Package : string;
    Q, Aresta : byte;
    Quebras : TQuebras;
    Pontas : array[false..true] of TPonta;
    PontoOrigemDestino : TPontoOrigemDestino;
  end;

  TComentariosRec = record
    ID, OwnerID, PackageOwner : string;
    Texto : string;
    Coordenada : TCoordenada;
  end;

  TLinkRec = record
    ID, Nome, OwnerPackage : string;
    Coordenada : TCoordenada;
    PrvID : integer;
  end;

type
  TParser = class
    Linha : string;
    XMI   : text;
    Buf   : array[0..4095] of char;
    CaseTool   : variant;
    function NextToken(Tokens : array of string) : integer;
    function GetToken(Fim : string) : string;
    function GetNextToken(Inicio, Fim : string) : string;
    function GetTag : string;
    function ProcuraPackage(ClasseID : string) : string;
    function ProcuraClasse(ClasseID : string) : string;
    procedure ExportPackages(EAPFile : string);
    procedure CarregaDesigner;
    procedure LerXMI(Modelo, Arquivo : string);
    procedure LerClasse;
    procedure CarregaClasse;
    procedure LerLink;
    procedure CarregaLinks;
    procedure CarregaAtributos;
    procedure CarregaMetodos;
    procedure LerAssociacao;
    procedure CarregaAssociacao;
    procedure LerComentario;
    procedure CarregaComentarios;
    procedure LerHeranca;
    procedure CarregaHeranca;
    procedure LerTransicoes;
    procedure CarregaTransicoes;
    procedure LerEstados;
    procedure CarregaEstados;
    procedure LerTagsGerais;
    procedure CarregaTagsGerais;
    procedure CarregaModelo;
    procedure LerPackage;
    procedure CarregaPackage;
    procedure LoadPosition;
    procedure LerDiagramaMaquinaEstados;
    procedure LerPosicaoDeClasses;
    procedure LerPosicaoDeHerancas;
    procedure LerPosicaoDeAssociacoes;
    procedure LerPosicaoDeEstados;
    procedure LerPosicaoDeTransicoes;
    procedure LerPosicaoDeComentarios;
    constructor Create;
  public
    Transicoes : array of TTransicaoRec;
    Estados : array of TEstadoRec;
    Herancas : array of THerancaRec;
    Packages   : array of TPackageRec;
    Classes_ : array of TClasse_;
    Enums : array of TEnumsRec;
    Associacoes : array of TAssociacoesRec;
    Comentarios : array of TComentariosRec;
    Links : array of TLinkRec;
    TagsGerais : array[0..100] of TTagsGerais;
    PkgId, Autor, Documentacao, Criacao, Modificacao, Enumeracoes, Inicializacao, Finalizacao, TagUses, Declarations, UnitName : string;
  end;
  procedure CarregaModelo(EAPFile : string);

  threadvar
    Parser : TParser;
    TagsGeraisQtd, Assoc, Com, T, E, I, C, H, Enum, L : integer;

implementation

uses
  SysUtils, StrUtils, Classes, epUtils, epPrevalence, epThread, ExtPascalUMLModel, TypInfo
  {$IFNDEF FPC}, COMObj, Variants, ActiveX{$ENDIF};

threadvar
  fModelo_Global : TModelo;

procedure ModelError(Condicao : boolean; S : string); begin
  if Condicao then begin
    Browser.Message('ERRO:===> ' + S);
    Error := true;
  end;
end;

function StrToIdent(S : string) : string;
var
  I : integer;
begin
  Result := '';
  for I := 0 to length(S)-1 do
    if S[I] in ['0'..'9', 'a'..'z', 'A'..'Z', '_'] then
      if (I > 0) and (S[I-1] = ' ') then
        Result := Result + UpperCase(S[I])
      else
        Result := Result + S[I]
end;

function GetEstereoTipoKind(Estereotipo : string) : T_Stereotype;
var
  Str : string;
begin
  Str:= lowerCase(Estereotipo);
  if pos('workflow', Str) > 0 then Result := _stWORKFLOW
  else
  if pos('wizard', Str) > 0 then Result := _stWIZARD
  else
  if pos('lifecycle', Str) > 0 then Result := _stLIFECYCLE
  else
  if pos('view', Str) > 0 then Result := _stVIEW
  else
  if Str = '' then Result := _stSTATEMACHINE
  else
    Result := _stINDEX;
end;

function isReservedWord(pWord : string) : boolean;
const
  ReservedWords: array[0..66] of string = ('and', 'array', 'as', 'asm', 'begin','case',
    'class','const','constructor','destructor', 'dispinterface', 'div', 'do','downto',
    'else', 'end', 'except', 'exports', 'file', 'finalization', 'finally', 'for','function',
    'goto', 'high', 'if', 'implementation', 'in','inherited','initialization', 'inline','interface',
    'is','label','library', 'low', 'mod','nil','not','object','of','or','out','packed','procedure',
    'program','property','raise','record','repeat', 'resourcestring', 'set','shl','shr', 'string',
    'then', 'threadvar', 'to','try','type','unit','until','uses','var','while','with','xor');
var
  I : integer;
begin
  Result := false;
  for i := 0 to 66 do
    if pWord = ReservedWords[I] then begin
      Result := true;
      exit;
    end;
end;

function NumPackage(ID : string) : integer;
var
  I : integer;
begin
  Result := -1;
  for I := 0 to Packs-1 do
    if UpperCase(Parser.Packages[I].ID) = UpperCase(ID) then begin
      Result := I;
      exit;
    end;
end;

function NumClasse(ID : string) : integer;
var
  I : integer;
begin
  Result := -1;
  for I := 0 to C-1 do
    if UpperCase(Parser.Classes_[I].ID) = UpperCase(ID) then begin
      Result := I;
      exit;
    end;
end;

function NumLink(ID : string) : integer;
var
  I : integer;
begin
  Result := -1;
  for I := 0 to L-1 do
    if UpperCase(Parser.Links[I].ID) = UpperCase(ID) then begin
      Result := I;
      exit;
    end;
end;

function NumHeranca(ID : string) : integer;
var
  I : integer;
begin
  Result := -1;
  for I := 0 to H-1 do
    if UpperCase(Parser.Herancas[I].ID) = UpperCase(ID) then begin
      Result := I;
      exit;
    end;
end;

function NumAssociacao(ID : string) : integer;
var
  I : integer;
begin
  Result := -1;
  for I := 0 to Assoc-1 do
    if UpperCase(Parser.Associacoes[I].ID) = UpperCase(ID) then begin
      Result := I;
      exit;
    end;
end;

function NumTransicao(ID : string) : integer;
var
  I : integer;
begin
  Result := -1;
  for I := 0 to T-1 do
    if UpperCase(Parser.Transicoes[I].ID) = UpperCase(ID) then begin
      Result := I;
      exit;
    end;
end;

function NumComentario(ID : string) : integer;
var
  I : integer;
begin
  Result := -1;
  for I := 0 to Com-1 do
    if UpperCase(Parser.Comentarios[I].ID) = UpperCase(ID) then begin
      Result := I;
      exit;
    end;
end;

function NumClasseOperacao(C : integer; Op : string) : integer;
var
  I : integer;
begin
  Result := -1;
  if C <> -1 then
    with Parser.Classes_[C] do
      for I := 0 to O-1 do
        if Operacoes[I].Nome = Op then begin
          Result := I;
          exit;
        end;
end;

function NumEstadoTransicao(C, O : integer; ID : string) : integer;
var
  I, J : integer;
begin
  Result := -1;
  if (C <> -1) and (O <> -1) then
    for I := 0 to T-1 do
      if UpperCase(Parser.Transicoes[I].ID) = Uppercase(ID) then with Parser.Classes_[C].Operacoes[O] do begin
        for J := 0 to E-1 do
          if Uppercase(Parser.Estados[Estados_[J].Estado].ID) = Uppercase(Parser.Transicoes[I].Origem) then begin
            Result := J;
            exit;
          end;
        exit;
      end;
end;

function NumEstadoTransicaoDestino(C, O : integer; ID : string) : integer;
var
  I, J : integer;
begin
  Result := -1;
  if (C <> -1) and (O <> -1) then
    for I := 0 to T-1 do
      if UpperCase(Parser.Transicoes[I].ID) = Uppercase(ID) then with Parser.Classes_[C].Operacoes[O] do begin
        for J := 0 to E-1 do
          if Uppercase(Parser.Estados[Estados_[J].Estado].ID) = Uppercase(Parser.Transicoes[I].Destino) then begin
            Result := J;
            exit;
          end;
        exit;
      end;
end;

function NumEstado(ID : string) : integer;
var
  I : integer;
begin
  Result := -1;
  for I := 0 to E-1 do
    if UpperCase(Parser.Estados[I].ID) = UpperCase(ID) then begin
      Result := I;
      exit;
    end;
end;

function AtribuiSequenceEstado(ID : string; Ordem : integer; Sequence : string) : integer;
var
  I : integer;
begin
  Result := -1;
  for I := 0 to E-1 do
    if UpperCase(Parser.Estados[I].ID) = UpperCase(ID) then begin
      Parser.Estados[I].Ordem    := Ordem;
      Parser.Estados[I].Sequence := Sequence;
      Result := I;
      exit;
    end;
end;

function AtribuiOrdemTransicao(ID : string; Ordem : integer) : integer;
var
  I : integer;
begin
  Result := -1;
  for I := 0 to T-1 do
    if UpperCase(Parser.Transicoes[I].ID) = UpperCase(ID) then begin
      if (Parser.Transicoes[I].Evento = 'Default') or (Parser.Transicoes[I].Condicao = '') then Ordem := MAXINT;
      Parser.Transicoes[I].Ordem := Ordem;
      Parser.Transicoes[I].OrdemOriginal := '{' + AnsiReplaceText(copy(ID, 6, 100), '_', '-') + '}';
      Result := I;
      exit;
    end;
end;
var
  Temp1 : TEstado_;
procedure QSortOperacao(var Operacao : TOperacao; L, R: integer);
var
  I, J, PP : integer;
  Key      : integer;
begin
  with Operacao do
    repeat
      I := L;
      J := R;
      PP := (L + R) shr 1;
      repeat
        Key:= Parser.Estados[Estados_[PP].Estado].Ordem;
        while Parser.Estados[Estados_[I].Estado].Ordem < Key do inc(I);
        while Parser.Estados[Estados_[J].Estado].Ordem > Key do dec(J);
        if I <= J then begin
          Temp1      := Estados_[I];
          Estados_[I] := Estados_[J];
          Estados_[J] := Temp1;
          if PP = I then PP := J
          else
          if PP = J then PP := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if L < J then QSortOperacao(Operacao, L, J);
      L := I;
    until I >= R;
end;

procedure QSortEstado(var pEstado : TEstado_; L, R: integer);
var
  I, J, P : integer;
  Temp    : integer;
  Key     : integer;
begin
  with pEstado do
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        Key:= Parser.Transicoes[Transicoes_[P]].Ordem;
        while Parser.Transicoes[Transicoes_[I]].Ordem < Key do inc(I);
        while Parser.Transicoes[Transicoes_[J]].Ordem > Key do dec(J);
        if I <= J then begin
          Temp          := Transicoes_[I];
          Transicoes_[I] := Transicoes_[J];
          Transicoes_[J] := Temp;
          if P = I then P := J
          else
          if P = J then P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if L < J then QSortEstado(pEstado, L, J);
      L := I;
    until I >= R;
end;

function InitialState(pE : TEstado_; pOp : TOperacao) : boolean;
var
  I, J : integer;
begin
  with pOp do
    for I := 0 to T-1 do
      if UpperCase(Parser.Transicoes[I].Destino) = UpperCase(Parser.Estados[pE.Estado].ID) then begin
        for J := 0 to epImporter.E-1 do
          if UpperCase(Parser.Transicoes[I].Origem) = UpperCase(Parser.Estados[J].ID) then begin
            Result := false;
            exit
          end;
        break;
      end;
  Result := true;
end;

procedure ClassificarEstados(Cl, Op : integer);
var
  I : integer;
begin
  with Parser.Classes_[Cl] do
    with Operacoes[Op] do begin
      for I := 0 to E-1 do
        if InitialState(Estados_[I], Operacoes[Op]) then
          Parser.Estados[Estados_[I].Estado].Ordem := 0;
      QSortOperacao(Operacoes[Op], 0, E-1);
      for I := 0 to E-1 do
        if Estados_[I].T > 0 then
          QSortEstado(Estados_[I], 0, Estados_[I].T-1);
    end;
end;

function IndiceTransicao(ID : string) : integer;
var
  J : integer;
begin
  Result := 0;
  for J := 1 to T-1 do
    if UpperCase(ID) = UpperCase(Parser.Transicoes[J].ID) then begin
      Result := J;
      exit;
    end;
end;

constructor TParser.Create; begin
  inherited Create;
  Enumeracoes := '';
  GlobalError := false;
  T := 0; E := 0; C := 0; H := 0; Assoc := 0; Com := 0; Enum := 0; Packs := 0; L := 0; Error := false;
  Inicializacao:=''; Finalizacao:=''; TagUses:=''; Declarations := ''; TagsGeraisQtd := 0;
  SetLength(Classes_, 1);
  SetLength(Classes_[0].Atributos, 1);
  SetLength(Associacoes, 1);
  SetLength(Comentarios, 1);
  SetLength(Estados, 1);
  SetLength(Transicoes, 1);
  SetLength(Herancas, 1);
  SetLength(Enums, 1);
  SetLength(Links, 1);
end;

function TParser.NextToken(Tokens : array of string) : integer;
var
  T, P, MP : integer;
begin
  Result := -1;
  MP := MAXINT;
  if Linha = '' then readln(XMI, Linha);
  while not EOF(XMI) or (Linha <> '') do begin
    for T := 0 to high(Tokens) do begin
      P := pos(Uppercase(Tokens[T]), Uppercase(Linha));
      if (P > 0) and (P < MP) then begin
        MP := P;
        Result := T;
      end;
    end;
    if Result <> -1 then begin
      Linha  := copy(Linha, MP + length(Tokens[Result]), length(Linha));
      exit;
    end;
    readln(XMI, Linha);
  end;
  Linha := ''
end;

function TParser.GetToken(Fim : string) : string;

  function ReplaceAlias(S : string) : string;
  begin
    Result := AnsiReplaceText(S,      '&lt;', '<');
    Result := AnsiReplaceText(Result, '&gt;', '>');
    Result := AnsiReplaceText(Result, '&quot;', '"');
    Result := AnsiReplaceText(Result, '&#xA;', #13#10);
  end;

var
  I : integer;
begin
  Result := '';
  repeat
    I := pos(Uppercase(Fim), Uppercase(Linha));
    if I <> 0 then
      Result := trim(Result + ReplaceAlias(copy(Linha, 1, I-1)))
    else begin
      Result := Result + ReplaceAlias(Linha);
      readln(XMI, Linha);
      Result := TrimRight(Result) + ^M^J;
    end;
  until (I <> 0) or EOF(XMI);
  Linha := trim(copy(Linha, I+1, length(Linha)));
end;

function TParser.GetNextToken(Inicio, Fim : string) : string; begin
  NextToken([Inicio]);
  Result := trim(GetToken(Fim))
end;

function TParser.GetTag : string; begin
  if NextToken(['value="', '/>']) = 0 then
    Result := trim(GetToken('"'))
  else
    Result := ''
end;

procedure TParser.LerTagsGerais; begin
  NextToken(['</UML:Model>']);
  repeat
    TagsGerais[TagsGeraisQtd].TagId := NextToken(['tag="OldName"', 'tag="PropOrder', 'tag="ViewFormat"', 'tag="Initialization"', 'tag="Finalization"',
    'tag="Uses"', 'tag="Identification"', 'tag="Declarations', '</XMI>']);
    if not (TagsGerais[TagsGeraisQtd].TagId in [0..7]) then exit;
    TagsGerais[TagsGeraisQtd].TagValue := GetTag;
    TagsGerais[TagsGeraisQtd].Id := GetNextToken('modelElement="', '"');
    if TagsGerais[TagsGeraisQtd].TagId in [3, 4, 5, 7] then
      TagsGerais[TagsGeraisQtd].Id := 'EAPK' + copy(TagsGerais[TagsGeraisQtd].Id, pos('EAID_', TagsGerais[TagsGeraisQtd].Id) + 4, MAXINT);

    inc(TagsGeraisQtd);
  until false;
end;

procedure TParser.CarregaTagsGerais;
var
  fPacote : TPacote;
  fClasse : TClasse;
  I, Num : integer;
begin
  for I := 0 to TagsGeraisQtd-1 do
    case TagsGerais[I].TagId of
      0 : begin
        Num := NumClasse(TagsGerais[I].Id);
        if  Num <> -1 then begin
          fClasse := ClasseList.Find(Classes_[Num].PrvID);
          fClasse.NomeAntigo := TagsGerais[I].TagValue;
        end;
      end;
      1 : begin
        Num := NumClasse(TagsGerais[I].Id);
        if  Num <> -1 then begin
          fClasse := ClasseList.Find(Classes_[Num].PrvID);
          if fClasse.PropOrder = '' then
            fClasse.PropOrder := TagsGerais[I].TagValue
          else
            fClasse.PropOrder := fClasse.PropOrder + ', ' + TagsGerais[I].TagValue;
        end;
      end;
      2 : begin
        Num := NumClasse(TagsGerais[I].Id);
        if  Num <> -1 then begin
          fClasse := ClasseList.Find(Classes_[Num].PrvID);
          if lowerCase(TagsGerais[I].TagValue) = 'grid' then
            fClasse.Apresentacao := caGrade
          else
            fClasse.Apresentacao := caCartao
        end;
      end;
      3 : begin
        Num := NumPackage(TagsGerais[I].Id);
        if  Num <> -1 then begin
          fPacote := PacoteList.Find(Packages[Num].PrvID);
          if fPacote.Inicializacao = '' then
            fPacote.Inicializacao := TagsGerais[I].TagValue
          else
            fPacote.Inicializacao := fPacote.Inicializacao + '; ' + TagsGerais[I].TagValue;
        end;
      end;
      4 : begin
        Num := NumPackage(TagsGerais[I].Id);
        if  Num <> -1 then begin
          fPacote := PacoteList.Find(Packages[Num].PrvID);
          if fPacote.Finalizacao = '' then
            fPacote.Finalizacao := TagsGerais[I].TagValue
          else
            fPacote.Finalizacao := fPacote.Finalizacao + '; ' + TagsGerais[I].TagValue;
        end;
      end;
      5 : begin
        Num := NumPackage(TagsGerais[I].Id);
        if  Num <> -1 then begin
          fPacote := PacoteList.Find(Packages[Num].PrvID);
          if fPacote.UnidadesExternas = '' then
            fPacote.UnidadesExternas := TagsGerais[I].TagValue
          else
            fPacote.UnidadesExternas := fPacote.UnidadesExternas + ', ' + TagsGerais[I].TagValue;
        end;
      end;
      6 : begin
        Num := NumClasse(TagsGerais[I].Id);
        if  Num <> -1 then begin
          fClasse := ClasseList.Find(Classes_[Num].PrvID);
          fClasse.Identificacao := TagsGerais[I].TagValue;
        end;
      end;
      7 : begin
        Num := NumPackage(TagsGerais[I].Id);
        if  Num <> -1 then begin
          fPacote := PacoteList.Find(Packages[Num].PrvID);
          if fPacote.Declaracao = '' then
            fPacote.Declaracao := TagsGerais[I].TagValue
          else
            fPacote.Declaracao := fPacote.Declaracao + TagsGerais[I].TagValue;
        end;

      end;
  end;
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

var
  PrefixoConstante : integer = 0;

procedure AlteraEnumeracao(Atributo : string; var E : string);
var
  L : TStringList;
  I, J, P, MaxOrder, Order : integer;
  Prefixo, Str : string;
begin
  L := TStringList.Create;
  try
    Prefixo := PrefixoEnumeration(Atributo);
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
          ModelError(true, 'Enumeração incorreta. Classe: ' + Parser.Classes_[C].Nome + '. Atributo: ' + Atributo + '. Enumeração: ' + E);
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
      if L[I] = '__' + IntToStr(I) then begin
        inc(PrefixoConstante);
        E := E + 'E' + intToStr(PrefixoConstante) + trim(L[I]) + ', ';
      end
      else
        E := E + Prefixo + trim(L[I]) + ', ';
    end;
    E := copy(E, 1, length(E)-2);
  finally
    for I := 0 to L.Count-1 do
      ModelError(not IsValidIdent(trim(L[I])), 'Enumeração inválida: ' + L[I] + ' no atributo: ' + Atributo);
    L.Free;
  end;
end;

procedure InserirEnums(pTipo, Enumeracao : string);
var
  I, V : integer;
begin
  with Parser.Enums[Enum] do begin
    Nome := pTipo;
    V := 0;
    for I := 1 to length(Enumeracao) do
      if Enumeracao[I] = ',' then inc(V);
    case V of
      0.. 7 : Tipo := 'Byte';
      8..15 : Tipo := 'Word';
    else
      Tipo := ''
    end;
  end;
  inc(Enum);
  SetLength(Parser.Enums, Enum+1);
end;

procedure TParser.CarregaModelo;
begin
  fModelo_Global := TModelo.Create;
  fModelo_Global.Nome := Modelo;
  fModelo_Global.Add;
end;

procedure TParser.LerPackage;
var
  Ver : string;
begin
  inc(Packs);
  SetLength(Packages, Packs);
  Ver := '0.0';
  repeat
    case NextToken(['name="', 'xmi.id="', '"author"', '"version"', '"documentation"', '</UML:ModelElement.taggedValue>']) of
      0 : begin
        Packages[Packs-1].Nome := GetToken('"');
        if Packages[Packs-1].Nome[1] = '_' then IsInternalPackage := True;
        ModelError(not IsValidIdent(Packages[Packs-1].Nome), 'Nome do package é inválido: ' + Packages[Packs-1].Nome);
      end;
      1 : begin
        PkgId := GetToken('"');
        Packages[Packs-1].ID := PkgId;
        Delete(PkgId, 1, 4);
      end;
      2 : Autor := GetTag;
      3 : Ver   := GetTag;
      4 : Documentacao := GetTag;
    else
      Browser.Message(StringOfChar('-', 150));
      Browser.Message('..Lendo Package: ' + Packages[Packs-1].Nome);
      break;
    end;
  until false;
end;

procedure TParser.CarregaPackage;
var
  fPacote : TPacote;
  I : integer;
begin
  for I := 0 to Packs-1 do begin
    fPacote := TPacote.Create;
    fPacote.Nome := Packages[I].Nome;
    fPacote.Modelo.Add(fModelo_Global);
    fPacote.Add;
    Packages[I].PrvID := fPacote.ID;
  end;
end;

function TirarUnique(S : string) : string;
var
  I : integer;
begin
  I := pos('unique', lowercase(S));
  if I <> 0 then begin
    Result := trim(copy(S, 1, I-1));
    Result := copy(Result, 1, length(Result)-1);
  end
  else
    Result := S
end;

function GetAjudaDica(Documentacao : string) : string; begin
  if pos('|', Documentacao) > 0 then
    Result := copy(Documentacao, 1, pos('|', Documentacao) - 1)
  else
    Result := Documentacao;
end;

function GetAjudaTexto(Documentacao : string) : string; begin
  if pos('|', Documentacao) > 0 then
    Result := copy(Documentacao, pos('|', Documentacao) +1, MAXINT)
  else
    Result := '';
end;

procedure CalculaPontosOrigemDestino(OrigemNormal, DestinoNormal : boolean; Aresta,
  XO, YO, WO, HO, XD, YD, WD, HD, SX, SY, EX, EY, PX1, PY1, PX2, PY2 : integer; var Quebras, PO, PD : byte);
{
OrigemNormal e DestinoNormal => para classe e estado = true/para juncao final e initial = false
xo e yo é o x e y da origem
wo e ho é a largura e altura da origem
----
sx e sy e ex e ey da associacao qdo nao tiver é zero
----
px e py = quebras
}
var
  AX1, AY1, AX2, AY2, PontosOrigem, PontosDestino : integer;
  M1, N1 : double;

  function CalcularArestaXY(var X, Y : integer) : integer;
  var
    BX1, BY1, BX2, BY2 : integer;
  begin
    for Result := 1 to 4 do begin
      case Result of
        1: begin
          BX1 := XD;
          BY1 := YD;
          BX2 := BX1 + WD;
          BY2 := YD;
        end;
        2: begin
          BX1 := XD + WD;
          BY1 := YD;
          BX2 := BX1;
          BY2 := YD + HD;
        end;
        3: begin
          BX1 := XD + WD;
          BY1 := YD + HD;
          BX2 := XD;
          BY2 := YD + HD;
        end;
      else
          BX1 := XD;
          BY1 := YD + HD;
          BX2 := XD;
          BY2 := YD;
      end;
      if Result in [1, 3] then begin
        Y := BY1;
        if (M1 = 1) or (M1 = 0) or
           (((AX1 >= BX1) and (AX1 <= BX2)) or ((AX1 <= BX1) and (AX1 >= BX2))) then
          X := AX1
        else
          X := abs(round((N1 - Y) / M1))
      end
      else begin
        X := BX1;
        if (M1 = 1) or (M1 = 0) or
           (((AY1 >= BY1) and (AY1 <= BY2)) or ((AY1 <= BY1) and (AY1 >= BY2))) then
          Y := AY1
        else
          Y := abs(round((M1 * X) + N1));
      end;
      if ((((X >= AX1) and (X <= AX2)) or ((X <= AX1) and (X >= AX2))) and
          (((Y >= AY1) and (Y <= AY2)) or ((Y <= AY1) and (Y >= AY2)))) and
         ((((X >= BX1) and (X <= BX2)) or ((X <= BX1) and (X >= BX2))) and
          (((Y >= BY1) and (Y <= BY2)) or ((Y <= BY1) and (Y >= BY2)))) then break
    end;
  end;

  procedure CalcularSemQuebra; forward;

  function CalcularPontoPelaAresta(Aresta, Pontos, XO, YO, WO, HO, X, Y : integer) : integer; begin
    case Aresta of
      1 : Result := round((X-XO) * Pontos/WO);
      2 : Result := Pontos + round((Y-YO) * Pontos/HO);
      3 : Result := (3 * Pontos) - round((X-XO) * Pontos/WO);
      4 : Result := (4 * Pontos) - round((Y-YO) * Pontos/HO);
    else
      Result := 0;
    end;
    if (Result >= (Aresta * Pontos)) or (Result < ((Aresta-1) * Pontos)) then
      if Quebras <> 0 then begin
        Quebras := 0;
        CalcularSemQuebra;
      end
      else begin
        if Aresta > 4 then
          Result := 0
        else
          Result := ((Aresta - 1) * Pontos) + (Pontos div 2)
      end;
  end;

  function CalcularAresta(XO, YO, WO, HO, X, Y : integer) : integer; begin
    if (X >= XO) and (X <= (XO + WO)) then
      if Y <= YO then
        Result := 1
      else
        Result := 3
    else
      if X <= XO then
        Result := 4
      else
        Result := 2
  end;

procedure CalcularSemQuebra;
var
  X, Y, AD : integer;
begin
  AX1 := XO + round(WO/2) + SX;
  AY1 := YO + round(HO/2) - SY;
  AX2 := XD + round(WD/2) + EX;
  AY2 := YD + round(HD/2) - EY;
  if AX1 <> AX2 then
    M1 := (AY2-AY1)/(AX2-AX1)
  else
    M1 := 1;
  N1 := AY1 - (M1 * AX1);
  PO := CalcularPontoPelaAresta(Aresta, PontosOrigem, XO, YO, WO, HO, AX1, AY1);
  AD := CalcularArestaXY(X, Y);
  PD := CalcularPontoPelaAresta(AD, PontosDestino, XD, YD, WD, HD, X, Y);
end;

begin
  if OrigemNormal then
    PontosOrigem := 50
  else
    PontosOrigem := 2;
  if DestinoNormal then
    PontosDestino := 50
  else
    PontosDestino := 2;
  if Quebras = 0 then
    CalcularSemQuebra
  else begin
    PO := CalcularPontoPelaAresta(Aresta, PontosOrigem, XO, YO, WO, HO, PX1, PY1);
    PD := CalcularPontoPelaAresta(CalcularAresta(XD, YD, WD, HD, PX2, PY2), PontosDestino, XD, YD, WD, HD, PX2, PY2);
  end;
  if (PD = 0) and (XO = XD) and (YO = YD) then
    if (PO + (PontosOrigem div 3)) div PontosOrigem = (Aresta - 1) then
      PD := PO + (PontosOrigem div 3)
    else
      PD := PO - (PontosOrigem div 3);
  if PO >= (PontosOrigem  * 4) then PO := PO - (PontosOrigem  * 4);
  if PD >= (PontosDestino * 4) then PD := PD - (PontosDestino * 4);
end;

procedure GetElementoTipado(var pElementoTipado : TElementoTipado; Type_ : string);
var
  S : string;
  fPacote : TPacote;
  fLink : TLink;
begin
  pElementoTipado.Mascara := '';
  S := Type_;
  if pos('..', Type_) <> 0 then Type_ := 'faixa';
  with pElementoTipado do
    case CaseOf(lowerCase(Type_), ['byte','smallint','shortint','word','integer','longint','longword','cardinal','int64','boolean','string','char','widechar',
         'double','datetime','tdatetime','date','tdate','time','ttime','currency','extended','comp','single','enumeration','set','faixa', 'ansistring']) of
        0: begin //byte
          Tipo := ettNumero;
          Tamanho := 2;
          Decimais := false;
          Sinalizado := false;
        end;
        1: begin //smallint
          Tipo := ettNumero;
          Tamanho := 4;
          Decimais := false;
          Sinalizado := true;
        end;
        2: begin //shortint
          Tipo := ettNumero;
          Tamanho := 2;
          Decimais := false;
          Sinalizado := true;
        end;
        3: begin //word
          Tipo := ettNumero;
          Tamanho := 4;
          Decimais := false;
          Sinalizado := false;
        end;
        4, 5: begin //integer, longint
          Tipo := ettNumero;
          Tamanho := 9;
          Decimais := false;
          Sinalizado := true;
        end;
        6, 7: begin //longword, cardinal
          Tipo := ettNumero;
          Tamanho := 9;
          Decimais := false;
          Sinalizado := false;
        end;
        8: begin //int64
          Tipo := ettNumero;
          Tamanho := 19;
          Decimais := false;
          Sinalizado := true;
        end;
        9: Tipo := ettLogico; //boolean
        10: Tipo := ettTexto; //string
        11: begin //char
          Tipo := ettTexto;
          Tamanho := 1;
        end;
        12: Tipo := ettTexto; //widechar
        13: begin //double
          Tipo := ettNumero;
          Tamanho := 15;
          Decimais := true;
          Sinalizado := true;
        end;
        14, 15: Tipo := ettDataHora; //datetime, tdatetime
        16, 17: Tipo := ettData; //date, tdate
        18, 19: Tipo := ettHora;//time, ttime
        20 : begin //currency
          Tipo := ettMonetario;
          Tamanho := 19;
          Decimais := true;
          Sinalizado := true;
        end;
        21, 22: begin //currency, extended, comp
          Tipo := ettNumero;
          Tamanho := 19;
          Decimais := true;
          Sinalizado := true;
        end;
        23: begin //single
          Tipo := ettNumero;
          Tamanho := 7;
          Decimais := true;
          Sinalizado := true;
        end;
        24: Tipo := ettEnumeracao; //enumeration
        25: Tipo := ettConjunto; //set
        26: begin //faixa
          Tipo := ettFaixa;
          Valores := S;
        end;
        27: Tipo := ettTexto; //ansistring
        else
          if (length(Type_) > 4) and (pos('list', lowercase(Type_)) = length(Type_) - 3) then begin
            Tipo := ettLista;
            Valores := Type_;
          end
          else begin
            fLink := nil;
            if (UpperCase(Type_[1]) = 'T') then S := copy(Type_, 2, MAXINT) else S := '';
            fPacote := fModelo_Global.Pacotes.First;
            while Assigned(fPacote) do begin //pode ser Classe
              fLink := TLink(fPacote.Classes.Find(Type_));
              if (fLink = nil) and (S <> '') then
                fLink := TLink(fPacote.Classes.Find(S));
              if fLink <> nil then break;
              fModelo_Global.Pacotes.Next(fPacote);
            end;
            if fLink <> nil then begin
              Tipo := ettClasse;
              TipoClasse := TClasse(fLink);
            end
            else begin
              Tipo := ettOutros;
              Valores := Type_;
            end;
          end;
  end;
end;

procedure TParser.LerClasse;
var
  IsAbstract, Pure, S,
  LastAtribEnum, PureEnumeration, KeyValue : string;
  I : integer;
  Override : boolean;
begin
  LastAtribEnum   := '';
  PureEnumeration := '';
  with Classes_[C] do begin
    Nome := GetNextToken('name="', '"');
    ModelError(not IsValidIdent(Nome), 'Nome de classe inválido: ' + Nome);
    Browser.Message(format('...Lendo Classe %d: %s', [C, Nome]));
    ID   := GetNextToken('id="', '"');
    Coordenada.Left := -1;
    Abstract := lowercase(GetNextToken('Abstract="', '"')) = 'true';
    repeat
      case NextToken(['"documentation"', '"stereotype"', '"date_created"', '"date_modified"', '"persistence"', '"alias"', '"package_name"',
                      '</UML:ModelElement.taggedValue>']) of
        0 : Documentacao := GetTag;
        1 : Estereotipo  := GetTag;
        2 : begin //não tem mais
          S := GetTag;
          if (S < Criacao) or (Criacao = '') then Criacao := S;
        end;
        3 : begin //não tem mais
          S := GetTag;
          if (S > Modificacao) or (Modificacao = '') then Modificacao := S;
        end;
        4 : begin
          S := lowercase(GetTag);
          Transient := S = 'transient';
        end;
        5 : Alias     := GetTag;
        6 : begin
          OwnerPackage := Packages[Packs-1].Nome;
          Package := GetTag;
        end;
        7 : break
      end;
    until false;
    // Ler Atributos, Operacoes e Overrides
    A := 0; O := 0; Overs := 0; Override := false;
    repeat
      case NextToken(['<UML:Attribute ', '<UML:Operation ', '</UML:Class>']) of
        0 : with Atributos[A] do begin
          Nome   := GetNextToken('name="', '"');
          ModelError(not IsValidIdent(Nome), 'Nome de atributo inválido: ' + Nome);
          ModelError(IsReservedWord(lowerCase(Nome)), 'Nome de atributo inválido: ' + Nome + '. É uma palavra reservada.');
          repeat
            case NextToken(['"type"', '"description"', '"stereotype"', '"Enumeration', 'tag="Check"', 'tag="CheckMessage"', '"scope"', '<UML:Expression body="',
                            '"Size"', '"ImportFiller', '"style"', '"Mask"', '"OldName"', '"Group"', '"derived"', 'visibility="', 'tag="Enabled"', 'tag="Visible"', '</UML:Attribute>']) of
              0 : Tipo := GetTag;
              1 : Descricao := GetTag;
              2 : begin
                Estereotipo := StringReplace(lowercase(GetTag), ' ', '', [rfReplaceAll]);
                if Estereotipo = 'autoinc' then begin
                  ModelError(Tipo <> 'Integer', 'Atributo AutoInc deve ser inteiro. Classe: ' + Classes_[C].Nome + ', Atributo: ' + Nome);
                  AutoInc := Nome;
                end;
                I := pos('override', Estereotipo);
                if I <> 0 then begin
                  Override := true;
                  I := pos('override,', Estereotipo);
                  if I <> 0 then
                    delete(Estereotipo, I, length('override,'));
                  I := pos(',override', Estereotipo);
                  if I <> 0 then
                    delete(Estereotipo, I, length(',override'));
                  I := pos('override', Estereotipo);
                  if I <> 0 then
                    delete(Estereotipo, I, length('override'));
                end;
              end;
              3 : begin
                if Classes_[C].Nome + Nome = LastAtribEnum then
                  PureEnumeration := PureEnumeration + ', ' + GetTag
                else begin
                  PureEnumeration := GetTag;
                  LastAtribEnum := Classes_[C].Nome + Nome;
                end;
                Enumeracao := PureEnumeration;
//05                AlteraEnumeracao(Classes_[C].Nome + Nome, Enumeracao);
                if Tipo <> 'Set' then Tipo := 'T' + Classes_[C].Nome + Nome;
                InserirEnums(ifthen(Tipo = 'Set',  'TSet' + Classes_[C].Nome + Nome, Tipo), Enumeracao);
              end;
              4 : Check := GetTag;
              5 : CheckMessage := GetTag;
              6 : begin
                Escopo := lowercase(GetTag);
                ModelError(not IsValidIdent(Escopo), 'Nome de escopo inválido: ' + Escopo + '. Classe: ' + Classes_[C].Nome + ', Atributo: ' + Nome);
              end;
              7 : Inicial := GetToken('"');
              8 : //não tem mais
                try
                  Tamanho := StrToInt(GetTag);
                except
                  ModelError(true, 'Tag Size incorreta. Classe: ' + Classes_[C].Nome + '. Atributo: ' + Atributos[A].Nome)
                end;
              9 : //não tem mais
                try
                  Filler := StrToInt(GetTag);
                except
                  ModelError(true, 'Tag Filler incorreta. Classe: ' + Classes_[C].Nome + '. Atributo: ' + Atributos[A].Nome)
                end;
              10: Alias   := GetTag;
              11: Mascara := GetTag;
              12: OldName := GetTag;
              13: Grupo   := GetTag;// não tem mais
              14: Derivado:= GetTag = '1';
              15: begin //não tem mais
                Escopo := GetToken('"');
                ModelError(not IsValidIdent(Escopo), 'Nome de escopo inválido: ' + Escopo + '. Classe: ' + Classes_[C].Nome + ', Atributo: ' + Nome);
              end;
              16: begin
                Enabled := GetTag;
                if Enabled <> '' then begin
                  if pos('enable', lowerCase(Estereotipo)) = 0 then
                    if Estereotipo = '' then
                      Estereotipo := 'ENABLE'
                    else
                      Estereotipo := Estereotipo + ', ENABLE';
                end;
              end;
              17: begin
                Visible := GetTag;
                if Visible <> '' then begin
                  if pos('visible', lowerCase(Estereotipo)) = 0 then
                    if Estereotipo = '' then
                      Estereotipo := 'VISIBLE'
                    else
                      Estereotipo := Estereotipo + ', VISIBLE';
                end;
              end;
              18: break;
            end;
          until false;
          if Override then begin
            Overrides[Overs].Nome         := Nome;
            Overrides[Overs].Alias        := Alias;
            Overrides[Overs].Mascara      := Mascara;
            Overrides[Overs].Descricao    := Descricao;
            Overrides[Overs].Grupo        := Grupo;
            Overrides[Overs].Estereotipo  := Estereotipo;
            Overrides[Overs].Check        := Check;
            Overrides[Overs].CheckMessage := CheckMessage;
            Overrides[Overs].Enabled      := Enabled;
            Overrides[Overs].Visible      := Visible;
            Overrides[Overs].Inicial      := Inicial;
            Overrides[Overs].Tipo         := Tipo;
            Overrides[Overs].Enumeracao   := Enumeracao;

            inc(Overs);
            Override := false;
            Alias        := '';
            Mascara      := '';
            Descricao    := '';
            Grupo        := '';
            Estereotipo  := '';
            Check        := '';
            CheckMessage := '';
            Enabled      := '';
            Visible      := '';
            Inicial      := '';
            Tipo         := '';
            Enumeracao   := '';
          end
          else begin
            inc(A);
            SetLength(Atributos, A+1);
          end;
        end;
        1 : with Operacoes[O] do begin
          KeyValue := '';
          Nome   := GetNextToken('name="', '"');
          ModelError(not IsValidIdent(Nome), 'Nome de método inválido: ' + Nome);
          Escopo := GetNextToken('visibility="', '"');
          ModelError(not IsValidIdent(Escopo), 'Visibilidade inválida: ' + Escopo + ' para método: ' + Nome);
          P := 0; E := 0; Pure := ''; IsAbstract := '';
          repeat
            case NextToken(['"type"', '"documentation"', '"stereotype"', '"scope"', '"isAbstract"', 'tag="pure"', 'tag="Key"', '"static"',
                            '"behaviour"', '"Filter"', '"Test"', '"style"', '"Mask"', '<UML:Parameter', '</UML:Operation>']) of
              0 : Tipo        := GetTag;
              1 : Descricao   := GetTag;
              2 : Estereotipo := GetTag;
              3 : Escopo      := GetTag;
              4 : IsAbstract  := GetTag;
              5 : Pure        := GetTag;
              6 : KeyValue    := GetTag;
              7 : Estatico    := GetTag;
              8 : Codigo      := GetTag;
              9 : Filtro      := GetTag;
              10: Teste       := GetNextToken('name="', '"/>');
              11: Alias       := GetTag;
              12: Mascara     := GetTag;
              13: with Parametros[P] do begin
                repeat
                  case NextToken(['name="', 'kind="', '"type"', '"note"', '"Alias"', '"Mask"', '<UML:Expression body="', '</UML:Parameter>']) of
                    0 : Nome      := GetToken('"');
                    1 : Escopo    := GetToken('"');
                    2 : Tipo      := GetTag;
                    3 : Descricao := GetTag;
                    4 : Alias     := GetTag;
                    5 : Mascara   := GetTag;
                    6 : Default   := GetToken('"');
                    7 : break;
                  end;
                until false;
                if Nome <> '' then inc(P)
              end;
              14 : break;
            end;
          until false;
          Estatico := IfThen(Estatico = '1', 'class ', '');
          if KeyValue <> '' then Estereotipo := KeyValue;
          if Pure = '1' then
            Diretiva := 'override; '
          else
          if IsAbstract = '1' then
            Diretiva := 'virtual; ';
          if lowerCase(Estereotipo) = 'pure' then begin
            Diretiva := 'override; ';
            Estereotipo := '';
          end;
          EstereoTipoKind := GetEstereoTipoKind(Estereotipo);
          if EstereoTipoKind <> _stINDEX then Estereotipo := '';

          inc(O)
        end;
        2 : break;
      end
    until false;
    Transients := 0;
    for I := 0 to A-1 do
      if Atributos[I].Escopo <> 'published' then inc(Transients);
  end;
  inc(C);
  SetLength(Classes_, C+1);
  SetLength(Classes_[C].Atributos, 1);
end;

procedure TParser.CarregaClasse;
var
  fClasse : TClasse;
  fAjuda : TAjuda;
  fAparencia : TAparencia; // cor $00DEF7FE
  Cl : integer;
begin
  for Cl := 0 to C-1 do with Classes_[Cl] do begin
    if Coordenada.Left = -1 then begin
      Browser.Message('Classe perdida no modelo: ' + Nome + '. Não Carregada.');
      continue;
    end;
    if OwnerPackage = Package then begin
      fClasse := TClasse.Create;
      fClasse.Identificacao := Identification;
      fClasse.Nome := Nome;
      if Documentacao <> '' then begin
        fAjuda := TAjuda.Create;
        fAjuda.Dica := GetAjudaDica(Documentacao);
        fAjuda.Texto := GetAjudaTexto(Documentacao);
        fAjuda.Add;
        fClasse.Ajuda := fAjuda;
      end;
      if Abstract then fClasse.Tipo := ctAbstrata;
      if Transient then fClasse.Tipo := ctTransiente;
      fClasse.PropOrder := PropOrder;
      if pos('hidden', trim(lowerCase(Alias))) = 1 then begin
        fClasse.Oculto := true;
        fClasse.Apelido :=  trim(copy(Alias, pos(',', Alias) +1 , MAXINT));
      end
      else begin
        fClasse.Oculto := false;
        if Nome <> Alias then fClasse.Apelido := trim(Alias);
      end;
      fAparencia := TAparencia.Create;
      fAparencia.X := Coordenada.Left;
      fAparencia.Y := Coordenada.Top;
      fAparencia.H := (Coordenada.Bottom - Coordenada.Top);
      fAparencia.W := (Coordenada.Right - Coordenada.Left);
      fAparencia.Add;
      fClasse.Aparencia := fAparencia;
      fClasse.Pacote := fModelo_Global.Pacotes.Find(Package);
      fClasse.Add;
      PrvID := fClasse.ID;
    end;
  end;
end;

procedure TParser.LerLink; begin
  with Links[L] do begin
    OwnerPackage := Packages[Packs-1].Nome;
    ID := UpperCase(GetNextToken('id="', '"')) + OwnerPackage;
    Nome := GetNextToken('name="', '"');
  end;
  inc(L);
  SetLength(Links, L+1);
end;

procedure TParser.CarregaLinks;
var
  Link : TLink;
  I, C : integer;
begin
  for I := 0 to L-1 do with Links[I] do
    if Coordenada.Right = 0 then
      Browser.Message('Link perdido no modelo: ' + Nome + '. Não Carregado.')
    else begin
      Link := TLink.Create;
      Link.Pacote := fModelo_Global.Pacotes.Find(OwnerPackage);
      C := NumClasse(copy(ID, 1, length(ID)-length(OwnerPackage)));
      if C <> -1 then
        Link.Classe := ClasseList.Find(Classes_[C].PrvID);
      Link.Nome := Link.Classe.Pacote.Nome + '_' + Link.Classe.GetIdentification;
      Link.Aparencia := TAparencia.Create;
      with Link.Aparencia do begin
        X := Coordenada.Left;
        Y := Coordenada.Top;
        H := Coordenada.Bottom - Coordenada.Top;
        W := Coordenada.Right - Coordenada.Left;
        Add;
      end;
      Link.Add;
      PrvID := Link.ID;
    end;
end;

procedure TParser.CarregaAtributos;
var
  fPacote : TPacote;
  fAjuda : TAjuda;
  fAtributo : TAtributo;
  Cl, At, Ov : integer;
begin
  for Cl := 0 to C-1 do with Classes_[Cl] do begin
    for At := 0 to A-1 do with Atributos[At] do begin
      fAtributo := TAtributo.Create;
      fAtributo.Nome := Nome;
      fAtributo.Ordem := At;
      if Enumeracao <> '' then
        if lowerCase(Tipo) <> 'set' then Tipo := 'enumeration';
      GetElementoTipado(TElementoTipado(fAtributo), Tipo{, 'Tipo de Atributo <' + Tipo + '> inválido. Classe: ' + Classes_[Cl].Nome + ', Atributo: ' + Nome});
      if fAtributo.Tipo in [ettEnumeracao, ettConjunto] then fAtributo.Valores := Enumeracao;
      if Descricao <> '' then begin
        fAjuda := TAjuda.Create;
        fAjuda.Dica := GetAjudaDica(Descricao);
        fAjuda.Texto := GetAjudaTexto(Descricao);
        fAjuda.Add;
        fAtributo.Ajuda := fAjuda;
      end;
      if Derivado then fAtributo.Caracteristicas := fAtributo.Caracteristicas + [acDerivado];
      if pos('hidden', trim(lowerCase(Alias))) = 1 then fAtributo.Caracteristicas := fAtributo.Caracteristicas + [acOculto];
      if pos('override', lowerCase(Estereotipo)) > 0 then fAtributo.Caracteristicas := fAtributo.Caracteristicas + [acSobreposto];
      if pos('readonly', lowerCase(Estereotipo)) > 0 then fAtributo.Caracteristicas := fAtributo.Caracteristicas + [acSomenteLeitura];
      if pos('notnull', lowerCase(Estereotipo)) > 0 then fAtributo.Caracteristicas := fAtributo.Caracteristicas + [acNaoNulo];
      fAtributo.Validacao := Check;
      fAtributo.MensagemValidacao := CheckMessage;
      if lowerCase(Escopo) = 'private' then fAtributo.Escopo := aePrivate else
      if lowerCase(Escopo) = 'protected' then fAtributo.Escopo := aeProtected else
      if lowerCase(Escopo) = 'public' then fAtributo.Escopo := aePublic;
      fAtributo.Inicial := Inicial;
      if pos('hidden', trim(lowerCase(Alias))) = 1 then
        fAtributo.Apelido := trim(copy(Alias, pos(',', Alias)+1 , MAXINT))
      else
        fAtributo.Apelido :=  trim(Alias);
      fAtributo.Mascara := Mascara;
      fAtributo.NomeAntigo := OldName;
      if pos('enable', lowerCase(Estereotipo)) > 0 then fAtributo.Habilitacao := Enabled;
      if pos('visible', lowerCase(Estereotipo)) > 0 then fAtributo.Visibilidade := Visible;
      fPacote := fModelo_Global.Pacotes.Find(Classes_[Cl].Package);
      fAtributo.Classe := TClasse(fPacote.Classes.Find(Classes_[Cl].Nome));
    end;
    for Ov := 0 to Overs-1 do with Overrides[Ov] do begin
      fAtributo := TAtributo.Create;
      fAtributo.Nome := Nome;
      fAtributo.Ordem := Ov + A;
      if Enumeracao <> '' then
        if lowerCase(Tipo) <> 'set' then Tipo := 'enumeration';
      GetElementoTipado(TElementoTipado(fAtributo), Tipo);
      if fAtributo.Tipo in [ettEnumeracao, ettConjunto] then fAtributo.Valores := Enumeracao;
      if Descricao <> '' then begin
        fAjuda := TAjuda.Create;
        fAjuda.Dica := GetAjudaDica(Descricao);
        fAjuda.Texto := GetAjudaTexto(Descricao);
        fAjuda.Add;
        fAtributo.Ajuda := fAjuda;
      end;
      if pos('hidden', trim(lowerCase(Alias))) = 1 then fAtributo.Caracteristicas := fAtributo.Caracteristicas + [acOculto];
      if pos('readonly', lowerCase(Estereotipo)) > 0 then fAtributo.Caracteristicas := fAtributo.Caracteristicas + [acSomenteLeitura];
      if pos('notnull', lowerCase(Estereotipo)) > 0 then fAtributo.Caracteristicas := fAtributo.Caracteristicas + [acNaoNulo];
      fAtributo.Caracteristicas := fAtributo.Caracteristicas + [acSobreposto];
      fAtributo.Validacao := Check;
      fAtributo.MensagemValidacao := CheckMessage;
      fAtributo.Escopo := aePublished;
      fAtributo.Inicial := Inicial;
      fAtributo.Apelido := trim(copy(Alias, pos(',', Alias) , MAXINT));
      fAtributo.Mascara := Mascara;
      fAtributo.Habilitacao := Enabled;
      fAtributo.Visibilidade := Visible;
      fPacote := fModelo_Global.Pacotes.Find(Classes_[Cl].Package);
      fAtributo.Classe := TClasse(fPacote.Classes.Find(Classes_[Cl].Nome));
    end;
  end;
end;

procedure TParser.CarregaMetodos;
var
  fPacote : TPacote;
  fAjuda : TAjuda;
  fElementoTipado : TElementoTipado;
  fParametro : TParametro;
  fMetodo : TMetodo;
  fIndice : TIndice;
  Cl, Op, Pa : integer;
begin
  fMetodo := nil;
  for Cl := 0 to C-1 do with Classes_[Cl] do
    for Op := 0 to O-1 do with Operacoes[Op] do begin
      if EstereoTipoKind = _stINDEX then begin
        fIndice := TIndice.Create;
        fIndice.Nome := Nome;
        if pos('hidden', trim(lowerCase(Alias))) = 1 then
          fIndice.Apelido := trim(copy(Alias, pos(',', Alias)+1 , MAXINT))
        else
          fIndice.Apelido :=  trim(Alias);
        if Descricao <> '' then begin
          fAjuda := TAjuda.Create;
          fAjuda.Dica := GetAjudaDica(Descricao);
          fAjuda.Texto := GetAjudaTexto(Descricao);
          fAjuda.Add;
          fIndice.Ajuda := fAjuda;
        end;
        fIndice.Expressao := TirarUnique(Estereotipo);
        fIndice.Unico := pos('unique', lowercase(Estereotipo)) > 0;

        if (Tipo <> '') and (lowerCase(Tipo) <> 'void') then begin
          fElementoTipado := TElementoTipado.Create;
          GetElementoTipado(fElementoTipado, Tipo);
          //gatilho para o cba
          if (fElementoTipado.Tipo = ettOutros) and (upperCase(fElementoTipado.Valores[1]) <> 'T') then
            fElementoTipado.Valores := 'T' + fElementoTipado.Valores;
          fElementoTipado.Add;
          fIndice.Retorno := fElementoTipado;
        end;
        fIndice.Add;
        fPacote := fModelo_Global.Pacotes.Find(Classes_[Cl].Package);
        fIndice.Classe := TClasse(fPacote.Classes.Find(Classes_[Cl].Nome));
      end
      else begin //Método
        fMetodo := TMetodo.Create;
        fMetodo.Nome := Nome;
        if pos('hidden', trim(lowerCase(Alias))) = 1 then
          fMetodo.Apelido := trim(copy(Alias, pos(',', Alias)+1 , MAXINT))
        else
          fMetodo.Apelido :=  trim(Alias);
        if Descricao <> '' then begin
          fAjuda := TAjuda.Create;
          fAjuda.Dica := GetAjudaDica(Descricao);
          fAjuda.Texto := GetAjudaTexto(Descricao);
          fAjuda.Add;
          fMetodo.Ajuda := fAjuda;
        end;
        if (Tipo <> '') and (lowerCase(Tipo) <> 'void') then begin
          fElementoTipado := TElementoTipado.Create;
          GetElementoTipado(fElementoTipado, Tipo);
          //gatilho para o cba
          if (fElementoTipado.Tipo = ettOutros) and (upperCase(fElementoTipado.Valores[1]) <> 'T') then
            fElementoTipado.Valores := 'T' + fElementoTipado.Valores;
          if (fElementoTipado.Tipo = ettLista) and (upperCase(fElementoTipado.Valores[1]) <> 'T') then
            fElementoTipado.Valores := 'T' + fElementoTipado.Valores;

          fElementoTipado.Add;
          fMetodo.Retorno := fElementoTipado;
          fMetodo.Retorno.Mascara := Mascara;
        end;
        if lowerCase(Escopo) = 'private' then fMetodo.Escopo := aePrivate else
        if lowerCase(Escopo) = 'protected' then fMetodo.Escopo := aeProtected else
        if lowerCase(Escopo) = 'public' then fMetodo.Escopo := aePublic;
        //2.0 fMetodo.Comentarios
        fMetodo.Acao := Codigo;
        if EstereoTipoKind = _stWorkFlow then fMetodo.Tipo := mtWorkflow else
        if EstereoTipoKind = _stWizard then fMetodo.Tipo := mtWizard else
        if EstereoTipoKind = _stView then fMetodo.Tipo := mtVisaoDeSeguranca else fMetodo.Tipo := mtMaquinaDeEstados;

        if pos('hidden', trim(lowerCase(Alias))) = 1 then fMetodo.Caracteristicas := fMetodo.Caracteristicas + [mcOculto];
        if Estatico = 'class ' then fMetodo.Caracteristicas := fMetodo.Caracteristicas + [mcDeClasse];
        ModelError(not IsValidIdent(Escopo), 'Visibilidade inválida: ' + Escopo + ' para método: ' + Nome);
        if Diretiva = 'override; ' then fMetodo.Caracteristicas := fMetodo.Caracteristicas + [mcSobreposto];
        if Diretiva = 'virtual; ' then fMetodo.Caracteristicas := fMetodo.Caracteristicas + [mcVirtual];
        fPacote := fModelo_Global.Pacotes.Find(Classes_[Cl].Package);
        fMetodo.Classe := TClasse(fPacote.Classes.Find(Classes_[Cl].Nome));
        fMetodo.Add;
        PrvID := fMetodo.ID;
      end;
      for Pa := 0 to P-1 do with Parametros[Pa] do begin
        fParametro := TParametro.Create;
        fParametro.Nome := Parametros[Pa].Nome;
        fParametro.Apelido := Parametros[Pa].Alias;
        if Descricao <> '' then begin
          fAjuda := TAjuda.Create;
          fAjuda.Dica := GetAjudaDica(Descricao);
          fAjuda.Texto := GetAjudaTexto(Descricao);
          fAjuda.Add;
          fParametro.Ajuda := fAjuda;
        end;
        GetElementoTipado(TElementoTipado(fParametro), Tipo);
        fParametro.Inicial := Default;
        fParametro.Mascara := Mascara;
        case CaseOf(Escopo, ['in', 'inout', 'out']) of
          0 : fParametro.Passagem := ppEntrada;
          1 : fParametro.Passagem := ppEntradaSaida;
          2 : fParametro.Passagem := ppSaida;
        end;
        fMetodo.Parametros.Add(fParametro);
      end;
    end;
end;

procedure TParser.LerAssociacao;
var
  P : boolean;
  Multiplicidade : string;
  I : integer;
begin
  with Associacoes[Assoc] do begin
    ID := UpperCase(GetNextToken('id="', '"'));
    Package := Packages[Packs-1].Nome;
    P := false;
    repeat
      case NextToken(['<UML:AssociationEnd ', '</UML:Association>']) of
        0 : begin
          with Pontas[P] do begin
            repeat
              case NextToken(['name="', '"description"', 'multiplicity="', 'aggregation="', 'isNavigable="', 'type="',
                              'Stereotype"', '"Alias"', '"OldName"', '"Group"', '"Initial"', 'tag="Check"', 'tag="CheckMessage"',
                              'tag="Enabled"', 'tag="Visible"', 'tag="constraint"', '</UML:AssociationEnd>']) of
                0 : Nome      := GetToken('"');
                1 : Descricao := GetTag;
                2 : begin
                  Multiplicidade := GetToken('"');
                  Min := '0';
                  Max := 'MAXINT';
                  I := pos('..', Multiplicidade);
                  if I = 0 then begin
                    if Multiplicidade <> '*' then begin
                      Max := Multiplicidade;
                      Min := Max
                    end;
                  end
                  else begin
                    Min := copy(Multiplicidade, 1, I-1);
                    Max := copy(Multiplicidade, I+2, 10);
                    if Max = '*' then Max := 'MAXINT';
                  end;
                end;
                3 : Agregacao   := GetToken('"');
                4 : Navegacao   := GetToken('"');
                5 : Classe      := GetToken('"');
                6 : Estereotipo := GetTag;
                7 : Alias       := GetTag;
                8 : OldName     := GetTag;
                9 : Grupo       := GetTag;
                10: Inicial     := GetTag;
                11: Check       := GetTag;
                12: CheckMessage:= GetTag;
                13: begin
                  Enabled := GetTag;
                  if Enabled <> '' then begin
                    if pos('enable', lowerCase(Estereotipo)) = 0 then
                      if Estereotipo = '' then
                        Estereotipo := 'ENABLE'
                      else
                        Estereotipo := Estereotipo + ', ENABLE';
                  end;
                end;
                14: begin
                  Visible := GetTag;
                  if Visible <> '' then begin
                    if pos('visible', lowerCase(Estereotipo)) = 0 then
                      if Estereotipo = '' then
                        Estereotipo := 'VISIBLE'
                      else
                        Estereotipo := Estereotipo + ', VISIBLE';
                  end;
                end;
//                13: Enabled     := GetTag;
//                14: Visible     := GetTag;
                15: AssociationConstraint := GetNextToken('value="', '"');
                16: break;
              end;
            until false;
          end;
          P := true;
        end;
        1 : break;
      end;
    until false;
  end;
  inc(Assoc);
  SetLength(Associacoes, Assoc+1);
end;

procedure TParser.LerComentario; begin
  with Comentarios[Com] do begin
    ID := UpperCase(GetNextToken('id="', '"'));
    Coordenada.Left := -1;
    if NextToken(['tag="documentation"', '</UML:Comment>']) = 0 then
      Texto := GetNextToken('value="', '"')
    else
      exit;
    if NextToken(['tag="package"', '</UML:Comment>']) = 0 then
      PackageOwner := GetNextToken('value="', '"')
    else
      exit;
  end;
  inc(Com);
  SetLength(Comentarios, Com+1);
end;

function TParser.ProcuraPackage(ClasseID : string) : string;
var
  Cl : integer;
begin
  Result := '';
  for Cl := 0 to C-1 do with Classes_[Cl] do
    if ID = ClasseID then begin
      Result := Package;
      break;
    end;
end;

function TParser.ProcuraClasse(ClasseID : string) : string;
var
  Cl : integer;
begin
  Result := '';
  for Cl := 0 to C-1 do with Classes_[Cl] do
    if ID = ClasseID then begin
      Result := Nome;
      break;
    end;
end;

procedure TParser.CarregaAssociacao;
var
  fPacote : TPacote;
  fClasse : TClasse;
  fLinkTrue,
  fLinkFalse : TLink;
  fIndice : TIndice;
  fAssociacao : TAssociacao;
  fPapel : TPapel;
  fAjuda : TAjuda;
//  fQuebra : TQuebra;
//2.0  fAparencia : TAparencia;
  P : boolean;
  ClasseNome,
  ClasseID : string;
  Ass, PX1, PY1, PX2, PY2 : integer;
  PO, PD : byte;

  function DescobreClassePai(pClasseID : string) : string;
  var
    I : integer;
  begin
    Result := '';
    for I := 0 to H-1 do with Parser.Herancas[I] do
      if upperCase(pClasseID) = upperCase(Filho) then begin
        Result := Classes_[NumClasse(Pai)].ID;
        exit;
      end;
  end;

  function CarregarCoordenadas(var Coordenadas : TCoordenada; Ponta : TPonta; AssocPackage : string; var Link : TLink) : boolean;
  var
    I : integer;
  begin
    Result := true;
    Link   := nil;
    I := NumClasse(Ponta.Classe);
    if I <> -1 then
      with Classes_[I] do
        if Package = AssocPackage then
          if Coordenada.Right = 0 then begin
            Browser.Message('Classe perdida no modelo: ' + Nome + '. Associação ' + Ponta.Nome +' não carregada.');
            Result := false;
          end
          else begin
            Coordenadas := Coordenada;
            Link := ClasseList.Find(PrvID)
          end
        else begin
          I := NumLink(Ponta.Classe + AssocPackage);
          if (I = -1) or (Links[I].Coordenada.Right = 0) then begin
            Browser.Message('Link perdido no modelo. Package: ' + Package + ' Classe: ' + ProcuraClasse(Ponta.Classe) + ' Link: ' + Links[I].Nome + '. Associação ' + Ponta.Nome +' não carregada.');
            Result := false;
          end
          else begin
            Coordenadas := Links[I].Coordenada;
            Link := LinkList.Find(Links[I].PrvID)
          end
        end
    else
      Result := false;
  end;

var
  CoordenadasDestino, CoordenadasOrigem : TCoordenada;
begin
  for Ass := 0 to Assoc-1 do with Associacoes[Ass] do
    if CarregarCoordenadas(CoordenadasDestino, Pontas[true], Package, fLinkTrue) then
      if CarregarCoordenadas(CoordenadasOrigem, Pontas[false], Package, fLinkFalse) then begin
        fAssociacao := TAssociacao.Create;
        fAssociacao.PontoOrigem := 0;
        fAssociacao.PontoDestino := 0;
        fAssociacao.Origem := fLinkTrue;
        fAssociacao.Destino := fLinkFalse;
        if Q > 0 then begin
          PX1 := Quebras[0].X;
          PY1 := Quebras[0].Y;
          PX2 := Quebras[Q-1].X;
          PY2 := Quebras[Q-1].Y;
        end
        else begin
          PX1 := 0; PY1 := 0; PX2 := 0; PY2 := 0;
        end;
        CalculaPontosOrigemDestino(true, true, Aresta,
                    CoordenadasOrigem.Left,
                    CoordenadasOrigem.Top,
                    CoordenadasOrigem.Right  - CoordenadasOrigem.Left,
                    CoordenadasOrigem.Bottom - CoordenadasOrigem.Top,
                    CoordenadasDestino.Left,
                    CoordenadasDestino.Top,
                    CoordenadasDestino.Right - CoordenadasDestino.Left,
                    CoordenadasDestino.Bottom - CoordenadasDestino.Top,
                    PontoOrigemDestino.SX,
                    PontoOrigemDestino.SY,
                    PontoOrigemDestino.EX,
                    PontoOrigemDestino.EY,
                    PX1, PY1, PX2, PY2,
                    Q, PO, PD);
        fAssociacao.PontoOrigem  := PD;
        fAssociacao.PontoDestino := PO;

        for P:= false to true do with Pontas[P] do begin
          if Nome = '' then continue;
          fPapel := TPapel.Create;
          fPapel.Nome := Nome;
//new
          if pos('hidden', trim(lowerCase(Alias))) = 1 then begin
            fPapel.Caracteristicas := fPapel.Caracteristicas + [pcOculto];
            fPapel.Apelido :=  trim(copy(Alias, pos(',', Alias) +1 , MAXINT));
          end
          else
            fPapel.Apelido :=  trim(Alias);
          if ((Pontas[P].Max = '1') and (Pontas[not P].Navegacao <> Navegacao)) then begin
            if Pontas[P].Agregacao = 'none' then fPapel.Agregacao := paNenhuma else
            if Pontas[P].Agregacao = 'composite' then fPapel.Agregacao := paForte else
            fPapel.Agregacao := paFraca;
          end
          else begin
            if Pontas[not P].Agregacao = 'none' then fPapel.Agregacao := paNenhuma else
            if Pontas[not P].Agregacao = 'composite' then fPapel.Agregacao := paForte else
            fPapel.Agregacao := paFraca;
          end;
//fim new
          //multiplicidade
          if Min = '0' then
            if Max = 'MAXINT' then fPapel.Multiplicidade := pm0_N
            else
            if Max = '1' then fPapel.Multiplicidade := pm0_1
            else
              fPapel.Multiplicidade := pmOutras
          else
          if Min = '1' then
            if Max = 'MAXINT' then fPapel.Multiplicidade := pm1_N
            else
            if Max = '1' then fPapel.Multiplicidade := pm1
            else
              fPapel.Multiplicidade := pmOutras
          else
            fPapel.Multiplicidade := pmOutras;
          fPapel.Minimo := StrtoInt(Min);
          if Max = 'MAXINT' then
            fPapel.Maximo := high(word)
          else
            fPapel.Maximo := StrtoInt(Max);
          fPapel.Constraint := AssociationConstraint;
          if pos('hidden', trim(lowerCase(Alias))) = 1 then fPapel.Caracteristicas := fPapel.Caracteristicas + [pcOculto];
          if pos('override', lowerCase(Estereotipo)) > 0 then begin
            fPapel.Caracteristicas := fPapel.Caracteristicas + [pcSopreposto];
            Delete(Estereotipo, pos('override', lowerCase(Estereotipo)), length('override'));
          end;
          if pos('readonly', lowerCase(Estereotipo)) > 0 then begin
            fPapel.Caracteristicas := fPapel.Caracteristicas + [pcSomenteLeitura];
            Delete(Estereotipo, pos('readonly', lowerCase(Estereotipo)), length('readonly'));
          end;
          Delete(Estereotipo, pos('check', lowerCase(Estereotipo)), length('check'));
          Delete(Estereotipo, pos('visible', lowerCase(Estereotipo)), length('visible'));
          Delete(Estereotipo, pos('enable', lowerCase(Estereotipo)), length('enable'));
          Estereotipo := StringReplace(Estereotipo, ',', '', [rfReplaceAll, rfIgnoreCase]);

          ClasseNome := ProcuraClasse(Classe);
          ClasseID := Classe;
          if Estereotipo <> '' then begin
            while true do begin
              fPacote := fModelo_Global.Pacotes.Find(ProcuraPackage(ClasseID));
              fClasse := TClasse(fPacote.Classes.Find(ProcuraClasse(ClasseID)));
              fIndice := fClasse.Indices.Find(Estereotipo);
              if fIndice = nil then begin
                ClasseID := DescobreClassePai(ClasseID);
                if ClasseID = '' then begin
                  Browser.Message('Índice ' + Estereotipo + ' da associação ' + Nome + ' da Classe ' + fClasse.Nome + ' não encontado');
                  break;
                end
                else
                  ClasseNome := ProcuraClasse(ClasseID);
              end
              else begin
                fPapel.Ordem := fIndice;
                break;
              end;
            end;
          end;

          fPapel.Habilitacao := Enabled;
          fPapel.Visibilidade := Visible;
          fPapel.NomeAntigo := OldName;
          fPapel.Validacao := Check;
          fPapel.MensagemValidacao := CheckMessage;
          fPapel.Inicial := Inicial;
          fPapel.Add;
          if Descricao <> '' then begin
            fAjuda := TAjuda.Create;
            fAjuda.Dica := GetAjudaDica(Descricao);
            fAjuda.Texto := GetAjudaTexto(Descricao);
            fAjuda.Add;
            fPapel.Ajuda := fAjuda;
          end;
          if P then begin
            fAssociacao.PapelOrigem := fPapel;
            (*if Q > 0 then
              for I := 0 to Q-1 do begin
                fQuebra := TQuebra.Create;
                fQuebra.Ordem := I + 1;
                fQuebra.X := Quebras[I].X;
                fQuebra.Y := Quebras[I].Y;
                fAssociacao.Quebras.Add(fQuebra);
              end;*)
          end
          else
            fAssociacao.PapelDestino := fPapel;
        end;
        fAssociacao.Add;
      end;
end;

procedure TParser.LerHeranca; begin
  with Herancas[H] do
    repeat
      case NextToken(['subtype="', 'supertype="', 'xmi.id="', '</UML:Generalization>']) of
        0 : Filho := GetToken('"');
        1 : Pai   := GetToken('"');
        2 : ID    := GetToken('"');
      else
        break;
      end;
    until false;
  inc(H);
  SetLength(Herancas, H+1);
end;

procedure TParser.CarregaHeranca;
var
  fPacote : TPacote;
  fHeranca : THeranca;
  I, Clo, Cld : integer;
  PX1, PY1, PX2, PY2 : integer;
  PO, PD : byte;
begin
  for I := 0 to H-1 do with Herancas[I] do begin
    Clo := NumClasse(Filho);
    with Classes_[Clo] do
      if Coordenada.Left = -1 then begin
        Browser.Message('Herança perdida no modelo. Pai: ' + Nome + '. Não Carregada.');
        continue;
      end;
    Cld := NumClasse(Pai);
    with Classes_[Cld] do
      if Coordenada.Left = -1 then begin
        Browser.Message('Herança perdida no modelo. Filho: ' + Nome + ' Não Carregada.');
        continue;
      end;
    fHeranca := THeranca.Create;
    fPacote := fModelo_Global.Pacotes.Find(ProcuraPackage(Filho));
    fHeranca.Origem := fPacote.Classes.Find(Procuraclasse(Filho));
    fPacote := fModelo_Global.Pacotes.Find(ProcuraPackage(Pai));
    fHeranca.Destino := fPacote.Classes.Find(Procuraclasse(Pai));
    if (Clo <> -1) and (Cld <> -1) then begin
      if Q > 0 then begin
        PX1 := Quebras[0].X;
        PY1 := Quebras[0].Y;
        PX2 := Quebras[Q-1].X;
        PY2 := Quebras[Q-1].Y;
      end
      else begin
        PX1 := 0; PY1 := 0; PX2 := 0; PY2 := 0;
      end;
      CalculaPontosOrigemDestino(true, true, Aresta,
                  Classes_[Clo].Coordenada.Left,
                  Classes_[Clo].Coordenada.Top,
                  Classes_[Clo].Coordenada.Right - Classes_[Clo].Coordenada.Left,
                  Classes_[Clo].Coordenada.Bottom - Classes_[Clo].Coordenada.Top,
                  Classes_[Cld].Coordenada.Left,
                  Classes_[Cld].Coordenada.Top,
                  Classes_[Cld].Coordenada.Right - Classes_[Cld].Coordenada.Left,
                  Classes_[Cld].Coordenada.Bottom - Classes_[Cld].Coordenada.Top,
                  PontoOrigemDestino.SX,
                  PontoOrigemDestino.SY,
                  PontoOrigemDestino.EX,
                  PontoOrigemDestino.EY,
                  PX1, PY1, PX2, PY2,
                  Q, PO, PD);
      fHeranca.PontoOrigem  := PO;
      fHeranca.PontoDestino := PD;
    end;
    fHeranca.Add;
  end;
end;

procedure TParser.CarregaComentarios;
var
  fComentario : TComentario;
  fAparencia : Taparencia;
  I, K, PrevID : integer;
  function NumOperacao(pID : string) : integer;
  var
    I, J : integer;
  begin
    Result := -1;
    for J := 0 to C-1 do begin
      with Parser.Classes_[J] do
        for I := 0 to O-1 do
          if UpperCase(Operacoes[I].StmId) = UpperCase(pID) then begin
            Result := Operacoes[I].PrvID;
            exit;
          end;
    end;
  end;
begin
  for I := 0 to Com-1 do with Comentarios[I] do begin
    if Coordenada.Left = -1 then begin
      Browser.Message('Comentário perdido no modelo: ' + Texto + '. Não Carregado.');
      continue;
    end;
    fComentario := TComentario.Create;
    fComentario.Texto := Texto;
    PrevID := NumOperacao(OwnerID);

    if PrevID > 0 then
      fComentario.Metodo := MetodoList.Find(PrevID)
    else begin
      K := NumPackage(PackageOwner);
      if K <> -1 then begin
        PrevID := Packages[K].PrvID;
        fComentario.Pacote := PacoteList.Find(PrevID);
      end;
    end;
    fAparencia := TAparencia.Create;
    fAparencia.X := Coordenada.Left;
    fAparencia.Y := Coordenada.Top;
    fAparencia.H := (Coordenada.Bottom - Coordenada.Top);
    fAparencia.W := (Coordenada.Right - Coordenada.Left);
    fAparencia.Add;
    fComentario.Aparencia := fAparencia;
    fComentario.Add;
  end;
end;

procedure TParser.LerTransicoes; begin
  repeat
    case NextToken(['<UML:Transition', '/UML:StateMachine.transitions']) of
      0 : with Transicoes[T] do begin
        ID      := UpperCase(GetNextToken('id="', '"'));
        Origem  := UpperCase(GetNextToken('source="', '"'));
        Destino := UpperCase(GetNextToken('target="', '"'));
        NumCl   := -1;
        NumOp   := -1;
        repeat
          case NextToken(['stereotype" value="', 'documentation" value="', 'privatedata1" value="',
            'privatedata2" value="', 'privatedata3" value="', 'Action" value="', '/UML:Transition>']) of
            0,
            1 : ;
            2 : begin
              Alias  := trim(GetToken('"'));
              Evento := StrToIdent(Alias);
            end;
            3 : Condicao := trim(GetToken('"'));
            4 : Acao     := trim(GetToken('"'));
            5 : CorpoAcao:= GetNextToken('Comment name="', '"');
          else
            if (Condicao = '') and (Acao = '') and (CorpoAcao = '') then Evento := 'Default';
            break
          end;
        until false;
      end;
    else
      exit;
    end;
    inc(T);
    SetLength(Transicoes, T+1);
  until false;
end;

procedure TParser.CarregaTransicoes;
var
  fEstadoBase : TEstadoBase;
//  fAparencia : TAparencia;
  fTransicao : TTransicao;
//  fQuebra : TQuebra;
  Tr, Eo, Ed : integer;
  PX1, PY1, PX2, PY2 : integer;
  PO, PD : byte;
begin
  for Tr := 0 to T-1 do
    with Transicoes[Tr] do begin
      if (NumCl = -1) or (NumOp = -1) then begin
        Browser.Message('Transição não localizada: ' + Evento);
        continue
      end;
      Eo := NumEstado(Transicoes[Tr].Origem);
      if Eo < 0 then begin
        Browser.Message('Origem da transição não localizado: ' + Evento + ':' + Origem);
        continue
      end
      else with Estados[Eo] do
        if (Coordenada.Left = -1) or (NumCl = -1) or (NumOp = -1) then continue;

      Ed := NumEstado(Transicoes[Tr].Destino);
      if Ed < 0 then begin
        Browser.Message('Destino da transição não localizado: ' + Evento + ':' + Destino);
        continue
      end
      else with Estados[Ed] do
        if (Coordenada.Left = -1) or (NumCl = -1) or (NumOp = -1) then continue;

      fTransicao := TTransicao.Create;
      fTransicao.Nome := ifthen(trim(Evento) = 'Default', '', Evento);
      if fTransicao.Nome <> Alias then fTransicao.Apelido := Alias;
      if lowercase(Condicao) <> 'true' then fTransicao.Condicao := Condicao;
      if CorpoAcao <> '' then Acao := Acao + ';' + CorpoAcao;
      fTransicao.Acao := StringReplace(Acao, '; ', ';'#13, [rfReplaceAll]);
      //2.0 ?? fTransicao.Apelido := Transicoes[Tr].Evento;//2.0 ??
      //2.0 ?? Transicoes[Tr].Ordem
      //2.0 ?? Transicoes[Tr].OrdemOriginal
      //2.0 ?? Transicoes[Tr].ID
      //2.0 ??Transicoes[Tr].CorpoAcao
      {
      fPacote := fModelo_Global.Pacotes.Find(Classes_[NumCl].Package);
      fClasse := fPacote.Classes.Find(Classes_[NumCl].Nome);
      fMetodo := fClasse.Metodos.Find(Classes_[NumCl].Operacoes[NumOp].Nome);
      }
      with Estados[Eo] do begin
        fEstadoBase := TEstadoBase(EstadoList.FindInFamily(PrvID));
        if fEstadoBase <> nil then fTransicao.Origem := fEstadoBase;
      end;
      with Estados[Ed] do begin
        fEstadoBase := TEstadoBase(EstadoList.FindInFamily(PrvID));
        if fEstadoBase <> nil then fTransicao.Destino := fEstadoBase;
      end;
      if Q > 0 then begin
        PX1 := Quebras[0].X;
        PY1 := Quebras[0].Y;
        PX2 := Quebras[Q-1].X;
        PY2 := Quebras[Q-1].Y;
      end
      else begin
        PX1 := 0; PY1 := 0; PX2 := 0; PY2 := 0;
      end;
      CalculaPontosOrigemDestino(
                  not ((Estados[Eo].Tipo = 'initial') or (Estados[Eo].Tipo = 'final') or (Estados[Eo].Tipo = 'juncao')),
                  not ((Estados[Ed].Tipo = 'initial') or (Estados[Ed].Tipo = 'final') or (Estados[Ed].Tipo = 'juncao')),
                  Aresta,
                  Estados[Eo].Coordenada.Left,
                  Estados[Eo].Coordenada.Top,
                  Estados[Eo].Coordenada.Right - Estados[Eo].Coordenada.Left,
                  Estados[Eo].Coordenada.Bottom - Estados[Eo].Coordenada.Top,
                  Estados[Ed].Coordenada.Left,
                  Estados[Ed].Coordenada.Top,
                  Estados[Ed].Coordenada.Right - Estados[Ed].Coordenada.Left,
                  Estados[Ed].Coordenada.Bottom - Estados[Ed].Coordenada.Top,
                  PontoOrigemDestino.SX,
                  PontoOrigemDestino.SY,
                  PontoOrigemDestino.EX,
                  PontoOrigemDestino.EY,
                  PX1, PY1, PX2, PY2,
                  Q, PO, PD);
        fTransicao.PontoOrigem  := PO;
        fTransicao.PontoDestino := PD;
      (*
      if Q > 0 then
        for I := 0 to Q-1 do begin
          fQuebra := TQuebra.Create;
          fQuebra.Ordem := I + 1;
          fQuebra.X := Quebras[I].X;
          fQuebra.Y := Quebras[I].Y;
          fTransicao.Quebras.Add(fQuebra);
        end;*)
      fTransicao.Add;
    end;
end;

function RemoverComentario(S : string) : string;
var
  I : integer;
begin
  if pos('//', S) = 1 then begin
    I := pos(#13, S);
    if I <> 0 then
      Result := trim(copy(S, I+1, length(S)))
    else
      Result := ''
  end
  else
    Result := S
end;

procedure TParser.LerEstados;
var
  vNome, vKind, vVariavel, vTipo, S : string;
begin
  repeat
    case NextToken(['<UML:SimpleState', '<UML:PseudoState', '</UML:StateMachine.top']) of
      0 : with Estados[E] do begin // Estado
        V := 0;
        Alias  := GetNextToken('name="', '"');
        Nome   := StrToIdent(Alias);
        ID     := UpperCase(GetNextToken('id="', '"'));
        Coordenada.Left := -1;
        Botoes := '[]';
        Tipo := 'padrao';
        NumCl := -1;
        NumOp := -1;
        repeat
          case NextToken(['<UML:Stereotype', '"documentation"', '"alias"',  '<UML:Attribute', '<UML:UninterpretedAction', '</UML:SimpleState']) of
            0 : Estereotipo  := GetNextToken('name="', '"');
            1 : Documentacao := GetNextToken('value="', '"');
            2 : Alias        := GetNextToken('value="', '"');
            3 : begin
              vVariavel := GetNextToken('name="', '"');
              Variaveis_ := Variaveis_ + vVariavel;
              NextToken(['<UML:Attribute.initialValue>']);
              if NextToken(['<UML:Expression body="', '</UML:Attribute.initialValue>']) = 0 then
                Variaveis[V].Inicial := trim(GetToken('"'));
              if NextToken(['"type" value="', '</UML:Attribute>']) = 0 then begin
                vTipo := trim(GetToken('"'));
                Variaveis_ := Variaveis_ + ' : ' + vTipo + ';';
              end
              else
                Variaveis_ := Variaveis_ + ',';
              Variaveis[V].Nome := vVariavel;
              Variaveis[V].Tipo := vTipo;
              inc(V);
            end;
            4 : begin
              S := GetNextToken('name="', '"');
              if S <> '//' then
                Acoes := Acoes + IfThen(Acoes <> '', ^M, '') + RemoverComentario(S);
              if NextToken(['"behaviour"',  '</UML:UninterpretedAction']) = 0 then begin
                S := GetTag;
                if S <> '//' then begin
                  S := AnsiReplaceText(S, ^M^J, ^M);
                  S := AnsiReplaceText(S, ';', '');
                  Acoes := Acoes + IfThen(Acoes <> '', ^M, '') + RemoverComentario(S);
                end;
              end;
            end
          else
            break
          end;
        until false;
        inc(E);
        SetLength(Estados, E+1);
      end;
    1 : with Estados[E] do begin
          if NextToken(['name="', 'id="']) = 0 then begin
            vNome := GetToken('"');
            ID := UpperCase(GetNextToken('id="', '"'));
          end
          else begin
            vNome := '';
            ID := UpperCase(GetToken('"'));
          end;
          Coordenada.Left := -1;
          if NextToken(['kind="', '">']) = 0 then
            vKind := GetToken('"')
          else
            vKind := '';
          Botoes := '[]';
          NumCl := -1;
          NumOp := -1;
          if vNome = 'Junction' then begin
            Tipo := 'juncao';
            Nome := vNome;
            inc(E);
            SetLength(Estados, E+1);
          end
          else
          if vKind = 'join' then begin
            Tipo := vKind;
            Nome := 'Join' + IntToStr(E);
            Acoes := vNome;
            inc(E);
            SetLength(Estados, E+1);
          end
          else
          if (vNome = '') and ((vKind = 'initial') or (vKind = 'final')) then begin
            ModelError(true, 'Estado inicial ou final sem nome em uma máquina de estado neste package');
            raise Exception.Create('');
          end
          else begin //inicial e final
            Nome := vNome;
            Tipo := vKind;
            inc(E);
            SetLength(Estados, E+1);
          end;
      end;
    else
      exit;
    end;
  until false;
end;

procedure TParser.CarregaEstados;
var
  fPacote : TPacote;
  fClasse : TClasse;
  fEstado : TEstado;
  fVariavel : TVariavel;
  fFormulario : TFormulario;
  fFormatacao : TFormatacao;
  fAjuda : TAjuda;
  fAparencia : TAparencia;
  Es, Va, P : integer;
  procedure CarregaPropriedadesComuns(var pEstadoBase : TEstadoBase); begin
    with Estados[Es] do begin
      pEstadoBase.Nome := Nome;
      if Nome <> Alias then pEstadoBase.Apelido := Alias;
      fAparencia := TAparencia.Create;
      fAparencia.X := Coordenada.Left;
      fAparencia.Y := Coordenada.Top;
      if (Tipo = 'initial') or (Tipo = 'final') or (Tipo = 'juncao') then  begin
        fAparencia.H := 18;
        fAparencia.W := 18;
        if Tipo <> 'final' then fAparencia.Cor := 0;
      end
      else begin
        fAparencia.H := (Coordenada.Bottom - Coordenada.Top);
        fAparencia.W := (Coordenada.Right - Coordenada.Left);
      end;
      fAparencia.Add;
      pEstadoBase.Aparencia := fAparencia;
      if Documentacao <> '' then begin
        fAjuda := TAjuda.Create;
        fAjuda.Dica := GetAjudaDica(Documentacao);
        fAjuda.Texto := GetAjudaTexto(Documentacao);
        fAjuda.Add;
        pEstadoBase.Ajuda := fAjuda;
      end;
      fPacote := fModelo_Global.Pacotes.Find(ProcuraPackage(Classes_[NumCl].ID));
      fClasse := TClasse(fPacote.Classes.Find(ProcuraClasse(Classes_[NumCl].ID)));
      pEstadoBase.Metodo := fClasse.Metodos.Find(Classes_[NumCl].Operacoes[NumOp].Nome);
    end;
  end;
  
  procedure MontaComandoEAlinhamento(const Def: string);
  var
    S : string;
    
    function DelimitedBy(const BeginDelimiter, EndDelimiter: char) : boolean;begin
      Result := (S[1] = BeginDelimiter) and (S[length(S)] = EndDelimiter) and (length(S) > 2);
    end;
    
  begin
    if Def = '' then exit;
    S := Def;
    if DelimitedBy('|', '|') then fFormatacao.Comando := fcCriarGrupo else
    if DelimitedBy('<', '>') then fFormatacao.Comando := fcCriarGrupoColapsavel else
    if DelimitedBy('>', '<') then fFormatacao.Comando := fcCriarGrupoColapsado else
    if DelimitedBy('[', ']') then fFormatacao.Comando := fcCriarAba else
    if S = '[]' then
      fFormatacao.Comando := fcFecharAbas
    else
      if S[1] = '#' then begin
        fFormatacao.Comando := fcMostrar;
        Delete(S, 1, 1);
      end
      else
        fFormatacao.Comando := fcEditar;
      if DelimitedBy('/', '/') then
        FFormatacao.LayOut := flOcuparTodaLargura
      else
        if S[1] = '/' then
          FFormatacao.LayOut := flIniciarNaMargemEsquerda
        else
          FFormatacao.LayOut := flPadrao;
  end;

begin
  for Es := 0 to E-1 do with Estados[Es] do begin
    if (Coordenada.Left = -1) or (NumCl = -1) or (NumOp = -1) then begin
      Browser.Message('Estado Perdido no Modelo: ' + Nome + ':' + ID + '. Não Carregado.');
      continue;
    end;
    if (Estereotipo = '') or (pos('subflow', lowerCase(Estereotipo)) > 0) then begin
      fEstado := TEstado.Create;
      CarregaPropriedadesComuns(TEstadoBase(fEstado));
      fEstado.Acao := Acoes;
      if Tipo = 'padrao' then
        fEstado.Tipo := etPadrao
      else
        if Tipo = 'join' then with Coordenada do begin
          if (Bottom - Top) > (Right - Left) then
            fEstado.Tipo := etForkV
          else
            fEstado.Tipo := etForkH
        end
      else
      if Tipo = 'juncao' then fEstado.Tipo := etJuncao else
      if Tipo = 'initial' then fEstado.Tipo := etInicial else
      if Tipo = 'final' then fEstado.Tipo := etFinal;
      if pos('subflow', lowerCase(Estados[Es].Estereotipo)) > 0 then fEstado.Subflow := true;
      fEstado.Add;
      for Va := 0 to V-1 do begin
        fVariavel := TVariavel.Create;
        fVariavel.Nome := Variaveis[Va].Nome;
        GetElementoTipado(TElementoTipado(fVariavel), Variaveis[Va].Tipo{, 'Tipo de Variavel <' + Variaveis[Va].Tipo + '> inválido. Classe: ' + Classes_[NumCl].Nome + ', Método: ' + Classes_[NumCl].Operacoes[NumOp].Nome + ', Variável: ' + Variaveis[Va].Nome});
        fVariavel.Inicial := Variaveis[Va].Inicial;
        fEstado.Variaveis.Add(fVariavel);
      end;
      PrvID := fEstado.ID;
    end
    else begin
      fFormulario := TFormulario.Create;
      CarregaPropriedadesComuns(TEstadoBase(fFormulario));
      fFormulario.Objeto := Estereotipo;
      for Va := 0 to V-1 do begin
        fFormatacao := TFormatacao.Create;
        fFormatacao.Ordem := Va;
        MontaComandoEAlinhamento(Variaveis[Va].Nome);
        if fFormatacao.Comando in [fcCriarGrupo, fcCriarGrupoColapsavel, fcCriarGrupoColapsado, fcCriarAba] then
          fFormatacao.Titulo := copy(Variaveis[Va].Nome, 2, length(Variaveis[Va].Nome) - 2)
        else begin
          while true do begin
            P := pos('#', Variaveis[Va].Nome);
            if P > 0 then Delete(Variaveis[Va].Nome, P, 1);
            P := pos('|', Variaveis[Va].Nome);
            if P > 0 then Delete(Variaveis[Va].Nome, P, 1);
            if P = 0 then break;
          end;
          fFormatacao.Propriedade := Variaveis[Va].Nome;
        end;
        fFormatacao.Formulario := fFormulario;
      end;
      PrvID := fFormulario.ID;
    end;
  end;
end;

procedure TParser.LerDiagramaMaquinaEstados;
var
  Nome, ID, StmID : string;
  Rank, SX, SY, Edge, Cl, Op, I,
  Left, Top : integer;
begin
  Nome := GetNextToken('name="', '"');
  StmID := GetNextToken('id="', '"');
  SX := 0; SY := 0; Edge := 0; Rank := 0; Cl := -1; Op := -1;
  while NextToken(['diagramType="StateDiagram"', '</UML:Diagram>']) = 0 do
    repeat
      try
        case NextToken(['tag="parent"', 'Left=', 'SX=', 'SY=', 'EDGE=', 'subject=', '</UML:Diagram.element>']) of
          0 : begin
            Cl := NumClasse(GetTag);
            if Cl <> -1 then begin
              Op := NumClasseOperacao(Cl, Nome);
              if Op = -1 then break;
              Classes_[Cl].Operacoes[Op].StmId := StmID;
              Classes_[Cl].Operacoes[Op].E := 0;
            end
            else
              break
          end;
          1 : begin
            Left := StrToInt(GetToken(';'));
            Top := StrToInt(GetNextToken('Top=', ';'));
            Rank := Left + (1000 * Top);
          end;
          2 : SX   := StrToInt(GetToken(';'));
          3 : SY   := StrToInt(GetToken(';'));
          4 : Edge := StrToInt(GetToken(';'));
          5 : begin
            ID := UpperCase(GetNextToken('"', '"'));
            if Rank = 0 then begin // Transição
              I := NumTransicao(ID);
              if I <> -1 then
                with Transicoes[I] do begin
                  NumCl := Cl;
                  NumOp := Op;
                end;
              Rank := (5-Edge) * 1000;
              case Edge of
                1 : dec(Rank, SX);
                2 : inc(Rank, SY);
                3 : inc(Rank, SX);
                4 : dec(Rank, SY);
              end;
              I := NumEstadoTransicao(Cl, Op, ID);
              if I <> -1 then with Classes_[Cl].Operacoes[Op].Estados_[I] do begin
                Transicoes_[T] := AtribuiOrdemTransicao(ID, Rank);
                inc(T);
              end
              else with Transicoes[IndiceTransicao(ID)] do
                ModelError(Evento <> 'Default', 'Transição solta: ' + Evento + ' , Classe: ' + Classes_[Cl].Nome + ', Máquina de Estado: ' + Nome + ', Ação: ' + Acao);
            end
            else // Estado
              if (Cl <> -1) and (Op <> -1) then with Classes_[Cl].Operacoes[Op] do begin
                I := NumEstado(ID);
                if I <> -1 then
                  with Estados[I] do begin
                    NumCl := Cl;
                    NumOp := Op;
                  end;
                I := AtribuiSequenceEstado(ID, Rank, GetNextToken('seqno="', '"'));
                if I <> -1 then begin
                  Estados_[E].Estado := I;
                  Estados_[E].T := 0;
                  inc(E);
                end;
              end;
            Rank := 0; SX := 0; Edge := 0;
          end
        else
          ClassificarEstados(Cl, Op);
          break
        end;
      except
        ModelError(true, 'Diagrama de máquina de estado: ' + Nome + ' tem problema de formatação no XMI');
      end;
    until false
end;

procedure TParser.LerPosicaoDeClasses;
var
  ID : string;
  lLeft, lTop, lRight, lBottom,
  Cl : integer;

procedure CarregaCoordenadaLink;
var
  L : integer;
begin
  L := NumLink(ID + Packages[Packs-1].Nome);
  if L <> -1 then
    with Links[L].Coordenada do begin
      Left   := lLeft;
      Top    := lTop;
      Right  := lRight;
      Bottom := lBottom;
    end;
end;

begin
  lLeft := 0; lTop := 0; lRight := 0; lBottom := 0;
  repeat
    case NextToken(['<UML:DiagramElement geometry="', '</XMI.content']) of
      0 : repeat
        case NextToken(['subject="', 'Left=', 'Top=', 'Right=', 'Bottom=', ';"/>']) of
          0 : begin
            ID := GetToken('"');
            Cl := NumClasse(ID);
            if Cl <> -1 then
              if Packages[Packs-1].Nome = Classes_[Cl].Package then
                with Classes_[Cl].Coordenada do begin
                  Left   := lLeft;
                  Top    := lTop;
                  Right  := lRight;
                  Bottom := lBottom;
                end
              else
                CarregaCoordenadaLink
            else
              CarregaCoordenadaLink;
            lLeft := 0; lTop := 0; lRight := 0; lBottom := 0;
          end;
          1 : lLeft   := StrToInt(GetToken(';'));
          2 : lTop    := StrToInt(GetToken(';'));
          3 : lRight  := StrToInt(GetToken(';'));
          4 : lBottom := StrToInt(GetToken(';'));
          else
            break;
        end;
      until false;
    else
      exit;
    end;
  until false;
end;

procedure TParser.LerPosicaoDeHerancas;
var
  ID, OwnerID, S : string;
  He, QtdQbr, I, P : integer;
  fSX, fSY, fEX, fEY, Ar : integer;
  Qbr : TQuebras;
begin
  QtdQbr := 0; fSX := 0; fSY := 0; fEX := 0; fEY := 0; Ar := 0;
  repeat
    case NextToken(['id="', '<UML:DiagramElement geometry="', '</XMI.content']) of
      0 : OwnerID := GetToken('"');
      1 : repeat
        case NextToken(['subject="', 'SX=', 'SY=', 'EX=', 'EY=', 'EDGE=', 'Path=', ';"/>']) of
          0 : begin
            ID := GetToken('"');
            He := NumHeranca(ID);
            if He <> -1 then
              with Herancas[He], PontoOrigemDestino do begin
                SX := fSX;
                SY := fSY;
                EX := fEX;
                EY := fEY;
                Aresta := Ar;
                for I := 0 to QtdQbr-1 do begin
                  Quebras[I].X := abs(Qbr[I].X);
                  Quebras[I].Y := abs(Qbr[I].Y);
                end;
                Q := QtdQbr;
              end;
            QtdQbr := 0; fSX := 0; fSY := 0; fEX := 0; fEY := 0; Ar := 0;
          end;
          1 : fSX := StrtoInt(GetToken(';'));
          2 : fSY := StrtoInt(GetToken(';'));
          3 : fEX := StrtoInt(GetToken(';'));
          4 : fEY := StrtoInt(GetToken(';'));
          5 : Ar  := StrtoInt(GetToken(';'));
          6 : begin
            S := GetToken(';"');
            QtdQbr := 0; P := 1;
            while P <= length(S) do begin
              Qbr[QtdQbr].X := StrtoInt(ExtractFromStr(S, P, ':'));
              Qbr[QtdQbr].Y := abs(StrtoInt(ExtractFromStr(S, P, '$')));
              inc(QtdQbr);
            end;
          end;
          else
            break;
        end;
      until false;
    else
      exit;
    end;
  until false;
end;

procedure TParser.LerPosicaoDeAssociacoes;
var
  ID : string;
  S : string;
  Ass, QtdQbr, I, P : integer;
  fSX, fSY, fEX, fEY, Ar : integer;
  Qbr : TQuebras;
begin
  QtdQbr := 0; fSX := 0; fSY := 0; fEX := 0; fEY := 0; Ar := 0;
  repeat
    case NextToken(['<UML:DiagramElement geometry="', '</XMI.content']) of
      0 : repeat
        case NextToken(['subject="', 'SX=', 'SY=', 'EX=', 'EY=', 'EDGE=', 'Path=', ';"/>']) of
          0 : begin
            ID := GetToken('"');
            Ass := NumAssociacao(ID);
            if Ass <> -1 then
              with Associacoes[Ass], PontoOrigemDestino do begin
                SX := fSX;
                SY := fSY;
                EX := fEX;
                EY := fEY;
                Aresta := Ar;
                for I := 0 to QtdQbr-1 do begin
                  Quebras[I].X := abs(Qbr[I].X);
                  Quebras[I].Y := abs(Qbr[I].Y);
                end;
                Q := QtdQbr;
              end;
            QtdQbr := 0; fSX := 0; fSY := 0; fEX := 0; fEY := 0; Ar := 0;
          end;
          1 : fSX := StrtoInt(GetToken(';'));
          2 : fSY := StrtoInt(GetToken(';'));
          3 : fEX := StrtoInt(GetToken(';'));
          4 : fEY := StrtoInt(GetToken(';'));
          5 : Ar  := StrtoInt(GetToken(';'));
          6 : begin
            S := GetToken(';"');
            QtdQbr := 0; P := 1;
            while P <= length(S) do begin
              Qbr[QtdQbr].X := StrtoInt(ExtractFromStr(S, P, ':'));
              Qbr[QtdQbr].Y := StrtoInt(ExtractFromStr(S, P, '$'));
              inc(QtdQbr);
            end;
          end;
          else
            break;
        end;
      until false;
    else
      exit;
    end;
  until false;
end;

procedure TParser.LerPosicaoDeEstados;
var
  ID : string;
  lLeft, lTop, lRight, lBottom,
  Es : integer;
begin
  lLeft := 0; lTop := 0; lRight := 0; lBottom := 0;
  repeat
    case NextToken(['<UML:DiagramElement geometry="', '</XMI.content']) of
      0 : repeat
        case NextToken(['subject="', 'Left=', 'Top=', 'Right=', 'Bottom=', ';"/>']) of
          0 : begin
            ID := GetToken('"');
            Es := NumEstado(ID);
            if Es <> -1 then
              with Estados[Es].Coordenada do begin
                Left   := lLeft;
                Top    := lTop;
                Right  := lRight;
                Bottom := lBottom;
              end;
            lLeft := 0; lTop := 0; lRight := 0; lBottom := 0;
          end;
          1 : lLeft   := StrToInt(GetToken(';'));
          2 : lTop    := StrToInt(GetToken(';'));
          3 : lRight  := StrToInt(GetToken(';'));
          4 : lBottom := StrToInt(GetToken(';'));
          else
            break;
        end;
      until false;
    else
      exit;
    end;
  until false;
end;

procedure TParser.LerPosicaoDeTransicoes;
var
  ID, S : string;
  Tr, QtdQbr, I, P : integer;
  fSX, fSY, fEX, fEY, Ar : integer;
  Qbr : TQuebras;
begin
  QtdQbr := 0; fSX := 0; fSY := 0; fEX := 0; fEY := 0; Ar := 0;
  repeat
    case NextToken(['<UML:DiagramElement geometry="', '</XMI.content']) of
      0 : repeat
        case NextToken(['subject="', 'SX=', 'SY=', 'EX=', 'EY=', 'EDGE=', 'Path=', ';"/>']) of
          0 : begin
            ID := GetToken('"');
            Tr := NumTransicao(ID);
            if Tr <> -1 then
              with Transicoes[Tr], PontoOrigemDestino do begin
                SX := fSX;
                SY := fSY;
                EX := fEX;
                EY := fEY;
                Aresta := Ar;
                for I := 0 to QtdQbr-1 do begin
                  Quebras[I].X := abs(Qbr[I].X);
                  Quebras[I].Y := abs(Qbr[I].Y);
                end;
                Q := QtdQbr;
              end;
            QtdQbr := 0; fSX := 0; fSY := 0; fEX := 0; fEY := 0; Ar := 0;
          end;
          1 : fSX := StrtoInt(GetToken(';'));
          2 : fSY := StrtoInt(GetToken(';'));
          3 : fEX := StrtoInt(GetToken(';'));
          4 : fEY := StrtoInt(GetToken(';'));
          5 : Ar  := StrtoInt(GetToken(';'));
          6 : begin
            S := GetToken(';"');
            QtdQbr := 0; P := 1;
            while P <= length(S) do begin
              Qbr[QtdQbr].X := StrtoInt(ExtractFromStr(S, P, ':'));
              Qbr[QtdQbr].Y := StrtoInt(ExtractFromStr(S, P, '$'));
              inc(QtdQbr);
            end;
          end;
          else
            break;
        end;
      until false;
    else
      exit;
    end;
  until false;
end;

procedure TParser.LerPosicaoDeComentarios;
var
  ID, OwnerID : string;
  Left, Top, Right, Bottom,
  Co : integer;
begin
  Left := 0; Top := 0; Right := 0; Bottom := 0;
  repeat
    case NextToken(['id="', '<UML:DiagramElement geometry="', '</XMI.content']) of
      0 : OwnerID := GetToken('"');
      1 :
        repeat
          case NextToken(['subject="', 'Left=', 'Top=', 'Right=', 'Bottom=', ';"/>']) of
            0 : begin
              ID := GetToken('"');
              Co := NumComentario(ID);
              if Co = -1 then begin
                Left := 0; Top := 0; Right := 0; Bottom := 0;
                continue;
              end;
              Comentarios[Co].OwnerID := OwnerID;
              Comentarios[Co].Coordenada.Left := Left;
              Comentarios[Co].Coordenada.Top := Top;
              Comentarios[Co].Coordenada.Right := Right;
              Comentarios[Co].Coordenada.Bottom := Bottom;
              Left := 0; Top := 0; Right := 0; Bottom := 0;
            end;
            1 : Left := StrToInt(GetToken(';'));
            2 : Top := StrToInt(GetToken(';'));
            3 : Right := StrToInt(GetToken(';'));
            4 : Bottom := StrToInt(GetToken(';'));
            else
              break;
          end;
        until false;
    else
      exit;
    end;
  until false;
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

procedure CarregaModelo(EAPFile : string); begin
  Parser := TParser.Create;
  with Parser do
    try
      ExportPackages(EAPFile);
      CarregaDesigner;
    finally
      Parser.Free;
    end;
end;

procedure TParser.ExportPackages(EAPFile : string);
var
  {$IFNDEF FPC}
  XMIFile : TFileName;
  ModifiedDate : TDateTime;
  K, H : Integer;
  CaseTool, M, P, I : variant;
  Model, PkgName, PkgGUID : string;
  Force : boolean;
  {$ELSE}
  SearchRec : TSearchRec;
  {$ENDIF}
begin
  if FileExists(EAPFile) then begin
    {$IFNDEF FPC}
    ModifiedDate := now;
    CoInitialize(nil);
    Browser.Message('..Conectando ao EA...');
    CaseTool := CreateOleObject('EA.Repository');
    Browser.Message(StringOfChar('-', 150));
    CaseTool.OpenFile(EAPFile);
    Browser.Message('..Conectado');
    M := CaseTool.Models.GetAt(0);
    Modelo := M.Name;
    Model := M.Name;
    P := M.Packages;
    H := P.Count;
    for K := 0 to H - 1 do begin
      Force := false;
      try
        PkgName := P.GetAt(K).Name;
        PkgGUID := P.GetAt(K).PackageGUID;
        ModifiedDate := P.GetAt(K).Diagrams.GetByName(PkgName).ModifiedDate;;
      except
        Force := true;
      end;
      if pos(',' + lowerCase(PkgName) + ',' ,',lpt,lpt1,lpt2,lpt3,prn,prn1,prn2,prn3,com,com1,com2,com3,con,con1,con2,con3,err,') <> 0 then
        PkgName := PkgName + '_';
      XMIFile := ExtractFilePath(EAPFile) + PkgName + '.xmi';
      if Force or not FileExists(XMIFile) or (ModifiedDate > FileDateToDateTime(FileAge(XMIFile))) then begin
        Browser.Message('Gerando ' + XMIFile);
        //CaseTool.GetProjectInterface.ExportPackageXMI(PkgGUID, 0, 1, -1, 1, -1, ShortString(XMIFile));
        I := CaseTool.GetProjectInterface;
        I.ExportPackageXMI(PkgGUID, 0, 1, -1, 1, -1, XMIFile);
        Browser.Message('XMI Generado');
      end
      else
        Browser.Message('XMI sem Alteração:' + XMIFile);
      Browser.Message(StringOfChar('-', 150));
      Browser.Message('..Carregando arquivo XMI: ' + XMIFile);
      LerXMI(Model, XMIFile);
    end;
    CaseTool.Exit;
    CaseTool := Unassigned;
    {$ELSE}
    Modelo := ExtractFileName(EAPFile);
    Modelo := copy(Modelo, 1, length(Modelo) - 4);
    if FindFirst(ExtractFilePath(EAPFile) + PathDelim + '*.xmi', faAnyFile, SearchRec) = 0 then
   	  repeat
        Browser.Message(StringOfChar('-', 150));
        Browser.Message('..Carregando arquivo XMI: ' + SearchRec.Name);
        LerXMI(Modelo, ExtractFilePath(EAPFile) + PathDelim + SearchRec.Name);
      until FindNext(SearchRec) <> 0;
    SysUtils.FindClose(SearchRec);
    {$ENDIF}
  end
  else begin
    Browser.Message('Arquivo ' + EAPFile + ' não existe');
    GlobalError := true;
  end;
end;

procedure TParser.CarregaDesigner; begin
  if GlobalError then begin
    Browser.Message('Detectado erro na importação. Carga não efetuada');
    exit;
  end;
  BeginTransaction;
  try
    Browser.Message('Carregando Modelo...');
    CarregaModelo;
    Browser.Message('Carregando Packages...');
    CarregaPackage;
    Browser.Message('Carregando Classes...');
    CarregaClasse;
    Browser.Message('Carregando Links...');
    CarregaLinks;
    Browser.Message('Carregando Heranças...');
    CarregaHeranca;
    Browser.Message('Carregando Atributos...');
    CarregaAtributos;
    Browser.Message('Carregando Metodos...');
    CarregaMetodos;
    Browser.Message('Carregando Associações...');
    CarregaAssociacao;
    Browser.Message('Carregando Estados...');
    CarregaEstados;
    Browser.Message('Carregando Transições...');
    CarregaTransicoes;
    Browser.Message('Carregando Comentários...');
    CarregaComentarios;
    Browser.Message('Carregando Informações Gerais...');
    CarregaTagsGerais;
    Browser.Message('Fim da Carga do Modelo...');
    EndTransaction;
  except
    RollBack;
  end;
end;

procedure TParser.LoadPosition; begin
  Browser.Message('Lendo posicionamentos...');
  if C <> 0 then begin
    reset(XMI);
    NextToken(['</UML:Model>']);
    NextToken(['<UML:Diagram.element>']);
    LerPosicaoDeClasses;
  end;
  if E <> 0 then begin
    reset(XMI);
    NextToken(['</UML:Model>']);
    NextToken(['<UML:Diagram.element>']);
    LerPosicaoDeEstados;
  end;
  if Assoc <> 0 then begin
    reset(XMI);
    NextToken(['</UML:Model>']);
    NextToken(['<UML:Diagram.element>']);
    LerPosicaoDeAssociacoes;
  end;
  if H <> 0 then begin
    reset(XMI);
    NextToken(['</UML:Model>']);
    NextToken(['<UML:Diagram.element>']);
    LerPosicaoDeHerancas;
  end;
  if T <> 0 then begin
    reset(XMI);
    NextToken(['</UML:Model>']);
    NextToken(['<UML:Diagram.element>']);
    LerPosicaoDeTransicoes;
  end;
  if Com <> 0 then begin
    reset(XMI);
    NextToken(['</UML:Model>']);
    NextToken(['<UML:Diagram ']);
    LerPosicaoDeComentarios;
  end;
  Browser.Message('Fim da leitura dos posicionamentos...');
end;

procedure TParser.LerXMI(Modelo, Arquivo : string); begin
  fModelo_Global := ModeloPorNomeList.Find(Modelo);
  if fModelo_Global <> nil then begin
    EExtP.CreateFmt('Modelo %s já existe. Importação abortada', [Modelo]);
    exit;
  end;
  IsInternalPackage:=false;
  Assign(XMI, Arquivo);
  XMIFile := Arquivo;
  SetTextBuf(XMI, Buf);
  reset(XMI);
  LerTagsGerais;
  reset(XMI);
  NextToken(['<UML:Package ']);
  LerPackage;
  try
    repeat
      case NextToken(['<UML:Package ', '<UML:Class ', '<UML:Association ', '<UML:Comment', '<UML:Generalization ', '<UML:StateMachine.transitions',
                      '<UML:StateMachine.top', '<UML:Diagram ', '<EAStub xmi', '</XMI>']) of
        0 : LerPackage;
        1 : LerClasse;
        2 : LerAssociacao;
        3 : LerComentario;
        4 : LerHeranca;
        5 : LerTransicoes;
        6 : LerEstados;
        7 : LerDiagramaMaquinaEstados;
        8 : LerLink;
        9 : break;
        else begin
          Browser.Message('Erro na Importacao do Modelo');
          break;
        end;
      end;
    until false;
    LoadPosition;
  except
    on E: Exception do ModelError(true, E.Message);
  end;
  if Error then GlobalError := true;
  close(XMI);
  DeleteFile(pchar('UML_EA.DTD'));
  DeleteFile(pchar(ChangeFileExt(XMIFile, '.deb')));
  DeleteFile(pchar(ChangeFileExt(XMIFile, '_export.log')));
end;
end.
