{$A1,B-,C-,D-,E-,F-,G+,H+,I-,J+,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V-,W-,X+,Y-,Z1}
{$IFDEF FPC}{$PACKSET 1}{$PACKRECORDS 1}{$Z1}{$ENDIF}
{$IFDEF DEBUG}{$D+,O-,A1,I+,W+,V-,R+,B-,Q+,S+,X+,P+,H+,J+,L+,Y+}{$ENDIF}
unit Designer;
{
Gerado por pitinnu versão 1.0
Usuário : wander
Máquina : NOTEBOOK-WANDER
}

interface

uses
  Classes, SysUtils, epUtils, epObjectList, epPrevalence, epWorkflow, ExtPascalUMLModel;

// Methods
function Elemento_ChecarNome(Self : TElemento) : Boolean;
function Elemento_DescreverAssociacao(Self : TElemento; pAssociacao : TAssociation) : String;
function Elemento_ExisteNomeDuplicado(Self : TElemento; pLista : TObjectList) : Boolean;
function ElementoTipado_GetIdentification(Self : TElementoTipado) : String;
function EstadoBase_CalcularPosicao(Self : TEstadoBase) : Integer;
function EstadoBase_ChecarNome(Self : TEstadoBase) : Boolean;
function EstadoBase_GetIdentification(Self : TEstadoBase) : String;
function Link_ContarHeranca(Self : TLink) : String;
procedure Metodo_EditarDiagrama(Self : TMetodo);
function Metodo_FormatarCaracteristicas(Self : TMetodo) : String;
function Metodo_FormatarEscopo(Self : TMetodo) : String;
function Metodo_FormatarParametros(Self : TMetodo) : String;
function Metodo_FormatarRetorno(Self : TMetodo) : String;
function Metodo_GetIdentification(Self : TMetodo) : String;
procedure Modelo_DispararServer;
procedure Modelo_GerarServer;
procedure Modelo_ImportarModelo(Modelo : String = 'C:\Trabalho\ExtPascal\ExtPascalUML\ExtPascalUML.EAP');
procedure Pacote_CopiarPacote(Self : TPacote);
procedure Pacote_EditarDiagrama(Self : TPacote);
procedure Pacote_SelecionarObjeto(Self : TClassPacote; pObjectPath : String);
function Papel_DescreverMultiplicidade(Self : TPapel) : String;
function Papel_FormatarConstraints(Self : TPapel) : String;
function Papel_GetIdentification(Self : TPapel) : String;
function Transicao_ChecarNome(Self : TTransicao) : Boolean;
function Transicao_GetIdentification(Self : TTransicao) : String;
function Classe_ChecarNome(Self : TClasse) : Boolean;
function Classe_GetIdentification(Self : TClasse) : String;
function Estado_GetIdentification(Self : TEstado) : String;
function Variavel_ChecarNome(Self : TVariavel) : Boolean;
function Variavel_GetIdentification(Self : TVariavel) : String;
function Atributo_ChecarNome(Self : TAtributo) : Boolean;
function Atributo_FormatarCaracteristicas(Self : TAtributo) : String;
function Atributo_FormatarDerivado(Self : TAtributo) : String;
function Atributo_FormatarEscopo(Self : TAtributo) : String;
function Atributo_GetIdentification(Self : TAtributo) : String;
function Parametro_ChecarNome(Self : TParametro) : Boolean;

implementation

uses
  TypInfo, StrUtils, DateUtils, Math, //MaskUtils,
  epCommon, epStateMachine, epThread, epImporter, epGenerator, epDesignerServer;

type
  T_Ajuda = class(TAjuda); T_ClassAjuda = class of T_Ajuda;
  T_Aparencia = class(TAparencia); T_ClassAparencia = class of T_Aparencia;
  T_Associacao = class(TAssociacao); T_ClassAssociacao = class of T_Associacao;
  T_Comentario = class(TComentario); T_ClassComentario = class of T_Comentario;
  T_Compilador = class(TCompilador); T_ClassCompilador = class of T_Compilador;
  T_Elemento = class(TElemento); T_ClassElemento = class of T_Elemento;
  T_Formatacao = class(TFormatacao); T_ClassFormatacao = class of T_Formatacao;
  T_Geracao = class(TGeracao); T_ClassGeracao = class of T_Geracao;
  T_Heranca = class(THeranca); T_ClassHeranca = class of T_Heranca;
  T_ParametroInicializacao = class(TParametroInicializacao); T_ClassParametroInicializacao = class of T_ParametroInicializacao;
  T_Servidor = class(TServidor); T_ClassServidor = class of T_Servidor;
  T_Versao = class(TVersao); T_ClassVersao = class of T_Versao;
  T_ElementoTipado = class(TElementoTipado); T_ClassElementoTipado = class of T_ElementoTipado;
  T_EstadoBase = class(TEstadoBase); T_ClassEstadoBase = class of T_EstadoBase;
  T_Indice = class(TIndice); T_ClassIndice = class of T_Indice;
  T_Link = class(TLink); T_ClassLink = class of T_Link;
  T_Metodo = class(TMetodo); T_ClassMetodo = class of T_Metodo;
  T_Modelo = class(TModelo); T_ClassModelo = class of T_Modelo;
  T_Pacote = class(TPacote); T_ClassPacote = class of T_Pacote;
  T_Papel = class(TPapel); T_ClassPapel = class of T_Papel;
  T_Transicao = class(TTransicao); T_ClassTransicao = class of T_Transicao;
  T_Classe = class(TClasse); T_ClassClasse = class of T_Classe;
  T_Estado = class(TEstado); T_ClassEstado = class of T_Estado;
  T_Formulario = class(TFormulario); T_ClassFormulario = class of T_Formulario;
  T_Variavel = class(TVariavel); T_ClassVariavel = class of T_Variavel;
  T_Validacao = class(TValidacao); T_ClassValidacao = class of T_Validacao;
  T_Atributo = class(TAtributo); T_ClassAtributo = class of T_Atributo;
  T_Parametro = class(TParametro); T_ClassParametro = class of T_Parametro;

// Cyclomatic Complexity: 0, Baixo
function Elemento_ChecarNome(Self : TElemento) : Boolean;
begin with T_Elemento(Self) do begin
Result := IsValidIdent(Nome)
end end;

// Cyclomatic Complexity: 0, Baixo
function Elemento_DescreverAssociacao(Self : TElemento; pAssociacao : TAssociation) : String;
var
  lObj: TTransient;
begin with T_Elemento(Self) do begin
  Result := '';
  lObj := pAssociacao.First;
  while Assigned(lObj) do begin
    AddToStr(Result, TPrevalent(lObj).GetIdentification);
    pAssociacao.Next(lObj);
  end;
end; end;

// Cyclomatic Complexity: 0, Baixo
function Elemento_ExisteNomeDuplicado(Self : TElemento; pLista : TObjectList) : Boolean;
var
  lElemento : TElemento;
begin with T_Elemento(Self) do begin
  Result := False;
  if not assigned(pLista) then exit;
  lElemento := T_Elemento(pLista.First); 
  while assigned(lElemento) do begin
    if (lElemento <> Self) and SameText(lElemento.Nome, Nome) then begin
      Result := True;
      exit;
    end;
    pLista.Next(TTransient(lElemento));
  end;
end; end;

// Cyclomatic Complexity: 0, Baixo
function ElementoTipado_GetIdentification(Self : TElementoTipado) : String;
begin with T_ElementoTipado(Self) do begin
case Tipo of
    ettFaixa,
    ettOutros : Result := Valores;
    ettClasse :
      if TipoClasse = nil then
        Result := ''
      else
        Result := TipoClasse.Nome;
    ettLista  : Result := 'Lista de ' + Valores;
  else
    Result := EnumToStr(TypeInfo(TElementoTipadoTipo), Integer(Tipo));
  end;
end end;

// Cyclomatic Complexity: 0, Baixo
function EstadoBase_CalcularPosicao(Self : TEstadoBase) : Integer;
begin with T_EstadoBase(Self) do begin
if Aparencia <> nil then
  Result := Aparencia.X + (1000 * Aparencia.Y)
else
  Result := 0;
end end;

// Cyclomatic Complexity: 0, Baixo
function EstadoBase_ChecarNome(Self : TEstadoBase) : Boolean;
begin with T_EstadoBase(Self) do begin
Result := true
end end;

// Cyclomatic Complexity: 0, Baixo
function EstadoBase_GetIdentification(Self : TEstadoBase) : String;
begin with T_EstadoBase(Self) do begin
Result := ifthen(Apelido <> '', Apelido, Nome)
end end;

// Cyclomatic Complexity: 0, Baixo
function Link_ContarHeranca(Self : TLink) : String;
var
  I: integer;
  lHeranca : THeranca;
begin with T_Link(Self) do begin
  I := 0;
  lHeranca := Pai;
  while assigned(lHeranca) do begin
    inc(I);
    lHeranca := lHeranca.Destino.Pai;
  end;
  str(I:4, Result);
  Result := Result + Nome
end; end;

// Cyclomatic Complexity: 0, Baixo
procedure Metodo_EditarDiagrama(Self : TMetodo);
begin with T_Metodo(Self) do begin
//Browser.EditDiagram(udkSTM, id)
end end;

// Cyclomatic Complexity: 0, Baixo
function Metodo_FormatarCaracteristicas(Self : TMetodo) : String;
var
  Mc : TMetodoCaracteristicas;
  S : string;
begin with T_Metodo(Self) do begin
  Result := '';
  if Caracteristicas <> [] then
    for Mc := low(Mc) to high(Mc) do
      if Mc in Caracteristicas then begin
        S := copy(GetEnumName(TypeInfo(TMetodoCaracteristicas), ord(Mc)), 3, 100);
        if Result = '' then
          Result := S
        else
          Result := Result + ', ' + S
      end;
  if Tipo <> mtMaquinaDeEstados then begin
    S := copy(GetEnumName(TypeInfo(TMetodoTipo), ord(Tipo)), 3, 100);
    if Result = '' then
      Result := S
    else
      Result := Result + ', ' + S
  end;
  if Result <> '' then
    Result := '«' + Result + '» '
end; end;

// Cyclomatic Complexity: 0, Baixo
function Metodo_FormatarEscopo(Self : TMetodo) : String;
begin with T_Metodo(Self) do begin
case Escopo of
      aePublished : Result := '~';
      aePublic    : Result := '+';
      aeProtected : Result := '#';
    else
      Result := '-';
    end;
end end;

// Cyclomatic Complexity: 0, Baixo
function Metodo_FormatarParametros(Self : TMetodo) : String;
var
    ParametroAtual : TParametro;
  begin with T_Metodo(Self) do begin
    Result := '';
    ParametroAtual := Parametros.First ;
    while ParametroAtual <> nil do begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + ParametroAtual.GetIdentification;
      Parametros.Next(ParametroAtual);
    end;
    if Result <> '' then
      Result := '(' + Result + ')';
  end; end;

// Cyclomatic Complexity: 0, Baixo
function Metodo_FormatarRetorno(Self : TMetodo) : String;
begin with T_Metodo(Self) do begin
if Retorno <> nil then
      Result := ': ' + Retorno.GetIdentification
    else
      Result := '';
end end;

// Cyclomatic Complexity: 0, Baixo
function Metodo_GetIdentification(Self : TMetodo) : String;
begin with T_Metodo(Self) do begin
try Result := FormatarEscopo + ' ' + FormatarCaracteristicas + Nome + FormatarParametros + FormatarRetorno; except Result := ' ' end;
end end;

// Cyclomatic Complexity: 0, Baixo
procedure Modelo_DispararServer;
type
  TContext = record
    This : TModelo;
    fGeracao : TGeracao;
  end;
var
  Context : TContext;
  StateMachine : TStateMachine;

procedure RecuperaGeracao_(var Context : TContext); begin
  with Context, T_Modelo(This) do fGeracao := GeracaoPorUsuarioList.Find(Browser.UserInfo.UserName)
end;

procedure AtualizaOpcoes_(var Context : TContext); begin
  with Context, T_Modelo(This) do Browser.Edit(fGeracao, 'Modelo, Ambiente')
end;

procedure DisparaServer_(var Context : TContext); begin
  with Context, T_Modelo(This) do DisparaObjectServer(fGeracao)
end;

function SemGeracao_(var Context : TContext) : boolean; begin
  with Context, T_Modelo(This) do begin
    Result := fGeracao = nil;
    if Result then fGeracao := TGeracao.Create;
  end;
end;

begin
  fillchar(Context, sizeof(Context), 0);
  StateMachine := TStateMachine.Create('Designer', 'Modelo', 'DispararServer', [1, 2, 1, 1]);
  with StateMachine do begin
    SetState(0, 'Declaracoes', nil);
    SetState(1, 'RecuperaGeracao', @RecuperaGeracao_);
    SetState(2, 'AtualizaOpcoes', @AtualizaOpcoes_);
    SetState(3, 'DisparaServer', @DisparaServer_);
    SetTransition(0, 0, 'Default', nil, 1);
    SetTransition(1, 0, 'SemGeracao', @SemGeracao_, 2);
    SetTransition(1, 1, 'Default', nil, 2);
    SetTransition(2, 0, 'Default', nil, 3);
    SetTransition(3, 0, 'Default', nil, -1);
  end;
  try
    inc(CallStateMachineCount);
    try
      BeginTransaction;
      StateMachine.Execute(@Context, true);
      if Threadtrans <> nil then EndTransaction;
    except
      Rollback;
    end;
  finally
    dec(CallStateMachineCount);
    StateMachine.Free;
  end;
end;

// Cyclomatic Complexity: 0, Baixo
procedure Modelo_GerarServer;
type
  TContext = record
    This : TModelo;
    fGeracao : TGeracao;
  end;
var
  Context : TContext;
  StateMachine : TStateMachine;

procedure RecuperaGeracao_(var Context : TContext); begin
  with Context, T_Modelo(This) do fGeracao := GeracaoPorUsuarioList.Find(Browser.UserInfo.UserName)
end;

procedure AtualizaOpcoes_(var Context : TContext); begin
  with Context, T_Modelo(This) do Browser.Edit(fGeracao, 'Modelo, Ambiente, Versao, Compilador, Opcoes')
end;

procedure GeraServer_(var Context : TContext); begin
  with Context, T_Modelo(This) do GerarObjectServer(fGeracao)
end;

function SemGeracao_(var Context : TContext) : boolean; begin
  with Context, T_Modelo(This) do begin
    Result := fGeracao = nil;
    if Result then fGeracao := TGeracao.Create;
  end;
end;

begin
  fillchar(Context, sizeof(Context), 0);
  StateMachine := TStateMachine.Create('Designer', 'Modelo', 'GerarServer', [1, 2, 1, 1]);
  with StateMachine do begin
    SetState(0, 'Declaracoes', nil);
    SetState(1, 'RecuperaGeracao', @RecuperaGeracao_);
    SetState(2, 'AtualizaOpcoes', @AtualizaOpcoes_);
    SetState(3, 'GeraServer', @GeraServer_);
    SetTransition(0, 0, 'Default', nil, 1);
    SetTransition(1, 0, 'SemGeracao', @SemGeracao_, 2);
    SetTransition(1, 1, 'Default', nil, 2);
    SetTransition(2, 0, 'Default', nil, 3);
    SetTransition(3, 0, 'Default', nil, -1);
  end;
  try
    inc(CallStateMachineCount);
    try
      BeginTransaction;
      StateMachine.Execute(@Context, true);
      if Threadtrans <> nil then EndTransaction;
    except
      Rollback;
    end;
  finally
    dec(CallStateMachineCount);
    StateMachine.Free;
  end;
end;

// Cyclomatic Complexity: 0, Baixo
procedure Modelo_ImportarModelo(Modelo : String);
type
  TContext = record
    This : TModelo;
    Modelo : String;
  end;
var
  Context : TContext;
  StateMachine : TStateMachine;

procedure DisparaImportacao_(var Context : TContext); begin
  with Context, T_Modelo(This) do CarregaModelo(Modelo)
end;

begin
  fillchar(Context, sizeof(Context), 0);
  Context.Modelo := Modelo;
  StateMachine := TStateMachine.Create('Designer', 'Modelo', 'ImportarModelo', [1]);
  with StateMachine do begin
    SetState(0, 'DisparaImportacao', @DisparaImportacao_);
    SetTransition(0, 0, 'Default', nil, -1);
  end;
  try
    inc(CallStateMachineCount);
    try
      BeginTransaction;
      StateMachine.Execute(@Context, true);
      if Threadtrans <> nil then EndTransaction;
    except
      Rollback;
    end;
  finally
    dec(CallStateMachineCount);
    StateMachine.Free;
  end;
end;

// Cyclomatic Complexity: 0, Baixo
procedure Pacote_CopiarPacote(Self : TPacote);
type
  TContext = record
    This : TPacote;
    fNovoPacote : TPacote;
  end;
var
  Context : TContext;
  StateMachine : TStateMachine;

procedure CriaNovoPacote_(var Context : TContext); begin
  with Context, T_Pacote(This) do begin
    TPrevalent(fNovoPacote) := T_Pacote(This).CopyObjectCascade;
Browser.Edit(fNovoPacote)
  end;
end;

begin
  fillchar(Context, sizeof(Context), 0);
  Context.This := Self;
  StateMachine := TStateMachine.Create('Designer', 'Pacote', 'CopiarPacote', [1, 1, 1]);
  with StateMachine do begin
    SetState(0, 'Declaracoes', nil);
    SetState(1, 'CriaNovoPacote', @CriaNovoPacote_);
    SetState(2, 'DuplicaLigacoes', nil);
    SetTransition(0, 0, 'Default', nil, 1);
    SetTransition(1, 0, 'Default', nil, 2);
    SetTransition(2, 0, 'Default', nil, -1);
  end;
  try
    inc(CallStateMachineCount);
    try
      BeginTransaction;
      StateMachine.Execute(@Context, true);
      if Threadtrans <> nil then EndTransaction;
    except
      Rollback;
    end;
  finally
    dec(CallStateMachineCount);
    StateMachine.Free;
  end;
end;

// Cyclomatic Complexity: 0, Baixo
procedure Pacote_EditarDiagrama(Self : TPacote);
begin with T_Pacote(Self) do begin
//Browser.EditDiagram(udkClass, id)
end end;

// Cyclomatic Complexity: 0, Baixo
procedure Pacote_SelecionarObjeto(Self : TClassPacote; pObjectPath : String);
var
  lClass: TClasse;
  lMetodo: TMetodo;
  lSelect, lFind: string;
  lEditObject: TPrevalent;
  I: integer;
begin with T_ClassPacote(Self) do begin
  I := 1;
  lClass := ClassePorNomeList.Find(ExtractFromStr(pObjectPath, I, '.'));
  if lClass = nil then exit;
  lFind := ExtractFromStr(pObjectPath, I, '.');
  lMetodo := lClass.Metodos.Find(lFind);
  if Assigned(lMetodo) and ((Trim(lMetodo.Acao) = '') or (lMetodo.Estados.First <> nil)) then
//    Browser.EditDiagram(udkSTM, lMetodo.ID, ExtractFromStr(pObjectPath, I, '.') {nome do estado})
  else begin
    if Assigned(lMetodo) then 
      lEditObject := lMetodo
    else 
      lEditObject :=TPrevalent( lClass.Atributos.Scan('Nome', lFind));
    if Assigned(lEditObject) then
      lSelect := lClass.Nome + ':' + lEditObject.ClassName + ':' + IntToStr(lEditObject.ID)
    else 
      lSelect := lClass.Nome;
//    Browser.EditDiagram(udkClass, lClass.Pacote.ID, lSelect);
  end;
end; end;

// Cyclomatic Complexity: 0, Baixo
function Papel_DescreverMultiplicidade(Self : TPapel) : String;
const
  StrsMultiplicidade: array[TPapelMultiplicidade] of string =
    ('0..*', '0..1', '1', '1..*', '');
begin with T_Papel(Self) do begin
  Result := StrsMultiplicidade[Multiplicidade];
  if Result = '' then
    if Maximo= 65535 then
       Result := IntToStr(Minimo) + '..*'
    else
       Result := IntToStr(Minimo) + '..' + IntToStr(Maximo);
end; end;

// Cyclomatic Complexity: 0, Baixo
function Papel_FormatarConstraints(Self : TPapel) : String;
begin with T_Papel(Self) do begin
Result := '';
if Ordem <> nil then
  Result := ': «' + Ordem.Nome + '»';
if Constraint <> '' then
  Result := Result + ': {' + Constraint + '}';
end end;

// Cyclomatic Complexity: 0, Baixo
function Papel_GetIdentification(Self : TPapel) : String;
begin with T_Papel(Self) do begin
Result := Nome + ': ' + DescreverMultiplicidade + FormatarConstraints
end end;

// Cyclomatic Complexity: 0, Baixo
function Transicao_ChecarNome(Self : TTransicao) : Boolean;
begin with T_Transicao(Self) do begin
Result := true
end end;

// Cyclomatic Complexity: 0, Baixo
function Transicao_GetIdentification(Self : TTransicao) : String;
begin with T_Transicao(Self) do begin
if Apelido <> '' then
    Result := Apelido
  else
    Result := Nome;
  if Condicao <> '' then
    Result := Result + #13'[' + Condicao + ']';
  if Acao <> '' then
    Result := Result + #13'/' + Acao;
end end;

// Cyclomatic Complexity: 0, Baixo
function Classe_ChecarNome(Self : TClasse) : Boolean;
begin with T_Classe(Self) do begin
Result:=Elemento_ChecarNome(Self) and not ExisteNomeDuplicado(Pacote.Classes)
end end;

// Cyclomatic Complexity: 0, Baixo
function Classe_GetIdentification(Self : TClasse) : String;
var
  Indice : TIndice;
begin with T_Classe(Self) do begin
  Result := '';
  Indice := Indices.First;
  while Indice <> nil do begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + Indice.Nome;
    Indices.Next(Indice)
  end;
  if Result <> '' then
    Result := ' {' + Result + '}';
  Result := Nome + Result;
end; end;

// Cyclomatic Complexity: 0, Baixo
function Estado_GetIdentification(Self : TEstado) : String;
begin with T_Estado(Self) do begin
Result := IfThen(Subflow, '«Subflow»'#13, '' ) + EstadoBase_GetIdentification(Self)
end end;

// Cyclomatic Complexity: 0, Baixo
function Variavel_ChecarNome(Self : TVariavel) : Boolean;
begin with T_Variavel(Self) do begin
Result := Elemento_ChecarNome(Self) and 
  ((Metodo = nil) or 
  (not ExisteNomeDuplicado(Metodo.Variaveis)) and 
  (not ExisteNomeDuplicado(Metodo.Classe.Atributos)) and 
  (not ExisteNomeDuplicado(Metodo.Parametros))) and 
  ((Estado = nil) or 
  (not ExisteNomeDuplicado(Estado.Variaveis)))
end end;

// Cyclomatic Complexity: 0, Baixo
function Variavel_GetIdentification(Self : TVariavel) : String;
begin with T_Variavel(Self) do begin
try Result := Nome + ': ' + ElementoTipado_GetIdentification(Self) + IfThen(Inicial = '', '', ' = ' + Inicial) except Result := ' ' end;
end end;

// Cyclomatic Complexity: 0, Baixo
function Atributo_ChecarNome(Self : TAtributo) : Boolean;
begin with T_Atributo(Self) do begin
Result := Variavel_ChecarNome(Self) and not ExisteNomeDuplicado(Classe.Atributos)
end end;

// Cyclomatic Complexity: 0, Baixo
function Atributo_FormatarCaracteristicas(Self : TAtributo) : String;
var
  C : TAtributoCaracteristicas;
  S : string;
begin with T_Atributo(Self) do begin
  Result := '';
  for C := acNaoNulo to acSobreposto do
    if C in Caracteristicas then begin
      S := copy(GetEnumName(TypeInfo(TAtributoCaracteristicas), ord(C)), 3, 100);
      if Result = '' then
        Result := S
      else
        Result := Result + ', ' + S
    end;
  if Result <> '' then
    Result := '«' + Result + '» '
end; end;

// Cyclomatic Complexity: 0, Baixo
function Atributo_FormatarDerivado(Self : TAtributo) : String;
begin with T_Atributo(Self) do begin
if ACDerivado in Caracteristicas then
       result := '/'
     else
        result := '';
end end;

// Cyclomatic Complexity: 0, Baixo
function Atributo_FormatarEscopo(Self : TAtributo) : String;
begin with T_Atributo(Self) do begin
case Escopo of
      aePublished : Result := '~';
      aePublic    : Result := '+';
      aeProtected : Result := '#';
    else
      Result := '-';
    end;
end end;

// Cyclomatic Complexity: 0, Baixo
function Atributo_GetIdentification(Self : TAtributo) : String;
begin with T_Atributo(Self) do begin
try Result := formatarEscopo + formatarDerivado + ' ' + formatarCaracteristicas + Variavel_GetIdentification(Self) except Result := ' ' end;
end end;

// Cyclomatic Complexity: 0, Baixo
function Parametro_ChecarNome(Self : TParametro) : Boolean;
begin with T_Parametro(Self) do begin
Result:= Variavel_ChecarNome(Self) and ((Metodo<> nil) and (not ExisteNomeDuplicado(Metodo.Parametros)) or (not ExisteNomeDuplicado(Metodo.Variaveis)) or (not ExisteNomeDuplicado(Metodo.Classe.Atributos)))
end end;
end.
