{$A1,B-,C-,D-,E-,F-,G+,H+,I-,J+,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V-,W-,X+,Y-,Z1}
{$IFDEF FPC}{$PACKSET 1}{$PACKRECORDS 1}{$Z1}{$ENDIF}
{$IFDEF DEBUG}{$D+,O-,A1,I+,W+,V-,R+,B-,Q+,S+,X+,P+,H+,J+,L+,Y+}{$ENDIF}
unit ExtPascalUMLModel;
{
Gerado por pitinnu versão 1.0
Usuário : wander
Máquina : NOTEBOOK-WANDER
}

interface

uses
  Classes, SysUtils, epUtils, epObjectList, epPrevalence, epWorkflow, epGDB;

type
  TAjuda = class; TClassAjuda = class of TAjuda;
  TAparencia = class; TClassAparencia = class of TAparencia;
  TAssociacao = class; TClassAssociacao = class of TAssociacao;
  TComentario = class; TClassComentario = class of TComentario;
  TCompilador = class; TClassCompilador = class of TCompilador;
  TElemento = class; TClassElemento = class of TElemento;
  TFormatacao = class; TClassFormatacao = class of TFormatacao;
  TGeracao = class; TClassGeracao = class of TGeracao;
  THeranca = class; TClassHeranca = class of THeranca;
  TParametroInicializacao = class; TClassParametroInicializacao = class of TParametroInicializacao;
  TServidor = class; TClassServidor = class of TServidor;
  TVersao = class; TClassVersao = class of TVersao;
  TElementoTipado = class; TClassElementoTipado = class of TElementoTipado;
  TEstadoBase = class; TClassEstadoBase = class of TEstadoBase;
  TIndice = class; TClassIndice = class of TIndice;
  TLink = class; TClassLink = class of TLink;
  TMetodo = class; TClassMetodo = class of TMetodo;
  TModelo = class; TClassModelo = class of TModelo;
  TPacote = class; TClassPacote = class of TPacote;
  TPapel = class; TClassPapel = class of TPapel;
  TTransicao = class; TClassTransicao = class of TTransicao;
  TClasse = class; TClassClasse = class of TClasse;
  TEstado = class; TClassEstado = class of TEstado;
  TFormulario = class; TClassFormulario = class of TFormulario;
  TVariavel = class; TClassVariavel = class of TVariavel;
  TValidacao = class; TClassValidacao = class of TValidacao;
  TAtributo = class; TClassAtributo = class of TAtributo;
  TParametro = class; TClassParametro = class of TParametro;

  TClasseIndicesAssociation = class;
  TFormularioFormatacoesAssociation = class;
  TPacoteClassesAssociation = class;
  TPacoteComentariosAssociation = class;
  TPacoteModeloAssociation = class;
  TModeloPacotesAssociation = class;
  TServidorInicializacaoAssociation = class;
  TEstadoBaseDestinosAssociation = class;
  TMetodoEstadosAssociation = class;
  TEstadoBaseOrigensAssociation = class;
  TEstadoVariaveisAssociation = class;
  TMetodoVariaveisAssociation = class;
  TLinkAssociacoesDestinoAssociation = class;
  TLinkAssociacoesOrigemAssociation = class;
  TMetodoComentariosAssociation = class;
  TMetodoParametrosAssociation = class;
  TLinkHerancasAssociation = class;
  TClasseMetodosAssociation = class;
  TClasseAtributosAssociation = class;
  TClasseLinksAssociation = class;
  TModeloGeracoesAssociation = class;

  // Enumeration(s) for TFormatacao
  TFormatacaoComando = (fcEditar, fcMostrar, fcCriarGrupo, fcCriarGrupoColapsavel, fcCriarGrupoColapsado, fcCriarAba, fcFecharAbas);
  TFormatacaoLayout = (flPadrao, flIniciarNaMargemEsquerda, flOcuparTodaLargura);

  // Enumeration(s) for TGeracao
  TGeracaoAmbiente = (gaDesenvolvimento, gaTeste, gaHomologacao, gaProducao);

  // Enumeration(s) for TElementoTipado
  TElementoTipadoTipo = (ettTexto, ettNumero, ettClasse, ettConjunto, ettData, ettDataHora, ettEnumeracao, ettFaixa, ettHora, ettLista, ettLogico, ettMonetario, ettOutros);

  // Enumeration(s) for TIndice
  TIndiceTipo = (itTexto, itNumero, itData, itDataHora, itHora, itMonetario);

  // Enumeration(s) for TMetodo
  TMetodoTipo = (mtMaquinaDeEstados, mtWorkflow, mtWizard, mtVisaoDeSeguranca);

  // Enumeration(s) for TPacote
  TPacoteAmbiente = (paDesenvolvimento, paHomologacao, paTeste, paProducao);

  // Enumeration(s) for TPapel
  TPapelAgregacao = (paNenhuma, paFraca, paForte);
  TPapelMultiplicidade = (pm0_N, pm0_1, pm1, pm1_N, pmOutras);

  // Enumeration(s) for TClasse
  TClasseTipo = (ctPrevalente, ctAbstrata, ctTransiente, ctNaoVersionada);
  TClasseApresentacao = (caGrade, caCartao);

  // Enumeration(s) for TEstado
  TEstadoTipo = (etPadrao, etInicial, etFinal, etJuncao, etForkv, etForkh);

  // Enumeration(s) for TAtributo
  TAtributoEscopo = (aePublished, aePrivate, aeProtected, aePublic);
  TAtributoOrdem = 0..9999;

  // Enumeration(s) for TParametro
  TParametroPassagem = (ppEntrada, ppEntradaSaida, ppSaida);

  // Set(s) for TGeracao
  TGeracaoOpcoes = (goAudit, goDebugger, goMetrics, goProfiler, goSecurity, goShowID, goTracer);
  TSetGeracaoOpcoes = set of TGeracaoOpcoes;

  // Set(s) for TParametroInicializacao
  TParametroInicializacaoAplicavel = (piaBrowser, piaServer);
  TSetParametroInicializacaoAplicavel = set of TParametroInicializacaoAplicavel;

  // Set(s) for TMetodo
  TMetodoCaracteristicas = (mcDeClasse, mcVirtual, mcOculto, mcSobreposto);
  TSetMetodoCaracteristicas = set of TMetodoCaracteristicas;

  // Set(s) for TPapel
  TPapelCaracteristicas = (pcOculto, pcSopreposto, pcSomenteLeitura);
  TSetPapelCaracteristicas = set of TPapelCaracteristicas;

  // Set(s) for TAtributo
  TAtributoCaracteristicas = (acDerivado, acSomenteLeitura, acNaoNulo, acOculto, acSobreposto);
  TSetAtributoCaracteristicas = set of TAtributoCaracteristicas;

  TAjudaList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Ajuda : TAjuda);
    procedure Delete(Ajuda : TAjuda);
    function First : TAjuda;
    function Last : TAjuda;
    function Next(var Ajuda : TAjuda) : boolean;
    function Prior(var Ajuda : TAjuda) : boolean;
    function Find(I : Integer) : TAjuda;
    function Near(I : Integer) : TAjuda;
  end;

  TAjuda = class(TPrevalent)
  private
    _Dica : String;
    _Texto : String;
    _Elemento : TElemento;
    function  GetDica : String;
    procedure SetDica(Value : String);
    function  GetTexto : String;
    procedure SetTexto(Value : String);
    function  GetElemento : TElemento;
    procedure SetElemento(Value : TElemento);
  published
    property Dica : String read GetDica write SetDica;
    property Texto : String read GetTexto write SetTexto;
    property Elemento : TElemento read GetElemento write SetElemento;
  end;

  TAparenciaList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Aparencia : TAparencia);
    procedure Delete(Aparencia : TAparencia);
    function First : TAparencia;
    function Last : TAparencia;
    function Next(var Aparencia : TAparencia) : boolean;
    function Prior(var Aparencia : TAparencia) : boolean;
    function Find(I : Integer) : TAparencia;
    function Near(I : Integer) : TAparencia;
  end;

  TAparencia = class(TPrevalent)
  private
    _X : Integer;
    _Y : Integer;
    _H : Integer;
    _W : Integer;
    _Cor : Integer;
    function  GetX : Integer;
    procedure SetX(Value : Integer);
    function  GetY : Integer;
    procedure SetY(Value : Integer);
    function  GetH : Integer;
    procedure SetH(Value : Integer);
    function  GetW : Integer;
    procedure SetW(Value : Integer);
    function  GetCor : Integer;
    procedure SetCor(Value : Integer);
  protected
    procedure New; override;
  published
    property X : Integer read GetX write SetX;
    property Y : Integer read GetY write SetY;
    property H : Integer read GetH write SetH;
    property W : Integer read GetW write SetW;
    property Cor : Integer read GetCor write SetCor;
  end;

  TAssociacaoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Associacao : TAssociacao);
    procedure Delete(Associacao : TAssociacao);
    function First : TAssociacao;
    function Last : TAssociacao;
    function Next(var Associacao : TAssociacao) : boolean;
    function Prior(var Associacao : TAssociacao) : boolean;
    function Find(I : Integer) : TAssociacao;
    function Near(I : Integer) : TAssociacao;
  end;

  TAssociacao = class(TPrevalent)
  private
    _PontoOrigem : Byte;
    _PontoDestino : Byte;
    _Quebras : String;
    _PapelDestino : TPapel;
    _PapelOrigem : TPapel;
    _Origem : TLink;
    _Destino : TLink;
    function  GetPontoOrigem : Byte;
    procedure SetPontoOrigem(Value : Byte);
    function  GetPontoDestino : Byte;
    procedure SetPontoDestino(Value : Byte);
    function  GetQuebras : String;
    procedure SetQuebras(Value : String);
    function  GetPapelDestino : TPapel;
    procedure SetPapelDestino(Value : TPapel);
    function  GetPapelOrigem : TPapel;
    procedure SetPapelOrigem(Value : TPapel);
    function  GetOrigem : TLink;
    procedure SetOrigem(Value : TLink);
    function  GetDestino : TLink;
    procedure SetDestino(Value : TLink);
  public
    PropIndex : Word;
  published
    property PontoOrigem : Byte read GetPontoOrigem write SetPontoOrigem;
    property PontoDestino : Byte read GetPontoDestino write SetPontoDestino;
    property Quebras : String read GetQuebras write SetQuebras;
    property PapelDestino : TPapel read GetPapelDestino write SetPapelDestino;
    property PapelOrigem : TPapel read GetPapelOrigem write SetPapelOrigem;
    property Origem : TLink read GetOrigem write SetOrigem;
    property Destino : TLink read GetDestino write SetDestino;
    function CheckPontoOrigem(var Message : String) : Boolean;
  end;

  TComentarioList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Comentario : TComentario);
    procedure Delete(Comentario : TComentario);
    function First : TComentario;
    function Last : TComentario;
    function Next(var Comentario : TComentario) : boolean;
    function Prior(var Comentario : TComentario) : boolean;
    function Find(I : Integer) : TComentario;
    function Near(I : Integer) : TComentario;
  end;

  TComentario = class(TPrevalent)
  private
    _Texto : String;
    _Pacote : TPacote;
    _Aparencia : TAparencia;
    _Metodo : TMetodo;
    function  GetTexto : String;
    procedure SetTexto(Value : String);
    function  GetPacote : TPacote;
    procedure SetPacote(Value : TPacote);
    function  GetAparencia : TAparencia;
    procedure SetAparencia(Value : TAparencia);
    function  GetMetodo : TMetodo;
    procedure SetMetodo(Value : TMetodo);
  published
    property Texto : String read GetTexto write SetTexto;
    property Pacote : TPacote read GetPacote write SetPacote;
    property Aparencia : TAparencia read GetAparencia write SetAparencia;
    property Metodo : TMetodo read GetMetodo write SetMetodo;
  end;

  TCompiladorList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Compilador : TCompilador);
    procedure Delete(Compilador : TCompilador);
    function First : TCompilador;
    function Last : TCompilador;
    function Next(var Compilador : TCompilador) : boolean;
    function Prior(var Compilador : TCompilador) : boolean;
    function Find(I : Integer) : TCompilador;
    function Near(I : Integer) : TCompilador;
  end;

  TCompilador = class(TPrevalent)
  private
    _Nome : String;
    _Executavel : String;
    _Opcoes : String;
    function  GetNome : String;
    procedure SetNome(Value : String);
    function  GetExecutavel : String;
    procedure SetExecutavel(Value : String);
    function  GetOpcoes : String;
    procedure SetOpcoes(Value : String);
  published
    property Nome : String read GetNome write SetNome;
    property Executavel : String read GetExecutavel write SetExecutavel;
    property Opcoes : String read GetOpcoes write SetOpcoes;
  end;

  TElementoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
  end;

  TElemento = class(TPrevalent)
  private
    _Nome : String;
    _Apelido : String;
    _Documentacao : String;
    _Ajuda : TAjuda;
    _Aparencia : TAparencia;
    function  GetNome : String;
    procedure SetNome(Value : String);
    function  GetApelido : String;
    procedure SetApelido(Value : String);
    function  GetDocumentacao : String;
    procedure SetDocumentacao(Value : String);
    function  GetAjuda : TAjuda;
    procedure SetAjuda(Value : TAjuda);
    function  GetAparencia : TAparencia;
    procedure SetAparencia(Value : TAparencia);
  protected
    function DescreverAssociacao(pAssociacao : TAssociation) : String;
    function ExisteNomeDuplicado(pLista : TObjectList) : Boolean;
    function ChecarNome : Boolean; virtual;
  published
    property Nome : String read GetNome write SetNome;
    property Apelido : String read GetApelido write SetApelido;
    property Documentacao : String read GetDocumentacao write SetDocumentacao;
    property Ajuda : TAjuda read GetAjuda write SetAjuda;
    property Aparencia : TAparencia read GetAparencia write SetAparencia;
    function PorNome : String;
  end;

  TFormatacaoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Formatacao : TFormatacao);
    procedure Delete(Formatacao : TFormatacao);
    function First : TFormatacao;
    function Last : TFormatacao;
    function Next(var Formatacao : TFormatacao) : boolean;
    function Prior(var Formatacao : TFormatacao) : boolean;
    function Find(I : Integer) : TFormatacao;
    function Near(I : Integer) : TFormatacao;
  end;

  TFormatacaoPorOrdemList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Formatacao : TFormatacao);
    procedure Delete(Formatacao : TFormatacao);
    function First : TFormatacao;
    function Last : TFormatacao;
    function Next(var Formatacao : TFormatacao) : boolean;
    function Prior(var Formatacao : TFormatacao) : boolean;
    function Find(W : Word) : TFormatacao;
    function Near(W : Word) : TFormatacao;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TFormatacao = class(TPrevalent)
  private
    _Ordem : Word;
    _Comando : TFormatacaoComando;
    _Titulo : String;
    _Propriedade : String;
    _Layout : TFormatacaoLayout;
    _Formulario : TFormulario;
    function  GetOrdem : Word;
    procedure SetOrdem(Value : Word);
    function  GetComando : TFormatacaoComando;
    procedure SetComando(Value : TFormatacaoComando);
    function  GetTitulo : String;
    procedure SetTitulo(Value : String);
    function  GetPropriedade : String;
    procedure SetPropriedade(Value : String);
    function  GetLayout : TFormatacaoLayout;
    procedure SetLayout(Value : TFormatacaoLayout);
    function  GetFormulario : TFormulario;
    procedure SetFormulario(Value : TFormulario);
  published
    property Ordem : Word read GetOrdem write SetOrdem;
    property Comando : TFormatacaoComando read GetComando write SetComando;
    property Titulo : String read GetTitulo write SetTitulo;
    property Propriedade : String read GetPropriedade write SetPropriedade;
    property Layout : TFormatacaoLayout read GetLayout write SetLayout;
    function GetIdentification : string; override;
    property Formulario : TFormulario read GetFormulario write SetFormulario;
    function PorOrdem : Word;
    function CheckOrdem(var Message : String) : Boolean;
    function VisibleTitulo : Boolean;
    function VisiblePropriedade : Boolean;
    function VisibleLayout : Boolean;
  end;

  TGeracaoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Geracao : TGeracao);
    procedure Delete(Geracao : TGeracao);
    function First : TGeracao;
    function Last : TGeracao;
    function Next(var Geracao : TGeracao) : boolean;
    function Prior(var Geracao : TGeracao) : boolean;
    function Find(I : Integer) : TGeracao;
    function Near(I : Integer) : TGeracao;
  end;

  TGeracaoPorUsuarioList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Geracao : TGeracao);
    procedure Delete(Geracao : TGeracao);
    function First : TGeracao;
    function Last : TGeracao;
    function Next(var Geracao : TGeracao) : boolean;
    function Prior(var Geracao : TGeracao) : boolean;
    function Find(S : String) : TGeracao;
    function Near(S : String) : TGeracao;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TGeracaoPorPortaList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Geracao : TGeracao);
    procedure Delete(Geracao : TGeracao);
    function First : TGeracao;
    function Last : TGeracao;
    function Next(var Geracao : TGeracao) : boolean;
    function Prior(var Geracao : TGeracao) : boolean;
    function Find(W : Word) : TGeracao;
    function Near(W : Word) : TGeracao;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TGeracao = class(TPrevalent)
  private
    _Usuario : String;
    _Ambiente : TGeracaoAmbiente;
    _Porta : Word;
    _Opcoes : TSetGeracaoOpcoes;
    _Compilador : TCompilador;
    _Modelo : TModelo;
    function  GetUsuario : String;
    procedure SetUsuario(Value : String);
    function  GetAmbiente : TGeracaoAmbiente;
    procedure SetAmbiente(Value : TGeracaoAmbiente);
    function  GetPorta : Word;
    procedure SetPorta(Value : Word);
    function  GetOpcoes : TSetGeracaoOpcoes;
    procedure SetOpcoes(Value : TSetGeracaoOpcoes);
    function  GetVersao : String;
    function  GetCompilador : TCompilador;
    procedure SetCompilador(Value : TCompilador);
    function  GetModelo : TModelo;
    procedure SetModelo(Value : TModelo);
  protected
    procedure New; override;
  published
    property Usuario : String read GetUsuario write SetUsuario;
    property Ambiente : TGeracaoAmbiente read GetAmbiente write SetAmbiente;
    property Porta : Word read GetPorta write SetPorta;
    property Opcoes : TSetGeracaoOpcoes read GetOpcoes write SetOpcoes;
    property Versao : String read GetVersao;
    property Compilador : TCompilador read GetCompilador write SetCompilador;
    property Modelo : TModelo read GetModelo write SetModelo;
    function PorUsuario : String;
    function PorPorta : Word;
  end;

  THerancaList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Heranca : THeranca);
    procedure Delete(Heranca : THeranca);
    function First : THeranca;
    function Last : THeranca;
    function Next(var Heranca : THeranca) : boolean;
    function Prior(var Heranca : THeranca) : boolean;
    function Find(I : Integer) : THeranca;
    function Near(I : Integer) : THeranca;
  end;

  THeranca = class(TPrevalent)
  private
    _PontoDestino : Byte;
    _PontoOrigem : Byte;
    _Quebras : String;
    _Origem : TLink;
    _Destino : TLink;
    function  GetPontoDestino : Byte;
    procedure SetPontoDestino(Value : Byte);
    function  GetPontoOrigem : Byte;
    procedure SetPontoOrigem(Value : Byte);
    function  GetQuebras : String;
    procedure SetQuebras(Value : String);
    function  GetOrigem : TLink;
    procedure SetOrigem(Value : TLink);
    function  GetDestino : TLink;
    procedure SetDestino(Value : TLink);
  published
    property PontoDestino : Byte read GetPontoDestino write SetPontoDestino;
    property PontoOrigem : Byte read GetPontoOrigem write SetPontoOrigem;
    property Quebras : String read GetQuebras write SetQuebras;
    property Origem : TLink read GetOrigem write SetOrigem;
    property Destino : TLink read GetDestino write SetDestino;
  end;

  TParametroInicializacaoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(ParametroInicializacao : TParametroInicializacao);
    procedure Delete(ParametroInicializacao : TParametroInicializacao);
    function First : TParametroInicializacao;
    function Last : TParametroInicializacao;
    function Next(var ParametroInicializacao : TParametroInicializacao) : boolean;
    function Prior(var ParametroInicializacao : TParametroInicializacao) : boolean;
    function Find(I : Integer) : TParametroInicializacao;
    function Near(I : Integer) : TParametroInicializacao;
  end;

  TParametroInicializacaoPorSessaoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(ParametroInicializacao : TParametroInicializacao);
    procedure Delete(ParametroInicializacao : TParametroInicializacao);
    function First : TParametroInicializacao;
    function Last : TParametroInicializacao;
    function Next(var ParametroInicializacao : TParametroInicializacao) : boolean;
    function Prior(var ParametroInicializacao : TParametroInicializacao) : boolean;
    function Find(S : String) : TParametroInicializacao;
    function Near(S : String) : TParametroInicializacao;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TParametroInicializacao = class(TPrevalent)
  private
    _Aplicavel : TSetParametroInicializacaoAplicavel;
    _Sessao : String;
    _Nome : String;
    _Valor : String;
    function  GetAplicavel : TSetParametroInicializacaoAplicavel;
    procedure SetAplicavel(Value : TSetParametroInicializacaoAplicavel);
    function  GetSessao : String;
    procedure SetSessao(Value : String);
    function  GetNome : String;
    procedure SetNome(Value : String);
    function  GetValor : String;
    procedure SetValor(Value : String);
  published
    property Aplicavel : TSetParametroInicializacaoAplicavel read GetAplicavel write SetAplicavel;
    property Sessao : String read GetSessao write SetSessao;
    property Nome : String read GetNome write SetNome;
    property Valor : String read GetValor write SetValor;
    function PorSessao : String;
  end;

  TServidorList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Servidor : TServidor);
    procedure Delete(Servidor : TServidor);
    function First : TServidor;
    function Last : TServidor;
    function Next(var Servidor : TServidor) : boolean;
    function Prior(var Servidor : TServidor) : boolean;
    function Find(I : Integer) : TServidor;
    function Near(I : Integer) : TServidor;
  end;

  TServidor = class(TPrevalent)
  private
    _RaizGeracao : String;
    _PortaInicial : Word;
    _PortaFinal : Word;
    _Inicializacao : TServidorInicializacaoAssociation;
    _Versao : TVersao;
    function  GetRaizGeracao : String;
    procedure SetRaizGeracao(Value : String);
    function  GetPortaInicial : Word;
    procedure SetPortaInicial(Value : Word);
    function  GetPortaFinal : Word;
    procedure SetPortaFinal(Value : Word);
    function  GetVersao : TVersao;
    procedure SetVersao(Value : TVersao);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property RaizGeracao : String read GetRaizGeracao write SetRaizGeracao;
    property PortaInicial : Word read GetPortaInicial write SetPortaInicial;
    property PortaFinal : Word read GetPortaFinal write SetPortaFinal;
    property Inicializacao : TServidorInicializacaoAssociation read _Inicializacao write _Inicializacao;
    property Versao : TVersao read GetVersao write SetVersao;
  end;

  TServidorInicializacaoAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(ParametroInicializacao : TParametroInicializacao);
    procedure Delete(ParametroInicializacao : TParametroInicializacao);
    function First : TParametroInicializacao;
    function Last : TParametroInicializacao;
    function Next(var ParametroInicializacao : TParametroInicializacao) : boolean;
    function Prior(var ParametroInicializacao : TParametroInicializacao) : boolean;
    function Find(S : String) : TParametroInicializacao;
    function Near(S : String) : TParametroInicializacao;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TVersaoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Versao : TVersao);
    procedure Delete(Versao : TVersao);
    function First : TVersao;
    function Last : TVersao;
    function Next(var Versao : TVersao) : boolean;
    function Prior(var Versao : TVersao) : boolean;
    function Find(I : Integer) : TVersao;
    function Near(I : Integer) : TVersao;
  end;

  TVersao = class(TPrevalent)
  private
    _Numero : String;
    _Path : String;
    function  GetNumero : String;
    procedure SetNumero(Value : String);
    function  GetPath : String;
    procedure SetPath(Value : String);
  published
    property Numero : String read GetNumero write SetNumero;
    property Path : String read GetPath write SetPath;
  end;

  TElementoTipadoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(ElementoTipado : TElementoTipado);
    procedure Delete(ElementoTipado : TElementoTipado);
    function First : TElementoTipado;
    function Last : TElementoTipado;
    function Next(var ElementoTipado : TElementoTipado) : boolean;
    function Prior(var ElementoTipado : TElementoTipado) : boolean;
    function Find(I : Integer) : TElementoTipado;
    function Near(I : Integer) : TElementoTipado;
  end;

  TElementoTipadoPorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(ElementoTipado : TElementoTipado);
    procedure Delete(ElementoTipado : TElementoTipado);
    function First : TElementoTipado;
    function Last : TElementoTipado;
    function Next(var ElementoTipado : TElementoTipado) : boolean;
    function Prior(var ElementoTipado : TElementoTipado) : boolean;
    function Find(S : String) : TElementoTipado;
    function Near(S : String) : TElementoTipado;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TElementoTipado = class(TElemento)
  private
    _Tipo : TElementoTipadoTipo;
    _Tamanho : Word;
    _Decimais : Boolean;
    _Sinalizado : Boolean;
    _Valores : String;
    _Mascara : String;
    _TipoClasse : TClasse;
    function  GetTipo : TElementoTipadoTipo;
    procedure SetTipo(Value : TElementoTipadoTipo);
    function  GetTamanho : Word;
    procedure SetTamanho(Value : Word);
    function  GetDecimais : Boolean;
    procedure SetDecimais(Value : Boolean);
    function  GetSinalizado : Boolean;
    procedure SetSinalizado(Value : Boolean);
    function  GetValores : String;
    procedure SetValores(Value : String);
    function  GetMascara : String;
    procedure SetMascara(Value : String);
    function  GetTipoClasse : TClasse;
    procedure SetTipoClasse(Value : TClasse);
  protected
    procedure New; override;
  public
    function GetIdentification : String; override;
  published
    property Tipo : TElementoTipadoTipo read GetTipo write SetTipo;
    property Tamanho : Word read GetTamanho write SetTamanho;
    property Decimais : Boolean read GetDecimais write SetDecimais;
    property Sinalizado : Boolean read GetSinalizado write SetSinalizado;
    property Valores : String read GetValores write SetValores;
    property Mascara : String read GetMascara write SetMascara;
    property TipoClasse : TClasse read GetTipoClasse write SetTipoClasse;
    function VisibleTamanho : Boolean;
    function VisibleDecimais : Boolean;
    function VisibleSinalizado : Boolean;
    function VisibleValores : Boolean;
    function VisibleMascara : Boolean;
    function CheckTipoClasse(var Message : String) : Boolean;
    function VisibleTipoClasse : Boolean;
  end;

  TEstadoBaseList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
  end;

  TEstadoBase = class(TElemento)
  private
    _Botoes : String;
    _Destinos : TEstadoBaseDestinosAssociation;
    _Metodo : TMetodo;
    _Origens : TEstadoBaseOrigensAssociation;
    function  GetBotoes : String;
    procedure SetBotoes(Value : String);
    function  GetPosicao : Integer;
    function  GetMetodo : TMetodo;
    procedure SetMetodo(Value : TMetodo);
  protected
    procedure New; override;
    procedure InternalFree; override;
    function ChecarNome : Boolean; override;
    function CalcularPosicao : Integer;
  public
    function GetIdentification : String; override;
  published
    property Botoes : String read GetBotoes write SetBotoes;
    property Posicao : Integer read GetPosicao;
    property Destinos : TEstadoBaseDestinosAssociation read _Destinos write _Destinos;
    property Metodo : TMetodo read GetMetodo write SetMetodo;
    property Origens : TEstadoBaseOrigensAssociation read _Origens write _Origens;
    function PorPosicao : Integer;
  end;

  TEstadoBaseDestinosAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Transicao : TTransicao);
    procedure Delete(Transicao : TTransicao);
    function First : TTransicao;
    function Last : TTransicao;
    function Next(var Transicao : TTransicao) : boolean;
    function Prior(var Transicao : TTransicao) : boolean;
    function Find(I : Integer) : TTransicao;
    function Near(I : Integer) : TTransicao;
  protected
    class function GetKeyCode : pointer; override;
  end;

  TEstadoBaseOrigensAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Transicao : TTransicao);
    procedure Delete(Transicao : TTransicao);
    function First : TTransicao;
    function Last : TTransicao;
    function Next(var Transicao : TTransicao) : boolean;
    function Prior(var Transicao : TTransicao) : boolean;
    function Find(I : Integer) : TTransicao;
    function Near(I : Integer) : TTransicao;
  end;

  TIndiceList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Indice : TIndice);
    procedure Delete(Indice : TIndice);
    function First : TIndice;
    function Last : TIndice;
    function Next(var Indice : TIndice) : boolean;
    function Prior(var Indice : TIndice) : boolean;
    function Find(I : Integer) : TIndice;
    function Near(I : Integer) : TIndice;
  end;

  TIndicePorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Indice : TIndice);
    procedure Delete(Indice : TIndice);
    function First : TIndice;
    function Last : TIndice;
    function Next(var Indice : TIndice) : boolean;
    function Prior(var Indice : TIndice) : boolean;
    function Find(S : String) : TIndice;
    function Near(S : String) : TIndice;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TIndice = class(TElemento)
  private
    _Expressao : String;
    _Unico : Boolean;
    _Tipo : TIndiceTipo;
    _Retorno : TElementoTipado;
    _Classe : TClasse;
    function  GetExpressao : String;
    procedure SetExpressao(Value : String);
    function  GetUnico : Boolean;
    procedure SetUnico(Value : Boolean);
    function  GetTipo : TIndiceTipo;
    procedure SetTipo(Value : TIndiceTipo);
    function  GetRetorno : TElementoTipado;
    procedure SetRetorno(Value : TElementoTipado);
    function  GetClasse : TClasse;
    procedure SetClasse(Value : TClasse);
  published
    property Expressao : String read GetExpressao write SetExpressao;
    property Unico : Boolean read GetUnico write SetUnico;
    property Tipo : TIndiceTipo read GetTipo write SetTipo;
    property Retorno : TElementoTipado read GetRetorno write SetRetorno;
    property Classe : TClasse read GetClasse write SetClasse;
  end;

  TLinkList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Link : TLink);
    procedure Delete(Link : TLink);
    function First : TLink;
    function Last : TLink;
    function Next(var Link : TLink) : boolean;
    function Prior(var Link : TLink) : boolean;
    function Find(I : Integer) : TLink;
    function Near(I : Integer) : TLink;
  end;

  TLinkPorHerancaNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Link : TLink);
    procedure Delete(Link : TLink);
    function First : TLink;
    function Last : TLink;
    function Next(var Link : TLink) : boolean;
    function Prior(var Link : TLink) : boolean;
    function Find(S : String) : TLink;
    function Near(S : String) : TLink;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TLinkPorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Link : TLink);
    procedure Delete(Link : TLink);
    function First : TLink;
    function Last : TLink;
    function Next(var Link : TLink) : boolean;
    function Prior(var Link : TLink) : boolean;
    function Find(S : String) : TLink;
    function Near(S : String) : TLink;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TLink = class(TElemento)
  private
    _Pacote : TPacote;
    _AssociacoesDestino : TLinkAssociacoesDestinoAssociation;
    _AssociacoesOrigem : TLinkAssociacoesOrigemAssociation;
    _Pai : THeranca;
    _Herancas : TLinkHerancasAssociation;
    _Classe : TClasse;
    function  GetHerancaNome : String;
    function  GetPacote : TPacote;
    procedure SetPacote(Value : TPacote);
    function  GetPai : THeranca;
    procedure SetPai(Value : THeranca);
    function  GetClasse : TClasse;
    procedure SetClasse(Value : TClasse);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property HerancaNome : String read GetHerancaNome;
    function GetIdentification : string; override;
    property Pacote : TPacote read GetPacote write SetPacote;
    property AssociacoesDestino : TLinkAssociacoesDestinoAssociation read _AssociacoesDestino write _AssociacoesDestino;
    property AssociacoesOrigem : TLinkAssociacoesOrigemAssociation read _AssociacoesOrigem write _AssociacoesOrigem;
    property Pai : THeranca read GetPai write SetPai;
    property Herancas : TLinkHerancasAssociation read _Herancas write _Herancas;
    property Classe : TClasse read GetClasse write SetClasse;
    function PorHerancaNome : String;
    function ContarHeranca : String;
    function ContarHeranca_Int(const Params : TMethodParams) : Variant;
  end;

  TLinkAssociacoesDestinoAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Associacao : TAssociacao);
    procedure Delete(Associacao : TAssociacao);
    function First : TAssociacao;
    function Last : TAssociacao;
    function Next(var Associacao : TAssociacao) : boolean;
    function Prior(var Associacao : TAssociacao) : boolean;
    function Find(I : Integer) : TAssociacao;
    function Near(I : Integer) : TAssociacao;
  end;

  TLinkAssociacoesOrigemAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Associacao : TAssociacao);
    procedure Delete(Associacao : TAssociacao);
    function First : TAssociacao;
    function Last : TAssociacao;
    function Next(var Associacao : TAssociacao) : boolean;
    function Prior(var Associacao : TAssociacao) : boolean;
    function Find(I : Integer) : TAssociacao;
    function Near(I : Integer) : TAssociacao;
  end;

  TLinkHerancasAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Heranca : THeranca);
    procedure Delete(Heranca : THeranca);
    function First : THeranca;
    function Last : THeranca;
    function Next(var Heranca : THeranca) : boolean;
    function Prior(var Heranca : THeranca) : boolean;
    function Find(I : Integer) : THeranca;
    function Near(I : Integer) : THeranca;
  end;

  TMetodoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Metodo : TMetodo);
    procedure Delete(Metodo : TMetodo);
    function First : TMetodo;
    function Last : TMetodo;
    function Next(var Metodo : TMetodo) : boolean;
    function Prior(var Metodo : TMetodo) : boolean;
    function Find(I : Integer) : TMetodo;
    function Near(I : Integer) : TMetodo;
  end;

  TMetodoPorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Metodo : TMetodo);
    procedure Delete(Metodo : TMetodo);
    function First : TMetodo;
    function Last : TMetodo;
    function Next(var Metodo : TMetodo) : boolean;
    function Prior(var Metodo : TMetodo) : boolean;
    function Find(S : String) : TMetodo;
    function Near(S : String) : TMetodo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TMetodo = class(TElemento)
  private
    _Escopo : TAtributoEscopo;
    _Tipo : TMetodoTipo;
    _Acao : String;
    _Caracteristicas : TSetMetodoCaracteristicas;
    _Estados : TMetodoEstadosAssociation;
    _Variaveis : TMetodoVariaveisAssociation;
    _Comentarios : TMetodoComentariosAssociation;
    _Parametros : TMetodoParametrosAssociation;
    _Retorno : TElementoTipado;
    _Classe : TClasse;
    function  GetEscopo : TAtributoEscopo;
    procedure SetEscopo(Value : TAtributoEscopo);
    function  GetTipo : TMetodoTipo;
    procedure SetTipo(Value : TMetodoTipo);
    function  GetAcao : String;
    procedure SetAcao(Value : String);
    function  GetCaracteristicas : TSetMetodoCaracteristicas;
    procedure SetCaracteristicas(Value : TSetMetodoCaracteristicas);
    function  GetTitulo : String;
    function  GetRetorno : TElementoTipado;
    procedure SetRetorno(Value : TElementoTipado);
    function  GetClasse : TClasse;
    procedure SetClasse(Value : TClasse);
  protected
    procedure New; override;
    function FormatarEscopo : String;
    function FormatarParametros : String;
    function FormatarRetorno : String;
    function FormatarCaracteristicas : String;
  public
    function GetIdentification : String; override;
  published
    property Escopo : TAtributoEscopo read GetEscopo write SetEscopo;
    property Tipo : TMetodoTipo read GetTipo write SetTipo;
    property Acao : String read GetAcao write SetAcao;
    property Caracteristicas : TSetMetodoCaracteristicas read GetCaracteristicas write SetCaracteristicas;
    property Titulo : String read GetTitulo;
    property Estados : TMetodoEstadosAssociation read _Estados write _Estados;
    property Variaveis : TMetodoVariaveisAssociation read _Variaveis write _Variaveis;
    property Comentarios : TMetodoComentariosAssociation read _Comentarios write _Comentarios;
    property Parametros : TMetodoParametrosAssociation read _Parametros write _Parametros;
    property Retorno : TElementoTipado read GetRetorno write SetRetorno;
    property Classe : TClasse read GetClasse write SetClasse;
    procedure EditarDiagrama;
    procedure EditarDiagrama_Int(const Params : TMethodParams);
  end;

  TMetodoEstadosAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(EstadoBase : TEstadoBase);
    procedure Delete(EstadoBase : TEstadoBase);
    function First : TEstadoBase;
    function Last : TEstadoBase;
    function Next(var EstadoBase : TEstadoBase) : boolean;
    function Prior(var EstadoBase : TEstadoBase) : boolean;
    function Find(I : Integer) : TEstadoBase;
    function Near(I : Integer) : TEstadoBase;
  protected
    class function GetKeyCode : pointer; override;
  end;

  TMetodoVariaveisAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Variavel : TVariavel);
    procedure Delete(Variavel : TVariavel);
    function First : TVariavel;
    function Last : TVariavel;
    function Next(var Variavel : TVariavel) : boolean;
    function Prior(var Variavel : TVariavel) : boolean;
    function Find(I : Integer) : TVariavel;
    function Near(I : Integer) : TVariavel;
  end;

  TMetodoComentariosAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Comentario : TComentario);
    procedure Delete(Comentario : TComentario);
    function First : TComentario;
    function Last : TComentario;
    function Next(var Comentario : TComentario) : boolean;
    function Prior(var Comentario : TComentario) : boolean;
    function Find(I : Integer) : TComentario;
    function Near(I : Integer) : TComentario;
  end;

  TMetodoParametrosAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Parametro : TParametro);
    procedure Delete(Parametro : TParametro);
    function First : TParametro;
    function Last : TParametro;
    function Next(var Parametro : TParametro) : boolean;
    function Prior(var Parametro : TParametro) : boolean;
    function Find(I : Integer) : TParametro;
    function Near(I : Integer) : TParametro;
  end;

  TModeloList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Modelo : TModelo);
    procedure Delete(Modelo : TModelo);
    function First : TModelo;
    function Last : TModelo;
    function Next(var Modelo : TModelo) : boolean;
    function Prior(var Modelo : TModelo) : boolean;
    function Find(I : Integer) : TModelo;
    function Near(I : Integer) : TModelo;
  end;

  TModeloPorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Modelo : TModelo);
    procedure Delete(Modelo : TModelo);
    function First : TModelo;
    function Last : TModelo;
    function Next(var Modelo : TModelo) : boolean;
    function Prior(var Modelo : TModelo) : boolean;
    function Find(S : String) : TModelo;
    function Near(S : String) : TModelo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TModelo = class(TElemento)
  private
    _Pacotes : TModeloPacotesAssociation;
    _Geracoes : TModeloGeracoesAssociation;
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Pacotes : TModeloPacotesAssociation read _Pacotes write _Pacotes;
    property Geracoes : TModeloGeracoesAssociation read _Geracoes write _Geracoes;
    class procedure ImportarModelo(Modelo : String = 'C:\projects\XDADesigner\XDANew.EAP');
    class procedure ImportarModelo_Int(const Params : TMethodParams);
    class procedure GerarServer;
    class procedure GerarServer_Int(const Params : TMethodParams);
    class procedure DispararServer;
    class procedure DispararServer_Int(const Params : TMethodParams);
  end;

  TModeloPacotesAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Pacote : TPacote);
    procedure Delete(Pacote : TPacote);
    function First : TPacote;
    function Last : TPacote;
    function Next(var Pacote : TPacote) : boolean;
    function Prior(var Pacote : TPacote) : boolean;
    function Find(S : String) : TPacote;
    function Near(S : String) : TPacote;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TModeloGeracoesAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Geracao : TGeracao);
    procedure Delete(Geracao : TGeracao);
    function First : TGeracao;
    function Last : TGeracao;
    function Next(var Geracao : TGeracao) : boolean;
    function Prior(var Geracao : TGeracao) : boolean;
    function Find(I : Integer) : TGeracao;
    function Near(I : Integer) : TGeracao;
  end;

  TPacoteList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Pacote : TPacote);
    procedure Delete(Pacote : TPacote);
    function First : TPacote;
    function Last : TPacote;
    function Next(var Pacote : TPacote) : boolean;
    function Prior(var Pacote : TPacote) : boolean;
    function Find(I : Integer) : TPacote;
    function Near(I : Integer) : TPacote;
  end;

  TPacotePorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Pacote : TPacote);
    procedure Delete(Pacote : TPacote);
    function First : TPacote;
    function Last : TPacote;
    function Next(var Pacote : TPacote) : boolean;
    function Prior(var Pacote : TPacote) : boolean;
    function Find(S : String) : TPacote;
    function Near(S : String) : TPacote;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TPacote = class(TElemento)
  private
    _Ambiente : TPacoteAmbiente;
    _Versao : String;
    _UnidadesExternas : String;
    _Declaracao : String;
    _Inicializacao : String;
    _Finalizacao : String;
    _Classes : TPacoteClassesAssociation;
    _Comentarios : TPacoteComentariosAssociation;
    _Modelo : TPacoteModeloAssociation;
    function  GetAmbiente : TPacoteAmbiente;
    procedure SetAmbiente(Value : TPacoteAmbiente);
    function  GetVersao : String;
    procedure SetVersao(Value : String);
    function  GetUnidadesExternas : String;
    procedure SetUnidadesExternas(Value : String);
    function  GetDeclaracao : String;
    procedure SetDeclaracao(Value : String);
    function  GetInicializacao : String;
    procedure SetInicializacao(Value : String);
    function  GetFinalizacao : String;
    procedure SetFinalizacao(Value : String);
    function  GetTitulo : String;
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Ambiente : TPacoteAmbiente read GetAmbiente write SetAmbiente;
    property Versao : String read GetVersao write SetVersao;
    property UnidadesExternas : String read GetUnidadesExternas write SetUnidadesExternas;
    property Declaracao : String read GetDeclaracao write SetDeclaracao;
    property Inicializacao : String read GetInicializacao write SetInicializacao;
    property Finalizacao : String read GetFinalizacao write SetFinalizacao;
    property Titulo : String read GetTitulo;
    property Classes : TPacoteClassesAssociation read _Classes write _Classes;
    property Comentarios : TPacoteComentariosAssociation read _Comentarios write _Comentarios;
    property Modelo : TPacoteModeloAssociation read _Modelo write _Modelo;
    procedure CopiarPacote;
    procedure CopiarPacote_Int(const Params : TMethodParams);
    procedure EditarDiagrama;
    procedure EditarDiagrama_Int(const Params : TMethodParams);
    class procedure SelecionarObjeto(pObjectPath : String);
    class procedure SelecionarObjeto_Int(const Params : TMethodParams);
  end;

  TPacoteClassesAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Link : TLink);
    procedure Delete(Link : TLink);
    function First : TLink;
    function Last : TLink;
    function Next(var Link : TLink) : boolean;
    function Prior(var Link : TLink) : boolean;
    function Find(S : String) : TLink;
    function Near(S : String) : TLink;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TPacoteComentariosAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Comentario : TComentario);
    procedure Delete(Comentario : TComentario);
    function First : TComentario;
    function Last : TComentario;
    function Next(var Comentario : TComentario) : boolean;
    function Prior(var Comentario : TComentario) : boolean;
    function Find(I : Integer) : TComentario;
    function Near(I : Integer) : TComentario;
  end;

  TPacoteModeloAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Modelo : TModelo);
    procedure Delete(Modelo : TModelo);
    function First : TModelo;
    function Last : TModelo;
    function Next(var Modelo : TModelo) : boolean;
    function Prior(var Modelo : TModelo) : boolean;
    function Find(I : Integer) : TModelo;
    function Near(I : Integer) : TModelo;
  end;

  TPapelList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Papel : TPapel);
    procedure Delete(Papel : TPapel);
    function First : TPapel;
    function Last : TPapel;
    function Next(var Papel : TPapel) : boolean;
    function Prior(var Papel : TPapel) : boolean;
    function Find(I : Integer) : TPapel;
    function Near(I : Integer) : TPapel;
  end;

  TPapelPorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Papel : TPapel);
    procedure Delete(Papel : TPapel);
    function First : TPapel;
    function Last : TPapel;
    function Next(var Papel : TPapel) : boolean;
    function Prior(var Papel : TPapel) : boolean;
    function Find(S : String) : TPapel;
    function Near(S : String) : TPapel;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TPapel = class(TElemento)
  private
    _Agregacao : TPapelAgregacao;
    _Multiplicidade : TPapelMultiplicidade;
    _Caracteristicas : TSetPapelCaracteristicas;
    _Minimo : Word;
    _Maximo : Word;
    _Constraint : String;
    _Habilitacao : String;
    _Visibilidade : String;
    _Validacao : String;
    _MensagemValidacao : String;
    _Inicial : String;
    _NomeAntigo : String;
    _Ordem : TIndice;
    _AssociacaoDestino : TAssociacao;
    _AssociacaoOrigem : TAssociacao;
    function  GetAgregacao : TPapelAgregacao;
    procedure SetAgregacao(Value : TPapelAgregacao);
    function  GetMultiplicidade : TPapelMultiplicidade;
    procedure SetMultiplicidade(Value : TPapelMultiplicidade);
    function  GetCaracteristicas : TSetPapelCaracteristicas;
    procedure SetCaracteristicas(Value : TSetPapelCaracteristicas);
    function  GetMinimo : Word;
    procedure SetMinimo(Value : Word);
    function  GetMaximo : Word;
    procedure SetMaximo(Value : Word);
    function  GetConstraint : String;
    procedure SetConstraint(Value : String);
    function  GetHabilitacao : String;
    procedure SetHabilitacao(Value : String);
    function  GetVisibilidade : String;
    procedure SetVisibilidade(Value : String);
    function  GetValidacao : String;
    procedure SetValidacao(Value : String);
    function  GetMensagemValidacao : String;
    procedure SetMensagemValidacao(Value : String);
    function  GetInicial : String;
    procedure SetInicial(Value : String);
    function  GetNomeAntigo : String;
    procedure SetNomeAntigo(Value : String);
    function  GetOrdem : TIndice;
    procedure SetOrdem(Value : TIndice);
    function  GetAssociacaoDestino : TAssociacao;
    procedure SetAssociacaoDestino(Value : TAssociacao);
    function  GetAssociacaoOrigem : TAssociacao;
    procedure SetAssociacaoOrigem(Value : TAssociacao);
  protected
    function DescreverMultiplicidade : String;
  public
    function FormatarConstraints : String;
    function GetIdentification : String; override;
  published
    property Agregacao : TPapelAgregacao read GetAgregacao write SetAgregacao;
    property Multiplicidade : TPapelMultiplicidade read GetMultiplicidade write SetMultiplicidade;
    property Caracteristicas : TSetPapelCaracteristicas read GetCaracteristicas write SetCaracteristicas;
    property Minimo : Word read GetMinimo write SetMinimo;
    property Maximo : Word read GetMaximo write SetMaximo;
    property Constraint : String read GetConstraint write SetConstraint;
    property Habilitacao : String read GetHabilitacao write SetHabilitacao;
    property Visibilidade : String read GetVisibilidade write SetVisibilidade;
    property Validacao : String read GetValidacao write SetValidacao;
    property MensagemValidacao : String read GetMensagemValidacao write SetMensagemValidacao;
    property Inicial : String read GetInicial write SetInicial;
    property NomeAntigo : String read GetNomeAntigo write SetNomeAntigo;
    property Ordem : TIndice read GetOrdem write SetOrdem;
    property AssociacaoDestino : TAssociacao read GetAssociacaoDestino write SetAssociacaoDestino;
    property AssociacaoOrigem : TAssociacao read GetAssociacaoOrigem write SetAssociacaoOrigem;
    function CheckMinimo(var Message : String) : Boolean;
    function VisibleMinimo : Boolean;
    function CheckMaximo(var Message : String) : Boolean;
    function VisibleMaximo : Boolean;
    function VisibleMensagemValidacao : Boolean;
  end;

  TTransicaoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Transicao : TTransicao);
    procedure Delete(Transicao : TTransicao);
    function First : TTransicao;
    function Last : TTransicao;
    function Next(var Transicao : TTransicao) : boolean;
    function Prior(var Transicao : TTransicao) : boolean;
    function Find(I : Integer) : TTransicao;
    function Near(I : Integer) : TTransicao;
  end;

  TTransicaoPorPontoOrigemList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Transicao : TTransicao);
    procedure Delete(Transicao : TTransicao);
    function First : TTransicao;
    function Last : TTransicao;
    function Next(var Transicao : TTransicao) : boolean;
    function Prior(var Transicao : TTransicao) : boolean;
    function Find(I : Integer) : TTransicao;
    function Near(I : Integer) : TTransicao;
  protected
    class function GetKeyCode : pointer; override;
  end;

  TTransicaoPorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Transicao : TTransicao);
    procedure Delete(Transicao : TTransicao);
    function First : TTransicao;
    function Last : TTransicao;
    function Next(var Transicao : TTransicao) : boolean;
    function Prior(var Transicao : TTransicao) : boolean;
    function Find(S : String) : TTransicao;
    function Near(S : String) : TTransicao;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TTransicao = class(TElemento)
  private
    _Condicao : String;
    _Acao : String;
    _PontoOrigem : Byte;
    _PontoDestino : Byte;
    _Quebras : String;
    _Origem : TEstadoBase;
    _Destino : TEstadoBase;
    function  GetCondicao : String;
    procedure SetCondicao(Value : String);
    function  GetAcao : String;
    procedure SetAcao(Value : String);
    function  GetPontoOrigem : Byte;
    procedure SetPontoOrigem(Value : Byte);
    function  GetPontoDestino : Byte;
    procedure SetPontoDestino(Value : Byte);
    function  GetQuebras : String;
    procedure SetQuebras(Value : String);
    function  GetOrigem : TEstadoBase;
    procedure SetOrigem(Value : TEstadoBase);
    function  GetDestino : TEstadoBase;
    procedure SetDestino(Value : TEstadoBase);
  protected
    function ChecarNome : Boolean; override;
  public
    function GetIdentification : String; override;
  published
    property Condicao : String read GetCondicao write SetCondicao;
    property Acao : String read GetAcao write SetAcao;
    property PontoOrigem : Byte read GetPontoOrigem write SetPontoOrigem;
    property PontoDestino : Byte read GetPontoDestino write SetPontoDestino;
    property Quebras : String read GetQuebras write SetQuebras;
    property Origem : TEstadoBase read GetOrigem write SetOrigem;
    property Destino : TEstadoBase read GetDestino write SetDestino;
    function PorPontoOrigem : Integer;
    function VisibleCondicao : Boolean;
  end;

  TClasseList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Classe : TClasse);
    procedure Delete(Classe : TClasse);
    function First : TClasse;
    function Last : TClasse;
    function Next(var Classe : TClasse) : boolean;
    function Prior(var Classe : TClasse) : boolean;
    function Find(I : Integer) : TClasse;
    function Near(I : Integer) : TClasse;
  end;

  TClassePorHerancaNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Classe : TClasse);
    procedure Delete(Classe : TClasse);
    function First : TClasse;
    function Last : TClasse;
    function Next(var Classe : TClasse) : boolean;
    function Prior(var Classe : TClasse) : boolean;
    function Find(S : String) : TClasse;
    function Near(S : String) : TClasse;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TClassePorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Classe : TClasse);
    procedure Delete(Classe : TClasse);
    function First : TClasse;
    function Last : TClasse;
    function Next(var Classe : TClasse) : boolean;
    function Prior(var Classe : TClasse) : boolean;
    function Find(S : String) : TClasse;
    function Near(S : String) : TClasse;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TClasse = class(TLink)
  private
    _Identificacao : String;
    _Tipo : TClasseTipo;
    _Apresentacao : TClasseApresentacao;
    _Oculto : Boolean;
    _NomeAntigo : String;
    _PropOrder : String;
    _Indices : TClasseIndicesAssociation;
    _Metodos : TClasseMetodosAssociation;
    _Atributos : TClasseAtributosAssociation;
    _Links : TClasseLinksAssociation;
    function  GetIdentificacao : String;
    procedure SetIdentificacao(Value : String);
    function  GetTipo : TClasseTipo;
    procedure SetTipo(Value : TClasseTipo);
    function  GetApresentacao : TClasseApresentacao;
    procedure SetApresentacao(Value : TClasseApresentacao);
    function  GetOculto : Boolean;
    procedure SetOculto(Value : Boolean);
    function  GetNomeAntigo : String;
    procedure SetNomeAntigo(Value : String);
    function  GetPropOrder : String;
    procedure SetPropOrder(Value : String);
    function  GetDescricaoAtributos : String;
    function  GetDescricaoMetodos : String;
  protected
    procedure New; override;
    procedure InternalFree; override;
    function ChecarNome : Boolean; override;
  public
    PropFamily : Word;
    function GetIdentification : String; override;
  published
    property Identificacao : String read GetIdentificacao write SetIdentificacao;
    property Tipo : TClasseTipo read GetTipo write SetTipo;
    property Apresentacao : TClasseApresentacao read GetApresentacao write SetApresentacao;
    property Oculto : Boolean read GetOculto write SetOculto;
    property NomeAntigo : String read GetNomeAntigo write SetNomeAntigo;
    property PropOrder : String read GetPropOrder write SetPropOrder;
    property DescricaoAtributos : String read GetDescricaoAtributos;
    property DescricaoMetodos : String read GetDescricaoMetodos;
    property Indices : TClasseIndicesAssociation read _Indices write _Indices;
    property Metodos : TClasseMetodosAssociation read _Metodos write _Metodos;
    property Atributos : TClasseAtributosAssociation read _Atributos write _Atributos;
    property Links : TClasseLinksAssociation read _Links write _Links;
  end;

  TClasseIndicesAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Indice : TIndice);
    procedure Delete(Indice : TIndice);
    function First : TIndice;
    function Last : TIndice;
    function Next(var Indice : TIndice) : boolean;
    function Prior(var Indice : TIndice) : boolean;
    function Find(S : String) : TIndice;
    function Near(S : String) : TIndice;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TClasseMetodosAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Metodo : TMetodo);
    procedure Delete(Metodo : TMetodo);
    function First : TMetodo;
    function Last : TMetodo;
    function Next(var Metodo : TMetodo) : boolean;
    function Prior(var Metodo : TMetodo) : boolean;
    function Find(S : String) : TMetodo;
    function Near(S : String) : TMetodo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TClasseAtributosAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Atributo : TAtributo);
    procedure Delete(Atributo : TAtributo);
    function First : TAtributo;
    function Last : TAtributo;
    function Next(var Atributo : TAtributo) : boolean;
    function Prior(var Atributo : TAtributo) : boolean;
    function Find(W : Word) : TAtributo;
    function Near(W : Word) : TAtributo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TClasseLinksAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Link : TLink);
    procedure Delete(Link : TLink);
    function First : TLink;
    function Last : TLink;
    function Next(var Link : TLink) : boolean;
    function Prior(var Link : TLink) : boolean;
    function Find(I : Integer) : TLink;
    function Near(I : Integer) : TLink;
  end;

  TEstadoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Estado : TEstado);
    procedure Delete(Estado : TEstado);
    function First : TEstado;
    function Last : TEstado;
    function Next(var Estado : TEstado) : boolean;
    function Prior(var Estado : TEstado) : boolean;
    function Find(I : Integer) : TEstado;
    function Near(I : Integer) : TEstado;
  end;

  TEstadoPorPosicaoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Estado : TEstado);
    procedure Delete(Estado : TEstado);
    function First : TEstado;
    function Last : TEstado;
    function Next(var Estado : TEstado) : boolean;
    function Prior(var Estado : TEstado) : boolean;
    function Find(I : Integer) : TEstado;
    function Near(I : Integer) : TEstado;
  protected
    class function GetKeyCode : pointer; override;
  end;

  TEstadoPorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Estado : TEstado);
    procedure Delete(Estado : TEstado);
    function First : TEstado;
    function Last : TEstado;
    function Next(var Estado : TEstado) : boolean;
    function Prior(var Estado : TEstado) : boolean;
    function Find(S : String) : TEstado;
    function Near(S : String) : TEstado;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TEstado = class(TEstadoBase)
  private
    _Tipo : TEstadoTipo;
    _Acao : String;
    _Subflow : Boolean;
    _MostrarAcao : Boolean;
    _Variaveis : TEstadoVariaveisAssociation;
    function  GetTipo : TEstadoTipo;
    procedure SetTipo(Value : TEstadoTipo);
    function  GetAcao : String;
    procedure SetAcao(Value : String);
    function  GetSubflow : Boolean;
    procedure SetSubflow(Value : Boolean);
    function  GetDescricaoVariaveis : String;
    function  GetMostrarAcao : Boolean;
    procedure SetMostrarAcao(Value : Boolean);
  protected
    procedure New; override;
  public
    function GetIdentification : String; override;
  published
    property Tipo : TEstadoTipo read GetTipo write SetTipo;
    property Acao : String read GetAcao write SetAcao;
    property Subflow : Boolean read GetSubflow write SetSubflow;
    property DescricaoVariaveis : String read GetDescricaoVariaveis;
    property MostrarAcao : Boolean read GetMostrarAcao write SetMostrarAcao;
    property Variaveis : TEstadoVariaveisAssociation read _Variaveis write _Variaveis;
    function CheckAcao(var Message : String) : Boolean;
  end;

  TEstadoVariaveisAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Variavel : TVariavel);
    procedure Delete(Variavel : TVariavel);
    function First : TVariavel;
    function Last : TVariavel;
    function Next(var Variavel : TVariavel) : boolean;
    function Prior(var Variavel : TVariavel) : boolean;
    function Find(I : Integer) : TVariavel;
    function Near(I : Integer) : TVariavel;
  end;

  TFormularioList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Formulario : TFormulario);
    procedure Delete(Formulario : TFormulario);
    function First : TFormulario;
    function Last : TFormulario;
    function Next(var Formulario : TFormulario) : boolean;
    function Prior(var Formulario : TFormulario) : boolean;
    function Find(I : Integer) : TFormulario;
    function Near(I : Integer) : TFormulario;
  end;

  TFormularioPorPosicaoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Formulario : TFormulario);
    procedure Delete(Formulario : TFormulario);
    function First : TFormulario;
    function Last : TFormulario;
    function Next(var Formulario : TFormulario) : boolean;
    function Prior(var Formulario : TFormulario) : boolean;
    function Find(I : Integer) : TFormulario;
    function Near(I : Integer) : TFormulario;
  protected
    class function GetKeyCode : pointer; override;
  end;

  TFormularioPorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Formulario : TFormulario);
    procedure Delete(Formulario : TFormulario);
    function First : TFormulario;
    function Last : TFormulario;
    function Next(var Formulario : TFormulario) : boolean;
    function Prior(var Formulario : TFormulario) : boolean;
    function Find(S : String) : TFormulario;
    function Near(S : String) : TFormulario;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TFormulario = class(TEstadoBase)
  private
    _Objeto : String;
    _SomenteLeitura : Boolean;
    _Formatacoes : TFormularioFormatacoesAssociation;
    function  GetObjeto : String;
    procedure SetObjeto(Value : String);
    function  GetSomenteLeitura : Boolean;
    procedure SetSomenteLeitura(Value : Boolean);
    function  GetDescricaoFormatacoes : String;
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Objeto : String read GetObjeto write SetObjeto;
    property SomenteLeitura : Boolean read GetSomenteLeitura write SetSomenteLeitura;
    property DescricaoFormatacoes : String read GetDescricaoFormatacoes;
    property Formatacoes : TFormularioFormatacoesAssociation read _Formatacoes write _Formatacoes;
  end;

  TFormularioFormatacoesAssociation = class(TAssociation)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Formatacao : TFormatacao);
    procedure Delete(Formatacao : TFormatacao);
    function First : TFormatacao;
    function Last : TFormatacao;
    function Next(var Formatacao : TFormatacao) : boolean;
    function Prior(var Formatacao : TFormatacao) : boolean;
    function Find(W : Word) : TFormatacao;
    function Near(W : Word) : TFormatacao;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TVariavelList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Variavel : TVariavel);
    procedure Delete(Variavel : TVariavel);
    function First : TVariavel;
    function Last : TVariavel;
    function Next(var Variavel : TVariavel) : boolean;
    function Prior(var Variavel : TVariavel) : boolean;
    function Find(I : Integer) : TVariavel;
    function Near(I : Integer) : TVariavel;
  end;

  TVariavelPorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Variavel : TVariavel);
    procedure Delete(Variavel : TVariavel);
    function First : TVariavel;
    function Last : TVariavel;
    function Next(var Variavel : TVariavel) : boolean;
    function Prior(var Variavel : TVariavel) : boolean;
    function Find(S : String) : TVariavel;
    function Near(S : String) : TVariavel;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TVariavel = class(TElementoTipado)
  private
    _Inicial : String;
    _Estado : TEstado;
    _Metodo : TMetodo;
    function  GetInicial : String;
    procedure SetInicial(Value : String);
    function  GetEstado : TEstado;
    procedure SetEstado(Value : TEstado);
    function  GetMetodo : TMetodo;
    procedure SetMetodo(Value : TMetodo);
  protected
    function ChecarNome : Boolean; override;
  public
    function GetIdentification : String; override;
  published
    property Inicial : String read GetInicial write SetInicial;
    property Estado : TEstado read GetEstado write SetEstado;
    property Metodo : TMetodo read GetMetodo write SetMetodo;
  end;

  TValidacaoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
  end;

  TValidacao = class(TVariavel)
  private
    _Validacao : String;
    _MensagemValidacao : String;
    function  GetValidacao : String;
    procedure SetValidacao(Value : String);
    function  GetMensagemValidacao : String;
    procedure SetMensagemValidacao(Value : String);
  published
    property Validacao : String read GetValidacao write SetValidacao;
    property MensagemValidacao : String read GetMensagemValidacao write SetMensagemValidacao;
    function CheckMensagemValidacao(var Message : String) : Boolean;
  end;

  TAtributoList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Atributo : TAtributo);
    procedure Delete(Atributo : TAtributo);
    function First : TAtributo;
    function Last : TAtributo;
    function Next(var Atributo : TAtributo) : boolean;
    function Prior(var Atributo : TAtributo) : boolean;
    function Find(I : Integer) : TAtributo;
    function Near(I : Integer) : TAtributo;
  end;

  TAtributoPorOrdemList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Atributo : TAtributo);
    procedure Delete(Atributo : TAtributo);
    function First : TAtributo;
    function Last : TAtributo;
    function Next(var Atributo : TAtributo) : boolean;
    function Prior(var Atributo : TAtributo) : boolean;
    function Find(W : Word) : TAtributo;
    function Near(W : Word) : TAtributo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TAtributoPorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Atributo : TAtributo);
    procedure Delete(Atributo : TAtributo);
    function First : TAtributo;
    function Last : TAtributo;
    function Next(var Atributo : TAtributo) : boolean;
    function Prior(var Atributo : TAtributo) : boolean;
    function Find(S : String) : TAtributo;
    function Near(S : String) : TAtributo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TAtributo = class(TValidacao)
  private
    _Escopo : TAtributoEscopo;
    _Caracteristicas : TSetAtributoCaracteristicas;
    _Ordem : TAtributoOrdem;
    _Habilitacao : String;
    _Visibilidade : String;
    _NomeAntigo : String;
    _Classe : TClasse;
    function  GetEscopo : TAtributoEscopo;
    procedure SetEscopo(Value : TAtributoEscopo);
    function  GetCaracteristicas : TSetAtributoCaracteristicas;
    procedure SetCaracteristicas(Value : TSetAtributoCaracteristicas);
    function  GetOrdem : TAtributoOrdem;
    procedure SetOrdem(Value : TAtributoOrdem);
    function  GetHabilitacao : String;
    procedure SetHabilitacao(Value : String);
    function  GetVisibilidade : String;
    procedure SetVisibilidade(Value : String);
    function  GetNomeAntigo : String;
    procedure SetNomeAntigo(Value : String);
    function  GetClasse : TClasse;
    procedure SetClasse(Value : TClasse);
  protected
    procedure New; override;
    function ChecarNome : Boolean; override;
    function FormatarEscopo : String;
    function FormatarDerivado : String;
    function FormatarCaracteristicas : String;
  public
    PropIndex : Word;
    function GetIdentification : String; override;
  published
    property Escopo : TAtributoEscopo read GetEscopo write SetEscopo;
    property Caracteristicas : TSetAtributoCaracteristicas read GetCaracteristicas write SetCaracteristicas;
    property Ordem : TAtributoOrdem read GetOrdem write SetOrdem;
    property Habilitacao : String read GetHabilitacao write SetHabilitacao;
    property Visibilidade : String read GetVisibilidade write SetVisibilidade;
    property NomeAntigo : String read GetNomeAntigo write SetNomeAntigo;
    property Classe : TClasse read GetClasse write SetClasse;
    function PorOrdem : Word;
    function CheckOrdem(var Message : String) : Boolean;
  end;

  TParametroList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Parametro : TParametro);
    procedure Delete(Parametro : TParametro);
    function First : TParametro;
    function Last : TParametro;
    function Next(var Parametro : TParametro) : boolean;
    function Prior(var Parametro : TParametro) : boolean;
    function Find(I : Integer) : TParametro;
    function Near(I : Integer) : TParametro;
  end;

  TParametroPorNomeList = class(TPrevalentList)
    class function GetObjectClass : TTransientClass; override;
    procedure Add(Parametro : TParametro);
    procedure Delete(Parametro : TParametro);
    function First : TParametro;
    function Last : TParametro;
    function Next(var Parametro : TParametro) : boolean;
    function Prior(var Parametro : TParametro) : boolean;
    function Find(S : String) : TParametro;
    function Near(S : String) : TParametro;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  end;

  TParametro = class(TValidacao)
  private
    _Passagem : TParametroPassagem;
    function  GetPassagem : TParametroPassagem;
    procedure SetPassagem(Value : TParametroPassagem);
  protected
    function ChecarNome : Boolean; override;
  published
    property Passagem : TParametroPassagem read GetPassagem write SetPassagem;
  end;

// Get Lists - Declarations
function AjudaList : TAjudaList;
function AparenciaList : TAparenciaList;
function AssociacaoList : TAssociacaoList;
function ComentarioList : TComentarioList;
function CompiladorList : TCompiladorList;
function ElementoList : TPrevalentList;
function FormatacaoList : TFormatacaoList;
function FormatacaoPorOrdemList : TFormatacaoPorOrdemList;
function GeracaoList : TGeracaoList;
function GeracaoPorUsuarioList : TGeracaoPorUsuarioList;
function GeracaoPorPortaList : TGeracaoPorPortaList;
function HerancaList : THerancaList;
function ParametroInicializacaoList : TParametroInicializacaoList;
function ParametroInicializacaoPorSessaoList : TParametroInicializacaoPorSessaoList;
function ServidorList : TServidorList;
function VersaoList : TVersaoList;
function ElementoTipadoList : TElementoTipadoList;
function ElementoTipadoPorNomeList : TElementoTipadoPorNomeList;
function EstadoBaseList : TPrevalentList;
function IndiceList : TIndiceList;
function IndicePorNomeList : TIndicePorNomeList;
function LinkList : TLinkList;
function LinkPorNomeList : TLinkPorNomeList;
function LinkPorHerancaNomeList : TLinkPorHerancaNomeList;
function MetodoList : TMetodoList;
function MetodoPorNomeList : TMetodoPorNomeList;
function ModeloList : TModeloList;
function ModeloPorNomeList : TModeloPorNomeList;
function PacoteList : TPacoteList;
function PacotePorNomeList : TPacotePorNomeList;
function PapelList : TPapelList;
function PapelPorNomeList : TPapelPorNomeList;
function TransicaoList : TTransicaoList;
function TransicaoPorNomeList : TTransicaoPorNomeList;
function TransicaoPorPontoOrigemList : TTransicaoPorPontoOrigemList;
function ClasseList : TClasseList;
function ClassePorNomeList : TClassePorNomeList;
function ClassePorHerancaNomeList : TClassePorHerancaNomeList;
function EstadoList : TEstadoList;
function EstadoPorNomeList : TEstadoPorNomeList;
function EstadoPorPosicaoList : TEstadoPorPosicaoList;
function FormularioList : TFormularioList;
function FormularioPorNomeList : TFormularioPorNomeList;
function FormularioPorPosicaoList : TFormularioPorPosicaoList;
function VariavelList : TVariavelList;
function VariavelPorNomeList : TVariavelPorNomeList;
function ValidacaoList : TPrevalentList;
function AtributoList : TAtributoList;
function AtributoPorNomeList : TAtributoPorNomeList;
function AtributoPorOrdemList : TAtributoPorOrdemList;
function ParametroList : TParametroList;
function ParametroPorNomeList : TParametroPorNomeList;
implementation

uses
  TypInfo, StrUtils, DateUtils, Math, //MaskUtils,
  epCommon, epThread, Designer;

// Global Lists
var
  FAjudaList : TAjudaList;
  FAparenciaList : TAparenciaList;
  FAssociacaoList : TAssociacaoList;
  FComentarioList : TComentarioList;
  FCompiladorList : TCompiladorList;
  FElementoList : TPrevalentList;
  FFormatacaoList : TFormatacaoList;
  FFormatacaoPorOrdemList : TFormatacaoPorOrdemList;
  FGeracaoList : TGeracaoList;
  FGeracaoPorPortaList : TGeracaoPorPortaList;
  FGeracaoPorUsuarioList : TGeracaoPorUsuarioList;
  FHerancaList : THerancaList;
  FParametroInicializacaoList : TParametroInicializacaoList;
  FParametroInicializacaoPorSessaoList : TParametroInicializacaoPorSessaoList;
  FServidorList : TServidorList;
  FVersaoList : TVersaoList;
  FElementoTipadoList : TElementoTipadoList;
  FElementoTipadoPorNomeList : TElementoTipadoPorNomeList;
  FEstadoBaseList : TPrevalentList;
  FIndiceList : TIndiceList;
  FIndicePorNomeList : TIndicePorNomeList;
  FLinkList : TLinkList;
  FLinkPorHerancaNomeList : TLinkPorHerancaNomeList;
  FLinkPorNomeList : TLinkPorNomeList;
  FMetodoList : TMetodoList;
  FMetodoPorNomeList : TMetodoPorNomeList;
  FModeloList : TModeloList;
  FModeloPorNomeList : TModeloPorNomeList;
  FPacoteList : TPacoteList;
  FPacotePorNomeList : TPacotePorNomeList;
  FPapelList : TPapelList;
  FPapelPorNomeList : TPapelPorNomeList;
  FTransicaoList : TTransicaoList;
  FTransicaoPorPontoOrigemList : TTransicaoPorPontoOrigemList;
  FTransicaoPorNomeList : TTransicaoPorNomeList;
  FClasseList : TClasseList;
  FClassePorHerancaNomeList : TClassePorHerancaNomeList;
  FClassePorNomeList : TClassePorNomeList;
  FEstadoList : TEstadoList;
  FEstadoPorPosicaoList : TEstadoPorPosicaoList;
  FEstadoPorNomeList : TEstadoPorNomeList;
  FFormularioList : TFormularioList;
  FFormularioPorPosicaoList : TFormularioPorPosicaoList;
  FFormularioPorNomeList : TFormularioPorNomeList;
  FVariavelList : TVariavelList;
  FVariavelPorNomeList : TVariavelPorNomeList;
  FValidacaoList : TPrevalentList;
  FAtributoList : TAtributoList;
  FAtributoPorOrdemList : TAtributoPorOrdemList;
  FAtributoPorNomeList : TAtributoPorNomeList;
  FParametroList : TParametroList;
  FParametroPorNomeList : TParametroPorNomeList;

// Get Lists
function AjudaList : TAjudaList; begin Result := FAjudaList end;
function AparenciaList : TAparenciaList; begin Result := FAparenciaList end;
function AssociacaoList : TAssociacaoList; begin Result := FAssociacaoList end;
function ComentarioList : TComentarioList; begin Result := FComentarioList end;
function CompiladorList : TCompiladorList; begin Result := FCompiladorList end;
function ElementoList : TPrevalentList; begin Result := FElementoList end;
function FormatacaoList : TFormatacaoList; begin Result := FFormatacaoList end;
function FormatacaoPorOrdemList : TFormatacaoPorOrdemList; begin Result := FFormatacaoPorOrdemList end;
function GeracaoList : TGeracaoList; begin Result := FGeracaoList end;
function GeracaoPorUsuarioList : TGeracaoPorUsuarioList; begin Result := FGeracaoPorUsuarioList end;
function GeracaoPorPortaList : TGeracaoPorPortaList; begin Result := FGeracaoPorPortaList end;
function HerancaList : THerancaList; begin Result := FHerancaList end;
function ParametroInicializacaoList : TParametroInicializacaoList; begin Result := FParametroInicializacaoList end;
function ParametroInicializacaoPorSessaoList : TParametroInicializacaoPorSessaoList; begin Result := FParametroInicializacaoPorSessaoList end;
function ServidorList : TServidorList; begin Result := FServidorList end;
function VersaoList : TVersaoList; begin Result := FVersaoList end;
function ElementoTipadoList : TElementoTipadoList; begin Result := FElementoTipadoList end;
function ElementoTipadoPorNomeList : TElementoTipadoPorNomeList; begin Result := FElementoTipadoPorNomeList end;
function EstadoBaseList : TPrevalentList; begin Result := FEstadoBaseList end;
function IndiceList : TIndiceList; begin Result := FIndiceList end;
function IndicePorNomeList : TIndicePorNomeList; begin Result := FIndicePorNomeList end;
function LinkList : TLinkList; begin Result := FLinkList end;
function LinkPorNomeList : TLinkPorNomeList; begin Result := FLinkPorNomeList end;
function LinkPorHerancaNomeList : TLinkPorHerancaNomeList; begin Result := FLinkPorHerancaNomeList end;
function MetodoList : TMetodoList; begin Result := FMetodoList end;
function MetodoPorNomeList : TMetodoPorNomeList; begin Result := FMetodoPorNomeList end;
function ModeloList : TModeloList; begin Result := FModeloList end;
function ModeloPorNomeList : TModeloPorNomeList; begin Result := FModeloPorNomeList end;
function PacoteList : TPacoteList; begin Result := FPacoteList end;
function PacotePorNomeList : TPacotePorNomeList; begin Result := FPacotePorNomeList end;
function PapelList : TPapelList; begin Result := FPapelList end;
function PapelPorNomeList : TPapelPorNomeList; begin Result := FPapelPorNomeList end;
function TransicaoList : TTransicaoList; begin Result := FTransicaoList end;
function TransicaoPorNomeList : TTransicaoPorNomeList; begin Result := FTransicaoPorNomeList end;
function TransicaoPorPontoOrigemList : TTransicaoPorPontoOrigemList; begin Result := FTransicaoPorPontoOrigemList end;
function ClasseList : TClasseList; begin Result := FClasseList end;
function ClassePorNomeList : TClassePorNomeList; begin Result := FClassePorNomeList end;
function ClassePorHerancaNomeList : TClassePorHerancaNomeList; begin Result := FClassePorHerancaNomeList end;
function EstadoList : TEstadoList; begin Result := FEstadoList end;
function EstadoPorNomeList : TEstadoPorNomeList; begin Result := FEstadoPorNomeList end;
function EstadoPorPosicaoList : TEstadoPorPosicaoList; begin Result := FEstadoPorPosicaoList end;
function FormularioList : TFormularioList; begin Result := FFormularioList end;
function FormularioPorNomeList : TFormularioPorNomeList; begin Result := FFormularioPorNomeList end;
function FormularioPorPosicaoList : TFormularioPorPosicaoList; begin Result := FFormularioPorPosicaoList end;
function VariavelList : TVariavelList; begin Result := FVariavelList end;
function VariavelPorNomeList : TVariavelPorNomeList; begin Result := FVariavelPorNomeList end;
function ValidacaoList : TPrevalentList; begin Result := FValidacaoList end;
function AtributoList : TAtributoList; begin Result := FAtributoList end;
function AtributoPorNomeList : TAtributoPorNomeList; begin Result := FAtributoPorNomeList end;
function AtributoPorOrdemList : TAtributoPorOrdemList; begin Result := FAtributoPorOrdemList end;
function ParametroList : TParametroList; begin Result := FParametroList end;
function ParametroPorNomeList : TParametroPorNomeList; begin Result := FParametroPorNomeList end;

procedure InitLists; begin
  FAjudaList := TAjudaList.Create;
  FAparenciaList := TAparenciaList.Create;
  FAssociacaoList := TAssociacaoList.Create;
  FComentarioList := TComentarioList.Create;
  FCompiladorList := TCompiladorList.Create;
  FElementoList := TElementoList.Create([lpAbstract]);
  FFormatacaoList := TFormatacaoList.Create;
  FFormatacaoPorOrdemList := TFormatacaoPorOrdemList.Create([lpDuplicates]);
  FGeracaoList := TGeracaoList.Create;
  FGeracaoPorPortaList := TGeracaoPorPortaList.Create([lpDuplicates]);
  FGeracaoPorUsuarioList := TGeracaoPorUsuarioList.Create([lpDuplicates]);
  FHerancaList := THerancaList.Create;
  FParametroInicializacaoList := TParametroInicializacaoList.Create;
  FParametroInicializacaoPorSessaoList := TParametroInicializacaoPorSessaoList.Create([lpDuplicates]);
  FServidorList := TServidorList.Create;
  FVersaoList := TVersaoList.Create;
  FElementoTipadoList := TElementoTipadoList.Create;
  FElementoTipadoPorNomeList := TElementoTipadoPorNomeList.Create([lpDuplicates]);
  FEstadoBaseList := TEstadoBaseList.Create([lpAbstract]);
  FIndiceList := TIndiceList.Create;
  FIndicePorNomeList := TIndicePorNomeList.Create([lpDuplicates]);
  FLinkList := TLinkList.Create;
  FLinkPorHerancaNomeList := TLinkPorHerancaNomeList.Create([lpDuplicates]);
  FLinkPorNomeList := TLinkPorNomeList.Create([lpDuplicates]);
  FMetodoList := TMetodoList.Create;
  FMetodoPorNomeList := TMetodoPorNomeList.Create([lpDuplicates]);
  FModeloList := TModeloList.Create;
  FModeloPorNomeList := TModeloPorNomeList.Create([lpDuplicates]);
  FPacoteList := TPacoteList.Create;
  FPacotePorNomeList := TPacotePorNomeList.Create([lpDuplicates]);
  FPapelList := TPapelList.Create;
  FPapelPorNomeList := TPapelPorNomeList.Create([lpDuplicates]);
  FTransicaoList := TTransicaoList.Create;
  FTransicaoPorPontoOrigemList := TTransicaoPorPontoOrigemList.Create([lpDuplicates]);
  FTransicaoPorNomeList := TTransicaoPorNomeList.Create([lpDuplicates]);
  FClasseList := TClasseList.Create;
  FClassePorHerancaNomeList := TClassePorHerancaNomeList.Create([lpDuplicates]);
  FClassePorNomeList := TClassePorNomeList.Create([lpDuplicates]);
  FEstadoList := TEstadoList.Create;
  FEstadoPorPosicaoList := TEstadoPorPosicaoList.Create([lpDuplicates]);
  FEstadoPorNomeList := TEstadoPorNomeList.Create([lpDuplicates]);
  FFormularioList := TFormularioList.Create;
  FFormularioPorPosicaoList := TFormularioPorPosicaoList.Create([lpDuplicates]);
  FFormularioPorNomeList := TFormularioPorNomeList.Create([lpDuplicates]);
  FVariavelList := TVariavelList.Create;
  FVariavelPorNomeList := TVariavelPorNomeList.Create([lpDuplicates]);
  FValidacaoList := TValidacaoList.Create([lpAbstract]);
  FAtributoList := TAtributoList.Create;
  FAtributoPorOrdemList := TAtributoPorOrdemList.Create([lpDuplicates]);
  FAtributoPorNomeList := TAtributoPorNomeList.Create([lpDuplicates]);
  FParametroList := TParametroList.Create;
  FParametroPorNomeList := TParametroPorNomeList.Create([lpDuplicates]);
end;

// Metadata

procedure InitAjuda; begin
  with Prevalence.Metadata('TAjuda') do begin
    InheritMetadata(0);
    AddMetadata(0, '', '', 'Designer');
    AddClassTags('PropOrder', 'Dica,Texto');
    AddMetadata(1, '', '', TElemento, 'Ajuda');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, '', '', 'Memo');
    AddMetadata(3, '', '', 'Memo');
  end;
end;

procedure InitAparencia; begin
  with Prevalence.Metadata('TAparencia') do begin
    InheritMetadata(0);
    AddMetadata(0, 'Aparência', '', 'Designer');
    AddUnidirectional(TComentario, 'Aparencia', [composite]);
    AddUnidirectional(TElemento, 'Aparencia', [composite]);
    AddMetadata(1, '', '');
  end;
end;

procedure InitAssociacao; begin
  with Prevalence.Metadata('TAssociacao') do begin
    InheritMetadata(0);
    AddMetadata(0, 'Associação', '', 'Designer');
    AddClassTags('PropOrder', 'PapelOrigem,PapelDestino');
    AddMetadata(1, '', '', TLink, 'AssociacoesOrigem');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, '', '', TLink, 'AssociacoesDestino');
    AddConstraints(2, [NOTNULL]);
    AddMetadata(3, 'Papel Origem', '', TPapel, 'AssociacaoOrigem');
    AddConstraints(3, [COMPOSITE]);
    AddMetadata(4, 'Papel Destino', '', TPapel, 'AssociacaoDestino');
    AddConstraints(4, [COMPOSITE]);
    AddMetadata(6, 'Hidden', '');
    AddMetadata(7, 'Hidden', '');
    AddConstraints(7, [CHECK]);
  end;
end;

procedure InitComentario; begin
  with Prevalence.Metadata('TComentario') do begin
    InheritMetadata(0);
    AddMetadata(0, 'Comentário', '', 'Designer');
    AddClassTags('PropOrder', '<Detalhamento>,/texto/');
    AddMetadata(1, 'Método', '', TMetodo, 'Comentarios');
    AddMetadata(2, '', '', TAparencia, '');
    AddConstraints(2, [COMPOSITE]);
    AddMetadata(3, '', '', TPacote, 'Comentarios');
    AddMetadata(4, '', '', 'Memo');
  end;
end;

procedure InitCompilador; begin
  with Prevalence.Metadata('TCompilador') do begin
    InheritMetadata(0);
    AddMetadata(0, '', '', 'Designer');
    AddUnidirectional(TGeracao, 'Compilador', [NOTNULL]);
    AddMetadata(1, 'Opções de Compilação', '', 'memo');
    AddMetadata(2, 'Executável', '', 'Memo');
  end;
end;

procedure InitElemento; begin
  with Prevalence.Metadata('TElemento') do begin
    InheritMetadata(1);
    AddMetadata(0, '', '', 'Designer');
    AddMetadata(1, 'hidden', '', TAparencia, '');
    AddConstraints(1, [COMPOSITE]);
    AddMetadata(2, '', '', TAjuda, 'Elemento');
    AddConstraints(2, [COMPOSITE]);
    AddMetadata(3, 'Documentação', '', 'editor');
    AddMetadata(4, '', '', 'Memo');
    AddMetadata(5, '', '', '[a-zA-Z_][a-zA-Z0-9_]{0,100}');
    AddMethod(0, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure InitFormatacao; begin
  with Prevalence.Metadata('TFormatacao') do begin
    InheritMetadata(1);
    AddMetadata(0, 'Formatação', '', 'Designer');
    AddClassTags('PropOrder', 'Titulo,Ordem,Comando,Propriedade,Layout');
    AddMetadata(1, 'Formulário', '', TFormulario, 'Formatacoes');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, '', '');
    AddConstraints(2, [VISIBLE]);
    AddMetadata(3, '', '');
    AddConstraints(3, [VISIBLE]);
    AddMetadata(4, 'Título', '');
    AddConstraints(4, [VISIBLE]);
    AddMetadata(6, '', '');
    AddConstraints(6, [CHECK]);
    AddMethod(0, 'PorOrdem', '', '', '', mkFunction, 0, TypeInfo(Word), _stINDEX);
  end;
end;

procedure InitGeracao; begin
  with Prevalence.Metadata('TGeracao') do begin
    InheritMetadata(2);
    AddMetadata(0, 'Geração', '', 'Designer');
    AddMetadata(1, '', '', TModelo, 'Geracoes');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, '', '', TCompilador, '');
    AddConstraints(2, [NOTNULL]);
    AddMetadata(3, 'Versão', '');
    AddConstraints(3, [READONLY]);
    AddMetadata(4, 'Opções', '');
    AddMetadata(5, '', '');
    AddConstraints(5, [READONLY]);
    AddMetadata(7, 'Usuário', '');
    AddMethod(0, 'PorPorta', '', '', '', mkFunction, 0, TypeInfo(Word), _stINDEX);
    AddMethod(1, 'PorUsuario', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure InitHeranca; begin
  with Prevalence.Metadata('THeranca') do begin
    InheritMetadata(0);
    AddMetadata(0, '', '', 'Designer');
    AddMetadata(1, '', '', TLink, 'Herancas');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, '', '', TLink, 'Pai');
    AddConstraints(2, [NOTNULL]);
  end;
end;

procedure InitParametroInicializacao; begin
  with Prevalence.Metadata('TParametroInicializacao') do begin
    InheritMetadata(1);
    AddMetadata(0, 'Parâmetro de Inicialização', '', 'Designer');
    AddMetadata(3, 'Sessão', '');
    AddMetadata(4, 'Aplicável', '');
    AddMethod(0, 'PorSessao', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure InitServidor; begin
  with Prevalence.Metadata('TServidor') do begin
    InheritMetadata(0);
    AddMetadata(0, 'Parâmetros Globais de Geração', '', 'Designer');
    AddMetadata(1, 'Versão', '', TVersao, '');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, 'Inicialização', '', TParametroInicializacao, '');
    AddMetadata(3, 'Número de Porta Final', '');
    AddMetadata(4, 'Número de Porta Inicial', '');
  end;
end;

procedure InitVersao; begin
  with Prevalence.Metadata('TVersao') do begin
    InheritMetadata(0);
    AddMetadata(0, 'Versão', '', 'Designer');
    AddUnidirectional(TServidor, 'Versao', [NOTNULL]);
    AddMetadata(1, '', '', 'Memo');
    AddMetadata(2, 'Número', '');
  end;
end;

procedure InitElementoTipado; begin
  with Prevalence.Metadata('TElementoTipado') do begin
    InheritMetadata(0);
    AddMetadata(0, '', '', 'Designer');
    AddClassTags('PropOrder', 'Nome,Tipo,TipoClasse,Valores,Tamanho,Decimais,Sinalizado,Mascara>Documentação<,/Ajuda/,Documentacao');
    AddMetadata(1, 'Classe', '', TClasse, '');
    AddConstraints(1, [CHECK, VISIBLE]);
    AddUnidirectional(TMetodo, 'Retorno', [composite]);
    AddUnidirectional(TIndice, 'Retorno', [NOTNULL]);
    AddMetadata(2, 'Máscara', '', 'Memo');
    AddConstraints(2, [VISIBLE]);
    AddMetadata(3, '', '', 'memo 40,6');
    AddConstraints(3, [VISIBLE]);
    AddMetadata(4, '', '');
    AddConstraints(4, [VISIBLE]);
    AddMetadata(5, '', '');
    AddConstraints(5, [VISIBLE]);
    AddMetadata(6, '', '');
    AddConstraints(6, [VISIBLE]);
  end;
end;

procedure InitEstadoBase; begin
  with Prevalence.Metadata('TEstadoBase') do begin
    InheritMetadata(1);
    AddMetadata(0, '', '', 'Designer');
    AddMetadata(1, '', '', TTransicao, 'Destino');
    AddConstraints(1, [COMPOSITE]);
    AddMetadata(2, '', '', TMetodo, 'Estados');
    AddConstraints(2, [NOTNULL]);
    AddMetadata(3, '', '', TTransicao, 'Origem');
    AddConstraints(3, [COMPOSITE]);
    AddMetadata(4, 'Hidden', '');
    AddConstraints(4, [READONLY]);
    AddMetadata(5, 'hidden', '');
    AddMethod(0, 'PorPosicao', '', '', '', mkFunction, 0, TypeInfo(Integer), _stINDEX);
  end;
end;

procedure InitIndice; begin
  with Prevalence.Metadata('TIndice') do begin
    InheritMetadata(0);
    AddMetadata(0, 'Índice', '', 'Designer');
    AddClassTags('PropOrder', '<Identificação>,Nome,Apelido,<Detalhamento>,Tipo,Expressao,Unico,Retorno,>Documentacao<,/Ajuda/,Documentacao');
    AddMetadata(1, '', '', TClasse, 'Indices');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, '', '', TElementoTipado, '');
    AddConstraints(2, [NOTNULL]);
    AddUnidirectional(TPapel, 'Ordem', []);
    AddMetadata(4, 'Único', '');
    AddMetadata(5, 'Expressão', '', 'Memo');
  end;
end;

procedure InitLink; begin
  with Prevalence.Metadata('TLink') do begin
    InheritMetadata(2);
    AddMetadata(0, '', '', 'Designer');
    AddMetadata(1, '', '', TClasse, 'Links');
    AddMetadata(2, 'Heranças', '', THeranca, 'Destino');
    AddConstraints(2, [COMPOSITE]);
    AddMetadata(3, '', '', THeranca, 'Origem');
    AddConstraints(3, [COMPOSITE]);
    AddMetadata(4, 'Associações Origem', '', TAssociacao, 'Destino');
    AddConstraints(4, [COMPOSITE]);
    AddMetadata(5, '', '', TAssociacao, 'Origem');
    AddConstraints(5, [COMPOSITE]);
    AddMetadata(6, '', '', TPacote, 'Classes');
    AddConstraints(6, [NOTNULL]);
    AddMetadata(7, 'Hidden', '');
    AddConstraints(7, [READONLY]);
    AddMethod(0, 'PorHerancaNome', 'Hidden', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
    AddMethod(1, 'ContarHeranca', '', '', '', mkFunction, 0, TypeInfo(String), _stSTATEMACHINE);
  end;
end;

procedure InitMetodo; begin
  with Prevalence.Metadata('TMetodo') do begin
    InheritMetadata(1);
    AddMetadata(0, 'Método', '', 'Designer');
    AddClassTags('PropOrder', 'Nome,Apelido,<Detalhamento>,Escopo,Tipo,Acao,Retorno,Caracteristicas,>Documentação<,/Ajuda/,Documentacao,Parametros,Variaveis');
    AddMetadata(1, '', '', TClasse, 'Metodos');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, '', '', TElementoTipado, '');
    AddConstraints(2, [COMPOSITE]);
    AddMetadata(3, 'Parâmetros', '', TParametro, '');
    AddConstraints(3, [COMPOSITE]);
    AddMetadata(4, 'hidden', '', TComentario, 'Metodo');
    AddConstraints(4, [COMPOSITE]);
    AddMetadata(5, 'Variáveis', '', TVariavel, 'Metodo');
    AddConstraints(5, [COMPOSITE]);
    AddMetadata(6, '', '', TEstadoBase, 'Metodo');
    AddConstraints(6, [COMPOSITE]);
    AddMetadata(7, 'Hidden', '');
    AddConstraints(7, [READONLY]);
    AddMetadata(8, 'Características', '');
    AddMetadata(9, 'Ação', '', 'memo');
    AddMetadata(11, '', '');
    AddMethod(0, 'EditarDiagrama', 'Editar Diagrama', '', '', mkProcedure, 0, nil, _stSTATEMACHINE);
  end;
end;

procedure InitModelo; begin
  with Prevalence.Metadata('TModelo') do begin
    InheritMetadata(3);
    AddMetadata(0, '', '', 'Designer');
    AddMetadata(1, 'Gerações', '', TGeracao, 'Modelo');
    AddConstraints(1, [COMPOSITE]);
    AddMetadata(2, '', '', TPacote, 'Modelo');
    AddConstraints(2, [SHARED]);
    AddMethod(0, 'DispararServer', 'Disparar ObjectServer', '', '', mkClassProcedure, 0, nil, _stWORKFLOW);
    AddMethod(1, 'GerarServer', 'Gerar ObjectServer', '', '', mkClassProcedure, 0, nil, _stWORKFLOW);
    AddMethod(2, 'ImportarModelo', 'Importar Modelo', '', '', mkClassProcedure, 1, nil, _stWORKFLOW);
      AddParam(0, 'Modelo', '', '', 'C:\projects\XDADesigner\XDANew.EAP', pfConst, TypeInfo(String));
  end;
end;

procedure InitPacote; begin
  with Prevalence.Metadata('TPacote') do begin
    InheritMetadata(3);
    AddMetadata(0, '', '', 'Designer');
    AddMetadata(1, '', '', TModelo, 'Pacotes');
    AddMetadata(2, 'Comentários', '', TComentario, 'Pacote');
    AddConstraints(2, [COMPOSITE]);
    AddMetadata(3, '', '', TLink, 'Pacote');
    AddConstraints(3, [COMPOSITE]);
    AddMetadata(4, 'Hidden', '');
    AddConstraints(4, [READONLY]);
    AddMetadata(5, 'Finalização', '', 'Memo');
    AddMetadata(6, 'Inicialização', '', 'Memo');
    AddMetadata(7, 'Declaração', '', 'Memo');
    AddMetadata(8, 'Utilização de outra Unit', '', 'Memo');
    AddMetadata(9, 'Versão', '');
    AddMethod(0, 'SelecionarObjeto', 'hidden', '', '', mkClassProcedure, 1, nil, _stSTATEMACHINE);
      AddParam(0, 'pObjectPath', '', '', '', pfConst, TypeInfo(String));
    AddMethod(1, 'EditarDiagrama', 'Editar Diagrama', '', '', mkProcedure, 0, nil, _stSTATEMACHINE);
    AddMethod(2, 'CopiarPacote', 'Copiar Pacote', '', '', mkProcedure, 0, nil, _stWORKFLOW);
  end;
end;

procedure InitPapel; begin
  with Prevalence.Metadata('TPapel') do begin
    InheritMetadata(0);
    AddMetadata(0, '', '', 'Designer');
    AddClassTags('PropOrder', '<Identificação>,Nome,Apelido,NomeAntigo,<Detalhamento>,Agregacao,Multiplicidade,Minimo,Maximo,Caracteristicas,Constraint,Inicial,Ordem,<Apresentação>,Habilitacao,Visibilidade,<Validação>,Validacao,MensagemValidacao ' + 
				',>Documentação<,/Ajuda/,Documentacao');
    AddMetadata(1, '', '', TAssociacao, 'PapelOrigem');
    AddMetadata(2, '', '', TAssociacao, 'PapelDestino');
    AddMetadata(3, '', '', TIndice, '');
    AddMetadata(4, 'Nome Antigo', '');
    AddMetadata(6, 'Mensagem de Validação', '', 'Memo');
    AddConstraints(6, [VISIBLE]);
    AddMetadata(7, 'Condição de Validação', '', 'Memo');
    AddMetadata(8, 'Condição de Visibilidade', '', 'Memo');
    AddMetadata(9, 'Condição de Habilitação', '', 'Memo');
    AddMetadata(11, 'Máximo', '');
    AddConstraints(11, [CHECK,VISIBLE]);
    AddMetadata(12, 'Mínimo', '');
    AddConstraints(12, [CHECK,VISIBLE]);
    AddMetadata(13, 'Características', '');
    AddMetadata(15, 'Agregação', '');
  end;
end;

procedure InitTransicao; begin
  with Prevalence.Metadata('TTransicao') do begin
    InheritMetadata(1);
    AddMetadata(0, 'Transição', '', 'Designer');
    AddClassTags('PropOrder', '<Identificação>,Nome,<Detalhamento>,Acao,Condicao,>Documentação<,/Ajuda/,Documentacao');
    AddMetadata(1, '', '', TEstadoBase, 'Origens');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, '', '', TEstadoBase, 'Destinos');
    AddConstraints(2, [NOTNULL]);
    AddMetadata(4, 'hidden', '');
    AddMetadata(5, 'hidden', '');
    AddMetadata(6, 'Ação', '', 'Memo');
    AddMetadata(7, 'Condição', '', 'memo');
    AddConstraints(7, [VISIBLE]);
    AddMethod(0, 'PorPontoOrigem', '', '', '', mkFunction, 0, TypeInfo(Integer), _stINDEX);
  end;
end;

procedure InitClasse; begin
  with Prevalence.Metadata('TClasse') do begin
    InheritMetadata(0);
    AddMetadata(0, '', '', 'Designer');
    AddClassTags('PropOrder', '<Identificação>,Nome, Apelido, Identificacao, NomeAntigo, <Detalhamento>, Tipo, Oculto, ' + 
				'Apresentacao, PropOrder, >Documentação<, Ajuda, Documentacao, Atributos,Metodos,Indices');
    AddMetadata(1, '', '', TLink, 'Classe');
    AddConstraints(1, [COMPOSITE]);
    AddMetadata(2, '', '', TAtributo, 'Classe');
    AddConstraints(2, [COMPOSITE]);
    AddMetadata(3, 'Métodos', '', TMetodo, 'Classe');
    AddConstraints(3, [COMPOSITE]);
    AddMetadata(4, 'Índices', '', TIndice, 'Classe');
    AddConstraints(4, [COMPOSITE]);
    AddUnidirectional(TElementoTipado, 'TipoClasse', []);
    AddMetadata(5, 'Hidden', '');
    AddConstraints(5, [READONLY]);
    AddMetadata(6, 'Hidden', '');
    AddConstraints(6, [READONLY]);
    AddMetadata(7, 'Ordem dos Atributos', '', 'Memo');
    AddMetadata(8, 'Nome Antigo', '');
    AddMetadata(10, 'Apresentação', '');
    AddMetadata(12, 'Identificação', '', 'Memo');
  end;
end;

procedure InitEstado; begin
  with Prevalence.Metadata('TEstado') do begin
    InheritMetadata(0);
    AddMetadata(0, '', '', 'Designer');
    AddClassTags('PropOrder', '<Identificação>,Nome,Apelido,<Detalhamento>,Tipo,/Acao/,MostrarAcao,>Documentação<,/Ajuda/,Documentacao,Variaveis');
    AddMetadata(1, 'Variáveis', '', TVariavel, 'Estado');
    AddConstraints(1, [COMPOSITE]);
    AddMetadata(2, 'Mostrar Ação no Diagrama', '');
    AddMetadata(3, 'hidden', '');
    AddConstraints(3, [READONLY]);
    AddMetadata(5, 'Ação', '', 'memo');
    AddConstraints(5, [CHECK]);
    AddMetadata(6, 'hidden', '');
  end;
end;

procedure InitFormulario; begin
  with Prevalence.Metadata('TFormulario') do begin
    InheritMetadata(0);
    AddMetadata(0, 'Formulário', '', 'Designer');
    AddClassTags('PropOrder', '<Identificação>,Nome,Apelido,<Detalhamento>,Objeto,SomenteLeitura,>Documentação<, ' + 
				'Ajuda,Documentacao,Formatacoes');
    AddMetadata(1, 'Formatações', '', TFormatacao, 'Formulario');
    AddConstraints(1, [COMPOSITE]);
    AddMetadata(2, 'hidden', '');
    AddConstraints(2, [READONLY]);
    AddMetadata(3, 'Somente Leitura', '');
  end;
end;

procedure InitVariavel; begin
  with Prevalence.Metadata('TVariavel') do begin
    InheritMetadata(0);
    AddMetadata(0, 'Variável', '', 'Designer');
    AddClassTags('PropOrder', 'Nome,Tipo,TipoClasse,Tamanho,Decimais,Sinalizado,Valores,Mascara,Inicial>Documentação<,/Ajuda/,Documentacao');
    AddMetadata(1, 'Método', '', TMetodo, 'Variaveis');
    AddMetadata(2, '', '', TEstado, 'Variaveis');
    AddMetadata(3, '', '', 'Memo');
  end;
end;

procedure InitValidacao; begin
  with Prevalence.Metadata('TValidacao') do begin
    InheritMetadata(0);
    AddMetadata(0, 'Validação de Variável', '', 'Designer');
    AddMetadata(1, 'Mensagem da Validação', '', 'Memo');
    AddConstraints(1, [CHECK]);
    AddMetadata(2, 'Condição de Validação', '', 'memo 40,6');
  end;
end;

procedure InitAtributo; begin
  with Prevalence.Metadata('TAtributo') do begin
    InheritMetadata(1);
    AddMetadata(0, '', '', 'Designer');
    AddClassTags('PropOrder', '<Identificação>,Nome,Apelido,NomeAntigo,<Detalhamento>,Tipo,TipoClasse,Tamanho,Decimais,Sinalizado,Valores,Mascara, ' + 
				'Escopo,Caracteristicas,Inicial,<Apresentação>,Ordem,Habilitacao,Visibilidade,<Validação>,Validacao,MensagemValidacao,>Documentação<,/Ajuda/,Documentacao');
    AddMetadata(1, '', '', TClasse, 'Atributos');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, 'Nome Antigo', '');
    AddMetadata(3, '', '', 'memo 40,6');
    AddMetadata(4, 'Condição de Habilitação', '', 'memo 40,6');
    AddMetadata(5, '', '');
    AddConstraints(5, [CHECK]);
    AddMetadata(6, 'Características', '');
    AddMetadata(7, '', '');
    AddMethod(0, 'PorOrdem', '', '', '', mkFunction, 0, TypeInfo(Word), _stINDEX);
  end;
end;

procedure InitParametro; begin
  with Prevalence.Metadata('TParametro') do begin
    InheritMetadata(0);
    AddMetadata(0, 'Parâmetro', '', 'Designer');
    AddClassTags('PropOrder', 'Nome,Tipo,TipoClasse,Passagem,Tamanho,Decimais,Sinalizado,Valores,Mascara,Inicial,Validacao,MensagemValidacao>Documentação<,/Ajuda/,Documentacao');
  end;
end;

procedure InitpitinnuModel; begin
  InitLists;
  InitAjuda;
  InitAparencia;
  InitAssociacao;
  InitComentario;
  InitCompilador;
  InitElemento;
  InitFormatacao;
  InitGeracao;
  InitHeranca;
  InitParametroInicializacao;
  InitServidor;
  InitVersao;
  InitElementoTipado;
  InitEstadoBase;
  InitIndice;
  InitLink;
  InitMetodo;
  InitModelo;
  InitPacote;
  InitPapel;
  InitTransicao;
  InitClasse;
  InitEstado;
  InitFormulario;
  InitVariavel;
  InitValidacao;
  InitAtributo;
  InitParametro;
end;

// Methods

{ TAjuda }

function TAjuda.GetDica : String; begin
  Result := TAjuda(Prevalence.GetNewImage(Self, 2))._Dica;
end;

procedure TAjuda.SetDica(Value : String); begin
  if Prevalence.IsInRecover then _Dica := Value else begin
    if (_Dica = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAjuda(NewImage)._Dica <> Value then begin
        TAjuda(NewImage)._Dica := Value;
        UpdateLog(2, NewImage, Stream)
      end;
  end;
end;

function TAjuda.GetTexto : String; begin
  Result := TAjuda(Prevalence.GetNewImage(Self, 3))._Texto;
end;

procedure TAjuda.SetTexto(Value : String); begin
  if Prevalence.IsInRecover then _Texto := Value else begin
    if (_Texto = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAjuda(NewImage)._Texto <> Value then begin
        TAjuda(NewImage)._Texto := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function TAjuda.GetElemento : TElemento; begin
  Result := TAjuda(Prevalence.GetNewImage(Self, 4))._Elemento;
  FixReference(TTransient(Result), TElemento);
end;

procedure TAjuda.SetElemento(Value : TElemento); begin
  if Prevalence.IsInRecover then _Elemento := Value else begin
    if (_Elemento = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAjuda(NewImage)._Elemento <> Value then begin
        TAjuda(NewImage)._Elemento := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

{ TAjudaList }
class function TAjudaList.GetObjectClass : TTransientClass; begin Result := TAjuda; end;
procedure TAjudaList.Add(Ajuda : TAjuda); begin inherited Add(TPrevalent(Ajuda)); end;
procedure TAjudaList.Delete(Ajuda : TAjuda); begin inherited Delete(TPrevalent(Ajuda)); end;
function TAjudaList.First : TAjuda; begin Result := TAjuda(inherited First); end;
function TAjudaList.Last : TAjuda; begin Result := TAjuda(inherited Last); end;
function TAjudaList.Next(var Ajuda : TAjuda) : boolean; begin Result := inherited Next(TTransient(Ajuda)); end;
function TAjudaList.Prior(var Ajuda : TAjuda) : boolean; begin Result := inherited Prior(TTransient(Ajuda)); end;
function TAjudaList.Find(I : Integer) : TAjuda; begin Result := TAjuda(inherited Find(I)); end;
function TAjudaList.Near(I : Integer) : TAjuda; begin Result := TAjuda(inherited Near(I)); end;

{ TAparencia }

procedure TAparencia.New; begin
  inherited;
  if Prevalence.IsInRecover then exit;
  try _Cor := $00DEF7FE except end;
end;

function TAparencia.GetX : Integer; begin
  Result := TAparencia(Prevalence.GetNewImage(Self, 2))._X;
end;

procedure TAparencia.SetX(Value : Integer); begin
  if Prevalence.IsInRecover then _X := Value else begin
    if (_X = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAparencia(NewImage)._X <> Value then begin
        TAparencia(NewImage)._X := Value;
        UpdateLog(2, NewImage, Stream)
      end;
  end;
end;

function TAparencia.GetY : Integer; begin
  Result := TAparencia(Prevalence.GetNewImage(Self, 3))._Y;
end;

procedure TAparencia.SetY(Value : Integer); begin
  if Prevalence.IsInRecover then _Y := Value else begin
    if (_Y = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAparencia(NewImage)._Y <> Value then begin
        TAparencia(NewImage)._Y := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function TAparencia.GetH : Integer; begin
  Result := TAparencia(Prevalence.GetNewImage(Self, 4))._H;
end;

procedure TAparencia.SetH(Value : Integer); begin
  if Prevalence.IsInRecover then _H := Value else begin
    if (_H = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAparencia(NewImage)._H <> Value then begin
        TAparencia(NewImage)._H := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

function TAparencia.GetW : Integer; begin
  Result := TAparencia(Prevalence.GetNewImage(Self, 5))._W;
end;

procedure TAparencia.SetW(Value : Integer); begin
  if Prevalence.IsInRecover then _W := Value else begin
    if (_W = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAparencia(NewImage)._W <> Value then begin
        TAparencia(NewImage)._W := Value;
        UpdateLog(5, NewImage, Stream)
      end;
  end;
end;

function TAparencia.GetCor : Integer; begin
  Result := TAparencia(Prevalence.GetNewImage(Self, 6))._Cor;
end;

procedure TAparencia.SetCor(Value : Integer); begin
  if Prevalence.IsInRecover then _Cor := Value else begin
    if (_Cor = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAparencia(NewImage)._Cor <> Value then begin
        TAparencia(NewImage)._Cor := Value;
        UpdateLog(6, NewImage, Stream)
      end;
  end;
end;

{ TAparenciaList }
class function TAparenciaList.GetObjectClass : TTransientClass; begin Result := TAparencia; end;
procedure TAparenciaList.Add(Aparencia : TAparencia); begin inherited Add(TPrevalent(Aparencia)); end;
procedure TAparenciaList.Delete(Aparencia : TAparencia); begin inherited Delete(TPrevalent(Aparencia)); end;
function TAparenciaList.First : TAparencia; begin Result := TAparencia(inherited First); end;
function TAparenciaList.Last : TAparencia; begin Result := TAparencia(inherited Last); end;
function TAparenciaList.Next(var Aparencia : TAparencia) : boolean; begin Result := inherited Next(TTransient(Aparencia)); end;
function TAparenciaList.Prior(var Aparencia : TAparencia) : boolean; begin Result := inherited Prior(TTransient(Aparencia)); end;
function TAparenciaList.Find(I : Integer) : TAparencia; begin Result := TAparencia(inherited Find(I)); end;
function TAparenciaList.Near(I : Integer) : TAparencia; begin Result := TAparencia(inherited Near(I)); end;

{ TAssociacao }

function TAssociacao.GetPontoOrigem : Byte; begin
  Result := TAssociacao(Prevalence.GetNewImage(Self, 2))._PontoOrigem;
end;

procedure TAssociacao.SetPontoOrigem(Value : Byte); begin
  if Prevalence.IsInRecover then _PontoOrigem := Value else begin
    if (_PontoOrigem = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAssociacao(NewImage)._PontoOrigem <> Value then begin
        TAssociacao(NewImage)._PontoOrigem := Value;
        UpdateLog(2, NewImage, Stream)
      end;
  end;
end;

function TAssociacao.GetPontoDestino : Byte; begin
  Result := TAssociacao(Prevalence.GetNewImage(Self, 3))._PontoDestino;
end;

procedure TAssociacao.SetPontoDestino(Value : Byte); begin
  if Prevalence.IsInRecover then _PontoDestino := Value else begin
    if (_PontoDestino = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAssociacao(NewImage)._PontoDestino <> Value then begin
        TAssociacao(NewImage)._PontoDestino := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function TAssociacao.GetQuebras : String; begin
  Result := TAssociacao(Prevalence.GetNewImage(Self, 4))._Quebras;
end;

procedure TAssociacao.SetQuebras(Value : String); begin
  if Prevalence.IsInRecover then _Quebras := Value else begin
    if (_Quebras = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAssociacao(NewImage)._Quebras <> Value then begin
        TAssociacao(NewImage)._Quebras := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

function TAssociacao.GetPapelDestino : TPapel; begin
  Result := TAssociacao(Prevalence.GetNewImage(Self, 5))._PapelDestino;
  FixReference(TTransient(Result), TPapel);
end;

procedure TAssociacao.SetPapelDestino(Value : TPapel); begin
  if Prevalence.IsInRecover then _PapelDestino := Value else begin
    if (_PapelDestino = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAssociacao(NewImage)._PapelDestino <> Value then begin
        TAssociacao(NewImage)._PapelDestino := Value;
        UpdateLog(5, NewImage, Stream)
      end;
  end;
end;

function TAssociacao.GetPapelOrigem : TPapel; begin
  Result := TAssociacao(Prevalence.GetNewImage(Self, 6))._PapelOrigem;
  FixReference(TTransient(Result), TPapel);
end;

procedure TAssociacao.SetPapelOrigem(Value : TPapel); begin
  if Prevalence.IsInRecover then _PapelOrigem := Value else begin
    if (_PapelOrigem = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAssociacao(NewImage)._PapelOrigem <> Value then begin
        TAssociacao(NewImage)._PapelOrigem := Value;
        UpdateLog(6, NewImage, Stream)
      end;
  end;
end;

function TAssociacao.GetOrigem : TLink; begin
  Result := TAssociacao(Prevalence.GetNewImage(Self, 7))._Origem;
  FixReference(TTransient(Result), TLink);
end;

procedure TAssociacao.SetOrigem(Value : TLink); begin
  if Prevalence.IsInRecover then _Origem := Value else begin
    if (_Origem = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAssociacao(NewImage)._Origem <> Value then begin
        TAssociacao(NewImage)._Origem := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

function TAssociacao.GetDestino : TLink; begin
  Result := TAssociacao(Prevalence.GetNewImage(Self, 8))._Destino;
  FixReference(TTransient(Result), TLink);
end;

procedure TAssociacao.SetDestino(Value : TLink); begin
  if Prevalence.IsInRecover then _Destino := Value else begin
    if (_Destino = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAssociacao(NewImage)._Destino <> Value then begin
        TAssociacao(NewImage)._Destino := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

function TAssociacao.CheckPontoOrigem(var Message : String) : Boolean; begin
  Result := (papelOrigem <> nil) or (PapelDestino <> nil);
  if Result then
    Message := ''
  else
    Message := 'É necessário definir Papel Origem e/ou PapelDestino na associação'
end;

{ TAssociacaoList }
class function TAssociacaoList.GetObjectClass : TTransientClass; begin Result := TAssociacao; end;
procedure TAssociacaoList.Add(Associacao : TAssociacao); begin inherited Add(TPrevalent(Associacao)); end;
procedure TAssociacaoList.Delete(Associacao : TAssociacao); begin inherited Delete(TPrevalent(Associacao)); end;
function TAssociacaoList.First : TAssociacao; begin Result := TAssociacao(inherited First); end;
function TAssociacaoList.Last : TAssociacao; begin Result := TAssociacao(inherited Last); end;
function TAssociacaoList.Next(var Associacao : TAssociacao) : boolean; begin Result := inherited Next(TTransient(Associacao)); end;
function TAssociacaoList.Prior(var Associacao : TAssociacao) : boolean; begin Result := inherited Prior(TTransient(Associacao)); end;
function TAssociacaoList.Find(I : Integer) : TAssociacao; begin Result := TAssociacao(inherited Find(I)); end;
function TAssociacaoList.Near(I : Integer) : TAssociacao; begin Result := TAssociacao(inherited Near(I)); end;

{ TLinkAssociacoesDestinoAssociation }
class function TLinkAssociacoesDestinoAssociation.GetObjectClass : TTransientClass; begin Result := TAssociacao; end;
procedure TLinkAssociacoesDestinoAssociation.Add(Associacao : TAssociacao); begin inherited Add(TPrevalent(Associacao)); end;
procedure TLinkAssociacoesDestinoAssociation.Delete(Associacao : TAssociacao); begin inherited Delete(TPrevalent(Associacao)); end;
function TLinkAssociacoesDestinoAssociation.First : TAssociacao; begin SetDependencyLists; Result := TAssociacao(inherited First); end;
function TLinkAssociacoesDestinoAssociation.Last : TAssociacao; begin SetDependencyLists; Result := TAssociacao(inherited Last); end;
function TLinkAssociacoesDestinoAssociation.Next(var Associacao : TAssociacao) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Associacao)); end;
function TLinkAssociacoesDestinoAssociation.Prior(var Associacao : TAssociacao) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Associacao)); end;
function TLinkAssociacoesDestinoAssociation.Find(I : Integer) : TAssociacao; begin Result := TAssociacao(inherited Find(I)); end;
function TLinkAssociacoesDestinoAssociation.Near(I : Integer) : TAssociacao; begin SetDependencyLists; Result := TAssociacao(inherited Near(I)); end;

{ TLinkAssociacoesOrigemAssociation }
class function TLinkAssociacoesOrigemAssociation.GetObjectClass : TTransientClass; begin Result := TAssociacao; end;
procedure TLinkAssociacoesOrigemAssociation.Add(Associacao : TAssociacao); begin inherited Add(TPrevalent(Associacao)); end;
procedure TLinkAssociacoesOrigemAssociation.Delete(Associacao : TAssociacao); begin inherited Delete(TPrevalent(Associacao)); end;
function TLinkAssociacoesOrigemAssociation.First : TAssociacao; begin SetDependencyLists; Result := TAssociacao(inherited First); end;
function TLinkAssociacoesOrigemAssociation.Last : TAssociacao; begin SetDependencyLists; Result := TAssociacao(inherited Last); end;
function TLinkAssociacoesOrigemAssociation.Next(var Associacao : TAssociacao) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Associacao)); end;
function TLinkAssociacoesOrigemAssociation.Prior(var Associacao : TAssociacao) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Associacao)); end;
function TLinkAssociacoesOrigemAssociation.Find(I : Integer) : TAssociacao; begin Result := TAssociacao(inherited Find(I)); end;
function TLinkAssociacoesOrigemAssociation.Near(I : Integer) : TAssociacao; begin SetDependencyLists; Result := TAssociacao(inherited Near(I)); end;

{ TComentario }

function TComentario.GetTexto : String; begin
  Result := TComentario(Prevalence.GetNewImage(Self, 2))._Texto;
end;

procedure TComentario.SetTexto(Value : String); begin
  if Prevalence.IsInRecover then _Texto := Value else begin
    if (_Texto = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TComentario(NewImage)._Texto <> Value then begin
        TComentario(NewImage)._Texto := Value;
        UpdateLog(2, NewImage, Stream)
      end;
  end;
end;

function TComentario.GetPacote : TPacote; begin
  Result := TComentario(Prevalence.GetNewImage(Self, 3))._Pacote;
  FixReference(TTransient(Result), TPacote);
end;

procedure TComentario.SetPacote(Value : TPacote); begin
  if Prevalence.IsInRecover then _Pacote := Value else begin
    if (_Pacote = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TComentario(NewImage)._Pacote <> Value then begin
        TComentario(NewImage)._Pacote := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function TComentario.GetAparencia : TAparencia; begin
  Result := TComentario(Prevalence.GetNewImage(Self, 4))._Aparencia;
  FixReference(TTransient(Result), TAparencia);
end;

procedure TComentario.SetAparencia(Value : TAparencia); begin
  if Prevalence.IsInRecover then _Aparencia := Value else begin
    if (_Aparencia = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TComentario(NewImage)._Aparencia <> Value then begin
        TComentario(NewImage)._Aparencia := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

function TComentario.GetMetodo : TMetodo; begin
  Result := TComentario(Prevalence.GetNewImage(Self, 5))._Metodo;
  FixReference(TTransient(Result), TMetodo);
end;

procedure TComentario.SetMetodo(Value : TMetodo); begin
  if Prevalence.IsInRecover then _Metodo := Value else begin
    if (_Metodo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TComentario(NewImage)._Metodo <> Value then begin
        TComentario(NewImage)._Metodo := Value;
        UpdateLog(5, NewImage, Stream)
      end;
  end;
end;

{ TComentarioList }
class function TComentarioList.GetObjectClass : TTransientClass; begin Result := TComentario; end;
procedure TComentarioList.Add(Comentario : TComentario); begin inherited Add(TPrevalent(Comentario)); end;
procedure TComentarioList.Delete(Comentario : TComentario); begin inherited Delete(TPrevalent(Comentario)); end;
function TComentarioList.First : TComentario; begin Result := TComentario(inherited First); end;
function TComentarioList.Last : TComentario; begin Result := TComentario(inherited Last); end;
function TComentarioList.Next(var Comentario : TComentario) : boolean; begin Result := inherited Next(TTransient(Comentario)); end;
function TComentarioList.Prior(var Comentario : TComentario) : boolean; begin Result := inherited Prior(TTransient(Comentario)); end;
function TComentarioList.Find(I : Integer) : TComentario; begin Result := TComentario(inherited Find(I)); end;
function TComentarioList.Near(I : Integer) : TComentario; begin Result := TComentario(inherited Near(I)); end;

{ TPacoteComentariosAssociation }
class function TPacoteComentariosAssociation.GetObjectClass : TTransientClass; begin Result := TComentario; end;
procedure TPacoteComentariosAssociation.Add(Comentario : TComentario); begin inherited Add(TPrevalent(Comentario)); end;
procedure TPacoteComentariosAssociation.Delete(Comentario : TComentario); begin inherited Delete(TPrevalent(Comentario)); end;
function TPacoteComentariosAssociation.First : TComentario; begin SetDependencyLists; Result := TComentario(inherited First); end;
function TPacoteComentariosAssociation.Last : TComentario; begin SetDependencyLists; Result := TComentario(inherited Last); end;
function TPacoteComentariosAssociation.Next(var Comentario : TComentario) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Comentario)); end;
function TPacoteComentariosAssociation.Prior(var Comentario : TComentario) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Comentario)); end;
function TPacoteComentariosAssociation.Find(I : Integer) : TComentario; begin Result := TComentario(inherited Find(I)); end;
function TPacoteComentariosAssociation.Near(I : Integer) : TComentario; begin SetDependencyLists; Result := TComentario(inherited Near(I)); end;

{ TMetodoComentariosAssociation }
class function TMetodoComentariosAssociation.GetObjectClass : TTransientClass; begin Result := TComentario; end;
procedure TMetodoComentariosAssociation.Add(Comentario : TComentario); begin inherited Add(TPrevalent(Comentario)); end;
procedure TMetodoComentariosAssociation.Delete(Comentario : TComentario); begin inherited Delete(TPrevalent(Comentario)); end;
function TMetodoComentariosAssociation.First : TComentario; begin SetDependencyLists; Result := TComentario(inherited First); end;
function TMetodoComentariosAssociation.Last : TComentario; begin SetDependencyLists; Result := TComentario(inherited Last); end;
function TMetodoComentariosAssociation.Next(var Comentario : TComentario) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Comentario)); end;
function TMetodoComentariosAssociation.Prior(var Comentario : TComentario) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Comentario)); end;
function TMetodoComentariosAssociation.Find(I : Integer) : TComentario; begin Result := TComentario(inherited Find(I)); end;
function TMetodoComentariosAssociation.Near(I : Integer) : TComentario; begin SetDependencyLists; Result := TComentario(inherited Near(I)); end;

{ TCompilador }

function TCompilador.GetNome : String; begin
  Result := TCompilador(Prevalence.GetNewImage(Self, 2))._Nome;
end;

procedure TCompilador.SetNome(Value : String); begin
  if Prevalence.IsInRecover then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TCompilador(NewImage)._Nome <> Value then begin
        TCompilador(NewImage)._Nome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
  end;
end;

function TCompilador.GetExecutavel : String; begin
  Result := TCompilador(Prevalence.GetNewImage(Self, 3))._Executavel;
end;

procedure TCompilador.SetExecutavel(Value : String); begin
  if Prevalence.IsInRecover then _Executavel := Value else begin
    if (_Executavel = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TCompilador(NewImage)._Executavel <> Value then begin
        TCompilador(NewImage)._Executavel := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function TCompilador.GetOpcoes : String; begin
  Result := TCompilador(Prevalence.GetNewImage(Self, 4))._Opcoes;
end;

procedure TCompilador.SetOpcoes(Value : String); begin
  if Prevalence.IsInRecover then _Opcoes := Value else begin
    if (_Opcoes = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TCompilador(NewImage)._Opcoes <> Value then begin
        TCompilador(NewImage)._Opcoes := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

{ TCompiladorList }
class function TCompiladorList.GetObjectClass : TTransientClass; begin Result := TCompilador; end;
procedure TCompiladorList.Add(Compilador : TCompilador); begin inherited Add(TPrevalent(Compilador)); end;
procedure TCompiladorList.Delete(Compilador : TCompilador); begin inherited Delete(TPrevalent(Compilador)); end;
function TCompiladorList.First : TCompilador; begin Result := TCompilador(inherited First); end;
function TCompiladorList.Last : TCompilador; begin Result := TCompilador(inherited Last); end;
function TCompiladorList.Next(var Compilador : TCompilador) : boolean; begin Result := inherited Next(TTransient(Compilador)); end;
function TCompiladorList.Prior(var Compilador : TCompilador) : boolean; begin Result := inherited Prior(TTransient(Compilador)); end;
function TCompiladorList.Find(I : Integer) : TCompilador; begin Result := TCompilador(inherited Find(I)); end;
function TCompiladorList.Near(I : Integer) : TCompilador; begin Result := TCompilador(inherited Near(I)); end;

{ TElemento }

function TElemento.GetNome : String; begin
  Result := TElemento(Prevalence.GetNewImage(Self, 2))._Nome;
end;

procedure TElemento.SetNome(Value : String); begin
  if Prevalence.IsInRecover then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TElemento(NewImage)._Nome <> Value then begin
        TElemento(NewImage)._Nome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
  end;
end;

function TElemento.GetApelido : String; begin
  Result := TElemento(Prevalence.GetNewImage(Self, 3))._Apelido;
end;

procedure TElemento.SetApelido(Value : String); begin
  if Prevalence.IsInRecover then _Apelido := Value else begin
    if (_Apelido = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TElemento(NewImage)._Apelido <> Value then begin
        TElemento(NewImage)._Apelido := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function TElemento.GetDocumentacao : String; begin
  Result := TElemento(Prevalence.GetNewImage(Self, 4))._Documentacao;
end;

procedure TElemento.SetDocumentacao(Value : String); begin
  if Prevalence.IsInRecover then _Documentacao := Value else begin
    if (_Documentacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TElemento(NewImage)._Documentacao <> Value then begin
        TElemento(NewImage)._Documentacao := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

function TElemento.GetAjuda : TAjuda; begin
  Result := TElemento(Prevalence.GetNewImage(Self, 5))._Ajuda;
  FixReference(TTransient(Result), TAjuda);
end;

procedure TElemento.SetAjuda(Value : TAjuda); begin
  if Prevalence.IsInRecover then _Ajuda := Value else begin
    if (_Ajuda = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TElemento(NewImage)._Ajuda <> Value then begin
        TElemento(NewImage)._Ajuda := Value;
        UpdateLog(5, NewImage, Stream)
      end;
  end;
end;

function TElemento.GetAparencia : TAparencia; begin
  Result := TElemento(Prevalence.GetNewImage(Self, 6))._Aparencia;
  FixReference(TTransient(Result), TAparencia);
end;

procedure TElemento.SetAparencia(Value : TAparencia); begin
  if Prevalence.IsInRecover then _Aparencia := Value else begin
    if (_Aparencia = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TElemento(NewImage)._Aparencia <> Value then begin
        TElemento(NewImage)._Aparencia := Value;
        UpdateLog(6, NewImage, Stream)
      end;
  end;
end;

function TElemento.PorNome : String; begin
  Result := Nome
end;
function TElemento.ChecarNome : Boolean; begin Result := Elemento_ChecarNome(Self) end;
function TElemento.DescreverAssociacao(pAssociacao : TAssociation) : String; begin Result := Elemento_DescreverAssociacao(Self, pAssociacao) end;
function TElemento.ExisteNomeDuplicado(pLista : TObjectList) : Boolean; begin Result := Elemento_ExisteNomeDuplicado(Self, pLista) end;

class function TElementoList.GetObjectClass : TTransientClass; begin
  Result := TElemento
end;

{ TFormatacao }

function TFormatacao.GetIdentification : string; begin
  try Result := ifthen(Comando in [fcEditar, fcMostrar], Propriedade, Titulo) + ': ' + EnumToStr(TypeInfo(TFormatacaoComando), Integer(Comando)) except Result := ' ' end;
end;

function TFormatacao.GetOrdem : Word; begin
  Result := TFormatacao(Prevalence.GetNewImage(Self, 2))._Ordem;
end;

procedure TFormatacao.SetOrdem(Value : Word); begin
  if Prevalence.IsInRecover then _Ordem := Value else begin
    if (_Ordem = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TFormatacao(NewImage)._Ordem <> Value then begin
        TFormatacao(NewImage)._Ordem := Value;
        UpdateLog(2, NewImage, Stream)
      end;
  end;
end;

function TFormatacao.GetComando : TFormatacaoComando; begin
  Result := TFormatacao(Prevalence.GetNewImage(Self, 3))._Comando;
end;

procedure TFormatacao.SetComando(Value : TFormatacaoComando); begin
  if Prevalence.IsInRecover then _Comando := Value else begin
    if (_Comando = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TFormatacao(NewImage)._Comando <> Value then begin
        TFormatacao(NewImage)._Comando := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function TFormatacao.GetTitulo : String; begin
  Result := TFormatacao(Prevalence.GetNewImage(Self, 4))._Titulo;
end;

procedure TFormatacao.SetTitulo(Value : String); begin
  if Prevalence.IsInRecover then _Titulo := Value else begin
    if (_Titulo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TFormatacao(NewImage)._Titulo <> Value then begin
        TFormatacao(NewImage)._Titulo := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

function TFormatacao.GetPropriedade : String; begin
  Result := TFormatacao(Prevalence.GetNewImage(Self, 5))._Propriedade;
end;

procedure TFormatacao.SetPropriedade(Value : String); begin
  if Prevalence.IsInRecover then _Propriedade := Value else begin
    if (_Propriedade = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TFormatacao(NewImage)._Propriedade <> Value then begin
        TFormatacao(NewImage)._Propriedade := Value;
        UpdateLog(5, NewImage, Stream)
      end;
  end;
end;

function TFormatacao.GetLayout : TFormatacaoLayout; begin
  Result := TFormatacao(Prevalence.GetNewImage(Self, 6))._Layout;
end;

procedure TFormatacao.SetLayout(Value : TFormatacaoLayout); begin
  if Prevalence.IsInRecover then _Layout := Value else begin
    if (_Layout = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TFormatacao(NewImage)._Layout <> Value then begin
        TFormatacao(NewImage)._Layout := Value;
        UpdateLog(6, NewImage, Stream)
      end;
  end;
end;

function TFormatacao.GetFormulario : TFormulario; begin
  Result := TFormatacao(Prevalence.GetNewImage(Self, 7))._Formulario;
  FixReference(TTransient(Result), TFormulario);
end;

procedure TFormatacao.SetFormulario(Value : TFormulario); begin
  if Prevalence.IsInRecover then _Formulario := Value else begin
    if (_Formulario = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TFormatacao(NewImage)._Formulario <> Value then begin
        TFormatacao(NewImage)._Formulario := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

function TFormatacao.PorOrdem : Word; begin
  Result := Ordem
end;

function TFormatacao.CheckOrdem(var Message : String) : Boolean; begin
  Result := true;
  if Result then
    Message := ''
  else
    Message := '.'
end;

function TFormatacao.VisibleTitulo : Boolean; begin
  Result := comando in [FCCriarGrupo, FCCriarGrupoColapsavel, FCCriarGrupoColapsado, FCCriarAba];
end;

function TFormatacao.VisiblePropriedade : Boolean; begin
  Result := comando in [FCEditar,FCMostrar];
end;

function TFormatacao.VisibleLayout : Boolean; begin
  Result := comando in [FCEditar,FCMostrar];
end;

{ TFormatacaoList }
class function TFormatacaoList.GetObjectClass : TTransientClass; begin Result := TFormatacao; end;
procedure TFormatacaoList.Add(Formatacao : TFormatacao); begin inherited Add(TPrevalent(Formatacao)); end;
procedure TFormatacaoList.Delete(Formatacao : TFormatacao); begin inherited Delete(TPrevalent(Formatacao)); end;
function TFormatacaoList.First : TFormatacao; begin Result := TFormatacao(inherited First); end;
function TFormatacaoList.Last : TFormatacao; begin Result := TFormatacao(inherited Last); end;
function TFormatacaoList.Next(var Formatacao : TFormatacao) : boolean; begin Result := inherited Next(TTransient(Formatacao)); end;
function TFormatacaoList.Prior(var Formatacao : TFormatacao) : boolean; begin Result := inherited Prior(TTransient(Formatacao)); end;
function TFormatacaoList.Find(I : Integer) : TFormatacao; begin Result := TFormatacao(inherited Find(I)); end;
function TFormatacaoList.Near(I : Integer) : TFormatacao; begin Result := TFormatacao(inherited Near(I)); end;

{ TFormatacaoPorOrdemList }
class function TFormatacaoPorOrdemList.GetObjectClass : TTransientClass; begin Result := TFormatacao; end;
procedure TFormatacaoPorOrdemList.Add(Formatacao : TFormatacao); begin inherited Add(TPrevalent(Formatacao)); end;
procedure TFormatacaoPorOrdemList.Delete(Formatacao : TFormatacao); begin inherited Delete(TPrevalent(Formatacao)); end;
function TFormatacaoPorOrdemList.First : TFormatacao; begin Result := TFormatacao(inherited First); end;
function TFormatacaoPorOrdemList.Last : TFormatacao; begin Result := TFormatacao(inherited Last); end;
function TFormatacaoPorOrdemList.Next(var Formatacao : TFormatacao) : boolean; begin Result := inherited Next(TTransient(Formatacao)); end;
function TFormatacaoPorOrdemList.Prior(var Formatacao : TFormatacao) : boolean; begin Result := inherited Prior(TTransient(Formatacao)); end;
function TFormatacaoPorOrdemList.Find(W : Word) : TFormatacao; begin Result := TFormatacao(inherited Find(W)); end;
function TFormatacaoPorOrdemList.Near(W : Word) : TFormatacao; begin Result := TFormatacao(inherited Near(W)); end;
class function TFormatacaoPorOrdemList.GetKeyCode : pointer; begin Result := @TFormatacao.PorOrdem; end;
class function TFormatacaoPorOrdemList.GetListType : TListType; begin Result := ltWord; end;

{ TFormularioFormatacoesAssociation }
class function TFormularioFormatacoesAssociation.GetObjectClass : TTransientClass; begin Result := TFormatacao; end;
procedure TFormularioFormatacoesAssociation.Add(Formatacao : TFormatacao); begin inherited Add(TPrevalent(Formatacao)); end;
procedure TFormularioFormatacoesAssociation.Delete(Formatacao : TFormatacao); begin inherited Delete(TPrevalent(Formatacao)); end;
function TFormularioFormatacoesAssociation.First : TFormatacao; begin SetDependencyLists; Result := TFormatacao(inherited First); end;
function TFormularioFormatacoesAssociation.Last : TFormatacao; begin SetDependencyLists; Result := TFormatacao(inherited Last); end;
function TFormularioFormatacoesAssociation.Next(var Formatacao : TFormatacao) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Formatacao)); end;
function TFormularioFormatacoesAssociation.Prior(var Formatacao : TFormatacao) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Formatacao)); end;
function TFormularioFormatacoesAssociation.Find(W : Word) : TFormatacao; begin Result := TFormatacao(inherited Find(W)); end;
function TFormularioFormatacoesAssociation.Near(W : Word) : TFormatacao; begin SetDependencyLists; Result := TFormatacao(inherited Near(W)); end;
class function TFormularioFormatacoesAssociation.GetKeyCode : pointer; begin Result := @TFormatacao.PorOrdem; end;
class function TFormularioFormatacoesAssociation.GetListType : TListType; begin Result := ltWord; end;

{ TGeracao }

procedure TGeracao.New; begin
  inherited;
  if Prevalence.IsInRecover then exit;
  try _Usuario := Browser.UserInfo.UserName except end;
end;

function TGeracao.GetUsuario : String; begin
  Result := TGeracao(Prevalence.GetNewImage(Self, 2))._Usuario;
end;

procedure TGeracao.SetUsuario(Value : String); begin
  if Prevalence.IsInRecover then _Usuario := Value else begin
    if (_Usuario = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TGeracao(NewImage)._Usuario <> Value then begin
        TGeracao(NewImage)._Usuario := Value;
        UpdateLog(2, NewImage, Stream)
      end;
  end;
end;

function TGeracao.GetAmbiente : TGeracaoAmbiente; begin
  Result := TGeracao(Prevalence.GetNewImage(Self, 3))._Ambiente;
end;

procedure TGeracao.SetAmbiente(Value : TGeracaoAmbiente); begin
  if Prevalence.IsInRecover then _Ambiente := Value else begin
    if (_Ambiente = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TGeracao(NewImage)._Ambiente <> Value then begin
        TGeracao(NewImage)._Ambiente := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function TGeracao.GetPorta : Word; begin
  Result := TGeracao(Prevalence.GetNewImage(Self, 4))._Porta;
end;

procedure TGeracao.SetPorta(Value : Word); begin
  if Prevalence.IsInRecover then _Porta := Value else begin
    if (_Porta = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TGeracao(NewImage)._Porta <> Value then begin
        TGeracao(NewImage)._Porta := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

function TGeracao.GetOpcoes : TSetGeracaoOpcoes; begin
  Result := TGeracao(Prevalence.GetNewImage(Self, 5))._Opcoes;
end;

procedure TGeracao.SetOpcoes(Value : TSetGeracaoOpcoes); begin
  if Prevalence.IsInRecover then _Opcoes := Value else begin
    if (_Opcoes = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TGeracao(NewImage)._Opcoes <> Value then begin
        TGeracao(NewImage)._Opcoes := Value;
        UpdateLog(5, NewImage, Stream)
      end;
  end;
end;

function TGeracao.GetVersao : String; begin
  try Result := ServidorList.Find(1).Versao.Numero except Result := ' ' end;
end;

function TGeracao.GetCompilador : TCompilador; begin
  Result := TGeracao(Prevalence.GetNewImage(Self, 7))._Compilador;
  FixReference(TTransient(Result), TCompilador);
end;

procedure TGeracao.SetCompilador(Value : TCompilador); begin
  if Prevalence.IsInRecover then _Compilador := Value else begin
    if (_Compilador = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TGeracao(NewImage)._Compilador <> Value then begin
        TGeracao(NewImage)._Compilador := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

function TGeracao.GetModelo : TModelo; begin
  Result := TGeracao(Prevalence.GetNewImage(Self, 8))._Modelo;
  FixReference(TTransient(Result), TModelo);
end;

procedure TGeracao.SetModelo(Value : TModelo); begin
  if Prevalence.IsInRecover then _Modelo := Value else begin
    if (_Modelo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TGeracao(NewImage)._Modelo <> Value then begin
        TGeracao(NewImage)._Modelo := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

function TGeracao.PorPorta : Word; begin
  Result := Porta
end;

function TGeracao.PorUsuario : String; begin
  Result := Usuario
end;

{ TGeracaoList }
class function TGeracaoList.GetObjectClass : TTransientClass; begin Result := TGeracao; end;
procedure TGeracaoList.Add(Geracao : TGeracao); begin inherited Add(TPrevalent(Geracao)); end;
procedure TGeracaoList.Delete(Geracao : TGeracao); begin inherited Delete(TPrevalent(Geracao)); end;
function TGeracaoList.First : TGeracao; begin Result := TGeracao(inherited First); end;
function TGeracaoList.Last : TGeracao; begin Result := TGeracao(inherited Last); end;
function TGeracaoList.Next(var Geracao : TGeracao) : boolean; begin Result := inherited Next(TTransient(Geracao)); end;
function TGeracaoList.Prior(var Geracao : TGeracao) : boolean; begin Result := inherited Prior(TTransient(Geracao)); end;
function TGeracaoList.Find(I : Integer) : TGeracao; begin Result := TGeracao(inherited Find(I)); end;
function TGeracaoList.Near(I : Integer) : TGeracao; begin Result := TGeracao(inherited Near(I)); end;

{ TGeracaoPorUsuarioList }
class function TGeracaoPorUsuarioList.GetObjectClass : TTransientClass; begin Result := TGeracao; end;
procedure TGeracaoPorUsuarioList.Add(Geracao : TGeracao); begin inherited Add(TPrevalent(Geracao)); end;
procedure TGeracaoPorUsuarioList.Delete(Geracao : TGeracao); begin inherited Delete(TPrevalent(Geracao)); end;
function TGeracaoPorUsuarioList.First : TGeracao; begin Result := TGeracao(inherited First); end;
function TGeracaoPorUsuarioList.Last : TGeracao; begin Result := TGeracao(inherited Last); end;
function TGeracaoPorUsuarioList.Next(var Geracao : TGeracao) : boolean; begin Result := inherited Next(TTransient(Geracao)); end;
function TGeracaoPorUsuarioList.Prior(var Geracao : TGeracao) : boolean; begin Result := inherited Prior(TTransient(Geracao)); end;
function TGeracaoPorUsuarioList.Find(S : String) : TGeracao; begin Result := TGeracao(inherited Find(S)); end;
function TGeracaoPorUsuarioList.Near(S : String) : TGeracao; begin Result := TGeracao(inherited Near(S)); end;
class function TGeracaoPorUsuarioList.GetKeyCode : pointer; begin Result := @TGeracao.PorUsuario; end;
class function TGeracaoPorUsuarioList.GetListType : TListType; begin Result := ltString; end;

{ TGeracaoPorPortaList }
class function TGeracaoPorPortaList.GetObjectClass : TTransientClass; begin Result := TGeracao; end;
procedure TGeracaoPorPortaList.Add(Geracao : TGeracao); begin inherited Add(TPrevalent(Geracao)); end;
procedure TGeracaoPorPortaList.Delete(Geracao : TGeracao); begin inherited Delete(TPrevalent(Geracao)); end;
function TGeracaoPorPortaList.First : TGeracao; begin Result := TGeracao(inherited First); end;
function TGeracaoPorPortaList.Last : TGeracao; begin Result := TGeracao(inherited Last); end;
function TGeracaoPorPortaList.Next(var Geracao : TGeracao) : boolean; begin Result := inherited Next(TTransient(Geracao)); end;
function TGeracaoPorPortaList.Prior(var Geracao : TGeracao) : boolean; begin Result := inherited Prior(TTransient(Geracao)); end;
function TGeracaoPorPortaList.Find(W : Word) : TGeracao; begin Result := TGeracao(inherited Find(W)); end;
function TGeracaoPorPortaList.Near(W : Word) : TGeracao; begin Result := TGeracao(inherited Near(W)); end;
class function TGeracaoPorPortaList.GetKeyCode : pointer; begin Result := @TGeracao.PorPorta; end;
class function TGeracaoPorPortaList.GetListType : TListType; begin Result := ltWord; end;

{ TModeloGeracoesAssociation }
class function TModeloGeracoesAssociation.GetObjectClass : TTransientClass; begin Result := TGeracao; end;
procedure TModeloGeracoesAssociation.Add(Geracao : TGeracao); begin inherited Add(TPrevalent(Geracao)); end;
procedure TModeloGeracoesAssociation.Delete(Geracao : TGeracao); begin inherited Delete(TPrevalent(Geracao)); end;
function TModeloGeracoesAssociation.First : TGeracao; begin SetDependencyLists; Result := TGeracao(inherited First); end;
function TModeloGeracoesAssociation.Last : TGeracao; begin SetDependencyLists; Result := TGeracao(inherited Last); end;
function TModeloGeracoesAssociation.Next(var Geracao : TGeracao) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Geracao)); end;
function TModeloGeracoesAssociation.Prior(var Geracao : TGeracao) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Geracao)); end;
function TModeloGeracoesAssociation.Find(I : Integer) : TGeracao; begin Result := TGeracao(inherited Find(I)); end;
function TModeloGeracoesAssociation.Near(I : Integer) : TGeracao; begin SetDependencyLists; Result := TGeracao(inherited Near(I)); end;

{ THeranca }

function THeranca.GetPontoDestino : Byte; begin
  Result := THeranca(Prevalence.GetNewImage(Self, 2))._PontoDestino;
end;

procedure THeranca.SetPontoDestino(Value : Byte); begin
  if Prevalence.IsInRecover then _PontoDestino := Value else begin
    if (_PontoDestino = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if THeranca(NewImage)._PontoDestino <> Value then begin
        THeranca(NewImage)._PontoDestino := Value;
        UpdateLog(2, NewImage, Stream)
      end;
  end;
end;

function THeranca.GetPontoOrigem : Byte; begin
  Result := THeranca(Prevalence.GetNewImage(Self, 3))._PontoOrigem;
end;

procedure THeranca.SetPontoOrigem(Value : Byte); begin
  if Prevalence.IsInRecover then _PontoOrigem := Value else begin
    if (_PontoOrigem = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if THeranca(NewImage)._PontoOrigem <> Value then begin
        THeranca(NewImage)._PontoOrigem := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function THeranca.GetQuebras : String; begin
  Result := THeranca(Prevalence.GetNewImage(Self, 4))._Quebras;
end;

procedure THeranca.SetQuebras(Value : String); begin
  if Prevalence.IsInRecover then _Quebras := Value else begin
    if (_Quebras = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if THeranca(NewImage)._Quebras <> Value then begin
        THeranca(NewImage)._Quebras := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

function THeranca.GetOrigem : TLink; begin
  Result := THeranca(Prevalence.GetNewImage(Self, 5))._Origem;
  FixReference(TTransient(Result), TLink);
end;

procedure THeranca.SetOrigem(Value : TLink); begin
  if Prevalence.IsInRecover then _Origem := Value else begin
    if (_Origem = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if THeranca(NewImage)._Origem <> Value then begin
        THeranca(NewImage)._Origem := Value;
        UpdateLog(5, NewImage, Stream)
      end;
  end;
end;

function THeranca.GetDestino : TLink; begin
  Result := THeranca(Prevalence.GetNewImage(Self, 6))._Destino;
  FixReference(TTransient(Result), TLink);
end;

procedure THeranca.SetDestino(Value : TLink); begin
  if Prevalence.IsInRecover then _Destino := Value else begin
    if (_Destino = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if THeranca(NewImage)._Destino <> Value then begin
        THeranca(NewImage)._Destino := Value;
        UpdateLog(6, NewImage, Stream)
      end;
  end;
end;

{ THerancaList }
class function THerancaList.GetObjectClass : TTransientClass; begin Result := THeranca; end;
procedure THerancaList.Add(Heranca : THeranca); begin inherited Add(TPrevalent(Heranca)); end;
procedure THerancaList.Delete(Heranca : THeranca); begin inherited Delete(TPrevalent(Heranca)); end;
function THerancaList.First : THeranca; begin Result := THeranca(inherited First); end;
function THerancaList.Last : THeranca; begin Result := THeranca(inherited Last); end;
function THerancaList.Next(var Heranca : THeranca) : boolean; begin Result := inherited Next(TTransient(Heranca)); end;
function THerancaList.Prior(var Heranca : THeranca) : boolean; begin Result := inherited Prior(TTransient(Heranca)); end;
function THerancaList.Find(I : Integer) : THeranca; begin Result := THeranca(inherited Find(I)); end;
function THerancaList.Near(I : Integer) : THeranca; begin Result := THeranca(inherited Near(I)); end;

{ TLinkHerancasAssociation }
class function TLinkHerancasAssociation.GetObjectClass : TTransientClass; begin Result := THeranca; end;
procedure TLinkHerancasAssociation.Add(Heranca : THeranca); begin inherited Add(TPrevalent(Heranca)); end;
procedure TLinkHerancasAssociation.Delete(Heranca : THeranca); begin inherited Delete(TPrevalent(Heranca)); end;
function TLinkHerancasAssociation.First : THeranca; begin SetDependencyLists; Result := THeranca(inherited First); end;
function TLinkHerancasAssociation.Last : THeranca; begin SetDependencyLists; Result := THeranca(inherited Last); end;
function TLinkHerancasAssociation.Next(var Heranca : THeranca) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Heranca)); end;
function TLinkHerancasAssociation.Prior(var Heranca : THeranca) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Heranca)); end;
function TLinkHerancasAssociation.Find(I : Integer) : THeranca; begin Result := THeranca(inherited Find(I)); end;
function TLinkHerancasAssociation.Near(I : Integer) : THeranca; begin SetDependencyLists; Result := THeranca(inherited Near(I)); end;

{ TParametroInicializacao }

function TParametroInicializacao.GetAplicavel : TSetParametroInicializacaoAplicavel; begin
  Result := TParametroInicializacao(Prevalence.GetNewImage(Self, 2))._Aplicavel;
end;

procedure TParametroInicializacao.SetAplicavel(Value : TSetParametroInicializacaoAplicavel); begin
  if Prevalence.IsInRecover then _Aplicavel := Value else begin
    if (_Aplicavel = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TParametroInicializacao(NewImage)._Aplicavel <> Value then begin
        TParametroInicializacao(NewImage)._Aplicavel := Value;
        UpdateLog(2, NewImage, Stream)
      end;
  end;
end;

function TParametroInicializacao.GetSessao : String; begin
  Result := TParametroInicializacao(Prevalence.GetNewImage(Self, 3))._Sessao;
end;

procedure TParametroInicializacao.SetSessao(Value : String); begin
  if Prevalence.IsInRecover then _Sessao := Value else begin
    if (_Sessao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TParametroInicializacao(NewImage)._Sessao <> Value then begin
        TParametroInicializacao(NewImage)._Sessao := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function TParametroInicializacao.GetNome : String; begin
  Result := TParametroInicializacao(Prevalence.GetNewImage(Self, 4))._Nome;
end;

procedure TParametroInicializacao.SetNome(Value : String); begin
  if Prevalence.IsInRecover then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TParametroInicializacao(NewImage)._Nome <> Value then begin
        TParametroInicializacao(NewImage)._Nome := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

function TParametroInicializacao.GetValor : String; begin
  Result := TParametroInicializacao(Prevalence.GetNewImage(Self, 5))._Valor;
end;

procedure TParametroInicializacao.SetValor(Value : String); begin
  if Prevalence.IsInRecover then _Valor := Value else begin
    if (_Valor = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TParametroInicializacao(NewImage)._Valor <> Value then begin
        TParametroInicializacao(NewImage)._Valor := Value;
        UpdateLog(5, NewImage, Stream)
      end;
  end;
end;

function TParametroInicializacao.PorSessao : String; begin
  Result := Sessao
end;

{ TParametroInicializacaoList }
class function TParametroInicializacaoList.GetObjectClass : TTransientClass; begin Result := TParametroInicializacao; end;
procedure TParametroInicializacaoList.Add(ParametroInicializacao : TParametroInicializacao); begin inherited Add(TPrevalent(ParametroInicializacao)); end;
procedure TParametroInicializacaoList.Delete(ParametroInicializacao : TParametroInicializacao); begin inherited Delete(TPrevalent(ParametroInicializacao)); end;
function TParametroInicializacaoList.First : TParametroInicializacao; begin Result := TParametroInicializacao(inherited First); end;
function TParametroInicializacaoList.Last : TParametroInicializacao; begin Result := TParametroInicializacao(inherited Last); end;
function TParametroInicializacaoList.Next(var ParametroInicializacao : TParametroInicializacao) : boolean; begin Result := inherited Next(TTransient(ParametroInicializacao)); end;
function TParametroInicializacaoList.Prior(var ParametroInicializacao : TParametroInicializacao) : boolean; begin Result := inherited Prior(TTransient(ParametroInicializacao)); end;
function TParametroInicializacaoList.Find(I : Integer) : TParametroInicializacao; begin Result := TParametroInicializacao(inherited Find(I)); end;
function TParametroInicializacaoList.Near(I : Integer) : TParametroInicializacao; begin Result := TParametroInicializacao(inherited Near(I)); end;

{ TParametroInicializacaoPorSessaoList }
class function TParametroInicializacaoPorSessaoList.GetObjectClass : TTransientClass; begin Result := TParametroInicializacao; end;
procedure TParametroInicializacaoPorSessaoList.Add(ParametroInicializacao : TParametroInicializacao); begin inherited Add(TPrevalent(ParametroInicializacao)); end;
procedure TParametroInicializacaoPorSessaoList.Delete(ParametroInicializacao : TParametroInicializacao); begin inherited Delete(TPrevalent(ParametroInicializacao)); end;
function TParametroInicializacaoPorSessaoList.First : TParametroInicializacao; begin Result := TParametroInicializacao(inherited First); end;
function TParametroInicializacaoPorSessaoList.Last : TParametroInicializacao; begin Result := TParametroInicializacao(inherited Last); end;
function TParametroInicializacaoPorSessaoList.Next(var ParametroInicializacao : TParametroInicializacao) : boolean; begin Result := inherited Next(TTransient(ParametroInicializacao)); end;
function TParametroInicializacaoPorSessaoList.Prior(var ParametroInicializacao : TParametroInicializacao) : boolean; begin Result := inherited Prior(TTransient(ParametroInicializacao)); end;
function TParametroInicializacaoPorSessaoList.Find(S : String) : TParametroInicializacao; begin Result := TParametroInicializacao(inherited Find(S)); end;
function TParametroInicializacaoPorSessaoList.Near(S : String) : TParametroInicializacao; begin Result := TParametroInicializacao(inherited Near(S)); end;
class function TParametroInicializacaoPorSessaoList.GetKeyCode : pointer; begin Result := @TParametroInicializacao.PorSessao; end;
class function TParametroInicializacaoPorSessaoList.GetListType : TListType; begin Result := ltString; end;

{ TServidorInicializacaoAssociation }
class function TServidorInicializacaoAssociation.GetObjectClass : TTransientClass; begin Result := TParametroInicializacao; end;
procedure TServidorInicializacaoAssociation.Add(ParametroInicializacao : TParametroInicializacao); begin inherited Add(TPrevalent(ParametroInicializacao)); end;
procedure TServidorInicializacaoAssociation.Delete(ParametroInicializacao : TParametroInicializacao); begin inherited Delete(TPrevalent(ParametroInicializacao)); end;
function TServidorInicializacaoAssociation.First : TParametroInicializacao; begin SetDependencyLists; Result := TParametroInicializacao(inherited First); end;
function TServidorInicializacaoAssociation.Last : TParametroInicializacao; begin SetDependencyLists; Result := TParametroInicializacao(inherited Last); end;
function TServidorInicializacaoAssociation.Next(var ParametroInicializacao : TParametroInicializacao) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(ParametroInicializacao)); end;
function TServidorInicializacaoAssociation.Prior(var ParametroInicializacao : TParametroInicializacao) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(ParametroInicializacao)); end;
function TServidorInicializacaoAssociation.Find(S : String) : TParametroInicializacao; begin Result := TParametroInicializacao(inherited Find(S)); end;
function TServidorInicializacaoAssociation.Near(S : String) : TParametroInicializacao; begin SetDependencyLists; Result := TParametroInicializacao(inherited Near(S)); end;
class function TServidorInicializacaoAssociation.GetKeyCode : pointer; begin Result := @TParametroInicializacao.PorSessao; end;
class function TServidorInicializacaoAssociation.GetListType : TListType; begin Result := ltString; end;

{ TServidor }

procedure TServidor.New; begin
  inherited;
  _Inicializacao := TServidorInicializacaoAssociation.Create(Self, 'Inicializacao');
end;

procedure TServidor.InternalFree; begin
  if _Inicializacao <> nil then _Inicializacao.InternalFree;
  inherited;
end;

function TServidor.GetRaizGeracao : String; begin
  Result := TServidor(Prevalence.GetNewImage(Self, 2))._RaizGeracao;
end;

procedure TServidor.SetRaizGeracao(Value : String); begin
  if Prevalence.IsInRecover then _RaizGeracao := Value else begin
    if (_RaizGeracao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TServidor(NewImage)._RaizGeracao <> Value then begin
        TServidor(NewImage)._RaizGeracao := Value;
        UpdateLog(2, NewImage, Stream)
      end;
  end;
end;

function TServidor.GetPortaInicial : Word; begin
  Result := TServidor(Prevalence.GetNewImage(Self, 3))._PortaInicial;
end;

procedure TServidor.SetPortaInicial(Value : Word); begin
  if Prevalence.IsInRecover then _PortaInicial := Value else begin
    if (_PortaInicial = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TServidor(NewImage)._PortaInicial <> Value then begin
        TServidor(NewImage)._PortaInicial := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function TServidor.GetPortaFinal : Word; begin
  Result := TServidor(Prevalence.GetNewImage(Self, 4))._PortaFinal;
end;

procedure TServidor.SetPortaFinal(Value : Word); begin
  if Prevalence.IsInRecover then _PortaFinal := Value else begin
    if (_PortaFinal = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TServidor(NewImage)._PortaFinal <> Value then begin
        TServidor(NewImage)._PortaFinal := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

function TServidor.GetVersao : TVersao; begin
  Result := TServidor(Prevalence.GetNewImage(Self, 6))._Versao;
  FixReference(TTransient(Result), TVersao);
end;

procedure TServidor.SetVersao(Value : TVersao); begin
  if Prevalence.IsInRecover then _Versao := Value else begin
    if (_Versao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TServidor(NewImage)._Versao <> Value then begin
        TServidor(NewImage)._Versao := Value;
        UpdateLog(6, NewImage, Stream)
      end;
  end;
end;

{ TServidorList }
class function TServidorList.GetObjectClass : TTransientClass; begin Result := TServidor; end;
procedure TServidorList.Add(Servidor : TServidor); begin inherited Add(TPrevalent(Servidor)); end;
procedure TServidorList.Delete(Servidor : TServidor); begin inherited Delete(TPrevalent(Servidor)); end;
function TServidorList.First : TServidor; begin Result := TServidor(inherited First); end;
function TServidorList.Last : TServidor; begin Result := TServidor(inherited Last); end;
function TServidorList.Next(var Servidor : TServidor) : boolean; begin Result := inherited Next(TTransient(Servidor)); end;
function TServidorList.Prior(var Servidor : TServidor) : boolean; begin Result := inherited Prior(TTransient(Servidor)); end;
function TServidorList.Find(I : Integer) : TServidor; begin Result := TServidor(inherited Find(I)); end;
function TServidorList.Near(I : Integer) : TServidor; begin Result := TServidor(inherited Near(I)); end;

{ TVersao }

function TVersao.GetNumero : String; begin
  Result := TVersao(Prevalence.GetNewImage(Self, 2))._Numero;
end;

procedure TVersao.SetNumero(Value : String); begin
  if Prevalence.IsInRecover then _Numero := Value else begin
    if (_Numero = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TVersao(NewImage)._Numero <> Value then begin
        TVersao(NewImage)._Numero := Value;
        UpdateLog(2, NewImage, Stream)
      end;
  end;
end;

function TVersao.GetPath : String; begin
  Result := TVersao(Prevalence.GetNewImage(Self, 3))._Path;
end;

procedure TVersao.SetPath(Value : String); begin
  if Prevalence.IsInRecover then _Path := Value else begin
    if (_Path = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TVersao(NewImage)._Path <> Value then begin
        TVersao(NewImage)._Path := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

{ TVersaoList }
class function TVersaoList.GetObjectClass : TTransientClass; begin Result := TVersao; end;
procedure TVersaoList.Add(Versao : TVersao); begin inherited Add(TPrevalent(Versao)); end;
procedure TVersaoList.Delete(Versao : TVersao); begin inherited Delete(TPrevalent(Versao)); end;
function TVersaoList.First : TVersao; begin Result := TVersao(inherited First); end;
function TVersaoList.Last : TVersao; begin Result := TVersao(inherited Last); end;
function TVersaoList.Next(var Versao : TVersao) : boolean; begin Result := inherited Next(TTransient(Versao)); end;
function TVersaoList.Prior(var Versao : TVersao) : boolean; begin Result := inherited Prior(TTransient(Versao)); end;
function TVersaoList.Find(I : Integer) : TVersao; begin Result := TVersao(inherited Find(I)); end;
function TVersaoList.Near(I : Integer) : TVersao; begin Result := TVersao(inherited Near(I)); end;

{ TElementoTipado }

procedure TElementoTipado.New; begin
  inherited;
  if Prevalence.IsInRecover then exit;
  try _Tamanho := 1 except end;
end;

function TElementoTipado.GetTipo : TElementoTipadoTipo; begin
  Result := TElementoTipado(Prevalence.GetNewImage(Self, 7))._Tipo;
end;

procedure TElementoTipado.SetTipo(Value : TElementoTipadoTipo); begin
  if Prevalence.IsInRecover then _Tipo := Value else begin
    if (_Tipo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TElementoTipado(NewImage)._Tipo <> Value then begin
        TElementoTipado(NewImage)._Tipo := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

function TElementoTipado.GetTamanho : Word; begin
  Result := TElementoTipado(Prevalence.GetNewImage(Self, 8))._Tamanho;
end;

procedure TElementoTipado.SetTamanho(Value : Word); begin
  if Prevalence.IsInRecover then _Tamanho := Value else begin
    if (_Tamanho = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TElementoTipado(NewImage)._Tamanho <> Value then begin
        TElementoTipado(NewImage)._Tamanho := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

function TElementoTipado.GetDecimais : Boolean; begin
  Result := TElementoTipado(Prevalence.GetNewImage(Self, 9))._Decimais;
end;

procedure TElementoTipado.SetDecimais(Value : Boolean); begin
  if Prevalence.IsInRecover then _Decimais := Value else begin
    if (_Decimais = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TElementoTipado(NewImage)._Decimais <> Value then begin
        TElementoTipado(NewImage)._Decimais := Value;
        UpdateLog(9, NewImage, Stream)
      end;
  end;
end;

function TElementoTipado.GetSinalizado : Boolean; begin
  Result := TElementoTipado(Prevalence.GetNewImage(Self, 10))._Sinalizado;
end;

procedure TElementoTipado.SetSinalizado(Value : Boolean); begin
  if Prevalence.IsInRecover then _Sinalizado := Value else begin
    if (_Sinalizado = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TElementoTipado(NewImage)._Sinalizado <> Value then begin
        TElementoTipado(NewImage)._Sinalizado := Value;
        UpdateLog(10, NewImage, Stream)
      end;
  end;
end;

function TElementoTipado.GetValores : String; begin
  Result := TElementoTipado(Prevalence.GetNewImage(Self, 11))._Valores;
end;

procedure TElementoTipado.SetValores(Value : String); begin
  if Prevalence.IsInRecover then _Valores := Value else begin
    if (_Valores = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TElementoTipado(NewImage)._Valores <> Value then begin
        TElementoTipado(NewImage)._Valores := Value;
        UpdateLog(11, NewImage, Stream)
      end;
  end;
end;

function TElementoTipado.GetMascara : String; begin
  Result := TElementoTipado(Prevalence.GetNewImage(Self, 12))._Mascara;
end;

procedure TElementoTipado.SetMascara(Value : String); begin
  if Prevalence.IsInRecover then _Mascara := Value else begin
    if (_Mascara = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TElementoTipado(NewImage)._Mascara <> Value then begin
        TElementoTipado(NewImage)._Mascara := Value;
        UpdateLog(12, NewImage, Stream)
      end;
  end;
end;

function TElementoTipado.GetTipoClasse : TClasse; begin
  Result := TElementoTipado(Prevalence.GetNewImage(Self, 13))._TipoClasse;
  FixReference(TTransient(Result), TClasse);
end;

procedure TElementoTipado.SetTipoClasse(Value : TClasse); begin
  if Prevalence.IsInRecover then _TipoClasse := Value else begin
    if (_TipoClasse = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TElementoTipado(NewImage)._TipoClasse <> Value then begin
        TElementoTipado(NewImage)._TipoClasse := Value;
        UpdateLog(13, NewImage, Stream)
      end;
  end;
end;
function TElementoTipado.GetIdentification : String; begin Result := ElementoTipado_GetIdentification(Self) end;

function TElementoTipado.VisibleTamanho : Boolean; begin
  Result := Tipo in [ettMonetario, ettNumero, ettTexto];
end;

function TElementoTipado.VisibleDecimais : Boolean; begin
  Result := Tipo in [ettMonetario, ettNumero];
end;

function TElementoTipado.VisibleSinalizado : Boolean; begin
  Result := Tipo in [ettMonetario, ettNumero];
end;

function TElementoTipado.VisibleValores : Boolean; begin
  Result := Tipo in [ettEnumeracao, ettConjunto, ettFaixa, ettLista, ettOutros];
end;

function TElementoTipado.VisibleMascara : Boolean; begin
  Result := Tipo in [ettMonetario, ettNumero, ettTexto];
end;

function TElementoTipado.CheckTipoClasse(var Message : String) : Boolean; begin
  Result := (tipo <> EttClasse) or (tipoClasse<>nil);
  if Result then
    Message := ''
  else
    Message := 'Escolha uma classe para definir o tipo do atributo'
end;

function TElementoTipado.VisibleTipoClasse : Boolean; begin
  Result := tipo = EttClasse;
end;

{ TElementoTipadoList }
class function TElementoTipadoList.GetObjectClass : TTransientClass; begin Result := TElementoTipado; end;
procedure TElementoTipadoList.Add(ElementoTipado : TElementoTipado); begin inherited Add(TPrevalent(ElementoTipado)); end;
procedure TElementoTipadoList.Delete(ElementoTipado : TElementoTipado); begin inherited Delete(TPrevalent(ElementoTipado)); end;
function TElementoTipadoList.First : TElementoTipado; begin Result := TElementoTipado(inherited First); end;
function TElementoTipadoList.Last : TElementoTipado; begin Result := TElementoTipado(inherited Last); end;
function TElementoTipadoList.Next(var ElementoTipado : TElementoTipado) : boolean; begin Result := inherited Next(TTransient(ElementoTipado)); end;
function TElementoTipadoList.Prior(var ElementoTipado : TElementoTipado) : boolean; begin Result := inherited Prior(TTransient(ElementoTipado)); end;
function TElementoTipadoList.Find(I : Integer) : TElementoTipado; begin Result := TElementoTipado(inherited Find(I)); end;
function TElementoTipadoList.Near(I : Integer) : TElementoTipado; begin Result := TElementoTipado(inherited Near(I)); end;

{ TElementoTipadoPorNomeList }
class function TElementoTipadoPorNomeList.GetObjectClass : TTransientClass; begin Result := TElementoTipado; end;
procedure TElementoTipadoPorNomeList.Add(ElementoTipado : TElementoTipado); begin inherited Add(TPrevalent(ElementoTipado)); end;
procedure TElementoTipadoPorNomeList.Delete(ElementoTipado : TElementoTipado); begin inherited Delete(TPrevalent(ElementoTipado)); end;
function TElementoTipadoPorNomeList.First : TElementoTipado; begin Result := TElementoTipado(inherited First); end;
function TElementoTipadoPorNomeList.Last : TElementoTipado; begin Result := TElementoTipado(inherited Last); end;
function TElementoTipadoPorNomeList.Next(var ElementoTipado : TElementoTipado) : boolean; begin Result := inherited Next(TTransient(ElementoTipado)); end;
function TElementoTipadoPorNomeList.Prior(var ElementoTipado : TElementoTipado) : boolean; begin Result := inherited Prior(TTransient(ElementoTipado)); end;
function TElementoTipadoPorNomeList.Find(S : String) : TElementoTipado; begin Result := TElementoTipado(inherited Find(S)); end;
function TElementoTipadoPorNomeList.Near(S : String) : TElementoTipado; begin Result := TElementoTipado(inherited Near(S)); end;
class function TElementoTipadoPorNomeList.GetKeyCode : pointer; begin Result := @TElementoTipado.PorNome; end;
class function TElementoTipadoPorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TEstadoBase }

procedure TEstadoBase.New; begin
  inherited;
  _Destinos := TEstadoBaseDestinosAssociation.Create(Self, 'Destinos');
  _Origens := TEstadoBaseOrigensAssociation.Create(Self, 'Origens');
end;

procedure TEstadoBase.InternalFree; begin
  if _Destinos <> nil then _Destinos.InternalFree;
  if _Origens <> nil then _Origens.InternalFree;
  inherited;
end;

function TEstadoBase.GetBotoes : String; begin
  Result := TEstadoBase(Prevalence.GetNewImage(Self, 7))._Botoes;
end;

procedure TEstadoBase.SetBotoes(Value : String); begin
  if Prevalence.IsInRecover then _Botoes := Value else begin
    if (_Botoes = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TEstadoBase(NewImage)._Botoes <> Value then begin
        TEstadoBase(NewImage)._Botoes := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

function TEstadoBase.GetPosicao : Integer; begin
  try Result := CalcularPosicao except Result := 0 end;
end;

function TEstadoBase.GetMetodo : TMetodo; begin
  Result := TEstadoBase(Prevalence.GetNewImage(Self, 10))._Metodo;
  FixReference(TTransient(Result), TMetodo);
end;

procedure TEstadoBase.SetMetodo(Value : TMetodo); begin
  if Prevalence.IsInRecover then _Metodo := Value else begin
    if (_Metodo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TEstadoBase(NewImage)._Metodo <> Value then begin
        TEstadoBase(NewImage)._Metodo := Value;
        UpdateLog(10, NewImage, Stream)
      end;
  end;
end;

function TEstadoBase.PorPosicao : Integer; begin
  Result := Posicao
end;
function TEstadoBase.CalcularPosicao : Integer; begin Result := EstadoBase_CalcularPosicao(Self) end;
function TEstadoBase.ChecarNome : Boolean; begin Result := EstadoBase_ChecarNome(Self) end;
function TEstadoBase.GetIdentification : String; begin Result := EstadoBase_GetIdentification(Self) end;

class function TEstadoBaseList.GetObjectClass : TTransientClass; begin
  Result := TEstadoBase
end;

{ TMetodoEstadosAssociation }
class function TMetodoEstadosAssociation.GetObjectClass : TTransientClass; begin Result := TEstadoBase; end;
procedure TMetodoEstadosAssociation.Add(EstadoBase : TEstadoBase); begin inherited Add(TPrevalent(EstadoBase)); end;
procedure TMetodoEstadosAssociation.Delete(EstadoBase : TEstadoBase); begin inherited Delete(TPrevalent(EstadoBase)); end;
function TMetodoEstadosAssociation.First : TEstadoBase; begin SetDependencyLists; Result := TEstadoBase(inherited First); end;
function TMetodoEstadosAssociation.Last : TEstadoBase; begin SetDependencyLists; Result := TEstadoBase(inherited Last); end;
function TMetodoEstadosAssociation.Next(var EstadoBase : TEstadoBase) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(EstadoBase)); end;
function TMetodoEstadosAssociation.Prior(var EstadoBase : TEstadoBase) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(EstadoBase)); end;
function TMetodoEstadosAssociation.Find(I : Integer) : TEstadoBase; begin Result := TEstadoBase(inherited Find(I)); end;
function TMetodoEstadosAssociation.Near(I : Integer) : TEstadoBase; begin SetDependencyLists; Result := TEstadoBase(inherited Near(I)); end;
class function TMetodoEstadosAssociation.GetKeyCode : pointer; begin Result := @TEstadoBase.PorPosicao; end;

{ TIndice }

function TIndice.GetExpressao : String; begin
  Result := TIndice(Prevalence.GetNewImage(Self, 7))._Expressao;
end;

procedure TIndice.SetExpressao(Value : String); begin
  if Prevalence.IsInRecover then _Expressao := Value else begin
    if (_Expressao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TIndice(NewImage)._Expressao <> Value then begin
        TIndice(NewImage)._Expressao := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

function TIndice.GetUnico : Boolean; begin
  Result := TIndice(Prevalence.GetNewImage(Self, 8))._Unico;
end;

procedure TIndice.SetUnico(Value : Boolean); begin
  if Prevalence.IsInRecover then _Unico := Value else begin
    if (_Unico = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TIndice(NewImage)._Unico <> Value then begin
        TIndice(NewImage)._Unico := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

function TIndice.GetTipo : TIndiceTipo; begin
  Result := TIndice(Prevalence.GetNewImage(Self, 9))._Tipo;
end;

procedure TIndice.SetTipo(Value : TIndiceTipo); begin
  if Prevalence.IsInRecover then _Tipo := Value else begin
    if (_Tipo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TIndice(NewImage)._Tipo <> Value then begin
        TIndice(NewImage)._Tipo := Value;
        UpdateLog(9, NewImage, Stream)
      end;
  end;
end;

function TIndice.GetRetorno : TElementoTipado; begin
  Result := TIndice(Prevalence.GetNewImage(Self, 10))._Retorno;
  FixReference(TTransient(Result), TElementoTipado);
end;

procedure TIndice.SetRetorno(Value : TElementoTipado); begin
  if Prevalence.IsInRecover then _Retorno := Value else begin
    if (_Retorno = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TIndice(NewImage)._Retorno <> Value then begin
        TIndice(NewImage)._Retorno := Value;
        UpdateLog(10, NewImage, Stream)
      end;
  end;
end;

function TIndice.GetClasse : TClasse; begin
  Result := TIndice(Prevalence.GetNewImage(Self, 11))._Classe;
  FixReference(TTransient(Result), TClasse);
end;

procedure TIndice.SetClasse(Value : TClasse); begin
  if Prevalence.IsInRecover then _Classe := Value else begin
    if (_Classe = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TIndice(NewImage)._Classe <> Value then begin
        TIndice(NewImage)._Classe := Value;
        UpdateLog(11, NewImage, Stream)
      end;
  end;
end;

{ TIndiceList }
class function TIndiceList.GetObjectClass : TTransientClass; begin Result := TIndice; end;
procedure TIndiceList.Add(Indice : TIndice); begin inherited Add(TPrevalent(Indice)); end;
procedure TIndiceList.Delete(Indice : TIndice); begin inherited Delete(TPrevalent(Indice)); end;
function TIndiceList.First : TIndice; begin Result := TIndice(inherited First); end;
function TIndiceList.Last : TIndice; begin Result := TIndice(inherited Last); end;
function TIndiceList.Next(var Indice : TIndice) : boolean; begin Result := inherited Next(TTransient(Indice)); end;
function TIndiceList.Prior(var Indice : TIndice) : boolean; begin Result := inherited Prior(TTransient(Indice)); end;
function TIndiceList.Find(I : Integer) : TIndice; begin Result := TIndice(inherited Find(I)); end;
function TIndiceList.Near(I : Integer) : TIndice; begin Result := TIndice(inherited Near(I)); end;

{ TIndicePorNomeList }
class function TIndicePorNomeList.GetObjectClass : TTransientClass; begin Result := TIndice; end;
procedure TIndicePorNomeList.Add(Indice : TIndice); begin inherited Add(TPrevalent(Indice)); end;
procedure TIndicePorNomeList.Delete(Indice : TIndice); begin inherited Delete(TPrevalent(Indice)); end;
function TIndicePorNomeList.First : TIndice; begin Result := TIndice(inherited First); end;
function TIndicePorNomeList.Last : TIndice; begin Result := TIndice(inherited Last); end;
function TIndicePorNomeList.Next(var Indice : TIndice) : boolean; begin Result := inherited Next(TTransient(Indice)); end;
function TIndicePorNomeList.Prior(var Indice : TIndice) : boolean; begin Result := inherited Prior(TTransient(Indice)); end;
function TIndicePorNomeList.Find(S : String) : TIndice; begin Result := TIndice(inherited Find(S)); end;
function TIndicePorNomeList.Near(S : String) : TIndice; begin Result := TIndice(inherited Near(S)); end;
class function TIndicePorNomeList.GetKeyCode : pointer; begin Result := @TIndice.PorNome; end;
class function TIndicePorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TClasseIndicesAssociation }
class function TClasseIndicesAssociation.GetObjectClass : TTransientClass; begin Result := TIndice; end;
procedure TClasseIndicesAssociation.Add(Indice : TIndice); begin inherited Add(TPrevalent(Indice)); end;
procedure TClasseIndicesAssociation.Delete(Indice : TIndice); begin inherited Delete(TPrevalent(Indice)); end;
function TClasseIndicesAssociation.First : TIndice; begin SetDependencyLists; Result := TIndice(inherited First); end;
function TClasseIndicesAssociation.Last : TIndice; begin SetDependencyLists; Result := TIndice(inherited Last); end;
function TClasseIndicesAssociation.Next(var Indice : TIndice) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Indice)); end;
function TClasseIndicesAssociation.Prior(var Indice : TIndice) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Indice)); end;
function TClasseIndicesAssociation.Find(S : String) : TIndice; begin Result := TIndice(inherited Find(S)); end;
function TClasseIndicesAssociation.Near(S : String) : TIndice; begin SetDependencyLists; Result := TIndice(inherited Near(S)); end;
class function TClasseIndicesAssociation.GetKeyCode : pointer; begin Result := @TIndice.PorNome; end;
class function TClasseIndicesAssociation.GetListType : TListType; begin Result := ltString; end;

{ TLink }

procedure TLink.New; begin
  inherited;
  _AssociacoesDestino := TLinkAssociacoesDestinoAssociation.Create(Self, 'AssociacoesDestino');
  _AssociacoesOrigem := TLinkAssociacoesOrigemAssociation.Create(Self, 'AssociacoesOrigem');
  _Herancas := TLinkHerancasAssociation.Create(Self, 'Herancas');
end;

procedure TLink.InternalFree; begin
  if _AssociacoesDestino <> nil then _AssociacoesDestino.InternalFree;
  if _AssociacoesOrigem <> nil then _AssociacoesOrigem.InternalFree;
  if _Herancas <> nil then _Herancas.InternalFree;
  inherited;
end;

function TLink.GetIdentification : string; begin
  try Result := Classe.Pacote.Nome + '.' + Classe.GetIdentification except Result := ' ' end;
end;

function TLink.GetHerancaNome : String; begin
  try Result := ContarHeranca except Result := ' ' end;
end;

function TLink.GetPacote : TPacote; begin
  Result := TLink(Prevalence.GetNewImage(Self, 8))._Pacote;
  FixReference(TTransient(Result), TPacote);
end;

procedure TLink.SetPacote(Value : TPacote); begin
  if Prevalence.IsInRecover then _Pacote := Value else begin
    if (_Pacote = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TLink(NewImage)._Pacote <> Value then begin
        TLink(NewImage)._Pacote := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

function TLink.GetPai : THeranca; begin
  Result := TLink(Prevalence.GetNewImage(Self, 11))._Pai;
  FixReference(TTransient(Result), THeranca);
end;

procedure TLink.SetPai(Value : THeranca); begin
  if Prevalence.IsInRecover then _Pai := Value else begin
    if (_Pai = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TLink(NewImage)._Pai <> Value then begin
        TLink(NewImage)._Pai := Value;
        UpdateLog(11, NewImage, Stream)
      end;
  end;
end;

function TLink.GetClasse : TClasse; begin
  Result := TLink(Prevalence.GetNewImage(Self, 13))._Classe;
  FixReference(TTransient(Result), TClasse);
end;

procedure TLink.SetClasse(Value : TClasse); begin
  if Prevalence.IsInRecover then _Classe := Value else begin
    if (_Classe = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TLink(NewImage)._Classe <> Value then begin
        TLink(NewImage)._Classe := Value;
        UpdateLog(13, NewImage, Stream)
      end;
  end;
end;

function TLink.PorHerancaNome : String; begin
  Result := HerancaNome
end;
function TLink.ContarHeranca : String; begin Result := Link_ContarHeranca(Self) end;
function TLink.ContarHeranca_Int(const Params : TMethodParams) : variant; begin Result := Link_ContarHeranca(Self); end;

{ TLinkList }
class function TLinkList.GetObjectClass : TTransientClass; begin Result := TLink; end;
procedure TLinkList.Add(Link : TLink); begin inherited Add(TPrevalent(Link)); end;
procedure TLinkList.Delete(Link : TLink); begin inherited Delete(TPrevalent(Link)); end;
function TLinkList.First : TLink; begin Result := TLink(inherited First); end;
function TLinkList.Last : TLink; begin Result := TLink(inherited Last); end;
function TLinkList.Next(var Link : TLink) : boolean; begin Result := inherited Next(TTransient(Link)); end;
function TLinkList.Prior(var Link : TLink) : boolean; begin Result := inherited Prior(TTransient(Link)); end;
function TLinkList.Find(I : Integer) : TLink; begin Result := TLink(inherited Find(I)); end;
function TLinkList.Near(I : Integer) : TLink; begin Result := TLink(inherited Near(I)); end;

{ TLinkPorHerancaNomeList }
class function TLinkPorHerancaNomeList.GetObjectClass : TTransientClass; begin Result := TLink; end;
procedure TLinkPorHerancaNomeList.Add(Link : TLink); begin inherited Add(TPrevalent(Link)); end;
procedure TLinkPorHerancaNomeList.Delete(Link : TLink); begin inherited Delete(TPrevalent(Link)); end;
function TLinkPorHerancaNomeList.First : TLink; begin Result := TLink(inherited First); end;
function TLinkPorHerancaNomeList.Last : TLink; begin Result := TLink(inherited Last); end;
function TLinkPorHerancaNomeList.Next(var Link : TLink) : boolean; begin Result := inherited Next(TTransient(Link)); end;
function TLinkPorHerancaNomeList.Prior(var Link : TLink) : boolean; begin Result := inherited Prior(TTransient(Link)); end;
function TLinkPorHerancaNomeList.Find(S : String) : TLink; begin Result := TLink(inherited Find(S)); end;
function TLinkPorHerancaNomeList.Near(S : String) : TLink; begin Result := TLink(inherited Near(S)); end;
class function TLinkPorHerancaNomeList.GetKeyCode : pointer; begin Result := @TLink.PorHerancaNome; end;
class function TLinkPorHerancaNomeList.GetListType : TListType; begin Result := ltString; end;

{ TLinkPorNomeList }
class function TLinkPorNomeList.GetObjectClass : TTransientClass; begin Result := TLink; end;
procedure TLinkPorNomeList.Add(Link : TLink); begin inherited Add(TPrevalent(Link)); end;
procedure TLinkPorNomeList.Delete(Link : TLink); begin inherited Delete(TPrevalent(Link)); end;
function TLinkPorNomeList.First : TLink; begin Result := TLink(inherited First); end;
function TLinkPorNomeList.Last : TLink; begin Result := TLink(inherited Last); end;
function TLinkPorNomeList.Next(var Link : TLink) : boolean; begin Result := inherited Next(TTransient(Link)); end;
function TLinkPorNomeList.Prior(var Link : TLink) : boolean; begin Result := inherited Prior(TTransient(Link)); end;
function TLinkPorNomeList.Find(S : String) : TLink; begin Result := TLink(inherited Find(S)); end;
function TLinkPorNomeList.Near(S : String) : TLink; begin Result := TLink(inherited Near(S)); end;
class function TLinkPorNomeList.GetKeyCode : pointer; begin Result := @TLink.PorNome; end;
class function TLinkPorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TPacoteClassesAssociation }
class function TPacoteClassesAssociation.GetObjectClass : TTransientClass; begin Result := TLink; end;
procedure TPacoteClassesAssociation.Add(Link : TLink); begin inherited Add(TPrevalent(Link)); end;
procedure TPacoteClassesAssociation.Delete(Link : TLink); begin inherited Delete(TPrevalent(Link)); end;
function TPacoteClassesAssociation.First : TLink; begin SetDependencyLists; Result := TLink(inherited First); end;
function TPacoteClassesAssociation.Last : TLink; begin SetDependencyLists; Result := TLink(inherited Last); end;
function TPacoteClassesAssociation.Next(var Link : TLink) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Link)); end;
function TPacoteClassesAssociation.Prior(var Link : TLink) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Link)); end;
function TPacoteClassesAssociation.Find(S : String) : TLink; begin Result := TLink(inherited Find(S)); end;
function TPacoteClassesAssociation.Near(S : String) : TLink; begin SetDependencyLists; Result := TLink(inherited Near(S)); end;
class function TPacoteClassesAssociation.GetKeyCode : pointer; begin Result := @TLink.PorNome; end;
class function TPacoteClassesAssociation.GetListType : TListType; begin Result := ltString; end;

{ TClasseLinksAssociation }
class function TClasseLinksAssociation.GetObjectClass : TTransientClass; begin Result := TLink; end;
procedure TClasseLinksAssociation.Add(Link : TLink); begin inherited Add(TPrevalent(Link)); end;
procedure TClasseLinksAssociation.Delete(Link : TLink); begin inherited Delete(TPrevalent(Link)); end;
function TClasseLinksAssociation.First : TLink; begin SetDependencyLists; Result := TLink(inherited First); end;
function TClasseLinksAssociation.Last : TLink; begin SetDependencyLists; Result := TLink(inherited Last); end;
function TClasseLinksAssociation.Next(var Link : TLink) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Link)); end;
function TClasseLinksAssociation.Prior(var Link : TLink) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Link)); end;
function TClasseLinksAssociation.Find(I : Integer) : TLink; begin Result := TLink(inherited Find(I)); end;
function TClasseLinksAssociation.Near(I : Integer) : TLink; begin SetDependencyLists; Result := TLink(inherited Near(I)); end;

{ TMetodo }

procedure TMetodo.New; begin
  inherited;
  _Estados := TMetodoEstadosAssociation.Create(Self, 'Estados');
  _Variaveis := TMetodoVariaveisAssociation.Create(Self, 'Variaveis');
  _Comentarios := TMetodoComentariosAssociation.Create(Self, 'Comentarios');
  _Parametros := TMetodoParametrosAssociation.Create(Self, 'Parametros');
  if Prevalence.IsInRecover then exit;
  try _Escopo := aePublished except end;
end;

function TMetodo.GetEscopo : TAtributoEscopo; begin
  Result := TMetodo(Prevalence.GetNewImage(Self, 7))._Escopo;
end;

procedure TMetodo.SetEscopo(Value : TAtributoEscopo); begin
  if Prevalence.IsInRecover then _Escopo := Value else begin
    if (_Escopo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TMetodo(NewImage)._Escopo <> Value then begin
        TMetodo(NewImage)._Escopo := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

function TMetodo.GetTipo : TMetodoTipo; begin
  Result := TMetodo(Prevalence.GetNewImage(Self, 8))._Tipo;
end;

procedure TMetodo.SetTipo(Value : TMetodoTipo); begin
  if Prevalence.IsInRecover then _Tipo := Value else begin
    if (_Tipo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TMetodo(NewImage)._Tipo <> Value then begin
        TMetodo(NewImage)._Tipo := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

function TMetodo.GetAcao : String; begin
  Result := TMetodo(Prevalence.GetNewImage(Self, 9))._Acao;
end;

procedure TMetodo.SetAcao(Value : String); begin
  if Prevalence.IsInRecover then _Acao := Value else begin
    if (_Acao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TMetodo(NewImage)._Acao <> Value then begin
        TMetodo(NewImage)._Acao := Value;
        UpdateLog(9, NewImage, Stream)
      end;
  end;
end;

function TMetodo.GetCaracteristicas : TSetMetodoCaracteristicas; begin
  Result := TMetodo(Prevalence.GetNewImage(Self, 10))._Caracteristicas;
end;

procedure TMetodo.SetCaracteristicas(Value : TSetMetodoCaracteristicas); begin
  if Prevalence.IsInRecover then _Caracteristicas := Value else begin
    if (_Caracteristicas = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TMetodo(NewImage)._Caracteristicas <> Value then begin
        TMetodo(NewImage)._Caracteristicas := Value;
        UpdateLog(10, NewImage, Stream)
      end;
  end;
end;

function TMetodo.GetTitulo : String; begin
  try Result := Classe.Nome + '.' + FormatarCaracteristicas + Nome + FormatarParametros + FormatarRetorno except Result := ' ' end;
end;

function TMetodo.GetRetorno : TElementoTipado; begin
  Result := TMetodo(Prevalence.GetNewImage(Self, 16))._Retorno;
  FixReference(TTransient(Result), TElementoTipado);
end;

procedure TMetodo.SetRetorno(Value : TElementoTipado); begin
  if Prevalence.IsInRecover then _Retorno := Value else begin
    if (_Retorno = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TMetodo(NewImage)._Retorno <> Value then begin
        TMetodo(NewImage)._Retorno := Value;
        UpdateLog(16, NewImage, Stream)
      end;
  end;
end;

function TMetodo.GetClasse : TClasse; begin
  Result := TMetodo(Prevalence.GetNewImage(Self, 17))._Classe;
  FixReference(TTransient(Result), TClasse);
end;

procedure TMetodo.SetClasse(Value : TClasse); begin
  if Prevalence.IsInRecover then _Classe := Value else begin
    if (_Classe = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TMetodo(NewImage)._Classe <> Value then begin
        TMetodo(NewImage)._Classe := Value;
        UpdateLog(17, NewImage, Stream)
      end;
  end;
end;
procedure TMetodo.EditarDiagrama; begin Metodo_EditarDiagrama(Self) end;
procedure TMetodo.EditarDiagrama_Int(const Params : TMethodParams); begin Metodo_EditarDiagrama(Self); end;
function TMetodo.FormatarCaracteristicas : String; begin Result := Metodo_FormatarCaracteristicas(Self) end;
function TMetodo.FormatarEscopo : String; begin Result := Metodo_FormatarEscopo(Self) end;
function TMetodo.FormatarParametros : String; begin Result := Metodo_FormatarParametros(Self) end;
function TMetodo.FormatarRetorno : String; begin Result := Metodo_FormatarRetorno(Self) end;
function TMetodo.GetIdentification : String; begin Result := Metodo_GetIdentification(Self) end;

{ TMetodoList }
class function TMetodoList.GetObjectClass : TTransientClass; begin Result := TMetodo; end;
procedure TMetodoList.Add(Metodo : TMetodo); begin inherited Add(TPrevalent(Metodo)); end;
procedure TMetodoList.Delete(Metodo : TMetodo); begin inherited Delete(TPrevalent(Metodo)); end;
function TMetodoList.First : TMetodo; begin Result := TMetodo(inherited First); end;
function TMetodoList.Last : TMetodo; begin Result := TMetodo(inherited Last); end;
function TMetodoList.Next(var Metodo : TMetodo) : boolean; begin Result := inherited Next(TTransient(Metodo)); end;
function TMetodoList.Prior(var Metodo : TMetodo) : boolean; begin Result := inherited Prior(TTransient(Metodo)); end;
function TMetodoList.Find(I : Integer) : TMetodo; begin Result := TMetodo(inherited Find(I)); end;
function TMetodoList.Near(I : Integer) : TMetodo; begin Result := TMetodo(inherited Near(I)); end;

{ TMetodoPorNomeList }
class function TMetodoPorNomeList.GetObjectClass : TTransientClass; begin Result := TMetodo; end;
procedure TMetodoPorNomeList.Add(Metodo : TMetodo); begin inherited Add(TPrevalent(Metodo)); end;
procedure TMetodoPorNomeList.Delete(Metodo : TMetodo); begin inherited Delete(TPrevalent(Metodo)); end;
function TMetodoPorNomeList.First : TMetodo; begin Result := TMetodo(inherited First); end;
function TMetodoPorNomeList.Last : TMetodo; begin Result := TMetodo(inherited Last); end;
function TMetodoPorNomeList.Next(var Metodo : TMetodo) : boolean; begin Result := inherited Next(TTransient(Metodo)); end;
function TMetodoPorNomeList.Prior(var Metodo : TMetodo) : boolean; begin Result := inherited Prior(TTransient(Metodo)); end;
function TMetodoPorNomeList.Find(S : String) : TMetodo; begin Result := TMetodo(inherited Find(S)); end;
function TMetodoPorNomeList.Near(S : String) : TMetodo; begin Result := TMetodo(inherited Near(S)); end;
class function TMetodoPorNomeList.GetKeyCode : pointer; begin Result := @TMetodo.PorNome; end;
class function TMetodoPorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TClasseMetodosAssociation }
class function TClasseMetodosAssociation.GetObjectClass : TTransientClass; begin Result := TMetodo; end;
procedure TClasseMetodosAssociation.Add(Metodo : TMetodo); begin inherited Add(TPrevalent(Metodo)); end;
procedure TClasseMetodosAssociation.Delete(Metodo : TMetodo); begin inherited Delete(TPrevalent(Metodo)); end;
function TClasseMetodosAssociation.First : TMetodo; begin SetDependencyLists; Result := TMetodo(inherited First); end;
function TClasseMetodosAssociation.Last : TMetodo; begin SetDependencyLists; Result := TMetodo(inherited Last); end;
function TClasseMetodosAssociation.Next(var Metodo : TMetodo) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Metodo)); end;
function TClasseMetodosAssociation.Prior(var Metodo : TMetodo) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Metodo)); end;
function TClasseMetodosAssociation.Find(S : String) : TMetodo; begin Result := TMetodo(inherited Find(S)); end;
function TClasseMetodosAssociation.Near(S : String) : TMetodo; begin SetDependencyLists; Result := TMetodo(inherited Near(S)); end;
class function TClasseMetodosAssociation.GetKeyCode : pointer; begin Result := @TMetodo.PorNome; end;
class function TClasseMetodosAssociation.GetListType : TListType; begin Result := ltString; end;

{ TModelo }

procedure TModelo.New; begin
  inherited;
  _Pacotes := TModeloPacotesAssociation.Create(Self, 'Pacotes');
  _Geracoes := TModeloGeracoesAssociation.Create(Self, 'Geracoes');
end;

procedure TModelo.InternalFree; begin
  if _Pacotes <> nil then _Pacotes.InternalFree;
  if _Geracoes <> nil then _Geracoes.InternalFree;
  inherited;
end;
class procedure TModelo.DispararServer; begin Modelo_DispararServer end;
class procedure TModelo.DispararServer_Int(const Params : TMethodParams); begin Modelo_DispararServer; end;
class procedure TModelo.GerarServer; begin Modelo_GerarServer end;
class procedure TModelo.GerarServer_Int(const Params : TMethodParams); begin Modelo_GerarServer; end;
class procedure TModelo.ImportarModelo(Modelo : String = 'C:\projects\XDADesigner\XDANew.EAP'); begin Modelo_ImportarModelo(Modelo) end;
class procedure TModelo.ImportarModelo_Int(const Params : TMethodParams); begin Modelo_ImportarModelo(Params[0]); end;

{ TModeloList }
class function TModeloList.GetObjectClass : TTransientClass; begin Result := TModelo; end;
procedure TModeloList.Add(Modelo : TModelo); begin inherited Add(TPrevalent(Modelo)); end;
procedure TModeloList.Delete(Modelo : TModelo); begin inherited Delete(TPrevalent(Modelo)); end;
function TModeloList.First : TModelo; begin Result := TModelo(inherited First); end;
function TModeloList.Last : TModelo; begin Result := TModelo(inherited Last); end;
function TModeloList.Next(var Modelo : TModelo) : boolean; begin Result := inherited Next(TTransient(Modelo)); end;
function TModeloList.Prior(var Modelo : TModelo) : boolean; begin Result := inherited Prior(TTransient(Modelo)); end;
function TModeloList.Find(I : Integer) : TModelo; begin Result := TModelo(inherited Find(I)); end;
function TModeloList.Near(I : Integer) : TModelo; begin Result := TModelo(inherited Near(I)); end;

{ TModeloPorNomeList }
class function TModeloPorNomeList.GetObjectClass : TTransientClass; begin Result := TModelo; end;
procedure TModeloPorNomeList.Add(Modelo : TModelo); begin inherited Add(TPrevalent(Modelo)); end;
procedure TModeloPorNomeList.Delete(Modelo : TModelo); begin inherited Delete(TPrevalent(Modelo)); end;
function TModeloPorNomeList.First : TModelo; begin Result := TModelo(inherited First); end;
function TModeloPorNomeList.Last : TModelo; begin Result := TModelo(inherited Last); end;
function TModeloPorNomeList.Next(var Modelo : TModelo) : boolean; begin Result := inherited Next(TTransient(Modelo)); end;
function TModeloPorNomeList.Prior(var Modelo : TModelo) : boolean; begin Result := inherited Prior(TTransient(Modelo)); end;
function TModeloPorNomeList.Find(S : String) : TModelo; begin Result := TModelo(inherited Find(S)); end;
function TModeloPorNomeList.Near(S : String) : TModelo; begin Result := TModelo(inherited Near(S)); end;
class function TModeloPorNomeList.GetKeyCode : pointer; begin Result := @TModelo.PorNome; end;
class function TModeloPorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TPacoteModeloAssociation }
class function TPacoteModeloAssociation.GetObjectClass : TTransientClass; begin Result := TModelo; end;
procedure TPacoteModeloAssociation.Add(Modelo : TModelo); begin inherited Add(TPrevalent(Modelo)); end;
procedure TPacoteModeloAssociation.Delete(Modelo : TModelo); begin inherited Delete(TPrevalent(Modelo)); end;
function TPacoteModeloAssociation.First : TModelo; begin SetDependencyLists; Result := TModelo(inherited First); end;
function TPacoteModeloAssociation.Last : TModelo; begin SetDependencyLists; Result := TModelo(inherited Last); end;
function TPacoteModeloAssociation.Next(var Modelo : TModelo) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Modelo)); end;
function TPacoteModeloAssociation.Prior(var Modelo : TModelo) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Modelo)); end;
function TPacoteModeloAssociation.Find(I : Integer) : TModelo; begin Result := TModelo(inherited Find(I)); end;
function TPacoteModeloAssociation.Near(I : Integer) : TModelo; begin SetDependencyLists; Result := TModelo(inherited Near(I)); end;

{ TPacote }

procedure TPacote.New; begin
  inherited;
  _Classes := TPacoteClassesAssociation.Create(Self, 'Classes');
  _Comentarios := TPacoteComentariosAssociation.Create(Self, 'Comentarios');
  _Modelo := TPacoteModeloAssociation.Create(Self, 'Modelo');
end;

procedure TPacote.InternalFree; begin
  if _Classes <> nil then _Classes.InternalFree;
  if _Comentarios <> nil then _Comentarios.InternalFree;
  if _Modelo <> nil then _Modelo.InternalFree;
  inherited;
end;

function TPacote.GetAmbiente : TPacoteAmbiente; begin
  Result := TPacote(Prevalence.GetNewImage(Self, 7))._Ambiente;
end;

procedure TPacote.SetAmbiente(Value : TPacoteAmbiente); begin
  if Prevalence.IsInRecover then _Ambiente := Value else begin
    if (_Ambiente = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPacote(NewImage)._Ambiente <> Value then begin
        TPacote(NewImage)._Ambiente := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

function TPacote.GetVersao : String; begin
  Result := TPacote(Prevalence.GetNewImage(Self, 8))._Versao;
end;

procedure TPacote.SetVersao(Value : String); begin
  if Prevalence.IsInRecover then _Versao := Value else begin
    if (_Versao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPacote(NewImage)._Versao <> Value then begin
        TPacote(NewImage)._Versao := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

function TPacote.GetUnidadesExternas : String; begin
  Result := TPacote(Prevalence.GetNewImage(Self, 9))._UnidadesExternas;
end;

procedure TPacote.SetUnidadesExternas(Value : String); begin
  if Prevalence.IsInRecover then _UnidadesExternas := Value else begin
    if (_UnidadesExternas = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPacote(NewImage)._UnidadesExternas <> Value then begin
        TPacote(NewImage)._UnidadesExternas := Value;
        UpdateLog(9, NewImage, Stream)
      end;
  end;
end;

function TPacote.GetDeclaracao : String; begin
  Result := TPacote(Prevalence.GetNewImage(Self, 10))._Declaracao;
end;

procedure TPacote.SetDeclaracao(Value : String); begin
  if Prevalence.IsInRecover then _Declaracao := Value else begin
    if (_Declaracao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPacote(NewImage)._Declaracao <> Value then begin
        TPacote(NewImage)._Declaracao := Value;
        UpdateLog(10, NewImage, Stream)
      end;
  end;
end;

function TPacote.GetInicializacao : String; begin
  Result := TPacote(Prevalence.GetNewImage(Self, 11))._Inicializacao;
end;

procedure TPacote.SetInicializacao(Value : String); begin
  if Prevalence.IsInRecover then _Inicializacao := Value else begin
    if (_Inicializacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPacote(NewImage)._Inicializacao <> Value then begin
        TPacote(NewImage)._Inicializacao := Value;
        UpdateLog(11, NewImage, Stream)
      end;
  end;
end;

function TPacote.GetFinalizacao : String; begin
  Result := TPacote(Prevalence.GetNewImage(Self, 12))._Finalizacao;
end;

procedure TPacote.SetFinalizacao(Value : String); begin
  if Prevalence.IsInRecover then _Finalizacao := Value else begin
    if (_Finalizacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPacote(NewImage)._Finalizacao <> Value then begin
        TPacote(NewImage)._Finalizacao := Value;
        UpdateLog(12, NewImage, Stream)
      end;
  end;
end;

function TPacote.GetTitulo : String; begin
  try Result := Nome except Result := ' ' end;
end;
procedure TPacote.CopiarPacote; begin Pacote_CopiarPacote(Self) end;
procedure TPacote.CopiarPacote_Int(const Params : TMethodParams); begin Pacote_CopiarPacote(Self); end;
procedure TPacote.EditarDiagrama; begin Pacote_EditarDiagrama(Self) end;
procedure TPacote.EditarDiagrama_Int(const Params : TMethodParams); begin Pacote_EditarDiagrama(Self); end;
class procedure TPacote.SelecionarObjeto(pObjectPath : String); begin Pacote_SelecionarObjeto(Self, pObjectPath) end;
class procedure TPacote.SelecionarObjeto_Int(const Params : TMethodParams); begin Pacote_SelecionarObjeto(Self, Params[0]); end;

{ TPacoteList }
class function TPacoteList.GetObjectClass : TTransientClass; begin Result := TPacote; end;
procedure TPacoteList.Add(Pacote : TPacote); begin inherited Add(TPrevalent(Pacote)); end;
procedure TPacoteList.Delete(Pacote : TPacote); begin inherited Delete(TPrevalent(Pacote)); end;
function TPacoteList.First : TPacote; begin Result := TPacote(inherited First); end;
function TPacoteList.Last : TPacote; begin Result := TPacote(inherited Last); end;
function TPacoteList.Next(var Pacote : TPacote) : boolean; begin Result := inherited Next(TTransient(Pacote)); end;
function TPacoteList.Prior(var Pacote : TPacote) : boolean; begin Result := inherited Prior(TTransient(Pacote)); end;
function TPacoteList.Find(I : Integer) : TPacote; begin Result := TPacote(inherited Find(I)); end;
function TPacoteList.Near(I : Integer) : TPacote; begin Result := TPacote(inherited Near(I)); end;

{ TPacotePorNomeList }
class function TPacotePorNomeList.GetObjectClass : TTransientClass; begin Result := TPacote; end;
procedure TPacotePorNomeList.Add(Pacote : TPacote); begin inherited Add(TPrevalent(Pacote)); end;
procedure TPacotePorNomeList.Delete(Pacote : TPacote); begin inherited Delete(TPrevalent(Pacote)); end;
function TPacotePorNomeList.First : TPacote; begin Result := TPacote(inherited First); end;
function TPacotePorNomeList.Last : TPacote; begin Result := TPacote(inherited Last); end;
function TPacotePorNomeList.Next(var Pacote : TPacote) : boolean; begin Result := inherited Next(TTransient(Pacote)); end;
function TPacotePorNomeList.Prior(var Pacote : TPacote) : boolean; begin Result := inherited Prior(TTransient(Pacote)); end;
function TPacotePorNomeList.Find(S : String) : TPacote; begin Result := TPacote(inherited Find(S)); end;
function TPacotePorNomeList.Near(S : String) : TPacote; begin Result := TPacote(inherited Near(S)); end;
class function TPacotePorNomeList.GetKeyCode : pointer; begin Result := @TPacote.PorNome; end;
class function TPacotePorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TModeloPacotesAssociation }
class function TModeloPacotesAssociation.GetObjectClass : TTransientClass; begin Result := TPacote; end;
procedure TModeloPacotesAssociation.Add(Pacote : TPacote); begin inherited Add(TPrevalent(Pacote)); end;
procedure TModeloPacotesAssociation.Delete(Pacote : TPacote); begin inherited Delete(TPrevalent(Pacote)); end;
function TModeloPacotesAssociation.First : TPacote; begin SetDependencyLists; Result := TPacote(inherited First); end;
function TModeloPacotesAssociation.Last : TPacote; begin SetDependencyLists; Result := TPacote(inherited Last); end;
function TModeloPacotesAssociation.Next(var Pacote : TPacote) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Pacote)); end;
function TModeloPacotesAssociation.Prior(var Pacote : TPacote) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Pacote)); end;
function TModeloPacotesAssociation.Find(S : String) : TPacote; begin Result := TPacote(inherited Find(S)); end;
function TModeloPacotesAssociation.Near(S : String) : TPacote; begin SetDependencyLists; Result := TPacote(inherited Near(S)); end;
class function TModeloPacotesAssociation.GetKeyCode : pointer; begin Result := @TPacote.PorNome; end;
class function TModeloPacotesAssociation.GetListType : TListType; begin Result := ltString; end;

{ TPapel }

function TPapel.GetAgregacao : TPapelAgregacao; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 7))._Agregacao;
end;

procedure TPapel.SetAgregacao(Value : TPapelAgregacao); begin
  if Prevalence.IsInRecover then _Agregacao := Value else begin
    if (_Agregacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._Agregacao <> Value then begin
        TPapel(NewImage)._Agregacao := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetMultiplicidade : TPapelMultiplicidade; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 8))._Multiplicidade;
end;

procedure TPapel.SetMultiplicidade(Value : TPapelMultiplicidade); begin
  if Prevalence.IsInRecover then _Multiplicidade := Value else begin
    if (_Multiplicidade = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._Multiplicidade <> Value then begin
        TPapel(NewImage)._Multiplicidade := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetCaracteristicas : TSetPapelCaracteristicas; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 9))._Caracteristicas;
end;

procedure TPapel.SetCaracteristicas(Value : TSetPapelCaracteristicas); begin
  if Prevalence.IsInRecover then _Caracteristicas := Value else begin
    if (_Caracteristicas = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._Caracteristicas <> Value then begin
        TPapel(NewImage)._Caracteristicas := Value;
        UpdateLog(9, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetMinimo : Word; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 10))._Minimo;
end;

procedure TPapel.SetMinimo(Value : Word); begin
  if Prevalence.IsInRecover then _Minimo := Value else begin
    if (_Minimo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._Minimo <> Value then begin
        TPapel(NewImage)._Minimo := Value;
        UpdateLog(10, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetMaximo : Word; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 11))._Maximo;
end;

procedure TPapel.SetMaximo(Value : Word); begin
  if Prevalence.IsInRecover then _Maximo := Value else begin
    if (_Maximo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._Maximo <> Value then begin
        TPapel(NewImage)._Maximo := Value;
        UpdateLog(11, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetConstraint : String; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 12))._Constraint;
end;

procedure TPapel.SetConstraint(Value : String); begin
  if Prevalence.IsInRecover then _Constraint := Value else begin
    if (_Constraint = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._Constraint <> Value then begin
        TPapel(NewImage)._Constraint := Value;
        UpdateLog(12, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetHabilitacao : String; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 13))._Habilitacao;
end;

procedure TPapel.SetHabilitacao(Value : String); begin
  if Prevalence.IsInRecover then _Habilitacao := Value else begin
    if (_Habilitacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._Habilitacao <> Value then begin
        TPapel(NewImage)._Habilitacao := Value;
        UpdateLog(13, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetVisibilidade : String; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 14))._Visibilidade;
end;

procedure TPapel.SetVisibilidade(Value : String); begin
  if Prevalence.IsInRecover then _Visibilidade := Value else begin
    if (_Visibilidade = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._Visibilidade <> Value then begin
        TPapel(NewImage)._Visibilidade := Value;
        UpdateLog(14, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetValidacao : String; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 15))._Validacao;
end;

procedure TPapel.SetValidacao(Value : String); begin
  if Prevalence.IsInRecover then _Validacao := Value else begin
    if (_Validacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._Validacao <> Value then begin
        TPapel(NewImage)._Validacao := Value;
        UpdateLog(15, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetMensagemValidacao : String; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 16))._MensagemValidacao;
end;

procedure TPapel.SetMensagemValidacao(Value : String); begin
  if Prevalence.IsInRecover then _MensagemValidacao := Value else begin
    if (_MensagemValidacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._MensagemValidacao <> Value then begin
        TPapel(NewImage)._MensagemValidacao := Value;
        UpdateLog(16, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetInicial : String; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 17))._Inicial;
end;

procedure TPapel.SetInicial(Value : String); begin
  if Prevalence.IsInRecover then _Inicial := Value else begin
    if (_Inicial = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._Inicial <> Value then begin
        TPapel(NewImage)._Inicial := Value;
        UpdateLog(17, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetNomeAntigo : String; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 18))._NomeAntigo;
end;

procedure TPapel.SetNomeAntigo(Value : String); begin
  if Prevalence.IsInRecover then _NomeAntigo := Value else begin
    if (_NomeAntigo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._NomeAntigo <> Value then begin
        TPapel(NewImage)._NomeAntigo := Value;
        UpdateLog(18, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetOrdem : TIndice; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 19))._Ordem;
  FixReference(TTransient(Result), TIndice);
end;

procedure TPapel.SetOrdem(Value : TIndice); begin
  if Prevalence.IsInRecover then _Ordem := Value else begin
    if (_Ordem = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._Ordem <> Value then begin
        TPapel(NewImage)._Ordem := Value;
        UpdateLog(19, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetAssociacaoDestino : TAssociacao; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 20))._AssociacaoDestino;
  FixReference(TTransient(Result), TAssociacao);
end;

procedure TPapel.SetAssociacaoDestino(Value : TAssociacao); begin
  if Prevalence.IsInRecover then _AssociacaoDestino := Value else begin
    if (_AssociacaoDestino = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._AssociacaoDestino <> Value then begin
        TPapel(NewImage)._AssociacaoDestino := Value;
        UpdateLog(20, NewImage, Stream)
      end;
  end;
end;

function TPapel.GetAssociacaoOrigem : TAssociacao; begin
  Result := TPapel(Prevalence.GetNewImage(Self, 21))._AssociacaoOrigem;
  FixReference(TTransient(Result), TAssociacao);
end;

procedure TPapel.SetAssociacaoOrigem(Value : TAssociacao); begin
  if Prevalence.IsInRecover then _AssociacaoOrigem := Value else begin
    if (_AssociacaoOrigem = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TPapel(NewImage)._AssociacaoOrigem <> Value then begin
        TPapel(NewImage)._AssociacaoOrigem := Value;
        UpdateLog(21, NewImage, Stream)
      end;
  end;
end;
function TPapel.DescreverMultiplicidade : String; begin Result := Papel_DescreverMultiplicidade(Self) end;
function TPapel.FormatarConstraints : String; begin Result := Papel_FormatarConstraints(Self) end;
function TPapel.GetIdentification : String; begin Result := Papel_GetIdentification(Self) end;

function TPapel.CheckMinimo(var Message : String) : Boolean; begin
  Result := (Multiplicidade <> pmOutras) or (Minimo <= Maximo);
  if Result then
    Message := ''
  else
    Message := 'Mínimo deve ser menor ou igual ao Máximo'
end;

function TPapel.VisibleMinimo : Boolean; begin
  Result := Multiplicidade = pmOutras;
end;

function TPapel.CheckMaximo(var Message : String) : Boolean; begin
  Result := (Multiplicidade <> pmOutras) or (Maximo >= Minimo);
  if Result then
    Message := ''
  else
    Message := 'Máximo deve ser maior ou igual ao Mínimo'
end;

function TPapel.VisibleMaximo : Boolean; begin
  Result := Multiplicidade = pmOutras;
end;

function TPapel.VisibleMensagemValidacao : Boolean; begin
  Result := Validacao <> '';
end;

{ TPapelList }
class function TPapelList.GetObjectClass : TTransientClass; begin Result := TPapel; end;
procedure TPapelList.Add(Papel : TPapel); begin inherited Add(TPrevalent(Papel)); end;
procedure TPapelList.Delete(Papel : TPapel); begin inherited Delete(TPrevalent(Papel)); end;
function TPapelList.First : TPapel; begin Result := TPapel(inherited First); end;
function TPapelList.Last : TPapel; begin Result := TPapel(inherited Last); end;
function TPapelList.Next(var Papel : TPapel) : boolean; begin Result := inherited Next(TTransient(Papel)); end;
function TPapelList.Prior(var Papel : TPapel) : boolean; begin Result := inherited Prior(TTransient(Papel)); end;
function TPapelList.Find(I : Integer) : TPapel; begin Result := TPapel(inherited Find(I)); end;
function TPapelList.Near(I : Integer) : TPapel; begin Result := TPapel(inherited Near(I)); end;

{ TPapelPorNomeList }
class function TPapelPorNomeList.GetObjectClass : TTransientClass; begin Result := TPapel; end;
procedure TPapelPorNomeList.Add(Papel : TPapel); begin inherited Add(TPrevalent(Papel)); end;
procedure TPapelPorNomeList.Delete(Papel : TPapel); begin inherited Delete(TPrevalent(Papel)); end;
function TPapelPorNomeList.First : TPapel; begin Result := TPapel(inherited First); end;
function TPapelPorNomeList.Last : TPapel; begin Result := TPapel(inherited Last); end;
function TPapelPorNomeList.Next(var Papel : TPapel) : boolean; begin Result := inherited Next(TTransient(Papel)); end;
function TPapelPorNomeList.Prior(var Papel : TPapel) : boolean; begin Result := inherited Prior(TTransient(Papel)); end;
function TPapelPorNomeList.Find(S : String) : TPapel; begin Result := TPapel(inherited Find(S)); end;
function TPapelPorNomeList.Near(S : String) : TPapel; begin Result := TPapel(inherited Near(S)); end;
class function TPapelPorNomeList.GetKeyCode : pointer; begin Result := @TPapel.PorNome; end;
class function TPapelPorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TTransicao }

function TTransicao.GetCondicao : String; begin
  Result := TTransicao(Prevalence.GetNewImage(Self, 7))._Condicao;
end;

procedure TTransicao.SetCondicao(Value : String); begin
  if Prevalence.IsInRecover then _Condicao := Value else begin
    if (_Condicao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TTransicao(NewImage)._Condicao <> Value then begin
        TTransicao(NewImage)._Condicao := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

function TTransicao.GetAcao : String; begin
  Result := TTransicao(Prevalence.GetNewImage(Self, 8))._Acao;
end;

procedure TTransicao.SetAcao(Value : String); begin
  if Prevalence.IsInRecover then _Acao := Value else begin
    if (_Acao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TTransicao(NewImage)._Acao <> Value then begin
        TTransicao(NewImage)._Acao := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

function TTransicao.GetPontoOrigem : Byte; begin
  Result := TTransicao(Prevalence.GetNewImage(Self, 9))._PontoOrigem;
end;

procedure TTransicao.SetPontoOrigem(Value : Byte); begin
  if Prevalence.IsInRecover then _PontoOrigem := Value else begin
    if (_PontoOrigem = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TTransicao(NewImage)._PontoOrigem <> Value then begin
        TTransicao(NewImage)._PontoOrigem := Value;
        UpdateLog(9, NewImage, Stream)
      end;
  end;
end;

function TTransicao.GetPontoDestino : Byte; begin
  Result := TTransicao(Prevalence.GetNewImage(Self, 10))._PontoDestino;
end;

procedure TTransicao.SetPontoDestino(Value : Byte); begin
  if Prevalence.IsInRecover then _PontoDestino := Value else begin
    if (_PontoDestino = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TTransicao(NewImage)._PontoDestino <> Value then begin
        TTransicao(NewImage)._PontoDestino := Value;
        UpdateLog(10, NewImage, Stream)
      end;
  end;
end;

function TTransicao.GetQuebras : String; begin
  Result := TTransicao(Prevalence.GetNewImage(Self, 11))._Quebras;
end;

procedure TTransicao.SetQuebras(Value : String); begin
  if Prevalence.IsInRecover then _Quebras := Value else begin
    if (_Quebras = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TTransicao(NewImage)._Quebras <> Value then begin
        TTransicao(NewImage)._Quebras := Value;
        UpdateLog(11, NewImage, Stream)
      end;
  end;
end;

function TTransicao.GetOrigem : TEstadoBase; begin
  Result := TTransicao(Prevalence.GetNewImage(Self, 12))._Origem;
  FixReference(TTransient(Result), TEstadoBase);
end;

procedure TTransicao.SetOrigem(Value : TEstadoBase); begin
  if Prevalence.IsInRecover then _Origem := Value else begin
    if (_Origem = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TTransicao(NewImage)._Origem <> Value then begin
        TTransicao(NewImage)._Origem := Value;
        UpdateLog(12, NewImage, Stream)
      end;
  end;
end;

function TTransicao.GetDestino : TEstadoBase; begin
  Result := TTransicao(Prevalence.GetNewImage(Self, 13))._Destino;
  FixReference(TTransient(Result), TEstadoBase);
end;

procedure TTransicao.SetDestino(Value : TEstadoBase); begin
  if Prevalence.IsInRecover then _Destino := Value else begin
    if (_Destino = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TTransicao(NewImage)._Destino <> Value then begin
        TTransicao(NewImage)._Destino := Value;
        UpdateLog(13, NewImage, Stream)
      end;
  end;
end;

function TTransicao.PorPontoOrigem : Integer; begin
  Result := -PontoOrigem
end;
function TTransicao.ChecarNome : Boolean; begin Result := Transicao_ChecarNome(Self) end;
function TTransicao.GetIdentification : String; begin Result := Transicao_GetIdentification(Self) end;

function TTransicao.VisibleCondicao : Boolean; begin
  Result := true;
end;

{ TTransicaoList }
class function TTransicaoList.GetObjectClass : TTransientClass; begin Result := TTransicao; end;
procedure TTransicaoList.Add(Transicao : TTransicao); begin inherited Add(TPrevalent(Transicao)); end;
procedure TTransicaoList.Delete(Transicao : TTransicao); begin inherited Delete(TPrevalent(Transicao)); end;
function TTransicaoList.First : TTransicao; begin Result := TTransicao(inherited First); end;
function TTransicaoList.Last : TTransicao; begin Result := TTransicao(inherited Last); end;
function TTransicaoList.Next(var Transicao : TTransicao) : boolean; begin Result := inherited Next(TTransient(Transicao)); end;
function TTransicaoList.Prior(var Transicao : TTransicao) : boolean; begin Result := inherited Prior(TTransient(Transicao)); end;
function TTransicaoList.Find(I : Integer) : TTransicao; begin Result := TTransicao(inherited Find(I)); end;
function TTransicaoList.Near(I : Integer) : TTransicao; begin Result := TTransicao(inherited Near(I)); end;

{ TTransicaoPorPontoOrigemList }
class function TTransicaoPorPontoOrigemList.GetObjectClass : TTransientClass; begin Result := TTransicao; end;
procedure TTransicaoPorPontoOrigemList.Add(Transicao : TTransicao); begin inherited Add(TPrevalent(Transicao)); end;
procedure TTransicaoPorPontoOrigemList.Delete(Transicao : TTransicao); begin inherited Delete(TPrevalent(Transicao)); end;
function TTransicaoPorPontoOrigemList.First : TTransicao; begin Result := TTransicao(inherited First); end;
function TTransicaoPorPontoOrigemList.Last : TTransicao; begin Result := TTransicao(inherited Last); end;
function TTransicaoPorPontoOrigemList.Next(var Transicao : TTransicao) : boolean; begin Result := inherited Next(TTransient(Transicao)); end;
function TTransicaoPorPontoOrigemList.Prior(var Transicao : TTransicao) : boolean; begin Result := inherited Prior(TTransient(Transicao)); end;
function TTransicaoPorPontoOrigemList.Find(I : Integer) : TTransicao; begin Result := TTransicao(inherited Find(I)); end;
function TTransicaoPorPontoOrigemList.Near(I : Integer) : TTransicao; begin Result := TTransicao(inherited Near(I)); end;
class function TTransicaoPorPontoOrigemList.GetKeyCode : pointer; begin Result := @TTransicao.PorPontoOrigem; end;

{ TTransicaoPorNomeList }
class function TTransicaoPorNomeList.GetObjectClass : TTransientClass; begin Result := TTransicao; end;
procedure TTransicaoPorNomeList.Add(Transicao : TTransicao); begin inherited Add(TPrevalent(Transicao)); end;
procedure TTransicaoPorNomeList.Delete(Transicao : TTransicao); begin inherited Delete(TPrevalent(Transicao)); end;
function TTransicaoPorNomeList.First : TTransicao; begin Result := TTransicao(inherited First); end;
function TTransicaoPorNomeList.Last : TTransicao; begin Result := TTransicao(inherited Last); end;
function TTransicaoPorNomeList.Next(var Transicao : TTransicao) : boolean; begin Result := inherited Next(TTransient(Transicao)); end;
function TTransicaoPorNomeList.Prior(var Transicao : TTransicao) : boolean; begin Result := inherited Prior(TTransient(Transicao)); end;
function TTransicaoPorNomeList.Find(S : String) : TTransicao; begin Result := TTransicao(inherited Find(S)); end;
function TTransicaoPorNomeList.Near(S : String) : TTransicao; begin Result := TTransicao(inherited Near(S)); end;
class function TTransicaoPorNomeList.GetKeyCode : pointer; begin Result := @TTransicao.PorNome; end;
class function TTransicaoPorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TEstadoBaseDestinosAssociation }
class function TEstadoBaseDestinosAssociation.GetObjectClass : TTransientClass; begin Result := TTransicao; end;
procedure TEstadoBaseDestinosAssociation.Add(Transicao : TTransicao); begin inherited Add(TPrevalent(Transicao)); end;
procedure TEstadoBaseDestinosAssociation.Delete(Transicao : TTransicao); begin inherited Delete(TPrevalent(Transicao)); end;
function TEstadoBaseDestinosAssociation.First : TTransicao; begin SetDependencyLists; Result := TTransicao(inherited First); end;
function TEstadoBaseDestinosAssociation.Last : TTransicao; begin SetDependencyLists; Result := TTransicao(inherited Last); end;
function TEstadoBaseDestinosAssociation.Next(var Transicao : TTransicao) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Transicao)); end;
function TEstadoBaseDestinosAssociation.Prior(var Transicao : TTransicao) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Transicao)); end;
function TEstadoBaseDestinosAssociation.Find(I : Integer) : TTransicao; begin Result := TTransicao(inherited Find(I)); end;
function TEstadoBaseDestinosAssociation.Near(I : Integer) : TTransicao; begin SetDependencyLists; Result := TTransicao(inherited Near(I)); end;
class function TEstadoBaseDestinosAssociation.GetKeyCode : pointer; begin Result := @TTransicao.PorPontoOrigem; end;

{ TEstadoBaseOrigensAssociation }
class function TEstadoBaseOrigensAssociation.GetObjectClass : TTransientClass; begin Result := TTransicao; end;
procedure TEstadoBaseOrigensAssociation.Add(Transicao : TTransicao); begin inherited Add(TPrevalent(Transicao)); end;
procedure TEstadoBaseOrigensAssociation.Delete(Transicao : TTransicao); begin inherited Delete(TPrevalent(Transicao)); end;
function TEstadoBaseOrigensAssociation.First : TTransicao; begin SetDependencyLists; Result := TTransicao(inherited First); end;
function TEstadoBaseOrigensAssociation.Last : TTransicao; begin SetDependencyLists; Result := TTransicao(inherited Last); end;
function TEstadoBaseOrigensAssociation.Next(var Transicao : TTransicao) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Transicao)); end;
function TEstadoBaseOrigensAssociation.Prior(var Transicao : TTransicao) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Transicao)); end;
function TEstadoBaseOrigensAssociation.Find(I : Integer) : TTransicao; begin Result := TTransicao(inherited Find(I)); end;
function TEstadoBaseOrigensAssociation.Near(I : Integer) : TTransicao; begin SetDependencyLists; Result := TTransicao(inherited Near(I)); end;

{ TClasse }

procedure TClasse.New; begin
  inherited;
  _Indices := TClasseIndicesAssociation.Create(Self, 'Indices');
  _Metodos := TClasseMetodosAssociation.Create(Self, 'Metodos');
  _Atributos := TClasseAtributosAssociation.Create(Self, 'Atributos');
  _Links := TClasseLinksAssociation.Create(Self, 'Links');
end;

procedure TClasse.InternalFree; begin
  if _Indices <> nil then _Indices.InternalFree;
  if _Metodos <> nil then _Metodos.InternalFree;
  if _Atributos <> nil then _Atributos.InternalFree;
  if _Links <> nil then _Links.InternalFree;
  inherited;
end;

function TClasse.GetIdentificacao : String; begin
  Result := TClasse(Prevalence.GetNewImage(Self, 14))._Identificacao;
end;

procedure TClasse.SetIdentificacao(Value : String); begin
  if Prevalence.IsInRecover then _Identificacao := Value else begin
    if (_Identificacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TClasse(NewImage)._Identificacao <> Value then begin
        TClasse(NewImage)._Identificacao := Value;
        UpdateLog(14, NewImage, Stream)
      end;
  end;
end;

function TClasse.GetTipo : TClasseTipo; begin
  Result := TClasse(Prevalence.GetNewImage(Self, 15))._Tipo;
end;

procedure TClasse.SetTipo(Value : TClasseTipo); begin
  if Prevalence.IsInRecover then _Tipo := Value else begin
    if (_Tipo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TClasse(NewImage)._Tipo <> Value then begin
        TClasse(NewImage)._Tipo := Value;
        UpdateLog(15, NewImage, Stream)
      end;
  end;
end;

function TClasse.GetApresentacao : TClasseApresentacao; begin
  Result := TClasse(Prevalence.GetNewImage(Self, 16))._Apresentacao;
end;

procedure TClasse.SetApresentacao(Value : TClasseApresentacao); begin
  if Prevalence.IsInRecover then _Apresentacao := Value else begin
    if (_Apresentacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TClasse(NewImage)._Apresentacao <> Value then begin
        TClasse(NewImage)._Apresentacao := Value;
        UpdateLog(16, NewImage, Stream)
      end;
  end;
end;

function TClasse.GetOculto : Boolean; begin
  Result := TClasse(Prevalence.GetNewImage(Self, 17))._Oculto;
end;

procedure TClasse.SetOculto(Value : Boolean); begin
  if Prevalence.IsInRecover then _Oculto := Value else begin
    if (_Oculto = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TClasse(NewImage)._Oculto <> Value then begin
        TClasse(NewImage)._Oculto := Value;
        UpdateLog(17, NewImage, Stream)
      end;
  end;
end;

function TClasse.GetNomeAntigo : String; begin
  Result := TClasse(Prevalence.GetNewImage(Self, 18))._NomeAntigo;
end;

procedure TClasse.SetNomeAntigo(Value : String); begin
  if Prevalence.IsInRecover then _NomeAntigo := Value else begin
    if (_NomeAntigo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TClasse(NewImage)._NomeAntigo <> Value then begin
        TClasse(NewImage)._NomeAntigo := Value;
        UpdateLog(18, NewImage, Stream)
      end;
  end;
end;

function TClasse.GetPropOrder : String; begin
  Result := TClasse(Prevalence.GetNewImage(Self, 19))._PropOrder;
end;

procedure TClasse.SetPropOrder(Value : String); begin
  if Prevalence.IsInRecover then _PropOrder := Value else begin
    if (_PropOrder = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TClasse(NewImage)._PropOrder <> Value then begin
        TClasse(NewImage)._PropOrder := Value;
        UpdateLog(19, NewImage, Stream)
      end;
  end;
end;

function TClasse.GetDescricaoAtributos : String; begin
  try Result := DescreverAssociacao(Atributos) except Result := ' ' end;
end;

function TClasse.GetDescricaoMetodos : String; begin
  try Result := DescreverAssociacao(Metodos) except Result := ' ' end;
end;
function TClasse.ChecarNome : Boolean; begin Result := Classe_ChecarNome(Self) end;
function TClasse.GetIdentification : String; begin Result := Classe_GetIdentification(Self) end;

{ TClasseList }
class function TClasseList.GetObjectClass : TTransientClass; begin Result := TClasse; end;
procedure TClasseList.Add(Classe : TClasse); begin inherited Add(TPrevalent(Classe)); end;
procedure TClasseList.Delete(Classe : TClasse); begin inherited Delete(TPrevalent(Classe)); end;
function TClasseList.First : TClasse; begin Result := TClasse(inherited First); end;
function TClasseList.Last : TClasse; begin Result := TClasse(inherited Last); end;
function TClasseList.Next(var Classe : TClasse) : boolean; begin Result := inherited Next(TTransient(Classe)); end;
function TClasseList.Prior(var Classe : TClasse) : boolean; begin Result := inherited Prior(TTransient(Classe)); end;
function TClasseList.Find(I : Integer) : TClasse; begin Result := TClasse(inherited Find(I)); end;
function TClasseList.Near(I : Integer) : TClasse; begin Result := TClasse(inherited Near(I)); end;

{ TClassePorHerancaNomeList }
class function TClassePorHerancaNomeList.GetObjectClass : TTransientClass; begin Result := TClasse; end;
procedure TClassePorHerancaNomeList.Add(Classe : TClasse); begin inherited Add(TPrevalent(Classe)); end;
procedure TClassePorHerancaNomeList.Delete(Classe : TClasse); begin inherited Delete(TPrevalent(Classe)); end;
function TClassePorHerancaNomeList.First : TClasse; begin Result := TClasse(inherited First); end;
function TClassePorHerancaNomeList.Last : TClasse; begin Result := TClasse(inherited Last); end;
function TClassePorHerancaNomeList.Next(var Classe : TClasse) : boolean; begin Result := inherited Next(TTransient(Classe)); end;
function TClassePorHerancaNomeList.Prior(var Classe : TClasse) : boolean; begin Result := inherited Prior(TTransient(Classe)); end;
function TClassePorHerancaNomeList.Find(S : String) : TClasse; begin Result := TClasse(inherited Find(S)); end;
function TClassePorHerancaNomeList.Near(S : String) : TClasse; begin Result := TClasse(inherited Near(S)); end;
class function TClassePorHerancaNomeList.GetKeyCode : pointer; begin Result := @TClasse.PorHerancaNome; end;
class function TClassePorHerancaNomeList.GetListType : TListType; begin Result := ltString; end;

{ TClassePorNomeList }
class function TClassePorNomeList.GetObjectClass : TTransientClass; begin Result := TClasse; end;
procedure TClassePorNomeList.Add(Classe : TClasse); begin inherited Add(TPrevalent(Classe)); end;
procedure TClassePorNomeList.Delete(Classe : TClasse); begin inherited Delete(TPrevalent(Classe)); end;
function TClassePorNomeList.First : TClasse; begin Result := TClasse(inherited First); end;
function TClassePorNomeList.Last : TClasse; begin Result := TClasse(inherited Last); end;
function TClassePorNomeList.Next(var Classe : TClasse) : boolean; begin Result := inherited Next(TTransient(Classe)); end;
function TClassePorNomeList.Prior(var Classe : TClasse) : boolean; begin Result := inherited Prior(TTransient(Classe)); end;
function TClassePorNomeList.Find(S : String) : TClasse; begin Result := TClasse(inherited Find(S)); end;
function TClassePorNomeList.Near(S : String) : TClasse; begin Result := TClasse(inherited Near(S)); end;
class function TClassePorNomeList.GetKeyCode : pointer; begin Result := @TClasse.PorNome; end;
class function TClassePorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TEstado }

procedure TEstado.New; begin
  inherited;
  _Variaveis := TEstadoVariaveisAssociation.Create(Self, 'Variaveis');
  if Prevalence.IsInRecover then exit;
  try _MostrarAcao := true except end;
end;

function TEstado.GetTipo : TEstadoTipo; begin
  Result := TEstado(Prevalence.GetNewImage(Self, 12))._Tipo;
end;

procedure TEstado.SetTipo(Value : TEstadoTipo); begin
  if Prevalence.IsInRecover then _Tipo := Value else begin
    if (_Tipo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TEstado(NewImage)._Tipo <> Value then begin
        TEstado(NewImage)._Tipo := Value;
        UpdateLog(12, NewImage, Stream)
      end;
  end;
end;

function TEstado.GetAcao : String; begin
  Result := TEstado(Prevalence.GetNewImage(Self, 13))._Acao;
end;

procedure TEstado.SetAcao(Value : String); begin
  if Prevalence.IsInRecover then _Acao := Value else begin
    if (_Acao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TEstado(NewImage)._Acao <> Value then begin
        TEstado(NewImage)._Acao := Value;
        UpdateLog(13, NewImage, Stream)
      end;
  end;
end;

function TEstado.GetSubflow : Boolean; begin
  Result := TEstado(Prevalence.GetNewImage(Self, 14))._Subflow;
end;

procedure TEstado.SetSubflow(Value : Boolean); begin
  if Prevalence.IsInRecover then _Subflow := Value else begin
    if (_Subflow = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TEstado(NewImage)._Subflow <> Value then begin
        TEstado(NewImage)._Subflow := Value;
        UpdateLog(14, NewImage, Stream)
      end;
  end;
end;

function TEstado.GetDescricaoVariaveis : String; begin
  try Result := DescreverAssociacao(Variaveis) except Result := ' ' end;
end;

function TEstado.GetMostrarAcao : Boolean; begin
  Result := TEstado(Prevalence.GetNewImage(Self, 16))._MostrarAcao;
end;

procedure TEstado.SetMostrarAcao(Value : Boolean); begin
  if Prevalence.IsInRecover then _MostrarAcao := Value else begin
    if (_MostrarAcao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TEstado(NewImage)._MostrarAcao <> Value then begin
        TEstado(NewImage)._MostrarAcao := Value;
        UpdateLog(16, NewImage, Stream)
      end;
  end;
end;
function TEstado.GetIdentification : String; begin Result := Estado_GetIdentification(Self) end;

function TEstado.CheckAcao(var Message : String) : Boolean; begin
  Result := (pos('Result',acao) = 0) and (pos('Self',acao)=0);
  if Result then
    Message := ''
  else
    Message := 'Encontrado(s) "Result" e/ou "Self" na ação. Utilize "Return" e "this" no lugar.'
end;

{ TEstadoList }
class function TEstadoList.GetObjectClass : TTransientClass; begin Result := TEstado; end;
procedure TEstadoList.Add(Estado : TEstado); begin inherited Add(TPrevalent(Estado)); end;
procedure TEstadoList.Delete(Estado : TEstado); begin inherited Delete(TPrevalent(Estado)); end;
function TEstadoList.First : TEstado; begin Result := TEstado(inherited First); end;
function TEstadoList.Last : TEstado; begin Result := TEstado(inherited Last); end;
function TEstadoList.Next(var Estado : TEstado) : boolean; begin Result := inherited Next(TTransient(Estado)); end;
function TEstadoList.Prior(var Estado : TEstado) : boolean; begin Result := inherited Prior(TTransient(Estado)); end;
function TEstadoList.Find(I : Integer) : TEstado; begin Result := TEstado(inherited Find(I)); end;
function TEstadoList.Near(I : Integer) : TEstado; begin Result := TEstado(inherited Near(I)); end;

{ TEstadoPorPosicaoList }
class function TEstadoPorPosicaoList.GetObjectClass : TTransientClass; begin Result := TEstado; end;
procedure TEstadoPorPosicaoList.Add(Estado : TEstado); begin inherited Add(TPrevalent(Estado)); end;
procedure TEstadoPorPosicaoList.Delete(Estado : TEstado); begin inherited Delete(TPrevalent(Estado)); end;
function TEstadoPorPosicaoList.First : TEstado; begin Result := TEstado(inherited First); end;
function TEstadoPorPosicaoList.Last : TEstado; begin Result := TEstado(inherited Last); end;
function TEstadoPorPosicaoList.Next(var Estado : TEstado) : boolean; begin Result := inherited Next(TTransient(Estado)); end;
function TEstadoPorPosicaoList.Prior(var Estado : TEstado) : boolean; begin Result := inherited Prior(TTransient(Estado)); end;
function TEstadoPorPosicaoList.Find(I : Integer) : TEstado; begin Result := TEstado(inherited Find(I)); end;
function TEstadoPorPosicaoList.Near(I : Integer) : TEstado; begin Result := TEstado(inherited Near(I)); end;
class function TEstadoPorPosicaoList.GetKeyCode : pointer; begin Result := @TEstado.PorPosicao; end;

{ TEstadoPorNomeList }
class function TEstadoPorNomeList.GetObjectClass : TTransientClass; begin Result := TEstado; end;
procedure TEstadoPorNomeList.Add(Estado : TEstado); begin inherited Add(TPrevalent(Estado)); end;
procedure TEstadoPorNomeList.Delete(Estado : TEstado); begin inherited Delete(TPrevalent(Estado)); end;
function TEstadoPorNomeList.First : TEstado; begin Result := TEstado(inherited First); end;
function TEstadoPorNomeList.Last : TEstado; begin Result := TEstado(inherited Last); end;
function TEstadoPorNomeList.Next(var Estado : TEstado) : boolean; begin Result := inherited Next(TTransient(Estado)); end;
function TEstadoPorNomeList.Prior(var Estado : TEstado) : boolean; begin Result := inherited Prior(TTransient(Estado)); end;
function TEstadoPorNomeList.Find(S : String) : TEstado; begin Result := TEstado(inherited Find(S)); end;
function TEstadoPorNomeList.Near(S : String) : TEstado; begin Result := TEstado(inherited Near(S)); end;
class function TEstadoPorNomeList.GetKeyCode : pointer; begin Result := @TEstado.PorNome; end;
class function TEstadoPorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TFormulario }

procedure TFormulario.New; begin
  inherited;
  _Formatacoes := TFormularioFormatacoesAssociation.Create(Self, 'Formatacoes');
end;

procedure TFormulario.InternalFree; begin
  if _Formatacoes <> nil then _Formatacoes.InternalFree;
  inherited;
end;

function TFormulario.GetObjeto : String; begin
  Result := TFormulario(Prevalence.GetNewImage(Self, 12))._Objeto;
end;

procedure TFormulario.SetObjeto(Value : String); begin
  if Prevalence.IsInRecover then _Objeto := Value else begin
    if (_Objeto = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TFormulario(NewImage)._Objeto <> Value then begin
        TFormulario(NewImage)._Objeto := Value;
        UpdateLog(12, NewImage, Stream)
      end;
  end;
end;

function TFormulario.GetSomenteLeitura : Boolean; begin
  Result := TFormulario(Prevalence.GetNewImage(Self, 13))._SomenteLeitura;
end;

procedure TFormulario.SetSomenteLeitura(Value : Boolean); begin
  if Prevalence.IsInRecover then _SomenteLeitura := Value else begin
    if (_SomenteLeitura = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TFormulario(NewImage)._SomenteLeitura <> Value then begin
        TFormulario(NewImage)._SomenteLeitura := Value;
        UpdateLog(13, NewImage, Stream)
      end;
  end;
end;

function TFormulario.GetDescricaoFormatacoes : String; begin
  try Result := DescreverAssociacao(Formatacoes) except Result := ' ' end;
end;

{ TFormularioList }
class function TFormularioList.GetObjectClass : TTransientClass; begin Result := TFormulario; end;
procedure TFormularioList.Add(Formulario : TFormulario); begin inherited Add(TPrevalent(Formulario)); end;
procedure TFormularioList.Delete(Formulario : TFormulario); begin inherited Delete(TPrevalent(Formulario)); end;
function TFormularioList.First : TFormulario; begin Result := TFormulario(inherited First); end;
function TFormularioList.Last : TFormulario; begin Result := TFormulario(inherited Last); end;
function TFormularioList.Next(var Formulario : TFormulario) : boolean; begin Result := inherited Next(TTransient(Formulario)); end;
function TFormularioList.Prior(var Formulario : TFormulario) : boolean; begin Result := inherited Prior(TTransient(Formulario)); end;
function TFormularioList.Find(I : Integer) : TFormulario; begin Result := TFormulario(inherited Find(I)); end;
function TFormularioList.Near(I : Integer) : TFormulario; begin Result := TFormulario(inherited Near(I)); end;

{ TFormularioPorPosicaoList }
class function TFormularioPorPosicaoList.GetObjectClass : TTransientClass; begin Result := TFormulario; end;
procedure TFormularioPorPosicaoList.Add(Formulario : TFormulario); begin inherited Add(TPrevalent(Formulario)); end;
procedure TFormularioPorPosicaoList.Delete(Formulario : TFormulario); begin inherited Delete(TPrevalent(Formulario)); end;
function TFormularioPorPosicaoList.First : TFormulario; begin Result := TFormulario(inherited First); end;
function TFormularioPorPosicaoList.Last : TFormulario; begin Result := TFormulario(inherited Last); end;
function TFormularioPorPosicaoList.Next(var Formulario : TFormulario) : boolean; begin Result := inherited Next(TTransient(Formulario)); end;
function TFormularioPorPosicaoList.Prior(var Formulario : TFormulario) : boolean; begin Result := inherited Prior(TTransient(Formulario)); end;
function TFormularioPorPosicaoList.Find(I : Integer) : TFormulario; begin Result := TFormulario(inherited Find(I)); end;
function TFormularioPorPosicaoList.Near(I : Integer) : TFormulario; begin Result := TFormulario(inherited Near(I)); end;
class function TFormularioPorPosicaoList.GetKeyCode : pointer; begin Result := @TFormulario.PorPosicao; end;

{ TFormularioPorNomeList }
class function TFormularioPorNomeList.GetObjectClass : TTransientClass; begin Result := TFormulario; end;
procedure TFormularioPorNomeList.Add(Formulario : TFormulario); begin inherited Add(TPrevalent(Formulario)); end;
procedure TFormularioPorNomeList.Delete(Formulario : TFormulario); begin inherited Delete(TPrevalent(Formulario)); end;
function TFormularioPorNomeList.First : TFormulario; begin Result := TFormulario(inherited First); end;
function TFormularioPorNomeList.Last : TFormulario; begin Result := TFormulario(inherited Last); end;
function TFormularioPorNomeList.Next(var Formulario : TFormulario) : boolean; begin Result := inherited Next(TTransient(Formulario)); end;
function TFormularioPorNomeList.Prior(var Formulario : TFormulario) : boolean; begin Result := inherited Prior(TTransient(Formulario)); end;
function TFormularioPorNomeList.Find(S : String) : TFormulario; begin Result := TFormulario(inherited Find(S)); end;
function TFormularioPorNomeList.Near(S : String) : TFormulario; begin Result := TFormulario(inherited Near(S)); end;
class function TFormularioPorNomeList.GetKeyCode : pointer; begin Result := @TFormulario.PorNome; end;
class function TFormularioPorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TVariavel }

function TVariavel.GetInicial : String; begin
  Result := TVariavel(Prevalence.GetNewImage(Self, 14))._Inicial;
end;

procedure TVariavel.SetInicial(Value : String); begin
  if Prevalence.IsInRecover then _Inicial := Value else begin
    if (_Inicial = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TVariavel(NewImage)._Inicial <> Value then begin
        TVariavel(NewImage)._Inicial := Value;
        UpdateLog(14, NewImage, Stream)
      end;
  end;
end;

function TVariavel.GetEstado : TEstado; begin
  Result := TVariavel(Prevalence.GetNewImage(Self, 15))._Estado;
  FixReference(TTransient(Result), TEstado);
end;

procedure TVariavel.SetEstado(Value : TEstado); begin
  if Prevalence.IsInRecover then _Estado := Value else begin
    if (_Estado = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TVariavel(NewImage)._Estado <> Value then begin
        TVariavel(NewImage)._Estado := Value;
        UpdateLog(15, NewImage, Stream)
      end;
  end;
end;

function TVariavel.GetMetodo : TMetodo; begin
  Result := TVariavel(Prevalence.GetNewImage(Self, 16))._Metodo;
  FixReference(TTransient(Result), TMetodo);
end;

procedure TVariavel.SetMetodo(Value : TMetodo); begin
  if Prevalence.IsInRecover then _Metodo := Value else begin
    if (_Metodo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TVariavel(NewImage)._Metodo <> Value then begin
        TVariavel(NewImage)._Metodo := Value;
        UpdateLog(16, NewImage, Stream)
      end;
  end;
end;
function TVariavel.ChecarNome : Boolean; begin Result := Variavel_ChecarNome(Self) end;
function TVariavel.GetIdentification : String; begin Result := Variavel_GetIdentification(Self) end;

{ TVariavelList }
class function TVariavelList.GetObjectClass : TTransientClass; begin Result := TVariavel; end;
procedure TVariavelList.Add(Variavel : TVariavel); begin inherited Add(TPrevalent(Variavel)); end;
procedure TVariavelList.Delete(Variavel : TVariavel); begin inherited Delete(TPrevalent(Variavel)); end;
function TVariavelList.First : TVariavel; begin Result := TVariavel(inherited First); end;
function TVariavelList.Last : TVariavel; begin Result := TVariavel(inherited Last); end;
function TVariavelList.Next(var Variavel : TVariavel) : boolean; begin Result := inherited Next(TTransient(Variavel)); end;
function TVariavelList.Prior(var Variavel : TVariavel) : boolean; begin Result := inherited Prior(TTransient(Variavel)); end;
function TVariavelList.Find(I : Integer) : TVariavel; begin Result := TVariavel(inherited Find(I)); end;
function TVariavelList.Near(I : Integer) : TVariavel; begin Result := TVariavel(inherited Near(I)); end;

{ TVariavelPorNomeList }
class function TVariavelPorNomeList.GetObjectClass : TTransientClass; begin Result := TVariavel; end;
procedure TVariavelPorNomeList.Add(Variavel : TVariavel); begin inherited Add(TPrevalent(Variavel)); end;
procedure TVariavelPorNomeList.Delete(Variavel : TVariavel); begin inherited Delete(TPrevalent(Variavel)); end;
function TVariavelPorNomeList.First : TVariavel; begin Result := TVariavel(inherited First); end;
function TVariavelPorNomeList.Last : TVariavel; begin Result := TVariavel(inherited Last); end;
function TVariavelPorNomeList.Next(var Variavel : TVariavel) : boolean; begin Result := inherited Next(TTransient(Variavel)); end;
function TVariavelPorNomeList.Prior(var Variavel : TVariavel) : boolean; begin Result := inherited Prior(TTransient(Variavel)); end;
function TVariavelPorNomeList.Find(S : String) : TVariavel; begin Result := TVariavel(inherited Find(S)); end;
function TVariavelPorNomeList.Near(S : String) : TVariavel; begin Result := TVariavel(inherited Near(S)); end;
class function TVariavelPorNomeList.GetKeyCode : pointer; begin Result := @TVariavel.PorNome; end;
class function TVariavelPorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TEstadoVariaveisAssociation }
class function TEstadoVariaveisAssociation.GetObjectClass : TTransientClass; begin Result := TVariavel; end;
procedure TEstadoVariaveisAssociation.Add(Variavel : TVariavel); begin inherited Add(TPrevalent(Variavel)); end;
procedure TEstadoVariaveisAssociation.Delete(Variavel : TVariavel); begin inherited Delete(TPrevalent(Variavel)); end;
function TEstadoVariaveisAssociation.First : TVariavel; begin SetDependencyLists; Result := TVariavel(inherited First); end;
function TEstadoVariaveisAssociation.Last : TVariavel; begin SetDependencyLists; Result := TVariavel(inherited Last); end;
function TEstadoVariaveisAssociation.Next(var Variavel : TVariavel) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Variavel)); end;
function TEstadoVariaveisAssociation.Prior(var Variavel : TVariavel) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Variavel)); end;
function TEstadoVariaveisAssociation.Find(I : Integer) : TVariavel; begin Result := TVariavel(inherited Find(I)); end;
function TEstadoVariaveisAssociation.Near(I : Integer) : TVariavel; begin SetDependencyLists; Result := TVariavel(inherited Near(I)); end;

{ TMetodoVariaveisAssociation }
class function TMetodoVariaveisAssociation.GetObjectClass : TTransientClass; begin Result := TVariavel; end;
procedure TMetodoVariaveisAssociation.Add(Variavel : TVariavel); begin inherited Add(TPrevalent(Variavel)); end;
procedure TMetodoVariaveisAssociation.Delete(Variavel : TVariavel); begin inherited Delete(TPrevalent(Variavel)); end;
function TMetodoVariaveisAssociation.First : TVariavel; begin SetDependencyLists; Result := TVariavel(inherited First); end;
function TMetodoVariaveisAssociation.Last : TVariavel; begin SetDependencyLists; Result := TVariavel(inherited Last); end;
function TMetodoVariaveisAssociation.Next(var Variavel : TVariavel) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Variavel)); end;
function TMetodoVariaveisAssociation.Prior(var Variavel : TVariavel) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Variavel)); end;
function TMetodoVariaveisAssociation.Find(I : Integer) : TVariavel; begin Result := TVariavel(inherited Find(I)); end;
function TMetodoVariaveisAssociation.Near(I : Integer) : TVariavel; begin SetDependencyLists; Result := TVariavel(inherited Near(I)); end;

{ TValidacao }

function TValidacao.GetValidacao : String; begin
  Result := TValidacao(Prevalence.GetNewImage(Self, 17))._Validacao;
end;

procedure TValidacao.SetValidacao(Value : String); begin
  if Prevalence.IsInRecover then _Validacao := Value else begin
    if (_Validacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TValidacao(NewImage)._Validacao <> Value then begin
        TValidacao(NewImage)._Validacao := Value;
        UpdateLog(17, NewImage, Stream)
      end;
  end;
end;

function TValidacao.GetMensagemValidacao : String; begin
  Result := TValidacao(Prevalence.GetNewImage(Self, 18))._MensagemValidacao;
end;

procedure TValidacao.SetMensagemValidacao(Value : String); begin
  if Prevalence.IsInRecover then _MensagemValidacao := Value else begin
    if (_MensagemValidacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TValidacao(NewImage)._MensagemValidacao <> Value then begin
        TValidacao(NewImage)._MensagemValidacao := Value;
        UpdateLog(18, NewImage, Stream)
      end;
  end;
end;

function TValidacao.CheckMensagemValidacao(var Message : String) : Boolean; begin
  Result := (length(trim(validacao)) = 0) or (length(trim(mensagemValidacao))>0);
  if Result then
    Message := ''
  else
    Message := 'É necessário fornecer uma mensagem de falha de validação'
end;

class function TValidacaoList.GetObjectClass : TTransientClass; begin
  Result := TValidacao
end;

{ TAtributo }

procedure TAtributo.New; begin
  inherited;
  if Prevalence.IsInRecover then exit;
  try _Escopo := aePublished except end;
  try _Ordem := 0 except end;
end;

function TAtributo.GetEscopo : TAtributoEscopo; begin
  Result := TAtributo(Prevalence.GetNewImage(Self, 19))._Escopo;
end;

procedure TAtributo.SetEscopo(Value : TAtributoEscopo); begin
  if Prevalence.IsInRecover then _Escopo := Value else begin
    if (_Escopo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAtributo(NewImage)._Escopo <> Value then begin
        TAtributo(NewImage)._Escopo := Value;
        UpdateLog(19, NewImage, Stream)
      end;
  end;
end;

function TAtributo.GetCaracteristicas : TSetAtributoCaracteristicas; begin
  Result := TAtributo(Prevalence.GetNewImage(Self, 20))._Caracteristicas;
end;

procedure TAtributo.SetCaracteristicas(Value : TSetAtributoCaracteristicas); begin
  if Prevalence.IsInRecover then _Caracteristicas := Value else begin
    if (_Caracteristicas = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAtributo(NewImage)._Caracteristicas <> Value then begin
        TAtributo(NewImage)._Caracteristicas := Value;
        UpdateLog(20, NewImage, Stream)
      end;
  end;
end;

function TAtributo.GetOrdem : TAtributoOrdem; begin
  Result := TAtributo(Prevalence.GetNewImage(Self, 21))._Ordem;
end;

procedure TAtributo.SetOrdem(Value : TAtributoOrdem); begin
  if Prevalence.IsInRecover then _Ordem := Value else begin
    if (_Ordem = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAtributo(NewImage)._Ordem <> Value then begin
        TAtributo(NewImage)._Ordem := Value;
        UpdateLog(21, NewImage, Stream)
      end;
  end;
end;

function TAtributo.GetHabilitacao : String; begin
  Result := TAtributo(Prevalence.GetNewImage(Self, 22))._Habilitacao;
end;

procedure TAtributo.SetHabilitacao(Value : String); begin
  if Prevalence.IsInRecover then _Habilitacao := Value else begin
    if (_Habilitacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAtributo(NewImage)._Habilitacao <> Value then begin
        TAtributo(NewImage)._Habilitacao := Value;
        UpdateLog(22, NewImage, Stream)
      end;
  end;
end;

function TAtributo.GetVisibilidade : String; begin
  Result := TAtributo(Prevalence.GetNewImage(Self, 23))._Visibilidade;
end;

procedure TAtributo.SetVisibilidade(Value : String); begin
  if Prevalence.IsInRecover then _Visibilidade := Value else begin
    if (_Visibilidade = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAtributo(NewImage)._Visibilidade <> Value then begin
        TAtributo(NewImage)._Visibilidade := Value;
        UpdateLog(23, NewImage, Stream)
      end;
  end;
end;

function TAtributo.GetNomeAntigo : String; begin
  Result := TAtributo(Prevalence.GetNewImage(Self, 24))._NomeAntigo;
end;

procedure TAtributo.SetNomeAntigo(Value : String); begin
  if Prevalence.IsInRecover then _NomeAntigo := Value else begin
    if (_NomeAntigo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAtributo(NewImage)._NomeAntigo <> Value then begin
        TAtributo(NewImage)._NomeAntigo := Value;
        UpdateLog(24, NewImage, Stream)
      end;
  end;
end;

function TAtributo.GetClasse : TClasse; begin
  Result := TAtributo(Prevalence.GetNewImage(Self, 25))._Classe;
  FixReference(TTransient(Result), TClasse);
end;

procedure TAtributo.SetClasse(Value : TClasse); begin
  if Prevalence.IsInRecover then _Classe := Value else begin
    if (_Classe = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TAtributo(NewImage)._Classe <> Value then begin
        TAtributo(NewImage)._Classe := Value;
        UpdateLog(25, NewImage, Stream)
      end;
  end;
end;

function TAtributo.PorOrdem : Word; begin
  Result := Ordem
end;
function TAtributo.ChecarNome : Boolean; begin Result := Atributo_ChecarNome(Self) end;
function TAtributo.FormatarCaracteristicas : String; begin Result := Atributo_FormatarCaracteristicas(Self) end;
function TAtributo.FormatarDerivado : String; begin Result := Atributo_FormatarDerivado(Self) end;
function TAtributo.FormatarEscopo : String; begin Result := Atributo_FormatarEscopo(Self) end;
function TAtributo.GetIdentification : String; begin Result := Atributo_GetIdentification(Self) end;

function TAtributo.CheckOrdem(var Message : String) : Boolean; begin
  Result := true; 
  if Result then
    Message := ''
  else
    Message := '.'
end;

{ TAtributoList }
class function TAtributoList.GetObjectClass : TTransientClass; begin Result := TAtributo; end;
procedure TAtributoList.Add(Atributo : TAtributo); begin inherited Add(TPrevalent(Atributo)); end;
procedure TAtributoList.Delete(Atributo : TAtributo); begin inherited Delete(TPrevalent(Atributo)); end;
function TAtributoList.First : TAtributo; begin Result := TAtributo(inherited First); end;
function TAtributoList.Last : TAtributo; begin Result := TAtributo(inherited Last); end;
function TAtributoList.Next(var Atributo : TAtributo) : boolean; begin Result := inherited Next(TTransient(Atributo)); end;
function TAtributoList.Prior(var Atributo : TAtributo) : boolean; begin Result := inherited Prior(TTransient(Atributo)); end;
function TAtributoList.Find(I : Integer) : TAtributo; begin Result := TAtributo(inherited Find(I)); end;
function TAtributoList.Near(I : Integer) : TAtributo; begin Result := TAtributo(inherited Near(I)); end;

{ TAtributoPorOrdemList }
class function TAtributoPorOrdemList.GetObjectClass : TTransientClass; begin Result := TAtributo; end;
procedure TAtributoPorOrdemList.Add(Atributo : TAtributo); begin inherited Add(TPrevalent(Atributo)); end;
procedure TAtributoPorOrdemList.Delete(Atributo : TAtributo); begin inherited Delete(TPrevalent(Atributo)); end;
function TAtributoPorOrdemList.First : TAtributo; begin Result := TAtributo(inherited First); end;
function TAtributoPorOrdemList.Last : TAtributo; begin Result := TAtributo(inherited Last); end;
function TAtributoPorOrdemList.Next(var Atributo : TAtributo) : boolean; begin Result := inherited Next(TTransient(Atributo)); end;
function TAtributoPorOrdemList.Prior(var Atributo : TAtributo) : boolean; begin Result := inherited Prior(TTransient(Atributo)); end;
function TAtributoPorOrdemList.Find(W : Word) : TAtributo; begin Result := TAtributo(inherited Find(W)); end;
function TAtributoPorOrdemList.Near(W : Word) : TAtributo; begin Result := TAtributo(inherited Near(W)); end;
class function TAtributoPorOrdemList.GetKeyCode : pointer; begin Result := @TAtributo.PorOrdem; end;
class function TAtributoPorOrdemList.GetListType : TListType; begin Result := ltWord; end;

{ TAtributoPorNomeList }
class function TAtributoPorNomeList.GetObjectClass : TTransientClass; begin Result := TAtributo; end;
procedure TAtributoPorNomeList.Add(Atributo : TAtributo); begin inherited Add(TPrevalent(Atributo)); end;
procedure TAtributoPorNomeList.Delete(Atributo : TAtributo); begin inherited Delete(TPrevalent(Atributo)); end;
function TAtributoPorNomeList.First : TAtributo; begin Result := TAtributo(inherited First); end;
function TAtributoPorNomeList.Last : TAtributo; begin Result := TAtributo(inherited Last); end;
function TAtributoPorNomeList.Next(var Atributo : TAtributo) : boolean; begin Result := inherited Next(TTransient(Atributo)); end;
function TAtributoPorNomeList.Prior(var Atributo : TAtributo) : boolean; begin Result := inherited Prior(TTransient(Atributo)); end;
function TAtributoPorNomeList.Find(S : String) : TAtributo; begin Result := TAtributo(inherited Find(S)); end;
function TAtributoPorNomeList.Near(S : String) : TAtributo; begin Result := TAtributo(inherited Near(S)); end;
class function TAtributoPorNomeList.GetKeyCode : pointer; begin Result := @TAtributo.PorNome; end;
class function TAtributoPorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TClasseAtributosAssociation }
class function TClasseAtributosAssociation.GetObjectClass : TTransientClass; begin Result := TAtributo; end;
procedure TClasseAtributosAssociation.Add(Atributo : TAtributo); begin inherited Add(TPrevalent(Atributo)); end;
procedure TClasseAtributosAssociation.Delete(Atributo : TAtributo); begin inherited Delete(TPrevalent(Atributo)); end;
function TClasseAtributosAssociation.First : TAtributo; begin SetDependencyLists; Result := TAtributo(inherited First); end;
function TClasseAtributosAssociation.Last : TAtributo; begin SetDependencyLists; Result := TAtributo(inherited Last); end;
function TClasseAtributosAssociation.Next(var Atributo : TAtributo) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Atributo)); end;
function TClasseAtributosAssociation.Prior(var Atributo : TAtributo) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Atributo)); end;
function TClasseAtributosAssociation.Find(W : Word) : TAtributo; begin Result := TAtributo(inherited Find(W)); end;
function TClasseAtributosAssociation.Near(W : Word) : TAtributo; begin SetDependencyLists; Result := TAtributo(inherited Near(W)); end;
class function TClasseAtributosAssociation.GetKeyCode : pointer; begin Result := @TAtributo.PorOrdem; end;
class function TClasseAtributosAssociation.GetListType : TListType; begin Result := ltWord; end;

{ TParametro }

function TParametro.GetPassagem : TParametroPassagem; begin
  Result := TParametro(Prevalence.GetNewImage(Self, 19))._Passagem;
end;

procedure TParametro.SetPassagem(Value : TParametroPassagem); begin
  if Prevalence.IsInRecover then _Passagem := Value else begin
    if (_Passagem = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if TParametro(NewImage)._Passagem <> Value then begin
        TParametro(NewImage)._Passagem := Value;
        UpdateLog(19, NewImage, Stream)
      end;
  end;
end;
function TParametro.ChecarNome : Boolean; begin Result := Parametro_ChecarNome(Self) end;

{ TParametroList }
class function TParametroList.GetObjectClass : TTransientClass; begin Result := TParametro; end;
procedure TParametroList.Add(Parametro : TParametro); begin inherited Add(TPrevalent(Parametro)); end;
procedure TParametroList.Delete(Parametro : TParametro); begin inherited Delete(TPrevalent(Parametro)); end;
function TParametroList.First : TParametro; begin Result := TParametro(inherited First); end;
function TParametroList.Last : TParametro; begin Result := TParametro(inherited Last); end;
function TParametroList.Next(var Parametro : TParametro) : boolean; begin Result := inherited Next(TTransient(Parametro)); end;
function TParametroList.Prior(var Parametro : TParametro) : boolean; begin Result := inherited Prior(TTransient(Parametro)); end;
function TParametroList.Find(I : Integer) : TParametro; begin Result := TParametro(inherited Find(I)); end;
function TParametroList.Near(I : Integer) : TParametro; begin Result := TParametro(inherited Near(I)); end;

{ TParametroPorNomeList }
class function TParametroPorNomeList.GetObjectClass : TTransientClass; begin Result := TParametro; end;
procedure TParametroPorNomeList.Add(Parametro : TParametro); begin inherited Add(TPrevalent(Parametro)); end;
procedure TParametroPorNomeList.Delete(Parametro : TParametro); begin inherited Delete(TPrevalent(Parametro)); end;
function TParametroPorNomeList.First : TParametro; begin Result := TParametro(inherited First); end;
function TParametroPorNomeList.Last : TParametro; begin Result := TParametro(inherited Last); end;
function TParametroPorNomeList.Next(var Parametro : TParametro) : boolean; begin Result := inherited Next(TTransient(Parametro)); end;
function TParametroPorNomeList.Prior(var Parametro : TParametro) : boolean; begin Result := inherited Prior(TTransient(Parametro)); end;
function TParametroPorNomeList.Find(S : String) : TParametro; begin Result := TParametro(inherited Find(S)); end;
function TParametroPorNomeList.Near(S : String) : TParametro; begin Result := TParametro(inherited Near(S)); end;
class function TParametroPorNomeList.GetKeyCode : pointer; begin Result := @TParametro.PorNome; end;
class function TParametroPorNomeList.GetListType : TListType; begin Result := ltString; end;

{ TMetodoParametrosAssociation }
class function TMetodoParametrosAssociation.GetObjectClass : TTransientClass; begin Result := TParametro; end;
procedure TMetodoParametrosAssociation.Add(Parametro : TParametro); begin inherited Add(TPrevalent(Parametro)); end;
procedure TMetodoParametrosAssociation.Delete(Parametro : TParametro); begin inherited Delete(TPrevalent(Parametro)); end;
function TMetodoParametrosAssociation.First : TParametro; begin SetDependencyLists; Result := TParametro(inherited First); end;
function TMetodoParametrosAssociation.Last : TParametro; begin SetDependencyLists; Result := TParametro(inherited Last); end;
function TMetodoParametrosAssociation.Next(var Parametro : TParametro) : boolean; begin SetDependencyLists; Result := inherited Next(TTransient(Parametro)); end;
function TMetodoParametrosAssociation.Prior(var Parametro : TParametro) : boolean; begin SetDependencyLists; Result := inherited Prior(TTransient(Parametro)); end;
function TMetodoParametrosAssociation.Find(I : Integer) : TParametro; begin Result := TParametro(inherited Find(I)); end;
function TMetodoParametrosAssociation.Near(I : Integer) : TParametro; begin SetDependencyLists; Result := TParametro(inherited Near(I)); end;

initialization
  SetLength(RecoverInits, length(RecoverInits) + 1);
  RecoverInits[high(RecoverInits)] := InitpitinnuModel;
end.
