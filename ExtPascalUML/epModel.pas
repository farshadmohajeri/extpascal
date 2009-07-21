{$A1,B-,C-,D-,E-,F-,G+,H+,I-,J+,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V-,W-,X+,Y-,Z1}
{$IFDEF FPC}{$PACKRECORDS 1}{$Z1}{$ENDIF}
{$IFDEF DEBUG}{$D+,O-,A1,I+,W+,V-,R+,B-,X+,P+,H+,J+,L+,Y+}{$ENDIF}

unit epModel;
{
Gerado por Generator Versão 1.9.44
Usuário : wander
Máquina : NOTEBOOK-WANDER
Em : 04/07/2007 19:5:01
}

{
Pontos de Função: FPA
  Total: 343, Média: 8.0, Risco geral: Baixo, Classes de alto risco: Nenhuma
Cyclomatic Complexity: v(G)
  Total: 56, Média: 3.3, Risco geral: Baixo, Métodos de alto risco: Nenhum
}

interface

uses
  Classes, SysUtils, epThread, epPrevalence, epObjectList, epCommon, epUtils, epScheduler; 

//*** User Declarations ***
type TUnpPrevalence = class(TPrevalence);
//*************************

type
  T_Aud_Arquivo = class;
  T_Aud_Classe = class;
  T_Aud_Criterio = class;
  T_Aud_Login = class;
  T_Aud_Operacao = class;
  T_Aud_Origem = class;
  T_Aud_Package = class;
  T_Aud_UpdObjeto = class;
  T_Classe = class;
  T_Dominio = class;
  T_ExportaImporta = class;
  T_Geral = class;
  T_Grupo = class;
  T_ImportaPropriedade = class;
  T_Log = class;
  T_Metodo = class;
  T_Ocupacao = class;
  T_OcupacaoAssociacao = class;
  T_Package = class;
  T_PermissaoClasse = class;
  T_PermissaoMetodo = class;
  T_PermissaoPackage = class;
  T_PermissaoPropriedade = class;
  T_PermissaoView = class;
  T_Propriedade = class;
  T_Run = class;
  T_Sequence = class;
  T_Servico = class;
  T_Sessao = class;
  T_UsuarioBloqueado = class;
  T_ExpPropriedade = class;
  T_Exporta = class;
  T_Importa = class;
  T_OcupacaoClasse = class;
  T_OcupacaoLista = class;
  T_OcupacaoPackage = class;
  T_Pendency = class;
  T_PropriedadeDelimitado = class;
  T_PropriedadePosicional = class;
  T_Task = class;
  T_ImportaDelimitado = class;
  T_ImportaPosicional = class;
  T_PendencyTimeoutTask = class;

  T_ImportaDelimitadoPropriedadesAssociation = class;
  T_ExportaPropriedadesAssociation = class;
  T_ImportaPosicionalPropriedadesAssociation = class;
  T_OcupacaoListaAssociacoesAssociation = class;
  T_OcupacaoClasseOcupacoesAssociation = class;
  T_OcupacaoPackageClassesAssociation = class;
  T_PackageOcupacoesAssociation = class;
  T_ClasseOcupacoesAssociation = class;
  T_Aud_LoginOperacoesAssociation = class;
  T_Aud_PackageOperacoesAssociation = class;
  T_Aud_OperacaoDetalhesAssociation = class;
  T_Aud_OrigemOperacoesAssociation = class;
  T_Aud_ClasseOperacoesAssociation = class;
  T_Aud_CriterioOperacoesAssociation = class;
  T_SessaoCriteriosAssociation = class;
  T_SessaoServicosAssociation = class;
  T_ClassePermissoesClassesAssociation = class;
  T_PermissaoPackagePermissoesClassesAssociation = class;
  T_MetodoPermissoesMetodosAssociation = class;
  T_PermissaoPackagePermissoesMetodosAssociation = class;
  T_ClasseMetodosAssociation = class;
  T_ClassePropriedadesAssociation = class;
  T_PackageClassesAssociation = class;
  T_PackagePermissaoPackageAssociation = class;
  T_PermissaoPackagePermissoesViewsAssociation = class;
  T_PermissaoPackagePermissoesPropriedadesAssociation = class;
  T_GrupoPackagesAssociation = class;
  T_MetodoPermissoesViewAssociation = class;
  T_GrupoCanDelegateAssociation = class;
  T_GrupoProfileAssociation = class;
  T_GrupoAssignedPendenciesAssociation = class;
  T_DominioGruposAssociation = class;
  T_GrupoPendenciesAssociation = class;
  T_PropriedadePermissoesPropriedadesAssociation = class;
  T_RunLogAssociation = class;

  // Enumeration(s) for T_ImportaPropriedade
  T_ImportaPropriedadeElemento = 1..100;

  // Enumeration(s) for T_Propriedade
  T_PropriedadeTipo = (_ptAtributo, _ptAssociacao, _ptReferencia);

  // Enumeration(s) for T_Run
  T_RunLogKind = (_rlkNone, _rlkSingle, _rlkAll);

  // Enumeration(s) for T_Importa
  T_ImportaSeExiste = (_iseIgnorar, _iseAtualizar);
  T_ImportaNumeroElementoChave = 1..100;

  // Enumeration(s) for T_Task
  T_TaskPeriodicityUnit = (_tpuMinutos, _tpuHoras, _tpuDias, _tpuMeses, _tpuAnos);

  // Pontos de Função: 8.0, Baixo
  T_Aud_Arquivo = class(TPrevalent)
  private
    _Nome : String; 
    _Data : DateTime; 
    _Status : T_Aud_ArquivoStatus; 
    function  GetNome : String;
    procedure SetNome(Value : String);
    function  GetData : DateTime;
    procedure SetData(Value : DateTime);
    function  GetStatus : T_Aud_ArquivoStatus;
    procedure SetStatus(Value : T_Aud_ArquivoStatus);
  published
    property Nome : String read GetNome write SetNome;
    property Data : DateTime read GetData write SetData;
    property Status : T_Aud_ArquivoStatus read GetStatus write SetStatus;
    function PorNome : String; 
    function PorStatus : word; 
    function PorData : DateTime; 
  end;

  T_Aud_ArquivoList = class(TPrevalentList)
  private
    function Get_Aud_Arquivo(I : integer) : T_Aud_Arquivo;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Arquivo : T_Aud_Arquivo);
    procedure Delete(_Aud_Arquivo : T_Aud_Arquivo);
    function Find(I : Integer) : T_Aud_Arquivo;
    function First : T_Aud_Arquivo;
    function Last : T_Aud_Arquivo;
    function Near(I : Integer) : T_Aud_Arquivo;
    function Next(var _Aud_Arquivo : T_Aud_Arquivo) : boolean;
    function Prior(var _Aud_Arquivo : T_Aud_Arquivo) : boolean;
    property _Aud_Arquivo[I : integer] : T_Aud_Arquivo read Get_Aud_Arquivo; default;
  end;

  T_Aud_ArquivoPorNomeList = class(TPrevalentList)
  private
    function Get_Aud_Arquivo(I : integer) : T_Aud_Arquivo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Arquivo : T_Aud_Arquivo);
    procedure Delete(_Aud_Arquivo : T_Aud_Arquivo);
    function Find(S : String) : T_Aud_Arquivo;
    function First : T_Aud_Arquivo;
    function Last : T_Aud_Arquivo;
    function Near(S : String) : T_Aud_Arquivo;
    function Next(var _Aud_Arquivo : T_Aud_Arquivo) : boolean;
    function Prior(var _Aud_Arquivo : T_Aud_Arquivo) : boolean;
    property _Aud_Arquivo[I : integer] : T_Aud_Arquivo read Get_Aud_Arquivo; default;
  end;

  T_Aud_ArquivoPorStatusList = class(TPrevalentList)
  private
    function Get_Aud_Arquivo(I : integer) : T_Aud_Arquivo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Arquivo : T_Aud_Arquivo);
    procedure Delete(_Aud_Arquivo : T_Aud_Arquivo);
    function Find(w : word) : T_Aud_Arquivo;
    function First : T_Aud_Arquivo;
    function Last : T_Aud_Arquivo;
    function Near(w : word) : T_Aud_Arquivo;
    function Next(var _Aud_Arquivo : T_Aud_Arquivo) : boolean;
    function Prior(var _Aud_Arquivo : T_Aud_Arquivo) : boolean;
    property _Aud_Arquivo[I : integer] : T_Aud_Arquivo read Get_Aud_Arquivo; default;
  end;

  T_Aud_ArquivoPorDataList = class(TPrevalentList)
  private
    function Get_Aud_Arquivo(I : integer) : T_Aud_Arquivo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Arquivo : T_Aud_Arquivo);
    procedure Delete(_Aud_Arquivo : T_Aud_Arquivo);
    function Find(D : DateTime) : T_Aud_Arquivo;
    function First : T_Aud_Arquivo;
    function Last : T_Aud_Arquivo;
    function Near(D : DateTime) : T_Aud_Arquivo;
    function Next(var _Aud_Arquivo : T_Aud_Arquivo) : boolean;
    function Prior(var _Aud_Arquivo : T_Aud_Arquivo) : boolean;
    property _Aud_Arquivo[I : integer] : T_Aud_Arquivo read Get_Aud_Arquivo; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Aud_Classe = class(TPrevalent)
  private
    _Nome : String; 
    _Operacoes : T_Aud_ClasseOperacoesAssociation; 
    function  GetNome : String;
    procedure SetNome(Value : String);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Nome : String read GetNome write SetNome;
    property Operacoes : T_Aud_ClasseOperacoesAssociation read _Operacoes write _Operacoes;
    function PorNome : String; 
  end;

  T_Aud_ClasseList = class(TPrevalentList)
  private
    function Get_Aud_Classe(I : integer) : T_Aud_Classe;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Classe : T_Aud_Classe);
    procedure Delete(_Aud_Classe : T_Aud_Classe);
    function Find(I : Integer) : T_Aud_Classe;
    function First : T_Aud_Classe;
    function Last : T_Aud_Classe;
    function Near(I : Integer) : T_Aud_Classe;
    function Next(var _Aud_Classe : T_Aud_Classe) : boolean;
    function Prior(var _Aud_Classe : T_Aud_Classe) : boolean;
    property _Aud_Classe[I : integer] : T_Aud_Classe read Get_Aud_Classe; default;
  end;

  T_Aud_ClassePorNomeList = class(TPrevalentList)
  private
    function Get_Aud_Classe(I : integer) : T_Aud_Classe;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Classe : T_Aud_Classe);
    procedure Delete(_Aud_Classe : T_Aud_Classe);
    function Find(S : String) : T_Aud_Classe;
    function First : T_Aud_Classe;
    function Last : T_Aud_Classe;
    function Near(S : String) : T_Aud_Classe;
    function Next(var _Aud_Classe : T_Aud_Classe) : boolean;
    function Prior(var _Aud_Classe : T_Aud_Classe) : boolean;
    property _Aud_Classe[I : integer] : T_Aud_Classe read Get_Aud_Classe; default;
  end;

  // Pontos de Função: 8.0, Baixo
  // Cyclomatic Complexity: 5, Média: 2.5, Baixo
  T_Aud_Criterio = class(TPrevalent)
  private
    _DataGeracao : DateTime; 
    _DataInicio : DateTime; 
    _DataFim : DateTime; 
    _Operacao : T_Operation; 
    _Usuario : String; 
    _Package : String; 
    _Classe : String;
    _Texto : String; 
    _Operacoes : T_Aud_CriterioOperacoesAssociation; 
    _Sessao : T_Sessao; 
    function  GetDataGeracao : DateTime;
    procedure SetDataGeracao(Value : DateTime);
    function  GetDataInicio : DateTime;
    procedure SetDataInicio(Value : DateTime);
    function  GetDataFim : DateTime;
    procedure SetDataFim(Value : DateTime);
    function  GetOperacao : T_Operation;
    procedure SetOperacao(Value : T_Operation);
    function  GetUsuario : String;
    procedure SetUsuario(Value : String);
    function  GetPackage : String;
    procedure SetPackage(Value : String);
    function  GetClasse : String;
    procedure SetClasse(Value : String);
    function  GetTexto : String;
    procedure SetTexto(Value : String);
    function  GetSessao : T_Sessao;
    procedure SetSessao(Value : T_Sessao);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property DataGeracao : DateTime read GetDataGeracao write SetDataGeracao;
    property DataInicio : DateTime read GetDataInicio write SetDataInicio;
    property DataFim : DateTime read GetDataFim write SetDataFim;
    property Operacao : T_Operation read GetOperacao write SetOperacao;
    property Usuario : String read GetUsuario write SetUsuario;
    property Package : String read GetPackage write SetPackage;
    property Classe : String read GetClasse write SetClasse;
    property Texto : String read GetTexto write SetTexto;
    property Operacoes : T_Aud_CriterioOperacoesAssociation read _Operacoes write _Operacoes;
    property Sessao : T_Sessao read GetSessao write SetSessao;
    class procedure GerarPesquisa; // Cyclomatic Complexity: 2, Baixo
    procedure RecuperarObjeto; // Cyclomatic Complexity: 3, Baixo
    function PorDataInicio : DateTime; 
    function PorUsuario : String; 
  end;

  T_Aud_CriterioList = class(TPrevalentList)
  private
    function Get_Aud_Criterio(I : integer) : T_Aud_Criterio;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Criterio : T_Aud_Criterio);
    procedure Delete(_Aud_Criterio : T_Aud_Criterio);
    function Find(I : Integer) : T_Aud_Criterio;
    function First : T_Aud_Criterio;
    function Last : T_Aud_Criterio;
    function Near(I : Integer) : T_Aud_Criterio;
    function Next(var _Aud_Criterio : T_Aud_Criterio) : boolean;
    function Prior(var _Aud_Criterio : T_Aud_Criterio) : boolean;
    property _Aud_Criterio[I : integer] : T_Aud_Criterio read Get_Aud_Criterio; default;
  end;

  T_Aud_CriterioPorDataInicioList = class(TPrevalentList)
  private
    function Get_Aud_Criterio(I : integer) : T_Aud_Criterio;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Criterio : T_Aud_Criterio);
    procedure Delete(_Aud_Criterio : T_Aud_Criterio);
    function Find(D : DateTime) : T_Aud_Criterio;
    function First : T_Aud_Criterio;
    function Last : T_Aud_Criterio;
    function Near(D : DateTime) : T_Aud_Criterio;
    function Next(var _Aud_Criterio : T_Aud_Criterio) : boolean;
    function Prior(var _Aud_Criterio : T_Aud_Criterio) : boolean;
    property _Aud_Criterio[I : integer] : T_Aud_Criterio read Get_Aud_Criterio; default;
  end;

  T_Aud_CriterioPorUsuarioList = class(TPrevalentList)
  private
    function Get_Aud_Criterio(I : integer) : T_Aud_Criterio;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Criterio : T_Aud_Criterio);
    procedure Delete(_Aud_Criterio : T_Aud_Criterio);
    function Find(S : String) : T_Aud_Criterio;
    function First : T_Aud_Criterio;
    function Last : T_Aud_Criterio;
    function Near(S : String) : T_Aud_Criterio;
    function Next(var _Aud_Criterio : T_Aud_Criterio) : boolean;
    function Prior(var _Aud_Criterio : T_Aud_Criterio) : boolean;
    property _Aud_Criterio[I : integer] : T_Aud_Criterio read Get_Aud_Criterio; default;
  end;

  T_SessaoCriteriosAssociation = class(TAssociation)
  private
    function Get_Aud_Criterio(I : integer) : T_Aud_Criterio;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Criterio : T_Aud_Criterio);
    procedure Delete(_Aud_Criterio : T_Aud_Criterio);
    function Find(I : Integer) : T_Aud_Criterio;
    function First : T_Aud_Criterio;
    function Last : T_Aud_Criterio;
    function Near(I : Integer) : T_Aud_Criterio;
    function Next(var _Aud_Criterio : T_Aud_Criterio) : boolean;
    function Prior(var _Aud_Criterio : T_Aud_Criterio) : boolean;
    property _Aud_Criterio[I : integer] : T_Aud_Criterio read Get_Aud_Criterio; default;
  end;

// Utilizado para controlar os logins e as trocas de perfis na auditoria
  // Pontos de Função: 8.0, Baixo
  T_Aud_Login = class(TPrevalent)
  private
    _Usuario : String; 
    _Nome : String; 
    _Maquina : String; 
    _Posicao : Integer; // posição no arquivo de auditoria
    _Perfil : String; 
    _Arquivo : T_Aud_Arquivo; 
    _Operacoes : T_Aud_LoginOperacoesAssociation;
    function  GetUsuario : String;
    procedure SetUsuario(Value : String);
    function  GetNome : String;
    procedure SetNome(Value : String);
    function  GetMaquina : String;
    procedure SetMaquina(Value : String);
    function  GetPosicao : Integer;
    procedure SetPosicao(Value : Integer);
    function  GetPerfil : String;
    procedure SetPerfil(Value : String);
    function  GetArquivo : T_Aud_Arquivo;
    procedure SetArquivo(Value : T_Aud_Arquivo);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Usuario : String read GetUsuario write SetUsuario;
    property Nome : String read GetNome write SetNome;
    property Maquina : String read GetMaquina write SetMaquina;
    property Posicao : Integer read GetPosicao write SetPosicao;
    property Perfil : String read GetPerfil write SetPerfil;
    property Arquivo : T_Aud_Arquivo read GetArquivo write SetArquivo;
    property Operacoes : T_Aud_LoginOperacoesAssociation read _Operacoes write _Operacoes;
    function PorUsuarioMaquinaPerfil : String; 
  end;

  T_Aud_LoginList = class(TPrevalentList)
  private
    function Get_Aud_Login(I : integer) : T_Aud_Login;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Login : T_Aud_Login);
    procedure Delete(_Aud_Login : T_Aud_Login);
    function Find(I : Integer) : T_Aud_Login;
    function First : T_Aud_Login;
    function Last : T_Aud_Login;
    function Near(I : Integer) : T_Aud_Login;
    function Next(var _Aud_Login : T_Aud_Login) : boolean;
    function Prior(var _Aud_Login : T_Aud_Login) : boolean;
    property _Aud_Login[I : integer] : T_Aud_Login read Get_Aud_Login; default;
  end;

  T_Aud_LoginPorUsuarioMaquinaPerfilList = class(TPrevalentList)
  private
    function Get_Aud_Login(I : integer) : T_Aud_Login;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Login : T_Aud_Login);
    procedure Delete(_Aud_Login : T_Aud_Login);
    function Find(S : String) : T_Aud_Login;
    function First : T_Aud_Login;
    function Last : T_Aud_Login;
    function Near(S : String) : T_Aud_Login;
    function Next(var _Aud_Login : T_Aud_Login) : boolean;
    function Prior(var _Aud_Login : T_Aud_Login) : boolean;
    property _Aud_Login[I : integer] : T_Aud_Login read Get_Aud_Login; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Aud_Operacao = class(TPrevalent)
  private
    _Operacao : T_Operation; 
    _Data : DateTime; 
    _IdentificacaoObjeto : String; 
    _Posicao : Integer; 
    _Arquivo : T_Aud_Arquivo; 
    _Login : T_Aud_Login; 
    _Package : T_Aud_Package; 
    _Detalhes : T_Aud_OperacaoDetalhesAssociation; 
    _Origem : T_Aud_Origem; 
    _Classe : T_Aud_Classe; 
    _Criterio : T_Aud_Criterio; 
    function  GetOperacao : T_Operation;
    procedure SetOperacao(Value : T_Operation);
    function  GetData : DateTime;
    procedure SetData(Value : DateTime);
    function  GetAliasClasse : String;
    function  GetIdentificacaoObjeto : String;
    procedure SetIdentificacaoObjeto(Value : String);
    function  GetPosicao : Integer;
    procedure SetPosicao(Value : Integer);
    function  GetArquivo : T_Aud_Arquivo;
    procedure SetArquivo(Value : T_Aud_Arquivo);
    function  GetLogin : T_Aud_Login;
    procedure SetLogin(Value : T_Aud_Login);
    function  GetPackage : T_Aud_Package;
    procedure SetPackage(Value : T_Aud_Package);
    function  GetOrigem : T_Aud_Origem;
    procedure SetOrigem(Value : T_Aud_Origem);
    function  GetClasse : T_Aud_Classe;
    procedure SetClasse(Value : T_Aud_Classe);
    function  GetCriterio : T_Aud_Criterio;
    procedure SetCriterio(Value : T_Aud_Criterio);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Operacao : T_Operation read GetOperacao write SetOperacao;
    property Data : DateTime read GetData write SetData;
    property AliasClasse : String read GetAliasClasse;
    property IdentificacaoObjeto : String read GetIdentificacaoObjeto write SetIdentificacaoObjeto;
    property Posicao : Integer read GetPosicao write SetPosicao;
    property Arquivo : T_Aud_Arquivo read GetArquivo write SetArquivo;
    property Login : T_Aud_Login read GetLogin write SetLogin;
    property Package : T_Aud_Package read GetPackage write SetPackage;
    property Detalhes : T_Aud_OperacaoDetalhesAssociation read _Detalhes write _Detalhes;
    property Origem : T_Aud_Origem read GetOrigem write SetOrigem;
    property Classe : T_Aud_Classe read GetClasse write SetClasse;
    property Criterio : T_Aud_Criterio read GetCriterio write SetCriterio;
  end;

  T_Aud_OperacaoList = class(TPrevalentList)
  private
    function Get_Aud_Operacao(I : integer) : T_Aud_Operacao;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Operacao : T_Aud_Operacao);
    procedure Delete(_Aud_Operacao : T_Aud_Operacao);
    function Find(I : Integer) : T_Aud_Operacao;
    function First : T_Aud_Operacao;
    function Last : T_Aud_Operacao;
    function Near(I : Integer) : T_Aud_Operacao;
    function Next(var _Aud_Operacao : T_Aud_Operacao) : boolean;
    function Prior(var _Aud_Operacao : T_Aud_Operacao) : boolean;
    property _Aud_Operacao[I : integer] : T_Aud_Operacao read Get_Aud_Operacao; default;
  end;

  T_Aud_LoginOperacoesAssociation = class(TAssociation)
  private
    function Get_Aud_Operacao(I : integer) : T_Aud_Operacao;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Operacao : T_Aud_Operacao);
    procedure Delete(_Aud_Operacao : T_Aud_Operacao);
    function Find(I : Integer) : T_Aud_Operacao;
    function First : T_Aud_Operacao;
    function Last : T_Aud_Operacao;
    function Near(I : Integer) : T_Aud_Operacao;
    function Next(var _Aud_Operacao : T_Aud_Operacao) : boolean;
    function Prior(var _Aud_Operacao : T_Aud_Operacao) : boolean;
    property _Aud_Operacao[I : integer] : T_Aud_Operacao read Get_Aud_Operacao; default;
  end;

  T_Aud_PackageOperacoesAssociation = class(TAssociation)
  private
    function Get_Aud_Operacao(I : integer) : T_Aud_Operacao;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Operacao : T_Aud_Operacao);
    procedure Delete(_Aud_Operacao : T_Aud_Operacao);
    function Find(I : Integer) : T_Aud_Operacao;
    function First : T_Aud_Operacao;
    function Last : T_Aud_Operacao;
    function Near(I : Integer) : T_Aud_Operacao;
    function Next(var _Aud_Operacao : T_Aud_Operacao) : boolean;
    function Prior(var _Aud_Operacao : T_Aud_Operacao) : boolean;
    property _Aud_Operacao[I : integer] : T_Aud_Operacao read Get_Aud_Operacao; default;
  end;

  T_Aud_OrigemOperacoesAssociation = class(TAssociation)
  private
    function Get_Aud_Operacao(I : integer) : T_Aud_Operacao;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Operacao : T_Aud_Operacao);
    procedure Delete(_Aud_Operacao : T_Aud_Operacao);
    function Find(I : Integer) : T_Aud_Operacao;
    function First : T_Aud_Operacao;
    function Last : T_Aud_Operacao;
    function Near(I : Integer) : T_Aud_Operacao;
    function Next(var _Aud_Operacao : T_Aud_Operacao) : boolean;
    function Prior(var _Aud_Operacao : T_Aud_Operacao) : boolean;
    property _Aud_Operacao[I : integer] : T_Aud_Operacao read Get_Aud_Operacao; default;
  end;

  T_Aud_ClasseOperacoesAssociation = class(TAssociation)
  private
    function Get_Aud_Operacao(I : integer) : T_Aud_Operacao;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Operacao : T_Aud_Operacao);
    procedure Delete(_Aud_Operacao : T_Aud_Operacao);
    function Find(I : Integer) : T_Aud_Operacao;
    function First : T_Aud_Operacao;
    function Last : T_Aud_Operacao;
    function Near(I : Integer) : T_Aud_Operacao;
    function Next(var _Aud_Operacao : T_Aud_Operacao) : boolean;
    function Prior(var _Aud_Operacao : T_Aud_Operacao) : boolean;
    property _Aud_Operacao[I : integer] : T_Aud_Operacao read Get_Aud_Operacao; default;
  end;

  T_Aud_CriterioOperacoesAssociation = class(TAssociation)
  private
    function Get_Aud_Operacao(I : integer) : T_Aud_Operacao;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Operacao : T_Aud_Operacao);
    procedure Delete(_Aud_Operacao : T_Aud_Operacao);
    function Find(I : Integer) : T_Aud_Operacao;
    function First : T_Aud_Operacao;
    function Last : T_Aud_Operacao;
    function Near(I : Integer) : T_Aud_Operacao;
    function Next(var _Aud_Operacao : T_Aud_Operacao) : boolean;
    function Prior(var _Aud_Operacao : T_Aud_Operacao) : boolean;
    property _Aud_Operacao[I : integer] : T_Aud_Operacao read Get_Aud_Operacao; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Aud_Origem = class(TPrevalent)
  private
    _Nome : String; 
    _Operacoes : T_Aud_OrigemOperacoesAssociation; 
    function  GetNome : String;
    procedure SetNome(Value : String);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Nome : String read GetNome write SetNome;
    property Operacoes : T_Aud_OrigemOperacoesAssociation read _Operacoes write _Operacoes;
    function PorNome : String; 
  end;

  T_Aud_OrigemList = class(TPrevalentList)
  private
    function Get_Aud_Origem(I : integer) : T_Aud_Origem;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Origem : T_Aud_Origem);
    procedure Delete(_Aud_Origem : T_Aud_Origem);
    function Find(I : Integer) : T_Aud_Origem;
    function First : T_Aud_Origem;
    function Last : T_Aud_Origem;
    function Near(I : Integer) : T_Aud_Origem;
    function Next(var _Aud_Origem : T_Aud_Origem) : boolean;
    function Prior(var _Aud_Origem : T_Aud_Origem) : boolean;
    property _Aud_Origem[I : integer] : T_Aud_Origem read Get_Aud_Origem; default;
  end;

  T_Aud_OrigemPorNomeList = class(TPrevalentList)
  private
    function Get_Aud_Origem(I : integer) : T_Aud_Origem;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Origem : T_Aud_Origem);
    procedure Delete(_Aud_Origem : T_Aud_Origem);
    function Find(S : String) : T_Aud_Origem;
    function First : T_Aud_Origem;
    function Last : T_Aud_Origem;
    function Near(S : String) : T_Aud_Origem;
    function Next(var _Aud_Origem : T_Aud_Origem) : boolean;
    function Prior(var _Aud_Origem : T_Aud_Origem) : boolean;
    property _Aud_Origem[I : integer] : T_Aud_Origem read Get_Aud_Origem; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Aud_Package = class(TPrevalent)
  private
    _Nome : String; 
    _Operacoes : T_Aud_PackageOperacoesAssociation;
    function  GetNome : String;
    procedure SetNome(Value : String);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Nome : String read GetNome write SetNome;
    property Operacoes : T_Aud_PackageOperacoesAssociation read _Operacoes write _Operacoes;
    function PorNome : String; 
  end;

  T_Aud_PackageList = class(TPrevalentList)
  private
    function Get_Aud_Package(I : integer) : T_Aud_Package;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Package : T_Aud_Package);
    procedure Delete(_Aud_Package : T_Aud_Package);
    function Find(I : Integer) : T_Aud_Package;
    function First : T_Aud_Package;
    function Last : T_Aud_Package;
    function Near(I : Integer) : T_Aud_Package;
    function Next(var _Aud_Package : T_Aud_Package) : boolean;
    function Prior(var _Aud_Package : T_Aud_Package) : boolean;
    property _Aud_Package[I : integer] : T_Aud_Package read Get_Aud_Package; default;
  end;

  T_Aud_PackagePorNomeList = class(TPrevalentList)
  private
    function Get_Aud_Package(I : integer) : T_Aud_Package;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_Package : T_Aud_Package);
    procedure Delete(_Aud_Package : T_Aud_Package);
    function Find(S : String) : T_Aud_Package;
    function First : T_Aud_Package;
    function Last : T_Aud_Package;
    function Near(S : String) : T_Aud_Package;
    function Next(var _Aud_Package : T_Aud_Package) : boolean;
    function Prior(var _Aud_Package : T_Aud_Package) : boolean;
    property _Aud_Package[I : integer] : T_Aud_Package read Get_Aud_Package; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Aud_UpdObjeto = class(TPrevalent)
  private
    _Propriedade : String; 
    _ValorAnterior : String; 
    _NovoValor : String; 
    _Operacao : T_Aud_Operacao; 
    function  GetPropriedade : String;
    procedure SetPropriedade(Value : String);
    function  GetValorAnterior : String;
    procedure SetValorAnterior(Value : String);
    function  GetNovoValor : String;
    procedure SetNovoValor(Value : String);
    function  GetOperacao : T_Aud_Operacao;
    procedure SetOperacao(Value : T_Aud_Operacao);
  published
    property Propriedade : String read GetPropriedade write SetPropriedade;
    property ValorAnterior : String read GetValorAnterior write SetValorAnterior;
    property NovoValor : String read GetNovoValor write SetNovoValor;
    property Operacao : T_Aud_Operacao read GetOperacao write SetOperacao;
  end;

  T_Aud_UpdObjetoList = class(TPrevalentList)
  private
    function Get_Aud_UpdObjeto(I : integer) : T_Aud_UpdObjeto;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_UpdObjeto : T_Aud_UpdObjeto);
    procedure Delete(_Aud_UpdObjeto : T_Aud_UpdObjeto);
    function Find(I : Integer) : T_Aud_UpdObjeto;
    function First : T_Aud_UpdObjeto;
    function Last : T_Aud_UpdObjeto;
    function Near(I : Integer) : T_Aud_UpdObjeto;
    function Next(var _Aud_UpdObjeto : T_Aud_UpdObjeto) : boolean;
    function Prior(var _Aud_UpdObjeto : T_Aud_UpdObjeto) : boolean;
    property _Aud_UpdObjeto[I : integer] : T_Aud_UpdObjeto read Get_Aud_UpdObjeto; default;
  end;

  T_Aud_OperacaoDetalhesAssociation = class(TAssociation)
  private
    function Get_Aud_UpdObjeto(I : integer) : T_Aud_UpdObjeto;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Aud_UpdObjeto : T_Aud_UpdObjeto);
    procedure Delete(_Aud_UpdObjeto : T_Aud_UpdObjeto);
    function Find(I : Integer) : T_Aud_UpdObjeto;
    function First : T_Aud_UpdObjeto;
    function Last : T_Aud_UpdObjeto;
    function Near(I : Integer) : T_Aud_UpdObjeto;
    function Next(var _Aud_UpdObjeto : T_Aud_UpdObjeto) : boolean;
    function Prior(var _Aud_UpdObjeto : T_Aud_UpdObjeto) : boolean;
    property _Aud_UpdObjeto[I : integer] : T_Aud_UpdObjeto read Get_Aud_UpdObjeto; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Classe = class(TPrevalent)
  private
    _AliasNome : String; 
    _Nome : String; 
    _Ocupacoes : T_ClasseOcupacoesAssociation; 
    _PermissoesClasses : T_ClassePermissoesClassesAssociation; 
    _Metodos : T_ClasseMetodosAssociation; 
    _Propriedades : T_ClassePropriedadesAssociation; 
    _Package : T_Package;
    function  GetAliasNome : String;
    procedure SetAliasNome(Value : String);
    function  GetNome : String;
    procedure SetNome(Value : String);
    function  GetPackage : T_Package;
    procedure SetPackage(Value : T_Package);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property AliasNome : String read GetAliasNome write SetAliasNome;
    property Nome : String read GetNome write SetNome;
    function GetIdentification : string; override;
    property Ocupacoes : T_ClasseOcupacoesAssociation read _Ocupacoes write _Ocupacoes;
    property PermissoesClasses : T_ClassePermissoesClassesAssociation read _PermissoesClasses write _PermissoesClasses;
    property Metodos : T_ClasseMetodosAssociation read _Metodos write _Metodos;
    property Propriedades : T_ClassePropriedadesAssociation read _Propriedades write _Propriedades;
    property Package : T_Package read GetPackage write SetPackage;
    function PorNome : String; 
    class procedure AtualizaClasses;
    function Get_Metodo(Nome : String) : T_Metodo; 
    function Get_Propriedade(Nome : String) : T_Propriedade; 
    function PorAlias : String; 
  end;

  T_ClasseList = class(TPrevalentList)
  private
    function Get_Classe(I : integer) : T_Classe;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Classe : T_Classe);
    procedure Delete(_Classe : T_Classe);
    function Find(I : Integer) : T_Classe;
    function First : T_Classe;
    function Last : T_Classe;
    function Near(I : Integer) : T_Classe;
    function Next(var _Classe : T_Classe) : boolean;
    function Prior(var _Classe : T_Classe) : boolean;
    property _Classe[I : integer] : T_Classe read Get_Classe; default;
  end;

  T_ClassePorNomeList = class(TPrevalentList)
  private
    function Get_Classe(I : integer) : T_Classe;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Classe : T_Classe);
    procedure Delete(_Classe : T_Classe);
    function Find(S : String) : T_Classe;
    function First : T_Classe;
    function Last : T_Classe;
    function Near(S : String) : T_Classe;
    function Next(var _Classe : T_Classe) : boolean;
    function Prior(var _Classe : T_Classe) : boolean;
    property _Classe[I : integer] : T_Classe read Get_Classe; default;
  end;

  T_ClassePorAliasList = class(TPrevalentList)
  private
    function Get_Classe(I : integer) : T_Classe;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Classe : T_Classe);
    procedure Delete(_Classe : T_Classe);
    function Find(S : String) : T_Classe;
    function First : T_Classe;
    function Last : T_Classe;
    function Near(S : String) : T_Classe;
    function Next(var _Classe : T_Classe) : boolean;
    function Prior(var _Classe : T_Classe) : boolean;
    property _Classe[I : integer] : T_Classe read Get_Classe; default;
  end;

  T_PackageClassesAssociation = class(TAssociation)
  private
    function Get_Classe(I : integer) : T_Classe;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Classe : T_Classe);
    procedure Delete(_Classe : T_Classe);
    function Find(I : Integer) : T_Classe;
    function First : T_Classe;
    function Last : T_Classe;
    function Near(I : Integer) : T_Classe;
    function Next(var _Classe : T_Classe) : boolean;
    function Prior(var _Classe : T_Classe) : boolean;
    property _Classe[I : integer] : T_Classe read Get_Classe; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Dominio = class(TPrevalent)
  private
    _Nome : String; 
    _Grupos : T_DominioGruposAssociation; 
    function  GetNome : String;
    procedure SetNome(Value : String);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Nome : String read GetNome write SetNome;
    property Grupos : T_DominioGruposAssociation read _Grupos write _Grupos;
    function PorNome : String; 
  end;

  T_DominioList = class(TPrevalentList)
  private
    function Get_Dominio(I : integer) : T_Dominio;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Dominio : T_Dominio);
    procedure Delete(_Dominio : T_Dominio);
    function Find(I : Integer) : T_Dominio;
    function First : T_Dominio;
    function Last : T_Dominio;
    function Near(I : Integer) : T_Dominio;
    function Next(var _Dominio : T_Dominio) : boolean;
    function Prior(var _Dominio : T_Dominio) : boolean;
    property _Dominio[I : integer] : T_Dominio read Get_Dominio; default;
  end;

  T_DominioPorNomeList = class(TPrevalentList)
  private
    function Get_Dominio(I : integer) : T_Dominio;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Dominio : T_Dominio);
    procedure Delete(_Dominio : T_Dominio);
    function Find(S : String) : T_Dominio;
    function First : T_Dominio;
    function Last : T_Dominio;
    function Near(S : String) : T_Dominio;
    function Next(var _Dominio : T_Dominio) : boolean;
    function Prior(var _Dominio : T_Dominio) : boolean;
    property _Dominio[I : integer] : T_Dominio read Get_Dominio; default;
  end;

  // Esta Classe permite a definição de importações de arquivos texto para dentro da Prevalência.
  // As importações são tratadas de duas formas:
  // 1. arquivos com delimitadores de campo;
  // 2. arquivos com tratamento posicional.
  // A forma de utilização da importação segue os seguintes passos:
  // 1.	Seleciona-se uma das classes inferiores "Importação com Delimitador" ou "Importação Posicional", conforme o arquivo a ser tratado;
  // 2.	Executa-se o método de classe "Gerar Modelo", onde será gerado um "Modelo de Importação", baseado na classe solicitada. Maiores informações consultar a documentação do Método;
  // 3.	Ajusta-se o "Modelo de Importação" gerado, personalizando-o ao arquivo;
  // 4.	Neste momento o arquivo já estará pronto para ser importado, devendo ser invocada a importação através de "Tarefas", selecionando "Importar Arquivo do Tipo Posicional" ou "Importar Arquivo do Tipo Delimitado" e escolhendo o "Modelo de Importação" previamente gerado.
  // Pontos de Função: 8.0, Baixo
  T_ExportaImporta = class(TPrevalent)
  private
    _Nome : String; 
    _Arquivo : String; 
    _Log : Boolean; 
    _FormatoDataHora : String; 
    _Classe : T_Classe; 
    function  GetNome : String;
    procedure SetNome(Value : String);
    function  GetArquivo : String;
    procedure SetArquivo(Value : String);
    function  GetLog : Boolean;
    procedure SetLog(Value : Boolean);
    function  GetFormatoDataHora : String;
    procedure SetFormatoDataHora(Value : String);
    function  GetClasse : T_Classe;
    procedure SetClasse(Value : T_Classe);
  protected
    procedure New; override;
  published
    property Nome : String read GetNome write SetNome;
    property Arquivo : String read GetArquivo write SetArquivo;
    property Log : Boolean read GetLog write SetLog;
    property FormatoDataHora : String read GetFormatoDataHora write SetFormatoDataHora;
    property Classe : T_Classe read GetClasse write SetClasse;
    function PorNome : string; 
  end;

  T_ExportaImportaList = class(TPrevalentList)
  public
    class function GetObjectClass : TTransientClass; override;
  end;

  T_ExportaImportaPorNomeList = class(TPrevalentList)
  public
  end;

  // Pontos de Função: 8.0, Baixo
  T_Geral = class(TPrevalent)
  published
    procedure CheckSort; 
  end;

  T_GeralList = class(TPrevalentList)
  private
    function Get_Geral(I : integer) : T_Geral;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Geral : T_Geral);
    procedure Delete(_Geral : T_Geral);
    function Find(I : Integer) : T_Geral;
    function First : T_Geral;
    function Last : T_Geral;
    function Near(I : Integer) : T_Geral;
    function Next(var _Geral : T_Geral) : boolean;
    function Prior(var _Geral : T_Geral) : boolean;
    property _Geral[I : integer] : T_Geral read Get_Geral; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Grupo = class(TPrevalent)
  private
    _Nome : String; // Nome do Grupo no sistema Operacional
    _Ativo : Boolean; 
    _AliasNome : String; 
    _Packages : T_GrupoPackagesAssociation; 
    _CanDelegate : T_GrupoCanDelegateAssociation; 
    _Profile : T_GrupoProfileAssociation; 
    _AssignedPendencies : T_GrupoAssignedPendenciesAssociation; 
    _Dominio : T_Dominio; 
    _Pendencies : T_GrupoPendenciesAssociation; 
    function  GetNome : String;
    procedure SetNome(Value : String);
    function  GetAtivo : Boolean;
    procedure SetAtivo(Value : Boolean);
    function  GetAliasNome : String;
    procedure SetAliasNome(Value : String);
    function  GetIdentificador : String;
    function  GetDominio : T_Dominio;
    procedure SetDominio(Value : T_Dominio);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Nome : String read GetNome write SetNome;
    property Ativo : Boolean read GetAtivo write SetAtivo;
    property AliasNome : String read GetAliasNome write SetAliasNome;
    property Identificador : String read GetIdentificador;
    function GetIdentification : string; override;
    property Packages : T_GrupoPackagesAssociation read _Packages write _Packages;
    property CanDelegate : T_GrupoCanDelegateAssociation read _CanDelegate write _CanDelegate;
    property Profile : T_GrupoProfileAssociation read _Profile write _Profile;
    property AssignedPendencies : T_GrupoAssignedPendenciesAssociation read _AssignedPendencies write _AssignedPendencies;
    property Dominio : T_Dominio read GetDominio write SetDominio;
    property Pendencies : T_GrupoPendenciesAssociation read _Pendencies write _Pendencies;
    function PorNome : String; 
    function PorIdentificador : String; 
  end;

  T_GrupoList = class(TPrevalentList)
  private
    function Get_Grupo(I : integer) : T_Grupo;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Grupo : T_Grupo);
    procedure Delete(_Grupo : T_Grupo);
    function Find(I : Integer) : T_Grupo;
    function First : T_Grupo;
    function Last : T_Grupo;
    function Near(I : Integer) : T_Grupo;
    function Next(var _Grupo : T_Grupo) : boolean;
    function Prior(var _Grupo : T_Grupo) : boolean;
    property _Grupo[I : integer] : T_Grupo read Get_Grupo; default;
  end;

  T_GrupoPorNomeList = class(TPrevalentList)
  private
    function Get_Grupo(I : integer) : T_Grupo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Grupo : T_Grupo);
    procedure Delete(_Grupo : T_Grupo);
    function Find(S : String) : T_Grupo;
    function First : T_Grupo;
    function Last : T_Grupo;
    function Near(S : String) : T_Grupo;
    function Next(var _Grupo : T_Grupo) : boolean;
    function Prior(var _Grupo : T_Grupo) : boolean;
    property _Grupo[I : integer] : T_Grupo read Get_Grupo; default;
  end;

  T_GrupoPorIdentificadorList = class(TPrevalentList)
  private
    function Get_Grupo(I : integer) : T_Grupo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Grupo : T_Grupo);
    procedure Delete(_Grupo : T_Grupo);
    function Find(S : String) : T_Grupo;
    function First : T_Grupo;
    function Last : T_Grupo;
    function Near(S : String) : T_Grupo;
    function Next(var _Grupo : T_Grupo) : boolean;
    function Prior(var _Grupo : T_Grupo) : boolean;
    property _Grupo[I : integer] : T_Grupo read Get_Grupo; default;
  end;

  T_GrupoProfileAssociation = class(TAssociation)
  private
    function Get_Grupo(I : integer) : T_Grupo;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Grupo : T_Grupo);
    procedure Delete(_Grupo : T_Grupo);
    function Find(I : Integer) : T_Grupo;
    function First : T_Grupo;
    function Last : T_Grupo;
    function Near(I : Integer) : T_Grupo;
    function Next(var _Grupo : T_Grupo) : boolean;
    function Prior(var _Grupo : T_Grupo) : boolean;
    property _Grupo[I : integer] : T_Grupo read Get_Grupo; default;
  end;

  T_GrupoCanDelegateAssociation = class(TAssociation)
  private
    function Get_Grupo(I : integer) : T_Grupo;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Grupo : T_Grupo);
    procedure Delete(_Grupo : T_Grupo);
    function Find(I : Integer) : T_Grupo;
    function First : T_Grupo;
    function Last : T_Grupo;
    function Near(I : Integer) : T_Grupo;
    function Next(var _Grupo : T_Grupo) : boolean;
    function Prior(var _Grupo : T_Grupo) : boolean;
    property _Grupo[I : integer] : T_Grupo read Get_Grupo; default;
  end;

  T_DominioGruposAssociation = class(TAssociation)
  private
    function Get_Grupo(I : integer) : T_Grupo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Grupo : T_Grupo);
    procedure Delete(_Grupo : T_Grupo);
    function Find(S : String) : T_Grupo;
    function First : T_Grupo;
    function Last : T_Grupo;
    function Near(S : String) : T_Grupo;
    function Next(var _Grupo : T_Grupo) : boolean;
    function Prior(var _Grupo : T_Grupo) : boolean;
    property _Grupo[I : integer] : T_Grupo read Get_Grupo; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_ImportaPropriedade = class(TPrevalent)
  private
    _Elemento : T_ImportaPropriedadeElemento; 
    _ListaElementoChave : String; 
    _Propriedade : T_Propriedade; 
    function  GetElemento : T_ImportaPropriedadeElemento;
    procedure SetElemento(Value : T_ImportaPropriedadeElemento);
    function  GetTipo : T_PropriedadeTipo;
    function  GetListaElementoChave : String;
    procedure SetListaElementoChave(Value : String);
    function  GetPropNome : string;
    function  GetPropriedade : T_Propriedade;
    procedure SetPropriedade(Value : T_Propriedade);
  published
    property Elemento : T_ImportaPropriedadeElemento read GetElemento write SetElemento;
    property Tipo : T_PropriedadeTipo read GetTipo;
    property ListaElementoChave : String read GetListaElementoChave write SetListaElementoChave;
    property PropNome : string read GetPropNome;
    property Propriedade : T_Propriedade read GetPropriedade write SetPropriedade;
    function PorElemento : Integer; 
    function PorPropNome : String; 
    function CheckListaElementoChave(var Message : String) : Boolean; virtual;
  end;

  T_ImportaPropriedadeList = class(TPrevalentList)
  public
    class function GetObjectClass : TTransientClass; override;
  end;

  T_ImportaPropriedadePorElementoList = class(TPrevalentList)
  public
  end;

  T_ImportaPropriedadePorPropNomeList = class(TPrevalentList)
  public
  end;

  // Pontos de Função: 8.0, Baixo
  T_Log = class(TPrevalent)
  private
    _DateTime : DateTime; 
    _Text : String; 
    function  GetDateTime : DateTime;
    procedure SetDateTime(Value : DateTime);
    function  GetText : String;
    procedure SetText(Value : String);
  published
    property DateTime : DateTime read GetDateTime write SetDateTime;
    property Text : String read GetText write SetText;
  end;

  T_LogList = class(TPrevalentList)
  private
    function Get_Log(I : integer) : T_Log;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Log : T_Log);
    procedure Delete(_Log : T_Log);
    function Find(I : Integer) : T_Log;
    function First : T_Log;
    function Last : T_Log;
    function Near(I : Integer) : T_Log;
    function Next(var _Log : T_Log) : boolean;
    function Prior(var _Log : T_Log) : boolean;
    property _Log[I : integer] : T_Log read Get_Log; default;
  end;

  T_RunLogAssociation = class(TAssociation)
  private
    function Get_Log(I : integer) : T_Log;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Log : T_Log);
    procedure Delete(_Log : T_Log);
    function Find(I : Integer) : T_Log;
    function First : T_Log;
    function Last : T_Log;
    function Near(I : Integer) : T_Log;
    function Next(var _Log : T_Log) : boolean;
    function Prior(var _Log : T_Log) : boolean;
    property _Log[I : integer] : T_Log read Get_Log; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Metodo = class(TPrevalent)
  private
    _AliasNome : String; 
    _Tipo : T_Stereotype; 
    _Nome : String; 
    _Bloqueado : Boolean; 
    _PermissoesMetodos : T_MetodoPermissoesMetodosAssociation; 
    _Classe : T_Classe; 
    _PermissoesView : T_MetodoPermissoesViewAssociation; 
    function  GetAliasNome : String;
    procedure SetAliasNome(Value : String);
    function  GetTipo : T_Stereotype;
    procedure SetTipo(Value : T_Stereotype);
    function  GetNome : String;
    procedure SetNome(Value : String);
    function  GetBloqueado : Boolean;
    procedure SetBloqueado(Value : Boolean);
    function  GetIdentificador : String;
    function  GetClasse : T_Classe;
    procedure SetClasse(Value : T_Classe);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property AliasNome : String read GetAliasNome write SetAliasNome;
    property Tipo : T_Stereotype read GetTipo write SetTipo;
    property Nome : String read GetNome write SetNome;
    property Bloqueado : Boolean read GetBloqueado write SetBloqueado;
    property Identificador : String read GetIdentificador;
    function GetIdentification : string; override;
    property PermissoesMetodos : T_MetodoPermissoesMetodosAssociation read _PermissoesMetodos write _PermissoesMetodos;
    property Classe : T_Classe read GetClasse write SetClasse;
    property PermissoesView : T_MetodoPermissoesViewAssociation read _PermissoesView write _PermissoesView;
    function PorNome : String; 
    function PorView : String; 
    function PorViewFilter : Boolean;
    function PorAlias : String;
    function PorIdentificador : String; 
  end;

  T_MetodoList = class(TPrevalentList)
  private
    function Get_Metodo(I : integer) : T_Metodo;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Metodo : T_Metodo);
    procedure Delete(_Metodo : T_Metodo);
    function Find(I : Integer) : T_Metodo;
    function First : T_Metodo;
    function Last : T_Metodo;
    function Near(I : Integer) : T_Metodo;
    function Next(var _Metodo : T_Metodo) : boolean;
    function Prior(var _Metodo : T_Metodo) : boolean;
    property _Metodo[I : integer] : T_Metodo read Get_Metodo; default;
  end;

  T_MetodoPorNomeList = class(TPrevalentList)
  private
    function Get_Metodo(I : integer) : T_Metodo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Metodo : T_Metodo);
    procedure Delete(_Metodo : T_Metodo);
    function Find(S : String) : T_Metodo;
    function First : T_Metodo;
    function Last : T_Metodo;
    function Near(S : String) : T_Metodo;
    function Next(var _Metodo : T_Metodo) : boolean;
    function Prior(var _Metodo : T_Metodo) : boolean;
    property _Metodo[I : integer] : T_Metodo read Get_Metodo; default;
  end;

  T_MetodoPorViewList = class(TPrevalentList)
  private
    function Get_Metodo(I : integer) : T_Metodo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Metodo : T_Metodo);
    procedure Delete(_Metodo : T_Metodo);
    function Find(S : String) : T_Metodo;
    function First : T_Metodo;
    function Last : T_Metodo;
    function Near(S : String) : T_Metodo;
    function Next(var _Metodo : T_Metodo) : boolean;
    function Prior(var _Metodo : T_Metodo) : boolean;
    property _Metodo[I : integer] : T_Metodo read Get_Metodo; default;
  end;

  T_MetodoPorAliasList = class(TPrevalentList)
  private
    function Get_Metodo(I : integer) : T_Metodo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Metodo : T_Metodo);
    procedure Delete(_Metodo : T_Metodo);
    function Find(S : String) : T_Metodo;
    function First : T_Metodo;
    function Last : T_Metodo;
    function Near(S : String) : T_Metodo;
    function Next(var _Metodo : T_Metodo) : boolean;
    function Prior(var _Metodo : T_Metodo) : boolean;
    property _Metodo[I : integer] : T_Metodo read Get_Metodo; default;
  end;

  T_MetodoPorIdentificadorList = class(TPrevalentList)
  private
    function Get_Metodo(I : integer) : T_Metodo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Metodo : T_Metodo);
    procedure Delete(_Metodo : T_Metodo);
    function Find(S : String) : T_Metodo;
    function First : T_Metodo;
    function Last : T_Metodo;
    function Near(S : String) : T_Metodo;
    function Next(var _Metodo : T_Metodo) : boolean;
    function Prior(var _Metodo : T_Metodo) : boolean;
    property _Metodo[I : integer] : T_Metodo read Get_Metodo; default;
  end;

  T_ClasseMetodosAssociation = class(TAssociation)
  private
    function Get_Metodo(I : integer) : T_Metodo;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Metodo : T_Metodo);
    procedure Delete(_Metodo : T_Metodo);
    function Find(S : String) : T_Metodo;
    function First : T_Metodo;
    function Last : T_Metodo;
    function Near(S : String) : T_Metodo;
    function Next(var _Metodo : T_Metodo) : boolean;
    function Prior(var _Metodo : T_Metodo) : boolean;
    property _Metodo[I : integer] : T_Metodo read Get_Metodo; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Ocupacao = class(TPrevalent)
  end;

  T_OcupacaoList = class(TPrevalentList)
  public
    class function GetObjectClass : TTransientClass; override;
  end;

  // Pontos de Função: 8.0, Baixo
  T_OcupacaoAssociacao = class(TPrevalent)
  private
    _Associacao : String; 
    _Count : Integer; 
    _Capacity : Integer; 
    _Lista : T_OcupacaoLista;
    function  GetAssociacao : String;
    procedure SetAssociacao(Value : String);
    function  GetCount : Integer;
    procedure SetCount(Value : Integer);
    function  GetCapacity : Integer;
    procedure SetCapacity(Value : Integer);
    function  GetMemoria : Double;
    function  GetLista : T_OcupacaoLista;
    procedure SetLista(Value : T_OcupacaoLista);
  published
    property Associacao : String read GetAssociacao write SetAssociacao;
    property Count : Integer read GetCount write SetCount;
    property Capacity : Integer read GetCapacity write SetCapacity;
    property Memoria : Double read GetMemoria;
    property Lista : T_OcupacaoLista read GetLista write SetLista;
  end;

  T_OcupacaoAssociacaoList = class(TPrevalentList)
  private
    function Get_OcupacaoAssociacao(I : integer) : T_OcupacaoAssociacao;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_OcupacaoAssociacao : T_OcupacaoAssociacao);
    procedure Delete(_OcupacaoAssociacao : T_OcupacaoAssociacao);
    function Find(I : Integer) : T_OcupacaoAssociacao;
    function First : T_OcupacaoAssociacao;
    function Last : T_OcupacaoAssociacao;
    function Near(I : Integer) : T_OcupacaoAssociacao;
    function Next(var _OcupacaoAssociacao : T_OcupacaoAssociacao) : boolean;
    function Prior(var _OcupacaoAssociacao : T_OcupacaoAssociacao) : boolean;
    property _OcupacaoAssociacao[I : integer] : T_OcupacaoAssociacao read Get_OcupacaoAssociacao; default;
  end;

  T_OcupacaoListaAssociacoesAssociation = class(TAssociation)
  private
    function Get_OcupacaoAssociacao(I : integer) : T_OcupacaoAssociacao;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_OcupacaoAssociacao : T_OcupacaoAssociacao);
    procedure Delete(_OcupacaoAssociacao : T_OcupacaoAssociacao);
    function Find(I : Integer) : T_OcupacaoAssociacao;
    function First : T_OcupacaoAssociacao;
    function Last : T_OcupacaoAssociacao;
    function Near(I : Integer) : T_OcupacaoAssociacao;
    function Next(var _OcupacaoAssociacao : T_OcupacaoAssociacao) : boolean;
    function Prior(var _OcupacaoAssociacao : T_OcupacaoAssociacao) : boolean;
    property _OcupacaoAssociacao[I : integer] : T_OcupacaoAssociacao read Get_OcupacaoAssociacao; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Package = class(TPrevalent)
  private
    _Nome : String;
    _Ocupacoes : T_PackageOcupacoesAssociation;
    _Classes : T_PackageClassesAssociation; 
    _PermissaoPackage : T_PackagePermissaoPackageAssociation; 
    function  GetNome : String;
    procedure SetNome(Value : String);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Nome : String read GetNome write SetNome;
    property Ocupacoes : T_PackageOcupacoesAssociation read _Ocupacoes write _Ocupacoes;
    property Classes : T_PackageClassesAssociation read _Classes write _Classes;
    property PermissaoPackage : T_PackagePermissaoPackageAssociation read _PermissaoPackage write _PermissaoPackage;
    function PorNome : String; 
  end;

  T_PackageList = class(TPrevalentList)
  private
    function Get_Package(I : integer) : T_Package;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Package : T_Package);
    procedure Delete(_Package : T_Package);
    function Find(I : Integer) : T_Package;
    function First : T_Package;
    function Last : T_Package;
    function Near(I : Integer) : T_Package;
    function Next(var _Package : T_Package) : boolean;
    function Prior(var _Package : T_Package) : boolean;
    property _Package[I : integer] : T_Package read Get_Package; default;
  end;

  T_PackagePorNomeList = class(TPrevalentList)
  private
    function Get_Package(I : integer) : T_Package;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Package : T_Package);
    procedure Delete(_Package : T_Package);
    function Find(S : String) : T_Package;
    function First : T_Package;
    function Last : T_Package;
    function Near(S : String) : T_Package;
    function Next(var _Package : T_Package) : boolean;
    function Prior(var _Package : T_Package) : boolean;
    property _Package[I : integer] : T_Package read Get_Package; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_PermissaoClasse = class(TPrevalent)
  private
    _Permissoes : T_ClassPermissionSet; 
    _Classe : T_Classe; 
    _PermissaoPackage : T_PermissaoPackage; 
    function  GetPermissoes : T_ClassPermissionSet;
    procedure SetPermissoes(Value : T_ClassPermissionSet);
    function  GetClasse : T_Classe;
    procedure SetClasse(Value : T_Classe);
    function  GetPermissaoPackage : T_PermissaoPackage;
    procedure SetPermissaoPackage(Value : T_PermissaoPackage);
  published
    property Permissoes : T_ClassPermissionSet read GetPermissoes write SetPermissoes;
    function GetIdentification : string; override;
    property Classe : T_Classe read GetClasse write SetClasse;
    property PermissaoPackage : T_PermissaoPackage read GetPermissaoPackage write SetPermissaoPackage;
  end;

  T_PermissaoClasseList = class(TPrevalentList)
  private
    function Get_PermissaoClasse(I : integer) : T_PermissaoClasse;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoClasse : T_PermissaoClasse);
    procedure Delete(_PermissaoClasse : T_PermissaoClasse);
    function Find(I : Integer) : T_PermissaoClasse;
    function First : T_PermissaoClasse;
    function Last : T_PermissaoClasse;
    function Near(I : Integer) : T_PermissaoClasse;
    function Next(var _PermissaoClasse : T_PermissaoClasse) : boolean;
    function Prior(var _PermissaoClasse : T_PermissaoClasse) : boolean;
    property _PermissaoClasse[I : integer] : T_PermissaoClasse read Get_PermissaoClasse; default;
  end;

  T_ClassePermissoesClassesAssociation = class(TAssociation)
  private
    function Get_PermissaoClasse(I : integer) : T_PermissaoClasse;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoClasse : T_PermissaoClasse);
    procedure Delete(_PermissaoClasse : T_PermissaoClasse);
    function Find(I : Integer) : T_PermissaoClasse;
    function First : T_PermissaoClasse;
    function Last : T_PermissaoClasse;
    function Near(I : Integer) : T_PermissaoClasse;
    function Next(var _PermissaoClasse : T_PermissaoClasse) : boolean;
    function Prior(var _PermissaoClasse : T_PermissaoClasse) : boolean;
    property _PermissaoClasse[I : integer] : T_PermissaoClasse read Get_PermissaoClasse; default;
  end;

  T_PermissaoPackagePermissoesClassesAssociation = class(TAssociation)
  private
    function Get_PermissaoClasse(I : integer) : T_PermissaoClasse;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoClasse : T_PermissaoClasse);
    procedure Delete(_PermissaoClasse : T_PermissaoClasse);
    function Find(I : Integer) : T_PermissaoClasse;
    function First : T_PermissaoClasse;
    function Last : T_PermissaoClasse;
    function Near(I : Integer) : T_PermissaoClasse;
    function Next(var _PermissaoClasse : T_PermissaoClasse) : boolean;
    function Prior(var _PermissaoClasse : T_PermissaoClasse) : boolean;
    property _PermissaoClasse[I : integer] : T_PermissaoClasse read Get_PermissaoClasse; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_PermissaoMetodo = class(TPrevalent)
  private
    _Permissao : Boolean; 
    _Metodo : T_Metodo; 
    _PermissaoPackage : T_PermissaoPackage; 
    function  GetPermissao : Boolean;
    procedure SetPermissao(Value : Boolean);
    function  GetMetodo : T_Metodo;
    procedure SetMetodo(Value : T_Metodo);
    function  GetPermissaoPackage : T_PermissaoPackage;
    procedure SetPermissaoPackage(Value : T_PermissaoPackage);
  protected
    procedure New; override;
  published
    property Permissao : Boolean read GetPermissao write SetPermissao;
    function GetIdentification : string; override;
    property Metodo : T_Metodo read GetMetodo write SetMetodo;
    property PermissaoPackage : T_PermissaoPackage read GetPermissaoPackage write SetPermissaoPackage;
  end;

  T_PermissaoMetodoList = class(TPrevalentList)
  private
    function Get_PermissaoMetodo(I : integer) : T_PermissaoMetodo;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoMetodo : T_PermissaoMetodo);
    procedure Delete(_PermissaoMetodo : T_PermissaoMetodo);
    function Find(I : Integer) : T_PermissaoMetodo;
    function First : T_PermissaoMetodo;
    function Last : T_PermissaoMetodo;
    function Near(I : Integer) : T_PermissaoMetodo;
    function Next(var _PermissaoMetodo : T_PermissaoMetodo) : boolean;
    function Prior(var _PermissaoMetodo : T_PermissaoMetodo) : boolean;
    property _PermissaoMetodo[I : integer] : T_PermissaoMetodo read Get_PermissaoMetodo; default;
  end;

  T_MetodoPermissoesMetodosAssociation = class(TAssociation)
  private
    function Get_PermissaoMetodo(I : integer) : T_PermissaoMetodo;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoMetodo : T_PermissaoMetodo);
    procedure Delete(_PermissaoMetodo : T_PermissaoMetodo);
    function Find(I : Integer) : T_PermissaoMetodo;
    function First : T_PermissaoMetodo;
    function Last : T_PermissaoMetodo;
    function Near(I : Integer) : T_PermissaoMetodo;
    function Next(var _PermissaoMetodo : T_PermissaoMetodo) : boolean;
    function Prior(var _PermissaoMetodo : T_PermissaoMetodo) : boolean;
    property _PermissaoMetodo[I : integer] : T_PermissaoMetodo read Get_PermissaoMetodo; default;
  end;

  T_PermissaoPackagePermissoesMetodosAssociation = class(TAssociation)
  private
    function Get_PermissaoMetodo(I : integer) : T_PermissaoMetodo;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoMetodo : T_PermissaoMetodo);
    procedure Delete(_PermissaoMetodo : T_PermissaoMetodo);
    function Find(I : Integer) : T_PermissaoMetodo;
    function First : T_PermissaoMetodo;
    function Last : T_PermissaoMetodo;
    function Near(I : Integer) : T_PermissaoMetodo;
    function Next(var _PermissaoMetodo : T_PermissaoMetodo) : boolean;
    function Prior(var _PermissaoMetodo : T_PermissaoMetodo) : boolean;
    property _PermissaoMetodo[I : integer] : T_PermissaoMetodo read Get_PermissaoMetodo; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_PermissaoPackage = class(TPrevalent)
  private
    _Ativa : Boolean;
    _PermissaoClasse : T_ClassPermissionSet; 
    _PermissaoPropriedade : T_PropPermission; 
    _PermissaoMetodo : Boolean; 
    _PermissoesClasses : T_PermissaoPackagePermissoesClassesAssociation; 
    _PermissoesMetodos : T_PermissaoPackagePermissoesMetodosAssociation; 
    _Package : T_Package; 
    _PermissoesViews : T_PermissaoPackagePermissoesViewsAssociation; 
    _PermissoesPropriedades : T_PermissaoPackagePermissoesPropriedadesAssociation; 
    _Grupo : T_Grupo;
    function  GetAtiva : Boolean;
    procedure SetAtiva(Value : Boolean);
    function  GetPermissaoClasse : T_ClassPermissionSet;
    procedure SetPermissaoClasse(Value : T_ClassPermissionSet);
    function  GetPermissaoPropriedade : T_PropPermission;
    procedure SetPermissaoPropriedade(Value : T_PropPermission);
    function  GetPermissaoMetodo : Boolean;
    procedure SetPermissaoMetodo(Value : Boolean);
    function  GetPackage : T_Package;
    procedure SetPackage(Value : T_Package);
    function  GetGrupo : T_Grupo;
    procedure SetGrupo(Value : T_Grupo);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Ativa : Boolean read GetAtiva write SetAtiva;
    property PermissaoClasse : T_ClassPermissionSet read GetPermissaoClasse write SetPermissaoClasse;
    property PermissaoPropriedade : T_PropPermission read GetPermissaoPropriedade write SetPermissaoPropriedade;
    property PermissaoMetodo : Boolean read GetPermissaoMetodo write SetPermissaoMetodo;
    function GetIdentification : string; override;
    property PermissoesClasses : T_PermissaoPackagePermissoesClassesAssociation read _PermissoesClasses write _PermissoesClasses;
    property PermissoesMetodos : T_PermissaoPackagePermissoesMetodosAssociation read _PermissoesMetodos write _PermissoesMetodos;
    property Package : T_Package read GetPackage write SetPackage;
    property PermissoesViews : T_PermissaoPackagePermissoesViewsAssociation read _PermissoesViews write _PermissoesViews;
    property PermissoesPropriedades : T_PermissaoPackagePermissoesPropriedadesAssociation read _PermissoesPropriedades write _PermissoesPropriedades;
    property Grupo : T_Grupo read GetGrupo write SetGrupo;
  end;

  T_PermissaoPackageList = class(TPrevalentList)
  private
    function Get_PermissaoPackage(I : integer) : T_PermissaoPackage;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoPackage : T_PermissaoPackage);
    procedure Delete(_PermissaoPackage : T_PermissaoPackage);
    function Find(I : Integer) : T_PermissaoPackage;
    function First : T_PermissaoPackage;
    function Last : T_PermissaoPackage;
    function Near(I : Integer) : T_PermissaoPackage;
    function Next(var _PermissaoPackage : T_PermissaoPackage) : boolean;
    function Prior(var _PermissaoPackage : T_PermissaoPackage) : boolean;
    property _PermissaoPackage[I : integer] : T_PermissaoPackage read Get_PermissaoPackage; default;
  end;

  T_PackagePermissaoPackageAssociation = class(TAssociation)
  private
    function Get_PermissaoPackage(I : integer) : T_PermissaoPackage;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoPackage : T_PermissaoPackage);
    procedure Delete(_PermissaoPackage : T_PermissaoPackage);
    function Find(I : Integer) : T_PermissaoPackage;
    function First : T_PermissaoPackage;
    function Last : T_PermissaoPackage;
    function Near(I : Integer) : T_PermissaoPackage;
    function Next(var _PermissaoPackage : T_PermissaoPackage) : boolean;
    function Prior(var _PermissaoPackage : T_PermissaoPackage) : boolean;
    property _PermissaoPackage[I : integer] : T_PermissaoPackage read Get_PermissaoPackage; default;
  end;

  T_GrupoPackagesAssociation = class(TAssociation)
  private
    function Get_PermissaoPackage(I : integer) : T_PermissaoPackage;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoPackage : T_PermissaoPackage);
    procedure Delete(_PermissaoPackage : T_PermissaoPackage);
    function Find(I : Integer) : T_PermissaoPackage;
    function First : T_PermissaoPackage;
    function Last : T_PermissaoPackage;
    function Near(I : Integer) : T_PermissaoPackage;
    function Next(var _PermissaoPackage : T_PermissaoPackage) : boolean;
    function Prior(var _PermissaoPackage : T_PermissaoPackage) : boolean;
    property _PermissaoPackage[I : integer] : T_PermissaoPackage read Get_PermissaoPackage; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_PermissaoPropriedade = class(TPrevalent)
  private
    _Permissao : T_PropPermission; 
    _PermissaoPackage : T_PermissaoPackage; 
    _Propriedade : T_Propriedade; 
    function  GetPermissao : T_PropPermission;
    procedure SetPermissao(Value : T_PropPermission);
    function  GetPermissaoPackage : T_PermissaoPackage;
    procedure SetPermissaoPackage(Value : T_PermissaoPackage);
    function  GetPropriedade : T_Propriedade;
    procedure SetPropriedade(Value : T_Propriedade);
  published
    property Permissao : T_PropPermission read GetPermissao write SetPermissao;
    function GetIdentification : string; override;
    property PermissaoPackage : T_PermissaoPackage read GetPermissaoPackage write SetPermissaoPackage;
    property Propriedade : T_Propriedade read GetPropriedade write SetPropriedade;
  end;

  T_PermissaoPropriedadeList = class(TPrevalentList)
  private
    function Get_PermissaoPropriedade(I : integer) : T_PermissaoPropriedade;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoPropriedade : T_PermissaoPropriedade);
    procedure Delete(_PermissaoPropriedade : T_PermissaoPropriedade);
    function Find(I : Integer) : T_PermissaoPropriedade;
    function First : T_PermissaoPropriedade;
    function Last : T_PermissaoPropriedade;
    function Near(I : Integer) : T_PermissaoPropriedade;
    function Next(var _PermissaoPropriedade : T_PermissaoPropriedade) : boolean;
    function Prior(var _PermissaoPropriedade : T_PermissaoPropriedade) : boolean;
    property _PermissaoPropriedade[I : integer] : T_PermissaoPropriedade read Get_PermissaoPropriedade; default;
  end;

  T_PermissaoPackagePermissoesPropriedadesAssociation = class(TAssociation)
  private
    function Get_PermissaoPropriedade(I : integer) : T_PermissaoPropriedade;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoPropriedade : T_PermissaoPropriedade);
    procedure Delete(_PermissaoPropriedade : T_PermissaoPropriedade);
    function Find(I : Integer) : T_PermissaoPropriedade;
    function First : T_PermissaoPropriedade;
    function Last : T_PermissaoPropriedade;
    function Near(I : Integer) : T_PermissaoPropriedade;
    function Next(var _PermissaoPropriedade : T_PermissaoPropriedade) : boolean;
    function Prior(var _PermissaoPropriedade : T_PermissaoPropriedade) : boolean;
    property _PermissaoPropriedade[I : integer] : T_PermissaoPropriedade read Get_PermissaoPropriedade; default;
  end;

  T_PropriedadePermissoesPropriedadesAssociation = class(TAssociation)
  private
    function Get_PermissaoPropriedade(I : integer) : T_PermissaoPropriedade;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoPropriedade : T_PermissaoPropriedade);
    procedure Delete(_PermissaoPropriedade : T_PermissaoPropriedade);
    function Find(I : Integer) : T_PermissaoPropriedade;
    function First : T_PermissaoPropriedade;
    function Last : T_PermissaoPropriedade;
    function Near(I : Integer) : T_PermissaoPropriedade;
    function Next(var _PermissaoPropriedade : T_PermissaoPropriedade) : boolean;
    function Prior(var _PermissaoPropriedade : T_PermissaoPropriedade) : boolean;
    property _PermissaoPropriedade[I : integer] : T_PermissaoPropriedade read Get_PermissaoPropriedade; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_PermissaoView = class(TPrevalent)
  private
    _Parametro : String; 
    _PermissaoVerdadeiro : T_ViewPermissionSet; 
    _PermissaoFalso : T_ViewPermissionSet;
    _PermissaoPackage : T_PermissaoPackage; 
    _View : T_Metodo; 
    function  GetParametro : String;
    procedure SetParametro(Value : String);
    function  GetPermissaoVerdadeiro : T_ViewPermissionSet;
    procedure SetPermissaoVerdadeiro(Value : T_ViewPermissionSet);
    function  GetPermissaoFalso : T_ViewPermissionSet;
    procedure SetPermissaoFalso(Value : T_ViewPermissionSet);
    function  GetPermissaoPackage : T_PermissaoPackage;
    procedure SetPermissaoPackage(Value : T_PermissaoPackage);
    function  GetView : T_Metodo;
    procedure SetView(Value : T_Metodo);
  published
    property Parametro : String read GetParametro write SetParametro;
    property PermissaoVerdadeiro : T_ViewPermissionSet read GetPermissaoVerdadeiro write SetPermissaoVerdadeiro;
    property PermissaoFalso : T_ViewPermissionSet read GetPermissaoFalso write SetPermissaoFalso;
    property PermissaoPackage : T_PermissaoPackage read GetPermissaoPackage write SetPermissaoPackage;
    property View : T_Metodo read GetView write SetView;
    function AssociationConstraintView : TObjectList; virtual;
  end;

  T_PermissaoViewList = class(TPrevalentList)
  private
    function Get_PermissaoView(I : integer) : T_PermissaoView;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoView : T_PermissaoView);
    procedure Delete(_PermissaoView : T_PermissaoView);
    function Find(I : Integer) : T_PermissaoView;
    function First : T_PermissaoView;
    function Last : T_PermissaoView;
    function Near(I : Integer) : T_PermissaoView;
    function Next(var _PermissaoView : T_PermissaoView) : boolean;
    function Prior(var _PermissaoView : T_PermissaoView) : boolean;
    property _PermissaoView[I : integer] : T_PermissaoView read Get_PermissaoView; default;
  end;

  T_PermissaoPackagePermissoesViewsAssociation = class(TAssociation)
  private
    function Get_PermissaoView(I : integer) : T_PermissaoView;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoView : T_PermissaoView);
    procedure Delete(_PermissaoView : T_PermissaoView);
    function Find(I : Integer) : T_PermissaoView;
    function First : T_PermissaoView;
    function Last : T_PermissaoView;
    function Near(I : Integer) : T_PermissaoView;
    function Next(var _PermissaoView : T_PermissaoView) : boolean;
    function Prior(var _PermissaoView : T_PermissaoView) : boolean;
    property _PermissaoView[I : integer] : T_PermissaoView read Get_PermissaoView; default;
  end;

  T_MetodoPermissoesViewAssociation = class(TAssociation)
  private
    function Get_PermissaoView(I : integer) : T_PermissaoView;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PermissaoView : T_PermissaoView);
    procedure Delete(_PermissaoView : T_PermissaoView);
    function Find(I : Integer) : T_PermissaoView;
    function First : T_PermissaoView;
    function Last : T_PermissaoView;
    function Near(I : Integer) : T_PermissaoView;
    function Next(var _PermissaoView : T_PermissaoView) : boolean;
    function Prior(var _PermissaoView : T_PermissaoView) : boolean;
    property _PermissaoView[I : integer] : T_PermissaoView read Get_PermissaoView; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Propriedade = class(TPrevalent)
  private
    _AliasNome : String; 
    _Nome : String; 
    _Tipo : T_PropriedadeTipo; 
    _Posicao : Integer; 
    _ReadOnly : Boolean; 
    _Classe : T_Classe; 
    _PermissoesPropriedades : T_PropriedadePermissoesPropriedadesAssociation; 
    function  GetAliasNome : String;
    procedure SetAliasNome(Value : String);
    function  GetNome : String;
    procedure SetNome(Value : String);
    function  GetTipo : T_PropriedadeTipo;
    procedure SetTipo(Value : T_PropriedadeTipo);
    function  GetPosicao : Integer;
    procedure SetPosicao(Value : Integer);
    function  GetReadOnly : Boolean;
    procedure SetReadOnly(Value : Boolean);
    function  GetClasse : T_Classe;
    procedure SetClasse(Value : T_Classe);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property AliasNome : String read GetAliasNome write SetAliasNome;
    property Nome : String read GetNome write SetNome;
    property Tipo : T_PropriedadeTipo read GetTipo write SetTipo;
    property Posicao : Integer read GetPosicao write SetPosicao;
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly;
    function GetIdentification : string; override;
    property Classe : T_Classe read GetClasse write SetClasse;
    property PermissoesPropriedades : T_PropriedadePermissoesPropriedadesAssociation read _PermissoesPropriedades write _PermissoesPropriedades;
    function PorNome : String; 
    function PorAlias : String;
  end;

  T_PropriedadeList = class(TPrevalentList)
  private
    function Get_Propriedade(I : integer) : T_Propriedade;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Propriedade : T_Propriedade);
    procedure Delete(_Propriedade : T_Propriedade);
    function Find(I : Integer) : T_Propriedade;
    function First : T_Propriedade;
    function Last : T_Propriedade;
    function Near(I : Integer) : T_Propriedade;
    function Next(var _Propriedade : T_Propriedade) : boolean;
    function Prior(var _Propriedade : T_Propriedade) : boolean;
    property _Propriedade[I : integer] : T_Propriedade read Get_Propriedade; default;
  end;

  T_PropriedadePorNomeList = class(TPrevalentList)
  private
    function Get_Propriedade(I : integer) : T_Propriedade;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Propriedade : T_Propriedade);
    procedure Delete(_Propriedade : T_Propriedade);
    function Find(S : String) : T_Propriedade;
    function First : T_Propriedade;
    function Last : T_Propriedade;
    function Near(S : String) : T_Propriedade;
    function Next(var _Propriedade : T_Propriedade) : boolean;
    function Prior(var _Propriedade : T_Propriedade) : boolean;
    property _Propriedade[I : integer] : T_Propriedade read Get_Propriedade; default;
  end;

  T_PropriedadePorAliasList = class(TPrevalentList)
  private
    function Get_Propriedade(I : integer) : T_Propriedade;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Propriedade : T_Propriedade);
    procedure Delete(_Propriedade : T_Propriedade);
    function Find(S : String) : T_Propriedade;
    function First : T_Propriedade;
    function Last : T_Propriedade;
    function Near(S : String) : T_Propriedade;
    function Next(var _Propriedade : T_Propriedade) : boolean;
    function Prior(var _Propriedade : T_Propriedade) : boolean;
    property _Propriedade[I : integer] : T_Propriedade read Get_Propriedade; default;
  end;

  T_ClassePropriedadesAssociation = class(TAssociation)
  private
    function Get_Propriedade(I : integer) : T_Propriedade;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Propriedade : T_Propriedade);
    procedure Delete(_Propriedade : T_Propriedade);
    function Find(S : String) : T_Propriedade;
    function First : T_Propriedade;
    function Last : T_Propriedade;
    function Near(S : String) : T_Propriedade;
    function Next(var _Propriedade : T_Propriedade) : boolean;
    function Prior(var _Propriedade : T_Propriedade) : boolean;
    property _Propriedade[I : integer] : T_Propriedade read Get_Propriedade; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Run = class(TPrevalent)
  private
    _RunObjectID : Integer; 
    _Parameters : String; // Fornecer os parâmetros para a execução do método, caso existam.
    // Definir se as ocorrências das execuções do método agendado devem ser armazenados.
    // "None" define para não armazenar,
    // "Single" define para armazenar somente a última
    // "All" define para armazenar todas execuções.
    _LogKind : T_RunLogKind;
    _Method : T_Metodo; 
    _Log : T_RunLogAssociation; 
    function  GetRunObjectID : Integer;
    procedure SetRunObjectID(Value : Integer);
    function  GetParameters : String;
    procedure SetParameters(Value : String);
    function  GetLogKind : T_RunLogKind;
    procedure SetLogKind(Value : T_RunLogKind);
    function  GetRunning : Boolean;
    function  GetMethod : T_Metodo;
    procedure SetMethod(Value : T_Metodo);
  protected
    procedure New; override;
    procedure InternalFree; override;
  public
    RunningThread : pointer; 
    function RunObject : TPrevalent; 
  published
    property RunObjectID : Integer read GetRunObjectID write SetRunObjectID;
    property Parameters : String read GetParameters write SetParameters;
    property LogKind : T_RunLogKind read GetLogKind write SetLogKind;
    property Running : Boolean read GetRunning;
    function GetIdentification : string; override;
    property Method : T_Metodo read GetMethod write SetMethod;
    property Log : T_RunLogAssociation read _Log write _Log;
  end;

  T_RunList = class(TPrevalentList)
  public
    class function GetObjectClass : TTransientClass; override;
  end;

  // Pontos de Função: 8.0, Baixo
  // Cyclomatic Complexity: 6, Média: 2.0, Baixo
  T_Sequence = class(TPrevalent)
  private
    _Name : String; 
    _InternalValue : Integer; 
    function  GetName : String;
    procedure SetName(Value : String);
    function  GetInternalValue : Integer;
    procedure SetInternalValue(Value : Integer);
  public
    class function Next(pName : String; pIncrement : Integer = 1) : integer; // Cyclomatic Complexity: 2, Baixo
    class function GetValue(pName : String) : integer; // Cyclomatic Complexity: 2, Baixo
    class procedure SetValue(pName : String; pValue : Integer); // Cyclomatic Complexity: 2, Baixo
  published
    property Name : String read GetName write SetName;
    property InternalValue : Integer read GetInternalValue write SetInternalValue;
    function ByName : String; 
  end;

  T_SequenceList = class(TPrevalentList)
  private
    function Get_Sequence(I : integer) : T_Sequence;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Sequence : T_Sequence);
    procedure Delete(_Sequence : T_Sequence);
    function Find(I : Integer) : T_Sequence;
    function First : T_Sequence;
    function Last : T_Sequence;
    function Near(I : Integer) : T_Sequence;
    function Next(var _Sequence : T_Sequence) : boolean;
    function Prior(var _Sequence : T_Sequence) : boolean;
    property _Sequence[I : integer] : T_Sequence read Get_Sequence; default;
  end;

  T_SequenceByNameList = class(TPrevalentList)
  private
    function Get_Sequence(I : integer) : T_Sequence;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Sequence : T_Sequence);
    procedure Delete(_Sequence : T_Sequence);
    function Find(S : String) : T_Sequence;
    function First : T_Sequence;
    function Last : T_Sequence;
    function Near(S : String) : T_Sequence;
    function Next(var _Sequence : T_Sequence) : boolean;
    function Prior(var _Sequence : T_Sequence) : boolean;
    property _Sequence[I : integer] : T_Sequence read Get_Sequence; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Servico = class(TPrevalent)
  private
    _Nome : String;
    _Sessao : T_Sessao; 
    function  GetNome : String;
    procedure SetNome(Value : String);
    function  GetSessao : T_Sessao;
    procedure SetSessao(Value : T_Sessao);
  published
    property Nome : String read GetNome write SetNome;
    property Sessao : T_Sessao read GetSessao write SetSessao;
    function PorNome : String; 
  end;

  T_ServicoList = class(TPrevalentList)
  private
    function Get_Servico(I : integer) : T_Servico;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Servico : T_Servico);
    procedure Delete(_Servico : T_Servico);
    function Find(I : Integer) : T_Servico;
    function First : T_Servico;
    function Last : T_Servico;
    function Near(I : Integer) : T_Servico;
    function Next(var _Servico : T_Servico) : boolean;
    function Prior(var _Servico : T_Servico) : boolean;
    property _Servico[I : integer] : T_Servico read Get_Servico; default;
  end;

  T_ServicoPorNomeList = class(TPrevalentList)
  private
    function Get_Servico(I : integer) : T_Servico;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Servico : T_Servico);
    procedure Delete(_Servico : T_Servico);
    function Find(S : String) : T_Servico;
    function First : T_Servico;
    function Last : T_Servico;
    function Near(S : String) : T_Servico;
    function Next(var _Servico : T_Servico) : boolean;
    function Prior(var _Servico : T_Servico) : boolean;
    property _Servico[I : integer] : T_Servico read Get_Servico; default;
  end;

  T_SessaoServicosAssociation = class(TAssociation)
  private
    function Get_Servico(I : integer) : T_Servico;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Servico : T_Servico);
    procedure Delete(_Servico : T_Servico);
    function Find(S : String) : T_Servico;
    function First : T_Servico;
    function Last : T_Servico;
    function Near(S : String) : T_Servico;
    function Next(var _Servico : T_Servico) : boolean;
    function Prior(var _Servico : T_Servico) : boolean;
    property _Servico[I : integer] : T_Servico read Get_Servico; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_Sessao = class(TPrevalent)
  private
    _Criterios : T_SessaoCriteriosAssociation;
    _Servicos : T_SessaoServicosAssociation; 
    function  GetUsuario : String;
    function  GetLogonDateTime : DateTime;
    function  GetEstacao : String;
    function  GetMetodoAtivo : String;
  protected
    procedure New; override;
    procedure InternalFree; override;
  public
    PThread : TepThread;
  published
    property Usuario : String read GetUsuario;
    property LogonDateTime : DateTime read GetLogonDateTime;
    property Estacao : String read GetEstacao;
    property MetodoAtivo : String read GetMetodoAtivo;
    function GetIdentification : string; override;
    property Criterios : T_SessaoCriteriosAssociation read _Criterios write _Criterios;
    property Servicos : T_SessaoServicosAssociation read _Servicos write _Servicos;
    function PorThread : Int64; 
    function PorUsuario : String; 
    procedure TerminaSessao; 
  end;

  T_SessaoList = class(TPrevalentList)
  private
    function Get_Sessao(I : integer) : T_Sessao;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Sessao : T_Sessao);
    procedure Delete(_Sessao : T_Sessao);
    function Find(I : Integer) : T_Sessao;
    function First : T_Sessao;
    function Last : T_Sessao;
    function Near(I : Integer) : T_Sessao;
    function Next(var _Sessao : T_Sessao) : boolean;
    function Prior(var _Sessao : T_Sessao) : boolean;
    property _Sessao[I : integer] : T_Sessao read Get_Sessao; default;
  end;

  T_SessaoPorThreadList = class(TPrevalentList)
  private
    function Get_Sessao(I : integer) : T_Sessao;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Sessao : T_Sessao);
    procedure Delete(_Sessao : T_Sessao);
    function Find(I : Int64) : T_Sessao;
    function First : T_Sessao;
    function Last : T_Sessao;
    function Near(I : Int64) : T_Sessao;
    function Next(var _Sessao : T_Sessao) : boolean;
    function Prior(var _Sessao : T_Sessao) : boolean;
    property _Sessao[I : integer] : T_Sessao read Get_Sessao; default;
  end;

  T_SessaoPorUsuarioList = class(TPrevalentList)
  private
    function Get_Sessao(I : integer) : T_Sessao;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Sessao : T_Sessao);
    procedure Delete(_Sessao : T_Sessao);
    function Find(S : String) : T_Sessao;
    function First : T_Sessao;
    function Last : T_Sessao;
    function Near(S : String) : T_Sessao;
    function Next(var _Sessao : T_Sessao) : boolean;
    function Prior(var _Sessao : T_Sessao) : boolean;
    property _Sessao[I : integer] : T_Sessao read Get_Sessao; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_UsuarioBloqueado = class(TPrevalent)
  private
    _Nome : String; 
    _Data : DateTime; 
    _Motivo : String;
    _Dominio : T_Dominio; 
    function  GetIdentificador : String;
    function  GetNome : String;
    procedure SetNome(Value : String);
    function  GetData : DateTime;
    procedure SetData(Value : DateTime);
    function  GetMotivo : String;
    procedure SetMotivo(Value : String);
    function  GetAdministrador : String;
    function  GetDominio : T_Dominio;
    procedure SetDominio(Value : T_Dominio);
  protected
    procedure New; override;
  published
    property Identificador : String read GetIdentificador;
    property Nome : String read GetNome write SetNome;
    property Data : DateTime read GetData write SetData;
    property Motivo : String read GetMotivo write SetMotivo;
    property Administrador : String read GetAdministrador;
    property Dominio : T_Dominio read GetDominio write SetDominio;
    function PorIdentificador : String; 
  end;

  T_UsuarioBloqueadoList = class(TPrevalentList)
  private
    function Get_UsuarioBloqueado(I : integer) : T_UsuarioBloqueado;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_UsuarioBloqueado : T_UsuarioBloqueado);
    procedure Delete(_UsuarioBloqueado : T_UsuarioBloqueado);
    function Find(I : Integer) : T_UsuarioBloqueado;
    function First : T_UsuarioBloqueado;
    function Last : T_UsuarioBloqueado;
    function Near(I : Integer) : T_UsuarioBloqueado;
    function Next(var _UsuarioBloqueado : T_UsuarioBloqueado) : boolean;
    function Prior(var _UsuarioBloqueado : T_UsuarioBloqueado) : boolean;
    property _UsuarioBloqueado[I : integer] : T_UsuarioBloqueado read Get_UsuarioBloqueado; default;
  end;

  T_UsuarioBloqueadoPorIdentificadorList = class(TPrevalentList)
  private
    function Get_UsuarioBloqueado(I : integer) : T_UsuarioBloqueado;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_UsuarioBloqueado : T_UsuarioBloqueado);
    procedure Delete(_UsuarioBloqueado : T_UsuarioBloqueado);
    function Find(S : String) : T_UsuarioBloqueado;
    function First : T_UsuarioBloqueado;
    function Last : T_UsuarioBloqueado;
    function Near(S : String) : T_UsuarioBloqueado;
    function Next(var _UsuarioBloqueado : T_UsuarioBloqueado) : boolean;
    function Prior(var _UsuarioBloqueado : T_UsuarioBloqueado) : boolean;
    property _UsuarioBloqueado[I : integer] : T_UsuarioBloqueado read Get_UsuarioBloqueado; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_ExpPropriedade = class(T_ImportaPropriedade)
  private
    _Exporta : T_Exporta; 
    function  GetExporta : T_Exporta;
    procedure SetExporta(Value : T_Exporta);
  published
    property Exporta : T_Exporta read GetExporta write SetExporta;
  end;

  T_ExpPropriedadeList = class(TPrevalentList)
  private
    function Get_ExpPropriedade(I : integer) : T_ExpPropriedade;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_ExpPropriedade : T_ExpPropriedade);
    procedure Delete(_ExpPropriedade : T_ExpPropriedade);
    function Find(I : Integer) : T_ExpPropriedade;
    function First : T_ExpPropriedade;
    function Last : T_ExpPropriedade;
    function Near(I : Integer) : T_ExpPropriedade;
    function Next(var _ExpPropriedade : T_ExpPropriedade) : boolean;
    function Prior(var _ExpPropriedade : T_ExpPropriedade) : boolean;
    property _ExpPropriedade[I : integer] : T_ExpPropriedade read Get_ExpPropriedade; default;
  end;

  T_ExpPropriedadePorElementoList = class(TPrevalentList)
  private
    function Get_ExpPropriedade(I : integer) : T_ExpPropriedade;
  protected
    class function GetKeyCode : pointer; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_ExpPropriedade : T_ExpPropriedade);
    procedure Delete(_ExpPropriedade : T_ExpPropriedade);
    function Find(I : Integer) : T_ExpPropriedade;
    function First : T_ExpPropriedade;
    function Last : T_ExpPropriedade;
    function Near(I : Integer) : T_ExpPropriedade;
    function Next(var _ExpPropriedade : T_ExpPropriedade) : boolean;
    function Prior(var _ExpPropriedade : T_ExpPropriedade) : boolean;
    property _ExpPropriedade[I : integer] : T_ExpPropriedade read Get_ExpPropriedade; default;
  end;

  T_ExpPropriedadePorPropNomeList = class(TPrevalentList)
  private
    function Get_ExpPropriedade(I : integer) : T_ExpPropriedade;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_ExpPropriedade : T_ExpPropriedade);
    procedure Delete(_ExpPropriedade : T_ExpPropriedade);
    function Find(S : String) : T_ExpPropriedade;
    function First : T_ExpPropriedade;
    function Last : T_ExpPropriedade;
    function Near(S : String) : T_ExpPropriedade;
    function Next(var _ExpPropriedade : T_ExpPropriedade) : boolean;
    function Prior(var _ExpPropriedade : T_ExpPropriedade) : boolean;
    property _ExpPropriedade[I : integer] : T_ExpPropriedade read Get_ExpPropriedade; default;
  end;

  T_ExportaPropriedadesAssociation = class(TAssociation)
  private
    function Get_ExpPropriedade(I : integer) : T_ExpPropriedade;
  protected
    class function GetKeyCode : pointer; override;
  public
    class function MinConstraint : integer; override;
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_ExpPropriedade : T_ExpPropriedade);
    procedure Delete(_ExpPropriedade : T_ExpPropriedade);
    function Find(I : Integer) : T_ExpPropriedade;
    function First : T_ExpPropriedade;
    function Last : T_ExpPropriedade;
    function Near(I : Integer) : T_ExpPropriedade;
    function Next(var _ExpPropriedade : T_ExpPropriedade) : boolean;
    function Prior(var _ExpPropriedade : T_ExpPropriedade) : boolean;
    property _ExpPropriedade[I : integer] : T_ExpPropriedade read Get_ExpPropriedade; default;
  end;

  // Pontos de Função: 8.0, Baixo
  // Cyclomatic Complexity: 4, Média: 2.0, Baixo
  T_Exporta = class(T_ExportaImporta)
  private
    _Propriedades : T_ExportaPropriedadesAssociation; 
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Propriedades : T_ExportaPropriedadesAssociation read _Propriedades write _Propriedades;
    class procedure GerarModelo(Modelo : String); // Cyclomatic Complexity: 2, Baixo
    class procedure Exportar(Modelo : T_Exporta);
    class procedure CustomizarModelo; // Cyclomatic Complexity: 2, Baixo
  end;

  T_ExportaList = class(TPrevalentList)
  private
    function Get_Exporta(I : integer) : T_Exporta;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Exporta : T_Exporta);
    procedure Delete(_Exporta : T_Exporta);
    function Find(I : Integer) : T_Exporta;
    function First : T_Exporta;
    function Last : T_Exporta;
    function Near(I : Integer) : T_Exporta;
    function Next(var _Exporta : T_Exporta) : boolean;
    function Prior(var _Exporta : T_Exporta) : boolean;
    property _Exporta[I : integer] : T_Exporta read Get_Exporta; default;
  end;

  T_ExportaPorNomeList = class(TPrevalentList)
  private
    function Get_Exporta(I : integer) : T_Exporta;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Exporta : T_Exporta);
    procedure Delete(_Exporta : T_Exporta);
    function Find(s : string) : T_Exporta;
    function First : T_Exporta;
    function Last : T_Exporta;
    function Near(s : string) : T_Exporta;
    function Next(var _Exporta : T_Exporta) : boolean;
    function Prior(var _Exporta : T_Exporta) : boolean;
    property _Exporta[I : integer] : T_Exporta read Get_Exporta; default;
  end;

  // Esta Classe permite a definição de importações de arquivos texto para dentro da Prevalência.
  // As importações são tratadas de duas formas:
  // 1. arquivos com delimitadores de campo;
  // 2. arquivos com tratamento posicional.
  // A forma de utilização da importação segue os seguintes passos:
  // 1.	Seleciona-se uma das classes inferiores "Importação com Delimitador" ou "Importação Posicional", conforme o arquivo a ser tratado;
  // 2.	Executa-se o método de classe "Gerar Modelo", onde será gerado um "Modelo de Importação", baseado na classe solicitada. Maiores informações consultar a documentação do Método;
  // 3.	Ajusta-se o "Modelo de Importação" gerado, personalizando-o ao arquivo;
  // 4.	Neste momento o arquivo já estará pronto para ser importado, devendo ser invocada a importação através de "Tarefas", selecionando "Importar Arquivo do Tipo Posicional" ou "Importar Arquivo do Tipo Delimitado" e escolhendo o "Modelo de Importação" previamente gerado.
  // Pontos de Função: 8.0, Baixo
  // Cyclomatic Complexity: 8, Média: 4.0, Baixo
  T_Importa = class(T_ExportaImporta)
  private
    _Lista : String; 
    _SeExiste : T_ImportaSeExiste; 
    _BalancedLine : Boolean;
    _NumeroElementoChave : T_ImportaNumeroElementoChave; 
    _SaltarFim : Integer; 
    _SaltarInicio : Integer; 
    function  GetLista : String;
    procedure SetLista(Value : String);
    function  GetSeExiste : T_ImportaSeExiste;
    procedure SetSeExiste(Value : T_ImportaSeExiste);
    function  GetBalancedLine : Boolean;
    procedure SetBalancedLine(Value : Boolean);
    function  GetNumeroElementoChave : T_ImportaNumeroElementoChave;
    procedure SetNumeroElementoChave(Value : T_ImportaNumeroElementoChave);
    function  GetSaltarFim : Integer;
    procedure SetSaltarFim(Value : Integer);
    function  GetSaltarInicio : Integer;
    procedure SetSaltarInicio(Value : Integer);
  protected
    procedure New; override;
  published
    property Lista : String read GetLista write SetLista;
    property SeExiste : T_ImportaSeExiste read GetSeExiste write SetSeExiste;
    property BalancedLine : Boolean read GetBalancedLine write SetBalancedLine;
    property NumeroElementoChave : T_ImportaNumeroElementoChave read GetNumeroElementoChave write SetNumeroElementoChave;
    property SaltarFim : Integer read GetSaltarFim write SetSaltarFim;
    property SaltarInicio : Integer read GetSaltarInicio write SetSaltarInicio;
    class procedure GerarModelo(Modelo : String; ImportacaoDelimitada : Boolean); // Cyclomatic Complexity: 4, Baixo
    class procedure Importar(Modelo : T_Importa); 
    class procedure CustomizarModelo(ImportacaoDelimitada : Boolean); // Cyclomatic Complexity: 4, Baixo
  end;

  T_ImportaList = class(TPrevalentList)
  public
    class function GetObjectClass : TTransientClass; override;
  end;

  T_ImportaPorNomeList = class(TPrevalentList)
  public
  end;

  // Pontos de Função: 8.0, Baixo
  T_OcupacaoClasse = class(T_Ocupacao)
  private
    _Ocupacoes : T_OcupacaoClasseOcupacoesAssociation; 
    _Package : T_OcupacaoPackage; 
    _Classe : T_Classe; 
    function  GetNome : String;
    function  GetPackage : T_OcupacaoPackage;
    procedure SetPackage(Value : T_OcupacaoPackage);
    function  GetClasse : T_Classe;
    procedure SetClasse(Value : T_Classe);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Nome : String read GetNome;
    property Ocupacoes : T_OcupacaoClasseOcupacoesAssociation read _Ocupacoes write _Ocupacoes;
    property Package : T_OcupacaoPackage read GetPackage write SetPackage;
    property Classe : T_Classe read GetClasse write SetClasse;
    function PorNome : String; 
  end;

  T_OcupacaoClasseList = class(TPrevalentList)
  private
    function Get_OcupacaoClasse(I : integer) : T_OcupacaoClasse;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_OcupacaoClasse : T_OcupacaoClasse);
    procedure Delete(_OcupacaoClasse : T_OcupacaoClasse);
    function Find(I : Integer) : T_OcupacaoClasse;
    function First : T_OcupacaoClasse;
    function Last : T_OcupacaoClasse;
    function Near(I : Integer) : T_OcupacaoClasse;
    function Next(var _OcupacaoClasse : T_OcupacaoClasse) : boolean;
    function Prior(var _OcupacaoClasse : T_OcupacaoClasse) : boolean;
    property _OcupacaoClasse[I : integer] : T_OcupacaoClasse read Get_OcupacaoClasse; default;
  end;

  T_OcupacaoClassePorNomeList = class(TPrevalentList)
  private
    function Get_OcupacaoClasse(I : integer) : T_OcupacaoClasse;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_OcupacaoClasse : T_OcupacaoClasse);
    procedure Delete(_OcupacaoClasse : T_OcupacaoClasse);
    function Find(S : String) : T_OcupacaoClasse;
    function First : T_OcupacaoClasse;
    function Last : T_OcupacaoClasse;
    function Near(S : String) : T_OcupacaoClasse;
    function Next(var _OcupacaoClasse : T_OcupacaoClasse) : boolean;
    function Prior(var _OcupacaoClasse : T_OcupacaoClasse) : boolean;
    property _OcupacaoClasse[I : integer] : T_OcupacaoClasse read Get_OcupacaoClasse; default;
  end;

  T_OcupacaoPackageClassesAssociation = class(TAssociation)
  private
    function Get_OcupacaoClasse(I : integer) : T_OcupacaoClasse;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_OcupacaoClasse : T_OcupacaoClasse);
    procedure Delete(_OcupacaoClasse : T_OcupacaoClasse);
    function Find(I : Integer) : T_OcupacaoClasse;
    function First : T_OcupacaoClasse;
    function Last : T_OcupacaoClasse;
    function Near(I : Integer) : T_OcupacaoClasse;
    function Next(var _OcupacaoClasse : T_OcupacaoClasse) : boolean;
    function Prior(var _OcupacaoClasse : T_OcupacaoClasse) : boolean;
    property _OcupacaoClasse[I : integer] : T_OcupacaoClasse read Get_OcupacaoClasse; default;
  end;

  T_ClasseOcupacoesAssociation = class(TAssociation)
  private
    function Get_OcupacaoClasse(I : integer) : T_OcupacaoClasse;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_OcupacaoClasse : T_OcupacaoClasse);
    procedure Delete(_OcupacaoClasse : T_OcupacaoClasse);
    function Find(I : Integer) : T_OcupacaoClasse;
    function First : T_OcupacaoClasse;
    function Last : T_OcupacaoClasse;
    function Near(I : Integer) : T_OcupacaoClasse;
    function Next(var _OcupacaoClasse : T_OcupacaoClasse) : boolean;
    function Prior(var _OcupacaoClasse : T_OcupacaoClasse) : boolean;
    property _OcupacaoClasse[I : integer] : T_OcupacaoClasse read Get_OcupacaoClasse; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_OcupacaoLista = class(T_Ocupacao)
  private
    _Indice : Integer; 
    _Associacoes : T_OcupacaoListaAssociacoesAssociation; 
    _Classe : T_OcupacaoClasse; 
    function  GetIndice : Integer;
    procedure SetIndice(Value : Integer);
    function  GetLista : String;
    function  GetCount : Integer;
    function  GetCapacity : Integer;
    function  GetMemoria : Double;
    function  GetClasse : T_OcupacaoClasse;
    procedure SetClasse(Value : T_OcupacaoClasse);
  protected
    procedure New; override;
    procedure InternalFree; override;
  public
    function CalculaOcupacaoMemoriaList : Double; 
  published
    property Indice : Integer read GetIndice write SetIndice;
    property Lista : String read GetLista;
    property Count : Integer read GetCount;
    property Capacity : Integer read GetCapacity;
    property Memoria : Double read GetMemoria;
    property Associacoes : T_OcupacaoListaAssociacoesAssociation read _Associacoes write _Associacoes;
    property Classe : T_OcupacaoClasse read GetClasse write SetClasse;
  end;

  T_OcupacaoListaList = class(TPrevalentList)
  private
    function Get_OcupacaoLista(I : integer) : T_OcupacaoLista;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_OcupacaoLista : T_OcupacaoLista);
    procedure Delete(_OcupacaoLista : T_OcupacaoLista);
    function Find(I : Integer) : T_OcupacaoLista;
    function First : T_OcupacaoLista;
    function Last : T_OcupacaoLista;
    function Near(I : Integer) : T_OcupacaoLista;
    function Next(var _OcupacaoLista : T_OcupacaoLista) : boolean;
    function Prior(var _OcupacaoLista : T_OcupacaoLista) : boolean;
    property _OcupacaoLista[I : integer] : T_OcupacaoLista read Get_OcupacaoLista; default;
  end;

  T_OcupacaoClasseOcupacoesAssociation = class(TAssociation)
  private
    function Get_OcupacaoLista(I : integer) : T_OcupacaoLista;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_OcupacaoLista : T_OcupacaoLista);
    procedure Delete(_OcupacaoLista : T_OcupacaoLista);
    function Find(I : Integer) : T_OcupacaoLista;
    function First : T_OcupacaoLista;
    function Last : T_OcupacaoLista;
    function Near(I : Integer) : T_OcupacaoLista;
    function Next(var _OcupacaoLista : T_OcupacaoLista) : boolean;
    function Prior(var _OcupacaoLista : T_OcupacaoLista) : boolean;
    property _OcupacaoLista[I : integer] : T_OcupacaoLista read Get_OcupacaoLista; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_OcupacaoPackage = class(T_Ocupacao)
  private
    _Classes : T_OcupacaoPackageClassesAssociation; 
    _Package : T_Package;
    function  GetPackage : T_Package;
    procedure SetPackage(Value : T_Package);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Classes : T_OcupacaoPackageClassesAssociation read _Classes write _Classes;
    property Package : T_Package read GetPackage write SetPackage;
    class procedure InicializaOcupacao; 
    function PorNome : String; 
  end;

  T_OcupacaoPackageList = class(TPrevalentList)
  private
    function Get_OcupacaoPackage(I : integer) : T_OcupacaoPackage;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_OcupacaoPackage : T_OcupacaoPackage);
    procedure Delete(_OcupacaoPackage : T_OcupacaoPackage);
    function Find(I : Integer) : T_OcupacaoPackage;
    function First : T_OcupacaoPackage;
    function Last : T_OcupacaoPackage;
    function Near(I : Integer) : T_OcupacaoPackage;
    function Next(var _OcupacaoPackage : T_OcupacaoPackage) : boolean;
    function Prior(var _OcupacaoPackage : T_OcupacaoPackage) : boolean;
    property _OcupacaoPackage[I : integer] : T_OcupacaoPackage read Get_OcupacaoPackage; default;
  end;

  T_OcupacaoPackagePorNomeList = class(TPrevalentList)
  private
    function Get_OcupacaoPackage(I : integer) : T_OcupacaoPackage;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_OcupacaoPackage : T_OcupacaoPackage);
    procedure Delete(_OcupacaoPackage : T_OcupacaoPackage);
    function Find(S : String) : T_OcupacaoPackage;
    function First : T_OcupacaoPackage;
    function Last : T_OcupacaoPackage;
    function Near(S : String) : T_OcupacaoPackage;
    function Next(var _OcupacaoPackage : T_OcupacaoPackage) : boolean;
    function Prior(var _OcupacaoPackage : T_OcupacaoPackage) : boolean;
    property _OcupacaoPackage[I : integer] : T_OcupacaoPackage read Get_OcupacaoPackage; default;
  end;

  T_PackageOcupacoesAssociation = class(TAssociation)
  private
    function Get_OcupacaoPackage(I : integer) : T_OcupacaoPackage;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_OcupacaoPackage : T_OcupacaoPackage);
    procedure Delete(_OcupacaoPackage : T_OcupacaoPackage);
    function Find(I : Integer) : T_OcupacaoPackage;
    function First : T_OcupacaoPackage;
    function Last : T_OcupacaoPackage;
    function Near(I : Integer) : T_OcupacaoPackage;
    function Next(var _OcupacaoPackage : T_OcupacaoPackage) : boolean;
    function Prior(var _OcupacaoPackage : T_OcupacaoPackage) : boolean;
    property _OcupacaoPackage[I : integer] : T_OcupacaoPackage read Get_OcupacaoPackage; default;
  end;

  // Pontos de Função: 8.0, Baixo
  // Cyclomatic Complexity: 19, Média: 3.2, Baixo
  T_Pendency = class(T_Run)
  private
    _DateTime : DateTime; 
    _DelegateUser : String; 
    _AssignedProfile : T_Grupo;
    _Delegate : T_Grupo; 
    _TimeOut : T_PendencyTimeoutTask; 
    function  GetClassAlias : String;
    function  GetDateTime : DateTime;
    procedure SetDateTime(Value : DateTime);
    function  GetDeadLine : DateTime;
    function  GetDelegateUser : String;
    procedure SetDelegateUser(Value : String);
    function  GetMethodAlias : String;
    function  GetObjIdentification : String;
    function  GetWorkFlowAlias : String;
    function  GetAssignedProfile : T_Grupo;
    procedure SetAssignedProfile(Value : T_Grupo);
    function  GetDelegate : T_Grupo;
    procedure SetDelegate(Value : T_Grupo);
    function  GetTimeOut : T_PendencyTimeoutTask;
    procedure SetTimeOut(Value : T_PendencyTimeoutTask);
    function CalcWorkFlowAlias : String; // Cyclomatic Complexity: 2, Baixo
    function GetDelegateProfiles : TObjectList; // Cyclomatic Complexity: 3, Baixo
    function CalcDeadLine : TDateTime; // Cyclomatic Complexity: 2, Baixo
    function InternalCheckDelegateUser : boolean; // Cyclomatic Complexity: 4, Baixo
  protected
    procedure New; override;
  public
    function ValidDelegateUser(aDelegateUser : String) : boolean; // Cyclomatic Complexity: 6, Baixo
    function ValidDelegateProfile(aDelegateProfileName : String) : boolean; 
  published
    property ClassAlias : String read GetClassAlias;
    property DateTime : DateTime read GetDateTime write SetDateTime;
    property DeadLine : DateTime read GetDeadLine;
    property DelegateUser : String read GetDelegateUser write SetDelegateUser;
    property MethodAlias : String read GetMethodAlias;
    property ObjIdentification : String read GetObjIdentification;
    property WorkFlowAlias : String read GetWorkFlowAlias;
    property AssignedProfile : T_Grupo read GetAssignedProfile write SetAssignedProfile;
    property Delegate : T_Grupo read GetDelegate write SetDelegate;
    property TimeOut : T_PendencyTimeoutTask read GetTimeOut write SetTimeOut;
    function ByPriority : Double; 
    function ByRunObject : String; 
    function GetDelegateUsers : String; // Cyclomatic Complexity: 2, Baixo
    function CheckDelegateUser(var Message : String) : Boolean; virtual;
    function EnabledDelegateUser : Boolean; virtual;
    function CheckDelegate(var Message : String) : Boolean; virtual;
    function AssociationConstraintDelegate : TObjectList; virtual;
  end;

  T_PendencyList = class(TPrevalentList)
  private
    function Get_Pendency(I : integer) : T_Pendency;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Pendency : T_Pendency);
    procedure Delete(_Pendency : T_Pendency);
    function Find(I : Integer) : T_Pendency;
    function First : T_Pendency;
    function Last : T_Pendency;
    function Near(I : Integer) : T_Pendency;
    function Next(var _Pendency : T_Pendency) : boolean;
    function Prior(var _Pendency : T_Pendency) : boolean;
    property _Pendency[I : integer] : T_Pendency read Get_Pendency; default;
  end;

  T_PendencyByPriorityList = class(TPrevalentList)
  private
    function Get_Pendency(I : integer) : T_Pendency;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Pendency : T_Pendency);
    procedure Delete(_Pendency : T_Pendency);
    function Find(D : Double) : T_Pendency;
    function First : T_Pendency;
    function Last : T_Pendency;
    function Near(D : Double) : T_Pendency;
    function Next(var _Pendency : T_Pendency) : boolean;
    function Prior(var _Pendency : T_Pendency) : boolean;
    property _Pendency[I : integer] : T_Pendency read Get_Pendency; default;
  end;

  T_PendencyByRunObjectList = class(TPrevalentList)
  private
    function Get_Pendency(I : integer) : T_Pendency;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Pendency : T_Pendency);
    procedure Delete(_Pendency : T_Pendency);
    function Find(S : String) : T_Pendency;
    function First : T_Pendency;
    function Last : T_Pendency;
    function Near(S : String) : T_Pendency;
    function Next(var _Pendency : T_Pendency) : boolean;
    function Prior(var _Pendency : T_Pendency) : boolean;
    property _Pendency[I : integer] : T_Pendency read Get_Pendency; default;
  end;

  T_GrupoAssignedPendenciesAssociation = class(TAssociation)
  private
    function Get_Pendency(I : integer) : T_Pendency;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Pendency : T_Pendency);
    procedure Delete(_Pendency : T_Pendency);
    function Find(I : Integer) : T_Pendency;
    function First : T_Pendency;
    function Last : T_Pendency;
    function Near(I : Integer) : T_Pendency;
    function Next(var _Pendency : T_Pendency) : boolean;
    function Prior(var _Pendency : T_Pendency) : boolean;
    property _Pendency[I : integer] : T_Pendency read Get_Pendency; default;
  end;

  T_GrupoPendenciesAssociation = class(TAssociation)
  private
    function Get_Pendency(I : integer) : T_Pendency;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Pendency : T_Pendency);
    procedure Delete(_Pendency : T_Pendency);
    function Find(I : Integer) : T_Pendency;
    function First : T_Pendency;
    function Last : T_Pendency;
    function Near(I : Integer) : T_Pendency;
    function Next(var _Pendency : T_Pendency) : boolean;
    function Prior(var _Pendency : T_Pendency) : boolean;
    property _Pendency[I : integer] : T_Pendency read Get_Pendency; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_PropriedadeDelimitado = class(T_ImportaPropriedade)
  private
    _ImportaDelimitado : T_ImportaDelimitado; 
    function  GetImportaDelimitado : T_ImportaDelimitado;
    procedure SetImportaDelimitado(Value : T_ImportaDelimitado);
  published
    property ImportaDelimitado : T_ImportaDelimitado read GetImportaDelimitado write SetImportaDelimitado;
  end;

  T_PropriedadeDelimitadoList = class(TPrevalentList)
  private
    function Get_PropriedadeDelimitado(I : integer) : T_PropriedadeDelimitado;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PropriedadeDelimitado : T_PropriedadeDelimitado);
    procedure Delete(_PropriedadeDelimitado : T_PropriedadeDelimitado);
    function Find(I : Integer) : T_PropriedadeDelimitado;
    function First : T_PropriedadeDelimitado;
    function Last : T_PropriedadeDelimitado;
    function Near(I : Integer) : T_PropriedadeDelimitado;
    function Next(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean;
    function Prior(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean;
    property _PropriedadeDelimitado[I : integer] : T_PropriedadeDelimitado read Get_PropriedadeDelimitado; default;
  end;

  T_PropriedadeDelimitadoPorElementoList = class(TPrevalentList)
  private
    function Get_PropriedadeDelimitado(I : integer) : T_PropriedadeDelimitado;
  protected
    class function GetKeyCode : pointer; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PropriedadeDelimitado : T_PropriedadeDelimitado);
    procedure Delete(_PropriedadeDelimitado : T_PropriedadeDelimitado);
    function Find(I : Integer) : T_PropriedadeDelimitado;
    function First : T_PropriedadeDelimitado;
    function Last : T_PropriedadeDelimitado;
    function Near(I : Integer) : T_PropriedadeDelimitado;
    function Next(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean;
    function Prior(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean;
    property _PropriedadeDelimitado[I : integer] : T_PropriedadeDelimitado read Get_PropriedadeDelimitado; default;
  end;

  T_PropriedadeDelimitadoPorPropNomeList = class(TPrevalentList)
  private
    function Get_PropriedadeDelimitado(I : integer) : T_PropriedadeDelimitado;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PropriedadeDelimitado : T_PropriedadeDelimitado);
    procedure Delete(_PropriedadeDelimitado : T_PropriedadeDelimitado);
    function Find(S : String) : T_PropriedadeDelimitado;
    function First : T_PropriedadeDelimitado;
    function Last : T_PropriedadeDelimitado;
    function Near(S : String) : T_PropriedadeDelimitado;
    function Next(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean;
    function Prior(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean;
    property _PropriedadeDelimitado[I : integer] : T_PropriedadeDelimitado read Get_PropriedadeDelimitado; default;
  end;

  T_ImportaDelimitadoPropriedadesAssociation = class(TAssociation)
  private
    function Get_PropriedadeDelimitado(I : integer) : T_PropriedadeDelimitado;
  protected
    class function GetKeyCode : pointer; override;
  public
    class function MinConstraint : integer; override;
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PropriedadeDelimitado : T_PropriedadeDelimitado);
    procedure Delete(_PropriedadeDelimitado : T_PropriedadeDelimitado);
    function Find(I : Integer) : T_PropriedadeDelimitado;
    function First : T_PropriedadeDelimitado;
    function Last : T_PropriedadeDelimitado;
    function Near(I : Integer) : T_PropriedadeDelimitado;
    function Next(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean;
    function Prior(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean;
    property _PropriedadeDelimitado[I : integer] : T_PropriedadeDelimitado read Get_PropriedadeDelimitado; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_PropriedadePosicional = class(T_ImportaPropriedade)
  private
    _Posicao : Integer; 
    _QuantidadePosicoes : Integer; 
    _ImportaPosicional : T_ImportaPosicional; 
    function  GetPosicao : Integer;
    procedure SetPosicao(Value : Integer);
    function  GetQuantidadePosicoes : Integer;
    procedure SetQuantidadePosicoes(Value : Integer);
    function  GetImportaPosicional : T_ImportaPosicional;
    procedure SetImportaPosicional(Value : T_ImportaPosicional);
  published
    property Posicao : Integer read GetPosicao write SetPosicao;
    property QuantidadePosicoes : Integer read GetQuantidadePosicoes write SetQuantidadePosicoes;
    property ImportaPosicional : T_ImportaPosicional read GetImportaPosicional write SetImportaPosicional;
    function CheckPosicao(var Message : String) : Boolean; virtual;
    function CheckQuantidadePosicoes(var Message : String) : Boolean; virtual;
  end;

  T_PropriedadePosicionalList = class(TPrevalentList)
  private
    function Get_PropriedadePosicional(I : integer) : T_PropriedadePosicional;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PropriedadePosicional : T_PropriedadePosicional);
    procedure Delete(_PropriedadePosicional : T_PropriedadePosicional);
    function Find(I : Integer) : T_PropriedadePosicional;
    function First : T_PropriedadePosicional;
    function Last : T_PropriedadePosicional;
    function Near(I : Integer) : T_PropriedadePosicional;
    function Next(var _PropriedadePosicional : T_PropriedadePosicional) : boolean;
    function Prior(var _PropriedadePosicional : T_PropriedadePosicional) : boolean;
    property _PropriedadePosicional[I : integer] : T_PropriedadePosicional read Get_PropriedadePosicional; default;
  end;

  T_PropriedadePosicionalPorElementoList = class(TPrevalentList)
  private
    function Get_PropriedadePosicional(I : integer) : T_PropriedadePosicional;
  protected
    class function GetKeyCode : pointer; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PropriedadePosicional : T_PropriedadePosicional);
    procedure Delete(_PropriedadePosicional : T_PropriedadePosicional);
    function Find(I : Integer) : T_PropriedadePosicional;
    function First : T_PropriedadePosicional;
    function Last : T_PropriedadePosicional;
    function Near(I : Integer) : T_PropriedadePosicional;
    function Next(var _PropriedadePosicional : T_PropriedadePosicional) : boolean;
    function Prior(var _PropriedadePosicional : T_PropriedadePosicional) : boolean;
    property _PropriedadePosicional[I : integer] : T_PropriedadePosicional read Get_PropriedadePosicional; default;
  end;

  T_PropriedadePosicionalPorPropNomeList = class(TPrevalentList)
  private
    function Get_PropriedadePosicional(I : integer) : T_PropriedadePosicional;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PropriedadePosicional : T_PropriedadePosicional);
    procedure Delete(_PropriedadePosicional : T_PropriedadePosicional);
    function Find(S : String) : T_PropriedadePosicional;
    function First : T_PropriedadePosicional;
    function Last : T_PropriedadePosicional;
    function Near(S : String) : T_PropriedadePosicional;
    function Next(var _PropriedadePosicional : T_PropriedadePosicional) : boolean;
    function Prior(var _PropriedadePosicional : T_PropriedadePosicional) : boolean;
    property _PropriedadePosicional[I : integer] : T_PropriedadePosicional read Get_PropriedadePosicional; default;
  end;

  T_ImportaPosicionalPropriedadesAssociation = class(TAssociation)
  private
    function Get_PropriedadePosicional(I : integer) : T_PropriedadePosicional;
  protected
    class function GetKeyCode : pointer; override;
  public
    class function MinConstraint : integer; override;
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PropriedadePosicional : T_PropriedadePosicional);
    procedure Delete(_PropriedadePosicional : T_PropriedadePosicional);
    function Find(I : Integer) : T_PropriedadePosicional;
    function First : T_PropriedadePosicional;
    function Last : T_PropriedadePosicional;
    function Near(I : Integer) : T_PropriedadePosicional;
    function Next(var _PropriedadePosicional : T_PropriedadePosicional) : boolean;
    function Prior(var _PropriedadePosicional : T_PropriedadePosicional) : boolean;
    property _PropriedadePosicional[I : integer] : T_PropriedadePosicional read Get_PropriedadePosicional; default;
  end;

  // Pontos de Função: 8.0, Baixo
  // Cyclomatic Complexity: 14, Média: 7.0, Baixo
  T_Task = class(T_Run)
  private
    _Active : Boolean; 
    _Periodicity : Word; 
    _PeriodicityUnit : T_TaskPeriodicityUnit; 
    _Start : DateTime; // Definir a partir de quando o agendamento do método deve ser iniciado.
    _RunUntil : DateTime; // Definir até quando o agendamento do método deve estar ativo. Caso fornecido, o agendamento não será mais executado após esta data/hora.
    _LastRan : DateTime; 
    function  GetActive : Boolean;
    procedure SetActive(Value : Boolean);
    function  GetPeriodicity : Word;
    procedure SetPeriodicity(Value : Word);
    function  GetPeriodicityUnit : T_TaskPeriodicityUnit;
    procedure SetPeriodicityUnit(Value : T_TaskPeriodicityUnit);
    function  GetStart : DateTime;
    procedure SetStart(Value : DateTime);
    function  GetRunUntil : DateTime;
    procedure SetRunUntil(Value : DateTime);
    function  GetLastRan : DateTime;
    procedure SetLastRan(Value : DateTime);
    function  GetNextStart : DateTime;
    function CalcNextStart : TDateTime; // Cyclomatic Complexity: 10, Baixo
  protected
    procedure New; override;
  published
    property Active : Boolean read GetActive write SetActive;
    property Periodicity : Word read GetPeriodicity write SetPeriodicity;
    property PeriodicityUnit : T_TaskPeriodicityUnit read GetPeriodicityUnit write SetPeriodicityUnit;
    property Start : DateTime read GetStart write SetStart;
    property RunUntil : DateTime read GetRunUntil write SetRunUntil;
    property LastRan : DateTime read GetLastRan write SetLastRan;
    property NextStart : DateTime read GetNextStart;
    function ByNextStart : DateTime; 
    class procedure Scheduler; 
    class procedure NewTask; // Cyclomatic Complexity: 4, Baixo
  end;

  T_TaskList = class(TPrevalentList)
  private
    function Get_Task(I : integer) : T_Task;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Task : T_Task);
    procedure Delete(_Task : T_Task);
    function Find(I : Integer) : T_Task;
    function First : T_Task;
    function Last : T_Task;
    function Near(I : Integer) : T_Task;
    function Next(var _Task : T_Task) : boolean;
    function Prior(var _Task : T_Task) : boolean;
    property _Task[I : integer] : T_Task read Get_Task; default;
  end;

  T_TaskByNextStartList = class(TPrevalentList)
  private
    function Get_Task(I : integer) : T_Task;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_Task : T_Task);
    procedure Delete(_Task : T_Task);
    function Find(D : DateTime) : T_Task;
    function First : T_Task;
    function Last : T_Task;
    function Near(D : DateTime) : T_Task;
    function Next(var _Task : T_Task) : boolean;
    function Prior(var _Task : T_Task) : boolean;
    property _Task[I : integer] : T_Task read Get_Task; default;
  end;

  // Modelo de Importação|
  // Geração do Modelo de Importação
  // Este método recebe os seguintes parâmetros:
  // 1.	Nome do Modelo de Importação:
  // Este parâmetro é um identificador do modelo que será gerado.
  // 2.	Classe
  // Neste parâmetro deverá ser selecionada a Classe do Modelo que sofrerá a importação das informações.
  // 3.	É Importação do Tipo Delimitada?
  // Neste parâmetro, quando informado "Sim" será gerado um modelo para importação do tipo delimitada, e quando informado "Não", será gerado um modelo para importação do tipo posicional.
  // Adequação do Modelo de Importação
  // Após execução deste método será gerado um "Modelo de Importação" que necessitará ser adequado ao arquivo. A adequação será abortada em 2 aspectos:
  // 1.	Adequação dos dados de Importação do Tipo Delimitada:
  // A adequação consiste alterar os valores default gerados:
  // a)	Arquivo de Entrada:
  // É o arquivo que será lido na importação. É importante lembrar que a localização é em relação ao local onde está sendo executado o Server.
  // b)	Lista para Importação
  // Quando é efetuada uma importação, pode-se informar a lista em que será testada a já existência do objeto na Prevalência. Por exemplo, "PorCodigo". Se este atributo não for informado não será testada a existência do objeto na Prevalência.
  // c)	Se o Registro já Existir
  // Este valor indica o comportamento da importação quando o objeto já existir na Prevalência. Os valor possíveis são "Atualiza", onde as propriedades do objeto serão atualizadas pelos valores recebidos no arquivo; ou "Ignora", onde o registro do arquivo será ignorado e o objeto na Prevalência não sofrerá atualização.
  // d)	Balanced Line
  // Não implementado nesta versão
  // e)	Log
  // Sendo informado "SIM", serão geradas mensagens para acompanhamento durante a importação. Atenção: a geração de Log reduz a performance da importação.
  // f)	Formato da Data/Hora
  // Deverá ser informado o formato da data/hora existente no arquivo. Este atributo só será considerado caso a Classe que receberá as informações possua propriedade deste tipo.
  // g)	Número do Elemento Chave
  // O Modelo gerado possui elementos, conforme veremos em seguida. Este valor indica qual elemento será utilizado como chave para teste de existência do objeto na Prevalência. Não sendo informado, não será testada a existência.
  // h)	Linhas a Ignorar no Final do Arquivo
  // Poderá ser informada a quantidade de linhas que deverá ser ignorada no final do arquivo.
  // i)	Linhas a Saltar no Início do Arquivo
  // Poderá ser informada a quantidade de linhas que deverá ser saltadas no início do arquivo.
  // j)	Delimitador
  // Deverá ser informado o valor do delimitador utilizado no arquivo. O Default é ‘,’, mas poderá ser utilizado qualquer outro. Possui apenas 1 posição.
  // k)	Propriedades geradas
  // Campos:
  // •	Número do Elemento: indica a posição do elemento no arquivo em relação ao delimitador. Exemplo:
  // AAAA,BBBB,CCCC
  // AAAA é o elemento de número 1
  // BBBB é o elemento de número 2
  // CCCC é o elemento de número 3
  // •	Tipo da Propriedade: não é alterável e serve para orientação da adequação da propriedade. Pode assumir os seguintes valores: Atributo, Referência ou Associação.
  // •	Lista Chave: esta propriedade deverá ser informada apenas no caso de Elementos do tipo Referência ou Associação e é utilizada com o mesmo propósito da propriedade Lista definida acima. O valor recuperado no Elemento será utilizado para acessar a Associação ou a Referência, pela "Lista Chave" informada e assim criar a ligação.
  // •	Propriedade: é a propriedade existente na classe que deverá ser relacionada com o Elemento do arquivo.
  // A adequação das propriedades poderá ser feita pelos seguintes motivos:
  // •	A ordem da propriedade não é a mesma da existente no arquivo:
  // o	Neste caso deve-se atribuir novos valores para o atributo Elemento.
  // •	Não interessa importar ou não existe no arquivo a Propriedade existente na Classe:
  // o	Exclui-se a o Elemento
  // •	Não desejo importar o Elemento Y:
  // o	Não é exigida seqüência sem "buracos" nos Elementos, já que este é um reflexo do formato do arquivo. Exemplificando, é possível no Modelo de importação existir os elementos 1, 2, 4 e 5, indicando que o elemento 3 do arquivo será ignorado.
  // •	Existência de Referência e Associação
  // o	Deverá ser informada a "Lista Chave". Por exemplo, PorCodigo.
  // Pontos de Função: 8.0, Baixo
  T_ImportaDelimitado = class(T_Importa)
  private
    _Delimitador : String; 
    _Propriedades : T_ImportaDelimitadoPropriedadesAssociation; 
    function  GetDelimitador : String;
    procedure SetDelimitador(Value : String);
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Delimitador : String read GetDelimitador write SetDelimitador;
    property Propriedades : T_ImportaDelimitadoPropriedadesAssociation read _Propriedades write _Propriedades;
    function CheckDelimitador(var Message : String) : Boolean; virtual;
  end;

  T_ImportaDelimitadoList = class(TPrevalentList)
  private
    function Get_ImportaDelimitado(I : integer) : T_ImportaDelimitado;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_ImportaDelimitado : T_ImportaDelimitado);
    procedure Delete(_ImportaDelimitado : T_ImportaDelimitado);
    function Find(I : Integer) : T_ImportaDelimitado;
    function First : T_ImportaDelimitado;
    function Last : T_ImportaDelimitado;
    function Near(I : Integer) : T_ImportaDelimitado;
    function Next(var _ImportaDelimitado : T_ImportaDelimitado) : boolean;
    function Prior(var _ImportaDelimitado : T_ImportaDelimitado) : boolean;
    property _ImportaDelimitado[I : integer] : T_ImportaDelimitado read Get_ImportaDelimitado; default;
  end;

  T_ImportaDelimitadoPorNomeList = class(TPrevalentList)
  private
    function Get_ImportaDelimitado(I : integer) : T_ImportaDelimitado;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_ImportaDelimitado : T_ImportaDelimitado);
    procedure Delete(_ImportaDelimitado : T_ImportaDelimitado);
    function Find(s : string) : T_ImportaDelimitado;
    function First : T_ImportaDelimitado;
    function Last : T_ImportaDelimitado;
    function Near(s : string) : T_ImportaDelimitado;
    function Next(var _ImportaDelimitado : T_ImportaDelimitado) : boolean;
    function Prior(var _ImportaDelimitado : T_ImportaDelimitado) : boolean;
    property _ImportaDelimitado[I : integer] : T_ImportaDelimitado read Get_ImportaDelimitado; default;
  end;

  // Modelo de Importação|
  // 2.	Adequação dos dados de Importação do Tipo Posicional:
  // A adequação consiste alterar os valores default gerados:
  // a)	Arquivo de Entrada:
  // É o arquivo que será lido na importação. É importante lembrar que a localização é em relação ao local onde está sendo executado o Server.
  // b)	Lista para Importação
  // Quando é efetuada uma importação, pode-se informar a lista em que será testada a já existência do objeto na Prevalência. Por exemplo, "PorCodigo". Se este atributo não for informado não será testada a existência do objeto na Prevalência.
  // c)	Se o Registro já Existir
  // Este valor indica o comportamento da importação quando o objeto já existir na Prevalência. Os valor possíveis são "Atualiza", onde as propriedades do objeto serão atualizadas pelos valores recebidos no arquivo; ou "Ignora", onde o registro do arquivo será ignorado e o objeto na Prevalência não sofrerá atualização.
  // d)	Balanced Line
  // Não implementado nesta versão
  // e)	Log
  // Sendo informado "SIM", serão geradas mensagens para acompanhamento durante a importação. Atenção: a geração de Log reduz a performance da importação.
  // f)	Formato da Data/Hora
  // Deverá ser informado o formato da data/hora existente no arquivo. Este atributo só será considerado caso a Classe que receberá as informações possua propriedade deste tipo.
  // g)	Número do Elemento Chave
  // O Modelo gerado possui elementos, conforme veremos em seguida. Este valor indica qual elemento será utilizado como chave para teste de existência do objeto na Prevalência. Não sendo informado, não será testada a existência.
  // h)	Linhas a Ignorar no Final do Arquivo
  // Poderá ser informada a quantidade de linhas que deverá ser ignorada no final do arquivo.
  // i)	Linhas a Saltar no Início do Arquivo
  // Poderá ser informada a quantidade de linhas que deverá ser saltadas no início do arquivo.
  // j)	Propriedades geradas
  // Campos:
  // Número do Elemento: indica a ordem do elemento
  // •	Tipo da Propriedade: não é alterável e serve para orientação da adequação da propriedade. Pode assumir os seguintes valores: Atributo, Referência ou Associação.
  // •	Lista Chave: esta propriedade deverá ser informada apenas no caso de Elementos do tipo Referência ou Associação e é utilizada com o mesmo propósito da propriedade Lista definida acima. O valor recuperado no Elemento será utilizado para acessar a Associação ou a Referência, pela "Lista Chave" informada e assim criar a ligação.
  // •	Propriedade: é a propriedade existente na classe que deverá ser relacionada com o Elemento do arquivo.
  // •	Posição Inicial: é a posição do elemento no registro do arquivo
  // •	Quantidade de posições: é a quantidade de posições do elemento a partir da posição inicial no registro do arquivo
  // A adequação das propriedades poderá/deverá ser feita pelos seguintes motivos:
  // •	Não interessa importar ou não existe no arquivo a Propriedade existente na Classe:
  // o	Exclui-se a o Elemento
  // •	Existência de Referência e Associação
  // o	Deverá ser informada a "Lista Chave". Por exemplo, PorCodigo.
  // •	Informar a Posição Inicial e a Quantidade de posições para cada um dos elementos
  // Pontos de Função: 8.0, Baixo
  T_ImportaPosicional = class(T_Importa)
  private
    _Propriedades : T_ImportaPosicionalPropriedadesAssociation; 
  protected
    procedure New; override;
    procedure InternalFree; override;
  published
    property Propriedades : T_ImportaPosicionalPropriedadesAssociation read _Propriedades write _Propriedades;
  end;

  T_ImportaPosicionalList = class(TPrevalentList)
  private
    function Get_ImportaPosicional(I : integer) : T_ImportaPosicional;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_ImportaPosicional : T_ImportaPosicional);
    procedure Delete(_ImportaPosicional : T_ImportaPosicional);
    function Find(I : Integer) : T_ImportaPosicional;
    function First : T_ImportaPosicional;
    function Last : T_ImportaPosicional;
    function Near(I : Integer) : T_ImportaPosicional;
    function Next(var _ImportaPosicional : T_ImportaPosicional) : boolean;
    function Prior(var _ImportaPosicional : T_ImportaPosicional) : boolean;
    property _ImportaPosicional[I : integer] : T_ImportaPosicional read Get_ImportaPosicional; default;
  end;

  T_ImportaPosicionalPorNomeList = class(TPrevalentList)
  private
    function Get_ImportaPosicional(I : integer) : T_ImportaPosicional;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_ImportaPosicional : T_ImportaPosicional);
    procedure Delete(_ImportaPosicional : T_ImportaPosicional);
    function Find(s : string) : T_ImportaPosicional;
    function First : T_ImportaPosicional;
    function Last : T_ImportaPosicional;
    function Near(s : string) : T_ImportaPosicional;
    function Next(var _ImportaPosicional : T_ImportaPosicional) : boolean;
    function Prior(var _ImportaPosicional : T_ImportaPosicional) : boolean;
    property _ImportaPosicional[I : integer] : T_ImportaPosicional read Get_ImportaPosicional; default;
  end;

  // Pontos de Função: 8.0, Baixo
  T_PendencyTimeoutTask = class(T_Task)
  private
    _Pendency : T_Pendency; 
    function  GetPendency : T_Pendency;
    procedure SetPendency(Value : T_Pendency);
  published
    property Pendency : T_Pendency read GetPendency write SetPendency;
  end;

  T_PendencyTimeoutTaskList = class(TPrevalentList)
  private
    function Get_PendencyTimeoutTask(I : integer) : T_PendencyTimeoutTask;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PendencyTimeoutTask : T_PendencyTimeoutTask);
    procedure Delete(_PendencyTimeoutTask : T_PendencyTimeoutTask);
    function Find(I : Integer) : T_PendencyTimeoutTask;
    function First : T_PendencyTimeoutTask;
    function Last : T_PendencyTimeoutTask;
    function Near(I : Integer) : T_PendencyTimeoutTask;
    function Next(var _PendencyTimeoutTask : T_PendencyTimeoutTask) : boolean;
    function Prior(var _PendencyTimeoutTask : T_PendencyTimeoutTask) : boolean;
    property _PendencyTimeoutTask[I : integer] : T_PendencyTimeoutTask read Get_PendencyTimeoutTask; default;
  end;

  T_PendencyTimeoutTaskByNextStartList = class(TPrevalentList)
  private
    function Get_PendencyTimeoutTask(I : integer) : T_PendencyTimeoutTask;
  protected
    class function GetKeyCode : pointer; override;
    class function GetListType : TListType; override;
  public
    class function GetObjectClass : TTransientClass; override;
    procedure Add(_PendencyTimeoutTask : T_PendencyTimeoutTask);
    procedure Delete(_PendencyTimeoutTask : T_PendencyTimeoutTask);
    function Find(D : DateTime) : T_PendencyTimeoutTask;
    function First : T_PendencyTimeoutTask;
    function Last : T_PendencyTimeoutTask;
    function Near(D : DateTime) : T_PendencyTimeoutTask;
    function Next(var _PendencyTimeoutTask : T_PendencyTimeoutTask) : boolean;
    function Prior(var _PendencyTimeoutTask : T_PendencyTimeoutTask) : boolean;
    property _PendencyTimeoutTask[I : integer] : T_PendencyTimeoutTask read Get_PendencyTimeoutTask; default;
  end;

// Get Lists - Declarations
function _Aud_ArquivoList : T_Aud_ArquivoList;
function _Aud_ArquivoPorNomeList : T_Aud_ArquivoPorNomeList;
function _Aud_ArquivoPorStatusList : T_Aud_ArquivoPorStatusList;
function _Aud_ArquivoPorDataList : T_Aud_ArquivoPorDataList;
function _Aud_ClasseList : T_Aud_ClasseList;
function _Aud_ClassePorNomeList : T_Aud_ClassePorNomeList;
function _Aud_CriterioList : T_Aud_CriterioList;
function _Aud_CriterioPorDataInicioList : T_Aud_CriterioPorDataInicioList;
function _Aud_CriterioPorUsuarioList : T_Aud_CriterioPorUsuarioList;
function _Aud_LoginList : T_Aud_LoginList;
function _Aud_LoginPorUsuarioMaquinaPerfilList : T_Aud_LoginPorUsuarioMaquinaPerfilList;
function _Aud_OperacaoList : T_Aud_OperacaoList;
function _Aud_OrigemList : T_Aud_OrigemList;
function _Aud_OrigemPorNomeList : T_Aud_OrigemPorNomeList;
function _Aud_PackageList : T_Aud_PackageList;
function _Aud_PackagePorNomeList : T_Aud_PackagePorNomeList;
function _Aud_UpdObjetoList : T_Aud_UpdObjetoList;
function _ClasseList : T_ClasseList;
function _ClassePorNomeList : T_ClassePorNomeList;
function _ClassePorAliasList : T_ClassePorAliasList;
function _DominioList : T_DominioList;
function _DominioPorNomeList : T_DominioPorNomeList;
function _ExportaImportaList : TPrevalentList;
function _GeralList : T_GeralList;
function _GrupoList : T_GrupoList;
function _GrupoPorNomeList : T_GrupoPorNomeList;
function _GrupoPorIdentificadorList : T_GrupoPorIdentificadorList;
function _ImportaPropriedadeList : TPrevalentList;
function _LogList : T_LogList;
function _MetodoList : T_MetodoList;
function _MetodoPorNomeList : T_MetodoPorNomeList;
function _MetodoPorViewList : T_MetodoPorViewList;
function _MetodoPorAliasList : T_MetodoPorAliasList;
function _MetodoPorIdentificadorList : T_MetodoPorIdentificadorList;
function _OcupacaoList : TPrevalentList;
function _OcupacaoAssociacaoList : T_OcupacaoAssociacaoList;
function _PackageList : T_PackageList;
function _PackagePorNomeList : T_PackagePorNomeList;
function _PermissaoClasseList : T_PermissaoClasseList;
function _PermissaoMetodoList : T_PermissaoMetodoList;
function _PermissaoPackageList : T_PermissaoPackageList;
function _PermissaoPropriedadeList : T_PermissaoPropriedadeList;
function _PermissaoViewList : T_PermissaoViewList;
function _PropriedadeList : T_PropriedadeList;
function _PropriedadePorNomeList : T_PropriedadePorNomeList;
function _PropriedadePorAliasList : T_PropriedadePorAliasList;
function _RunList : TPrevalentList;
function _SequenceList : T_SequenceList;
function _SequenceByNameList : T_SequenceByNameList;
function _ServicoList : T_ServicoList;
function _ServicoPorNomeList : T_ServicoPorNomeList;
function _SessaoList : T_SessaoList;
function _SessaoPorThreadList : T_SessaoPorThreadList;
function _SessaoPorUsuarioList : T_SessaoPorUsuarioList;
function _UsuarioBloqueadoList : T_UsuarioBloqueadoList;
function _UsuarioBloqueadoPorIdentificadorList : T_UsuarioBloqueadoPorIdentificadorList;
function _ExpPropriedadeList : T_ExpPropriedadeList;
function _ExpPropriedadePorElementoList : T_ExpPropriedadePorElementoList;
function _ExpPropriedadePorPropNomeList : T_ExpPropriedadePorPropNomeList;
function _ExportaList : T_ExportaList;
function _ExportaPorNomeList : T_ExportaPorNomeList;
function _ImportaList : TPrevalentList;
function _OcupacaoClasseList : T_OcupacaoClasseList;
function _OcupacaoClassePorNomeList : T_OcupacaoClassePorNomeList;
function _OcupacaoListaList : T_OcupacaoListaList;
function _OcupacaoPackageList : T_OcupacaoPackageList;
function _OcupacaoPackagePorNomeList : T_OcupacaoPackagePorNomeList;
function _PendencyList : T_PendencyList;
function _PendencyByPriorityList : T_PendencyByPriorityList;
function _PendencyByRunObjectList : T_PendencyByRunObjectList;
function _PropriedadeDelimitadoList : T_PropriedadeDelimitadoList;
function _PropriedadeDelimitadoPorElementoList : T_PropriedadeDelimitadoPorElementoList;
function _PropriedadeDelimitadoPorPropNomeList : T_PropriedadeDelimitadoPorPropNomeList;
function _PropriedadePosicionalList : T_PropriedadePosicionalList;
function _PropriedadePosicionalPorElementoList : T_PropriedadePosicionalPorElementoList;
function _PropriedadePosicionalPorPropNomeList : T_PropriedadePosicionalPorPropNomeList;
function _TaskList : T_TaskList;
function _TaskByNextStartList : T_TaskByNextStartList;
function _ImportaDelimitadoList : T_ImportaDelimitadoList;
function _ImportaDelimitadoPorNomeList : T_ImportaDelimitadoPorNomeList;
function _ImportaPosicionalList : T_ImportaPosicionalList;
function _ImportaPosicionalPorNomeList : T_ImportaPosicionalPorNomeList;
function _PendencyTimeoutTaskList : T_PendencyTimeoutTaskList;
function _PendencyTimeoutTaskByNextStartList : T_PendencyTimeoutTaskByNextStartList;

// Initialize for Prevalence Recover
procedure InitepModel;

implementation

uses StrUtils, DateUtils, Math, TypInfo, epProperties, epServer, epStateMachine;

// Global Lists
var
  F_Aud_ArquivoList : T_Aud_ArquivoList;
  F_Aud_ArquivoPorNomeList : T_Aud_ArquivoPorNomeList;
  F_Aud_ArquivoPorStatusList : T_Aud_ArquivoPorStatusList;
  F_Aud_ArquivoPorDataList : T_Aud_ArquivoPorDataList;
  F_Aud_ClasseList : T_Aud_ClasseList;
  F_Aud_ClassePorNomeList : T_Aud_ClassePorNomeList;
  F_Aud_CriterioList : T_Aud_CriterioList;
  F_Aud_CriterioPorDataInicioList : T_Aud_CriterioPorDataInicioList;
  F_Aud_CriterioPorUsuarioList : T_Aud_CriterioPorUsuarioList;
  F_Aud_LoginList : T_Aud_LoginList;
  F_Aud_LoginPorUsuarioMaquinaPerfilList : T_Aud_LoginPorUsuarioMaquinaPerfilList;
  F_Aud_OperacaoList : T_Aud_OperacaoList;
  F_Aud_OrigemList : T_Aud_OrigemList;
  F_Aud_OrigemPorNomeList : T_Aud_OrigemPorNomeList;
  F_Aud_PackageList : T_Aud_PackageList;
  F_Aud_PackagePorNomeList : T_Aud_PackagePorNomeList;
  F_Aud_UpdObjetoList : T_Aud_UpdObjetoList;
  F_ClasseList : T_ClasseList;
  F_ClassePorNomeList : T_ClassePorNomeList;
  F_ClassePorAliasList : T_ClassePorAliasList;
  F_DominioList : T_DominioList;
  F_DominioPorNomeList : T_DominioPorNomeList;
  F_ExportaImportaList : TPrevalentList;
  F_GeralList : T_GeralList;
  F_GrupoList : T_GrupoList;
  F_GrupoPorNomeList : T_GrupoPorNomeList;
  F_GrupoPorIdentificadorList : T_GrupoPorIdentificadorList;
  F_ImportaPropriedadeList : TPrevalentList;
  F_LogList : T_LogList;
  F_MetodoList : T_MetodoList;
  F_MetodoPorNomeList : T_MetodoPorNomeList;
  F_MetodoPorViewList : T_MetodoPorViewList;
  F_MetodoPorAliasList : T_MetodoPorAliasList;
  F_MetodoPorIdentificadorList : T_MetodoPorIdentificadorList;
  F_OcupacaoList : TPrevalentList;
  F_OcupacaoAssociacaoList : T_OcupacaoAssociacaoList;
  F_PackageList : T_PackageList;
  F_PackagePorNomeList : T_PackagePorNomeList;
  F_PermissaoClasseList : T_PermissaoClasseList;
  F_PermissaoMetodoList : T_PermissaoMetodoList;
  F_PermissaoPackageList : T_PermissaoPackageList;
  F_PermissaoPropriedadeList : T_PermissaoPropriedadeList;
  F_PermissaoViewList : T_PermissaoViewList;
  F_PropriedadeList : T_PropriedadeList;
  F_PropriedadePorNomeList : T_PropriedadePorNomeList;
  F_PropriedadePorAliasList : T_PropriedadePorAliasList;
  F_RunList : TPrevalentList;
  F_SequenceList : T_SequenceList;
  F_SequenceByNameList : T_SequenceByNameList;
  F_ServicoList : T_ServicoList;
  F_ServicoPorNomeList : T_ServicoPorNomeList;
  F_SessaoList : T_SessaoList;
  F_SessaoPorThreadList : T_SessaoPorThreadList;
  F_SessaoPorUsuarioList : T_SessaoPorUsuarioList;
  F_UsuarioBloqueadoList : T_UsuarioBloqueadoList;
  F_UsuarioBloqueadoPorIdentificadorList : T_UsuarioBloqueadoPorIdentificadorList;
  F_ExpPropriedadeList : T_ExpPropriedadeList;
  F_ExpPropriedadePorElementoList : T_ExpPropriedadePorElementoList;
  F_ExpPropriedadePorPropNomeList : T_ExpPropriedadePorPropNomeList;
  F_ExportaList : T_ExportaList;
  F_ExportaPorNomeList : T_ExportaPorNomeList;
  F_ImportaList : TPrevalentList;
  F_OcupacaoClasseList : T_OcupacaoClasseList;
  F_OcupacaoClassePorNomeList : T_OcupacaoClassePorNomeList;
  F_OcupacaoListaList : T_OcupacaoListaList;
  F_OcupacaoPackageList : T_OcupacaoPackageList;
  F_OcupacaoPackagePorNomeList : T_OcupacaoPackagePorNomeList;
  F_PendencyList : T_PendencyList;
  F_PendencyByPriorityList : T_PendencyByPriorityList;
  F_PendencyByRunObjectList : T_PendencyByRunObjectList;
  F_PropriedadeDelimitadoList : T_PropriedadeDelimitadoList;
  F_PropriedadeDelimitadoPorElementoList : T_PropriedadeDelimitadoPorElementoList;
  F_PropriedadeDelimitadoPorPropNomeList : T_PropriedadeDelimitadoPorPropNomeList;
  F_PropriedadePosicionalList : T_PropriedadePosicionalList;
  F_PropriedadePosicionalPorElementoList : T_PropriedadePosicionalPorElementoList;
  F_PropriedadePosicionalPorPropNomeList : T_PropriedadePosicionalPorPropNomeList;
  F_TaskList : T_TaskList;
  F_TaskByNextStartList : T_TaskByNextStartList;
  F_ImportaDelimitadoList : T_ImportaDelimitadoList;
  F_ImportaDelimitadoPorNomeList : T_ImportaDelimitadoPorNomeList;
  F_ImportaPosicionalList : T_ImportaPosicionalList;
  F_ImportaPosicionalPorNomeList : T_ImportaPosicionalPorNomeList;
  F_PendencyTimeoutTaskList : T_PendencyTimeoutTaskList;
  F_PendencyTimeoutTaskByNextStartList : T_PendencyTimeoutTaskByNextStartList;

// Get Lists
function _Aud_ArquivoList : T_Aud_ArquivoList; begin Result := F_Aud_ArquivoList end;
function _Aud_ArquivoPorNomeList : T_Aud_ArquivoPorNomeList; begin Result := F_Aud_ArquivoPorNomeList end;
function _Aud_ArquivoPorStatusList : T_Aud_ArquivoPorStatusList; begin Result := F_Aud_ArquivoPorStatusList end;
function _Aud_ArquivoPorDataList : T_Aud_ArquivoPorDataList; begin Result := F_Aud_ArquivoPorDataList end;
function _Aud_ClasseList : T_Aud_ClasseList; begin Result := F_Aud_ClasseList end;
function _Aud_ClassePorNomeList : T_Aud_ClassePorNomeList; begin Result := F_Aud_ClassePorNomeList end;
function _Aud_CriterioList : T_Aud_CriterioList; begin Result := F_Aud_CriterioList end;
function _Aud_CriterioPorDataInicioList : T_Aud_CriterioPorDataInicioList; begin Result := F_Aud_CriterioPorDataInicioList end;
function _Aud_CriterioPorUsuarioList : T_Aud_CriterioPorUsuarioList; begin Result := F_Aud_CriterioPorUsuarioList end;
function _Aud_LoginList : T_Aud_LoginList; begin Result := F_Aud_LoginList end;
function _Aud_LoginPorUsuarioMaquinaPerfilList : T_Aud_LoginPorUsuarioMaquinaPerfilList; begin Result := F_Aud_LoginPorUsuarioMaquinaPerfilList end;
function _Aud_OperacaoList : T_Aud_OperacaoList; begin Result := F_Aud_OperacaoList end;
function _Aud_OrigemList : T_Aud_OrigemList; begin Result := F_Aud_OrigemList end;
function _Aud_OrigemPorNomeList : T_Aud_OrigemPorNomeList; begin Result := F_Aud_OrigemPorNomeList end;
function _Aud_PackageList : T_Aud_PackageList; begin Result := F_Aud_PackageList end;
function _Aud_PackagePorNomeList : T_Aud_PackagePorNomeList; begin Result := F_Aud_PackagePorNomeList end;
function _Aud_UpdObjetoList : T_Aud_UpdObjetoList; begin Result := F_Aud_UpdObjetoList end;
function _ClasseList : T_ClasseList; begin Result := F_ClasseList end;
function _ClassePorNomeList : T_ClassePorNomeList; begin Result := F_ClassePorNomeList end;
function _ClassePorAliasList : T_ClassePorAliasList; begin Result := F_ClassePorAliasList end;
function _DominioList : T_DominioList; begin Result := F_DominioList end;
function _DominioPorNomeList : T_DominioPorNomeList; begin Result := F_DominioPorNomeList end;
function _ExportaImportaList : TPrevalentList; begin Result := F_ExportaImportaList end;
function _GeralList : T_GeralList; begin Result := F_GeralList end;
function _GrupoList : T_GrupoList; begin Result := F_GrupoList end;
function _GrupoPorNomeList : T_GrupoPorNomeList; begin Result := F_GrupoPorNomeList end;
function _GrupoPorIdentificadorList : T_GrupoPorIdentificadorList; begin Result := F_GrupoPorIdentificadorList end;
function _ImportaPropriedadeList : TPrevalentList; begin Result := F_ImportaPropriedadeList end;
function _LogList : T_LogList; begin Result := F_LogList end;
function _MetodoList : T_MetodoList; begin Result := F_MetodoList end;
function _MetodoPorNomeList : T_MetodoPorNomeList; begin Result := F_MetodoPorNomeList end;
function _MetodoPorViewList : T_MetodoPorViewList; begin Result := F_MetodoPorViewList end;
function _MetodoPorAliasList : T_MetodoPorAliasList; begin Result := F_MetodoPorAliasList end;
function _MetodoPorIdentificadorList : T_MetodoPorIdentificadorList; begin Result := F_MetodoPorIdentificadorList end;
function _OcupacaoList : TPrevalentList; begin Result := F_OcupacaoList end;
function _OcupacaoAssociacaoList : T_OcupacaoAssociacaoList; begin Result := F_OcupacaoAssociacaoList end;
function _PackageList : T_PackageList; begin Result := F_PackageList end;
function _PackagePorNomeList : T_PackagePorNomeList; begin Result := F_PackagePorNomeList end;
function _PermissaoClasseList : T_PermissaoClasseList; begin Result := F_PermissaoClasseList end;
function _PermissaoMetodoList : T_PermissaoMetodoList; begin Result := F_PermissaoMetodoList end;
function _PermissaoPackageList : T_PermissaoPackageList; begin Result := F_PermissaoPackageList end;
function _PermissaoPropriedadeList : T_PermissaoPropriedadeList; begin Result := F_PermissaoPropriedadeList end;
function _PermissaoViewList : T_PermissaoViewList; begin Result := F_PermissaoViewList end;
function _PropriedadeList : T_PropriedadeList; begin Result := F_PropriedadeList end;
function _PropriedadePorNomeList : T_PropriedadePorNomeList; begin Result := F_PropriedadePorNomeList end;
function _PropriedadePorAliasList : T_PropriedadePorAliasList; begin Result := F_PropriedadePorAliasList end;
function _RunList : TPrevalentList; begin Result := F_RunList end;
function _SequenceList : T_SequenceList; begin Result := F_SequenceList end;
function _SequenceByNameList : T_SequenceByNameList; begin Result := F_SequenceByNameList end;
function _ServicoList : T_ServicoList; begin Result := F_ServicoList end;
function _ServicoPorNomeList : T_ServicoPorNomeList; begin Result := F_ServicoPorNomeList end;
function _SessaoList : T_SessaoList; begin Result := F_SessaoList end;
function _SessaoPorThreadList : T_SessaoPorThreadList; begin Result := F_SessaoPorThreadList end;
function _SessaoPorUsuarioList : T_SessaoPorUsuarioList; begin Result := F_SessaoPorUsuarioList end;
function _UsuarioBloqueadoList : T_UsuarioBloqueadoList; begin Result := F_UsuarioBloqueadoList end;
function _UsuarioBloqueadoPorIdentificadorList : T_UsuarioBloqueadoPorIdentificadorList; begin Result := F_UsuarioBloqueadoPorIdentificadorList end;
function _ExpPropriedadeList : T_ExpPropriedadeList; begin Result := F_ExpPropriedadeList end;
function _ExpPropriedadePorElementoList : T_ExpPropriedadePorElementoList; begin Result := F_ExpPropriedadePorElementoList end;
function _ExpPropriedadePorPropNomeList : T_ExpPropriedadePorPropNomeList; begin Result := F_ExpPropriedadePorPropNomeList end;
function _ExportaList : T_ExportaList; begin Result := F_ExportaList end;
function _ExportaPorNomeList : T_ExportaPorNomeList; begin Result := F_ExportaPorNomeList end;
function _ImportaList : TPrevalentList; begin Result := F_ImportaList end;
function _OcupacaoClasseList : T_OcupacaoClasseList; begin Result := F_OcupacaoClasseList end;
function _OcupacaoClassePorNomeList : T_OcupacaoClassePorNomeList; begin Result := F_OcupacaoClassePorNomeList end;
function _OcupacaoListaList : T_OcupacaoListaList; begin Result := F_OcupacaoListaList end;
function _OcupacaoPackageList : T_OcupacaoPackageList; begin Result := F_OcupacaoPackageList end;
function _OcupacaoPackagePorNomeList : T_OcupacaoPackagePorNomeList; begin Result := F_OcupacaoPackagePorNomeList end;
function _PendencyList : T_PendencyList; begin Result := F_PendencyList end;
function _PendencyByPriorityList : T_PendencyByPriorityList; begin Result := F_PendencyByPriorityList end;
function _PendencyByRunObjectList : T_PendencyByRunObjectList; begin Result := F_PendencyByRunObjectList end;
function _PropriedadeDelimitadoList : T_PropriedadeDelimitadoList; begin Result := F_PropriedadeDelimitadoList end;
function _PropriedadeDelimitadoPorElementoList : T_PropriedadeDelimitadoPorElementoList; begin Result := F_PropriedadeDelimitadoPorElementoList end;
function _PropriedadeDelimitadoPorPropNomeList : T_PropriedadeDelimitadoPorPropNomeList; begin Result := F_PropriedadeDelimitadoPorPropNomeList end;
function _PropriedadePosicionalList : T_PropriedadePosicionalList; begin Result := F_PropriedadePosicionalList end;
function _PropriedadePosicionalPorElementoList : T_PropriedadePosicionalPorElementoList; begin Result := F_PropriedadePosicionalPorElementoList end;
function _PropriedadePosicionalPorPropNomeList : T_PropriedadePosicionalPorPropNomeList; begin Result := F_PropriedadePosicionalPorPropNomeList end;
function _TaskList : T_TaskList; begin Result := F_TaskList end;
function _TaskByNextStartList : T_TaskByNextStartList; begin Result := F_TaskByNextStartList end;
function _ImportaDelimitadoList : T_ImportaDelimitadoList; begin Result := F_ImportaDelimitadoList end;
function _ImportaDelimitadoPorNomeList : T_ImportaDelimitadoPorNomeList; begin Result := F_ImportaDelimitadoPorNomeList end;
function _ImportaPosicionalList : T_ImportaPosicionalList; begin Result := F_ImportaPosicionalList end;
function _ImportaPosicionalPorNomeList : T_ImportaPosicionalPorNomeList; begin Result := F_ImportaPosicionalPorNomeList end;
function _PendencyTimeoutTaskList : T_PendencyTimeoutTaskList; begin Result := F_PendencyTimeoutTaskList end;
function _PendencyTimeoutTaskByNextStartList : T_PendencyTimeoutTaskByNextStartList; begin Result := F_PendencyTimeoutTaskByNextStartList end;

procedure InitLists; begin
  F_Aud_ArquivoList := T_Aud_ArquivoList.Create;
  F_Aud_ArquivoPorNomeList := T_Aud_ArquivoPorNomeList.Create([lpDuplicates]);
  F_Aud_ArquivoPorStatusList := T_Aud_ArquivoPorStatusList.Create([lpDuplicates]);
  F_Aud_ArquivoPorDataList := T_Aud_ArquivoPorDataList.Create([lpDuplicates]);
  F_Aud_ClasseList := T_Aud_ClasseList.Create([lpTransient], '', '');
  F_Aud_ClassePorNomeList := T_Aud_ClassePorNomeList.Create([lpDuplicates]);
  F_Aud_CriterioList := T_Aud_CriterioList.Create([lpTransient], '', '');
  F_Aud_CriterioPorDataInicioList := T_Aud_CriterioPorDataInicioList.Create([lpDuplicates]);
  F_Aud_CriterioPorUsuarioList := T_Aud_CriterioPorUsuarioList.Create([lpDuplicates]);
  F_Aud_LoginList := T_Aud_LoginList.Create([lpTransient], '', '');
  F_Aud_LoginPorUsuarioMaquinaPerfilList := T_Aud_LoginPorUsuarioMaquinaPerfilList.Create([lpDuplicates]);
  F_Aud_OperacaoList := T_Aud_OperacaoList.Create([lpTransient], '', '');
  F_Aud_OrigemList := T_Aud_OrigemList.Create([lpTransient], '', '');
  F_Aud_OrigemPorNomeList := T_Aud_OrigemPorNomeList.Create([lpDuplicates]);
  F_Aud_PackageList := T_Aud_PackageList.Create([lpTransient], '', '');
  F_Aud_PackagePorNomeList := T_Aud_PackagePorNomeList.Create([lpDuplicates]);
  F_Aud_UpdObjetoList := T_Aud_UpdObjetoList.Create;
  F_ClasseList := T_ClasseList.Create;
  F_ClassePorNomeList := T_ClassePorNomeList.Create([lpDuplicates]);
  F_ClassePorAliasList := T_ClassePorAliasList.Create([lpDuplicates]);
  F_DominioList := T_DominioList.Create;
  F_DominioPorNomeList := T_DominioPorNomeList.Create([lpDuplicates]);
  F_ExportaImportaList := T_ExportaImportaList.Create([lpAbstract]);
  F_GeralList := T_GeralList.Create;
  F_GrupoList := T_GrupoList.Create;
  F_GrupoPorNomeList := T_GrupoPorNomeList.Create([lpDuplicates]);
  F_GrupoPorIdentificadorList := T_GrupoPorIdentificadorList.Create([lpDuplicates]);
  F_ImportaPropriedadeList := T_ImportaPropriedadeList.Create([lpAbstract]);
  F_LogList := T_LogList.Create;
  F_MetodoList := T_MetodoList.Create;
  F_MetodoPorNomeList := T_MetodoPorNomeList.Create([lpDuplicates]);
  F_MetodoPorViewList := T_MetodoPorViewList.Create([lpDuplicates],'PorViewFilter');
  F_MetodoPorAliasList := T_MetodoPorAliasList.Create([lpDuplicates]);
  F_MetodoPorIdentificadorList := T_MetodoPorIdentificadorList.Create([lpDuplicates]);
  F_OcupacaoList := T_OcupacaoList.Create([lpAbstract]);
  F_OcupacaoAssociacaoList := T_OcupacaoAssociacaoList.Create([lpTransient], '', '');
  F_PackageList := T_PackageList.Create;
  F_PackagePorNomeList := T_PackagePorNomeList.Create([lpDuplicates]);
  F_PermissaoClasseList := T_PermissaoClasseList.Create;
  F_PermissaoMetodoList := T_PermissaoMetodoList.Create;
  F_PermissaoPackageList := T_PermissaoPackageList.Create;
  F_PermissaoPropriedadeList := T_PermissaoPropriedadeList.Create;
  F_PermissaoViewList := T_PermissaoViewList.Create;
  F_PropriedadeList := T_PropriedadeList.Create;
  F_PropriedadePorNomeList := T_PropriedadePorNomeList.Create([lpDuplicates]);
  F_PropriedadePorAliasList := T_PropriedadePorAliasList.Create([lpDuplicates]);
  F_RunList := T_RunList.Create([lpAbstract]);
  F_SequenceList := T_SequenceList.Create;
  F_SequenceByNameList := T_SequenceByNameList.Create([lpDuplicates]);
  F_ServicoList := T_ServicoList.Create([lpTransient], '', '');
  F_ServicoPorNomeList := T_ServicoPorNomeList.Create([lpDuplicates]);
  F_SessaoList := T_SessaoList.Create([lpTransient], '', '');
  F_SessaoPorThreadList := T_SessaoPorThreadList.Create([lpDuplicates]);
  F_SessaoPorUsuarioList := T_SessaoPorUsuarioList.Create([lpDuplicates]);
  F_UsuarioBloqueadoList := T_UsuarioBloqueadoList.Create;
  F_UsuarioBloqueadoPorIdentificadorList := T_UsuarioBloqueadoPorIdentificadorList.Create([lpDuplicates]);
  F_ExpPropriedadeList := T_ExpPropriedadeList.Create;
  F_ExpPropriedadePorElementoList := T_ExpPropriedadePorElementoList.Create([lpDuplicates]);
  F_ExpPropriedadePorPropNomeList := T_ExpPropriedadePorPropNomeList.Create([lpDuplicates]);
  F_ExportaList := T_ExportaList.Create;
  F_ExportaPorNomeList := T_ExportaPorNomeList.Create([lpDuplicates]);
  F_ImportaList := T_ImportaList.Create([lpAbstract]);
  F_OcupacaoClasseList := T_OcupacaoClasseList.Create([lpTransient], '', '');
  F_OcupacaoClassePorNomeList := T_OcupacaoClassePorNomeList.Create([lpDuplicates]);
  F_OcupacaoListaList := T_OcupacaoListaList.Create([lpTransient], '', '');
  F_OcupacaoPackageList := T_OcupacaoPackageList.Create([lpTransient], '', '');
  F_OcupacaoPackagePorNomeList := T_OcupacaoPackagePorNomeList.Create([lpDuplicates]);
  F_PendencyList := T_PendencyList.Create;
  F_PendencyByPriorityList := T_PendencyByPriorityList.Create([lpDuplicates]);
  F_PendencyByRunObjectList := T_PendencyByRunObjectList.Create([lpDuplicates]);
  F_PropriedadeDelimitadoList := T_PropriedadeDelimitadoList.Create;
  F_PropriedadeDelimitadoPorElementoList := T_PropriedadeDelimitadoPorElementoList.Create([lpDuplicates]);
  F_PropriedadeDelimitadoPorPropNomeList := T_PropriedadeDelimitadoPorPropNomeList.Create([lpDuplicates]);
  F_PropriedadePosicionalList := T_PropriedadePosicionalList.Create;
  F_PropriedadePosicionalPorElementoList := T_PropriedadePosicionalPorElementoList.Create([lpDuplicates]);
  F_PropriedadePosicionalPorPropNomeList := T_PropriedadePosicionalPorPropNomeList.Create([lpDuplicates]);
  F_TaskList := T_TaskList.Create;
  F_TaskByNextStartList := T_TaskByNextStartList.Create([lpDuplicates]);
  F_ImportaDelimitadoList := T_ImportaDelimitadoList.Create;
  F_ImportaDelimitadoPorNomeList := T_ImportaDelimitadoPorNomeList.Create([lpDuplicates]);
  F_ImportaPosicionalList := T_ImportaPosicionalList.Create;
  F_ImportaPosicionalPorNomeList := T_ImportaPosicionalPorNomeList.Create([lpDuplicates]);
  F_PendencyTimeoutTaskList := T_PendencyTimeoutTaskList.Create;
  F_PendencyTimeoutTaskByNextStartList := T_PendencyTimeoutTaskByNextStartList.Create([lpDuplicates]);
end;

  // Metadata

procedure Init_Aud_Arquivo; begin
  with Prevalence.Metadata('T_Aud_Arquivo') do begin
    InheritMetadata(3);
    AddMetadata(0, 'hidden', '', '_Auditory');
    AddUnidirectional(T_Aud_Operacao, 'Arquivo', [NOTNULL]);
    AddUnidirectional(T_Aud_Login, 'Arquivo', [NOTNULL]);
    AddMethod(0, 'PorData', '', '', '', mkFunction, 0, TypeInfo(DateTime), _stINDEX);
    AddMethod(1, 'PorStatus', '', '', '', mkFunction, 0, TypeInfo(word), _stINDEX);
    AddMethod(2, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_Aud_Classe; begin
  with Prevalence.Metadata('T_Aud_Classe') do begin
    InheritMetadata(1);
    AddMetadata(0, 'Classe', '', '_Auditory');
    AddMetadata(1, '', '', T_Aud_Operacao, 'Classe');
    AddMethod(0, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_Aud_Criterio; begin
  with Prevalence.Metadata('T_Aud_Criterio') do begin
    InheritMetadata(4);
    AddMetadata(0, 'Critério de Pesquisa', '', '_Auditory');
    AddMetadata(1, '', '', T_Sessao, 'Criterios');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, '', '', T_Aud_Operacao, 'Criterio');
    AddMetadata(6, 'Usuário', '');
    AddMetadata(7, 'Operação', '');
    AddMetadata(8, 'Data Fim da Pesquisa', '');
    AddMetadata(9, 'Data Início da Pesquisa', '');
    AddMetadata(10, 'Data de Geração do Critério', '');
    AddMethod(0, 'PorUsuario', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
    AddMethod(1, 'PorDataInicio', '', '', '', mkFunction, 0, TypeInfo(DateTime), _stINDEX);
    AddMethod(2, 'RecuperarObjeto', '_Auditory|Consultar Pesquisa de Auditoria', '', '', mkProcedure, 0, nil, _stWORKFLOW);
    AddMethod(3, 'GerarPesquisa', '_Auditory|Gerar Pesquisa de Auditoria', '', '', mkClassProcedure, 0, nil, _stWORKFLOW);
  end;
end;

procedure Init_Aud_Login; begin
  with Prevalence.Metadata('T_Aud_Login') do begin
    InheritMetadata(1);
    AddMetadata(0, 'Login', 'Utilizado para controlar os logins e as trocas de perfis na auditoria', '_Auditory');
    AddMetadata(1, '', '', T_Aud_Operacao, 'Login');
    AddMetadata(2, '', '', T_Aud_Arquivo, '');
    AddConstraints(2, [NOTNULL]);
    AddMetadata(4, 'Posição', 'posição no arquivo de auditoria');
    AddMetadata(5, 'Máquina', '');
    AddMetadata(7, 'Usuário', '');
    AddMethod(0, 'PorUsuarioMaquinaPerfil', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_Aud_Operacao; begin
  with Prevalence.Metadata('T_Aud_Operacao') do begin
    InheritMetadata(0);
    AddMetadata(0, 'Operação', '', '_Auditory');
    AddClassTags('PropOrder', 'Operacao,Data,Login,Package,AliasClasse,IdentificacaoObjeto,Origem');
    AddMetadata(1, '', '', T_Aud_Criterio, 'Operacoes');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, '', '', T_Aud_Classe, 'Operacoes');
    AddConstraints(2, [NOTNULL]);
    AddMetadata(3, 'Origem da Operação', '', T_Aud_Origem, 'Operacoes');
    AddConstraints(3, [NOTNULL]);
    AddMetadata(4, '', '', T_Aud_UpdObjeto, 'Operacao');
    AddMetadata(5, '', '', T_Aud_Package, 'Operacoes');
    AddConstraints(5, [NOTNULL]);
    AddMetadata(6, 'Usuário', '', T_Aud_Login, 'Operacoes');
    AddConstraints(6, [NOTNULL]);
    AddMetadata(7, '', '', T_Aud_Arquivo, '');
    AddConstraints(7, [NOTNULL]);
    AddMetadata(8, 'Posição', '');
    AddMetadata(9, 'Identificação do Objeto', '');
    AddMetadata(10, 'Classe', '');
    AddConstraints(10, [READONLY]);
    AddMetadata(12, 'Operação', '');
  end;
end;

procedure Init_Aud_Origem; begin
  with Prevalence.Metadata('T_Aud_Origem') do begin
    InheritMetadata(1);
    AddMetadata(0, 'Origem da Operação', '', '_Auditory');
    AddMetadata(1, '', '', T_Aud_Operacao, 'Origem');
    AddMethod(0, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_Aud_Package; begin
  with Prevalence.Metadata('T_Aud_Package') do begin
    InheritMetadata(1);
    AddMetadata(0, 'Package', '', '_Auditory');
    AddMetadata(1, '', '', T_Aud_Operacao, 'Package');
    AddMethod(0, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_Aud_UpdObjeto; begin
  with Prevalence.Metadata('T_Aud_UpdObjeto') do begin
    InheritMetadata(0);
    AddMetadata(0, 'hidden', '', '_Auditory');
    AddMetadata(1, '', '', T_Aud_Operacao, 'Detalhes');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, 'Novo Valor', '');
    AddMetadata(3, 'Valor Anterior', '');
  end;
end;

procedure Init_Classe; begin
  with Prevalence.Metadata('T_Classe') do begin
    InheritMetadata(5);
    AddMetadata(0, 'hidden', '', '_Security');
    AddClassTags('PropOrder', 'Package,AliasNome,Nome,Propriedades,Metodos,PermissoesClasses');
    AddMetadata(1, '', '', T_Package, 'Classes');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, '', '', T_Propriedade, 'Classe');
    AddConstraints(2, [COMPOSITE]);
    AddMetadata(3, '', '', T_Metodo, 'Classe');
    AddConstraints(3, [COMPOSITE]);
    AddMetadata(4, 'Permissões de Classes', '', T_PermissaoClasse, 'Classe');
    AddConstraints(4, [COMPOSITE]);
    AddMetadata(5, 'Ocupações', '', T_OcupacaoClasse, 'Classe');
    AddUnidirectional(T_ExportaImporta, 'Classe', [NOTNULL]);
    AddMetadata(7, 'Alias', '');
    AddMethod(0, 'PorAlias', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
    AddMethod(1, 'Get_Propriedade', 'hidden', '', '', mkFunction, 1, TypeInfo(T_Propriedade), _stSTATEMACHINE);
      AddParam(0, 'Nome', '', '', '', pfConst, TypeInfo(String));
    AddMethod(2, 'Get_Metodo', 'hidden', '', '', mkFunction, 1, TypeInfo(T_Metodo), _stSTATEMACHINE);
      AddParam(0, 'Nome', '', '', '', pfConst, TypeInfo(String));
    AddMethod(3, 'AtualizaClasses', 'hidden', '', '', mkClassProcedure, 0, nil, _stSTATEMACHINE);
    AddMethod(4, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_Dominio; begin
  with Prevalence.Metadata('T_Dominio') do begin
    InheritMetadata(1);
    AddMetadata(0, 'Domínio', '', '_Security');
    AddMetadata(1, '', '', T_Grupo, 'Dominio');
    AddConstraints(1, [COMPOSITE]);
    AddUnidirectional(T_UsuarioBloqueado, 'Dominio', [NOTNULL]);
    AddMethod(0, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_ExportaImporta; begin
  with Prevalence.Metadata('T_ExportaImporta') do begin
    InheritMetadata(1);
    AddMetadata(0, 'hidden', 'Esta Classe permite a definição de importações|O ExtPascalUML permite que sejam efetuadas ' + 
				'importações de arquivos texto para dentro da Prevalência.'#13#10'As importações são ' + 
				'tratadas de duas formas:'#13#10'1. arquivos com delimitadores de campo;'#13#10'2. ' + 
				'arquivos com tratamento posicional.'#13#10'A forma de utilização da importação no ExtPascalUML ' + 
				'segue os seguintes passos:'#13#10'1.	Seleciona-se uma das classes inferiores ' + 
				'"Importação com Delimitador" ou "Importação Posicional", conforme o arquivo a ser ' + 
				'tratado;'#13#10'2.	Executa-se o método de classe "Gerar Modelo", onde será gerado um ' + 
				'"Modelo de Importação", baseado na classe solicitada. Maiores informações consultar a ' + 
				'documentação do Método;'#13#10'3.	Ajusta-se o "Modelo de Importação" gerado, ' + 
				'personalizando-o ao arquivo;'#13#10'4.	Neste momento o arquivo já estará pronto para ser ' + 
				'importado, devendo ser invocada a importação através de "Tarefas", selecionando ' + 
				'"Importar Arquivo do Tipo Posicional" ou "Importar Arquivo do Tipo Delimitado" e ' + 
				'escolhendo o "Modelo de Importação" previamente gerado.', '_Administration');
    AddMetadata(1, '', '', T_Classe, '');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, 'Formato Data/Hora', '');
    AddMetadata(3, '', '');
    AddMetadata(4, '', '', 'File');
    AddConstraints(4, [NOTNULL]);
    AddMetadata(5, 'Modelo', '');
    AddConstraints(5, [READONLY]);
    AddMethod(0, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(string), _stINDEX);
  end;
end;

procedure Init_Geral; begin
  with Prevalence.Metadata('T_Geral') do begin
    InheritMetadata(1);
    AddMetadata(0, 'Rotinas Gerais', '', '_Administration');
    AddMethod(0, 'CheckSort', 'Verifica Ordenação das Listas', '', '', mkProcedure, 0, nil, _stSTATEMACHINE);
  end;
end;

procedure Init_Grupo; begin
  with Prevalence.Metadata('T_Grupo') do begin
    InheritMetadata(2);
    AddMetadata(0, 'Grupo', '', '_Security');
    AddClassTags('PropOrder', 'Identificador,Nome,AliasNome,Dominio,Ativo,Packages,CanDelegate');
    AddMetadata(1, 'Pendências', '', T_Pendency, 'Delegate');
    AddMetadata(2, 'Domínio', '', T_Dominio, 'Grupos');
    AddConstraints(2, [NOTNULL]);
    AddMetadata(3, '', '', T_Pendency, 'AssignedProfile');
    AddMetadata(4, '', '', T_Grupo, 'CanDelegate');
    AddMetadata(5, 'Delegação permitida', '', T_Grupo, 'Profile');
    AddMetadata(6, '', '', T_PermissaoPackage, 'Grupo');
    AddConstraints(6, [COMPOSITE]);
    AddMetadata(7, 'hidden', '');
    AddConstraints(7, [READONLY]);
    AddMetadata(8, 'Alias', '');
    AddMetadata(9, '', '');
    AddMetadata(10, '', 'Nome do Grupo no sistema Operacional');
    AddMethod(0, 'PorIdentificador', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
    AddMethod(1, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_ImportaPropriedade; begin
  with Prevalence.Metadata('T_ImportaPropriedade') do begin
    InheritMetadata(2);
    AddMetadata(0, 'hidden', '', '_Administration');
    AddMetadata(1, '', '', T_Propriedade, '');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, 'hidden', '');
    AddConstraints(2, [READONLY]);
    AddMetadata(3, 'Lista Chave (para Associação ou Referência)', '');
    AddConstraints(3, [CHECK]);
    AddOldName(3, 'ListaChave');
    AddMetadata(4, 'Tipo do Elemento', '');
    AddConstraints(4, [READONLY]);
    AddMetadata(5, 'Número do Elemento', '');
    AddMethod(0, 'PorPropNome', 'hidden', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
    AddMethod(1, 'PorElemento', '', '', '', mkFunction, 0, TypeInfo(Integer), _stINDEX);
  end;
end;

procedure Init_Log; begin
  with Prevalence.Metadata('T_Log') do begin
    InheritMetadata(0);
    AddMetadata(0, '', '', '_Workflow');
    AddMetadata(1, 'Descrição', '', '.{0,255}');
    AddConstraints(1, [READONLY]);
    AddMetadata(2, 'Data / Hora', '');
    AddConstraints(2, [READONLY]);
  end;
end;

procedure Init_Metodo; begin
  with Prevalence.Metadata('T_Metodo') do begin
    InheritMetadata(4);
    AddMetadata(0, 'hidden', '', '_Security');
    AddClassTags('PropOrder', 'Classe,AliasNome,Tipo,Nome,Bloqueado,PermissoesMetodos');
    AddMetadata(1, 'Visão', '', T_PermissaoView, 'View');
    AddMetadata(2, '', '', T_Classe, 'Metodos');
    AddConstraints(2, [NOTNULL]);
    AddMetadata(3, 'Permissões de Métodos', '', T_PermissaoMetodo, 'Metodo');
    AddConstraints(3, [COMPOSITE]);
    AddUnidirectional(T_Run, 'Method', [NOTNULL]);
    AddMetadata(4, 'hidden', '');
    AddConstraints(4, [READONLY]);
    AddMetadata(5, '', '');
    AddMetadata(8, 'Alias', '');
    AddMethod(0, 'PorIdentificador', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
    AddMethod(1, 'PorAlias', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
    AddMethod(2, 'PorView', 'PorVisão', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
    AddMethod(3, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_Ocupacao; begin
  with Prevalence.Metadata('T_Ocupacao') do begin
    InheritMetadata(0);
    AddMetadata(0, 'hidden', '', '_Administration');
  end;
end;

procedure Init_OcupacaoAssociacao; begin
  with Prevalence.Metadata('T_OcupacaoAssociacao') do begin
    InheritMetadata(0);
    AddMetadata(0, 'hidden', '', '_Administration');
    AddMetadata(1, '', '', T_OcupacaoLista, 'Associacoes');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, 'Memória', '');
    AddConstraints(2, [READONLY]);
    AddMetadata(5, 'Associação', '');
  end;
end;

procedure Init_Package; begin
  with Prevalence.Metadata('T_Package') do begin
    InheritMetadata(1);
    AddMetadata(0, 'hidden', '', '_Security');
    AddMetadata(1, 'Permissão de Package', '', T_PermissaoPackage, 'Package');
    AddConstraints(1, [COMPOSITE]);
    AddMetadata(2, '', '', T_Classe, 'Package');
    AddConstraints(2, [COMPOSITE]);
    AddMetadata(3, 'Ocupações', '', T_OcupacaoPackage, 'Package');
    AddMethod(0, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_PermissaoClasse; begin
  with Prevalence.Metadata('T_PermissaoClasse') do begin
    InheritMetadata(0);
    AddMetadata(0, 'hidden', '', '_Security');
    AddClassTags('PropOrder', 'Classe,Permissoes,View,Parametros,PermissaoPackage');
    AddMetadata(1, 'Permissão de Package', '', T_PermissaoPackage, 'PermissoesClasses');
    AddMetadata(2, '', '', T_Classe, 'PermissoesClasses');
    AddConstraints(2, [NOTNULL]);
    AddMetadata(3, 'Permissões', '');
  end;
end;

procedure Init_PermissaoMetodo; begin
  with Prevalence.Metadata('T_PermissaoMetodo') do begin
    InheritMetadata(0);
    AddMetadata(0, 'hidden', '', '_Security');
    AddClassTags('PropOrder', 'Metodo,Permissao,PermissaoPackage');
    AddMetadata(1, 'Permissão de Package', '', T_PermissaoPackage, 'PermissoesMetodos');
    AddMetadata(2, 'Método', '', T_Metodo, 'PermissoesMetodos');
    AddConstraints(2, [NOTNULL]);
    AddMetadata(3, 'Permissão de Execução', '');
  end;
end;

procedure Init_PermissaoPackage; begin
  with Prevalence.Metadata('T_PermissaoPackage') do begin
    InheritMetadata(0);
    AddMetadata(0, 'Permissão de Package', '', '_Security');
    AddClassTags('PropOrder', 'Grupo,Package,Ativa,PermissaoClasse,PermissaoPropriedade,PermissaoMetodo,PermissoesClasses,PermissoesPropriedades,PermissoesMetodos,PermissoesViews');
    AddMetadata(1, 'Grupo Associado', '', T_Grupo, 'Packages');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, 'Permissões de Propriedades', '', T_PermissaoPropriedade, 'PermissaoPackage');
    AddConstraints(2, [COMPOSITE]);
    AddMetadata(3, 'Visões', '', T_PermissaoView, 'PermissaoPackage');
    AddConstraints(3, [COMPOSITE]);
    AddMetadata(4, '', '', T_Package, 'PermissaoPackage');
    AddMetadata(5, 'Permissões de Métodos', '', T_PermissaoMetodo, 'PermissaoPackage');
    AddConstraints(5, [COMPOSITE]);
    AddMetadata(6, 'Permissões de Classes', '', T_PermissaoClasse, 'PermissaoPackage');
    AddConstraints(6, [COMPOSITE]);
    AddMetadata(7, 'Permissão Default dos Métodos', '');
    AddMetadata(8, 'Permissão Default das Propriedades', '');
    AddMetadata(9, 'Permissao Default das Classes', '');
    AddMetadata(10, '', '');
  end;
end;

procedure Init_PermissaoPropriedade; begin
  with Prevalence.Metadata('T_PermissaoPropriedade') do begin
    InheritMetadata(0);
    AddMetadata(0, 'hidden', '', '_Security');
    AddClassTags('PropOrder', 'Propriedade,Permissao,PermissaoPackage');
    AddMetadata(1, '', '', T_Propriedade, 'PermissoesPropriedades');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, 'Permissão de Package', '', T_PermissaoPackage, 'PermissoesPropriedades');
    AddMetadata(3, 'Permissão', '');
  end;
end;

procedure Init_PermissaoView; begin
  with Prevalence.Metadata('T_PermissaoView') do begin
    InheritMetadata(0);
    AddMetadata(0, 'hidden', '', '_Security');
    AddClassTags('PropOrder', 'View,PermissaoPackage,Parametro, PermissaoVerdadeiro, PermissaoFalso');
    AddMetadata(1, '', '', T_Metodo, 'PermissoesView');
    AddConstraints(1, [NOTNULL, ASSOCIATIONCONSTRAINT]);
    AddMetadata(2, '', '', T_PermissaoPackage, 'PermissoesViews');
    AddMetadata(3, 'Permissão se Falso', '');
    AddMetadata(4, 'Permissão se Verdadeiro', '');
    AddMetadata(5, 'Parâmetro', '');
  end;
end;

procedure Init_Propriedade; begin
  with Prevalence.Metadata('T_Propriedade') do begin
    InheritMetadata(2);
    AddMetadata(0, 'hidden', '', '_Security');
    AddClassTags('PropOrder', 'Classe,AliasNome,Nome,Tipo,ReadOnly,PermissoesPropriedades');
    AddMetadata(1, 'Permissões de Propriedades', '', T_PermissaoPropriedade, 'Propriedade');
    AddConstraints(1, [COMPOSITE]);
    AddMetadata(2, '', '', T_Classe, 'Propriedades');
    AddConstraints(2, [NOTNULL]);
    AddUnidirectional(T_ImportaPropriedade, 'Propriedade', [NOTNULL]);
    AddMetadata(4, 'Posição na Classe', '');
    AddMetadata(7, 'Alias', '');
    AddMethod(0, 'PorAlias', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
    AddMethod(1, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_Run; begin
  with Prevalence.Metadata('T_Run') do begin
    InheritMetadata(0);
    AddMetadata(0, '', '', '_Workflow');
    AddMetadata(1, '', '', T_Log, '');
    AddMetadata(2, 'Método', '', T_Metodo, '');
    AddConstraints(2, [NOTNULL]);
    AddMetadata(3, 'Em execução', '');
    AddConstraints(3, [READONLY]);
    AddMetadata(4, 'Log', 'Definir se as ocorrências das execuções do método agendado devem ser armazenados.' +
				''#13#10'"None" define para não armazenar,'#13#10'"Single" define para armazenar somente a ' +
				'última'#13#10'"All" define para armazenar todas execuções.');
    AddMetadata(5, 'Parâmetros', 'Fornecer os parâmetros para a execução do método, caso existam.');
  end;
end;

procedure Init_Sequence; begin
  with Prevalence.Metadata('T_Sequence') do begin
    InheritMetadata(1);
    AddMetadata(0, 'hidden', '', '_Administration');
    AddMethod(0, 'ByName', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_Servico; begin
  with Prevalence.Metadata('T_Servico') do begin
    InheritMetadata(1);
    AddMetadata(0, 'Serviços disparados pelo usuário', '', '_Security');
    AddMetadata(1, 'Sessão', '', T_Sessao, 'Servicos');
    AddConstraints(1, [NOTNULL]);
    AddMethod(0, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_Sessao; begin
  with Prevalence.Metadata('T_Sessao') do begin
    InheritMetadata(3);
    AddMetadata(0, 'Sessão', '', '_Security');
    AddMetadata(1, 'Serviços', '', T_Servico, 'Sessao');
    AddMetadata(2, '', '', T_Aud_Criterio, 'Sessao');
    AddMetadata(3, 'Método Ativo no Momento', '');
    AddConstraints(3, [READONLY]);
    AddMetadata(4, 'Estação', '');
    AddConstraints(4, [READONLY]);
    AddMetadata(5, 'Data/Hora do Logon', '');
    AddConstraints(5, [READONLY]);
    AddMetadata(6, 'Usuário', '');
    AddConstraints(6, [READONLY]);
    AddMethod(0, 'TerminaSessao', 'Termina Sessão', '', '', mkProcedure, 0, nil, _stSTATEMACHINE);
    AddMethod(1, 'PorUsuario', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
    AddMethod(2, 'PorThread', 'hidden', '', '', mkFunction, 0, TypeInfo(Int64), _stINDEX);
  end;
end;

procedure Init_UsuarioBloqueado; begin
  with Prevalence.Metadata('T_UsuarioBloqueado') do begin
    InheritMetadata(1);
    AddMetadata(0, 'Usuário Bloqueado', '', '_Security');
    AddMetadata(1, 'Domínio', '', T_Dominio, '');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, '', '');
    AddConstraints(2, [READONLY]);
    AddMetadata(4, 'Data/Hora', '');
    AddConstraints(4, [READONLY]);
    AddMetadata(5, 'Usuário Bloqueado', '');
    AddMetadata(6, '', '');
    AddConstraints(6, [READONLY]);
    AddMethod(0, 'PorIdentificador', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_ExpPropriedade; begin
  with Prevalence.Metadata('T_ExpPropriedade') do begin
    InheritMetadata(0);
    AddMetadata(0, 'hidden', '', '_Administration');
    AddMetadata(1, '', '', T_Exporta, 'Propriedades');
    AddConstraints(1, [NOTNULL]);
    AddOverride('ListaElementoChave', 'Propriedade Chave (para Referência ou Associação)', '', '', '', []);
  end;
end;

procedure Init_Exporta; begin
  with Prevalence.Metadata('T_Exporta') do begin
    InheritMetadata(3);
    AddMetadata(0, 'hidden', '', '_Administration');
    AddMetadata(1, '', '', T_ExpPropriedade, 'Exporta');
    AddConstraints(1, [NOTNULL, COMPOSITE]);
    AddOverride('Arquivo', 'Arquivo de Saida', '', 'SaveFile', '', []);
    AddMethod(0, 'CustomizarModelo', 'ExtPascalUML|Exportação|Customizar Modelo Gerado', '', '', mkClassProcedure, 0, nil, _stWORKFLOW);
    AddMethod(1, 'Exportar', 'ExtPascalUML|Exportação|Exportar', '', '', mkClassProcedure, 1, nil, _stWORKFLOW);
      AddParam(0, 'Modelo', '', '', 'T_Exporta(0)', pfConst, TypeInfo(T_Exporta));
    AddMethod(2, 'GerarModelo', 'ExtPascalUML|Exportação|Gerar Modelo de Exportação', '', '', mkClassProcedure, 1, nil, _stWORKFLOW);
      AddParam(0, 'Modelo', '', '', '', pfConst, TypeInfo(String));
  end;
end;

procedure Init_Importa; begin
  with Prevalence.Metadata('T_Importa') do begin
    InheritMetadata(3);
    AddMetadata(0, 'hidden', 'Esta Classe permite a definição de importações|ExtPascalUML permite que sejam efetuadas ' +
				'importações de arquivos texto para dentro da Prevalência.'#13#10'As importações são ' +
				'tratadas de duas formas:'#13#10'1. arquivos com delimitadores de campo;'#13#10'2. ' +
				'arquivos com tratamento posicional.'#13#10'A forma de utilização da importação no ExtPascalUML ' +
				'segue os seguintes passos:'#13#10'1.	Seleciona-se uma das classes inferiores ' +
				'"Importação com Delimitador" ou "Importação Posicional", conforme o arquivo a ser ' +
				'tratado;'#13#10'2.	Executa-se o método de classe "Gerar Modelo", onde será gerado um ' +
				'"Modelo de Importação", baseado na classe solicitada. Maiores informações consultar a ' +
				'documentação do Método;'#13#10'3.	Ajusta-se o "Modelo de Importação" gerado, ' +
				'personalizando-o ao arquivo;'#13#10'4.	Neste momento o arquivo já estará pronto para ser ' +
				'importado, devendo ser invocada a importação através de "Tarefas", selecionando ' +
				'"Importar Arquivo do Tipo Posicional" ou "Importar Arquivo do Tipo Delimitado" e ' +
				'escolhendo o "Modelo de Importação" previamente gerado.', '_Administration');
    AddMetadata(1, 'Linhas Saltar no Início', '');
    AddMetadata(2, 'Linhas a Ignorar no Fim', '');
    AddMetadata(3, 'Número Elemento Chave', '');
    AddMetadata(4, 'hidden', '');
    AddConstraints(4, [READONLY]);
    AddMetadata(5, 'Se Objeto Existir', '');
    AddMetadata(6, 'Lista para Importação', '');
    AddMethod(0, 'CustomizarModelo', 'ExtPascalUML|Importação|Customizar Modelo Gerado', '', '', mkClassProcedure, 1, nil, _stWORKFLOW);
      AddParam(0, 'ImportacaoDelimitada', '', '', 'Boolean(0)', pfConst, TypeInfo(Boolean));
    AddMethod(1, 'Importar', 'ExtPascalUML|Importação|Importar', '', '', mkClassProcedure, 1, nil, _stWORKFLOW);
      AddParam(0, 'Modelo', '', '', 'T_Importa(0)', pfConst, TypeInfo(T_Importa));
    AddMethod(2, 'GerarModelo', 'ExtPascalUML|Importação|Gerar Modelo de Importação', '', '', mkClassProcedure, 2, nil, _stWORKFLOW);
      AddParam(0, 'Modelo', '', '', '', pfConst, TypeInfo(String));
      AddParam(1, 'ImportacaoDelimitada', '', '', 'Boolean(0)', pfConst, TypeInfo(Boolean));
  end;
end;

procedure Init_OcupacaoClasse; begin
  with Prevalence.Metadata('T_OcupacaoClasse') do begin
    InheritMetadata(1);
    AddMetadata(0, 'hidden', '', '_Administration');
    AddMetadata(1, '', '', T_Classe, 'Ocupacoes');
    AddMetadata(2, '', '', T_OcupacaoPackage, 'Classes');
    AddMetadata(3, 'Ocupações', '', T_OcupacaoLista, 'Classe');
    AddMetadata(4, '', '');
    AddConstraints(4, [READONLY]);
    AddMethod(0, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
  end;
end;

procedure Init_OcupacaoLista; begin
  with Prevalence.Metadata('T_OcupacaoLista') do begin
    InheritMetadata(0);
    AddMetadata(0, 'hidden', '', '_Administration');
    AddMetadata(1, '', '', T_OcupacaoClasse, 'Ocupacoes');
    AddMetadata(2, '', '', T_OcupacaoAssociacao, 'Lista');
    AddMetadata(3, 'Memória', '');
    AddConstraints(3, [READONLY]);
    AddMetadata(4, '', '');
    AddConstraints(4, [READONLY]);
    AddMetadata(5, '', '');
    AddConstraints(5, [READONLY]);
    AddMetadata(6, '', '');
    AddConstraints(6, [READONLY]);
    AddMetadata(7, 'hidden', '');
  end;
end;

procedure Init_OcupacaoPackage; begin
  with Prevalence.Metadata('T_OcupacaoPackage') do begin
    InheritMetadata(2);
    AddMetadata(0, 'hidden', '', '_Administration');
    AddMetadata(1, '', '', T_Package, 'Ocupacoes');
    AddMetadata(2, '', '', T_OcupacaoClasse, 'Package');
    AddMethod(0, 'PorNome', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
    AddMethod(1, 'InicializaOcupacao', 'hidden', '', '', mkClassProcedure, 0, nil, _stSTATEMACHINE);
  end;
end;

procedure Init_Pendency; begin
  with Prevalence.Metadata('T_Pendency') do begin
    InheritMetadata(3);
    AddMetadata(0, 'Pendência', '', '_Workflow');
    AddMetadata(1, '', '', T_PendencyTimeoutTask, 'Pendency');
    AddConstraints(1, [COMPOSITE]);
    AddMetadata(2, 'Delegar para', '', T_Grupo, 'Pendencies');
    AddConstraints(2, [CHECK, ASSOCIATIONCONSTRAINT]);
    AddMetadata(3, '', '', T_Grupo, 'AssignedPendencies');
    AddMetadata(4, 'Fluxo', '');
    AddConstraints(4, [READONLY]);
    AddMetadata(5, 'Identificação', '', '.{0,127}');
    AddConstraints(5, [READONLY]);
    AddMetadata(6, 'Pendência', '');
    AddConstraints(6, [READONLY]);
    AddMetadata(7, 'Usuário', '', 'Values GetDelegateUsers');
    AddConstraints(7, [ENABLED,CHECK]);
    AddMetadata(8, 'Prazo', '');
    AddConstraints(8, [READONLY]);
    AddMetadata(9, 'Data / Hora', '');
    AddConstraints(9, [READONLY]);
    AddMetadata(10, 'Classe', '');
    AddConstraints(10, [READONLY]);
    AddMethod(0, 'GetDelegateUsers', 'hidden', '', '', mkFunction, 0, TypeInfo(String), _stSTATEMACHINE);
    AddMethod(1, 'ByRunObject', '', '', '', mkFunction, 0, TypeInfo(String), _stINDEX);
    AddMethod(2, 'ByPriority', '', '', '', mkFunction, 0, TypeInfo(Double), _stINDEX);
  end;
end;

procedure Init_PropriedadeDelimitado; begin
  with Prevalence.Metadata('T_PropriedadeDelimitado') do begin
    InheritMetadata(0);
    AddMetadata(0, 'hidden', '', '_Administration');
    AddMetadata(1, '', '', T_ImportaDelimitado, 'Propriedades');
    AddConstraints(1, [NOTNULL]);
    AddOverride('ListaElementoChave', 'Lista Chave (para Referência ou Associação)', '', '', '', []);
  end;
end;

procedure Init_PropriedadePosicional; begin
  with Prevalence.Metadata('T_PropriedadePosicional') do begin
    InheritMetadata(0);
    AddMetadata(0, 'hidden', '', '_Administration');
    AddMetadata(1, '', '', T_ImportaPosicional, 'Propriedades');
    AddConstraints(1, [NOTNULL]);
    AddMetadata(2, 'Posições', '');
    AddConstraints(2, [CHECK]);
    AddMetadata(3, 'Posição Inicial', '');
    AddConstraints(3, [CHECK]);
    AddOverride('ListaElementoChave', 'Lista Chave (para Referência ou Associação)', '', '', '', []);
  end;
end;

procedure Init_Task; begin
  with Prevalence.Metadata('T_Task') do begin
    InheritMetadata(3);
    AddMetadata(0, 'Tarefa', '', '_Workflow');
    AddClassTags('PropOrder', 'Method,Parameters,Running,Active,NextStart,LastRan,SpecificDateTime,Month,Day,Weekday,Hour,Minute,Periodicity,PeriodicityUnit,Start,RunUntil,LogKind,Log');
    AddMetadata(1, 'Próxima execução', '');
    AddConstraints(1, [READONLY]);
    AddMetadata(2, 'Última execução', '');
    AddConstraints(2, [READONLY]);
    AddMetadata(3, 'Executar até', 'Definir até quando o agendamento do método deve estar ativo. Caso fornecido, o ' + 
				'agendamento não será mais executado após esta data/hora.');
    AddMetadata(4, 'Executar a partir de', 'Definir a partir de quando o agendamento do método deve ser iniciado.');
    AddMetadata(5, 'Unidade', '');
    AddConstraints(5, [VISIBLE]);
    AddMetadata(6, 'Periodicidade', '');
    AddConstraints(6, [VISIBLE]);
    AddMetadata(7, 'Ativa', '');
    AddMethod(0, 'NewTask', 'ExtPascalUML|Agendamento de tarefas|Agendar tarefa', '', '', mkClassProcedure, 0, nil, _stWIZARD);
    AddMethod(1, 'Scheduler', 'ExtPascalUML|Agendamento de tarefas|Gerenciar', '', '', mkClassProcedure, 0, nil, _stWIZARD);
    AddMethod(2, 'ByNextStart', 'Início', '', '', mkFunction, 0, TypeInfo(DateTime), _stINDEX);
  end;
end;

procedure Init_ImportaDelimitado; begin
  with Prevalence.Metadata('T_ImportaDelimitado') do begin
    InheritMetadata(0);
    AddMetadata(0, 'hidden', 'Modelo de Importação|'#13#10'Geração do Modelo de Importação'#13#10'Este método recebe os ' + 
				'seguintes parâmetros:'#13#10'1.	Nome do Modelo de Importação:'#13#10'Este parâmetro é um ' + 
				'identificador do modelo que será gerado.'#13#10'2.	Classe'#13#10'Neste parâmetro deverá ' + 
				'ser selecionada a Classe do Modelo que sofrerá a importação das informações.'#13#10'3.	É ' + 
				'Importação do Tipo Delimitada?'#13#10'Neste parâmetro, quando informado "Sim" será gerado ' + 
				'um modelo para importação do tipo delimitada, e quando informado "Não", será gerado um ' + 
				'modelo para importação do tipo posicional.'#13#10'Adequação do Modelo de ' + 
				'Importação'#13#10'Após execução deste método será gerado um "Modelo de Importação" que ' + 
				'necessitará ser adequado ao arquivo. A adequação será abortada em 2 aspectos:'#13#10'1.	' + 
				'Adequação dos dados de Importação do Tipo Delimitada:'#13#10'A adequação consiste alterar ' + 
				'os valores default gerados:'#13#10'a)	Arquivo de Entrada:'#13#10'É o arquivo que será ' + 
				'lido na importação. É importante lembrar que a localização é em relação ao local onde ' + 
				'está sendo executado o Server.'#13#10'b)	Lista para Importação'#13#10'Quando é efetuada ' + 
				'uma importação, pode-se informar a lista em que será testada a já existência do objeto na ' + 
				'Prevalência. Por exemplo, "PorCodigo". Se este atributo não for informado não será ' + 
				'testada a existência do objeto na Prevalência.'#13#10'c)	Se o Registro já ' + 
				'Existir'#13#10'Este valor indica o comportamento da importação quando o objeto já existir ' + 
				'na Prevalência. Os valor possíveis são "Atualiza", onde as propriedades do objeto serão ' + 
				'atualizadas pelos valores recebidos no arquivo; ou "Ignora", onde o registro do arquivo ' + 
				'será ignorado e o objeto na Prevalência não sofrerá atualização.'#13#10'd)	Balanced ' + 
				'Line'#13#10'Não implementado nesta versão'#13#10'e)	Log'#13#10'Sendo informado "SIM", ' + 
				'serão geradas mensagens para acompanhamento durante a importação. Atenção: a geração de ' + 
				'Log reduz a performance da importação.'#13#10'f)	Formato da Data/Hora'#13#10'Deverá ser ' + 
				'informado o formato da data/hora existente no arquivo. Este atributo só será considerado ' + 
				'caso a Classe que receberá as informações possua propriedade deste tipo.'#13#10'g)	Número ' + 
				'do Elemento Chave'#13#10'O Modelo gerado possui elementos, conforme veremos em seguida. ' + 
				'Este valor indica qual elemento será utilizado como chave para teste de existência do ' + 
				'objeto na Prevalência. Não sendo informado, não será testada a existência.'#13#10'h)	' + 
				'Linhas a Ignorar no Final do Arquivo'#13#10'Poderá ser informada a quantidade de linhas ' + 
				'que deverá ser ignorada no final do arquivo.'#13#10'i)	Linhas a Saltar no Início do ' + 
				'Arquivo'#13#10'Poderá ser informada a quantidade de linhas que deverá ser saltadas no ' + 
				'início do arquivo.'#13#10'j)	Delimitador'#13#10'Deverá ser informado o valor do ' + 
				'delimitador utilizado no arquivo. O Default é ‘,’, mas poderá ser utilizado qualquer ' + 
				'outro. Possui apenas 1 posição.'#13#10'k)	Propriedades geradas'#13#10'Campos:'#13#10'•	' + 
				'Número do Elemento: indica a posição do elemento no arquivo em relação ao delimitador. ' + 
				'Exemplo:'#13#10'AAAA,BBBB,CCCC'#13#10'AAAA é o elemento de número 1'#13#10'BBBB é o ' + 
				'elemento de número 2'#13#10'CCCC é o elemento de número 3'#13#10'•	Tipo da Propriedade: ' + 
				'não é alterável e serve para orientação da adequação da propriedade. Pode assumir os ' + 
				'seguintes valores: Atributo, Referência ou Associação.'#13#10'•	Lista Chave: esta ' + 
				'propriedade deverá ser informada apenas no caso de Elementos do tipo Referência ou ' + 
				'Associação e é utilizada com o mesmo propósito da propriedade Lista definida acima. O ' + 
				'valor recuperado no Elemento será utilizado para acessar a Associação ou a Referência, ' + 
				'pela "Lista Chave" informada e assim criar a ligação.'#13#10'•	Propriedade: é a ' + 
				'propriedade existente na classe que deverá ser relacionada com o Elemento do arquivo.' + 
				''#13#10'A adequação das propriedades poderá ser feita pelos seguintes motivos:'#13#10'•	A ' + 
				'ordem da propriedade não é a mesma da existente no arquivo:'#13#10'o	Neste caso deve-se ' + 
				'atribuir novos valores para o atributo Elemento.'#13#10'•	Não interessa importar ou não ' + 
				'existe no arquivo a Propriedade existente na Classe:'#13#10'o	Exclui-se a o ' + 
				'Elemento'#13#10'•	Não desejo importar o Elemento Y:'#13#10'o	Não é exigida seqüência sem ' + 
				'"buracos" nos Elementos, já que este é um reflexo do formato do arquivo. Exemplificando, ' + 
				'é possível no Modelo de importação existir os elementos 1, 2, 4 e 5, indicando que o ' + 
				'elemento 3 do arquivo será ignorado.'#13#10'•	Existência de Referência e ' + 
				'Associação'#13#10'o	Deverá ser informada a "Lista Chave". Por exemplo, PorCodigo.', '_Administration');
    AddMetadata(1, '', '', T_PropriedadeDelimitado, 'ImportaDelimitado');
    AddConstraints(1, [NOTNULL, COMPOSITE]);
    AddMetadata(2, '', '', '.');
    AddConstraints(2, [NOTNULL,CHECK]);
  end;
end;

procedure Init_ImportaPosicional; begin
  with Prevalence.Metadata('T_ImportaPosicional') do begin
    InheritMetadata(0);
    AddMetadata(0, 'hidden', 'Modelo de Importação|'#13#10'2.	Adequação dos dados de Importação do Tipo ' + 
				'Posicional:'#13#10'A adequação consiste alterar os valores default gerados:'#13#10'a)	' + 
				'Arquivo de Entrada:'#13#10'É o arquivo que será lido na importação. É importante lembrar ' + 
				'que a localização é em relação ao local onde está sendo executado o Server.'#13#10'b)	' + 
				'Lista para Importação'#13#10'Quando é efetuada uma importação, pode-se informar a lista ' + 
				'em que será testada a já existência do objeto na Prevalência. Por exemplo, "PorCodigo". ' + 
				'Se este atributo não for informado não será testada a existência do objeto na Prevalência.' + 
				''#13#10'c)	Se o Registro já Existir'#13#10'Este valor indica o comportamento da ' + 
				'importação quando o objeto já existir na Prevalência. Os valor possíveis são "Atualiza", ' + 
				'onde as propriedades do objeto serão atualizadas pelos valores recebidos no arquivo; ou ' + 
				'"Ignora", onde o registro do arquivo será ignorado e o objeto na Prevalência não sofrerá ' + 
				'atualização.'#13#10'd)	Balanced Line'#13#10'Não implementado nesta versão'#13#10'e)	' + 
				'Log'#13#10'Sendo informado "SIM", serão geradas mensagens para acompanhamento durante a ' + 
				'importação. Atenção: a geração de Log reduz a performance da importação.'#13#10'f)	' + 
				'Formato da Data/Hora'#13#10'Deverá ser informado o formato da data/hora existente no ' + 
				'arquivo. Este atributo só será considerado caso a Classe que receberá as informações ' + 
				'possua propriedade deste tipo.'#13#10'g)	Número do Elemento Chave'#13#10'O Modelo gerado ' + 
				'possui elementos, conforme veremos em seguida. Este valor indica qual elemento será ' + 
				'utilizado como chave para teste de existência do objeto na Prevalência. Não sendo ' + 
				'informado, não será testada a existência.'#13#10'h)	Linhas a Ignorar no Final do ' + 
				'Arquivo'#13#10'Poderá ser informada a quantidade de linhas que deverá ser ignorada no ' + 
				'final do arquivo.'#13#10'i)	Linhas a Saltar no Início do Arquivo'#13#10'Poderá ser ' + 
				'informada a quantidade de linhas que deverá ser saltadas no início do arquivo.'#13#10'j)	' + 
				'Propriedades geradas'#13#10'Campos:'#13#10'Número do Elemento: indica a ordem do ' + 
				'elemento'#13#10'•	Tipo da Propriedade: não é alterável e serve para orientação da ' + 
				'adequação da propriedade. Pode assumir os seguintes valores: Atributo, Referência ou ' + 
				'Associação.'#13#10'•	Lista Chave: esta propriedade deverá ser informada apenas no caso de ' + 
				'Elementos do tipo Referência ou Associação e é utilizada com o mesmo propósito da ' + 
				'propriedade Lista definida acima. O valor recuperado no Elemento será utilizado para ' + 
				'acessar a Associação ou a Referência, pela "Lista Chave" informada e assim criar a ' + 
				'ligação.'#13#10'•	Propriedade: é a propriedade existente na classe que deverá ser ' + 
				'relacionada com o Elemento do arquivo.'#13#10'•	Posição Inicial: é a posição do elemento ' + 
				'no registro do arquivo'#13#10'•	Quantidade de posições: é a quantidade de posições do ' + 
				'elemento a partir da posição inicial no registro do arquivo'#13#10'A adequação das ' + 
				'propriedades poderá/deverá ser feita pelos seguintes motivos:'#13#10'•	Não interessa ' + 
				'importar ou não existe no arquivo a Propriedade existente na Classe:'#13#10'o	Exclui-se a ' + 
				'o Elemento'#13#10'•	Existência de Referência e Associação'#13#10'o	Deverá ser informada a ' + 
				'"Lista Chave". Por exemplo, PorCodigo.'#13#10'•	Informar a Posição Inicial e a Quantidade ' + 
				'de posições para cada um dos elementos', '_Administration');
    AddMetadata(1, '', '', T_PropriedadePosicional, 'ImportaPosicional');
    AddConstraints(1, [NOTNULL, COMPOSITE]);
  end;
end;

procedure Init_PendencyTimeoutTask; begin
  with Prevalence.Metadata('T_PendencyTimeoutTask') do begin
    InheritMetadata(0);
    AddMetadata(0, '', '', '_Workflow');
    AddMetadata(1, '', '', T_Pendency, 'TimeOut');
    AddConstraints(1, [NOTNULL]);
  end;
end;

procedure InitepModel; begin
  InitLists;
  Init_Aud_Arquivo;
  Init_Aud_Classe;
  Init_Aud_Criterio;
  Init_Aud_Login;
  Init_Aud_Operacao;
  Init_Aud_Origem;
  Init_Aud_Package;
  Init_Aud_UpdObjeto;
  Init_Classe;
  Init_Dominio;
  Init_ExportaImporta;
  Init_Geral;
  Init_Grupo;
  Init_ImportaPropriedade;
  Init_Log;
  Init_Metodo;
  Init_Ocupacao;
  Init_OcupacaoAssociacao;
  Init_Package;
  Init_PermissaoClasse;
  Init_PermissaoMetodo;
  Init_PermissaoPackage;
  Init_PermissaoPropriedade;
  Init_PermissaoView;
  Init_Propriedade;
  Init_Run;
  Init_Sequence;
  Init_Servico;
  Init_Sessao;
  Init_UsuarioBloqueado;
  Init_ExpPropriedade;
  Init_Exporta;
  Init_Importa;
  Init_OcupacaoClasse;
  Init_OcupacaoLista;
  Init_OcupacaoPackage;
  Init_Pendency;
  Init_PropriedadeDelimitado;
  Init_PropriedadePosicional;
  Init_Task;
  Init_ImportaDelimitado;
  Init_ImportaPosicional;
  Init_PendencyTimeoutTask;
end;

// Methods

function T_Aud_Arquivo.GetNome : String; begin Result := T_Aud_Arquivo(Prevalence.GetNewImage(Self, 2))._Nome end;

procedure T_Aud_Arquivo.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Arquivo(NewImage)._Nome <> Value then begin
        T_Aud_Arquivo(NewImage)._Nome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Arquivo.GetData : DateTime; begin Result := T_Aud_Arquivo(Prevalence.GetNewImage(Self, 3))._Data end;

procedure T_Aud_Arquivo.SetData(Value : DateTime); begin
  if Prevalence.IsInRecoverSnapShot then _Data := Value else begin
    if (_Data = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Arquivo(NewImage)._Data <> Value then begin
        T_Aud_Arquivo(NewImage)._Data := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Arquivo.GetStatus : T_Aud_ArquivoStatus; begin Result := T_Aud_Arquivo(Prevalence.GetNewImage(Self, 4))._Status end;

procedure T_Aud_Arquivo.SetStatus(Value : T_Aud_ArquivoStatus); begin
  if Prevalence.IsInRecoverSnapShot then _Status := Value else begin
    if (_Status = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Arquivo(NewImage)._Status <> Value then begin
        T_Aud_Arquivo(NewImage)._Status := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Aud_Arquivo.PorNome : String; begin
  Result := Nome
end;

// Cyclomatic Complexity: 1, Baixo
function T_Aud_Arquivo.PorStatus : word; begin
  Result := word(Status)
end;

// Cyclomatic Complexity: 1, Baixo
function T_Aud_Arquivo.PorData : DateTime; begin
  Result := Data
end;

{ T_Aud_ArquivoList }

class function T_Aud_ArquivoList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Arquivo
end;

procedure T_Aud_ArquivoList.Add(_Aud_Arquivo : T_Aud_Arquivo); begin
  inherited Add(TPrevalent(_Aud_Arquivo))
end;

procedure T_Aud_ArquivoList.Delete(_Aud_Arquivo : T_Aud_Arquivo); begin
  inherited Delete(TPrevalent(_Aud_Arquivo))
end;

function T_Aud_ArquivoList.Find(I : Integer) : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited Find(I))
end;

function T_Aud_ArquivoList.First : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited First)
end;

function T_Aud_ArquivoList.Last : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited Last)
end;

function T_Aud_ArquivoList.Near(I : Integer) : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited Near(I))
end;

function T_Aud_ArquivoList.Next(var _Aud_Arquivo : T_Aud_Arquivo) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Arquivo))
end;

function T_Aud_ArquivoList.Prior(var _Aud_Arquivo : T_Aud_Arquivo) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Arquivo))
end;

function T_Aud_ArquivoList.Get_Aud_Arquivo(I : Integer) : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(Objects[I])
end;

{ T_Aud_ArquivoPorNomeList }

class function T_Aud_ArquivoPorNomeList.GetKeyCode : pointer; begin
  Result := @T_Aud_Arquivo.PorNome
end;

class function T_Aud_ArquivoPorNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_Aud_ArquivoPorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Arquivo
end;

procedure T_Aud_ArquivoPorNomeList.Add(_Aud_Arquivo : T_Aud_Arquivo); begin
  inherited Add(TPrevalent(_Aud_Arquivo))
end;

procedure T_Aud_ArquivoPorNomeList.Delete(_Aud_Arquivo : T_Aud_Arquivo); begin
  inherited Delete(TPrevalent(_Aud_Arquivo))
end;

function T_Aud_ArquivoPorNomeList.Find(S : String) : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited Find(S))
end;

function T_Aud_ArquivoPorNomeList.First : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited First)
end;

function T_Aud_ArquivoPorNomeList.Last : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited Last)
end;

function T_Aud_ArquivoPorNomeList.Near(S : String) : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited Near(S))
end;

function T_Aud_ArquivoPorNomeList.Next(var _Aud_Arquivo : T_Aud_Arquivo) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Arquivo))
end;

function T_Aud_ArquivoPorNomeList.Prior(var _Aud_Arquivo : T_Aud_Arquivo) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Arquivo))
end;

function T_Aud_ArquivoPorNomeList.Get_Aud_Arquivo(I : Integer) : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(Objects[I])
end;

{ T_Aud_ArquivoPorStatusList }

class function T_Aud_ArquivoPorStatusList.GetKeyCode : pointer; begin
  Result := @T_Aud_Arquivo.PorStatus
end;

class function T_Aud_ArquivoPorStatusList.GetListType : TListType; begin
  Result := ltword
end;

class function T_Aud_ArquivoPorStatusList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Arquivo
end;

procedure T_Aud_ArquivoPorStatusList.Add(_Aud_Arquivo : T_Aud_Arquivo); begin
  inherited Add(TPrevalent(_Aud_Arquivo))
end;

procedure T_Aud_ArquivoPorStatusList.Delete(_Aud_Arquivo : T_Aud_Arquivo); begin
  inherited Delete(TPrevalent(_Aud_Arquivo))
end;

function T_Aud_ArquivoPorStatusList.Find(w : word) : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited Find(w))
end;

function T_Aud_ArquivoPorStatusList.First : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited First)
end;

function T_Aud_ArquivoPorStatusList.Last : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited Last)
end;

function T_Aud_ArquivoPorStatusList.Near(w : word) : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited Near(w))
end;

function T_Aud_ArquivoPorStatusList.Next(var _Aud_Arquivo : T_Aud_Arquivo) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Arquivo))
end;

function T_Aud_ArquivoPorStatusList.Prior(var _Aud_Arquivo : T_Aud_Arquivo) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Arquivo))
end;

function T_Aud_ArquivoPorStatusList.Get_Aud_Arquivo(I : Integer) : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(Objects[I])
end;

{ T_Aud_ArquivoPorDataList }

class function T_Aud_ArquivoPorDataList.GetKeyCode : pointer; begin
  Result := @T_Aud_Arquivo.PorData
end;

class function T_Aud_ArquivoPorDataList.GetListType : TListType; begin
  Result := ltDateTime
end;

class function T_Aud_ArquivoPorDataList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Arquivo
end;

procedure T_Aud_ArquivoPorDataList.Add(_Aud_Arquivo : T_Aud_Arquivo); begin
  inherited Add(TPrevalent(_Aud_Arquivo))
end;

procedure T_Aud_ArquivoPorDataList.Delete(_Aud_Arquivo : T_Aud_Arquivo); begin
  inherited Delete(TPrevalent(_Aud_Arquivo))
end;

function T_Aud_ArquivoPorDataList.Find(D : DateTime) : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited Find(D))
end;

function T_Aud_ArquivoPorDataList.First : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited First)
end;

function T_Aud_ArquivoPorDataList.Last : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited Last)
end;

function T_Aud_ArquivoPorDataList.Near(D : DateTime) : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(inherited Near(D))
end;

function T_Aud_ArquivoPorDataList.Next(var _Aud_Arquivo : T_Aud_Arquivo) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Arquivo))
end;

function T_Aud_ArquivoPorDataList.Prior(var _Aud_Arquivo : T_Aud_Arquivo) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Arquivo))
end;

function T_Aud_ArquivoPorDataList.Get_Aud_Arquivo(I : Integer) : T_Aud_Arquivo; begin
  Result := T_Aud_Arquivo(Objects[I])
end;

{ T_Aud_Classe }

procedure T_Aud_Classe.New; begin
  inherited;
  _Operacoes := T_Aud_ClasseOperacoesAssociation.Create(Self, 'Operacoes', true);
end;

procedure T_Aud_Classe.InternalFree; begin
  if _Operacoes <> nil then _Operacoes.InternalFree;
  inherited;
end;

function T_Aud_Classe.GetNome : String; begin Result := T_Aud_Classe(Prevalence.GetNewImage(Self, 2))._Nome end;

procedure T_Aud_Classe.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Classe(NewImage)._Nome <> Value then begin
        T_Aud_Classe(NewImage)._Nome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Aud_Classe.PorNome : String; begin
  Result := Nome
end;

{ T_Aud_ClasseList }

class function T_Aud_ClasseList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Classe
end;

procedure T_Aud_ClasseList.Add(_Aud_Classe : T_Aud_Classe); begin
  inherited Add(TPrevalent(_Aud_Classe))
end;

procedure T_Aud_ClasseList.Delete(_Aud_Classe : T_Aud_Classe); begin
  inherited Delete(TPrevalent(_Aud_Classe))
end;

function T_Aud_ClasseList.Find(I : Integer) : T_Aud_Classe; begin
  Result := T_Aud_Classe(inherited Find(I))
end;

function T_Aud_ClasseList.First : T_Aud_Classe; begin
  Result := T_Aud_Classe(inherited First)
end;

function T_Aud_ClasseList.Last : T_Aud_Classe; begin
  Result := T_Aud_Classe(inherited Last)
end;

function T_Aud_ClasseList.Near(I : Integer) : T_Aud_Classe; begin
  Result := T_Aud_Classe(inherited Near(I))
end;

function T_Aud_ClasseList.Next(var _Aud_Classe : T_Aud_Classe) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Classe))
end;

function T_Aud_ClasseList.Prior(var _Aud_Classe : T_Aud_Classe) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Classe))
end;

function T_Aud_ClasseList.Get_Aud_Classe(I : Integer) : T_Aud_Classe; begin
  Result := T_Aud_Classe(Objects[I])
end;

{ T_Aud_ClassePorNomeList }

class function T_Aud_ClassePorNomeList.GetKeyCode : pointer; begin
  Result := @T_Aud_Classe.PorNome
end;

class function T_Aud_ClassePorNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_Aud_ClassePorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Classe
end;

procedure T_Aud_ClassePorNomeList.Add(_Aud_Classe : T_Aud_Classe); begin
  inherited Add(TPrevalent(_Aud_Classe))
end;

procedure T_Aud_ClassePorNomeList.Delete(_Aud_Classe : T_Aud_Classe); begin
  inherited Delete(TPrevalent(_Aud_Classe))
end;

function T_Aud_ClassePorNomeList.Find(S : String) : T_Aud_Classe; begin
  Result := T_Aud_Classe(inherited Find(S))
end;

function T_Aud_ClassePorNomeList.First : T_Aud_Classe; begin
  Result := T_Aud_Classe(inherited First)
end;

function T_Aud_ClassePorNomeList.Last : T_Aud_Classe; begin
  Result := T_Aud_Classe(inherited Last)
end;

function T_Aud_ClassePorNomeList.Near(S : String) : T_Aud_Classe; begin
  Result := T_Aud_Classe(inherited Near(S))
end;

function T_Aud_ClassePorNomeList.Next(var _Aud_Classe : T_Aud_Classe) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Classe))
end;

function T_Aud_ClassePorNomeList.Prior(var _Aud_Classe : T_Aud_Classe) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Classe))
end;

function T_Aud_ClassePorNomeList.Get_Aud_Classe(I : Integer) : T_Aud_Classe; begin
  Result := T_Aud_Classe(Objects[I])
end;

{ T_Aud_Criterio }

procedure T_Aud_Criterio.New; begin
  inherited;
  _Operacoes := T_Aud_CriterioOperacoesAssociation.Create(Self, 'Operacoes', true);
  try _Sessao := _SessaoPorUsuarioList.Find(Browser.UserInfo.Username) except end; //*
  if Prevalence.IsInRecover then exit;
  try _DataGeracao := Now except end;
  try _DataInicio := Now - 30 except end;
  try _DataFim := Now except end;
  try _Operacao := _opAll except end;
end;

procedure T_Aud_Criterio.InternalFree; begin
  if _Operacoes <> nil then _Operacoes.InternalFree;
  inherited;
end;

function T_Aud_Criterio.GetDataGeracao : DateTime; begin Result := T_Aud_Criterio(Prevalence.GetNewImage(Self, 2))._DataGeracao end;

procedure T_Aud_Criterio.SetDataGeracao(Value : DateTime); begin
  if Prevalence.IsInRecoverSnapShot then _DataGeracao := Value else begin
    if (_DataGeracao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Criterio(NewImage)._DataGeracao <> Value then begin
        T_Aud_Criterio(NewImage)._DataGeracao := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Criterio.GetDataInicio : DateTime; begin Result := T_Aud_Criterio(Prevalence.GetNewImage(Self, 3))._DataInicio end;

procedure T_Aud_Criterio.SetDataInicio(Value : DateTime); begin
  if Prevalence.IsInRecoverSnapShot then _DataInicio := Value else begin
    if (_DataInicio = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Criterio(NewImage)._DataInicio <> Value then begin
        T_Aud_Criterio(NewImage)._DataInicio := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Criterio.GetDataFim : DateTime; begin Result := T_Aud_Criterio(Prevalence.GetNewImage(Self, 4))._DataFim end;

procedure T_Aud_Criterio.SetDataFim(Value : DateTime); begin
  if Prevalence.IsInRecoverSnapShot then _DataFim := Value else begin
    if (_DataFim = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Criterio(NewImage)._DataFim <> Value then begin
        T_Aud_Criterio(NewImage)._DataFim := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Criterio.GetOperacao : T_Operation; begin Result := T_Aud_Criterio(Prevalence.GetNewImage(Self, 5))._Operacao end;

procedure T_Aud_Criterio.SetOperacao(Value : T_Operation); begin
  if Prevalence.IsInRecoverSnapShot then _Operacao := Value else begin
    if (_Operacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Criterio(NewImage)._Operacao <> Value then begin
        T_Aud_Criterio(NewImage)._Operacao := Value;
        UpdateLog(5, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Criterio.GetUsuario : String; begin Result := T_Aud_Criterio(Prevalence.GetNewImage(Self, 6))._Usuario end;

procedure T_Aud_Criterio.SetUsuario(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Usuario := Value else begin
    if (_Usuario = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Criterio(NewImage)._Usuario <> Value then begin
        T_Aud_Criterio(NewImage)._Usuario := Value;
        UpdateLog(6, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Criterio.GetPackage : String; begin Result := T_Aud_Criterio(Prevalence.GetNewImage(Self, 7))._Package end;

procedure T_Aud_Criterio.SetPackage(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Package := Value else begin
    if (_Package = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Criterio(NewImage)._Package <> Value then begin
        T_Aud_Criterio(NewImage)._Package := Value;
        UpdateLog(7, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Criterio.GetClasse : String; begin Result := T_Aud_Criterio(Prevalence.GetNewImage(Self, 8))._Classe end;

procedure T_Aud_Criterio.SetClasse(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Classe := Value else begin
    if (_Classe = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Criterio(NewImage)._Classe <> Value then begin
        T_Aud_Criterio(NewImage)._Classe := Value;
        UpdateLog(8, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Criterio.GetTexto : String; begin Result := T_Aud_Criterio(Prevalence.GetNewImage(Self, 9))._Texto end;

procedure T_Aud_Criterio.SetTexto(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Texto := Value else begin
    if (_Texto = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Criterio(NewImage)._Texto <> Value then begin
        T_Aud_Criterio(NewImage)._Texto := Value;
        UpdateLog(9, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Criterio.GetSessao : T_Sessao; begin Result := T_Aud_Criterio(Prevalence.GetNewImage(Self, 11))._Sessao end;

procedure T_Aud_Criterio.SetSessao(Value : T_Sessao); begin
  if Prevalence.IsInRecover then _Sessao := Value else begin
    if (_Sessao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Criterio(NewImage)._Sessao <> Value then begin
        T_Aud_Criterio(NewImage)._Sessao := Value;
        UpdateLog(11, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 2, Baixo
class procedure T_Aud_Criterio.GerarPesquisa;
type
  TContext = record
    This : T_Aud_Criterio;
    Obj : T_Aud_Criterio;
  end;
var
  Context : TContext;
  StateMachine : TStateMachine;

procedure CriaObjeto(var Context : TContext); begin
  with Context, This do Obj := T_Aud_Criterio.Create;
end;

procedure InformaCriterio(var Context : TContext); begin
  with Context, This do begin
    Browser.Edit(Obj, '', 'InformaCriterio', '', StateMachine.FixButtons([bbCancel, bbFinish]));
  end;
end;

procedure SalvaCriterio(var Context : TContext); begin
  with Context, This do Obj.Add;
end;

procedure ConsultaPesquisa(var Context : TContext); begin
  with Context, This do Obj.RecuperarObjeto;
end;

procedure GeraPesquisa(var Context : TContext); begin
  with Context, This do GerarPesquisaAuditoria(Obj);
end;

function DesejaConsultarAgora(var Context : TContext) : boolean; begin
  with Context, This do Result := Browser.Confirm('Deseja consultar agora a pesquisa gerada?');
end;

begin
  fillchar(Context, sizeof(Context), 0);
  StateMachine := TStateMachine.Create('_Auditory', '_Aud_Criterio', 'GerarPesquisa', [1, 1, 1, 1, 1, 2]);
  with StateMachine do begin
    SetState(0, 'Inicializa', nil);
    SetState(1, 'CriaObjeto', @CriaObjeto);
    SetState(2, 'InformaCriterio', @InformaCriterio, [bbCancel, bbFinish]);
    SetState(3, 'SalvaCriterio', @SalvaCriterio);
    SetState(4, 'ConsultaPesquisa', @ConsultaPesquisa);
    SetState(5, 'GeraPesquisa', @GeraPesquisa);
    SetTransition(0, 0, 'Default', nil, 1);
    SetTransition(1, 0, 'Default', nil, 2);
    SetTransition(2, 0, 'Default', nil, 3);
    SetTransition(3, 0, 'Default', nil, 5);
    SetTransition(4, 0, 'Default', nil, -1);
    SetTransition(5, 0, 'DesejaConsultarAgora', @DesejaConsultarAgora, 4);
    SetTransition(5, 1, 'Default', nil, -1);
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

// Cyclomatic Complexity: 3, Baixo
procedure T_Aud_Criterio.RecuperarObjeto;
type
  TContext = record
    This : T_Aud_Criterio;
    Obj : TPrevalent;
    Ope : TTransient;
    Cri : TPrevalent;
  end;
var
  Context : TContext;
  StateMachine : TStateMachine;

procedure RecuperaCriterio(var Context : TContext); begin
  with Context, This do Ope := Browser.Choose(This.Operacoes);
end;

procedure LocalizaObjeto(var Context : TContext); begin
  with Context, This do Obj := TPrevalent(RecuperaObjetoAuditoria(Ope));
end;

procedure ExibeObjeto(var Context : TContext); begin
  with Context, This do begin
    Browser.Edit(Obj, '', 'ExibeObjeto', '', StateMachine.FixButtons([bbCancel, bbBack, bbNext]));
  end;
end;

function ObjetoRecuperado(var Context : TContext) : boolean; begin
  with Context, This do Result := Obj <> nil;
end;

begin
  fillchar(Context, sizeof(Context), 0);
  Context.This := Self;
  StateMachine := TStateMachine.Create('_Auditory', '_Aud_Criterio', 'RecuperarObjeto', [1, 1, 2, 2]);
  with StateMachine do begin
    SetState(0, 'Inicializa', nil);
    SetState(1, 'RecuperaCriterio', @RecuperaCriterio);
    SetState(2, 'LocalizaObjeto', @LocalizaObjeto);
    SetState(3, 'ExibeObjeto', @ExibeObjeto, [bbCancel, bbBack, bbNext]);
    SetTransition(0, 0, 'Default', nil, 1);
    SetTransition(1, 0, 'Default', nil, 2);
    SetTransition(2, 0, 'ObjetoRecuperado', @ObjetoRecuperado, 3);
    SetTransition(2, 1, 'Default', nil, 1);
    SetTransition(3, 0, 'Default', nil, 1);
    SetTransition(3, 1, 'Default', nil, -1);
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

// Cyclomatic Complexity: 1, Baixo
function T_Aud_Criterio.PorDataInicio : DateTime; begin
  Result := DataInicio
end;

// Cyclomatic Complexity: 1, Baixo
function T_Aud_Criterio.PorUsuario : String; begin
  Result := Sessao.Usuario
end;

{ T_Aud_CriterioList }

class function T_Aud_CriterioList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Criterio
end;

procedure T_Aud_CriterioList.Add(_Aud_Criterio : T_Aud_Criterio); begin
  inherited Add(TPrevalent(_Aud_Criterio))
end;

procedure T_Aud_CriterioList.Delete(_Aud_Criterio : T_Aud_Criterio); begin
  inherited Delete(TPrevalent(_Aud_Criterio))
end;

function T_Aud_CriterioList.Find(I : Integer) : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(inherited Find(I))
end;

function T_Aud_CriterioList.First : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(inherited First)
end;

function T_Aud_CriterioList.Last : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(inherited Last)
end;

function T_Aud_CriterioList.Near(I : Integer) : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(inherited Near(I))
end;

function T_Aud_CriterioList.Next(var _Aud_Criterio : T_Aud_Criterio) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Criterio))
end;

function T_Aud_CriterioList.Prior(var _Aud_Criterio : T_Aud_Criterio) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Criterio))
end;

function T_Aud_CriterioList.Get_Aud_Criterio(I : Integer) : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(Objects[I])
end;

{ T_Aud_CriterioPorDataInicioList }

class function T_Aud_CriterioPorDataInicioList.GetKeyCode : pointer; begin
  Result := @T_Aud_Criterio.PorDataInicio
end;

class function T_Aud_CriterioPorDataInicioList.GetListType : TListType; begin
  Result := ltDateTime
end;

class function T_Aud_CriterioPorDataInicioList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Criterio
end;

procedure T_Aud_CriterioPorDataInicioList.Add(_Aud_Criterio : T_Aud_Criterio); begin
  inherited Add(TPrevalent(_Aud_Criterio))
end;

procedure T_Aud_CriterioPorDataInicioList.Delete(_Aud_Criterio : T_Aud_Criterio); begin
  inherited Delete(TPrevalent(_Aud_Criterio))
end;

function T_Aud_CriterioPorDataInicioList.Find(D : DateTime) : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(inherited Find(D))
end;

function T_Aud_CriterioPorDataInicioList.First : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(inherited First)
end;

function T_Aud_CriterioPorDataInicioList.Last : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(inherited Last)
end;

function T_Aud_CriterioPorDataInicioList.Near(D : DateTime) : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(inherited Near(D))
end;

function T_Aud_CriterioPorDataInicioList.Next(var _Aud_Criterio : T_Aud_Criterio) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Criterio))
end;

function T_Aud_CriterioPorDataInicioList.Prior(var _Aud_Criterio : T_Aud_Criterio) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Criterio))
end;

function T_Aud_CriterioPorDataInicioList.Get_Aud_Criterio(I : Integer) : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(Objects[I])
end;

{ T_Aud_CriterioPorUsuarioList }

class function T_Aud_CriterioPorUsuarioList.GetKeyCode : pointer; begin
  Result := @T_Aud_Criterio.PorUsuario
end;

class function T_Aud_CriterioPorUsuarioList.GetListType : TListType; begin
  Result := ltString
end;

class function T_Aud_CriterioPorUsuarioList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Criterio
end;

procedure T_Aud_CriterioPorUsuarioList.Add(_Aud_Criterio : T_Aud_Criterio); begin
  inherited Add(TPrevalent(_Aud_Criterio))
end;

procedure T_Aud_CriterioPorUsuarioList.Delete(_Aud_Criterio : T_Aud_Criterio); begin
  inherited Delete(TPrevalent(_Aud_Criterio))
end;

function T_Aud_CriterioPorUsuarioList.Find(S : String) : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(inherited Find(S))
end;

function T_Aud_CriterioPorUsuarioList.First : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(inherited First)
end;

function T_Aud_CriterioPorUsuarioList.Last : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(inherited Last)
end;

function T_Aud_CriterioPorUsuarioList.Near(S : String) : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(inherited Near(S))
end;

function T_Aud_CriterioPorUsuarioList.Next(var _Aud_Criterio : T_Aud_Criterio) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Criterio))
end;

function T_Aud_CriterioPorUsuarioList.Prior(var _Aud_Criterio : T_Aud_Criterio) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Criterio))
end;

function T_Aud_CriterioPorUsuarioList.Get_Aud_Criterio(I : Integer) : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(Objects[I])
end;

{ T_SessaoCriteriosAssociation }

class function T_SessaoCriteriosAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Criterio
end;

procedure T_SessaoCriteriosAssociation.Add(_Aud_Criterio : T_Aud_Criterio); begin
  inherited Add(TPrevalent(_Aud_Criterio))
end;

procedure T_SessaoCriteriosAssociation.Delete(_Aud_Criterio : T_Aud_Criterio); begin
  inherited Delete(TPrevalent(_Aud_Criterio))
end;

function T_SessaoCriteriosAssociation.Find(I : Integer) : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(inherited Find(I))
end;

function T_SessaoCriteriosAssociation.First : T_Aud_Criterio; begin
  SetDependencyLists;
  Result := T_Aud_Criterio(inherited First)
end;

function T_SessaoCriteriosAssociation.Last : T_Aud_Criterio; begin
  SetDependencyLists;
  Result := T_Aud_Criterio(inherited Last)
end;

function T_SessaoCriteriosAssociation.Near(I : Integer) : T_Aud_Criterio; begin
  SetDependencyLists;
  Result := T_Aud_Criterio(inherited Near(I))
end;

function T_SessaoCriteriosAssociation.Next(var _Aud_Criterio : T_Aud_Criterio) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Aud_Criterio))
end;

function T_SessaoCriteriosAssociation.Prior(var _Aud_Criterio : T_Aud_Criterio) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Aud_Criterio))
end;

function T_SessaoCriteriosAssociation.Get_Aud_Criterio(I : Integer) : T_Aud_Criterio; begin
  Result := T_Aud_Criterio(Objects[I])
end;

{ T_Aud_Login }

procedure T_Aud_Login.New; begin
  inherited;
  _Operacoes := T_Aud_LoginOperacoesAssociation.Create(Self, 'Operacoes', true);
end;

procedure T_Aud_Login.InternalFree; begin
  if _Operacoes <> nil then _Operacoes.InternalFree;
  inherited;
end;

function T_Aud_Login.GetUsuario : String; begin Result := T_Aud_Login(Prevalence.GetNewImage(Self, 2))._Usuario end;

procedure T_Aud_Login.SetUsuario(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Usuario := Value else begin
    if (_Usuario = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Login(NewImage)._Usuario <> Value then begin
        T_Aud_Login(NewImage)._Usuario := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Login.GetNome : String; begin Result := T_Aud_Login(Prevalence.GetNewImage(Self, 3))._Nome end;

procedure T_Aud_Login.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Login(NewImage)._Nome <> Value then begin
        T_Aud_Login(NewImage)._Nome := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Login.GetMaquina : String; begin Result := T_Aud_Login(Prevalence.GetNewImage(Self, 4))._Maquina end;

procedure T_Aud_Login.SetMaquina(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Maquina := Value else begin
    if (_Maquina = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Login(NewImage)._Maquina <> Value then begin
        T_Aud_Login(NewImage)._Maquina := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Login.GetPosicao : Integer; begin Result := T_Aud_Login(Prevalence.GetNewImage(Self, 5))._Posicao end;

procedure T_Aud_Login.SetPosicao(Value : Integer); begin
  if Prevalence.IsInRecoverSnapShot then _Posicao := Value else begin
    if (_Posicao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Login(NewImage)._Posicao <> Value then begin
        T_Aud_Login(NewImage)._Posicao := Value;
        UpdateLog(5, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Login.GetPerfil : String; begin Result := T_Aud_Login(Prevalence.GetNewImage(Self, 6))._Perfil end;

procedure T_Aud_Login.SetPerfil(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Perfil := Value else begin
    if (_Perfil = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Login(NewImage)._Perfil <> Value then begin
        T_Aud_Login(NewImage)._Perfil := Value;
        UpdateLog(6, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Login.GetArquivo : T_Aud_Arquivo; begin Result := T_Aud_Login(Prevalence.GetNewImage(Self, 7))._Arquivo end;

procedure T_Aud_Login.SetArquivo(Value : T_Aud_Arquivo); begin
  if Prevalence.IsInRecover then _Arquivo := Value else begin
    if (_Arquivo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Login(NewImage)._Arquivo <> Value then begin
        T_Aud_Login(NewImage)._Arquivo := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Aud_Login.PorUsuarioMaquinaPerfil : String; begin
  Result := Usuario+'.'+Maquina+'.'+Perfil
end;

{ T_Aud_LoginList }

class function T_Aud_LoginList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Login
end;

procedure T_Aud_LoginList.Add(_Aud_Login : T_Aud_Login); begin
  inherited Add(TPrevalent(_Aud_Login))
end;

procedure T_Aud_LoginList.Delete(_Aud_Login : T_Aud_Login); begin
  inherited Delete(TPrevalent(_Aud_Login))
end;

function T_Aud_LoginList.Find(I : Integer) : T_Aud_Login; begin
  Result := T_Aud_Login(inherited Find(I))
end;

function T_Aud_LoginList.First : T_Aud_Login; begin
  Result := T_Aud_Login(inherited First)
end;

function T_Aud_LoginList.Last : T_Aud_Login; begin
  Result := T_Aud_Login(inherited Last)
end;

function T_Aud_LoginList.Near(I : Integer) : T_Aud_Login; begin
  Result := T_Aud_Login(inherited Near(I))
end;

function T_Aud_LoginList.Next(var _Aud_Login : T_Aud_Login) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Login))
end;

function T_Aud_LoginList.Prior(var _Aud_Login : T_Aud_Login) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Login))
end;

function T_Aud_LoginList.Get_Aud_Login(I : Integer) : T_Aud_Login; begin
  Result := T_Aud_Login(Objects[I])
end;

{ T_Aud_LoginPorUsuarioMaquinaPerfilList }

class function T_Aud_LoginPorUsuarioMaquinaPerfilList.GetKeyCode : pointer; begin
  Result := @T_Aud_Login.PorUsuarioMaquinaPerfil
end;

class function T_Aud_LoginPorUsuarioMaquinaPerfilList.GetListType : TListType; begin
  Result := ltString
end;

class function T_Aud_LoginPorUsuarioMaquinaPerfilList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Login
end;

procedure T_Aud_LoginPorUsuarioMaquinaPerfilList.Add(_Aud_Login : T_Aud_Login); begin
  inherited Add(TPrevalent(_Aud_Login))
end;

procedure T_Aud_LoginPorUsuarioMaquinaPerfilList.Delete(_Aud_Login : T_Aud_Login); begin
  inherited Delete(TPrevalent(_Aud_Login))
end;

function T_Aud_LoginPorUsuarioMaquinaPerfilList.Find(S : String) : T_Aud_Login; begin
  Result := T_Aud_Login(inherited Find(S))
end;

function T_Aud_LoginPorUsuarioMaquinaPerfilList.First : T_Aud_Login; begin
  Result := T_Aud_Login(inherited First)
end;

function T_Aud_LoginPorUsuarioMaquinaPerfilList.Last : T_Aud_Login; begin
  Result := T_Aud_Login(inherited Last)
end;

function T_Aud_LoginPorUsuarioMaquinaPerfilList.Near(S : String) : T_Aud_Login; begin
  Result := T_Aud_Login(inherited Near(S))
end;

function T_Aud_LoginPorUsuarioMaquinaPerfilList.Next(var _Aud_Login : T_Aud_Login) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Login))
end;

function T_Aud_LoginPorUsuarioMaquinaPerfilList.Prior(var _Aud_Login : T_Aud_Login) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Login))
end;

function T_Aud_LoginPorUsuarioMaquinaPerfilList.Get_Aud_Login(I : Integer) : T_Aud_Login; begin
  Result := T_Aud_Login(Objects[I])
end;

procedure T_Aud_Operacao.New; begin
  inherited;
  _Detalhes := T_Aud_OperacaoDetalhesAssociation.Create(Self, 'Detalhes', true);
end;

procedure T_Aud_Operacao.InternalFree; begin
  if _Detalhes <> nil then _Detalhes.InternalFree;
  inherited;
end;

function T_Aud_Operacao.GetOperacao : T_Operation; begin Result := T_Aud_Operacao(Prevalence.GetNewImage(Self, 2))._Operacao end;

procedure T_Aud_Operacao.SetOperacao(Value : T_Operation); begin
  if Prevalence.IsInRecoverSnapShot then _Operacao := Value else begin
    if (_Operacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Operacao(NewImage)._Operacao <> Value then begin
        T_Aud_Operacao(NewImage)._Operacao := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Operacao.GetData : DateTime; begin Result := T_Aud_Operacao(Prevalence.GetNewImage(Self, 3))._Data end;

procedure T_Aud_Operacao.SetData(Value : DateTime); begin
  if Prevalence.IsInRecoverSnapShot then _Data := Value else begin
    if (_Data = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Operacao(NewImage)._Data <> Value then begin
        T_Aud_Operacao(NewImage)._Data := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Operacao.GetAliasClasse : String; begin
  try Result := Classe.Nome except Result := ' ' end;
end;

function T_Aud_Operacao.GetIdentificacaoObjeto : String; begin Result := T_Aud_Operacao(Prevalence.GetNewImage(Self, 5))._IdentificacaoObjeto end;

procedure T_Aud_Operacao.SetIdentificacaoObjeto(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _IdentificacaoObjeto := Value else begin
    if (_IdentificacaoObjeto = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Operacao(NewImage)._IdentificacaoObjeto <> Value then begin
        T_Aud_Operacao(NewImage)._IdentificacaoObjeto := Value;
        UpdateLog(5, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Operacao.GetPosicao : Integer; begin Result := T_Aud_Operacao(Prevalence.GetNewImage(Self, 6))._Posicao end;

procedure T_Aud_Operacao.SetPosicao(Value : Integer); begin
  if Prevalence.IsInRecoverSnapShot then _Posicao := Value else begin
    if (_Posicao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Operacao(NewImage)._Posicao <> Value then begin
        T_Aud_Operacao(NewImage)._Posicao := Value;
        UpdateLog(6, NewImage, Stream)
      end;
    end;
end;

function T_Aud_Operacao.GetArquivo : T_Aud_Arquivo; begin Result := T_Aud_Operacao(Prevalence.GetNewImage(Self, 7))._Arquivo end;

procedure T_Aud_Operacao.SetArquivo(Value : T_Aud_Arquivo); begin
  if Prevalence.IsInRecover then _Arquivo := Value else begin
    if (_Arquivo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Operacao(NewImage)._Arquivo <> Value then begin
        T_Aud_Operacao(NewImage)._Arquivo := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

function T_Aud_Operacao.GetLogin : T_Aud_Login; begin Result := T_Aud_Operacao(Prevalence.GetNewImage(Self, 8))._Login end;

procedure T_Aud_Operacao.SetLogin(Value : T_Aud_Login); begin
  if Prevalence.IsInRecover then _Login := Value else begin
    if (_Login = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Operacao(NewImage)._Login <> Value then begin
        T_Aud_Operacao(NewImage)._Login := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

function T_Aud_Operacao.GetPackage : T_Aud_Package; begin Result := T_Aud_Operacao(Prevalence.GetNewImage(Self, 9))._Package end;

procedure T_Aud_Operacao.SetPackage(Value : T_Aud_Package); begin
  if Prevalence.IsInRecover then _Package := Value else begin
    if (_Package = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Operacao(NewImage)._Package <> Value then begin
        T_Aud_Operacao(NewImage)._Package := Value;
        UpdateLog(9, NewImage, Stream)
      end;
  end;
end;

function T_Aud_Operacao.GetOrigem : T_Aud_Origem; begin Result := T_Aud_Operacao(Prevalence.GetNewImage(Self, 11))._Origem end;

procedure T_Aud_Operacao.SetOrigem(Value : T_Aud_Origem); begin
  if Prevalence.IsInRecover then _Origem := Value else begin
    if (_Origem = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Operacao(NewImage)._Origem <> Value then begin
        T_Aud_Operacao(NewImage)._Origem := Value;
        UpdateLog(11, NewImage, Stream)
      end;
  end;
end;

function T_Aud_Operacao.GetClasse : T_Aud_Classe; begin Result := T_Aud_Operacao(Prevalence.GetNewImage(Self, 12))._Classe end;

procedure T_Aud_Operacao.SetClasse(Value : T_Aud_Classe); begin
  if Prevalence.IsInRecover then _Classe := Value else begin
    if (_Classe = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Operacao(NewImage)._Classe <> Value then begin
        T_Aud_Operacao(NewImage)._Classe := Value;
        UpdateLog(12, NewImage, Stream)
      end;
  end;
end;

function T_Aud_Operacao.GetCriterio : T_Aud_Criterio; begin Result := T_Aud_Operacao(Prevalence.GetNewImage(Self, 13))._Criterio end;

procedure T_Aud_Operacao.SetCriterio(Value : T_Aud_Criterio); begin
  if Prevalence.IsInRecover then _Criterio := Value else begin
    if (_Criterio = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Operacao(NewImage)._Criterio <> Value then begin
        T_Aud_Operacao(NewImage)._Criterio := Value;
        UpdateLog(13, NewImage, Stream)
      end;
  end;
end;

{ T_Aud_OperacaoList }

class function T_Aud_OperacaoList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Operacao
end;

procedure T_Aud_OperacaoList.Add(_Aud_Operacao : T_Aud_Operacao); begin
  inherited Add(TPrevalent(_Aud_Operacao))
end;

procedure T_Aud_OperacaoList.Delete(_Aud_Operacao : T_Aud_Operacao); begin
  inherited Delete(TPrevalent(_Aud_Operacao))
end;

function T_Aud_OperacaoList.Find(I : Integer) : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(inherited Find(I))
end;

function T_Aud_OperacaoList.First : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(inherited First)
end;

function T_Aud_OperacaoList.Last : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(inherited Last)
end;

function T_Aud_OperacaoList.Near(I : Integer) : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(inherited Near(I))
end;

function T_Aud_OperacaoList.Next(var _Aud_Operacao : T_Aud_Operacao) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Operacao))
end;

function T_Aud_OperacaoList.Prior(var _Aud_Operacao : T_Aud_Operacao) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Operacao))
end;

function T_Aud_OperacaoList.Get_Aud_Operacao(I : Integer) : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(Objects[I])
end;

{ T_Aud_LoginOperacoesAssociation }

class function T_Aud_LoginOperacoesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Operacao
end;

procedure T_Aud_LoginOperacoesAssociation.Add(_Aud_Operacao : T_Aud_Operacao); begin
  inherited Add(TPrevalent(_Aud_Operacao))
end;

procedure T_Aud_LoginOperacoesAssociation.Delete(_Aud_Operacao : T_Aud_Operacao); begin
  inherited Delete(TPrevalent(_Aud_Operacao))
end;

function T_Aud_LoginOperacoesAssociation.Find(I : Integer) : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(inherited Find(I))
end;

function T_Aud_LoginOperacoesAssociation.First : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited First)
end;

function T_Aud_LoginOperacoesAssociation.Last : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited Last)
end;

function T_Aud_LoginOperacoesAssociation.Near(I : Integer) : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited Near(I))
end;

function T_Aud_LoginOperacoesAssociation.Next(var _Aud_Operacao : T_Aud_Operacao) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Aud_Operacao))
end;

function T_Aud_LoginOperacoesAssociation.Prior(var _Aud_Operacao : T_Aud_Operacao) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Aud_Operacao))
end;

function T_Aud_LoginOperacoesAssociation.Get_Aud_Operacao(I : Integer) : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(Objects[I])
end;

{ T_Aud_PackageOperacoesAssociation }

class function T_Aud_PackageOperacoesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Operacao
end;

procedure T_Aud_PackageOperacoesAssociation.Add(_Aud_Operacao : T_Aud_Operacao); begin
  inherited Add(TPrevalent(_Aud_Operacao))
end;

procedure T_Aud_PackageOperacoesAssociation.Delete(_Aud_Operacao : T_Aud_Operacao); begin
  inherited Delete(TPrevalent(_Aud_Operacao))
end;

function T_Aud_PackageOperacoesAssociation.Find(I : Integer) : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(inherited Find(I))
end;

function T_Aud_PackageOperacoesAssociation.First : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited First)
end;

function T_Aud_PackageOperacoesAssociation.Last : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited Last)
end;

function T_Aud_PackageOperacoesAssociation.Near(I : Integer) : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited Near(I))
end;

function T_Aud_PackageOperacoesAssociation.Next(var _Aud_Operacao : T_Aud_Operacao) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Aud_Operacao))
end;

function T_Aud_PackageOperacoesAssociation.Prior(var _Aud_Operacao : T_Aud_Operacao) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Aud_Operacao))
end;

function T_Aud_PackageOperacoesAssociation.Get_Aud_Operacao(I : Integer) : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(Objects[I])
end;

{ T_Aud_OrigemOperacoesAssociation }

class function T_Aud_OrigemOperacoesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Operacao
end;

procedure T_Aud_OrigemOperacoesAssociation.Add(_Aud_Operacao : T_Aud_Operacao); begin
  inherited Add(TPrevalent(_Aud_Operacao))
end;

procedure T_Aud_OrigemOperacoesAssociation.Delete(_Aud_Operacao : T_Aud_Operacao); begin
  inherited Delete(TPrevalent(_Aud_Operacao))
end;

function T_Aud_OrigemOperacoesAssociation.Find(I : Integer) : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(inherited Find(I))
end;

function T_Aud_OrigemOperacoesAssociation.First : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited First)
end;

function T_Aud_OrigemOperacoesAssociation.Last : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited Last)
end;

function T_Aud_OrigemOperacoesAssociation.Near(I : Integer) : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited Near(I))
end;

function T_Aud_OrigemOperacoesAssociation.Next(var _Aud_Operacao : T_Aud_Operacao) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Aud_Operacao))
end;

function T_Aud_OrigemOperacoesAssociation.Prior(var _Aud_Operacao : T_Aud_Operacao) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Aud_Operacao))
end;

function T_Aud_OrigemOperacoesAssociation.Get_Aud_Operacao(I : Integer) : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(Objects[I])
end;

{ T_Aud_ClasseOperacoesAssociation }

class function T_Aud_ClasseOperacoesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Operacao
end;

procedure T_Aud_ClasseOperacoesAssociation.Add(_Aud_Operacao : T_Aud_Operacao); begin
  inherited Add(TPrevalent(_Aud_Operacao))
end;

procedure T_Aud_ClasseOperacoesAssociation.Delete(_Aud_Operacao : T_Aud_Operacao); begin
  inherited Delete(TPrevalent(_Aud_Operacao))
end;

function T_Aud_ClasseOperacoesAssociation.Find(I : Integer) : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(inherited Find(I))
end;

function T_Aud_ClasseOperacoesAssociation.First : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited First)
end;

function T_Aud_ClasseOperacoesAssociation.Last : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited Last)
end;

function T_Aud_ClasseOperacoesAssociation.Near(I : Integer) : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited Near(I))
end;

function T_Aud_ClasseOperacoesAssociation.Next(var _Aud_Operacao : T_Aud_Operacao) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Aud_Operacao))
end;

function T_Aud_ClasseOperacoesAssociation.Prior(var _Aud_Operacao : T_Aud_Operacao) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Aud_Operacao))
end;

function T_Aud_ClasseOperacoesAssociation.Get_Aud_Operacao(I : Integer) : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(Objects[I])
end;

{ T_Aud_CriterioOperacoesAssociation }

class function T_Aud_CriterioOperacoesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Operacao
end;

procedure T_Aud_CriterioOperacoesAssociation.Add(_Aud_Operacao : T_Aud_Operacao); begin
  inherited Add(TPrevalent(_Aud_Operacao))
end;

procedure T_Aud_CriterioOperacoesAssociation.Delete(_Aud_Operacao : T_Aud_Operacao); begin
  inherited Delete(TPrevalent(_Aud_Operacao))
end;

function T_Aud_CriterioOperacoesAssociation.Find(I : Integer) : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(inherited Find(I))
end;

function T_Aud_CriterioOperacoesAssociation.First : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited First)
end;

function T_Aud_CriterioOperacoesAssociation.Last : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited Last)
end;

function T_Aud_CriterioOperacoesAssociation.Near(I : Integer) : T_Aud_Operacao; begin
  SetDependencyLists;
  Result := T_Aud_Operacao(inherited Near(I))
end;

function T_Aud_CriterioOperacoesAssociation.Next(var _Aud_Operacao : T_Aud_Operacao) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Aud_Operacao))
end;

function T_Aud_CriterioOperacoesAssociation.Prior(var _Aud_Operacao : T_Aud_Operacao) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Aud_Operacao))
end;

function T_Aud_CriterioOperacoesAssociation.Get_Aud_Operacao(I : Integer) : T_Aud_Operacao; begin
  Result := T_Aud_Operacao(Objects[I])
end;

{ T_Aud_Origem }

procedure T_Aud_Origem.New; begin
  inherited;
  _Operacoes := T_Aud_OrigemOperacoesAssociation.Create(Self, 'Operacoes', true);
end;

procedure T_Aud_Origem.InternalFree; begin
  if _Operacoes <> nil then _Operacoes.InternalFree;
  inherited;
end;

function T_Aud_Origem.GetNome : String; begin Result := T_Aud_Origem(Prevalence.GetNewImage(Self, 2))._Nome end;

procedure T_Aud_Origem.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Origem(NewImage)._Nome <> Value then begin
        T_Aud_Origem(NewImage)._Nome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Aud_Origem.PorNome : String; begin
  Result := Nome
end;

{ T_Aud_OrigemList }

class function T_Aud_OrigemList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Origem
end;

procedure T_Aud_OrigemList.Add(_Aud_Origem : T_Aud_Origem); begin
  inherited Add(TPrevalent(_Aud_Origem))
end;

procedure T_Aud_OrigemList.Delete(_Aud_Origem : T_Aud_Origem); begin
  inherited Delete(TPrevalent(_Aud_Origem))
end;

function T_Aud_OrigemList.Find(I : Integer) : T_Aud_Origem; begin
  Result := T_Aud_Origem(inherited Find(I))
end;

function T_Aud_OrigemList.First : T_Aud_Origem; begin
  Result := T_Aud_Origem(inherited First)
end;

function T_Aud_OrigemList.Last : T_Aud_Origem; begin
  Result := T_Aud_Origem(inherited Last)
end;

function T_Aud_OrigemList.Near(I : Integer) : T_Aud_Origem; begin
  Result := T_Aud_Origem(inherited Near(I))
end;

function T_Aud_OrigemList.Next(var _Aud_Origem : T_Aud_Origem) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Origem))
end;

function T_Aud_OrigemList.Prior(var _Aud_Origem : T_Aud_Origem) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Origem))
end;

function T_Aud_OrigemList.Get_Aud_Origem(I : Integer) : T_Aud_Origem; begin
  Result := T_Aud_Origem(Objects[I])
end;

{ T_Aud_OrigemPorNomeList }

class function T_Aud_OrigemPorNomeList.GetKeyCode : pointer; begin
  Result := @T_Aud_Origem.PorNome
end;

class function T_Aud_OrigemPorNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_Aud_OrigemPorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Origem
end;

procedure T_Aud_OrigemPorNomeList.Add(_Aud_Origem : T_Aud_Origem); begin
  inherited Add(TPrevalent(_Aud_Origem))
end;

procedure T_Aud_OrigemPorNomeList.Delete(_Aud_Origem : T_Aud_Origem); begin
  inherited Delete(TPrevalent(_Aud_Origem))
end;

function T_Aud_OrigemPorNomeList.Find(S : String) : T_Aud_Origem; begin
  Result := T_Aud_Origem(inherited Find(S))
end;

function T_Aud_OrigemPorNomeList.First : T_Aud_Origem; begin
  Result := T_Aud_Origem(inherited First)
end;

function T_Aud_OrigemPorNomeList.Last : T_Aud_Origem; begin
  Result := T_Aud_Origem(inherited Last)
end;

function T_Aud_OrigemPorNomeList.Near(S : String) : T_Aud_Origem; begin
  Result := T_Aud_Origem(inherited Near(S))
end;

function T_Aud_OrigemPorNomeList.Next(var _Aud_Origem : T_Aud_Origem) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Origem))
end;

function T_Aud_OrigemPorNomeList.Prior(var _Aud_Origem : T_Aud_Origem) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Origem))
end;

function T_Aud_OrigemPorNomeList.Get_Aud_Origem(I : Integer) : T_Aud_Origem; begin
  Result := T_Aud_Origem(Objects[I])
end;

{ T_Aud_Package }

procedure T_Aud_Package.New; begin
  inherited;
  _Operacoes := T_Aud_PackageOperacoesAssociation.Create(Self, 'Operacoes', true);
end;

procedure T_Aud_Package.InternalFree; begin
  if _Operacoes <> nil then _Operacoes.InternalFree;
  inherited;
end;

function T_Aud_Package.GetNome : String; begin Result := T_Aud_Package(Prevalence.GetNewImage(Self, 2))._Nome end;

procedure T_Aud_Package.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_Package(NewImage)._Nome <> Value then begin
        T_Aud_Package(NewImage)._Nome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Aud_Package.PorNome : String; begin
  Result := Nome
end;

{ T_Aud_PackageList }

class function T_Aud_PackageList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Package
end;

procedure T_Aud_PackageList.Add(_Aud_Package : T_Aud_Package); begin
  inherited Add(TPrevalent(_Aud_Package))
end;

procedure T_Aud_PackageList.Delete(_Aud_Package : T_Aud_Package); begin
  inherited Delete(TPrevalent(_Aud_Package))
end;

function T_Aud_PackageList.Find(I : Integer) : T_Aud_Package; begin
  Result := T_Aud_Package(inherited Find(I))
end;

function T_Aud_PackageList.First : T_Aud_Package; begin
  Result := T_Aud_Package(inherited First)
end;

function T_Aud_PackageList.Last : T_Aud_Package; begin
  Result := T_Aud_Package(inherited Last)
end;

function T_Aud_PackageList.Near(I : Integer) : T_Aud_Package; begin
  Result := T_Aud_Package(inherited Near(I))
end;

function T_Aud_PackageList.Next(var _Aud_Package : T_Aud_Package) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Package))
end;

function T_Aud_PackageList.Prior(var _Aud_Package : T_Aud_Package) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Package))
end;

function T_Aud_PackageList.Get_Aud_Package(I : Integer) : T_Aud_Package; begin
  Result := T_Aud_Package(Objects[I])
end;

{ T_Aud_PackagePorNomeList }

class function T_Aud_PackagePorNomeList.GetKeyCode : pointer; begin
  Result := @T_Aud_Package.PorNome
end;

class function T_Aud_PackagePorNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_Aud_PackagePorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_Package
end;

procedure T_Aud_PackagePorNomeList.Add(_Aud_Package : T_Aud_Package); begin
  inherited Add(TPrevalent(_Aud_Package))
end;

procedure T_Aud_PackagePorNomeList.Delete(_Aud_Package : T_Aud_Package); begin
  inherited Delete(TPrevalent(_Aud_Package))
end;

function T_Aud_PackagePorNomeList.Find(S : String) : T_Aud_Package; begin
  Result := T_Aud_Package(inherited Find(S))
end;

function T_Aud_PackagePorNomeList.First : T_Aud_Package; begin
  Result := T_Aud_Package(inherited First)
end;

function T_Aud_PackagePorNomeList.Last : T_Aud_Package; begin
  Result := T_Aud_Package(inherited Last)
end;

function T_Aud_PackagePorNomeList.Near(S : String) : T_Aud_Package; begin
  Result := T_Aud_Package(inherited Near(S))
end;

function T_Aud_PackagePorNomeList.Next(var _Aud_Package : T_Aud_Package) : boolean; begin
  Result := inherited Next(TTransient(_Aud_Package))
end;

function T_Aud_PackagePorNomeList.Prior(var _Aud_Package : T_Aud_Package) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_Package))
end;

function T_Aud_PackagePorNomeList.Get_Aud_Package(I : Integer) : T_Aud_Package; begin
  Result := T_Aud_Package(Objects[I])
end;

function T_Aud_UpdObjeto.GetPropriedade : String; begin Result := T_Aud_UpdObjeto(Prevalence.GetNewImage(Self, 2))._Propriedade end;

procedure T_Aud_UpdObjeto.SetPropriedade(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Propriedade := Value else begin
    if (_Propriedade = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_UpdObjeto(NewImage)._Propriedade <> Value then begin
        T_Aud_UpdObjeto(NewImage)._Propriedade := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_Aud_UpdObjeto.GetValorAnterior : String; begin Result := T_Aud_UpdObjeto(Prevalence.GetNewImage(Self, 3))._ValorAnterior end;

procedure T_Aud_UpdObjeto.SetValorAnterior(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _ValorAnterior := Value else begin
    if (_ValorAnterior = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_UpdObjeto(NewImage)._ValorAnterior <> Value then begin
        T_Aud_UpdObjeto(NewImage)._ValorAnterior := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_Aud_UpdObjeto.GetNovoValor : String; begin Result := T_Aud_UpdObjeto(Prevalence.GetNewImage(Self, 4))._NovoValor end;

procedure T_Aud_UpdObjeto.SetNovoValor(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _NovoValor := Value else begin
    if (_NovoValor = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_UpdObjeto(NewImage)._NovoValor <> Value then begin
        T_Aud_UpdObjeto(NewImage)._NovoValor := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

function T_Aud_UpdObjeto.GetOperacao : T_Aud_Operacao; begin Result := T_Aud_UpdObjeto(Prevalence.GetNewImage(Self, 5))._Operacao end;

procedure T_Aud_UpdObjeto.SetOperacao(Value : T_Aud_Operacao); begin
  if Prevalence.IsInRecover then _Operacao := Value else begin
    if (_Operacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Aud_UpdObjeto(NewImage)._Operacao <> Value then begin
        T_Aud_UpdObjeto(NewImage)._Operacao := Value;
        UpdateLog(5, NewImage, Stream)
      end;
  end;
end;

{ T_Aud_UpdObjetoList }

class function T_Aud_UpdObjetoList.GetObjectClass : TTransientClass; begin
  Result := T_Aud_UpdObjeto
end;

procedure T_Aud_UpdObjetoList.Add(_Aud_UpdObjeto : T_Aud_UpdObjeto); begin
  inherited Add(TPrevalent(_Aud_UpdObjeto))
end;

procedure T_Aud_UpdObjetoList.Delete(_Aud_UpdObjeto : T_Aud_UpdObjeto); begin
  inherited Delete(TPrevalent(_Aud_UpdObjeto))
end;

function T_Aud_UpdObjetoList.Find(I : Integer) : T_Aud_UpdObjeto; begin
  Result := T_Aud_UpdObjeto(inherited Find(I))
end;

function T_Aud_UpdObjetoList.First : T_Aud_UpdObjeto; begin
  Result := T_Aud_UpdObjeto(inherited First)
end;

function T_Aud_UpdObjetoList.Last : T_Aud_UpdObjeto; begin
  Result := T_Aud_UpdObjeto(inherited Last)
end;

function T_Aud_UpdObjetoList.Near(I : Integer) : T_Aud_UpdObjeto; begin
  Result := T_Aud_UpdObjeto(inherited Near(I))
end;

function T_Aud_UpdObjetoList.Next(var _Aud_UpdObjeto : T_Aud_UpdObjeto) : boolean; begin
  Result := inherited Next(TTransient(_Aud_UpdObjeto))
end;

function T_Aud_UpdObjetoList.Prior(var _Aud_UpdObjeto : T_Aud_UpdObjeto) : boolean; begin
  Result := inherited Prior(TTransient(_Aud_UpdObjeto))
end;

function T_Aud_UpdObjetoList.Get_Aud_UpdObjeto(I : Integer) : T_Aud_UpdObjeto; begin
  Result := T_Aud_UpdObjeto(Objects[I])
end;

{ T_Aud_OperacaoDetalhesAssociation }

class function T_Aud_OperacaoDetalhesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Aud_UpdObjeto
end;

procedure T_Aud_OperacaoDetalhesAssociation.Add(_Aud_UpdObjeto : T_Aud_UpdObjeto); begin
  inherited Add(TPrevalent(_Aud_UpdObjeto))
end;

procedure T_Aud_OperacaoDetalhesAssociation.Delete(_Aud_UpdObjeto : T_Aud_UpdObjeto); begin
  inherited Delete(TPrevalent(_Aud_UpdObjeto))
end;

function T_Aud_OperacaoDetalhesAssociation.Find(I : Integer) : T_Aud_UpdObjeto; begin
  Result := T_Aud_UpdObjeto(inherited Find(I))
end;

function T_Aud_OperacaoDetalhesAssociation.First : T_Aud_UpdObjeto; begin
  SetDependencyLists;
  Result := T_Aud_UpdObjeto(inherited First)
end;

function T_Aud_OperacaoDetalhesAssociation.Last : T_Aud_UpdObjeto; begin
  SetDependencyLists;
  Result := T_Aud_UpdObjeto(inherited Last)
end;

function T_Aud_OperacaoDetalhesAssociation.Near(I : Integer) : T_Aud_UpdObjeto; begin
  SetDependencyLists;
  Result := T_Aud_UpdObjeto(inherited Near(I))
end;

function T_Aud_OperacaoDetalhesAssociation.Next(var _Aud_UpdObjeto : T_Aud_UpdObjeto) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Aud_UpdObjeto))
end;

function T_Aud_OperacaoDetalhesAssociation.Prior(var _Aud_UpdObjeto : T_Aud_UpdObjeto) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Aud_UpdObjeto))
end;

function T_Aud_OperacaoDetalhesAssociation.Get_Aud_UpdObjeto(I : Integer) : T_Aud_UpdObjeto; begin
  Result := T_Aud_UpdObjeto(Objects[I])
end;

{ T_Classe }

procedure T_Classe.New; begin
  inherited;
  _Ocupacoes := T_ClasseOcupacoesAssociation.Create(Self, 'Ocupacoes', true);
  _PermissoesClasses := T_ClassePermissoesClassesAssociation.Create(Self, 'PermissoesClasses');
  _Metodos := T_ClasseMetodosAssociation.Create(Self, 'Metodos');
  _Propriedades := T_ClassePropriedadesAssociation.Create(Self, 'Propriedades');
end;

procedure T_Classe.InternalFree; begin
  if _Ocupacoes <> nil then _Ocupacoes.InternalFree;
  if _PermissoesClasses <> nil then _PermissoesClasses.InternalFree;
  if _Metodos <> nil then _Metodos.InternalFree;
  if _Propriedades <> nil then _Propriedades.InternalFree;
  inherited;
end;

function T_Classe.GetIdentification : string; begin
  try Result := AliasNome + '('+Package.Nome+')' except Result := ' ' end;
end;

function T_Classe.GetAliasNome : String; begin Result := T_Classe(Prevalence.GetNewImage(Self, 2))._AliasNome end;

procedure T_Classe.SetAliasNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _AliasNome := Value else begin
    if (_AliasNome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Classe(NewImage)._AliasNome <> Value then begin
        T_Classe(NewImage)._AliasNome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_Classe.GetNome : String; begin Result := T_Classe(Prevalence.GetNewImage(Self, 3))._Nome end;

procedure T_Classe.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Classe(NewImage)._Nome <> Value then begin
        T_Classe(NewImage)._Nome := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_Classe.GetPackage : T_Package; begin Result := T_Classe(Prevalence.GetNewImage(Self, 8))._Package end;

procedure T_Classe.SetPackage(Value : T_Package); begin
  if Prevalence.IsInRecover then _Package := Value else begin
    if (_Package = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Classe(NewImage)._Package <> Value then begin
        T_Classe(NewImage)._Package := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Classe.PorNome : String; begin
  Result := Nome
end;

// Cyclomatic Complexity: 1, Baixo
class procedure T_Classe.AtualizaClasses;
type
  TContext = record
  end;
var
  Context : TContext;
  StateMachine : TStateMachine;

procedure Carga(var Context : TContext); begin
  with Context do CarregaSeguranca;
end;

begin
  fillchar(Context, sizeof(Context), 0);
  StateMachine := TStateMachine.Create('_Security', '_Classe', 'AtualizaClasses', [1]);
  with StateMachine do begin
    SetState(0, 'Carga', @Carga);
    SetTransition(0, 0, 'Default', nil, -1);
  end;
  try
    inc(CallStateMachineCount);
    StateMachine.Execute(@Context);
  finally
    dec(CallStateMachineCount);
    StateMachine.Free;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Classe.Get_Metodo(Nome : String) : T_Metodo; begin
  Result := Self._Metodos.Find(Nome);
end;

// Cyclomatic Complexity: 1, Baixo
function T_Classe.Get_Propriedade(Nome : String) : T_Propriedade; begin
  Result := Self._Propriedades.Find(Nome);
end;

// Cyclomatic Complexity: 1, Baixo
function T_Classe.PorAlias : String; begin
  Result := AliasNome
end;

{ T_ClasseList }

class function T_ClasseList.GetObjectClass : TTransientClass; begin
  Result := T_Classe
end;

procedure T_ClasseList.Add(_Classe : T_Classe); begin
  inherited Add(TPrevalent(_Classe))
end;

procedure T_ClasseList.Delete(_Classe : T_Classe); begin
  inherited Delete(TPrevalent(_Classe))
end;

function T_ClasseList.Find(I : Integer) : T_Classe; begin
  Result := T_Classe(inherited Find(I))
end;

function T_ClasseList.First : T_Classe; begin
  Result := T_Classe(inherited First)
end;

function T_ClasseList.Last : T_Classe; begin
  Result := T_Classe(inherited Last)
end;

function T_ClasseList.Near(I : Integer) : T_Classe; begin
  Result := T_Classe(inherited Near(I))
end;

function T_ClasseList.Next(var _Classe : T_Classe) : boolean; begin
  Result := inherited Next(TTransient(_Classe))
end;

function T_ClasseList.Prior(var _Classe : T_Classe) : boolean; begin
  Result := inherited Prior(TTransient(_Classe))
end;

function T_ClasseList.Get_Classe(I : Integer) : T_Classe; begin
  Result := T_Classe(Objects[I])
end;

{ T_ClassePorNomeList }

class function T_ClassePorNomeList.GetKeyCode : pointer; begin
  Result := @T_Classe.PorNome
end;

class function T_ClassePorNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_ClassePorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_Classe
end;

procedure T_ClassePorNomeList.Add(_Classe : T_Classe); begin
  inherited Add(TPrevalent(_Classe))
end;

procedure T_ClassePorNomeList.Delete(_Classe : T_Classe); begin
  inherited Delete(TPrevalent(_Classe))
end;

function T_ClassePorNomeList.Find(S : String) : T_Classe; begin
  Result := T_Classe(inherited Find(S))
end;

function T_ClassePorNomeList.First : T_Classe; begin
  Result := T_Classe(inherited First)
end;

function T_ClassePorNomeList.Last : T_Classe; begin
  Result := T_Classe(inherited Last)
end;

function T_ClassePorNomeList.Near(S : String) : T_Classe; begin
  Result := T_Classe(inherited Near(S))
end;

function T_ClassePorNomeList.Next(var _Classe : T_Classe) : boolean; begin
  Result := inherited Next(TTransient(_Classe))
end;

function T_ClassePorNomeList.Prior(var _Classe : T_Classe) : boolean; begin
  Result := inherited Prior(TTransient(_Classe))
end;

function T_ClassePorNomeList.Get_Classe(I : Integer) : T_Classe; begin
  Result := T_Classe(Objects[I])
end;

{ T_ClassePorAliasList }

class function T_ClassePorAliasList.GetKeyCode : pointer; begin
  Result := @T_Classe.PorAlias
end;

class function T_ClassePorAliasList.GetListType : TListType; begin
  Result := ltString
end;

class function T_ClassePorAliasList.GetObjectClass : TTransientClass; begin
  Result := T_Classe
end;

procedure T_ClassePorAliasList.Add(_Classe : T_Classe); begin
  inherited Add(TPrevalent(_Classe))
end;

procedure T_ClassePorAliasList.Delete(_Classe : T_Classe); begin
  inherited Delete(TPrevalent(_Classe))
end;

function T_ClassePorAliasList.Find(S : String) : T_Classe; begin
  Result := T_Classe(inherited Find(S))
end;

function T_ClassePorAliasList.First : T_Classe; begin
  Result := T_Classe(inherited First)
end;

function T_ClassePorAliasList.Last : T_Classe; begin
  Result := T_Classe(inherited Last)
end;

function T_ClassePorAliasList.Near(S : String) : T_Classe; begin
  Result := T_Classe(inherited Near(S))
end;

function T_ClassePorAliasList.Next(var _Classe : T_Classe) : boolean; begin
  Result := inherited Next(TTransient(_Classe))
end;

function T_ClassePorAliasList.Prior(var _Classe : T_Classe) : boolean; begin
  Result := inherited Prior(TTransient(_Classe))
end;

function T_ClassePorAliasList.Get_Classe(I : Integer) : T_Classe; begin
  Result := T_Classe(Objects[I])
end;

{ T_PackageClassesAssociation }

class function T_PackageClassesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Classe
end;

procedure T_PackageClassesAssociation.Add(_Classe : T_Classe); begin
  inherited Add(TPrevalent(_Classe))
end;

procedure T_PackageClassesAssociation.Delete(_Classe : T_Classe); begin
  inherited Delete(TPrevalent(_Classe))
end;

function T_PackageClassesAssociation.Find(I : Integer) : T_Classe; begin
  Result := T_Classe(inherited Find(I))
end;

function T_PackageClassesAssociation.First : T_Classe; begin
  SetDependencyLists;
  Result := T_Classe(inherited First)
end;

function T_PackageClassesAssociation.Last : T_Classe; begin
  SetDependencyLists;
  Result := T_Classe(inherited Last)
end;

function T_PackageClassesAssociation.Near(I : Integer) : T_Classe; begin
  SetDependencyLists;
  Result := T_Classe(inherited Near(I))
end;

function T_PackageClassesAssociation.Next(var _Classe : T_Classe) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Classe))
end;

function T_PackageClassesAssociation.Prior(var _Classe : T_Classe) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Classe))
end;

function T_PackageClassesAssociation.Get_Classe(I : Integer) : T_Classe; begin
  Result := T_Classe(Objects[I])
end;

{ T_Dominio }

procedure T_Dominio.New; begin
  inherited;
  _Grupos := T_DominioGruposAssociation.Create(Self, 'Grupos');
end;

procedure T_Dominio.InternalFree; begin
  if _Grupos <> nil then _Grupos.InternalFree;
  inherited;
end;

function T_Dominio.GetNome : String; begin Result := T_Dominio(Prevalence.GetNewImage(Self, 2))._Nome end;

procedure T_Dominio.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Dominio(NewImage)._Nome <> Value then begin
        T_Dominio(NewImage)._Nome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Dominio.PorNome : String; begin
  Result := Nome
end;

{ T_DominioList }

class function T_DominioList.GetObjectClass : TTransientClass; begin
  Result := T_Dominio
end;

procedure T_DominioList.Add(_Dominio : T_Dominio); begin
  inherited Add(TPrevalent(_Dominio))
end;

procedure T_DominioList.Delete(_Dominio : T_Dominio); begin
  inherited Delete(TPrevalent(_Dominio))
end;

function T_DominioList.Find(I : Integer) : T_Dominio; begin
  Result := T_Dominio(inherited Find(I))
end;

function T_DominioList.First : T_Dominio; begin
  Result := T_Dominio(inherited First)
end;

function T_DominioList.Last : T_Dominio; begin
  Result := T_Dominio(inherited Last)
end;

function T_DominioList.Near(I : Integer) : T_Dominio; begin
  Result := T_Dominio(inherited Near(I))
end;

function T_DominioList.Next(var _Dominio : T_Dominio) : boolean; begin
  Result := inherited Next(TTransient(_Dominio))
end;

function T_DominioList.Prior(var _Dominio : T_Dominio) : boolean; begin
  Result := inherited Prior(TTransient(_Dominio))
end;

function T_DominioList.Get_Dominio(I : Integer) : T_Dominio; begin
  Result := T_Dominio(Objects[I])
end;

{ T_DominioPorNomeList }

class function T_DominioPorNomeList.GetKeyCode : pointer; begin
  Result := @T_Dominio.PorNome
end;

class function T_DominioPorNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_DominioPorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_Dominio
end;

procedure T_DominioPorNomeList.Add(_Dominio : T_Dominio); begin
  inherited Add(TPrevalent(_Dominio))
end;

procedure T_DominioPorNomeList.Delete(_Dominio : T_Dominio); begin
  inherited Delete(TPrevalent(_Dominio))
end;

function T_DominioPorNomeList.Find(S : String) : T_Dominio; begin
  Result := T_Dominio(inherited Find(S))
end;

function T_DominioPorNomeList.First : T_Dominio; begin
  Result := T_Dominio(inherited First)
end;

function T_DominioPorNomeList.Last : T_Dominio; begin
  Result := T_Dominio(inherited Last)
end;

function T_DominioPorNomeList.Near(S : String) : T_Dominio; begin
  Result := T_Dominio(inherited Near(S))
end;

function T_DominioPorNomeList.Next(var _Dominio : T_Dominio) : boolean; begin
  Result := inherited Next(TTransient(_Dominio))
end;

function T_DominioPorNomeList.Prior(var _Dominio : T_Dominio) : boolean; begin
  Result := inherited Prior(TTransient(_Dominio))
end;

function T_DominioPorNomeList.Get_Dominio(I : Integer) : T_Dominio; begin
  Result := T_Dominio(Objects[I])
end;

{ T_ExportaImporta }

procedure T_ExportaImporta.New; begin
  inherited;
  if Prevalence.IsInRecover then exit;
  try _Log := True except end;
  try _FormatoDataHora := 'MM/DD/YYYY HH:MM:SS' except end;
end;

function T_ExportaImporta.GetNome : String; begin Result := T_ExportaImporta(Prevalence.GetNewImage(Self, 2))._Nome end;

procedure T_ExportaImporta.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_ExportaImporta(NewImage)._Nome <> Value then begin
        T_ExportaImporta(NewImage)._Nome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_ExportaImporta.GetArquivo : String; begin Result := T_ExportaImporta(Prevalence.GetNewImage(Self, 3))._Arquivo end;

procedure T_ExportaImporta.SetArquivo(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Arquivo := Value else begin
    if (_Arquivo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_ExportaImporta(NewImage)._Arquivo <> Value then begin
        T_ExportaImporta(NewImage)._Arquivo := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_ExportaImporta.GetLog : Boolean; begin Result := T_ExportaImporta(Prevalence.GetNewImage(Self, 4))._Log end;

procedure T_ExportaImporta.SetLog(Value : Boolean); begin
  if Prevalence.IsInRecoverSnapShot then _Log := Value else begin
    if (_Log = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_ExportaImporta(NewImage)._Log <> Value then begin
        T_ExportaImporta(NewImage)._Log := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

function T_ExportaImporta.GetFormatoDataHora : String; begin Result := T_ExportaImporta(Prevalence.GetNewImage(Self, 5))._FormatoDataHora end;

procedure T_ExportaImporta.SetFormatoDataHora(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _FormatoDataHora := Value else begin
    if (_FormatoDataHora = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_ExportaImporta(NewImage)._FormatoDataHora <> Value then begin
        T_ExportaImporta(NewImage)._FormatoDataHora := Value;
        UpdateLog(5, NewImage, Stream)
      end;
    end;
end;

function T_ExportaImporta.GetClasse : T_Classe; begin Result := T_ExportaImporta(Prevalence.GetNewImage(Self, 6))._Classe end;

procedure T_ExportaImporta.SetClasse(Value : T_Classe); begin
  if Prevalence.IsInRecover then _Classe := Value else begin
    if (_Classe = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_ExportaImporta(NewImage)._Classe <> Value then begin
        T_ExportaImporta(NewImage)._Classe := Value;
        UpdateLog(6, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_ExportaImporta.PorNome : string; begin
  Result := Nome
end;

class function T_ExportaImportaList.GetObjectClass : TTransientClass; begin
  Result := T_ExportaImporta
end;

// Cyclomatic Complexity: 1, Baixo
procedure T_Geral.CheckSort; begin
  Prevalence.CheckSortList(true);
end;

{ T_GeralList }

class function T_GeralList.GetObjectClass : TTransientClass; begin
  Result := T_Geral
end;

procedure T_GeralList.Add(_Geral : T_Geral); begin
  inherited Add(TPrevalent(_Geral))
end;

procedure T_GeralList.Delete(_Geral : T_Geral); begin
  inherited Delete(TPrevalent(_Geral))
end;

function T_GeralList.Find(I : Integer) : T_Geral; begin
  Result := T_Geral(inherited Find(I))
end;

function T_GeralList.First : T_Geral; begin
  Result := T_Geral(inherited First)
end;

function T_GeralList.Last : T_Geral; begin
  Result := T_Geral(inherited Last)
end;

function T_GeralList.Near(I : Integer) : T_Geral; begin
  Result := T_Geral(inherited Near(I))
end;

function T_GeralList.Next(var _Geral : T_Geral) : boolean; begin
  Result := inherited Next(TTransient(_Geral))
end;

function T_GeralList.Prior(var _Geral : T_Geral) : boolean; begin
  Result := inherited Prior(TTransient(_Geral))
end;

function T_GeralList.Get_Geral(I : Integer) : T_Geral; begin
  Result := T_Geral(Objects[I])
end;

{ T_Grupo }

procedure T_Grupo.New; begin
  inherited;
  _Packages := T_GrupoPackagesAssociation.Create(Self, 'Packages');
  _CanDelegate := T_GrupoCanDelegateAssociation.Create(Self, 'CanDelegate');
  _Profile := T_GrupoProfileAssociation.Create(Self, 'Profile');
  _AssignedPendencies := T_GrupoAssignedPendenciesAssociation.Create(Self, 'AssignedPendencies');
  _Pendencies := T_GrupoPendenciesAssociation.Create(Self, 'Pendencies');
  if Prevalence.IsInRecover then exit;
  try _Ativo := True except end;
end;

procedure T_Grupo.InternalFree; begin
  if _Packages <> nil then _Packages.InternalFree;
  if _CanDelegate <> nil then _CanDelegate.InternalFree;
  if _Profile <> nil then _Profile.InternalFree;
  if _AssignedPendencies <> nil then _AssignedPendencies.InternalFree;
  if _Pendencies <> nil then _Pendencies.InternalFree;
  inherited;
end;

function T_Grupo.GetIdentification : string; begin
  try Result := Identificador except Result := ' ' end;
end;

function T_Grupo.GetNome : String; begin Result := T_Grupo(Prevalence.GetNewImage(Self, 2))._Nome end;

procedure T_Grupo.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Grupo(NewImage)._Nome <> Value then begin
        T_Grupo(NewImage)._Nome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_Grupo.GetAtivo : Boolean; begin Result := T_Grupo(Prevalence.GetNewImage(Self, 3))._Ativo end;

procedure T_Grupo.SetAtivo(Value : Boolean); begin
  if Prevalence.IsInRecoverSnapShot then _Ativo := Value else begin
    if (_Ativo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Grupo(NewImage)._Ativo <> Value then begin
        T_Grupo(NewImage)._Ativo := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_Grupo.GetAliasNome : String; begin Result := T_Grupo(Prevalence.GetNewImage(Self, 4))._AliasNome end;

procedure T_Grupo.SetAliasNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _AliasNome := Value else begin
    if (_AliasNome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Grupo(NewImage)._AliasNome <> Value then begin
        T_Grupo(NewImage)._AliasNome := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

function T_Grupo.GetIdentificador : String; begin
  try Result := ifthen(AliasNome='',Nome,AliasNome) except Result := ' ' end;
end;

function T_Grupo.GetDominio : T_Dominio; begin Result := T_Grupo(Prevalence.GetNewImage(Self, 10))._Dominio end;

procedure T_Grupo.SetDominio(Value : T_Dominio); begin
  if Prevalence.IsInRecover then _Dominio := Value else begin
    if (_Dominio = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Grupo(NewImage)._Dominio <> Value then begin
        T_Grupo(NewImage)._Dominio := Value;
        UpdateLog(10, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Grupo.PorNome : String; begin
  Result := Nome
end;

// Cyclomatic Complexity: 1, Baixo
function T_Grupo.PorIdentificador : String; begin
  Result := Identificador
end;

{ T_GrupoList }

class function T_GrupoList.GetObjectClass : TTransientClass; begin
  Result := T_Grupo
end;

procedure T_GrupoList.Add(_Grupo : T_Grupo); begin
  inherited Add(TPrevalent(_Grupo))
end;

procedure T_GrupoList.Delete(_Grupo : T_Grupo); begin
  inherited Delete(TPrevalent(_Grupo))
end;

function T_GrupoList.Find(I : Integer) : T_Grupo; begin
  Result := T_Grupo(inherited Find(I))
end;

function T_GrupoList.First : T_Grupo; begin
  Result := T_Grupo(inherited First)
end;

function T_GrupoList.Last : T_Grupo; begin
  Result := T_Grupo(inherited Last)
end;

function T_GrupoList.Near(I : Integer) : T_Grupo; begin
  Result := T_Grupo(inherited Near(I))
end;

function T_GrupoList.Next(var _Grupo : T_Grupo) : boolean; begin
  Result := inherited Next(TTransient(_Grupo))
end;

function T_GrupoList.Prior(var _Grupo : T_Grupo) : boolean; begin
  Result := inherited Prior(TTransient(_Grupo))
end;

function T_GrupoList.Get_Grupo(I : Integer) : T_Grupo; begin
  Result := T_Grupo(Objects[I])
end;

{ T_GrupoPorNomeList }

class function T_GrupoPorNomeList.GetKeyCode : pointer; begin
  Result := @T_Grupo.PorNome
end;

class function T_GrupoPorNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_GrupoPorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_Grupo
end;

procedure T_GrupoPorNomeList.Add(_Grupo : T_Grupo); begin
  inherited Add(TPrevalent(_Grupo))
end;

procedure T_GrupoPorNomeList.Delete(_Grupo : T_Grupo); begin
  inherited Delete(TPrevalent(_Grupo))
end;

function T_GrupoPorNomeList.Find(S : String) : T_Grupo; begin
  Result := T_Grupo(inherited Find(S))
end;

function T_GrupoPorNomeList.First : T_Grupo; begin
  Result := T_Grupo(inherited First)
end;

function T_GrupoPorNomeList.Last : T_Grupo; begin
  Result := T_Grupo(inherited Last)
end;

function T_GrupoPorNomeList.Near(S : String) : T_Grupo; begin
  Result := T_Grupo(inherited Near(S))
end;

function T_GrupoPorNomeList.Next(var _Grupo : T_Grupo) : boolean; begin
  Result := inherited Next(TTransient(_Grupo))
end;

function T_GrupoPorNomeList.Prior(var _Grupo : T_Grupo) : boolean; begin
  Result := inherited Prior(TTransient(_Grupo))
end;

function T_GrupoPorNomeList.Get_Grupo(I : Integer) : T_Grupo; begin
  Result := T_Grupo(Objects[I])
end;

{ T_GrupoPorIdentificadorList }

class function T_GrupoPorIdentificadorList.GetKeyCode : pointer; begin
  Result := @T_Grupo.PorIdentificador
end;

class function T_GrupoPorIdentificadorList.GetListType : TListType; begin
  Result := ltString
end;

class function T_GrupoPorIdentificadorList.GetObjectClass : TTransientClass; begin
  Result := T_Grupo
end;

procedure T_GrupoPorIdentificadorList.Add(_Grupo : T_Grupo); begin
  inherited Add(TPrevalent(_Grupo))
end;

procedure T_GrupoPorIdentificadorList.Delete(_Grupo : T_Grupo); begin
  inherited Delete(TPrevalent(_Grupo))
end;

function T_GrupoPorIdentificadorList.Find(S : String) : T_Grupo; begin
  Result := T_Grupo(inherited Find(S))
end;

function T_GrupoPorIdentificadorList.First : T_Grupo; begin
  Result := T_Grupo(inherited First)
end;

function T_GrupoPorIdentificadorList.Last : T_Grupo; begin
  Result := T_Grupo(inherited Last)
end;

function T_GrupoPorIdentificadorList.Near(S : String) : T_Grupo; begin
  Result := T_Grupo(inherited Near(S))
end;

function T_GrupoPorIdentificadorList.Next(var _Grupo : T_Grupo) : boolean; begin
  Result := inherited Next(TTransient(_Grupo))
end;

function T_GrupoPorIdentificadorList.Prior(var _Grupo : T_Grupo) : boolean; begin
  Result := inherited Prior(TTransient(_Grupo))
end;

function T_GrupoPorIdentificadorList.Get_Grupo(I : Integer) : T_Grupo; begin
  Result := T_Grupo(Objects[I])
end;

{ T_GrupoProfileAssociation }

class function T_GrupoProfileAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Grupo
end;

procedure T_GrupoProfileAssociation.Add(_Grupo : T_Grupo); begin
  inherited Add(TPrevalent(_Grupo))
end;

procedure T_GrupoProfileAssociation.Delete(_Grupo : T_Grupo); begin
  inherited Delete(TPrevalent(_Grupo))
end;

function T_GrupoProfileAssociation.Find(I : Integer) : T_Grupo; begin
  Result := T_Grupo(inherited Find(I))
end;

function T_GrupoProfileAssociation.First : T_Grupo; begin
  SetDependencyLists;
  Result := T_Grupo(inherited First)
end;

function T_GrupoProfileAssociation.Last : T_Grupo; begin
  SetDependencyLists;
  Result := T_Grupo(inherited Last)
end;

function T_GrupoProfileAssociation.Near(I : Integer) : T_Grupo; begin
  SetDependencyLists;
  Result := T_Grupo(inherited Near(I))
end;

function T_GrupoProfileAssociation.Next(var _Grupo : T_Grupo) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Grupo))
end;

function T_GrupoProfileAssociation.Prior(var _Grupo : T_Grupo) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Grupo))
end;

function T_GrupoProfileAssociation.Get_Grupo(I : Integer) : T_Grupo; begin
  Result := T_Grupo(Objects[I])
end;

{ T_GrupoCanDelegateAssociation }

class function T_GrupoCanDelegateAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Grupo
end;

procedure T_GrupoCanDelegateAssociation.Add(_Grupo : T_Grupo); begin
  inherited Add(TPrevalent(_Grupo))
end;

procedure T_GrupoCanDelegateAssociation.Delete(_Grupo : T_Grupo); begin
  inherited Delete(TPrevalent(_Grupo))
end;

function T_GrupoCanDelegateAssociation.Find(I : Integer) : T_Grupo; begin
  Result := T_Grupo(inherited Find(I))
end;

function T_GrupoCanDelegateAssociation.First : T_Grupo; begin
  SetDependencyLists;
  Result := T_Grupo(inherited First)
end;

function T_GrupoCanDelegateAssociation.Last : T_Grupo; begin
  SetDependencyLists;
  Result := T_Grupo(inherited Last)
end;

function T_GrupoCanDelegateAssociation.Near(I : Integer) : T_Grupo; begin
  SetDependencyLists;
  Result := T_Grupo(inherited Near(I))
end;

function T_GrupoCanDelegateAssociation.Next(var _Grupo : T_Grupo) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Grupo))
end;

function T_GrupoCanDelegateAssociation.Prior(var _Grupo : T_Grupo) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Grupo))
end;

function T_GrupoCanDelegateAssociation.Get_Grupo(I : Integer) : T_Grupo; begin
  Result := T_Grupo(Objects[I])
end;

{ T_DominioGruposAssociation }

class function T_DominioGruposAssociation.GetKeyCode : pointer; begin
  Result := @T_Grupo.PorNome
end;

class function T_DominioGruposAssociation.GetListType : TListType; begin
  Result := ltString
end;

class function T_DominioGruposAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Grupo
end;

procedure T_DominioGruposAssociation.Add(_Grupo : T_Grupo); begin
  inherited Add(TPrevalent(_Grupo))
end;

procedure T_DominioGruposAssociation.Delete(_Grupo : T_Grupo); begin
  inherited Delete(TPrevalent(_Grupo))
end;

function T_DominioGruposAssociation.Find(S : String) : T_Grupo; begin
  Result := T_Grupo(inherited Find(S))
end;

function T_DominioGruposAssociation.First : T_Grupo; begin
  SetDependencyLists;
  Result := T_Grupo(inherited First)
end;

function T_DominioGruposAssociation.Last : T_Grupo; begin
  SetDependencyLists;
  Result := T_Grupo(inherited Last)
end;

function T_DominioGruposAssociation.Near(S : String) : T_Grupo; begin
  SetDependencyLists;
  Result := T_Grupo(inherited Near(S))
end;

function T_DominioGruposAssociation.Next(var _Grupo : T_Grupo) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Grupo))
end;

function T_DominioGruposAssociation.Prior(var _Grupo : T_Grupo) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Grupo))
end;

function T_DominioGruposAssociation.Get_Grupo(I : Integer) : T_Grupo; begin
  Result := T_Grupo(Objects[I])
end;

function T_ImportaPropriedade.GetElemento : T_ImportaPropriedadeElemento; begin Result := T_ImportaPropriedade(Prevalence.GetNewImage(Self, 2))._Elemento end;

procedure T_ImportaPropriedade.SetElemento(Value : T_ImportaPropriedadeElemento); begin
  if Prevalence.IsInRecoverSnapShot then _Elemento := Value else begin
    if (_Elemento = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_ImportaPropriedade(NewImage)._Elemento <> Value then begin
        T_ImportaPropriedade(NewImage)._Elemento := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_ImportaPropriedade.GetTipo : T_PropriedadeTipo; begin
  try Result := Propriedade.Tipo except Result := T_PropriedadeTipo(0) end;
end;

function T_ImportaPropriedade.GetListaElementoChave : String; begin Result := T_ImportaPropriedade(Prevalence.GetNewImage(Self, 4))._ListaElementoChave end;

procedure T_ImportaPropriedade.SetListaElementoChave(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _ListaElementoChave := Value else begin
    if (_ListaElementoChave = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_ImportaPropriedade(NewImage)._ListaElementoChave <> Value then begin
        T_ImportaPropriedade(NewImage)._ListaElementoChave := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

function T_ImportaPropriedade.GetPropNome : string; begin
  try Result := Propriedade.Classe.Nome+'.'+Propriedade.Nome except Result := ' ' end;
end;

function T_ImportaPropriedade.GetPropriedade : T_Propriedade; begin Result := T_ImportaPropriedade(Prevalence.GetNewImage(Self, 6))._Propriedade end;

procedure T_ImportaPropriedade.SetPropriedade(Value : T_Propriedade); begin
  if Prevalence.IsInRecover then _Propriedade := Value else begin
    if (_Propriedade = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_ImportaPropriedade(NewImage)._Propriedade <> Value then begin
        T_ImportaPropriedade(NewImage)._Propriedade := Value;
        UpdateLog(6, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_ImportaPropriedade.PorElemento : Integer; begin
  Result := Elemento
end;

// Cyclomatic Complexity: 1, Baixo
function T_ImportaPropriedade.PorPropNome : String; begin
  Result := PropNome
end;

function T_ImportaPropriedade.CheckListaElementoChave(var Message : String) : Boolean; begin
  Result := not ((Tipo in [_ptReferencia, _ptAssociacao]) and (ListaElementoChave = ''));
  if Result then
    Message := ''
  else
    Message := 'Lista Chave deve ser informada para Elementos do Tipo Associação ou Referência'
end;

class function T_ImportaPropriedadeList.GetObjectClass : TTransientClass; begin
  Result := T_ImportaPropriedade
end;

function T_Log.GetDateTime : DateTime; begin Result := T_Log(Prevalence.GetNewImage(Self, 2))._DateTime end;

procedure T_Log.SetDateTime(Value : DateTime); begin
  if Prevalence.IsInRecoverSnapShot then _DateTime := Value else begin
    if (_DateTime = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Log(NewImage)._DateTime <> Value then begin
        T_Log(NewImage)._DateTime := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_Log.GetText : String; begin Result := T_Log(Prevalence.GetNewImage(Self, 3))._Text end;

procedure T_Log.SetText(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Text := Value else begin
    if (_Text = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Log(NewImage)._Text <> Value then begin
        T_Log(NewImage)._Text := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

{ T_LogList }

class function T_LogList.GetObjectClass : TTransientClass; begin
  Result := T_Log
end;

procedure T_LogList.Add(_Log : T_Log); begin
  inherited Add(TPrevalent(_Log))
end;

procedure T_LogList.Delete(_Log : T_Log); begin
  inherited Delete(TPrevalent(_Log))
end;

function T_LogList.Find(I : Integer) : T_Log; begin
  Result := T_Log(inherited Find(I))
end;

function T_LogList.First : T_Log; begin
  Result := T_Log(inherited First)
end;

function T_LogList.Last : T_Log; begin
  Result := T_Log(inherited Last)
end;

function T_LogList.Near(I : Integer) : T_Log; begin
  Result := T_Log(inherited Near(I))
end;

function T_LogList.Next(var _Log : T_Log) : boolean; begin
  Result := inherited Next(TTransient(_Log))
end;

function T_LogList.Prior(var _Log : T_Log) : boolean; begin
  Result := inherited Prior(TTransient(_Log))
end;

function T_LogList.Get_Log(I : Integer) : T_Log; begin
  Result := T_Log(Objects[I])
end;

{ T_RunLogAssociation }

class function T_RunLogAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Log
end;

procedure T_RunLogAssociation.Add(_Log : T_Log); begin
  inherited Add(TPrevalent(_Log))
end;

procedure T_RunLogAssociation.Delete(_Log : T_Log); begin
  inherited Delete(TPrevalent(_Log))
end;

function T_RunLogAssociation.Find(I : Integer) : T_Log; begin
  Result := T_Log(inherited Find(I))
end;

function T_RunLogAssociation.First : T_Log; begin
  SetDependencyLists;
  Result := T_Log(inherited First)
end;

function T_RunLogAssociation.Last : T_Log; begin
  SetDependencyLists;
  Result := T_Log(inherited Last)
end;

function T_RunLogAssociation.Near(I : Integer) : T_Log; begin
  SetDependencyLists;
  Result := T_Log(inherited Near(I))
end;

function T_RunLogAssociation.Next(var _Log : T_Log) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Log))
end;

function T_RunLogAssociation.Prior(var _Log : T_Log) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Log))
end;

function T_RunLogAssociation.Get_Log(I : Integer) : T_Log; begin
  Result := T_Log(Objects[I])
end;

{ T_Metodo }

procedure T_Metodo.New; begin
  inherited;
  _PermissoesMetodos := T_MetodoPermissoesMetodosAssociation.Create(Self, 'PermissoesMetodos');
  _PermissoesView := T_MetodoPermissoesViewAssociation.Create(Self, 'PermissoesView');
  if Prevalence.IsInRecover then exit;
  try _Bloqueado := False except end;
end;

procedure T_Metodo.InternalFree; begin
  if _PermissoesMetodos <> nil then _PermissoesMetodos.InternalFree;
  if _PermissoesView <> nil then _PermissoesView.InternalFree;
  inherited;
end;

function T_Metodo.GetIdentification : string; begin
  try Result := AliasNome + '('+Classe.Package.Nome +'.'+ Classe.Nome +')' except Result := ' ' end;
end;

function T_Metodo.GetAliasNome : String; begin Result := T_Metodo(Prevalence.GetNewImage(Self, 2))._AliasNome end;

procedure T_Metodo.SetAliasNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _AliasNome := Value else begin
    if (_AliasNome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Metodo(NewImage)._AliasNome <> Value then begin
        T_Metodo(NewImage)._AliasNome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_Metodo.GetTipo : T_Stereotype; begin Result := T_Metodo(Prevalence.GetNewImage(Self, 3))._Tipo end;

procedure T_Metodo.SetTipo(Value : T_Stereotype); begin
  if Prevalence.IsInRecoverSnapShot then _Tipo := Value else begin
    if (_Tipo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Metodo(NewImage)._Tipo <> Value then begin
        T_Metodo(NewImage)._Tipo := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_Metodo.GetNome : String; begin Result := T_Metodo(Prevalence.GetNewImage(Self, 4))._Nome end;

procedure T_Metodo.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Metodo(NewImage)._Nome <> Value then begin
        T_Metodo(NewImage)._Nome := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

function T_Metodo.GetBloqueado : Boolean; begin Result := T_Metodo(Prevalence.GetNewImage(Self, 5))._Bloqueado end;

procedure T_Metodo.SetBloqueado(Value : Boolean); begin
  if Prevalence.IsInRecoverSnapShot then _Bloqueado := Value else begin
    if (_Bloqueado = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Metodo(NewImage)._Bloqueado <> Value then begin
        T_Metodo(NewImage)._Bloqueado := Value;
        UpdateLog(5, NewImage, Stream)
      end;
    end;
end;

function T_Metodo.GetIdentificador : String; begin
  try Result := Classe.Package.Nome + '.' + Classe.Nome + '.' + Nome except Result := ' ' end;
end;

function T_Metodo.GetClasse : T_Classe; begin Result := T_Metodo(Prevalence.GetNewImage(Self, 8))._Classe end;

procedure T_Metodo.SetClasse(Value : T_Classe); begin
  if Prevalence.IsInRecover then _Classe := Value else begin
    if (_Classe = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Metodo(NewImage)._Classe <> Value then begin
        T_Metodo(NewImage)._Classe := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Metodo.PorNome : String; begin
  Result := Nome
end;

// Cyclomatic Complexity: 1, Baixo
function T_Metodo.PorView : String; begin
  Result := Nome
end;

function T_Metodo.PorViewFilter : Boolean; begin
  Result := Tipo = _stView
end;

// Cyclomatic Complexity: 1, Baixo
function T_Metodo.PorAlias : String; begin
  Result := AliasNome
end;

// Cyclomatic Complexity: 1, Baixo
function T_Metodo.PorIdentificador : String; begin
  Result := Identificador
end;

{ T_MetodoList }

class function T_MetodoList.GetObjectClass : TTransientClass; begin
  Result := T_Metodo
end;

procedure T_MetodoList.Add(_Metodo : T_Metodo); begin
  inherited Add(TPrevalent(_Metodo))
end;

procedure T_MetodoList.Delete(_Metodo : T_Metodo); begin
  inherited Delete(TPrevalent(_Metodo))
end;

function T_MetodoList.Find(I : Integer) : T_Metodo; begin
  Result := T_Metodo(inherited Find(I))
end;

function T_MetodoList.First : T_Metodo; begin
  Result := T_Metodo(inherited First)
end;

function T_MetodoList.Last : T_Metodo; begin
  Result := T_Metodo(inherited Last)
end;

function T_MetodoList.Near(I : Integer) : T_Metodo; begin
  Result := T_Metodo(inherited Near(I))
end;

function T_MetodoList.Next(var _Metodo : T_Metodo) : boolean; begin
  Result := inherited Next(TTransient(_Metodo))
end;

function T_MetodoList.Prior(var _Metodo : T_Metodo) : boolean; begin
  Result := inherited Prior(TTransient(_Metodo))
end;

function T_MetodoList.Get_Metodo(I : Integer) : T_Metodo; begin
  Result := T_Metodo(Objects[I])
end;

{ T_MetodoPorNomeList }

class function T_MetodoPorNomeList.GetKeyCode : pointer; begin
  Result := @T_Metodo.PorNome
end;

class function T_MetodoPorNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_MetodoPorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_Metodo
end;

procedure T_MetodoPorNomeList.Add(_Metodo : T_Metodo); begin
  inherited Add(TPrevalent(_Metodo))
end;

procedure T_MetodoPorNomeList.Delete(_Metodo : T_Metodo); begin
  inherited Delete(TPrevalent(_Metodo))
end;

function T_MetodoPorNomeList.Find(S : String) : T_Metodo; begin
  Result := T_Metodo(inherited Find(S))
end;

function T_MetodoPorNomeList.First : T_Metodo; begin
  Result := T_Metodo(inherited First)
end;

function T_MetodoPorNomeList.Last : T_Metodo; begin
  Result := T_Metodo(inherited Last)
end;

function T_MetodoPorNomeList.Near(S : String) : T_Metodo; begin
  Result := T_Metodo(inherited Near(S))
end;

function T_MetodoPorNomeList.Next(var _Metodo : T_Metodo) : boolean; begin
  Result := inherited Next(TTransient(_Metodo))
end;

function T_MetodoPorNomeList.Prior(var _Metodo : T_Metodo) : boolean; begin
  Result := inherited Prior(TTransient(_Metodo))
end;

function T_MetodoPorNomeList.Get_Metodo(I : Integer) : T_Metodo; begin
  Result := T_Metodo(Objects[I])
end;

{ T_MetodoPorViewList }

class function T_MetodoPorViewList.GetKeyCode : pointer; begin
  Result := @T_Metodo.PorView
end;

class function T_MetodoPorViewList.GetListType : TListType; begin
  Result := ltString
end;

class function T_MetodoPorViewList.GetObjectClass : TTransientClass; begin
  Result := T_Metodo
end;

procedure T_MetodoPorViewList.Add(_Metodo : T_Metodo); begin
  inherited Add(TPrevalent(_Metodo))
end;

procedure T_MetodoPorViewList.Delete(_Metodo : T_Metodo); begin
  inherited Delete(TPrevalent(_Metodo))
end;

function T_MetodoPorViewList.Find(S : String) : T_Metodo; begin
  Result := T_Metodo(inherited Find(S))
end;

function T_MetodoPorViewList.First : T_Metodo; begin
  Result := T_Metodo(inherited First)
end;

function T_MetodoPorViewList.Last : T_Metodo; begin
  Result := T_Metodo(inherited Last)
end;

function T_MetodoPorViewList.Near(S : String) : T_Metodo; begin
  Result := T_Metodo(inherited Near(S))
end;

function T_MetodoPorViewList.Next(var _Metodo : T_Metodo) : boolean; begin
  Result := inherited Next(TTransient(_Metodo))
end;

function T_MetodoPorViewList.Prior(var _Metodo : T_Metodo) : boolean; begin
  Result := inherited Prior(TTransient(_Metodo))
end;

function T_MetodoPorViewList.Get_Metodo(I : Integer) : T_Metodo; begin
  Result := T_Metodo(Objects[I])
end;

{ T_MetodoPorAliasList }

class function T_MetodoPorAliasList.GetKeyCode : pointer; begin
  Result := @T_Metodo.PorAlias
end;

class function T_MetodoPorAliasList.GetListType : TListType; begin
  Result := ltString
end;

class function T_MetodoPorAliasList.GetObjectClass : TTransientClass; begin
  Result := T_Metodo
end;

procedure T_MetodoPorAliasList.Add(_Metodo : T_Metodo); begin
  inherited Add(TPrevalent(_Metodo))
end;

procedure T_MetodoPorAliasList.Delete(_Metodo : T_Metodo); begin
  inherited Delete(TPrevalent(_Metodo))
end;

function T_MetodoPorAliasList.Find(S : String) : T_Metodo; begin
  Result := T_Metodo(inherited Find(S))
end;

function T_MetodoPorAliasList.First : T_Metodo; begin
  Result := T_Metodo(inherited First)
end;

function T_MetodoPorAliasList.Last : T_Metodo; begin
  Result := T_Metodo(inherited Last)
end;

function T_MetodoPorAliasList.Near(S : String) : T_Metodo; begin
  Result := T_Metodo(inherited Near(S))
end;

function T_MetodoPorAliasList.Next(var _Metodo : T_Metodo) : boolean; begin
  Result := inherited Next(TTransient(_Metodo))
end;

function T_MetodoPorAliasList.Prior(var _Metodo : T_Metodo) : boolean; begin
  Result := inherited Prior(TTransient(_Metodo))
end;

function T_MetodoPorAliasList.Get_Metodo(I : Integer) : T_Metodo; begin
  Result := T_Metodo(Objects[I])
end;

{ T_MetodoPorIdentificadorList }

class function T_MetodoPorIdentificadorList.GetKeyCode : pointer; begin
  Result := @T_Metodo.PorIdentificador
end;

class function T_MetodoPorIdentificadorList.GetListType : TListType; begin
  Result := ltString
end;

class function T_MetodoPorIdentificadorList.GetObjectClass : TTransientClass; begin
  Result := T_Metodo
end;

procedure T_MetodoPorIdentificadorList.Add(_Metodo : T_Metodo); begin
  inherited Add(TPrevalent(_Metodo))
end;

procedure T_MetodoPorIdentificadorList.Delete(_Metodo : T_Metodo); begin
  inherited Delete(TPrevalent(_Metodo))
end;

function T_MetodoPorIdentificadorList.Find(S : String) : T_Metodo; begin
  Result := T_Metodo(inherited Find(S))
end;

function T_MetodoPorIdentificadorList.First : T_Metodo; begin
  Result := T_Metodo(inherited First)
end;

function T_MetodoPorIdentificadorList.Last : T_Metodo; begin
  Result := T_Metodo(inherited Last)
end;

function T_MetodoPorIdentificadorList.Near(S : String) : T_Metodo; begin
  Result := T_Metodo(inherited Near(S))
end;

function T_MetodoPorIdentificadorList.Next(var _Metodo : T_Metodo) : boolean; begin
  Result := inherited Next(TTransient(_Metodo))
end;

function T_MetodoPorIdentificadorList.Prior(var _Metodo : T_Metodo) : boolean; begin
  Result := inherited Prior(TTransient(_Metodo))
end;

function T_MetodoPorIdentificadorList.Get_Metodo(I : Integer) : T_Metodo; begin
  Result := T_Metodo(Objects[I])
end;

{ T_ClasseMetodosAssociation }

class function T_ClasseMetodosAssociation.GetKeyCode : pointer; begin
  Result := @T_Metodo.PorNome
end;

class function T_ClasseMetodosAssociation.GetListType : TListType; begin
  Result := ltString
end;

class function T_ClasseMetodosAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Metodo
end;

procedure T_ClasseMetodosAssociation.Add(_Metodo : T_Metodo); begin
  inherited Add(TPrevalent(_Metodo))
end;

procedure T_ClasseMetodosAssociation.Delete(_Metodo : T_Metodo); begin
  inherited Delete(TPrevalent(_Metodo))
end;

function T_ClasseMetodosAssociation.Find(S : String) : T_Metodo; begin
  Result := T_Metodo(inherited Find(S))
end;

function T_ClasseMetodosAssociation.First : T_Metodo; begin
  SetDependencyLists;
  Result := T_Metodo(inherited First)
end;

function T_ClasseMetodosAssociation.Last : T_Metodo; begin
  SetDependencyLists;
  Result := T_Metodo(inherited Last)
end;

function T_ClasseMetodosAssociation.Near(S : String) : T_Metodo; begin
  SetDependencyLists;
  Result := T_Metodo(inherited Near(S))
end;

function T_ClasseMetodosAssociation.Next(var _Metodo : T_Metodo) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Metodo))
end;

function T_ClasseMetodosAssociation.Prior(var _Metodo : T_Metodo) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Metodo))
end;

function T_ClasseMetodosAssociation.Get_Metodo(I : Integer) : T_Metodo; begin
  Result := T_Metodo(Objects[I])
end;

class function T_OcupacaoList.GetObjectClass : TTransientClass; begin
  Result := T_Ocupacao
end;

function T_OcupacaoAssociacao.GetAssociacao : String; begin Result := T_OcupacaoAssociacao(Prevalence.GetNewImage(Self, 2))._Associacao end;

procedure T_OcupacaoAssociacao.SetAssociacao(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Associacao := Value else begin
    if (_Associacao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_OcupacaoAssociacao(NewImage)._Associacao <> Value then begin
        T_OcupacaoAssociacao(NewImage)._Associacao := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_OcupacaoAssociacao.GetCount : Integer; begin Result := T_OcupacaoAssociacao(Prevalence.GetNewImage(Self, 3))._Count end;

procedure T_OcupacaoAssociacao.SetCount(Value : Integer); begin
  if Prevalence.IsInRecoverSnapShot then _Count := Value else begin
    if (_Count = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_OcupacaoAssociacao(NewImage)._Count <> Value then begin
        T_OcupacaoAssociacao(NewImage)._Count := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_OcupacaoAssociacao.GetCapacity : Integer; begin Result := T_OcupacaoAssociacao(Prevalence.GetNewImage(Self, 4))._Capacity end;

procedure T_OcupacaoAssociacao.SetCapacity(Value : Integer); begin
  if Prevalence.IsInRecoverSnapShot then _Capacity := Value else begin
    if (_Capacity = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_OcupacaoAssociacao(NewImage)._Capacity <> Value then begin
        T_OcupacaoAssociacao(NewImage)._Capacity := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

function T_OcupacaoAssociacao.GetMemoria : Double; begin
  try Result := Capacity * sizeof(integer) div 1024 except Result := 0 end;
end;

function T_OcupacaoAssociacao.GetLista : T_OcupacaoLista; begin Result := T_OcupacaoAssociacao(Prevalence.GetNewImage(Self, 6))._Lista end;

procedure T_OcupacaoAssociacao.SetLista(Value : T_OcupacaoLista); begin
  if Prevalence.IsInRecover then _Lista := Value else begin
    if (_Lista = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_OcupacaoAssociacao(NewImage)._Lista <> Value then begin
        T_OcupacaoAssociacao(NewImage)._Lista := Value;
        UpdateLog(6, NewImage, Stream)
      end;
  end;
end;

{ T_OcupacaoAssociacaoList }

class function T_OcupacaoAssociacaoList.GetObjectClass : TTransientClass; begin
  Result := T_OcupacaoAssociacao
end;

procedure T_OcupacaoAssociacaoList.Add(_OcupacaoAssociacao : T_OcupacaoAssociacao); begin
  inherited Add(TPrevalent(_OcupacaoAssociacao))
end;

procedure T_OcupacaoAssociacaoList.Delete(_OcupacaoAssociacao : T_OcupacaoAssociacao); begin
  inherited Delete(TPrevalent(_OcupacaoAssociacao))
end;

function T_OcupacaoAssociacaoList.Find(I : Integer) : T_OcupacaoAssociacao; begin
  Result := T_OcupacaoAssociacao(inherited Find(I))
end;

function T_OcupacaoAssociacaoList.First : T_OcupacaoAssociacao; begin
  Result := T_OcupacaoAssociacao(inherited First)
end;

function T_OcupacaoAssociacaoList.Last : T_OcupacaoAssociacao; begin
  Result := T_OcupacaoAssociacao(inherited Last)
end;

function T_OcupacaoAssociacaoList.Near(I : Integer) : T_OcupacaoAssociacao; begin
  Result := T_OcupacaoAssociacao(inherited Near(I))
end;

function T_OcupacaoAssociacaoList.Next(var _OcupacaoAssociacao : T_OcupacaoAssociacao) : boolean; begin
  Result := inherited Next(TTransient(_OcupacaoAssociacao))
end;

function T_OcupacaoAssociacaoList.Prior(var _OcupacaoAssociacao : T_OcupacaoAssociacao) : boolean; begin
  Result := inherited Prior(TTransient(_OcupacaoAssociacao))
end;

function T_OcupacaoAssociacaoList.Get_OcupacaoAssociacao(I : Integer) : T_OcupacaoAssociacao; begin
  Result := T_OcupacaoAssociacao(Objects[I])
end;

{ T_OcupacaoListaAssociacoesAssociation }

class function T_OcupacaoListaAssociacoesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_OcupacaoAssociacao
end;

procedure T_OcupacaoListaAssociacoesAssociation.Add(_OcupacaoAssociacao : T_OcupacaoAssociacao); begin
  inherited Add(TPrevalent(_OcupacaoAssociacao))
end;

procedure T_OcupacaoListaAssociacoesAssociation.Delete(_OcupacaoAssociacao : T_OcupacaoAssociacao); begin
  inherited Delete(TPrevalent(_OcupacaoAssociacao))
end;

function T_OcupacaoListaAssociacoesAssociation.Find(I : Integer) : T_OcupacaoAssociacao; begin
  Result := T_OcupacaoAssociacao(inherited Find(I))
end;

function T_OcupacaoListaAssociacoesAssociation.First : T_OcupacaoAssociacao; begin
  SetDependencyLists;
  Result := T_OcupacaoAssociacao(inherited First)
end;

function T_OcupacaoListaAssociacoesAssociation.Last : T_OcupacaoAssociacao; begin
  SetDependencyLists;
  Result := T_OcupacaoAssociacao(inherited Last)
end;

function T_OcupacaoListaAssociacoesAssociation.Near(I : Integer) : T_OcupacaoAssociacao; begin
  SetDependencyLists;
  Result := T_OcupacaoAssociacao(inherited Near(I))
end;

function T_OcupacaoListaAssociacoesAssociation.Next(var _OcupacaoAssociacao : T_OcupacaoAssociacao) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_OcupacaoAssociacao))
end;

function T_OcupacaoListaAssociacoesAssociation.Prior(var _OcupacaoAssociacao : T_OcupacaoAssociacao) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_OcupacaoAssociacao))
end;

function T_OcupacaoListaAssociacoesAssociation.Get_OcupacaoAssociacao(I : Integer) : T_OcupacaoAssociacao; begin
  Result := T_OcupacaoAssociacao(Objects[I])
end;

{ T_Package }

procedure T_Package.New; begin
  inherited;
  _Ocupacoes := T_PackageOcupacoesAssociation.Create(Self, 'Ocupacoes', true);
  _Classes := T_PackageClassesAssociation.Create(Self, 'Classes');
  _PermissaoPackage := T_PackagePermissaoPackageAssociation.Create(Self, 'PermissaoPackage');
end;

procedure T_Package.InternalFree; begin
  if _Ocupacoes <> nil then _Ocupacoes.InternalFree;
  if _Classes <> nil then _Classes.InternalFree;
  if _PermissaoPackage <> nil then _PermissaoPackage.InternalFree;
  inherited;
end;

function T_Package.GetNome : String; begin Result := T_Package(Prevalence.GetNewImage(Self, 2))._Nome end;

procedure T_Package.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Package(NewImage)._Nome <> Value then begin
        T_Package(NewImage)._Nome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Package.PorNome : String; begin
  Result := Nome
end;

{ T_PackageList }

class function T_PackageList.GetObjectClass : TTransientClass; begin
  Result := T_Package
end;

procedure T_PackageList.Add(_Package : T_Package); begin
  inherited Add(TPrevalent(_Package))
end;

procedure T_PackageList.Delete(_Package : T_Package); begin
  inherited Delete(TPrevalent(_Package))
end;

function T_PackageList.Find(I : Integer) : T_Package; begin
  Result := T_Package(inherited Find(I))
end;

function T_PackageList.First : T_Package; begin
  Result := T_Package(inherited First)
end;

function T_PackageList.Last : T_Package; begin
  Result := T_Package(inherited Last)
end;

function T_PackageList.Near(I : Integer) : T_Package; begin
  Result := T_Package(inherited Near(I))
end;

function T_PackageList.Next(var _Package : T_Package) : boolean; begin
  Result := inherited Next(TTransient(_Package))
end;

function T_PackageList.Prior(var _Package : T_Package) : boolean; begin
  Result := inherited Prior(TTransient(_Package))
end;

function T_PackageList.Get_Package(I : Integer) : T_Package; begin
  Result := T_Package(Objects[I])
end;

{ T_PackagePorNomeList }

class function T_PackagePorNomeList.GetKeyCode : pointer; begin
  Result := @T_Package.PorNome
end;

class function T_PackagePorNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_PackagePorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_Package
end;

procedure T_PackagePorNomeList.Add(_Package : T_Package); begin
  inherited Add(TPrevalent(_Package))
end;

procedure T_PackagePorNomeList.Delete(_Package : T_Package); begin
  inherited Delete(TPrevalent(_Package))
end;

function T_PackagePorNomeList.Find(S : String) : T_Package; begin
  Result := T_Package(inherited Find(S))
end;

function T_PackagePorNomeList.First : T_Package; begin
  Result := T_Package(inherited First)
end;

function T_PackagePorNomeList.Last : T_Package; begin
  Result := T_Package(inherited Last)
end;

function T_PackagePorNomeList.Near(S : String) : T_Package; begin
  Result := T_Package(inherited Near(S))
end;

function T_PackagePorNomeList.Next(var _Package : T_Package) : boolean; begin
  Result := inherited Next(TTransient(_Package))
end;

function T_PackagePorNomeList.Prior(var _Package : T_Package) : boolean; begin
  Result := inherited Prior(TTransient(_Package))
end;

function T_PackagePorNomeList.Get_Package(I : Integer) : T_Package; begin
  Result := T_Package(Objects[I])
end;

function T_PermissaoClasse.GetIdentification : string; begin
  try Result := Classe.Package.Nome + '.' + Classe.Nome+'->'+PermissaoPackage.Grupo.Dominio.Nome +'/'+PermissaoPackage.Grupo.Nome+'.'+PermissaoPackage.Package.Nome except Result := ' ' end;
end;

function T_PermissaoClasse.GetPermissoes : T_ClassPermissionSet; begin Result := T_PermissaoClasse(Prevalence.GetNewImage(Self, 2))._Permissoes end;

procedure T_PermissaoClasse.SetPermissoes(Value : T_ClassPermissionSet); begin
  if Prevalence.IsInRecoverSnapShot then _Permissoes := Value else begin
    if (_Permissoes = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoClasse(NewImage)._Permissoes <> Value then begin
        T_PermissaoClasse(NewImage)._Permissoes := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_PermissaoClasse.GetClasse : T_Classe; begin Result := T_PermissaoClasse(Prevalence.GetNewImage(Self, 3))._Classe end;

procedure T_PermissaoClasse.SetClasse(Value : T_Classe); begin
  if Prevalence.IsInRecover then _Classe := Value else begin
    if (_Classe = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoClasse(NewImage)._Classe <> Value then begin
        T_PermissaoClasse(NewImage)._Classe := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function T_PermissaoClasse.GetPermissaoPackage : T_PermissaoPackage; begin Result := T_PermissaoClasse(Prevalence.GetNewImage(Self, 4))._PermissaoPackage end;

procedure T_PermissaoClasse.SetPermissaoPackage(Value : T_PermissaoPackage); begin
  if Prevalence.IsInRecover then _PermissaoPackage := Value else begin
    if (_PermissaoPackage = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoClasse(NewImage)._PermissaoPackage <> Value then begin
        T_PermissaoClasse(NewImage)._PermissaoPackage := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

{ T_PermissaoClasseList }

class function T_PermissaoClasseList.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoClasse
end;

procedure T_PermissaoClasseList.Add(_PermissaoClasse : T_PermissaoClasse); begin
  inherited Add(TPrevalent(_PermissaoClasse))
end;

procedure T_PermissaoClasseList.Delete(_PermissaoClasse : T_PermissaoClasse); begin
  inherited Delete(TPrevalent(_PermissaoClasse))
end;

function T_PermissaoClasseList.Find(I : Integer) : T_PermissaoClasse; begin
  Result := T_PermissaoClasse(inherited Find(I))
end;

function T_PermissaoClasseList.First : T_PermissaoClasse; begin
  Result := T_PermissaoClasse(inherited First)
end;

function T_PermissaoClasseList.Last : T_PermissaoClasse; begin
  Result := T_PermissaoClasse(inherited Last)
end;

function T_PermissaoClasseList.Near(I : Integer) : T_PermissaoClasse; begin
  Result := T_PermissaoClasse(inherited Near(I))
end;

function T_PermissaoClasseList.Next(var _PermissaoClasse : T_PermissaoClasse) : boolean; begin
  Result := inherited Next(TTransient(_PermissaoClasse))
end;

function T_PermissaoClasseList.Prior(var _PermissaoClasse : T_PermissaoClasse) : boolean; begin
  Result := inherited Prior(TTransient(_PermissaoClasse))
end;

function T_PermissaoClasseList.Get_PermissaoClasse(I : Integer) : T_PermissaoClasse; begin
  Result := T_PermissaoClasse(Objects[I])
end;

{ T_ClassePermissoesClassesAssociation }

class function T_ClassePermissoesClassesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoClasse
end;

procedure T_ClassePermissoesClassesAssociation.Add(_PermissaoClasse : T_PermissaoClasse); begin
  inherited Add(TPrevalent(_PermissaoClasse))
end;

procedure T_ClassePermissoesClassesAssociation.Delete(_PermissaoClasse : T_PermissaoClasse); begin
  inherited Delete(TPrevalent(_PermissaoClasse))
end;

function T_ClassePermissoesClassesAssociation.Find(I : Integer) : T_PermissaoClasse; begin
  Result := T_PermissaoClasse(inherited Find(I))
end;

function T_ClassePermissoesClassesAssociation.First : T_PermissaoClasse; begin
  SetDependencyLists;
  Result := T_PermissaoClasse(inherited First)
end;

function T_ClassePermissoesClassesAssociation.Last : T_PermissaoClasse; begin
  SetDependencyLists;
  Result := T_PermissaoClasse(inherited Last)
end;

function T_ClassePermissoesClassesAssociation.Near(I : Integer) : T_PermissaoClasse; begin
  SetDependencyLists;
  Result := T_PermissaoClasse(inherited Near(I))
end;

function T_ClassePermissoesClassesAssociation.Next(var _PermissaoClasse : T_PermissaoClasse) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_PermissaoClasse))
end;

function T_ClassePermissoesClassesAssociation.Prior(var _PermissaoClasse : T_PermissaoClasse) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_PermissaoClasse))
end;

function T_ClassePermissoesClassesAssociation.Get_PermissaoClasse(I : Integer) : T_PermissaoClasse; begin
  Result := T_PermissaoClasse(Objects[I])
end;

{ T_PermissaoPackagePermissoesClassesAssociation }

class function T_PermissaoPackagePermissoesClassesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoClasse
end;

procedure T_PermissaoPackagePermissoesClassesAssociation.Add(_PermissaoClasse : T_PermissaoClasse); begin
  inherited Add(TPrevalent(_PermissaoClasse))
end;

procedure T_PermissaoPackagePermissoesClassesAssociation.Delete(_PermissaoClasse : T_PermissaoClasse); begin
  inherited Delete(TPrevalent(_PermissaoClasse))
end;

function T_PermissaoPackagePermissoesClassesAssociation.Find(I : Integer) : T_PermissaoClasse; begin
  Result := T_PermissaoClasse(inherited Find(I))
end;

function T_PermissaoPackagePermissoesClassesAssociation.First : T_PermissaoClasse; begin
  SetDependencyLists;
  Result := T_PermissaoClasse(inherited First)
end;

function T_PermissaoPackagePermissoesClassesAssociation.Last : T_PermissaoClasse; begin
  SetDependencyLists;
  Result := T_PermissaoClasse(inherited Last)
end;

function T_PermissaoPackagePermissoesClassesAssociation.Near(I : Integer) : T_PermissaoClasse; begin
  SetDependencyLists;
  Result := T_PermissaoClasse(inherited Near(I))
end;

function T_PermissaoPackagePermissoesClassesAssociation.Next(var _PermissaoClasse : T_PermissaoClasse) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_PermissaoClasse))
end;

function T_PermissaoPackagePermissoesClassesAssociation.Prior(var _PermissaoClasse : T_PermissaoClasse) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_PermissaoClasse))
end;

function T_PermissaoPackagePermissoesClassesAssociation.Get_PermissaoClasse(I : Integer) : T_PermissaoClasse; begin
  Result := T_PermissaoClasse(Objects[I])
end;

procedure T_PermissaoMetodo.New; begin
  inherited;
  if Prevalence.IsInRecover then exit;
  try _Permissao := True except end;
end;

function T_PermissaoMetodo.GetIdentification : string; begin
  try Result := Metodo.Classe.Package.Nome + '.' + Metodo.Classe.Nome + '.' + Metodo.Nome+'->'+PermissaoPackage.Grupo.Dominio.Nome +'/'+PermissaoPackage.Grupo.Nome+'.'+PermissaoPackage.Package.Nome except Result := ' ' end;
end;

function T_PermissaoMetodo.GetPermissao : Boolean; begin Result := T_PermissaoMetodo(Prevalence.GetNewImage(Self, 2))._Permissao end;

procedure T_PermissaoMetodo.SetPermissao(Value : Boolean); begin
  if Prevalence.IsInRecoverSnapShot then _Permissao := Value else begin
    if (_Permissao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoMetodo(NewImage)._Permissao <> Value then begin
        T_PermissaoMetodo(NewImage)._Permissao := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_PermissaoMetodo.GetMetodo : T_Metodo; begin Result := T_PermissaoMetodo(Prevalence.GetNewImage(Self, 3))._Metodo end;

procedure T_PermissaoMetodo.SetMetodo(Value : T_Metodo); begin
  if Prevalence.IsInRecover then _Metodo := Value else begin
    if (_Metodo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoMetodo(NewImage)._Metodo <> Value then begin
        T_PermissaoMetodo(NewImage)._Metodo := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function T_PermissaoMetodo.GetPermissaoPackage : T_PermissaoPackage; begin Result := T_PermissaoMetodo(Prevalence.GetNewImage(Self, 4))._PermissaoPackage end;

procedure T_PermissaoMetodo.SetPermissaoPackage(Value : T_PermissaoPackage); begin
  if Prevalence.IsInRecover then _PermissaoPackage := Value else begin
    if (_PermissaoPackage = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoMetodo(NewImage)._PermissaoPackage <> Value then begin
        T_PermissaoMetodo(NewImage)._PermissaoPackage := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

{ T_PermissaoMetodoList }

class function T_PermissaoMetodoList.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoMetodo
end;

procedure T_PermissaoMetodoList.Add(_PermissaoMetodo : T_PermissaoMetodo); begin
  inherited Add(TPrevalent(_PermissaoMetodo))
end;

procedure T_PermissaoMetodoList.Delete(_PermissaoMetodo : T_PermissaoMetodo); begin
  inherited Delete(TPrevalent(_PermissaoMetodo))
end;

function T_PermissaoMetodoList.Find(I : Integer) : T_PermissaoMetodo; begin
  Result := T_PermissaoMetodo(inherited Find(I))
end;

function T_PermissaoMetodoList.First : T_PermissaoMetodo; begin
  Result := T_PermissaoMetodo(inherited First)
end;

function T_PermissaoMetodoList.Last : T_PermissaoMetodo; begin
  Result := T_PermissaoMetodo(inherited Last)
end;

function T_PermissaoMetodoList.Near(I : Integer) : T_PermissaoMetodo; begin
  Result := T_PermissaoMetodo(inherited Near(I))
end;

function T_PermissaoMetodoList.Next(var _PermissaoMetodo : T_PermissaoMetodo) : boolean; begin
  Result := inherited Next(TTransient(_PermissaoMetodo))
end;

function T_PermissaoMetodoList.Prior(var _PermissaoMetodo : T_PermissaoMetodo) : boolean; begin
  Result := inherited Prior(TTransient(_PermissaoMetodo))
end;

function T_PermissaoMetodoList.Get_PermissaoMetodo(I : Integer) : T_PermissaoMetodo; begin
  Result := T_PermissaoMetodo(Objects[I])
end;

{ T_MetodoPermissoesMetodosAssociation }

class function T_MetodoPermissoesMetodosAssociation.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoMetodo
end;

procedure T_MetodoPermissoesMetodosAssociation.Add(_PermissaoMetodo : T_PermissaoMetodo); begin
  inherited Add(TPrevalent(_PermissaoMetodo))
end;

procedure T_MetodoPermissoesMetodosAssociation.Delete(_PermissaoMetodo : T_PermissaoMetodo); begin
  inherited Delete(TPrevalent(_PermissaoMetodo))
end;

function T_MetodoPermissoesMetodosAssociation.Find(I : Integer) : T_PermissaoMetodo; begin
  Result := T_PermissaoMetodo(inherited Find(I))
end;

function T_MetodoPermissoesMetodosAssociation.First : T_PermissaoMetodo; begin
  SetDependencyLists;
  Result := T_PermissaoMetodo(inherited First)
end;

function T_MetodoPermissoesMetodosAssociation.Last : T_PermissaoMetodo; begin
  SetDependencyLists;
  Result := T_PermissaoMetodo(inherited Last)
end;

function T_MetodoPermissoesMetodosAssociation.Near(I : Integer) : T_PermissaoMetodo; begin
  SetDependencyLists;
  Result := T_PermissaoMetodo(inherited Near(I))
end;

function T_MetodoPermissoesMetodosAssociation.Next(var _PermissaoMetodo : T_PermissaoMetodo) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_PermissaoMetodo))
end;

function T_MetodoPermissoesMetodosAssociation.Prior(var _PermissaoMetodo : T_PermissaoMetodo) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_PermissaoMetodo))
end;

function T_MetodoPermissoesMetodosAssociation.Get_PermissaoMetodo(I : Integer) : T_PermissaoMetodo; begin
  Result := T_PermissaoMetodo(Objects[I])
end;

{ T_PermissaoPackagePermissoesMetodosAssociation }

class function T_PermissaoPackagePermissoesMetodosAssociation.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoMetodo
end;

procedure T_PermissaoPackagePermissoesMetodosAssociation.Add(_PermissaoMetodo : T_PermissaoMetodo); begin
  inherited Add(TPrevalent(_PermissaoMetodo))
end;

procedure T_PermissaoPackagePermissoesMetodosAssociation.Delete(_PermissaoMetodo : T_PermissaoMetodo); begin
  inherited Delete(TPrevalent(_PermissaoMetodo))
end;

function T_PermissaoPackagePermissoesMetodosAssociation.Find(I : Integer) : T_PermissaoMetodo; begin
  Result := T_PermissaoMetodo(inherited Find(I))
end;

function T_PermissaoPackagePermissoesMetodosAssociation.First : T_PermissaoMetodo; begin
  SetDependencyLists;
  Result := T_PermissaoMetodo(inherited First)
end;

function T_PermissaoPackagePermissoesMetodosAssociation.Last : T_PermissaoMetodo; begin
  SetDependencyLists;
  Result := T_PermissaoMetodo(inherited Last)
end;

function T_PermissaoPackagePermissoesMetodosAssociation.Near(I : Integer) : T_PermissaoMetodo; begin
  SetDependencyLists;
  Result := T_PermissaoMetodo(inherited Near(I))
end;

function T_PermissaoPackagePermissoesMetodosAssociation.Next(var _PermissaoMetodo : T_PermissaoMetodo) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_PermissaoMetodo))
end;

function T_PermissaoPackagePermissoesMetodosAssociation.Prior(var _PermissaoMetodo : T_PermissaoMetodo) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_PermissaoMetodo))
end;

function T_PermissaoPackagePermissoesMetodosAssociation.Get_PermissaoMetodo(I : Integer) : T_PermissaoMetodo; begin
  Result := T_PermissaoMetodo(Objects[I])
end;

procedure T_PermissaoPackage.New; begin
  inherited;
  _PermissoesClasses := T_PermissaoPackagePermissoesClassesAssociation.Create(Self, 'PermissoesClasses');
  _PermissoesMetodos := T_PermissaoPackagePermissoesMetodosAssociation.Create(Self, 'PermissoesMetodos');
  _PermissoesViews := T_PermissaoPackagePermissoesViewsAssociation.Create(Self, 'PermissoesViews');
  _PermissoesPropriedades := T_PermissaoPackagePermissoesPropriedadesAssociation.Create(Self, 'PermissoesPropriedades');
  if Prevalence.IsInRecover then exit;
  try _Ativa := True except end;
  try _PermissaoMetodo := False except end;
end;

procedure T_PermissaoPackage.InternalFree; begin
  if _PermissoesClasses <> nil then _PermissoesClasses.InternalFree;
  if _PermissoesMetodos <> nil then _PermissoesMetodos.InternalFree;
  if _PermissoesViews <> nil then _PermissoesViews.InternalFree;
  if _PermissoesPropriedades <> nil then _PermissoesPropriedades.InternalFree;
  inherited;
end;

function T_PermissaoPackage.GetIdentification : string; begin
  try Result := Grupo.Dominio.Nome +'/'+Grupo.Nome+'.'+Package.Nome except Result := ' ' end;
end;

function T_PermissaoPackage.GetAtiva : Boolean; begin Result := T_PermissaoPackage(Prevalence.GetNewImage(Self, 2))._Ativa end;

procedure T_PermissaoPackage.SetAtiva(Value : Boolean); begin
  if Prevalence.IsInRecoverSnapShot then _Ativa := Value else begin
    if (_Ativa = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoPackage(NewImage)._Ativa <> Value then begin
        T_PermissaoPackage(NewImage)._Ativa := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_PermissaoPackage.GetPermissaoClasse : T_ClassPermissionSet; begin Result := T_PermissaoPackage(Prevalence.GetNewImage(Self, 3))._PermissaoClasse end;

procedure T_PermissaoPackage.SetPermissaoClasse(Value : T_ClassPermissionSet); begin
  if Prevalence.IsInRecoverSnapShot then _PermissaoClasse := Value else begin
    if (_PermissaoClasse = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoPackage(NewImage)._PermissaoClasse <> Value then begin
        T_PermissaoPackage(NewImage)._PermissaoClasse := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_PermissaoPackage.GetPermissaoPropriedade : T_PropPermission; begin Result := T_PermissaoPackage(Prevalence.GetNewImage(Self, 4))._PermissaoPropriedade end;

procedure T_PermissaoPackage.SetPermissaoPropriedade(Value : T_PropPermission); begin
  if Prevalence.IsInRecoverSnapShot then _PermissaoPropriedade := Value else begin
    if (_PermissaoPropriedade = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoPackage(NewImage)._PermissaoPropriedade <> Value then begin
        T_PermissaoPackage(NewImage)._PermissaoPropriedade := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

function T_PermissaoPackage.GetPermissaoMetodo : Boolean; begin Result := T_PermissaoPackage(Prevalence.GetNewImage(Self, 5))._PermissaoMetodo end;

procedure T_PermissaoPackage.SetPermissaoMetodo(Value : Boolean); begin
  if Prevalence.IsInRecoverSnapShot then _PermissaoMetodo := Value else begin
    if (_PermissaoMetodo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoPackage(NewImage)._PermissaoMetodo <> Value then begin
        T_PermissaoPackage(NewImage)._PermissaoMetodo := Value;
        UpdateLog(5, NewImage, Stream)
      end;
    end;
end;

function T_PermissaoPackage.GetPackage : T_Package; begin Result := T_PermissaoPackage(Prevalence.GetNewImage(Self, 8))._Package end;

procedure T_PermissaoPackage.SetPackage(Value : T_Package); begin
  if Prevalence.IsInRecover then _Package := Value else begin
    if (_Package = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoPackage(NewImage)._Package <> Value then begin
        T_PermissaoPackage(NewImage)._Package := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

function T_PermissaoPackage.GetGrupo : T_Grupo; begin Result := T_PermissaoPackage(Prevalence.GetNewImage(Self, 11))._Grupo end;

procedure T_PermissaoPackage.SetGrupo(Value : T_Grupo); begin
  if Prevalence.IsInRecover then _Grupo := Value else begin
    if (_Grupo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoPackage(NewImage)._Grupo <> Value then begin
        T_PermissaoPackage(NewImage)._Grupo := Value;
        UpdateLog(11, NewImage, Stream)
      end;
  end;
end;

{ T_PermissaoPackageList }

class function T_PermissaoPackageList.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoPackage
end;

procedure T_PermissaoPackageList.Add(_PermissaoPackage : T_PermissaoPackage); begin
  inherited Add(TPrevalent(_PermissaoPackage))
end;

procedure T_PermissaoPackageList.Delete(_PermissaoPackage : T_PermissaoPackage); begin
  inherited Delete(TPrevalent(_PermissaoPackage))
end;

function T_PermissaoPackageList.Find(I : Integer) : T_PermissaoPackage; begin
  Result := T_PermissaoPackage(inherited Find(I))
end;

function T_PermissaoPackageList.First : T_PermissaoPackage; begin
  Result := T_PermissaoPackage(inherited First)
end;

function T_PermissaoPackageList.Last : T_PermissaoPackage; begin
  Result := T_PermissaoPackage(inherited Last)
end;

function T_PermissaoPackageList.Near(I : Integer) : T_PermissaoPackage; begin
  Result := T_PermissaoPackage(inherited Near(I))
end;

function T_PermissaoPackageList.Next(var _PermissaoPackage : T_PermissaoPackage) : boolean; begin
  Result := inherited Next(TTransient(_PermissaoPackage))
end;

function T_PermissaoPackageList.Prior(var _PermissaoPackage : T_PermissaoPackage) : boolean; begin
  Result := inherited Prior(TTransient(_PermissaoPackage))
end;

function T_PermissaoPackageList.Get_PermissaoPackage(I : Integer) : T_PermissaoPackage; begin
  Result := T_PermissaoPackage(Objects[I])
end;

{ T_PackagePermissaoPackageAssociation }

class function T_PackagePermissaoPackageAssociation.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoPackage
end;

procedure T_PackagePermissaoPackageAssociation.Add(_PermissaoPackage : T_PermissaoPackage); begin
  inherited Add(TPrevalent(_PermissaoPackage))
end;

procedure T_PackagePermissaoPackageAssociation.Delete(_PermissaoPackage : T_PermissaoPackage); begin
  inherited Delete(TPrevalent(_PermissaoPackage))
end;

function T_PackagePermissaoPackageAssociation.Find(I : Integer) : T_PermissaoPackage; begin
  Result := T_PermissaoPackage(inherited Find(I))
end;

function T_PackagePermissaoPackageAssociation.First : T_PermissaoPackage; begin
  SetDependencyLists;
  Result := T_PermissaoPackage(inherited First)
end;

function T_PackagePermissaoPackageAssociation.Last : T_PermissaoPackage; begin
  SetDependencyLists;
  Result := T_PermissaoPackage(inherited Last)
end;

function T_PackagePermissaoPackageAssociation.Near(I : Integer) : T_PermissaoPackage; begin
  SetDependencyLists;
  Result := T_PermissaoPackage(inherited Near(I))
end;

function T_PackagePermissaoPackageAssociation.Next(var _PermissaoPackage : T_PermissaoPackage) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_PermissaoPackage))
end;

function T_PackagePermissaoPackageAssociation.Prior(var _PermissaoPackage : T_PermissaoPackage) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_PermissaoPackage))
end;

function T_PackagePermissaoPackageAssociation.Get_PermissaoPackage(I : Integer) : T_PermissaoPackage; begin
  Result := T_PermissaoPackage(Objects[I])
end;

{ T_GrupoPackagesAssociation }

class function T_GrupoPackagesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoPackage
end;

procedure T_GrupoPackagesAssociation.Add(_PermissaoPackage : T_PermissaoPackage); begin
  inherited Add(TPrevalent(_PermissaoPackage))
end;

procedure T_GrupoPackagesAssociation.Delete(_PermissaoPackage : T_PermissaoPackage); begin
  inherited Delete(TPrevalent(_PermissaoPackage))
end;

function T_GrupoPackagesAssociation.Find(I : Integer) : T_PermissaoPackage; begin
  Result := T_PermissaoPackage(inherited Find(I))
end;

function T_GrupoPackagesAssociation.First : T_PermissaoPackage; begin
  SetDependencyLists;
  Result := T_PermissaoPackage(inherited First)
end;

function T_GrupoPackagesAssociation.Last : T_PermissaoPackage; begin
  SetDependencyLists;
  Result := T_PermissaoPackage(inherited Last)
end;

function T_GrupoPackagesAssociation.Near(I : Integer) : T_PermissaoPackage; begin
  SetDependencyLists;
  Result := T_PermissaoPackage(inherited Near(I))
end;

function T_GrupoPackagesAssociation.Next(var _PermissaoPackage : T_PermissaoPackage) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_PermissaoPackage))
end;

function T_GrupoPackagesAssociation.Prior(var _PermissaoPackage : T_PermissaoPackage) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_PermissaoPackage))
end;

function T_GrupoPackagesAssociation.Get_PermissaoPackage(I : Integer) : T_PermissaoPackage; begin
  Result := T_PermissaoPackage(Objects[I])
end;

function T_PermissaoPropriedade.GetIdentification : string; begin
  try Result := Propriedade.Classe.Package.Nome + '.' + Propriedade.Classe.Nome + '.' + Propriedade.Nome+'->'+PermissaoPackage.Grupo.Dominio.Nome +'/'+PermissaoPackage.Grupo.Nome+'.'+PermissaoPackage.Package.Nome except Result := ' ' end;
end;

function T_PermissaoPropriedade.GetPermissao : T_PropPermission; begin Result := T_PermissaoPropriedade(Prevalence.GetNewImage(Self, 2))._Permissao end;

procedure T_PermissaoPropriedade.SetPermissao(Value : T_PropPermission); begin
  if Prevalence.IsInRecoverSnapShot then _Permissao := Value else begin
    if (_Permissao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoPropriedade(NewImage)._Permissao <> Value then begin
        T_PermissaoPropriedade(NewImage)._Permissao := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_PermissaoPropriedade.GetPermissaoPackage : T_PermissaoPackage; begin Result := T_PermissaoPropriedade(Prevalence.GetNewImage(Self, 3))._PermissaoPackage end;

procedure T_PermissaoPropriedade.SetPermissaoPackage(Value : T_PermissaoPackage); begin
  if Prevalence.IsInRecover then _PermissaoPackage := Value else begin
    if (_PermissaoPackage = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoPropriedade(NewImage)._PermissaoPackage <> Value then begin
        T_PermissaoPropriedade(NewImage)._PermissaoPackage := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

function T_PermissaoPropriedade.GetPropriedade : T_Propriedade; begin Result := T_PermissaoPropriedade(Prevalence.GetNewImage(Self, 4))._Propriedade end;

procedure T_PermissaoPropriedade.SetPropriedade(Value : T_Propriedade); begin
  if Prevalence.IsInRecover then _Propriedade := Value else begin
    if (_Propriedade = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoPropriedade(NewImage)._Propriedade <> Value then begin
        T_PermissaoPropriedade(NewImage)._Propriedade := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

{ T_PermissaoPropriedadeList }

class function T_PermissaoPropriedadeList.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoPropriedade
end;

procedure T_PermissaoPropriedadeList.Add(_PermissaoPropriedade : T_PermissaoPropriedade); begin
  inherited Add(TPrevalent(_PermissaoPropriedade))
end;

procedure T_PermissaoPropriedadeList.Delete(_PermissaoPropriedade : T_PermissaoPropriedade); begin
  inherited Delete(TPrevalent(_PermissaoPropriedade))
end;

function T_PermissaoPropriedadeList.Find(I : Integer) : T_PermissaoPropriedade; begin
  Result := T_PermissaoPropriedade(inherited Find(I))
end;

function T_PermissaoPropriedadeList.First : T_PermissaoPropriedade; begin
  Result := T_PermissaoPropriedade(inherited First)
end;

function T_PermissaoPropriedadeList.Last : T_PermissaoPropriedade; begin
  Result := T_PermissaoPropriedade(inherited Last)
end;

function T_PermissaoPropriedadeList.Near(I : Integer) : T_PermissaoPropriedade; begin
  Result := T_PermissaoPropriedade(inherited Near(I))
end;

function T_PermissaoPropriedadeList.Next(var _PermissaoPropriedade : T_PermissaoPropriedade) : boolean; begin
  Result := inherited Next(TTransient(_PermissaoPropriedade))
end;

function T_PermissaoPropriedadeList.Prior(var _PermissaoPropriedade : T_PermissaoPropriedade) : boolean; begin
  Result := inherited Prior(TTransient(_PermissaoPropriedade))
end;

function T_PermissaoPropriedadeList.Get_PermissaoPropriedade(I : Integer) : T_PermissaoPropriedade; begin
  Result := T_PermissaoPropriedade(Objects[I])
end;

{ T_PermissaoPackagePermissoesPropriedadesAssociation }

class function T_PermissaoPackagePermissoesPropriedadesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoPropriedade
end;

procedure T_PermissaoPackagePermissoesPropriedadesAssociation.Add(_PermissaoPropriedade : T_PermissaoPropriedade); begin
  inherited Add(TPrevalent(_PermissaoPropriedade))
end;

procedure T_PermissaoPackagePermissoesPropriedadesAssociation.Delete(_PermissaoPropriedade : T_PermissaoPropriedade); begin
  inherited Delete(TPrevalent(_PermissaoPropriedade))
end;

function T_PermissaoPackagePermissoesPropriedadesAssociation.Find(I : Integer) : T_PermissaoPropriedade; begin
  Result := T_PermissaoPropriedade(inherited Find(I))
end;

function T_PermissaoPackagePermissoesPropriedadesAssociation.First : T_PermissaoPropriedade; begin
  SetDependencyLists;
  Result := T_PermissaoPropriedade(inherited First)
end;

function T_PermissaoPackagePermissoesPropriedadesAssociation.Last : T_PermissaoPropriedade; begin
  SetDependencyLists;
  Result := T_PermissaoPropriedade(inherited Last)
end;

function T_PermissaoPackagePermissoesPropriedadesAssociation.Near(I : Integer) : T_PermissaoPropriedade; begin
  SetDependencyLists;
  Result := T_PermissaoPropriedade(inherited Near(I))
end;

function T_PermissaoPackagePermissoesPropriedadesAssociation.Next(var _PermissaoPropriedade : T_PermissaoPropriedade) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_PermissaoPropriedade))
end;

function T_PermissaoPackagePermissoesPropriedadesAssociation.Prior(var _PermissaoPropriedade : T_PermissaoPropriedade) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_PermissaoPropriedade))
end;

function T_PermissaoPackagePermissoesPropriedadesAssociation.Get_PermissaoPropriedade(I : Integer) : T_PermissaoPropriedade; begin
  Result := T_PermissaoPropriedade(Objects[I])
end;

{ T_PropriedadePermissoesPropriedadesAssociation }

class function T_PropriedadePermissoesPropriedadesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoPropriedade
end;

procedure T_PropriedadePermissoesPropriedadesAssociation.Add(_PermissaoPropriedade : T_PermissaoPropriedade); begin
  inherited Add(TPrevalent(_PermissaoPropriedade))
end;

procedure T_PropriedadePermissoesPropriedadesAssociation.Delete(_PermissaoPropriedade : T_PermissaoPropriedade); begin
  inherited Delete(TPrevalent(_PermissaoPropriedade))
end;

function T_PropriedadePermissoesPropriedadesAssociation.Find(I : Integer) : T_PermissaoPropriedade; begin
  Result := T_PermissaoPropriedade(inherited Find(I))
end;

function T_PropriedadePermissoesPropriedadesAssociation.First : T_PermissaoPropriedade; begin
  SetDependencyLists;
  Result := T_PermissaoPropriedade(inherited First)
end;

function T_PropriedadePermissoesPropriedadesAssociation.Last : T_PermissaoPropriedade; begin
  SetDependencyLists;
  Result := T_PermissaoPropriedade(inherited Last)
end;

function T_PropriedadePermissoesPropriedadesAssociation.Near(I : Integer) : T_PermissaoPropriedade; begin
  SetDependencyLists;
  Result := T_PermissaoPropriedade(inherited Near(I))
end;

function T_PropriedadePermissoesPropriedadesAssociation.Next(var _PermissaoPropriedade : T_PermissaoPropriedade) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_PermissaoPropriedade))
end;

function T_PropriedadePermissoesPropriedadesAssociation.Prior(var _PermissaoPropriedade : T_PermissaoPropriedade) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_PermissaoPropriedade))
end;

function T_PropriedadePermissoesPropriedadesAssociation.Get_PermissaoPropriedade(I : Integer) : T_PermissaoPropriedade; begin
  Result := T_PermissaoPropriedade(Objects[I])
end;

function T_PermissaoView.GetParametro : String; begin Result := T_PermissaoView(Prevalence.GetNewImage(Self, 2))._Parametro end;

procedure T_PermissaoView.SetParametro(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Parametro := Value else begin
    if (_Parametro = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoView(NewImage)._Parametro <> Value then begin
        T_PermissaoView(NewImage)._Parametro := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_PermissaoView.GetPermissaoVerdadeiro : T_ViewPermissionSet; begin Result := T_PermissaoView(Prevalence.GetNewImage(Self, 3))._PermissaoVerdadeiro end;

procedure T_PermissaoView.SetPermissaoVerdadeiro(Value : T_ViewPermissionSet); begin
  if Prevalence.IsInRecoverSnapShot then _PermissaoVerdadeiro := Value else begin
    if (_PermissaoVerdadeiro = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoView(NewImage)._PermissaoVerdadeiro <> Value then begin
        T_PermissaoView(NewImage)._PermissaoVerdadeiro := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_PermissaoView.GetPermissaoFalso : T_ViewPermissionSet; begin Result := T_PermissaoView(Prevalence.GetNewImage(Self, 4))._PermissaoFalso end;

procedure T_PermissaoView.SetPermissaoFalso(Value : T_ViewPermissionSet); begin
  if Prevalence.IsInRecoverSnapShot then _PermissaoFalso := Value else begin
    if (_PermissaoFalso = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoView(NewImage)._PermissaoFalso <> Value then begin
        T_PermissaoView(NewImage)._PermissaoFalso := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

function T_PermissaoView.GetPermissaoPackage : T_PermissaoPackage; begin Result := T_PermissaoView(Prevalence.GetNewImage(Self, 5))._PermissaoPackage end;

procedure T_PermissaoView.SetPermissaoPackage(Value : T_PermissaoPackage); begin
  if Prevalence.IsInRecover then _PermissaoPackage := Value else begin
    if (_PermissaoPackage = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoView(NewImage)._PermissaoPackage <> Value then begin
        T_PermissaoView(NewImage)._PermissaoPackage := Value;
        UpdateLog(5, NewImage, Stream)
      end;
  end;
end;

function T_PermissaoView.GetView : T_Metodo; begin Result := T_PermissaoView(Prevalence.GetNewImage(Self, 6))._View end;

procedure T_PermissaoView.SetView(Value : T_Metodo); begin
  if Prevalence.IsInRecover then _View := Value else begin
    if (_View = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PermissaoView(NewImage)._View <> Value then begin
        T_PermissaoView(NewImage)._View := Value;
        UpdateLog(6, NewImage, Stream)
      end;
  end;
end;

function T_PermissaoView.AssociationConstraintView : TObjectList; begin
  Result := _MetodoPorViewList;
end;

{ T_PermissaoViewList }

class function T_PermissaoViewList.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoView
end;

procedure T_PermissaoViewList.Add(_PermissaoView : T_PermissaoView); begin
  inherited Add(TPrevalent(_PermissaoView))
end;

procedure T_PermissaoViewList.Delete(_PermissaoView : T_PermissaoView); begin
  inherited Delete(TPrevalent(_PermissaoView))
end;

function T_PermissaoViewList.Find(I : Integer) : T_PermissaoView; begin
  Result := T_PermissaoView(inherited Find(I))
end;

function T_PermissaoViewList.First : T_PermissaoView; begin
  Result := T_PermissaoView(inherited First)
end;

function T_PermissaoViewList.Last : T_PermissaoView; begin
  Result := T_PermissaoView(inherited Last)
end;

function T_PermissaoViewList.Near(I : Integer) : T_PermissaoView; begin
  Result := T_PermissaoView(inherited Near(I))
end;

function T_PermissaoViewList.Next(var _PermissaoView : T_PermissaoView) : boolean; begin
  Result := inherited Next(TTransient(_PermissaoView))
end;

function T_PermissaoViewList.Prior(var _PermissaoView : T_PermissaoView) : boolean; begin
  Result := inherited Prior(TTransient(_PermissaoView))
end;

function T_PermissaoViewList.Get_PermissaoView(I : Integer) : T_PermissaoView; begin
  Result := T_PermissaoView(Objects[I])
end;

{ T_PermissaoPackagePermissoesViewsAssociation }

class function T_PermissaoPackagePermissoesViewsAssociation.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoView
end;

procedure T_PermissaoPackagePermissoesViewsAssociation.Add(_PermissaoView : T_PermissaoView); begin
  inherited Add(TPrevalent(_PermissaoView))
end;

procedure T_PermissaoPackagePermissoesViewsAssociation.Delete(_PermissaoView : T_PermissaoView); begin
  inherited Delete(TPrevalent(_PermissaoView))
end;

function T_PermissaoPackagePermissoesViewsAssociation.Find(I : Integer) : T_PermissaoView; begin
  Result := T_PermissaoView(inherited Find(I))
end;

function T_PermissaoPackagePermissoesViewsAssociation.First : T_PermissaoView; begin
  SetDependencyLists;
  Result := T_PermissaoView(inherited First)
end;

function T_PermissaoPackagePermissoesViewsAssociation.Last : T_PermissaoView; begin
  SetDependencyLists;
  Result := T_PermissaoView(inherited Last)
end;

function T_PermissaoPackagePermissoesViewsAssociation.Near(I : Integer) : T_PermissaoView; begin
  SetDependencyLists;
  Result := T_PermissaoView(inherited Near(I))
end;

function T_PermissaoPackagePermissoesViewsAssociation.Next(var _PermissaoView : T_PermissaoView) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_PermissaoView))
end;

function T_PermissaoPackagePermissoesViewsAssociation.Prior(var _PermissaoView : T_PermissaoView) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_PermissaoView))
end;

function T_PermissaoPackagePermissoesViewsAssociation.Get_PermissaoView(I : Integer) : T_PermissaoView; begin
  Result := T_PermissaoView(Objects[I])
end;

{ T_MetodoPermissoesViewAssociation }

class function T_MetodoPermissoesViewAssociation.GetObjectClass : TTransientClass; begin
  Result := T_PermissaoView
end;

procedure T_MetodoPermissoesViewAssociation.Add(_PermissaoView : T_PermissaoView); begin
  inherited Add(TPrevalent(_PermissaoView))
end;

procedure T_MetodoPermissoesViewAssociation.Delete(_PermissaoView : T_PermissaoView); begin
  inherited Delete(TPrevalent(_PermissaoView))
end;

function T_MetodoPermissoesViewAssociation.Find(I : Integer) : T_PermissaoView; begin
  Result := T_PermissaoView(inherited Find(I))
end;

function T_MetodoPermissoesViewAssociation.First : T_PermissaoView; begin
  SetDependencyLists;
  Result := T_PermissaoView(inherited First)
end;

function T_MetodoPermissoesViewAssociation.Last : T_PermissaoView; begin
  SetDependencyLists;
  Result := T_PermissaoView(inherited Last)
end;

function T_MetodoPermissoesViewAssociation.Near(I : Integer) : T_PermissaoView; begin
  SetDependencyLists;
  Result := T_PermissaoView(inherited Near(I))
end;

function T_MetodoPermissoesViewAssociation.Next(var _PermissaoView : T_PermissaoView) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_PermissaoView))
end;

function T_MetodoPermissoesViewAssociation.Prior(var _PermissaoView : T_PermissaoView) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_PermissaoView))
end;

function T_MetodoPermissoesViewAssociation.Get_PermissaoView(I : Integer) : T_PermissaoView; begin
  Result := T_PermissaoView(Objects[I])
end;

{ T_Propriedade }

procedure T_Propriedade.New; begin
  inherited;
  _PermissoesPropriedades := T_PropriedadePermissoesPropriedadesAssociation.Create(Self, 'PermissoesPropriedades');
end;

procedure T_Propriedade.InternalFree; begin
  if _PermissoesPropriedades <> nil then _PermissoesPropriedades.InternalFree;
  inherited;
end;

function T_Propriedade.GetIdentification : string; begin
  try Result := AliasNome+'('+Classe.Package.Nome + '.' + Classe.Nome+')' except Result := ' ' end;
end;

function T_Propriedade.GetAliasNome : String; begin Result := T_Propriedade(Prevalence.GetNewImage(Self, 2))._AliasNome end;

procedure T_Propriedade.SetAliasNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _AliasNome := Value else begin
    if (_AliasNome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Propriedade(NewImage)._AliasNome <> Value then begin
        T_Propriedade(NewImage)._AliasNome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_Propriedade.GetNome : String; begin Result := T_Propriedade(Prevalence.GetNewImage(Self, 3))._Nome end;

procedure T_Propriedade.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Propriedade(NewImage)._Nome <> Value then begin
        T_Propriedade(NewImage)._Nome := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_Propriedade.GetTipo : T_PropriedadeTipo; begin Result := T_Propriedade(Prevalence.GetNewImage(Self, 4))._Tipo end;

procedure T_Propriedade.SetTipo(Value : T_PropriedadeTipo); begin
  if Prevalence.IsInRecoverSnapShot then _Tipo := Value else begin
    if (_Tipo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Propriedade(NewImage)._Tipo <> Value then begin
        T_Propriedade(NewImage)._Tipo := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

function T_Propriedade.GetPosicao : Integer; begin Result := T_Propriedade(Prevalence.GetNewImage(Self, 5))._Posicao end;

procedure T_Propriedade.SetPosicao(Value : Integer); begin
  if Prevalence.IsInRecoverSnapShot then _Posicao := Value else begin
    if (_Posicao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Propriedade(NewImage)._Posicao <> Value then begin
        T_Propriedade(NewImage)._Posicao := Value;
        UpdateLog(5, NewImage, Stream)
      end;
    end;
end;

function T_Propriedade.GetReadOnly : Boolean; begin Result := T_Propriedade(Prevalence.GetNewImage(Self, 6))._ReadOnly end;

procedure T_Propriedade.SetReadOnly(Value : Boolean); begin
  if Prevalence.IsInRecoverSnapShot then _ReadOnly := Value else begin
    if (_ReadOnly = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Propriedade(NewImage)._ReadOnly <> Value then begin
        T_Propriedade(NewImage)._ReadOnly := Value;
        UpdateLog(6, NewImage, Stream)
      end;
    end;
end;

function T_Propriedade.GetClasse : T_Classe; begin Result := T_Propriedade(Prevalence.GetNewImage(Self, 7))._Classe end;

procedure T_Propriedade.SetClasse(Value : T_Classe); begin
  if Prevalence.IsInRecover then _Classe := Value else begin
    if (_Classe = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Propriedade(NewImage)._Classe <> Value then begin
        T_Propriedade(NewImage)._Classe := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Propriedade.PorNome : String; begin
  Result := Nome
end;

// Cyclomatic Complexity: 1, Baixo
function T_Propriedade.PorAlias : String; begin
  Result := AliasNome
end;

{ T_PropriedadeList }

class function T_PropriedadeList.GetObjectClass : TTransientClass; begin
  Result := T_Propriedade
end;

procedure T_PropriedadeList.Add(_Propriedade : T_Propriedade); begin
  inherited Add(TPrevalent(_Propriedade))
end;

procedure T_PropriedadeList.Delete(_Propriedade : T_Propriedade); begin
  inherited Delete(TPrevalent(_Propriedade))
end;

function T_PropriedadeList.Find(I : Integer) : T_Propriedade; begin
  Result := T_Propriedade(inherited Find(I))
end;

function T_PropriedadeList.First : T_Propriedade; begin
  Result := T_Propriedade(inherited First)
end;

function T_PropriedadeList.Last : T_Propriedade; begin
  Result := T_Propriedade(inherited Last)
end;

function T_PropriedadeList.Near(I : Integer) : T_Propriedade; begin
  Result := T_Propriedade(inherited Near(I))
end;

function T_PropriedadeList.Next(var _Propriedade : T_Propriedade) : boolean; begin
  Result := inherited Next(TTransient(_Propriedade))
end;

function T_PropriedadeList.Prior(var _Propriedade : T_Propriedade) : boolean; begin
  Result := inherited Prior(TTransient(_Propriedade))
end;

function T_PropriedadeList.Get_Propriedade(I : Integer) : T_Propriedade; begin
  Result := T_Propriedade(Objects[I])
end;

{ T_PropriedadePorNomeList }

class function T_PropriedadePorNomeList.GetKeyCode : pointer; begin
  Result := @T_Propriedade.PorNome
end;

class function T_PropriedadePorNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_PropriedadePorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_Propriedade
end;

procedure T_PropriedadePorNomeList.Add(_Propriedade : T_Propriedade); begin
  inherited Add(TPrevalent(_Propriedade))
end;

procedure T_PropriedadePorNomeList.Delete(_Propriedade : T_Propriedade); begin
  inherited Delete(TPrevalent(_Propriedade))
end;

function T_PropriedadePorNomeList.Find(S : String) : T_Propriedade; begin
  Result := T_Propriedade(inherited Find(S))
end;

function T_PropriedadePorNomeList.First : T_Propriedade; begin
  Result := T_Propriedade(inherited First)
end;

function T_PropriedadePorNomeList.Last : T_Propriedade; begin
  Result := T_Propriedade(inherited Last)
end;

function T_PropriedadePorNomeList.Near(S : String) : T_Propriedade; begin
  Result := T_Propriedade(inherited Near(S))
end;

function T_PropriedadePorNomeList.Next(var _Propriedade : T_Propriedade) : boolean; begin
  Result := inherited Next(TTransient(_Propriedade))
end;

function T_PropriedadePorNomeList.Prior(var _Propriedade : T_Propriedade) : boolean; begin
  Result := inherited Prior(TTransient(_Propriedade))
end;

function T_PropriedadePorNomeList.Get_Propriedade(I : Integer) : T_Propriedade; begin
  Result := T_Propriedade(Objects[I])
end;

{ T_PropriedadePorAliasList }

class function T_PropriedadePorAliasList.GetKeyCode : pointer; begin
  Result := @T_Propriedade.PorAlias
end;

class function T_PropriedadePorAliasList.GetListType : TListType; begin
  Result := ltString
end;

class function T_PropriedadePorAliasList.GetObjectClass : TTransientClass; begin
  Result := T_Propriedade
end;

procedure T_PropriedadePorAliasList.Add(_Propriedade : T_Propriedade); begin
  inherited Add(TPrevalent(_Propriedade))
end;

procedure T_PropriedadePorAliasList.Delete(_Propriedade : T_Propriedade); begin
  inherited Delete(TPrevalent(_Propriedade))
end;

function T_PropriedadePorAliasList.Find(S : String) : T_Propriedade; begin
  Result := T_Propriedade(inherited Find(S))
end;

function T_PropriedadePorAliasList.First : T_Propriedade; begin
  Result := T_Propriedade(inherited First)
end;

function T_PropriedadePorAliasList.Last : T_Propriedade; begin
  Result := T_Propriedade(inherited Last)
end;

function T_PropriedadePorAliasList.Near(S : String) : T_Propriedade; begin
  Result := T_Propriedade(inherited Near(S))
end;

function T_PropriedadePorAliasList.Next(var _Propriedade : T_Propriedade) : boolean; begin
  Result := inherited Next(TTransient(_Propriedade))
end;

function T_PropriedadePorAliasList.Prior(var _Propriedade : T_Propriedade) : boolean; begin
  Result := inherited Prior(TTransient(_Propriedade))
end;

function T_PropriedadePorAliasList.Get_Propriedade(I : Integer) : T_Propriedade; begin
  Result := T_Propriedade(Objects[I])
end;

{ T_ClassePropriedadesAssociation }

class function T_ClassePropriedadesAssociation.GetKeyCode : pointer; begin
  Result := @T_Propriedade.PorNome
end;

class function T_ClassePropriedadesAssociation.GetListType : TListType; begin
  Result := ltString
end;

class function T_ClassePropriedadesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Propriedade
end;

procedure T_ClassePropriedadesAssociation.Add(_Propriedade : T_Propriedade); begin
  inherited Add(TPrevalent(_Propriedade))
end;

procedure T_ClassePropriedadesAssociation.Delete(_Propriedade : T_Propriedade); begin
  inherited Delete(TPrevalent(_Propriedade))
end;

function T_ClassePropriedadesAssociation.Find(S : String) : T_Propriedade; begin
  Result := T_Propriedade(inherited Find(S))
end;

function T_ClassePropriedadesAssociation.First : T_Propriedade; begin
  SetDependencyLists;
  Result := T_Propriedade(inherited First)
end;

function T_ClassePropriedadesAssociation.Last : T_Propriedade; begin
  SetDependencyLists;
  Result := T_Propriedade(inherited Last)
end;

function T_ClassePropriedadesAssociation.Near(S : String) : T_Propriedade; begin
  SetDependencyLists;
  Result := T_Propriedade(inherited Near(S))
end;

function T_ClassePropriedadesAssociation.Next(var _Propriedade : T_Propriedade) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Propriedade))
end;

function T_ClassePropriedadesAssociation.Prior(var _Propriedade : T_Propriedade) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Propriedade))
end;

function T_ClassePropriedadesAssociation.Get_Propriedade(I : Integer) : T_Propriedade; begin
  Result := T_Propriedade(Objects[I])
end;

{ T_Run }

procedure T_Run.New; begin
  inherited;
  _Log := T_RunLogAssociation.Create(Self, 'Log');
  if Prevalence.IsInRecover then exit;
  try _LogKind := _rlkNone except end;
end;

procedure T_Run.InternalFree; begin
  if _Log <> nil then _Log.InternalFree;
  inherited;
end;

function T_Run.GetIdentification : string; begin
  try Result := Method.GetIdentification except Result := ' ' end;
end;

function T_Run.GetRunObjectID : Integer; begin Result := T_Run(Prevalence.GetNewImage(Self, 2))._RunObjectID end;

procedure T_Run.SetRunObjectID(Value : Integer); begin
  if Prevalence.IsInRecoverSnapShot then _RunObjectID := Value else begin
    if (_RunObjectID = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Run(NewImage)._RunObjectID <> Value then begin
        T_Run(NewImage)._RunObjectID := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_Run.GetParameters : String; begin Result := T_Run(Prevalence.GetNewImage(Self, 3))._Parameters end;

procedure T_Run.SetParameters(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Parameters := Value else begin
    if (_Parameters = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Run(NewImage)._Parameters <> Value then begin
        T_Run(NewImage)._Parameters := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_Run.GetLogKind : T_RunLogKind; begin Result := T_Run(Prevalence.GetNewImage(Self, 4))._LogKind end;

procedure T_Run.SetLogKind(Value : T_RunLogKind); begin
  if Prevalence.IsInRecoverSnapShot then _LogKind := Value else begin
    if (_LogKind = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Run(NewImage)._LogKind <> Value then begin
        T_Run(NewImage)._LogKind := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

function T_Run.GetRunning : Boolean; begin
  try Result := Assigned(RunningThread) except Result := Boolean(0) end;
end;

function T_Run.GetMethod : T_Metodo; begin Result := T_Run(Prevalence.GetNewImage(Self, 6))._Method end;

procedure T_Run.SetMethod(Value : T_Metodo); begin
  if Prevalence.IsInRecover then _Method := Value else begin
    if (_Method = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Run(NewImage)._Method <> Value then begin
        T_Run(NewImage)._Method := Value;
        UpdateLog(6, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Run.RunObject : TPrevalent; begin
  Result := TPrevalent(Prevalence.PrevalentLists('T' + Method.Classe.Nome + 'List').Find(RunObjectID))
end;

class function T_RunList.GetObjectClass : TTransientClass; begin
  Result := T_Run
end;

function T_Sequence.GetName : String; begin Result := T_Sequence(Prevalence.GetNewImage(Self, 2))._Name end;

procedure T_Sequence.SetName(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Name := Value else begin
    if (_Name = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Sequence(NewImage)._Name <> Value then begin
        T_Sequence(NewImage)._Name := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_Sequence.GetInternalValue : Integer; begin Result := T_Sequence(Prevalence.GetNewImage(Self, 3))._InternalValue end;

procedure T_Sequence.SetInternalValue(Value : Integer); begin
  if Prevalence.IsInRecoverSnapShot then _InternalValue := Value else begin
    if (_InternalValue = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Sequence(NewImage)._InternalValue <> Value then begin
        T_Sequence(NewImage)._InternalValue := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Sequence.ByName : String; begin
  Result := Name
end;

// Cyclomatic Complexity: 2, Baixo
class function T_Sequence.Next(pName : String; pIncrement : Integer = 1) : integer;
  var
   I : integer;
  S : T_Sequence;
begin
  if _SequenceByNameList.FindNear(pName, I, false) then begin
   S := T_Sequence(_SequenceList.InternalObjects[I]);
   S._InternalValue := S._InternalValue + pIncrement;
   TUnpPrevalence(Prevalence).Log(_opUpdate, S, true);
  end
  else begin
    S := T_Sequence.Create;
    S._Name := pName;
    S._InternalValue := pIncrement;
    S.Add;
    TUnpPrevalence(Prevalence).Log(_opAdd, S, true);
  end;
   Result := S._InternalValue
end;

// Cyclomatic Complexity: 2, Baixo
class function T_Sequence.GetValue(pName : String) : integer;
  var
  I  : integer;
  S : T_Sequence;
begin
  if _SequenceByNameList.FindNear(pName, I, false) then begin
    S := T_Sequence(_SequenceList.InternalObjects[I]);
    Result := S._InternalValue;
  end
  else Result := 0;
end;

// Cyclomatic Complexity: 2, Baixo
class procedure T_Sequence.SetValue(pName : String; pValue : Integer);
  var
  I  : integer;
  S : T_Sequence;
begin
  if _SequenceByNameList.FindNear(pName, I, false) then begin
    S := T_Sequence(_SequenceList.InternalObjects[I]);
    S._InternalValue := pValue;
    TUnpPrevalence(Prevalence).Log(_opUpdate, S, true);
  end
  else begin
    S := T_Sequence.Create;
    S._Name := pName;
    S._InternalValue := pValue;
    S.Add;
    TUnpPrevalence(Prevalence).Log(_opAdd, S, true);
  end;
end;

{ T_SequenceList }

class function T_SequenceList.GetObjectClass : TTransientClass; begin
  Result := T_Sequence
end;

procedure T_SequenceList.Add(_Sequence : T_Sequence); begin
  inherited Add(TPrevalent(_Sequence))
end;

procedure T_SequenceList.Delete(_Sequence : T_Sequence); begin
  inherited Delete(TPrevalent(_Sequence))
end;

function T_SequenceList.Find(I : Integer) : T_Sequence; begin
  Result := T_Sequence(inherited Find(I))
end;

function T_SequenceList.First : T_Sequence; begin
  Result := T_Sequence(inherited First)
end;

function T_SequenceList.Last : T_Sequence; begin
  Result := T_Sequence(inherited Last)
end;

function T_SequenceList.Near(I : Integer) : T_Sequence; begin
  Result := T_Sequence(inherited Near(I))
end;

function T_SequenceList.Next(var _Sequence : T_Sequence) : boolean; begin
  Result := inherited Next(TTransient(_Sequence))
end;

function T_SequenceList.Prior(var _Sequence : T_Sequence) : boolean; begin
  Result := inherited Prior(TTransient(_Sequence))
end;

function T_SequenceList.Get_Sequence(I : Integer) : T_Sequence; begin
  Result := T_Sequence(Objects[I])
end;

{ T_SequenceByNameList }

class function T_SequenceByNameList.GetKeyCode : pointer; begin
  Result := @T_Sequence.ByName
end;

class function T_SequenceByNameList.GetListType : TListType; begin
  Result := ltString
end;

class function T_SequenceByNameList.GetObjectClass : TTransientClass; begin
  Result := T_Sequence
end;

procedure T_SequenceByNameList.Add(_Sequence : T_Sequence); begin
  inherited Add(TPrevalent(_Sequence))
end;

procedure T_SequenceByNameList.Delete(_Sequence : T_Sequence); begin
  inherited Delete(TPrevalent(_Sequence))
end;

function T_SequenceByNameList.Find(S : String) : T_Sequence; begin
  Result := T_Sequence(inherited Find(S))
end;

function T_SequenceByNameList.First : T_Sequence; begin
  Result := T_Sequence(inherited First)
end;

function T_SequenceByNameList.Last : T_Sequence; begin
  Result := T_Sequence(inherited Last)
end;

function T_SequenceByNameList.Near(S : String) : T_Sequence; begin
  Result := T_Sequence(inherited Near(S))
end;

function T_SequenceByNameList.Next(var _Sequence : T_Sequence) : boolean; begin
  Result := inherited Next(TTransient(_Sequence))
end;

function T_SequenceByNameList.Prior(var _Sequence : T_Sequence) : boolean; begin
  Result := inherited Prior(TTransient(_Sequence))
end;

function T_SequenceByNameList.Get_Sequence(I : Integer) : T_Sequence; begin
  Result := T_Sequence(Objects[I])
end;

function T_Servico.GetNome : String; begin Result := T_Servico(Prevalence.GetNewImage(Self, 2))._Nome end;

procedure T_Servico.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Servico(NewImage)._Nome <> Value then begin
        T_Servico(NewImage)._Nome := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_Servico.GetSessao : T_Sessao; begin Result := T_Servico(Prevalence.GetNewImage(Self, 3))._Sessao end;

procedure T_Servico.SetSessao(Value : T_Sessao); begin
  if Prevalence.IsInRecover then _Sessao := Value else begin
    if (_Sessao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Servico(NewImage)._Sessao <> Value then begin
        T_Servico(NewImage)._Sessao := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Servico.PorNome : String; begin
  Result := Nome
end;

{ T_ServicoList }

class function T_ServicoList.GetObjectClass : TTransientClass; begin
  Result := T_Servico
end;

procedure T_ServicoList.Add(_Servico : T_Servico); begin
  inherited Add(TPrevalent(_Servico))
end;

procedure T_ServicoList.Delete(_Servico : T_Servico); begin
  inherited Delete(TPrevalent(_Servico))
end;

function T_ServicoList.Find(I : Integer) : T_Servico; begin
  Result := T_Servico(inherited Find(I))
end;

function T_ServicoList.First : T_Servico; begin
  Result := T_Servico(inherited First)
end;

function T_ServicoList.Last : T_Servico; begin
  Result := T_Servico(inherited Last)
end;

function T_ServicoList.Near(I : Integer) : T_Servico; begin
  Result := T_Servico(inherited Near(I))
end;

function T_ServicoList.Next(var _Servico : T_Servico) : boolean; begin
  Result := inherited Next(TTransient(_Servico))
end;

function T_ServicoList.Prior(var _Servico : T_Servico) : boolean; begin
  Result := inherited Prior(TTransient(_Servico))
end;

function T_ServicoList.Get_Servico(I : Integer) : T_Servico; begin
  Result := T_Servico(Objects[I])
end;

{ T_ServicoPorNomeList }

class function T_ServicoPorNomeList.GetKeyCode : pointer; begin
  Result := @T_Servico.PorNome
end;

class function T_ServicoPorNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_ServicoPorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_Servico
end;

procedure T_ServicoPorNomeList.Add(_Servico : T_Servico); begin
  inherited Add(TPrevalent(_Servico))
end;

procedure T_ServicoPorNomeList.Delete(_Servico : T_Servico); begin
  inherited Delete(TPrevalent(_Servico))
end;

function T_ServicoPorNomeList.Find(S : String) : T_Servico; begin
  Result := T_Servico(inherited Find(S))
end;

function T_ServicoPorNomeList.First : T_Servico; begin
  Result := T_Servico(inherited First)
end;

function T_ServicoPorNomeList.Last : T_Servico; begin
  Result := T_Servico(inherited Last)
end;

function T_ServicoPorNomeList.Near(S : String) : T_Servico; begin
  Result := T_Servico(inherited Near(S))
end;

function T_ServicoPorNomeList.Next(var _Servico : T_Servico) : boolean; begin
  Result := inherited Next(TTransient(_Servico))
end;

function T_ServicoPorNomeList.Prior(var _Servico : T_Servico) : boolean; begin
  Result := inherited Prior(TTransient(_Servico))
end;

function T_ServicoPorNomeList.Get_Servico(I : Integer) : T_Servico; begin
  Result := T_Servico(Objects[I])
end;

{ T_SessaoServicosAssociation }

class function T_SessaoServicosAssociation.GetKeyCode : pointer; begin
  Result := @T_Servico.PorNome
end;

class function T_SessaoServicosAssociation.GetListType : TListType; begin
  Result := ltString
end;

class function T_SessaoServicosAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Servico
end;

procedure T_SessaoServicosAssociation.Add(_Servico : T_Servico); begin
  inherited Add(TPrevalent(_Servico))
end;

procedure T_SessaoServicosAssociation.Delete(_Servico : T_Servico); begin
  inherited Delete(TPrevalent(_Servico))
end;

function T_SessaoServicosAssociation.Find(S : String) : T_Servico; begin
  Result := T_Servico(inherited Find(S))
end;

function T_SessaoServicosAssociation.First : T_Servico; begin
  SetDependencyLists;
  Result := T_Servico(inherited First)
end;

function T_SessaoServicosAssociation.Last : T_Servico; begin
  SetDependencyLists;
  Result := T_Servico(inherited Last)
end;

function T_SessaoServicosAssociation.Near(S : String) : T_Servico; begin
  SetDependencyLists;
  Result := T_Servico(inherited Near(S))
end;

function T_SessaoServicosAssociation.Next(var _Servico : T_Servico) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Servico))
end;

function T_SessaoServicosAssociation.Prior(var _Servico : T_Servico) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Servico))
end;

function T_SessaoServicosAssociation.Get_Servico(I : Integer) : T_Servico; begin
  Result := T_Servico(Objects[I])
end;

{ T_Sessao }

procedure T_Sessao.New; begin
  inherited;
  _Criterios := T_SessaoCriteriosAssociation.Create(Self, 'Criterios', true);
  _Servicos := T_SessaoServicosAssociation.Create(Self, 'Servicos', true);
end;

procedure T_Sessao.InternalFree; begin
  if _Criterios <> nil then _Criterios.InternalFree;
  if _Servicos <> nil then _Servicos.InternalFree;
  inherited;
end;

function T_Sessao.GetIdentification : string; begin
  try Result := Usuario except Result := ' ' end;
end;

function T_Sessao.GetUsuario : String; begin
  try Result := PThread.UserInfo.UserName except Result := ' ' end;
end;

function T_Sessao.GetLogonDateTime : DateTime; begin
  try Result := PThread.ConnectionDateTime except Result := 0 end;
end;

function T_Sessao.GetEstacao : String; begin
  try Result := PThread.UserInfo.ComputerName except Result := ' ' end;
end;

function T_Sessao.GetMetodoAtivo : String; begin
  try Result := PThread.UserInfo.RunningMethodName except Result := ' ' end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Sessao.PorThread : Int64; begin
  Result := PtrInt(PThread)
end;

// Cyclomatic Complexity: 1, Baixo
function T_Sessao.PorUsuario : String; begin
  Result := Usuario
end;

// Cyclomatic Complexity: 1, Baixo
procedure T_Sessao.TerminaSessao; begin
  KillSession(PThread)
end;

{ T_SessaoList }

class function T_SessaoList.GetObjectClass : TTransientClass; begin
  Result := T_Sessao
end;

procedure T_SessaoList.Add(_Sessao : T_Sessao); begin
  inherited Add(TPrevalent(_Sessao))
end;

procedure T_SessaoList.Delete(_Sessao : T_Sessao); begin
  inherited Delete(TPrevalent(_Sessao))
end;

function T_SessaoList.Find(I : Integer) : T_Sessao; begin
  Result := T_Sessao(inherited Find(I))
end;

function T_SessaoList.First : T_Sessao; begin
  Result := T_Sessao(inherited First)
end;

function T_SessaoList.Last : T_Sessao; begin
  Result := T_Sessao(inherited Last)
end;

function T_SessaoList.Near(I : Integer) : T_Sessao; begin
  Result := T_Sessao(inherited Near(I))
end;

function T_SessaoList.Next(var _Sessao : T_Sessao) : boolean; begin
  Result := inherited Next(TTransient(_Sessao))
end;

function T_SessaoList.Prior(var _Sessao : T_Sessao) : boolean; begin
  Result := inherited Prior(TTransient(_Sessao))
end;

function T_SessaoList.Get_Sessao(I : Integer) : T_Sessao; begin
  Result := T_Sessao(Objects[I])
end;

{ T_SessaoPorThreadList }

class function T_SessaoPorThreadList.GetKeyCode : pointer; begin
  Result := @T_Sessao.PorThread
end;

class function T_SessaoPorThreadList.GetListType : TListType; begin
  Result := ltInt64
end;

class function T_SessaoPorThreadList.GetObjectClass : TTransientClass; begin
  Result := T_Sessao
end;

procedure T_SessaoPorThreadList.Add(_Sessao : T_Sessao); begin
  inherited Add(TPrevalent(_Sessao))
end;

procedure T_SessaoPorThreadList.Delete(_Sessao : T_Sessao); begin
  inherited Delete(TPrevalent(_Sessao))
end;

function T_SessaoPorThreadList.Find(I : Int64) : T_Sessao; begin
  Result := T_Sessao(inherited Find(I))
end;

function T_SessaoPorThreadList.First : T_Sessao; begin
  Result := T_Sessao(inherited First)
end;

function T_SessaoPorThreadList.Last : T_Sessao; begin
  Result := T_Sessao(inherited Last)
end;

function T_SessaoPorThreadList.Near(I : Int64) : T_Sessao; begin
  Result := T_Sessao(inherited Near(I))
end;

function T_SessaoPorThreadList.Next(var _Sessao : T_Sessao) : boolean; begin
  Result := inherited Next(TTransient(_Sessao))
end;

function T_SessaoPorThreadList.Prior(var _Sessao : T_Sessao) : boolean; begin
  Result := inherited Prior(TTransient(_Sessao))
end;

function T_SessaoPorThreadList.Get_Sessao(I : Integer) : T_Sessao; begin
  Result := T_Sessao(Objects[I])
end;

{ T_SessaoPorUsuarioList }

class function T_SessaoPorUsuarioList.GetKeyCode : pointer; begin
  Result := @T_Sessao.PorUsuario
end;

class function T_SessaoPorUsuarioList.GetListType : TListType; begin
  Result := ltString
end;

class function T_SessaoPorUsuarioList.GetObjectClass : TTransientClass; begin
  Result := T_Sessao
end;

procedure T_SessaoPorUsuarioList.Add(_Sessao : T_Sessao); begin
  inherited Add(TPrevalent(_Sessao))
end;

procedure T_SessaoPorUsuarioList.Delete(_Sessao : T_Sessao); begin
  inherited Delete(TPrevalent(_Sessao))
end;

function T_SessaoPorUsuarioList.Find(S : String) : T_Sessao; begin
  Result := T_Sessao(inherited Find(S))
end;

function T_SessaoPorUsuarioList.First : T_Sessao; begin
  Result := T_Sessao(inherited First)
end;

function T_SessaoPorUsuarioList.Last : T_Sessao; begin
  Result := T_Sessao(inherited Last)
end;

function T_SessaoPorUsuarioList.Near(S : String) : T_Sessao; begin
  Result := T_Sessao(inherited Near(S))
end;

function T_SessaoPorUsuarioList.Next(var _Sessao : T_Sessao) : boolean; begin
  Result := inherited Next(TTransient(_Sessao))
end;

function T_SessaoPorUsuarioList.Prior(var _Sessao : T_Sessao) : boolean; begin
  Result := inherited Prior(TTransient(_Sessao))
end;

function T_SessaoPorUsuarioList.Get_Sessao(I : Integer) : T_Sessao; begin
  Result := T_Sessao(Objects[I])
end;

{ T_UsuarioBloqueado }

procedure T_UsuarioBloqueado.New; begin
  inherited;
  if Prevalence.IsInRecover then exit;
  try _Data := Now except end;
end;

function T_UsuarioBloqueado.GetIdentificador : String; begin
  try Result := Dominio.Nome+'/'+Nome except Result := ' ' end;
end;

function T_UsuarioBloqueado.GetNome : String; begin Result := T_UsuarioBloqueado(Prevalence.GetNewImage(Self, 3))._Nome end;

procedure T_UsuarioBloqueado.SetNome(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Nome := Value else begin
    if (_Nome = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_UsuarioBloqueado(NewImage)._Nome <> Value then begin
        T_UsuarioBloqueado(NewImage)._Nome := Value;
        UpdateLog(3, NewImage, Stream)
      end;
    end;
end;

function T_UsuarioBloqueado.GetData : DateTime; begin Result := T_UsuarioBloqueado(Prevalence.GetNewImage(Self, 4))._Data end;

procedure T_UsuarioBloqueado.SetData(Value : DateTime); begin
  if Prevalence.IsInRecoverSnapShot then _Data := Value else begin
    if (_Data = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_UsuarioBloqueado(NewImage)._Data <> Value then begin
        T_UsuarioBloqueado(NewImage)._Data := Value;
        UpdateLog(4, NewImage, Stream)
      end;
    end;
end;

function T_UsuarioBloqueado.GetMotivo : String; begin Result := T_UsuarioBloqueado(Prevalence.GetNewImage(Self, 5))._Motivo end;

procedure T_UsuarioBloqueado.SetMotivo(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Motivo := Value else begin
    if (_Motivo = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_UsuarioBloqueado(NewImage)._Motivo <> Value then begin
        T_UsuarioBloqueado(NewImage)._Motivo := Value;
        UpdateLog(5, NewImage, Stream)
      end;
    end;
end;

function T_UsuarioBloqueado.GetAdministrador : String; begin
  try Result := GetUserInfo(tuUserName) except Result := ' ' end;
end;

function T_UsuarioBloqueado.GetDominio : T_Dominio; begin Result := T_UsuarioBloqueado(Prevalence.GetNewImage(Self, 7))._Dominio end;

procedure T_UsuarioBloqueado.SetDominio(Value : T_Dominio); begin
  if Prevalence.IsInRecover then _Dominio := Value else begin
    if (_Dominio = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_UsuarioBloqueado(NewImage)._Dominio <> Value then begin
        T_UsuarioBloqueado(NewImage)._Dominio := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_UsuarioBloqueado.PorIdentificador : String; begin
  Result := Identificador
end;

{ T_UsuarioBloqueadoList }

class function T_UsuarioBloqueadoList.GetObjectClass : TTransientClass; begin
  Result := T_UsuarioBloqueado
end;

procedure T_UsuarioBloqueadoList.Add(_UsuarioBloqueado : T_UsuarioBloqueado); begin
  inherited Add(TPrevalent(_UsuarioBloqueado))
end;

procedure T_UsuarioBloqueadoList.Delete(_UsuarioBloqueado : T_UsuarioBloqueado); begin
  inherited Delete(TPrevalent(_UsuarioBloqueado))
end;

function T_UsuarioBloqueadoList.Find(I : Integer) : T_UsuarioBloqueado; begin
  Result := T_UsuarioBloqueado(inherited Find(I))
end;

function T_UsuarioBloqueadoList.First : T_UsuarioBloqueado; begin
  Result := T_UsuarioBloqueado(inherited First)
end;

function T_UsuarioBloqueadoList.Last : T_UsuarioBloqueado; begin
  Result := T_UsuarioBloqueado(inherited Last)
end;

function T_UsuarioBloqueadoList.Near(I : Integer) : T_UsuarioBloqueado; begin
  Result := T_UsuarioBloqueado(inherited Near(I))
end;

function T_UsuarioBloqueadoList.Next(var _UsuarioBloqueado : T_UsuarioBloqueado) : boolean; begin
  Result := inherited Next(TTransient(_UsuarioBloqueado))
end;

function T_UsuarioBloqueadoList.Prior(var _UsuarioBloqueado : T_UsuarioBloqueado) : boolean; begin
  Result := inherited Prior(TTransient(_UsuarioBloqueado))
end;

function T_UsuarioBloqueadoList.Get_UsuarioBloqueado(I : Integer) : T_UsuarioBloqueado; begin
  Result := T_UsuarioBloqueado(Objects[I])
end;

{ T_UsuarioBloqueadoPorIdentificadorList }

class function T_UsuarioBloqueadoPorIdentificadorList.GetKeyCode : pointer; begin
  Result := @T_UsuarioBloqueado.PorIdentificador
end;

class function T_UsuarioBloqueadoPorIdentificadorList.GetListType : TListType; begin
  Result := ltString
end;

class function T_UsuarioBloqueadoPorIdentificadorList.GetObjectClass : TTransientClass; begin
  Result := T_UsuarioBloqueado
end;

procedure T_UsuarioBloqueadoPorIdentificadorList.Add(_UsuarioBloqueado : T_UsuarioBloqueado); begin
  inherited Add(TPrevalent(_UsuarioBloqueado))
end;

procedure T_UsuarioBloqueadoPorIdentificadorList.Delete(_UsuarioBloqueado : T_UsuarioBloqueado); begin
  inherited Delete(TPrevalent(_UsuarioBloqueado))
end;

function T_UsuarioBloqueadoPorIdentificadorList.Find(S : String) : T_UsuarioBloqueado; begin
  Result := T_UsuarioBloqueado(inherited Find(S))
end;

function T_UsuarioBloqueadoPorIdentificadorList.First : T_UsuarioBloqueado; begin
  Result := T_UsuarioBloqueado(inherited First)
end;

function T_UsuarioBloqueadoPorIdentificadorList.Last : T_UsuarioBloqueado; begin
  Result := T_UsuarioBloqueado(inherited Last)
end;

function T_UsuarioBloqueadoPorIdentificadorList.Near(S : String) : T_UsuarioBloqueado; begin
  Result := T_UsuarioBloqueado(inherited Near(S))
end;

function T_UsuarioBloqueadoPorIdentificadorList.Next(var _UsuarioBloqueado : T_UsuarioBloqueado) : boolean; begin
  Result := inherited Next(TTransient(_UsuarioBloqueado))
end;

function T_UsuarioBloqueadoPorIdentificadorList.Prior(var _UsuarioBloqueado : T_UsuarioBloqueado) : boolean; begin
  Result := inherited Prior(TTransient(_UsuarioBloqueado))
end;

function T_UsuarioBloqueadoPorIdentificadorList.Get_UsuarioBloqueado(I : Integer) : T_UsuarioBloqueado; begin
  Result := T_UsuarioBloqueado(Objects[I])
end;

function T_ExpPropriedade.GetExporta : T_Exporta; begin Result := T_ExpPropriedade(Prevalence.GetNewImage(Self, 7))._Exporta end;

procedure T_ExpPropriedade.SetExporta(Value : T_Exporta); begin
  if Prevalence.IsInRecover then _Exporta := Value else begin
    if (_Exporta = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_ExpPropriedade(NewImage)._Exporta <> Value then begin
        T_ExpPropriedade(NewImage)._Exporta := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

{ T_ExpPropriedadeList }

class function T_ExpPropriedadeList.GetObjectClass : TTransientClass; begin
  Result := T_ExpPropriedade
end;

procedure T_ExpPropriedadeList.Add(_ExpPropriedade : T_ExpPropriedade); begin
  inherited Add(TPrevalent(_ExpPropriedade))
end;

procedure T_ExpPropriedadeList.Delete(_ExpPropriedade : T_ExpPropriedade); begin
  inherited Delete(TPrevalent(_ExpPropriedade))
end;

function T_ExpPropriedadeList.Find(I : Integer) : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(inherited Find(I))
end;

function T_ExpPropriedadeList.First : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(inherited First)
end;

function T_ExpPropriedadeList.Last : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(inherited Last)
end;

function T_ExpPropriedadeList.Near(I : Integer) : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(inherited Near(I))
end;

function T_ExpPropriedadeList.Next(var _ExpPropriedade : T_ExpPropriedade) : boolean; begin
  Result := inherited Next(TTransient(_ExpPropriedade))
end;

function T_ExpPropriedadeList.Prior(var _ExpPropriedade : T_ExpPropriedade) : boolean; begin
  Result := inherited Prior(TTransient(_ExpPropriedade))
end;

function T_ExpPropriedadeList.Get_ExpPropriedade(I : Integer) : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(Objects[I])
end;

{ T_ExpPropriedadePorElementoList }

class function T_ExpPropriedadePorElementoList.GetKeyCode : pointer; begin
  Result := @T_ExpPropriedade.PorElemento
end;

class function T_ExpPropriedadePorElementoList.GetObjectClass : TTransientClass; begin
  Result := T_ExpPropriedade
end;

procedure T_ExpPropriedadePorElementoList.Add(_ExpPropriedade : T_ExpPropriedade); begin
  inherited Add(TPrevalent(_ExpPropriedade))
end;

procedure T_ExpPropriedadePorElementoList.Delete(_ExpPropriedade : T_ExpPropriedade); begin
  inherited Delete(TPrevalent(_ExpPropriedade))
end;

function T_ExpPropriedadePorElementoList.Find(I : Integer) : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(inherited Find(I))
end;

function T_ExpPropriedadePorElementoList.First : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(inherited First)
end;

function T_ExpPropriedadePorElementoList.Last : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(inherited Last)
end;

function T_ExpPropriedadePorElementoList.Near(I : Integer) : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(inherited Near(I))
end;

function T_ExpPropriedadePorElementoList.Next(var _ExpPropriedade : T_ExpPropriedade) : boolean; begin
  Result := inherited Next(TTransient(_ExpPropriedade))
end;

function T_ExpPropriedadePorElementoList.Prior(var _ExpPropriedade : T_ExpPropriedade) : boolean; begin
  Result := inherited Prior(TTransient(_ExpPropriedade))
end;

function T_ExpPropriedadePorElementoList.Get_ExpPropriedade(I : Integer) : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(Objects[I])
end;

{ T_ExpPropriedadePorPropNomeList }

class function T_ExpPropriedadePorPropNomeList.GetKeyCode : pointer; begin
  Result := @T_ExpPropriedade.PorPropNome
end;

class function T_ExpPropriedadePorPropNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_ExpPropriedadePorPropNomeList.GetObjectClass : TTransientClass; begin
  Result := T_ExpPropriedade
end;

procedure T_ExpPropriedadePorPropNomeList.Add(_ExpPropriedade : T_ExpPropriedade); begin
  inherited Add(TPrevalent(_ExpPropriedade))
end;

procedure T_ExpPropriedadePorPropNomeList.Delete(_ExpPropriedade : T_ExpPropriedade); begin
  inherited Delete(TPrevalent(_ExpPropriedade))
end;

function T_ExpPropriedadePorPropNomeList.Find(S : String) : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(inherited Find(S))
end;

function T_ExpPropriedadePorPropNomeList.First : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(inherited First)
end;

function T_ExpPropriedadePorPropNomeList.Last : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(inherited Last)
end;

function T_ExpPropriedadePorPropNomeList.Near(S : String) : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(inherited Near(S))
end;

function T_ExpPropriedadePorPropNomeList.Next(var _ExpPropriedade : T_ExpPropriedade) : boolean; begin
  Result := inherited Next(TTransient(_ExpPropriedade))
end;

function T_ExpPropriedadePorPropNomeList.Prior(var _ExpPropriedade : T_ExpPropriedade) : boolean; begin
  Result := inherited Prior(TTransient(_ExpPropriedade))
end;

function T_ExpPropriedadePorPropNomeList.Get_ExpPropriedade(I : Integer) : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(Objects[I])
end;

{ T_ExportaPropriedadesAssociation }

class function T_ExportaPropriedadesAssociation.MinConstraint : integer; begin
  Result := 1
end;

class function T_ExportaPropriedadesAssociation.GetKeyCode : pointer; begin
  Result := @T_ExpPropriedade.PorElemento
end;

class function T_ExportaPropriedadesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_ExpPropriedade
end;

procedure T_ExportaPropriedadesAssociation.Add(_ExpPropriedade : T_ExpPropriedade); begin
  inherited Add(TPrevalent(_ExpPropriedade))
end;

procedure T_ExportaPropriedadesAssociation.Delete(_ExpPropriedade : T_ExpPropriedade); begin
  inherited Delete(TPrevalent(_ExpPropriedade))
end;

function T_ExportaPropriedadesAssociation.Find(I : Integer) : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(inherited Find(I))
end;

function T_ExportaPropriedadesAssociation.First : T_ExpPropriedade; begin
  SetDependencyLists;
  Result := T_ExpPropriedade(inherited First)
end;

function T_ExportaPropriedadesAssociation.Last : T_ExpPropriedade; begin
  SetDependencyLists;
  Result := T_ExpPropriedade(inherited Last)
end;

function T_ExportaPropriedadesAssociation.Near(I : Integer) : T_ExpPropriedade; begin
  SetDependencyLists;
  Result := T_ExpPropriedade(inherited Near(I))
end;

function T_ExportaPropriedadesAssociation.Next(var _ExpPropriedade : T_ExpPropriedade) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_ExpPropriedade))
end;

function T_ExportaPropriedadesAssociation.Prior(var _ExpPropriedade : T_ExpPropriedade) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_ExpPropriedade))
end;

function T_ExportaPropriedadesAssociation.Get_ExpPropriedade(I : Integer) : T_ExpPropriedade; begin
  Result := T_ExpPropriedade(Objects[I])
end;

{ T_Exporta }

procedure T_Exporta.New; begin
  inherited;
  _Propriedades := T_ExportaPropriedadesAssociation.Create(Self, 'Propriedades');
end;

procedure T_Exporta.InternalFree; begin
  if _Propriedades <> nil then _Propriedades.InternalFree;
  inherited;
end;

// Cyclomatic Complexity: 2, Baixo
class procedure T_Exporta.GerarModelo(Modelo : String);
type
  TContext = record
    This : T_Exporta;
    Modelo : String;
    Obj : TObject;
    fClasse : T_Classe;
  end;
var
  Context : TContext;
  StateMachine : TStateMachine;

procedure CriaModelo(var Context : TContext); begin
  with Context, This do begin
    fClasse := T_Classe(Browser.Choose(_ClassePorNomeList, [], '', 'Escolha a Classe a ser Exportada'));
    Obj := CreateExport(Modelo, fClasse);
    ;
  end;
end;

procedure CustomizarModeloDeExportacao(var Context : TContext); begin
  with Context, This do begin
    Browser.Edit(T_Exporta(Obj), '', 'Customizar Modelo de Exportação', '', StateMachine.FixButtons([bbCancel, bbFinish]));
  end;
end;

procedure ErroCriaModelo(var Context : TContext); begin
  with Context, This do Raise Exception.Create('Erro na criação do Modelo');
end;

function CriaObjOK(var Context : TContext) : boolean; begin
  with Context, This do Result := Obj <> nil;
end;

begin
  fillchar(Context, sizeof(Context), 0);
  Context.Modelo := Modelo;
  StateMachine := TStateMachine.Create('_Administration', '_Exporta', 'GerarModelo', [1, 2, 1, 1]);
  with StateMachine do begin
    SetState(0, 'Iniciais', nil);
    SetState(1, 'CriaModelo', @CriaModelo);
    SetState(2, 'CustomizarModeloDeExportacao', @CustomizarModeloDeExportacao, [bbCancel, bbFinish]);
    SetState(3, 'ErroCriaModelo', @ErroCriaModelo);
    SetTransition(0, 0, 'Default', nil, 1);
    SetTransition(1, 0, 'CriaObjOK', @CriaObjOK, 2);
    SetTransition(1, 1, 'Default', nil, 3);
    SetTransition(2, 0, 'Default', nil, -1);
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

// Cyclomatic Complexity: 1, Baixo
class procedure T_Exporta.Exportar(Modelo : T_Exporta); begin
  Export(Modelo)
end;

// Cyclomatic Complexity: 2, Baixo
class procedure T_Exporta.CustomizarModelo;
type
  TContext = record
    This : T_Exporta;
    fExporta : T_Exporta;
  end;
var
  Context : TContext;
  StateMachine : TStateMachine;

procedure SelecionaModelo(var Context : TContext); begin
  with Context, This do begin
    fExporta := T_Exporta(Browser.choose(_ExportaPorNomeList, [], '', 'Escolha o Modelo a ser Customizado'));
    ;
  end;
end;

procedure CustomizarModeloGerado(var Context : TContext); begin
  with Context, This do begin
    Browser.Edit(fExporta, '', 'Customizar Modelo Gerado', '', StateMachine.FixButtons([bbCancel, bbFinish]));
  end;
end;

function Selecionado(var Context : TContext) : boolean; begin
  with Context, This do Result := fExporta <> nil;
end;

begin
  fillchar(Context, sizeof(Context), 0);
  StateMachine := TStateMachine.Create('_Administration', '_Exporta', 'CustomizarModelo', [1, 2, 1]);
  with StateMachine do begin
    SetState(0, 'Iniciais', nil);
    SetState(1, 'SelecionaModelo', @SelecionaModelo);
    SetState(2, 'CustomizarModeloGerado', @CustomizarModeloGerado, [bbCancel, bbFinish]);
    SetTransition(0, 0, 'Default', nil, 1);
    SetTransition(1, 0, 'Selecionado', @Selecionado, 2);
    SetTransition(1, 1, 'Default', nil, -1);
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

{ T_ExportaList }

class function T_ExportaList.GetObjectClass : TTransientClass; begin
  Result := T_Exporta
end;

procedure T_ExportaList.Add(_Exporta : T_Exporta); begin
  inherited Add(TPrevalent(_Exporta))
end;

procedure T_ExportaList.Delete(_Exporta : T_Exporta); begin
  inherited Delete(TPrevalent(_Exporta))
end;

function T_ExportaList.Find(I : Integer) : T_Exporta; begin
  Result := T_Exporta(inherited Find(I))
end;

function T_ExportaList.First : T_Exporta; begin
  Result := T_Exporta(inherited First)
end;

function T_ExportaList.Last : T_Exporta; begin
  Result := T_Exporta(inherited Last)
end;

function T_ExportaList.Near(I : Integer) : T_Exporta; begin
  Result := T_Exporta(inherited Near(I))
end;

function T_ExportaList.Next(var _Exporta : T_Exporta) : boolean; begin
  Result := inherited Next(TTransient(_Exporta))
end;

function T_ExportaList.Prior(var _Exporta : T_Exporta) : boolean; begin
  Result := inherited Prior(TTransient(_Exporta))
end;

function T_ExportaList.Get_Exporta(I : Integer) : T_Exporta; begin
  Result := T_Exporta(Objects[I])
end;

{ T_ExportaPorNomeList }

class function T_ExportaPorNomeList.GetKeyCode : pointer; begin
  Result := @T_Exporta.PorNome
end;

class function T_ExportaPorNomeList.GetListType : TListType; begin
  Result := ltstring
end;

class function T_ExportaPorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_Exporta
end;

procedure T_ExportaPorNomeList.Add(_Exporta : T_Exporta); begin
  inherited Add(TPrevalent(_Exporta))
end;

procedure T_ExportaPorNomeList.Delete(_Exporta : T_Exporta); begin
  inherited Delete(TPrevalent(_Exporta))
end;

function T_ExportaPorNomeList.Find(s : string) : T_Exporta; begin
  Result := T_Exporta(inherited Find(s))
end;

function T_ExportaPorNomeList.First : T_Exporta; begin
  Result := T_Exporta(inherited First)
end;

function T_ExportaPorNomeList.Last : T_Exporta; begin
  Result := T_Exporta(inherited Last)
end;

function T_ExportaPorNomeList.Near(s : string) : T_Exporta; begin
  Result := T_Exporta(inherited Near(s))
end;

function T_ExportaPorNomeList.Next(var _Exporta : T_Exporta) : boolean; begin
  Result := inherited Next(TTransient(_Exporta))
end;

function T_ExportaPorNomeList.Prior(var _Exporta : T_Exporta) : boolean; begin
  Result := inherited Prior(TTransient(_Exporta))
end;

function T_ExportaPorNomeList.Get_Exporta(I : Integer) : T_Exporta; begin
  Result := T_Exporta(Objects[I])
end;

{ T_Importa }

procedure T_Importa.New; begin
  inherited;
  if Prevalence.IsInRecover then exit;
  try _SeExiste := _iseAtualizar except end;
  try _BalancedLine := False except end;
  try _NumeroElementoChave := 1 except end;
  try _SaltarFim := 0 except end;
  try _SaltarInicio := 0 except end;
end;

function T_Importa.GetLista : String; begin Result := T_Importa(Prevalence.GetNewImage(Self, 7))._Lista end;

procedure T_Importa.SetLista(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Lista := Value else begin
    if (_Lista = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Importa(NewImage)._Lista <> Value then begin
        T_Importa(NewImage)._Lista := Value;
        UpdateLog(7, NewImage, Stream)
      end;
    end;
end;

function T_Importa.GetSeExiste : T_ImportaSeExiste; begin Result := T_Importa(Prevalence.GetNewImage(Self, 8))._SeExiste end;

procedure T_Importa.SetSeExiste(Value : T_ImportaSeExiste); begin
  if Prevalence.IsInRecoverSnapShot then _SeExiste := Value else begin
    if (_SeExiste = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Importa(NewImage)._SeExiste <> Value then begin
        T_Importa(NewImage)._SeExiste := Value;
        UpdateLog(8, NewImage, Stream)
      end;
    end;
end;

function T_Importa.GetBalancedLine : Boolean; begin Result := T_Importa(Prevalence.GetNewImage(Self, 9))._BalancedLine end;

procedure T_Importa.SetBalancedLine(Value : Boolean); begin
  if Prevalence.IsInRecoverSnapShot then _BalancedLine := Value else begin
    if (_BalancedLine = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Importa(NewImage)._BalancedLine <> Value then begin
        T_Importa(NewImage)._BalancedLine := Value;
        UpdateLog(9, NewImage, Stream)
      end;
    end;
end;

function T_Importa.GetNumeroElementoChave : T_ImportaNumeroElementoChave; begin Result := T_Importa(Prevalence.GetNewImage(Self, 10))._NumeroElementoChave end;

procedure T_Importa.SetNumeroElementoChave(Value : T_ImportaNumeroElementoChave); begin
  if Prevalence.IsInRecoverSnapShot then _NumeroElementoChave := Value else begin
    if (_NumeroElementoChave = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Importa(NewImage)._NumeroElementoChave <> Value then begin
        T_Importa(NewImage)._NumeroElementoChave := Value;
        UpdateLog(10, NewImage, Stream)
      end;
    end;
end;

function T_Importa.GetSaltarFim : Integer; begin Result := T_Importa(Prevalence.GetNewImage(Self, 11))._SaltarFim end;

procedure T_Importa.SetSaltarFim(Value : Integer); begin
  if Prevalence.IsInRecoverSnapShot then _SaltarFim := Value else begin
    if (_SaltarFim = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Importa(NewImage)._SaltarFim <> Value then begin
        T_Importa(NewImage)._SaltarFim := Value;
        UpdateLog(11, NewImage, Stream)
      end;
    end;
end;

function T_Importa.GetSaltarInicio : Integer; begin Result := T_Importa(Prevalence.GetNewImage(Self, 12))._SaltarInicio end;

procedure T_Importa.SetSaltarInicio(Value : Integer); begin
  if Prevalence.IsInRecoverSnapShot then _SaltarInicio := Value else begin
    if (_SaltarInicio = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Importa(NewImage)._SaltarInicio <> Value then begin
        T_Importa(NewImage)._SaltarInicio := Value;
        UpdateLog(12, NewImage, Stream)
      end;
    end;
end;

// Cyclomatic Complexity: 4, Baixo
class procedure T_Importa.GerarModelo(Modelo : String; ImportacaoDelimitada : Boolean);
type
  TContext = record
    This : T_Importa;
    Modelo : String;
    ImportacaoDelimitada : Boolean;
    Obj : TObject;
    fClasse : T_Classe;
  end;
var
  Context : TContext;
  StateMachine : TStateMachine;

procedure Iniciais(var Context : TContext); begin
  with Context, This do fClasse := T_Classe(Browser.Choose(_ClassePorNomeList, [], '', 'Escolha a Classe a ser Importada'));
end;

procedure CriaImportacaoDelimitada(var Context : TContext); begin
  with Context, This do begin
    Obj := CreateImportDelimitado(Modelo, fClasse);
    ;
  end;
end;

procedure CriaImportacaoPosicional(var Context : TContext); begin
  with Context, This do begin
    Obj := CreateImportPosicional(Modelo, fClasse);
    ;
  end;
end;

procedure CustomizacaoDoModeloDeImportacaoDelimitada(var Context : TContext); begin
  with Context, This do begin
    Browser.Edit(T_ImportaDelimitado(Obj), '', 'Customização do Modelo de Importacao Delimitada', '', StateMachine.FixButtons([bbCancel, bbFinish]));
  end;
end;

procedure CustomizacaoDoModeloDeImportacaoPosicional(var Context : TContext); begin
  with Context, This do begin
    Browser.Edit(T_ImportaPosicional(Obj), '', 'Customização do Modelo de Importacao Posicional', '', StateMachine.FixButtons([bbCancel, bbFinish]));
  end;
end;

procedure ErroNaCriacao(var Context : TContext); begin
  with Context, This do Raise Exception.Create('Erro na criação do Modelo');
end;

function EhImportacaoDelimitada(var Context : TContext) : boolean; begin
  with Context, This do Result := ImportacaoDelimitada;
end;

function CriadoOKDelimitada(var Context : TContext) : boolean; begin
  with Context, This do Result := Obj <> nil;
end;

function CriadoOKPosicional(var Context : TContext) : boolean; begin
  with Context, This do Result := Obj <> nil;
end;

begin
  fillchar(Context, sizeof(Context), 0);
  Context.Modelo := Modelo;
  Context.ImportacaoDelimitada := ImportacaoDelimitada;
  StateMachine := TStateMachine.Create('_Administration', '_Importa', 'GerarModelo', [2, 2, 2, 1, 1, 1]);
  with StateMachine do begin
    SetState(0, 'Iniciais', @Iniciais);
    SetState(1, 'CriaImportacaoDelimitada', @CriaImportacaoDelimitada);
    SetState(2, 'CriaImportacaoPosicional', @CriaImportacaoPosicional);
    SetState(3, 'CustomizacaoDoModeloDeImportacaoDelimitada', @CustomizacaoDoModeloDeImportacaoDelimitada, [bbCancel, bbFinish]);
    SetState(4, 'CustomizacaoDoModeloDeImportacaoPosicional', @CustomizacaoDoModeloDeImportacaoPosicional, [bbCancel, bbFinish]);
    SetState(5, 'ErroNaCriacao', @ErroNaCriacao);
    SetTransition(0, 0, 'EhImportacaoDelimitada', @EhImportacaoDelimitada, 1);
    SetTransition(0, 1, 'Default', nil, 2);
    SetTransition(1, 0, 'CriadoOKDelimitada', @CriadoOKDelimitada, 3);
    SetTransition(1, 1, 'Default', nil, 5);
    SetTransition(2, 0, 'CriadoOKPosicional', @CriadoOKPosicional, 4);
    SetTransition(2, 1, 'Default', nil, 5);
    SetTransition(3, 0, 'Default', nil, -1);
    SetTransition(4, 0, 'Default', nil, -1);
    SetTransition(5, 0, 'Default', nil, -1);
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

// Cyclomatic Complexity: 1, Baixo
class procedure T_Importa.Importar(Modelo : T_Importa); begin
  Import(Modelo)
end;

// Cyclomatic Complexity: 4, Baixo
class procedure T_Importa.CustomizarModelo(ImportacaoDelimitada : Boolean);
type
  TContext = record
    This : T_Importa;
    ImportacaoDelimitada : Boolean;
    fImporta : T_Importa;
  end;
var
  Context : TContext;
  StateMachine : TStateMachine;

procedure SelecionaModeloDelimitado(var Context : TContext); begin
  with Context, This do begin
    fImporta := T_Importa(Browser.choose(_ImportaDelimitadoPorNomeList, [], '', 'Escolha o Modelo a ser Customizado'));
    ;
  end;
end;

procedure SelecionaModeloPosicional(var Context : TContext); begin
  with Context, This do begin
    fImporta := T_Importa(Browser.choose(_ImportaPosicionalPorNomeList, [], '', 'Escolha o Modelo a ser Customizado'));
    ;
  end;
end;

procedure CustomizarModeloGerado(var Context : TContext); begin
  with Context, This do begin
    Browser.Edit(fImporta, '', 'Customizar Modelo Gerado', '', StateMachine.FixButtons([bbCancel, bbFinish]));
  end;
end;

function ehImportacaoDelimitada(var Context : TContext) : boolean; begin
  with Context, This do Result := ImportacaoDelimitada;
end;

function DelimitadoSelecionado(var Context : TContext) : boolean; begin
  with Context, This do Result := fImporta <> nil;
end;

function PosicionalSelecionado(var Context : TContext) : boolean; begin
  with Context, This do Result := fImporta <> nil;
end;

begin
  fillchar(Context, sizeof(Context), 0);
  Context.ImportacaoDelimitada := ImportacaoDelimitada;
  StateMachine := TStateMachine.Create('_Administration', '_Importa', 'CustomizarModelo', [2, 2, 2, 1]);
  with StateMachine do begin
    SetState(0, 'Iniciais', nil);
    SetState(1, 'SelecionaModeloDelimitado', @SelecionaModeloDelimitado);
    SetState(2, 'SelecionaModeloPosicional', @SelecionaModeloPosicional);
    SetState(3, 'CustomizarModeloGerado', @CustomizarModeloGerado, [bbCancel, bbFinish]);
    SetTransition(0, 0, 'ehImportacaoDelimitada', @ehImportacaoDelimitada, 1);
    SetTransition(0, 1, 'Default', nil, 2);
    SetTransition(1, 0, 'DelimitadoSelecionado', @DelimitadoSelecionado, 3);
    SetTransition(1, 1, 'Default', nil, -1);
    SetTransition(2, 0, 'PosicionalSelecionado', @PosicionalSelecionado, 3);
    SetTransition(2, 1, 'Default', nil, -1);
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

class function T_ImportaList.GetObjectClass : TTransientClass; begin
  Result := T_Importa
end;

{ T_OcupacaoClasse }

procedure T_OcupacaoClasse.New; begin
  inherited;
  _Ocupacoes := T_OcupacaoClasseOcupacoesAssociation.Create(Self, 'Ocupacoes', true);
end;

procedure T_OcupacaoClasse.InternalFree; begin
  if _Ocupacoes <> nil then _Ocupacoes.InternalFree;
  inherited;
end;

function T_OcupacaoClasse.GetNome : String; begin
  try Result := Classe.AliasNome except Result := ' ' end;
end;

function T_OcupacaoClasse.GetPackage : T_OcupacaoPackage; begin Result := T_OcupacaoClasse(Prevalence.GetNewImage(Self, 4))._Package end;

procedure T_OcupacaoClasse.SetPackage(Value : T_OcupacaoPackage); begin
  if Prevalence.IsInRecover then _Package := Value else begin
    if (_Package = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_OcupacaoClasse(NewImage)._Package <> Value then begin
        T_OcupacaoClasse(NewImage)._Package := Value;
        UpdateLog(4, NewImage, Stream)
      end;
  end;
end;

function T_OcupacaoClasse.GetClasse : T_Classe; begin Result := T_OcupacaoClasse(Prevalence.GetNewImage(Self, 5))._Classe end;

procedure T_OcupacaoClasse.SetClasse(Value : T_Classe); begin
  if Prevalence.IsInRecover then _Classe := Value else begin
    if (_Classe = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_OcupacaoClasse(NewImage)._Classe <> Value then begin
        T_OcupacaoClasse(NewImage)._Classe := Value;
        UpdateLog(5, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_OcupacaoClasse.PorNome : String; begin
  Result := Nome
end;

{ T_OcupacaoClasseList }

class function T_OcupacaoClasseList.GetObjectClass : TTransientClass; begin
  Result := T_OcupacaoClasse
end;

procedure T_OcupacaoClasseList.Add(_OcupacaoClasse : T_OcupacaoClasse); begin
  inherited Add(TPrevalent(_OcupacaoClasse))
end;

procedure T_OcupacaoClasseList.Delete(_OcupacaoClasse : T_OcupacaoClasse); begin
  inherited Delete(TPrevalent(_OcupacaoClasse))
end;

function T_OcupacaoClasseList.Find(I : Integer) : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(inherited Find(I))
end;

function T_OcupacaoClasseList.First : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(inherited First)
end;

function T_OcupacaoClasseList.Last : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(inherited Last)
end;

function T_OcupacaoClasseList.Near(I : Integer) : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(inherited Near(I))
end;

function T_OcupacaoClasseList.Next(var _OcupacaoClasse : T_OcupacaoClasse) : boolean; begin
  Result := inherited Next(TTransient(_OcupacaoClasse))
end;

function T_OcupacaoClasseList.Prior(var _OcupacaoClasse : T_OcupacaoClasse) : boolean; begin
  Result := inherited Prior(TTransient(_OcupacaoClasse))
end;

function T_OcupacaoClasseList.Get_OcupacaoClasse(I : Integer) : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(Objects[I])
end;

{ T_OcupacaoClassePorNomeList }

class function T_OcupacaoClassePorNomeList.GetKeyCode : pointer; begin
  Result := @T_OcupacaoClasse.PorNome
end;

class function T_OcupacaoClassePorNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_OcupacaoClassePorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_OcupacaoClasse
end;

procedure T_OcupacaoClassePorNomeList.Add(_OcupacaoClasse : T_OcupacaoClasse); begin
  inherited Add(TPrevalent(_OcupacaoClasse))
end;

procedure T_OcupacaoClassePorNomeList.Delete(_OcupacaoClasse : T_OcupacaoClasse); begin
  inherited Delete(TPrevalent(_OcupacaoClasse))
end;

function T_OcupacaoClassePorNomeList.Find(S : String) : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(inherited Find(S))
end;

function T_OcupacaoClassePorNomeList.First : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(inherited First)
end;

function T_OcupacaoClassePorNomeList.Last : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(inherited Last)
end;

function T_OcupacaoClassePorNomeList.Near(S : String) : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(inherited Near(S))
end;

function T_OcupacaoClassePorNomeList.Next(var _OcupacaoClasse : T_OcupacaoClasse) : boolean; begin
  Result := inherited Next(TTransient(_OcupacaoClasse))
end;

function T_OcupacaoClassePorNomeList.Prior(var _OcupacaoClasse : T_OcupacaoClasse) : boolean; begin
  Result := inherited Prior(TTransient(_OcupacaoClasse))
end;

function T_OcupacaoClassePorNomeList.Get_OcupacaoClasse(I : Integer) : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(Objects[I])
end;

{ T_OcupacaoPackageClassesAssociation }

class function T_OcupacaoPackageClassesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_OcupacaoClasse
end;

procedure T_OcupacaoPackageClassesAssociation.Add(_OcupacaoClasse : T_OcupacaoClasse); begin
  inherited Add(TPrevalent(_OcupacaoClasse))
end;

procedure T_OcupacaoPackageClassesAssociation.Delete(_OcupacaoClasse : T_OcupacaoClasse); begin
  inherited Delete(TPrevalent(_OcupacaoClasse))
end;

function T_OcupacaoPackageClassesAssociation.Find(I : Integer) : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(inherited Find(I))
end;

function T_OcupacaoPackageClassesAssociation.First : T_OcupacaoClasse; begin
  SetDependencyLists;
  Result := T_OcupacaoClasse(inherited First)
end;

function T_OcupacaoPackageClassesAssociation.Last : T_OcupacaoClasse; begin
  SetDependencyLists;
  Result := T_OcupacaoClasse(inherited Last)
end;

function T_OcupacaoPackageClassesAssociation.Near(I : Integer) : T_OcupacaoClasse; begin
  SetDependencyLists;
  Result := T_OcupacaoClasse(inherited Near(I))
end;

function T_OcupacaoPackageClassesAssociation.Next(var _OcupacaoClasse : T_OcupacaoClasse) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_OcupacaoClasse))
end;

function T_OcupacaoPackageClassesAssociation.Prior(var _OcupacaoClasse : T_OcupacaoClasse) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_OcupacaoClasse))
end;

function T_OcupacaoPackageClassesAssociation.Get_OcupacaoClasse(I : Integer) : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(Objects[I])
end;

{ T_ClasseOcupacoesAssociation }

class function T_ClasseOcupacoesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_OcupacaoClasse
end;

procedure T_ClasseOcupacoesAssociation.Add(_OcupacaoClasse : T_OcupacaoClasse); begin
  inherited Add(TPrevalent(_OcupacaoClasse))
end;

procedure T_ClasseOcupacoesAssociation.Delete(_OcupacaoClasse : T_OcupacaoClasse); begin
  inherited Delete(TPrevalent(_OcupacaoClasse))
end;

function T_ClasseOcupacoesAssociation.Find(I : Integer) : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(inherited Find(I))
end;

function T_ClasseOcupacoesAssociation.First : T_OcupacaoClasse; begin
  SetDependencyLists;
  Result := T_OcupacaoClasse(inherited First)
end;

function T_ClasseOcupacoesAssociation.Last : T_OcupacaoClasse; begin
  SetDependencyLists;
  Result := T_OcupacaoClasse(inherited Last)
end;

function T_ClasseOcupacoesAssociation.Near(I : Integer) : T_OcupacaoClasse; begin
  SetDependencyLists;
  Result := T_OcupacaoClasse(inherited Near(I))
end;

function T_ClasseOcupacoesAssociation.Next(var _OcupacaoClasse : T_OcupacaoClasse) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_OcupacaoClasse))
end;

function T_ClasseOcupacoesAssociation.Prior(var _OcupacaoClasse : T_OcupacaoClasse) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_OcupacaoClasse))
end;

function T_ClasseOcupacoesAssociation.Get_OcupacaoClasse(I : Integer) : T_OcupacaoClasse; begin
  Result := T_OcupacaoClasse(Objects[I])
end;

{ T_OcupacaoLista }

procedure T_OcupacaoLista.New; begin
  inherited;
  _Associacoes := T_OcupacaoListaAssociacoesAssociation.Create(Self, 'Associacoes', true);
end;

procedure T_OcupacaoLista.InternalFree; begin
  if _Associacoes <> nil then _Associacoes.InternalFree;
  inherited;
end;

function T_OcupacaoLista.GetIndice : Integer; begin Result := T_OcupacaoLista(Prevalence.GetNewImage(Self, 2))._Indice end;

procedure T_OcupacaoLista.SetIndice(Value : Integer); begin
  if Prevalence.IsInRecoverSnapShot then _Indice := Value else begin
    if (_Indice = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_OcupacaoLista(NewImage)._Indice <> Value then begin
        T_OcupacaoLista(NewImage)._Indice := Value;
        UpdateLog(2, NewImage, Stream)
      end;
    end;
end;

function T_OcupacaoLista.GetLista : String; begin
  try Result := prevalence.prevalentlists(Indice).classname except Result := ' ' end;
end;

function T_OcupacaoLista.GetCount : Integer; begin
  try Result := Prevalence.PrevalentLists(Indice).Count except Result := 0 end;
end;

function T_OcupacaoLista.GetCapacity : Integer; begin
  try Result := Prevalence.PrevalentLists(Indice).Capacity except Result := 0 end;
end;

function T_OcupacaoLista.GetMemoria : Double; begin
  try Result := CalculaOcupacaoMemoriaList except Result := 0 end;
end;

function T_OcupacaoLista.GetClasse : T_OcupacaoClasse; begin Result := T_OcupacaoLista(Prevalence.GetNewImage(Self, 8))._Classe end;

procedure T_OcupacaoLista.SetClasse(Value : T_OcupacaoClasse); begin
  if Prevalence.IsInRecover then _Classe := Value else begin
    if (_Classe = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_OcupacaoLista(NewImage)._Classe <> Value then begin
        T_OcupacaoLista(NewImage)._Classe := Value;
        UpdateLog(8, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_OcupacaoLista.CalculaOcupacaoMemoriaList : Double; begin
  Result := CalculaOcupacaoMemoriaLista(Self)
end;

{ T_OcupacaoListaList }

class function T_OcupacaoListaList.GetObjectClass : TTransientClass; begin
  Result := T_OcupacaoLista
end;

procedure T_OcupacaoListaList.Add(_OcupacaoLista : T_OcupacaoLista); begin
  inherited Add(TPrevalent(_OcupacaoLista))
end;

procedure T_OcupacaoListaList.Delete(_OcupacaoLista : T_OcupacaoLista); begin
  inherited Delete(TPrevalent(_OcupacaoLista))
end;

function T_OcupacaoListaList.Find(I : Integer) : T_OcupacaoLista; begin
  Result := T_OcupacaoLista(inherited Find(I))
end;

function T_OcupacaoListaList.First : T_OcupacaoLista; begin
  Result := T_OcupacaoLista(inherited First)
end;

function T_OcupacaoListaList.Last : T_OcupacaoLista; begin
  Result := T_OcupacaoLista(inherited Last)
end;

function T_OcupacaoListaList.Near(I : Integer) : T_OcupacaoLista; begin
  Result := T_OcupacaoLista(inherited Near(I))
end;

function T_OcupacaoListaList.Next(var _OcupacaoLista : T_OcupacaoLista) : boolean; begin
  Result := inherited Next(TTransient(_OcupacaoLista))
end;

function T_OcupacaoListaList.Prior(var _OcupacaoLista : T_OcupacaoLista) : boolean; begin
  Result := inherited Prior(TTransient(_OcupacaoLista))
end;

function T_OcupacaoListaList.Get_OcupacaoLista(I : Integer) : T_OcupacaoLista; begin
  Result := T_OcupacaoLista(Objects[I])
end;

{ T_OcupacaoClasseOcupacoesAssociation }

class function T_OcupacaoClasseOcupacoesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_OcupacaoLista
end;

procedure T_OcupacaoClasseOcupacoesAssociation.Add(_OcupacaoLista : T_OcupacaoLista); begin
  inherited Add(TPrevalent(_OcupacaoLista))
end;

procedure T_OcupacaoClasseOcupacoesAssociation.Delete(_OcupacaoLista : T_OcupacaoLista); begin
  inherited Delete(TPrevalent(_OcupacaoLista))
end;

function T_OcupacaoClasseOcupacoesAssociation.Find(I : Integer) : T_OcupacaoLista; begin
  Result := T_OcupacaoLista(inherited Find(I))
end;

function T_OcupacaoClasseOcupacoesAssociation.First : T_OcupacaoLista; begin
  SetDependencyLists;
  Result := T_OcupacaoLista(inherited First)
end;

function T_OcupacaoClasseOcupacoesAssociation.Last : T_OcupacaoLista; begin
  SetDependencyLists;
  Result := T_OcupacaoLista(inherited Last)
end;

function T_OcupacaoClasseOcupacoesAssociation.Near(I : Integer) : T_OcupacaoLista; begin
  SetDependencyLists;
  Result := T_OcupacaoLista(inherited Near(I))
end;

function T_OcupacaoClasseOcupacoesAssociation.Next(var _OcupacaoLista : T_OcupacaoLista) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_OcupacaoLista))
end;

function T_OcupacaoClasseOcupacoesAssociation.Prior(var _OcupacaoLista : T_OcupacaoLista) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_OcupacaoLista))
end;

function T_OcupacaoClasseOcupacoesAssociation.Get_OcupacaoLista(I : Integer) : T_OcupacaoLista; begin
  Result := T_OcupacaoLista(Objects[I])
end;

{ T_OcupacaoPackage }

procedure T_OcupacaoPackage.New; begin
  inherited;
  _Classes := T_OcupacaoPackageClassesAssociation.Create(Self, 'Classes', true);
end;

procedure T_OcupacaoPackage.InternalFree; begin
  if _Classes <> nil then _Classes.InternalFree;
  inherited;
end;

function T_OcupacaoPackage.GetPackage : T_Package; begin Result := T_OcupacaoPackage(Prevalence.GetNewImage(Self, 3))._Package end;

procedure T_OcupacaoPackage.SetPackage(Value : T_Package); begin
  if Prevalence.IsInRecover then _Package := Value else begin
    if (_Package = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_OcupacaoPackage(NewImage)._Package <> Value then begin
        T_OcupacaoPackage(NewImage)._Package := Value;
        UpdateLog(3, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
class procedure T_OcupacaoPackage.InicializaOcupacao; begin
  InicializaClassesOcupacao;
end;

// Cyclomatic Complexity: 1, Baixo
function T_OcupacaoPackage.PorNome : String; begin
  Result := Package.Nome
end;

{ T_OcupacaoPackageList }

class function T_OcupacaoPackageList.GetObjectClass : TTransientClass; begin
  Result := T_OcupacaoPackage
end;

procedure T_OcupacaoPackageList.Add(_OcupacaoPackage : T_OcupacaoPackage); begin
  inherited Add(TPrevalent(_OcupacaoPackage))
end;

procedure T_OcupacaoPackageList.Delete(_OcupacaoPackage : T_OcupacaoPackage); begin
  inherited Delete(TPrevalent(_OcupacaoPackage))
end;

function T_OcupacaoPackageList.Find(I : Integer) : T_OcupacaoPackage; begin
  Result := T_OcupacaoPackage(inherited Find(I))
end;

function T_OcupacaoPackageList.First : T_OcupacaoPackage; begin
  Result := T_OcupacaoPackage(inherited First)
end;

function T_OcupacaoPackageList.Last : T_OcupacaoPackage; begin
  Result := T_OcupacaoPackage(inherited Last)
end;

function T_OcupacaoPackageList.Near(I : Integer) : T_OcupacaoPackage; begin
  Result := T_OcupacaoPackage(inherited Near(I))
end;

function T_OcupacaoPackageList.Next(var _OcupacaoPackage : T_OcupacaoPackage) : boolean; begin
  Result := inherited Next(TTransient(_OcupacaoPackage))
end;

function T_OcupacaoPackageList.Prior(var _OcupacaoPackage : T_OcupacaoPackage) : boolean; begin
  Result := inherited Prior(TTransient(_OcupacaoPackage))
end;

function T_OcupacaoPackageList.Get_OcupacaoPackage(I : Integer) : T_OcupacaoPackage; begin
  Result := T_OcupacaoPackage(Objects[I])
end;

{ T_OcupacaoPackagePorNomeList }

class function T_OcupacaoPackagePorNomeList.GetKeyCode : pointer; begin
  Result := @T_OcupacaoPackage.PorNome
end;

class function T_OcupacaoPackagePorNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_OcupacaoPackagePorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_OcupacaoPackage
end;

procedure T_OcupacaoPackagePorNomeList.Add(_OcupacaoPackage : T_OcupacaoPackage); begin
  inherited Add(TPrevalent(_OcupacaoPackage))
end;

procedure T_OcupacaoPackagePorNomeList.Delete(_OcupacaoPackage : T_OcupacaoPackage); begin
  inherited Delete(TPrevalent(_OcupacaoPackage))
end;

function T_OcupacaoPackagePorNomeList.Find(S : String) : T_OcupacaoPackage; begin
  Result := T_OcupacaoPackage(inherited Find(S))
end;

function T_OcupacaoPackagePorNomeList.First : T_OcupacaoPackage; begin
  Result := T_OcupacaoPackage(inherited First)
end;

function T_OcupacaoPackagePorNomeList.Last : T_OcupacaoPackage; begin
  Result := T_OcupacaoPackage(inherited Last)
end;

function T_OcupacaoPackagePorNomeList.Near(S : String) : T_OcupacaoPackage; begin
  Result := T_OcupacaoPackage(inherited Near(S))
end;

function T_OcupacaoPackagePorNomeList.Next(var _OcupacaoPackage : T_OcupacaoPackage) : boolean; begin
  Result := inherited Next(TTransient(_OcupacaoPackage))
end;

function T_OcupacaoPackagePorNomeList.Prior(var _OcupacaoPackage : T_OcupacaoPackage) : boolean; begin
  Result := inherited Prior(TTransient(_OcupacaoPackage))
end;

function T_OcupacaoPackagePorNomeList.Get_OcupacaoPackage(I : Integer) : T_OcupacaoPackage; begin
  Result := T_OcupacaoPackage(Objects[I])
end;

{ T_PackageOcupacoesAssociation }

class function T_PackageOcupacoesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_OcupacaoPackage
end;

procedure T_PackageOcupacoesAssociation.Add(_OcupacaoPackage : T_OcupacaoPackage); begin
  inherited Add(TPrevalent(_OcupacaoPackage))
end;

procedure T_PackageOcupacoesAssociation.Delete(_OcupacaoPackage : T_OcupacaoPackage); begin
  inherited Delete(TPrevalent(_OcupacaoPackage))
end;

function T_PackageOcupacoesAssociation.Find(I : Integer) : T_OcupacaoPackage; begin
  Result := T_OcupacaoPackage(inherited Find(I))
end;

function T_PackageOcupacoesAssociation.First : T_OcupacaoPackage; begin
  SetDependencyLists;
  Result := T_OcupacaoPackage(inherited First)
end;

function T_PackageOcupacoesAssociation.Last : T_OcupacaoPackage; begin
  SetDependencyLists;
  Result := T_OcupacaoPackage(inherited Last)
end;

function T_PackageOcupacoesAssociation.Near(I : Integer) : T_OcupacaoPackage; begin
  SetDependencyLists;
  Result := T_OcupacaoPackage(inherited Near(I))
end;

function T_PackageOcupacoesAssociation.Next(var _OcupacaoPackage : T_OcupacaoPackage) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_OcupacaoPackage))
end;

function T_PackageOcupacoesAssociation.Prior(var _OcupacaoPackage : T_OcupacaoPackage) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_OcupacaoPackage))
end;

function T_PackageOcupacoesAssociation.Get_OcupacaoPackage(I : Integer) : T_OcupacaoPackage; begin
  Result := T_OcupacaoPackage(Objects[I])
end;

{ T_Pendency }

procedure T_Pendency.New; begin
  inherited;
  if Prevalence.IsInRecover then exit;
  try _DateTime := Now except end;
end;

function T_Pendency.GetClassAlias : String; begin
  try Result := Method.Classe.AliasNome except Result := ' ' end;
end;

function T_Pendency.GetDateTime : DateTime; begin Result := T_Pendency(Prevalence.GetNewImage(Self, 9))._DateTime end;

procedure T_Pendency.SetDateTime(Value : DateTime); begin
  if Prevalence.IsInRecoverSnapShot then _DateTime := Value else begin
    if (_DateTime = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Pendency(NewImage)._DateTime <> Value then begin
        T_Pendency(NewImage)._DateTime := Value;
        UpdateLog(9, NewImage, Stream)
      end;
    end;
end;

function T_Pendency.GetDeadLine : DateTime; begin
  try Result := CalcDeadLine except Result := 0 end;
end;

function T_Pendency.GetDelegateUser : String; begin Result := T_Pendency(Prevalence.GetNewImage(Self, 11))._DelegateUser end;

procedure T_Pendency.SetDelegateUser(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _DelegateUser := Value else begin
    if (_DelegateUser = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Pendency(NewImage)._DelegateUser <> Value then begin
        T_Pendency(NewImage)._DelegateUser := Value;
        UpdateLog(11, NewImage, Stream)
      end;
    end;
end;

function T_Pendency.GetMethodAlias : String; begin
  try Result := Copy(Method.AliasNome,9,MaxInt) except Result := ' ' end;
end;

function T_Pendency.GetObjIdentification : String; begin
  try Result := RunObject.GetIdentification except Result := ' ' end;
end;

function T_Pendency.GetWorkFlowAlias : String; begin
  try Result := CalcWorkFlowAlias except Result := ' ' end;
end;

function T_Pendency.GetAssignedProfile : T_Grupo; begin Result := T_Pendency(Prevalence.GetNewImage(Self, 15))._AssignedProfile end;

procedure T_Pendency.SetAssignedProfile(Value : T_Grupo); begin
  if Prevalence.IsInRecover then _AssignedProfile := Value else begin
    if (_AssignedProfile = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Pendency(NewImage)._AssignedProfile <> Value then begin
        T_Pendency(NewImage)._AssignedProfile := Value;
        UpdateLog(15, NewImage, Stream)
      end;
  end;
end;

function T_Pendency.GetDelegate : T_Grupo; begin Result := T_Pendency(Prevalence.GetNewImage(Self, 16))._Delegate end;

procedure T_Pendency.SetDelegate(Value : T_Grupo); begin
  if Prevalence.IsInRecover then _Delegate := Value else begin
    if (_Delegate = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Pendency(NewImage)._Delegate <> Value then begin
        T_Pendency(NewImage)._Delegate := Value;
        UpdateLog(16, NewImage, Stream)
      end;
  end;
end;

function T_Pendency.GetTimeOut : T_PendencyTimeoutTask; begin Result := T_Pendency(Prevalence.GetNewImage(Self, 17))._TimeOut end;

procedure T_Pendency.SetTimeOut(Value : T_PendencyTimeoutTask); begin
  if Prevalence.IsInRecover then _TimeOut := Value else begin
    if (_TimeOut = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Pendency(NewImage)._TimeOut <> Value then begin
        T_Pendency(NewImage)._TimeOut := Value;
        UpdateLog(17, NewImage, Stream)
      end;
  end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Pendency.ByPriority : Double; begin
  Result := DeadLine
end;

// Cyclomatic Complexity: 1, Baixo
function T_Pendency.ByRunObject : String; begin
  Result := Method.Classe.Nome + ':' + IntToStr(RunObjectID)
end;

// Cyclomatic Complexity: 2, Baixo
function T_Pendency.CalcWorkFlowAlias : String; begin
  // Locates the Workflow method alias
with Prevalence.Metadata('T' + Method.Classe.Nome).MethodByName(ExtractFromStr(Method.Nome, '_'))^ do
begin
  Result := Alias;//*Trim(StringReplace(ExtractAlias(Alias), '|', ' ', [rfReplaceAll]));
  if Result = '' then
    Result := Name;
end;
end;

// Cyclomatic Complexity: 3, Baixo
function T_Pendency.GetDelegateProfiles : TObjectList;
  var
  Profile: T_Grupo;
begin
  Profile := _GrupoPorNomeList.Find(Browser.UserInfo.Profile);
  if Assigned(Profile) then
    Result := Profile.CanDelegate
  else Result := _GrupoPorNomeList;
end;

// Cyclomatic Complexity: 2, Baixo
function T_Pendency.GetDelegateUsers : String; begin
  if Assigned(Delegate) then Result := GetProfileUsers(Delegate.Nome) else Result := '';
end;

// Cyclomatic Complexity: 2, Baixo
function T_Pendency.CalcDeadLine : TDateTime; begin
  if TimeOut=nil then Result := DateTime else Result := TimeOut.Start
end;

// Cyclomatic Complexity: 6, Baixo
function T_Pendency.ValidDelegateUser(aDelegateUser : String) : boolean;
  var
  Usr, Usrs: string;
  aPos: integer;
begin
  Result := True;
exit;
  aDelegateUser := Trim(aDelegateUser);
  if not GetSecurity or (aDelegateUser = '') then exit;
  Usrs := GetDelegateUsers;
  Usr := GetUserInfo(tuUserFullNameAndUser, aDelegateUser);
  if Usr = '' then Usr := aDelegateUser;
  aPos := 1;
  while aPos <= length(Usrs) do
    if SameText(ExtractFromStr(Usrs, aPos), Usr) then exit;
  Result := False;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Pendency.ValidDelegateProfile(aDelegateProfileName : String) : boolean; begin
  Result := (aDelegateProfileName = '') or (GetDelegateProfiles.Find(_GrupoPorNomeList.Find(aDelegateProfileName)) <> nil);
end;

// Cyclomatic Complexity: 4, Baixo
function T_Pendency.InternalCheckDelegateUser : boolean; begin
  //var
//  CompleteName: string;
begin
  Result := ValidDelegateUser(DelegateUser);
  // Não ajusta mais o nome, pois demora para pegar o nome completo no AD
{  CompleteName := GetUserInfo(tuUserFullNameAndUser, DelegateUser);
  if (CompleteName <> '') and not SameText(CompleteName, DelegateUser) then
    DelegateUser := CompleteName;}
end;
end;

function T_Pendency.CheckDelegateUser(var Message : String) : Boolean; begin
  Result := InternalCheckDelegateUser;
  if Result then
    Message := ''
  else
    Message := 'Não é permitida a delegação para este usuário'
end;

function T_Pendency.EnabledDelegateUser : Boolean; begin
  Result := Assigned(Delegate);
end;

function T_Pendency.CheckDelegate(var Message : String) : Boolean; begin
  Result := True; if Delegate=nil then DelegateUser:='';
  if Result then
    Message := ''
  else
    Message := '.'
end;

function T_Pendency.AssociationConstraintDelegate : TObjectList; begin
  Result := GetDelegateProfiles;
end;

{ T_PendencyList }

class function T_PendencyList.GetObjectClass : TTransientClass; begin
  Result := T_Pendency
end;

procedure T_PendencyList.Add(_Pendency : T_Pendency); begin
  inherited Add(TPrevalent(_Pendency))
end;

procedure T_PendencyList.Delete(_Pendency : T_Pendency); begin
  inherited Delete(TPrevalent(_Pendency))
end;

function T_PendencyList.Find(I : Integer) : T_Pendency; begin
  Result := T_Pendency(inherited Find(I))
end;

function T_PendencyList.First : T_Pendency; begin
  Result := T_Pendency(inherited First)
end;

function T_PendencyList.Last : T_Pendency; begin
  Result := T_Pendency(inherited Last)
end;

function T_PendencyList.Near(I : Integer) : T_Pendency; begin
  Result := T_Pendency(inherited Near(I))
end;

function T_PendencyList.Next(var _Pendency : T_Pendency) : boolean; begin
  Result := inherited Next(TTransient(_Pendency))
end;

function T_PendencyList.Prior(var _Pendency : T_Pendency) : boolean; begin
  Result := inherited Prior(TTransient(_Pendency))
end;

function T_PendencyList.Get_Pendency(I : Integer) : T_Pendency; begin
  Result := T_Pendency(Objects[I])
end;

{ T_PendencyByPriorityList }

class function T_PendencyByPriorityList.GetKeyCode : pointer; begin
  Result := @T_Pendency.ByPriority
end;

class function T_PendencyByPriorityList.GetListType : TListType; begin
  Result := ltDouble
end;

class function T_PendencyByPriorityList.GetObjectClass : TTransientClass; begin
  Result := T_Pendency
end;

procedure T_PendencyByPriorityList.Add(_Pendency : T_Pendency); begin
  inherited Add(TPrevalent(_Pendency))
end;

procedure T_PendencyByPriorityList.Delete(_Pendency : T_Pendency); begin
  inherited Delete(TPrevalent(_Pendency))
end;

function T_PendencyByPriorityList.Find(D : Double) : T_Pendency; begin
  Result := T_Pendency(inherited Find(D))
end;

function T_PendencyByPriorityList.First : T_Pendency; begin
  Result := T_Pendency(inherited First)
end;

function T_PendencyByPriorityList.Last : T_Pendency; begin
  Result := T_Pendency(inherited Last)
end;

function T_PendencyByPriorityList.Near(D : Double) : T_Pendency; begin
  Result := T_Pendency(inherited Near(D))
end;

function T_PendencyByPriorityList.Next(var _Pendency : T_Pendency) : boolean; begin
  Result := inherited Next(TTransient(_Pendency))
end;

function T_PendencyByPriorityList.Prior(var _Pendency : T_Pendency) : boolean; begin
  Result := inherited Prior(TTransient(_Pendency))
end;

function T_PendencyByPriorityList.Get_Pendency(I : Integer) : T_Pendency; begin
  Result := T_Pendency(Objects[I])
end;

{ T_PendencyByRunObjectList }

class function T_PendencyByRunObjectList.GetKeyCode : pointer; begin
  Result := @T_Pendency.ByRunObject
end;

class function T_PendencyByRunObjectList.GetListType : TListType; begin
  Result := ltString
end;

class function T_PendencyByRunObjectList.GetObjectClass : TTransientClass; begin
  Result := T_Pendency
end;

procedure T_PendencyByRunObjectList.Add(_Pendency : T_Pendency); begin
  inherited Add(TPrevalent(_Pendency))
end;

procedure T_PendencyByRunObjectList.Delete(_Pendency : T_Pendency); begin
  inherited Delete(TPrevalent(_Pendency))
end;

function T_PendencyByRunObjectList.Find(S : String) : T_Pendency; begin
  Result := T_Pendency(inherited Find(S))
end;

function T_PendencyByRunObjectList.First : T_Pendency; begin
  Result := T_Pendency(inherited First)
end;

function T_PendencyByRunObjectList.Last : T_Pendency; begin
  Result := T_Pendency(inherited Last)
end;

function T_PendencyByRunObjectList.Near(S : String) : T_Pendency; begin
  Result := T_Pendency(inherited Near(S))
end;

function T_PendencyByRunObjectList.Next(var _Pendency : T_Pendency) : boolean; begin
  Result := inherited Next(TTransient(_Pendency))
end;

function T_PendencyByRunObjectList.Prior(var _Pendency : T_Pendency) : boolean; begin
  Result := inherited Prior(TTransient(_Pendency))
end;

function T_PendencyByRunObjectList.Get_Pendency(I : Integer) : T_Pendency; begin
  Result := T_Pendency(Objects[I])
end;

{ T_GrupoAssignedPendenciesAssociation }

class function T_GrupoAssignedPendenciesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Pendency
end;

procedure T_GrupoAssignedPendenciesAssociation.Add(_Pendency : T_Pendency); begin
  inherited Add(TPrevalent(_Pendency))
end;

procedure T_GrupoAssignedPendenciesAssociation.Delete(_Pendency : T_Pendency); begin
  inherited Delete(TPrevalent(_Pendency))
end;

function T_GrupoAssignedPendenciesAssociation.Find(I : Integer) : T_Pendency; begin
  Result := T_Pendency(inherited Find(I))
end;

function T_GrupoAssignedPendenciesAssociation.First : T_Pendency; begin
  SetDependencyLists;
  Result := T_Pendency(inherited First)
end;

function T_GrupoAssignedPendenciesAssociation.Last : T_Pendency; begin
  SetDependencyLists;
  Result := T_Pendency(inherited Last)
end;

function T_GrupoAssignedPendenciesAssociation.Near(I : Integer) : T_Pendency; begin
  SetDependencyLists;
  Result := T_Pendency(inherited Near(I))
end;

function T_GrupoAssignedPendenciesAssociation.Next(var _Pendency : T_Pendency) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Pendency))
end;

function T_GrupoAssignedPendenciesAssociation.Prior(var _Pendency : T_Pendency) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Pendency))
end;

function T_GrupoAssignedPendenciesAssociation.Get_Pendency(I : Integer) : T_Pendency; begin
  Result := T_Pendency(Objects[I])
end;

{ T_GrupoPendenciesAssociation }

class function T_GrupoPendenciesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_Pendency
end;

procedure T_GrupoPendenciesAssociation.Add(_Pendency : T_Pendency); begin
  inherited Add(TPrevalent(_Pendency))
end;

procedure T_GrupoPendenciesAssociation.Delete(_Pendency : T_Pendency); begin
  inherited Delete(TPrevalent(_Pendency))
end;

function T_GrupoPendenciesAssociation.Find(I : Integer) : T_Pendency; begin
  Result := T_Pendency(inherited Find(I))
end;

function T_GrupoPendenciesAssociation.First : T_Pendency; begin
  SetDependencyLists;
  Result := T_Pendency(inherited First)
end;

function T_GrupoPendenciesAssociation.Last : T_Pendency; begin
  SetDependencyLists;
  Result := T_Pendency(inherited Last)
end;

function T_GrupoPendenciesAssociation.Near(I : Integer) : T_Pendency; begin
  SetDependencyLists;
  Result := T_Pendency(inherited Near(I))
end;

function T_GrupoPendenciesAssociation.Next(var _Pendency : T_Pendency) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_Pendency))
end;

function T_GrupoPendenciesAssociation.Prior(var _Pendency : T_Pendency) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_Pendency))
end;

function T_GrupoPendenciesAssociation.Get_Pendency(I : Integer) : T_Pendency; begin
  Result := T_Pendency(Objects[I])
end;

function T_PropriedadeDelimitado.GetImportaDelimitado : T_ImportaDelimitado; begin Result := T_PropriedadeDelimitado(Prevalence.GetNewImage(Self, 7))._ImportaDelimitado end;

procedure T_PropriedadeDelimitado.SetImportaDelimitado(Value : T_ImportaDelimitado); begin
  if Prevalence.IsInRecover then _ImportaDelimitado := Value else begin
    if (_ImportaDelimitado = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PropriedadeDelimitado(NewImage)._ImportaDelimitado <> Value then begin
        T_PropriedadeDelimitado(NewImage)._ImportaDelimitado := Value;
        UpdateLog(7, NewImage, Stream)
      end;
  end;
end;

{ T_PropriedadeDelimitadoList }

class function T_PropriedadeDelimitadoList.GetObjectClass : TTransientClass; begin
  Result := T_PropriedadeDelimitado
end;

procedure T_PropriedadeDelimitadoList.Add(_PropriedadeDelimitado : T_PropriedadeDelimitado); begin
  inherited Add(TPrevalent(_PropriedadeDelimitado))
end;

procedure T_PropriedadeDelimitadoList.Delete(_PropriedadeDelimitado : T_PropriedadeDelimitado); begin
  inherited Delete(TPrevalent(_PropriedadeDelimitado))
end;

function T_PropriedadeDelimitadoList.Find(I : Integer) : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(inherited Find(I))
end;

function T_PropriedadeDelimitadoList.First : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(inherited First)
end;

function T_PropriedadeDelimitadoList.Last : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(inherited Last)
end;

function T_PropriedadeDelimitadoList.Near(I : Integer) : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(inherited Near(I))
end;

function T_PropriedadeDelimitadoList.Next(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean; begin
  Result := inherited Next(TTransient(_PropriedadeDelimitado))
end;

function T_PropriedadeDelimitadoList.Prior(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean; begin
  Result := inherited Prior(TTransient(_PropriedadeDelimitado))
end;

function T_PropriedadeDelimitadoList.Get_PropriedadeDelimitado(I : Integer) : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(Objects[I])
end;

{ T_PropriedadeDelimitadoPorElementoList }

class function T_PropriedadeDelimitadoPorElementoList.GetKeyCode : pointer; begin
  Result := @T_PropriedadeDelimitado.PorElemento
end;

class function T_PropriedadeDelimitadoPorElementoList.GetObjectClass : TTransientClass; begin
  Result := T_PropriedadeDelimitado
end;

procedure T_PropriedadeDelimitadoPorElementoList.Add(_PropriedadeDelimitado : T_PropriedadeDelimitado); begin
  inherited Add(TPrevalent(_PropriedadeDelimitado))
end;

procedure T_PropriedadeDelimitadoPorElementoList.Delete(_PropriedadeDelimitado : T_PropriedadeDelimitado); begin
  inherited Delete(TPrevalent(_PropriedadeDelimitado))
end;

function T_PropriedadeDelimitadoPorElementoList.Find(I : Integer) : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(inherited Find(I))
end;

function T_PropriedadeDelimitadoPorElementoList.First : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(inherited First)
end;

function T_PropriedadeDelimitadoPorElementoList.Last : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(inherited Last)
end;

function T_PropriedadeDelimitadoPorElementoList.Near(I : Integer) : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(inherited Near(I))
end;

function T_PropriedadeDelimitadoPorElementoList.Next(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean; begin
  Result := inherited Next(TTransient(_PropriedadeDelimitado))
end;

function T_PropriedadeDelimitadoPorElementoList.Prior(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean; begin
  Result := inherited Prior(TTransient(_PropriedadeDelimitado))
end;

function T_PropriedadeDelimitadoPorElementoList.Get_PropriedadeDelimitado(I : Integer) : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(Objects[I])
end;

{ T_PropriedadeDelimitadoPorPropNomeList }

class function T_PropriedadeDelimitadoPorPropNomeList.GetKeyCode : pointer; begin
  Result := @T_PropriedadeDelimitado.PorPropNome
end;

class function T_PropriedadeDelimitadoPorPropNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_PropriedadeDelimitadoPorPropNomeList.GetObjectClass : TTransientClass; begin
  Result := T_PropriedadeDelimitado
end;

procedure T_PropriedadeDelimitadoPorPropNomeList.Add(_PropriedadeDelimitado : T_PropriedadeDelimitado); begin
  inherited Add(TPrevalent(_PropriedadeDelimitado))
end;

procedure T_PropriedadeDelimitadoPorPropNomeList.Delete(_PropriedadeDelimitado : T_PropriedadeDelimitado); begin
  inherited Delete(TPrevalent(_PropriedadeDelimitado))
end;

function T_PropriedadeDelimitadoPorPropNomeList.Find(S : String) : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(inherited Find(S))
end;

function T_PropriedadeDelimitadoPorPropNomeList.First : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(inherited First)
end;

function T_PropriedadeDelimitadoPorPropNomeList.Last : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(inherited Last)
end;

function T_PropriedadeDelimitadoPorPropNomeList.Near(S : String) : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(inherited Near(S))
end;

function T_PropriedadeDelimitadoPorPropNomeList.Next(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean; begin
  Result := inherited Next(TTransient(_PropriedadeDelimitado))
end;

function T_PropriedadeDelimitadoPorPropNomeList.Prior(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean; begin
  Result := inherited Prior(TTransient(_PropriedadeDelimitado))
end;

function T_PropriedadeDelimitadoPorPropNomeList.Get_PropriedadeDelimitado(I : Integer) : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(Objects[I])
end;

{ T_ImportaDelimitadoPropriedadesAssociation }

class function T_ImportaDelimitadoPropriedadesAssociation.MinConstraint : integer; begin
  Result := 1
end;

class function T_ImportaDelimitadoPropriedadesAssociation.GetKeyCode : pointer; begin
  Result := @T_PropriedadeDelimitado.PorElemento
end;

class function T_ImportaDelimitadoPropriedadesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_PropriedadeDelimitado
end;

procedure T_ImportaDelimitadoPropriedadesAssociation.Add(_PropriedadeDelimitado : T_PropriedadeDelimitado); begin
  inherited Add(TPrevalent(_PropriedadeDelimitado))
end;

procedure T_ImportaDelimitadoPropriedadesAssociation.Delete(_PropriedadeDelimitado : T_PropriedadeDelimitado); begin
  inherited Delete(TPrevalent(_PropriedadeDelimitado))
end;

function T_ImportaDelimitadoPropriedadesAssociation.Find(I : Integer) : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(inherited Find(I))
end;

function T_ImportaDelimitadoPropriedadesAssociation.First : T_PropriedadeDelimitado; begin
  SetDependencyLists;
  Result := T_PropriedadeDelimitado(inherited First)
end;

function T_ImportaDelimitadoPropriedadesAssociation.Last : T_PropriedadeDelimitado; begin
  SetDependencyLists;
  Result := T_PropriedadeDelimitado(inherited Last)
end;

function T_ImportaDelimitadoPropriedadesAssociation.Near(I : Integer) : T_PropriedadeDelimitado; begin
  SetDependencyLists;
  Result := T_PropriedadeDelimitado(inherited Near(I))
end;

function T_ImportaDelimitadoPropriedadesAssociation.Next(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_PropriedadeDelimitado))
end;

function T_ImportaDelimitadoPropriedadesAssociation.Prior(var _PropriedadeDelimitado : T_PropriedadeDelimitado) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_PropriedadeDelimitado))
end;

function T_ImportaDelimitadoPropriedadesAssociation.Get_PropriedadeDelimitado(I : Integer) : T_PropriedadeDelimitado; begin
  Result := T_PropriedadeDelimitado(Objects[I])
end;

function T_PropriedadePosicional.GetPosicao : Integer; begin Result := T_PropriedadePosicional(Prevalence.GetNewImage(Self, 7))._Posicao end;

procedure T_PropriedadePosicional.SetPosicao(Value : Integer); begin
  if Prevalence.IsInRecoverSnapShot then _Posicao := Value else begin
    if (_Posicao = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PropriedadePosicional(NewImage)._Posicao <> Value then begin
        T_PropriedadePosicional(NewImage)._Posicao := Value;
        UpdateLog(7, NewImage, Stream)
      end;
    end;
end;

function T_PropriedadePosicional.GetQuantidadePosicoes : Integer; begin Result := T_PropriedadePosicional(Prevalence.GetNewImage(Self, 8))._QuantidadePosicoes end;

procedure T_PropriedadePosicional.SetQuantidadePosicoes(Value : Integer); begin
  if Prevalence.IsInRecoverSnapShot then _QuantidadePosicoes := Value else begin
    if (_QuantidadePosicoes = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PropriedadePosicional(NewImage)._QuantidadePosicoes <> Value then begin
        T_PropriedadePosicional(NewImage)._QuantidadePosicoes := Value;
        UpdateLog(8, NewImage, Stream)
      end;
    end;
end;

function T_PropriedadePosicional.GetImportaPosicional : T_ImportaPosicional; begin Result := T_PropriedadePosicional(Prevalence.GetNewImage(Self, 9))._ImportaPosicional end;

procedure T_PropriedadePosicional.SetImportaPosicional(Value : T_ImportaPosicional); begin
  if Prevalence.IsInRecover then _ImportaPosicional := Value else begin
    if (_ImportaPosicional = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PropriedadePosicional(NewImage)._ImportaPosicional <> Value then begin
        T_PropriedadePosicional(NewImage)._ImportaPosicional := Value;
        UpdateLog(9, NewImage, Stream)
      end;
  end;
end;

function T_PropriedadePosicional.CheckPosicao(var Message : String) : Boolean; begin
  Result := Posicao > 0;
  if Result then
    Message := ''
  else
    Message := 'Posição deve ser maior que zero'
end;

function T_PropriedadePosicional.CheckQuantidadePosicoes(var Message : String) : Boolean; begin
  Result := QuantidadePosicoes > 0;
  if Result then
    Message := ''
  else
    Message := 'A Quantidade de posições deve ser maior que zero'
end;

{ T_PropriedadePosicionalList }

class function T_PropriedadePosicionalList.GetObjectClass : TTransientClass; begin
  Result := T_PropriedadePosicional
end;

procedure T_PropriedadePosicionalList.Add(_PropriedadePosicional : T_PropriedadePosicional); begin
  inherited Add(TPrevalent(_PropriedadePosicional))
end;

procedure T_PropriedadePosicionalList.Delete(_PropriedadePosicional : T_PropriedadePosicional); begin
  inherited Delete(TPrevalent(_PropriedadePosicional))
end;

function T_PropriedadePosicionalList.Find(I : Integer) : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(inherited Find(I))
end;

function T_PropriedadePosicionalList.First : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(inherited First)
end;

function T_PropriedadePosicionalList.Last : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(inherited Last)
end;

function T_PropriedadePosicionalList.Near(I : Integer) : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(inherited Near(I))
end;

function T_PropriedadePosicionalList.Next(var _PropriedadePosicional : T_PropriedadePosicional) : boolean; begin
  Result := inherited Next(TTransient(_PropriedadePosicional))
end;

function T_PropriedadePosicionalList.Prior(var _PropriedadePosicional : T_PropriedadePosicional) : boolean; begin
  Result := inherited Prior(TTransient(_PropriedadePosicional))
end;

function T_PropriedadePosicionalList.Get_PropriedadePosicional(I : Integer) : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(Objects[I])
end;

{ T_PropriedadePosicionalPorElementoList }

class function T_PropriedadePosicionalPorElementoList.GetKeyCode : pointer; begin
  Result := @T_PropriedadePosicional.PorElemento
end;

class function T_PropriedadePosicionalPorElementoList.GetObjectClass : TTransientClass; begin
  Result := T_PropriedadePosicional
end;

procedure T_PropriedadePosicionalPorElementoList.Add(_PropriedadePosicional : T_PropriedadePosicional); begin
  inherited Add(TPrevalent(_PropriedadePosicional))
end;

procedure T_PropriedadePosicionalPorElementoList.Delete(_PropriedadePosicional : T_PropriedadePosicional); begin
  inherited Delete(TPrevalent(_PropriedadePosicional))
end;

function T_PropriedadePosicionalPorElementoList.Find(I : Integer) : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(inherited Find(I))
end;

function T_PropriedadePosicionalPorElementoList.First : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(inherited First)
end;

function T_PropriedadePosicionalPorElementoList.Last : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(inherited Last)
end;

function T_PropriedadePosicionalPorElementoList.Near(I : Integer) : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(inherited Near(I))
end;

function T_PropriedadePosicionalPorElementoList.Next(var _PropriedadePosicional : T_PropriedadePosicional) : boolean; begin
  Result := inherited Next(TTransient(_PropriedadePosicional))
end;

function T_PropriedadePosicionalPorElementoList.Prior(var _PropriedadePosicional : T_PropriedadePosicional) : boolean; begin
  Result := inherited Prior(TTransient(_PropriedadePosicional))
end;

function T_PropriedadePosicionalPorElementoList.Get_PropriedadePosicional(I : Integer) : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(Objects[I])
end;

{ T_PropriedadePosicionalPorPropNomeList }

class function T_PropriedadePosicionalPorPropNomeList.GetKeyCode : pointer; begin
  Result := @T_PropriedadePosicional.PorPropNome
end;

class function T_PropriedadePosicionalPorPropNomeList.GetListType : TListType; begin
  Result := ltString
end;

class function T_PropriedadePosicionalPorPropNomeList.GetObjectClass : TTransientClass; begin
  Result := T_PropriedadePosicional
end;

procedure T_PropriedadePosicionalPorPropNomeList.Add(_PropriedadePosicional : T_PropriedadePosicional); begin
  inherited Add(TPrevalent(_PropriedadePosicional))
end;

procedure T_PropriedadePosicionalPorPropNomeList.Delete(_PropriedadePosicional : T_PropriedadePosicional); begin
  inherited Delete(TPrevalent(_PropriedadePosicional))
end;

function T_PropriedadePosicionalPorPropNomeList.Find(S : String) : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(inherited Find(S))
end;

function T_PropriedadePosicionalPorPropNomeList.First : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(inherited First)
end;

function T_PropriedadePosicionalPorPropNomeList.Last : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(inherited Last)
end;

function T_PropriedadePosicionalPorPropNomeList.Near(S : String) : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(inherited Near(S))
end;

function T_PropriedadePosicionalPorPropNomeList.Next(var _PropriedadePosicional : T_PropriedadePosicional) : boolean; begin
  Result := inherited Next(TTransient(_PropriedadePosicional))
end;

function T_PropriedadePosicionalPorPropNomeList.Prior(var _PropriedadePosicional : T_PropriedadePosicional) : boolean; begin
  Result := inherited Prior(TTransient(_PropriedadePosicional))
end;

function T_PropriedadePosicionalPorPropNomeList.Get_PropriedadePosicional(I : Integer) : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(Objects[I])
end;

{ T_ImportaPosicionalPropriedadesAssociation }

class function T_ImportaPosicionalPropriedadesAssociation.MinConstraint : integer; begin
  Result := 1
end;

class function T_ImportaPosicionalPropriedadesAssociation.GetKeyCode : pointer; begin
  Result := @T_PropriedadePosicional.PorElemento
end;

class function T_ImportaPosicionalPropriedadesAssociation.GetObjectClass : TTransientClass; begin
  Result := T_PropriedadePosicional
end;

procedure T_ImportaPosicionalPropriedadesAssociation.Add(_PropriedadePosicional : T_PropriedadePosicional); begin
  inherited Add(TPrevalent(_PropriedadePosicional))
end;

procedure T_ImportaPosicionalPropriedadesAssociation.Delete(_PropriedadePosicional : T_PropriedadePosicional); begin
  inherited Delete(TPrevalent(_PropriedadePosicional))
end;

function T_ImportaPosicionalPropriedadesAssociation.Find(I : Integer) : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(inherited Find(I))
end;

function T_ImportaPosicionalPropriedadesAssociation.First : T_PropriedadePosicional; begin
  SetDependencyLists;
  Result := T_PropriedadePosicional(inherited First)
end;

function T_ImportaPosicionalPropriedadesAssociation.Last : T_PropriedadePosicional; begin
  SetDependencyLists;
  Result := T_PropriedadePosicional(inherited Last)
end;

function T_ImportaPosicionalPropriedadesAssociation.Near(I : Integer) : T_PropriedadePosicional; begin
  SetDependencyLists;
  Result := T_PropriedadePosicional(inherited Near(I))
end;

function T_ImportaPosicionalPropriedadesAssociation.Next(var _PropriedadePosicional : T_PropriedadePosicional) : boolean; begin
  SetDependencyLists;
  Result := inherited Next(TTransient(_PropriedadePosicional))
end;

function T_ImportaPosicionalPropriedadesAssociation.Prior(var _PropriedadePosicional : T_PropriedadePosicional) : boolean; begin
  SetDependencyLists;
  Result := inherited Prior(TTransient(_PropriedadePosicional))
end;

function T_ImportaPosicionalPropriedadesAssociation.Get_PropriedadePosicional(I : Integer) : T_PropriedadePosicional; begin
  Result := T_PropriedadePosicional(Objects[I])
end;

{ T_Task }

procedure T_Task.New; begin
  inherited;
  if Prevalence.IsInRecover then exit;
  try _Active := True except end;
  try _PeriodicityUnit := _tpuDias except end;
  try _Start := Now except end;
end;

function T_Task.GetActive : Boolean; begin Result := T_Task(Prevalence.GetNewImage(Self, 8))._Active end;

procedure T_Task.SetActive(Value : Boolean); begin
  if Prevalence.IsInRecoverSnapShot then _Active := Value else begin
    if (_Active = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Task(NewImage)._Active <> Value then begin
        T_Task(NewImage)._Active := Value;
        UpdateLog(8, NewImage, Stream)
      end;
    end;
end;

function T_Task.GetPeriodicity : Word; begin Result := T_Task(Prevalence.GetNewImage(Self, 9))._Periodicity end;

procedure T_Task.SetPeriodicity(Value : Word); begin
  if Prevalence.IsInRecoverSnapShot then _Periodicity := Value else begin
    if (_Periodicity = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Task(NewImage)._Periodicity <> Value then begin
        T_Task(NewImage)._Periodicity := Value;
        UpdateLog(9, NewImage, Stream)
      end;
    end;
end;

function T_Task.GetPeriodicityUnit : T_TaskPeriodicityUnit; begin Result := T_Task(Prevalence.GetNewImage(Self, 10))._PeriodicityUnit end;

procedure T_Task.SetPeriodicityUnit(Value : T_TaskPeriodicityUnit); begin
  if Prevalence.IsInRecoverSnapShot then _PeriodicityUnit := Value else begin
    if (_PeriodicityUnit = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Task(NewImage)._PeriodicityUnit <> Value then begin
        T_Task(NewImage)._PeriodicityUnit := Value;
        UpdateLog(10, NewImage, Stream)
      end;
    end;
end;

function T_Task.GetStart : DateTime; begin Result := T_Task(Prevalence.GetNewImage(Self, 11))._Start end;

procedure T_Task.SetStart(Value : DateTime); begin
  if Prevalence.IsInRecoverSnapShot then _Start := Value else begin
    if (_Start = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Task(NewImage)._Start <> Value then begin
        T_Task(NewImage)._Start := Value;
        UpdateLog(11, NewImage, Stream)
      end;
    end;
end;

function T_Task.GetRunUntil : DateTime; begin Result := T_Task(Prevalence.GetNewImage(Self, 12))._RunUntil end;

procedure T_Task.SetRunUntil(Value : DateTime); begin
  if Prevalence.IsInRecoverSnapShot then _RunUntil := Value else begin
    if (_RunUntil = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Task(NewImage)._RunUntil <> Value then begin
        T_Task(NewImage)._RunUntil := Value;
        UpdateLog(12, NewImage, Stream)
      end;
    end;
end;

function T_Task.GetLastRan : DateTime; begin Result := T_Task(Prevalence.GetNewImage(Self, 13))._LastRan end;

procedure T_Task.SetLastRan(Value : DateTime); begin
  if Prevalence.IsInRecoverSnapShot then _LastRan := Value else begin
    if (_LastRan = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_Task(NewImage)._LastRan <> Value then begin
        T_Task(NewImage)._LastRan := Value;
        UpdateLog(13, NewImage, Stream)
      end;
    end;
end;

function T_Task.GetNextStart : DateTime; begin
  try Result := CalcNextStart except Result := 0 end;
end;

// Cyclomatic Complexity: 1, Baixo
function T_Task.ByNextStart : DateTime; begin
  Result := NextStart
end;

// Cyclomatic Complexity: 1, Baixo
class procedure T_Task.Scheduler;
type
  TContext = record
    This : T_Task;
  end;
var
  Context : TContext;
  StateMachine : TStateMachine;

procedure Refresh(var Context : TContext); begin
//*  with Context, This do Browser.Refresh;
end;

procedure AgendamentoDeTarefas(var Context : TContext); begin
  with Context, This do begin
    Browser.Edit(_TaskByNextStartList, '', 'Agendamento de tarefas', '', StateMachine.FixButtons([bbCancel, bbFinish]));
  end;
end;

begin
  fillchar(Context, sizeof(Context), 0);
  StateMachine := TStateMachine.Create('_Workflow', '_Task', 'Scheduler', [1, 1]);
  with StateMachine do begin
    SetState(0, 'Refresh', @Refresh);
    SetState(1, 'AgendamentoDeTarefas', @AgendamentoDeTarefas, [bbCancel, bbFinish]);
    SetTransition(0, 0, 'Default', nil, 1);
    SetTransition(1, 0, 'Default', nil, -1);
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

// Cyclomatic Complexity: 10, Baixo
function T_Task.CalcNextStart : TDateTime; begin
  Result := LastRan;
  if Start > Result then
    Result := Start;
  if LastRan <> 0 then
    case PeriodicityUnit of
      _tpuMinutos : Result := IncMinute(Result, Periodicity);
      _tpuHoras   : Result := IncHour(Result, Periodicity);
      _tpuDias    : Result := IncDay(Result, Periodicity);
      _tpuMeses   : Result := IncMonth(Result, Periodicity);
      _tpuAnos    : Result := IncYear(Result, Periodicity);
    end;
  if (RunUntil > 0) and (Result > RunUntil) then
    Result := 0; // Empty date: should not run anymore
end;

// Cyclomatic Complexity: 4, Baixo
class procedure T_Task.NewTask;
type
  TContext = record
    This : T_Task;
    Task : T_Task;
    RunObj : TPrevalent;
    List : TObjectList;
  end;
var
  Context : TContext;
  StateMachine : TStateMachine;

procedure Initialize(var Context : TContext); begin
  with Context, This do Task := T_Task.Create;
end;

procedure ChooseObject(var Context : TContext); begin
  with Context, This do begin
    //;
    if (List = nil) or (List.Count = 0) then Browser.Error('Não existe nenhum item existente para o agendamento desta tarefa. Operação cancelada.');
    ;
    RunObj := TPrevalent(Browser.Choose(List, '', 'Escolha o objeto no qual será executada a tarefa agendada'));
    ;
  end;
end;

procedure CriarNovaTarefaAgendada(var Context : TContext); begin
  with Context, This do begin
    Browser.Edit(Task, 'Method,Parameters,LogKind,|Periodicidade|,Periodicity,PeriodicityUnit,' + 
'|Período em que o agendamento estará ativo|,Start,RunUntil', 'Criar nova tarefa agendada', 'Entre com os dados para o agendamento da tarefa.', StateMachine.FixButtons([bbCancel, bbBack, bbNext]));
  end;
end;

procedure Success(var Context : TContext); begin
  with Context, This do Browser.ShowMessage('Tarefa agendada com sucesso.');
end;

procedure SetRunObject(var Context : TContext); begin
  with Context, This do Task.RunObjectID := RunObj.ID;
end;

function EmptyObject(var Context : TContext) : boolean; begin
  with Context, This do begin
    Result := RunObj = nil;
    if Result then Browser.MessageDlg('Favor escolher um item, para que a tarefa agendada seja executada neste item.');
  end;
end;

function WithoutPeriodicity(var Context : TContext) : boolean; begin
  with Context, This do begin
    Result := Task.Periodicity <= 0;
    if Result then Browser.MessageDlg('Deve ser fornecida a periodicidade de execução da tarefa.', mtError);
  end;
end;

function ObjectMethod(var Context : TContext) : boolean; begin
  with Context, This do begin
    Result := Prevalence.Metadata('T' + Task.Method.Classe.Nome).MethodByName(Task.Method.Nome).Kind in [mkProcedure, mkFunction];
    if Result then List := Prevalence.PrevalentLists('T' + Task.Method.Classe.Nome + 'List');
  end;
end;

begin
  fillchar(Context, sizeof(Context), 0);
  StateMachine := TStateMachine.Create('_Workflow', '_Task', 'NewTask', [1, 2, 2, 2, 1, 1]);
  with StateMachine do begin
    SetState(0, 'Initialize', @Initialize);
    SetState(1, 'ChooseObject', @ChooseObject);
    SetState(2, 'CriarNovaTarefaAgendada', @CriarNovaTarefaAgendada, [bbCancel, bbBack, bbNext]);
    SetState(3, 'CheckMethodKind', nil);
    SetState(4, 'Success', @Success);
    SetState(5, 'SetRunObject', @SetRunObject);
    SetTransition(0, 0, 'Default', nil, 2);
    SetTransition(1, 0, 'EmptyObject', @EmptyObject, 1);
    SetTransition(1, 1, 'Default', nil, 5);
    SetTransition(2, 0, 'WithoutPeriodicity', @WithoutPeriodicity, 2);
    SetTransition(2, 1, 'Default', nil, 3);
    SetTransition(3, 0, 'ObjectMethod', @ObjectMethod, 1);
    SetTransition(3, 1, 'Default', nil, 4);
    SetTransition(4, 0, 'Default', nil, -1);
    SetTransition(5, 0, 'Default', nil, 4);
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

{ T_TaskList }

class function T_TaskList.GetObjectClass : TTransientClass; begin
  Result := T_Task
end;

procedure T_TaskList.Add(_Task : T_Task); begin
  inherited Add(TPrevalent(_Task))
end;

procedure T_TaskList.Delete(_Task : T_Task); begin
  inherited Delete(TPrevalent(_Task))
end;

function T_TaskList.Find(I : Integer) : T_Task; begin
  Result := T_Task(inherited Find(I))
end;

function T_TaskList.First : T_Task; begin
  Result := T_Task(inherited First)
end;

function T_TaskList.Last : T_Task; begin
  Result := T_Task(inherited Last)
end;

function T_TaskList.Near(I : Integer) : T_Task; begin
  Result := T_Task(inherited Near(I))
end;

function T_TaskList.Next(var _Task : T_Task) : boolean; begin
  Result := inherited Next(TTransient(_Task))
end;

function T_TaskList.Prior(var _Task : T_Task) : boolean; begin
  Result := inherited Prior(TTransient(_Task))
end;

function T_TaskList.Get_Task(I : Integer) : T_Task; begin
  Result := T_Task(Objects[I])
end;

{ T_TaskByNextStartList }

class function T_TaskByNextStartList.GetKeyCode : pointer; begin
  Result := @T_Task.ByNextStart
end;

class function T_TaskByNextStartList.GetListType : TListType; begin
  Result := ltDateTime
end;

class function T_TaskByNextStartList.GetObjectClass : TTransientClass; begin
  Result := T_Task
end;

procedure T_TaskByNextStartList.Add(_Task : T_Task); begin
  inherited Add(TPrevalent(_Task))
end;

procedure T_TaskByNextStartList.Delete(_Task : T_Task); begin
  inherited Delete(TPrevalent(_Task))
end;

function T_TaskByNextStartList.Find(D : DateTime) : T_Task; begin
  Result := T_Task(inherited Find(D))
end;

function T_TaskByNextStartList.First : T_Task; begin
  Result := T_Task(inherited First)
end;

function T_TaskByNextStartList.Last : T_Task; begin
  Result := T_Task(inherited Last)
end;

function T_TaskByNextStartList.Near(D : DateTime) : T_Task; begin
  Result := T_Task(inherited Near(D))
end;

function T_TaskByNextStartList.Next(var _Task : T_Task) : boolean; begin
  Result := inherited Next(TTransient(_Task))
end;

function T_TaskByNextStartList.Prior(var _Task : T_Task) : boolean; begin
  Result := inherited Prior(TTransient(_Task))
end;

function T_TaskByNextStartList.Get_Task(I : Integer) : T_Task; begin
  Result := T_Task(Objects[I])
end;

procedure T_ImportaDelimitado.New; begin
  inherited;
  _Propriedades := T_ImportaDelimitadoPropriedadesAssociation.Create(Self, 'Propriedades');
  if Prevalence.IsInRecover then exit;
  try _Delimitador := ',' except end;
end;

procedure T_ImportaDelimitado.InternalFree; begin
  if _Propriedades <> nil then _Propriedades.InternalFree;
  inherited;
end;

function T_ImportaDelimitado.GetDelimitador : String; begin Result := T_ImportaDelimitado(Prevalence.GetNewImage(Self, 13))._Delimitador end;

procedure T_ImportaDelimitado.SetDelimitador(Value : String); begin
  if Prevalence.IsInRecoverSnapShot then _Delimitador := Value else begin
    if (_Delimitador = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_ImportaDelimitado(NewImage)._Delimitador <> Value then begin
        T_ImportaDelimitado(NewImage)._Delimitador := Value;
        UpdateLog(13, NewImage, Stream)
      end;
    end;
end;

function T_ImportaDelimitado.CheckDelimitador(var Message : String) : Boolean; begin
  Result := (Delimitador <> '[') and (Delimitador <> ']');
  if Result then
    Message := ''
  else
    Message := 'Delimitador Inválido. Deve ser diferente de "[" e "]"'
end;

{ T_ImportaDelimitadoList }

class function T_ImportaDelimitadoList.GetObjectClass : TTransientClass; begin
  Result := T_ImportaDelimitado
end;

procedure T_ImportaDelimitadoList.Add(_ImportaDelimitado : T_ImportaDelimitado); begin
  inherited Add(TPrevalent(_ImportaDelimitado))
end;

procedure T_ImportaDelimitadoList.Delete(_ImportaDelimitado : T_ImportaDelimitado); begin
  inherited Delete(TPrevalent(_ImportaDelimitado))
end;

function T_ImportaDelimitadoList.Find(I : Integer) : T_ImportaDelimitado; begin
  Result := T_ImportaDelimitado(inherited Find(I))
end;

function T_ImportaDelimitadoList.First : T_ImportaDelimitado; begin
  Result := T_ImportaDelimitado(inherited First)
end;

function T_ImportaDelimitadoList.Last : T_ImportaDelimitado; begin
  Result := T_ImportaDelimitado(inherited Last)
end;

function T_ImportaDelimitadoList.Near(I : Integer) : T_ImportaDelimitado; begin
  Result := T_ImportaDelimitado(inherited Near(I))
end;

function T_ImportaDelimitadoList.Next(var _ImportaDelimitado : T_ImportaDelimitado) : boolean; begin
  Result := inherited Next(TTransient(_ImportaDelimitado))
end;

function T_ImportaDelimitadoList.Prior(var _ImportaDelimitado : T_ImportaDelimitado) : boolean; begin
  Result := inherited Prior(TTransient(_ImportaDelimitado))
end;

function T_ImportaDelimitadoList.Get_ImportaDelimitado(I : Integer) : T_ImportaDelimitado; begin
  Result := T_ImportaDelimitado(Objects[I])
end;

{ T_ImportaDelimitadoPorNomeList }

class function T_ImportaDelimitadoPorNomeList.GetKeyCode : pointer; begin
  Result := @T_ImportaDelimitado.PorNome
end;

class function T_ImportaDelimitadoPorNomeList.GetListType : TListType; begin
  Result := ltstring
end;

class function T_ImportaDelimitadoPorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_ImportaDelimitado
end;

procedure T_ImportaDelimitadoPorNomeList.Add(_ImportaDelimitado : T_ImportaDelimitado); begin
  inherited Add(TPrevalent(_ImportaDelimitado))
end;

procedure T_ImportaDelimitadoPorNomeList.Delete(_ImportaDelimitado : T_ImportaDelimitado); begin
  inherited Delete(TPrevalent(_ImportaDelimitado))
end;

function T_ImportaDelimitadoPorNomeList.Find(s : string) : T_ImportaDelimitado; begin
  Result := T_ImportaDelimitado(inherited Find(s))
end;

function T_ImportaDelimitadoPorNomeList.First : T_ImportaDelimitado; begin
  Result := T_ImportaDelimitado(inherited First)
end;

function T_ImportaDelimitadoPorNomeList.Last : T_ImportaDelimitado; begin
  Result := T_ImportaDelimitado(inherited Last)
end;

function T_ImportaDelimitadoPorNomeList.Near(s : string) : T_ImportaDelimitado; begin
  Result := T_ImportaDelimitado(inherited Near(s))
end;

function T_ImportaDelimitadoPorNomeList.Next(var _ImportaDelimitado : T_ImportaDelimitado) : boolean; begin
  Result := inherited Next(TTransient(_ImportaDelimitado))
end;

function T_ImportaDelimitadoPorNomeList.Prior(var _ImportaDelimitado : T_ImportaDelimitado) : boolean; begin
  Result := inherited Prior(TTransient(_ImportaDelimitado))
end;

function T_ImportaDelimitadoPorNomeList.Get_ImportaDelimitado(I : Integer) : T_ImportaDelimitado; begin
  Result := T_ImportaDelimitado(Objects[I])
end;

procedure T_ImportaPosicional.New; begin
  inherited;
  _Propriedades := T_ImportaPosicionalPropriedadesAssociation.Create(Self, 'Propriedades');
end;

procedure T_ImportaPosicional.InternalFree; begin
  if _Propriedades <> nil then _Propriedades.InternalFree;
  inherited;
end;

{ T_ImportaPosicionalList }

class function T_ImportaPosicionalList.GetObjectClass : TTransientClass; begin
  Result := T_ImportaPosicional
end;

procedure T_ImportaPosicionalList.Add(_ImportaPosicional : T_ImportaPosicional); begin
  inherited Add(TPrevalent(_ImportaPosicional))
end;

procedure T_ImportaPosicionalList.Delete(_ImportaPosicional : T_ImportaPosicional); begin
  inherited Delete(TPrevalent(_ImportaPosicional))
end;

function T_ImportaPosicionalList.Find(I : Integer) : T_ImportaPosicional; begin
  Result := T_ImportaPosicional(inherited Find(I))
end;

function T_ImportaPosicionalList.First : T_ImportaPosicional; begin
  Result := T_ImportaPosicional(inherited First)
end;

function T_ImportaPosicionalList.Last : T_ImportaPosicional; begin
  Result := T_ImportaPosicional(inherited Last)
end;

function T_ImportaPosicionalList.Near(I : Integer) : T_ImportaPosicional; begin
  Result := T_ImportaPosicional(inherited Near(I))
end;

function T_ImportaPosicionalList.Next(var _ImportaPosicional : T_ImportaPosicional) : boolean; begin
  Result := inherited Next(TTransient(_ImportaPosicional))
end;

function T_ImportaPosicionalList.Prior(var _ImportaPosicional : T_ImportaPosicional) : boolean; begin
  Result := inherited Prior(TTransient(_ImportaPosicional))
end;

function T_ImportaPosicionalList.Get_ImportaPosicional(I : Integer) : T_ImportaPosicional; begin
  Result := T_ImportaPosicional(Objects[I])
end;

{ T_ImportaPosicionalPorNomeList }

class function T_ImportaPosicionalPorNomeList.GetKeyCode : pointer; begin
  Result := @T_ImportaPosicional.PorNome
end;

class function T_ImportaPosicionalPorNomeList.GetListType : TListType; begin
  Result := ltstring
end;

class function T_ImportaPosicionalPorNomeList.GetObjectClass : TTransientClass; begin
  Result := T_ImportaPosicional
end;

procedure T_ImportaPosicionalPorNomeList.Add(_ImportaPosicional : T_ImportaPosicional); begin
  inherited Add(TPrevalent(_ImportaPosicional))
end;

procedure T_ImportaPosicionalPorNomeList.Delete(_ImportaPosicional : T_ImportaPosicional); begin
  inherited Delete(TPrevalent(_ImportaPosicional))
end;

function T_ImportaPosicionalPorNomeList.Find(s : string) : T_ImportaPosicional; begin
  Result := T_ImportaPosicional(inherited Find(s))
end;

function T_ImportaPosicionalPorNomeList.First : T_ImportaPosicional; begin
  Result := T_ImportaPosicional(inherited First)
end;

function T_ImportaPosicionalPorNomeList.Last : T_ImportaPosicional; begin
  Result := T_ImportaPosicional(inherited Last)
end;

function T_ImportaPosicionalPorNomeList.Near(s : string) : T_ImportaPosicional; begin
  Result := T_ImportaPosicional(inherited Near(s))
end;

function T_ImportaPosicionalPorNomeList.Next(var _ImportaPosicional : T_ImportaPosicional) : boolean; begin
  Result := inherited Next(TTransient(_ImportaPosicional))
end;

function T_ImportaPosicionalPorNomeList.Prior(var _ImportaPosicional : T_ImportaPosicional) : boolean; begin
  Result := inherited Prior(TTransient(_ImportaPosicional))
end;

function T_ImportaPosicionalPorNomeList.Get_ImportaPosicional(I : Integer) : T_ImportaPosicional; begin
  Result := T_ImportaPosicional(Objects[I])
end;

function T_PendencyTimeoutTask.GetPendency : T_Pendency; begin Result := T_PendencyTimeoutTask(Prevalence.GetNewImage(Self, 15))._Pendency end;

procedure T_PendencyTimeoutTask.SetPendency(Value : T_Pendency); begin
  if Prevalence.IsInRecover then _Pendency := Value else begin
    if (_Pendency = Value) and (Prevalence.FindInUpdate(Self) = nil) then exit;
    with Prevalence.GetImages(Self) do
      if T_PendencyTimeoutTask(NewImage)._Pendency <> Value then begin
        T_PendencyTimeoutTask(NewImage)._Pendency := Value;
        UpdateLog(15, NewImage, Stream)
      end;
  end;
end;

{ T_PendencyTimeoutTaskList }

class function T_PendencyTimeoutTaskList.GetObjectClass : TTransientClass; begin
  Result := T_PendencyTimeoutTask
end;

procedure T_PendencyTimeoutTaskList.Add(_PendencyTimeoutTask : T_PendencyTimeoutTask); begin
  inherited Add(TPrevalent(_PendencyTimeoutTask))
end;

procedure T_PendencyTimeoutTaskList.Delete(_PendencyTimeoutTask : T_PendencyTimeoutTask); begin
  inherited Delete(TPrevalent(_PendencyTimeoutTask))
end;

function T_PendencyTimeoutTaskList.Find(I : Integer) : T_PendencyTimeoutTask; begin
  Result := T_PendencyTimeoutTask(inherited Find(I))
end;

function T_PendencyTimeoutTaskList.First : T_PendencyTimeoutTask; begin
  Result := T_PendencyTimeoutTask(inherited First)
end;

function T_PendencyTimeoutTaskList.Last : T_PendencyTimeoutTask; begin
  Result := T_PendencyTimeoutTask(inherited Last)
end;

function T_PendencyTimeoutTaskList.Near(I : Integer) : T_PendencyTimeoutTask; begin
  Result := T_PendencyTimeoutTask(inherited Near(I))
end;

function T_PendencyTimeoutTaskList.Next(var _PendencyTimeoutTask : T_PendencyTimeoutTask) : boolean; begin
  Result := inherited Next(TTransient(_PendencyTimeoutTask))
end;

function T_PendencyTimeoutTaskList.Prior(var _PendencyTimeoutTask : T_PendencyTimeoutTask) : boolean; begin
  Result := inherited Prior(TTransient(_PendencyTimeoutTask))
end;

function T_PendencyTimeoutTaskList.Get_PendencyTimeoutTask(I : Integer) : T_PendencyTimeoutTask; begin
  Result := T_PendencyTimeoutTask(Objects[I])
end;

{ T_PendencyTimeoutTaskByNextStartList }

class function T_PendencyTimeoutTaskByNextStartList.GetKeyCode : pointer; begin
  Result := @T_PendencyTimeoutTask.ByNextStart
end;

class function T_PendencyTimeoutTaskByNextStartList.GetListType : TListType; begin
  Result := ltDateTime
end;

class function T_PendencyTimeoutTaskByNextStartList.GetObjectClass : TTransientClass; begin
  Result := T_PendencyTimeoutTask
end;

procedure T_PendencyTimeoutTaskByNextStartList.Add(_PendencyTimeoutTask : T_PendencyTimeoutTask); begin
  inherited Add(TPrevalent(_PendencyTimeoutTask))
end;

procedure T_PendencyTimeoutTaskByNextStartList.Delete(_PendencyTimeoutTask : T_PendencyTimeoutTask); begin
  inherited Delete(TPrevalent(_PendencyTimeoutTask))
end;

function T_PendencyTimeoutTaskByNextStartList.Find(D : DateTime) : T_PendencyTimeoutTask; begin
  Result := T_PendencyTimeoutTask(inherited Find(D))
end;

function T_PendencyTimeoutTaskByNextStartList.First : T_PendencyTimeoutTask; begin
  Result := T_PendencyTimeoutTask(inherited First)
end;

function T_PendencyTimeoutTaskByNextStartList.Last : T_PendencyTimeoutTask; begin
  Result := T_PendencyTimeoutTask(inherited Last)
end;

function T_PendencyTimeoutTaskByNextStartList.Near(D : DateTime) : T_PendencyTimeoutTask; begin
  Result := T_PendencyTimeoutTask(inherited Near(D))
end;

function T_PendencyTimeoutTaskByNextStartList.Next(var _PendencyTimeoutTask : T_PendencyTimeoutTask) : boolean; begin
  Result := inherited Next(TTransient(_PendencyTimeoutTask))
end;

function T_PendencyTimeoutTaskByNextStartList.Prior(var _PendencyTimeoutTask : T_PendencyTimeoutTask) : boolean; begin
  Result := inherited Prior(TTransient(_PendencyTimeoutTask))
end;

function T_PendencyTimeoutTaskByNextStartList.Get_PendencyTimeoutTask(I : Integer) : T_PendencyTimeoutTask; begin
  Result := T_PendencyTimeoutTask(Objects[I])
end;

var
  QtdInits : integer;

initialization
  SetLength(RecoverInits, length(RecoverInits) + 1);
  RecoverInits[high(RecoverInits)] := InitepModel;
  QtdInits := length(Initializations);
  SetLength(Initializations, (QtdInits + 1) * 2);
  Initializations[QtdInits + 0] := @T_OcupacaoPackage.InicializaOcupacao;
  Initializations[QtdInits + 1] := @T_Classe.AtualizaClasses;
end.
