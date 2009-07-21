{$I epDirectives.inc}

unit epCommon;

interface

uses TypInfo;

type
  {$IFNDEF FPC}
  PtrInt  = integer;
  PtrUInt = cardinal;
  {$ENDIF}
  TUMLDiagramKind = (udkClass, udkSTM);
  T_Operation = (_opAdd, _opUpdate, _opDelete, _opAddAssociation, _opDeleteAssociation, _opLogin, _opBeginTransaction, _opAll);

  TConstraint  = (AUTOINC, READONLY, NOTNULL, CHECK, COMPOSITE, SHARED, ENABLED, VISIBLE, ASSOCIATIONCONSTRAINT);
  TConstraints = set of TConstraint;

  T_HasPropString  = (_hpNONE, _hpHAVE, _hpHAVENT);

  T_Stereotype = (_stSTATEMACHINE, _stWORKFLOW, _stWIZARD, _stLIFECYCLE, _stINDEX, _stVIEW);

  //Segurança
  T_PropPermission     = (_ppModify, _ppShow, _ppHide);
  T_ClassPermission    = (_cpShow, _cpInsert, _cpModify, _cpDelete, _cpHide);
  T_ClassPermissionSet = set of T_ClassPermission;
  T_ViewPermission     = (_vpShow, _vpModify, _vpDelete);
  T_ViewPermissionSet  = set of T_ViewPermission;

  TDisableClassPermission = (cpInsert, cpModify, cpDelete);
  TDisableClassPermissions = set of TDisableClassPermission;

  T_ViewFormat         = (_vfGrid, _vfCard);

  T_PropKind           = (_pkProperty, _pkReference, _pkAssociation);
  T_ObjectModified     = set of (_omData, _omInformation, _omIdentification);
  //Auditoria
  T_Aud_ArquivoStatus = (_casAtivo, _casArquivado);
  T_Aud_ArquivoTipo = (_catSnapshot,_catLog, _catAudit);
  T_MapeamentoAuditLog = packed record
    Operation : T_Operation;
    audPos,
    logPos  : integer;
  end;
  P_MapeamentoAuditLog = ^T_MapeamentoAuditLog;

  TObjectProcedure = procedure of object;

  THeaderEnums = packed record
    EnumValor : integer;
    EnumName : string;
  end;

  THeaderProps = packed record
    PropIndex : word;
    PropName : string;
    PropAlias : string;
    PropMask : string;
    PropKind : TTypeKind;
    PropSubKind : word;
    Enums : array of THeaderEnums;
  end;
  THeaderRecord = packed record
    PackageName : string;
    ListIndex : word;
    ClassName : string;
    ClassAlias : string;
    Props : array of THeaderProps;
  end;
  THeaderData = array of THeaderRecord;
  THeaderFile = packed record
    Version : integer;
    Date    : double;
    HeaderData : THeaderData;
  end;
  TEvolveRecord = packed record
    ClassName : string;
    ClassID   : integer;
    Props : array of packed record
      Name    : string;
      ID      : integer;
      Kind    : TTypeKind;
      SubKind : word;
      PropInfo: PPropInfo;
    end;
    Package   : string;
    Methods : array of packed record
      Name    : string;
      ID      : integer;
    end;
  end;
  TEvolveData = array of TEvolveRecord;

const
  NONELIST = 65535;
  ENDUPDATE = NONELIST-1; // Indica final do update
  _StrAssociation            = 'Association';
  _StrNodeIconDelimiter      = #1;

  ProxyProtocolVersion       = 55;
                                                 
  ProxyFirstObjectID         = -1;
  ProxyLastObjectID          = -2;

  strCacheDelimiter          = #255;
  strFilterIdDelimiter       = #254;
  strEmptyReference          = 'nenhum(a)';

  _MethodStartParamsDelimiter = '(';

type
(*  TProxyCommand    = (cmdGetListObjects     = 1,
                      cmdModifyObject       = 2,
                      cmdCount              = 3,
                      cmdCreateObject       = 4,
                      cmdDeleteObject       = 5,
                      cmdPackagedMetada     = 6,
                      cmdGetHint            = 7,
                      cmdGetObject          = 8,
                      cmdBeginTransaction   = 9,
                      cmdEndTransaction     = 10,
                      cmdRollBack           = 11,
                      cmdSetObjectProp      = 12,
                      cmdPostObject         = 13, // confirm updates made with cmdSetObjectProp

                      cmdCreateFilteredList = 70,
                      cmdFreeFilteredList   = 71,

                      cmdGetAssocConstraint = 77,

                      cmdAssocCount         = 101,
                      cmdAssocGetObjects    = 102,
                      cmdAssocAdd           = 103,
                      cmdAssocDelete        = 104,
                      cmdAssocGetAllIDs     = 105,
                      cmdAssocAddRemoveIDs  = 106,

                      cmdEvaluate           = 120, // Debugger commands
                      cmdModify,
                      cmdAddBreakpoint,
                      cmdAddWatchpoint,
                      cmdDeleteDebugpoint,
                      cmdDisableDebugpoint,
                      cmdEnableDebugpoint,
                      cmdAddWatch,
                      cmdDeleteWatch,
                      cmdStepInto,
                      cmdStepOver,
                      cmdPause,
                      cmdReset,
                      cmdRun,
                      cmdRunToCursor,
                      cmdBackToCursor,
                      cmdRunUntilReturn,
                      cmdShowCallStack,
                      cmdShowDebugpoints,
                      cmdShowLocals,
                      cmdShowModules,
                      cmdShowThreads,
                      cmdShowWatches,
                      cmdShowTrace,

                      cmdCreateObjWizard    = 170,
                      cmdEditObjWizard      = 171,
                      cmdStartWorkFlow      = 177,
                      cmdExecutePendency    = 178,
                      cmdGetPendencyObject  = 179,
                      cmdBroadcastDelegated = 180,

                      cmdMethodGetHint      = 207,

                      cmdGetBrowserTasks    = 249,
                      cmdSetProfile         = 250,
                      cmdConnect            = 251,
                      cmdSnapshot           = 252,
                      cmdGetBrowserClasses  = 253,
                      cmdAuthenticate       = 254);

  TBrowserCommand  = (bcShowMessage, bcMessageDlg, bcGetParams, bcWorkFlowEnd,
    bcNewPendency, bcEditObject, bcEditList, bcChoose, bcRefresh, bcServerKilled,
    bcEditDiagram, bcClearCache, bcExecuteBrowser, bcDisablePermissions,
    bcReport);
*)
  { Browser buttons }
  TBrowserButton   = (bbBack, bbNext, bbYes, bbNo, bbOk, bbExecute, bbFinish, bbCancel);
  TBrowserButtons  = set of TBrowserButton;

  TMsgDlgType    = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp);
  TMsgDlgButtons = set of TMsgDlgBtn;

  TBrowserEditFormat = (beGrid, beCard);

const
  BrowserButtonsWithoutDataSaving: TBrowserButtons = [bbBack, bbCancel];
  {$IFDEF FPC}
	KindFPC2Delphi : array[TTypeKind] of byte = (0, 1, 2, 3, 4, 6, 8, 5, 10, 10, 11, 12, 13, 14, 15, 7, 7, 9, 3, 16, 16, 17, 15);
	KindDelphi2FPC : array[TTypeKind] of byte = (0, 1, 2, 3, 4, 7, 5, 15, 6, 17, 9, 10, 11, 12, 13, 14, 19, 17, 21, 0, 0, 0, 0);
  {$ENDIF}
implementation

end.
