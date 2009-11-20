unit Grammar;

interface
 type p = procedure (s:array of char); pascal;
const
  // Productions id
  Start        = #128; ParIdentList = #129; IdentList    = #130; UsesClause   = #131; ExportsList = #132;
  DeclSection  = #133; VarDecl      = #134; VarList      = #135; VarInit      = #136; Type_        = #137;
  EnumList     = #138; CompoundStmt = #139; Statement    = #140; StmtList     = #141; Expression   = #142;
  ToOrDownto   = #143; WithList     = #144; IntSection   = #145; ImplSection  = #146; InitSection  = #147;
  TypeDecl     = #148; StringLength = #149; ArrayDim     = #150; ClassDecl    = #151; QualId       = #152;
  LabelAssign  = #153; LabelList    = #154; ClassHerit   = #155; FieldDecl    = #156; MethodDecl   = #157;
  FormalParams = #158; FormalList   = #159; FormalParam  = #160; ParamInit    = #161; ParamSpec    = #162;
  ConstDecl    = #163; ConstType    = #164; Ordinal      = #165; OrdinalType  = #166; ArrayOfType  = #167;
  TypeId       = #168; ParamType    = #169; PropInterf   = #170; PropIndex    = #171; PropRead     = #172;
  PropWrite    = #173; PropStored   = #174; PropDef      = #175; PropImplem   = #176; RelOp        = #177;
  MetId        = #178; AssignStmt   = #179; ElseBranch   = #180; ExprList     = #181; CaseList     = #182;
  EndCaseList  = #183; SetList      = #184; InterDecl    = #185; LabelId      = #186; SubRange     = #187;
  FileOf       = #188; ForStmt      = #189; PropParams   = #190; IdentDir     = #191; NameDir      = #192;
  GUID         = #193; ExceptFin    = #194; ExceptHand   = #195; ExceptType   = #196; ExceptList   = #197;
  InterfMet    = #198; InterDir     = #199; AbstractDir  = #200; FinSection   = #201; RaiseStmt    = #202;
  RaiseAt      = #203; PackedDecl   = #204; ObjHerit     = #205; ObjDecl      = #206; ForwardClass = #207;
  RsrcDecl     = #208; OfObject     = #209; MethodDir    = #210; ExternalDir  = #211; Directives   = #211;

  // Other non terminals
  Ident = #240; StringConst = #241; CharConst = #242; IntConst = #243; RealConst = #244;
  // Grammar commands
  Require = #253; Mark = #254; Pop = #255;

  SimpleType = '|' + Ident + '|' + SubRange + '|INTEGER|' + '|BOOLEAN|' + '|BYTE|' + '|WORD|' + '|CARDINAL|' + '|LONGINT|' +
    '|INT64|' + '|UINT64|' + '|CHAR|' + '|WIDECHAR|' + '|WIDESTRING|' +'|LONGWORD|' + '|SHORTINT|' + '|SMALLINT|' +
    '|PCHAR|' + '|POINTER|' + '|REAL|' + '|SINGLE|' + '|DOUBLE|' + '|EXTENDED|' + '|CURRENCY|' + '|COMP|' +
    '|BYTEBOOL|' + '|WORDBOOL|'+ '|LONGBOOL|';

  Productions : array[Start..Directives] of string = (
// Start
  '|PROGRAM|' + Ident + ParIdentList + ';' + UsesClause + InterDecl + Require + CompoundStmt  + '.' +
  '|UNIT|'    + Ident + ';' + Require + IntSection + Require + ImplSection + Require + InitSection + '.' +
  '|LIBRARY|' + Ident + ';' + UsesClause + DeclSection + Require + CompoundStmt  + '.' +
  '|PACKAGE|' + Ident + ';' + 'REQUIRES' + Ident + IdentList + 'CONTAINS' + Ident + IdentList + 'END.',
// ParIdentList
  '|(|' + Ident + IdentList + ')',
// IdentList
  '|,|' + Ident + IdentList,
// UsesClause
  '|USES|' + Ident + IdentList + ';',
// ExportsList
  '|,|' + Ident + FormalParams + PropIndex + NameDir + ExportsList,
// DeclSection
  'Declaration Section' +
  '|VAR|'         + Require + VarDecl + DeclSection +
  '|CONST|'       + Require + ConstDecl + DeclSection +
  '|TYPE|'        + Require + TypeDecl + DeclSection +
  '|LABEL|'       + Require + LabelId + LabelList + DeclSection +
  '|PROCEDURE|'   + Ident + MetId + FormalParams + ';' + Directives + ExternalDir + InterDecl + CompoundStmt + ';' + Mark + DeclSection +
  '|FUNCTION|'    + Ident + MetId + FormalParams + ':' + Ident + ';' + Directives + ExternalDir + InterDecl + CompoundStmt + ';' + Mark + DeclSection +
  '|CONSTRUCTOR|' + Ident + MetId + FormalParams + ';' + Directives + InterDecl + Require + CompoundStmt + ';' + DeclSection +
  '|DESTRUCTOR|'  + Ident + MetId + FormalParams + ';' + Directives + InterDecl + Require + CompoundStmt + ';' + DeclSection +
  '|THREADVAR|'   + Require + VarDecl + DeclSection +
  '|EXPORTS|'     + Ident + FormalParams + PropIndex + NameDir + ExportsList + DeclSection +
  '|RESOURCESTRING|' + Require + RsrcDecl + DeclSection,
// VarDecl
  '|' + Ident + '|' + VarList + ':' + Require + Type_ + VarInit + ';' + VarDecl,
// VarList
  '|,|' + Ident + VarList,
// VarInit
  '|=|' + Require + Expression +
  '|ABSOLUTE|' + Ident,
// Type_
  SimpleType +
  '|ARRAY|'  + ArrayDim + 'OF' + Require + Type_ +
  '|STRING|' + StringLength +
  '|'+ IntConst + '|' + Require + SubRange +
  '|'+ CharConst + '|' + Require + SubRange +
  '|(|' + Ident + EnumList + ')' +
  '|^|' + Ident +
  '|RECORD|' + FieldDecl + 'END' +
  '|CLASS|' + ForwardClass + ClassHerit + FieldDecl + MethodDecl + ClassDecl + 'END' + Mark +
  '|OBJECT|' + ObjHerit + FieldDecl + MethodDecl + ObjDecl + 'END' +
  '|INTERFACE|' + ForwardClass + ParIdentList + GUID + InterfMet + 'END' + Mark +
  '|SET|' + 'OF' + Require + OrdinalType +
  '|PROCEDURE|' + FormalParams + OfObject + Directives +
  '|FUNCTION|' + FormalParams + ':' + Ident + OfObject + Directives +
  '|PACKED|' + PackedDecl +
  '|FILE|'   + FileOf +
  '|TEXT|'   +
  '|TYPE|'   + Ident,
// EnumList
  '|,|' + Ident + EnumList,
// CompoundStmt
  '|BEGIN|' + Statement + StmtList + 'END',
// Statement
  '|' + Ident + '|' + LabelAssign + AssignStmt + Mark +
  '|BEGIN|' + Statement + StmtList + 'END' +
  '|IF|' + Require + Expression + 'THEN' + Statement + ElseBranch +
  '|REPEAT|' + Statement + StmtList + 'UNTIL' + Require + Expression +
  '|WHILE|' + Require + Expression + 'DO' + Statement +
  '|FOR|' + Ident + QualId + Require + ForStmt + 'DO' + Statement +
  '|WITH|' + Ident + QualId + WithList + 'DO' + Statement +
  '|CASE|' + Require + Expression + 'OF' + Require + Expression + SetList + ':' + Statement + CaseList + Mark +
  '|TRY|' + Statement + StmtList + ExceptFin +
  '|GOTO|' + Require + LabelId +
  '|INHERITED|' +
  '|RAISE|' + RaiseStmt +
  '|' + IntConst + '|' + ':' + Statement +
  '|ASM|' + (*AsmStatement +*) 'END',
// StmtList
  '|;|' + Statement + StmtList,
// Expression
  'Expression' +
  '|' + Ident + '|' + QualId + RelOp + Expression +
  '|' + IntConst + '|' + RelOp + Expression +
  '|' + StringConst + '|' + RelOp + Expression +
  '|' + CharConst + '|' + RelOp + Expression +
  '|' + RealConst + '|' + RelOp + Expression +
  '|+|' + Expression +
  '|-|' + Expression +
  '|NOT|' + Expression +
  '|(|' + Expression + ExprList + ')' + RelOp + Expression +
  '|NIL|' +
  '|@|' + Expression +
  '|[|' + Expression + SetList + ']' + Expression,
// ToOrDownto
  '|TO||DOWNTO|',
// WithList
  '|,|' + Ident + QualId + WithList,
// IntSection
  '|INTERFACE|' + UsesClause + InterDecl,
// ImplSection
  '|IMPLEMENTATION|'+ UsesClause + DeclSection,
// InitSection
  '|BEGIN|' + Statement + StmtList + 'END' +
  '|INITIALIZATION|' + Statement + StmtList + FinSection +
  '|END|',
// TypeDecl
  '|' + Ident + '|' + '=' + Require + Type_ + ';' + TypeDecl,
// StringLength
  '|[|' + Require + IntConst + ']',
// ArrayDim
  '|[|' + Require + Expression + SetList + ']',
// ClassDecl
  '|PRIVATE|' + FieldDecl + MethodDecl + ClassDecl +
  '|PROTECTED|' + FieldDecl + MethodDecl + ClassDecl +
  '|PUBLIC|' + FieldDecl + MethodDecl + ClassDecl +
  '|PUBLISHED|' + FieldDecl + MethodDecl + ClassDecl +
  '|AUTOMATED|' + FieldDecl + MethodDecl + ClassDecl,
// QualId
  '|.|' + Ident + QualId +
  '|(|' + Expression + ExprList + ')' + QualId +
  '|[|' + Require + Expression + ExprList + ']' + QualId,
// LabelAssign
  '|.|' + Ident + QualId +
  '|(|' + Expression + ExprList + ')' + QualId +
  '|[|' + Require + Expression + ExprList + ']' + QualId +
  '|:|' + Statement + Pop,
// LabelList
  '|,|' + Require + LabelId + LabelList,
// ClassHerit
  '|(|' + Ident + IdentList + ')' +
  '|OF|' + Ident + Pop,
// FieldDecl
  '|' + Ident + '|' + VarList + ':' + Require + Type_ + ';' + FieldDecl,
// MethodDecl
  '|PROCEDURE|'   + Ident + FormalParams + ';' + MethodDir + Directives + MethodDecl +
  '|FUNCTION|'    + Ident + FormalParams + ':' + Ident + ';' + MethodDir + Directives + MethodDecl +
  '|CONSTRUCTOR|' + Ident + FormalParams + ';' + MethodDir + Directives + MethodDecl +
  '|DESTRUCTOR|'  + Ident + FormalParams + ';' + MethodDir + Directives + MethodDecl +
  '|PROPERTY|'    + Ident + PropParams + PropInterf + PropIndex + PropRead + PropWrite + PropStored + PropDef + PropImplem + ';' + MethodDecl,
// FormalParams
  '|(|' + FormalParam + FormalList + ')',
// FormalList
  '|;|' + FormalParam + FormalList,
// FormalParam
  '|' + Ident + '|' + IdentList + Require + ParamSpec + ParamInit +
  '|VAR|' + Ident + IdentList + ParamSpec +
  '|CONST|' + Ident + IdentList + ParamSpec + ParamInit +
  '|OUT|' + Ident + IdentList + Require + ParamSpec,
// ParamInit
  '|=|' + Ident,
// ParamSpec
  '|:|' + Require + ParamType,
// ConstDecl
  '|' + Ident + '|' + ConstType + '=' + Require + Expression + ';' + ConstDecl,
// ConstType
  '|:|' + Require + Type_,
// Ordinal
  '|' + IntConst + '|' +
  '|' + CharConst + '|' +
  '|' + Ident + '|',
// OrdinalType
  '|' + Ident + '|' + SubRange +
  '|' + IntConst + '|' + Require + SubRange +
  '|' + CharConst + '|' + Require + SubRange,
// ArrayOfType
  SimpleType +
  '|CONST|',
// TypeId
  SimpleType,
// ParamType
  SimpleType +
  '|STRING||FILE||TEXT|' +
  '|ARRAY|' + 'OF' + Require + ArrayOfType,
// PropInterf
  '|:|' + TypeId,
// PropIndex
  '|INDEX|' + IntConst,
// PropRead
  '|READ|' + Ident,
// PropWrite
  '|WRITE|' + Ident,
// PropStored
  '|STORED|' + Ident,
// PropDefault
  '|DEFAULT|' + Expression +
  '|NODEFAULT|',
// PropImplem
  '|IMPLEMENTS|' + Ident + IdentList,
// RelOp
  '|>||<||>=||<=||<>||=||IN||IS||AS|' +
  '|+||-||AND||OR|',
// MetId
  '|.|' + Ident,
// AssignStmt
  '|:=|' + Require + Expression,
// ElseBranch
  '|ELSE|' + Statement,
// ExprList
  '|,|' + Require + Expression + SetList,
// CaseList
  '|;|' + EndCaseList + Require + Expression + SetList + ':' + Statement + CaseList +
  '|ELSE|' + Statement + StmtList + 'END' +
  '|END|',
// EndCaseList
  '|ELSE|' + Statement + StmtList + 'END' + Pop +
  '|END|' + Pop,
// SetList
  '|,|' + Require + Expression + SetList +
  '|..|' + Require + Expression + ExprList,
// InterDecl
  'Declaration Section' +
  '|VAR|' + Require + VarDecl + InterDecl +
  '|CONST|' + Require + ConstDecl + InterDecl +
  '|TYPE|' + Require + TypeDecl + InterDecl +
  '|LABEL|' + Require + LabelId + LabelList + InterDecl +
  '|PROCEDURE|' + Ident + FormalParams + ';' + Directives + ExternalDir + InterDecl + Require + CompoundStmt + ';' + InterDecl +
  '|FUNCTION|'  + Ident + FormalParams + ':' + Ident + ';' + Directives + ExternalDir + InterDecl + Require + CompoundStmt + ';' + InterDecl +
  '|RESOURCESTRING|' + Require + RsrcDecl + DeclSection,
// LabelId
  '|' + Ident + '|' +
  '|' + IntConst + '|',
// SubRange
  '|..|' + Require + Ordinal,
// FileOf
  '|OF|' + Require + TypeId,
// ForStmt
  '|:=|' + Require + Expression + Require + ToOrDownto + Require + Expression +
  '|IN|' + Require + Expression,
// PropParams
  '|[|' + Require + FormalParam + FormalList + ']',
// IdentDir
  '|' + StringConst + '|' +
  '|' + Ident + '|' +
  '|' + CharConst + '|',
// NameDir
  '|NAME|' + Require + IdentDir,
// GUID
  '|[|' + IdentDir + ']',
// ExceptFin
  '|EXCEPT|' + ExceptHand + Statement + StmtList + Mark +
  '|FINALLY|' + Statement + StmtList,
// ExceptHand
  '|ON|' + Ident + ExceptType + 'DO' + Statement + ExceptList + EndCaseList + Pop,
// ExceptType
  '|:|' + Ident,
// ExceptList
  '|;|' + ExceptHand,
// InterfMet
  '|PROCEDURE|' + Ident + FormalParams + ';' + InterDir + InterfMet +
  '|FUNCTION|'  + Ident + FormalParams + ':' + Ident + ';' + InterDir + InterfMet +
  '|PROPERTY|'  + Ident + PropParams + PropInterf + PropIndex + PropRead + PropWrite + PropDef + ';' + InterfMet,
// InterDir
  '|CDECL|;' + '|SAFECALL|;' + '|STDCALL|;' + '|REGISTER|;' + '|PASCAL|;',
// AbstractDir
  '|ABSTRACT|;',
// FinSection
  '|FINALIZATION|' + Statement + StmtList + 'END',
// RaiseStmt
  '|' + Ident + '|' + QualId + RaiseAt,
// RaiseAt
  '|AT|' + Require + Expression,
// PackedDecl
  '|ARRAY|' + ArrayDim + 'OF' + Require + Type_ +
  '|RECORD|' + FieldDecl + 'END' +
  '|CLASS|' + ForwardClass + ClassHerit + FieldDecl + MethodDecl + ClassDecl + 'END' + Mark + // Forwardclass
  '|OBJECT|' + ObjHerit + FieldDecl + MethodDecl + ObjDecl + 'END' +
  '|SET|' + 'OF' + Require + OrdinalType +
  '|FILE|' + FileOf,
// ObjHerit
  '|(|' + Ident + ')',
// ObjDecl
  '|PRIVATE|' + FieldDecl + MethodDecl + ObjDecl +
  '|PROTECTED|' + FieldDecl + MethodDecl + ObjDecl +
  '|PUBLIC|' + FieldDecl + MethodDecl + ObjDecl,
// ForwardClass
  '|;|' + Pop,
// RsrcDecl
  '|' + Ident + '|' + '=' + StringConst + ';' + RsrcDecl,
// OfObject
  '|OF|' + 'OBJECT',
// MethodDir
  '|REINTRODUCE|;' + MethodDir + '|OVERLOAD|;' + MethodDir + '|VIRTUAL|;' + AbstractDir + '|OVERRIDE|;' +
  '|MESSAGE|;' + LabelId + '|DYNAMIC|;' + AbstractDir,
// ExternalDir
  '|EXTERNAL|' + IdentDir + PropIndex + NameDir + ';' + Pop +
// Directives
  '|FORWARD|;' + Pop +
  '|FAR|;' + Directives + '|NEAR|;' + Directives + '|EXPORT|;' + Directives + // Deprecateds
  '|CDECL|;' + '|SAFECALL|;' + '|STDCALL|;' + '|REGISTER|;' + '|PASCAL|;' + '|INLINE|;'
  );

implementation

end.
