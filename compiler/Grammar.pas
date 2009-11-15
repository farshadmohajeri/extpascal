unit Grammar;

interface

const
  // Productions id
  Start       = #128; ProgramParams = #129; IdentList  = #130; UsesClause   = #131; UsesList    = #132;
  DeclSection = #133; VarDecl       = #134; VarList    = #135; VarInit      = #136; Type_       = #137;
  EnumList    = #138; CompoundStmt  = #139; Statement  = #140; StmtList     = #141; Expression  = #142;
  ToOrDownto  = #143; WithList      = #144; IntSection = #145; ImplSection  = #146; InitSection = #147;
  TypeDecl    = #148; StringLength  = #149; ArrayDim   = #150; ClassDecl    = #151; QualId      = #152;
  Param       = #153; ParamList     = #154; ClassHerit = #155; VarClassDecl = #156; MetClassDecl= #157;
  FormalParams= #158; FormalList    = #159; FormalParam= #160; ParamInit    = #161; ParamSpec   = #162;
  ConstDecl   = #163; ConstType     = #164; OrdinalType= #165; TypedConst   = #166; ConstList   = #167;
  TypeId      = #168; ParamType     = #169; PropInterf = #170; PropIndex    = #171; PropRead    = #172;
  PropWrite   = #173; PropStored    = #174; PropDef    = #175; PropImplem   = #176; RelOp       = #177;
  MetId       = #178; AssignStmt    = #179; ElseBranch = #180; ExprList     = #181; CaseList    = #182;
  EndCaseList = #183; SetList       = #184; InterDecl  = #185; Directives   = #186;

  // Other non terminals
  Ident = #240; StringConst = #241; CharConst = #242; IntConst = #243; RealConst = #244; ConstExpr = #245; LabelId = #246;
  // Grammar commands
  Required = #253; Mark = #254; Pop = #255;

  SimpleType = '|' + Ident + '|' + '|INTEGER|' + '|BOOLEAN|' + '|BYTE|' + '|WORD|' + '|CARDINAL|' + '|LONGINT|' + '|INT64|' + '|UINT64|' +
    '|CHAR|' + '|WIDECHAR|' + '|WIDESTRING|' +'|LONGWORD|' + '|SHORTINT|' + '|SMALLINT|' + '|PCHAR|' + '|POINTER|' +
    '|REAL|' + '|SINGLE|' + '|DOUBLE|' + '|EXTENDED|' + '|CURRENCY|' + '|COMP|';

  Productions : array[Start..Directives] of string = (
// Start
  '|PROGRAM|' + Ident + ProgramParams + ';' + UsesClause + DeclSection + CompoundStmt  + '.' +
  '|UNIT|' + Ident + ';' + Required + IntSection + Required + ImplSection + InitSection + '.',
  //   '|LIBRARY|' + Ident + ';' + ProgramBlock + '.',
  //   '|PACKAGE|' + Ident + ';' + RequiresClause + ContainsClause + 'END .',
// Params
  '|(|' + Ident + IdentList + ')',
// IdentList
  '|,|' + Ident + IdentList,
// UsesClause
  '|USES|' + Ident + UsesList + ';',
// UsesList
  '|,|' + Ident + UsesList,
// DeclSection
  'Declaration Section' +
  '|VAR|' + Required + VarDecl + DeclSection +
  '|CONST|' + Required + ConstDecl + DeclSection +
  '|TYPE|' + Required + TypeDecl + DeclSection +
  '|LABEL|' + LabelId + UsesList + DeclSection +
  '|PROCEDURE|'   + Ident + MetId + FormalParams + ';' + Directives + InterDecl + CompoundStmt + ';' + DeclSection +
  '|FUNCTION|'    + Ident + MetId + FormalParams + ':' + Ident + ';' + Directives + InterDecl + CompoundStmt + ';' + DeclSection +
  '|CONSTRUCTOR|' + Ident + MetId + FormalParams + ';' + Directives + InterDecl + Required + CompoundStmt + ';' + DeclSection +
  '|DESTRUCTOR|'  + Ident + MetId + FormalParams + ';' + Directives + InterDecl + Required + CompoundStmt + ';' + DeclSection,
// VarDecl
  '|' + Ident + '|' + VarList + ':' + Type_ + VarInit + ';' + VarDecl,
// VarList
  '|,|' + Ident + VarList,
// VarInit
  '|=|' + ConstExpr +
  '|ABSOLUTE|' + Ident,
// Type_
  SimpleType +
  '|ARRAY|' + ArrayDim + 'OF' + Type_ +
  '|STRING|' + StringLength +
  '|'+ ConstExpr + '|' + '..' + ConstExpr +
  '|(|' + Ident + EnumList + ')' +
  '|^|' + Ident +
  '|CLASS|' + ClassHerit + VarClassDecl + MetClassDecl + ClassDecl + 'END' +
  '|SET|' + 'OF' + OrdinalType +
  '|FILE|' + 'OF' + TypeId,
// EnumList
  '|,|' + Ident + EnumList,
// CompoundStmt
  '|BEGIN|' + Statement + StmtList + 'END',
// Statement
  '|' + Ident + '|' + QualId + AssignStmt +
  '|BEGIN|' + Statement + StmtList + 'END' +
  '|IF|' + Expression + 'THEN' + Statement + ElseBranch +
  '|REPEAT|' + Statement + StmtList + 'UNTIL' + Expression +
  '|WHILE|' + Expression + 'DO' + Statement +
  '|FOR|' + Ident + QualId + ':=' + Expression + ToOrDownto + Expression + 'DO' + Statement +
  '|WITH|' + Ident + QualId + WithList + 'DO' + Statement +
//  '|;|' + Statement +
  '|CASE|' + Expression + 'OF' + Expression + SetList + ':' + Statement + CaseList + Mark +
  '|GOTO|' + LabelId +
  '|INHERITED|' +
  //   '|ASM|'  + AsmStatement + 'END' +
  '|' + LabelId + '|' + ':' + Statement,
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
  '|[|' + Expression + SetList + ']' + Expression,
// ToOrDownto
  '|TO|' + '|DOWNTO|',
// WithList
  '|,|' + Ident + QualId + WithList,
// IntSection
  '|INTERFACE|' + UsesClause + DeclSection,
// ImplSection
  '|IMPLEMENTATION|'+ UsesClause + DeclSection,
// InitSection
  '|BEGIN|' + Statement + StmtList + 'END' +
  '|END|',
// TypeDecl
  '|' + Ident + '|' + '=' + Type_ + ';' + TypeDecl,
// StringLength
  '|[|' + Required + IntConst + ']',
// ArrayDim
  '|[|' + Required + Expression + SetList + ']',
// ClassDecl
  '|PRIVATE|' + VarClassDecl + MetClassDecl + ClassDecl +
  '|PROTECTED|' + VarClassDecl + MetClassDecl + ClassDecl +
  '|PUBLIC|' + VarClassDecl + MetClassDecl + ClassDecl +
  '|PUBLISHED|' + VarClassDecl + MetClassDecl + ClassDecl +
  '|AUTOMATED|' + VarClassDecl + MetClassDecl + ClassDecl,
// QualId
  '|.|' + Ident + QualId +
  '|(|' + Expression + ExprList + ')' + QualId +
  '|[|' + Required + Expression + ExprList + ']' + QualId ,
// Param
  '|' + IntConst + '|' +
  '|' + Ident + '|' + QualId +
  '|' + StringConst + '|' +
  '|' + CharConst + '|',
// ParamList
  '|,|' + Required + Param + ParamList,
// ClassHerit
  '|(|' + Ident + IdentList + ')' +
  '|OF|' + Ident + Pop,
// VarClassDecl
  '|' + Ident + '|' + VarList + ':' + Type_ + ';' + VarClassDecl,
// MetClassDecl
  '|PROCEDURE|'   + Ident + FormalParams + ';' + Directives + MetClassDecl +
  '|FUNCTION|'    + Ident + FormalParams + ':' + Ident + ';' + Directives + MetClassDecl +
  '|CONSTRUCTOR|' + Ident + FormalParams + ';' + Directives + MetClassDecl +
  '|DESTRUCTOR|'  + Ident + FormalParams + ';' + Directives + MetClassDecl +
  '|PROPERTY|'    + Ident + PropInterf + PropIndex + PropRead + PropWrite + PropStored + PropDef + PropImplem + ';' + MetClassDecl,
// FormalParams
  '|(|' + FormalParam + FormalList + ')',
// FormalList
  '|;|' + FormalParam + FormalList,
// FormalParam
  '|' + Ident + '|' + IdentList + ':' + ParamType + ParamInit +
  '|VAR|' + Ident + IdentList + ParamSpec +
  '|CONST|' + Ident + IdentList + ParamSpec + ParamInit +
  '|OUT|' + Ident + IdentList + ParamSpec,
// ParamInit
  '|=|' + Ident,
// ParamSpec
  '|:|' + ParamType,
// ConstDecl
  '|' + Ident + '|' + ConstType + '=' + Expression + ';' + ConstDecl,
// ConstType
  '|:|' + Type_,
// OrdinalType
  '|' + IntConst + '|' +
  '|' + CharConst + '|' +
  '|' + Ident + '|',
// TypedConst
  '',
 // '|' + Expression + '|' + ConstList +
 // '|(|' + TypedConst + ')',
// ConstList
  '|,|' + Expression + ConstList,
// TypeId
  SimpleType,
// ParamType
  SimpleType +
  '|STRING|' +
  '|FILE|' +
  '|ARRAY|' + 'OF' + TypeId,
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
  '|DEFAULT|' + Ident +
  '|NODEFAULT|',
// PropImplem
  '|IMPLEMENTS|' + TypeId,
// RelOp
  '|>||<||>=||<=||<>||=||IN||IS||AS|' +
  '|+||-||AND||OR|',
// MetId
  '|.|' + Ident,
// AssignStmt
  '|:=|' + Required + Expression,
// ElseBranch
  '|ELSE|' + Statement,
// ExprList
  '|,|' + Expression + SetList,
// CaseList
  '|;|' + EndCaseList + Expression + SetList + ':' + Statement + CaseList +
  '|ELSE|' + Statement + StmtList + 'END' +
  '|END|',
// EndCaseList
  '|ELSE|' + Statement + StmtList + 'END' + Pop +
  '|END|' + Pop,
// SetList
  '|,|' + Expression + SetList +
  '|..|' + Expression + ExprList,
// InterDecl
  'Declaration Section' +
  '|VAR|' + Required + VarDecl + InterDecl +
  '|CONST|' + Required + ConstDecl + InterDecl +
  '|TYPE|' + Required + TypeDecl + InterDecl +
  '|LABEL|' + LabelId + UsesList + InterDecl +
  '|PROCEDURE|' + Ident + FormalParams + ';' + Directives + InterDecl + CompoundStmt + ';' + InterDecl +
  '|FUNCTION|'  + Ident + FormalParams + ':' + Ident + ';' + Directives + InterDecl + CompoundStmt + ';' + InterDecl,
// Directives
  '|VIRTUAL|;' + Directives + '|OVERRIDE|;' + Directives + '|OVERLOAD|;' + Directives + '|REINTRODUCE|;' + Directives + '|INLINE|' + Directives +
  '|EXTERNAL|;' + Directives + '|FORWARD|;' + Directives + '|MESSAGE|;' + Directives + '|FAR|;' + Directives + '|DYNAMIC|;' + Directives + '|EXPORT|;' + Directives +
  '|CDECL|;' + Directives + '|SAFECALL|;' + Directives + '|STDCALL|;' + Directives + '|REGISTER|;' + Directives + '|PASCAL|;' + Directives
  );

implementation

end.
