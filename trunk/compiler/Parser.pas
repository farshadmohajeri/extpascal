unit Parser;
{
Author: Wanderlan Santos dos Anjos, wanderlan.anjos@gmail.com
Date: dec-2008
License: <extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
interface

uses
  Scanner;

type
  TSymbol = string[15];
  TStack  = array[1..100] of TSymbol;
  TParser = class(TScanner)
  private
    Symbol     : TSymbol;
    Symbols    : TStack;
    Production : string;
    procedure ExpandProduction(T : string);
  public
    procedure Compile;
  end;

implementation

uses
  SysUtils;

const
  // Productions
  Start       = #128; ProgramParams = #129; IdentList  = #130; UsesClause   = #131; UsesList    = #132;
  DeclSection = #133; VarDecl       = #134; VarList    = #135; VarInit      = #136; Type_       = #137;
  EnumList    = #138; CompoundStmt  = #139; Statement  = #140; EndStatement = #141; Expression  = #142;
  ToOrDownto  = #143; WithList      = #144; IntSection = #145; ImplSection  = #146; InitSection = #147;
  TypeDecl    = #148; StringLength  = #149; ArrayDim   = #150; ClassDecl    = #151; QualId      = #152;
  Param       = #153; ParamList     = #154; ClassHerit = #155; VarClassDecl = #156; MetClassDecl= #157;
  FormalParams= #158; FormalList    = #159; FormalParam= #160; ParamInit    = #161; ParamType   = #162;
  ConstDecl   = #163; Directives    = #164;

  // Other non terminals
  Ident = #240; StringConst = #241; IntConst = #242; RealConst = #243; ConstExpr = #244; LabelId = #245; TypeId = #246; ClassId = #247;

const
  Grammar : array[Start..Directives] of string = (
// Start
  '|PROGRAM|' + Ident + ProgramParams + ';' + UsesClause + DeclSection + CompoundStmt  + '.' +
  '|UNIT|' + Ident + ';' + IntSection + ImplSection + InitSection + '.',
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
  '|VAR|' + VarDecl + DeclSection +
  '|CONST|' + ConstDecl + DeclSection +
  '|TYPE|' + TypeDecl + DeclSection +
  '|LABEL|' + LabelId + UsesList + DeclSection,
// VarDecl
  '|' + Ident + '|' + VarList + ':' + Type_ + VarInit + ';' + VarDecl,
// VarList
  '|,|' + Ident + VarList,
// VarInit
  '|=|' + ConstExpr +
  '|ABSOLUTE|' + Ident,
// Type_
  '|' + Ident + '|' +
  '|INTEGER|' + '|CHAR|' + '|BOOLEAN|' + '|BYTE|' + '|WORD|' + '|CARDINAL|' + '|LONGINT|' + '|INT64|' + '|UINT64|' +
  '|WIDECHAR|' + '|LONGWORD|' + '|SHORTINT|' + '|SMALLINT|' + '|PCHAR|' + '|POINTER|' +
  '|REAL|' + '|SINGLE|' + '|DOUBLE|' + '|EXTENDED|' + '|CURRENCY|' + '|COMP|' +
  '|'+ ConstExpr + '|' + '..' + ConstExpr +
  '|(|' + Ident + EnumList + ')' +
  '|STRING|' + StringLength +
  '|ARRAY|' + ArrayDim + 'OF' + Type_ +
  '|WIDESTRING|' +
  '|^|' + Ident +
  '|CLASS|' + ClassHerit + ClassDecl + 'END',
// EnumList
  '|,|' + Ident + EnumList,
// CompoundStmt
  '|BEGIN|' + Statement + 'END',
// Statement
  '|' + Ident + '|' + QualId + Statement +
  '|BEGIN|' + Statement + 'END' +
  '|REPEAT|' + Statement + 'UNTIL' + Expression +
  '|WHILE|' + Expression + 'DO' + Statement +
  //   '|FOR|' + QualId + ':=' + Expression + ToOrDownto + Expression + 'DO' + Statement +
  '|WITH|' + Ident + QualId + WithList + 'DO' + Statement +
  '|;|' + Statement +
  '|GOTO|' + LabelId + EndStatement +
  '|INHERITED|' + EndStatement +
  //   '|ASM|'  + AsmStatement + 'END' +
  '|' + LabelId + '|' + ':' + Statement,
// EndStatement
  '|;|' + Statement,
// Expression
  '',
// ToOrDownto
  '|TO|' + '|DOWNTO|',
// WithList
  '|,|' + Ident + QualId + WithList,
// IntSection
  '|INTERFACE|' + UsesClause + DeclSection,
// ImplSection
  '|IMPLEMENTATION|'+ UsesClause + DeclSection,
// InitSection
  '|BEGIN|' + Statement + 'END',
// TypeDecl
  '|' + Ident + '|' + '=' + Type_ + ';' + TypeDecl,
// StringLength
  '|[|' + IntConst + ']',
// ArrayDim !!!!! Faltam várias dimensões
  '|[|' + IntConst + '..' + IntConst + ']',
// ClassDecl
  '|PRIVATE|' + VarClassDecl + MetClassDecl + ClassDecl +
  '|PUBLIC|' + VarClassDecl + MetClassDecl + ClassDecl +
  '|PROTECTED|' + VarClassDecl + MetClassDecl + ClassDecl +
  '|OF|' + Ident,
// QualId
  '|.|' + Ident + QualId +
  '|(|' + Param + ParamList + ')' + QualId,
// Param
  '|' + IntConst + '|' +
  '|' + Ident + '|' + QualId +
  '|' + StringConst + '|',
// ParamList
  '|,|' + Param + ParamList,
// ClassHerit
  '|(|' + Ident + IdentList + ')',
// VarClassDecl
  '|' + Ident + '|' + VarList + ':' + Type_ + ';' + VarClassDecl,
// MetClassDecl
  '|PROCEDURE|'   + Ident + FormalParams + ';' + Directives +
  '|FUNCTION|'    + Ident + FormalParams + ':' + Ident + ';' + Directives +
  '|CONSTRUCTOR|' + Ident + FormalParams + ';' + Directives +
  '|DESTRUCTOR|'  + Ident + FormalParams + ';' + Directives,
// FormalParams
  '|(|' + FormalParam + FormalList + ')',
// FormalList
  '|;|' + FormalParam + FormalList,
// FormalParam
  '|' + Ident + '|' + IdentList + ':' + Ident + ParamInit +
  '|VAR|' + Ident + IdentList + ParamType +
  '|CONST|' + Ident + IdentList + ParamType + ParamInit +
  '|OUT|' + Ident + IdentList + ParamType,
// ParamInit
  '|=|' + IntConst,
// ParamType
  '|:|' + Ident,
// ConstDecl
  '|' + Ident + '|' + '=' + StringConst + ';',
// Directives
  '|VIRTUAL|;' + Directives + '|OVERRIDE|;' + Directives + '|OVERLOAD|;' + Directives + '|REINTRODUCE|;' + Directives +
  '|EXTERNAL|;' + Directives + '|FORWARD|;' + Directives + '|MESSAGE|;' + Directives + '|FAR|;' + Directives + '|DYNAMIC|;' + Directives + '|EXPORT|;' + Directives +
  '|CDECL|;' + Directives + '|SAFECALL|;' + Directives + '|STDCALL|;' + Directives + '|REGISTER|;' + Directives + '|PASCAL|;' + Directives
  );

procedure TParser.Compile; begin
  Symbols[1] := Start;
  Symbol     := Start;
  Top        := 1;
  repeat
    case Symbol[1] of
      #0..#127           : MatchToken(Symbol); // Terminal
      Start..pred(Ident) : ExpandProduction(Token.Lexeme) // Production
    else // Other Terminal
      MatchTerminal(TTokenKind(byte(Symbol[1]) - byte(pred(Ident))));
    end;
    if Top > 1 then begin
      dec(Top);
      Symbol := Symbols[Top];
    end;
  until EndSource or (Top = 0);
  if Errors <> 0 then
    writeln('Compilation with ', Errors, ' error(s)')
  else
    writeln('Successful compilation')
end;

procedure TParser.ExpandProduction(T : string);
var
  P, TopAux : integer;
  Aux  : TStack;
  LenT : integer;
begin
  Production := Grammar[Symbol[1]];
  P := pos('|' + UpperCase(T) + '|', Production); // find FIRST or FOLLOW terminal
  if (P = 0) and (Token.Kind <> tkUndefined) then begin
    P := pos('|' + char(byte(Token.Kind) + byte(pred(Ident))) + '|', Production);
    LenT := 1
  end
  else
    LenT := length(T);
  if P <> 0 then begin
    dec(Top);
    TopAux := 1;
    Aux[1] := copy(Production, P + 1, LenT);
    inc(P, LenT + 2);
    for P := P to length(Production) do
      case Production[P] of
        Start..#255 : begin // Nonterminal
          inc(TopAux);
          Aux[TopAux] := Production[P];
        end;
        '|' : break; // End production
      else
        if (Aux[TopAux] <> '') and (Aux[TopAux][1] >= Start) then begin // begin terminal
          inc(TopAux);
          Aux[TopAux] := Production[P]
        end
        else // Terminal
          Aux[TopAux] := Aux[TopAux] + Production[P]
      end;
    for TopAux := TopAux downto 1 do begin // push at reverse order
      inc(Top);
      Symbols[Top] := Aux[TopAux];
    end;
    inc(Top);
  end;
end;

end.
