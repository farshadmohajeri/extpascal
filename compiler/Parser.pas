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
  Start       = #128; ProgramParams = #129; FileList   = #130; UsesClause   = #131; UsesList    = #132;
  DeclSection = #133; VarDecl       = #134; VarList    = #135; VarInit      = #136; Type_       = #137;
  EnumList    = #138; CompoundStmt  = #139; Statement  = #140; EndStatement = #141; Expression  = #142;
  ToOrDownto  = #143; WithList      = #144; IntSection = #145; ImplSection  = #146; InitSection = #147;
  TypeDecl    = #148; StringLength  = #149; ArrayDim   = #150; ClassDecl    = #151; QualId      = #152;
  Param       = #153; ParamList     = #154;

  // Other non terminals
  Ident = #240; StringConst = #241; IntConst = #242; RealConst = #243; ConstExpr = #244; LabelId = #245; TypeId = #246; ClassId = #247;

  BeginDecl = '|VAR||CONST||TYPE||LABEL||PROCEDURE||FUNCTION||BEGIN|';

const
  Grammar : array[Start..ParamList] of string = (
// Start
   '|PROGRAM|' + Ident + ProgramParams + ';' + UsesClause + DeclSection + CompoundStmt  + '.' +
   '|UNIT|' + Ident + ';' + IntSection + ImplSection + InitSection + '.',
   //   '|LIBRARY|' + Ident + ';' + ProgramBlock + '.',
   //   '|PACKAGE|' + Ident + ';' + RequiresClause + ContainsClause + 'END .',
// ProgramParams
   '|(|' + Ident + FileList + ')',
// FileList
   '|,|' + Ident + FileList,
// UsesClause
   '|USES|'  + Ident + UsesList + ';' +
   BeginDecl,
// UsesList
   '|,|' + Ident + UsesList,
// DeclSection
   '|VAR|' + VarDecl + DeclSection +
   //   '|CONST|' + Ident ConstantDecl + DeclSection +
   '|TYPE|' + TypeDecl + DeclSection +
   '|LABEL|' + LabelId + UsesList + DeclSection,
// VarDecl
   '|' + Ident + '|' + VarList + ':' + Type_ + VarInit + ';' + VarDecl +
   BeginDecl,
// VarList
   '|,|' + Ident + VarList,
// VarInit
   '|=|' + ConstExpr +
   '|ABSOLUTE|' + Ident,
// Type_
   '|' + TypeId + '|' +
   '|INTEGER|' + '|CHAR|' + '|BOOLEAN|' + '|BYTE|' + '|WORD|' + '|CARDINAL|' + '|LONGINT|' + '|INT64|' + '|UINT64|' +
   '|WIDECHAR|' + '|LONGWORD|' + '|SHORTINT|' + '|SMALLINT|' + '|PCHAR|' + '|POINTER|' +
   '|REAL|' + '|SINGLE|' + '|DOUBLE|' + '|EXTENDED|' + '|CURRENCY|' + '|COMP|' +
   '|'+ ConstExpr + '|' + '..' + ConstExpr +
   '|(|' + Ident + EnumList + ')' +
   '|STRING|' + StringLength +
   '|ARRAY|' + ArrayDim + 'OF' + Type_ +
   '|WIDESTRING|' +
   '|^|' + TypeId +
   '|CLASS|' + ClassDecl,
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
// InterfaceSection
   '|INTERFACE|' + UsesClause + DeclSection,
// ImplementationSection
   '|IMPLEMENTATION|'+ UsesClause + DeclSection,
// InitSection
   '|BEGIN|' + Statement + 'END',
// TypeDecl
   '|' + Ident + '|' + '=' + Type_ + ';' + TypeDecl +
   BeginDecl,
// StringLength
   '|[|' + IntConst + ']',
// ArrayDim !!!!! Faltam várias dimensões
   '|[|' + IntConst + '..' + IntConst + ']',
// ClassDecl
   '|(|' + ClassId + ')' +
   //'|PRIVATE|' + VarClassDecl + MethodClassDecl + 'END' +
   '|PUBLIC|' +
   '|PROTECTED|' +
   '|OF|' + ClassId,
// QualId
   '|.|' + Ident + QualId +
   '|(|' + Param + ParamList + ')' + QualId,
// Param
   '|' + IntConst + '|' +
   '|' + Ident + '|' + QualId +
   '|' + StringConst + '|',
// ParamList
   '|,|' + Param + ParamList
  );

procedure TParser.Compile; begin
  Symbols[1] := Start;
  Symbol     := Start;
  Top        := 1;
  repeat
    case Symbol[1] of
      #0..#127         : MatchToken(Symbol); // Terminal
      Start..ParamList : ExpandProduction(Token.Lexeme) // Production
    else // Other Terminal
      MatchTerminal(TTokenKind(byte(Symbol[1]) - 239));
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
    P := pos('|' + char(byte(Token.Kind) + 239) + '|', Production);
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
        ' ' : begin // optional begin of terminal
          inc(TopAux);
          Aux[TopAux] := '';
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
