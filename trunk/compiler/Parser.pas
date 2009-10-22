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
    Top        : cardinal;
    Production : string;
    procedure ExpandProduction(T : string);
  public
    procedure Compile;
  end;

implementation

uses
  SysUtils;

const
  ClassId               = #0#237;
  TypeId                = #0#236;
  LabelId               = #0#235;
  ConstExpr             = #0#234;
  RealConst             = #0#233;
  IntegerConst          = #0#232;
  StringConst           = #0#231;
  Ident                 = #0#230;
  Start                 = #0#1;
  ProgramParams         = #0#2;
  FileList              = #0#3;
  UsesClause            = #0#4;
  UsesList              = #0#5;
  DeclSection           = #0#6;
  VarDecl               = #0#7;
  VarList               = #0#8;
  VarInit               = #0#9;
  Type_                 = #0#10;
  EnumList              = #0#11;
  CompoundStmt          = #0#12;
  Statement             = #0#13;
  EndStatement          = #0#14;
  Expression            = #0#15;
  ToOrDownto            = #0#16;
  WithList              = #0#17;
  InterfaceSection      = #0#18;
  ImplementationSection = #0#19;
  InitSection           = #0#20;
  TypeDecl              = #0#21;
  StringLength          = #0#22;
  ArrayDimension        = #0#23;
  ClassDecl             = #0#24;
  QualId                = #0#25;
  Param                 = #0#26;
  ParamList             = #0#27;
  BeginDecl             = '|VAR||CONST||TYPE||LABEL||PROCEDURE||FUNCTION||BEGIN|';

const
  Grammar : array[1..27] of string = (
//1- Start
   '|PROGRAM|' + Ident + ProgramParams + ';' + UsesClause + DeclSection + CompoundStmt  + '.' +
   '|UNIT|' + Ident + ';' + InterfaceSection + ImplementationSection + InitSection + '.',
   //   '|LIBRARY|' + Ident + ';' + ProgramBlock + '.',
   //   '|PACKAGE|' + Ident + ';' + RequiresClause + ContainsClause + 'END .',
//2- ProgramParams
   '|(|' + Ident + FileList + ')',
//3- FileList
   '|,|' + Ident + FileList,
//4- UsesClause
   '|USES|'  + Ident + UsesList + ';' +
   BeginDecl,
//5- UsesList
   '|,|' + Ident + UsesList,
//6- DeclSection
   '|VAR|' + VarDecl + DeclSection +
   //   '|CONST|' + Ident ConstantDecl + DeclSection +
   '|TYPE|' + TypeDecl + DeclSection +
   '|LABEL|' + LabelId + UsesList + DeclSection,
//7- VarDecl
   '|' + Ident + '|' + VarList + ':' + Type_ + VarInit + ';' + VarDecl +
   BeginDecl,
//8- VarList
   '|,|' + Ident + VarList,
//9- VarInit
   '|=|' + ConstExpr +
   '|ABSOLUTE|' + Ident,
//10- Type_
   '|' + TypeId + '|' +
   '|INTEGER|' + '|CHAR|' + '|BOOLEAN|' + '|BYTE|' + '|WORD|' + '|CARDINAL|' + '|LONGINT|' + '|INT64|' + '|UINT64|' +
   '|WIDECHAR|' + '|LONGWORD|' + '|SHORTINT|' + '|SMALLINT|' + '|PCHAR|' + '|POINTER|' +
   '|REAL|' + '|SINGLE|' + '|DOUBLE|' + '|EXTENDED|' + '|CURRENCY|' + '|COMP|' +
   '|'+ ConstExpr + '|' + '..' + ConstExpr +
   '|(|' + Ident + EnumList + ')' +
   '|STRING|' + StringLength +
   '|ARRAY|' + ArrayDimension + 'OF' + Type_ +
   '|WIDESTRING|' +
   '|^|' + TypeId +
   '|CLASS|' + ClassDecl,
//11- EnumList
   '|,|' + Ident + EnumList,
//12- CompoundStmt
   '|BEGIN|' + Statement + 'END',
//13- Statement
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
//14- EndStatement
   '|;|' + Statement,
//15- Expression
   '',
//16- ToOrDownto
   '|TO|' + '|DOWNTO|',
//17- WithList
   '|,|' + Ident + QualId + WithList,
//18- InterfaceSection
   '|INTERFACE|' + UsesClause + DeclSection,
//19- ImplementationSection
   '|IMPLEMENTATION|'+ UsesClause + DeclSection,
//20- InitSection
   '|BEGIN|' + Statement + 'END',
//21- TypeDecl
   '|' + Ident + '|' + '=' + Type_ + ';' + TypeDecl +
   BeginDecl,
//22- StringLength
   '|[|' + IntegerConst + ']',
//23- ArrayDimension !!!!! Faltam várias dimensões
   '|[|' + IntegerConst + '..' + IntegerConst + ']',
//24- ClassDecl
   '|(|' + ClassId + ')' +
   //'|PRIVATE|' + VarClassDecl + MethodClassDecl + 'END' +
   '|PUBLIC|' +
   '|PROTECTED|' +
   '|OF|' + ClassId,
//25- QualId
   '|.|' + Ident + QualId +
   '|(|' + Param + ParamList + ')' + QualId,
//26- Param
   '|' + ConstExpr + '|' +
   '|' + Ident + '|' + QualId +
   '|' + StringConst + '|',
//27- ParamList
   '|,|' + Param + ParamList
  );

procedure TParser.Compile; begin
  Symbols[1] := Start;
  Symbol     := Start;
  Top        := 1;
  repeat
    if Symbol[1] <> #0 then // Terminal
      MatchToken(Symbol)
    else
      if Symbol[2] < #230 then // NonTerminal
        ExpandProduction(Token.Lexeme)
      else  // Other Terminal
        MatchTerminal(Symbol[2]);
    dec(Top);
    Symbol := Symbols[Top];
  until EndSource or (Top = 0)
end;

procedure TParser.ExpandProduction(T : string);
var
  P, TopAux : integer;
  Aux  : TStack;
  LenT : integer;
begin
  Production := Grammar[byte(Symbol[2])];
  P := pos('|' + UpperCase(T) + '|', Production); // find FIRST or FOLLOW terminal
  if (P = 0) and (Token.Kind <> tkUndefined) then begin
    P := pos('|'#0 + char(byte(Token.Kind) + 229) + '|', Production);
    LenT := 2
  end
  else
    LenT := length(T);
  if P <> 0 then begin
    dec(Top);
    TopAux := 1;
    Aux[1] := copy(Production, P + 1, LenT);
    inc(P, LenT + 2);
    while P <= length(Production) do begin
      case Production[P] of
        #0 : begin // Nonterminal
          inc(TopAux);
          Aux[TopAux] := copy(Production, P, 2);
          inc(P);
        end;
        ' ' : begin // optional begin of terminal
          inc(TopAux);
          Aux[TopAux] := '';
        end;
        '|' : break; // End production
      else
        if (Aux[TopAux] <> '') and (Aux[TopAux][1] = #0) then begin // begin terminal
          inc(TopAux);
          Aux[TopAux] := Production[P]
        end
        else // Terminal
          Aux[TopAux] := Aux[TopAux] + Production[P]
      end;
      inc(P);
    end;
    for TopAux := TopAux downto 1 do begin // push at reverse order
      inc(Top);
      Symbols[Top] := Aux[TopAux];
    end;
    inc(Top);
  end;
end;

end.
