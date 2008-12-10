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
    function TokensExpected : string;
    function FirstExpected  : string;
    function ExpandProduction(T : string) : boolean;
    procedure RecoverError;
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
  Empty                 = #0#0;
  Start                 = #0#1;
  ProgramParams         = #0#2;
  FileList              = #0#3;
  UsesClause            = #0#4;
  UsesList              = #0#5;
  DeclSection           = #0#6;
  VarDecl               = #0#7;
  VarInit               = #0#8;
  VarList               = #0#9;
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

  BeginDecl = '|VAR|' + Empty + '|CONST|' + Empty + '|TYPE|'  + Empty + '|LABEL|' + Empty +
    '|PROCEDURE|' + Empty + '|FUNCTION|' + Empty + '|BEGIN|' + Empty;

const
  Grammar : array[1..24] of string = (
// Start
   '|PROGRAM|' + Ident + ProgramParams + ';' + UsesClause + DeclSection + CompoundStmt  + '.' +
   '|UNIT|' + Ident + ';' + InterfaceSection + ImplementationSection + InitSection + '.',
//   '|LIBRARY|' + Ident + ';' + ProgramBlock + '.',
//   '|PACKAGE|' + Ident + ';' + RequiresClause + ContainsClause + 'END .',
// ProgramParams
   '|(|' + Ident + FileList + ')' +
   '|;|' + Empty,
// FileList
   '|,|' + Ident + FileList +
   '|)|' + Empty,
// UsesClause
   '|USES|'  + Ident + UsesList + ';' +
   BeginDecl,
// UsesList
   '|,|' + Ident + UsesList +
   '|;|' + Empty,
// DeclSection
   '|VAR|' + VarDecl + DeclSection +
//   '|CONST|' + ConstantDecl +
   '|TYPE|' + TypeDecl + DeclSection +
   '|LABEL|' + LabelId +
   '|BEGIN|' + Empty,
// VarDecl
   Ident + VarList + ':' + Type_ + VarInit + ';' + VarDecl +
   BeginDecl,
// VarList
   '|,|' + Ident + VarList +
   '|:|' + Empty,
// VarInit
   '|=|' + ConstExpr +
   '|ABSOLUTE|' + Ident +
   ';' + Empty,
// Type_
   '|' + TypeId + '|' +
   '|INTEGER|' + '|CHAR|' + '|BOOLEAN|' + '|BYTE|' + '|WORD|' + '|CARDINAL|' + '|LONGINT|' + '|INT64|' + '|UINT64|' +
   '|WIDECHAR|' + '|LONGWORD|' + '|SHORTINT|' + '|SMALLINT|' + '|PCHAR|' +
   '|REAL|' + '|SINGLE|' + '|DOUBLE|' + '|EXTENDED|' + '|CURRENCY|' + '|COMP|' +
   '|'+ ConstExpr + '|' + '..' + ConstExpr +
   '|(|' + Ident + EnumList + ')' +
   '|STRING|' + StringLength +
   '|ARRAY|' + ArrayDimension + 'OF' + Type_ +
   '|WIDESTRING|' +
   '|^|' + TypeId +
   '|CLASS|' + ClassDecl,
// EnumList
   '|,|' + Ident + EnumList +
   '|)|' + Empty,
// CompoundStmt
   '|BEGIN|' + Statement + 'END',
// Statement
   '|BEGIN|' + Statement + 'END' +
   '|REPEAT|' + Statement + 'UNTIL' + Expression +
   '|WHILE|' + Expression + 'DO' + Statement +
//   '|FOR|' + QualId + ':=' + Expression + ToOrDownto + Expression + 'DO' + Statement +
   '|WITH|' + Ident + WithList + 'DO' + Statement +
   '|;|' + Statement +
   '|GOTO|' + LabelId + EndStatement +
   '|INHERITED|' + EndStatement +
//   '|ASM|'  + AsmStatement + 'END' +
   '|' + LabelId + '|' + ':' + Statement +
   '|END|' + Empty,
// EndStatement
   '|;|' + Statement +
   '|END|' + Empty,
// Expression
   '',
// ToOrDownto
   '|TO|' + '|DOWNTO|',
// WithList
   '|,|' + Ident + WithList +
   '|DO|' + Empty,
// InterfaceSection
   '|INTERFACE|' + UsesClause + DeclSection +
   '|IMPLEMENTATION|' + Empty,
// InterfaceSection
   '|IMPLEMENTATION|'+ UsesClause + DeclSection +
   '|BEGIN|' + Empty,
// InitSection
   '|BEGIN|' + Statement + 'END',
// TypeDecl
   Ident + '=' + Type_ + ';' + TypeDecl +
   BeginDecl,
// StringLength
   '|[|' + IntegerConst + ']' +
   '|;|' + Empty,
// ArrayDimension
   '|[|' + IntegerConst + '..' + IntegerConst + ']' +
   '|OF|' + Empty,
// ClassDecl
   '|(|' + ClassId + ')' +
//   '|PRIVATE|' + VarClassDecl + MethodClassDecl + 'END' +
   '|PUBLIC|' +
   '|PROTECTED|' +
   '|OF|' + ClassId
  );

procedure TParser.Compile; begin
  Symbols[1] := Start;
  Symbol     := Start;
  Top        := 1;
  repeat
    if Symbol[1] <> #0 then begin // Terminal
      MatchToken(Symbol);
      dec(Top);
    end
    else
      if Symbol[2] < #230 then begin // NonTerminal
        if not ExpandProduction(Token.Lexeme) then RecoverError;
      end
      else begin // Other Terminal
        MatchTerminal(Symbol[2]);
        dec(Top);
      end;
    Symbol := Symbols[Top];
  until EndSource
end;

procedure TParser.RecoverError;
var
  TE : string;
begin
  TE := TokensExpected;
  Report(TE + ' expected but ''' + Token.Lexeme + ''' found.');
  NextToken;
  if Top = 0 then begin
    ExpandProduction(FirstExpected);
    dec(Top)
  end
  else // Synchronize
    while pos('''' + UpperCase(Token.Lexeme) + '''', TE) = 0 do NextToken;
end;

function TParser.TokensExpected : string;
var
  I   : integer;
  Ins : boolean;
begin
  Result := '';
  Ins    := false;
  for I := 1 to length(Production) do
    if Production[I] = '|' then begin
      Ins := not Ins;
      if Ins then
        Result := Result + ''''
      else
        Result := Result + ''', '
    end
    else
      if Ins then
        Result := Result + Production[I];
  SetLength(Result, length(Result)-2);
  I := LastDelimiter(',', Result);
  if I <> 0 then begin
    delete(Result, I, 2);
    insert(' or ', Result, I);
  end;
end;

function TParser.FirstExpected : string; begin
  Result := copy(Production, 2, pos('|', copy(Production, 2, 100))-1)
end;

function TParser.ExpandProduction(T : string) : boolean;
var
  P, TopAux : integer;
  Aux : TStack;
begin
  Production := Grammar[byte(Symbol[2])];
  P := pos('|' + UpperCase(T) + '|', Production); // find FIRST or FOLLOW terminal
  if (P <> 0) or (Production[1] = #0) then begin
    Result := true;
    dec(Top);
    if Production[1] = #0 then begin
      TopAux := 0;
      P := 1
    end
    else begin
      TopAux := 1;
      Aux[1] := copy(Production, P + 1, length(T));
      inc(P, length(T) + 2);
    end;
    while P <= length(Production) do begin
      case Production[P] of
        #0 : // Nonterminal
          if Production[P + 1] = #0 then // Empty
            exit
          else begin
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
  end
  else
    Result := false;
end;

end.
