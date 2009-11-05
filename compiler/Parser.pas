unit Parser;
{
Author: Wanderlan Santos dos Anjos, wanderlan.anjos@gmail.com
Date: jan-2010
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
    procedure PopSymbol;
  protected
    procedure RecoverFromError; override;
  public
    procedure Compile;
  end;

implementation

uses
  SysUtils, Grammar;

procedure TParser.PopSymbol; begin
  if Top > 1 then begin
    dec(Top);
    Symbol := Symbols[Top];
    case Symbol[1] of
      Mark : PopSymbol;
      Pop  : begin
        repeat
          dec(Top);
        until (Symbols[Top] = Mark) or (Top <= 2);
        dec(Top);
        Symbol := Symbols[Top];
      end;
    end;
  end;
end;

procedure TParser.RecoverFromError; begin
  inherited;
  while (Symbol <> ';') and (Top > 1) do PopSymbol;
  inc(Top);
end;

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
    PopSymbol;
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
  Production := Productions[Symbol[1]];
  if T <> '' then
    P := pos('|' + UpperCase(T) + '|', Production) // find FIRST or FOLLOW terminal
  else
    P := 0;
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
        else begin // Terminal
          if Production[P-1] = '|' then begin
            inc(TopAux);
            Aux[TopAux] := '';
          end;
          Aux[TopAux] := Aux[TopAux] + Production[P]
        end;
      end;
    for TopAux := TopAux downto 1 do begin // push at reverse order
      inc(Top);
      Symbols[Top] := Aux[TopAux];
    end;
    inc(Top);
  end
end;

end.
