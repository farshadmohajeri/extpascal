unit Scanner;
{
Author: Wanderlan Santos dos Anjos, wanderlan.anjos@gmail.com
Date: dec-2008
License: <extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
interface

type
  TTokenKind = (tkUndefined, tkIdentifier, tkStringConstant, tkIntegerConstant, tkRealConstant, tkConstantExpression,
                tkLabelIdentifier, tkTypeIdentifier, tkClassIdentifier, tkReservedWord);
  TToken = class
    Lexeme       : string;
    Kind         : TTokenKind;
    RealValue    : double;
    IntegerValue : Int64;
  end;
  TSetChar = set of char;
  TScanner = class
  private
    Buf : array[1..32768] of char;
    Arq : text;
    SourceName,
    Line         : string;
    FToken       : TToken;
    CommentStyle : char;
    FEndSource   : boolean;
    LenLine      : integer;
    procedure NextChar(C: char);
    procedure FindEndComment(EndComment: string);
  protected
    FLineNumber, Top, First, FErrors, FMaxErrors : integer;
    procedure ScanChars(Chars: array of TSetChar; Tam : array of integer; Optional : boolean = false);
    procedure NextToken;
  public
    constructor Create(Source : string; MaxErrors : integer);
    destructor Destroy; override;
    procedure Error(Msg : string);
    procedure ErrorExpected(Expected, Found : string);
    procedure MatchToken(TokenExpected : string);
    procedure MatchTerminal(KindExpected : TTokenKind);
    property LineNumber : integer read FLineNumber;
    property ColNumber : integer read First;
    property Token : TToken read FToken;
    property EndSource : boolean read FEndSource;
    property Errors : integer read FErrors;
  end;

implementation

uses
  SysUtils;

const
  ReservedWords = '.and.array.as.asm.begin.case.class.const.constructor.destructor.dispinterface.div.do.downto.else.end.except.exports.'+
    'false.file.finalization.finally.for.function.goto.if.implementation.in.inherited.initialization.inline.interface.is.label.library.mod.nil.'+
    'not.object.of.or.out.packed.procedure.program.property.raise.record.repeat.resourcestring.set.shl.shr.then.threadvar.to.try.true.type.unit.'+
    'until.uses.var.while.with.xor.';
  Kinds : array[TTokenKind] of string = ('Undefined', 'Identifier', 'String Constant', 'Integer Constant', 'Real Constant', 'Constant Expression',
     'Label Identifier', 'Type Identifier', 'Class Identifier', 'Reserved Word');

constructor TScanner.Create(Source: string; MaxErrors : integer); begin
  SourceName := Source;
   if FileExists(Source) then begin
     assign(Arq, Source);
     SetTextBuf(Arq, Buf);
     reset(Arq);
     First := 1;
     FMaxErrors := MaxErrors;
     FToken := TToken.Create;
     NextToken;
   end
   else begin
     FEndSource := true;
     Error('Source file ''' + Source + ''' not found');
   end;
end;

destructor TScanner.Destroy; begin
  inherited;
  FToken.Free;
  close(Arq)
end;

procedure TScanner.ScanChars(Chars : array of TSetChar; Tam : array of integer; Optional : boolean = false);
var
  I, T, Last : integer;
begin
  FToken.Lexeme := '';
  FToken.Kind   := tkUndefined;
  for I := 0 to high(Chars) do begin
    Last := First;
    T    := 1;
    while (Last <= LenLine) and (T <= Tam[I]) and (Line[Last] in Chars[I]) do begin
      inc(Last);
      inc(T);
    end;
    if Last > First then begin
      FToken.Lexeme := FToken.Lexeme + copy(Line, First, Last - First);
      First := Last;
    end
    else
      if Optional then exit;
  end;
end;

procedure TScanner.NextChar(C : char); begin
  if (First < length(Line)) and (Line[First + 1] = C) then begin
    FToken.Lexeme := copy(Line, First, 2);
    inc(First, 2);
  end
  else begin
    FToken.Lexeme := Line[First];
    inc(First);
  end;
  FToken.Kind := tkUndefined;
end;

procedure TScanner.FindEndComment(EndComment : string);
var
  PosEnd : integer;
begin
  PosEnd := pos(EndComment, Line);
  if PosEnd <> 0 then begin // End comment in same line
    CommentStyle := #0;
    First := PosEnd + length(EndComment);
  end
  else
    First := MAXINT;
end;

// Implementar {$, (*$
procedure TScanner.NextToken;
var
  Str : string;
begin
  while not FEndSource do begin
    while First > LenLine do begin
      readln(Arq, Line);
      inc(FLineNumber);
      LenLine := length(Line);
      FEndSource := EOF(Arq) and (LenLine = 0);
      if FEndSource then exit;
      First := 1;
    end;
    // End comment across many lines
    case CommentStyle of
      #0  : ;
      '{' : begin FindEndComment('}');  continue; end;
      '*' : begin FindEndComment('*)'); continue; end;
    end;
    case Line[First] of
      ' ', #9 : begin // SkipBlank
        inc(First);
        while (First <= LenLine) and (Line[First] in [' ', #9]) do inc(First);
      end;
      'A'..'Z', '_', 'a'..'z' : begin // Identifiers
        ScanChars([['A'..'Z', 'a'..'z', '_', '0'..'9']], [255]);
        if (length(FToken.Lexeme) < 2) or (pos('.' + LowerCase(FToken.Lexeme) + '.', ReservedWords) = 0) then
          FToken.Kind := tkIdentifier
        else
          FToken.Kind := tkReservedWord;
        exit;
      end;
      ';', ',', '=', ')', '[', ']', '+', '-', '^', '@' : begin
        FToken.Lexeme := Line[First];
        FToken.Kind   := tkUndefined;
        inc(First);
        exit;
      end;
      '''': begin // strings
        Str := '';
        repeat
          inc(First);
          ScanChars([[#0..#255] - [''''], ['''']], [500, 1]);
          Str := Str + FToken.Lexeme;
          repeat
            ScanChars([['^'], ['@'..'Z']], [1, 1], true);
            if length(FToken.Lexeme) = 2 then
              Str := copy(Str, 1, length(Str)-1) + char(byte(FToken.Lexeme[2]) - ord('@')) + '''';
          until FToken.Lexeme = '';
          repeat
            ScanChars([['#'], ['0'..'9']], [1, 3], true);
            if length(FToken.Lexeme) >= 2 then
              Str := copy(Str, 1, length(Str)-1) + char(StrToIntDef(copy(FToken.Lexeme, 2, 100), 0)) + '''';
          until FToken.Lexeme = '';
        until Line[First] <> '''';
        FToken.Lexeme := copy(Str, 1, length(Str)-1);
        FToken.Kind   := tkStringConstant;
        exit;
      end;
      '0'..'9': begin // Numbers
        ScanChars([['0'..'9'], ['.'], ['0'..'9'], ['E', 'e'], ['+', '-'], ['0'..'9']], [28, 1, 27, 1, 1, 3], true);
        FToken.Lexeme := UpperCase(FToken.Lexeme);
        if FToken.Lexeme[length(FToken.Lexeme)] in ['.', 'E', '+', '-'] then begin
          dec(First);
          SetLength(FToken.Lexeme, length(FToken.Lexeme)-1);
        end;
        if (pos('.', FToken.Lexeme) <> 0) or (pos('E', FToken.Lexeme) <> 0) then
          FToken.Kind := tkRealConstant
        else
          if length(FToken.Lexeme) > 18 then
            FToken.Kind := tkRealConstant
          else
            FToken.Kind := tkIntegerConstant;
        if FToken.Kind = tkRealConstant then
          FToken.RealValue := StrToFloat(FToken.Lexeme)
        else
          FToken.IntegerValue := StrToInt(FToken.Lexeme);
        exit;
      end;
      '(' :
        if (length(Line) > First) and (Line[First + 1] = '*') then begin // Comment Style (*
          CommentStyle := '*';
          FindEndComment('*)');
        end
        else begin
          FToken.Lexeme := '(';
          FToken.Kind   := tkUndefined;
          inc(First);
          exit
        end;
      '/' :
        if (length(Line) > First) and (Line[First + 1] = '/') then // Comment Style //
          First := MAXINT
        else begin
          FToken.Lexeme := '/';
          FToken.Kind   := tkUndefined;
          inc(First);
          exit
        end;
      '{' : begin CommentStyle := '{'; FindEndComment('}'); end;
      '.' : begin NextChar('.'); exit; end;
      '>',
      '<',
      ':' : begin NextChar('='); exit; end;
      '*' : begin NextChar('*'); exit; end;
      '#' : begin
        ScanChars([['#'], ['0'..'9']], [1, 5]);
        if (FToken.Lexeme = '#') and (Line[First] = '$') then begin
          ScanChars([['$'], ['0'..'9', 'A'..'F', 'a'..'f']], [1, 4]);
          FToken.Lexeme := char(StrToInt(FToken.Lexeme));
        end
        else
          FToken.Lexeme := char(StrToInt(copy(FToken.Lexeme, 2, 5)));
        FToken.Kind := tkStringConstant;
        exit;
      end;
      '$' : begin // Hexadecimal
        ScanChars([['$'], ['0'..'9', 'A'..'F', 'a'..'f']], [1, 16]);
        FToken.Kind := tkIntegerConstant;
        FToken.IntegerValue := StrToInt(FToken.Lexeme);
        exit;
      end;
    else
      Error('Invalid character ''' + Line[First] + ''' ($' + IntToHex(ord(Line[First]), 4) + ')');
      First := MAXINT;
    end;
  end;
end;

procedure TScanner.Error(Msg : string); begin
  writeln('[Error] ' + ExtractFileName(SourceName) + '('+ IntToStr(LineNumber) + ', ' + IntToStr(ColNumber) + '): ' + Msg);
  inc(FErrors);
  if FErrors >= FMaxErrors then FEndSource := true;
end;

procedure TScanner.ErrorExpected(Expected, Found : string); begin
  Error('''' + Expected + ''' expected but ''' + Found + ''' found')
end;

procedure TScanner.MatchTerminal(KindExpected : TTokenKind);
var
  KindFound : TTokenKind;
begin
  KindFound := FToken.Kind;
  if KindExpected <> KindFound then begin
    ErrorExpected(Kinds[KindExpected], FToken.Lexeme);
    repeat // Recover from error
      NextToken
    until (KindExpected = FToken.Kind) or EndSource;
    inc(Top);
  end
  else
    NextToken
end;

procedure TScanner.MatchToken(TokenExpected : string); begin
  if TokenExpected <> UpperCase(FToken.Lexeme) then begin
    ErrorExpected(TokenExpected, FToken.Lexeme);
    repeat // Recover from error
      NextToken
    until (TokenExpected = UpperCase(FToken.Lexeme)) or EndSource;
    inc(Top);
  end
  else
    NextToken;
end;

end.
