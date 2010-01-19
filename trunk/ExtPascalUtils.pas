{
Unit for complementary functions
Author: Wanderlan Santos dos Anjos (wanderlan.anjos@gmail.com)
Date: jul-2008
License: BSD<extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
unit ExtPascalUtils;

{$IFDEF FPC}{$MACRO ON}{$MODE DELPHI}{$ENDIF}

interface

uses
  Classes, TypInfo;

const
  ExtPascalVersion = '0.9.8';
  IISDelim = '`'; // For CGI IIS bug

{$IF not Defined(FPC) and (RTLVersion <= 17)}
type
  // Implements StrictDelimiter property for FPC 2.2.2, Delphi 7 and older versions
  TStringList = class(Classes.TStringList)
  private
    function GetDelimitedText : string;
    procedure SetDelimitedText(const AValue : string);
  public
    StrictDelimiter : boolean; // Missing property in FPC 2.2.2, Delphi 7 an older versions
    property DelimitedText : string read GetDelimitedText write SetDelimitedText; // Property override for FPC 2.2.2, Delphi 7 an older versions
  end;
{$IFEND}

type
  TBrowser      = (brUnknown, brIE, brFirefox, brChrome, brSafari, brOpera, brKonqueror); // Internet Browsers
  TCSSUnit      = (cssPX, cssPerc, cssEM, cssEX, cssIN, cssCM, cssMM, cssPT, cssPC, cssnone); // HTML CSS units
  TExtProcedure = procedure of object; // Defines a procedure than can be called by a <link TExtObject.Ajax, AJAX> request

{
Mimics preg_match php function. Searches S for a match to delimiter strings given in Delims parameter
@param Delims Delimiter strings to match
@param S Subject string
@param Matches Substrings from Subject string delimited by Delimiter strings. <b>Matches (TStringList) should already be created</b>.
@param Remove matches strings from S, default is true
@return True if some match hit, false otherwise
}
function Extract(const Delims : array of string; var S : string; var Matches : TStringList; Remove : boolean = true) : boolean;

{
Mimics explode php function.
Creates a TStringList where each string is a substring formed by the splitting of S string through delimiter Delim.
@param Delim Delimiter used to split the string
@param S Source string to split
@return TStringList created with substrings from S
}
function Explode(Delim : char; const S : string; Separator : char = '=') : TStringList;

{
The opposite of LastDelimiter RTL function.
Returns the index of the first occurence in a string of the characters specified.
If none of the characters in Delimiters appears in string S, function returns zero.
@param Delimiters String where each character is a valid delimiter.
@param S String to search for delimiters.
@param Offset Index from where the search begins.
}
function FirstDelimiter(const Delimiters, S : string; Offset : integer = 1) : integer;

// The opposite of "StrUtils.PosEx" function. Returns the index value of the last occurrence of a specified substring in a given string.
function RPosEx(const Substr, Str : string; Offset : integer = 1) : integer;

{
Returns the number of occurrences of Substr in Str until UntilStr occurs
@param Substr String to count in Str
@param Str String where the counting will be done
@param UntilStr Optional String, stop counting if this string occurs
}
function CountStr(const Substr, Str : string; UntilStr : string = '') : integer;

{
Converts a string with param place holders to a JavaScript string. Converts a string representing a regular expression to a JavaScript RegExp.
Replaces " to ', ^M^J to <br/> and isolated ^M or ^J to <br/>, surrounds the string with " and insert %0..%9 JS place holders.
When setting a TExtFormTextField value (in property setter setvalue), the UseBR should be set to false,
because otherwise it is impossible to display multiline text in a TExtFormTextArea.
@param S Source string with param place holders or RegExpr
@param UseBR If true uses replace ^M^J to <br/> else to \n
@return a well formatted JS string
}
function StrToJS(const S : string; UseBR : boolean = true) : string;

{
Finds S string in Cases array, returning its index or -1 if not found. Good to use in Pascal "case" command. Similar to AnsiIndexText.
@param S Source string where to search
@param Cases String array to find in S
}
function CaseOf(const S : string; const Cases : array of string) : integer;

{
Finds Cases array in S string, returning its index or -1 if not found. Good to use in Pascal "case" command. Reverse to AnsiIndexStr.
@param S string to find in Cases array
@param Cases String array where to search
}
function RCaseOf(const S : string; const Cases : array of string) : integer;

{
Converts a Pascal enumerated type constant into a JS string, used internally by ExtToPascal wrapper. See ExtFixes.txt for more information.
@param TypeInfo Type information record that describes the enumerated type, use TypeInfo() function with enumerated type
@param Value The enumerated value, represented as an integer
@return JS string
}
function EnumToJSString(TypeInfo : PTypeInfo; Value : integer) : string;

{
Helper function to make code more pascalish, use
@example <code>BodyStyle := SetPaddings(10, 15);</code>
instead
@example <code>BodyStyle := 'padding:10px 15px';</code>
}
function SetPaddings(Top : integer; Right : integer = 0; Bottom : integer = -1; Left : integer = 0; CSSUnit : TCSSUnit = cssPX;
  Header : boolean = true) : string;

{
Helper function to make code more pascalish, use
@example <code>Margins := SetMargins(3, 3, 3);</code>
instead
@example <code>Margins := '3 3 3 0';</code>
}
function SetMargins(Top : integer; Right : integer = 0; Bottom : integer = 0; Left : integer = 0; CSSUnit : TCSSUnit = cssNone;
  Header : boolean = false) : string;

// Returns true if BeforesS string occurs before AfterS string in S string
function Before(const BeforeS, AfterS, S : string) : boolean;

// Returns true if all chars in S are uppercase
function IsUpperCase(S : string) : boolean;

// Beautify generated JS commands from ExtPascal, automatically used when DEBUGJS symbol is defined
function BeautifyJS(const AScript : string; const StartingLevel : integer = 0; SplitHTMLNewLine : boolean = true) : string;

// Beautify generated CSS from ExtPascal, automatically used when DEBUGJS symbol is defined
function BeautifyCSS(const AStyle : string) : string;

implementation

uses
  StrUtils, SysUtils, Math;

{$IF not Defined(FPC) and (RTLVersion <= 17)}
function TStringList.GetDelimitedText: string;
var
  I : integer;
  P : pchar;
begin
  Result := '';
  for I := 0 to Count-1 do begin
    P := pchar(Strings[I]);
    if not StrictDelimiter then
      while not(P^ in [#0..' ', QuoteChar, Delimiter]) do inc(P)
    else
      while not(P^ in [#0, Delimiter]) do inc(P);
    // strings in list may to contain #0
    if (P <> pchar(Strings[I]) + length(Strings[I])) and not StrictDelimiter then
      Result := Result + QuoteChar + Strings[I] + QuoteChar
    else
      Result := Result + Strings[I];
    if I < Count-1 then Result := Result + Delimiter;
  end;
  if (length(Result) = 0) and (Count = 1) then Result := QuoteChar + QuoteChar;
end;

procedure TStringList.SetDelimitedText(const AValue : string);
var
  I, J : integer;
  aNotFirst : boolean;
begin
  BeginUpdate;
  I := 1;
  aNotFirst := false;
  try
    Clear;
    while I <= length(AValue) do begin
      // skip delimiter
      if aNotFirst and (I <= length(AValue)) and (AValue[I] = Delimiter) then inc(I);
      // skip spaces
      if not StrictDelimiter then
        while (I <= length(AValue)) and (ord(AValue[I]) <= ord(' ')) do inc(I);
      // read next string
      if I <= length(AValue) then begin
        if AValue[I] = QuoteChar then begin
          // next string is quoted
          J := I + 1;
          while (J <= length(AValue)) and ((AValue[J] <> QuoteChar) or
             ((J+1 <= length(AValue)) and (AValue[J+1] = QuoteChar))) do
            if (J <= length(AValue)) and (AValue[J] = QuoteChar) then
              inc(J, 2)
            else
              inc(J);
          // J is position of closing quote
          Add(StringReplace(Copy(AValue, I+1, J-I-1), QuoteChar + QuoteChar, QuoteChar, [rfReplaceAll]));
          I := J + 1;
        end
        else begin
          // next string is not quoted
          J := I;
          if not StrictDelimiter then
            while (J <= length(AValue)) and (ord(AValue[J]) > ord(' ')) and (AValue[J] <> Delimiter) do inc(J)
          else
            while (J <= length(AValue)) and (AValue[J] <> Delimiter) do inc(J);
          Add(copy(AValue, I, J-i));
          I := J;
        end;
      end
      else
        if aNotFirst then Add('');
      // skip spaces
      if not StrictDelimiter then
        while (I <= length(AValue)) and (ord(AValue[I]) <= ord(' ')) do inc(I);
      aNotFirst:=true;
    end;
  finally
    EndUpdate;
  end;
end;
{$IFEND}

function Extract(const Delims : array of string; var S : string; var Matches : TStringList; Remove : boolean = true) : boolean;
var
  I, J : integer;
  Points : array of integer;
begin
  Result := false;
  if Matches <> nil then Matches.Clear;
  SetLength(Points, length(Delims));
  J := 1;
  for I := 0 to high(Delims) do begin
    J := PosEx(Delims[I], S, J);
    Points[I] := J;
    if J = 0 then
      exit
    else
      inc(J, length(Delims[I]));
  end;
  for I := 0 to high(Delims)-1 do begin
    J := Points[I] + length(Delims[I]);
    Matches.Add(trim(copy(S, J, Points[I+1]-J)));
  end;
  if Remove then S := copy(S, Points[high(Delims)] + length(Delims[high(Delims)]), length(S));
  Result := true
end;

function Explode(Delim : char; const S : string; Separator : char = '=') : TStringList;
var
  I : integer;
begin
  Result := TStringList.Create;
  Result.StrictDelimiter := true;
  Result.Delimiter := Delim;
  Result.DelimitedText := S;
  Result.NameValueSeparator := Separator;
  for I := 0 to Result.Count-1 do Result[I] := trim(Result[I]);
end;

function FirstDelimiter(const Delimiters, S : string; Offset : integer = 1) : integer;
var
  I : integer;
begin
  for Result := Offset to length(S) do
    for I := 1 to length(Delimiters) do
      if Delimiters[I] = S[Result] then exit;
  Result := 0;
end;

function RPosEx(const Substr, Str : string; Offset : integer = 1) : integer;
var
  I : integer;
begin
  Result := PosEx(Substr, Str, Offset);
  while Result <> 0 do begin
    I := PosEx(Substr, Str, Result+1);
    if I = 0 then
      break
    else
      Result := I
  end;
end;

function CountStr(const Substr, Str : string; UntilStr : string = '') : integer;
var
  I, J : integer;
begin
  I := 0;
  Result := 0;
  J := Pos(UntilStr, Str);
  repeat
    I := PosEx(Substr, Str, I+1);
    if (J <> 0) and (J < I) then exit;
    if I <> 0 then inc(Result);
  until I = 0;
end;

function StrToJS(const S : string; UseBR : boolean = true) : string;
var
  I, J : integer;
  BR   : string;
begin
  BR := IfThen(UseBR, '<br/>', '\n');
  Result := AnsiReplaceStr(AnsiReplaceStr(S, '"', ''''), ^M^J, BR);
  Result := AnsiReplaceStr(Result, ^M, BR);
  Result := AnsiReplaceStr(Result, ^J, BR);
  if (Result <> '') and (Result[1] = #3) then begin // Is RegEx
    delete(Result, 1, 1);
    if Pos('/', Result) <> 1 then Result := '/' + Result + '/';
  end
  else begin
    I := pos('%', Result);
    if (pos(';', Result) = 0) and (I <> 0) and ((length(Result) > 1) and (Result[I+1] in ['0'..'9'])) then begin // Has param place holder, ";" disable place holder
      J := FirstDelimiter(' "''[]{}><=!*-+/,', Result, I+2);
      if J = 0 then J := length(Result)+1;
      if J <> (length(Result)+1) then begin
        insert('+"', Result, J);
        Result := Result + '"';
      end;
      if I <> 1 then begin
        insert('"+', Result, I);
        Result := '"' + Result;
      end;
    end
    else
      if (I = 1) and (length(Result) > 1) and (Result[2] in ['a'..'z', 'A'..'Z']) then
        Result := copy(Result, 2, length(Result))
      else
        Result := '"' + Result + '"'
  end;
end;

function CaseOf(const S : string; const Cases : array of string) : integer; begin
  for Result := 0 to high(Cases) do
    if SameText(S, Cases[Result]) then exit;
  Result := -1;
end;

function RCaseOf(const S : string; const Cases : array of string) : integer; begin
  for Result := 0 to high(Cases) do
    if pos(Cases[Result], S) <> 0 then exit;
  Result := -1;
end;

function EnumToJSString(TypeInfo : PTypeInfo; Value : integer) : string;
var
  I : integer;
  JS: string;
begin
  Result := '';
  JS := GetEnumName(TypeInfo, Value);
  for I := 1 to length(JS) do
    if JS[I] in ['A'..'Z'] then begin
      Result := LowerCase(copy(JS, I, 100));
      if Result = 'perc' then Result := '%';
      exit
    end;
end;

function SetPaddings(Top : integer; Right : integer = 0; Bottom : integer = -1; Left : integer = 0; CSSUnit : TCSSUnit = cssPX;
  Header : boolean = true) : string;
begin
  Result := Format('%s%d%3:s %2:d%3:s', [IfThen(Header, 'padding: ', ''), Top, Right, EnumToJSString(TypeInfo(TCSSUnit), ord(CSSUnit))]);
  if Bottom <> -1 then
    Result := Result + Format(' %d%2:s %1:d%2:s', [Bottom, Left, EnumToJSString(TypeInfo(TCSSUnit), ord(CSSUnit))]);
end;

function SetMargins(Top : integer; Right : integer = 0; Bottom : integer = 0; Left : integer = 0; CSSUnit : TCSSUnit = cssNone;
  Header : boolean = false) : string;
begin
  Result := Format('%s%d%5:s %2:d%5:s %3:d%5:s %4:d%s', [IfThen(Header, 'margin: ', ''), Top, Right, Bottom, Left,
    EnumToJSString(TypeInfo(TCSSUnit), ord(CSSUnit))])
end;

function Before(const BeforeS, AfterS, S : string) : boolean;
var
  I : integer;
begin
  I := pos(BeforeS, S);
  Result := (I <> 0) and (I < pos(AfterS, S))
end;

function IsUpperCase(S : string) : boolean;
var
  I : integer;
begin
  Result := false;
  for I := 1 to length(S) do
    if S[I] in ['a'..'z'] then exit;
  Result := true;
end;

function SpaceIdents(const aLevel: integer; const aWidth: string = '   '): string;
var
  c: integer;
begin
  Result := '';
  if aLevel < 1 then Exit;
  for c := 1 to aLevel do Result := Result + aWidth;
end;

function MinValueOf(Values : array of integer; const MinValue : integer = 0) : integer;
var
  I : integer;
begin
  for I := 0 to High(Values) do
    if Values[I] <= MinValue then Values[I] := MAXINT;
  Result := MinIntValue(Values);
  // if all are the minimum value then return 0
  if Result = MAXINT then Result := MinValue;
end;

function BeautifyJS(const AScript : string; const StartingLevel : integer = 0; SplitHTMLNewLine : boolean = true) : string;
var
  pBlockBegin, pBlockEnd, pPropBegin, pPropEnd, pStatEnd, {pFuncBegin,} pSqrBegin, pSqrEnd,
  pFunction, pString, pOpPlus, pOpMinus, pOpTime, {pOpDivide,} pOpEqual, pRegex : integer;
  P, Lvl : integer;
  Res : string;

  function AddNewLine(const atPos : integer; const AddText : string) : integer; begin
    insert(^M^J + AddText, Res, atPos);
    Result := length(^M^J + AddText);
  end;

  function SplitHTMLString(AStart, AEnd : integer): integer;  // range is including the quotes
  var
    br,pe,ps: integer;
    s: string;
  begin
    Result := AEnd;
    s := copy(res, AStart, AEnd);
    // find html new line (increase verbosity)
    br := PosEx('<br>', res, AStart+1);
    pe := PosEx('</p>', res, AStart+1);
    ps := MinValueOf([br,pe]);
    // html new line is found
    // Result-5 is to skip the mark at the end of the line
    while (ps > 0) and (ps < Result-5) do begin
      s := '"+'^M^J+SpaceIdents(Lvl)+SpaceIdents(3,'  ')+'"';
      Insert(s, res, ps+4);
      Result := Result + length(s);
      // find next new line
      br := PosEx('<br>', res, ps+length(s)+4);
      pe := PosEx('</p>', res, ps+length(s)+4);
      ps := MinValueOf([br,pe]);
    end;
  end;

var
  Backward, onReady, inProp, inNew : boolean;
  LvlProp, i, j, k : integer;
begin
  // skip empty script
  if AScript = '' then exit;
  P := 1;
  Res := AScript;
  inNew := true;
  inProp := false;
  onReady := false;
  LvlProp := 1000; // max identation depth
  Lvl := StartingLevel;
  // remove space in the beginning
  if Res[1] = ' ' then Delete(Res, 1, 1);
  // proceed the whole generated script by scanning the text
  while p > 0 do begin
    // chars that will be processed (10 signs)
    inc(P);
    pString     := PosEx('"', Res, P);
    pOpEqual    := PosEx('=', Res, P);
    pOpPlus     := PosEx('+', Res, P);
    pOpMinus    := PosEx('-', Res, P);
    pOpTime     := PosEx('*', Res, P);
    //pOpDivide   := PosEx('/', Res, P);
    pBlockBegin := PosEx('{', Res, P);
    pBlockEnd   := PosEx('}', Res, P);
    pPropBegin  := PosEx(':', Res, P);
    pPropEnd    := PosEx(',', Res, P);
    pStatEnd    := PosEx(';', Res, P);
    //pFuncBegin  := PosEx('(', Res, P);
    pSqrBegin   := PosEx('[', Res, P);
    pSqrEnd     := PosEx(']', Res, P);
    pFunction   := PosEx('function', Res, P);
    pRegex      := PosEx('regex', Res, P);
    // process what is found first
    P := MinValueOf([pBlockBegin, pBlockEnd, pPropBegin, pPropEnd, pStatEnd, {pFuncBegin,} pSqrBegin, pSqrEnd,
                     pString, pOpEqual, pOpPlus, pOpMinus, pOpTime, {pOpDivide,} pFunction, pRegex]);
    // keep Ext's onReady function at the first line
    if (not onReady) and (P > 0) and (length(Res) >= P) and (res[p] = 'f') then
      if Copy(Res, P-9, 9) = '.onReady(' then begin
        onReady := true;
        continue;
      end;
    // now, let's proceed with what char is found
    if P > 0 then begin
      // reset inProp status based on minimum lvlProp
      if inProp then inProp := Lvl >= LvlProp; // or (lvl > StartingLevel);
      // process chars
      case res[p] of // skip string by jump to the next mark
        '"' :
          if Res[P+1] = '"' then // skip empty string
            inc(P)
          else
            if SplitHTMLNewLine then // proceed html string value
              P := SplitHTMLString(P, PosEx('"', Res, P+1))
            else // just skip the string
              P := PosEx('"', Res, P+1);
        '=', '+', '*', '/': begin // neat the math operator
          insert(' ', Res, P);   inc(P);
          if Res[P+1] = '=' then inc(P); // double equals
          insert(' ', Res, P+1); inc(P);
        end;
        '{' : // statement block begin
          if Res[P+1] = '}' then // skip empty statement
            inc(P)
          else begin
            inc(Lvl); // Increase identation level
            inProp := false;
            inc(P, AddNewLine(P+1, SpaceIdents(Lvl)));
          end;
        '}' : begin // statement block end
          // some pair values are treated specially: keep },{ pair intact to save empty lines
          if (length(Res) >= (P+2)) and (Res[P+1] = ',') and (Res[P+2] = '{') then begin
            dec(Lvl);
            inc(P, AddNewLine(P, SpaceIdents(Lvl)) + 2);
            inc(Lvl);
            inc(P, AddNewLine(P+1, SpaceIdents(Lvl)));
            continue;
          end;
          if not inNew then // special })] pair for items property group object ending
            inNew := (Res[P+1] = ')') and (Res[P+2] = ']');
          // common treatment for block ending
          dec(Lvl); // decrease identation level
          P := P + AddNewLine(P, SpaceIdents(lvl));
          // bring the following trails
          I := P;
          Backward := false;
          repeat
            inc(I);
            // find multiple statement block end
            if (length(Res) >= I) and (Res[I] in ['{', '}', ';']) then backward := true;
            if inNew and (length(Res) >= I) and (Res[I] = ']') then backward := true;
          until (I > length(Res)) or (Res[I] = ',') or backward;
          if not backward then // add new line
            inc(P, AddNewLine(i+1, SpaceIdents(Lvl)))
          else // suspend new line to proceed with next block
            P := i-1;
        end;
        ';' : begin // end of statement
          // fix to ExtPascal parser bug which become helpful, because it could be mark of new object creation
          if (length(Res) >= P+2) and (Res[P+1] = ' ') and (Res[P+2] = 'O') then begin  // ; O string
            inProp := false;
            delete(Res, P+1, 1);
            inc(P, AddNewLine(P+1, ^M^J+SpaceIdents(Lvl)));
            continue;
          end;
          if (length(Res) >= P+1) and (Res[P+1] = '}') then continue; // skip if it's already at the end of block
          if P = length(Res) then // skip identation on last end of statement
            inc(P, AddNewLine(P+1, SpaceIdents(StartingLevel-1)))
          else
            inc(P, AddNewLine(P+1, SpaceIdents(lvl)));
        end;
        '[' : begin // square declaration begin
          if Res[P+1] = '[' then begin // double square treat as sub level
            inc(Lvl);
            inc(P, AddNewLine(p+1, SpaceIdents(Lvl)));
            inProp := true;
            continue;
          end;
          // find special pair within square block
          i := PosEx(']', Res, P+1);
          j := PosEx('{', Res, P+1);
          k := PosEx('new ', Res, P+1);
          if (j > 0) and (j < i) then begin // new block found in property value
            inc(Lvl);
            // new object found in property value, add new line
            if (k > 0) and (k < i) then begin
              inNew := true;
              inc(P, AddNewLine(P+1, SpaceIdents(Lvl)));
            end
            else begin // move forward to next block beginning
              inNew := false;
              inc(J, AddNewLine(J+1, SpaceIdents(Lvl)));
              P := j-1;
            end;
          end
          else // no sub block found, move at the end of square block
            P := i;
        end;
        ']' : // square declaration end
          if Res[P-1] = ']' then begin // double square ending found, end sub block
            dec(Lvl);
            inc(P, AddNewLine(P, SpaceIdents(Lvl)));
          end
          else // skip processing if not part of square sub block
            if not inNew then
              continue
            else begin // end of block square items group
              dec(Lvl);
              inc(P, AddNewLine(P, SpaceIdents(Lvl)));
            end;
        (*'(' : begin // function declaration begin
          // check whether it contains string inside
          { TODO : currently this is a quick hack! beware! should be fixed soon! }
          if res[p+1] = '"' then continue;
          if inProp then begin // proceed only within property
            // skip function without sub block
            i := PosEx(')', res, p+1);
            p := PosEx('{', res, p+1);
            p := IfThen((p > 0) and (p < i), p-1, i);
          end;
        end; *)
        ':' : begin // property value begin
          if Res[P+1] <> ' ' then begin // separate name:value with a space
            insert(' ', Res, P+1);
            inc(P);
          end;
          inProp := true;
          if Lvl < LvlProp then LvlProp := Lvl; // get minimum depth level of property
        end;
        ',' : // property value end
          if inProp then inc(P, AddNewLine(P+1, SpaceIdents(Lvl)));
        'f' : begin // independent function definition
          if inProp then Continue; // skip function if within property
          if copy(Res, P, 8) = 'function' then // add new line for independent function
            inc(P, AddNewLine(P, SpaceIdents(Lvl)) + 7);
        end;
        'r' : begin
          P := PosEx('/', Res, P);
          P := PosEx('/', Res, P+1);
        end;
      end;
    end;
  end;
  Result := Res;
end;

function BeautifyCSS(const AStyle : string) : string;
var
  pOpen, pClose, pProp, pEnd, pString : integer;
  P, Lvl : integer;
  Res : string;
begin
  P := 1;
  Lvl := 0;
  Res := ^M^J+AStyle;
  while P > 0 do begin
    inc(P);
    pString := PosEx('''', Res, P);
    pOpen   := PosEx('{',  Res, P);
    pClose  := PosEx('}',  Res, P);
    pProp   := PosEx(':',  Res, P);
    pEnd    := PosEx(';',  Res, P);
    P := MinValueOf([pString, pOpen, pClose, pProp, pEnd]);
    if P > 0 then
      case Res[p] of
        '''' : P := PosEx('''', Res, P+1);
        '{' : begin
          Inc(lvl);
          if (res[p-1] <> ' ') then begin
            Insert(' ', res, p);
            p := p+1;
          end;
          Insert(^M^J+SpaceIdents(lvl), res, p+1);
          p := p + Length(^M^J+SpaceIdents(lvl));
        end;
        '}' : begin
          dec(lvl);
          insert(^M^J+SpaceIdents(lvl), Res, P);
          inc(P, length(^M^J+SpaceIdents(Lvl)));
          insert(^M^J+SpaceIdents(lvl), Res, P+1);
          inc(P, length(^M^J+SpaceIdents(Lvl)));
        end;
        ':' :
          if Res[P+1] <> ' ' then begin
            insert(' ', Res, P+1);
            inc(P);
          end;
        ';' : begin
          if Res[P+1] = '}' then continue;
          if Res[P+1] = ' ' then delete(Res, P+1, 1);
          insert(^M^J+SpaceIdents(Lvl), Res, P+1);
          inc(P, length(^M^J+SpaceIdents(Lvl)));
        end;
      end;
  end;
  Result := Res;
end;

end.
