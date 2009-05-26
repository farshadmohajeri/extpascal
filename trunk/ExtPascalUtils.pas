{
Unit for complementary functions
Author: Wanderlan Santos dos Anjos (wanderlan.anjos@gmail.com)
Date: jul-2008
License: BSD<extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
unit ExtPascalUtils;

interface

uses
  Classes, TypInfo;

const
  ExtPascalVersion = '0.9.5';

{$IF (Defined(FPC) and not(Defined(FPC2_2_4) or Defined(FPC2_3_1))) or (RTLVersion <= 17)}
type
  // Implements StrictDelimiter property for FPC 2.2.2, Delphi 7 and older versions
  TStringList = class(Classes.TStringList)
  private
    function GetDelimitedText : string;
    procedure SetDelimitedText(const AValue : string);
  public
    StrictDelimiter : boolean; // Missing property in FPC, Delphi 7 an older versions
    property DelimitedText : string read GetDelimitedText write SetDelimitedText; // Property override for FPC, Delphi 7 an older versions
  end;
{$IFEND}

type
  TCSSUnit = (cssPX, cssPerc, cssEM, cssEX, cssIN, cssCM, cssMM, cssPT, cssPC, cssnone);

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
function Explode(Delim : char; const S : string) : TStringList;

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
Returns the number of occurrences of Substr in Str
@param Substr String to count in Str
@param Str String where the counting will be done
}
function CountStr(const Substr, Str : string) : integer;

// Replaces " to ' and ^M^J to <br/> and surrounds the string with "
function StrToJS(const S : string) : string;

{
Finds S in Cases array, returning its index or -1 if not found. Good to use in Pascal "case" command
@param S String to find in Cases array
@param Cases String array where to search
}
function CaseOf(const S : string; const Cases : array of string) : integer;

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

function Before(const BeforeS, AfterS, S : string) : boolean;
function IsUpperCase(S : string) : boolean;

implementation

uses
  StrUtils, SysUtils;

{$IF (Defined(FPC) and not(Defined(FPC2_2_4) or Defined(FPC2_3_1))) or (RTLVersion <= 17)}
function TStringList.GetDelimitedText: string;
var
  I : integer;
  P : pchar;
begin
  {$IFDEF FPC}CheckSpecialChars;{$ENDIF}
  Result := '';
  for I := 0 to Count-1 do begin
    P := pchar(Strings[I]);
    if not StrictDelimiter then
      while not(P^ in [#0..' ', QuoteChar, Delimiter]) do inc(P)
    else
      while not(P^ in [QuoteChar, Delimiter]) do inc(P);
    // strings in list may to contain #0
    if P <> (pchar(Strings[I]) + length(Strings[I])) then
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
  {$IFDEF FPC}CheckSpecialChars;{$ENDIF}
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

function Explode(Delim : char; const S : string) : TStringList;
var
  I : integer;
begin
  Result := TStringList.Create;
  Result.StrictDelimiter := true;
  Result.Delimiter := Delim;
  Result.DelimitedText := S;
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

function CountStr(const Substr, Str : string) : integer;
var
  I : integer;
begin
  I := 0;
  Result := 0;
  repeat
    I := PosEx(Substr, Str, I+1);
    if I <> 0 then inc(Result);
  until I = 0;
end;

function StrToJS(const S : string) : string; begin
  Result := AnsiReplaceStr(AnsiReplaceStr(S, '"', ''''), ^M^J, '<br/>');
  if (Result <> '') and (Result[1] = #3) then begin // RegEx
    delete(Result, 1, 1);
    if Pos('/', Result) <> 1 then Result := '/' + Result + '/';
  end
  else
    if not((length(Result) > 1) and (Result[1] = '%') and (Result[2] in ['0'..'9'])) then
      Result := '"' + Result + '"'
end;

function CaseOf(const S : string; const Cases : array of string) : integer; begin
  for Result := 0 to high(Cases) do
    if SameText(S, Cases[Result]) then exit;
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
  Result := Format('%s%d%3:s %2:d%3:s', [IfThen(Header, 'padding:', ''), Top, Right, EnumToJSString(TypeInfo(TCSSUnit), ord(CSSUnit))]);
  if Bottom <> -1 then
    Result := Result + Format('%d%2:s %1:d%2:s', [Bottom, Left, EnumToJSString(TypeInfo(TCSSUnit), ord(CSSUnit))]);
end;

function SetMargins(Top : integer; Right : integer = 0; Bottom : integer = 0; Left : integer = 0; CSSUnit : TCSSUnit = cssNone;
  Header : boolean = false) : string;
begin
  Result := Format('%s%d%5:s %2:d%5:s %3:d%5:s %4:d%s', [IfThen(Header, 'margin:', ''), Top, Right, Bottom, Left,
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
end.
