{
Unit for complementary functions
Author: Wanderlan Santos dos Anjos (wanderlan.anjos@gmail.com)
Date: jul-2008
License: BSD<extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
unit ExtPascalUtils;

interface

uses
  Classes;

{$IF Defined(FPC) or (RTLVersion <= 17)}
type
  // Implements StrictDelimiter property for FPC, Delphi 7 and older versions
  TStringList = class(Classes.TStringList)
  private
    function GetDelimitedText : string;
    procedure SetDelimitedText(const AValue : string);
  public
    StrictDelimiter : boolean;
    property DelimitedText : string read GetDelimitedText write SetDelimitedText;
  end;
{$IFEND}

{
Mimics preg_match php function. Searches S for a match to delimiter strings given in Delims parameter
@param Delims Delimiter strings to match
@param S Subject string
@param Matches Substrings from Subject string delimited by Delimiter strings. <b>Matches (TStringList) should already be created</b>.
@return True if some match hit, false otherwise
}
function Extract(Delims : array of string; S : string; var Matches : TStringList) : boolean;

{
Mimics explode php function.
Creates a TStringList where each string is a substring formed by the splitting of S string through delimiter Delim.
@param Delim Delimiter used to split the string
@param S Source string to split
@return TStringList created with substrings from S
}
function Explode(Delim : char; S : string) : TStringList;

{
Opposite of LastDelimiter RTL function.
Returns the index of the first occurence in a string of the characters specified.
If none of the characters in Delimiters appears in string S, function returns zero.
@param Delimiters String where each character is a valid delimiter.
@param S String to search for delimiters.
@param Offset Index from where the search begins.
}
function FirstDelimiter(Delimiters, S : string; Offset : integer = 1) : integer;

implementation

uses
  StrUtils, SysUtils;

{$IF Defined(FPC) or (RTLVersion <= 17)}
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

function Extract(Delims : array of string; S : string; var Matches : TStringList) : boolean;
var
  I, J : integer;
begin
  Result := false;
  if Matches <> nil then Matches.Clear;
  J := 1;
  for I := 0 to high(Delims) do begin
    J := posex(Delims[I], S, J);
    if J = 0 then
      exit
    else
      inc(J, length(Delims[I]));
  end;
  J := 1;
  for I := 0 to high(Delims)-1 do begin
    J := posex(Delims[I], S, J);
    inc(J, length(Delims[I]));
    Matches.Add(trim(copy(S, J, posex(Delims[I+1], S, J)-J)));
  end;
  Result := true
end;

function Explode(Delim : char; S : string) : TStringList;
var
  I : integer;
begin
  Result := TStringList.Create;
  Result.StrictDelimiter := true;
  Result.Delimiter := Delim;
  Result.DelimitedText := S;
  for I := 0 to Result.Count-1 do Result[I] := trim(Result[I]);
end;

function FirstDelimiter(Delimiters, S : string; Offset : integer = 1) : integer;
var
  I : integer;
begin
  for Result := Offset to length(S) do
    for I := 1 to length(Delimiters) do
      if Delimiters[I] = S[Result] then exit;
  Result := 0;
end;

end.

