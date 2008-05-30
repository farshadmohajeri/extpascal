unit ExtPascalUtils;

interface

uses
  Classes;

{$IF Defined(FPC) or (RTLVersion <= 17)}
type
  TStringList = class(Classes.TStringList)
  private
    function GetDelimitedText : string;
    procedure SetDelimitedText(const AValue : string);
  public
    StrictDelimiter : boolean;
    property DelimitedText : string read GetDelimitedText write SetDelimitedText;
  end;
{$IFEND}

// Mimics preg_match php function
function Extract(Delims : array of string; S : string; var Matches : TStringList) : boolean;
// Mimics explode php function
function Explode(Delim : char; S : string) : TStringList;

implementation

uses
  StrUtils, SysUtils;

{$IF Defined(FPC) or (RTLVersion <= 17)}
function TStringList.GetDelimitedText: string;
var
  I : integer;
  p : pchar;
begin
  {$IFDEF FPC}CheckSpecialChars;{$ENDIF}
  result:='';
  for i:=0 to count-1 do begin
    p := pchar(strings[i]);
    if not StrictDelimiter then
      while not(p^ in [#0..' ',QuoteChar,Delimiter]) do inc(p)
    else
      while not(p^ in [QuoteChar,Delimiter]) do inc(p);
    // strings in list may contain #0
    if p <> pchar(strings[i]) + length(strings[i]) then
      Result := Result + QuoteChar + Strings[I] + QuoteChar
    else
      Result := Result + strings[i];
    if I < Count-1 then Result:=Result+Delimiter;
  end;
  if (Length(Result)=0)and(count=1) then
    Result := QuoteChar + QuoteChar;
end;

procedure TStringList.SetDelimitedText(const AValue : string);
var
  I, J : integer;
  aNotFirst:boolean;
begin
  {$IFDEF FPC}CheckSpecialChars;{$ENDIF}
  BeginUpdate;
  i := 1;
  aNotFirst := false;
  try
    Clear;
    while i<=length(AValue) do begin
      // skip delimiter
      if aNotFirst and (i<=length(AValue)) and (AValue[i]=Delimiter) then inc(i);
      // skip spaces
      if not StrictDelimiter then
        while (i<=length(AValue)) and (Ord(AValue[i])<=Ord(' ')) do inc(i);
      // read next string
      if i<=length(AValue) then begin
        if AValue[i]=QuoteChar then begin
          // next string is quoted
          j:=i+1;
          while (j<=length(AValue)) and ((AValue[j]<>QuoteChar) or
               ((j+1<=length(AValue)) and (AValue[j+1]=QuoteChar))) do
            if (j<=length(AValue)) and (AValue[j]=QuoteChar) then
              inc(j,2)
            else
              inc(j);
          // j is position of closing quote
          Add(StringReplace(Copy(AValue,i+1,j-i-1), QuoteChar+QuoteChar,QuoteChar, [rfReplaceAll]));
          i:=j+1;
        end
        else begin
          // next string is not quoted
          j:=i;
          if not StrictDelimiter then
            while (j<=length(AValue)) and (Ord(AValue[j])>Ord(' ')) and (AValue[j]<>Delimiter) do inc(j)
          else
            while (j<=length(AValue)) and (AValue[j]<>Delimiter) do inc(j);
          Add(Copy(AValue,i,j-i));
          i:=j;
        end;
      end
      else
        if aNotFirst then Add('');
      // skip spaces
      if not StrictDelimiter then
        while (i<=length(AValue)) and (Ord(AValue[i])<=Ord(' ')) do inc(i);
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

end.

