unit ExtPascalUtils;

interface

uses
  Classes;

// Mimics preg_match php function
function Extract(Delims : array of string; S : string; var Matches : TStringList) : boolean;
// Mimics explode php function
function Explode(Delim : char; S : string) : TStringList;

implementation

uses
  StrUtils, SysUtils;

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

