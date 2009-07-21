unit epSupportGDB;

interface

function Length(A : pchar): integer; cdecl; 
function Equal(A, B : pchar): boolean; cdecl; 
function Diff(A, B : pchar): boolean; cdecl;
function Pos(A, B : pchar): integer; cdecl;

implementation

function Length(A : pchar): integer; cdecl; begin
  Result := system.length(ansistring(A)) 
end;
  
function Equal(A, B : pchar): boolean; cdecl; begin
  Result := ansistring(A) = ansistring(B)
end;

function Diff(A, B : pchar): boolean; cdecl; begin
  Result := ansistring(A) <> ansistring(B)
end;

function Pos(A, B : pchar): integer; cdecl; begin
	Result := system.pos(ansistring(A), ansistring(B))
end;

begin
  Length(nil);
  Equal(nil, nil); // force link functions
  Diff(nil, nil);
  Pos(nil, nil);
end.
