program TestExtPascal;

{$APPTYPE CONSOLE}

uses
  ExtPascal, Ext, ExtGlobal, ExtUtil, ExtAir, ExtData, ExtDD, ExtForm, ExtLayout, ExtMenu, ExtState, ExtTree, ExtGrid;

var
  W : ExtWindow;
begin
  W := ExtWindow.Create;
  with W do begin
    layout := 'fit';
    width := 300;
    height:= 150;
    closable := false;
    resizable := false;
    plain := true;
    items := 'dsdsd';
    show;
  end;
end.

