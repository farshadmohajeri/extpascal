program ExtPascalCompiler;
{
Author: Wanderlan Santos dos Anjos, wanderlan.anjos@gmail.com
Date: jan-2010
License: <extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
Based on Dragon Book
}
{$APPTYPE CONSOLE}

uses
  Parser;

begin
  writeln('LLVM-Pascal Version 0.1');
  writeln('(c) 2010 by Wanderlan Santos dos Anjos, BSD license');
  writeln('http://extpascal.googlecode.com/'^M^J);
  with TParser.Create(ParamStr(1), 10) do Compile;
  readln;
end.
