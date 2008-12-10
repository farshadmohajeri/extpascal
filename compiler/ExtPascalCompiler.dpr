program ExtPascalCompiler;
{
Author: Wanderlan Santos dos Anjos, wanderlan.anjos@gmail.com
Date: dec-2008
License: <extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
Based on Dragon Book
}
uses
  Parser;

begin
  with TParser.Create(ParamStr(1)) do Compile;
  readln;
end.

