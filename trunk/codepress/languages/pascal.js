// CodePress regex for Object Pascal(Delphi or FPC)
// by Wanderlan Santos dos Anjos
Language.syntax=[
// Strings
  {input:/\'(.*?)(\'|<br>|<\/br>|<\/P>|\n)/gi,output:"<s>'$1'</s>"},
// Reserved words
  {input:/\b(absolute|abstract|and|array|as|asm|begin|case|cdecl|class|const|constructor|destructor|div|do|downto|else|end|except|exports|external|file|finalization|finally|for|forward|function|goto|if|implementation|in|index|inherited|initialization|inline|interface|is|label|library|mod|nil|not|object|of|on|or|out|operator|override|packed|pascal|procedure|program|property|private|protected|published|public|raise|record|register|reintroduce|repeat|resourcestring|safecall|set|shl|shr|stdcall|string|then|threadvar|to|try|type|unit|until|uses|var|virtual|while|with|xor)\b/gi,output:'<b>$1</b>'},
// Numbers
  {input:/\b([+-]?[\d\.]+)\b/g,output:'<a>$1</a>'},
// Hexa numbers
  {input:/\b($[\dABCDEF]+)\b/gi,output:'<a>$1</a>'},
// Chars hexa
  {input:/\b(#$[\dABCDEF]+)\b/gi,output:'<s>$1</s>'},
// Chars
  {input:/\b(#\d+)\b/g,output:'<s>$1</s>'},
// Comments //
  {input:/([^:]|^)\/\/(.*?)(<br>|<\/br>|<\/P>|\n)/g,output:'$1<i>//$2</i>$3'},
// Comments (**)
  {input:/\(\*(.*?)\*\)/g,output:'<i>(*$1*)</i>'},
// Comments {}
   {input:/\{(.*?)\}/g,output:'<i>{$1}</i>'},
// Directives {$}
  {input:/\{\$(.*?)\}/g,output:'<u>{$$$1}</u>'}]
Language.snippets=[]
Language.complete=[
  {input:'\'',output:'\'$0\''},
  {input:'(',output:'\($0\)'},
  {input:'[',output:'\[$0\]'},
  {input:'begin',output:'{\n\t$0\nend;'}]
Language.shortcuts=[]
