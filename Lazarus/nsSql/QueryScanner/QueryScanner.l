  uses SysUtils;


NewLine       \n
Space         [ \t\f]
Comment       \-\-.*{NewLine}
WhiteSpace    ({Space}|{NewLine}|{Comment})*
Alpha	      [A-Za-z]
Digit	      [0-9]
Id            ({Alpha}|\_)({Digit}|{Alpha}|\_|\-)*
NDQUOTE        ([^\"])|(\n\-{Space}*\")
DString        \"({NDQUOTE}|\"\")*\"
NSQUOTE        ([^\'])|(\n\-{Space}*\')
SString        \'({NSQUOTE}|\'\')*\'
NoDefineName   \={Id}\.{Id}
DefineName     \={Id}
SelectStart     (S|s)(E|e)(L|l)(E|e)(C|c)(T|t){WhiteSpace}\*{WhiteSpace}(F|f)(R|r)(O|o)(M|m)
Separator      \;
%%

{SString}       return (_STRING);
{DString}       return (_STRING);
{NoDefineName}  return (_ANY);
{DefineName}    return (_DEFINE_NAME);
{WhiteSpace}    return (_WHITESPACE);
{SelectStart}   return (_SELECTSTAR);
{Separator}     return (_SEPARATOR);
.               return (_ANY);

