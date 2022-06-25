  uses SysUtils;

%start InitState IgnoreState

terminator			\;
is                              \=
dot                             \.
lt                              \<
gt                              \>
openParenthesis			\(
closeParenthesis                \)
openBracket			\[
closeBracket                    \]
openBrace			\{
closeBrace			\}
comma				\,
plus				\+
minus				\-
whitespace                      [\ \t]+
lineending                      \n
comment                         \/\/.*
commentbegin                    \/\*
commentend                      \*\/
letter				[A-Za-z]
decimalDigit                    [0-9]
octalDigit			[0-7]
hexDigit			[0-9A-Fa-f]

decimalLit			[1-9]{decimalDigit}*
octalLit			0{octalDigit}*
hexLit				0[xX]{hexDigit}* 

decimals			{decimalDigit}*
exponent			[Ee][\+|\-]{decimals}
floatLit			({decimals}?\.{decimals}?{exponent}?)|({decimals}{exponent})|(\.{decimals}{exponent}?)|(inf)|(nan)

boolLit				(true)|(false)

stringLit			(\'[^\']*\')|(\"[^\"]*\")

ident				{letter}({letter}|{decimalDigit}|\_)*
fullIdent			{ident}(\.{ident})*

%%

<InitState>{terminator}         return (_TERMINATOR);
<InitState>{lineending}         return (_LINEENDING);
<InitState>{is}                 return (_IS);
<InitState>{openParenthesis}    return (_OPENPARENTHESIS);
<InitState>{closeParenthesis}   return (_CLOSEPARENTHESIS);
<InitState>{openBracket}        return (_OPENBRACKET);
<InitState>{closeBracket}       return (_CLOSEBRACKET);
<InitState>{openBrace}          return (_OPENBRACE);
<InitState>{closeBrace}         return (_CLOSEBRACE);
<InitState>{comma}              return (_COMMA);
<InitState>{plus}               return (_PLUS);
<InitState>{minus}              return (_MINUS);
<InitState>{decimalLit}		return (_DECIMALLIT);
<InitState>{octalLit}		return (_OCTALLIT);
<InitState>{hexLit}		return (_HEXLIT);
<InitState>{floatLit}           return (_FLOATLIT);
<InitState>{boolLit}            return (_BOOLLIT);
<InitState>{stringLit}          return (_STRINGLIT);
<InitState>{ident}              return (_IDENT);
<InitState>{fullIdent}          return (_FULLIDENT);
<InitState>{whitespace}         return (_IGNORE);
<InitState>{comment}            return (_IGNORE);
<InitState>{dot}                return (_DOT);
<InitState>{lt}                 return (_LT);
<InitState>{gt}                 return (_GT);
<InitState>{commentbegin}       begin
                                  start (IgnoreState);
                                  return (_IGNORE);
                                end;
<IgnoreState>{commentend}       begin
                                  start (InitState);
                                  return (_IGNORE);
                                end;
<IgnoreState>.                  return (_IGNORE);
<IgnoreState>{lineending}       return (_LINEENDING);

