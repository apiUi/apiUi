  uses SysUtils;

%start InitState IgnoreState

String          \"[^\"]*\"
WhiteSpace      [ \t\n\r]
Colon           \:
Comma           \,
LeftSquare      \[
RightSquare     \]
LeftCurly       \{
RightCurly      \}
Digit	        [0-9]
Number          {Digit}*(\.{Digit}+)?([Ee][+-]?{Digit}+)?
False           false
Null           null
True            true

%%

<InitState>{String}           return (_STRING);
<InitState>{WhiteSpace}       return (_IGNORE);
<InitState>{Colon}            return (_COLON);
<InitState>{Comma}            return (_COMMA);
<InitState>{LeftSquare}       return (_LEFT_SQUARE_BRACKET);
<InitState>{RightSquare}      return (_RIGHT_SQUARE_BRACKET);
<InitState>{LeftCurly}        return (_LEFT_CURLY_BRACKET);
<InitState>{RightCurly}       return (_RIGHT_CURLY_BRACKET);
<InitState>{Number}           return (_NUMBER);
<InitState>{False}            return (_FALSE);
<InitState>{Null}             return (_NULL);
<InitState>{True}             return (_TRUE);

