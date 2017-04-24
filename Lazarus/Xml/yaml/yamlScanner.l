  uses SysUtils;

%start InitState MidlState RighState

NewStreamSep    \-\-\-\ *
LineCont        \\[\n\r ]+\\ *
Comment         \ *\#[^\n]*
Name            [^ \-\n\:][^\n\:]*\:
Value           [^ \-\n\:][^\n\:]*
dqString        \"((\\.)|[^\"\n])*\"
sqString        \'((\\.)|[^\'\n])*\'
String          {dqString}|{sqString}
NewLine         \n
cr              \r
Space           (\ )
Pipe            (\|)({Comment})?
IndentSpace     {Space}+
HyphenSpace     {Space}*\-
Rest            [^ \n]
RestEol         [^\|\\\n]+
Digit	        [0-9]
Number          {Digit}*(\.{Digit}+)?([Ee][+-]?{Digit}+)?
False           false
Null            null
True            true

%%

{NewStreamSep}                return (_COMMENT);
{LineCont}                    return (_LINECONT);
{NewLine}                     begin
                                Start (InitState);
                                return (_NEWLINE);
                              end;
{cr}                          return (_WHITESPACE);
{Comment}                     begin
                                Start (InitState);
                                return (_COMMENT);
                              end;
<InitState>{String}           return (_VALUE);
<InitState>{Name}             begin
                                Start (MidlState);
                                return (_NAME);
                              end;
<InitState>{IndentSpace}      return (_INDENT);
<InitState>{HyphenSpace}      return (_HYPHENINDENT);
<InitState>{Value}            return (_VALUE);
<InitState>{Pipe}             return (_PIPE);
<InitState>{Rest}             return (_WHITESPACE);
<MidlState>{NewLine}          begin
                                Start (InitState);
                                return (_NEWLINE);
                              end;
<MidlState>{IndentSpace}      begin
                                Start (RighState);
                                return (_WHITESPACE);
                              end;
<MidlState>{Pipe}             begin
                                Start (RighState);
                                return (_PIPE);
                              end;
<MidlState>{Rest}             begin
                                Start (InitState);
                                return (_WHITESPACE);
                              end;
<MidlState>{String}           return (_VALUE);
<RighState>{NewLine}          begin
                                Start (InitState);
                                return (_NEWLINE);
                              end;
<RighState>{Pipe}             return (_PIPE);
<RighState>{IndentSpace}      return (_WHITESPACE);
<RighState>{RestEol}          return (_VALUE);
<RighState>{String}           return (_VALUE);

