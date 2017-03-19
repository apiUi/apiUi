  uses SysUtils;

%start InitState MidlState RighState

Comment         \#[^\n]*
Name            [^ \-\n\:][^\n\:]*\:
Value           [^ \-\n\:][^\n\:]*
dqString        \"((\\.)|[^\"\n])*\"
sqString        \'((\\.)|[^\'\n])*\'
String          {dqString}|{sqString}
NewLine         \n
Space           (\ )
Pipe            (\|)
IndentSpace     {Space}+
HyphenSpace     {Space}*\-
Rest            [^ \n]
RestEol         [^\|\n]+
Digit	        [0-9]
Number          {Digit}*(\.{Digit}+)?([Ee][+-]?{Digit}+)?
False           false
Null            null
True            true

%%

{Comment}                     begin
                                Start (InitState);
                                return (_COMMENT);
                              end;
<InitState>{NewLine}          begin
                                Start (InitState);
                                return (_NEWLINE);
                              end;
<InitState>{Name}             begin
                                Start (MidlState);
                                return (_NAME);
                              end;
<InitState>{IndentSpace}      return (_INDENT);
<InitState>{HyphenSpace}      return (_HYPHENINDENT);
<InitState>{String}           return (_STRING);
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
<MidlState>{String}           return (_STRING);
<RighState>{NewLine}          begin
                                Start (InitState);
                                return (_NEWLINE);
                              end;
<RighState>{Pipe}             return (_PIPE);
<RighState>{IndentSpace}      return (_WHITESPACE);
<RighState>{RestEol}          return (_VALUE);

