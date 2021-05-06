  uses SysUtils;

%start InitState MidlState RighState

NewStreamSep    \-\-\-\ *
LineCont        \\[\n\r ]+\\ *
Comment         \#[^\n]*
Name            [^ \-\n\:][^\n\:]*\:
Value           [^ \-\n\:][^\n\:]*
dqString        \"((\\.)|[^\"\n])*\"
sqString        \'((\\.)|[^\'\n])*\'
String          {dqString}|{sqString}
NewLine         \n
cr              \r
Space           (\ )
ReplCrAndStrip  (\>)\-({Comment})?
ReplCrAndKeep   (\>)\+({Comment})?
ReplCrAndClip   (\>)({Comment})?
KeepCrAndStrip  (\|)\-({Comment})?
KeepCrAndKeep   (\|)\+({Comment})?
KeepCrAndClip   (\|)({Comment})?
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
<InitState>{ReplCrAndStrip}   return (_REPLCRANDSTRIP);
<InitState>{ReplCrAndKeep}    return (_REPLCRANDKEEP);
<InitState>{ReplCrAndClip}    return (_REPLCRANDCLIP);
<InitState>{KeepCrAndStrip}   return (_KEEPCRANDSTRIP);
<InitState>{KeepCrAndKeep}    return (_KEEPCRANDKEEP);
<InitState>{KeepCrAndClip}    return (_KEEPCRANDCLIP);
<InitState>{Rest}             return (_WHITESPACE);
<MidlState>{NewLine}          begin
                                Start (InitState);
                                return (_NEWLINE);
                              end;
<MidlState>{IndentSpace}      begin
                                Start (RighState);
                                return (_WHITESPACE);
                              end;
<MidlState>{ReplCrAndStrip}   begin
                                Start (RighState);
                                return (_REPLCRANDSTRIP);
                              end;
<MidlState>{ReplCrAndKeep}    begin
                                Start (RighState);
                                return (_REPLCRANDKEEP);
                              end;
<MidlState>{ReplCrAndClip}    begin
                                Start (RighState);
                                return (_REPLCRANDCLIP);
                              end;
<MidlState>{KeepCrAndStrip}   begin
                                Start (RighState);
                                return (_KEEPCRANDSTRIP);
                              end;
<MidlState>{KeepCrAndKeep}    begin
                                Start (RighState);
                                return (_KEEPCRANDKEEP);
                              end;
<MidlState>{KeepCrAndClip}    begin
                                Start (RighState);
                                return (_KEEPCRANDCLIP);
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
<RighState>{ReplCrAndStrip}   return (_REPLCRANDSTRIP);
<RighState>{ReplCrAndKeep}    return (_REPLCRANDKEEP);
<RighState>{ReplCrAndClip}    return (_REPLCRANDCLIP);
<RighState>{KeepCrAndStrip}   return (_KEEPCRANDSTRIP);
<RighState>{KeepCrAndKeep}    return (_KEEPCRANDKEEP);
<RighState>{KeepCrAndClip}    return (_KEEPCRANDCLIP);
<RighState>{IndentSpace}      return (_WHITESPACE);
<RighState>{RestEol}          return (_VALUE);
<RighState>{String}           return (_VALUE);

