  uses SysUtils;

%start InitState CDataState

CDataTag           <\!\[CDATA\[
CDataEndTag        \]\]>
Space              [ \t]
NewLine            \n{Space}*
CommentTag         <(\?|!)[^\[][^>]+>
EndTag             <\/[^>]*>
EmptyTag           <[^>/]+\/>
Tag                <[^>]+>
EscapeChar         &[^;]+;
%%

<InitState>{CDataTag}     begin
                            start (CDataState);
                            return (_CDATATAG);
                          end;
<InitState>{CommentTag}   return (_IGNORE);
<InitState>{EndTag}       return (_ENDTAG);
<InitState>{EmptyTag}     return (_EMPTYTAG);
<InitState>{Tag}          return (_TAG);
<InitState>{NewLine}      return (_IGNORE);
<InitState>{EscapeChar}   return (_ESCAPECHAR);
<InitState>.              return (_VALUE);
<CDataState>{CDataEndTag}  begin
                            start (InitState);
                            return (_CDATAENDTAG);
                          end;
<CDataState>.             return (_CDATA);
<CDataState>\n            return (_CDATA);

