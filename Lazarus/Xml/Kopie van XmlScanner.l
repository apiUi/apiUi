  uses SysUtils;

%start InitState TagState

NewLine            \n
Space              [ \t\f]
WhiteSpace         ({Space}|{NewLine})+
Tag                <[^>]*>
EndTag             </[^>]*>
%%

<InitState>{Tag}                             begin
                                               start (TagState);
                                               return (_TAG);
                                             end;
<InitState>.                                 return (_IGNORE);
<InitState>{NewLine}                         return (_IGNORE);
<TagState>{NewLine}                          begin
                                               start (InitState);
                                               return (_IGNORE);
                                             end;
<TagState>{Tag}                              return (_TAG);
<TagState>{EndTag}                           begin
                                               start (InitState);
                                               return (_ENDTAG);
                                             end;
<TagState>.+                                 return (_VALUE);

