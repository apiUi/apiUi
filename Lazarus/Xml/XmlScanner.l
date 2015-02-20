  uses SysUtils;

%start InitState AttributeState ValueState CDataState IgnoreState

CData              <\!\[CDATA\[
EndCData           \]\]>
Space              [ \t]
NewLine            {Space}*\n{Space}*
XmlProlog          <\?
EndXmlProlog       \?>
XmlComment         <\!\-\-
EndXmlComment      \-\->
EndTag             {Space}*>
EmptyEndTag        \/>
SlashTag           <\/[^> \t]*
Tag                <[^\!\?][^> \/\n\t]*
EscapeChar         &[^;]+;
Is                 =
Id                 [A-Za-z][^= \n]*
String             (\"[^\"]*\")|(\'[^\']*\')
%%

<InitState>{CData}            begin
                                start (CDataState);
                                return (_CDATATAG);
                              end;
<InitState>{XmlComment}       begin
                                popState := InitState;
                                start (IgnoreState);
                                return (_IGNORE);
                              end;
<InitState>{XmlProlog}        begin
                                start (IgnoreState);
                                return (_IGNORE);
                              end;
<InitState>{EndTag}           return (_ENDTAG);
<InitState>{SlashTag}         return (_SLASHTAG);
<InitState>{Tag}              begin
                                start (AttributeState);
                                return (_TAG);
                              end;
<InitState>{NewLine}          return (_IGNORE);
<InitState>{EscapeChar}       return (_ESCAPECHAR);
<InitState>.                  return (_VALUE);

<ValueState>{XmlComment}       begin
                                popState := ValueState;
                                start (IgnoreState);
                                return (_IGNORE);
                              end;
<ValueState>{CData}           begin
                                start (CDataState);
                                return (_CDATATAG);
                              end;
<ValueState>{SlashTag}        begin
                                start (InitState);
                                return (_SLASHTAG);
                              end;
<ValueState>{Tag}             begin
                                start (AttributeState);
                                return (_TAG);
                              end;
<ValueState>{NewLine}         return (_VALUE);
<ValueState>{EscapeChar}      return (_ESCAPECHAR);
<ValueState>.                 return (_VALUE);

<CDataState>{EndCData}        begin
                                start (InitState);
                                return (_CDATAENDTAG);
                              end;
<CDataState>.                 return (_CDATA);
<CDataState>\n                return (_CDATA);
<IgnoreState>{EndXmlProlog}   begin
                                start (InitState);
                                return (_IGNORE);
                              end;
<IgnoreState>{EndXmlComment}  begin
                                start (popState);
                                return (_IGNORE);
                              end;
<IgnoreState>.                return (_IGNORE);
<IgnoreState>\n               return (_IGNORE);
<AttributeState>{Is}          return (_IS);
<AttributeState>{Id}          return (_ATTRIBUTENAME);
<AttributeState>{String}      return (_ATTRIBUTEVALUE);
<AttributeState>{EndTag}      begin
                                start (ValueState);
                                return (_ENDTAG);
                              end;
<AttributeState>{EmptyEndTag} begin
                                start (InitState);
                                return (_EMPTYENDTAG);
                              end;
<AttributeState>.             return (_IGNORE);
<AttributeState>\n            return (_IGNORE);

