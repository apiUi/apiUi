  uses SysUtils;

NewLine       \n
Comment       \/\/.*{NewLine}
Space         [ \t\f]
WhiteSpace    ({Space}|{NewLine})
Alpha	      [A-Za-z]
Digit	      [0-9]
Index         \[{Digit}+\]
Dot           \.
Id            {Alpha}({Digit}|{Alpha}|\_|\-|\$)*
FieldID       {Id}({Dot}{Id}({Index})?)+
dynId         {Dot}{Id}({Dot}{Id})*
IdDot         {Id}{Dot}
DynFld        {dynId}|{IdDot}|{Dot}
DynFldG       \.?({Id}\.)+\*
As            (A|a)(S|s)
SqlParamField \:{FieldID}
AsFieldId     {As}{WhiteSpace}+{SqlParamField}
Num           {Digit}*(\.{Digit}+)?([Ee][+-]?{Digit}+)?
AsDynFld      {As}{WhiteSpace}+\:{DynFld}
NQUOTE        [^']
Loop          (L|l)(O|o)(O|o)(P|p)
BeginLayout   (L|l)(A|a)(Y|y)(O|o)(U|u)(T|t){WhiteSpace}*\{({Space}*{NewLine})?
LayoutField   \<\<{FieldID}\>\>
String        \'{NQUOTE}*\'
End           {Space}*\}
Exec          (E|e)(X|x)(E|e)(C|c)
Sql           (S|s)(Q|q)(L|l)
Yag           [Rr][Ee][Gg][Ee][Xx]
Delete        (D|d)(E|e)(L|l)(E|e)(T|t)(E|e)
Insert        (I|i)(N|n)(S|s)(E|e)(R|r)(T|t)
Select        (S|s)(E|e)(L|l)(E|e)(C|c)(T|t)
Update        (U|u)(P|p)(D|d)(A|a)(T|t)(E|e)
ExecSql       {Exec}{WhiteSpace}+{Sql}
ExecSqlDelete {Exec}{WhiteSpace}+{Sql}{WhiteSpace}+{Delete}
ExecSqlInsert {Exec}{WhiteSpace}+{Sql}{WhiteSpace}+{Insert}
ExecSqlSelect {Exec}{WhiteSpace}+{Sql}{WhiteSpace}+{Select}
ExecSqlUpdate {Exec}{WhiteSpace}+{Sql}{WhiteSpace}+{Update}
ExecYag       {Exec}{WhiteSpace}+{Yag}
%%

{FieldID}              return (_FIELDID);
{AsFieldId}            return (_AS_FIELDID);
{DynFld}               return (_DYNFLD);
":="		       return (_ASSIGNMENT);
"{"                    return (_BEGIN);
{Loop}                 return (_LOOP);
{BeginLayout}          return (_BEGIN_LAYOUT);
{LayoutField}          return (_LAYOUT_FIELD);
{String}               return (_CHARACTER_STRING);
":"		       return (_COLON);
","		       return (_COMMA);
".."		       return (_DOTDOT);
{Id}                   return (_NOID);
{End}                  return (_END);
"="		       return (_EQUAL);
{ExecSqlDelete}        return (_EXEC_SQLEXEC);
{ExecSqlInsert}        return (_EXEC_SQLINSERT);
{ExecSqlSelect}        return (_EXEC_SQLSELECT);
{ExecSqlUpdate}        return (_EXEC_SQLEXEC);
{ExecSql}              return (_EXEC_SQL);
{ExecYag}              return (_EXEC_YAG);
{Comment}              return (_COMMENT);
"&"		       return (_AMPERSAND);
">="		       return (_GE);
">"		       return (_GT);
"["		       return (_LBRAC);
"<="		       return (_LE);
"("		       return (_LPAREN);
"<"		       return (_LT);
"-"		       return (_MINUS);
"<>"		       return (_NOTEQUAL);
"+"		       return (_PLUS);
"]"		       return (_RBRAC);
{Num}                  return (_REALNUMBER);
")"		       return (_RPAREN);
";"		       return (_SEMICOLON);
"/"		       return (_SLASH);
{SqlParamField}        return (_SQLPARAM_FIELD);
"*"		       return (_STAR);
"**"		       return (_STARSTAR);
"^"		       return (_UPARROW);
{Space}	               return (_SPACE);
{NewLine}              return (_NEWLINE);
.		       return(_ILLEGAL);

