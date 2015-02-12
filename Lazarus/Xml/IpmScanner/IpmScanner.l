  uses SysUtils;

%start InitState PicState

A                  (A|a)
B                  (B|b)
C                  (C|c)
D                  (D|d)
E                  (E|e)
F                  (F|f)
G                  (G|g)
H                  (H|h)
I                  (I|i)
J                  (J|j)
K                  (K|k)
L                  (L|l)
M                  (M|m)
N                  (N|n)
O                  (O|o)
P                  (P|p)
Q                  (Q|q)
R                  (R|r)
S                  (S|s)
T                  (T|t)
U                  (U|u)
V                  (V|v)
W                  (W|w)
X                  (X|x)
Y                  (Y|y)
Z                  (Z|z)
NewLine            \n
Comma              ","
Space              [ \t\f]
WhiteSpace         ({Space}|{NewLine})+
Alpha	           [A-Za-z]
Digit	           [0-9]
Integer            {Digit}+
Pic                {P}{I}{C}({T}{U}{R}{E})?{Space}({I}{S}{Space})?
PicByte            [\(\)\+\-Zz0-9AaBbNnPpVvXxSs]
PicClause          {PicByte}+
Id                 {Alpha}({Digit}|{Alpha}|\-|\_)*
dNQUOTE             ([^\"])|(\n\-{Space}*\")
dString             \"({dNQUOTE}|\"\")*\"
sNQUOTE             ([^\'])|(\n\-{Space}*\')
sString             \'({sNQUOTE}|\'\')*\'
String             {dString}|{sString}
Comment            ^(" ")?(\*|\/).*{NewLine}
ConditionLevel      ^{Space}*88{Space}+
Directive          ^(\?|\/).*{NewLine}
AlphaPic           (X|x)\({Integer}\)
%%

<InitState>\.                                return (_TERMINATOR);
<InitState>{Id}                              return (_NOID);
<InitState>{Pic}                             begin
                                               start (PicState);
                                               return (_PICTURE);
                                             end;
<PicState>{PicClause}                        begin
                                               start (InitState);
                                               return (_PICTURECLAUSE);
                                             end;
<InitState>{ConditionLevel}                  return (_CONDITIONLEVEL);
<InitState>{Id}                              return (_NOID);
<InitState>{String}                          return (_STRING);
<InitState>{Integer}                         return (_INTEGER);
<InitState>{Comma}                           return (_IGNORE);

{Directive}                                  return (_IGNORE);
{Comment}                                    return (_IGNORE);
.                                            return (_IGNORE);
{NewLine}                                    return (_IGNORE);

