/* Yacc grammar; maintained by Jan Bouwman

*/

%{
%}

%token DF DFS DFX FRAME_ SFBSS SFD SFDS SFS SFSS SFSSS SFSSSS SFSX SFSXX SFV SFX
%token SFOG, SFOV, SFOS, SFOSB, SFOSS, SFOSX, SFOSSS, SFOSSSS
%token VFV VFS VFSS VFSX VFSSS VFSSX VFSSSS VFX VFOGS
%token VFOV VFOS VFOSB VFOSS VFOSX VFOSSS VFOSSSB VFOSSX VFOSSSS VFOB VFOX VFOD
%token SLFOS SLFOSS
%token VFG VFGG VFGGG VFGGGG
%token XFD XFG XFGG XFS XFSX XFV XFX XFXX XFFRAME
%token XFOV XFOS XFOSX XFOX XFOXX
%token BFLD DFLD SFLD IFLD XFLD GFLD PFLD
%token _NOID, _DYNFLD
%token _ALIAS _AMPERSAND _AND _ARRAY _AS _AS_DYNFLD _AS_FIELDID _ASSIGNMENT
%token _BEGIN _BEGIN_LAYOUT _LOOP
%token _CASE _CHARACTER_STRING _COLON _COMMA _COMMENT _CONST
%token _DATETIME _DAYS _DECLARE _DIGSEQ _DIV _DO _DOT _DOTDOT _DOWNTO
%token _EACH _ELSE _END _EQUAL _EVALUATE _EXEC_SQL _EXEC_SQLEXEC _EXEC_SQLINSERT _EXEC_SQLSELECT
%token _EXEC_YAG
%token _EXTERNAL
%token _FALSE _FIELDID _FILE _FLOAT _FOR _FOREACH _FORWARD _FRAME _FUNCTION
%token _GE _GOTO _GT
%token _IDENTIFIER _IF _IN _INTEGER
%token _LABEL _LAYOUT_FIELD _LAYOUT_TOKEN _LBRAC _LE _LPAREN _LT
%token _MINUS _MOD _MONTHS
%token _NEW _NEWLINE _NIL _NOT _NOTEQUAL
%token _OF _OR _OTHERWISE
%token _PACKED _PLUS _POINTER _PROCEDURE _PROGRAM
%token _RBRAC _REALNUMBER _RECORD _REPEAT _RPAREN
%token _SEMICOLON _SET _SLASH _SQLEXEC _SQLPARAM_FIELD _SQLSELECT _SQLTOKEN
%token _SPACE _STAR _STARSTAR _STRING
%token _THEN _TO _TRUE _TYPE
%token _UNTIL _UPARROW
%token _VAR _VOID VOIDFUNCTION_
%token _WEEKS _WHILE _WHITESPACE _WITH _WITHDO _WITHNEWDO
%token _YAG _YEARS

%token _ILLEGAL

%left _PLUS _MINUS
%left _STAR _SLASH _MOD
%left _UPARROW
%right UMINUS
%left _OR
%left _AND
%left _EQUAL
%right _NOT

%%
%{
uses Bind, Xmlz, SysUtils, Math, Dialogs, Frame
   ;
var
  LocalParser: TParser;
  LocalBind: TBind;
  yyRegEx: TStringList;
  yyRegExCursor: Integer;
  yySql: YYSType;
  yyBeginLayout: YYSType;
  yyEndLayout: YYSType;
  LocalFrame: TFrame;
  LocalCount: Integer;
  ParamCounter: Integer;

procedure CreateLocalParser (aParser: TParser);
begin
  LocalParser := TParser.Create(aParser.Owner);
  LocalParser.OnPutData := aParser.OnPutData;
  LocalParser.OnGetData := aParser.OnGetData;
  LocalParser.OnHaveData := aParser.OnHaveData;
  LocalParser.OnError := aParser.OnError;
  LocalParser.OnEvaluateString := aParser.OnEvaluateString;
  LocalParser.OnStoreObject := aParser.OnStoreObject;
  LocalParser.OnCreateQuery := aParser.OnCreateQuery;
  LocalParser.OnHaveSqlToken := aParser.OnHaveSqlToken;
  LocalParser.OnHaveSqlBind := aParser.OnHaveSqlBind;
  LocalParser.OnHaveSqlParam := aParser.OnHaveSqlParam;
  LocalParser.OnHaveSqlInsertParam := aParser.OnHaveSqlInsertParam;
  LocalParser.OnNeedSqlExec := aParser.OnNeedSqlExec;
  LocalParser.OnNeedSqlOpen := aParser.OnNeedSqlOpen;
  LocalParser.OnNeedSqlClose := aParser.OnNeedSqlClose;
  LocalParser.OnNeedSqlNextRow := aParser.OnNeedSqlNextRow;
  LocalParser.OnNeedSqlFirstRow := aParser.OnNeedSqlFirstRow;
end;

procedure ClearBinds (arg: TBindList);
var
  x: Integer;
  Bind: TBind;
begin
  for x := 0 to arg.Count - 1 do
  begin
    Bind := arg.Binds [x];
    case Bind.Token of
      FRAME_: (Bind.yy.yyObject as TFrame).Free;
      VOIDFUNCTION_: (Bind.yy.yyObject as TParser).Free;
    end;
    Bind.Free;
  end;
  arg.Clear;
end;

function TParser.Execute: Integer;
begin
  cFed := nil;
  pFed := nil;
  result := inherited;
end;

procedure TParser.Prepare;
begin
  if not Assigned (FOnStoreObject) then
    RaiseException ('No procedure assigned to OnStoreObject');
  ClearBinds (InternalBinds);
  ClearBinds (ArgumentBinds);
  inherited Prepare;
end;

destructor TParser.Destroy;
begin
  ClearBinds (InternalBinds);
  ClearBinds (ArgumentBinds);
  inherited Destroy;
end;

function TParser.yylex: Integer;
var
  Bind: TBind;
  BindIndex: Integer;
begin
  result := inherited yylex;
  case result of
    // _FIELDID,
    _NOID:
    begin
      if InternalBinds.Find (Uppercase (yylval.TokenString), BindIndex) then
      begin
        Bind := InternalBinds.Binds [BindIndex];
        yylval.Token := Bind.Token;
        yylval.yy.yyObject := Bind;
        yylval.yyRead := yylval.yy;
        result := Bind.Token;
      end;
    end;
    _AS_FIELDID, _SQLPARAM_FIELD, _LAYOUT_FIELD:
    begin
      if (yylval.yy.yyObject = Nil) then
      begin
        if InternalBinds.Find (Uppercase (yylval.TokenString), BindIndex) then
        begin
          yylval.yy.yyObject := InternalBinds.Binds [BindIndex];
          yylval.yyRead := yylval.yy;
        end
        else
          RaiseException ('Unknown field');
      end;
    end;
  end;
end;

function DateTimeAddYears (Dt: TDateTime; Years: Extended): TDateTime;
var
  Year, Month, Day: Word;
  Ok: Boolean;
begin
  DecodeDate (Dt, Year, Month, Day);
  Year := Year + Trunc (Years);
  Ok := False;
  while not Ok do
  begin
    try
      result := EncodeDate (Year, Month, Day);
      Ok := True;
    except
      Dec (Day);
    end;
  end;
end;

function DateTimeAddMonths (Dt: TDateTime; Months: Extended): TDateTime;
var
  Year, Month, Day: Word;
  Ok: Boolean;
begin
  DecodeDate (Dt, Year, Month, Day);
  Inc (Month, Trunc (Months));
  while (Month > 12) do
  begin
    Inc (Year);
    Dec (Month, 12);
  end;
  while (Month < 1) do
  begin
    Dec (Year);
    Inc (Month, 12);
  end;
  Ok := False;
  while not Ok do
  begin
    try
      result := EncodeDate (Year, Month, Day);
      Ok := True;
    except
      Dec (Day);
    end;
  end;
end;


%}

Start	:
          {
            if DoIt then yyInitParse;
          }
          OptionalDeclarationList
          OptionalStatementList
          {
            if DoIt then yyTermParse;
          }
	;

OptionalDeclarationList:
          /* void */
        | DeclarationList
        ;

DeclarationList:
          Declaration
        | DeclarationList Declaration
        ;

Declaration:
          DateTimeDeclaration _SEMICOLON
        | ExtendedDeclaration _SEMICOLON
        | IntegerDeclaration _SEMICOLON
        | StringDeclaration _SEMICOLON
        | PointerDeclaration _SEMICOLON
        ;

DateTimeDeclaration:
          _DATETIME _FIELDID
          {
            $1.Token := _SEMICOLON;
            $1.NextToken := $2.NextToken.NextToken;
          }
        | _DATETIME _FIELDID _ASSIGNMENT dExpr
          {
            $1.Token := _SEMICOLON;
            $2.Token := DFLD;
          }
        ;

ExtendedDeclaration:
          _FLOAT _FIELDID
          {
            $1.Token := _SEMICOLON;
            $1.NextToken := $2.NextToken.NextToken;
          }
        | _FLOAT _FIELDID _ASSIGNMENT xExpr
          {
            $1.Token := _SEMICOLON;
            $2.Token := XFLD;
          }
        ;

IntegerDeclaration:
          _INTEGER _FIELDID
          {
            $1.Token := _SEMICOLON;
            $1.NextToken := $2.NextToken.NextToken;
          }
        | _INTEGER _FIELDID _ASSIGNMENT xExpr
          {
            $1.Token := _SEMICOLON;
            $2.Token := IFLD;
          }
        ;

StringDeclaration:
          _STRING _FIELDID
          {
            $1.Token := _SEMICOLON;
            $1.NextToken := $2.NextToken.NextToken;
          }
        | _STRING _FIELDID _ASSIGNMENT sExpr
          {
            $1.Token := _SEMICOLON;
            $2.Token := SFLD;
          }
        ;

PointerDeclaration:
          _POINTER _FIELDID
          {
            $1.Token := _SEMICOLON;
            $1.NextToken := $2.NextToken;
          }
        | _POINTER _FIELDID _ASSIGNMENT _AMPERSAND PointerRef
          {
            $1.Token := _SEMICOLON;
            $1.NextToken := $2;
            $2.Token := PFLD;
            TBind($2.yy.yyObject).yy.yyObject := $5.yy.yyObject;
            $2.yyRead := $2.yy;
          }
        ;

PointerRef:
          GFLD
          { yylval := $1;
            $$.yy.yyObject := TBind($1.yy.yyObject).yy.yyObject;
          }
        | DynToken
          { yylval := $1;
            if Preparing then
            begin
              if cFed = nil then
                yyerror ('syntax error, no parent for: ' + $1.TokenString);
              if not Assigned (cFed.PrepareBindableOnAliasField ($1.TokenString)) then
                yyerror ($1.TokenString + ' not found');
            end;
            if DoIt then
              $$.yy.yyObject := cFed.FindBindableOnAliasField ($1.TokenString);
          }
       ;

OptionalStatementList:
          /* void */
        | StatementList
        ;

StatementList:
          Statement
        | StatementList Statement
        ;

Statement:
          _SEMICOLON
        | BlokStatement
        | Assignment _SEMICOLON
        | IfStatement
        | WhileStatement
        | sExpr _SEMICOLON
        | xExpr _SEMICOLON
        | VoidStatement _SEMICOLON
        | VoidCall
        | CreateFunction
        | DeclareForEachStatement
        | ForEachStatement
        | DeclareWithStatement
        | WithStatement
        | WithNewStatement
        | SlStatement
        | DeclareWithNewStatement
        | DeclareSqlSelectStatement
        | SqlSelectStatement
        | SqlExecStatement _SEMICOLON
        | DeclareSqlInsertStatement _SEMICOLON
        | DeclareSqlExecStatement _SEMICOLON
        ;

VoidCall:
          VFV _LPAREN _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionV(); }
        | VFG    _LPAREN gExpr                                        _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionG    ($3); }
        | VFGG   _LPAREN gExpr _COMMA gExpr                           _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionGG   ($3, $5); }
        | VFGGG  _LPAREN gExpr _COMMA gExpr _COMMA gExpr              _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionGGG  ($3, $5, $7); }
        | VFGGGG _LPAREN gExpr _COMMA gExpr _COMMA gExpr _COMMA gExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionGGGG ($3, $5, $7, $9); }
        | VFS _LPAREN sExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionS ($3.yyString); }
        | VFSS _LPAREN sExpr _COMMA sExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionSS ($3.yyString, $5.yyString); }
        | VFSX _LPAREN sExpr _COMMA xExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionSX ($3.yyString, $5.yy.yyExtended); }
        | VFSSS _LPAREN sExpr _COMMA sExpr _COMMA sExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionSSS ($3.yyString, $5.yyString, $7.yyString); }
        | VFSSX _LPAREN sExpr _COMMA sExpr _COMMA xExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionSSX ($3.yyString, $5.yyString, $7.yy.yyExtended); }
        | VFSSSS _LPAREN sExpr _COMMA sExpr _COMMA sExpr _COMMA sExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionSSSS ($3.yyString, $5.yyString, $7.yyString, $9.yyString); }
        | VFX _LPAREN xExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionX ($3.yy.yyExtended); }
        | VFOV _LPAREN _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionOV (Owner); }
        | VFOD _LPAREN dExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionOD (Owner, $3.yy.yyDateTime); }
        | VFOGS _LPAREN gExpr _COMMA sExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionOGS (Owner, $3, $5.yyString); }
        | VFOS _LPAREN sExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionOS (Owner, $3.yyString); }
        | VFOSB _LPAREN sExpr _COMMA bExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionOSB (Owner, $3.yyString, $5.yy.yyBoolean); }
        | VFOSS _LPAREN sExpr _COMMA sExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionOSS (Owner, $3.yyString, $5.yyString); }
        | VFOSX _LPAREN sExpr _COMMA xExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionOSX (Owner, $3.yyString, $5.yy.yyExtended); }
        | VFOSSS _LPAREN sExpr _COMMA sExpr _COMMA sExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionOSSS (Owner, $3.yyString, $5.yyString, $7.yyString); }
        | VFOSSSB _LPAREN sExpr _COMMA sExpr _COMMA sExpr _COMMA bExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionOSSSB (Owner, $3.yyString, $5.yyString, $7.yyString, $9.yy.yyBoolean); }
        | VFOSSX _LPAREN sExpr _COMMA sExpr _COMMA xExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionOSSX (Owner, $3.yyString, $5.yyString, $7.yy.yyExtended); }
        | VFOSSSS _LPAREN sExpr _COMMA sExpr _COMMA sExpr _COMMA sExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionOSSSS (Owner, $3.yyString, $5.yyString, $7.yyString, $9.yyString); }
        | VFOB _LPAREN bExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionOB (Owner, $3.yy.yyBoolean); }
        | VFOX _LPAREN xExpr _RPAREN { if DoIt then ($1.yy.yyObject as TBind).yy.yyVFunctionOX (Owner, $3.yy.yyExtended); }
        ;

CreateFunction:
           CreateVoidFunction
        ;

CreateVoidFunction:
          _VOID                   /* 1 */
          _NOID                   /* 2 */
          /* $3 create executable */
          {
            PushObject (LocalParser as TObject);
            CreateLocalParser (self);
            LocalBind := TBind.Create;
            LocalBind.Id := Uppercase ($2.TokenString);
            LocalBind.Token := VOIDFUNCTION_;
            LocalBind.yy.yyObject := LocalParser as TObject;
            InternalBinds.AddObject (LocalBind.Id, LocalBind);
          }
          _LPAREN                 /* 4 */
          OptionalArgumentList    /* 5 */
          _RPAREN                 /* 6 */
          _BEGIN                  /* 7 */
          Start                   /* 8 */
          _END                    /* 9 */
          /* assign tokensream to it   */
          /* shortcut the functiondecl.*/
          /* pop localparser           */
          {
            LocalParser.LexItems := $7.NextToken;
            $1.Token := _SEMICOLON;
            $1.NextToken := $9.NextToken;
            $9.Token := _SEMICOLON;
            $9.NextToken := nil;
            LocalParser := PopObject as TParser;
          }
          ;

OptionalArgumentList:
            /* void */
          | ArgumentList
          ;

ArgumentList:
            Argument
          | ArgumentList _COMMA Argument
          ;

Argument:
            ArgumentFormat ArgumentId
            {
              ShowMessage ($1.Tokenstring + ' ' + $2.TokenString);
            }
          ;

ArgumentFormat:
            _DATETIME
          | _FLOAT
          | _INTEGER
          | _STRING
          ;

ArgumentId:
            _FIELDID
          ;

BlokStatement:
          _BEGIN
          {
            if not Preparing then
              $1.Block.InitBinds;
            PushBoolean (DoIt);
            PushBoolean (Cond);
          }
          Start
          _END
          {
            Cond := PopBoolean;
            DoIt := PopBoolean;
          }
        ;

VoidStatement:
          _EVALUATE _LPAREN _CHARACTER_STRING _RPAREN
          {
            if DoIt then yyEvaluateString ($3.yyString);
          }
        | VOIDFUNCTION_ _LPAREN _RPAREN
          {
            if DoIt then
            begin
              LocalBind := $1.yy.yyObject as TBind;
              LocalParser := LocalBind.yy.yyObject as TParser;
              LocalParser.DoIt := DoIt;
              LocalParser.Execute;
            end;
          }
        |
        ;
WhileStatement:
          {
            PushBoolean (Cond);
          }
          _WHILE BooleanExpr _DO TrueStatement
          {
            if (DoIt and Cond) then
              yyjump ($2);
            Cond := PopBoolean;
          }
        ;
IfStatement:
          {
            PushBoolean (Cond);
          }
          IfOrIfThenElse
          {
            Cond := PopBoolean;
          }
        ;

IfOrIfThenElse:
          _IF BooleanExpr _THEN TrueStatement
        | _IF BooleanExpr _THEN TrueStatement _ELSE FalseStatement
        ;

TrueStatement:
          {
            PushBoolean (DoIt);
            if DoIt then
              DoIt := Cond;
          }
          Statement
          {
            DoIt := PopBoolean;
          }
        ;

FalseStatement:
          {
            PushBoolean (DoIt);
            if DoIt then
              DoIt := not Cond;
          }
          Statement
          {
            DoIt := PopBoolean;
          }
        ;

Assignment:
          BFLD _ASSIGNMENT bExpr
          {
            yylval := $1;
            if DoIt then PutDataEvent ($1, $3);
          }
        | DFLD _ASSIGNMENT dExpr
          {
            yylval := $1;
            if DoIt then PutDataEvent ($1, $3);
          }
        | SFLD _ASSIGNMENT sExpr
          {
            yylval := $1;
            if DoIt then PutDataEvent ($1, $3);
          }
        | XFLD _ASSIGNMENT xExpr
          {
            yylval := $1;
            if DoIt then PutDataEvent ($1, $3);
          }
        | IFLD _ASSIGNMENT xExpr
          {
            yylval := $1;
            if DoIt then PutDataEvent ($1, $3);
          }
        | GFLD _ASSIGNMENT gExpr
          {
            yylval := $1;
            if DoIt then PutDataEvent ($1, $3);
          }
        | _STAR PFLD _ASSIGNMENT gExpr
          {
            yylval := $2;
            if DoIt then PutDataEvent ($2, $4);
          }
        | PFLD _ASSIGNMENT _AMPERSAND PointerRef
          {
            if (DoIt or Preparing) then
            begin
              TBind($1.yy.yyObject).yy.yyObject := $4.yy.yyObject;
              $1.yyRead := $1.yy;
            end;
          }
        | PFLD _ASSIGNMENT _NIL
          {
            if (DoIt or Preparing) then
            begin
              TBind($1.yy.yyObject).yy.yyObject := nil;
              $1.yyRead := $1.yy;
            end;
          }
        | DFLD _ASSIGNMENT _NIL
          {
            yylval := $1;
            if DoIt then PutDataEvent ($1, nil);
          }
        | SFLD _ASSIGNMENT _NIL
          {
            yylval := $1;
            if DoIt then PutDataEvent ($1, nil);
          }
        | XFLD _ASSIGNMENT _NIL
          {
            yylval := $1;
            if DoIt then PutDataEvent ($1, nil);
          }
        | IFLD _ASSIGNMENT _NIL
          {
            yylval := $1;
            if DoIt then PutDataEvent ($1, nil);
          }
        | GFLD _ASSIGNMENT _NIL
          {
            yylval := $1;
            if DoIt then PutDataEvent ($1, nil);
          }
        | _STAR PFLD _ASSIGNMENT _NIL
          {
            yylval := $2;
            if DoIt then PutDataEvent ($2, nil);
          }
        | DynToken _ASSIGNMENT SFLD
          {
            yylval := $1;
            if Preparing then
            begin
              if cFed = nil then
                yyerror ('syntax error, no parent for: ' + $1.TokenString);
              if not Assigned (cFed.PrepareBindableOnAliasField($1.TokenString)) then
                yyerror ($1.TokenString + ' not found');
            end;
            if DoIt then
            begin
              xObject := cFed.FindBindableOnAliasField($1.TokenString);
              if Assigned (xObject) then with xObject as TCustomBindable do
              begin
                Value := $3.ValueAsString;
                Checked := True;
              end;
            end;
          }
        | DynToken _ASSIGNMENT gExpr
          {
            yylval := $1;
            if Preparing then
            begin
              if cFed = nil then
                yyerror ('syntax error, no parent for: ' + $1.TokenString);
              if not Assigned (cFed.PrepareBindableOnAliasField($1.TokenString)) then
                yyerror ($1.TokenString + ' not found');
            end;
            if DoIt then
            begin
              xObject := cFed.FindBindableOnAliasField($1.TokenString);
              if Assigned (xObject) then with xObject as TCustomBindable do
                PutGroupData ($3.yy.yyObject)
              else
                yyerror ($1.TokenString + ' not found');
            end;
          }
        | DynToken _ASSIGNMENT sExpr
          {
            yylval := $1;
            if Preparing then
            begin
              if cFed = nil then
                yyerror ('syntax error, no parent for: ' + $1.TokenString);
              if not Assigned (cFed.PrepareBindableOnAliasField($1.TokenString)) then
                yyerror ($1.TokenString + ' not found');
            end;
            if DoIt then
            begin
              xObject := cFed.FindBindableOnAliasField($1.TokenString);
              if Assigned (xObject) then with xObject as TCustomBindable do
              begin
                if $3.yy.yyObject is TXml then
                  PutGroupData ($3.yy.yyObject)
                else
                begin
                  if Children.Count > 0 then
                    yyerror (Format ('Assignment not allowed: %s := %s', [$1.TokenString, $3.yyString]));
                  Value := $3.yyString;
                  Checked := True;
                end;
              end;
            end;
          }
        | DynToken _ASSIGNMENT _NIL
          {
            yylval := $1;
            if Preparing then
            begin
              if cFed = nil then
                yyerror ('syntax error, no parent for: ' + $1.TokenString);
              if not Assigned (cFed.PrepareBindableOnAliasField($1.TokenString)) then
                yyerror ($1.TokenString + ' not found');
            end;
            if DoIt then
            begin
              xObject := cFed.FindBindableOnAliasField($1.TokenString);
              if Assigned (xObject) then with xObject as TCustomBindable do
                Reset;
            end;
          }
        ;


gExpr:    GFLD { yylval := $1; if DoIt then GetObjectEvent ($1, $$); }
        | _STAR PFLD { yylval := $2; if DoIt then GetObjectEvent ($2, $$); }
        | SFLD { yylval := $1; if DoIt then GetObjectEvent ($1, $$); }
        | DFLD { yylval := $1; if DoIt then GetObjectEvent ($1, $$); }
        | IFLD { yylval := $1; if DoIt then GetObjectEvent ($1, $$); }
        | XFLD { yylval := $1; if DoIt then GetObjectEvent ($1, $$); }
        | DynToken
          { yylval := $1;
            if Preparing then
            begin
              if cFed = nil then
                yyerror ('syntax error, no parent for: ' + $1.TokenString);
              if not Assigned (cFed.PrepareBindableOnAliasField ($1.TokenString)) then
                yyerror ($1.TokenString + ' not found');
            end;
            if DoIt then
              try $$.yy.yyObject := cFed.FindBindableOnAliasField ($1.TokenString); except end;
          }
        ;

dExpr:    dExpr _PLUS dExpr	 { $$.yy.yyDateTime := $1.yy.yyDateTime + $3.yy.yyDateTime; }
	| dExpr _MINUS dExpr	 { $$.yy.yyDateTime := $1.yy.yyDateTime - $3.yy.yyDateTime; }
	| dExpr _PLUS xExpr	 { $$.yy.yyDateTime := $1.yy.yyDateTime + $3.yy.yyExtended; }
	| dExpr _MINUS xExpr	 { $$.yy.yyDateTime := $1.yy.yyDateTime - $3.yy.yyExtended; }
	| xExpr _PLUS dExpr	 { $$.yy.yyDateTime := $1.yy.yyExtended + $3.yy.yyDateTime; }
	| xExpr _MINUS dExpr	 { $$.yy.yyDateTime := $1.yy.yyExtended - $3.yy.yyDateTime; }
	| _LPAREN dExpr _RPAREN { $$.yy.yyExtended := $2.yy.yyExtended; }
        | DFLD
          { yylval := $1;
            if DoIt then GetDataEvent ($1, $$);
          }
        | DF _LPAREN _RPAREN { if DoIt then $$.yy.yyDateTime := ($1.yy.yyObject as TBind).yy.yyDFunction(); }
        | DFS _LPAREN sExpr _RPAREN { if DoIt then $$.yy.yyDateTime := ($1.yy.yyObject as TBind).yy.yyDFunctionS ($3.yyString); }
        | DFX _LPAREN xExpr _RPAREN { if DoIt then $$.yy.yyDateTime := ($1.yy.yyObject as TBind).yy.yyDFunctionX ($3.yy.yyExtended); }
        | dExpr _PLUS _YEARS _LPAREN xExpr _RPAREN { if DoIt then $$.yy.yyDateTime := DateTimeAddYears ($1.yy.yyDateTime, $5.yy.yyExtended);}
        | dExpr _MINUS _YEARS _LPAREN xExpr _RPAREN { if DoIt then $$.yy.yyDateTime := DateTimeAddYears ($1.yy.yyDateTime, -1 * $5.yy.yyExtended);}
        | dExpr _PLUS _MONTHS _LPAREN xExpr _RPAREN { if DoIt then $$.yy.yyDateTime := DateTimeAddMonths ($1.yy.yyDateTime, $5.yy.yyExtended);}
        | dExpr _MINUS _MONTHS _LPAREN xExpr _RPAREN { if DoIt then $$.yy.yyDateTime := DateTimeAddMonths ($1.yy.yyDateTime, -1 * $5.yy.yyExtended);}
        | dExpr _PLUS _WEEKS _LPAREN xExpr _RPAREN { if DoIt then $$.yy.yyDateTime := $1.yy.yyDateTime + 7 * $5.yy.yyExtended;}
        | dExpr _MINUS _WEEKS _LPAREN xExpr _RPAREN { if DoIt then $$.yy.yyDateTime := $1.yy.yyDateTime - 7 * $5.yy.yyExtended;}
        | dExpr _PLUS _DAYS _LPAREN xExpr _RPAREN { if DoIt then $$.yy.yyDateTime := $1.yy.yyDateTime + $5.yy.yyExtended;}
        | dExpr _MINUS _DAYS _LPAREN xExpr _RPAREN { if DoIt then $$.yy.yyDateTime := $1.yy.yyDateTime - $5.yy.yyExtended;}
	;

sExpr:    sExpr _PLUS sExpr	{ $$.yyString := $1.yyString + $3.yyString; }
	| _LPAREN sExpr _RPAREN { $$.yyString := $2.yyString; $$.yy.yyObject := $2.yy.yyObject; }
	| _CHARACTER_STRING     { $$.yyString := $1.yyString; }
        | SFLD
          { yylval := $1;
            if DoIt then GetDataEvent ($1, $$);
          }
        | DynToken
          { yylval := $1;
            if Preparing then
            begin
              if cFed = nil then
                yyerror ('syntax error, no parent for: ' + $1.TokenString);
              if not Assigned (cFed.PrepareBindableOnAliasField ($1.TokenString)) then
                yyerror ($1.TokenString + ' not found');
            end;
            if DoIt then
            begin
              xObject := cFed.FindBindableOnAliasField ($1.TokenString);
              if Assigned (xObject) then with xObject as TCustomBindable do
              begin
                $$.yy.yyObject := $1.yy.yyObject; // double function
                $$.yyString := Value;
                $$.yyRead := $$.yy;
              end;
            end;
          }
        | SFS _LPAREN sExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionS ($3.yyString); }
        | SFSS _LPAREN sExpr _COMMA sExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionSS ($3.yyString, $5.yyString); }
        | SFSSS _LPAREN sExpr _COMMA sExpr _COMMA sExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionSSS ($3.yyString, $5.yyString, $7.yyString); }
        | SFSSSS _LPAREN sExpr _COMMA sExpr _COMMA sExpr _COMMA sExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionSSSS ($3.yyString, $5.yyString, $7.yyString, $9.yyString); }
        | SFSX _LPAREN sExpr _COMMA xExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionSX ($3.yyString, $5.yy.yyExtended); }
        | SFSXX _LPAREN sExpr _COMMA xExpr _COMMA xExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionSXX ($3.yyString, $5.yy.yyExtended, $7.yy.yyExtended); }
        | SFV _LPAREN _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionV (); }
        | SFOG _LPAREN gExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionOG (Owner, $3); }
        | SFOV _LPAREN _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionOV (Owner); }
        | SFOS _LPAREN sExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionOS (Owner, $3.yyString); }
        | SFOSB _LPAREN sExpr _COMMA bExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionOSB (Owner, $3.yyString, $5.yy.yyBoolean); }
        | SFOSS _LPAREN sExpr _COMMA sExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionOSS (Owner, $3.yyString, $5.yyString); }
        | SFOSX _LPAREN sExpr _COMMA xExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionOSX (Owner, $3.yyString, $5.yy.yyExtended); }
        | SFOSSS _LPAREN sExpr _COMMA sExpr _COMMA sExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionOSSS (Owner, $3.yyString, $5.yyString, $7.yyString); }
        | SFOSSSS _LPAREN sExpr _COMMA sExpr _COMMA sExpr _COMMA sExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionOSSSS (Owner, $3.yyString, $5.yyString, $7.yyString, $9.yyString); }
        | SFX _LPAREN xExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionX ($3.yy.yyExtended); }
        | SFBSS _LPAREN bExpr _COMMA sExpr _COMMA sExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionBSS ($3.yy.yyBoolean, $5.yyString, $7.yyString); }
        | SFD _LPAREN dExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionD ($3.yy.yyDateTime); }
        | SFDS _LPAREN dExpr _COMMA sExpr _RPAREN { if DoIt then $$.yyString := ($1.yy.yyObject as TBind).yy.yySFunctionDS ($3.yy.yyDateTime, $5.yyString); }
	;

xExpr:    xExpr _AND xExpr	 { $$.yy.yyExtended := Trunc ($1.yy.yyExtended) and Trunc($3.yy.yyExtended); }
	| xExpr _PLUS xExpr	 { $$.yy.yyExtended := $1.yy.yyExtended + $3.yy.yyExtended; }
	| xExpr _MINUS xExpr	 { $$.yy.yyExtended := $1.yy.yyExtended - $3.yy.yyExtended; }
	| xExpr _STAR xExpr	 { $$.yy.yyExtended := $1.yy.yyExtended * $3.yy.yyExtended; }
	| xExpr _SLASH xExpr	 { if DoIt then $$.yy.yyExtended := $1.yy.yyExtended / $3.yy.yyExtended; }
        | xExpr _MOD xExpr      { $$.yy.yyExtended := $1.yy.yyExtended - trunc ($1.yy.yyExtended / $3.yy.yyExtended) * $3.yy.yyExtended; }
        | xExpr _UPARROW xExpr  { $$.yy.yyExtended := Power ($1.yy.yyExtended , $3.yy.yyExtended); }
	| _LPAREN xExpr _RPAREN { $$.yy.yyExtended := $2.yy.yyExtended; }
	| _MINUS xExpr          { $$.yy.yyExtended := -$2.yy.yyExtended; }
          %prec UMINUS
	| _REALNUMBER           { $$.yy.yyExtended := $1.yy.yyExtended; }
        | XFLD
          { yylval := $1;
            if DoIt then GetDataEvent ($1, $$);
          }
        | IFLD
          { yylval := $1;
            if DoIt then GetDataEvent ($1, $$);
          }
        | XFD _LPAREN dExpr _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionD ($3.yy.yyDateTime); }
        | XFV _LPAREN _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionV(); }
        | XFG _LPAREN gExpr _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionG ($3); }
        | XFGG _LPAREN gExpr _COMMA gExpr _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionGG ($3, $5); }
        | XFS _LPAREN sExpr _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionS ($3.yyString); }
        | XFSX _LPAREN sExpr _COMMA xExpr _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionSX ($3.yyString, $5.yy.yyExtended); }
        | XFX _LPAREN xExpr _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionX ($3.yy.yyExtended); }
        | XFXX _LPAREN xExpr _COMMA xExpr _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionXX ($3.yy.yyExtended, $5.yy.yyExtended); }
        | XFFRAME _LPAREN FRAME_ _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionObject ($3.yy.yyObject); }
        | XFOV _LPAREN _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionOV (Owner); }
        | XFOS _LPAREN sExpr _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionOS (Owner, $3.yyString); }
        | XFOSX _LPAREN sExpr _COMMA xExpr _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionOSX (Owner, $3.yyString, $5.yy.yyExtended); }
        | XFOX _LPAREN xExpr _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionOX (Owner, $3.yy.yyExtended); }
        | XFOXX _LPAREN xExpr _COMMA xExpr _RPAREN { if DoIt then $$.yy.yyExtended := ($1.yy.yyObject as TBind).yy.yyXFunctionOXX (Owner, $3.yy.yyExtended, $5.yy.yyExtended); }
	;

BooleanExpr:
          bExpr
          {
              Cond := $1.yy.yyBoolean;
          }
        ;

bExpr:    bExpr _OR bExpr   { $$.yy.yyBoolean := $1.yy.yyBoolean or $3.yy.yyBoolean; }
        | bExpr _AND bExpr  { $$.yy.yyBoolean := $1.yy.yyBoolean and $3.yy.yyBoolean; }
        | _NOT bExpr        { $$.yy.yyBoolean := not $2.yy.yyBoolean; }
        | _LPAREN bExpr _RPAREN {$$.yy.yyBoolean := $2.yy.yyBoolean; }
	| _FALSE            { $$.yy.yyBoolean := False; }
	| _TRUE             { $$.yy.yyBoolean := True; }
        | RelationalExpression {$$.yy.yyBoolean := $1.yy.yyBoolean; }
        ;

RelationalExpression:
          xExpr {$$.yy.yyBoolean := ($$.yy.yyExtended <> 0); }
        | xExpr _LT xExpr {$$.yy.yyBoolean := ($1.yy.yyExtended < $3.yy.yyExtended);}
        | xExpr _LE xExpr {$$.yy.yyBoolean := ($1.yy.yyExtended <= $3.yy.yyExtended);}
        | xExpr _EQUAL xExpr {$$.yy.yyBoolean := ($1.yy.yyExtended = $3.yy.yyExtended);}
        | xExpr _GE xExpr {$$.yy.yyBoolean := ($1.yy.yyExtended >= $3.yy.yyExtended);}
        | xExpr _GT xExpr {$$.yy.yyBoolean := ($1.yy.yyExtended > $3.yy.yyExtended);}
        | xExpr _NOTEQUAL xExpr {$$.yy.yyBoolean := ($1.yy.yyExtended <> $3.yy.yyExtended);}
        | sExpr {$$.yy.yyBoolean := ($$.yyString <> ''); }
        | sExpr _LT sExpr {$$.yy.yyBoolean := ($1.yyString < $3.yyString);}
        | sExpr _LE sExpr {$$.yy.yyBoolean := ($1.yyString <= $3.yyString);}
        | sExpr _EQUAL sExpr {$$.yy.yyBoolean := ($1.yyString = $3.yyString);}
        | sExpr _GE sExpr {$$.yy.yyBoolean := ($1.yyString >= $3.yyString);}
        | sExpr _GT sExpr {$$.yy.yyBoolean := ($1.yyString > $3.yyString);}
        | sExpr _NOTEQUAL sExpr {$$.yy.yyBoolean := ($1.yyString <> $3.yyString);}
        | dExpr {$$.yy.yyBoolean := ($$.yy.yyDateTime <> 0); }
        | dExpr _LT dExpr {$$.yy.yyBoolean := ($1.yy.yyDateTime < $3.yy.yyDateTime);}
        | dExpr _LE dExpr {$$.yy.yyBoolean := ($1.yy.yyDateTime <= $3.yy.yyDateTime);}
        | dExpr _EQUAL dExpr {$$.yy.yyBoolean := ($1.yy.yyDateTime = $3.yy.yyDateTime);}
        | dExpr _GE dExpr {$$.yy.yyBoolean := ($1.yy.yyDateTime >= $3.yy.yyDateTime);}
        | dExpr _GT dExpr {$$.yy.yyBoolean := ($1.yy.yyDateTime > $3.yy.yyDateTime);}
        | dExpr _NOTEQUAL dExpr {$$.yy.yyBoolean := ($1.yy.yyDateTime <> $3.yy.yyDateTime);}
        ;

SqlSelectStatement:
          _SQLSELECT
          /* yySql.tag is used to keep track of qrt open state */
          {
            PushBoolean (Cond);
            PushObject (yySql);
            yySql := $1;
            if (DoIt) then
            begin
              if (yySql.Tag <> 1) then
              begin
                if not Assigned (FOnNeedSqlOpen) then
                  RaiseException ('No procedure assigned to OnNeedSqlOpen')
                else
                  FOnNeedSqlOpen (Self, yySql.yy.yyObject);
                if not Assigned (FOnNeedSqlFirstRow) then
                  RaiseException ('No procedure assigned to OnNeedSqlFirstRow')
                else
                  FOnNeedSqlFirstRow (Self, yySql.yy.yyObject, Cond);
              end
              else
              begin
                if not Assigned (FOnNeedSqlNextRow) then
                  RaiseException ('No procedure assigned to OnNeedSqlNextRow')
                else
                  FOnNeedSqlNextRow (Self, yySql.yy.yyObject, Cond);
              end;
              yySql.Tag := 1;
            end;
            PushBoolean (DoIt);
            if (DoIt) then DoIt := Cond;
          }
          OptionalSqlLoop
          {
            DoIt := PopBoolean;
            if (DoIt) then
            begin
              if (Cond) then
              begin
                yyTemp.Token := _SEMICOLON;
                yyTemp.NextToken := yySql;
                yyjump (yyTemp);
              end
              else
              begin
                if not Assigned (FOnNeedSqlClose) then
                  RaiseException ('No procedure assigned to OnNeedSqlClose')
                else
                  FOnNeedSqlClose (Self, yySql.yy.yyObject);
                yySql.Tag := 0;
              end;
            end;
            yySql := PopObject as YYSType;
            Cond := PopBoolean;
          }
        ;

DeclareForEachStatement:
          _FOR _EACH
          {
            PushObject (pFed);
            PushObject (cFed);
            if not Assigned (FOnStoreObject) then
              RaiseException ('No procedure assigned to OnStoreObject');
            cFed := TFed.Create;
            cFed.Parent := pFed;
            cFed.isCursor := True;
            FOnStoreObject (Self, cFed);
            $1.yy.yyObject := cFed;
            $1.yyRead := $1.yy;
            pFed := cFed;
          }
          DeclareGroup OptionalAlias _DO BlokStatement
          {
            $1.Token := _FOREACH;
            $1.NextToken := $6.NextToken;
            cFed := TFed (PopObject);
            pFed := TFed (PopObject);
          }
          ;

DeclareWithStatement:
          _WITH
          {
            PushObject (pFed);
            PushObject (cFed);
            if not Assigned (FOnStoreObject) then
              RaiseException ('No procedure assigned to OnStoreObject');
            cFed := TFed.Create;
            cFed.Parent := pFed;
            cFed.isCursor := False;
            FOnStoreObject (Self, cFed);
            $1.yy.yyObject := cFed;
            $1.yyRead := $1.yy;
            pFed := cFed;
          }
          DeclareGroup OptionalAlias _DO BlokStatement
          {
            $1.Token := _WITHDO;
            $1.NextToken := $5.NextToken;
            cFed := TFed (PopObject);
            pFed := TFed (PopObject);
          }
          ;

DeclareWithNewStatement:
          _WITH _NEW
          {
            PushObject (pFed);
            PushObject (cFed);
            if not Assigned (FOnStoreObject) then
              RaiseException ('No procedure assigned to OnStoreObject');
            cFed := TFed.Create;
            cFed.Parent := pFed;
            cFed.isCursor := False;
            FOnStoreObject (Self, cFed);
            $1.yy.yyObject := cFed;
            $1.yyRead := $1.yy;
            pFed := cFed;
          }
          DeclareGroup OptionalAlias _DO BlokStatement
          {
            $1.Token := _WITHNEWDO;
            $1.NextToken := $6.NextToken;
            cFed := TFed (PopObject);
            pFed := TFed (PopObject);
          }
          ;

DeclareGroup:
          GFLD
          {
            yylval := $1;
            GetObjectEvent ($1, yyTemp);
            cFed.TokenString := $1.TokenString;
            cFed.Name := $1.TokenString;
            cFed.FirstBind := TCustomBindable(yyTemp.yy.yyObject);
            if not (cFed.FirstBind is TXml) then
              yyerror ('only implemented for XML type data');
            (cFed.FirstBind as TXml).ExtendRecursivity;
            cFed.isSubElement := False;
            cFed.isDynamic := False;
          }
        | DynToken
          {
            yylval := $1;
            cFed.isSubElement := True;
            if cFed.Parent = nil then yyerror ('syntax error, no parent for: ' + $1.TokenString);
            cFed.TokenString := $1.TokenString;
            //* do not assign cFed.Name here! */
            cFed.Anchor := cFed.FindAnchor (cFed.TokenString);
            cFed.FirstBind := cFed.Anchor.PrepareBindableOnAliasField (cFed.TokenString);
            if not Assigned (cFed.FirstBind) then
            begin
              if Assigned (cFed.Parent)
              and Assigned (cFed.Parent.FirstBind) then
                yyerror (cFed.Name + ' not found in ' + cFed.Parent.FirstBind.FullCaption)
              else
                yyerror (cFed.Name + ' not found');
            end
            else
              (cFed.FirstBind as TXml).ExtendRecursivity;
            cFed.isDynamic := True;
          }
        ;

DynToken:
          _DYNFLD
        | _FIELDID
        ;

OptionalAlias:
          /* void */
        | _NOID
          {
            cFed.Alias := $1.TokenString;
          }
        ;

WithNewStatement:
          _WITHNEWDO
          {
            PushObject (cFed);
            cFed := $1.yy.yyObject as TFed;
            if DoIt then
              if cFed.isDynamic then
                cFed.FirstBind := cFed.Parent.FindBindableOnAliasField (cFed.TokenString);
            if DoIt then cFed.New;
          }
          BlokStatement
          {
            cFed := PopObject as TFed;
          }
          ;

SlStatement:
          _FOR _EACH SLFOS _LPAREN sExpr _RPAREN _AS SFLD
          {
            PushBoolean (DoIt);
            if DoIt then
            begin
              if not ($1.yy.yyObject is TParserStringList) then
              begin
                $1.yy.yyObject := ($3.yy.yyObject as TBind).yy.yySLFunctionOS (Owner, $5.yyString);
                $1.yyRead := $1.yy;
                $1.Tag := -1;
              end;
              $1.Tag := $1.Tag + 1;
              DoIt := ($1.Tag < ($1.yy.yyObject as TParserStringList).Count);
              if DoIt then with $1.yy.yyObject as TParserStringList do
              begin
                yytemp.yyString := Strings[$1.Tag];
                PutDataEvent ($8, yytemp);
                ProcessIndex ($1.Tag);
              end;
            end;
          }
          _DO BlokStatement
          {
            if DoIt then
              yyjump ($1)
            else
            begin
              FreeAndNil ($1.yy.yyObject);
              $1.yyRead := $1.yy;
            end;
            DoIt := PopBoolean;
          }
        | _FOR _EACH SLFOSS _LPAREN sExpr _COMMA sExpr _RPAREN _AS SFLD
          {
            PushBoolean (DoIt);
            if DoIt then
            begin
              if not ($1.yy.yyObject is TParserStringList) then
              begin
                $1.yy.yyObject := ($3.yy.yyObject as TBind).yy.yySLFunctionOSS ($1.yy.yyObject, $5.yyString, $7.yyString);
                $1.yyRead := $1.yy;
                $1.Tag := -1;
              end;
              $1.Tag := $1.Tag + 1;
              DoIt := ($1.Tag < ($1.yy.yyObject as TParserStringList).Count);
              if DoIt then with $1.yy.yyObject as TParserStringList do
              begin
                yytemp.yyString := Strings[$1.Tag];
                PutDataEvent ($10, yytemp);
                ProcessIndex ($1.Tag);
              end;
            end;
          }
          _DO BlokStatement
          {
            if DoIt then
              yyjump ($1)
            else
            begin
              FreeAndNil ($1.yy.yyObject);
              $1.yyRead := $1.yy;
            end;
            DoIt := PopBoolean;
          }
          ;

ForEachStatement:
          _FOREACH
          {
            PushBoolean (Cond);
            cFed := $1.yy.yyObject as TFed;
            with (cFed) do
            begin
              if (DoIt) then
              begin
                if (not isOpen) then
                begin
                  Open;
                  First;
                end
                else
                  Next;
              end;
              PushObject (cFed);
              PushBoolean (DoIt);
              DoIt := DoIt and (not Eof);
            end;
          }
          BlokStatement
          {
            DoIt := PopBoolean;
            cFed := PopObject as TFed;
            if (DoIt) then
            begin
              if (not cFed.Eof) then
                yyjump ($1)
              else
              begin
                $1.Tag := 0;
                ($1.yy.yyObject as TFed).Close;
              end;
            end;
            Cond := PopBoolean;
          }
          ;

WithStatement:
          _WITHDO
          {
            PushObject (pFed);
            PushObject (cFed);
            cFed := $1.yy.yyObject as TFed;
            if DoIt then
              if cFed.isDynamic then
                cFed.FirstBind := cFed.Parent.FindBindableOnAliasField (cFed.TokenString);
            PushBoolean (DoIt);
          }
          BlokStatement
          {
            DoIt := PopBoolean;
            cFed := PopObject as TFed;
            pFed := PopObject as TFed;
          }
          ;

DeclareSqlSelectStatement:
          _EXEC_SQLSELECT
          {
            SqlUsed := True;
            PushObject (yySql as TObject);
            yySql := $1;
            if not Assigned (FOnCreateQuery) then
              RaiseException ('No procedure assigned to OnCreateQuery')
            else
              FOnCreateQuery (Self, yySql.yy.yyObject);
            if not Assigned (FOnHaveSqlToken) then
              RaiseException ('No procedure assigned to OnHaveSqlToken')
            else
              FOnHaveSqlToken (Self, yySql.yy.yyObject, 'Select');
            yySql.yyRead := yySql.yy;
          }
          DeclareSqlTokenList
          OptionalSqlLoop
          {
            yySql.Token := _SQLSELECT;
            yySql := PopObject as YYSType;
          }
        ;

DeclareSqlTokenList:
          DeclareSqlToken
        | DeclareSqlTokenList DeclareSqlToken
        ;

DeclareSqlToken:
           _SQLTOKEN
           {
             yySql.NextToken := $1.NextToken; // to skip the declaretokens at execution
             if not Assigned (FOnHaveSqlToken) then
               RaiseException ('No procedure assigned to OnHaveSqlToken')
             else
               FOnHaveSqlToken (Self, yySql.yy.yyObject, ' ' + $1.TokenString);
           }
         | _AS_FIELDID
           {
             yySql.NextToken := $1.NextToken; // to skip the declaretokens at execution
             if not Assigned (FOnHaveSqlBind) then
               RaiseException ('No procedure assigned to OnHaveSqlBind')
             else
               FOnHaveSqlBind (Self, yySql.yy.yyObject, $1.yy.yyObject as TBind);
           }
         | _SQLPARAM_FIELD
           {
             yySql.NextToken := $1.NextToken; // to skip the declaretokens at execution
             if not Assigned (FOnHaveSqlParam) then
               RaiseException ('No procedure assigned to OnHaveSqlParam')
             else
               FOnHaveSqlParam (Self, yySql.yy.yyObject, $1.yy.yyObject as TBind);
             if not Assigned (FOnHaveSqlToken) then
               RaiseException ('No procedure assigned to OnHaveSqlToken')
             else
               FOnHaveSqlToken (Self, yySql.yy.yyObject, ':' + $1.TokenString);
           }
         ;

OptionalSqlLoop:
          _SEMICOLON
        | _LOOP BlokStatement
        ;

SqlExecStatement:
          _SQLEXEC
          {
            PushObject (yySql as TObject);
            yySql := $1;
            if (DoIt) then
            begin
              if not Assigned (FOnNeedSqlExec) then
                RaiseException ('No procedure assigned to OnNeedSqlExec')
              else
                FOnNeedSqlExec (Self, yySql.yy.yyObject);
            end;
            yySql := PopObject as YYSType;
          }
        ;

DeclareSqlInsertStatement:
          _EXEC_SQLINSERT
          {
            SqlUsed := True;
            PushObject (yySql as TObject);
            yySql := $1;
            if not Assigned (FOnCreateQuery) then
              RaiseException ('No procedure assigned to OnCreateQuery')
            else
              FOnCreateQuery (Self, yySql.yy.yyObject);
            if not Assigned (FOnHaveSqlToken) then
              RaiseException ('No procedure assigned to OnHaveSqlToken')
            else
              FOnHaveSqlToken (Self, yySql.yy.yyObject, 'Insert');
            yySql.yyRead := yySql.yy;
          }
          DeclareSqlInsertTokenList
          {
            yySql.Token := _SQLEXEC;
            FOnFinishInsertQuery ( Self
                                 , yySql.yy.yyObject
                                 );
            yySql := PopObject as YYSType;
          }
        ;

DeclareSqlInsertTokenList:
          DeclareSqlInsertToken
        | DeclareSqlInsertTokenList DeclareSqlInsertToken
        ;

DeclareSqlInsertToken:
           _SQLTOKEN
           {
             yySql.NextToken := $1.NextToken;
             if not Assigned (FOnHaveSqlToken) then
               RaiseException ('No procedure assigned to OnHaveSqlToken')
             else
               FOnHaveSqlToken (Self, yySql.yy.yyObject, ' ' + $1.TokenString);
           }
         | _AS_FIELDID
           {
             yySql.NextToken := $1.NextToken;
             if not Assigned (FOnHaveSqlInsertParam) then
               RaiseException ('No procedure assigned to OnHaveSqlInsertParam')
             else
               FOnHaveSqlInsertParam (Self, yySql.yy.yyObject, $1.yy.yyObject as TBind);
           }
         ;

DeclareSqlExecStatement:
          _EXEC_SQLEXEC
          {
            SqlUsed := True;
            PushObject (yySql as TObject);
            yySql := $1;
            if not Assigned (FOnCreateQuery) then
              RaiseException ('No procedure assigned to OnCreateQuery')
            else
              FOnCreateQuery (Self, yySql.yy.yyObject);
            if not Assigned (FOnHaveSqlToken) then
              RaiseException ('No procedure assigned to OnHaveSqlToken')
            else
            begin
              if (Pos ('DELETE', UpperCase ($1.TokenString)) > 0) then
                FOnHaveSqlToken (Self, yySql.yy.yyObject, 'Delete')
              else
                FOnHaveSqlToken (Self, yySql.yy.yyObject, 'Update');
            end;
            yySql.yyRead := yySql.yy;
          }
          DeclareSqlExecTokenList
          {
            yySql.Token := _SQLEXEC;
            yySql := PopObject as YYSType;
          }
        ;

DeclareSqlExecTokenList:
          DeclareSqlExecToken
        | DeclareSqlExecTokenList DeclareSqlExecToken
        ;

DeclareSqlExecToken:
           _SQLTOKEN
           {
             yySql.NextToken := $1.NextToken;
             if not Assigned (FOnHaveSqlToken) then
               RaiseException ('No procedure assigned to OnHaveSqlToken')
             else
               FOnHaveSqlToken (Self, yySql.yy.yyObject, ' ' + $1.TokenString);
           }
         | _SQLPARAM_FIELD
           {
             yySql.NextToken := $1.NextToken;
             if not Assigned (FOnHaveSqlParam) then
               RaiseException ('No procedure assigned to OnHaveSqlParam')
             else
               FOnHaveSqlParam (Self, yySql.yy.yyObject, $1.yy.yyObject as TBind);
             if not Assigned (FOnHaveSqlToken) then
               RaiseException ('No procedure assigned to OnHaveSqlToken')
             else
               FOnHaveSqlToken (Self, yySql.yy.yyObject, ':' + $1.TokenString);
           }
         ;
%%



