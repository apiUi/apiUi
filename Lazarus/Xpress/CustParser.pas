{
This file is part of the apiUi project
Copyright (c) 2009-2021 by Jan Bouwman

See the file COPYING, included in this distribution,
for details about the copyright.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
unit CustParser;

interface
uses
  Classes, ParserClasses, SysUtils, Bind
;

const
  InternalStackSize = 2048;

type TCustParser = class (TObject)
  private
    FOnGetAbortPressed, FOnGetDoExit: TBooleanFunction;
    fDoIt: Boolean;
    function getDoIt: Boolean;
protected
  fOwner: TObject;
  LexItem: YYSType;
  Cond: Boolean;
  Stack: array [0..InternalStackSize] of YYRType;
  StackIndex: Integer;
  FOnHaveScanned: TOnHaveScannedEvent;
  FOnHaveData: TOnHaveDataEvent;
  FOnEvaluateString: TOnEvaluateStringEvent;
  FOnPutData: TOnPutDataEvent;
  FOnGetData: TOnGetDataEvent;
  FOnGetObject: TOnGetDataEvent;
  FOnStoreObject: TOnStoreObject;
  FOnCreateQuery: TOnCreateObject;
  FOnHaveSqlToken: TOnHaveSqlToken;
  FOnHaveSqlBind: TOnHaveSqlBind;
  FOnHaveSqlParam: TOnHaveSqlParam;
  FOnHaveSqlInsertParam: TOnHaveSqlParam;
  FOnNeedSqlExec: TOnNeedSqlExec;
  FOnNeedSqlOpen: TOnNeedSqlOpen;
  FOnNeedSqlClose: TOnNeedSqlClose;
  FOnNeedSqlNextRow: TOnNeedSqlNextRow;
  FOnNeedSqlFirstRow: TOnNeedSqlFirstRow;
  FOnFinishInsertQuery: TOnFinishInsertQuery;
  FToken     : Integer; (* yylex return value *)
  FOnError: TOnErrorEvent;

  InternalBinds: TBindList;
  yychar   : Integer; (* current lookahead character *)
  yynerrs  : Integer; (* current number of syntax errors reported by the
                         parser *)
  yydebug  : Boolean; (* set to true to enable debugging output of parser *)
  yyflag    : ( yyfnone, yyfaccept, yyfabort, yyferror );
  yyerrflag : Integer;
  yyTemp: YYSType;
  procedure PutDataEvent (Adress: YYSType; Data: YYSType);
  procedure GetDataEvent (Adress: YYSType; Data: YYSType);
  procedure GetObjectEvent (Adress: YYSType; Data: YYSType);
  procedure RaiseException ( msg : String );
  function yylex: Integer; virtual;
  procedure yyerror ( msg : String );
  procedure yyclearin;
  procedure yyaccept;
  procedure yyabort;
  procedure yyerrlab;
  procedure yyerrok;
  procedure yyjump (yyval: YYSType); virtual;
  procedure yyEvaluateString (arg: String); virtual;
  procedure yyInitParse; virtual;
  procedure yyTermParse; virtual;
  function yyparse: Integer; virtual;
  published

public
  Preparing: Boolean;
  yylval : YYSType;
  LexItems : YYSType;
  ArgumentBinds: TBindList;
  SqlUsed: Boolean;
  property Owner: TObject read fOwner;
  property DoIt: Boolean read getDoIt write fDoIt;
  property OnGetAbortPressed: TBooleanFunction read FOnGetAbortPressed write FOnGetAbortPressed;
  property OnGetDoExit: TBooleanFunction read FOnGetDoExit write FOnGetDoExit;
  property OnPutData: TOnPutDataEvent read FOnPutData write FOnPutData;
  property OnGetData: TOnGetDataEvent read FOnGetData write FOnGetData;
  property OnGetObject: TOnGetDataEvent read FOnGetObject write FOnGetObject;
  property OnHaveScanned: TOnHaveScannedEvent read FOnHaveScanned write FOnHaveScanned;
  property OnHaveData: TOnHaveDataEvent read FOnHaveData write FOnHaveData;
  property OnEvaluateString: TOnEvaluateStringEvent read FOnEvaluateString write FOnEvaluateString;
  property OnError: TOnErrorEvent read FOnError write FOnError;
  property OnStoreObject: TOnStoreObject read FOnStoreObject write FOnStoreObject;
  property OnCreateQuery: TOnCreateObject read FOnCreateQuery write FOnCreateQuery;
  property OnHaveSqlToken: TOnHaveSqlToken read FOnHaveSqlToken write FOnHaveSqlToken;
  property OnHaveSqlBind: TOnHaveSqlBind read FOnHaveSqlBind write FOnHaveSqlBind;
  property OnHaveSqlParam: TOnHaveSqlParam read FOnHaveSqlParam write FOnHaveSqlParam;
  property OnHaveSqlInsertParam: TOnHaveSqlParam read FOnHaveSqlInsertParam write FOnHaveSqlInsertParam;
  property OnNeedSqlExec: TOnNeedSqlExec read FOnNeedSqlExec write FOnNeedSqlExec;
  property OnNeedSqlOpen: TOnNeedSqlOpen read FOnNeedSqlOpen write FOnNeedSqlOpen;
  property OnNeedSqlClose: TOnNeedSqlClose read FOnNeedSqlClose write FOnNeedSqlClose;
  property OnNeedSqlNextRow: TOnNeedSqlNextRow read FOnNeedSqlNextRow write FOnNeedSqlNextRow;
  property OnNeedSqlFirstRow: TOnNeedSqlFirstRow read FOnNeedSqlFirstRow write FOnNeedSqlFirstRow;
  property OnFinshInsertQuery: TOnFinishInsertQuery read FOnFinishInsertQuery write FOnFinishInsertQuery;
  procedure PushBoolean (arg: Boolean);
  function PopBoolean: Boolean;
  procedure PushPointer (arg: Pointer);
  function PopPointer: Pointer;
  procedure PushInteger (arg: Integer);
  function PopInteger: Integer;
  function PushObject (arg: TObject): TObject;
  function PopObject: TObject;
  procedure HaveData (aObject: TObject; aString: String);
  procedure Prepare; virtual;
  function Execute: Integer; virtual;
  constructor Create (aOwner: TObject);
  destructor Destroy; override;
end;

implementation

constructor TCustParser.Create(aOwner: TObject);
begin
  inherited Create;
  fOwner := aOwner;
  StackIndex := 0;
  InternalBinds := TBindList.Create;
  InternalBinds.Sorted := True;
  InternalBinds.Duplicates := DupError;
  ArgumentBinds := TBindList.Create;
  ArgumentBinds.Sorted := True;
  ArgumentBinds.Duplicates := DupError;
  yyTemp := YYSType.Create;
end;

destructor TCustParser.Destroy;
begin
  InternalBinds.Clear;
  InternalBinds.Free;
  ArgumentBinds.Clear;
  ArgumentBinds.Free;
  yyTemp.Free;
  inherited Destroy;
end;

procedure TCustParser.PushBoolean (arg: Boolean);
begin
  if StackIndex > InternalStackSize then
    RaiseException ('Internal stack overflow')
  else
  begin
    Stack [StackIndex].yyBoolean := arg;
    Inc (StackIndex);
  end;
end;

function TCustParser.PopBoolean: Boolean;
begin
  if StackIndex <= 0 then
    RaiseException ('Internal stack underflow')
  else
  begin
    Dec (StackIndex);
    result := Stack [StackIndex].yyBoolean;
  end;
end;

procedure TCustParser.PushInteger (arg: Integer);
begin
  if StackIndex > InternalStackSize then
    RaiseException ('Internal stack overflow')
  else
  begin
    Stack [StackIndex].yyInteger := arg;
    Inc (StackIndex);
  end;
end;

function TCustParser.PopInteger: Integer;
begin
  if StackIndex <= 0 then
    RaiseException ('Internal stack underflow')
  else
  begin
    Dec (StackIndex);
    result := Stack [StackIndex].yyInteger ;
  end;
end;

procedure TCustParser.PushPointer (arg: Pointer);
begin
  if StackIndex > InternalStackSize then
    RaiseException ('Internal stack overflow')
  else
  begin
    Stack [StackIndex].yyPointer := arg;
    Inc (StackIndex);
  end;
end;

function TCustParser.PopPointer: Pointer;
begin
  if StackIndex <= 0 then
    RaiseException ('Internal stack underflow')
  else
  begin
    Dec (StackIndex);
    result := Stack [StackIndex].yyPointer;
  end;
end;

function TCustParser.PushObject (arg: TObject): TObject;
begin
  result := arg;
  if StackIndex > InternalStackSize then
    RaiseException ('Internal stack overflow')
  else
  begin
    Stack [StackIndex].yyObject := arg;
    Inc (StackIndex);
  end;
end;

function TCustParser.PopObject: TObject;
begin
  if StackIndex <= 0 then
    RaiseException ('Internal stack underflow')
  else
  begin
    Dec (StackIndex);
    result := Stack [StackIndex].yyObject;
  end;
end;

procedure TCustParser.PutDataEvent ( Adress: YYSType; Data: YYSType);
begin
  if Assigned (FOnPutData) then
    FOnPutData (Self, Adress, Data);
end;

procedure TCustParser.GetDataEvent ( Adress: YYSType; Data: YYSType);
begin
  if Assigned (FOnGetData) then
    FOnGetData (Self, Adress, Data);
end;

function TCustParser.getDoIt: Boolean;
begin
  Result := fDoIt
         and (   (not Assigned (FOnGetAbortPressed))
              or (not FOnGetAbortPressed())
             )
         and (   (not Assigned (FOnGetDoExit))
              or (not FOnGetDoExit())
             );
end;

procedure TCustParser.GetObjectEvent ( Adress: YYSType; Data: YYSType);
begin
  if Assigned (FOnGetObject) then
    FOnGetObject (Self, Adress, Data);
end;

procedure TCustParser.RaiseException ( msg : String );
begin
  if Assigned (FOnError) then
    FOnError ( Self
             , yylval.LineNumber
             , yylval.ColumnNumber
             , yylval.Offset
             , yylval.TokenString
             , msg)
  else
    Raise EParserException.Create (msg)
  ;
end(*RaiseException*);

function TCustParser.yylex: Integer;
begin
  if LexItem = nil then
    result := -1
  else
  begin
    LexItem.yyString := LexItem.yyStringRead;
    LexItem.yy := LexItem.yyRead;
    yylval := LexItem;
    result := LexItem.Token;
    LexItem := LexItem.NextToken;
  end;
end;

procedure TCustParser.yyerror ( msg : String );
  begin
    RaiseException (msg);
  end(*yyerrmsg*);

procedure TCustParser.yyclearin;
  begin
    yychar := -1;
  end(*yyclearin*);

procedure TCustParser.yyaccept;
  begin
    yyflag := yyfaccept;
  end(*yyaccept*);

procedure TCustParser.yyabort;
  begin
    yyflag := yyfabort;
  end(*yyabort*);

procedure TCustParser.yyerrlab;
  begin
    yyflag := yyferror;
  end(*yyerrlab*);

procedure TCustParser.yyerrok;
  begin
    yyerrflag := 0;
  end(*yyerrork*);

function TCustParser.yyparse : Integer;
begin
  result := 1;
end;

procedure TCustParser.yyInitParse;
begin
end;

procedure TCustParser.yyTermParse;
begin
end;

procedure TCustParser.yyEvaluateString (arg: String);
begin
  if not Assigned (FOnEvaluateString) then
    RaiseException ('No procedure assigned to OnEvaluateString')
  else
    FOnEvaluateString (Self, arg);
end;

procedure TCustParser.yyjump (yyval: YYSType);
begin
  LexItem := yyval;
end;

procedure TCustParser.HaveData (aObject: TObject; aString: String);
begin
  OnHaveData (Self, aString);
end;

procedure TCustParser.Prepare;
begin
  fDoIt := False;
  Preparing := True;
  try
    Execute;
  finally
    Preparing := False;
  end;
end;

function TCustParser.Execute: Integer;
begin
  StackIndex := 0;
  LexItem := LexItems;
  result := yyparse;
end;

end.
