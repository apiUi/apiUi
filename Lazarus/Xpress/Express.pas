{$mode delphi}
unit Express;

interface
uses Bind
   , Classes
   , ParserClasses
   , Parser
   , CustParser, Scanner, CustScanner
   , sqldb
   , XpQuery
;

const
  InternalStackSize = 256;
  InitialState = 0;
  InSqlState = 1;
  LayoutState = 2;

type

{ TExpress }

 TExpress = class (TObject)
private
  rootBlock, currBlock, prntBlock: TBlock;
  ScannerState: Integer;
  Stack: array [0..InternalStackSize] of Integer;
  StackIndex: Integer;
  LexItem, PrevItem: YYSType;
  LexicalList: YYSType;
  fBindList: TBindList;
  Scanner: TScanner;
  Parser: TParser;
  Blocks: TStringList;
  ObjList: TParserClassList;
  FDatabase: TSQLConnection;
  XpQueryList: TXpQueryList;
  fXpLoopQuery: TXpQuery;
  FOnError: TOnErrorEvent;
  FOnHaveData: TOnHaveDataEvent;
  FOnNeedData: TOnNeedDataEvent;
  OneLiner: String;
  FirstData: Boolean;
  fFunctionProtoTypes: TStringList;
  fTextLines: TStringList;
  fTextLineNo: Integer;
  fOnGetAbortPressed, fOnGetDoExit: TBooleanFunction;
  function GetSqlUsed: Boolean;
  procedure ExpressError ( Sender: TObject
                         ; LineNumber: Integer
                         ; ColumnNumber: Integer
                         ; Offset: Integer
                         ; TokenString: String
                         ; Data: String
                         );
  procedure ClearLexicalList;
  procedure OnToken (Sender: TObject);
  procedure ScannerNeedsData ( Sender:TObject
                             ; var MoreData: Boolean
                             ; var Data: String
                             );
  procedure StoreObject (Sender: TObject; aObject: TObject);
  procedure CreateQuery (Sender: TObject; var Query: TObject);
  procedure HaveSqlToken (Sender: TObject; Query: TObject; Str: String);
  procedure HaveSqlBind (Sender: TObject; Query: TObject; Bind: TBind);
  procedure HaveSqlParam (Sender: TObject; Query: TObject; Param: TBind);
  procedure HaveSqlInsertParam (Sender: TObject; Query: TObject; Param: TBind);
  procedure NeedSqlExec (Sender: TObject; Query: TObject);
  procedure NeedSqlOpen (Sender: TObject; Query: TObject);
  procedure NeedSqlClose (Sender: TObject; Query: TObject);
  procedure NeedNextSqlRow (Sender: TObject; Query: TObject; var MoreData: Boolean);
  procedure NeedFirstSqlRow (Sender: TObject; Query: TObject; var MoreData: Boolean);
  procedure FinishInsertQuery (Sender: TObject; Query: TObject);
  function IsKeyWord (id : string; var token : integer; var Address: Pointer) : boolean;
  function TokenToFloat (arg: String): Extended;
  function LayoutFieldId2FieldId (arg: String): String;
  procedure ScannerError (Sender: TObject; Data: String);
  function AsFieldId2FieldId (arg: String): String;
  procedure PassString (Sender: TObject; var MoreData: Boolean;
  var Data: string);
  procedure PassText (Sender: TObject; var MoreData: Boolean;
  var Data: string);
  procedure EvaluateString (Sender: TObject; arg: String);
  procedure PutData  (Sender: TObject; Adress: YYSType; Data: YYSType);
  procedure GetData  (Sender: TObject; Adress: YYSType; Data: YYSType);
  procedure GetObject  (Sender: TObject; Adress: YYSType; Data: YYSType);
  procedure PushInteger (arg: Integer);
  function PopInteger: Integer;
  procedure HaveData (aObject: TObject; aString: String);
    function getContext: TObject;
    procedure setContext(const Value: TObject);
    function getScriptText: String;
    procedure setScriptText(const Value: String);
  procedure SetOnGetAbortPressed(const Value: TBooleanFunction);
  procedure SetOnGetDoExit(const Value: TBooleanFunction);
published
  property Database: TSQLConnection read fDatabase write fDatabase;
  property OnHaveData: TOnHaveDataEvent read FOnHaveData write FOnHaveData;
  property OnNeedData: TOnNeedDataEvent read FOnNeedData write FOnNeedData;
  property OnError: TOnErrorEvent read FOnError write FOnError;
  property ScriptText: String read getScriptText write setScriptText;
public
  uwaString: String;
  property uwaLoopQry: TXpQuery read fXpLoopQuery;
  property FunctionProtoTypes: TStringList read fFunctionProtoTypes;
  property SqlUsed: Boolean read GetSqlUsed;
  property ScannedItems: YYSType read LexicalList;
  property OnGetAbortPressed: TBooleanFunction read fOnGetAbortPressed write SetOnGetAbortPressed;
  property OnGetDoExit: TBooleanFunction read fOnGetDoExit write SetOnGetDoExit;
  property Context: TObject read getContext write setContext;
  function BindsAsText: String;
  procedure Prepare;
  procedure Execute;
  procedure CheckScript (aStringList: TStringList; aOnError: TOnErrorEvent);
  procedure ExecuteScript (aStringList: TStringList; aOnError: TOnErrorEvent);
  procedure BindBoolean (Id: String; var Adress: Boolean);
  procedure BindDateTime (Id: String; var Adress: TDateTime);
  procedure BindInteger (Id: String; var Adress: Integer);
  procedure BindExtended (Id: String; var Adress: Extended);
  procedure BindString (Id: String; var Adress: String);
  procedure BindDateTimeObject (Id: String; aBindable: TCustomBindable);
  procedure BindIntegerObject (Id: String; aBindable: TCustomBindable);
  procedure BindExtendedObject (Id: String; aBindable: TCustomBindable);
  procedure BindStringObject (Id: String; aBindable: TCustomBindable);
  procedure BindGroupObject (Id: String; aBindable: TCustomBindable);
  procedure BindFunctionEx (Id: String; Adr: Pointer; Token: Integer);
  procedure BindFunction (Id: String; Adr: Pointer; Token: Integer; Prototype: String);
  procedure BindBuildIns;
  function DebugTokenStringList: String;
  function FindBind (aId: String): TBind;
  constructor Create (aOwner: TObject);
  destructor Destroy; override;
end;
{$INCLUDE Parser.def}

procedure RaiseError (aTitle: String);

implementation

uses Frame
   , SysUtils
   , Dialogs
   , Math
   ;

procedure RaiseError (aTitle: String);
begin
  raise Exception.Create (aTitle);
end;


function ExpandFrame (arg: TObject): Extended;
var
  vBind: TBind;
  vFrame: TFrame;
begin
  vBind := arg as TBind;
  vFrame := vBind.yy.yyObject as TFrame;
  vFrame.Expand;
  result := 1;
end;

function ShowExtended (arg: Extended): Extended;
begin
  ShowMessage (FloatToStr (arg));
  result := arg;
end;

function AbsExtended (arg: Extended): Extended;
begin
  result := Abs (arg);
end;

function CeilExtended (arg: Extended): Extended;
begin
  result := ceil (arg);
end;

function CosExtended (arg: Extended): Extended;
var
  Sine, Cosine: Extended;
begin
  SinCos (arg, Sine, CoSine);
  result := CoSine;
end;

function FloorExtended (arg: Extended): Extended;
begin
  result := floor (arg);
end;

function LnExtended (arg: Extended): Extended;
begin
  result := LnXP1 (arg - 1);
end;

function LengthExtended (arg: String): Extended;
begin
  result := Length (arg);
end;

function SinExtended (arg: Extended): Extended;
var
  Sine, Cosine: Extended;
begin
  SinCos (arg, Sine, CoSine);
  result := Sine;
end;

function xIntToStr (arg: Extended): String;
begin
  result := FloatToStr (Int(Arg));
end;

function xStrToInt (arg: String): Extended;
begin
  result := StrToInt (arg);
end;

function SubString ( s: String
                   ; i: Extended
                   ; c: Extended
                   ): String;

begin
  result := Copy (s, Trunc (i), Trunc (c));
end;

function TExpress.DebugTokenStringList: String;
var
  Lex: YYSType;
begin
  result := '';
  with TStringList.Create do
  try
    lex := LexicalList;
    while (lex <> nil) do
    begin
      Add ( IntToStr (Lex.Token)
          + '  :  '
          + TokenNames [Lex.Token]
          + '   '
          + Lex.TokenString
          + '  ('
          + IntToStr (Lex.LineNumber)
          + ' : '
          + IntToStr (Lex.ColumnNumber)
          + ')'
          );
      lex := lex.NextToken;
    end;
    result := Text;
  finally
    Free;
  end;
end;

procedure TExpress.ScannerError (Sender: TObject; Data: String);
begin
  ShowMessage ('Scanner: ' + Data);
end;

function TExpress.AsFieldId2FieldId (arg: String): String;
begin
  result := Copy (arg, Pos (':', arg) + 1, Length (arg));
end;

function TExpress.LayoutFieldId2FieldId (arg: String): String;
begin
  result := Copy (arg, 3, Length (arg) - 4);
end;

procedure TExpress.PushInteger (arg: Integer);
begin
  if StackIndex > InternalStackSize then
    raise Exception.Create ('Internal stack overflow')
  else
  begin
    Stack [StackIndex] := arg;
    Inc (StackIndex);
  end;
end;

procedure TExpress.HaveData (aObject: TObject; aString: String);
begin
  if Assigned (FOnHaveData) then
    FOnHaveData (Self, aString)
  else
    raise Exception.Create ('No OnHaveData proc assigned');
end;

function TExpress.PopInteger: Integer;
begin
  if StackIndex <= 0 then
    Raise Exception.Create ('Internal stack underflow')
  else
  begin
    Dec (StackIndex);
    result := Stack [StackIndex];
  end;
end;

procedure TExpress.PassText(Sender: TObject; var MoreData: Boolean;
  var Data: string);
begin
  MoreData := (fTextLineNo < fTextLines.Count);
  if MoreData then
  begin
    Data := fTextLines.Strings [fTextLineNo];
    Inc (fTextLineNo);
  end;
end;

procedure TExpress.PassString (Sender: TObject; var MoreData: Boolean;
  var Data: string);
begin
  MoreData := FirstData;
  Data := OneLiner;
  FirstData := False;
end;

procedure TExpress.EvaluateString (Sender: TObject; arg: String);
var
  Xpress: TExpress;
begin
  Xpress := TExpress.Create (nil);
  Xpress.OneLiner := arg;
  Xpress.FirstData := True;
  try
    Xpress.OnError := OnError;
    Xpress.fOnNeedData := Xpress.PassString;
    XPress.fBindList := fBindList;
    Xpress.Prepare;
    Xpress.Execute;
  finally
    Xpress.Free;
  end;
end;

procedure TExpress.CheckScript (aStringList : TStringList; aOnError: TOnErrorEvent);
var
  Xpress: TExpress;
  xStringList: TStringList;
  xBindList: TBindList;
begin
  Xpress := TExpress.Create (Parser.Owner);
  try
    Xpress.OnError := aOnError;
    xBindList := Xpress.fBindList;
    xStringList := Xpress.fTextLines;
    try
      XPress.fBindList := fBindList;
      Xpress.fTextLines := aStringList;
      Xpress.Prepare;
    finally
      XPress.fBindList := xBindList;
      Xpress.fTextLines := xStringList;
    end;
  finally
    Xpress.Free;
  end;
end;

procedure TExpress.ExecuteScript (aStringList: TStringList; aOnError: TOnErrorEvent);
var
  Xpress: TExpress;
  xStringList: TStringList;
  xBindList: TBindList;
begin
  Xpress := TExpress.Create (Parser.Owner);
  try
    Xpress.OnError := aOnError;
    Xpress.OnGetDoExit := OnGetDoExit;
    Xpress.OnGetAbortPressed := OnGetAbortPressed;
    XPress.Database := Database;
    Xpress.Context := Context;
    xBindList := Xpress.fBindList;
    xStringList := Xpress.fTextLines;
    try
      XPress.fBindList := fBindList;
      Xpress.fTextLines := aStringList;
      Xpress.Prepare;
      Xpress.Execute;
    finally
      XPress.fBindList := xBindList;
      Xpress.fTextLines := xStringList;
    end;
  finally
    Xpress.Free;
  end;
end;

procedure TExpress.ExpressError ( Sender: TObject
                                ; LineNumber: Integer
                                ; ColumnNumber: Integer
                                ; Offset: Integer
                                ; TokenString: String
                                ; Data: String
                                );
begin
  if Assigned (FOnError) then
    FOnError ( Self
             , Parser.yylval.LineNumber
             , Parser.yylval.ColumnNumber
             , Parser.yylval.Offset
             , Parser.yylval.TokenString
             , Data
             )
  else
    raise Exception.CreateFmt ( 'Error encountered "%s" at line %d, column %d, "%s"'
                              , [Data, LineNumber, ColumnNumber, TokenString] );
end;

procedure TExpress.ClearLexicalList;
var
  y: YYSType;
  n: YYSType;
begin
  y := LexicalList;
  while y <> nil do
  begin
    n := y.Next;
    y.Free;
    y := n;
  end;
  LexicalList := nil;
  PrevItem := nil;
end;

function TExpress.TokenToFloat (arg: String): Extended;
var
  SwapSeparator: Char;
begin
  SwapSeparator := DecimalSeparator;
  if SwapSeparator <> '.' then
    DecimalSeparator := '.';
  try
    result := StrToFloat (arg);
  finally
    if SwapSeparator <> DecimalSeparator then
      DecimalSeparator := SwapSeparator;
  end;
end;

procedure TExpress.ScannerNeedsData ( Sender:TObject
                                    ; var MoreData: Boolean
                                    ; var Data: String
                                    );
begin
  if Assigned (FOnNeedData) then
    FOnNeedData (Self, MoreData, Data)
  else
    MoreData := False;
end;

procedure TExpress.setContext(const Value: TObject);
begin
  Parser.Data := Value;
end;

procedure TExpress.SetOnGetAbortPressed(const Value: TBooleanFunction);
begin
  fOnGetAbortPressed := Value;
  if Assigned (Parser) then
    Parser.OnGetAbortPressed := Value;
end;

procedure TExpress.SetOnGetDoExit(const Value: TBooleanFunction);
begin
  fOnGetDoExit := Value;
  if Assigned (Parser) then
    Parser.OnGetDoExit := Value;
end;

function TExpress .BindsAsText : String ;
begin
  result := fBindList.Text;
end;

procedure TExpress.setScriptText(const Value: String);
begin
  fTextLines.Text := Value;
  fOnNeedData := PassText;
  fTextLineNo := 0;
end;

procedure TExpress.StoreObject(Sender: TObject; aObject: TObject);
begin
  ObjList.AddObject('', aObject);
end;

procedure TExpress.CreateQuery ( Sender:TObject
                               ; var Query: TObject
                               );
var
  Qry: TXpQuery;
begin
  Qry := TXpQuery.Create (nil);
  Qry.DataBase := Database;
{  Qry.UniDirectional := True; }
  Qry.InsertValuesString := 'values';
  Qry.InsertValuesSeparator := '(';
  Query := Qry as TObject;
  XpQueryList.AddObject ('', Qry);
end;

procedure TExpress.HaveSqlToken ( Sender:TObject
                                ; Query: TObject
                                ; Str: String
                                );
var
  Qry: TXpQuery;
begin
  Qry := Query as TXpQuery;
  Qry.SqlStrings.Add (Str);
end;

procedure TExpress.HaveSqlBind ( Sender:TObject
                               ; Query: TObject
                               ; Bind: TBind
                               );
var
  Qry: TXpQuery;
begin
  Qry := Query as TXpQuery;
  Qry.BindList.AddObject ('', Bind);
end;

procedure TExpress.HaveSqlParam ( Sender:TObject
                                ; Query: TObject
                                ; Param: TBind
                                );
var
  Qry: TXpQuery;
  f: Integer;
begin
  Qry := Query as TXpQuery;
{$ifdef FPC}
  if not (Qry.ParamList.Find(Param.Id, f)) then
    Qry.ParamList.AddObject (Param.Id, Param);
{$else}
  Qry.ParamList.AddObject ('', Param);
{$endif}
end;

procedure TExpress.HaveSqlInsertParam ( Sender:TObject
                                ; Query: TObject
                                ; Param: TBind
                                );
var
  Qry: TXpQuery;
begin
  HaveSqlParam (Sender, Query, Param);
  Qry := Query as TXpQuery;
  Qry.InsertValuesString := Qry.InsertValuesString
                          + Qry.InsertValuesSeparator
                          + '?';
  Qry.InsertValuesSeparator := ',';
end;

procedure TExpress.NeedSqlExec ( Sender:TObject
                               ; Query: TObject
                               );
var
  Qry: TXpQuery;
  xTransaction: TSQLTransaction;
  x: Integer;
  Bindable: TCustomBindable;
begin
  Qry := Query as TXpQuery;
  for x := 0 to Qry.ParamList.Count - 1 do
  begin
    if (x < Qry.Params.Count) then {just to make sure}
    begin
      if (Qry.ParamList.Binds [x].BindsAnObject)
      then
      begin
        Bindable := TCustomBindable (Qry.ParamList.Binds [x].yy.yyPointer);
        case Qry.ParamList.Binds [x].Token of
          DFLD: Qry.Params.Items [x].Value := Bindable.GetDateTimeData;
          SFLD: Qry.Params.Items [x].Value := Bindable.GetStringData;
          IFLD: Qry.Params.Items [x].Value := Bindable.GetIntegerData;
          XFLD: Qry.Params.Items [x].Value := Bindable.GetExtendedData;
        end;
      end
      else
      begin
        case Qry.ParamList.Binds [x].Token of
          DFLD: Qry.Params.Items [x].Value := PTDateTime (Qry.ParamList.Binds [x].yy.yyPointer)^;
          SFLD: Qry.Params.Items [x].Value := PString (Qry.ParamList.Binds [x].yy.yyPointer)^;
          IFLD: Qry.Params.Items [x].Value := PInteger (Qry.ParamList.Binds [x].yy.yyPointer)^;
          XFLD: Qry.Params.Items [x].Value := PExtended (Qry.ParamList.Binds [x].yy.yyPointer)^;
        end;
      end;
    end;
  end;
  xTransaction := TSQLTransaction.Create(nil);
  xTransaction.DataBase := Qry.Database;
  Qry.Transaction := xTransaction;
  if Qry.DataBase is TSqlConnector then with Qry.DataBase as TSqlConnector do
  begin
    if ConnectorType = 'Oracle' then
    begin
      ExecuteDirect('commit');
      ExecuteDirect('set transaction read write');
//    ExecuteDirect(Qry.SQL.Text);
      Qry.ExecSQL;
      ExecuteDirect('commit');
    end
    else
    begin
      try
        xTransaction.StartTransaction;
        Qry.ExecSQL;
      finally
        xTransaction.Commit;
        xTransaction.Free;
      end;
    end;
  end;
end;

procedure TExpress.NeedSqlOpen ( Sender:TObject
                               ; Query: TObject
                               );
var
  Qry: TXpQuery;
  x: Integer;
  Bindable: TCustomBindable;
begin
  Qry := Query as TXpQuery;
{$ifdef FPC}
  if (Qry.Params.Count <> Qry.ParamList.Count) then
  begin
    raise Exception.Create ('Number of actual params does not match number of required params');
  end;
{$endif}
  if not Qry.DataBase.Connected then
    Qry.DataBase.Connected := True;
  Qry.SQL.Text := Qry.SqlStrings.Text;
  for x := 0 to Qry.ParamList.Count - 1 do
  begin
    if Qry.ParamList.Binds [x].BindsAnObject then
    begin
      Bindable := TCustomBindable (Qry.BindList.Binds [x].yy.yyPointer);
      case Qry.ParamList.Binds [x].Token of
        DFLD: Qry.Params.Items [x].Value := Bindable.GetDateTimeData;
        SFLD: Qry.Params.Items [x].Value := Bindable.GetStringData;
        IFLD: Qry.Params.Items [x].Value := Bindable.GetIntegerData;
        XFLD: Qry.Params.Items [x].Value := Bindable.GetExtendedData;
      end;
    end
    else
    begin
      case Qry.ParamList.Binds [x].Token of
        DFLD: Qry.Params.Items [x].Value := PTDateTime (Qry.ParamList.Binds [x].yy.yyPointer)^;
        SFLD: Qry.Params.Items [x].Value := PString (Qry.ParamList.Binds [x].yy.yyPointer)^;
        IFLD: Qry.Params.Items [x].Value := PInteger (Qry.ParamList.Binds [x].yy.yyPointer)^;
        XFLD: Qry.Params.Items [x].Value := PExtended (Qry.ParamList.Binds [x].yy.yyPointer)^;
      end;
    end;
  end;
  try
    Qry.Open;
  except
    on e: Exception do
    begin
      raise Exception.Create(e.Message + CRLF + Qry.SqlStrings.Text);
    end;
  end;
  if (Qry.FieldCount <> Qry.BindList.Count) then
    raise Exception.Create ('Number of retrieved columns does not match number of program variables');
  Qry.QueryVerb := xqvSelect;
end;

procedure TExpress.NeedSqlClose ( Sender: TObject
                                ; Query: TObject
                                );
var
  Qry: TXpQuery;
begin
  Qry := Query as TXpQuery;
  Qry.Close;
end;

procedure TExpress.NeedNextSqlRow ( Sender: TObject
                                  ; Query: TObject
                                  ; var MoreData: Boolean
                                  );
var
  Qry: TXpQuery;
  x: Integer;
  Bindable: TCustomBindable;
begin
  if Assigned (OnGetAbortPressed)
  and OnGetAbortPressed then
  begin
    MoreData := False;
    Exit;
  end;
  Qry := Query as TXpQuery;
  fXpLoopQuery := Qry;
  Qry.Next;
  MoreData := not Qry.Eof;
  if (MoreData) then
  begin
    for x := 0 to Qry.FieldCount - 1 do
    begin
      if Qry.BindList.Binds [x].BindsAnObject = True then
      begin
        Bindable := TCustomBindable (Qry.BindList.Binds [x].yy.yyPointer);
        case Qry.BindList.Binds [x].Token of
          DFLD: Bindable.PutDateTimeData (Qry.Fields [x].AsDateTime);
          SFLD: Bindable.PutStringData (Qry.Fields [x].AsString);
          IFLD: Bindable.PutIntegerData (Qry.Fields [x].AsInteger);
          XFLD: Bindable.PutExtendedData (Qry.Fields [x].AsFloat);
        end;
      end
      else
      begin
        case Qry.BindList.Binds [x].Token of
          DFLD: PTDateTime (Qry.BindList.Binds [x].yy.yyPointer)^ := Qry.Fields [x].AsDateTime;
          SFLD: PString (Qry.BindList.Binds [x].yy.yyPointer)^ := Qry.Fields [x].AsString;
          IFLD: PInteger (Qry.BindList.Binds [x].yy.yyPointer)^ := Qry.Fields [x].AsInteger;
          XFLD: PExtended (Qry.BindList.Binds [x].yy.yyPointer)^ := Qry.Fields [x].AsFloat;
        end;
      end;
    end;
  end;
end;

procedure TExpress.NeedFirstSqlRow ( Sender: TObject
                                   ; Query: TObject
                                   ; var MoreData: Boolean
                                   );
var
  Qry: TXpQuery;
  x: Integer;
  Bindable: TCustomBindable;
begin
  Qry := Query as TXpQuery;
  Qry.First;
  fXpLoopQuery := Qry;
  MoreData := not Qry.Eof;
  if (MoreData) then
  begin
    for x := 0 to Qry.FieldCount - 1 do
    begin
      if Qry.BindList.Binds [x].BindsAnObject = True then
      begin
        Bindable := TCustomBindable (Qry.BindList.Binds [x].yy.yyPointer);
        case Qry.BindList.Binds [x].Token of
          DFLD: Bindable.PutDateTimeData (Qry.Fields [x].AsDateTime);
          SFLD: Bindable.PutStringData (Qry.Fields [x].AsString);
          IFLD: Bindable.PutIntegerData (Qry.Fields [x].AsInteger);
          XFLD: Bindable.PutExtendedData (Qry.Fields [x].AsFloat);
        end;
      end
      else
      begin
        case Qry.BindList.Binds [x].Token of
          DFLD: PTDateTime (Qry.BindList.Binds [x].yy.yyPointer)^ := Qry.Fields [x].AsDateTime;
          SFLD: PString (Qry.BindList.Binds [x].yy.yyPointer)^ := Qry.Fields [x].AsString;
          IFLD: PInteger (Qry.BindList.Binds [x].yy.yyPointer)^ := Qry.Fields [x].AsInteger;
          XFLD: PExtended (Qry.BindList.Binds [x].yy.yyPointer)^ := Qry.Fields [x].AsFloat;
        end;
      end;
    end;
  end;
end;

procedure TExpress .FinishInsertQuery (Sender : TObject ; Query : TObject );
var
  Qry: TXpQuery;
begin
  Qry := Query as TXpQuery;
  Qry.SqlStrings.Add (Qry.InsertValuesString + ')');
  Qry.QueryVerb := xqvInsert;
end;

function TExpress.getContext: TObject;
begin
  result := Parser.Data;
end;

function TExpress.IsKeyWord (id : string; var token : integer; var Address: Pointer) : boolean;
type LexItemRec = record
  Wrd: String;
  Tkn: Integer;
  Adr: Pointer;
end;

const
  NoOfLexItems = 683 - 626;
  LexItems : array [1..NoOfLexItems] of LexItemRec =
  ( (Wrd: 'AND'; Tkn: _AND; Adr: nil)
  , (Wrd: 'ARRAY'; Tkn: _ARRAY; adr: nil)
  , (Wrd: 'AS'; Tkn: _AS; adr: nil)
  , (Wrd: 'BEGIN'; Tkn: _BEGIN; adr: nil)
  , (Wrd: 'CASE'; Tkn: _CASE; adr: nil)
  , (Wrd: 'CONST'; Tkn: _CONST; adr: nil)
  , (Wrd: 'DAYS'; Tkn: _DAYS; adr: nil)
  , (Wrd: 'DECLARE'; Tkn: _DECLARE; adr: nil)
  , (Wrd: 'DIV'; Tkn: _DIV; adr: nil)
  , (Wrd: 'DO'; Tkn: _DO; adr: nil)
  , (Wrd: 'DOWNTO'; Tkn: _DOWNTO; adr: nil)
  , (Wrd: 'EACH'; Tkn: _EACH; adr: nil)
  , (Wrd: 'ELSE'; Tkn: _ELSE; adr: nil)
  , (Wrd: 'END'; Tkn: _END; adr: nil)
  , (Wrd: 'EVALUATE'; Tkn: _EVALUATE; adr: nil)
  , (Wrd: 'EXTERNAL'; Tkn: _EXTERNAL; adr: nil)
  , (Wrd: 'EXTERN'; Tkn: _EXTERNAL; adr: nil)
  , (Wrd: 'FALSE'; Tkn: _FALSE; adr: nil)
  , (Wrd: 'FILE'; Tkn: _FILE; adr: nil)
  , (Wrd: 'FLOAT'; Tkn: _FLOAT; adr: nil)
  , (Wrd: 'FOR'; Tkn: _FOR; adr: nil)
  , (Wrd: 'FORWARD'; Tkn: _FORWARD; adr: nil)
  , (Wrd: 'FRAME'; Tkn: _FRAME; adr: nil)
  , (Wrd: 'FUNCTION'; Tkn: _FUNCTION; adr: nil)
  , (Wrd: 'GOTO'; Tkn: _GOTO; adr: nil)
  , (Wrd: 'IF'; Tkn: _IF; adr: nil)
  , (Wrd: 'IN'; Tkn: _IN; adr: nil)
  , (Wrd: 'INTEGER'; Tkn: _INTEGER; adr: nil)
  , (Wrd: 'LABEL'; Tkn: _LABEL; adr: nil)
  , (Wrd: 'MOD'; Tkn: _MOD; adr: nil)
  , (Wrd: 'MONTHS'; Tkn: _MONTHS; adr: nil)
  , (Wrd: 'NEW'; Tkn: _NEW; adr: nil)
  , (Wrd: 'NIL'; Tkn: _NIL; adr: nil)
  , (Wrd: 'NOT'; Tkn: _NOT; adr: nil)
  , (Wrd: 'OF'; Tkn: _OF; adr: nil)
  , (Wrd: 'OR'; Tkn: _OR; adr: nil)
  , (Wrd: 'OTHERWISE'; Tkn: _OTHERWISE; adr: nil)
  , (Wrd: 'PACKED'; Tkn: _PACKED; adr: nil)
  , (Wrd: 'POINTER'; Tkn: _POINTER; adr: nil)
  , (Wrd: 'PROCEDURE'; Tkn: _PROCEDURE; adr: nil)
  , (Wrd: 'PROGRAM'; Tkn: _PROGRAM; adr: nil)
  , (Wrd: 'RECORD'; Tkn: _RECORD; adr: nil)
  , (Wrd: 'REPEAT'; Tkn: _REPEAT; adr: nil)
  , (Wrd: 'SET'; Tkn: _SET; adr: nil)
  , (Wrd: 'STRING'; Tkn: _STRING; Adr: nil)
  , (Wrd: 'THEN'; Tkn: _THEN; adr: nil)
  , (Wrd: 'TIMESTAMP'; Tkn: _DATETIME; adr: nil)
  , (Wrd: 'TO'; Tkn: _TO; adr: nil)
  , (Wrd: 'TRUE'; Tkn: _TRUE; adr: nil)
  , (Wrd: 'TYPE'; Tkn: _TYPE; adr: nil)
  , (Wrd: 'UNTIL'; Tkn: _UNTIL; adr: nil)
  , (Wrd: 'VAR'; Tkn: _VAR; adr: nil)
  , (Wrd: 'VOID'; Tkn: _VOID; Adr: nil)
  , (Wrd: 'WEEKS'; Tkn: _WEEKS; adr: nil)
  , (Wrd: 'WHILE'; Tkn: _WHILE; adr: nil)
  , (Wrd: 'WITH'; Tkn: _WITH; adr: nil)
  , (Wrd: 'YEARS'; Tkn: _YEARS; adr: nil)
  );

var m, n, k : integer;
begin
  id := uppercase (id);
  m := 1;
  n := NoOfLexItems;
  while m <= n do
  begin
    k := m + (n-m) div 2;
    if id = LexItems [k].Wrd then
    begin
      result := true;
      token := LexItems [k].Tkn;
      Address := LexItems [k].Adr;
      exit;
    end
    else
      if id > LexItems [k].Wrd then
        m := k+1
      else
        n := k-1;
  end;
  result := false;
end;

procedure TExpress.BindBuildIns;
type LexItemRec = record
  Wrd: String;
  Tkn: Integer;
  Adr: Pointer;
end;

function xpStrToDate(aStr: String): TDateTime;
begin
  result := StrToDate(aStr);
end;

function xpStrToDateTime(aStr: String): TDateTime;
begin
  result := StrToDateTime(aStr);
end;

function xpDateToStr(aDate: TDateTime): String;
begin
  result := DateToStr(aDate);
end;

function xpDateTimeToStr(aDateTime: TDateTime): String;
begin
  result := DateTimeToStr(aDateTime);
end;

function xpTimeToStr(aTime: TDateTime): String;
begin
  result := TimeToStr(aTime);
end;

function xpStrToTime(aStr: String): TDateTime;
begin
  result := StrToTime(aStr);
end;

function xplowercase(aStr: String): String;
begin
  result := lowercase(aStr);
end;

function xpuppercase(aStr: String): String;
begin
  result := uppercase(aStr);
end;

const
  NoOfLexItems = 546 - 515;
  LexItems : array [1..NoOfLexItems] of LexItemRec =
  ( (Wrd: 'ABS'; Tkn: XfX; Adr: @AbsExtended)
  , (Wrd: 'ARCCOS'; Tkn: XfX; Adr: @ArcCos)
  , (Wrd: 'ARCCOSH'; Tkn: XfX; Adr: @ArcCosh)
  , (Wrd: 'ARCSIN'; Tkn: XfX; Adr: @ArcSin)
  , (Wrd: 'ARCSINH'; Tkn: XfX; Adr: @ArcSinh)
  , (Wrd: 'ARCTAN2'; Tkn: XFXX; Adr: @ArcTan2)
  , (Wrd: 'ARCTANH'; Tkn: XfX; Adr: @ArcTanh)
  , (Wrd: 'CEIL'; Tkn: XfX; Adr: @CeilExtended)
  , (Wrd: 'COS'; Tkn: XfX; Adr: @CosExtended)
  , (Wrd: 'COSH'; Tkn: XfX; Adr: @Cosh)
  , (Wrd: 'COTAN'; Tkn: XfX; Adr: @Cotan)
  , (Wrd: 'DATE'; Tkn: DFS; adr: @xpStrToDate)
  , (Wrd: 'DATETIME'; Tkn: DFS; adr: @xpStrToDateTime)
  , (Wrd: 'DATETIMETOSTR'; Tkn: SFD; adr: @xpDateTimeToStr)
  , (Wrd: 'DATETOSTR'; Tkn: SFD; adr: @xpDateToStr)
  , (Wrd: 'EXPANDFRAME'; Tkn: XfFRAME; Adr: @ExpandFrame)
  , (Wrd: 'FLOOR'; Tkn: XfX; Adr: @FloorExtended)
  , (Wrd: 'INTTOSTR'; Tkn: SFX; Adr: @xIntToStr)
  , (Wrd: 'LENGTH'; Tkn: XFS; Adr: @LengthExtended)
  , (Wrd: 'LN'; Tkn: XfX; Adr: @LnExtended)
  , (Wrd: 'LOWERCASE'; Tkn: SFS; adr: @xplowercase)
  , (Wrd: 'NOW'; Tkn: DF; adr: @now)
  , (Wrd: 'SHOW'; Tkn: XfX; Adr: @ShowExtended)
  , (Wrd: 'SIN'; Tkn: XfX; Adr: @SinExtended)
  , (Wrd: 'SINH'; Tkn: XfX; Adr: @Sinh)
  , (Wrd: 'STRTOINT'; Tkn: XFS; Adr: @xStrToInt)
  , (Wrd: 'SUBSTRING'; Tkn: SFSXX; Adr: @SubString)
  , (Wrd: 'TIME'; Tkn: DFS; adr: @xpStrToTime)
  , (Wrd: 'TIMETOSTR'; Tkn: SFD; adr: @xpTimeToStr)
  , (Wrd: 'TODAY'; Tkn: DF; adr: @Date)
  , (Wrd: 'UPPERCASE'; Tkn: SFS; adr: @xpuppercase)
  );

var m: integer;
begin
  for m := 1 to NoOfLexItems do
  begin
    BindFunctionEx ( LexItems [m].Wrd
                   , LexItems [m].Adr
                   , LexItems [m].Tkn
                   );
  end;
end;

procedure TExpress.OnToken (Sender: TObject);
  procedure __begin (aLex: YYSType);
  begin
    Parser.PushObject (prntBlock);
    Parser.PushObject (currBlock);
    currBlock := TBlock.Create;
    currBlock.Parent := prntBlock;
    Blocks.AddObject ('', currBlock);
    aLex.Block := currBlock;
    prntBlock := currBlock;
    PushInteger (ScannerState);
    ScannerState := InitialState;
  end;
  procedure __end (aLex: YYSType);
  begin
    ScannerState := PopInteger;
    currBlock := TBlock (Parser.PopObject);
    prntBlock := TBlock (Parser.PopObject);
  end;
var
  xScanner: TScanner;
  Lexical: YYSType;
  Bind: TBind;
  BindIndex: Integer;
begin
  Scanner := Sender as TScanner;
  if (   (Scanner.Token = _SPACE)
      or (Scanner.Token = _NEWLINE)
      or (Scanner.Token = _COMMENT)
      )
  and (ScannerState <> LayoutState) then
    exit;
  Lexical := YYSType.Create;
  Lexical.Prev := PrevItem;
  PrevItem := Lexical;
  Lexical.Block := currBlock;
  if LexicalList = nil then
    LexicalList := Lexical
  else
  begin
    LexItem.Next := Lexical;
    LexItem.NextToken := Lexical;
  end;
  LexItem := Lexical;
  Lexical.Next := nil;
  Lexical.NextToken := nil;
  Lexical.LineNumber := Scanner.LineNumber;
  Lexical.ColumnNumber := Scanner.ColumnNumber;
  Lexical.Token := Scanner.Token;
  Lexical.TokenString := Scanner.TokenAsString;

  case Lexical.Token of
    _EXEC_SQLEXEC, _EXEC_SQLINSERT, _EXEC_SQLSELECT:
    begin
      PushInteger (ScannerState);
      ScannerState := InSqlState;
    end;
    _SEMICOLON:
    begin
      if ScannerState = InSqlState then
      begin
        ScannerState := PopInteger;
      end;
    end;
    _BEGIN: __begin(Lexical);
    _LOOP: ScannerState := PopInteger;
    _BEGIN_LAYOUT:
    begin
      Parser.PushObject (prntBlock);
      Parser.PushObject (currBlock);
      currBlock := TBlock.Create;
      currBlock.Parent := prntBlock;
      Blocks.AddObject ('', currBlock);
      Lexical.Block := currBlock;
      prntBlock := currBlock;
      PushInteger (ScannerState);
      ScannerState := LayoutState;
    end;
    _END:  __end(Lexical);
  end;

  case ScannerState of
    InSqlState:
    begin
      case Lexical.Token of
        _AS_FIELDID, _SQLPARAM_FIELD:
        begin
          lexical.TokenString := AsFieldId2FieldId (lexical.TokenString);
          Bind := Lexical.Block.BindedByName(Uppercase (lexical.TokenString));
          if not Assigned (Bind) then
            if fBindList.Find (Uppercase (Lexical.TokenString), BindIndex) then
              Bind := fBindList.Binds [BindIndex];
          if Assigned (Bind) then
            Lexical.yy.yyObject := Bind;
        end;
        _END, _EXEC_SQLEXEC, _EXEC_SQLINSERT, _EXEC_SQLSELECT:
        begin
        end
        else
        begin
          Lexical.Token := _SQLTOKEN;
        end;
      end;
    end;
    InitialState:
    begin
      case Lexical.Token of
        _NOID:
        begin
          if not IsKeyWord ( Scanner.TokenAsString
                           , Lexical.Token
                           , Lexical.yy.yyPointer
                           ) then
          begin
            if fBindList.Find (Uppercase (Scanner.TokenAsString), BindIndex) then
            begin { maybe it's a user binded function}
              Bind := fBindList.Binds [BindIndex];
              Lexical.Token := Bind.Token;
              Lexical.yy.yyObject := Bind;
            end
            else {we have no idea}
            begin
              Lexical.Token := _NOID;
              Lexical.yyString := Scanner.TokenAsString;
              Lexical.yyStringRead := Lexical.yyString;
            end;
          end
          else
          begin
            if Lexical.Token = _BEGIN then __begin(Lexical);
            if Lexical.Token = _END then __end(Lexical);
          end;
        end;
        _FIELDID:
        begin
          if Lexical.isLocalDeclaration then
          begin
            Bind := TBind.Create;
            Bind.Id := Uppercase (Lexical.TokenString);
            Bind.Token := Lexical.DeclarationToken;
            case Bind.Token of
              SFLD: Bind.yy.yyPointer := @Bind.Str;
              PFLD:
                begin
                  Bind.yy.yyObject := nil;
                  Bind.BindsAnObject := True;
                end;
              else Bind.yy.yyPointer := @Bind.Storage;
            end;
            Lexical.yy.yyPointer := Bind;
            Lexical.Block.Binds.AddObject (Bind.Id, Bind);
          end
          else
          begin
            Bind := Lexical.Block.BindedByName(Uppercase (Scanner.TokenAsString));
            if not Assigned (Bind) then
              if fBindList.Find (Uppercase (Scanner.TokenAsString), BindIndex) then
                Bind := fBindList.Binds [BindIndex];
            if Assigned (Bind) then
            begin
              Lexical.Token := Bind.Token;
              Lexical.yy.yyObject := Bind;
            end;
          end;
        end;
        _CHARACTER_STRING:
        begin
          Lexical.yyString := Copy (Scanner.TokenAsString, 2, Length (Scanner.TokenAsString) - 2);
          Lexical.yyStringRead := Lexical.yyString;
        end;
        _REALNUMBER:
        begin
          Lexical.yy.yyExtended := TokenToFloat (Scanner.TokenAsString);
        end;
      end;
    end;
    LayoutState:
    begin
      case Lexical.Token of
        _LAYOUT_FIELD:
        begin
          lexical.TokenString := LayoutFieldId2FieldId (lexical.TokenString);
          if fBindList.Find (Uppercase (lexical.TokenString), BindIndex) then
          begin
            Lexical.yy.yyObject := fBindList.Binds [BindIndex];
          end;
        end;
        _BEGIN_LAYOUT, _NEWLINE:
        begin
        end
        else
        begin
          Lexical.Token := _LAYOUT_TOKEN;
        end;
      end;
    end;
  end;
  Lexical.yyRead := Lexical.yy;
end;

procedure TExpress.Prepare;
begin
  try
    XpQueryList.Clear;
    with Blocks do
      while Count  > 0 do
      begin
        Objects[0].Free;
        Delete(0);
      end;
    prntBlock := nil;
    currBlock := TBlock.Create;
    currBlock.Parent := prntBlock;
    Blocks.AddObject ('', currBlock);
    prntBlock := currBlock;
    rootBlock := currBlock;
    while ObjList.Count  > 0 do
    begin
      ObjList.Objects[0].Free;
      ObjList.Delete(0);
    end;
    ClearLexicalList;
    fTextLineNo := 0;
    Scanner.OnToken := OnToken;
    StackIndex := 0;
    ScannerState := InitialState;
    Scanner.Execute;
    Parser.LexItems := LexicalList;
    Parser.Prepare;
  except
    raise;
  end;
end;

procedure TExpress.BindBoolean (Id: String; var Adress: Boolean);
var
  Bind: TBind;
begin
  Bind := TBind.Create;
  Bind.BindsAnObject := False;
  Bind.Id := Uppercase (Id);
  Bind.Token := BFLD;
  Bind.yy.yyPointer := @Adress;
  fBindList.AddObject (Bind.Id, Bind);
end;

procedure TExpress.BindDateTime (Id: String; var Adress: TDateTime);
var
  Bind: TBind;
begin
  Bind := TBind.Create;
  Bind.BindsAnObject := False;
  Bind.Id := Uppercase (Id);
  Bind.Token := DFLD;
  Bind.yy.yyPointer := @Adress;
  fBindList.AddObject (Bind.Id, Bind);
end;

procedure TExpress.BindInteger (Id: String; var Adress: Integer);
var
  Bind: TBind;
begin
  Bind := TBind.Create;
  Bind.BindsAnObject := False;
  Bind.Id := Uppercase (Id);
  Bind.Token := IFLD;
  Bind.yy.yyPointer := @Adress;
  fBindList.AddObject (Bind.Id, Bind);
end;

procedure TExpress.BindExtended (Id: String; var Adress: Extended);
var
  Bind: TBind;
begin
  Bind := TBind.Create;
  Bind.BindsAnObject := False;
  Bind.Id := Uppercase (Id);
  Bind.Token := XFLD;
  Bind.yy.yyPointer := @Adress;
  fBindList.AddObject (Bind.Id, Bind);
end;

procedure TExpress.BindString (Id: String; var Adress: String);
var
  Bind: TBind;
begin
  Bind := TBind.Create;
  Bind.BindsAnObject := False;
  Bind.Id := Uppercase (Id);
  Bind.Token := SFLD;
  Bind.yy.yyPointer := @Adress;
  fBindList.AddObject (Bind.Id, Bind);
end;

procedure TExpress.BindDateTimeObject (Id: String; aBindable: TCustomBindable);
var
  Bind: TBind;
begin
  Bind := TBind.Create;
  Bind.BindsAnObject := True;
  Bind.Id := Uppercase (Id);
  Bind.Token := DFLD;
  Bind.yy.yyPointer := aBindable;
  fBindList.AddObject (Bind.Id, Bind);
end;

procedure TExpress.BindIntegerObject (Id: String; aBindable: TCustomBindable);
var
  Bind: TBind;
begin
  Bind := TBind.Create;
  Bind.BindsAnObject := True;
  Bind.Id := Uppercase (Id);
  Bind.Token := IFLD;
  Bind.yy.yyPointer := aBindable;
  fBindList.AddObject (Bind.Id, Bind);
end;

procedure TExpress.BindExtendedObject (Id: String; aBindable: TCustomBindable);
var
  Bind: TBind;
begin
  Bind := TBind.Create;
  Bind.BindsAnObject := True;
  Bind.Id := Uppercase (Id);
  Bind.Token := XFLD;
  Bind.yy.yyPointer := aBindable;
  fBindList.AddObject (Bind.Id, Bind);
end;

procedure TExpress.BindStringObject (Id: String; aBindable: TCustomBindable);
var
  Bind: TBind;
begin
  Bind := TBind.Create;
  Bind.BindsAnObject := True;
  Bind.Id := Uppercase (Id);
  Bind.Token := SFLD;
  Bind.yy.yyPointer := aBindable;
  fBindList.AddObject (Bind.Id, Bind);
end;

procedure TExpress.BindGroupObject (Id: String; aBindable: TCustomBindable);
var
  Bind: TBind;
begin
  Bind := TBind.Create;
  Bind.BindsAnObject := True;
  Bind.Id := Uppercase (Id);
  Bind.Token := GFLD;
  Bind.yy.yyPointer := aBindable;
  fBindList.AddObject (Bind.Id, Bind);
end;

procedure TExpress.BindFunction (Id: String; Adr: Pointer; Token: Integer; Prototype: String);
var
  Bind: TBind;
begin
  Bind := TBind.Create;
  Bind.BindsAnObject := False;
  Bind.Id := Uppercase (Id);
  Bind.Token := Token;
  Bind.yy.yyPointer := Adr;
  fBindList.AddObject (Bind.Id, Bind);
  fFunctionPrototypes.Add (Id + ' ' + Prototype);
end;

procedure TExpress.BindFunctionEx(Id: String; Adr: Pointer; Token: Integer);
var
  Bind: TBind;
begin
  Bind := TBind.Create;
  Bind.BindsAnObject := False;
  Bind.Id := Uppercase (Id);
  Bind.Token := Token;
  Bind.yy.yyPointer := Adr;
  fBindList.AddObject (Bind.Id, Bind);
end;

procedure TExpress.Execute;
var
  x: Integer;
begin
  for x := 0 to XpQueryList.Count - 1 do
    XpQueryList.XpQueries [x].xpPrep;
  for x := 0 to ObjList.Count - 1 do
    ObjList.Objects [x].Init;
  LexItem := LexicalList;
  Parser.DoIt := True;
  try
    currBlock := rootBlock;
    currBlock.InitBinds;
    Parser.Execute;
  except
    for x := 0 to XpQueryList.Count - 1 do
      try
        XpQueryList.XpQueries [x].Close;
      except
      end;
    raise;
  end;
end;

procedure TExpress.PutData  (Sender: TObject; Adress: YYSType; Data: YYSType);
var
  Bind: TBind;
  Bindable: TCustomBindable;
begin
  Bind := TBind (Adress.yy.yyObject);
  if not Bind.BindsAnObject then
  begin
    case Bind.Token of
      BFLD: PBoolean (Bind.yy.yyPointer)^ := Data.yy.yyBoolean;
      DFLD: PTDateTime (Bind.yy.yyPointer)^ := Data.yy.yyDateTime;
      SFLD: PString (Bind.yy.yyPointer)^ := Data.yyString;
      IFLD: PInteger (Bind.yy.yyPointer)^ := Trunc (Data.yy.yyExtended);
      XFLD: PExtended (Bind.yy.yyPointer)^ := Data.yy.yyExtended;
    end;
  end
  else
  begin
    Bindable := TCustomBindable (Bind.yy.yyPointer);
    if not Assigned (Bindable) then
      raise Exception.Create('Error assigning data, destination is nil');
    if Assigned (Data) then
    begin
      case Bind.Token of
        DFLD: Bindable.PutDateTimeData (Data.yy.yyDateTime);
        SFLD: Bindable.PutStringData (Data.yyString);
        IFLD: Bindable.PutIntegerData (Data.yy.yyExtended);
        XFLD: Bindable.PutExtendedData (Data.yy.yyExtended);
        GFLD: Bindable.PutGroupData (TCustomBindable (Data.yy.yyPointer));
        PFLD: Bindable.PutGroupData (TCustomBindable (Data.yy.yyPointer));
      end;
    end
    else
      Bindable.Reset;
  end;
end;

procedure TExpress.GetData  (Sender: TObject; Adress: YYSType; Data: YYSType);
var
  Bind: TBind;
  Bindable: TCustomBindable;
begin
  Bind := TBind (Adress.yy.yyObject);
  if not Bind.BindsAnObject then
  begin
    case Bind.Token of
      BFLD: Data.yy.yyBoolean := PBoolean (Bind.yy.yyPointer)^;
      DFLD: Data.yy.yyDateTime := PTDateTime (Bind.yy.yyPointer)^;
      SFLD: Data.yyString := PString (Bind.yy.yyPointer)^;
      IFLD: Data.yy.yyExtended := PInteger (Bind.yy.yyPointer)^;
      XFLD: Data.yy.yyExtended := PExtended (Bind.yy.yyPointer)^;
    end;
  end
  else
  begin
    Bindable := TCustomBindable (Bind.yy.yyPointer);
    case Bind.Token of
      DFLD: Data.yy.yyDateTime := Bindable.GetDateTimeData;
      SFLD: Data.yyString := Bindable.GetStringData;
      IFLD: Data.yy.yyExtended := Bindable.GetIntegerData;
      XFLD: Data.yy.yyExtended := Bindable.GetExtendedData;
      GFLD: Data.yy.yyObject := Bindable;
    end;
  end;
end;

procedure TExpress.GetObject  (Sender: TObject; Adress: YYSType; Data: YYSType);
var
  Bind: TBind;
  Bindable: TCustomBindable;
begin
  Bind := TBind (Adress.yy.yyObject);
  Bindable := TCustomBindable (Bind.yy.yyPointer);
  Data.yy.yyObject := Bindable;
end;

function TExpress.getScriptText: String;
begin
  result := fTextLines.Text;
end;

constructor TExpress.Create (aOwner: TObject);
begin
  fTextLines := TStringList.Create;
  fOnNeedData := PassText;
  fFunctionProtoTypes := TStringList.Create;
  fFunctionProtoTypes.Sorted := True;
  fFunctionProtoTypes.Duplicates := dupError;
  fFunctionProtoTypes.CaseSensitive := False;
  XpQueryList := TXpQueryList.Create;
  Blocks := TStringList.Create;
  ObjList := TParserClassList.Create;
  Scanner := TScanner.Create;
  Scanner.OnNeedData := ScannerNeedsData;
  Scanner.OnError := ScannerError;
  Parser := TParser.Create(aOwner);
  Parser.OnGetAbortPressed := OnGetAbortPressed;
  Parser.OnPutData := PutData;
  Parser.OnGetData := GetData;
  Parser.OnGetObject := GetObject;
  Parser.OnHaveData := HaveData;
  Parser.OnError := ExpressError;
  Parser.OnEvaluateString := EvaluateString;
  Parser.OnStoreObject := StoreObject;
  Parser.OnCreateQuery := CreateQuery;
  Parser.OnHaveSqlToken := HaveSqlToken;
  Parser.OnHaveSqlBind := HaveSqlBind;
  Parser.OnHaveSqlParam := HaveSqlParam;
  Parser.OnHaveSqlInsertParam := HaveSqlInsertParam;
  Parser.OnNeedSqlExec := NeedSqlExec;
  Parser.OnNeedSqlOpen := NeedSqlOpen;
  Parser.OnNeedSqlClose := NeedSqlClose;
  Parser.OnNeedSqlNextRow := NeedNextSqlRow;
  Parser.OnNeedSqlFirstRow := NeedFirstSqlRow;
  Parser.OnFinshInsertQuery := FinishInsertQuery;
  LexicalList:= nil;
  fBindList := TBindList.Create;
  fBindList.Sorted := True;
  fBindList.Duplicates := DupError;
  BindBuildIns;
end;

destructor TExpress.Destroy;
var
  x: Integer;
begin
  for x := 0 to fBindList.Count - 1 do
    fBindList.Binds[x].Free;
  fBindList.Clear;
  fBindList.Free;
  Parser.Free;
  Scanner.Free;
  ClearLexicalList;
  LexicalList.Free;
  XpQueryList.Clear;
  XpQueryList.Free;
  with ObjList do
  begin
    while Count > 0 do
    begin
      Objects[0].Free;
      Delete(0);
    end;
    Free;
  end;
  with Blocks do
  begin
    while Count > 0 do
    begin
      Objects[0].Free;
      Delete(0);
    end;
    Free;
  end;
  FreeAndNil(fFunctionProtoTypes);
  FreeAndNil (fTextLines);
  inherited Destroy;
end;

function TExpress.GetSqlUsed: Boolean;
begin
  result := Parser.SqlUsed;
end;

function TExpress.FindBind(aId: String): TBind;
var
  f: Integer;
begin
  if fBindList.Find (Uppercase (aId), f) then
    result := fBindList.Binds [f]
  else
    result := nil;
end;

end.
