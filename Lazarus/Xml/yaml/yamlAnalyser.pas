{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

unit yamlAnalyser;

interface
uses Classes
   , ParserClasses
   , igGlobals
   , yamlParser
   , yamlScanner
   , CustScanner
   , Xmlz
   ;

const InternalStackSize = 256;
const InitState = 2; {taken from Scanner.pas}

type

{ TyamlAnalyser }

 TyamlAnalyser = class (TComponent)
private
  Stack: array [0..InternalStackSize] of Integer;
  StackIndex, HyphenIndent, lexIndent, Offset: Integer;
  LexItem, PrevLexItem: YYSType;
  LexicalList: YYSType;
  Scanner: TyamlScanner;
  Parser: TyamlParser;
  FOnError: TOnErrorEvent;
  FOnHaveData: TOnHaveDataEvent;
  FOnNeedData: TOnNeedDataEvent;
  ValueString: String;
  function GetTokenNames (Index : Integer ): String ;
  procedure SetOnHaveScanned (aEvent: TOnHaveScannedEvent);
  function GetOnHaveScanned: TOnHaveScannedEvent;
  procedure PrepareParsing;
  procedure ShowTokens;
  procedure AnalyserParserError ( Sender: TObject
                                ; LineNumber: Integer
                                ; ColumnNumber: Integer
                                ; Offset: Integer
                                ; TokenString: String
                                ; Data: String
                                );
  procedure AnalyserScannerError (Sender: TObject; Data: String);
  procedure ClearLexicalList;
  procedure OnToken (Sender: TObject);
  procedure ScannerNeedsData ( Sender:TObject
                             ; var MoreData: Boolean
                             ; var Data: String
                             );
  function TokenToFloat (arg: String): Extended;
  procedure PushInteger (arg: Integer);
  function PopInteger: Integer;
  procedure HaveData (aObject: TObject; aString: String);
  function GetTokenName (Index: Integer): String;
public
  StartState: Integer;
  Xml: TXml;
  property TokenNames [Index: Integer]: String read GetTokenNames;
published
  property ScannedItems: YYSType read LexicalList;
  property OnHaveScanned: TOnHaveScannedEvent read GetOnHaveScanned write SetOnHaveScanned;
  property OnHaveData: TOnHaveDataEvent read FOnHaveData write FOnHaveData;
  property OnNeedData: TOnNeedDataEvent read FOnNeedData write FOnNeedData;
  property OnError: TOnErrorEvent read FOnError write FOnError;
  procedure DebugTokenStringList (arg: TStringList);
  procedure Prepare;
  procedure Execute;
  constructor Create (AComponent: TComponent); override;
  destructor Destroy; override;
end;

implementation

uses SysUtils, StrUtils, Dialogs, ShowMemo, Forms, RegExpr;

procedure TyamlAnalyser.DebugTokenStringList (arg: TStringList);
var
  Lex: YYSType;
begin
  lex := LexicalList;
  arg.Clear;
  while (lex <> nil) do
  begin
    arg.Add ( IntToStr (Lex.Token)
            + '  :  '
    {
            + TokenNames [Lex.Token]
            + '   '
    }
            + Lex.TokenString
            + '  ('
            + IntToStr (Lex.LineNumber)
            + ' : '
            + IntToStr (Lex.ColumnNumber)
            + ')'
            );
    lex := lex.NextToken;
  end;
end;

procedure TyamlAnalyser.ShowTokens;
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  DebugTokenStringList (StringList);
  ShowMemoDlg := TShowMemoDlg.Create (Application);
  ShowMemoDlg.Caption := 'Overview of Tokens';
  ShowMemoDlg.Strings := StringList;
  ShowMemoDlg.ShowModal;
  ShowMemoDlg.Free;
  StringList.Free;
end;

procedure TyamlAnalyser.PushInteger (arg: Integer);
begin
  if StackIndex > InternalStackSize then
    raise Exception.Create ('Internal stack overflow')
  else
  begin
    Stack [StackIndex] := arg;
    Inc (StackIndex);
  end;
end;

procedure TyamlAnalyser.SetOnHaveScanned (aEvent: TOnHaveScannedEvent);
begin
  Parser.OnHaveScanned := aEvent;
end;

function TyamlAnalyser .GetTokenNames (Index : Integer ): String ;
const
{$INCLUDE parser.def}
begin
  result := TokenNames[index];
end;

function TyamlAnalyser.GetOnHaveScanned: TOnHaveScannedEvent;
begin
  result := Parser.OnHaveScanned;
end;

procedure TyamlAnalyser .PrepareParsing ;
var
  lx, lx_1, lx_2: YYSType;
  sep: String;
  rx: TRegExpr;
begin
  rx := TRegExpr.Create('\ *\#[^\n]*$'); // yaml comment
  try
    lx := LexicalList;
    while (Assigned(lx)) do
    begin
      if lx.Token = _VALUE then
      begin
        lx_1 := lx.NextToken;
        if Assigned (lx_1)
        and (lx_1.Token <> _LINECONT) then
        begin
          if rx.Exec (lx.yyStringRead)then
            lx.yyStringRead := Copy (lx.yyStringRead, 1, rx.MatchPos[0] - 1);
        end;
        while (Assigned (lx_1))
        and (   (lx_1.Token = _LINECONT)
             or (lx_1.Token = _VALUE)
            ) do
        begin
          if lx_1.Token = _LINECONT then
            lx.yyStringRead := lx.yyStringRead + ' '
          else
            lx.yyStringRead := lx.yyStringRead + lx_1.yyStringRead;
          lx_1 := lx_1.NextToken;
        end;
        lx.NextToken := lx_1;
      end;
      if lx.Token = _PIPE then
      begin
        lx_1 := lx.NextToken;
        sep := '';
        lx.yyStringRead := '';
        while Assigned (lx_1)
        and (lx_1.yy.yyInteger > lx.yy.yyInteger) do
        begin
          if (lx_1.Token <> _INDENT) then
          begin
            lx.yyStringRead := lx.yyStringRead + sep + lx_1.yyStringRead;
            sep := ' ';
          end;
          lx_1 := lx_1.NextToken;
        end;
        lx.NextToken := lx_1;
        lx.Token := _VALUE;
      end;
      lx := lx.NextToken;
    end;
  finally
    rx.Free;
  end;
  lx := LexicalList;
  lx_1 := lx;
  lx_2 := lx_1;
  while Assigned (lx) do
  begin
    if lx.Token = _NAME then
      lx.yyStringRead := Copy (lx.yyStringRead, 1, Length (lx.yyStringRead) - 1);
    if (lx.Token = _VALUE)
    or (lx.Token = _NAME) then
    begin
      if lx.yyStringRead <> '' then
      begin
        if (lx.yyStringRead[1] = '"')
        and (lx.yyStringRead[system.Length(lx.yyStringRead)] = '"')then
          lx.yyStringRead := Copy(lx.yyStringRead, 2, system.Length(lx.yyStringRead) - 2);
      end;
    end;
    if Assigned (lx) then
    begin
      if (lx.Token = _VALUE)
      and (lx_1.Token = _INDENT)
      and (lx_2.Token = _HYPHENINDENT)
      then
      begin
        lx_2.Token := _ARRAYVALUE;
        lx_2.yyStringRead := lx.yyStringRead;
        lx_2.NextToken := lx.NextToken;
      end;
      lx_2 := lx_1;
      lx_1 := lx;
      lx := lx.NextToken;
    end;
  end;
end;

procedure TyamlAnalyser.HaveData (aObject: TObject; aString: String);
begin
  if Assigned (FOnHaveData) then
    FOnHaveData (Self, aString)
  else
    raise Exception.Create ('No OnHaveData proc assigned');
end;

function TyamlAnalyser .GetTokenName (Index : Integer ): String ;
begin

end;

function TyamlAnalyser.PopInteger: Integer;
begin
  if StackIndex <= 0 then
    Raise Exception.Create ('Internal stack underflow')
  else
  begin
    Dec (StackIndex);
    result := Stack [StackIndex];
  end;
end;

procedure TyamlAnalyser.AnalyserScannerError (Sender: TObject; Data: String);
begin
    ShowMessage ('Scanner: ' + Data);
end;

procedure TyamlAnalyser.AnalyserParserError
                                ( Sender: TObject
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
    Raise Exception.Create ( 'Error reading Line: '
                           + IntToStr (LineNumber)
                           + ' Column: '
                           + IntToStr (ColumnNumber)
                           + ' Token: '
                           + Data
                           );
end;

procedure TyamlAnalyser.ClearLexicalList;
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
end;

function TyamlAnalyser.TokenToFloat (arg: String): Extended;
var
  SwapSeparator: Char;
begin
  SwapSeparator := DecimalSeparator;
  if SwapSeparator <> '.' then
    DecimalSeparator := '.';
  result := StrToFloat (arg);
  if SwapSeparator <> DecimalSeparator then
    DecimalSeparator := SwapSeparator;
end;

procedure TyamlAnalyser.ScannerNeedsData ( Sender:TObject
                                    ; var MoreData: Boolean
                                    ; var Data: String
                                    );
begin
  if Assigned (FOnNeedData) then
    FOnNeedData (Self, MoreData, Data)
  else
    MoreData := False;
end;

procedure TyamlAnalyser.OnToken (Sender: TObject);
var
  xScanner: TyamlScanner;
  Lexical: YYSType;
  CobolNumberString: String;
  EscapeChar: String;
  HexCode: Integer;  // hex character code (-1 on error)
begin
  xScanner := Sender as TyamlScanner;

  if (xScanner.Token <> _INDENT) then
    HyphenIndent := 0;
  if (xScanner.Token = _HYPHENINDENT) then
  begin
    HyphenIndent := Length(xScanner.TokenAsString);
    lexIndent := HyphenIndent;
  end;
  if (xScanner.Token = _INDENT) then
    lexIndent := HyphenIndent + Length(xScanner.TokenAsString);
  if (xScanner.Token = _NEWLINE)
  or (xScanner.Token = _COMMENT)
  then
    lexIndent := 0;

  if True then
  begin
    if (xScanner.Token = _NEWLINE) then
    begin
      lexIndent := 0;
      Offset := Offset + Length (LineEnding);
      exit;
    end;
    if (xScanner.Token = _WHITESPACE)
    or (xScanner.Token = _COMMENT)
    then
    begin
      Offset := Offset + Length (xScanner.TokenAsString);
      exit;
    end;
  end;

  Lexical := YYSType.Create;
  Lexical.yy.yyInteger := lexIndent;
  if LexicalList = nil then
    LexicalList := Lexical
  else
  begin
    PrevLexItem.Next := Lexical;
    PrevLexItem.NextToken := Lexical;
  end;
  Lexical.Next := nil;
  Lexical.NextToken := nil;
  Lexical.LineNumber := Scanner.LineNumber;
  Lexical.Offset := Offset;
  Lexical.ColumnNumber := Scanner.ColumnNumber;
  Lexical.Token := Scanner.Token;
  Lexical.TokenString := Scanner.TokenAsString;
  Lexical.yyStringRead := Lexical.TokenString;
  Lexical.yyRead := Lexical.yy;
  PrevLexItem := Lexical;
  Offset := Offset + Length (Lexical.TokenString);
end;

procedure TyamlAnalyser.Prepare;
begin
  ClearLexicalList;
  ValueString := '';
  Scanner.OnToken := OnToken;
  StackIndex := 0;
  Offset := 1;
  lexIndent := 0;
  HyphenIndent := 0;
  Scanner.Start (StartState);
  Scanner.Execute;
  PrepareParsing;
  Parser.LexItems := LexicalList;
  Parser.Xml := Xml;
  {ifdef DEBUG}
{  ShowTokens; }
  {endifdef DEBUG}
{  Parser.Prepare; }
end;

procedure TyamlAnalyser.Execute;
begin
  LexItem := LexicalList;
  Parser.DoIt := True;
  Parser.Execute;
end;

constructor TyamlAnalyser.Create (AComponent: TComponent);
begin
  inherited Create (AComponent);
  Scanner := TyamlScanner.Create;
  Scanner.OnNeedData := ScannerNeedsData;
  Scanner.OnError := AnalyserScannerError;
  Parser := TyamlParser.Create (AComponent);
  Parser.OnHaveData := HaveData;
  Parser.OnError := AnalyserParserError;
  LexicalList:= nil;
end;

destructor TyamlAnalyser.Destroy;
begin
  Parser.Free;
  Scanner.Free;
  ClearLexicalList;
  LexicalList.Free;
  inherited Destroy;
end;

end.
