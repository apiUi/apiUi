unit CustAnalyser;

interface
uses Classes
   , ParserClasses
   , CustParser
   , CustScanner
   ;

const InternalStackSize = 256;

type TCustAnalyser = class (TObject)
protected
  Stack: array [0..InternalStackSize] of Integer;
  StackIndex: Integer;
  LexItem: YYSType;
  LexicalList: YYSType;
  Scanner: TCustScanner;
  Parser: TCustParser;
  procedure ShowTokens;
  procedure SetOnHaveScanned (arg: TOnHaveScannedEvent);
  function GetOnHaveScanned: TOnHaveScannedEvent;
  procedure SetOnNeedData (arg: TOnNeedDataEvent);
  function GetOnNeedData: TOnNeedDataEvent;
  procedure SetOnError (arg: TOnErrorEvent);
  function GetOnError: TOnErrorEvent;
  procedure SetOnScannerError (arg: TOnScannerErrorEvent);
  function GetOnScannerError: TOnScannerErrorEvent;
  procedure AnalyserScannerError (Sender: TObject; Data: String);
  procedure ClearLexicalList;
  procedure OnToken (Sender: TObject);
  function TokenToFloat (arg: String): Extended;
  procedure PushInteger (arg: Integer);
  function PopInteger: Integer;
  procedure HaveToken (var Ignore: Boolean; Lexical: YYSType); virtual;
published
  property OnHaveScanned: TOnHaveScannedEvent read GetOnHaveScanned write SetOnHaveScanned;
  property OnNeedData: TOnNeedDataEvent read GetOnNeedData write SetOnNeedData;
  property OnError: TOnErrorEvent read GetOnError write SetOnError;
  property OnScannerError: TOnScannerErrorEvent read GetOnScannerError write SetOnScannerError;
  property ScannedItems: YYSType read LexicalList;
  procedure DebugTokenStringList (arg: TStringList);
  procedure Prepare; dynamic;
  procedure Execute; dynamic;
  constructor Create (aOwner: TObject); override;
  destructor Destroy; override;
public
  InitState: Integer;
end;

implementation

uses SysUtils
   ;

procedure TCustAnalyser.DebugTokenStringList (arg: TStringList);
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

procedure TCustAnalyser.ShowTokens;
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  DebugTokenStringList (StringList);
{
  ShowMemoDlg := TShowMemoDlg.Create (Application);
  ShowMemoDlg.Caption := 'Overview of Tokens';
  ShowMemoDlg.Strings := StringList;
  ShowMemoDlg.ShowModal;
  ShowMemoDlg.Free;
}
  StringList.Free;
end;

procedure TCustAnalyser.PushInteger (arg: Integer);
begin
  if StackIndex > InternalStackSize then
    raise Exception.Create ('Internal stack overflow')
  else
  begin
    Stack [StackIndex] := arg;
    Inc (StackIndex);
  end;
end;

function TCustAnalyser.PopInteger: Integer;
begin
  if StackIndex <= 0 then
    Raise Exception.Create ('Internal stack underflow')
  else
  begin
    Dec (StackIndex);
    result := Stack [StackIndex];
  end;
end;

procedure TCustAnalyser.AnalyserScannerError (Sender: TObject; Data: String);
begin
    WriteLn ('Scanner: ' + Data);
end;

function TCustAnalyser.GetOnHaveScanned: TOnHaveScannedEvent;
begin
  result := Parser.OnHaveScanned;
end;

procedure TCustAnalyser.SetOnHaveScanned (arg: TOnHaveScannedEvent);
begin
  if assigned (Parser) then
    Parser.OnHaveScanned := arg;
end;

function TCustAnalyser.GetOnNeedData: TOnNeedDataEvent;
begin
  result := Scanner.OnNeedData;
end;

procedure TCustAnalyser.SetOnNeedData (arg: TOnNeedDataEvent);
begin
  if assigned (Scanner) then
    Scanner.OnNeedData := arg;
end;

function TCustAnalyser.GetOnError: TOnErrorEvent;
begin
  result := Parser.OnError;
end;

procedure TCustAnalyser.SetOnError (arg: TOnErrorEvent);
begin
  if assigned (Parser) then
    Parser.OnError := arg;
end;

function TCustAnalyser.GetOnScannerError: TOnScannerErrorEvent;
begin
  result := Scanner.OnError;
end;

procedure TCustAnalyser.SetOnScannerError (arg: TOnScannerErrorEvent);
begin
  if assigned (Scanner) then
    Scanner.OnError := arg;
end;

procedure TCustAnalyser.ClearLexicalList;
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

function TCustAnalyser.TokenToFloat (arg: String): Extended;
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

procedure TCustAnalyser.HaveToken (var Ignore: Boolean; Lexical: YYSType);
begin
  Ignore := (Lexical.Token = _COMMENT);
  if Lexical.Token = _COMMENT then
  begin
    Lexical.Token := Lexical.Token;
    ShowMessage ('//');
  end;
end;

procedure TCustAnalyser.OnToken (Sender: TObject);
var
  Scanner: TCustScanner;
  Lexical: YYSType;
  CobolNumberString: String;
  Ignore: Boolean;
begin
  Scanner := Sender as TCustScanner;
  Lexical := YYSType.Create;
  Lexical.Next := nil;
  Lexical.NextToken := nil;
  Lexical.LineNumber := Scanner.LineNumber;
  Lexical.ColumnNumber := Scanner.ColumnNumber;
  Lexical.Offset := Scanner.Offset;
  Lexical.Token := Scanner.Token;
  Lexical.TokenString := Scanner.TokenAsString;
  Ignore := False;
  HaveToken (Ignore, Lexical);
  if Ignore then
    Lexical.Free
  else
  begin
    Lexical.yyRead := Lexical.yy;
    if LexicalList = nil then
      LexicalList := Lexical
    else
    begin
      LexItem.Next := Lexical;
      LexItem.NextToken := Lexical;
    end;
    LexItem := Lexical;
  end;
end;

procedure TCustAnalyser.Prepare;
begin
  ClearLexicalList;
  Scanner.OnToken := OnToken;
  StackIndex := 0;
  Scanner.Start (InitState);
  Scanner.Execute;
  Parser.LexItems := LexicalList;
  {ifdef DEBUG}
{  ShowTokens; }
  {endifdef DEBUG}
{  Parser.Prepare; }
end;

procedure TCustAnalyser.Execute;
begin
  LexItem := LexicalList;
  Parser.DoIt := True;
  Parser.Execute;
end;

constructor TCustAnalyser.Create (aOnwer: TObject);
begin
  inherited Create (AComponent);
  LexicalList:= nil;
end;

destructor TCustAnalyser.Destroy;
begin
  ClearLexicalList;
  LexicalList.Free;
  inherited Destroy;
end;

end.
