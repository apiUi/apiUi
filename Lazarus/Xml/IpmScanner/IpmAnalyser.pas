{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit IpmAnalyser;

{$MODE Delphi}

interface
uses Classes
   , ParserClasses
   , IPMPARSER
   , IPMSCANNER
   , CustScanner
   ;

const InternalStackSize = 256;
const InitState = 2; {taken from Scanner.pas}

type TIpmAnalyser = class (TObject)
private
  Stack: array [0..InternalStackSize] of Integer;
  StackIndex: Integer;
  LexItem: YYSType;
  LexicalList: YYSType;
  Scanner: TIpmScanner;
  Parser: TIpmParser;
  FOnError: TOnErrorEvent;
  FOnHaveData: TOnHaveDataEvent;
  FOnNeedData: TOnNeedDataEvent;
  procedure SetOnHaveScanned (aEvent: TOnHaveScannedEvent);
  function GetOnHaveScanned: TOnHaveScannedEvent;
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
  function IsKeyWord (id : string; var token : integer; var Address: Pointer) : boolean;
  function TokenToFloat (arg: String): Extended;
  procedure PushInteger (arg: Integer);
  function PopInteger: Integer;
  procedure HaveData (aObject: TObject; aString: String);
public
  StartState: Integer;
  property ScannedItems: YYSType read LexicalList;
  property OnHaveScanned: TOnHaveScannedEvent read GetOnHaveScanned write SetOnHaveScanned;
  property OnHaveData: TOnHaveDataEvent read FOnHaveData write FOnHaveData;
  property OnNeedData: TOnNeedDataEvent read FOnNeedData write FOnNeedData;
  property OnError: TOnErrorEvent read FOnError write FOnError;
  procedure DebugTokenStringList (arg: TStringList);
  procedure Prepare;
  procedure Execute;
  constructor Create (aOwner: TObject);
  destructor Destroy; override;
end;

implementation

uses SysUtils, Dialogs, Forms;

procedure TIpmAnalyser.DebugTokenStringList (arg: TStringList);
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

procedure TIpmAnalyser.ShowTokens;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    DebugTokenStringList (sl);
{
    ShowMemoDlg := TShowMemoDlg.Create (Application);
    try
      ShowMemoDlg.Caption := 'Overview of Tokens';
      ShowMemoDlg.Strings := sl;
      ShowMemoDlg.ShowModal;
    finally
      ShowMemoDlg.Free;
    end;
}
    ShowMessage (sl.Text);
  finally
    sl.Free;
  end;
end;

procedure TIpmAnalyser.PushInteger (arg: Integer);
begin
  if StackIndex > InternalStackSize then
    raise Exception.Create ('Internal stack overflow')
  else
  begin
    Stack [StackIndex] := arg;
    Inc (StackIndex);
  end;
end;

procedure TIpmAnalyser.SetOnHaveScanned (aEvent: TOnHaveScannedEvent);
begin
  Parser.OnHaveScanned := aEvent;
end;

function TIpmAnalyser.GetOnHaveScanned: TOnHaveScannedEvent;
begin
  result := Parser.OnHaveScanned;
end;

procedure TIpmAnalyser.HaveData (aObject: TObject; aString: String);
begin
  if Assigned (FOnHaveData) then
    FOnHaveData (Self, aString)
  else
    raise Exception.Create ('No OnHaveData proc assigned');
end;

function TIpmAnalyser.PopInteger: Integer;
begin
  if StackIndex <= 0 then
    Raise Exception.Create ('Internal stack underflow')
  else
  begin
    Dec (StackIndex);
    result := Stack [StackIndex];
  end;
end;

procedure TIpmAnalyser.AnalyserScannerError (Sender: TObject; Data: String);
begin
    ShowMessage ('Scanner: ' + Data);
end;

procedure TIpmAnalyser.AnalyserParserError
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
    ShowMessage (Data);
end;

procedure TIpmAnalyser.ClearLexicalList;
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

function TIpmAnalyser.TokenToFloat (arg: String): Extended;
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

procedure TIpmAnalyser.ScannerNeedsData ( Sender:TObject
                                    ; var MoreData: Boolean
                                    ; var Data: String
                                    );
begin
  if Assigned (FOnNeedData) then
    FOnNeedData (Self, MoreData, Data)
  else
    MoreData := False;
end;

function TIpmAnalyser.IsKeyWord (id : string; var token : integer; var Address: Pointer) : boolean;
type LexItem = record
  Wrd: String;
  Tkn: Integer;
  Adr: Pointer;
end;

const
  NoOfLexItems = 32;
  LexItems : array [1..NoOfLexItems] of LexItem =
  ( (Wrd: 'ARE'; Tkn: _IS; adr: nil)
  , (Wrd: 'BINARY'; Tkn: _COMP; adr: nil)
  , (Wrd: 'BY'; Tkn: _BY; adr: nil)
  , (Wrd: 'COMP'; Tkn: _COMP; adr: nil)
  , (Wrd: 'COMP-4'; Tkn: _COMP; adr: nil)
  , (Wrd: 'DEPENDING'; Tkn: _DEPENDING; adr: nil)
  , (Wrd: 'DISPLAY'; Tkn: _DISPLAY; adr: nil)
  , (Wrd: 'IN'; Tkn: _IN; adr: nil)
  , (Wrd: 'INDEXED'; Tkn: _INDEXED; adr: nil)
  , (Wrd: 'IS'; Tkn: _IS; adr: nil)
  , (Wrd: 'LEADING'; Tkn: _LEADING; adr: nil)
  , (Wrd: 'NATIVE-2'; Tkn: _NATIVE_2; adr: nil)
  , (Wrd: 'NATIVE-4'; Tkn: _NATIVE_4; adr: nil)
  , (Wrd: 'NATIVE-8'; Tkn: _NATIVE_8; adr: nil)
  , (Wrd: 'OCCURS'; Tkn: _OCCURS; adr: nil)
  , (Wrd: 'OF'; Tkn: _OF; adr: nil)
  , (Wrd: 'ON'; Tkn: _ON; adr: nil)
  , (Wrd: 'REDEFINES'; Tkn: _REDEFINES; adr: nil)
  , (Wrd: 'SEPARATE'; Tkn: _SEPARATE; adr: nil)
  , (Wrd: 'SIGN'; Tkn: _SIGN; adr: nil)
  , (Wrd: 'SPACE'; Tkn: _SPACES; adr: nil)
  , (Wrd: 'SPACES'; Tkn: _SPACES; adr: nil)
  , (Wrd: 'THROUGH'; Tkn: _THROUGH; adr: nil)
  , (Wrd: 'THRU'; Tkn: _THROUGH; adr: nil)
  , (Wrd: 'TIMES'; Tkn: _TIMES; adr: nil)
  , (Wrd: 'TO'; Tkn: _TO; adr: nil)
  , (Wrd: 'TRAILING'; Tkn: _TRAILING; adr: nil)
  , (Wrd: 'USAGE'; Tkn: _USAGE; adr: nil)
  , (Wrd: 'VALUE'; Tkn: _VALUE; adr: nil)
  , (Wrd: 'VALUES'; Tkn: _VALUE; adr: nil)
  , (Wrd: 'ZERO'; Tkn: _ZEROES; adr: nil)
  , (Wrd: 'ZEROES'; Tkn: _ZEROES; adr: nil)
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

procedure TIpmAnalyser.OnToken (Sender: TObject);
var
  Scanner: TIpmScanner;
  Lexical: YYSType;
  CobolNumberString: String;
begin
  Scanner := Sender as TIpmScanner;
  if (Scanner.Token = _IGNORE) then
    exit;
  Lexical := YYSType.Create;
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
    _NOID:
    begin
      IsKeyWord ( Scanner.TokenAsString
                , Lexical.Token
                , Lexical.yy.yyPointer
                );
      case Lexical.Token of
        _SPACES:
        begin {treath spaces as empty string}
          Lexical.Token := _STRING;
          Lexical.TokenString := '" "';
          Lexical.yyString := ' ';
          Lexical.yyStringRead := Lexical.yyString;
        end;
        _ZEROES:
        begin {treath zeroes as integer with value 0}
          Lexical.Token := _INTEGER;
          Lexical.TokenString := '0';
          Lexical.yyString := '0';
          Lexical.yyStringRead := Lexical.yyString;
        end;
      end;
    end;
    _STRING:
    begin
      Lexical.yyString := Copy (Scanner.TokenAsString, 2, system.Length (Scanner.TokenAsString) - 2);
      Lexical.yyStringRead := Lexical.yyString;
    end;
    _INTEGER:
    begin
      Lexical.yy.yyInteger := Trunc (TokenToFloat (Scanner.TokenAsString));
    end;
  end;
  Lexical.yyRead := Lexical.yy;
end;

procedure TIpmAnalyser.Prepare;
begin
  ClearLexicalList;
  Scanner.OnToken := OnToken;
  StackIndex := 0;
  Scanner.Start (StartState);
  Scanner.Execute;
  Parser.LexItems := LexicalList;
  {ifdef DEBUG}
{  ShowTokens; }
  {endifdef DEBUG}
{  Parser.Prepare; }
end;

procedure TIpmAnalyser.Execute;
begin
  LexItem := LexicalList;
  Parser.DoIt := True;
  Parser.Execute;
end;

constructor TIpmAnalyser.Create (aOwner: TObject);
begin
  Scanner := TIpmScanner.Create;
  Scanner.OnNeedData := ScannerNeedsData;
  Scanner.OnError := AnalyserScannerError;
  Parser := TIpmParser.Create (aOwner);
  Parser.OnHaveData := HaveData;
  Parser.OnError := AnalyserParserError;
  LexicalList:= nil;
end;

destructor TIpmAnalyser.Destroy;
begin
  Parser.Free;
  Scanner.Free;
  ClearLexicalList;
  LexicalList.Free;
  inherited Destroy;
end;

end.
