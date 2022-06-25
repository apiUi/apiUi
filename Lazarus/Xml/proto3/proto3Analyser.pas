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
unit proto3Analyser;

{$MODE Delphi}

interface
uses Classes
   , ParserClasses
   , proto3Parser
   , proto3Scanner
   , CustScanner
   , Wsdlz, Xsdz, xmlio
   ;

const InternalStackSize = 256;
const InitState = 2; {taken from Scanner.pas}
  {$INCLUDE proto3parser.def}

type

{ TProto3Analyser }

 TProto3Analyser = class (TObject)
private
  Stack: array [0..InternalStackSize] of Integer;
  StackIndex, offset: Integer;
  LexItem, prevItem: YYSType;
  LexicalList: YYSType;
  Scanner: TProto3Scanner;
  Parser: TProto3Parser;
  FOnError: TOnErrorEvent;
  FOnHaveData: TOnHaveDataEvent;
  FOnNeedData: TOnNeedDataEvent;
  function GetTokenNames(Index: Integer): String;
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
  function IsKeyWord (prevToken: Integer; id : string; var token : integer; var Address: Pointer) : boolean;
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
  property TokenNames [Index: Integer]: String read GetTokenNames;
  procedure DebugTokenStringList (arg: TStringList);
  procedure Prepare;
  procedure Execute;
  constructor Create (aOwner: TObject);
  destructor Destroy; override;
end;

implementation

uses SysUtils, Dialogs, Forms, Math;

procedure TProto3Analyser.DebugTokenStringList (arg: TStringList);
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

procedure TProto3Analyser.ShowTokens;
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

procedure TProto3Analyser.PushInteger (arg: Integer);
begin
  if StackIndex > InternalStackSize then
    raise Exception.Create ('Internal stack overflow')
  else
  begin
    Stack [StackIndex] := arg;
    Inc (StackIndex);
  end;
end;

procedure TProto3Analyser.SetOnHaveScanned (aEvent: TOnHaveScannedEvent);
begin
  Parser.OnHaveScanned := aEvent;
end;

function TProto3Analyser.GetTokenNames(Index: Integer): String;
begin
  result := dTokenNames[index];
end;

function TProto3Analyser.GetOnHaveScanned: TOnHaveScannedEvent;
begin
  result := Parser.OnHaveScanned;
end;

procedure TProto3Analyser.HaveData (aObject: TObject; aString: String);
begin
  if Assigned (FOnHaveData) then
    FOnHaveData (Self, aString)
  else
    raise Exception.Create ('No OnHaveData proc assigned');
end;

function TProto3Analyser.PopInteger: Integer;
begin
  if StackIndex <= 0 then
    Raise Exception.Create ('Internal stack underflow')
  else
  begin
    Dec (StackIndex);
    result := Stack [StackIndex];
  end;
end;

procedure TProto3Analyser.AnalyserScannerError (Sender: TObject; Data: String);
begin
    ShowMessage ('Scanner: ' + Data);
end;

procedure TProto3Analyser.AnalyserParserError
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

procedure TProto3Analyser.ClearLexicalList;
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
  prevItem := nil;
end;

procedure TProto3Analyser.ScannerNeedsData ( Sender:TObject
                                    ; var MoreData: Boolean
                                    ; var Data: String
                                    );
begin
  if Assigned (FOnNeedData) then
    FOnNeedData (Self, MoreData, Data)
  else
    MoreData := False;
end;

function TProto3Analyser.IsKeyWord (prevToken: Integer; id : string; var token : integer; var Address: Pointer) : boolean;
type LexItem = record
  Wrd: String;
  Tkn: Integer;
  Adr: Pointer;
end;

const
  NoOfLexItems = 36;
  LexItems : array [1..NoOfLexItems] of LexItem =
    ( (Wrd: 'bool'; Tkn: _BOOL; adr: nil)
    , (Wrd: 'bytes'; Tkn: _BYTES; adr: nil)
    , (Wrd: 'double'; Tkn: _DOUBLE; adr: nil)
    , (Wrd: 'enum'; Tkn: _ENUM; adr: nil)
    , (Wrd: 'enumType'; Tkn: _ENUMTYPE; adr: nil)
    , (Wrd: 'fixed32'; Tkn: _FIXED32; adr: nil)
    , (Wrd: 'fixed64'; Tkn: _FIXED64; adr: nil)
    , (Wrd: 'float'; Tkn: _FLOAT; adr: nil)
    , (Wrd: 'import'; Tkn: _IMPORT; adr: nil)
    , (Wrd: 'int32'; Tkn: _INT32; adr: nil)
    , (Wrd: 'int64'; Tkn: _INT64; adr: nil)
    , (Wrd: 'map'; Tkn: _MAP; adr: nil)
    , (Wrd: 'max'; Tkn: _MAX; adr: nil)
    , (Wrd: 'message'; Tkn: _MESSAGE; adr: nil)
    , (Wrd: 'messageType'; Tkn: _MESSAGETYPE; adr: nil)
    , (Wrd: 'oneof'; Tkn: _ONEOF; adr: nil)
    , (Wrd: 'option'; Tkn: _OPTION; adr: nil)
    , (Wrd: 'package'; Tkn: _PACKAGE; adr: nil)
    , (Wrd: 'packed'; Tkn: _PACKED; adr: nil)
    , (Wrd: 'public'; Tkn: _PUBLIC; adr: nil)
    , (Wrd: 'repeated'; Tkn: _REPEATED; adr: nil)
    , (Wrd: 'reserved'; Tkn: _RESERVED; adr: nil)
    , (Wrd: 'returns'; Tkn: _RETURNS; adr: nil)
    , (Wrd: 'rpc'; Tkn: _RPC; adr: nil)
    , (Wrd: 'service'; Tkn: _SERVICE; adr: nil)
    , (Wrd: 'sfixed32'; Tkn: _SFIXED32; adr: nil)
    , (Wrd: 'sfixed64'; Tkn: _SFIXED64; adr: nil)
    , (Wrd: 'sint32'; Tkn: _SINT32; adr: nil)
    , (Wrd: 'sint64'; Tkn: _SINT64; adr: nil)
    , (Wrd: 'stream'; Tkn: _STREAM; adr: nil)
    , (Wrd: 'string'; Tkn: _STRING; adr: nil)
    , (Wrd: 'syntax'; Tkn: _SYNTAX; adr: nil)
    , (Wrd: 'to'; Tkn: _TO; adr: nil)
    , (Wrd: 'uint32'; Tkn: _UINT32; adr: nil)
    , (Wrd: 'uint64'; Tkn: _UINT64; adr: nil)
    , (Wrd: 'weak'; Tkn: _WEAK; adr: nil)
  );

var m, n, k : integer;
begin
//id := uppercase (id);
  result := False;
  if (prevToken <> _TERMINATOR)
  and (prevToken <> _OPENBRACE)
  and (prevToken <> _CLOSEBRACE)
  and (prevToken <> _CLOSEPARENTHESIS)
  and (prevToken <> _REPEATED)
  and (prevToken <> _IMPORT)
  and (prevToken <> _DECIMALLIT)
  and (prevToken <> _OCTALLIT)
  and (prevToken <> _HEXLIT)
  and (prevToken <> _LT)
  then
    Exit;
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

procedure TProto3Analyser.OnToken (Sender: TObject);
  function StrToIntOctal (s: string): Longint;
  var
    x: Integer;
  begin
    result := 0;
    for x := 1 to Length(s) do
      result := result * 8 + StrToInt (s[x]);
  end;
  function StrToIntHex (s: string): Longint;
  var
    x: Integer;
    d: Integer;
  begin
    // s: 0[xX]{hexDigit}*
    result := 0;
    s := UpperCase(s);
    if Length (s) > 2 then
      for x := 3 to Length(s) do
      begin
        if s[x] > '9' then
          d := 10 + Ord (s[x]) - Ord ('A')
        else
          d := Ord (s[x]) - Ord ('0');
        result := result * 16 + d;
      end;
  end;
var
  Scanner: TProto3Scanner;
  Lexical: YYSType;
  prevToken: Integer;
begin
  Scanner := Sender as TProto3Scanner;
  if (Scanner.Token = _IGNORE) then
  begin
    offset := offset + Length (Scanner.TokenAsString);
    exit;
  end;
  if (Scanner.Token = _LINEENDING) then
  begin
    offset := offset + Length (LineEnding);
    exit;
  end;
  Lexical := YYSType.Create;
  Lexical.Prev := PrevItem;
  if LexicalList = nil then
  begin
    LexicalList := Lexical;
    prevToken := _TERMINATOR;
  end
  else
  begin
    LexItem.Next := Lexical;
    LexItem.NextToken := Lexical;
    prevToken := prevItem.Token;
  end;
  PrevItem := Lexical;
  LexItem := Lexical;
  Lexical.Next := nil;
  Lexical.NextToken := nil;
  Lexical.LineNumber := Scanner.LineNumber;
  Lexical.ColumnNumber := Scanner.ColumnNumber;
  Lexical.Token := Scanner.Token;
  Lexical.TokenString := Scanner.TokenAsString;
  Lexical.yyString := Lexical.TokenString;
  Lexical.yyStringRead := Lexical.yyString;
  Lexical.Offset := Offset;
  case Lexical.Token of
    _IDENT:
      begin
        IsKeyWord ( prevToken
                  , Scanner.TokenAsString
                  , Lexical.Token
                  , Lexical.yy.yyPointer
                  );
      end;
    _STRINGLIT:
      begin
        Lexical.yyString := Copy (Scanner.TokenAsString, 2, system.Length (Scanner.TokenAsString) - 2);
        Lexical.yyStringRead := Lexical.yyString;
      end;
    _DECIMALLIT:
    begin
      Lexical.yy.yyInteger := StrToInt64 (Scanner.TokenAsString);
    end;
    _OCTALLIT:
    begin
      Lexical.yy.yyInteger := StrToIntOctal (Scanner.TokenAsString);
    end;
    _HEXLIT:
    begin
      Lexical.yy.yyInteger := StrToIntHex (Scanner.TokenAsString);
    end;
    _FLOATLIT:
    begin
      if Scanner.TokenAsString = 'nan' then
        Lexical.yy.yyExtended := Math.NaN
      else
      begin
        if Scanner.TokenAsString = 'inf' then
          Lexical.yy.yyExtended := Math.Infinity
        else
          Lexical.yy.yyExtended := StrToFloat (Scanner.TokenAsString);
      end;
    end;
  end;
  Lexical.yyRead := Lexical.yy;
  offset := offset + Length(Scanner.TokenAsString);
  prevToken := Lexical.Token;
end;

procedure TProto3Analyser.Prepare;
begin
  ClearLexicalList;
  Scanner.OnToken := OnToken;
  StackIndex := 0;
  offset := 1;
  Scanner.Start (StartState);
  Scanner.Execute;
  Parser.LexItems := LexicalList;
  Parser.Prepare;
  {ifdef DEBUG}
{  ShowTokens; }
  {endifdef DEBUG}
{  Parser.Prepare; }
end;

procedure TProto3Analyser.Execute;
begin
  LexItem := LexicalList;
  Parser.DoIt := True;
  Parser.Execute;
end;

constructor TProto3Analyser.Create (aOwner: TObject);
begin
  Scanner := TProto3Scanner.Create;
  Scanner.OnNeedData := ScannerNeedsData;
  Scanner.OnError := AnalyserScannerError;
  Parser := TProto3Parser.Create (aOwner);
  Parser.wsdl := TWsdl.Create(nil, false);
  Parser.OnHaveData := HaveData;
  Parser.OnError := AnalyserParserError;
  LexicalList:= nil;
end;

destructor TProto3Analyser.Destroy;
begin
  FreeAndNil (Parser.wsdl);
  Parser.Free;
  Scanner.Free;
  ClearLexicalList;
  LexicalList.Free;
  inherited Destroy;
end;

end.
