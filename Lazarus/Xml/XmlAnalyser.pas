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
unit XmlAnalyser;

interface
uses Classes
   , ParserClasses
   , XmlParser
   , XmlScanner
   , CustScanner
   , Xmlz
   ;

const InternalStackSize = 256;
const InitState = 2; {taken from Scanner.pas}

type TXmlAnalyser = class (TObject)
private
  Stack: array [0..InternalStackSize] of Integer;
  StackIndex: Integer;
  LexItem: YYSType;
  LexicalList: YYSType;
  Scanner: TXmlScanner;
  Parser: TXmlParser;
  FOnError: TOnErrorEvent;
  FOnHaveData: TOnHaveDataEvent;
  FOnNeedData: TOnNeedDataEvent;
  ValueString: String;
  function getBaseXml: TXml;
  procedure setBaseXml(const Value: TXml);
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
  function TokenToFloat (arg: String): Extended;
  procedure PushInteger (arg: Integer);
  function PopInteger: Integer;
  procedure HaveData (aObject: TObject; aString: String);
public
  StartState: Integer;
  property BaseXml: TXml read getBaseXml write setBaseXml;
  property ScannedItems: YYSType read LexicalList;
  property OnHaveScanned: TOnHaveScannedEvent read GetOnHaveScanned write SetOnHaveScanned;
  property OnHaveData: TOnHaveDataEvent read FOnHaveData write FOnHaveData;
  property OnNeedData: TOnNeedDataEvent read FOnNeedData write FOnNeedData;
  property OnError: TOnErrorEvent read FOnError write FOnError;
  procedure DebugTokenStringList (arg: TStringList);
  procedure Prepare;
  procedure Execute;
  constructor Create (aOwner: TOBject);
  destructor Destroy; override;
end;

implementation

uses SysUtils, StrUtils;

procedure TXmlAnalyser.DebugTokenStringList (arg: TStringList);
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

procedure TXmlAnalyser.ShowTokens;
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

procedure TXmlAnalyser.PushInteger (arg: Integer);
begin
  if StackIndex > InternalStackSize then
    raise Exception.Create ('Internal stack overflow')
  else
  begin
    Stack [StackIndex] := arg;
    Inc (StackIndex);
  end;
end;

procedure TXmlAnalyser.SetOnHaveScanned (aEvent: TOnHaveScannedEvent);
begin
  Parser.OnHaveScanned := aEvent;
end;

function TXmlAnalyser.GetOnHaveScanned: TOnHaveScannedEvent;
begin
  result := Parser.OnHaveScanned;
end;

procedure TXmlAnalyser.HaveData (aObject: TObject; aString: String);
begin
  if Assigned (FOnHaveData) then
    FOnHaveData (Self, aString)
  else
    raise Exception.Create ('No OnHaveData proc assigned');
end;

function TXmlAnalyser.PopInteger: Integer;
begin
  if StackIndex <= 0 then
    Raise Exception.Create ('Internal stack underflow')
  else
  begin
    Dec (StackIndex);
    result := Stack [StackIndex];
  end;
end;

procedure TXmlAnalyser.AnalyserScannerError (Sender: TObject; Data: String);
begin
  raise Exception.Create ('Scanner: ' + Data);
end;

procedure TXmlAnalyser.AnalyserParserError
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
    Raise Exception.Create ( 'Error reading XML Line: '
                           + IntToStr (LineNumber)
                           + ' Column: '
                           + IntToStr (ColumnNumber)
                           + ' Token: '
                           + Data
                           );
end;

procedure TXmlAnalyser.ClearLexicalList;
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

function TXmlAnalyser.TokenToFloat (arg: String): Extended;
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

procedure TXmlAnalyser.ScannerNeedsData ( Sender:TObject
                                    ; var MoreData: Boolean
                                    ; var Data: String
                                    );
begin
  if Assigned (FOnNeedData) then
    FOnNeedData (Self, MoreData, Data)
  else
    MoreData := False;
end;

procedure TXmlAnalyser.OnToken (Sender: TObject);
var
  xScanner: TXmlScanner;
  Lexical: YYSType;
  CobolNumberString: String;
  EscapeChar: String;
  HexCode: Integer;  // hex character code (-1 on error)
begin
  xScanner := Sender as TXmlScanner;
  if (xScanner.Token = _IGNORE) then
    exit;
  if (xScanner.Token = _VALUE) then
  begin
    ValueString := ValueString + xScanner.TokenAsString;
    exit;
  end;
  if (xScanner.Token = _ESCAPECHAR) then
  begin
    EscapeChar := uppercase (xScanner.TokenAsString);
    if EscapeChar = '&LT;' then
      EscapeChar := '<';
    if EscapeChar = '&GT;' then
      EscapeChar := '>';
    if EscapeChar = '&AMP;' then
      EscapeChar := '&';
    if EscapeChar = '&APOS;' then
      EscapeChar := '''';
    if EscapeChar = '&QUOT;' then
      EscapeChar := '"';
    if LeftStr (EscapeChar, 3) = '&#X' then // &#xXX; - XML encoding convention
    begin
      EscapeChar := '$' + Copy (EscapeChar, 4, Length(EscapeChar) - 4);
      HexCode := StrToIntDef(EscapeChar, -1);
      if HexCode = -1 then
        raise Exception.Create('Invalid Hex: ' + EscapeChar);
      EscapeChar := Chr (HexCode);
    end;
    if LeftStr (EscapeChar, 2) = '&%' then // &%XX; - IPMdata spec convention
    begin
      EscapeChar := '$' + Copy (EscapeChar, 3, Length(EscapeChar) - 3);
      HexCode := StrToIntDef(EscapeChar, -1);
      if HexCode = -1 then
        raise Exception.Create('Invalide Hex: ' + EscapeChar);
      EscapeChar := Chr (HexCode);
    end;
    ValueString := ValueString + EscapeChar;
    exit;
  end;
  if (   (xScanner.Token = _SLASHTAG)
      or (xScanner.Token = _TAG)
      or (xScanner.Token = _ENDTAG)
     )
  and (ValueString <> '')
  then
  begin
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
    Lexical.LineNumber := xScanner.LineNumber; // not correct
    Lexical.ColumnNumber := xScanner.ColumnNumber;
    Lexical.Token := _VALUE;
    Lexical.TokenString := ValueString;
    Lexical.yyRead := Lexical.yy;
    ValueString := '';
  end;
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
  Lexical.LineNumber := xScanner.LineNumber;
  Lexical.ColumnNumber := xScanner.ColumnNumber;
  Lexical.Token := xScanner.Token;
  Lexical.TokenString := xScanner.TokenAsString;
{
  case Lexical.Token of
    _TAG: ValueString := '';
    _ENDTAG: ValueString := '';
  end;
}
  Lexical.yyRead := Lexical.yy;
end;

procedure TXmlAnalyser.Prepare;
begin
  ClearLexicalList;
  ValueString := '';
  Scanner.OnToken := @OnToken;
  StackIndex := 0;
  Scanner.Start (StartState);
  Scanner.Execute;
  Parser.LexItems := LexicalList;
  {ifdef DEBUG}
{  ShowTokens; }
  {endifdef DEBUG}
{  Parser.Prepare; }
end;

procedure TXmlAnalyser.Execute;
begin
  LexItem := LexicalList;
  Parser.DoIt := True;
  Parser.Execute;
end;

constructor TXmlAnalyser.Create (aOwner: TOBject);
begin
  inherited Create;
  Scanner := TXmlScanner.Create;
  Scanner.OnNeedData := @ScannerNeedsData;
  Scanner.OnError := @AnalyserScannerError;
  Parser := TXmlParser.Create(aOwner);
  Parser.OnHaveData := @HaveData;
  Parser.OnError := @AnalyserParserError;
  LexicalList:= nil;
end;

destructor TXmlAnalyser.Destroy;
begin
  Parser.Free;
  Scanner.Free;
  ClearLexicalList;
  LexicalList.Free;
  inherited Destroy;
end;

function TXmlAnalyser.getBaseXml: TXml;
begin
  result  := Parser.BaseXml
end;

procedure TXmlAnalyser.setBaseXml(const Value: TXml);
begin
  Parser.BaseXml := Value;
end;

end.
