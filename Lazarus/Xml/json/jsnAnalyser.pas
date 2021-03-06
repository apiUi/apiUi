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
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

unit jsnAnalyser;

interface
uses Classes
   , ParserClasses
   , igGlobals
   , jsnParser
   , jsnScanner
   , CustScanner
   , Xmlz
   ;

const InternalStackSize = 256;
const InitState = 2; {taken from Scanner.pas}

type TjsnAnalyser = class (TComponent)
private
  Stack: array [0..InternalStackSize] of Integer;
  StackIndex: Integer;
  LexItem: YYSType;
  LexicalList: YYSType;
  Scanner: TjsnScanner;
  Parser: TjsnParser;
  FOnError: TOnErrorEvent;
  FOnHaveData: TOnHaveDataEvent;
  FOnNeedData: TOnNeedDataEvent;
  ValueString: String;
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
  Xml: TXml;
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

uses SysUtils, StrUtils, Dialogs, ShowMemo, Forms;

procedure TjsnAnalyser.DebugTokenStringList (arg: TStringList);
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

procedure TjsnAnalyser.ShowTokens;
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

procedure TjsnAnalyser.PushInteger (arg: Integer);
begin
  if StackIndex > InternalStackSize then
    raise Exception.Create ('Internal stack overflow')
  else
  begin
    Stack [StackIndex] := arg;
    Inc (StackIndex);
  end;
end;

procedure TjsnAnalyser.SetOnHaveScanned (aEvent: TOnHaveScannedEvent);
begin
  Parser.OnHaveScanned := aEvent;
end;

function TjsnAnalyser.GetOnHaveScanned: TOnHaveScannedEvent;
begin
  result := Parser.OnHaveScanned;
end;

procedure TjsnAnalyser.HaveData (aObject: TObject; aString: String);
begin
  if Assigned (FOnHaveData) then
    FOnHaveData (Self, aString)
  else
    raise Exception.Create ('No OnHaveData proc assigned');
end;

function TjsnAnalyser.PopInteger: Integer;
begin
  if StackIndex <= 0 then
    Raise Exception.Create ('Internal stack underflow')
  else
  begin
    Dec (StackIndex);
    result := Stack [StackIndex];
  end;
end;

procedure TjsnAnalyser.AnalyserScannerError (Sender: TObject; Data: String);
begin
    ShowMessage ('Scanner: ' + Data);
end;

procedure TjsnAnalyser.AnalyserParserError
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

procedure TjsnAnalyser.ClearLexicalList;
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

function TjsnAnalyser.TokenToFloat (arg: String): Extended;
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

procedure TjsnAnalyser.ScannerNeedsData ( Sender:TObject
                                    ; var MoreData: Boolean
                                    ; var Data: String
                                    );
begin
  if Assigned (FOnNeedData) then
    FOnNeedData (Self, MoreData, Data)
  else
    MoreData := False;
end;

procedure TjsnAnalyser.OnToken (Sender: TObject);
var
  xScanner: TjsnScanner;
  Lexical: YYSType;
  CobolNumberString: String;
  EscapeChar: String;
  HexCode: Integer;  // hex character code (-1 on error)
begin
  xScanner := Sender as TjsnScanner;
  if (xScanner.Token = _IGNORE) then
    exit;
{
  if (Scanner.Token = _VALUE) then
  begin
    ValueString := ValueString + Scanner.TokenAsString;
    exit;
  end;
  if (Scanner.Token = _ESCAPECHAR) then
  begin
    EscapeChar := uppercase (Scanner.TokenAsString);
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
}
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
{
  case Lexical.Token of
    _TAG: ValueString := '';
    _ENDTAG: ValueString := '';
  end;
}
  Lexical.yyRead := Lexical.yy;
end;

procedure TjsnAnalyser.Prepare;
begin
  ClearLexicalList;
  ValueString := '';
  Scanner.OnToken := OnToken;
  StackIndex := 0;
  Scanner.Start (StartState);
  Scanner.Execute;
  Parser.LexItems := LexicalList;
  Parser.Xml := Xml;
  {ifdef DEBUG}
{  ShowTokens; }
  {endifdef DEBUG}
{  Parser.Prepare; }
end;

procedure TjsnAnalyser.Execute;
begin
  LexItem := LexicalList;
  Parser.DoIt := True;
  Parser.Execute;
end;

constructor TjsnAnalyser.Create (AComponent: TComponent);
begin
  inherited Create (AComponent);
  Scanner := TjsnScanner.Create;
  Scanner.OnNeedData := ScannerNeedsData;
  Scanner.OnError := AnalyserScannerError;
  Parser := TjsnParser.Create (AComponent);
  Parser.OnHaveData := HaveData;
  Parser.OnError := AnalyserParserError;
  LexicalList:= nil;
end;

destructor TjsnAnalyser.Destroy;
begin
  Parser.Free;
  Scanner.Free;
  ClearLexicalList;
  LexicalList.Free;
  inherited Destroy;
end;

end.
