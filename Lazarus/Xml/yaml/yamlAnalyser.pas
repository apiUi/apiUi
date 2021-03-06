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

uses SysUtils, StrUtils {$ifndef NoGUI}, Dialogs{$endif}, ShowMemo, Forms, RegExpr;

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
  sep, s: String;
begin
  try
    // from multi-line to one token...
    lx := LexicalList;
    while (Assigned(lx)) do
    begin
      if (lx.Token = _REPLCRANDSTRIP)
      or (lx.Token = _REPLCRANDKEEP)
      or (lx.Token = _REPLCRANDCLIP)
      or (lx.Token = _KEEPCRANDSTRIP)
      or (lx.Token = _KEEPCRANDKEEP)
      or (lx.Token = _KEEPCRANDCLIP)
      then
      begin
        lx_1 := lx.NextToken;
        sep := '';
        lx.yyStringRead := '';
        while Assigned (lx_1)
        and (lx_1.yy.yyInteger > lx.yy.yyInteger) do
        begin
          lx.yyStringRead := lx.yyStringRead + sep + lx_1.yyStringRead;
          if (lx.Token = _REPLCRANDSTRIP)
          or (lx.Token = _REPLCRANDKEEP)
          or (lx.Token = _REPLCRANDCLIP)
          then
            sep := ' '
          else
            sep := LineEnding;
          lx_1 := lx_1.NextToken;
        end;
        if (lx.Token = _REPLCRANDCLIP)
        or (lx.Token = _KEEPCRANDCLIP)
        then
        begin
          if Copy ( lx.yyStringRead
                  , Length(lx.yyStringRead) - Length(LineEnding)
                  , Length(LineEnding)
                  ) <> LineEnding
          then
            lx.yyStringRead := lx.yyStringRead + LineEnding;
        end;
        if (lx.Token = _REPLCRANDSTRIP)
        or (lx.Token = _KEEPCRANDSTRIP)
        then
        begin
          if Copy ( lx.yyStringRead
                  , Length(lx.yyStringRead) - Length(LineEnding)
                  , Length(LineEnding)
                  ) = LineEnding
          then
            lx.yyStringRead := Copy ( lx.yyStringRead
                                    , 1
                                    , Length(lx.yyStringRead) - Length(LineEnding)
                                    );
        end;
        lx.NextToken := lx_1;
        lx.Token := _VALUE;
      end;
      lx := lx.NextToken;
    end;
    // skip comments....
    lx := LexicalList;
    lx_1 := lx;
    while Assigned (lx) do
    begin
      while Assigned (lx)
      and (lx.Token <> _COMMENT) do
      begin
        lx_1 := lx;
        lx := lx.NextToken;
      end;
      if Assigned (lx) then
      begin
        while Assigned (lx)
        and (lx.Token = _COMMENT) do
          lx := lx.NextToken;
        lx_1.NextToken := lx;
        lx_1 := lx;
      end;
    end;
    // from continued on next line to one token...
    lx := LexicalList;
    while (Assigned(lx)) do
    begin
      if lx.Token = _VALUE then
      begin
        lx_1 := lx.NextToken;
        while (Assigned (lx_1))
        and (   (lx_1.Token = _LINECONT)
             or (lx_1.Token = _VALUE)
            ) do
        begin
          if lx_1.Token = _LINECONT then
            lx.yyStringRead := lx.yyStringRead + ' '
          else
            lx.yyStringRead := lx.yyStringRead + LineEnding + lx_1.yyStringRead;
          lx_1 := lx_1.NextToken;
        end;
        lx.NextToken := lx_1;
      end;
      lx := lx.NextToken;
    end;
  finally
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
        or (lx.yyStringRead[1] = '''') then
        begin
          s := TrimRight(lx.yyStringRead);
          if (Length(s) > 1)
          and (s [system.Length(s)] = s [1]) then
            lx.yyStringRead := Copy(s, 2, system.Length(s) - 2);
        end;
      end;
    end;
    if Assigned (lx) then
    begin
      if (lx_2.Token = _HYPHENINDENT)
      and (lx_1.Token = _HYPHENINDENT)
      and (lx.Token = _VALUE) then
      begin
        lx_2.Token := _ARRAYVALUE;
        lx_2.yyStringRead := lx_1.yyStringRead + lx.yyStringRead;
        lx_1.Token := _COMMENT;
        lx_2.NextToken := lx.NextToken;
      end;
      if (lx.Token = _VALUE)
      and (lx_1.Token = _HYPHENINDENT)
      then
      begin
        lx_1.Token := _ARRAYVALUE;
        lx_1.yyStringRead := lx.yyStringRead;
        lx_1.NextToken := lx.NextToken;
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
  {$ifndef NoGUI}
  ShowMessage ('Scanner: ' + Data);
  {$else}
  raise Exception.CreateFmt('Scanner: %s', [Data]);
  {$endif}
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
  begin
    FOnNeedData (Self, MoreData, Data);
  end
  else
    MoreData := False;
end;

procedure TyamlAnalyser.OnToken (Sender: TObject);
var
  xScanner: TyamlScanner;
  Lexical: YYSType;
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
  if (xScanner.Token = _NEWLINE) then
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
    or (xScanner.Token = _INDENT)
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
  Lexical.LineNumber := xScanner.LineNumber;
  Lexical.Offset := Offset;
  Lexical.ColumnNumber := xScanner.ColumnNumber;
  Lexical.Token := xScanner.Token;
  Lexical.TokenString := xScanner.TokenAsString;
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
