{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

unit yamlTestUnit ;


interface

uses
  Classes , SysUtils , FileUtil , SynEdit , Forms , Controls , Graphics ,
  Dialogs , StdCtrls , VirtualTrees, YAMLSCANNER, YAMLPARSER, ParserClasses, CustScanner;
const InternalStackSize = 256;
const InitState = 2; {taken from Scanner.pas}

type

  vtColumnType = (vtToken, vtValue, vtName, vtOffset, vtLength, vtFB, vtText);
  { TForm1 }

  TForm1 = class(TForm )
    Button1 : TButton ;
    SynEdit : TSynEdit ;
    TreeView : TVirtualStringTree ;
    procedure Button1Click (Sender : TObject );
    procedure FormCreate (Sender : TObject );
    procedure FormDestroy (Sender : TObject );
    procedure FormShow (Sender : TObject );
    procedure TreeViewFocusChanged (Sender : TBaseVirtualTree ;
      Node : PVirtualNode ; Column : TColumnIndex );
    procedure TreeViewGetText (Sender : TBaseVirtualTree ;
      Node : PVirtualNode ; Column : TColumnIndex ; TextType : TVSTTextType ;
      var CellText : String );
  private
    LineNumber, Offset: Integer;
    StartState: Integer;
    Stack: array [0..InternalStackSize] of Integer;
    StackIndex: Integer;
    PrevLexItem: YYSType;
    LexicalList: YYSType;
    FOnError: TOnErrorEvent;
    FOnHaveData: TOnHaveDataEvent;
    procedure AnalyserScannerError (Sender: TObject; Data: String);
    procedure OnToken (Sender: TObject);
    procedure ScannerNeedsData ( Sender:TObject
                               ; var MoreData: Boolean
                               ; var Data: String
                               );
  public
    scanner: TyamlScanner;
  end;

var
  Form1 : TForm1 ;
  TokenNames: array [256+1.._NOTUSEDLASTONE] of String =
  ( '_COMMENT'
  , '_NAME'
  , '_NEWLINE'
  , '_WHITESPACE'
  , '_INDENT'
  , '_HYPHENINDENT'
  , '_VALUE'
  , '_PIPE'
  , '_LEFT_SQUARE_BRACKET'
  , '_LEFT_CURLY_BRACKET'
  , '_RIGHT_SQUARE_BRACKET'
  , '_RIGHT_CURLY_BRACKET'
  , '_COLON'
  , '_COMMA'
  , '_STRING'
  , '_NUMBER'
  , '_FALSE'
  , '_NULL'
  , '_TRUE'
  , '_IS'
  , '_IGNORE'
  , '_NOTUSEDLASTONE'
  );

implementation

{$R *.lfm}

{ TForm1 }
type
  PBindTreeRec = ^TBindTreeRec;

  TBindTreeRec = record
    lex: YYSType;

  end;

procedure TForm1 .FormShow (Sender : TObject );
begin
  TreeView.Clear;
  Scanner.OnToken := OnToken;
  StackIndex := 0;
  Scanner.Start (InitState);
  LineNumber := 0;
  Offset := 1;
  Scanner.Execute;
  TreeView.SetFocus;
  TreeView.FocusedNode := TreeView.GetFirst;
end;

procedure TForm1 .TreeViewFocusChanged (Sender : TBaseVirtualTree ;
  Node : PVirtualNode ; Column : TColumnIndex );
var
  Lex: YYSType;
  Data: PBindTreeRec;
begin
  Data := TreeView.GetNodeData(Node);
  Lex := Data.lex;
  SynEdit.SelStart:=lex.Offset;
  SynEdit.SelEnd:=lex.Offset + Length (lex.TokenString);
end;

procedure TForm1 .TreeViewGetText (Sender : TBaseVirtualTree ;
  Node : PVirtualNode ; Column : TColumnIndex ; TextType : TVSTTextType ;
  var CellText : String );
var
  Lex: YYSType;
  Data: PBindTreeRec;
begin
  CellText := '';
  Data := TreeView.GetNodeData(Node);
  if Assigned (Data) then
  begin
    Lex := Data.lex;
    case vtColumnType (Column) of
      vtToken: CellText := IntToStr (Lex.Token);
      vtValue: CellText := IntToStr (Lex.yy.yyInteger);
      vtName: CellText := TokenNames [Lex.Token];
      vtOffset: CellText := IntToStr(Lex.Offset);
      vtLength: CellText := IntToStr(Length (Lex.TokenString));
      vtFB: If Length (Lex.TokenString) > 0 then CellText := IntToStr(Ord (Lex.TokenString[1]));
      vtText: CellText := Lex.TokenString;
    end;
  end;
end;

procedure TForm1 .AnalyserScannerError (Sender : TObject ; Data : String );
begin
  ShowMessage ('Scanner: ' + Data);
end;

procedure TForm1 .OnToken (Sender : TObject );
var
  xScanner: TyamlScanner;
  Lexical: YYSType;
  CobolNumberString: String;
  EscapeChar: String;
  HexCode: Integer;  // hex character code (-1 on error)
  ChildNode: PVirtualNode;
  Data: PBindTreeRec;
begin
  xScanner := Sender as TyamlScanner;
  if (xScanner.Token = _WHITESPACE)
  or (    (xScanner.Token = _VALUE)
      and (Length (xScanner.TokenAsString) = 1)
      and (Ord (xScanner.TokenAsString[1]) = 13)
     )
  then
  begin
    Offset := Offset + Length (xScanner.TokenAsString);
    exit;
  end;
  Lexical := YYSType.Create;
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
  if (Lexical.Token = _INDENT)
  or (Lexical.Token = _HYPHENINDENT) then
    Lexical.yy.yyInteger := Length (Lexical.TokenString);
  if (Lexical.Token = _INDENT)
  and (PrevLexItem.Token = _HYPHENINDENT) then
    Lexical.yy.yyInteger := Lexical.yy.yyInteger + PrevLexItem.yy.yyInteger;
  Lexical.yyRead := Lexical.yy;
  PrevLexItem := Lexical;

  Offset := Offset + Length (Lexical.TokenString);
  ChildNode := TreeView.AddChild(nil);
  Data := TreeView.GetNodeData(ChildNode);
  Data.lex := Lexical;
end;

procedure TForm1 .ScannerNeedsData (Sender : TObject ; var MoreData : Boolean ;
  var Data : String );
begin
  MoreData := (LineNumber < SynEdit.Lines.Count);
  if MoreData then
    Data := SynEdit.Lines.Text;
  LineNumber := MaxInt;
end;

procedure TForm1 .FormCreate (Sender : TObject );
begin
  scanner := TyamlScanner.Create;
  scanner.OnNeedData := ScannerNeedsData;
  Scanner.OnError := AnalyserScannerError;
  TreeView.NodeDataSize := SizeOf(TBindTreeRec);
end;

procedure TForm1 .Button1Click (Sender : TObject );
begin
  FormShow(nil);
end;

procedure TForm1 .FormDestroy (Sender : TObject );
begin
  scanner.Free;
end;

end.

