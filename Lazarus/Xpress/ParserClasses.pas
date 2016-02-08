
unit ParserClasses;

interface
uses
  Classes, SysUtils, StrUtils, Bind
;

type TScanned = ( scMessageNumber
                , scCalledServer
                , scCalledModule
                , scSqlInsert
                , scSqlUpdate
                , scSqlDelete
                , scSqlSelect
                , scServiceName007
                , scServiceName008
                , scCopyMember
                , scCopyStatement
                , scCopyMemberToScan
                , scBaseView
                , scViewColBase
                , scViewColName
                , psServer
                , scStringList
                , psParam
                );

type TParserClass = class (TObject)
public
  procedure Init; Virtual;
end;

type TParserClassList = class (TStringList)
  private
    function GetObject(Index: integer): TParserClass;
    procedure SetObject(Index: integer; const Value: TParserClass);
  public
    property Objects [Index: integer]: TParserClass read GetObject write SetObject;
end;

type TBlock = class (TParserClass)
public
  Binds: TBindList;
  Parent: TBlock;
  function BindedByName(aName: String): TBind;
  procedure InitBinds;
  constructor Create;
  destructor Destroy; override;
end;

type TFed = class (TParserClass)
private
  fCursor: Integer;
  fIsOpen: Boolean;
    fTokenString: String;
  function getEof: Boolean;
  function getData: TCustomBindable;
    procedure SetTokenString(const Value: String);
public
  Name, Alias: String;
  FirstBind: TCustomBindable;
  List: TStringList;
  Parent, Anchor: TFed;
  isSubElement: Boolean;
  isCursor, isDynamic: Boolean;
  property TokenString: String read fTokenString write SetTokenString;
  property isOpen: Boolean read fIsOpen;
  property Eof: Boolean read getEof;
  property Data: TCustomBindable read getData;
  property Cursor: Integer read fCursor;
  function BindableByName (aName: String): TCustomBindable;
  function Ancestor (aAlias: String): TFed;
  function FindAnchor (aAlias: String): TFed;
  function PrepareBindableOnAliasField (aName: String): TCustomBindable;
  function FindBindableOnAliasField (aName: String): TCustomBindable;
  procedure New;
  procedure Open;
  procedure Close;
  procedure First;
  procedure Next;
  procedure Init; Override;
  constructor Create;
  destructor Destroy; override;
end;

type YYSType = class
public
  Next, Prev: YYSType; {links all yystype's }
  NextToken: YYSType; {links yystype's to execute }
  LineNumber: Integer; {linenumber of token}
  ColumnNumber: Integer; {columnnumber of token}
  Offset: Integer;
  Block: TBlock;
  Token: Integer; {token}
  TokenString: String; {the string as read}
  yyStringRead: String; {the value of a stringtoken (without quotes)}
  yyString: String; {gets value from yyStringRead}
  yyRead: YYRType; {all token values except strngs}
  yy: YYRType; {gets its value from yyRead}
  Tag: Integer; { feel free to use it for whatever }
  function isLocalDeclaration: Boolean;
  function DeclarationToken: Integer;
  function ValueAsString: String;
end;

type TOnHaveDataEvent = procedure ( Sender: TObject
                                  ; aString: String
                                  ) of Object;
type TOnEvaluateStringEvent = procedure ( Sender:TObject
                                        ; StringToEvaluate: String
                                        ) of Object;
type TOnPutDataEvent = procedure ( Sender: TObject
                                 ; Bind: YYSType
                                 ; Data: YYSType
                                 ) of Object;
type TOnGetDataEvent = procedure ( Sender: TObject
                                 ; Bind: YYSType
                                 ; Data: YYSType
                                 ) of Object;
type EParserException = class (Exception);
{
type TOnErrorEvent = procedure ( Sender:TObject
                               ; Data: String
                               ) of Object;
}
type TOnErrorEvent = procedure ( Sender:TObject
                               ; LineNumber: Integer
                               ; ColumnNumber: Integer
                               ; Offset: Integer
                               ; TokenString: String
                               ; Data: String
                               ) of Object;
type TOnStoreObject = procedure ( Sender:TObject
                                ; aObject: TObject
                                ) of Object;
type TOnCreateObject = procedure ( Sender:TObject
                                 ; var Query: TObject
                                 ) of Object;
type TOnHaveScannedEvent = procedure ( Sender: TObject
                                     ; aScanned: TScanned
                                     ) of Object;
type TOnHaveSqlToken = procedure ( Sender:TObject
                                   ; Query: TObject
                                   ; Str: String
                                   ) of Object;
type TOnHaveSqlBind = procedure ( Sender:TObject
                                   ; Query: TObject
                                   ; Bind: TBind
                                   ) of Object;
type TOnHaveSqlParam = procedure ( Sender:TObject
                                   ; Query: TObject
                                   ; Param: TBind
                                   ) of Object;
type TOnNeedSqlExec = procedure ( Sender:TObject
                                ; Query: TObject
                                ) of Object;
type TOnNeedSqlOpen = procedure ( Sender:TObject
                                ; Query: TObject
                                ) of Object;
type TOnNeedSqlClose = procedure ( Sender:TObject
                                 ; Query: TObject
                                 ) of Object;
type TOnNeedSqlNextRow = procedure ( Sender:TObject
                                   ; Query: TObject
                                   ; var MoreData: Boolean
                                   ) of Object;
type TOnNeedSqlFirstRow = procedure ( Sender:TObject
                                    ; Query: TObject
                                    ; var MoreData: Boolean
                                    ) of Object;
type TOnFinishInsertQuery = procedure ( Sender:TObject
                                      ; Query: TObject
                                      ) of Object;

type TBooleanFunction = function: Boolean of Object;



var
  _OnParseErrorEvent: TOnErrorEvent;
  _ParseFileName: String;

implementation

uses Xsdz
   , Xmlz
   ;


function TFed.Ancestor(aAlias: String): TFed;
  function _Ancestor(aFed: TFed): TFed;
  begin
    result := aFed;
    if Assigned (aFed)
    and (aFed.Alias <> aAlias) then
      result := _Ancestor (aFed.Parent);
  end;
begin
  result := _Ancestor (Self);
end;

function TFed.PrepareBindableOnAliasField(aName: String): TCustomBindable;
var
  p: Integer;
  xFed: TFed;
  r: String;
begin
  result := nil;
  if AnsiRightStr (aName, 1) = '*' then
    aName := Copy (aName, 1, Length (aName) - 1);
  xFed := Self;
  p := Pos('.', aName);
  r := Copy(aName, p, 30000);
  if p > 1 then
    xFed := Ancestor(Copy(aName, 1, p - 1));
  if Assigned (xFed) then
    if r = '.' then
      result := xFed.FirstBind
    else
      if Assigned (xFed.FirstBind) then
        with xFed.FirstBind do
          result := FindUQ(Name + r);
end;

procedure TFed.SetTokenString(const Value: String);
var
  p: Integer;
begin
  fTokenString := Value;
  Name := Copy (Value, Pos ('.', Value), 30000);
end;

function TFed.FindAnchor(aAlias: String): TFed;
var
  p: Integer;
begin
  p := Pos ('.', aAlias);
  if p = 1 then
    result := Parent
  else
    result := Parent.Ancestor(Copy (aAlias, 1, p - 1));
end;

function TFed.FindBindableOnAliasField (aName: String): TCustomBindable;
var
  p: Integer;
  xFed: TFed;
  r: String;
begin
  result := nil;
  if AnsiRightStr (aName, 1) = '*' then
    aName := Copy (aName, 1, Length (aName) - 1);
  xFed := Self;
  p := Pos('.', aName);
  r := Copy(aName, p, 30000);
  if p > 1 then
    xFed := Ancestor(Copy(aName, 1, p - 1));
  if Assigned (xFed) then
    if r = '.' then
      result := xFed.Data
    else
      with xFed.Data do
        result := FindUQ(Name + r);
end;

function TFed.BindableByName(aName: String): TCustomBindable;
var
  x: Integer;
begin
  with (Data as TXml) do
    result := FindUQ(Name + aName);
  if not Assigned (result) then
    raise Exception.CreateFmt('Error: %s not found in %s', [aName, Data.FullIndexCaption]);
end;

procedure TFed.Close;
begin
  List.Clear;
  fIsOpen := False;
end;

constructor TFed.Create;
begin
  inherited;
  List := TStringList.Create;
end;

destructor TFed.Destroy;
begin
  List.Free;
  inherited;
end;

procedure TFed.First;
begin
  fCursor := 0;
end;

function TFed.getData: TCustomBindable;
begin
  if isCursor then
  begin
    if Eof then raise Exception.CreateFmt('TFed.GetData at EOF (%s)', [Name]);
    result := List.Objects[fCursor] as TCustomBindable;
  end
  else
    result := FirstBind;
end;

function TFed.getEof: Boolean;
begin
  result := not (fCursor < List.Count);
end;

procedure TFed.Init;
begin
  Close;
end;

procedure TFed.New;
var
  x, y: Integer;
  xXml, cXml: TXml;
begin
  xXml := nil;
  y := -1;
  cXml := FirstBind.Parent as TXml;
  begin
    for x := 0 to cXml.Items.Count - 1 do
    begin
      if cXml.Items.XmlItems[x].Name = FirstBind.Name then
      begin
        y := x;
        if not cXml.Items.XmlItems[x].Checked then
        begin
          cXml.Items.XmlItems[x].Checked := True;
          FirstBind := cXml.Items.XmlItems[x];
          Exit;
        end;
      end;
    end;
    xXml := TXml.Create(0, (FirstBind as TXml).Xsd);
    cXml.InsertXml(y+1, xXml);
    xXml.Parent := cXml;
    xXml.Checked := True;
    FirstBind := xXml;
  end;
end;

procedure TFed.Next;
begin
  Inc (fCursor);
end;

procedure TFed.Open;
  function _split (s: String): TStringList;
  var
    x: Integer;
    ss: String;
  begin
    result := TStringList.Create;
    x := 1;
    ss := '';
    while x <= Length (s) do
    begin
      if s[x] = '.' then
      begin
        result.Add(ss);
        ss := '';
      end
      else
        ss := ss + s[x];
      Inc (x);
    end;
    result.Add(ss)
  end;
  procedure _CreateList (rXml: TXml; aList, Sl: TStringList; i: Integer);
  var
    x: Integer;
  begin
    if rXml.Checked
    and (rXml.Name = Sl.Strings[i]) then
    begin
      if i < (Sl.Count - 1) then
      begin
        for x := 0 to rXml.Items.Count - 1 do
          _CreateList (rXml.Items.XmlItems[x], aList, Sl, i + 1);
      end
      else
        aList.AddObject('', rXml);
    end;
  end;
var
  x: Integer;
  pXml: TXml;
  s: String;
  sl: TStringList;
begin
  List.Clear;
  if isSubElement then
  begin
    if not Assigned (Anchor) then raise Exception.CreateFmt('TFed.Open: No anchor for %s', [Name]);
    pXml := Anchor.Data as TXml;
    s := pXml.Name + Name;
  end
  else
  begin
    if not Assigned (FirstBind) then raise Exception.Create('TFed.Open: No FirstBind assigned');
    if not (FirstBind is TXml) then raise Exception.Create('TFed.Open: Only for XML');
    if not Assigned (FirstBind.Parent) then raise Exception.Create('TFed.Open: Not for root XMLs');
    s := (FirstBind as TXml).FullCaption;
    pXml := (FirstBind as TXml).Root;
  end;
  sl := _Split (s);
  try
    _CreateList (pXml, List, sl, 0);
  finally
    sl.Free;
  end;
  fIsOpen := True;
end;

{ TBlock }

function TBlock.BindedByName(aName: String): TBind;
var
  f: Integer;
begin
  result := nil;
  if not Assigned (Self) then Exit;
  if Binds.Find (aName, f) then
    result := Binds.Binds[f]
  else
    result := Parent.BindedByName(aName);
end;

constructor TBlock.Create;
begin
  Binds := TBindList.Create;
  Binds.Sorted := True;
  Binds.Duplicates := DupError;
end;

destructor TBlock.Destroy;
var
  x: Integer;
begin
  for x := 0 to Binds.Count - 1 do with Binds.Binds[x] do
  begin
    case Token of
      VOIDFUNCTION_: yy.yyObject.Free;
    end;
    Free;
  end;
  Binds.Clear;
  Binds.Free;
  inherited;
end;

procedure TBlock.InitBinds;
var
  x: Integer;
begin
  for x := 0 to Binds.Count - 1 do with Binds.Binds[x] do
    Init;
end;

{ YYSType }

function YYSType.DeclarationToken: Integer;
begin
  result := Token;
  if isLocalDeclaration then
    case prev.Token of
      _DATETIME: result := DFLD;
      _FLOAT: result := XFLD;
      _INTEGER: result := IFLD;
      _STRING: result := SFLD;
      _POINTER: result := PFLD;
    end;

end;

function YYSType.isLocalDeclaration: Boolean;
begin
  result := Assigned (Prev)
        and (   (Prev.Token = _DATETIME)
             or (Prev.Token = _FLOAT)
             or (Prev.Token = _INTEGER)
             or (Prev.Token = _STRING)
             or (Prev.Token = _POINTER)
            );
end;

function YYSType.ValueAsString: String;
  function dateTimeAsXsd(aDT: TDateTime): String;
  var
    year, month, day, hour, min, sec, msec: Word;
  begin
    DecodeDate(aDT,year,month,day);
    DecodeTime(aDT,hour,min,sec,msec);
    result := Format('%.*d-%.*d-%.*dT%.*d:%.*d:%.*d.%.*d'
                  , [4, year, 2, month, 2, day, 2, hour, 2, min, 2, sec, 3, msec]);
  end;

var
  Bind: TBind;
  Bindable: TCustomBindable;
begin
  Bind := TBind (yy.yyObject);
  if not Bind.BindsAnObject then
  begin
    case Bind.Token of
      BFLD: result := ifThen (PBoolean (Bind.yy.yyPointer)^, 'true', 'false');
      DFLD: result := dateTimeAsXsd (PTDateTime (Bind.yy.yyPointer)^);
      SFLD: result := PString (Bind.yy.yyPointer)^;
      IFLD: IntToStr (PInteger (Bind.yy.yyPointer)^);
      XFLD: FloatToStr (PExtended (Bind.yy.yyPointer)^);
    end;
  end
  else
  begin
    Bindable := TCustomBindable (Bind.yy.yyPointer);
    result := Bindable.Value;
  end;
end;

{ TParserClass }

procedure TParserClass.Init;
begin

end;

{ TParserClassList }

function TParserClassList.GetObject(Index: integer): TParserClass;
begin
  result := inherited Objects [Index] as TParserClass;
end;

procedure TParserClassList.SetObject(Index: integer; const Value: TParserClass);
begin
  (self as TStringList).Objects [Index] := Value;
end;

initialization
  _OnParseErrorEvent := nil;
  _ParseFileName := '';

end.
