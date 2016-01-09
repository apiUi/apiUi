unit Bind;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses Classes
   , Graphics
   ;

type TBindExpandStyle = (esAll, esOne, esUsed);
type TxvViewType = (xvAll, xvRequired, xvUsed, xvReqUsed);
type PTDateTime = ^TDateTime;
type PBoolean = ^Boolean;
type PInteger = ^Integer;
type DFunction = function : TDateTime;
type DFunctionX = function (arg: Extended): TDateTime;
type DFunctionS = function (arg: String): TDateTime;
type SFunctionD = function (arg: TDateTime): String;
type SFunctionBSS = function (arg1: Boolean; arg2, arg3: String): String;
type SFunctionDS = function (arg1: TDateTime; arg2: String): String;
type SFunctionS = function (arg: String): String;
type SFunctionSS = function (arg1, arg2: String): String;
type SFunctionSSS = function (arg1, arg2, arg3: String): String;
type SFunctionSSSS = function (arg1, arg2, arg3, arg4: String): String;
type SFunctionSX = function (arg1: String; arg2: Extended): String;
type SFunctionSXX = function (arg1: String; arg2, arg3: Extended): String;
type SFunctionV = function : String;
type SFunctionX = function (arg1: Extended): String;
type XFunctionD = function (arg: TDateTime): Extended;
type VFunctionV = procedure;
type VFunctionD = procedure (arg: TDateTime);
type VFunctionG = procedure (arg1: TObject);
type VFunctionGG = procedure (arg1, arg2: TObject);
type VFunctionGGG = procedure (arg1, arg2, arg3: TObject);
type VFunctionGGGG = procedure (arg1, arg2, arg3, arg4: TObject);
type VFunctionS = procedure (arg: String);
type VFunctionSS = procedure (arg1, arg2: String);
type VFunctionSX = procedure (arg1: String; arg2: Extended);
type VFunctionSSX = procedure (arg1, arg2: String; arg3: Extended);
type VFunctionSSI = procedure (arg1, arg2: String; arg3: Integer);
type VFunctionSSS = procedure (arg1, arg2, arg3: String);
type VFunctionSSSS = procedure (arg1, arg2, arg3, arg4: String);
type VFunctionX = procedure (arg: Extended);
type XFunctionS = function (arg: String): Extended;
type XFunctionSX = function (arg1: String; arg2: Extended): Extended;
type XFunctionV = function: Extended;
type XFunctionX = function (arg: Extended): Extended;
type XFunctionXX = function (arg1, arg2: Extended): Extended;
type XFunctionG = function (arg1: TObject): Extended;
type XFunctionGG = function (arg1, arg2: TObject): Extended;
type XFunctionObject = function (arg1: TObject): Extended;
type VFunctionOV = procedure (obj: TObject);
type VFunctionOD = procedure (obj: TObject; arg: TDateTime);
type VFunctionOG = procedure (obj: TObject; arg1: TObject);
type VFunctionOGG = procedure (obj: TObject; arg1, arg2: TObject);
type VFunctionOGGG = procedure (obj: TObject; arg1, arg2, arg3: TObject);
type VFunctionOGGGG = procedure (obj: TObject; arg1, arg2, arg3, arg4: TObject);
type VFunctionOS = procedure (obj: TObject; arg: String);
type VFunctionOSS = procedure (obj: TObject; arg1, arg2: String);
type VFunctionOSX = procedure (obj: TObject; arg1: String; arg2: Extended);
type VFunctionOSSX = procedure (obj: TObject; arg1, arg2: String; arg3: Extended);
type VFunctionOSSI = procedure (obj: TObject; arg1, arg2: String; arg3: Integer);
type VFunctionOSSS = procedure (obj: TObject; arg1, arg2, arg3: String);
type VFunctionOSSSS = procedure (obj: TObject; arg1, arg2, arg3, arg4: String);
type VFunctionOX = procedure (obj: TObject; arg: Extended);
type SFunctionOV = function (obj: TObject): String;
type XFunctionOV = function (obj: TObject): Extended;
type XFunctionOS = function (obj: TObject; arg: String): Extended;
type XFunctionOX = function (obj: TObject; arg: Extended): Extended;
type XFunctionOXX = function (obj: TObject; arg1, arg2: Extended): Extended;
type SLFunctionOS = function (obj: TObject; arg1: String): TStringList;
type SLFunctionOSS = function (obj: TObject; arg1, arg2: String): TStringList;
type YYRType = record
  case Integer of
   1: (yyPointer : Pointer);
   2: (yyInteger : Integer);
   3: (yyExtended : Extended);
   4: (yyBoolean: Boolean);
   5: (yyDateTime: TDateTime);
   6: (yyDFunction : DFunction);
   7: (yyDFunctionX : DFunctionX);
   8: (yyDFunctionS : DFunctionS);
   9: (yySFunctionBSS : SFunctionBSS);
  10: (yySFunctionD : SFunctionD);
  11: (yySFunctionDS : SFunctionDS);
  12: (yySFunctionS : SFunctionS);
  13: (yySFunctionSS : SFunctionSS);
  14: (yySFunctionSSS : SFunctionSSS);
  15: (yySFunctionSSSS : SFunctionSSSS);
  16: (yySFunctionSX : SFunctionSX);
  17: (yySFunctionSXX : SFunctionSXX);
  18: (yySFunctionV : SFunctionV);
  19: (yySFunctionX : SFunctionX);
  20: (yyVFunctionV : VFunctionV);
  21: (yyVFunctionD : VFunctionD);
  22: (yyVFunctionS : VFunctionS);
  23: (yyVFunctionSS : VFunctionSS);
  24: (yyVFunctionSX : VFunctionSX);
  25: (yyVFunctionSSS : VFunctionSSS);
  26: (yyVFunctionSSX : VFunctionSSX);
  27: (yyVFunctionSSSS : VFunctionSSSS);
  28: (yyVFunctionX : VFunctionX);
  29: (yyXFunctionD : XFunctionD);
  30: (yyXFunctionS : XFunctionS);
  31: (yyXFunctionSX : XFunctionSX);
  32: (yyXFunctionV : XFunctionV);
  33: (yyXFunctionX : XFunctionX);
  34: (yyXFunctionXX : XFunctionXX);
  35: (yyXFunctionG : XFunctionG);
  36: (yyXFunctionGG : XFunctionGG);
  37: (yyVFunctionG : VFunctionG);
  38: (yyVFunctionGG : VFunctionGG);
  39: (yyVFunctionGGG : VFunctionGGG);
  40: (yyVFunctionGGGG : VFunctionGGGG);
  41: (yyXFunctionObject : XFunctionObject);
  42: (yyObject : TObject);
  43: (yyVFunctionOV : VFunctionOV);
  44: (yyVFunctionOD : VFunctionOD);
  45: (yyVFunctionOS : VFunctionOS);
  46: (yyVFunctionOSS : VFunctionOSS);
  47: (yyVFunctionOSX : VFunctionOSX);
  48: (yyVFunctionOSSS : VFunctionOSSS);
  49: (yyVFunctionOSSX : VFunctionOSSX);
  50: (yyVFunctionOSSSS : VFunctionOSSSS);
  51: (yyVFunctionOX : VFunctionOX);
  52: (yySFunctionOV : SFunctionOV);
  53: (yyXFunctionOV : XFunctionOV);
  54: (yyXFunctionOS : XFunctionOS);
  55: (yyXFunctionOX : XFunctionOX);
  56: (yyXFunctionOXX : XFunctionOXX);
  57: (yySLFunctionOS : SLFunctionOS);
  58: (yySLFunctionOSS : SLFunctionOSS);
end;

type TBindableList = class;

  { TCustomBindable }

  TCustomBindable = class (TObject)
  private
    fHasUnExpectedValue: Boolean;
    fhasRelevance: Boolean;
    function getIsExpression: Boolean;
    function getValueAsInteger: Integer;
    procedure setValueAsInteger(const aValue: Integer);
    function getChecked: Boolean;
    function getRoot: TCustomBindable;
    function getValueAsBoolean: Boolean;
    function getCheckedAllUp: Boolean;
    procedure setValueAsBoolean(const aValue: Boolean);
    procedure setHasUnexpectedValue(const Value: Boolean);
    function getIsEvaluation: Boolean;
    procedure sethasRelevance(const Value: Boolean);
public
  Name: String;
  Group: Boolean;
  Value: String;
  NsPrefix: String;
  Checker: String;
  DoExpectValue: Boolean;
  ExpectedValue: String;
  NullValue: Boolean;
  CorrelationValue: String;
  Parent: TCustomBindable;
  RefId: Integer;
    fChecked: Boolean;
    fPrevChecked: Boolean;
  isProcessed: Boolean;
  Tag: LongInt;
  function hasNoDuplicatesOn (aCaption: String; aOnlyWhenChecked: Boolean; var oBind, dBind: TCustomBindable): Boolean;
  procedure Reset; Virtual;
  procedure ResetExpectedValues; Virtual;
  procedure setChecked(const aValue: Boolean); Virtual;
  procedure SeparateNsPrefixes;
  procedure ExploreRelevancy;
  procedure Populate (aViewType: TxvViewType); Virtual;
  function FindUQ (aName: String): TCustomBindable; Virtual;
  function Children: TBindableList; Virtual;
  procedure Bind(aRoot: String; aExpress: TObject; aMaxOccurrences: Integer); Virtual;
  function IsValueValid (var aMessage: String): Boolean; Virtual;
  function GetFullIndexCaption: String; Virtual;
  function GetIndexCaption: String; Virtual;
  function GetCaption: String; Virtual;
  function GetFullCaption: String;
  function GetDateTimeData: TDateTime; Virtual;
  function GetStringData: String; Virtual;
  function GetExtendedData: Extended; Virtual;
  function GetIntegerData: Extended; Virtual;
  procedure PutDateTimeData (aDateTime: TDateTime); Virtual;
  procedure PutStringData (aString: String); Virtual;
  procedure PutExtendedData (aExtended: Extended); Virtual;
  procedure PutIntegerData (aExtended: Extended); Virtual;
  procedure PutGroupData (aObject: TObject); Virtual;
  function GetExpectedStringData: String; Virtual;
  procedure PutExpectedStringData (aString: String); Virtual;
  function MergeChecked: Boolean; Virtual;
  procedure Font (aFont: TFont); Virtual;
  function bgColor (aReadOnly: Boolean; aColumn: Integer): TColor; Virtual;
  function IsAncestorOf (aBindable: TCustomBindable): Boolean;
  function UpLineAsText: String; Virtual;
  constructor Create; Overload;
  property isEvaluation: Boolean read getIsEvaluation;
  property isExpression: Boolean read getIsExpression;
  property hasRelevance: Boolean read fhasRelevance write sethasRelevance;
  property Checked: Boolean read getChecked write setChecked;
  property IndexCaption: String read GetIndexCaption;
  property FullIndexCaption: String read GetFullIndexCaption;
  property FullCaption: string read getFullCaption;
  property Root: TCustomBindable read getRoot;
  property HasUnExpectedValue: Boolean read fHasUnExpectedValue write setHasUnexpectedValue;
  property ValueAsBoolean: Boolean read getValueAsBoolean write setValueAsBoolean;
  property ValueAsInteger: Integer read getValueAsInteger write setValueAsInteger;
  property CheckedAllUp: Boolean read getCheckedAllUp;
end;

TBindableList = class (TStringList)
  private
    procedure SetBindable(Index: integer; const Value: TCustomBindable);
    function getValueText: String;
protected
  function GetBindable (Index: integer): TCustomBindable;
public
  procedure ClearListOnly;
  property ValueText: String read getValueText;
  property Bindables [Index: integer]: TCustomBindable read GetBindable write SetBindable;
  procedure Clear; override;
end;

type TBind = class (TObject)
protected
  function ValueAsString: String;
public
  Token: Integer;
  BindsAnObject: Boolean;
  Id: String;
  yy: YYRType;
  Storage: Extended;
  Str: String;
  property AsString: String read ValueAsString;
  procedure Init;
end;

type TBindList = class (TStringList)
protected
  function GetBind (Index: integer): TBind;
public
  property Binds [Index: integer]: TBind read GetBind;
  procedure Clear; override;
end;

{$INCLUDE parser.def}
var
  bindNilStr: String;
  bindRefId: Integer;
const treeTagColumn = 0;
const treeValueColumn = 2;
const treeButtonColumn = 1;
const LF = #10;
const CRLF = #13#10;

implementation

uses
  SysUtils
;

function TBindList.GetBind (Index: integer): TBind;
begin
  result := TBind (Objects [index]);
end;

procedure TBindList.Clear;
begin
  inherited Clear;
end;

procedure TBind.Init;
begin
  case self.Token of
    DFLD: PTDateTime (yy.yyPointer)^ := 0;
    SFLD: PString (yy.yyPointer)^ := '';
    IFLD: PInteger (yy.yyPointer)^ := 0;
    XFLD: PExtended (yy.yyPointer)^ := 0;
  end;
end;

function TBind.ValueAsString: String;
begin
  case Token of
    DFLD: result := DateToStr (PTDateTime (yy.yyPointer)^);
    SFLD: result := PString (yy.yyPointer)^;
    IFLD: result := IntToStr (PInteger (yy.yyPointer)^);
    XFLD: result := FloatToStr (PExtended (yy.yyPointer)^);
  end;
end;

function TCustomBindable.getCheckedAllUp: Boolean;
begin
  if not Checked then
    result := False
  else
    if not Assigned (Parent) then
      result := True
    else
      result := Parent.getCheckedAllUp;
end;

function TCustomBindable.GetDateTimeData: TDateTime;
begin
  result := StrToDateTime (Value);
end;

function TCustomBindable.GetStringData: String;
begin
  result := Value;
end;

function TCustomBindable.GetExtendedData: Extended;
begin
  if (Value = '')
  or (NullValue) then
    result := 0
  else
    result := StrToFloat (Value);
end;

function TCustomBindable.GetIntegerData: Extended;
begin
  if (Value = '')
  or (NullValue) then
    result := 0
  else
    result := StrToFloat (Value);
end;

procedure TCustomBindable.PutDateTimeData (aDateTime: TDateTime);
begin
  Value := DateTimeToStr (aDateTime);
end;

procedure TCustomBindable.PutStringData (aString: String);
begin
  if Self = nil then Exit;
  Value := aString;
end;

procedure TCustomBindable.PutExtendedData (aExtended: Extended);
begin
{  Value := FloatToStr (aExtended); }
  Value := FloatToStrF (aExtended, ffGeneral, 18, 4);
end;

procedure TCustomBindable.PutGroupData (aObject: TObject);
begin
{  Value := FloatToStr (aExtended); }
  raise Exception.Create ('Should be overridden');
end;

procedure TCustomBindable.PutIntegerData (aExtended: Extended);
begin
  Value := IntToStr (Trunc (aExtended));
end;

{ TBindableList }

procedure TBindableList.Clear;
begin
  inherited;
end;

procedure TBindableList.ClearListOnly;
begin
  inherited Clear;
end;

function TBindableList.GetBindable(Index: integer): TCustomBindable;
begin
  result := TCustomBindable (Objects [index]);
end;

function TBindableList.getValueText: String;
var
  sl: TStringList;
  x: Integer;
begin
  sl := TStringList.Create;
  try
    for x := 0 to Count - 1 do
    begin
      if Assigned (Bindables[x]) then
      begin
        if Bindables [x].Checked then
          sl.Add (Bindables [x].Value)
        else
          sl.Add (bindNilStr);
      end
      else
        sl.Add('?');
    end;
    result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure TBindableList.SetBindable(Index: integer;
  const Value: TCustomBindable);
begin
  Objects [Index] := Value;
end;

function TCustomBindable.GetExpectedStringData: String;
begin
  result := ExpectedValue;
end;

procedure TCustomBindable.PutExpectedStringData(aString: String);
  procedure _setDoExpects (aBind: TCustomBindable);
  begin
    if aBind = nil then
      exit;
    aBind.DoExpectValue := True;
    _setDoExpects (aBind.Parent);
  end;
begin
  if Self = nil then Exit;
  ExpectedValue := aString;
  DoExpectValue := True;
  if aString <> bindNilStr then
    _setDoExpects (Self.Parent);
end;

procedure TCustomBindable.setHasUnexpectedValue(const Value: Boolean);
begin
  fHasUnExpectedValue := Value;
  if (Value)
  and (Assigned (Parent)) then
  begin
    Parent.HasUnExpectedValue := True;
  end;
end;

function TCustomBindable.GetFullIndexCaption: String;
begin
  result := '';
  if Self = nil then
    exit;
  if Parent = nil then
    result := GetIndexCaption
  else
    result := Parent.GetFullIndexCaption
            + '.'
            + GetCaption;
end;

function TCustomBindable.GetFullCaption: String;
begin
  result := '';
  if Self = nil then
    exit;
  if Parent = nil then
    result := GetCaption
  else
    result := Parent.GetFullCaption
            + '.'
            + GetCaption;
end;

procedure TCustomBindable.Bind(aRoot: String; aExpress: TObject; aMaxOccurrences: Integer);
begin
  raise Exception.Create ('Should be overridden');
end;

function TCustomBindable.GetCaption: String;
begin
  result := Name;
end;

function TCustomBindable.getValueAsBoolean: Boolean;
begin
  if (Value = '1') or (Value = 'true') then
  begin
    result := True;
    exit;
  end;
  if (Value = '0') or (Value = 'false') or (Value = '')  then
  begin
    result := False;
    exit;
  end;
  raise Exception.Create('Value is not a Boolean: ' + Value);
end;

procedure TCustomBindable.setValueAsBoolean(const aValue: Boolean);
begin
  if (aValue) then
    Value := 'true'
  else
    Value := 'false';
end;

function TCustomBindable.getRoot: TCustomBindable;
begin
  if Assigned (Parent) then
    result := Parent.Root
  else
    result := self;
end;

procedure TCustomBindable.setChecked(const aValue: Boolean);
begin
  if (aValue)
  and (Assigned (Parent)) then
    Parent.Checked := True;
  fPrevChecked := fChecked;
  fChecked := aValue;
end;

procedure TCustomBindable.SeparateNsPrefixes;
var
  x: Integer;
begin
  x := Pos(':', Name);
  if x > 0 then
  begin
    NsPrefix:= Copy(Name, 1, x - 1);
    Name := Copy (Name, x + 1, 300000);
  end;
  for x := 0 to Children.Count -1 do
    Children.Bindables[x].SeparateNsPrefixes;
end;

procedure TCustomBindable.sethasRelevance(const Value: Boolean);
begin
  fhasRelevance := Value;
  if Value
  and Assigned (Parent) then
    Parent.hasRelevance := Value;
end;

function TCustomBindable.getChecked: Boolean;
begin
  result := fChecked;
end;

function TCustomBindable.MergeChecked: Boolean;
begin
  result := Checked or fPrevChecked;
end;

function TCustomBindable.getValueAsInteger: Integer;
begin
  result := StrToIntDef (Value, 0);
end;

function TCustomBindable.hasNoDuplicatesOn(aCaption: String;
  aOnlyWhenChecked: Boolean; var oBind, dBind: TCustomBindable): Boolean;
var
  slist: TStringList;
  cBind: TCustomBindable;
  function _right (s: String): String;
  var
    p: Integer;
  begin
    p := pos('.', s);
    if p < 1 then
      result := ''
    else
      result := Copy (s, p + 1, Length(s));
  end;
  function _left (s: String): String;
  var
    p: Integer;
  begin
    p := pos('.', s);
    if p < 1 then
      result := s
    else
      result := Copy (s, 1, p - 1);
  end;
  procedure _scan (aBind: TCustomBindable; s: String);
  var
    x, f: Integer;
    sl, sr: String;
  begin
    if not Result then Exit;
    if aOnlyWhenChecked and not aBind.Checked then Exit;
    sr := _right(s);
    if sr = '' then
    begin
      if slist.Find(aBind.Value, f) then
      begin
        result := False;
        oBind := slist.Objects[f] as TCustomBindable;
        dBind := aBind;
        exit;
      end;
      slist.AddObject(aBind.Value, aBind);
      exit;
    end;
    sl := _left (sr);
    for x := 0 to aBind.Children.Count - 1 do
      if aBind.Children.Bindables[x].Name = sl then
        _scan(aBind.Children.Bindables[x], sr);
  end;
begin
  result := True;
  oBind := nil;
  dBind := nil;
  if self = nil then Exit;
  if _left (aCaption) <> Name then Exit;
  slist := TStringList.Create;
  slist.Sorted := True;
  try
    _scan (self, aCaption);
  finally
    slist.Clear;
    slist.Free;
  end;
end;

procedure TCustomBindable.setValueAsInteger(const aValue: Integer);
begin
  Value := IntToStr (aValue);
end;

function TCustomBindable.IsAncestorOf(aBindable: TCustomBindable): Boolean;
begin
  if not Assigned (Self) then
    result := False
  else
    if not Assigned (aBindable) then
      result := False
    else
      if aBindable.Parent = self then
        result := True
      else
        result := IsAncestorOf (aBindable.Parent);
end;

constructor TCustomBindable.Create;
begin
  inherited Create;
  RefId := bindRefId;
  Inc (bindRefId);
end;

procedure TCustomBindable.ExploreRelevancy;
var
  x: Integer;
begin
  hasRelevance := Checked
               or (Value <> '');
  for x := 0 to Children.Count - 1 do
    Children.Bindables[x].ExploreRelevancy;
end;

function TCustomBindable.Children: TBindableList;
begin
  raise Exception.Create (self.ClassName + ': Virtual Children called')
end;

function TCustomBindable.GetIndexCaption: String;
begin
  raise Exception.Create (self.ClassName + ': Virtual GetIndexCaption called')
end;

function TCustomBindable.IsValueValid(var aMessage: String): Boolean;
begin
  raise Exception.Create (self.ClassName + ': Virtual IsValueValid called')
end;

function TCustomBindable.FindUQ (aName: String): TCustomBindable;
begin
  raise Exception.Create (self.ClassName + ': Virtual FindUQ called')
end;

procedure TCustomBindable.Populate (aViewType: TxvViewType);
begin
  raise Exception.Create (self.ClassName + ': Virtual Populate called')
end;

procedure TCustomBindable.Reset;
  procedure _reset (a: TCustomBindable);
  var
    x: Integer;
  begin
    for x := 0 to a.Children.Count - 1 do
      _reset (a.Children.Bindables [x]);
    a.fChecked := False;
    a.fPrevChecked := False;
  end;
begin
  if Assigned (self) then
    _reset (self);
end;

procedure TCustomBindable.ResetExpectedValues;
  procedure _reset (a: TCustomBindable);
  var
    x: Integer;
  begin
    for x := 0 to a.Children.Count - 1 do
      _reset (a.Children.Bindables [x]);
    a.HasUnExpectedValue := False;
    a.DoExpectValue := False;
    a.ExpectedValue := '';
  end;
begin
  _reset (self);
end;

function TCustomBindable.UpLineAsText: String;
begin

end;

function TCustomBindable.bgColor (aReadOnly: Boolean; aColumn: Integer): TColor;
begin
  if aReadOnly
  or (Children.Count > 0) then
    result := clBtnFace
  else
  begin
    if self = nil then
      result := $CFFFFF
    else
      result := clWhite;
  end;
end;

procedure TCustomBindable.Font(aFont: TFont);
begin
  exit;
end;

function TCustomBindable.getIsEvaluation: Boolean;
begin
  result := (Checker <> '');
end;

function TCustomBindable.getIsExpression: Boolean;
begin
  result := (Copy (Value, 1, 2) = ':=');
end;

initialization
  bindNilStr := '&nil';
  bindRefId := 0;
end.
