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
unit Bind;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses Classes
{$ifndef NoGUI}
   , Graphics
{$endif}
   , xmlio
   ;

resourcestring
  S_XML_REGEXP_LINK = '(?i)((FTP|HTTPS?|FILE|DOC)://([_a-z\d\-\/]+(\.[_a-z\d\-\/]+)+)(((:\d+)?/[_a-z\d\-\+\\\./]+)+)*([\?#][a-z0-9=%&_/\+\-\.]+)?)';

type
  TjsonType = (jsonNone, jsonString, jsonNumber, jsonBoolean, jsonObject,
    jsonArray, jsonArrayValue);
const JsonTypeNames: array [jsonNone..jsonArrayValue] of String =
( 'jsonNone'
, 'jsonString'
, 'jsonNumber'
, 'jsonBoolean'
, 'jsonObject'
, 'jsonArray'
, 'jsonArrayValue'
);

type TBindExpandStyle = (esAll, esOne, esUsed, edBestEffort);
const TBindExpandStyleStr: array [TBindExpandStyle] of String = ('All', 'One', 'Used', 'BestEffort');
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
type VFunctionOI = procedure (obj: TObject; aIndex: Integer);
type VFunctionOV = procedure (obj: TObject);
type VFunctionOD = procedure (obj: TObject; arg: TDateTime);
type VFunctionOG = procedure (obj: TObject; arg1: TObject);
type VFunctionOGS = procedure (obj: TObject; arg1: TObject; arg2: String);
type VFunctionOGG = procedure (obj: TObject; arg1, arg2: TObject);
type VFunctionOGGG = procedure (obj: TObject; arg1, arg2, arg3: TObject);
type VFunctionOGGGG = procedure (obj: TObject; arg1, arg2, arg3, arg4: TObject);
type VFunctionOS = procedure (obj: TObject; arg: String);
type VFunctionOSB = procedure (obj: TObject; arg1: String; arg2: Boolean);
type VFunctionOSS = procedure (obj: TObject; arg1, arg2: String);
type VFunctionOSX = procedure (obj: TObject; arg1: String; arg2: Extended);
type VFunctionOSSX = procedure (obj: TObject; arg1, arg2: String; arg3: Extended);
type VFunctionOSSI = procedure (obj: TObject; arg1, arg2: String; arg3: Integer);
type VFunctionOSSS = procedure (obj: TObject; arg1, arg2, arg3: String);
type VFunctionOSSSB = procedure (obj: TObject; arg1, arg2, arg3: String; arg4: Boolean);
type VFunctionOSSSS = procedure (obj: TObject; arg1, arg2, arg3, arg4: String);
type VFunctionOB = procedure (obj: TObject; arg: Boolean);
type VFunctionOX = procedure (obj: TObject; arg: Extended);
type SFunctionOG = function (obj: TObject; arg1: TObject): String;
type SFunctionOV = function (obj: TObject): String;
type XFunctionOV = function (obj: TObject): Extended;
type XFunctionOS = function (obj: TObject; arg: String): Extended;
type XFunctionOSX = function (obj: TObject; arg1: String; arg2: Extended): Extended;
type XFunctionOX = function (obj: TObject; arg: Extended): Extended;
type XFunctionOXX = function (obj: TObject; arg1, arg2: Extended): Extended;
type SFunctionOS = function (obj: TObject; arg: String): String;
type SFunctionOSB = function (obj: TObject; arg1: String; arg2: Boolean): String;
type SFunctionOSS = function (obj: TObject; arg1, arg2: String): String;
type SFunctionOSX = function (obj: TObject; arg1: String; arg2: Extended): String;
type SFunctionOSSX = function (obj: TObject; arg1, arg2: String; arg3: Extended): String;
type SFunctionOSSSX = function (obj: TObject; arg1, arg2, arg3: String; arg4: Extended): String;
type SFunctionOSSI = function (obj: TObject; arg1, arg2: String; arg3: Integer): String;
type SFunctionOSSS = function (obj: TObject; arg1, arg2, arg3: String): String;
type SFunctionOSSSB = function (obj: TObject; arg1, arg2, arg3: String; arg4: Boolean): String;
type SFunctionOSSSS = function (obj: TObject; arg1, arg2, arg3, arg4: String): String;

{ TParserStringList }

 TParserStringList = class (TJBStringList)
public
  aObject: TObject;
  aIndexProcedure: VFunctionOI;
  aProcedure: VFunctionG;
  procedure ProcessIndex (aIndex: Integer);
end;
type SLFunctionOS = function (obj: TObject; arg1: String): TParserStringList;
type SLFunctionOSS = function (obj: TObject; arg1, arg2: String): TParserStringList;
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
  21: (yyVFunctionOGS : VFunctionOGS);
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
  46: (yyVFunctionOSB : VFunctionOSB);
  47: (yyVFunctionOSS : VFunctionOSS);
  48: (yyVFunctionOSX : VFunctionOSX);
  49: (yyVFunctionOSSS : VFunctionOSSS);
  50: (yyVFunctionOSSSB : VFunctionOSSSB);
  51: (yyVFunctionOSSX : VFunctionOSSX);
  52: (yyVFunctionOSSSS : VFunctionOSSSS);
  53: (yyVFunctionOB : VFunctionOB);
  54: (yyVFunctionOX : VFunctionOX);
  55: (yySFunctionOG : SFunctionOG);
  56: (yySFunctionOV : SFunctionOV);
  57: (yyXFunctionOV : XFunctionOV);
  58: (yyXFunctionOS : XFunctionOS);
  59: (yyXFunctionOX : XFunctionOX);
  60: (yyXFunctionOXX : XFunctionOXX);
  61: (yySLFunctionOS : SLFunctionOS);
  62: (yySLFunctionOSS : SLFunctionOSS);
  63: (yySFunctionOS : SFunctionOS);
  64: (yySFunctionOSB : SFunctionOSB);
  65: (yySFunctionOSS : SFunctionOSS);
  66: (yySFunctionOSX : SFunctionOSX);
  67: (yySFunctionOSSX : SFunctionOSSX);
  68: (yySFunctionOSSSX : SFunctionOSSSX);
  69: (yySFunctionOSSI : SFunctionOSSI);
  70: (yySFunctionOSSS : SFunctionOSSS);
  71: (yySFunctionOSSSB : SFunctionOSSSB);
  72: (yySFunctionOSSSS : SFunctionOSSSS);
  73: (yyXFunctionOSX : XFunctionOSX);


end;

type TBindableList = class;

  { TCustomBindable }

  TCustomBindable = class (TObject)
  private
    fhasRelevance: Boolean;
    fIsProcessed: Boolean;
    fisValidated: Boolean;
    fhasValidationMessage: Boolean;
    fValidationMesssage: String;
    function getIsExpression: Boolean;
    function getTotalNumberOfSubElements: Integer;
    function getValueAsInteger: Integer;
    function getYamlValue: String;
    procedure sethasValidationMessage(aValue: Boolean);
    procedure setIsProcessed(AValue: Boolean);
    procedure setValidationMesssage(AValue: String);
    procedure setValueAsInteger(const aValue: Integer);
    function getChecked: Boolean;
    function getRoot: TCustomBindable;
    function GetOccurrence: Integer;
    function getValueAsBoolean: Boolean;
    function getCheckedAllUp: Boolean;
    procedure setValueAsBoolean(const aValue: Boolean);
    function getIsEvaluation: Boolean;
    procedure sethasRelevance(const Value: Boolean);
    procedure setValueAsTimeStamp (AValue : TDateTime );
public
  Name: String;
  Group: Boolean;
  Value: String;
  NsPrefix: String;
  Checker: String;
  NullValue: Boolean;
  CorrelationValue: String;
  Parent: TCustomBindable;
  RefId: Integer;
  LoadIndex: Integer;
    fChecked: Boolean;
    fPrevChecked: Boolean;
  SourceFileName: String;
  Tag: PtrInt;
  jsonType: TjsonType;
  procedure PopulateSourceFileName (aName: String);
  function thisBind: TCustomBindable;
  function isValueLink: Boolean;
  function IsRequired: Boolean; Virtual;
  function hasNoDuplicatesOn (aCaption: String; aOnlyWhenChecked: Boolean; var oBind, dBind: TCustomBindable): Boolean;
  procedure Reset; Virtual;
  procedure setChecked(const aValue: Boolean); Virtual;
  procedure ExploreRelevancy;
  procedure Populate (aViewType: TxvViewType); Virtual;
  function FindUQ (aName: String): TCustomBindable; Virtual;
  function Children: TBindableList; Virtual;
  procedure Bind(aRoot: String; aExpress: TObject; aMaxOccurrences: Integer); Virtual;
  function IsEditingAllowed: Boolean; Virtual;
  function IsValueValid: Boolean; Virtual;
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
  function MergeChecked: Boolean; Virtual;
  {$ifndef NoGUI}
  procedure Font (aFont: TFont); Virtual;
  function bgValueColor (aReadOnly: Boolean): TColor; Virtual;
  {$endif}
  function IsAncestorOf (aBindable: TCustomBindable): Boolean;
  function UplineAsList: TBindableList;
  function UpLineAsText: String; Virtual;
  function AllValidationsMessage: String;
  constructor Create; Overload;
  property Occurrence: Integer read GetOccurrence;
  property yamlValue: String read getYamlValue;
  property totalNumberOfSubElements: Integer read getTotalNumberOfSubElements;
  property isEvaluation: Boolean read getIsEvaluation;
  property isExpression: Boolean read getIsExpression;
  property hasRelevance: Boolean read fhasRelevance write sethasRelevance;
  property ValidationMesssage: String read fValidationMesssage write setValidationMesssage;
  property isValidated: Boolean read fisValidated;
  property hasValidationMessage: Boolean read fhasValidationMessage write sethasValidationMessage;
  property Checked: Boolean read getChecked write setChecked;
  property isProcessed: Boolean read fIsProcessed write setIsProcessed;
  property IndexCaption: String read GetIndexCaption;
  property FullIndexCaption: String read GetFullIndexCaption;
  property FullCaption: string read getFullCaption;
  property Root: TCustomBindable read getRoot;
  property ValueAsBoolean: Boolean read getValueAsBoolean write setValueAsBoolean;
  property ValueAsTimeStamp: TDateTime write setValueAsTimeStamp;
  property ValueAsInteger: Integer read getValueAsInteger write setValueAsInteger;
  property CheckedAllUp: Boolean read getCheckedAllUp;
end;

{ TBindableList }

TBindableList = class (TJBStringList)
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
  function Clone: TBindableList;
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

type TBindList = class (TJBStringList)
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
const CRLF = LineEnding;

implementation

uses
  SysUtils
, RegExpr
;

{ TParserStringList }

procedure TParserStringList.ProcessIndex (aIndex: Integer );
begin
  if Assigned (aIndexProcedure) then
    aIndexProcedure (aObject, aIndex);
end;

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
  if Value = bindNilStr then
  begin
    Value := '';
    fChecked := False;
  end;
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

function TBindableList.Clone: TBindableList;
var
  x: Integer;
begin
  result := TBindableList.Create;
  for x := 0 to Count - 1 do
    result.AddObject(Strings[x], Objects[x]);
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
  sl: TJBStringList;
  x: Integer;
begin
  sl := TJBStringList.Create;
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

function TCustomBindable.IsEditingAllowed: Boolean;
begin
  result := False; // avoid warning
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

function TCustomBindable.GetOccurrence: Integer;
var
  x: Integer;
begin
  result := 1;
  if Assigned (Parent) then
  begin
    result := 0;
    for x := 0 to Parent.Children.Count - 1 do
    begin
      if Parent.Children.Bindables[x].Name = self.Name then
        Inc (result);
      if Parent.Children.Bindables[x] = self then
        Exit;
    end;
  end;
end;

procedure TCustomBindable.setChecked(const aValue: Boolean);
begin
  if Assigned(Self) then
  begin
    if (aValue)
    and (Assigned (Parent)) then
      Parent.Checked := True;
    fPrevChecked := fChecked;
    fChecked := aValue;
  end;
end;

procedure TCustomBindable.sethasRelevance(const Value: Boolean);
begin
  fhasRelevance := Value;
  if Value
  and Assigned (Parent) then
    Parent.hasRelevance := Value;
end;

procedure TCustomBindable .setValueAsTimeStamp (AValue : TDateTime );
var
  year, month, day, hour, min, sec, msec: Word;
begin
  DecodeDate(AValue,year,month,day);
  DecodeTime(AValue,hour,min,sec,msec);
  Value := Format('%.*d-%.*d-%.*dT%.*d:%.*d:%.*d.%.*d'
                , [4, year, 2, month, 2, day, 2, hour, 2, min, 2, sec, 3, msec]);
end;

function TCustomBindable.UplineAsList: TBindableList;
  procedure _fill (aList: TBindableList; aBind: TCustomBindable);
  begin
    if Assigned (aBind.Parent) then
      _fill (aList, aBind.Parent);
    aList.AddObject(aBind.Name, aBind);
  end;
begin
  result := TBindableList.Create;
  _fill (result, Self);
end;

procedure TCustomBindable.PopulateSourceFileName(aName: String);
var
  x: Integer;
begin
  SourceFileName := aName;
  for x := 0 to Children.Count - 1 do
    Children.Bindables[x].PopulateSourceFileName(aName);
end;

function TCustomBindable.thisBind: TCustomBindable;
begin
  result := self;
end;

function TCustomBindable.isValueLink: Boolean;
var
  s: String;
begin
  result := False;
  exit;
  if self.Checked
  and (self.Children.Count = 0)
  and (Length (Value) > 5) then
  begin
    s := Copy(Value, 1, 6);
    if (Pos (':', s) > 0) then
    begin
      with TRegExpr.Create (S_XML_REGEXP_LINK) do
      try
        result := Exec (Value) and (MatchLen[0] = Length(Value));
      finally
        Free;
      end;
    end;
  end;
end;

function TCustomBindable.IsRequired: Boolean;
begin
  result := False;
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

function TCustomBindable.getYamlValue: String;
  function _needToDQ: Boolean;
  var
    x: Integer;
  begin
    result := False;
    for x := 1 to Length(Value) do
      if not Result then
        Result := (Pos(Value [x], '":-{}[]!#|>&%@') > 0);
  end;
  function _DQ: String;
  var
    x: Integer;
  begin
    result := '';
    for x := 1 to Length(Value) do
    begin
      if (Value [x] = '"')
      or (Value [x] = '\') then
        result := result + '\';
      Result := Result + Value [x];
    end;
  end;

begin
  if _needToDQ then
    result := '"' + _DQ + '"'
  else
    result := Value;
end;

procedure TCustomBindable.sethasValidationMessage(aValue: Boolean);
begin
  if fhasValidationMessage = aValue then Exit;
  fhasValidationMessage := aValue;
  if aValue
  and Assigned (Parent) then
    Parent.hasValidationMessage := aValue;
end;

procedure TCustomBindable.setIsProcessed(AValue: Boolean);
var
  x: Integer;
begin
  fIsProcessed := AValue;
  if AValue = False then
    for x := 0 to Children.Count - 1 do
      Children.Bindables[x].isProcessed := AValue;
end;

procedure TCustomBindable.setValidationMesssage(AValue: String);
begin
  fIsValidated := True;
  if fValidationMesssage = AValue then Exit;
  fValidationMesssage := AValue;
  if aValue <> '' then
    hasValidationMessage := True;
end;

function TCustomBindable.hasNoDuplicatesOn(aCaption: String;
  aOnlyWhenChecked: Boolean; var oBind, dBind: TCustomBindable): Boolean;
var
  slist: TJBStringList;
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
  slist := TJBStringList.Create;
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
  LoadIndex := -1;
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
  result := nil;
  raise Exception.Create (self.ClassName + ': Virtual Children called')
end;

function TCustomBindable.GetIndexCaption: String;
begin
  result := '';
  raise Exception.Create (self.ClassName + ': Virtual GetIndexCaption called')
end;

function TCustomBindable.IsValueValid: Boolean;
begin
  result := false;
  raise Exception.Create (self.ClassName + ': Virtual IsValueValid called')
end;

function TCustomBindable.FindUQ (aName: String): TCustomBindable;
begin
  result := nil;
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

function TCustomBindable.UpLineAsText: String;
begin
  result := 'function TCustomBindable.UpLineAsText: String;';
end;

function TCustomBindable.AllValidationsMessage: String;
var
  x: Integer;
begin
  result := ValidationMesssage;
  for x := 0 to Children.Count - 1 do
  begin
    if Children.Bindables[x].hasValidationMessage then
    begin
      if Result <> '' then
        Result := Result + LineEnding;
      Result := Result + Children.Bindables[x].AllValidationsMessage;
    end;
  end;
end;

{$ifndef NoGUI}
function TCustomBindable.bgValueColor (aReadOnly: Boolean): TColor;
begin
  if aReadOnly
  or (Children.Count > 0) then
    result :=  clBtnFace
  else
  begin
    if self = nil then
      result := $CFFFFF
    else
      result := clWhite;
  end;
end;
{$endif}

{$ifndef NoGUI}
procedure TCustomBindable.Font(aFont: TFont);
begin
  exit;
end;
{$endif}

function TCustomBindable.getIsEvaluation: Boolean;
begin
  result := (Checker <> '');
end;

function TCustomBindable.getIsExpression: Boolean;
begin
  result := (Copy (Value, 1, 2) = ':=');
end;

function TCustomBindable.getTotalNumberOfSubElements: Integer;
var
  x: Integer;
begin
  result := Children.Count;
  for x := 0 to Children.Count - 1 do
    result += Children.Bindables[x].getTotalNumberOfSubElements;
end;

initialization
  bindNilStr := '&nil';
  bindRefId := 0;
end.
