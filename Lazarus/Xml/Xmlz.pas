{$MODE Delphi}

{$define noJSON}
{$define noXMLDOM}

unit Xmlz;

interface
uses Classes
   , Express
   , ParserClasses
   , Xsdz
   , Bind
   , Graphics
   ;

resourcestring
  S_XML_REGEXP_LINK = '(?i)(FTP|HTTP|FILE|DOC)://([_a-z\d\-/]+(\.[_a-z\d\-/]+)+)((/[ _a-z\d\-\\\./]+)+)*([\?#][a-z0-9=&_/]+)?';

type TOnHaveString = procedure ( aString: String) of Object;
type TAnsiStringFunctionAnsiString = function (aString: AnsiString): AnsiString of Object;

type TXmlTagsCase = (xmlTCDontChange, xmlTCUpperCase, xmlTCLowerCase);
type TXmlTagsHyphen = (xmlTHDontChange, xmlTHHyphen, xmlTHReplaceBy);
type TXmlBrowser = (xmlBInternal, xmlBShell);
type

  { TXmlAttribute }

  TXmlAttribute = class(TCustomBindable)
private
public
  {Name: String; now in TCustomBindable }
  {Value: String; now in TCustomBindable }
  XsdAttr: TXsdAttr;
  LineNo: Integer;
  function isXmlNsAttribute: Boolean;
  function isXmlTypeDefAttribute: Boolean;
  function GetFullIndexCaption: String; Override;
  function IsValueValidAgainstXsd (var aMessageString: String): Boolean;
  function IsMoveUpPossible: Boolean;
  function IsMoveDownPossible: Boolean;
  procedure Font (aFont: TFont); Override;
  function bgColor (aReadOnly: Boolean; aColumn: Integer): TColor; Override;
  procedure MoveUp;
  procedure MoveDown;
  procedure Bind (aRoot: String; aExpress: TObject; aMaxOccurrences: Integer); Override;
  function GetStringData: String; override;
  procedure PutStringData (aString: String); override;
  constructor CreateAsString(aName, aString: String);
  constructor CreateAsInteger(aName: String;  aInteger: Integer);
  constructor CreateAsBoolean(aName: String; aBoolean: Boolean);
end;

type

{ TXmlAttributeList }

 TXmlAttributeList = class (TStringList)
  private
    function getAttributeByTag(Index: String): TXmlAttribute;
    function getBooleanByTag(Index: String): Boolean;
    function getBooleanByTagDef(Index: String; Def: Boolean): Boolean;
    function getValueByTag(Index: String): String;
    function getValueByTagDef (Index , Def : String ): String ;
protected
  function GetXmlAttribute (Index: integer): TXmlAttribute;
public
  property XmlAttributes [Index: integer]: TXmlAttribute read GetXmlAttribute;
  property BooleanByTag [Index: String]: Boolean read getBooleanByTag;
  property BooleanByTagDef [Index: String; Def: Boolean]: Boolean read getBooleanByTagDef;
  property ValueByTag [Index: String]: String read getValueByTag;
  property ValueByTagDef [Index, Def: String]: String read getValueByTagDef;
  property AttributeByTag [Index: String]: TXmlAttribute read getAttributeByTag;
  procedure Clear; override;
end;

type
  TXmlList = class;

  { TXml }

  TXml = class(TCustomBindable)
  private
    scanLineNumber: Integer;
    fFileContents: TStringList;
    fTypeDef: TXsdDataType;
    function getAttributeBooleanByTag (Index : String ): Boolean ;
    function getAttributeBooleanByTagDef (Index : String ; aDefault : Boolean
      ): Boolean ;
    function getAttributeValueByTag (Index: String ): String ;
    function getAttributeValueByTagDef (Index , aDefault : String ): String ;
    function getDocumentationText: String;
    function getAppinfoText: String;
    function getIsSoapEnvelope : Boolean ;
    function getIsSoapHeader : Boolean ;
    function getIsSoapBody : Boolean ;
    function getItemByTag (Index : String ): TXml ;
    function getTypeDef: TXsdDataType;


    function IsValueValid (var aMessage: String): Boolean; Override;
    function getChecked: Boolean;
    function GetIndexCaption: String; Override;
    function GetFullIndexCaption: String; Override;
    function getRoot: TXml;
    function getTagName: String;
    procedure setTagName(const aValue: String);
    function getCDataValue: String;
    function GetUQCaption: String;
    function getEncodedValue: String;
    function getText: String;
    procedure setText(const aValue: String);
    procedure AnalyserNeedData( Sender: TObject
                              ; var MoreData: Boolean
                              ; var Data: String
                              );
    procedure LoadJSON (aErrorFound: TOnErrorEvent);
    procedure LoadXML (aErrorFound: TOnErrorEvent);
    function GetIndexString: String;
    function GetCaption: String;
    function GetFullCaption: String;
    function GetFullUQCaption: String;
    function GetGroup: Boolean;
  public
    CData: Boolean;
    jsonType: TjsonType;
    NameSpace: String;
    {Value: String; now in TCustomBindable }
    {Parent: TXml; now in TCustomBindable}
    Items: TXmlList;
    Attributes: TXmlAttributeList;
    Xsd: TXsd;
    Ipm: TObject;
    LineNo: Integer;
    ValidationMesssage: String;
    property isSoapEnvelope: Boolean read getIsSoapEnvelope;
    property isSoapHeader: Boolean read getIsSoapHeader;
    property isSoapBody: Boolean read getIsSoapBody;
    property ItemByTag [Index: String]: TXml read getItemByTag;
    property AttributeValueByTagDef [Index, aDefault: String]: String read getAttributeValueByTagDef;
    property AttributeValueByTag [Index: String]: String read getAttributeValueByTag;
    property AttributeBooleanByTagDef [Index: String; aDefault: Boolean]: Boolean read getAttributeBooleanByTagDef;
    property AttributeBooleanByTag [Index: String]: Boolean read getAttributeBooleanByTag;
    property DocumentationText: String read getDocumentationText;
    property AppinfoText: String read getAppinfoText;
    property TypeDef: TXsdDataType read getTypeDef write fTypeDef;
    property TagName: String read getTagName write setTagName;
    property CDataValue: String read getCDataValue;
    property EncodedValue: String read getEncodedValue;
    property Group: Boolean read GetGroup;
    property Caption: String read GetCaption;
    property IndexCaption: String read GetIndexCaption;
    property UQCaption: String read GetUQCaption;
    property FullIndexCaption: String read GetFullIndexCaption;
    property FullCaption: String read GetFullCaption;
    property FullUQCaption: String read GetFullUQCaption;
    property Text: String read getText write setText;
    property Root: TXml read getRoot;
    function PrefixToNameSpace(aPrefix: String): String;
    procedure NamespacesToPrefixes (aOnlyWhenChecked: Boolean; aSl: TStringList);
    function ExpandPrefixedName (aDefaultNS, aName: String): String;
    function IndentString (x: Integer): String;
    function EncodeXml (aValue: String): String;
    function asHtmlString: String;
    function asString: String;
    function asAssignments: String;
    function AsText ( aUseNameSpaces: Boolean
                    ; aIndent: Integer
                    ; OnlyWhenChecked: Boolean
                    ; Encoded: Boolean
                    ): String;
    procedure setChecked(const aValue: Boolean); Override;
    function Children: TBindableList; Override;
    function GetStringData: String; override;
    procedure PutStringData (aString: String); override;
    procedure PutGroupData (aObject: TObject); override;
    procedure Bind (aRoot: String; aExpress: TObject; aMaxOccurrences: Integer); Override;
    procedure CheckAllAttributes (aChecked: Boolean);
    procedure Populate(aViewType: TxvViewType); Override;
    procedure Reset;
    procedure ResetValues;
    procedure Clean(aMinOccurs, aMinRecursive: Integer);
    procedure MergePreviousChecked;
    procedure TakeOver (aXml: TXml);
    function AddXml (aChildXml: TXml): TXml;
    procedure ExtendRecursivity;
    procedure ResolveNameSpaces;
    procedure InsertXml (aIndex: Integer; aXml: TXml);
    procedure InsertAttribute (aIndex: Integer; aAttr: TXmlAttribute);
    procedure AddAttribute (aAttr: TXmlAttribute);
    function DeleteChild (aXml: TXml): TXml;
    procedure DeleteAttribute (aAttr: TXmlAttribute);
    procedure LoadValues (aXml: TXml; aAddUnknowns, aOnlyWhenChecked, aCopyCheckers: Boolean); Overload;
    procedure LoadValues (aXml: TXml; aAddUnknowns, aOnlyWhenChecked: Boolean); Overload;
    procedure LoadValues (aXml: TXml; aAddUnknowns: Boolean); Overload;
    procedure CopyValues (aXml: TXml; aDoReset, aSkipAssignments: Boolean);
    procedure CopyDownLine (aXml: TXml; aOnlyWhenChecked: Boolean);
    procedure CopyRelevancy (aXml: TXml);
    procedure ResetExpectedValues;
    procedure CheckExpectedValues;
    procedure CheckDownline (aChecked: Boolean);
    function UpLineAsText: String; Override;
    procedure ForgetXsd;
    procedure SetXsdReadOnly;
    procedure GetOverrulingsAsStringList (aList: TStringList);
    procedure SetFileNamesRelative (aFileName: String);
    function Stream: String;
    function StreamXML ( aUseNameSpaces: Boolean
                        ; aAsPrefix: Boolean
                        ; aIndent: Integer
                        ; aOnlyWhenChecked: Boolean
                        ; aEncoded: Boolean
                        ): String;
    function XmlStreamer ( aUseNameSpaces: Boolean
                         ; aAsPrefix: Boolean
                         ; aIndent: Integer
                         ; OnlyWhenChecked: Boolean
                         ; Encoded: Boolean
                         ): TXml;
    function StreamJSON ( aIndent: Integer
                        ; OnlyWhenChecked: Boolean
                        ): String;
    procedure LoadFromFile (aFileName: String; ErrorFound: TOnErrorEvent);
    procedure LoadFromString (aString: String; ErrorFound: TOnErrorEvent);
    procedure LoadJsonFromFile (aFileName: String; ErrorFound: TOnErrorEvent);
    procedure LoadJsonFromString (aString: String; ErrorFound: TOnErrorEvent);
    function FindByRefId (aRefId: Integer): TXml;
    function FindUQ (aName: String): TCustomBindable; Override;
    function FindUQValue (aName: String): String;
    function FindUQXml (aName: String): TXml;
    function FindUQBind (aName: String): TCustomBindable;
    function FindCheckedXml (aName: String): TXml;
    function FindXml (aName: String): TXml;
    function IsValueValidAgainstXsd (var aMessageString: String): Boolean;
    function IndexOfRepeatableItem: Integer;
    function IndexOfRecursiveItem: Integer;
    function NumberOfSubItemsWithTag (aTag: String; OnlyWhenChecked: Boolean): Integer;
    function IsMoveUpPossible: Boolean;
    function IsMoveDownPossible: Boolean;
    procedure Font (aFont: TFont); Override;
    function bgColor (aReadOnly: Boolean; aColumn: Integer): TColor; Override;
    procedure XsdCreate (aLevel: Integer; aXsd: TXsd);
    procedure MoveUp;
    procedure MoveDown;
    procedure Sort (aRecurringElementsPath, aSubElementsPath: String);
    constructor Create; Overload;
    constructor CreateAsTimeStamp (aTagName: String; aTimeStame: TDateTime); Overload;
    constructor CreateAsBoolean (aTagName: String; aBoolean: Boolean); Overload;
    constructor CreateAsInteger (aTagName: String; aInteger: Integer); Overload;
    constructor CreateAsString (aTagName: String; aString: String); Overload;
    constructor Create (aLevel: Integer; aXsd: TXsd); Overload;
    destructor Destroy; override;
  end;

  TXmlList = class (TBindableList)
  private
    function getXmlValueByTagDef(Index, aDefault: String): String;
    function getXmlIntegerByTagDef(Index: String; aDefault: Integer): Integer;
    function getXmlIntegerByTag(Index: String): Integer;
    function getXmlBooleanByTagDef(Index: String; aDefault: Boolean): Boolean;
    function getXmlBooleanByTag(Index: String): Boolean;
    function getXmlValueByTag(Index: String): String;
    function getXmlItemByTag(Index: String): TXml;
    function getCheckedXmlItemByTag(Index: String): TXml;
    procedure SetXml(Index: integer; const Value: TXml);
    function getXmlCheckedBooleanByTag(Index: String): Boolean;
    function getXmlCheckedBooleanByTagDef(Index: String;
      aDefault: Boolean): Boolean;
    function getXmlCheckedIntegerByTag(Index: String): Integer;
    function getXmlCheckedIntegerByTagDef(Index: String;
      aDefault: Integer): Integer;
    function getXmlCheckedItemByTag(Index: String): TXml;
    function getXmlCheckedValueByTag(Index: String): String;
    function getXmlCheckedValueByTagDef(Index, aDefault: String): String;
    procedure setXmlValueByTag(Index: String; const aValue: String);
    procedure setXmlCheckedValueByTag(Index: String; const Value: String);
  protected
    function GetXml (Index: integer): TXml;
  public
    property XmlItems [Index: integer]: TXml read GetXml write SetXml;
    property XmlValueByTag [Index: String]: String read getXmlValueByTag write setXmlValueByTag;
    property XmlValueByTagDef [Index: String; aDefault: String]: String read getXmlValueByTagDef;
    property XmlIntegerByTag [Index: String]: Integer read getXmlIntegerByTag;
    property XmlIntegerByTagDef [Index: String; aDefault: Integer]: Integer read getXmlIntegerByTagDef;
    property XmlBooleanByTag [Index: String]: Boolean read getXmlBooleanByTag;
    property XmlBooleanByTagDef [Index: String; aDefault: Boolean]: Boolean read getXmlBooleanByTagDef;
    property XmlItemByTag [Index: String]: TXml read getXmlItemByTag;
    property XmlCheckedItemByTag [Index: String]: TXml read getCheckedXmlItemByTag;
    property XmlCheckedValueByTag [Index: String]: String read getXmlCheckedValueByTag write setXmlCheckedValueByTag;
    property XmlCheckedValueByTagDef [Index: String; aDefault: String]: String read getXmlCheckedValueByTagDef;
    property XmlCheckedIntegerByTag [Index: String]: Integer read getXmlCheckedIntegerByTag;
    property XmlCheckedIntegerByTagDef [Index: String; aDefault: Integer]: Integer read getXmlCheckedIntegerByTagDef;
    property XmlCheckedBooleanByTag [Index: String]: Boolean read getXmlCheckedBooleanByTag;
    property XmlCheckedBooleanByTagDef [Index: String; aDefault: Boolean]: Boolean read getXmlCheckedBooleanByTagDef;
    procedure ClearListOnly;
    procedure Clear; override;
  end;

  TXmlCoverageType = (ctXmlElement, ctXmlAttribute, ctXmlValue);
  TXmlCvrg = class (TXml) // a Xml derived for Coverage report
  private
    fIgnore: Boolean;
    fPresent: Boolean;
    distinctValues: TStringList;
    function getIsIgnored: Boolean;
    procedure setIgnore(const aValue: Boolean);
    function getXmlItems(Index: integer): TXmlCvrg;
    function getHasIgnored: Boolean;
    function getDistinctCounter: Integer;
    function getIsAttrbute: Boolean;
    function getIsValue: Boolean;
    procedure setIsAttrbute(const aValue: Boolean);
    procedure setIsValue(const aValue: Boolean);
    function getDisplayName: String;
    function getDisplayCount: String;
    function getDisplayDistinctValueCounter: String;
    function getDisplayEmptyCount: String;
    function getDisplayNilCount: String;
  published
  public
    GreenCounter, OrangeCounter, RedCounter, Counter, NilCounter, EmptyCounter: Integer;
    recurs: TXmlCvrg; // when assigned, points at a parent having same XSD
    CoverageType: TXmlCoverageType;
    property isAttrbute: Boolean read getIsAttrbute write setIsAttrbute;
    property isValue: Boolean read getIsValue write setIsValue;
    property DistinctCounter: Integer read getDistinctCounter;
    property Ignore: Boolean read fIgnore write setIgnore;
    procedure CalculateCoverage;
    property isIgnored: Boolean read getIsIgnored;
    property hasIgnored: Boolean read getHasIgnored;
    property displayName: String read getDisplayName;
    function DisplayCoverage (doShowIgnoreds: Boolean): String;
    function DisplayPercentage (doShowIgnoreds: Boolean): String;
    property displayCount: String read getDisplayCount;
    property displayNilCount: String read getDisplayNilCount;
    property displayEmptyCount: String read getDisplayEmptyCount;
    property displayDistinctValueCounter: String read getDisplayDistinctValueCounter;
    property XmlItems [Index: integer]: TXmlCvrg read getXmlItems;
    function AddXml (aChildXml: TXmlCvrg): TXmlCvrg;
    procedure CountUsage (dataXml: TXml; aOnlyWhenChecked: Boolean);
    constructor CreateFromXsd (aName: String; aXsd: TXsd);
    constructor CreateFromIpm (aName: String; aIpm: TObject);
    constructor CreateAsString(aName, aString: String); overload;
    constructor Create; overload;
    destructor Destroy; override;
  end;

function GenerateXmlHeader (aGenerateXmlComment: Boolean): String;
function xmlEncodeXml (aValue: String): String;
function xmlDecodeXml (aValue: String): String;
function strAdd (aString, aStringToAdd: String): String;
procedure xmlSetDefaultColors;
function ColorToHtml (aColor: TColor): String;
function HtmlToColor (aHtml: String): TColor;
function textToHtml (aString: String): String;

const BOM = #$EF#$BB#$BF;
const CheckedAtttributeName = 'checked__';
const UncheckedAtttributeName = 'unchecked__';

var
  XmlConvention: String;
  XmlVersion: String;
  XmlEncoding: String;
  XmlTagsCase: TXmlTagsCase;
  XmlTagsHyphen: TXmlTagsHyphen;
  XmlReplaceHyphenBy: String;
  XmlBrowser: TXmlBrowser;
  CDataString: String;
  IsRootElement: Boolean;
  _xmlUserName: String;
  _xmlProgName: String;
  _xmlProgVersion: String;
  _xmlLicensed: Boolean;
  bgCorrelationItemColor: TColor;
  bgExpectedValueColor: TColor;
  bgNilValueColor: TColor;
  bgElementValueColor: TColor;
  fgMissingColor: TColor;
  fgUnknownDatatypeColor: TColor;
  DecryptString: TAnsiStringFunctionAnsiString;
  EncryptString: TAnsiStringFunctionAnsiString;

implementation

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
{$ENDIF}
  SysUtils
   , StrUtils
   , Types
   , XmlzConsts
   , XmlAnalyser
{$ifdef JSON}
   , JsonAnalyser
{$endif}
   , RegExpr
   , xmlio
   , HashUtilz
   ;

function textToHtml (aString: String): String;
  function _docLink (aString: String): TXml;
  begin
    result := TXml.CreateAsString ( 'a'
                                  , Copy (aString, 7 , Length (aString))
                                  );
    result.AddAttribute ( TXmlAttribute.CreateAsString
                          ( 'href'
                          , 'file://'
                          + ReplaceText
                              ( ExtractFilePath (ParamStr(0)) + 'Documentation\'
                              , '\'
                              , '/'
                              )
                          + Copy (aString, 7 , Length (aString))
                          )
                        );
    result.AddAttribute ( TXmlAttribute.CreateAsString ('target', '_blank'));
  end;
var
  rx: TRegExpr;
  rslt: Boolean;
  p: Integer;
begin
  result := '';
  rx := TRegExpr.Create;
  try
    rx.Expression := S_XML_REGEXP_LINK;
    with TXml.CreateAsString('html', '') do
    try
      with AddXml (TXml.CreateAsString('body','')) do
      begin
        AddAttribute(TXmlAttribute.CreateAsString('bgcolor', 'RGB(212,208,200)')); // bg read-only gray
        with AddXml (TXml.CreateAsString('p','')) do
        begin
          p := 1;
          rslt := Rx.Exec(aString);
          while rslt do
          begin
            if rx.MatchPos [0] > p then
              AddXml(TXml.CreateAsString('', Copy (aString, p, rx.MatchPos[0] - p)));
            if uppercase (copy (rx.Match[0],1 , 6)) = 'DOC://' then
              AddXml (_DocLink(rx.Match[0]))
            else
              with AddXml (TXml.CreateAsString('a', rx.Match[0])) do
              begin
                AddAttribute (TXmlAttribute.CreateAsString('href', rx.Match[0]));
                AddAttribute (TXmlAttribute.CreateAsString('target', '_blank'));
              end;
            p := rx.MatchPos[0] + rx.MatchLen[0];
            rslt := Rx.ExecNext;
          end;
          if p < Length (aString) then
            AddXml(TXml.CreateAsString('', Copy (aString, p, Length (aString))));
        end;
      end;
      result := asHtmlString;
    finally
      free;
    end;
  finally
    rx.Free;
  end;
end;

function strAdd (aString, aStringToAdd: String): String;
begin
  if aStringToAdd <> '' then
    result := aString + aStringToAdd + #$D#$A
  else
    result := aString;
end;

function ColorToHtml (aColor: TColor): String;
var
  s: String;
begin
  s := IntToHex(aColor, 6);
  result := '#' + Copy (s, 5, 2) + Copy (s, 3, 2) + Copy (s, 1, 2);
end;

function HtmlToColor (aHtml: String): TColor;
  function _TryHexToInt(const HexStr: string; out Value: Integer): Boolean;
  var
    E: Integer; // error code
  begin
    Val(SysUtils.HexDisplayPrefix + HexStr, Value, E);
    Result := E = 0;
  end;
  function _TryHexToBuf(HexStr: string; var Buf): Boolean;
  var
    I: Integer;       // loops through characters of string
    PB: ^Byte;        // references each byte in buffer
    ByteVal: Integer; // a byte value from hex string
  begin
    Result := False;
    if HexStr = '' then
      Exit;
    if Odd(Length(HexStr)) then
      HexStr := '0' + HexStr;
    I := 1;
    PB := @Buf;
    while I <= Length(HexStr) do
    begin
      if not _TryHexToInt(HexStr[I] + HexStr[I + 1], ByteVal) then
        Exit;
      PB^ := Byte(ByteVal);
      Inc(I, 2);
      Inc(PB);
    end;
    Result := True;
  end;
var
  s: String;
  i: Integer;
begin
  s := Copy (aHtml, 6, 2) + Copy (aHtml, 4, 2) + Copy (aHtml, 2, 2);
  if s = '' then
    s := '000000';
  if not _TryHexToInt(s, i) then
    raise SysUtils.EConvertError.CreateFmt('''%s'' is not a valid hexadecimal value', [s]);
  result := i;
end;

procedure xmlSetDefaultColors;
begin
  bgCorrelationItemColor := clMoneyGreen;
  bgExpectedValueColor := $E7FFE7;
  bgNilValueColor := $CFFFFF;
  bgElementValueColor := clWhite;
  fgMissingColor := clRed;
  fgUnknownDatatypeColor := clRed;
end;

function xmlDecodeXml (aValue: String): String;
var
  p, z: Integer;
  s, es, ds: String;
begin
  p := Pos('&', aValue);
  if p < 1 then
  begin
    result := aValue;
    exit;
  end;
  s := Copy (aValue, p, Length (aValue));
  z := Pos (';', s);
  if z < 1  then
    raise Exception.CreateFmt('xmlDecodeXml (%s): Illegal escape sequence at position %d', [aValue, p]);
  es := Copy (s, 1, z - 1);
  ds := '';
  if es = '&amp'  then ds := '&';
  if es = '&quot' then ds := '"';
  if es = '&apos' then ds := '''';
  if es = '&lt'   then ds := '<';
  if es = '&gt'   then ds := '>';
  if ds = '' then
    if LeftStr(es, 3) = '&#x' then
      try ds := Char(StrToInt('$'+Copy(es,4,6))); except ds := ''; end;
  if ds = '' then
    if LeftStr(es, 2) = '&#' then
      try ds := Char(StrToInt(Copy(es,3,6))); except ds := ''; end;
  if ds = ''  then
    raise Exception.CreateFmt('xmlDecodeXml (%s): Illegal escape sequence %s', [aValue, es + ';']);
  result := Copy (aValue, 1, p - 1)
          + ds
          + xmlDecodeXml (Copy (s, z + 1, Length (s)))
          ;
end;

function xmlEncodeXml (aValue: String): String;
var
  x: Integer; // loops thru characters in string
begin
  Result := '';
  for x := 1 to Length(aValue) do
  begin
    if (    (aValue [x] = #13)
        and (x < Length (aValue))
        and (aValue [x + 1] = #10)
       )
    or (    (aValue [x] = #10)
        and (x > 1)
        and (aValue [x - 1] = #13)
       ) then
      Result := Result + aValue[x]
    else
    begin
      case aValue[x] of
        ' '..'!': Result := Result + aValue[x];
        '"': result := result + '&quot;';
        '#'..'%': Result := Result + aValue[x];
        '&': result := result + '&amp;';
        '''': result := result + '&apos;';
        '('..';': Result := Result + aValue[x];
        '<': result := result + '&lt;';
        '=': Result := Result + aValue[x];
        '>': result := result + '&gt;';
        '?'..'~': Result := Result + aValue[x];
        else
          Result := Result + '&#x' + SysUtils.IntToHex(Ord(aValue[x]), 2) + ';';
      end;
    end;
  end;
end;

function GenerateXmlHeader (aGenerateXmlComment: Boolean): String;
var
  xVersion: String;
  xEncoding: String;
begin
  result := '';
  xsiGenerated := False;
  xsdGenerated := False;
  if XmlVersion <> '' then
    xVersion := XmlVersion
  else
    xVersion := '1.0';
  if XmlEncoding <> '' then
    xEncoding := XmlEncoding
  else
    xEncoding := 'UTF-8';
  if aGenerateXmlComment then
  begin
    result := StrAdd ( result
                     , '<?xml version="'
                     +  xVersion
                     + '" encoding="'
                     +  xEncoding
                     + '" ?>'
                     );
    result := StrAdd ( result
                     , '<!-- '
                     + DateToStr (Now)
                     + ' '
                     + TimeToStr (Now)
                     + ': Generated with '
                     + _xmlProgName
                     + ' '
                     + _xmlProgVersion
                     + ' by '
                     + _xmlUserName
                     + ' -->'
                     );
  end;
end;

procedure TXml.AnalyserNeedData(Sender: TObject; var MoreData: Boolean;
  var Data: String);
begin
  if scanLineNumber = fFileContents.Count then
    MoreData := False
  else
  begin
    Data := fFileContents.Strings [scanLineNumber];
    Inc (scanLineNumber);
  end;
end;

constructor TXml.Create;
begin
  inherited Create;
  CData := False;
  jsonType := jsonNone;
  Items := TXmlList.Create;
  Attributes := TXmlAttributeList.Create;
end;

destructor TXml.Destroy;
begin
  if not Assigned (self) then Exit;
  if Assigned (Items) then
  begin
    Items.Clear;
    Items.Free;
  end;
  if Assigned (Attributes) then
  begin
    Attributes.Clear;
    Attributes.Free;
  end;
  inherited Destroy;
end;

procedure TXml.ExtendRecursivity;
var
  x, maxElm, curDepth: Integer;
begin
  if (Items.Count > 0)
  or (not Assigned (TypeDef))
  then Exit;
  maxElm := TypeDef.xsdDescr.xsdElementsWhenRepeatable;
  curDepth := TypeDef._DepthBillOfMaterial;
  TypeDef.xsdDescr.xsdElementsWhenRepeatable := 1;
  TypeDef._DepthBillOfMaterial := xsdMaxDepthBillOfMaterials + 1;
  xsdMaxDepthBillOfMaterials := 1;
  try
    for x := 0 to TypeDef.ElementDefs.Count - 1 do
      AddXml (TXml.Create(0, TypeDef.ElementDefs.Xsds[x]));
  finally
    TypeDef.xsdDescr.xsdElementsWhenRepeatable := maxElm;
    TypeDef._DepthBillOfMaterial := curDepth;
  end;
end;

procedure TXml.ResolveNameSpaces;
  function _ResolveNamespace (aXml: TXml; aNsPrefix: String): String;
  var
    nsAttr: TXmlAttribute;
  begin
    result := '';
    if not Assigned(aXml) then exit;
    if aNsPrefix = '' then
      nsAttr := aXml.Attributes.AttributeByTag['xmlns']
    else
      nsAttr := aXml.Attributes.AttributeByTag['xmlns:' + aNsPrefix];
    if Assigned (nsAttr) then
      result := nsAttr.Value
    else
      result := _ResolveNamespace(aXml.Parent as TXml, aNsPrefix);
  end;
var
  x: Integer;
begin
  NameSpace:=_ResolveNamespace(Self, NsPrefix);
  for x := 0 to Items.Count - 1 do
    Items.XmlItems[x].ResolveNameSpaces;
end;

procedure TXmlList.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    XmlItems [x].Free;
  inherited;
end;

procedure TXmlList.ClearListOnly;
begin
  inherited Clear;
end;

function TXmlList.GetXml (Index: integer): TXml;
begin
  result := TXml (Objects [index]);
end;

procedure TXml.LoadFromFile(aFileName: String; ErrorFound: TOnErrorEvent);
begin
  fFileContents := TStringList.Create;
  try
    fFileContents.Text := xmlio.ReadStringFromFile (aFileName);
    LoadXml (ErrorFound);
  finally
    fFileContents.Free;
  end;
end;

procedure TXml.LoadJsonFromFile(aFileName: String; ErrorFound: TOnErrorEvent);
begin
  fFileContents := TStringList.Create;
  try
    fFileContents.Text := ReadStringFromFile (aFileName);
    LoadJson (ErrorFound);
  finally
    fFileContents.Free;
  end;
end;

procedure TXml.LoadJsonFromString(aString: String; ErrorFound: TOnErrorEvent);
begin
  fFileContents := TStringList.Create;
  try
    fFileContents.Text := aString;
    LoadJson (ErrorFound);
  finally
    fFileContents.Free;
  end;
end;

procedure TXml.LoadFromString(aString: String; ErrorFound: TOnErrorEvent);
begin
  fFileContents := TStringList.Create;
  try
    fFileContents.Text := aString;
    LoadXml (ErrorFound);
  finally
    fFileContents.Free;
  end;
end;

procedure TXml.LoadJSON(aErrorFound: TOnErrorEvent);
{$ifdef json}
var
  jsonAnalyser: TjsonAnalyser;
{$endif}
begin
{$ifdef json}
  Items.Clear;
  jsonAnalyser := TjsonAnalyser.Create (nil);
  jsonAnalyser.StartState := InitState;
  jsonAnalyser.OnNeedData := AnalyserNeedData;
  jsonAnalyser.OnError := aErrorFound;
  jsonAnalyser.Xml := Self;
  try
    scanLineNumber := 0;
    jsonAnalyser.Prepare;
    scanLineNumber := 0;
    jsonAnalyser.Execute;
  finally
    jsonAnalyser.Free;
  end;
{$endif}
end;


procedure TXml.LoadXML (aErrorFound: TOnErrorEvent);
var
  XmlAnalyser: TXmlAnalyser;
begin
  Items.Clear;
  XmlAnalyser := TXmlAnalyser.Create (nil);
  XmlAnalyser.StartState := InitState;
  XmlAnalyser.OnNeedData := AnalyserNeedData;
  XmlAnalyser.OnError := aErrorFound;
  try
    scanLineNumber := 0;
    XmlAnalyser.Prepare;
    scanLineNumber := 0;
    XmlAnalyser.Execute;
  finally
    if Assigned (XmlAnalyser.BaseXml) then
    begin
      TakeOver (XmlAnalyser.BaseXml);
{
      TagName := BaseXml.TagName;
      CData := BaseXml.CData;
      Value := BaseXml.Value;
      Parent := BaseXml.Parent;
      Items.Free;
      Items := BaseXml.Items;
      Attributes.Clear;
      Attributes := BaseXml.Attributes;
}
    end;
    XmlAnalyser.BaseXml.Free;
    XmlAnalyser.BaseXml := nil;
    XmlAnalyser.Free;
  end;
end;

procedure TXml.GetOverrulingsAsStringList (aList: TStringList);
  procedure _get (aXml: TXml);
  var
    x: Integer;
  begin
    if not Assigned (aXml.Xsd) then
      exit;
    if Assigned (aXml.TypeDef) then
      aList.AddObject('', aXml);
    for x := 0 to aXml.Items.Count - 1 do
      _Get (aXml.Items.XmlItems [x]);
  end;
begin
  aList.Clear;
  _Get (self);
end;

function TXml.asAssignments: String;
  function _i(i: Integer): String;
  begin
    result := '';
    while i > 0 do
    begin
      result := result + ' ';
      Dec(i);
    end;
  end;
  function _toXml (aValue: String): String;
  var
    x: Integer;
  begin
    result := '';
    for x := 1 to Length (aValue) do
    begin
      case aValue [x] of
        'x': result := result + 'x';  //tja, je moet wat
      else
        result := result + aValue [x];
      end;
    end;
  end;
  function _asAssignments (i: Integer; aXml: TXml): String;
  var
    x: Integer;
  begin
    result := '';
    if not aXml.Checked then Exit;
    if aXml.TypeDef.ElementDefs.Count = 0 then
    begin
      if Copy (aXml.Value, 1, 2) = ':=' then
        result := _i(i) + '.' + aXml.TagName + ' ' + aXml.Value + ';' + CRLF
      else
        result := _i(i) + '.' + aXml.TagName + ' := ''' + _toXml(aXml.Value) + ''';' + CRLF;
    end
    else
    begin
      result := _i(i) 
              + Format('with %s.%s do' , [IfThen(aXml.Xsd.maxOccurs<>'1', 'new '), aXml.Name]) + CRLF 
              + _i(i) + '{' + CRLF;
      for x := 0 to aXml.Items.Count - 1 do
        result := result
                + _asAssignments(i + 2, aXml.Items.XmlItems[x])
                ;
      result := result + _i(i) + '}' + CRLF;
    end;
  end;
begin
  if not Assigned(TypeDef) then
    raise Exception.Create('AsAssignment only allowed when associated with an XSD');
  result := '{' + CRLF
          + '  .' + self.Name + ' := nil;' + CRLF
          + _asAssignments(2, self)
          + '}' + CRLF
end;

function TXml.asHtmlString: String;
  function _ValueToHtml (aValue: String): String;
  var
    x: Integer;
  begin
    result := '';
    if (AnsiLeftStr(aValue, 6) = '<html>') then
    begin
      x := Pos('</html>', aValue);
      result := Copy (aValue, 7, x - 7);
      result := aValue;
      exit;
    end;
    for x := 1 to Length (aValue) do
    begin
      case aValue [x] of
        '<': result := result + '&lt;';
        '>': result := result + '&gt;';
        '&': result := result + '&amp;';
//      '''': result := result + '&apos;';
//      '"': result := result + '&quot;';
        '_': result := result + '&nbsp;';
      else
        if Ord (aValue[x]) = 10 then
          result := result + '<br>'
        else
          result := result + aValue [x];
      end;
    end;
  end;
var
  x: Integer;
begin
  result := '';
  if TagName <> '' then
  begin
    result := '<' + TagName;
    for x := 0 to Attributes.Count - 1 do
      result := result
              + ' '
              + Attributes.XmlAttributes [x].Name
              + '="' + Attributes.XmlAttributes [x].Value
              + '"'
              ;
    result := result + '>';
  end;
  result := result + _ValueToHtml (Value);
  for x := 0 to Items.Count - 1 do
    result := result + Items.XmlItems [x].asHtmlString;
  if (TagName <> '')
  and (TagName <> 'br') then
    result := result + '</' + TagName + '>';
end;

function TXml.asString: String;
  function _ValueToXml (aValue: String): String;
  var
    x: Integer;
  begin
    result := '';
    for x := 1 to Length (aValue) do
    begin
      case aValue [x] of
        '<': result := result + '&lt;';
        '>': result := result + '&gt;';
        '&': result := result + '&amp;';
        '''': result := result + '&apos;';
        '"': result := result + '&quot;';
      else
        result := result + aValue [x];
      end;
    end;
  end;
var
  x: Integer;
begin
  result := '<' + TagName;
  for x := 0 to Attributes.Count - 1 do
    result := result
            + ' '
            + Attributes.XmlAttributes [x].Name
            + '="' + Attributes.XmlAttributes [x].Value
            + '"'
            ;
  result := result + '>' + _ValueToXml (Value);
  for x := 0 to Items.Count - 1 do
    result := result + Items.XmlItems [x].asString;
  result := result + '</' + TagName + '>';
end;

function TXml.AsText(aUseNameSpaces: Boolean; aIndent: Integer;
  OnlyWhenChecked: Boolean; Encoded: Boolean): String;
begin
  result := StreamXML ( aUseNameSpaces
                      , True
                      , aIndent
                      , OnlyWhenChecked
                      , Encoded
                      );
end;

function TXml.Stream: String;
  function _Stream (aIndent: Integer; aXml: TXml): String;
    function _StreamAttributes: String;
    var
      x: Integer;
    begin
      result := '';
      for x := 0 to aXml.Attributes.Count - 1 do
      begin
        if aXml.Attributes.XmlAttributes[x].Checked then with aXml.Attributes.XmlAttributes[x] do
          result := result + ' ' + Name + '="' + EncodeXml(Value) + '"';
      end;
    end;
  var
    x: Integer;
  begin
    result := '';
    result := IndentString(aIndent) + '<' + aXml.Name + _StreamAttributes;
    if (aXml.Value = '')
    and (aXml.Items.Count = 0)
    then
      result := result + '/>'
    else
    begin
      result := result + '>';
      if aXml.Items.Count = 0  then
        result := result + aXml.EncodeXml(aXml.Value)
      else
      begin
        result := result + CRLF;
        for x := 0 to aXml.Items.Count - 1 do
          result := result + _Stream(aIndent + 2, aXml.Items.XmlItems[x]);
        result := result + IndentString(aIndent);
      end;
      result := result + '</' + aXml.Name + '>';
    end;
    result := result + CRLF;
  end;
begin
  result := _Stream(0, self);
end;

function TXml.IndentString (x: Integer): String;
begin
  SetLength(result, x);
  while x > 0 do
  begin
    result[x] := ' ';
    Dec (x);
  end;
end;

function TXml.StreamJSON(aIndent: Integer; OnlyWhenChecked: Boolean): String;
  function _ValueToJSON (aValue: String): String;
  var
    x: Integer;
  begin
    result := '';
    for x := 1 to Length (aValue) do
    begin
      case aValue [x] of
        '<': result := result + '<';
      else
        result := result + aValue [x];
      end;
    end;
  end;
  function _StreamJSONValue (aXml: TXml; aIndent: Integer): String;
  var
    x: Integer;
    xSep: String;
    xJsonType: TjsonType;
    xSwapParent: TCustomBindable;
  begin
    result := '';
    xJsonType := aXml.jsonType;
    if Assigned (aXml.Parent) then with aXml.Parent as TXml do
      if jsonType = jsonArray then
        xJsonType := jsonArrayValue;
    case xJsonType of
      jsonNone: ;
      jsonString: result := '"' + _ValueToJSON(aXml.Value) + '"';
      jsonNumber: result := aXml.Value;
      jsonBoolean: result := aXml.Value;
      jsonObject:
        begin
          result := #$D#$A + IndentString(aIndent) + '{ ';
          xSep := '';
          for x := 0 to aXml.Items.Count - 1 do
          begin
            if aXml.Items.XmlItems[x].Checked or (not OnlyWhenChecked) then
            begin
              result := result
                      + xSep
                      + '"' + aXml.Items.XmlItems[x].Name + '": '
                      + _StreamJSONValue (aXml.Items.XmlItems[x], aIndent + 2);
              xSep := #$D#$A + IndentString(aIndent) + ', ';
            end;
          end;
          result := result + #$D#$A + IndentString(aIndent) + '}';
        end;
      jsonArrayValue:
        begin
          xSwapParent := aXml.Parent;
          aXml.Parent := nil;
          try
            result := result
                      + xSep
                      + _StreamJSONValue (aXml, aIndent + 0);
          finally
            aXml.Parent := xSwapParent;
          end;
        end;
      jsonArray:
        begin
          result := #$D#$A + IndentString(aIndent) + '[ ';
          xSep := '';
          for x := 0 to aXml.Items.Count - 1 do
          begin
            if aXml.Items.XmlItems[x].Checked or (not OnlyWhenChecked) then
            begin
              result := result
                      + xSep
                      + _StreamJSONValue (aXml.Items.XmlItems[x], aIndent + 2);
              xSep := #$D#$A + IndentString(aIndent) + ', ';
            end;
          end;
          result := result + #$D#$A + IndentString(aIndent) + ']';
        end;
    end;
  end;
begin
  result := '';
  if OnlyWhenChecked and not Checked then Exit;
  result := _StreamJsonValue(Self, aIndent);
end;

function TXml.EncodeXml (aValue: String): String;
var
  x: Integer;
begin
  result := '';
  for x := 1 to Length (aValue) do
  begin
    case aValue [x] of
      '<': result := result + '&lt;';
      '>': result := result + '&gt;';
      '&': result := result + '&amp;';
      '''': result := result + '&apos;';
      '"': result := result + '&quot;';
    else
      result := result + aValue [x];
    end;
  end;
end;

function TXml.StreamXML ( aUseNameSpaces: Boolean
                        ; aAsPrefix: Boolean
                        ; aIndent: Integer
                        ; aOnlyWhenChecked: Boolean
                        ; aEncoded: Boolean
                        ): String;
var
  nsAttributes: TStringList;
  function _doEncode (aXml: TXml): Boolean;
  begin
    if aXml.Name = 'GenericData' then
      aXml.Name := 'GenericData';
    result := (Assigned (aXml.Xsd))
          and (aXml.Xsd.ElementNameSpace <> '')
          and (aXml.Xsd.DoNotEncode = False)
          and (   aEncoded
               or (    Assigned (aXml.Xsd.sType)
                   and aXml.Xsd.sType.IsExtention
                   and (aXml.Xsd.sType.Name <> '')
                  )
               or (Assigned (aXml.Xsd.sType) and (aXml.Xsd.sType.ExtendedByList.Count > 0))
              );
  end;
  function _fdQual (spec: Boolean): Boolean;
  begin
    result := (   (spec and (xsdFormDefault = xsdFDAccordingWsdl))
               or (xsdFormDefault = xsdFDQualified)
              )
  end;
  function _PrefixedTagname (aXml: TXml): string;
  begin
    if (aXml.NameSpace <> '')
    and aAsPrefix then
      result := aXml.NsPrefix + ':' + aXml.Name
    else
      result := aXml.Name;
  end;
  function _xmlNsStrings (aXml: TXml; aEncoded: Boolean): String;
  var
    n: Integer;
  begin
    result := '';
    if (aUseNameSpaces)
    and (aAsPrefix)
    and (aXml = self) then
    begin
      for n := 0 to nsAttributes.Count - 1 do
        result := result + Format(' xmlns:ns%d="%s"', [n + 1, nsAttributes.Strings[n]]);
      if aEncoded then
        result := result + aXml.TypeDef.XsiNameSpaceAttribute;
    end;
  end;
  function _StreamXML(aXml: TXml; aIndent: Integer): String;
  var
    x, f: Integer;
    xString: String;
    xTagName: String;
    xAttrNSPrefix: String;
    _xsiGenerated: Boolean; //stack xsiGenerated
    _xsdGenerated: Boolean; //stack xsdGenerated
  begin
    result := '';
    if (aOnlyWhenChecked and not aXml.Checked) then
      exit;
    _xsiGenerated := xsiGenerated; // push boolean
    _xsdGenerated := xsdGenerated; // push boolean
    try
      if (_doEncode (aXml))
      then
      begin
        xTagName := _PrefixedTagname (aXml);
        xString := IndentString (aIndent)
                 + '<'
                 + xTagName
                 ;
        if aUseNameSpaces
        and (not aAsPrefix)
        and (aXml.NameSpace <> '')
        and (   (aXml = self)
             or (    Assigned(aXml.Parent)
                 and ((aXml.Parent as TXml).NameSpace <> aXml.NameSpace)
                )
            ) then
          xString := xString + ' xmlns="' + aXml.NameSpace+ '"';
        xString := xString + _xmlNsStrings (aXml, True);
        xString := xString
                 + ' '
                 + 'xsi:type="'
                 ;
        if aXml.TypeDef.IsBuiltIn then
          xString := xString + 'xsd:'
        else
        begin
          if nsAttributes.Find(aXml.TypeDef.NameSpace, f) then
            xString := xString + 'ns' + IntToStr(f) + ':'
          else
            xString := xString + 'nsx:';
        end;
        xString := xString
                 + aXml.TypeDef.Name
                 + '"'
                 ;
        xString := xString
                 + aXml.TypeDef.XsiNameSpaceAttribute
                 ;
      end // if doEncode
      else
      begin
        xTagName := _PrefixedTagname(aXml);
        xString := IndentString (aIndent)
                 + '<'
                 + xTagName
                 ;
        if aXml = self then
          xString := xString + _xmlNsStrings (aXml, False);
        if aUseNameSpaces
        and (not aAsPrefix)
        and (aXml.NameSpace <> '')
        and (   (aXml = self)
             or (    Assigned (aXml.Xsd)
                 and aXml.Xsd.FormDefaultQualified
                )
             or (    Assigned(aXml.Parent)
                 and ((aXml.Parent as TXml).NameSpace <> aXml.NameSpace)
                )
            ) then
          xString := xString + ' xmlns="' + aXml.NameSpace+ '"';
      end;
      for x := 0 to aXml.Attributes.Count - 1 do
      begin
        if (not aOnlyWhenChecked)
        or (aXml.Attributes.XmlAttributes [x].Checked) then
        begin
          if Assigned (aXml.Attributes.XmlAttributes [x].XsdAttr)
          and _fdQual (aXml.Attributes.XmlAttributes [x].XsdAttr.FormDefaultQualified) then
            xAttrNSPrefix := aXml.Attributes.XmlAttributes [x].XsdAttr.NSPrefix + ':'
          else
            xAttrNSPrefix := '';
          xString := xString
                   + ' '
                   + xAttrNSPrefix
                   + aXml.Attributes.XmlAttributes [x].Name
                   + '="'
                   + EncodeXml (aXml.Attributes.XmlAttributes [x].Value)
                   + '"'
                   ;
        end;
      end;
      xString := xString + '>';
      if aXml.Group = True then
      begin
        result := StrAdd (result, xString);
        for x := 0 to aXml.Items.Count - 1 do
          result := result
                  + _StreamXML (aXml.Items.XmlItems [x], aIndent + 2);
        result := StrAdd ( result
                         , IndentString (aIndent)
                         + '</'
                         + xTagName
                         + '>'
                         );
      end
      else
      begin
        if aXml.CData then
        begin
        result := StrAdd ( result
                         , IndentString (aIndent)
                         + '<![CDATA['
                         + aXml.Value
                         + ']]>'
                         )
        end
        else
        begin
          result := StrAdd ( result
                           , xString
                           + aXml.EncodedValue
                           + '</'
                           + xTagName
                           + '>'
                           );
        end;
      end;
    finally
      xsiGenerated := _xsiGenerated; // pop boolean
      xsdGenerated := _xsdGenerated; // pop boolean
    end;
  end;
begin
  result := '';
  nsAttributes := TStringList.Create;
  try
    NamespacesToPrefixes (aOnlyWhenChecked, nsAttributes);
    if aIndent = 0 then
      result := GenerateXmlHeader (True);
    result := result + _StreamXML(Self, aIndent);
  finally
    nsAttributes.Free;
  end;
end;

function TXml .XmlStreamer (aUseNameSpaces : Boolean ; aAsPrefix : Boolean ;
  aIndent : Integer ; OnlyWhenChecked : Boolean ; Encoded : Boolean ): TXml ;
begin
  { TODO : replace quickfit by desired solution }
  result := TXml.Create;
  result.LoadFromString(Self.StreamXML(aUseNameSpaces, aAsPrefix, 2, OnlyWhenChecked, Encoded), nil);
end;

{ TXmlAttributeList }

procedure TXmlAttributeList.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    XmlAttributes [x].Free;
  inherited;
end;

function TXmlAttributeList.getAttributeByTag(Index: String): TXmlAttribute;
var
  x: Integer;
begin
  result := nil;
  for x := 0 to Count - 1 do
  begin
    if (XmlAttributes[x].Name = Index) then
    begin
      result := XmlAttributes [x];
      exit;
    end;
  end;
end;

function TXmlAttributeList.getBooleanByTag(Index: String): Boolean;
begin
  result := getBooleanByTagDef (Index, False);
end;

function TXmlAttributeList.getBooleanByTagDef(Index: String; Def: Boolean
  ): Boolean;
var
  xAttr: TXmlAttribute;
begin
  result := False;
  xAttr := getAttributeByTag(Index);
  if Assigned (xAttr) then
    result := (xAttr.Value = 'true') or (xAttr.Value = '1')
  else
    result := Def;
end;

function TXmlAttributeList.getValueByTag(Index: String): String;
begin
  result := getValueByTagDef(Index, '');
end;

function TXmlAttributeList .getValueByTagDef (Index , Def : String ): String ;
var
  xAttr: TXmlAttribute;
begin
  xAttr := getAttributeByTag(Index);
  if Assigned (xAttr) then
    result := xAttr.Value
  else
    result := Def;
end;

function TXmlAttributeList.GetXmlAttribute(Index: integer): TXmlAttribute;
begin
  result := TXmlAttribute (Objects [index]);
end;

procedure TXml.LoadValues(aXml: TXml; aAddUnknowns, aOnlyWhenChecked, aCopyCheckers: Boolean);
  function _getExtendedTypedef (aName: String; aTypeDef: TXsdDataType): TXsdDataType;
    function __get (aTypeDef: TXsdDataType): TXsdDataType;
    var
      x: Integer;
    begin
      result := nil;
      if aTypeDef.Name = aName then
        result := aTypeDef
      else
      begin
        for x := 0 to aTypeDef.ExtendedByList.Count - 1 do
        begin
          result := __get(aTypeDef.ExtendedByList.XsdDataTypes[x]);
          if Assigned (result) then
            Exit;
        end;
      end;
    end;
  begin
    result := nil;
    while aTypeDef.IsExtention and Assigned (aTypeDef.BaseDataType) do
      aTypeDef := aTypeDef.BaseDataType;
    result := __Get (aTypeDef);
  end;
var
  x: Integer;
  y: Integer;
  xXml: TXml;
  cXml: TXml; {xml with corr. tagname but checked}
  yXml: TXml; {xml with corr. tagname and unchecked}
  nXml: TXml; {new xml in case only cXml found}
  xXmlAttr: TXmlAttribute;
  xtndDatatype: TXsdDataType;
  _Checked: Boolean;
  _Name: String;
begin
  if self = nil then exit;
  if aXml = nil then exit;
  if not (self is TXml) then
    raise Exception.Create('Self is not an XML');
  if not (aXml is TXml) then
    raise Exception.Create ( 'Not valid XML data ');
  if aOnlyWhenChecked and (not aXml.Checked) then Exit;
  if NameWithoutPrefix (TagName) <> NameWithoutPrefix (aXml.TagName) then
    exit;
  for x := 0 to Items.Count - 1 do
    Items.XmlItems[x].isProcessed := False;
  if (aXml.Items.Count > 0)
  and (Items.Count = 0) then
    ExtendRecursivity;
  if aXml.Value <> '' then
    if Group then
      aXml.Value := '';

  if (   (aXml.Group and (Value <> ''))
      or ((aXml.Value <> '') and Group)
     )
  then begin
    raise Exception.Create ( 'Mismatch on grouping for tag: '
                           + TagName
                           );
  end; {if aXml.Group <> aIpmItem.Group}

  if Assigned (Xsd) then
  begin
    if Assigned (TypeDef)
    and (   (TypeDef.IsExtention and Assigned (TypeDef.BaseDataType))
         or (TypeDef.ExtendedByList.Count > 0)
        ) then
    begin
      if Assigned (aXml.Xsd) then
      begin
        if TypeDef <> aXml.TypeDef then
        begin
          xtndDatatype := _getExtendedTypedef (aXml.TypeDef.Name, Xsd.sType);
          if Assigned (xtndDatatype)
          and (xtndDatatype <> TypeDef) then
          begin
            TypeDef := xtndDatatype;
            XsdCreate(0, Xsd);
          end;
        end;
      end
      else
      begin // in case aXml loaded from a string, maybe there is a xsi:type
        for x := 0 to aXml.Attributes.Count - 1 do
        begin
          if NameWithoutPrefix (aXml.Attributes.XmlAttributes[x].Name) = 'type' then
          begin
            xtndDatatype := _getExtendedTypedef (NameWithoutPrefix (aXml.Attributes.XmlAttributes[x].Value), Xsd.sType);
            if Assigned (xtndDatatype)
            and (xtndDatatype <> TypeDef) then
            begin
              TypeDef := xtndDatatype;
              XsdCreate(0, Xsd);
            end;
          end;
        end;
      end;
    end;
  end;
  if aCopyCheckers then
    Checker := aXml.Checker;
  fChecked := aXml.Checked;
  Value := aXml.Value;
  for x := 0 to aXml.Items.Count - 1 do
  begin
    xXml := aXml.Items.XmlItems [x];
    y := 0;
    yXml := nil;
    cXml := nil;
    while (y < Items.Count)
    and (yXml = nil)
    do begin
      if (  NameWithoutPrefix (Items.XmlItems [y].TagName)
          = NameWithoutPrefix (xXml.TagName)
         ) then
      begin
        cXml := Items.XmlItems [y];
        if (Items.XmlItems [y].isProcessed = False) then
          yXml := Items.XmlItems [y];
      end;
      Inc (y);
    end;
    if yXml <> nil then
      yXml.LoadValues (xXml, aAddUnknowns, aOnlyWhenChecked, aCopyCheckers)
    else
    begin
      nXml := nil;
      if cXml <> nil then
      begin
        if Assigned (cXml.Xsd) then
          nXml := TXml.Create(0, cXml.Xsd)
        else
        begin
          nXml := TXml.Create;
          nXml.TagName := xXml.TagName;
        end;
        nXml.Parent := self;
        Items.InsertObject( Items.IndexOfObject (cXml) + 1
                                      , nXml.TagName
                                      , nXml
                                      );
        nXml.LoadValues(xXml, aAddUnknowns, aOnlyWhenChecked, aCopyCheckers);
      end
      else
      begin
        if aAddUnknowns then
        begin
          nXml := TXml.Create;
          nXml.TagName := xXml.TagName;
          AddXml(nXml);
          nXml.LoadValues(xXml, aAddUnknowns, aOnlyWhenChecked, aCopyCheckers);
        end;
      end;
    end;
  end; {for every xml.item}
  for y := 0 to aXml.Attributes.Count - 1 do
  begin
    if aXml.Attributes.XmlAttributes[y].Name = CheckedAtttributeName then
      fChecked := aXml.Attributes.XmlAttributes[y].ValueAsBoolean
    else
    begin
      if (   (not aOnlyWhenChecked)
          or aXml.Attributes.XmlAttributes [y].Checked
         )
      and (  (not Assigned (Xsd))
           or (    (not aXml.Attributes.XmlAttributes [y].isXmlNsAttribute)
               and (not aXml.Attributes.XmlAttributes [y].isXmlTypeDefAttribute)
              )
          )
      then
      begin
        _Checked := True;
        _Name := NameWithoutPrefix (aXml.Attributes.XmlAttributes [y].Name);
        if AnsiStartsStr (UncheckedAtttributeName, _Name) then
        begin
          _Checked := False;
          _Name := StuffString(_Name, 1, Length(UncheckedAtttributeName), '');
        end;
        xXmlAttr := nil;
        for x := 0 to Attributes.Count - 1 do
        begin
          if NameWithoutPrefix (Attributes.XmlAttributes [x].Name)
           = _Name then
            xXmlAttr := Attributes.XmlAttributes[x];
        end;
        if (not Assigned (xXmlAttr))
        and aAddUnknowns then
        begin
          xXmlAttr := TXmlAttribute.CreateAsString(_Name, '');
          AddAttribute (xXmlAttr);
        end;
        if Assigned (xXmlAttr) then
        begin
          xXmlAttr.Value := aXml.Attributes.XmlAttributes [y].Value;
          xXmlAttr.Checked := _Checked;
        end;
      end;
    end;
  end;
  isProcessed := True;
end;

procedure TXml.LoadValues(aXml: TXml; aAddUnknowns, aOnlyWhenChecked: Boolean);
begin
  LoadValues(aXml, aAddUnknowns, aOnlyWhenChecked, True);
end;

procedure TXml.LoadValues(aXml: TXml; aAddUnknowns: Boolean);
begin
  LoadValues(aXml, aAddUnknowns, False, True);
end;

{ TXmlAttribute }


function TXmlAttribute.IsMoveUpPossible: Boolean;
begin
  if Assigned (XsdAttr) then
    raise Exception.Create('IsMoveUpPossible not yet possible with XSD');
  result := False;
  if Assigned (Parent) then
    if ((Parent as TXml).Attributes.IndexOfObject(Self) > 0) then
      result := True;
end;

procedure TXmlAttribute.MoveUp;
var
  x: Integer;
  xXml: TXml;
  xAttr: TXmlAttribute;
  s: String;
begin
  xXml := Parent as TXml;
  x := xXml.Attributes.IndexOfObject(Self);
  if x < 1 then
    raise Exception.Create ( 'Move up (Attr: '
                           + IntToStr (x)
                           + ') index out of bound'
                           );
  xAttr := xXml.Attributes.XmlAttributes [x - 1];
  xXml.Attributes.Objects [x - 1] :=
    xXml.Attributes.Objects [x];
  xXml.Attributes.Objects [x] := xAttr;
  s := xXml.Attributes.Strings [x - 1];
  xXml.Attributes.Strings [x - 1] := xXml.Attributes.Strings [x];
  xXml.Attributes.Strings [x] := s;
end;

procedure TXmlAttribute.PutStringData(aString: String);
begin
  if Self = nil then Exit;
  if aString = bindNilStr then
  begin
    Value := '';
    Checked := False;
  end
  else
  begin
    Value := aString;
    Checked := True;
    Parent.Checked := True;
  end;
end;

function TXmlAttribute.IsMoveDownPossible: Boolean;
begin
  if Assigned (XsdAttr) then
    raise Exception.Create('IsMoveDownPossible not yet possible with XSD');
  result := False;
  if Assigned (Parent) then
    if ((Parent as TXml).Attributes.IndexOfObject(Self) <
       ((Parent as TXml).Attributes.Count - 1)) then
      result := True;
end;

procedure TXmlAttribute.MoveDown;
var
  x: Integer;
  xXml: TXml;
  xAttr: TXmlAttribute;
  s: String;
begin
  xXml := Parent as TXml;
  x := xXml.Attributes.IndexOfObject(Self);
  if (x > xXml.Attributes.Count - 2) then
    raise Exception.Create ( 'Move down (Attr: '
                           + IntToStr (x)
                           + ') index out of bound'
                           );
  xAttr := xXml.Attributes.XmlAttributes [x + 1];
  xXml.Attributes.Objects [x + 1] :=
    xXml.Attributes.Objects [x];
  xXml.Attributes.Objects [x] := xAttr;
  s := xXml.Attributes.Strings [x + 1];
  xXml.Attributes.Strings [x + 1] := xXml.Attributes.Strings [x];
  xXml.Attributes.Strings [x] := s;
end;

function TXmlAttribute.IsValueValidAgainstXsd(var aMessageString: String): Boolean;
var
  aXsdDataType: TXsdDataType;
begin
  if Assigned (XsdAttr) then
  begin
    aXsdDataType := XsdAttr as TXsdDataType;
    result := aXsdDataType.IsValidValue(Name, Value, aMessageString);
  end
  else
    result := True;
end;

function TXml.IsMoveUpPossible: Boolean;
begin
  result := False;
  if Assigned (Xsd) then
{
    raise Exception.Create('IsMoveUpPossible not yet possible with XSD');
}
    exit;
  if Assigned (Parent) then
    if ((Parent as TXml).Items.IndexOfObject(Self) > 0) then
      result := True;
end;

function TXml.IsMoveDownPossible: Boolean;
begin
  result := False;
  if Assigned (Xsd) then
{
    raise Exception.Create('IsMoveDownPossible not yet possible with XSD');
}
    exit;
  if Assigned (Parent) then
    if ((Parent as TXml).Items.IndexOfObject(Self) <
       ((Parent as TXml).Items.Count - 1)) then
      result := True;
end;

procedure TXml.MoveUp;
var
  x: Integer;
  pXml: TXml;
  xXml: TXml;
  s: String;
begin
  pXml := Parent as TXml;
  x := pXml.Items.IndexOfObject(Self);
  if x < 1 then
    raise Exception.Create ( 'Move up (Element: '
                           + IntToStr (x)
                           + ') index out of bound'
                           );
  xXml := pXml.Items.XmlItems [x - 1];
  pXml.Items.Objects [x - 1] :=
    pXml.Items.Objects [x];
  pXml.Items.Objects [x] := xXml;
  s := pXml.Items.Strings [x - 1];
  pXml.Items.Strings [x - 1] := pXml.Items.Strings [x];
  pXml.Items.Strings [x] := s;
end;

procedure TXml.MoveDown;
var
  x: Integer;
  pXml: TXml;
  xXml: TXml;
  s: String;
begin
  pXml := Parent as TXml;
  x := pXml.Items.IndexOfObject(Self);
  if (x > pXml.Items.Count - 2) then
    raise Exception.Create ( 'Move down (Element: '
                           + IntToStr (x)
                           + ') index out of bound'
                           );
  xXml := pXml.Items.XmlItems [x + 1];
  pXml.Items.Objects [x + 1] :=
    pXml.Items.Objects [x];
  pXml.Items.Objects [x] := xXml;
  s := pXml.Items.Strings [x + 1];
  pXml.Items.Strings [x + 1] := pXml.Items.Strings [x];
  pXml.Items.Strings [x] := s;
end;

function TXml.IsValueValidAgainstXsd(var aMessageString: String): Boolean;
begin
  result := (TypeDef = nil)
         or TypeDef.IsValidValue(Name, Value, aMessageString);
end;

procedure TXml.CheckAllAttributes(aChecked: Boolean);
var
  x: Integer;
begin
  for x := 0 to Attributes.Count - 1 do
    Attributes.XmlAttributes [x].Checked := aChecked;
  for x := 0 to Items.Count - 1 do
    Items.XmlItems [x].CheckAllAttributes (aChecked);
end;

procedure TXml.Populate(aViewType: TxvViewType);
  procedure _Populate (aXml: TXml);
  var
    x: Integer;
  begin
    if (aXml.Value = '')
    and Assigned (aXml.Xsd)
    and (   (aViewType = xvAll)
         or (aXml.Xsd.isRequired)
        ) then
    begin
      for x := 0 to aXml.Items.Count - 1 do
        _Populate (aXml.Items.XmlItems [x]);
      aXml.Value := aXml.TypeDef.populateValue (aXml.TagName);
      aXml.Checked := True;
    end;
  end;
var
  x: Integer;
begin
{
  for x := 0 to Attributes.Count - 1 do
    Attributes.XmlAttributes [x].Checked := False;
}
  if (Value = '')
  and Assigned (Xsd) then
  begin
    for x := 0 to Items.Count - 1 do
      _Populate (Items.XmlItems [x]);
    Value := TypeDef.populateValue (TagName);
    Checked := True;
  end;
end;

procedure TXml.Reset;
var
  x: Integer;
begin
  for x := 0 to Attributes.Count - 1 do
    Attributes.XmlAttributes [x].Checked := False;
  for x := 0 to Items.Count - 1 do
    Items.XmlItems [x].Reset;
  Checked := False;
end;

procedure TXml.ResetValues;
var
  x: Integer;
begin
  if Self = nil then Exit;
  for x := 0 to Attributes.Count - 1 do
  begin
    Attributes.XmlAttributes [x].Checked := False;
    Attributes.XmlAttributes [x].Value := '';
  end;
  for x := 0 to Items.Count - 1 do
    Items.XmlItems [x].ResetValues;
  Checked := False;
  Value := '';
end;

procedure TXml.Clean(aMinOccurs, aMinRecursive: Integer);
  procedure _Clean (aXml: TXml);
  var
    x, y: Integer;
  begin
    for x := aXml.Items.Count - 1 downto 0 do
    begin
      if aXml.Items.XmlItems[x].Checked then
        _Clean(aXml.Items.XmlItems[x])
      else
      begin
        if (aXml.Items.XmlItems[x].IndexOfRepeatableItem >= aMinOccurs) then
        begin
          aXml.Items.XmlItems[x].Free;
          aXml.Items.Delete(x);
        end
        else
        begin
          if (aXml.Items.XmlItems[x].IndexOfRecursiveItem >= aMinRecursive) then
          with aXml.Items.XmlItems[x] do
          begin
            for y := Items.Count - 1 downto 0 do
            begin
              Items.XmlItems[y].Free;
              Items.Delete(y);
            end;
          end
          else
          begin
            aXml.Items.XmlItems[x].ResetValues;
            _Clean(aXml.Items.XmlItems[x]);
          end;
        end;
      end;
    end;
    if not aXml.Checked then
      aXml.Value := '';
  end;
begin
  if aMinOccurs < 1 then raise Exception.Create('TXml.Clean(aMinOccurs, aMinRecursive: Integer): aMinOccurs < 1');
  if aMinRecursive < 1 then raise Exception.Create('TXml.Clean(aMinOccurs, aMinRecursive: Integer): aMinRecursive < 1');
  _Clean(Self);
end;

function TXml.IndexOfRepeatableItem: Integer;
var
  x: Integer;
begin
  result := 0;
  if not Assigned (Parent) then Exit;
  for x := 0 to (Parent as TXml).Items.Count - 1 do
    if (Parent as TXml).Items.XmlItems [x] = Self then
      Exit
    else
      if (Parent as TXml).Items.XmlItems [x].TagName = TagName then
        Inc (result);
end;

function TXml.IndexOfRecursiveItem: Integer;
var
  pXml: TXml;
begin
  result := 0;
  pXml := Parent as TXml;
  while Assigned (pXml) do
  begin
    if (pXml.Xsd = self.Xsd)
    and (pXml.Name = self.Name) then
      Inc (result);
    pXml := pXml.Parent as TXml;
  end;
end;

function TXml.NumberOfSubItemsWithTag(aTag: String; OnlyWhenChecked: Boolean): Integer;
var
  x: Integer;
begin
  result := 0;
  for x := 0 to Items.Count - 1 do
    if Items.XmlItems [x].TagName = aTag then
      if (not OnlyWhenChecked)
      or (Items.XmlItems [x].Checked)
      then
        Inc (result);
end;

procedure TXml.XsdCreate (aLevel: Integer; aXsd: TXsd);
var
  ChildXML: TXml;
  xChildIndex: Integer;
  xAttr: TXmlAttribute;
  xAttrIndex: Integer;
  xOccurs, minOccurs, maxOccurs: Integer;
  xDataType: TXsdDataType;
begin
  try
    if not Assigned (aXsd) then Exit;
    if not Assigned (TypeDef) then Exit;
    CData := False;
    Items.Clear;
    Attributes.Clear;
    Xsd := aXsd;
    xDataType := TypeDef;
    jsonType := xDataType.jsonType;
    NameSpace := aXsd.ElementNameSpace;
    for xAttrIndex := 0 to xDataType.AttributeDefs.Count - 1 do
    begin
      xAttr := TXmlAttribute.Create;
      xAttr.Name := xDataType.AttributeDefs.XsdAttrs [xAttrIndex].Name;
      xAttr.Value := '';
      xAttr.Checked := False;
      xAttr.XsdAttr := xDataType.AttributeDefs.XsdAttrs [xAttrIndex];
      AddAttribute (xAttr);
    end;
    if xDataType._DepthBillOfMaterial >= xsdMaxDepthBillOfMaterials then
      exit;
    if aLevel > xsdMaxDepthXmlGen then
      exit;
    Inc (xDataType._DepthBillOfMaterial);
    try
      for xChildIndex := 0 to xDataType.ElementDefs.Count - 1 do
      begin
        minOccurs := StrToIntDef (xDataType.ElementDefs.Xsds [xChildIndex].minOccurs, 1);
        if xDataType.ElementDefs.Xsds [xChildIndex].maxOccurs = 'unbounded' then
          maxOccurs := aXsd.XsdDescr.xsdElementsWhenRepeatable
        else
          maxOccurs := StrToIntDef (xDataType.ElementDefs.Xsds [xChildIndex].maxOccurs, 1);
        if minOccurs < 1 then
          minOccurs := 1; {Create xml even if optional}
        xOccurs := aXsd.XsdDescr.xsdElementsWhenRepeatable;
        if minOccurs > xOccurs then
          xOccurs := minOccurs;
        if maxOccurs < xOccurs then
          xOccurs := maxOccurs;
        while xOccurs > 0 do
        begin
          ChildXml := TXml.Create (aLevel + 1, xDataType.ElementDefs.Xsds [xChildIndex]);
          Items.AddObject(ChildXml.TagName, ChildXml);
          ChildXml.Parent := self;
          Dec (xOccurs);
        end; {while xOccurs > 0}
      end; {for each elementdef of xsd}
    finally
      Dec (xDataType._DepthBillOfMaterial);
    end;
  except
    on e: exception do
      raise Exception.CreateFmt ( 'TXml.XsdCreate (aLevel: %d; aXsd: %s): %s'
                                , [aLevel, aXsd.ElementName, e.Message + LineEnding]
                                );
  end;
end;

constructor TXml.Create(aLevel: Integer; aXsd: TXsd);
begin
  if not Assigned (aXsd) then
    raise Exception.Create ('Create(aXsd: TXsd): Argument is nil');
  inherited Create;
  jsonType := jsonNone;
  Checked := False;
  CData := False;
  Items := TXmlList.Create;
  Attributes := TXmlAttributeList.Create;
  TagName := aXsd.ElementName;
  TypeDef := aXsd.sType;
  XsdCreate(aLevel, aXsd);
end;

function TXmlList.getXmlBooleanByTag(Index: String): Boolean;
var
  xXml: TXml;
begin
  xXml := getXmlItemByTag(Index);
  if Assigned (xXml) then
    result := (xXml.Value = 'true')
           or (xXml.Value = '1')
  else
    result := False;
end;

function TXmlList.getXmlBooleanByTagDef(Index: String;
  aDefault: Boolean): Boolean;
var
  xXml: TXml;
begin
  xXml := getXmlItemByTag(Index);
  if Assigned (xXml) then
    result := (xXml.Value = 'true')
           or (xXml.Value = '1')
  else
    result := aDefault;
end;

function TXmlList.getXmlCheckedBooleanByTag(Index: String): Boolean;
var
  xXml: TXml;
begin
  xXml := getXmlItemByTag(Index);
  if Assigned (xXml)
  and xXml.Checked then
    result := (xXml.Value = 'true')
           or (xXml.Value = '1')
  else
    result := False;
end;

function TXmlList.getXmlCheckedBooleanByTagDef(Index: String;
  aDefault: Boolean): Boolean;
var
  xXml: TXml;
begin
  xXml := getXmlItemByTag(Index);
  if Assigned (xXml)
  and xXml.Checked then
    result := (xXml.Value = 'true')
           or (xXml.Value = '1')
  else
    result := aDefault;
end;

function TXmlList.getXmlCheckedIntegerByTag(Index: String): Integer;
var
  xXml: TXml;
begin
  xXml := getXmlItemByTag(Index);
  if Assigned (xXml)
  and xXml.Checked then
    result := StrToIntDef (xXml.Value, 0)
  else
    result := 0;
end;

function TXmlList.getXmlCheckedIntegerByTagDef(Index: String;
  aDefault: Integer): Integer;
var
  xXml: TXml;
begin
  xXml := getXmlCheckedItemByTag(Index);
  if Assigned (xXml) then
    result := StrToIntDef (xXml.Value, aDefault)
  else
    result := aDefault;
end;

function TXmlList.getXmlCheckedItemByTag(Index: String): TXml;
var
  x: Integer;
begin
  result := nil;
  for x := 0 to Count - 1 do
  begin
    if (NameWithoutPrefix(XmlItems[x].TagName) = Index) then
    begin
      if XmlItems[x].Checked then
        result := XmlItems [x];
      exit;
    end;
  end;
end;

function TXmlList.getXmlCheckedValueByTag(Index: String): String;
var
  xXml: TXml;
begin
  xXml := getXmlCheckedItemByTag(Index);
  if Assigned (xXml) then
    result := xXml.Value
  else
    result := '';
end;

function TXmlList.getXmlCheckedValueByTagDef(Index, aDefault: String): String;
var
  xXml: TXml;
begin
  xXml := getXmlCheckedItemByTag(Index);
  if Assigned (xXml) then
    result := xXml.Value
  else
    result := aDefault;
end;

function TXmlList.getXmlIntegerByTag(Index: String): Integer;
var
  xXml: TXml;
begin
  xXml := getXmlItemByTag(Index);
  if Assigned (xXml) then
    result := StrToIntDef (xXml.Value, 0)
  else
    result := 0;
end;

function TXmlList.getXmlIntegerByTagDef(Index: String;
  aDefault: Integer): Integer;
var
  xXml: TXml;
begin
  xXml := getXmlItemByTag(Index);
  if Assigned (xXml) then
    result := StrToIntDef (xXml.Value, aDefault)
  else
    result := aDefault;
end;

function TXmlList.getXmlItemByTag(Index: String): TXml;
var
  x: Integer;
begin
  result := nil;
  for x := 0 to Count - 1 do
  begin
    if (NameWithoutPrefix(XmlItems[x].TagName) = Index) then
    begin
      result := XmlItems [x];
      exit;
    end;
  end;
end;

function TXmlList.getCheckedXmlItemByTag(Index: String): TXml;
var
  x: Integer;
begin
  result := nil;
  for x := 0 to Count - 1 do
  begin
    if (NameWithoutPrefix(XmlItems[x].TagName) = Index) then
    begin
      result := XmlItems [x];
      if not result.Checked then
        result := nil;
      exit;
    end;
  end;
end;

procedure TXmlList.setXmlCheckedValueByTag(Index: String; const Value: String);
var
  xXml: TXml;
begin
  xXml := getXmlItemByTag(Index);
  if Assigned (xXml) then
    xXml.Value := Value
  else
  begin
    xXml := TXml.CreateAsString(Index, Value);
    AddObject(Value, xXml);
  end;
  xXml.Checked := True;
end;

procedure TXmlList.setXmlValueByTag(Index: String; const aValue: String);
var
  xXml: TXml;
begin
  xXml := getXmlItemByTag(Index);
  if Assigned (xXml) then
    xXml.Value := aValue
  else
  begin
    xXml := TXml.CreateAsString(Index, aValue);
    AddObject(aValue, xXml);
  end;
end;

function TXmlList.getXmlValueByTag(Index: String): String;
var
  xXml: TXml;
begin
  xXml := getXmlItemByTag(Index);
  if Assigned (xXml) then
    result := xXml.Value
  else
    result := '';
end;

function TXmlList.getXmlValueByTagDef(Index, aDefault: String): String;
var
  xXml: TXml;
begin
  xXml := getXmlItemByTag(Index);
  if Assigned (xXml) then
    result := xXml.Value
  else
    result := aDefault;
end;

procedure TXmlList.SetXml(Index: integer; const Value: TXml);
begin
  Objects [index]:= Value;
end;

function TXml.GetIndexCaption: String;
  function _Index: String;
  var
    i, x: Integer;
    p: TXml;
  begin
    result := '';
    if Assigned (Parent) then
    begin
      i := 0;
      p := Parent as TXml;
      for x := 0 to p.Items.Count - 1 do
      begin
        if p.Items.XmlItems[x].TagName = TagName then
          Inc (i);
        if p.Items.XmlItems[x] = Self then
        begin
          if i > 1 then
            result := '[' + IntToStr (i) + ']';
          exit;
        end;
      end;
    end;
  end;
begin
  result := '';
  if Self = nil then
    exit;
  result := NameWithoutPrefix (TagName) + _Index;
end;

function TXml.GetCaption: String;
begin
  result := '';
  if Self = nil then
    exit;
  result := TagName; {wanna add a seqnumber}
end;

function TXml.GetUQCaption: String;
begin
  result := NameWithoutPrefix (GetCaption);
end;

function TXml.GetFullUQCaption: String;
begin
  result := '';
  if Self = nil then
    exit;
  if Parent = nil then
    result := NameWithoutPrefix (GetCaption)
  else
    result := (Parent as TXml).GetFullUQCaption
            + '.'
            + NameWithoutPrefix (GetCaption);
end;

function TXml.GetFullIndexCaption: String;
begin
  result := '';
  if Self = nil then
    exit;
  if Parent = nil then
    result := GetIndexCaption
  else
    result := (Parent as TXml).GetFullIndexCaption
            + '.'
            + GetIndexCaption;
end;

function TXml.GetFullCaption: String;
begin
  result := '';
  if Self = nil then
    exit;
  if Parent = nil then
    result := GetCaption
  else
    result := (Parent as TXml).GetFullCaption
            + '.'
            + GetCaption
            + GetIndexString
            ;
end;

function TXml.FindUQBind(aName: String): TCustomBindable;
var
  x: Integer;
  y: Integer;
  xName: String;
  newName: String;
begin
  result := nil;
  x := Pos ('.', aName);
  if x = 0 then
    xName := NameWithoutPrefix (aName)
  else
    xName := NameWithoutPrefix (Copy (aName, 1, x - 1));
  if (xName = NameWithoutPrefix(TagName))
  or (xName = GetIndexCaption)
  then
  begin
    if x = 0 then
      result := self
    else
    begin
      newName := Copy(aName, x + 1, Length (aName));
      for y := 0 to Attributes.Count - 1 do
      begin
        if Attributes.XmlAttributes[y].Name = newName then
        begin
          result := Attributes.XmlAttributes[y];
          Exit;
        end;
      end;
      ExtendRecursivity;
      for y := 0 to Items.Count - 1 do
      begin
        result := Items.XmlItems [y].FindUQBind(newName);
        if result <> nil then
          exit;
      end;
    end;
  end;
end;

function TXml.FindUQValue(aName: String): String;
var
  x: Integer;
  y: Integer;
  xName: String;
  newName: String;
begin
  result := '';
  x := Pos ('.', aName);
  if x = 0 then
    xName := NameWithoutPrefix (aName)
  else
    xName := NameWithoutPrefix (Copy (aName, 1, x - 1));
  if (xName = NameWithoutPrefix(TagName))
  or (xName = GetIndexCaption)
  then
  begin
    if x = 0 then
      result := Value
    else
    begin
      newName := Copy(aName, x + 1, Length (aName));
      if newName[1] = '@' then
      begin
        newName := Copy (NewName, 2, 3000);
        for y := 0 to Attributes.Count - 1 do
        begin
          if Attributes.XmlAttributes [y].Name = newName then
          begin
            result := Attributes.XmlAttributes [y].Value;
            exit;
          end;
        end;
      end
      else
      begin
        for y := 0 to Items.Count - 1 do
        begin
          result := Items.XmlItems [y].FindUQValue(newName);
          if result <> '' then
            exit;
        end;
      end;
    end;
  end;
end;

function TXml.FindUQXml(aName: String): TXml;
var
  x: Integer;
  y: Integer;
  xName: String;
  newName: String;
begin
  result := nil;
  x := Pos ('.', aName);
  if x = 0 then
    xName := NameWithoutPrefix (aName)
  else
    xName := NameWithoutPrefix (Copy (aName, 1, x - 1));
  if (xName = NameWithoutPrefix(TagName))
  or (xName = GetIndexCaption)
  or (xName = '*')
  then
  begin
    if x = 0 then
      result := self
    else
    begin
      newName := Copy(aName, x + 1, Length (aName));
{
      for y := 0 to Attributes.Count - 1 do
      begin
        result := Attributes.XmlAttributes [y];
        if result <> nil then
          exit;
      end;
}
      for y := 0 to Items.Count - 1 do
      begin
        result := Items.XmlItems [y].FindUQXml(newName);
        if result <> nil then
          exit;
      end;
    end;
  end;
end;

function TXml.FindCheckedXml(aName: String): TXml;
var
  x: Integer;
  y: Integer;
  xName: String;
  newName: String;
begin
  result := nil;
  if not Checked then
    Exit;
  x := Pos ('.', aName);
  if x = 0 then
    xName := aName
  else
    xName := Copy (aName, 1, x - 1);
  if (xName = TagName)
  or (xName = '*')
  then begin
    if x = 0 then
      result := self
    else
    begin
      newName := Copy(aName, x + 1, Length (aName));
      for y := 0 to Items.Count - 1 do
      begin
        result := Items.XmlItems [y].FindCheckedXml(newName);
        if result <> nil then
          exit;
      end;
    end;
  end;
end;

function TXml.FindXml(aName: String): TXml;
var
  x: Integer;
  y: Integer;
  xName: String;
  newName: String;
begin
  result := nil;
  x := Pos ('.', aName);
  if x = 0 then
    xName := aName
  else
    xName := Copy (aName, 1, x - 1);
  if (xName = TagName)
  or (xName = '*')
  then begin
    if x = 0 then
      result := self
    else
    begin
      newName := Copy(aName, x + 1, Length (aName));
      for y := 0 to Items.Count - 1 do
      begin
        result := Items.XmlItems [y].FindXml(newName);
        if result <> nil then
          exit;
      end;
    end;
  end;
end;

procedure TXml.TakeOver(aXml: TXml);
var
  x: Integer;
begin
  TagName := aXml.TagName;
  CData := aXml.CData;
  jsonType := aXml.jsonType;
  Value := aXml.Value;
  DoExpectValue := aXml.DoExpectValue;
  ExpectedValue := aXml.ExpectedValue;
//Parent := aXml.Parent;
  Items.Clear;
  Items.Free;
  Items := aXml.Items;
  for x := 0 to Items.Count - 1 do
    Items.XmlItems [x].Parent := self;
  aXml.Items := TXmlList.Create;
  Attributes.Clear;
  Attributes.Free;
  Attributes := aXml.Attributes;
  for x := 0 to Attributes.Count - 1 do
    Attributes.XmlAttributes [x].Parent := self;
  aXml.Attributes := TXmlAttributeList.Create;
  Checked := aXml.Checked;
  Xsd := aXml.Xsd;
end;

function TXml.DeleteChild(aXml: TXml): TXml;
var
  x: Integer;
begin
  x := Items.IndexOfObject(aXml);
  if x < 0 then
    raise Exception.Create('Passed XML object not a child of addressed XML object');
  aXml.Free;
  Items.Delete(x);
  if Items.Count = 0 then
    result := nil
  else
  begin
    if x < Items.Count then
      result := Items.XmlItems [x]
    else
      result := Items.XmlItems [x - 1];
  end;
end;

procedure TXml.DeleteAttribute(aAttr: TXmlAttribute);
var
  x: Integer;
begin
  x := Attributes.IndexOfObject(aAttr);
  if x < 0 then
    raise Exception.Create('Passed XML Attribute does not belong to addressed XML object');
  aAttr.Free;
  Attributes.Delete(x);
end;

function TXml.GetGroup: Boolean;
begin
  result := (Items.Count > 0)
end;

function TXml.PrefixToNameSpace(aPrefix: String): String;
  function _ResolveNamespace (aXml: TXml; aNsPrefix: String): String;
  var
    nsAttr: TXmlAttribute;
  begin
    result := '';
    if not Assigned(aXml)
    or (aNsPrefix = '') then
      exit;
    nsAttr := aXml.Attributes.AttributeByTag['xmlns:' + aNsPrefix];
    if Assigned (nsAttr) then
      result := nsAttr.Value
    else
      result := _ResolveNamespace(aXml.Parent as TXml, aNsPrefix);
  end;
var
  p: Integer;
begin
  p := Pos (':', aPrefix);
  if p < 1 then
    result := _ResolveNamespace(self, aPrefix)
  else
    result := _ResolveNamespace(self, Copy (aPrefix, 1, p - 1));
end;

procedure TXml.NamespacesToPrefixes (aOnlyWhenChecked: Boolean; aSl: TStringList);
  procedure _scanForNs (aXml: TXml; aParentNs: String);
  var
    x: Integer;
  begin
    if (not aXml.Checked)
    and (aOnlyWhenChecked) then
      exit;
    if aXml.NameSpace <> aParentNs then
      aSl.Add (aXml.NameSpace);
    for x := 0 to aXml.Items.Count - 1 do
      _scanForNs(aXml.Items.XmlItems[x], aXml.NameSpace);
  end;
  procedure _fillNs (aXml: TXml; parentPrefix, parentNameSpace: String);
  var
    x, f: Integer;
  begin
    if (not aXml.Checked)
    and (aOnlyWhenChecked) then
      exit;
    if aXml.NameSpace = parentNameSpace then
      aXml.NsPrefix := parentPrefix
    else
    begin
      if aSl.Find(aXml.NameSpace, f) then
        aXml.NsPrefix := 'ns' + IntToStr(f + 1)
      else
        aXml.NsPrefix := '';
    end;
    for x := 0 to aXml.Items.Count - 1 do
      _fillNs(aXml.Items.XmlItems[x], aXml.NsPrefix, aXml.NameSpace);
  end;
begin
  if not Assigned (aSl) then
    raise Exception.Create ('TXml.NamespacesToPrefixes (aOnlyWhenChecked: Boolean; aSl: TStringList):: aSl not asigned');
  aSl.Clear;
  aSl.Sorted := True;
  aSl.Duplicates := dupIgnore;
  _scanForNs (self, '');
  _fillNs (self, '', '');
end;

function TXml.ExpandPrefixedName (aDefaultNS, aName: String): String ;
var
  p: Integer;
  ns: String;
begin
  result := '';
  if aName = '' then Exit;
  p := Pos (':', aName);
  if p < 1 then
    result := aDefaultNS + ';' + aName
  else
  begin
    ns := PrefixToNameSpace(Copy (aName, 1, p - 1));
    if ns = '' then
      ns := aDefaultNS;
    result := ns + ';' + Copy(aName, p + 1, Length (aName));
  end;
end;

procedure TXml.setChecked(const aValue: Boolean);
var
  x: Integer;
  xParent: TXml;
begin
  {
    if we are working with xsd's, the upline is checked when self is checked
    if self is a element in a choice, siblings with another xsd are unchecked
  }
  if (aValue)
  and (Assigned (Parent))
  and (Parent is TXml) then
  begin
    xParent := Parent as TXml;
    if (Assigned (xParent.Xsd)) then
    begin
      if xParent.TypeDef.ContentModel = 'Choice' then
      begin
        for x := 0 to xParent.Items.Count - 1 do
          if xParent.Items.XmlItems [x].Xsd <> Xsd then
            xParent.Items.XmlItems [x].Checked := False;
      end;
    end;
  end;
  if (aValue)
  and (Assigned (Parent)) then
    Parent.Checked := True;
  fPrevChecked := fChecked;
  fChecked := aValue;
end;

procedure TXml.SetFileNamesRelative(aFileName: String);
var
  x: Integer;
begin
  if Assigned (TypeDef) then
  begin
    if TypeDef.Name = 'FileNameType' then
      Value := ExtractRelativeFileName (aFileName, Value)
    else
    begin
      for x := 0 to Items.Count - 1 do
        Items.XmlItems[x].SetFileNamesRelative(aFileName);
    end;
  end;
end;

function TXml.AddXml(aChildXml: TXml): TXml;
begin
  if Assigned (aChildXml) then
  begin
    Items.AddObject(aChildXml.TagName, aChildXml);
    aChildXml.Parent := self;
  end;
  result := aChildXml;
end;

procedure TXml.InsertXml(aIndex: Integer; aXml: TXml);
begin
  Items.InsertObject(aIndex, aXml.TagName, aXml);
  aXml.Parent := self;
end;

procedure TXml.InsertAttribute(aIndex: Integer; aAttr: TXmlAttribute);
begin
  Attributes.InsertObject(aIndex, aAttr.Name, aAttr);
  aAttr.Parent := self;
end;

procedure TXml.AddAttribute(aAttr: TXmlAttribute);
begin
  Attributes.AddObject(aAttr.Name, aAttr);
  aAttr.Parent := self;
end;

function TXml.getText: String;
begin
  result := StreamXML ( True
                      , True
                      , 0
                      , False
                      , False
                      );
end;

procedure TXml.setText(const aValue: String);
begin
  LoadFromString (aValue, nil);
end;

function TXml.getEncodedValue: String;
var
  x: Integer; // loops thru characters in string
begin
  Result := '';
  for x := 1 to Length(Value) do
  begin
    {
    if (    (Value [x] = #13)
        and (x < Length (Value))
        and (Value [x + 1] = #10)
       )
    or (    (Value [x] = #10)
        and (x > 1)
        and (Value [x - 1] = #13)
       ) then
      Result := Result + Value[x]
    else }
    begin
      case Value[x] of
        ' '..'!': Result := Result + Value[x];
        '"': result := result + '&quot;';
        '#'..'%': Result := Result + Value[x];
        '&': result := result + '&amp;';
        '''': result := result + '&apos;';
        '('..';': Result := Result + Value[x];
        '<': result := result + '&lt;';
        '=': Result := Result + Value[x];
        '>': result := result + '&gt;';
        '?'..'~': Result := Result + Value[x];
        else
          Result := Result + '&#x' + SysUtils.IntToHex(Ord(Value[x]), 2) + ';';
      end;
    end;
  end;
end;

function TXml.getCDataValue: String;
begin
  if (Items.Count > 0)
  and (Items.XmlItems [0].CData) then
    result := Items.XmlItems [0].Value
  else
    result := Value;
end;

procedure TXml.Bind(aRoot: String; aExpress: TObject; aMaxOccurrences: Integer);
var
  x: Integer;
  xSubItems: TXmlList;
  xRoot: String;
begin
  if Self = nil then Exit;
  xRoot := aRoot;
  if xRoot <> '' then
    xRoot := xRoot + '.';
  if Assigned ((aExpress as TExpress).FindBind (xRoot + FullIndexCaption)) then
    exit;
  if Group = True then
  begin
    (aExpress as TExpress).BindGroupObject (xRoot + FullIndexCaption, Self);
    xSubItems := Items as TXmlList;
    for x := 0 to xSubItems.Count - 1 do
    begin
      xSubItems.XmlItems [x].Bind (aRoot, aExpress, aMaxOccurrences);
    end;
  end
  else
  begin
{
    if UpperCase (Caption) <> 'FILLER' then
    begin
      if Numeric then
        aExpress.BindExtendedObject (xRoot + FullCaption, Self)
      else
}
        (aExpress as TExpress).BindStringObject (xRoot + FullIndexCaption, Self);
{
    end;
}
  end;
  for x := 0 to Attributes.Count - 1 do
    Attributes.XmlAttributes[x].Bind(xRoot + FullCaption, aExpress, aMaxOccurrences);
end;

function TXml.getTagName: String;
begin
   result := Name;
end;

procedure TXml.setTagName(const aValue: String);
begin
  Name := aValue;
end;

procedure TXml.CopyValues(aXml: TXml; aDoReset, aSkipAssignments: Boolean);
var
  x: Integer;
  y: Integer;
  xXml: TXml;
  yXml: TXml; {xml with corr. tagname and unchecked}
  xMatch: Boolean;
begin
  if self = nil then exit;
  if not (self is TXml) then
    raise Exception.Create('is not an XML');
  if aXml = nil then
    raise Exception.Create ( 'Not valid XML data '
                           );
  if NameWithoutPrefix (TagName) <> NameWithoutPrefix (aXml.TagName) then
    exit;
  if aDoReset then
//  Reset;
  begin
    Checked := False;
    for x := 0 to Items.Count - 1 do
      Items.XmlItems[x].Checked := False; // no need to go deeper since this routine itself is recursive
      for x := 0 to Attributes.Count - 1 do
        Attributes.XmlAttributes[x].Checked := False;
  end;
  if not aXml.Checked then
    exit;
  Checked := True;
  if not (aSkipAssignments and (Copy (Value, 1, 2) = ':=')) then
    Value := aXml.Value;
  for x := 0 to aXml.Items.Count - 1 do
  begin
    if aXml.Items.XmlItems[x].Checked then
    begin
      xMatch := False;
      xXml := aXml.Items.XmlItems [x];
      for y := 0 to Items.Count - 1 do
      begin
        yXml := Items.XmlItems [y];
        if (not xMatch)
        and (  NameWithoutPrefix (yXml.TagName)
             = NameWithoutPrefix (xXml.TagName)
            )
        and (not yXml.Checked) then
        begin
          xMatch := True;
          yXml.CopyValues(xXml, aDoReset, aSkipAssignments);
        end;
      end;
    end;
  end; {for every xml.item}
  for x := 0 to Attributes.Count - 1 do
  begin
    for y := 0 to aXml.Attributes.Count - 1 do
    begin
      if (  NameWithoutPrefix (Attributes.XmlAttributes [x].Name)
          = NameWithoutPrefix (aXml.Attributes.XmlAttributes [y].Name)
         )
      and aXml.Attributes.XmlAttributes[y].Checked then
      begin
        Attributes.XmlAttributes [x].Value := aXml.Attributes.XmlAttributes [y].Value;
        Attributes.XmlAttributes [x].Checked := True;
      end;
    end;
  end;
end;

procedure TXml.PutGroupData(aObject: TObject);
var
  srcXml: TXml;
  swapName:String;
begin
  srcXml := TXml (aObject);
  Reset;
  swapName := srcXml.TagName;
  try
    srcXml.TagName := TagName;
    LoadValues (srcXml, False, True, False);
    Checked := True;
  finally
    srcXml.TagName := swapName;
  end;
end;

procedure TXml.ForgetXsd;
var
  x: Integer;
begin
  Xsd := Nil;
  for x := 0 to Items.Count - 1 do
    Items.XmlItems [x].ForgetXsd;
end;

constructor TXml.CreateAsTimeStamp (aTagName: String; aTimeStame: TDateTime); Overload;
begin
  inherited Create;
  jsonType := jsonNone;
  CData := False;
  Items := TXmlList.Create;
  Attributes := TXmlAttributeList.Create;
  TagName := aTagname;
  ValueAsTimeStamp := aTimeStame;
end;

constructor TXml.CreateAsBoolean(aTagName: String; aBoolean: Boolean);
begin
  inherited Create;
  jsonType := jsonNone;
  CData := False;
  Items := TXmlList.Create;
  Attributes := TXmlAttributeList.Create;
  TagName := aTagname;
  ValueAsBoolean := aBoolean;
end;

constructor TXmlAttribute.CreateAsInteger(aName: String; aInteger: Integer);
begin
  inherited Create;
  Name := aName;
  ValueAsInteger := aInteger;
end;

constructor TXml.CreateAsInteger(aTagName: String; aInteger: Integer);
begin
  inherited Create;
  jsonType := jsonNone;
  CData := False;
  Items := TXmlList.Create;
  Attributes := TXmlAttributeList.Create;
  TagName := aTagname;
  ValueAsInteger := aInteger;
end;

constructor TXml.CreateAsString(aTagName: String; aString: String);
begin
  inherited Create;
  jsonType := jsonNone;
  CData := False;
  Items := TXmlList.Create;
  Attributes := TXmlAttributeList.Create;
  TagName := aTagname;
  Value := aString;
end;

constructor TXmlAttribute.CreateAsString(aName, aString: String);
begin
  inherited Create;
  Name := aName;
  Value := aString;
end;

constructor TXmlAttribute.CreateAsBoolean(aName: String; aBoolean: Boolean);
begin
  inherited Create;
  Name := aName;
  if aBoolean then
    Value := 'true'
  else
    Value := 'false';
end;

procedure TXml.CheckExpectedValues;
  procedure _reset (aXml: TXml);
  var
    x: Integer;
  begin
    for x := 0 to aXml.Items.Count - 1 do
      _reset (aXml.Items.XmlItems [x]);
    aXml.HasUnExpectedValue := False;
  end;
  procedure _set (aXml: TXml);
  var
    x: Integer;
  begin
    for x := 0 to aXml.Items.Count - 1 do
      _set (aXml.Items.XmlItems [x]);
    if aXml.DoExpectValue then
    begin
      if aXml.Checked then
      begin
        if (aXml.Value <> aXml.ExpectedValue) then
          aXml.HasUnExpectedValue := True;
      end
      else
      begin
        if aXml.ExpectedValue <> bindNilStr then
          aXml.HasUnExpectedValue := True;
      end;
    end;
  end;
begin
  _reset (self);
  _set (self);
end;

procedure TXml.ResetExpectedValues;
  procedure _reset (aXml: TXml);
  var
    x: Integer;
  begin
    for x := 0 to aXml.Items.Count - 1 do
      _reset (aXml.Items.XmlItems [x]);
    aXml.HasUnExpectedValue := False;
    aXml.DoExpectValue := False;
    aXml.ExpectedValue := '';
  end;
begin
  _reset (self);
end;

function TXml.getRoot: TXml;
begin
  result := (inherited Root) as TXml;
end;

function TXml.GetStringData: String;
begin
  if Self = nil then Exit;
  if Checked then
    result := Value
  else
    result := bindNilStr;
end;

procedure TXml.PutStringData(aString: String);
var
  xMessage: String;
begin
  if Self = nil then Exit;
  if aString = bindNilStr then
  begin
    Value := '';
    Checked := False;
  end
  else
  begin
    Value := aString;
    if Assigned (Xsd)
    and (xsdValidateAssignmentsAgainstSchema)
    then
    begin
      if not IsValueValidAgainstXsd(xMessage) then
        raise Exception.Create(FullIndexCaption + ' (Assignment): ' + xMessage);
    end;
    Checked := True;
  end;
end;

procedure TXmlAttribute.Bind(aRoot: String; aExpress: TObject; aMaxOccurrences: Integer);
var
  xRoot: String;
begin
  xRoot := aRoot;
  if xRoot <> '' then
    xRoot := xRoot + '.';
  if Assigned ((aExpress as TExpress).FindBind (xRoot + Name)) then
    exit;
  (aExpress as TExpress).BindStringObject (xRoot + Name, Self);
end;

procedure TXml.CopyRelevancy(aXml: TXml);
{
  Create an Xml copy that can be saved with checked___ attribute
}
  procedure _Copy (dXml, sXml: TXml);
  var
    x: Integer;
    cXml: TXml;
    xAttr: TXmlAttribute;
  begin
    dXml.TagName := sXml.TagName;
    dXml.jsonType := sXml.jsonType;
    dXml.Value := sXml.Value;
    dXml.Xsd := sXml.Xsd;
    dXml.TypeDef := sXml.TypeDef;
    dXml.fChecked := sXml.fChecked;
    dXml.RefId := sXml.RefId;
    dXml.AddAttribute(TXmlAttribute.CreateAsBoolean(CheckedAtttributeName, sXml.Checked));
    for x := 0 to sXml.Attributes.Count - 1 do with sXml.Attributes.XmlAttributes[x] do
    begin
      xAttr := TXmlAttribute.Create;
      xAttr.Name := IfThen ( not Checked
                           , UncheckedAtttributeName
                           , ''
                           )
                  + Name
                  ;
      xAttr.Value := Value;
      xAttr.fChecked := fChecked;
      xAttr.XsdAttr := XsdAttr;
      dXml.AddAttribute(xAttr)
    end;
    for x := 0 to sXml.Items.Count - 1 do
    begin
      if (sXml.Items.XmlItems [x].hasRelevance) then
      begin
        cXml := dXml.AddXml(TXml.CreateAsString(sXml.Items.XmlItems [x].TagName, sXml.Items.XmlItems [x].Value));
        _Copy (cXml, sXml.Items.XmlItems [x]);
      end;
    end;
  end;
begin
  Items.Clear;
  Attributes.Clear;
  aXml.ExploreRelevancy;
  _Copy (Self, aXml);
end;

procedure TXml.CopyDownLine(aXml: TXml; aOnlyWhenChecked: Boolean);
  procedure _Copy (dXml, sXml: TXml);
  var
    x: Integer;
    cXml: TXml;
    xAttr: TXmlAttribute;
  begin
    dXml.TagName := sXml.TagName;
    dXml.jsonType := sXml.jsonType;
    dXml.Value := sXml.Value;
    dXml.Xsd := sXml.Xsd;
    dXml.TypeDef := sXml.TypeDef;
    dXml.fChecked := sXml.fChecked;
    dXml.RefId := sXml.RefId;
    for x := 0 to sXml.Attributes.Count - 1 do
    begin
      xAttr := TXmlAttribute.Create;
      xAttr.Name := sXml.Attributes.XmlAttributes[x].Name;
      xAttr.Value := sXml.Attributes.XmlAttributes[x].Value;
      xAttr.fChecked := sXml.Attributes.XmlAttributes[x].fChecked;
      xAttr.XsdAttr := sXml.Attributes.XmlAttributes[x].XsdAttr;
      dXml.AddAttribute(xAttr)
    end;
    for x := 0 to sXml.Items.Count - 1 do
    begin
      if (not aOnlyWhenChecked)
      or (sXml.Items.XmlItems [x].Checked) then
      begin
        cXml := dXml.AddXml(TXml.CreateAsString(sXml.Items.XmlItems [x].TagName, sXml.Items.XmlItems [x].Value));
        _Copy (cXml, sXml.Items.XmlItems [x]);
      end;
    end;
  end;
begin
  Items.Clear;
  Attributes.Clear;
  _Copy (Self, aXml);
end;

function TXml.GetIndexString: String;
var
  n, x: Integer;
begin
  result := '';
  if (Self = nil)
  or (Parent = nil)
  or (Xsd = nil)
  or (Xsd.maxOccurs = '1') then
    exit;
  n := 0;
  x := 0;
  while ((Parent as TXml).Items.XmlItems[x] <> self) do
  begin
    if ((Parent as TXml).Items.XmlItems[x].Xsd = Xsd) then
      Inc (n);
    Inc (x);
  end;
  if (n > 0) then
    result := '[' + IntToStr (n + 1) + ']';
end;

function TXml.getChecked: Boolean;
begin
  result := inherited Checked;
end;

procedure TXml.MergePreviousChecked;
var
  x: Integer;
begin
  Checked := inherited MergeChecked;
  for x := 0 to Items.Count - 1 do
    Items.XmlItems[x].MergePreviousChecked;
end;

function TXml.FindByRefId(aRefId: Integer): TXml;
var
  x: Integer;
begin
  result := nil;
  if aRefId = RefId then
    result := self
  else
  begin
    for x := Items.Count - 1 downto 0 do
    begin
      if (Items.XmlItems[x].RefId <= aRefId)
//      and (x < xsdElementsWhenRepeatable)
      then
      begin
        result := Items.XmlItems[x].FindByRefId(aRefId);
        break;
      end;
    end;
  end;
end;

procedure TXml.CheckDownline (aChecked: Boolean);
var
  x: Integer;
begin
  if Assigned (self) then
  begin
    Checked := aChecked;
    for x := 0 to Items.Count - 1 do
      Items.XmlItems [x].CheckDownLine(aChecked);
  end;
end;

procedure TXml.SetXsdReadOnly;
  procedure _setXsdReadOnly (aXml: TXml);
  var
    x: Integer;
  begin
    if not Assigned (aXml.Xsd) then Exit;
    aXml.Xsd.isReadOnly := True;
    for x := 0 to aXml.Items.Count - 1 do
      _setXsdReadOnly (aXml.Items.XmlItems [x]);
  end;
  procedure _setXsdEditAllowed (aXml: TXml);
  var
    x: Integer;
  begin
    if not Assigned (aXml.Xsd) then Exit;
    if aXml.Checked then
      aXml.Xsd.isReadOnly := False;
    for x := 0 to aXml.Items.Count - 1 do
      _setXsdEditAllowed (aXml.Items.XmlItems [x]);
  end;
begin
  _setXsdReadOnly (Self);
  _setXsdEditAllowed (Self);
end;

function TXml.UpLineAsText: String;
  function BooleanAsText (aBool: Boolean): String;
  begin
    if aBool then
      result := '(1)'
    else
      result := '(0)';
  end;
begin
  result := '';
  if Assigned (Parent) then
    result := (Parent as TXml).UpLineAsText + '.';
  result := result + Name + BooleanAsText(Checked);
end;

procedure TXml.Sort(aRecurringElementsPath, aSubElementsPath: String);
  procedure _sortElms (aParent: TXml; aRecurringPath, aSubPath: String);
  var
    x, y: Integer;
    sl: TStringList;
  begin
    sl := TStringList.Create;
    try
      for x := 0 to aParent.Items.Count - 1 do
        if NameWithoutPrefix (aParent.Items.XmlItems[x].Name) = aRecurringPath then
          sl.AddObject( aParent.Items.XmlItems[x].FindUQValue(aSubPath)
                      , aParent.Items.XmlItems[x]
                      );
      if sl.Count > 1 then
      begin
        sl.Sort;
        y := 0;
        for x := 0 to aParent.Items.Count - 1 do
        begin
          if  NameWithoutPrefix (aParent.Items.XmlItems[x].Name) = aRecurringPath then
          begin
            aParent.Items.XmlItems[x] := sl.Objects[y] as TXml;
            Inc (y);
          end;
        end;
      end;
    finally
      sl.Free;
    end;
  end;
  procedure _sort (aXml: TXml; aParName, aElemName, aSubElem: STring);
  var
    x: Integer;
    xName: String;
  begin
    x := Pos ('.', aParName);
    if x = 0 then
    begin
      if NameWithoutPrefix (aParName) = NameWithoutPrefix(aXml.Name) then
        _sortElms (aXml, aElemName, aSubElem);
    end
    else
    begin
      if NameWithoutPrefix (Copy (aParName, 1, x - 1)) = NameWithoutPrefix (aXml.Name) then
      begin
        xName := Copy (aParName, x + 1, 3000);
        for x := 0 to aXml.Items.Count - 1 do
          _sort (aXml.Items.XmlItems[x], xName, aElemName, aSubElem);
      end;
    end;
  end;
var
  x, p: Integer;
begin
  if Copy (aRecurringElementsPath, 1, 2) = '*.' then
  begin
    _sortElms (self, Copy (aRecurringElementsPath, 3, 1000), aSubElementsPath);
    for x := 0 to Items.Count - 1 do
      Items.XmlItems[x].Sort(aRecurringElementsPath, aSubElementsPath);
  end
  else
  begin
    p := 0;
    for x := 1 to Length (aRecurringElementsPath) do
       if aRecurringElementsPath[x] = '.' then
         p := x;
    _sort ( self
          , Copy (aRecurringElementsPath, 1, p - 1)
          , Copy (aRecurringElementsPath, p + 1, 3000)
          , aSubElementsPath
          );
    _sortElms (self, Copy (aRecurringElementsPath, 3, 1000), aSubElementsPath);
  end;
end;

function TXml.Children: TBindableList;
begin
  result := Items;
end;

function TXml.IsValueValid(var aMessage: String): Boolean;
begin
  result := TypeDef.IsValidXml(self, aMessage);
end;

function TXml.FindUQ(aName: String): TCustomBindable;
begin
  result := FindUQBind (aName);
end;

function TXmlAttribute .isXmlNsAttribute : Boolean ;
begin
  result := (Name = 'xmlns')
         or (Copy (Name, 1, 6) = 'xmlns:')
          ;
end;

function TXmlAttribute.isXmlTypeDefAttribute: Boolean;
begin
  result := (Name = 'type');    { TODO : xsi namespace }
end;

function TXmlAttribute.GetFullIndexCaption: String;
begin
  result := Parent.GetFullIndexCaption + '.' + Name;
end;

function TXmlAttribute.GetStringData: String;
begin
  if Self = nil then Exit;
  if Checked then
    result := Value
  else
    result := bindNilStr;
end;

function TXml.bgColor(aReadOnly: Boolean; aColumn: Integer): TColor;
begin
  result := bgElementValueColor;
  if aReadOnly
  or (    Assigned (Xsd)
      and (Xsd.isReadOnly)
      and (aColumn = treeValueColumn)
     ) then
    result := clBtnFace
  else
  begin
    if aColumn = treeValueColumn then
    begin
      if not CheckedAllUp then
      begin
        result := bgNilValueColor;
        Exit;
      end;
      if (Group)
      or (    Assigned (Xsd)
          and (TypeDef.ContentModel = 'Empty')
         )
      then
      begin
        result := clBtnFace;
        exit;
      end;
    end;
    if aColumn = treeTagColumn then
    begin
      result := clWhite;
      exit;
    end;
  end;
end;

function TXmlAttribute.bgColor(aReadOnly: Boolean; aColumn: Integer): TColor;
begin
  result := clWhite;
  if aReadOnly then
    result := clBtnFace
  else
  begin
    if aColumn = treeValueColumn then
    begin
      if not CheckedAllUp then
      begin
        result := bgNilValueColor;
        Exit;
      end;
      result := bgElementValueColor;
      Exit;
    end;
    if aColumn = treeTagColumn then
    begin
      result := clWhite;
      exit;
    end;
  end;
end;

procedure TXmlAttribute.Font(aFont: TFont);
begin
  if (    Assigned (Self.XsdAttr)
      and (Self.XsdAttr.Use = 'required')
     ) then
  begin
    aFont.Style := aFont.Style + [fsBold];
    if Assigned (Self.Parent)
    and Self.Parent.CheckedAllUp
    and not Self.Checked then
      aFont.Color := clRed {clLtGray}	;
  end
  else
  begin
    if (not Assigned (Self.XsdAttr)) then
      aFont.Color := clRed {clLtGray}	;
  end;
end;

procedure TXml.Font(aFont: TFont);
begin
  if (not Assigned (Self.Xsd)) then
  begin
    aFont.Color := fgUnknownDatatypeColor;
    exit;
  end;
  try
    if Assigned (Self.Xsd)
    and (StrToIntDef (Self.Xsd.minOccurs, 0) > 0)
    and Assigned (Self.Parent)
    and Assigned (TXml(Self.Parent).Xsd)
    and (TXml(Self.Parent).TypeDef.ContentModel <> 'Choice')
    then
    begin
      aFont.Style := aFont.Style + [fsBold];
      if Self.Parent.CheckedAllUp then
        if not Self.Checked then
          aFont.Color := fgMissingColor;
    end;
  except
  end;
end;

function TXml.getTypeDef: TXsdDataType;
begin
  if Assigned (fTypeDef) then
    Result := fTypeDef
  else
    if Assigned (Xsd) then
      Result := Xsd.sType
    else
      Result := nil;
end;

function TXml.getDocumentationText: String;
begin
  result := '';
  if Assigned (Self.Xsd) then
    result := Self.Xsd.Documentation.Text;
  if (result = '') then
    if Assigned (Self.TypeDef) then
      result := Self.TypeDef.Documentation.Text;
end;

function TXml.getAttributeValueByTag (Index: String): String ;
begin
  result := getAttributeValueByTagDef(Index, '');
end;

function TXml .getAttributeBooleanByTag (Index : String ): Boolean ;
begin
  result := getAttributeBooleanByTagDef(Index, false);
end;

function TXml .getAttributeBooleanByTagDef (Index : String ; aDefault : Boolean
  ): Boolean ;
begin
  result := aDefault;
  if not Assigned (self) then Exit;
  result := Attributes.BooleanByTagDef[Index, aDefault];
end;

function TXml.getAttributeValueByTagDef (Index , aDefault : String ): String ;
begin
  result := aDefault;
  if not Assigned (self) then Exit;
  result := Attributes.ValueByTagDef[Index, aDefault];
end;

function TXml.getAppinfoText: String;
begin
  result := '';
  if Assigned (Self.TypeDef) then
    result := Self.TypeDef.Appinfo.Text;
  if Assigned (Self.Xsd)
  and (Self.Xsd.Appinfo.Text <> result) then
    result := result + Self.Xsd.Appinfo.Text;
end;

function TXml.getIsSoapEnvelope : Boolean ;
begin
  result := (TagName = 'Envelope')
        and (   (NameSpace = scSoapEnvNameSpaceV1_1)
             or (NameSpace = scSoapEnvNameSpaceV1_2)
            );
end;

function TXml .getIsSoapHeader : Boolean ;
begin
  result := (TagName = 'Header')
        and (   (NameSpace = scSoapEnvNameSpaceV1_1)
             or (NameSpace = scSoapEnvNameSpaceV1_2)
            );
end;

function TXml .getIsSoapBody : Boolean ;
begin
  result := (TagName = 'Body')
        and (   (NameSpace = scSoapEnvNameSpaceV1_1)
             or (NameSpace = scSoapEnvNameSpaceV1_2)
            );
end;

function TXml.getItemByTag (Index: String): TXml ;
begin
  result := nil;
  if not Assigned (self) then Exit;
  result := Items.XmlItemByTag[Index];
end;

{ TXmlCvrg }

function TXmlCvrg.AddXml(aChildXml: TXmlCvrg): TXmlCvrg;
begin
  result := Inherited AddXml (aChildXml) as TXmlCvrg;
end;

procedure TXmlCvrg.CalculateCoverage;
  procedure _Init(aXmlCvrg: TXmlCvrg);
  var
    x: Integer;
  begin
    aXmlCvrg.GreenCounter := 0;
    aXmlCvrg.OrangeCounter := 0;
    aXmlCvrg.RedCounter := 0;
    for x := 0 to aXmlCvrg.Items.Count - 1 do
      _Init(aXmlCvrg.XmlItems[x]);
  end;
  procedure _Aggregate(aXmlCvrg: TXmlCvrg);
  var
    x: Integer;
  begin
    for x := 0 to aXmlCvrg.Items.Count - 1 do
    begin
      _Aggregate(aXmlCvrg.Items.XmlItems[x] as TXmlCvrg);
      Inc (aXmlCvrg.GreenCounter, aXmlCvrg.XmlItems[x].GreenCounter);
      Inc (aXmlCvrg.OrangeCounter, aXmlCvrg.XmlItems[x].OrangeCounter);
      Inc (aXmlCvrg.RedCounter, aXmlCvrg.XmlItems[x].RedCounter);
    end;
    if aXmlCvrg.Ignore then
    begin
      Inc (aXmlCvrg.OrangeCounter, 1 + aXmlCvrg.GreenCounter + aXmlCvrg.RedCounter);
      aXmlCvrg.GreenCounter := 0;
      aXmlCvrg.RedCounter := 0;
    end
    else
    begin
      if (aXmlCvrg.Counter > 0) then
        Inc (aXmlCvrg.GreenCounter)
      else
        Inc (aXmlCvrg.RedCounter);
    end;
  end;
begin
  _Init (self);
  _Aggregate (self);
end;

procedure TXmlCvrg.CountUsage(dataXml: TXml; aOnlyWhenChecked: Boolean);
  procedure _count (aXmlCvrg: TXmlCvrg; aXml: TXml);
  var
    v, cx, dx: Integer;
    cXml: TXmlCvrg;
  begin
    if aOnlyWhenChecked and not aXml.Checked then Exit;
    if NameWithoutPrefix(aXmlCvrg.Name) <> NameWithoutPrefix(aXml.Name) then Exit;
    aXmlCvrg.fPresent := true;
    Inc (aXmlCvrg.Counter);
    if (    Assigned (aXmlCvrg.Xsd)
        and Assigned (aXmlCvrg.Xsd.sType)
        and (aXmlCvrg.Xsd.sType.ElementDefs.Count = 0)
       )
    or (aXmlCvrg.isAttrbute)
{$ifdef IPMZ}
    or (    Assigned (aXmlCvrg.Ipm)
        and ((aXmlCvrg.Ipm as TIpmItem).Items.Count = 0)
       )
{$endif}
    then
    begin
      if aXml.Value = '' then
        Inc (aXmlCvrg.EmptyCounter)
      else
      begin
        if Length (aXml.Value) > 41 then
          aXmlCvrg.distinctValues.Add(Sha1(aXml.Value))
        else
          aXmlCvrg.distinctValues.Add(aXml.Value);
      end;
      if Assigned(aXmlCvrg.Xsd)
      and Assigned (aXmlCvrg.Xsd.sType) then
      begin
        if (aXmlCvrg.Xsd.sType.Enumerations.Count > 0)
        or (aXmlCvrg.Xsd.sType.BaseDataTypeName = 'boolean') then
        begin
          for v := 0 to aXmlCvrg.Items.Count - 1 do with (aXmlCvrg.Items.XmlItems[v] as TXmlCvrg) do
          begin
            if isValue
            and (Name = aXml.Value) then
            begin
              fPresent := True;
              Inc (Counter);
            end;
          end;
        end;
      end;
    end;
    cXml := aXmlCvrg;
    if Assigned (cXml.recurs) then
      cXml := cXml.recurs;
    for cx := 0 to cXml.Items.Count - 1 do
    begin
      cXml.XmlItems[cx].fPresent := false;
      if cXml.XmlItems[cx].isAttrbute then
      begin
        for dx := 0 to aXml.Attributes.Count - 1 do
        with aXml.Attributes.XmlAttributes[dx] do
        begin
          if Name = cXml.XmlItems[cx].Name then
          begin
            Inc (cXml.XmlItems[cx].Counter);
            cXml.XmlItems[cx].fPresent := true;
            if Value = '' then
              Inc (cXml.XmlItems[cx].EmptyCounter)
            else
            begin
              if Length (Value) > 41 then
                cXml.XmlItems[cx].distinctValues.Add(Sha1(Value))
              else
                cXml.XmlItems[cx].distinctValues.Add(Value);
            end;
            for v := 0 to cXml.XmlItems[cx].Items.Count - 1 do with (cXml.XmlItems[cx].Items.XmlItems[v] as TXmlCvrg) do
            begin
              if isValue
              and (Name = aXml.Value) then
              begin
                fPresent := True;
                Inc (Counter);
              end;
            end;
          end;
        end;
      end
      else
      begin
        for dx := 0 to aXml.Items.Count - 1 do
          _count (cXml.XmlItems[cx], aXml.Items.XmlItems[dx]);
      end;
      if not cXml.XmlItems[cx].fPresent then
        Inc (cXml.XmlItems[cx].NilCounter);
    end;
  end;
begin
  _count(self, dataXml);
end;

constructor TXmlCvrg.Create;
begin
  inherited Create;
  distinctValues := TStringList.Create;
  distinctValues.Sorted := True;
  distinctValues.Duplicates := dupIgnore;
end;

constructor TXmlCvrg.CreateAsString(aName, aString: String);
begin
  inherited CreateAsString(aName, aString);
  distinctValues := TStringList.Create;
  distinctValues.Sorted := True;
  distinctValues.Duplicates := dupIgnore;
end;

constructor TXmlCvrg.CreateFromXsd(aName: String; aXsd: TXsd);
  procedure _FromXsd (aXml: TXmlCvrg; aName: String; aXsd: TXsd);
  var
    x, v: Integer;
    cXml, pXml: TXmlCvrg;
  begin
    aXml.Name := aName;
    aXml.Xsd := aXsd;
    aXml.TypeDef := aXsd.sType;
    if not Assigned (aXml.distinctValues) then
    begin
      aXml.distinctValues := TStringList.Create;
      aXml.distinctValues.Sorted := True;
      aXml.distinctValues.Duplicates := dupIgnore;
    end;
    if not Assigned (aXsd.sType) then
      Exit;
    for v := 0 to aXsd.sType.Enumerations.Count - 1 do
    begin
      with aXml.AddXml(TXmlCvrg.CreateAsString((aXsd.sType.Enumerations.Objects[v] as TXsdEnumeration).Value, '')) do
      begin
        isValue := True;
      end;
    end;
    if (aXsd.sType.BaseDataTypeName = 'boolean')
    and (aXsd.sType.Enumerations.Count = 0) then
    begin
      with aXml.AddXml(TXmlCvrg.CreateAsString('false', '')) do isValue := True;
      with aXml.AddXml(TXmlCvrg.CreateAsString('true', '')) do isValue := True;
//    with aXml.AddXml(TXmlCvrg.CreateAsString('0', '')) do isValue := True;
//    with aXml.AddXml(TXmlCvrg.CreateAsString('1', '')) do isValue := True;
    end;
    if aXsd._Processed then
    begin
      pXml := aXml.Parent as TXmlCvrg;
      while Assigned (pXml)
      and (pXml.Xsd <> aXsd) do
        pXml := pXml.Parent as TXmlCvrg;
      if Assigned (pXml) then
        aXml.recurs := pXml;
      Exit;
    end;
    aXsd._Processed := True;
    try
      for x := 0 to aXsd.sType.AttributeDefs.Count - 1 do
      begin
        with aXml.AddXml(TXmlCvrg.CreateAsString(aXsd.sType.AttributeDefs.XsdAttrs[x].Name, '')) do
        begin
          isAttrbute := True;
          for v := 0 to aXsd.sType.AttributeDefs.XsdAttrs[x].Enumerations.Count - 1 do
          begin
            with aXml.AddXml(TXmlCvrg.CreateAsString((aXsd.sType.AttributeDefs.XsdAttrs[x].Enumerations.Objects[v] as TXsdEnumeration).Value, '')) do
            begin
              isValue := True;
            end;
          end;
        end;
      end;
      for x := 0 to aXsd.sType.ElementDefs.Count - 1 do
      begin
        cXml := aXml.AddXml(TXmlCvrg.CreateAsString(aXsd.sType.ElementDefs.Xsds[x].ElementName, ''));
        _FromXsd(cXml, aXsd.sType.ElementDefs.Xsds[x].ElementName, aXsd.sType.ElementDefs.Xsds[x])
      end;
    finally
      aXsd._Processed := False;
    end;
  end;
begin
  inherited Create;
  _FromXsd(self, aName, aXsd);
end;

constructor TXmlCvrg.CreateFromIpm(aName: String; aIpm: TObject);
{$ifdef IPMZ}
  procedure _FromIpm (aXml: TXmlCvrg; aName: String; aIpm: TIpmItem);
  var
    x, v: Integer;
    cXml, pXml: TXmlCvrg;
  begin
    if aIpm.Occurrence > 1  then Exit;
    aXml.Name := aName;
    aXml.Ipm := aIpm;
    aXml.distinctValues := TStringList.Create;
    aXml.distinctValues.Sorted := True;
    aXml.distinctValues.Duplicates := dupIgnore;
    for x := 0 to aIpm.Items.Count - 1 do
    begin
      if aIpm.Items.IpmItems[x].Occurrence = 0 then
      begin
        cXml := aXml.AddXml(TXmlCvrg.CreateAsString(aIpm.Items.IpmItems[x].Name, ''));
        _FromIpm(cXml, aIpm.Items.IpmItems[x].Name, aIpm.Items.IpmItems[x])
      end;
    end;
  end;
  {$endif}
begin
  inherited Create;
  {$ifdef IPMZ}
  _FromIpm(self, aName, aIpm as TIpmItem);
  {$endif}
end;

destructor TXmlCvrg.Destroy;
begin
  distinctValues.Clear;
  distinctValues.Free;
  inherited;
end;

function TXmlCvrg.getDisplayCount: String;
begin
  result := IntToStr(Counter);
end;

function TXmlCvrg.DisplayCoverage (doShowIgnoreds: Boolean): String;
var
  Denominator: Integer;
begin
  Denominator := GreenCounter + RedCounter;
  if doShowIgnoreds then
    Inc (Denominator, OrangeCounter);
  result := IntToStr(GreenCounter) + '/' + IntToStr (Denominator);
end;

function TXmlCvrg.getDisplayDistinctValueCounter: String;
begin
  result := '';
  if (    Assigned (Xsd)
      and Assigned(Xsd.sType)
      and (Xsd.sType.ElementDefs.Count = 0)
     )
  or (isAttrbute)
  {$ifdef IPMZ}
  or (    Assigned (Ipm)
      and ((Ipm as TIpmItem).Items.Count = 0)
     )
  {$endif}
  then
    result := IntToStr (DistinctCounter);
end;

function TXmlCvrg.getDisplayEmptyCount: String;
begin
  result := '';
  if (    Assigned (Xsd)
      and Assigned (Xsd.sType)
      and (Xsd.sType.ElementDefs.Count = 0)
     )
  or (isAttrbute)
  {$ifdef IPMZ}
  or (    Assigned (Ipm)
      and ((Ipm as TIpmItem).Items.Count = 0)
     )
  {$endif}
  then
    result := IntToStr (EmptyCounter);
end;

function TXmlCvrg.getDisplayName: String;
begin
  result := Name;
  case CoverageType of
    ctXmlAttribute: result := '@' + Name;
    ctXmlValue: result := '$' + Name;
  end;
end;

function TXmlCvrg.getDisplayNilCount: String;
begin
  result := '';
  if not isValue then
    result := IntToStr(NilCounter);
end;

function TXmlCvrg.DisplayPercentage (doShowIgnoreds: Boolean): String;
var
  Denominator: Integer;
begin
  result := '';
  Denominator := GreenCounter + RedCounter;
  if doShowIgnoreds then
    Inc (Denominator, OrangeCounter);
  if Denominator <> 0 then
    result := IntToStr(Round(100 * GreenCounter / Denominator));
end;

function TXmlCvrg.getDistinctCounter: Integer;
begin
  result := distinctValues.Count;
end;

function TXmlCvrg.getHasIgnored: Boolean;
var
  x: Integer;
begin
  result := Ignore;
  if not result then
  begin
    for x := 0 to Items.Count - 1 do
    begin
      result := XmlItems[x].hasIgnored;
      if result then
        exit;
    end;
  end;
end;

function TXmlCvrg.getIsAttrbute: Boolean;
begin
  result := (CoverageType = ctXmlAttribute);
end;

function TXmlCvrg.getIsIgnored: Boolean;
begin
  result := Ignore;
  if (not result) and Assigned (Parent) then
    result := (Parent as TXmlCvrg).getIsIgnored;
end;

function TXmlCvrg.getIsValue: Boolean;
begin
  result := (CoverageType = ctXmlValue);
end;

function TXmlCvrg.getXmlItems(Index: integer): TXmlCvrg;
begin
  result := Items.XmlItems[Index] as TXmlCvrg;
end;

procedure TXmlCvrg.setIgnore(const aValue: Boolean);
  procedure _up (a: TXmlCvrg);
  begin
    if Assigned (a) then
    begin
      a.fIgnore := aValue;
      _up (a.Parent as TXmlCvrg);
    end;
  end;
  procedure _down (a: TXmlCvrg);
  var
    x: Integer;
  begin
    a.fIgnore := aValue;
    for x := 0 to a.Items.Count - 1 do
      _down (a.Items.XmlItems[x] as TXmlCvrg);
  end;
begin
  _down (self);
  if not aValue then
    _up (self.Parent as TXmlCvrg);
end;

procedure TXmlCvrg.setIsAttrbute(const aValue: Boolean);
begin
  CoverageType := ctXmlAttribute;
end;

procedure TXmlCvrg.setIsValue(const aValue: Boolean);
begin
  CoverageType := ctXmlValue;
end;

initialization
  _xmlUserName := GetUserName;
  _xmlProgName := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(ParamStr(0)), '');
  _xmlProgVersion := xmlio.GetVersion;
  _xmlLicensed := True;
  xmlSetDefaultColors;

end.
