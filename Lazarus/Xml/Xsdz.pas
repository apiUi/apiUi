// BaseDataTypeName van root-type laten zijn
// Annotattions in de juiste volgorde (meest sppecifiek boven aan)
// TODO Attributes binnen TypeDefs
// global Attributes and referencing globalAttributes (no type... refereneces...)
// ..
{$mode DELPHI}

unit Xsdz;

interface


uses Classes
   , Variants
   , ParserClasses
   , xmlzConsts
   , LazFileUtils
   , XmlIO
   ;

resourcestring
  S_DOLLARREF = '$ref';

type
  TXsdFormDefault = (xsdFDAccordingWsdl, xsdFDQualified, xsdFDUnqualified);
  TXsdType = (dtSimpleType, dtComplexType, dtAttributeType);
  TElementOrTypeDefRef = (etElementRef, etTypeDefRef)  ;
  TBooleanFunctionObject = function (arg: TObject): Boolean of Object;


type
  TjsonType = (jsonNone, jsonString, jsonNumber, jsonBoolean, jsonObject,
    jsonArray, jsonArrayValue);
type TOperationParametersType = (oppBody, oppPath, oppQuery, oppHeader, oppForm);
const OperationParametersTypeNames: array [oppBody..oppForm] of String =
( 'Body'
, 'Path'
, 'Query'
, 'Header'
, 'Form'
);
function NameToOperationParametersType (aName:String): TOperationParametersType;
type TCollectionFormat = (ocfSingle, ocfCSV, ocfSSV, ocfTSV, ocfPipes, ocfMulti);

type
  TOnHaveString = procedure(aString: String) of Object;
  TOnCheckNewValue = function (aBind: TObject; aNewText: String): Boolean of Object;

type
  TOnDoSelectValue = procedure(var Ok: Boolean; var NewString: String;
    curString: String) of Object;

type
  TXsdDataTypeList = class;
  TXsdDataType = class;
  TXsdAttrList = class;
  TXsdAttr = class;
  TXsdList = class;
  TXsd = class;
  TXsdDescr = class;

  TXsdDataTypeList = class(TStringList)
  protected
    function GetXsdDataType(Index: integer): TXsdDataType;
  public
    property XsdDataTypes[Index: integer]: TXsdDataType read GetXsdDataType;
  end;

  TXsdEnumeration = class(TObject)
  public
    Value: String;
    Annotation: String;
    function Clone: TXsdEnumeration;
  end;

  { TXsdDataType }

  TXsdDataType = class(TObject)
  private
    function getUniqueId: String;
    function getNSAttributes: String;
    function getXsiNameSpaceAttribute: String;
    function getNSPrefix: String;
    function getElementByName(Index: String): TXsd;
  public
    xsdType: TXsdType;
    Name: String;
    NameSpace: String;
    IsComplex: Boolean;
    IsBuiltIn: Boolean;
    IsExtention: Boolean;
    isAbstract: Boolean;
    isMixed: Boolean;
    xsdDescr: TXsdDescr;
    _Processed, _isGroup: Boolean;
    _DepthBillOfMaterial: integer;
    _Ficticious: Boolean;
    _Error: Boolean;
    _Extended: Boolean;
    BaseDataTypeName: String;
    BaseNameSpace: String;
    BaseDataType: TXsdDataType;
    DefaultValue: String;
    ContentModel: String;
    DerivationMethod: String;
    CollectionFormat: TCollectionFormat;
    Length: String;
    MinLength: String;
    MaxLength: String;
    MinItems: String;
    MaxItems: String;
    MultipleOf: String;
    uniqueItems: String;
    maxProperties: String;
    MinProperties: String;
    Required: Boolean; // json
    JsonXmlName: String;
    Pattern: String;
    Whitespace: String;
    MaxInclusive: String;
    MaxExclusive: String;
    MinInclusive: String;
    MinExclusive: String;
    Numeric: String;
    TotalDigits: String;
    FractionalDigits: String;
    Enumerations: TStringList;
    Documentation: TStringList;
    Appinfo: TStringList;
    ExtendedByList: TXsdDataTypeList;
    ElementDefs: TXsdList;
    AttributeDefs: TXsdAttrList;
    Manually: Boolean;
    ManuallyUsedAtPath: String;
    dollarRef: String;
    SourceFileName: String;
{
    isMaxLengthAdjusted: Boolean;
    MaxOccursAdjusted: Integer;
}
    OnDoSelectValue: TOnDoSelectValue;
    jsonType: TjsonType;
    function populateValue(aTag: String): String;
    function ccbPictureClause: string;
    function SchemaAsXml(aName: String): TObject;
    function SchemaAsText(aName: String): String;
    function AddXsd(aChildXsd: TXsd): TXsd;
    constructor Create; Overload;
    constructor Create(aXsdDescr: TXsdDescr); Overload;
    constructor Create(aXsdDescr: TXsdDescr; aSource: TXsdDataType); Overload;
    destructor Destroy; override;
    function IsValidValue(aName, aValue: String; var aMessage: String): Boolean;
    function IsValidXml(aXml: TObject; var aMessage: String): Boolean;
    property uniqueId: String read getUniqueId;
    property NSPrefix: String read getNSPrefix;
    property NSAttributes: String read getNSAttributes;
    property XsiNameSpaceAttribute: String read getXsiNameSpaceAttribute;
    property ElementByName[Index: String]: TXsd read getElementByName;
  end;

  TXsdAttr = class(TXsdDataType)
  private
  public
    Use: String;
    FormDefaultQualified: Boolean;
    constructor Create(aXsdDescr: TXsdDescr); Overload;
    constructor Create(aXsdDescr: TXsdDescr; aSource: TXsdAttr); Overload;
    destructor Destroy; override;
  end;

  TXsdAttrList = class(TStringList)
  protected
    function GetXsdAttr(Index: integer): TXsdAttr;
  public
    property XsdAttrs[Index: integer]: TXsdAttr read GetXsdAttr;
    procedure Clear; override;
  end;

  TXsd = class(TObject)
  private
    function getNSPrefix: String;
    function getXsdByCaption(Index: String): TXsd;
  public
    _Processed, _isGroup: Boolean;
    xsdDescr: TXsdDescr;
    FileName: String;
    ElementName: String;
    ElementNameSpace: String;
    FormQualified: Boolean;
    DoNotEncode: Boolean;
    isReadOnly: Boolean;
    isCheckboxDisabled: Boolean;
    isOneOfGroupLevel: Integer;
    ParametersType: TOperationParametersType;
    MediaType: String;
    ResponseNo: Integer;
    minOccurs: String;
    maxOccurs: String;
    _UnknownType: String;
    _DataTypeName: String;
    _NameSpace: String;
    _RefElementName: String;
    _RefNameSpace: String;
    _SelfRef: Boolean;
    sType: TXsdDataType;
    Manually: Boolean;
    _ElementOrTypeDefRef: TElementOrTypeDefRef;
    InitialCollapsed: Boolean;
    Documentation: TStringList;
    Appinfo: TStringList;
    Obj: TObject;
    EditProcedure: TBooleanFunctionObject;
    CheckNewValue: TOnCheckNewValue;
    SourceFileName: String;
    function FindXsd(aString: String): TXsd;
    property NSPrefix: String read getNSPrefix;
    property XsdByCaption[Index: String]: TXsd read getXsdByCaption;
    function AddElementDef(aXsdDescr: TXsdDescr; aName: String;
      aType: TXsdDataType): TXsd;
    function IsTypeDefEnabled: Boolean;
    function ccbOccursClause: String;
    function isRequired: Boolean;
    function JsonSchemaAsXml: TObject;
    procedure ClearNameSpace;
    procedure GenerateHtmlReport(aStringList: TStringList);
    procedure GenerateReport(aStringList: TStringList);
    procedure GenerateCopyBook(aPrefix, aSuffix, a88Prefix, a88Suffix,
      aLevel: String; aStringList: TStringList);
    constructor Create(aXsdDescr: TXsdDescr); Overload;
    constructor Create(aXsdDescr: TXsdDescr; aSource: TXsd); Overload;
    destructor Destroy; override;
  end;

  TXsdList = class(TStringList)
  private
    function GetXsdByName(Index: String): TXsd;
  protected
    function GetXsd(Index: integer): TXsd;
  published
  public
    property Xsds[Index: integer]: TXsd read GetXsd;
    property XsdByName[Index: String]: TXsd read GetXsdByName;
    procedure ResetProcessed;
  end;

  { TXsdDescr }

  TXsdDescr = class(TObject)
  private
    fMainFileName: String;
    function AddSimpleType(aXml: TObject; aTargetNamespace: String; aGlobal: Boolean): TXsdDataType;
    function ArrayContains(const Value: String;
      const Values: array of String): Boolean;
    function AddComplexType(aXml: TObject; aTargetNamespace: String; isGlobalDefined: Boolean): TXsdDataType;
    procedure AppendDoc (var aDoc: String; aString: String);
    procedure AddDataTypeFacets(aTypeDef: TXsdDataType; aXml: TObject);
    function AddAttributeDef (aTypeDef: TXsdDataType; aXml: TObject; aTargetNamespace: String): TXsdAttr;
    function AddElement(aTypeDef: TXsdDataType; aXml: TObject; aTargetNameSpace: String; aRoot: Boolean): TXsd;
    procedure AddGroup(aTypeDef: TXsdDataType; aXml: TObject; aTargetNameSpace: String);
  public
    Garbage: TStringList;
    xsdFileNames: TStringList;
    Prefix: String;
    SchemaName: String;
    Alias: String;
    TargetNamespace: String;
    TargetNSPrefix: String;
    NamespaceURI: String;
    TypeDefs: TXsdDataTypeList;
    FileContents: TStringList;
    NameSpaceList: TStringList;
    ReadFileNames: TStringList;
    TypeDef: TXsdDataType;
    ChangedElementDefs: TObject;
    function CreateXsdFromXmlSample (aXml: TObject; aLinkXmlToXsd: Boolean): TXsd;
    function CreateXsdFromJsonSample (aXml: TObject; aLinkXmlToXsd: Boolean): TXsd;
    function GenerateNameSpaceAttributes: String;
    function NameSpacePrefix(aNameSpace: String): String;
    procedure AddNameSpace(aNameSpace: String);
    procedure Finalise;
    procedure Clear;
    function FindTypeDef(aNameSpace, aName: String): TXsdDataType;
    function FindElement(aNameSpace, aName: String): TXsd;
    function xsdFindTypeDef(aNameSpace, aName: String): TXsdDataType;
    procedure AddBuiltIns;
    procedure AddXsdFromXml(aXml: TObject; aFileName: String; ErrorFound: TOnErrorEvent);
    function AddTypeDefFromJsonXml(aFileName, aNameSpace: String; aXml: TObject; ErrorFound: TOnErrorEvent): TXsdDataType;
    procedure AddXsdFromFile(aOverruleNamespace, aFileName: String; ErrorFound: TOnErrorEvent);
    function LoadXsdFromFile(aFileName: String; ErrorFound: TOnErrorEvent): Boolean;
    function LoadXsdFromXmlSampleFile(aFileName: String; ErrorFound: TOnErrorEvent): TXsd;
    function LoadXsdFromJsonSampleFile(aFileName: String; ErrorFound: TOnErrorEvent): TXsd;
    function LoadXsdFromString(aString: String; ErrorFound: TOnErrorEvent): Boolean;
    function ChangedElementTypedefsAsXml: TObject;
    procedure ChangedElementTypedefsFromXml (aXml: TObject);
    function AddedTypeDefElementsAsXml: TObject;
    procedure AddedTypeDefElementsFromXml(aXml: TObject);
    procedure ChangeElementsDatatype(aNameSpace, aName: String; aNewType: TXsdDataType);
    constructor Create;
    destructor Destroy; override;
  end;

  TXsdDescrList = class(TStringList)
  private
    function GetXsdDescr(Index: integer): TXsdDescr;
  public
    function FindTypeDef(aNameSpace: String; aName: String): TXsdDataType;
    property XsdDescrs[Index: integer]: TXsdDescr read GetXsdDescr;
    procedure Clear; override;
  end;

function NameWithoutPrefix(aName: String): String;
function xsdGenerateXsiNameSpaceAttribute: String;
function xsdGenerateXsdNameSpaceAttribute: String;

var
  xsiGenerated: Boolean;
  xsdGenerated: Boolean;
  xsdFormDefault: TXsdFormDefault;
  xsdElementFormDefaultQualified: Boolean;
  xsdAttributeFormDefaultQualified: Boolean;
  xsdValidateAssignmentsAgainstSchema: Boolean;
  xsdMaxDepthBillOfMaterials: integer;
  xsdMaxDepthXmlGen: Integer;
  defaultXsdMaxDepthBillOfMaterials: integer;
  defaultXsdMaxDepthXmlGen: Integer;
  systemStarting: Boolean;

implementation

uses sysutils
   , strutils
   , RegExpr
   , Dialogs
   , xmlxsdparser
   , Xmlz
   ;

{ TXsdList }

function NameToOperationParametersType(aName: String): TOperationParametersType;
begin
  result := oppBody; // avoid warning
  if aName = 'Body' then begin result := oppBody; Exit; end;
  if aName = 'Path' then begin result := oppPath; Exit; end;
  if aName = 'Query' then begin result := oppQuery; Exit; end;
  if aName = 'Header' then begin result := oppHeader; Exit; end;
  if aName = 'Form' then begin result := oppForm; Exit; end;
  raise Exception.Create('function OperationParametersType(aName: String): TOperationParametersType; illegal arg: ' + aName);
end;

function NameWithoutPrefix(aName: String): String;
begin
  result := Copy (aName, Pos (':', aName) + 1, MaxInt);
end;

procedure AddDocumentation(aStringList: TStringList; aXml: TXml);
var
  x, y: integer;
begin
  for x := 0 to aXml.Items.Count - 1 do
  begin
    if aXml.Items.XmlItems[x].Name = tagAnnotation then with aXml.Items.XmlItems[x] do
    begin
      for y := 0 to Items.Count - 1 do
        if Items.XmlItems[y].Name = tagDocumentation then
          aStringList.Add (Items.XmlItems[y].Value);
    end;
  end;
end;

procedure AddAppInfo(aStringList: TStringList; aXml: TXml);
var
  x, y: integer;
begin
  for x := 0 to aXml.Items.Count - 1 do
  begin
    if aXml.Items.XmlItems[x].Name = tagAnnotation then with aXml.Items.XmlItems[x] do
    begin
      for y := 0 to Items.Count - 1 do
        if Items.XmlItems[y].Name = tagAppInfo then
          aStringList.Add (Items.XmlItems[y].Value);
    end;
  end;
end;

function MatchPattern(const ARegExpr, AInputStr: RegExprString): Boolean;
var
  Rx: TRegExpr;
begin
  if Length(AInputStr) > 8000 then
    raise Exception.Create(
      'Due to size of input, checking against datatype disabled');
  Rx := TRegExpr.Create;
  try
    Rx.Expression := '^' + ARegExpr + '$';
    result := Rx.Exec(AInputStr);
  finally
    Rx.Free;
  end;
end; { of function ExecRegExpr }

function TXsdList.GetXsd(Index: integer): TXsd;
begin
  result := TXsd(Objects[index]);
end;

function TXsdList.GetXsdByName(Index: String): TXsd;
var
  x: integer;
begin
  result := nil;
  for x := 0 to Count - 1 do
    if Xsds[x].ElementName = Index then
    begin
      result := Xsds[x];
      exit;
    end;
end;

procedure TXsdList.ResetProcessed;
var
  x: integer;
begin
  for x := 0 to Count - 1 do
    Xsds[x]._Processed := False;
end;

{ TXsd }

function TXsdDataType.AddXsd(aChildXsd: TXsd): TXsd;
begin
  result := aChildXsd;
  if not Assigned(self) then
    raise Exception.Create('TXsdDataType.AddXsd(aChildXsd: TXsd): nil self');
  if Assigned(aChildXsd) then
    ElementDefs.AddObject(aChildXsd.ElementName, aChildXsd)
  else
    raise Exception.Create
      ('TXsdDataType.AddXsd(aChildXsd: TXsd): nil argument');
  { aChildXsd.Parent := self; }
end;

constructor TXsd.Create(aXsdDescr: TXsdDescr);
begin
  inherited Create;
  Documentation := TStringList.Create;
  Appinfo := TStringList.Create;
  xsdDescr := aXsdDescr;
  minOccurs := '1';
  maxOccurs := '1';
  ResponseNo := 200;
  ParametersType := oppBody;
  FormQualified := xsdElementFormDefaultQualified;
end;

constructor TXsd.Create(aXsdDescr: TXsdDescr; aSource: TXsd);
begin
  inherited Create;
  self.xsdDescr := aXsdDescr;
  self.Documentation := TStringList.Create;
  self.Appinfo := TStringList.Create;
  self.ElementName := aSource.ElementName;
  self.ElementNameSpace := aSource.ElementNameSpace;
  self.minOccurs := aSource.minOccurs;
  self.maxOccurs := aSource.maxOccurs;
  self.ParametersType := aSource.ParametersType;
  self.sType := aSource.sType;
  self.InitialCollapsed := aSource.InitialCollapsed;
  self.Documentation.Text := aSource.Documentation.Text;
  self.Appinfo.Text := aSource.Appinfo.Text;
  self.ResponseNo := aSource.ResponseNo;
  self.FileName := aSource.FileName;
  self.SourceFileName := aSource.SourceFileName;
  self.FormQualified := aSource.FormQualified;
end;

destructor TXsd.Destroy;
begin
  Documentation.Free;
  Appinfo.Free;
  FreeAndNil(Obj);
end;

function TXsd.FindXsd(aString: String): TXsd;
  function Uq(s: String): String;
  var
    x: integer;
  begin
    result := '';
    for x := 1 to system.Length(s) do
    begin
      if s[x] = ':' then
        result := ''
      else
        result := result + s[x];
    end;
  end;

var
  x, y: integer;
  xName, newName: String;
begin
  result := nil;
  x := Pos('.', aString);
  if x = 0 then
    xName := Uq(aString)
  else
    xName := Uq(Copy(aString, 1, x - 1));
  if (xName = Uq(ElementName)) then
  begin
    if x = 0 then
      result := self
    else
    begin
      newName := Copy(aString, x + 1, Length(aString));
      for y := 0 to sType.ElementDefs.Count - 1 do
      begin
        result := sType.ElementDefs.Xsds[y].FindXsd(newName);
        if result <> nil then
          exit;
      end;
    end;
  end;
end;

constructor TXsdDescr.Create;
begin
  TypeDef := TXsdDataType.Create(self);
  TypeDefs := TXsdDataTypeList.Create;
  TypeDefs.Sorted := True;
  TypeDefs.CaseSensitive := True;
  FileContents := TStringList.Create;
  Garbage := TStringList.Create;
  ReadFileNames := TXsdList.Create;
  ReadFileNames.Sorted := True;
  ReadFileNames.CaseSensitive := False;
  NameSpaceList := TXsdList.Create;
  NameSpaceList.Sorted := True;
  NameSpaceList.CaseSensitive := True;
  ChangedElementDefs := TXml.CreateAsString('ChangedElementDefs', '');
end;

destructor TXsdDescr.Destroy;
begin
  Clear;
  FreeAndNil(TypeDefs);
  FreeAndNil(TypeDef);
  FreeAndNil(FileContents);
  FreeAndNil(Garbage);
  FreeAndNil(xsdFileNames);
  FreeAndNil(ReadFileNames);
  if Assigned(NameSpaceList) then
    NameSpaceList.Clear;
  FreeAndNil(NameSpaceList);
  FreeAndNil(ChangedElementDefs);
  inherited;
end;

function TXsdDescr.ChangedElementTypedefsAsXml: TObject;
var
  xmlResult: TXml;
  x: Integer;
begin
  xmlResult := TXml.CreateAsString((ChangedElementDefs as TXml).Name, '');
  result := xmlResult;
  xmlResult.LoadValues(ChangedElementDefs as TXml, True);
end;

procedure TXsdDescr.ChangedElementTypedefsFromXml(aXml: TObject);
var
  xXml, yXml: TXml;
  x: Integer;
  xTypeDef: TXsdDataType;
  xNameSpace, xName: String;
begin
  xXml := ChangedElementDefs as TXml;
  xXml.LoadValues (aXml as TXml, True);
  for x := 0 to xXml.Items.Count - 1 do
  begin
    with xXml.Items.XmlItems[x] do
    begin
      if Name = 'ChangedElementTypedef' then
      begin
        xNameSpace:=Items.XmlValueByTag['NameSpace'];
        xName:=Items.XmlValueByTag['Name'];
        yXml := Items.XmlItemByTag['TypeDef'];
        if Assigned (yXml) then
        begin
          xTypeDef := FindTypeDef(yXml.Items.XmlValueByTag['NameSpace'], yXml.Items.XmlValueByTag['Name']);
          if Assigned (xTypeDef) then
            ChangeElementsDatatype(xNameSpace, xName, xTypeDef);
        end;
      end;
    end;
  end;
end;


function TXsdDescr.AddedTypeDefElementsAsXml: TObject;
var
  x, y: integer;
  nTypeDef: TXsdDataType;
  XmlResult: TXml;
  sXml: TXml;
begin
  XmlResult := TXml.CreateAsString('AddedTypeDefElements', '');
  result := XmlResult;
  for x := 0 to TypeDefs.Count - 1 do
  begin
    if TypeDefs.XsdDataTypes[x].Manually then
    begin
      nTypeDef := TypeDefs.XsdDataTypes[x];
      sXml := XmlResult.AddXml(TXml.CreateAsString('AddedTypeDefElement', ''));
      with sXml do
      begin
        AddXml (TXml.CreateAsString('UsedAt', nTypeDef.ManuallyUsedAtPath));
        for y := 0 to nTypeDef.ElementDefs.Count - 1 do
        begin
          with AddXml(TXml.CreateAsString('Added', '')) do
          begin
            AddXml(TXml.CreateAsString('NameSpace',
                nTypeDef.ElementDefs.Xsds[y].sType.NameSpace));
            AddXml(TXml.CreateAsString('Name',
                nTypeDef.ElementDefs.Xsds[y].sType.Name));
            AddXml(TXml.CreateAsString('ElementName',
                nTypeDef.ElementDefs.Xsds[y].ElementName));
          end;
        end;
      end;
    end;
  end;
end;

procedure TXsdDescr.AddDataTypeFacets(aTypeDef: TXsdDataType; aXml: TObject);
  function _Annotation (aXml: TXml): String;
  var
    annXml: TXml;
  begin
    result := '';
    annXml := aXml.Items.XmlItemByTag[tagAnnotation];
    if Assigned (annXml) then
      result := annXml.Items.XmlValueByTag[tagDocumentation];
  end;
  procedure _scan (aXml: TXml);
    function _facetVal (aFacet: String): String;
    var
      xXml: TXml;
    begin
      result := '';
      xXml := aXml.Items.XmlItemByTag[aFacet];
      if Assigned (xXml) then
        result := xXml.Attributes.ValueByTag[tagValue];
    end;
  var
    x: Integer;
    xEnumeration: TXsdEnumeration;
    xSep: String;
  begin
    aTypeDef.Length := _FacetVal(fctLength);
    aTypeDef.MinLength := _FacetVal(fctMinLength);
    aTypeDef.MaxLength := _FacetVal(fctMaxLength);
    aTypeDef.Whitespace := _FacetVal(fctWhitespace);
    aTypeDef.MaxInclusive := _FacetVal(fctMaxInclusive);
    aTypeDef.MaxExclusive := _FacetVal(fctMaxExclusive);
    aTypeDef.MinInclusive := _FacetVal(fctMinInclusive);
    aTypeDef.MinExclusive := _FacetVal(fctMinExclusive);
    aTypeDef.Numeric := _FacetVal(fctNumeric);
    aTypeDef.TotalDigits := _FacetVal(fctTotalDigits);
    aTypeDef.FractionalDigits := _FacetVal(fctFractionalDigits);
    xSep := '';
    for x := 0 to aXml.Items.Count - 1 do with aXml.Items do
    begin
      if XmlItems[x].Name = fctEnumeration then
      begin
        xEnumeration := TXsdEnumeration.Create;
        xEnumeration.Value := XmlItems[x].Attributes.ValueByTag[tagValue];
        xEnumeration.Annotation:= _Annotation (XmlItems[x]);
        aTypeDef.Enumerations.AddObject(xEnumeration.Value, xEnumeration);
      end;
      if XmlItems[x].Name = fctPattern then
      begin
        aTypeDef.Pattern := aTypeDef.Pattern
                          + xSep
                          + '(' + XmlItems[x].Attributes.ValueByTag[tagValue] + ')'
                          ;
        xSep := '|';
      end;
    end;
  end;
begin
  if not (aXml is TXml) then
    raise Exception.Create('Illegal: AddDataTypeFacets(aTypeDef: TXsdDataType; aXml: TObject)');
  _Scan (aXml as TXml);
end;

function TXsdDescr.AddAttributeDef(aTypeDef: TXsdDataType; aXml: TObject; aTargetNamespace: String): TXsdAttr;
var
  xXml: TXml;
  xRestricts: TXml;
  xAttr: TXmlAttribute;
  x: Integer;
begin
  if not (aXml is TXml) then
    raise Exception.Create('Illegal TXsdDescr.AddAttributeDef(aTypeDef: TXsdDataType; aXml: TObject; aTargetNamespace: String): TXsdAttr');
  xXml := aXml as TXml;
  result := TXsdAttr.Create(self);
  result.SourceFileName := xXml.SourceFileName;
  Garbage.AddObject('', result);
  result.Name := xXml.Attributes.ValueByTag[tagName];
  result.NameSpace := aTargetNamespace;
  xAttr := xXml.Attributes.AttributeByTag[tagType];
  if Assigned (xAttr) then
  begin
    result.BaseDataTypeName:=NameWithoutPrefix(xAttr.Value);
    result.BaseNameSpace:=xXml.PrefixToNameSpace(xAttr.Value);
  end;
  xAttr := xXml.Attributes.AttributeByTag[tagForm];
  if Assigned (xAttr) then
    result.FormDefaultQualified := (xAttr.Value = tagQualified);
  result.use := xXml.Attributes.ValueByTag[tagUse];
  AddDocumentation(result.Documentation, xXml);
  AddAppinfo(result.Appinfo, xXml);
  for x := 0 to xXml.Items.Count - 1 do
  begin
    if xXml.Items.XmlItems[x].Name = tagSimpleType then
    with xXml.Items.XmlItems[x] do
    begin
      xRestricts := Items.XmlItemByTag[tagRestriction];
      if Assigned (xRestricts) then
      begin
        xAttr := xRestricts.Attributes.AttributeByTag[tagBase];
        if Assigned (xAttr) then
        begin
          result.BaseDataTypeName := NameWithoutPrefix(xAttr.Value);
          result.BaseNameSpace := xRestricts.PrefixToNameSpace (xAttr.Value);
          if result.BaseNameSpace = '' then
            result.BaseNameSpace := aTargetNamespace;
        end;
        AddDataTypeFacets (result, xRestricts);
      end;
    end;
  end;
  aTypeDef.AttributeDefs.AddObject(result.Name, result);
end;

function TXsdDescr.AddSimpleType(aXml: TObject; aTargetNamespace: String; aGlobal: Boolean): TXsdDataType;
var
  f: integer;
  xXml: TXml;
  xRestricts: TXml;
  xBase: TXmlAttribute;
begin
  if not (aXml is TXml) then
    raise Exception.Create('Illegal: AddSimpleType(aXml: TObject; aTargetNamespace: String');
  xXml := aXml as TXml;
  result := TXsdDataType.Create(self);
  result.SourceFileName := xXml.SourceFileName;
  result.xsdType:= dtSimpleType;
  Garbage.AddObject('', result);
  result.Name := xXml.Attributes.ValueByTag[tagName];
  result.NameSpace := aTargetNamespace;
  if (not aGlobal)
  or (not TypeDefs.Find(result.NameSpace + ';' + result.Name, f)) then
  begin
    AddDocumentation(result.Documentation, xXml);
    AddAppinfo(result.Appinfo, xXml);
    xRestricts := xXml.Items.XmlItemByTag[tagRestriction];
    if Assigned(xRestricts) then
    begin
      xBase := xRestricts.Attributes.AttributeByTag[tagBase];
      if Assigned (xBase) then
      begin
        result.BaseDataTypeName := NameWithoutPrefix(xBase.Value);
        result.BaseNameSpace := xRestricts.PrefixToNameSpace (xBase.Value);
        if result.BaseNameSpace = '' then
          result.BaseNameSpace := aTargetNamespace;
      end;
      AddDataTypeFacets (result, xRestricts);
    end;
    result.IsBuiltIn := ArrayContains(result.BaseDataTypeName, builtInTypeNames)
                    and (result.BaseNameSpace = scXMLSchemaURI)
                      ;
    if aGlobal then
      TypeDefs.AddObject(result.NameSpace + ';' + result.Name, result);
  end;
end;

function TXsdDescr.ArrayContains(const Value: String;
  const Values: array of String): Boolean;
var
  I: integer;
begin
  for I := Low(Values) to High(Values) do
    if Value = Values[I] then
    begin
      result := True;
      exit;
    end;
  result := False;
end;

function TXsdDescr.AddComplexType(aXml: TObject; aTargetNamespace: String; isGlobalDefined: Boolean): TXsdDataType;
  function _ifthen(aBool: Boolean; aTrue, aFalse: String): String;
  begin
    if aBool then
      result := aTrue
    else
      result := aFalse;
  end;
  procedure  _FillExtentionBase(aXml: TXml; aType: TXsdDataType);
  var
    xBase: TXmlAttribute;
  begin
    xBase := aXml.Attributes.AttributeByTag[tagBase];
    if Assigned (xBase) then
    begin
      aType.IsExtention := True;
      aType.BaseDataTypeName := NameWithoutPrefix(xBase.Value);
      aType.BaseNameSpace := aXml.PrefixToNameSpace (xBase.Value);
      if aType.BaseNameSpace = '' then
        aType.BaseNameSpace := aTargetNamespace;
    end;
  end;
  procedure  _FillRestrictionBase(aXml: TXml; aType: TXsdDataType);
  var
    xBase: TXmlAttribute;
  begin
    xBase := aXml.Attributes.AttributeByTag[tagBase];
    if Assigned (xBase) then
    begin
      aType.BaseDataTypeName := NameWithoutPrefix(xBase.Value);
      aType.BaseNameSpace := aXml.PrefixToNameSpace (xBase.Value);
      if aType.BaseNameSpace = '' then
        aType.BaseNameSpace := aTargetNamespace;
    end;
  end;
  function _ContentModel (aXml: TXml): String;
  var
    x: Integer;
  begin
    result := '';
    for x := 0 to aXml.Items.Count - 1 do with aXml.Items.XmlItems[x] do
    begin
      if Name = tagAll then result := _ifthen (result = '', 'All', 'Sequence');
      if Name = tagChoice then result := _ifthen (result = '', 'Choice', 'Sequence');
      if Name = tagSequence then result := _ifthen (result = '', 'Sequence', 'Sequence');
      if Name = tagGroup then result := _ifthen (result = '', 'Sequence', result);
    end;
    if Result = '' then Result := 'Empty';  // weet je het zeker ???
  end;
  procedure _SearchAndAdd (aXml: TXml; aTypeDef: TXsdDataType; aForceOptional: Boolean);
  {
    Xml-schema allows nested Sequences and Choices
    In this implementation we will make a flat list out of a nested structure
    loosing and changing some information which will allow invalid combinations of xml-elements
  }
  var
    x: Integer;
  begin
    if aXml.Name = tagAttribute then
      AddAttributeDef(aTypeDef, aXml, aTargetNameSpace);
    if (aXml.Name = tagElement)
    or (aXml.Name = tagGroup) then
      with AddElement(aTypeDef, aXml, aTargetNameSpace, False) do
      begin
        if aForceOptional then
          minOccurs := '0';
      end;
    if (aXml.Name = tagAll)
    or (aXml.Name = tagChoice)
    or (aXml.Name = tagSequence)
    or (aXml.Name = tagExtension)
    or (aXml.Name = tagRestriction)
    then
      for x := 0 to aXml.Items.Count - 1 do
        _SearchAndAdd (aXml.Items.XmlItems[x], aTypeDef, (aXml.Name = tagChoice));
  end;
var
  xXml: TXml;
  x, y, f: Integer;
begin
  if not (aXml is TXml) then
    raise Exception.Create('Illegal: AddComplexType(aXml: TObject; aTargetNamespace: String; isGlobalDefined: Boolean): TXsdDataType');
  xXml := aXml as TXml;
  result := TXsdDataType.Create(self);
  result._isGroup := (xXml.Name = tagGroup);
  result.SourceFileName := xXml.SourceFileName;
  result.xsdType:= dtComplexType;
  Garbage.AddObject('', result);
  result.Name := xXml.Attributes.ValueByTag[tagName];
  result.NameSpace := aTargetNamespace;
  if (not isGlobalDefined)
  or not TypeDefs.Find(result.NameSpace + ';' + result.Name, f) then
  begin
    AddDocumentation(result.Documentation, xXml);
    AddAppinfo(result.Appinfo, xXml);
    // OK all
    // anyAttribute
    // attribute
    // attributeGroup
    // OK choice
    // OK complexContent
    // group
    // OK sequence
    // OK simpleContent
    result.IsComplex:=True;
    result.IsBuiltIn:=False;
    result.isAbstract:= xXml.Attributes.BooleanByTag[tagAbstract];
    result.isMixed:= xXml.Attributes.BooleanByTag[tagMixed];
    result.ContentModel:= _ContentModel (xXml);
    for x := 0 to xXml.Items.Count - 1 do
    begin
      if (xXml.Items.XmlItems[x].Name = tagAttribute)
      then begin
         AddAttributeDef (result, xXml.Items.XmlItems[x], aTargetNamespace);
      end;
      if (xXml.Items.XmlItems[x].Name = tagAll)
      or (xXml.Items.XmlItems[x].Name = tagChoice)
      or (xXml.Items.XmlItems[x].Name = tagSequence)
      then begin
        _SearchAndAdd (xXml.Items.XmlItems[x], result, False);
      end;
      if (xXml.Items.XmlItems[x].Name = tagComplexContent)
      or (xXml.Items.XmlItems[x].Name = tagSimpleContent)
      then with xXml.Items.XmlItems[x] do
      begin
        for y := 0 to Items.Count - 1 do
        begin
          if Items.XmlItems[y].Name = tagRestriction then
          begin
            result.DerivationMethod := 'Restriction';
            _FillRestrictionBase(Items.XmlItems[y], result);
            _SearchAndAdd (Items.XmlItems[y], result, False);
          end;
          if Items.XmlItems[y].Name = tagExtension then
          begin
            result.DerivationMethod := 'Extension';
            if Items.XmlItems[y].Attributes.ValueByTag[tagBase] = 'simple02-99' then
              result.DerivationMethod := 'Extension';
            _FillExtentionBase(Items.XmlItems[y], result);
            _SearchAndAdd (Items.XmlItems[y], result, False);
          end;
        end;
      end;
    end;
  end;
  if isGlobalDefined then
    TypeDefs.AddObject(result.NameSpace + ';' + result.Name, result);
end;

procedure TXsdDescr.AppendDoc (var aDoc: String ; aString: String);
begin
  if Length (aDoc) > 0 then
    aDoc := aDoc + LineEnding + aString
  else
    aDoc := aString;
end;


procedure TXsdDescr.AddBuiltIns;
var
  xTypeDef: TXsdDataType;
  x, f: integer;
begin
  for x := Low(xmlzConsts.builtInTypeNames) to High(xmlzConsts.builtInTypeNames) do
  begin
    if not TypeDefs.Find(scXMLSchemaURI + ';' + xmlzConsts.builtInTypeNames[x], f) then
    begin
      xTypeDef := TXsdDataType.Create(self);
      Garbage.AddObject('', xTypeDef);
      xTypeDef.xsdType := dtSimpleType;
      xTypeDef.Name := xmlzConsts.builtInTypeNames[x];
      xTypeDef.NameSpace := scXMLSchemaURI;
      xTypeDef.IsBuiltIn := True;
      xTypeDef.BaseDataTypeName := xTypeDef.Name;
      xTypeDef.xsdDescr := self;
      TypeDefs.AddObject(xTypeDef.NameSpace + ';' + xTypeDef.Name, xTypeDef);
    end;
  end;
end;

procedure TXsdDescr.AddXsdFromXml(aXml: TObject; aFileName: String; ErrorFound: TOnErrorEvent);
var
  x: Integer;
  xXml: TXml;
  xTargetNameSpace: String;
  swapElementFormDefaultQualified: Boolean;
  swapAttributeFormDefaultQualified: Boolean;
begin
  if not (aXml is TXml) then raise Exception.Create('Illegal arg: TXsdDescr.AddXsdFromXml(aXml: TObject; aFileName: String; ErrorFound: TOnErrorEvent)');
  xXml := aXml as TXml;
  if not (    (xXml.Name = tagSchema)
         )
  and not (    (xXml.Name = tagTypes)
          ) then
    raise Exception.CreateFmt ('%s is not a XML schema (%s:%s)', [aFileName, xXml.NameSpace, xXml.Name]);
  swapElementFormDefaultQualified := xsdElementFormDefaultQualified;
  swapAttributeFormDefaultQualified := xsdAttributeFormDefaultQualified;
  try
    if xXml.Name = tagTypes then
    begin
      for x := 0 to xXml.Items.Count - 1 do with xXml.Items do
        if XmlItems[x].Name = tagSchema then
          AddXsdFromXml (XmlItems[x], aFileName, ErrorFound);
      exit; // only here for the schema(s)
    end;
    xTargetNameSpace := xXml.Attributes.ValueByTag[tagTargetNamespace];
    for x := 0 to xXml.Items.Count - 1 do with xXml.Items do
    begin
      if (XmlItems[x].Name = tagImport)
      then
        AddXsdFromFile ('', ExpandRelativeFileName(aFileName, XmlItems[x].Attributes.ValueByTag[tagSchemaLocation]), ErrorFound);
      if (XmlItems[x].Name = tagInclude)
      then
        AddXsdFromFile (xTargetNameSpace, ExpandRelativeFileName(aFileName, XmlItems[x].Attributes.ValueByTag[tagSchemaLocation]), ErrorFound);
    end;
    xsdElementFormDefaultQualified := (xXml.Attributes.ValueByTag[tagElementFormDefault] = tagQualified);
    xsdAttributeFormDefaultQualified := (xXml.Attributes.ValueByTag[tagAttributeFormDefault] = tagQualified);
    for x := 0 to xXml.Items.Count - 1 do with xXml.Items do
      if XmlItems[x].Name = tagSimpleType then
        AddSimpleType (XmlItems[x], xTargetNameSpace, True);
    for x := 0 to xXml.Items.Count - 1 do with xXml.Items do
      if (XmlItems[x].Name = tagComplexType)
      or (XmlItems[x].Name = tagGroup) then
        AddComplexType (XmlItems[x], xTargetNameSpace, True);
    for x := 0 to xXml.Items.Count - 1 do with xXml.Items do
      if XmlItems[x].Name = tagElement then
        AddElement (TypeDef, XmlItems[x], xTargetNameSpace, True);
  finally
    xsdElementFormDefaultQualified := swapElementFormDefaultQualified;
    xsdAttributeFormDefaultQualified := swapAttributeFormDefaultQualified;
  end;
end;

function TXsdDescr.AddTypeDefFromJsonXml (aFileName, aNameSpace: String; aXml: TObject; ErrorFound: TOnErrorEvent): TXsdDataType;
  function _AddTypeDefFromJsonXml (aList: TXsdDataTypeList; aXml: TXml): TXsdDataType;
    procedure _baseTypeToJson (aType: TXsdDataType);
    begin
      if aType.BaseDataTypeName = 'string' then aType.jsonType := jsonString;
      if aType.BaseDataTypeName = 'object' then
      begin
        aType.jsonType := jsonObject;
        aType.ContentModel := 'All';
      end;
      if aType.BaseDataTypeName = 'array' then
      begin
        aType.jsonType := jsonArray;
        aType.ContentModel := 'All';
      end;
      if aType.BaseDataTypeName = 'boolean' then aType.jsonType := jsonBoolean;
      if aType.BaseDataTypeName = 'number' then
      begin
        aType.jsonType := jsonNumber;
        aType.BaseDataTypeName := 'float';
      end;
      if aType.BaseDataTypeName = 'integer' then aType.jsonType := jsonNumber;
      if aType.BaseDataTypeName = 'any' then
      begin
        aType.jsonType := jsonObject;
        aType.BaseDataTypeName := 'anyType';
      end;
      aType.BaseDataType := self.FindTypeDef(scXMLSchemaURI, aType.BaseDataTypeName);
    end;

    procedure _scan (aXml: TXml);
    var
      x, y, z, f: Integer;
      xXml, yXml, zXml: TXml;
      xEnum: TXsdEnumeration;
      xXsd, yXsd: TXsd;
      xMinExcl, xMaxExcl: Boolean;
    begin
      xMaxExcl := False;
      xMinExcl := False;
      for x := 0 to aXml.Items.Count - 1 do
      begin
        xXml := aXml.Items.XmlItems[x];
        if (xXml.Name = '_') then
          for y := 0 to xXml.Items.Count - 1 do
            _scan(xXml.Items.XmlItems[y]);
        if (xXml.Name = 'allOf') then // TODO ...
          for y := 0 to xXml.Items.Count - 1 do
            _scan(xXml.Items.XmlItems[y]);
        if (xXml.Name = 'anyOf') then // TODO ...
          for y := 0 to xXml.Items.Count - 1 do
            _scan(xXml.Items.XmlItems[y]);
        if (xXml.Name = 'oneOf') then // TODO ...
          for y := 0 to xXml.Items.Count - 1 do
            _scan(xXml.Items.XmlItems[y]);
        if (xXml.Name = 'properties') then
        begin
          for y := 0 to xXml.Items.Count - 1 do
          begin
            yXml := xXml.Items.XmlItems[y];
            xXsd := TXsd.Create(self);
            self.Garbage.AddObject('', xXsd);
            xXsd.ElementName := yXml.Name;
            xXsd.sType := self.AddTypeDefFromJsonXml(aFileName, aNameSpace + '/' + result.Name, yXml, ErrorFound);
            xXsd.sType.Name := xXsd.ElementName;
            xXsd.minOccurs := '0';
            result.ElementDefs.AddObject(xXsd.ElementName, xXsd);
          end;
        end;
        if xXml.Name = S_DOLLARREF then
        begin
          result.dollarRef := xXml.Value;
          if result.dollarRef[1] = '#' then
            result.dollarRef := aFileName + Copy (result.dollarRef, 2, 10000)
          else
            result.dollarRef := ExpandRelativeFileName(aFileName, result.dollarRef);
        end;
        if xXml.Name = 'type' then
        begin
          result.BaseDataTypeName := xXml.Value;
          _baseTypeToJson(result);
        end;
        if xXml.Name = 'schema' then
        begin
          for y := 0 to xXml.Items.Count - 1 do
          begin
            yXml := xXml.Items.XmlItems[y];
            if (yXml.Name = '_') then
              for z := 0 to yXml.Items.Count - 1 do
                _scan(yXml.Items.XmlItems[z]);
            if (yXml.Name = 'allOf') then // TODO ...
              for z := 0 to yXml.Items.Count - 1 do
                _scan(yXml.Items.XmlItems[z]);
            if (yXml.Name = 'anyOf') then // TODO ...
              for z := 0 to yXml.Items.Count - 1 do
                _scan(yXml.Items.XmlItems[z]);
            if (yXml.Name = 'oneOf') then // TODO ...
              for z := 0 to yXml.Items.Count - 1 do
                _scan(yXml.Items.XmlItems[z]);
            if yXml.Name = 'type' then
            begin
              result.BaseDataTypeName := yXml.Value;
              _baseTypeToJson(result);
            end;
            if yXml.Name = 'items' then
            begin
              yXsd := TXsd.Create(self);
              self.Garbage.AddObject('', yXsd);
              yXsd.ElementName := '_';
              yXsd.sType := self.AddTypeDefFromJsonXml(aFileName, aNameSpace + '/' + result.Name, yXml, ErrorFound);
              yXsd.sType.Name := yXsd.ElementName;
              yXsd.minOccurs := '0';
              yXsd.maxOccurs := 'unbounded';
              result.ElementDefs.AddObject(yXsd.ElementName, yXsd);
            end;
            if yXml.Name = S_DOLLARREF then
            begin
              result.dollarRef := yXml.Value;
              if result.dollarRef[1] = '#' then
                result.dollarRef := aFileName + Copy (result.dollarRef, 2, 10000)
              else
                result.dollarRef := ExpandRelativeFileName(aFileName, result.dollarRef);
            end;
          end;
        end;
        if xXml.Name = 'collectionFormat' then
        begin
          if xXml.Value = 'csv' then result.CollectionFormat := ocfCSV;
          if xXml.Value = 'ssv' then result.CollectionFormat := ocfSSV;
          if xXml.Value = 'tsv' then result.CollectionFormat := ocfTSV;
          if xXml.Value = 'pipes' then result.CollectionFormat := ocfPipes;
          if xXml.Value = 'multi' then result.CollectionFormat := ocfMulti;
        end;
        if xXml.Name = 'format' then;
        if (xXml.Name = 'items') then
        begin
          xXsd := TXsd.Create(self);
          self.Garbage.AddObject('', xXsd);
          xXsd.ElementName := '_';
          xXsd.sType := self.AddTypeDefFromJsonXml(aFileName, aNameSpace + '/' + result.Name, xXml, ErrorFound);
          xXsd.sType.Name := xXsd.ElementName;
          xXsd.minOccurs := '0';
          xXsd.maxOccurs := 'unbounded';
          result.ElementDefs.AddObject(xXsd.ElementName, xXsd);
        end;
        if xXml.Name = 'title' then result.Documentation.Add(xXml.Value);
        if xXml.Name = 'description' then
        begin
          result.Documentation.Add (xXml.Value);
        end;
        if xXml.Name = 'default ' then result.DefaultValue := xXml.Value;
        if xXml.Name = 'multipleOf' then ;
        if xXml.Name = 'maximum' then result.MaxInclusive := xXml.Value;
        if xXml.Name = 'exclusiveMaximum' then xMaxExcl := xXml.ValueAsBoolean;
        if xXml.Name = 'minimum' then result.MinInclusive := xXml.Value;
        if xXml.Name = 'exclusiveMinimum' then xMinExcl := xXml.ValueAsBoolean;
        if xXml.Name = 'maxLength' then result.MaxLength := xXml.Value;
        if xXml.Name = 'minLength' then result.MinLength := xXml.Value;
        if xXml.Name = 'pattern' then result.Pattern := xXml.Value;
        if xXml.Name = 'maxItems' then result.MaxItems := xXml.Value;
        if xXml.Name = 'minItems' then result.MinItems := xXml.Value;
        if xXml.Name = 'uniqueItems' then result.uniqueItems := xXml.Value;
        if xXml.Name = 'maxProperties' then result.maxProperties := xXml.Value;
        if xXml.Name = 'minProperties' then result.minProperties := xXml.Value;
        if xXml.Name = 'enum' then
        begin
          for y := 0 to xXml.Items.Count - 1 do
          begin
            xEnum := TXsdEnumeration.Create;
            xEnum.Value := xXml.Items.XmlItems[y].Value;
            result.Enumerations.AddObject(xEnum.Value, xEnum);
          end;
        end;

        if xXml.Name = 'xml' then
        begin
          for y := 0 to xXml.Items.Count - 1 do with xXml.Items.XmlItems[y] do
          begin
            if Name = 'name' then result.JsonXmlName := Value;
          end;
        end;
      end;

      if xMaxExcl then
      begin
        result.MaxExclusive := result.MaxInclusive;
        result.MaxInclusive := '';
      end;
      if xMinExcl then
      begin
        result.MinExclusive := result.MinInclusive;
        result.MinInclusive := '';
      end;

      xXml := aXml.Items.XmlItemByTag['required'];
      if Assigned(xXml) then
      begin
        for y := 0 to xXml.Items.Count - 1 do
        begin
          yXml := xXml.Items.XmlItems[y];
          xXsd := result.ElementByName[yXml.Value];
          if Assigned (xXsd) then
            xXsd.minOccurs := '1';
        end;
      end;
    end;

  var
    xDoc: String;
  begin
    result := TXsdDataType.Create(self);
    result.xsdType:= dtSimpleType;
    Garbage.AddObject('', result);
    result.Name := aXml.Name;
    result.NameSpace := aNameSpace;
    self.TypeDefs.AddObject(result.NameSpace + '/' + result.Name, result);
    xDoc := '';
    _scan(aXml);
  end;
begin
  if not (aXml is TXml) then raise Exception.Create('Illegal arg: TXsdDescr.AddXsdFromJsonXml(aXml: TObject; ErrorFound: TOnErrorEvent)');
  result := _AddTypeDefFromJsonXml (TypeDefs, aXml as TXml);
end;

procedure TXsdDescr.AddXsdFromFile(aOverruleNamespace, aFileName: String; ErrorFound: TOnErrorEvent);
  procedure _inheritTargetNamespace (aXml: TXml);
  var
    xAttr: TXmlAttribute;
  begin
    xAttr := aXml.Attributes.AttributeByTag[tagTargetNamespace];
    if Assigned (xAttr) then
      xAttr.Value := aOverruleNamespace
    else
      aXml.AddAttribute(TXmlAttribute.CreateAsString(tagTargetNamespace, aOverruleNamespace));
  end;
var
  x: Integer;
  xXml: TXml;
begin
  if ReadFileNames.Find(aOverruleNamespace + ';' + aFileName, x) then Exit;
  ReadFileNames.Add(aOverruleNamespace + ';' + aFileName);
  xXml := TXml.Create;
  try
    xXml.LoadFromString(ReadStringFromFile(aFileName), ErrorFound);
    xXml.PopulateSourceFileName(aFileName);
    xXml.SeparateNsPrefixes;
    xXml.ResolveNameSpaces;
    if aOverruleNamespace <> '' then
      _inheritTargetNamespace (xXml);
    AddXsdFromXml(xXml, aFileName, ErrorFound);
  finally
    xXml.Free;
  end;
end;

function TXsdDescr.LoadXsdFromFile(aFileName: String; ErrorFound: TOnErrorEvent): Boolean;
begin
  result := False;
  Clear;
  AddBuiltIns;
  if UpperCase(LeftStr(aFileName, 7)) <> 'HTTP://' then
    fMainFileName := ExpandFileNameUTF8(aFileName)
  else
    fMainFileName := aFileName;
  AddXsdFromFile ('', fMainFileName, ErrorFound);
  Finalise;
end;

function TXsdDescr.LoadXsdFromXmlSampleFile (aFileName: String; ErrorFound: TOnErrorEvent): TXsd ;
var
  xXml: TXml;
begin
  xXml := TXml.Create;
  try
    xXml.LoadFromFile(aFileName,nil);
    if xXml.Name = '' then
      raise Exception.Create('LoadXsdFromXmlSampleFile: Could not read as Xml: ' + aFileName);
    xXml.SeparateNsPrefixes;
    xXml.ResolveNameSpaces;
    result := CreateXsdFromXmlSample (xXml, False);
  finally
    xXml.Free;
  end;
end;

function TXsdDescr.LoadXsdFromJsonSampleFile (aFileName: String; ErrorFound: TOnErrorEvent): TXsd ;
var
  xXml: TXml;
begin
  xXml := TXml.Create;
  try
    try
      xXml.LoadJsonFromFile(aFileName,nil);
    except
      raise Exception.Create('LoadXsdFromXmlSampleFile: Could not read as Json: ' + aFileName);
    end;
    result := CreateXsdFromJsonSample (xXml, False);
    result.FileName := aFileName;
  finally
    xXml.Free;
  end;
end;

function TXsdDescr .LoadXsdFromString (aString : String ;
  ErrorFound : TOnErrorEvent ): Boolean ;
var
  xXml: TXml;
begin
  result := False;
  Clear;
  AddBuiltIns;
  xXml := TXml.Create;
  try
    xXml.LoadFromString(aString, ErrorFound);
    xXml.SeparateNsPrefixes;
    xXml.ResolveNameSpaces;
    AddXsdFromXml(xXml, '', ErrorFound);
  finally
    xXml.Free;
  end;
  Finalise;
end;

function TXsd.IsTypeDefEnabled: Boolean;
begin
  result := (    Assigned(self)
             and (   (    sType.IsExtention
                      and Assigned(sType.BaseDataType)
                     )
                  or (sType.ExtendedByList.Count > 0)
                  or (    (sType.xsdType = dtComplexType)
                      and (sType.ElementDefs.Count = 0)
                      and (sType.AttributeDefs.Count = 0)
                     )
                 )
            );
end;

procedure TXsd.GenerateReport(aStringList: TStringList);
  function IndentString(x: integer): String;
  begin
    result := '';
    while x > 0 do
    begin
      result := result + '  '; // 2 spaces
      Dec(x);
    end;
  end;
  procedure _GenerateReport(aIndent: integer; aXsd: TXsd;
    aParentType: TXsdDataType);
  var
    xString: String;
    xSeparator: String;
    x: integer;
  begin
    xString := IndentString(aIndent);
    xString := xString + aXsd.ElementName + #9;
    xString := xString + aXsd.minOccurs + #9;
    xString := xString + aXsd.maxOccurs + #9;
    xString := xString + aXsd.sType.ContentModel + #9;
    xString := xString + aXsd.sType.Name + #9;
    xString := xString + aXsd.sType.BaseDataTypeName + #9;
    if aXsd.sType.Length <> '' then
    begin
      xString := xString + aXsd.sType.Length + #9;
      xString := xString + aXsd.sType.Length + #9;
    end
    else
    begin
      xString := xString + aXsd.sType.MinLength + #9;
      xString := xString + aXsd.sType.MaxLength + #9;
    end;
    xString := xString + aXsd.sType.MaxInclusive + #9;
    xString := xString + aXsd.sType.MaxExclusive + #9;
    xString := xString + aXsd.sType.MinInclusive + #9;
    xString := xString + aXsd.sType.MinExclusive + #9;
    xString := xString + aXsd.sType.Numeric + #9;
    xString := xString + aXsd.sType.TotalDigits + #9;
    xString := xString + aXsd.sType.FractionalDigits + #9;
    xString := xString + aXsd.sType.Whitespace + #9;
    xSeparator := '';
    for x := 0 to aXsd.sType.Enumerations.Count - 1 do
    begin
      xString := xString + xSeparator + aXsd.sType.Enumerations.Strings[x];
      xSeparator := ';';
    end;
    xString := xString + aXsd.sType.Pattern + #9;
    xString := xString + aXsd.ElementNameSpace + #9;
    xString := xString + IntToStr(aIndent);
    aStringList.Add(xString);
    if aXsd.sType._DepthBillOfMaterial > 0 then
      aStringList.Add(IndentString(aIndent + 1) + 'recursive...')
    else
    begin
      Inc(aXsd.sType._DepthBillOfMaterial);
      try
        for x := 0 to aXsd.sType.ElementDefs.Count - 1 do
        begin
          _GenerateReport(aIndent + 1, aXsd.sType.ElementDefs.Xsds[x],
            aXsd.sType);
        end;
      finally
        Dec(aXsd.sType._DepthBillOfMaterial);
      end;
    end;
  end;

begin
  aStringList.Clear;
  aStringList.Add('Tag' + #9 + 'minOccurs' + #9 + 'maxOccurs' + #9 +
      'ContentModel' + #9 + 'DataType' + #9 + 'BaseDataType' + #9 +
      'minLength' + #9 + 'maxLength' + #9 + 'MaxIncl' + #9 +
      'MaxExcl' + #9 + 'MinIncl' + #9 + 'MinExcl' + #9 + 'Numeric' + #9 +
      'TotalDigits' + #9 + 'FractionalDigits' + #9 + 'WhiteSpace' + #9 +
      'Tokens' + #9 + 'NameSpace' + #9 + 'nsPrefix' + #9 + 'Indent');
  _GenerateReport(0, self, nil);
end;

procedure TXsd.GenerateCopyBook(aPrefix, aSuffix, a88Prefix, a88Suffix,
  aLevel: String; aStringList: TStringList);
  function IndentString(x: integer): String;
  begin
    result := '';
    x := 3 * x - 2;
    while x > 0 do
    begin
      result := result + ' ';
      Dec(x);
    end;
  end;
  procedure _GenerateReport(aIndent: integer; aXsd: TXsd);
  var
    xString: String;
    x: integer;
  begin
    if aXsd._Processed then
      exit;
    xString := IndentString(aIndent) + Format('0%d ', [aIndent])
      + aPrefix + aXsd.ElementName + aSuffix + aXsd.sType.ccbPictureClause;
    if (aXsd.maxOccurs <> '1') and (aXsd.maxOccurs <> '01') then
      xString := xString + ' occurs ' + aXsd.maxOccurs + ' times';
    xString := xString + '.';
    aStringList.Add(xString);
    for x := 0 to aXsd.sType.Enumerations.Count - 1 do
    begin
      xString := IndentString(aIndent + 1)
        + '88 ' + a88Prefix + aXsd.sType.Enumerations.Strings[x]
        + a88Suffix + ' value "' + aXsd.sType.Enumerations.Strings[x] + '".';
      aStringList.Add(xString);
    end;

    aXsd._Processed := True;
    try
      for x := 0 to aXsd.sType.ElementDefs.Count - 1 do
      begin
        _GenerateReport(aIndent + 1, aXsd.sType.ElementDefs.Xsds[x]);
      end;
    finally
      aXsd._Processed := False;
    end;
  end;

begin
  aStringList.Clear;
  _GenerateReport(StrToInt(aLevel), self);
end;

function TXsdDataType.ccbPictureClause: string;
var
  x, l: integer;
begin
  result := '';
  if ElementDefs.Count > 0 then
    exit;
  result := ' ' + Name + ':' + BaseDataTypeName;
  if BaseDataTypeName = 'string' then
  begin
    if MaxLength <> '' then
    begin
      result := ' pic x(' + MaxLength + ')';
      exit;
    end;
    if Enumerations.Count > 0 then
    begin
      l := 1;
      for x := 0 to Enumerations.Count - 1 do
      begin
        if system.Length(Enumerations.Strings[x]) > l then
          l := system.Length(Enumerations.Strings[x]);
      end;
      result := ' pic x(' + IntToStr(l) + ')';
      exit;
    end;
    if Pattern <> '' then
    begin
      result := ' pic BasedOnPattern(' + Pattern + ')';
      exit;
    end;
    exit;
  end;
  if BaseDataTypeName = 'boolean' then
  begin
    result := ' pic x(1)';
    exit;
  end;
end;

function TXsd.ccbOccursClause: String;
begin
  result := '';
  if (maxOccurs <> '1') and (maxOccurs <> '01') then
    result := ' occurs ' + maxOccurs + ' times';
end;

procedure TXsd.GenerateHtmlReport(aStringList: TStringList);
  function IndentString(x: integer): String;
  begin
    result := '';
    while x > 0 do
    begin
      result := result + '.';
      Dec(x);
    end;
  end;
  procedure _GenerateReport(aIndent: integer; aXsd: TXsd);
  var
    xString: String;
    xSeparator: String;
    x: integer;
  begin
    xString := '<tr bgcolor="#FFFFFF" align="left">';
    xString := xString + '<td valign="top">';
    if aXsd.sType.ElementDefs.Count > 0 then
      xString := xString + '<a name="' + aXsd.sType.Name + '"/>';
    xString := xString + IndentString(aIndent)
      + aXsd.ElementName + '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.minOccurs + '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.maxOccurs + '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.sType.ContentModel +
      '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.sType.Name + '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.sType.BaseDataTypeName +
      '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.sType.MinLength +
      '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.sType.MaxLength +
      '&nbsp;</td>';
    xString := xString + '<td valign="top">';
    xSeparator := '';
    for x := 0 to aXsd.sType.Enumerations.Count - 1 do
    begin
      xString := xString + xSeparator + aXsd.sType.Enumerations.Strings[x];
      xSeparator := '<br>';
    end;
    xString := xString + '&nbsp;</td>';
    xString := xString + '<td valign="top">';
    xSeparator := '';
    for x := 0 to aXsd.sType.Documentation.Count - 1 do
    begin
      xString := xString + xSeparator + aXsd.sType.Documentation.Strings[x];
      xSeparator := '<br>';
    end;
    for x := 0 to aXsd.sType.Appinfo.Count - 1 do
    begin
      xString := xString + xSeparator + aXsd.sType.Appinfo.Strings[x];
      xSeparator := '<br>';
    end;
    xString := xString + '&nbsp;</td>';
    xString := xString + '</tr>';
    aStringList.Add(xString);
    for x := 0 to aXsd.sType.ElementDefs.Count - 1 do
    begin
      _GenerateReport(aIndent + 2, aXsd.sType.ElementDefs.Xsds[x]);
    end;
  end;

begin
  aStringList.Clear;
  aStringList.Add('<html>');
  aStringList.Add('<a name="_home"/>');
  aStringList.Add('<p><table border="1">');
  aStringList.Add('<tr bgcolor="#FFFFFF" align="left">' +
      '<td valign="top"><b>Tag&nbsp;</b></td>' +
      '<td valign="top"><b>minOccurs&nbsp;</b></td>' +
      '<td valign="top"><b>maxOccurs&nbsp;</b></td>' +
      '<td valign="top"><b>ContentModel&nbsp;</b></td>' +
      '<td valign="top"><b>DataType&nbsp;</b></td>' +
      '<td valign="top"><b>BaseDataType&nbsp;</b></td>' +
      '<td valign="top"><b>minLength&nbsp;</b></td>' +
      '<td valign="top"><b>maxLength&nbsp;</b></td>' +
      '<td valign="top"><b>Tokens&nbsp;</b></td>' +
      '<td valign="top"><b>Annotation&nbsp;</b></td>' + '</tr>');
  _GenerateReport(0, self);
  aStringList.Add('</table>');
  aStringList.Add(
    '<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>');
  aStringList.Add(
    '<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>');
  aStringList.Add('Previous lines intentional left blank');
  aStringList.Add('</html>');
end;

function TXsdDataType.populateValue(aTag: String): String;
begin
  if ElementDefs.Count > 0 then
    exit;
  if Enumerations.Count > 0 then
  begin
    result := Enumerations.Strings[0];
    exit;
  end;
  if MinInclusive <> '' then
  begin
    result := MinInclusive;
    exit;
  end;
  if MaxInclusive <> '' then
  begin
    result := MinInclusive;
    exit;
  end;
  if MinExclusive <> '' then
  begin
    result := MinExclusive;
    exit;
  end;
  if MaxExclusive <> '' then
  begin
    result := MaxExclusive;
    exit;
  end;
  if BaseDataTypeName = 'boolean' then
  begin
    result := 'true';
    exit;
  end;
  if BaseDataTypeName = 'byte' then
  begin
    result := '123';
  end;
  if (BaseDataTypeName = 'decimal') or (BaseDataTypeName = 'double') or
    (BaseDataTypeName = 'float') then
  begin
    result := '122.745';
  end;
  if (BaseDataTypeName = 'int') or (BaseDataTypeName = 'integer') or
    (BaseDataTypeName = 'long') then
  begin
    result := '642';
  end;
  if (BaseDataTypeName = 'negativeInteger') then
  begin
    result := '-25556';
  end;
  if (BaseDataTypeName = 'nonNegativeInteger') then
  begin
    result := '125556';
  end;
  if (BaseDataTypeName = 'nonPositiveInteger') then
  begin
    result := '-56';
  end;
  if (BaseDataTypeName = 'short') then
  begin
    result := '67';
  end;
  if (BaseDataTypeName = 'unsignedByte') or (BaseDataTypeName = 'unsignedInt')
    or (BaseDataTypeName = 'unsignedLong') or
    (BaseDataTypeName = 'unsignedShort') or
    (BaseDataTypeName = 'positiveInteger') then
  begin
    result := '255';
  end;
  if (BaseDataTypeName = 'date') then
  begin
    result := '2020-12-20';
  end;
  if (BaseDataTypeName = 'dateTime') then
  begin
    result := '1999-09-20T04:05:06.12300';
  end;
  if (BaseDataTypeName = 'duration') then
  begin
    result := '-P120D';
  end;
  if (BaseDataTypeName = 'gDay') then
  begin
    result := '---05';
  end;
  if (BaseDataTypeName = 'gMonth') then
  begin
    result := '--07';
  end;
  if (BaseDataTypeName = 'gMonthDay') then
  begin
    result := '--08-17';
  end;
  if (BaseDataTypeName = 'gYear') then
  begin
    result := '1956';
  end;
  if (BaseDataTypeName = 'gYearMonth') then
  begin
    result := '1956-08';
  end;
  if (BaseDataTypeName = 'date') then
  begin
    result := '2010-12-11';
  end;
  if (BaseDataTypeName = 'time') then
  begin
    result := '13:20:00.100000';
  end;
  if (BaseDataTypeName = 'dateTime') then
  begin
    result := '2010-12-11T13:30:00.200000';
  end;
  if (BaseDataTypeName = 'qName') then
  begin
    result := 'ns:Name';
  end;
  if (BaseDataTypeName = 'base64Binary') then
  begin
    result := 'C30B11734a4fafce43faf48be03d34e12FBe168f';
  end;
  if (BaseDataTypeName = 'hexBinary') then
  begin
    result := 'C30B117FF';
  end;

  if (result = '') then
    result := aTag;
  if MaxLength <> '' then
  begin
    try
      result := Copy(result, 1, StrToInt(MaxLength));
    except
    end;
  end;
end;

function TXsd.isRequired: Boolean;
begin
  result := (StrToIntDef(minOccurs, 0) > 0);
end;

function TXsd.JsonSchemaAsXml: TObject;
var
  x, n: Integer;
  xXml: TXml;
  xJsonType: TjsonType;
begin
  {TODO Make recurisive}
  if _Processed then
  begin
    result := TXml.CreateAsString(ElementName, 'RECURSIVE');
    Exit;
  end;
  _Processed := True;
  try
    result := TXml.CreateAsString(ElementName, '');
    xXml := result as TXml;
    if maxOccurs <> '1' then
    begin
      xXml.AddXml(TXml.CreateAsString('type', 'array'));
      xXml := xXml.AddXml(TXml.CreateAsString('items', ''));
    end;
    with xXml do
    begin
      if sType.ElementDefs.Count = 0 then
      begin
        xJsonType := jsonString;
        if sType.BaseDataTypeName = 'boolan' then xJsonType := jsonBoolean;
        if sType.BaseDataTypeName = 'decimal' then xJsonType := jsonNumber;
        if sType.BaseDataTypeName = 'integer' then xJsonType := jsonNumber;
        if sType.BaseDataTypeName = 'byte' then xJsonType := jsonNumber;
        with AddXml(TXml.CreateAsString('type', sType.BaseDataTypeName)) do
        begin
          if Value = 'time' then Value := 'dateTime';
          if Value = 'decimal' then Value := 'float';
          if Value = 'duration' then Value := 'string';
        end;
        if sType.MaxInclusive <> '' then
          AddXml (TXml.CreateAsString('maximum', sType.MaxInclusive)).jsonType := xJsonType;
        if sType.MaxExclusive <> '' then
        begin
          AddXml (TXml.CreateAsString('maximum', sType.MaxExclusive)).jsonType := xJsonType;
          AddXml (TXml.CreateAsBoolean('exclusiveMaximum', True));
        end;
        if sType.MinInclusive <> '' then
          AddXml (TXml.CreateAsString('minimum', sType.MinInclusive)).jsonType := xJsonType;
        if sType.MinExclusive <> '' then
        begin
          AddXml (TXml.CreateAsString('minimum', sType.MinExclusive)).jsonType := xJsonType;
          AddXml (TXml.CreateAsBoolean('exclusiveMinimum', True));
        end;
        if sType.MaxLength <> '' then
          AddXml (TXml.CreateAsString('maxLength', sType.MaxLength)).jsonType := jsonNumber;
        if sType.MinLength <> '' then
          AddXml (TXml.CreateAsString('minLength', sType.MinLength)).jsonType := jsonNumber;
        if sType.Pattern <> '' then
          AddXml (TXml.CreateAsString('pattern ', sType.Pattern));
        if sType.maxItems <> '' then
          AddXml (TXml.CreateAsString('maxItems', sType.MaxItems)).jsonType := jsonNumber;
        if sType.MinItems <> '' then
          AddXml (TXml.CreateAsString('minItems', sType.MinItems)).jsonType := jsonNumber;
        if sType.maxProperties <> '' then
          AddXml (TXml.CreateAsString('maxProperties', sType.maxProperties)).jsonType := jsonNumber;
        if sType.MinProperties <> '' then
          AddXml (TXml.CreateAsString('minProperties', sType.MinProperties)).jsonType := jsonNumber;
        if sType.Enumerations.Count > 0 then
        begin
          with AddXml(TXml.CreateAsString('enum', '')) do
          begin
            jsonType := jsonArray;
            for x := 0 to sType.Enumerations.Count - 1 do
            begin
              AddXml (TXml.CreateAsString('-', sType.Enumerations.Strings[x])).jsonType := xJsonType;
            end;
          end;
        end;
      end
      else
      begin
        AddXml(TXml.CreateAsString('type', 'object'));
        with AddXml(TXml.CreateAsString('properties', '')) do
        begin
          for x := 0 to sType.ElementDefs.Count - 1 do
            AddXml (sType.ElementDefs.Xsds[x].JsonSchemaAsXml as TXml);
        end;
        n := 0;
        for x := 0 to sType.ElementDefs.Count - 1 do
          if sType.ElementDefs.Xsds[x].minOccurs <> '0' then
            Inc (n);
        if (n > 0) then with AddXml(TXml.CreateAsString('required', '')) do
        begin
          for x := 0 to sType.ElementDefs.Count - 1 do
            if sType.ElementDefs.Xsds[x].minOccurs <> '0' then
              AddXml (TXml.CreateAsString ('_', sType.ElementDefs.Xsds[x].ElementName));
        end;
      end;
    end;
  finally
    _Processed := False;
  end;
end;

procedure TXsd.ClearNameSpace;
var
  x: Integer;
begin
  if _Processed then Exit;
  ElementNameSpace := '';
  _Processed := True;
  if Assigned (sType) then
    for x := 0 to sType.ElementDefs.Count - 1 do
      sType.ElementDefs.Xsds[x].ClearNameSpace;
  _Processed := False;
end;

function TXsd.getNSPrefix: String;
begin
  result := xsdDescr.NameSpacePrefix(ElementNameSpace);
end;

function TXsd.getXsdByCaption(Index: String): TXsd;
var
  x: Integer;
  y: Integer;
  xName: String;
  newName: String;
begin
  result := nil;
  x := Pos ('.', Index);
  if x = 0 then
    xName := Index
  else
    xName := Copy (Index, 1, x - 1);
  if (xName = ElementName)
  or (xName = '*')
  then
  begin
    if x = 0 then
      result := self
    else
    begin
      newName := Copy(Index, x + 1, Length (Index));
      for y := 0 to sType.ElementDefs.Count - 1 do
      begin
        result := sType.ElementDefs.Xsds [y].getXsdByCaption(newName);
        if result <> nil then
          exit;
      end;
    end;
  end;
end;

function TXsd.AddElementDef(aXsdDescr: TXsdDescr; aName: String; aType: TXsdDataType): TXsd;
var
  oType, nType: TXsdDataType;
begin
  oType := sType;
  if not oType.Manually then
  begin
    nType := TXsdDataType.Create(aXsdDescr, oType);
    aXsdDescr.Garbage.AddObject('', nType);
  end
  else
    nType := oType;
  nType.Manually := True;
  result := TXsd.Create(aXsdDescr);
  aXsdDescr.Garbage.AddObject('', result);
  result.FormQualified := FormQualified;
  result.Manually := True;
  result.sType := aType;
  result.ElementName := aName;
  result.ElementNameSpace := aType.NameSpace;
  result.minOccurs := '0';
  nType.ElementDefs.AddObject(result.ElementName, result);
  nType.Manually := True;
  if nType <> oType then
    aXsdDescr.TypeDefs.AddObject(nType.NameSpace + ';' + nType.Name, nType);
  sType := nType;
end;

{ TXsdDataTypeList }

function TXsdDataTypeList.GetXsdDataType(Index: integer): TXsdDataType;
begin
  result := TXsdDataType(Objects[index]);
end;

{ TXsdAttrList }

procedure TXsdAttrList.Clear;
begin
  inherited;
end;

function TXsdAttrList.GetXsdAttr(Index: integer): TXsdAttr;
begin
  result := TXsdAttr(Objects[index]);
end;

{ TXsdDataType }

constructor TXsdDataType.Create(aXsdDescr: TXsdDescr);
begin
  _Error := False;
  jsonType := jsonNone;
  xsdDescr := aXsdDescr;
  Enumerations := TStringList.Create;
  Enumerations.Sorted := True;
  Enumerations.Duplicates := dupIgnore;
  Enumerations.CaseSensitive := True;
  Documentation := TStringList.Create;
  Appinfo := TStringList.Create;
  ExtendedByList := TXsdDataTypeList.Create;
  ElementDefs := TXsdList.Create;
  AttributeDefs := TXsdAttrList.Create;
end;

constructor TXsdDataType.Create(aXsdDescr: TXsdDescr; aSource: TXsdDataType);
var
  x: integer;
begin
  if not Assigned (aSource) then
    raise Exception.Create('constructor TXsdDataType.Create(aXsdDescr: TXsdDescr; nul as: TXsdDataType)');
  self._Error := False;
  self.xsdDescr := aXsdDescr;
  self.Name := aSource.Name;
  self.IsComplex := aSource.IsComplex;
  self.IsBuiltIn := False;
  self.xsdDescr := aSource.xsdDescr;
  self.NameSpace := aSource.NameSpace;
  self.Name := aSource.Name;
  self.BaseDataTypeName := aSource.BaseDataTypeName;
  self.BaseNameSpace := aSource.BaseNameSpace;
  self.ContentModel := aSource.ContentModel;
  self.DerivationMethod := aSource.DerivationMethod;
  self.Length := aSource.Length;
  self.MinLength := aSource.MinLength;
  self.MaxLength := aSource.MaxLength;
  self.Pattern := aSource.Pattern;
  self.Whitespace := aSource.Whitespace;
  self.MaxInclusive := aSource.MaxInclusive;
  self.MaxExclusive := aSource.MaxExclusive;
  self.MinInclusive := aSource.MinInclusive;
  self.MinExclusive := aSource.MinExclusive;
  self.Numeric := aSource.Numeric;
  self.TotalDigits := aSource.TotalDigits;
  self.FractionalDigits := aSource.FractionalDigits;
  self.Enumerations := TStringList.Create;
  self.Enumerations.Sorted := True;
  self.Enumerations.Duplicates := dupIgnore;
  self.Enumerations.CaseSensitive := True;
  self.Enumerations.Text := aSource.Enumerations.Text;
  for x := 0 to aSource.Enumerations.Count - 1 do
    self.Enumerations.Objects[x] :=
      (aSource.Enumerations.Objects[x] as TXsdEnumeration).Clone;
  self.Documentation := TStringList.Create;
  self.Documentation.Text := aSource.Documentation.Text;
  self.Appinfo := TStringList.Create;
  self.Appinfo.Text := aSource.Appinfo.Text;
  self.ExtendedByList := TXsdDataTypeList.Create;
  self.ElementDefs := TXsdList.Create;
  for x := 0 to aSource.ExtendedByList.Count - 1 do
    self.ExtendedByList.AddObject(aSource.ExtendedByList.Strings[x],
      aSource.ExtendedByList.XsdDataTypes[x]);
  for x := 0 to aSource.ElementDefs.Count - 1 do
    xsdDescr.Garbage.AddObject('', AddXsd(TXsd.Create(xsdDescr,
          aSource.ElementDefs.Xsds[x])));
  self.AttributeDefs := TXsdAttrList.Create;
  self.AttributeDefs.Text := aSource.AttributeDefs.Text;
  for x := 0 to aSource.AttributeDefs.Count - 1 do
    self.AttributeDefs.Objects[x] := TXsdAttr.Create(aXsdDescr,
      aSource.AttributeDefs.XsdAttrs[x]);
  self.Manually := aSource.Manually;
  self.jsonType := aSource.jsonType;
end;

destructor TXsdDataType.Destroy;
var
  x: integer;
begin
  try
    for x := 0 to Enumerations.Count - 1 do
      (Enumerations.Objects[x] as TXsdEnumeration).Free;
    Enumerations.Clear;
    Enumerations.Free;
    Documentation.Clear;
    Documentation.Free;
    Appinfo.Clear;
    Appinfo.Free;
    ExtendedByList.Clear;
    ExtendedByList.Free;
    ElementDefs.Clear;
    ElementDefs.Free;
    AttributeDefs.Clear;
    AttributeDefs.Free;
  except
    raise ;
  end;
  inherited;
end;

function TXsdDataType.getElementByName(Index: String): TXsd;
var
  x: integer;
  xName: String;
begin
  result := nil;
  xName := NameWithoutPrefix(Index);
  for x := 0 to ElementDefs.Count - 1 do
  begin
    if ElementDefs.Xsds[x].ElementName = xName then
    begin
      result := ElementDefs.Xsds[x];
      exit;
    end;
  end;
end;

function TXsdDataType.getNSAttributes: String;
begin
  result := xsdDescr.GenerateNameSpaceAttributes;
end;

function TXsdDataType.getNSPrefix: String;
begin
  result := xsdDescr.NameSpacePrefix(NameSpace);
end;

function TXsdDataType.SchemaAsXml(aName: String): TObject;
var
  xXml: TXml;
  idList: TStringList;
  function _typeAsXml(aType: TXsdDataType): TXml;
  var
    x, E: integer;
  begin
    result := TXml.Create;
    with result do
    begin
      if aType.ContentModel = '' then // simpletype??
      begin
        TagName := 'xs:simpleType';
        with AddXml(TXml.CreateAsString('xs:restriction', '')) do
        begin
          AddAttribute(TXmlAttribute.CreateAsString('base',
              'xs:' + aType.BaseDataTypeName));
          if aType.Length <> '' then
            with AddXml(TXml.CreateAsString('xs:length', '')) do
              AddAttribute(TXmlAttribute.CreateAsString('value', aType.Length));
          if aType.MinLength <> '' then
            with AddXml(TXml.CreateAsString('xs:minLength', '')) do
              AddAttribute(TXmlAttribute.CreateAsString('value',
                  aType.MinLength));
          if aType.MinInclusive <> '' then
            with AddXml(TXml.CreateAsString('xs:minInclusive', '')) do
              AddAttribute(TXmlAttribute.CreateAsString('value',
                  aType.MinInclusive));
          if aType.MinExclusive <> '' then
            with AddXml(TXml.CreateAsString('xs:minExclusive', '')) do
              AddAttribute(TXmlAttribute.CreateAsString('value',
                  aType.MinExclusive));
          if aType.MaxLength <> '' then
            with AddXml(TXml.CreateAsString('xs:maxLength', '')) do
              AddAttribute(TXmlAttribute.CreateAsString('value',
                  aType.MaxLength));
          if aType.MaxInclusive <> '' then
            with AddXml(TXml.CreateAsString('xs:maxInclusive', '')) do
              AddAttribute(TXmlAttribute.CreateAsString('value',
                  aType.MaxInclusive));
          if aType.MaxExclusive <> '' then
            with AddXml(TXml.CreateAsString('xs:maxExclusive', '')) do
              AddAttribute(TXmlAttribute.CreateAsString('value',
                  aType.MaxExclusive));
          if aType.TotalDigits <> '' then
            with AddXml(TXml.CreateAsString('xs:totalDigits', '')) do
              AddAttribute(TXmlAttribute.CreateAsString('value',
                  aType.TotalDigits));
          if aType.FractionalDigits <> '' then
            with AddXml(TXml.CreateAsString('xs:fractionDigits', '')) do
              AddAttribute(TXmlAttribute.CreateAsString('value',
                  aType.FractionalDigits));
          if aType.Whitespace <> '' then
            with AddXml(TXml.CreateAsString('xs:whiteSpace', '')) do
              AddAttribute(TXmlAttribute.CreateAsString('value',
                  aType.Whitespace));
          if aType.Pattern <> '' then
            with AddXml(TXml.CreateAsString('xs:pattern', '')) do
              AddAttribute(TXmlAttribute.CreateAsString('value',
                  StringReplace(aType.Pattern, #$D#$A, '(@@)', [rfReplaceAll])));
          for E := 0 to aType.Enumerations.Count - 1 do
            with AddXml(TXml.CreateAsString('xs:enumeration', '')) do
              AddAttribute(TXmlAttribute.CreateAsString('value',
                  aType.Enumerations.Strings[E]));
        end; // restriction
      end // if simpletype
      else
      begin
        TagName := 'xs:complexType';
        with AddXml(TXml.CreateAsString('xs:' + LowerCase(aType.ContentModel),
            '')) do
        begin
          for x := 0 to aType.ElementDefs.Count - 1 do
          begin
            with AddXml(TXml.CreateAsString('xs:element', '')) do
            begin
              if aType.ElementDefs.Xsds[x]._Processed then
                AddAttribute(TXmlAttribute.CreateAsString('ref',
                    aType.ElementDefs.Xsds[x].ElementName))
              else
                AddAttribute(TXmlAttribute.CreateAsString('name',
                    aType.ElementDefs.Xsds[x].ElementName));
              AddAttribute(TXmlAttribute.CreateAsString('minOccurs',
                  aType.ElementDefs.Xsds[x].minOccurs));
              AddAttribute(TXmlAttribute.CreateAsString('maxOccurs',
                  aType.ElementDefs.Xsds[x].maxOccurs));
              if not aType.ElementDefs.Xsds[x]._Processed then
              begin
                aType.ElementDefs.Xsds[x]._Processed := True;
                try
                  AddXml(_typeAsXml(aType.ElementDefs.Xsds[x].sType));
                finally
                  aType.ElementDefs.Xsds[x]._Processed := False;
                end;
              end;
            end;
          end;
        end;
        for x := 0 to aType.AttributeDefs.Count - 1 do
          with AddXml(TXml.CreateAsString('xs:attribute', '')) do
          begin
            AddAttribute(TXmlAttribute.CreateAsString('name',
                aType.AttributeDefs.XsdAttrs[x].Name));
            AddAttribute(TXmlAttribute.CreateAsString('type',
                'xs:' + aType.AttributeDefs.XsdAttrs[x].BaseDataTypeName));
          end;
        // raise Exception.Create(aType.ContentModel + ' (contentmodel): not yet supported')
      end;
    end; // with result
  end;
  procedure _generateComplexType(aXml: TXml; aType: TXsdDataType;
    aName: String);
  var
    x, f: integer;
  begin
    if idList.Find('Type' + aType.uniqueId + '_' + aName, f) then
      exit;
    idList.Add('Type' + aType.uniqueId + '_' + aName);
    for x := 0 to aType.ElementDefs.Count - 1 do
    begin
      if aType.ElementDefs.Xsds[x].sType.ContentModel <> '' then
      begin
        if not aType.ElementDefs.Xsds[x]._Processed then
        begin
          aType.ElementDefs.Xsds[x]._Processed := True;
          _generateComplexType(aXml, aType.ElementDefs.Xsds[x].sType,
            aType.ElementDefs.Xsds[x].ElementName);
          aType.ElementDefs.Xsds[x]._Processed := False;
        end;
      end;
    end;
    with aXml.AddXml(TXml.CreateAsString('xs:complexType', '')) do
    begin
      AddAttribute(TXmlAttribute.CreateAsString('name',
          'Type' + aType.uniqueId + '_' + aName));
      with AddXml(TXml.CreateAsString('xs:' + LowerCase(aType.ContentModel),
          '')) do
      begin
        for x := 0 to aType.ElementDefs.Count - 1 do
        begin
          with AddXml(TXml.CreateAsString('xs:element', '')) do
          begin
            AddAttribute(TXmlAttribute.CreateAsString('name',
                aType.ElementDefs.Xsds[x].ElementName));
            if aType.ElementDefs.Xsds[x].sType.ContentModel = '' then
              AddXml(_typeAsXml(aType.ElementDefs.Xsds[x].sType))
            else
              AddAttribute(TXmlAttribute.CreateAsString('type',
                  'Type' + aType.ElementDefs.Xsds[x].sType.uniqueId + '_' +
                    aType.ElementDefs.Xsds[x].ElementName));
            AddAttribute(TXmlAttribute.CreateAsString('minOccurs',
                aType.ElementDefs.Xsds[x].minOccurs));
            AddAttribute(TXmlAttribute.CreateAsString('maxOccurs',
                aType.ElementDefs.Xsds[x].maxOccurs));
          end;
        end;
      end;
      for x := 0 to aType.AttributeDefs.Count - 1 do
        with AddXml(TXml.CreateAsString('xs:attribute', '')) do
        begin
          AddAttribute(TXmlAttribute.CreateAsString('name',
              aType.AttributeDefs.XsdAttrs[x].Name));
          AddAttribute(TXmlAttribute.CreateAsString('type',
              'xs:' + aType.AttributeDefs.XsdAttrs[x].BaseDataTypeName));
        end;
    end;
  end;

begin
  idList := TStringList.Create;
  try
    idList.Sorted := True;
    xXml := TXml.CreateAsString('xs:schema', '');
    with xXml do
    begin
      AddAttribute(TXmlAttribute.CreateAsString('xmlns:xs', scXMLSchemaURI));
      {
        if FormDefaultQualified then
        AddAttribute (TXmlAttribute.CreateAsString ('elementFormDefault', 'qualified'))
        else
        }
      AddAttribute(TXmlAttribute.CreateAsString('elementFormDefault',
          'qualified'));
      {
        if False then
        AddAttribute (TXmlAttribute.CreateAsString ('attributeFormDefault', 'qualified'))
        else
        AddAttribute (TXmlAttribute.CreateAsString ('attributeFormDefault', 'unqualified'));
        }
      if ContentModel <> '' then
      begin
        _generateComplexType(xXml, self, aName);
      end;
      with AddXml(TXml.CreateAsString('xs:element', '')) do
      begin
        AddAttribute(TXmlAttribute.CreateAsString('name', aName));
        if ContentModel = '' then
          AddXml(_typeAsXml(self))
        else
          AddAttribute(TXmlAttribute.CreateAsString('type',
              'Type' + uniqueId + '_' + aName));
      end;
    end;
    result := xXml as TObject;
  finally
    FreeAndNil(idList);
  end;
end;

function TXsdDataType.SchemaAsText(aName: String): String;
begin
  with self.SchemaAsXml(aName) as TXml do
  try
    result := Text;
  finally
    Free;
  end;
end;

function TXsdDataType.getXsiNameSpaceAttribute: String;
begin
  result := xsdGenerateXsiNameSpaceAttribute;
end;

function TXsdDataType.IsValidXml(aXml: TObject; var aMessage: String): Boolean;
var
  x, y, n: Integer;
  xXml: TXml;
  xXsd: TXsd;
begin
  result := True;
  xXml := aXml as TXml;
  xXml.ValidationMesssage := '';
  if not xXml.Checked then
    Exit;

  // check namespace
  if Assigned (xXml.Xsd)
  and (xXml.NameSpace <> '')
  and (xXml.NameSpace <> xXml.Xsd.ElementNameSpace)
  //and (NameSpace <> scXMLSchemaURI)
  then
  begin
    result := False;
    xXml.ValidationMesssage := Format( 'Found NameSpace %s at %s, expected %s'
                                     , [ xXml.NameSpace
                                       , xXml.Name
                                       , xXml.Xsd.ElementNameSpace
                                       ]
                                     );
    aMessage := aMessage + xXml.ValidationMesssage + LineEnding;
    Exit;
  end;


  // check value
  if ElementDefs.Count = 0 then
  begin
    if not IsValidValue(xXml.Name, xXml.Value, xXml.ValidationMesssage) then
    begin
      result := False;
      aMessage := aMessage + xXml.ValidationMesssage + LineEnding;
    end;
    Exit;
  end;

  // unexpected elements
  for x := 0 to xXml.Items.Count - 1 do with xXml.Items.XmlItems[x] do
  begin
    if Checked
    and not Assigned (TypeDef) then
    begin
      result := False;
      xXml.ValidationMesssage := 'Unexpected element ' + NameSpace + ':' + Name;
      aMessage := aMessage + xXml.ValidationMesssage + LineEnding;
      Exit;
    end;
  end;

  // more than maxOcurrence
  for x := 0 to ElementDefs.Count - 1 do
  begin
    if ElementDefs.Xsds[x].maxOccurs <> tagUnbounded then
    begin
      n := 0;
      for y := 0 to xXml.Items.Count - 1 do
        if xXml.Items.XmlItems[y].Checked
        and (xXml.Items.XmlItems[y].Xsd = ElementDefs.Xsds[x]) then
          Inc (n);
      if n > StrToInt(ElementDefs.Xsds[x].maxOccurs) then
      begin
        result := False;
        xXml.ValidationMesssage := Format( 'Number of elements (%d) exceeds maximum (%s) for element %s'
                                         , [n, ElementDefs.Xsds[x].maxOccurs, ElementDefs.Xsds[x].ElementName]
                                         );
        aMessage := aMessage + xXml.ValidationMesssage + LineEnding;
        Exit;
      end;
    end;
  end;

  // less than minOcurrence
  for x := 0 to ElementDefs.Count - 1 do
  begin
    if ElementDefs.Xsds[x].minOccurs <> '0' then
    begin
      n := 0;
      for y := 0 to xXml.Items.Count - 1 do
        if xXml.Items.XmlItems[y].Checked
        and (xXml.Items.XmlItems[y].Xsd = ElementDefs.Xsds[x]) then
          Inc (n);
      if n < StrToInt(ElementDefs.Xsds[x].minOccurs) then
      begin
        if (n > 0)
        or (ContentModel <> tagChoice) then
        begin
          result := False;
          xXml.ValidationMesssage := Format( 'Number of elements (%d) less then minimum (%s) for element %s'
                                           , [n, ElementDefs.Xsds[x].minOccurs, ElementDefs.Xsds[x].ElementName]
                                           );
          aMessage := aMessage + xXml.ValidationMesssage + LineEnding;
          Exit;
        end;
      end;
    end;
  end;

  // check Choice
  if ContentModel = tagChoice then
  begin
    xXsd := nil;
    for x := 0 to xXml.Items.Count - 1 do with xXml.Items.XmlItems[x] do
    begin
      if Checked then
      begin
        if Assigned(xXsd) then
        begin
          if Xsd <> xXsd then
          begin
            result := False;
            xXml.ValidationMesssage := Format( 'Element %s not allowed after %s in a choice'
                                             , [Name, xXsd.ElementName]
                                             );
            aMessage := aMessage + xXml.ValidationMesssage + LineEnding;
            Exit;
          end
        else
          xXsd := Xsd;
        end;
      end;
    end;
  end;

  { TODO : check order of elements }
  if ContentModel = tagSequence then
  begin
  end;

  for x := 0 to xXml.Items.Count - 1 do with xXml.Items do
    if (XmlItems[x].Checked)
    and Assigned (XmlItems[x].TypeDef) then
      result := XmlItems[x].TypeDef.IsValidXml(XmlItems[x], aMessage);
end;

function TXsdDataType.IsValidValue(aName, aValue: String;
  var aMessage: String): Boolean;

  function _fractionDigits (aValue: String): Integer;
  var
    i: Integer;
    s: String;
  begin
    s := aValue;
    if Pos ('E', s) > 0 then
    begin
      s := FloatToStr(xsdParseDecimal(s));
      if Pos ('E', s) > 0 then
        raise Exception.Create ('failed to check totalDigits, the value may be OK');
    end;
    result := 0;
    i := system.Length (s);
    while i > 0 do
    begin
      if s [i] = DefaultFormatSettings.DecimalSeparator then
        Exit;
      if (s [i] > '0')
      and (s <= '9') then
        Inc (result);
      Dec (i);
    end;
    if i = 0 then
      result := 0;
  end;
  function _totalDigits (aValue: String): Integer;
  var
    i: Integer;
    s: String;
  begin
    s := aValue;
    if Pos ('E', s) > 0 then
    begin
      s := FloatToStr(xsdParseDecimal(s));
      if Pos ('E', s) > 0 then
        raise Exception.Create ('failed to check totalDigits, the value may be OK');
    end;
    result := 0; // start with fractionDigits
    i := 1;
    while i <= system.Length (s) do
    begin
      if s [i] = DefaultFormatSettings.DecimalSeparator then
      begin
        result := result + _fractionDigits(aValue);
        Exit;
      end;
      if (s [i] > '0')
      and (s <= '9') then
        Inc (result);
      Inc (i);
    end;
  end;

var
  xDateTime: TDateTime;
  xDecimal: Extended;
  xInt64: Int64;
  f: Integer;
  procedure _checkDecimal;
  var
    Sep: Char;
  begin
   Sep := DefaultFormatSettings.DecimalSeparator;
   DefaultFormatSettings.DecimalSeparator :='.';
   try
     if (MinInclusive <> '')
     and (xdecimal < xsdParsedecimal(MinInclusive)) then
       raise Exception.CreateFmt('Value violates MinIncl constraint (%s)', [MinInclusive]);
     if (MaxInclusive <> '')
     and (xdecimal > xsdParseDecimal(MaxInclusive)) then
       raise Exception.CreateFmt('Value violates MaxIncl constraint (%s)', [MaxInclusive]);
     if (MinExclusive <> '')
     and (xdecimal <= xsdParseDecimal(MinExclusive)) then
       raise Exception.CreateFmt('Value violates MinExcl constraint (%s)', [MinExclusive]);
     if (MaxExclusive <> '')
     and (xdecimal >= xsdParseDecimal(MaxExclusive)) then
       raise Exception.CreateFmt('Value violates MaxExcl constraint (%s)', [MaxExclusive]);
     if (TotalDigits <> '')
     and (_totalDigits (aValue) > StrToInt(TotalDigits)) then
       raise Exception.CreateFmt('Value violates TotalDigits constraint (%s)', [TotalDigits]);
     if (FractionalDigits <> '')
     and (_fractionDigits (aValue) > StrToInt(FractionalDigits)) then
       raise Exception.CreateFmt('Value violates FractionDigits constraint (%s)', [FractionalDigits]);
   finally
     DefaultFormatSettings.DecimalSeparator :=sep;
   end;
  end;
  procedure _checkInteger;
  begin
    if (MinInclusive <> '')
    and (xInt64 < xsdParseInteger(MinInclusive)) then
      raise Exception.CreateFmt('Value violates MinIncl constraint (%s)', [MinInclusive]);
    if (MaxInclusive <> '')
    and (xInt64 > xsdParseInteger(MaxInclusive)) then
      raise Exception.CreateFmt('Value violates MaxIncl constraint (%s)', [MaxInclusive]);
    if (MinExclusive <> '')
    and (xInt64 <= xsdParseInteger(MinExclusive)) then
      raise Exception.CreateFmt('Value violates MinExcl constraint (%s)', [MinExclusive]);
    if (MaxExclusive <> '')
    and (xInt64 >= xsdParseInteger(MaxExclusive)) then
      raise Exception.CreateFmt('Value violates MaxExcl constraint (%s)', [MaxExclusive]);
    if (TotalDigits <> '')
    and (_totalDigits (aValue) > StrToInt(TotalDigits)) then
      raise Exception.CreateFmt('Value violates TotalDigits constraint (%s)', [TotalDigits]);
      { TODO : check for other facets on Integer
  }
  end;

begin
  result := True;
  if Name = 'FileNameType' then
  begin
    if not FileExists(aValue) then
    begin
      aMessage := 'Value: "' + aValue + '" File does not exist';
      result := False;
    end;
    exit;
  end;
  if Name = 'FolderNameType' then
  begin
    if not DirectoryExists(aValue) then
    begin
      aMessage := 'Value: "' + aValue + '" Folder does not exist';
      result := False;
    end;
    exit;
  end;
  try
    // general facets
    if (Length <> '')
    and (System.Length (aValue) <> StrToInt(Length)) then
      raise Exception.CreateFmt('Value violates length constraint (%s)', [Length]);
    if (MinLength <> '')
    and (System.Length (aValue) < StrToInt(MinLength)) then
      raise Exception.CreateFmt('Value violates minLength constraint (%s)', [MinLength]);
    if (MaxLength <> '')
    and (System.Length (aValue) > StrToInt(MaxLength)) then
      raise Exception.CreateFmt('Value violates maxLength constraint (%s)', [MaxLength]);
    // general enumeration
    if Assigned (Enumerations)
    and (Enumerations.Count > 0) then
      if not Enumerations.Find (aValue, f) then
        if Enumerations.Count < 13 then
          raise Exception.CreateFmt('Value violates enumeration constraint (%s)', [LineEnding + Enumerations.Text])
        else
          raise Exception.Create('Value violates enumeration constraint');
    // general pattern
    if (Pattern <> '') then
    begin
      with TRegExpr.Create do
      try
        Expression := '^(' + Pattern + ')$';
        if not Exec(aValue) then
          raise Exception.CreateFmt('Value violates pattern constraint (%s)', [Pattern]);
      finally
        Free;
      end;
    end;
    { TODO : whitespace }
    if BaseDataTypeName = 'boolean' then
    begin
      xsdParseBoolean(aValue);
    end;
    if BaseDataTypeName = 'date' then
    begin
      xDateTime := xsdParseDate(aValue);
      if (MinInclusive <> '')
      and (xDateTime < xsdParseDate(MinInclusive)) then
        raise Exception.CreateFmt('Value violates MinIncl constraint (%s)', [MinInclusive]);
      if (MaxInclusive <> '')
      and (xDateTime > xsdParseDate(MaxInclusive)) then
        raise Exception.CreateFmt('Value violates MaxIncl constraint (%s)', [MaxInclusive]);
      if (MinExclusive <> '')
      and (xDateTime <= xsdParseDate(MinExclusive)) then
        raise Exception.CreateFmt('Value violates MinExcl constraint (%s)', [MinExclusive]);
      if (MaxExclusive <> '')
      and (xDateTime >= xsdParseDate(MaxExclusive)) then
        raise Exception.CreateFmt('Value violates MaxExcl constraint (%s)', [MaxExclusive]);
    end;
    if BaseDataTypeName = 'dateTime' then
    begin
      xDateTime := xsdParseDateTime(aValue);
      if (MinInclusive <> '')
      and (xDateTime < xsdParseDateTime(MinInclusive)) then
        raise Exception.CreateFmt('Value violates MinIncl constraint (%s)', [MinInclusive]);
      if (MaxInclusive <> '')
      and (xDateTime > xsdParseDateTime(MaxInclusive)) then
        raise Exception.CreateFmt('Value violates MaxIncl constraint (%s)', [MaxInclusive]);
      if (MinExclusive <> '')
      and (xDateTime <= xsdParseDateTime(MinExclusive)) then
        raise Exception.CreateFmt('Value violates MinExcl constraint (%s)', [MinExclusive]);
      if (MaxExclusive <> '')
      and (xDateTime >= xsdParseDateTime(MaxExclusive)) then
        raise Exception.CreateFmt('Value violates MaxExcl constraint (%s)', [MaxExclusive]);
    end;
    if BaseDataTypeName = 'time' then
    begin
      xDateTime := xsdParseTime(aValue);
      if (MinInclusive <> '')
      and (xDateTime < xsdParseTime(MinInclusive)) then
        raise Exception.CreateFmt('Value violates MinIncl constraint (%s)', [MinInclusive]);
      if (MaxInclusive <> '')
      and (xDateTime > xsdParseTime(MaxInclusive)) then
        raise Exception.CreateFmt('Value violates MaxIncl constraint (%s)', [MaxInclusive]);
      if (MinExclusive <> '')
      and (xDateTime <= xsdParseTime(MinExclusive)) then
        raise Exception.CreateFmt('Value violates MinExcl constraint (%s)', [MinExclusive]);
      if (MaxExclusive <> '')
      and (xDateTime >= xsdParseTime(MaxExclusive)) then
        raise Exception.CreateFmt('Value violates MaxExcl constraint (%s)', [MaxExclusive]);
    end;
    if BaseDataTypeName = 'decimal' then
    begin
      xdecimal := xsdParseDecimal(aValue);
      _checkDecimal;
    end;
    if BaseDataTypeName = 'double' then
    begin
      xdecimal := xsdParseDouble(aValue);
      _checkDecimal;
    end;
    if BaseDataTypeName = 'float' then
    begin
      xdecimal := xsdParseFloat(aValue);
      _checkDecimal;
    end;
    if BaseDataTypeName = 'byte' then
    begin
      xInt64 := xsdParseByte(aValue);
      _checkInteger;
    end;
    if BaseDataTypeName = 'integer' then
    begin
      xInt64 := xsdParseInteger(aValue);
      _checkInteger;
    end;
    if BaseDataTypeName = 'nonNegativeInteger' then
    begin
      xInt64 := xsdParseNonNegativeInteger(aValue);
      _checkInteger;
    end;
    if BaseDataTypeName = 'nonPositiveInteger' then
    begin
      xInt64 := xsdParseNonPositiveInteger(aValue);
      _checkInteger;
    end;
    if BaseDataTypeName = 'negativeInteger' then
    begin
      xInt64 := xsdParseNegativeInteger(aValue);
      _checkInteger;
    end;
    if BaseDataTypeName = 'positiveInteger' then
    begin
      xInt64 := xsdParsePositiveInteger(aValue);
      _checkInteger;
    end;
    if BaseDataTypeName = 'short' then
    begin
      xInt64 := xsdParseShort(aValue);
      _checkInteger;
    end;
    if BaseDataTypeName = 'int' then
    begin
      xInt64 := xsdParseInt(aValue);
      _checkInteger;
    end;
    if BaseDataTypeName = 'long' then
    begin
      xInt64 := xsdParseLong(aValue);
      _checkInteger;
    end;
    if BaseDataTypeName = 'unsignedByte' then
    begin
      xInt64 := xsdParseUnsignedByte(aValue);
      _checkInteger;
    end;
    if BaseDataTypeName = 'unsignedShort' then
    begin
      xInt64 := xsdParseUnsignedShort(aValue);
      _checkInteger;
    end;
    if BaseDataTypeName = 'unsignedInt' then
    begin
      xInt64 := xsdParseUnsignedInt(aValue);
      _checkInteger;
    end;
    if BaseDataTypeName = 'unsignedLong' then
    begin
      xInt64 := xsdParseUnsignedLong(aValue);
      _checkInteger;
    end;
    { TODO : those never seen datatypes like gMonth, gDay and others
duration also never seen but probably used... }
  except
    on E: Exception do
    begin
      aMessage := Format('Value: "%s" Validate XML Error, reason %s.%sThe Element "%s" failed to parse'
                        , [aValue, E.Message,LineEnding, aName]);
      result := False;
    end;
  end;
end;

function TXsdDataType.getUniqueId: String;
begin
  result := IntToStr(integer(self));
end;

constructor TXsdDataType.Create;
begin
  raise Exception.Create('TXsdDataType.Create constructor without arguments');
end;

{ TXsdAttr }

constructor TXsdAttr.Create(aXsdDescr: TXsdDescr);
begin
  inherited Create(aXsdDescr);
  FormDefaultQualified := xsdAttributeFormDefaultQualified;
end;

constructor TXsdAttr.Create(aXsdDescr: TXsdDescr; aSource: TXsdAttr);
begin
  inherited Create(aXsdDescr, aSource);
  self.Use := aSource.Use;
end;

destructor TXsdAttr.Destroy;
begin
  inherited;
end;

{ TXsdDescrList }

procedure TXsdDescrList.Clear;
var
  x: integer;
begin
  for x := 0 to Count - 1 do
    XsdDescrs[x].Free;
  inherited;
end;

function TXsdDescrList.GetXsdDescr(Index: integer): TXsdDescr;
begin
  result := TXsdDescr(Objects[index]);
end;

function TXsdDescrList.FindTypeDef(aNameSpace: String;
  aName: String): TXsdDataType;
var
  xd: integer;
  xt: integer;
begin
  result := nil;
  for xd := 0 to Count - 1 do
  begin
    for xt := 0 to XsdDescrs[xd].TypeDefs.Count - 1 do
    begin
      result := XsdDescrs[xd].FindTypeDef(aNameSpace, aName);
      if Assigned(result) then
        exit;
    end;
  end;
end;

function TXsdDescr.xsdFindTypeDef(aNameSpace, aName: String): TXsdDataType;
var
  x: integer;
begin
  result := nil;
  for x := 0 to TypeDefs.Count - 1 do
  begin
    if (TypeDefs.XsdDataTypes[x].NameSpace = aNameSpace) and
      (TypeDefs.XsdDataTypes[x].Name = aName) then
    begin
      result := TypeDefs.XsdDataTypes[x];
      exit;
    end;
  end;
end;

procedure TXsdDescr.AddNameSpace(aNameSpace: String);
var
  x: integer;
begin
  if not NameSpaceList.Find(aNameSpace, x) then
    NameSpaceList.Add(aNameSpace);
end;

function TXsdDescr.GenerateNameSpaceAttributes: String;
var
  x: integer;
begin
  result := '';
  if not Assigned(self) then
    if not Assigned(self) then
      exit;
  for x := 0 to NameSpaceList.Count - 1 do
  begin
    if (NameSpaceList.Strings[x] <> '') and
      (NameSpaceList.Objects[x] <> Pointer(0)) then
      result := result + ' ' + 'xmlns:' + NameSpacePrefix
        (NameSpaceList.Strings[x]) + '="' + NameSpaceList.Strings[x] + '"';
  end;
end;

function xsdGenerateXsiNameSpaceAttribute: String;
begin
  if not xsiGenerated then
  begin
    xsiGenerated := True;
    result := ' xmlns:xsi="' + scXMLSchemaInstanceURI + '"';
  end
  else
    result := '';
end;

function xsdGenerateXsdNameSpaceAttribute: String;
begin
  if not xsdGenerated then
  begin
    xsdGenerated := True;
    result := ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"';
  end
  else
    result := '';
end;

function TXsdDescr.NameSpacePrefix(aNameSpace: String): String;
var
  x: integer;
begin
  if aNameSpace = scXMLSchemaURI then
    result := 'xsd'
  else
  begin
    if NameSpaceList.Find(aNameSpace, x) then
      result := 'ns' + IntToStr(x + 1)
    else
      result := 'ns';
  end;
end;

function TXsdDescr.AddElement(aTypeDef: TXsdDataType; aXml: TObject; aTargetNameSpace: String; aRoot: Boolean): TXsd;
var
  xAtt: TXmlAttribute;
  xXml, cXml: TXml;
begin
  if not (aXml is TXml) then
    raise Exception.Create('Illegal: AddElement(aTypeDef: TXsdDataType; aXml: TObject): TXsd;');
  xXml := aXml as TXml;
  result := TXsd.Create(self);
  if xXml.Name = tagGroup then
    result._isGroup := True;
  result.SourceFileName := xXml.SourceFileName;
  Garbage.AddObject('', Result);
  result.FileName := fMainFileName;
  result.ElementName := xXml.Attributes.ValueByTag[tagName];
  result.ElementNameSpace := aTargetNamespace;
  AddNameSpace(result.ElementNameSpace);
  result.minOccurs := xXml.Attributes.ValueByTagDef[tagMinOccurs, '1'];
  result.maxOccurs := xXml.Attributes.ValueByTagDef[tagMaxOccurs, '1'];
  if aRoot then
    Result.FormQualified := True
  else
  begin
    xAtt := xXml.Attributes.AttributeByTag[tagForm];
    if Assigned (xAtt) then
      result.FormQualified := (xAtt.Value = tagQualified);
  end;
  xAtt := xXml.Attributes.AttributeByTag[tagType];
  if Assigned (xAtt) then
  begin
    result._DataTypeName := NameWithoutPrefix(xAtt.Value);
    result._NameSpace := xXml.PrefixToNameSpace (xAtt.Value);
    if result._NameSpace = '' then
      result._NameSpace := aTargetNamespace;
  end;
  xAtt := xXml.Attributes.AttributeByTag[tagRef];
  if Assigned (xAtt) then
  begin
    result._RefElementName := NameWithoutPrefix(xAtt.Value);
    result._RefNameSpace := xXml.PrefixToNameSpace (xAtt.Value);
    if result._RefNameSpace = '' then
      result._RefNameSpace := aTargetNamespace;
    result.ElementName := result._RefElementName;
    result.ElementNameSpace := result._RefNameSpace;
    if result._isGroup then
    begin
      result._NameSpace := result._RefNameSpace;
      result._DataTypeName := result._RefElementName;
    end;
  end;
  cXml := xXml.Items.XmlItemByTag[tagComplexType];
  if Assigned (cXml) then
    result.sType := AddComplexType(cXml, aTargetNameSpace, False);
  cXml := xXml.Items.XmlItemByTag[tagSimpleType];
  if Assigned (cXml) then
    result.sType := AddSimpleType(cXml, aTargetNameSpace, False);
  AddDocumentation(result.Documentation, xXml);
  AddAppinfo(result.Appinfo, xXml);
  aTypeDef.AddXsd(result);
end;

procedure TXsdDescr.AddGroup(aTypeDef: TXsdDataType; aXml: TObject;
  aTargetNameSpace: String);
var
  x: Integer;
  xXml: TXml;
  xAttr: TXmlAttribute;
  xGroupType: TXsdDataType;
  xXsd: TXsd;
begin
  xXml := aXml as TXml;
  xAttr := xXml.Attributes.AttributeByTag[tagRef];
  if not Assigned(xAttr) then
  begin
    SjowMessage(Format ('AddGroup %s %s %s: %s', [xXml.NameSpace, xXml.Name, aTargetNameSpace, 'no ref attr found']));
    Exit;
  end;
  xGroupType := FindTypeDef(xXml.PrefixToNameSpace(xAttr.Value), NameWithoutPrefix(xAttr.Value));
  if not Assigned (xGroupType) then
  begin
    SjowMessage(Format ('AddGroup %s %s %s: %s', [xXml.NameSpace, xXml.Name, aTargetNameSpace, 'Group not found']));
    Exit;
  end;
  SjowMessage(Format ('+++AddGroup %s %s %s', [xXml.NameSpace, xXml.Name, aTargetNameSpace]));
  for x := 0 to xGroupType.ElementDefs.Count - 1 do
  begin
    xXsd := TXsd.Create(self, xGroupType.ElementDefs.Xsds[x]);
    Garbage.AddObject('', xXsd);
    aTypeDef.ElementDefs.AddObject(xXsd.ElementName, xXsd);
  end;
end;

function TXsdDescr.CreateXsdFromXmlSample (aXml: TObject; aLinkXmlToXsd: Boolean): TXsd;
  procedure _guessBaseDataType (aType: TXsdDataType; aXml: TXml);
  begin
    if (aXml.Items.Count = 0)
    and (aXml.Value <> '') then with aType do
    begin
      BaseNameSpace := scXMLSchemaURI;
      BaseDataTypeName := 'string';
      try xsdParseInteger(aXml.Value); BaseDataTypeName := 'integer'; exit; except end;
      try xsdParseDecimal(aXml.Value); BaseDataTypeName := 'decimal'; exit; except end;
      try xsdParseDateTime(aXml.Value); BaseDataTypeName := 'dateTime'; exit; except end;
      try xsdParseDate(aXml.Value); BaseDataTypeName := 'date'; exit; except end;
      try xsdParseTime(aXml.Value); BaseDataTypeName := 'time'; exit; except end;
      try xsdParseBoolean(aXml.Value); BaseDataTypeName := 'boolean'; exit; except end;
    end;
  end;
  procedure _AddXsdAttribute (aXsd: TXsd; aAtt: TXmlAttribute);
  var
    x: Integer;
    xXsdAtt: TXsdAttr;
  begin
    if aAtt.isXmlNsAttribute then Exit;
    xXsdAtt := nil;
    for x := 0 to aXsd.sType.AttributeDefs.Count - 1 do
      if aXsd.sType.AttributeDefs.XsdAttrs [x].Name = aAtt.Name then
        xXsdAtt := aXsd.sType.AttributeDefs.XsdAttrs [x];
    if not Assigned (xXsdAtt) then
    begin
      xXsdAtt := TXsdAttr.Create (self);
      self.Garbage.AddObject ('', xXsdAtt);
      xXsdAtt.Name := aAtt.Name;
      aXsd.sType.AttributeDefs.AddObject('', xXsdAtt);
      xXsdAtt._Ficticious := True;
    end;
    if aLinkXmlToXsd then
      aAtt.XsdAttr := xXsdAtt;
  end;
  procedure _ChildXml (aXsd: TXsd; aXml: TXml);
  var
    x: Integer;
    xXsd: TXsd;
  begin
    xXsd := nil;
    for x := 0 to aXsd.sType.ElementDefs.Count - 1 do
      if (aXsd.sType.ElementDefs.Xsds[x].ElementName = aXml.Name) then
        xXsd := aXsd.sType.ElementDefs.Xsds [x];
    if not Assigned (xXsd) then
    begin
      xXsd := TXsd.Create (self);
      self.Garbage.AddObject ('', xXsd);
      xXsd.sType := TXsdDataType.Create(self);
      self.Garbage.AddObject ('', xXsd.sType);
      aXsd.sType.ElementDefs.AddObject('', xXsd);
      xXsd.minOccurs := '0';
      xXsd.maxOccurs := '1';
      xXsd.ElementName := aXml.Name;
      xXsd.ElementNameSpace := aXml.NameSpace;
      xXsd.FormQualified := (aXml.NsPrefix <> '')
                                or (aXml.AttributeValueByTag['xmlns'] = aXml.NameSpace);
      xXsd.sType.Name := aXml.Name + 'Type';
      xXsd.sType.NameSpace := aXml.NameSpace;
      xXsd.DoNotEncode := True;
      xXsd.sType._Ficticious := True;
      _guessBaseDataType(xXsd.sType, aXml);
    end;
    if xXsd._Processed then
      xXsd.maxOccurs := 'unbounded';
    xXsd._Processed := True;
    if aLinkXmlToXsd then
      aXml.Xsd := xXsd;
    for x := 0 to aXml.Attributes.Count - 1 do
      _AddXsdAttribute (xXsd, aXml.Attributes.XmlAttributes[x]);
    for x := 0 to xXsd.sType.ElementDefs.Count - 1 do
      xXsd.sType.ElementDefs.Xsds [x]._Processed := False;
    for x := 0 to aXml.Items.Count - 1 do
      _ChildXml (xXsd, aXml.Items.XmlItems[x]);
    for x := 0 to xXsd.sType.ElementDefs.Count - 1 do
      xXsd.sType.ElementDefs.Xsds [x]._Processed := False;
  end;
var
  xXml: TXml;
  x: Integer;
begin
  if not Assigned (aXml) then Exit;
  if not (aXml is TXml) then Exit;
  xXml := aXml as TXml;
  try
    result := TXsd.Create(self);
    self.Garbage.AddObject ('', result);
    result.ElementName := xXml.Name;
    result.ElementNameSpace := xXml.NameSpace;
    result.maxOccurs := '1';
    result.sType := TXsdDataType.Create(self);
    self.Garbage.AddObject('', result.sType);
    Result.sType.NameSpace := xXml.NameSpace;
    result.sType.Name := xXml.Name + 'Type';
    result.sType._Ficticious := True;
    _guessBaseDataType(result.sType, xXml);
    result.DoNotEncode := True;
    if aLinkXmlToXsd then
      xXml.Xsd := result;
    for x := 0 to xXml.Attributes.Count - 1 do
      _AddXsdAttribute (result, xXml.Attributes.XmlAttributes[x]);
    for x := 0 to xXml.Items.Count - 1 do
      _ChildXml (result, xXml.Items.XmlItems[x]);
  finally
  end;
end;

function TXsdDescr.CreateXsdFromJsonSample(aXml: TObject; aLinkXmlToXsd: Boolean
  ): TXsd;
  procedure _guessBaseDataTypeJson (aType: TXsdDataType; aXml: TXml);
  begin
    aType.BaseNameSpace := scXMLSchemaURI;
    if aXml.jsonType = jsonNone then
    begin
      if (aXml.Items.Count = 0) then with aXml do
      begin
        jsonType := jsonString;
        try xsdParseDecimal(Value); jsonType := jsonNumber; except end;
        try xsdParseBoolean(aXml.Value); jsonType := jsonBoolean; except end;
      end
      else
      begin
        aXml.jsonType := jsonObject;
      end;
    end;
    aType.jsonType := aXml.jsonType;
    with aType do
    begin
      case jsonType of
        jsonNone: ;
        jsonString: BaseDataTypeName := 'string';
        jsonNumber: BaseDataTypeName := 'decimal';
        jsonBoolean: BaseDataTypeName := 'boolean';
        jsonObject: ;
        jsonArray: ;
        jsonArrayValue: ;
      end;
    end;
  end;
  procedure _ChildXml (aXsd: TXsd; aXml: TXml);
  var
    x: Integer;
    xXsd: TXsd;
  begin
    xXsd := nil;
    for x := 0 to aXsd.sType.ElementDefs.Count - 1 do
      if (aXsd.sType.ElementDefs.Xsds[x].ElementName = aXml.Name) then
        xXsd := aXsd.sType.ElementDefs.Xsds [x];
    if not Assigned (xXsd) then
    begin
      xXsd := TXsd.Create (self);
      self.Garbage.AddObject ('', xXsd);
      xXsd.sType := TXsdDataType.Create(self);
      self.Garbage.AddObject ('', xXsd.sType);
      aXsd.sType.ElementDefs.AddObject('', xXsd);
      xXsd.minOccurs := '0';
      xXsd.maxOccurs := '1';
      xXsd.ElementName := aXml.Name;
      xXsd.ElementNameSpace := aXml.NameSpace;
      xXsd.FormQualified := (aXml.NsPrefix <> '')
                                or (aXml.AttributeValueByTag['xmlns'] = aXml.NameSpace);
      xXsd.sType.Name := aXml.Name + 'Type';
      xXsd.sType.NameSpace := aXml.NameSpace;
      xXsd.DoNotEncode := True;
      xXsd.sType._Ficticious := True;
      _guessBaseDataTypeJson(xXsd.sType, aXml);
    end;
    if xXsd._Processed then
      xXsd.maxOccurs := 'unbounded';
    xXsd._Processed := True;
    if aLinkXmlToXsd then
      aXml.Xsd := xXsd;
    for x := 0 to xXsd.sType.ElementDefs.Count - 1 do
      xXsd.sType.ElementDefs.Xsds [x]._Processed := False;
    for x := 0 to aXml.Items.Count - 1 do
      _ChildXml (xXsd, aXml.Items.XmlItems[x]);
    for x := 0 to xXsd.sType.ElementDefs.Count - 1 do
      xXsd.sType.ElementDefs.Xsds [x]._Processed := False;
  end;
var
  xXml: TXml;
  x: Integer;
begin
  if not Assigned (aXml) then Exit;
  if not (aXml is TXml) then Exit;
  result := TXsd.Create(self);
  self.Garbage.AddObject ('', result);
  xXml := aXml as TXml;
  result.ElementName := xXml.Name;
  result.ElementNameSpace := xXml.NameSpace;
  result.maxOccurs := '1';
  result.sType := TXsdDataType.Create(self);
  self.Garbage.AddObject('', result.sType);
  Result.sType.NameSpace := xXml.NameSpace;
  result.sType.Name := xXml.Name + 'Type';
  result.sType._Ficticious := True;
  _guessBaseDataTypeJson(result.sType, xXml);
  result.DoNotEncode := True;
  if aLinkXmlToXsd then
    xXml.Xsd := result;
  for x := 0 to xXml.Items.Count - 1 do
    _ChildXml (result, xXml.Items.XmlItems[x]);
  Result.sType.jsonType := jsonObject;
end;

procedure TXsdDescr.Clear;
var
  x: integer;
begin
  if Assigned (Garbage) then
  begin
    for x := 0 to Garbage.Count - 1 do
      Garbage.Objects[x].Free;
    Garbage.Clear;
  end;
  if Assigned (ReadFileNames) then with ReadFileNames do
  begin
    for x := 0 to Count - 1 do
      if Assigned(Objects[x]) then
        Objects[x].Free;
    Clear;
  end;
  if Assigned (xsdFileNames) then
    xsdFileNames.Clear;
  if Assigned (NameSpaceList) then
    NameSpaceList.Clear;
  if Assigned (TypeDef) then
    TypeDef.ElementDefs.Clear;
  if Assigned (TypeDefs) then
    TypeDefs.Clear;
end;

procedure TXsdDescr.AddedTypeDefElementsFromXml(aXml: TObject);
  function _findLast (f,s: String): Integer;
  var
    x: Integer;
  begin
    result := 0;
    for x := 1 to Length (s) do
      if s[x] = f then
        result := x;
  end;
  function __UsedAt(aType: TXsdDataType; s: String): TXsd;
  var
    x, p: integer;
    lft, rght: String;
  begin
    result := nil;
    if not Assigned(aType) then
      exit;
    p := Pos('<>', s);
    if p > 0 then
    begin
      lft := LeftStr(s, p - 1);
      rght := Copy(s, p + 2, Length(s));
    end
    else
    begin
      lft := s;
      rght := '';
    end;
    p := Pos (';', lft);
    if p < 1 then // pre 9.0 now working with semicolom because of http prolog in namespaces
    begin
      p := _findLast(':', lft);
      if p > 0 then
        lft [p] := ';';
    end;
    if (p = 1) then // pre 9.0 version - namespace for element not always specified
      lft := aType.NameSpace + Lft;
    for x := 0 to aType.ElementDefs.Count - 1 do
    begin
      if (aType.ElementDefs.Xsds[x].ElementNameSpace + ';' +
          aType.ElementDefs.Xsds[x].ElementName = lft) then
      begin
        if rght = '' then
          result := aType.ElementDefs.Xsds[x]
        else
          result := __UsedAt(aType.ElementDefs.Xsds[x].sType, rght);
        exit;
      end;
    end;
  end;
  function _UsedAt(s: String): TXsd;
  var
    p: integer;
    r, NameSpace, name: String;
  begin
    result := nil;
    p := Pos('<>', s);
    r := LeftStr(s, p - 1);
    s := Copy(s, p + 2, Length(s));
    p := Pos(';', r);
    if p < 1 then // pre 9.0 style ??
      p := _findLast (':', r);
    NameSpace := LeftStr(r, p - 1);
    name := Copy(r, p + 1, Length(r) - p);
    result := __UsedAt(FindTypeDef(NameSpace, name), s);
  end;

var
  sType: TXsdDataType;
  x, E: integer;
  xXsd: TXsd;
  xXml, dXml: TXml;
begin
  xXml := aXml as TXml;
  if xXml.Name <> 'AddedTypeDefElements' then
    exit;
  for x := 0 to xXml.Items.Count - 1 do
  begin
    if xXml.Items.XmlItems[x].Name = 'AddedTypeDefElement' then
    begin
      dXml := xXml.Items.XmlItems[x];
      xXsd := _UsedAt(dXml.Items.XmlValueByTag['UsedAt']);
      if Assigned(xXsd) then
      begin
        for E := 0 to dXml.Items.Count - 1 do
        begin
          if dXml.Items.XmlItems[E].Name = 'Added' then
          begin
            sType := FindTypeDef( dXml.Items.XmlItems[E].Items.XmlValueByTag ['NameSpace']
                                , dXml.Items.XmlItems[E].Items.XmlValueByTag['Name']
                                );
            if Assigned(sType) then
            begin
              xXsd.AddElementDef( self
                                , dXml.Items.XmlItems[E].Items.XmlValueByTag['ElementName']
                                , sType
                                );
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TXsdDescr.ChangeElementsDatatype(aNameSpace, aName: String; aNewType: TXsdDataType);
  procedure _process (aTypeDef: TXsdDataType);
  var
    x: Integer;
  begin
    if not Assigned(aTypeDef) then Exit;
    if aTypeDef._Processed then Exit;
    aTypeDef._Processed := True;
    try
      for x := 0 to aTypeDef.ElementDefs.Count - 1 do with aTypeDef.ElementDefs.Xsds[x] do
      begin
        if (ElementNameSpace = aNameSpace)
        and (ElementName = aName) then
          sType := aNewType; // else ???? well ...
        _process(sType);
      end;
    finally
      aTypeDef._Processed := False;
    end;
  end;
var
  x: Integer;
begin
  _process (TypeDef);
  for x := 0 to TypeDefs.Count - 1 do
    _process (Typedefs.XsdDataTypes[x]);
end;

procedure TXsdDescr.Finalise;
  procedure _checkLock;
  var
    x: Integer;
  begin
    for x := 0 to Garbage.Count - 1 do
      if Garbage.Objects[x] is TXsdDataType then with (Garbage.Objects[x] as TXsdDataType) do
        if _Processed then
          ShowMessage ('Fnd: ' + BaseNameSpace + ';' + BaseDataTypeName);
  end;

  procedure _linkToBase(aTypeDef: TXsdDataType);
  var
    x: Integer;
  begin
    if not Assigned (aTypeDef) then Exit;
    with aTypeDef do
    begin
      if _Processed then Exit;
      _Processed := True;
      try
        if Assigned (BaseDataType) then Exit;
        if not IsBuiltIn
        and (BaseDataTypeName <> '') then
        begin
          BaseDataType := FindTypeDef(BaseNameSpace, BaseDataTypeName);
          if not Assigned (BaseDataType) then
            BaseDataType := FindTypeDef(scXMLSchemaURI, xsdString);
          if IsExtention
          and Assigned (BaseDataType) then
            BaseDataType.ExtendedByList.AddObject ('', aTypeDef);
        end;
        for x := 0 to ElementDefs.Count - 1 do
          _linkToBase(ElementDefs.Xsds[x].sType);
      finally
        _Processed := False;
      end;
    end;
  end;

  procedure _expandGroups (aTrack: String; aTypeDef: TXsdDataType);
  var
    x, y, d: Integer;
    xType, dbType: TXsdDataType;
    xXsd, dbXsd: TXsd;
  begin
    if not Assigned (aTypeDef) then
      Exit;
    if aTypeDef._Processed then
      Exit;
    aTypeDef._Processed := True;
    try
      try
        with aTypeDef do
        begin
          x := 0;
          while x < ElementDefs.Count do
          begin
            while ElementDefs.Xsds[x]._isGroup
            and Assigned (ElementDefs.Xsds[x].sType) do
            begin
              dbXsd := ElementDefs.Xsds[x];
              dbType := dbXsd.sType;
              if not ElementDefs.Xsds[x].sType._isGroup then
              begin
                ElementDefs.Xsds[x]._isGroup := False;  // to avoid endless looping...
                SjowMessage
                     (Format
                       ( '(expanding Groups) Found datatype (%s:%s) on element (%s:%s) not a group. %s '
                       , [ ElementDefs.Xsds[x]._NameSpace
                         , ElementDefs.Xsds[x]._DataTypeName
                         , ElementDefs.Xsds[x].ElementNameSpace
                         , ElementDefs.Xsds[x].ElementName
                         , aTrack
                         ]
                       )
                     )
              end
              else
              begin
                xType := ElementDefs.Xsds[x].sType;
                ElementDefs.Delete(x);
                for d := xType.ElementDefs.Count - 1 downto 0 do
                begin
                  xXsd := TXsd.Create(self, xType.ElementDefs.Xsds[d]);
                  Garbage.AddObject('', xXsd);
                  ElementDefs.InsertObject(x, xXsd.ElementName, xXsd);
                end;
              end;
            end;
            if Assigned (ElementDefs.Xsds[x].sType) then
              _expandGroups(aTrack + '.' + ElementDefs.Xsds[x].ElementName, ElementDefs.Xsds[x].sType);
            Inc (x);
          end;
        end;
      except
        on e: Exception do
        begin
          raise Exception.Create (aTrack + ' => ' + e.Message);
        end;
      end;
    finally
      aTypeDef._Processed := False;
    end;
  end;

  procedure _linkElmntToType(aTrack: String; aTypeDef: TXsdDataType);
  var
    x, d: Integer;
  begin
    if not Assigned (aTypeDef) then Exit;
    if aTypeDef._Processed then Exit;
    aTypeDef._Processed := True;
    try
      try
        with aTypeDef do
        begin
          for x := 0 to ElementDefs.Count - 1 do
          with ElementDefs.Xsds[x] do
          begin
            if not Assigned (sType)
            and (  (_DataTypeName <> '')
                 or _isGroup
                ) then
            begin
              sType := FindTypeDef(_NameSpace, _DataTypeName);
              if not Assigned (sType) then
                SjowMessage(Format
//              raise Exception.CreateFmt
                       ( 'Coud not find datatype (%s:%s) on element (%s:%s) %S'
                       , [_NameSpace, _DataTypeName, ElementNameSpace, ElementName, aTrack]
                       )
                       )
              else
              begin
                for d := 0 to sType.Documentation.Count - 1 do
                  Documentation.Add (sType.Documentation.Strings[d]);
              end;
            end;
            _linkElmntToType(aTrack + '.' + ElementName, sType);
          end;
        end;
      except
        on e: Exception do
        begin
          raise Exception.Create (aTrack + ' => ' + e.Message);
        end;
      end;
    finally
      aTypeDef._Processed := False;
    end;
  end;

  procedure _formQualifyNamespaces (aTypeDef: TXsdDataType);
  var
    x, y: Integer;
    refXsd: TXsd;
  begin
    if not Assigned (aTypeDef) then Exit;
    with aTypeDef do
    begin
      if _Processed then Exit;
      _Processed := True;
      try
        for x := 0 to ElementDefs.Count - 1 do
        with ElementDefs.Xsds[x] do
        begin
          if ElementName = 'uqelem0' then
            ElementName := 'uqelem0';
          if not FormQualified then
            ElementNameSpace := '';
          _formQualifyNamespaces (sType);
        end;
      finally
        _Processed := False;
      end;
    end;
  end;

  procedure _linkElmntToRef (aTypeDef: TXsdDataType; aRoot: Boolean);
  var
    x, y: Integer;
    refXsd: TXsd;
  begin
    if not Assigned (aTypeDef) then Exit;
    with aTypeDef do
    begin
      if _Processed then Exit;
      _Processed := True;
      try
        for x := 0 to ElementDefs.Count - 1 do
        with ElementDefs.Xsds[x] do
        begin
          if not Assigned (sType)
          and (_RefElementName <> '') then
          begin
            refXsd := FindElement(_RefNameSpace, _RefElementName);
            if Assigned (refXsd) then
            begin
              sType := refXsd.sType;
              if not aRoot then
                FormQualified := refXsd.FormQualified;
              for y := 0 to refXsd.Documentation.Count - 1 do
                Documentation.Add(refXsd.Documentation.Strings[y]);
            end
            else
              raise Exception.CreateFmt
                     ( 'Coud not find datatype (%s:%s) on element (%s:%s)'
                     , [_NameSpace, _DataTypeName, ElementNameSpace, ElementName]
                     );
          end;
          _linkElmntToRef(sType, False);
        end;
      finally
        _Processed := False;
      end;
    end;
  end;

  procedure _linkElmntToDefaultType(aTrack: String; aTypeDef: TXsdDataType);
  var
    x: Integer;
  begin
    if not Assigned (aTypeDef) then Exit;
    if aTypeDef._Processed then Exit;
    aTypeDef._Processed := True;
    try
      try
        with aTypeDef do
          for x := 0 to ElementDefs.Count - 1 do with ElementDefs.Xsds[x] do
            if not Assigned (sType) then
              sType := FindTypeDef(scXMLSchemaURI, xsdString)
            else
              _linkElmntToDefaultType(aTrack + '.' + ElementName, sType);
      except
        on e: Exception do
        begin
          raise Exception.Create (aTrack + ' => ' + e.Message);
        end;
      end;
    finally
      aTypeDef._Processed := False;
    end;
  end;


  procedure _inheritFacets (aTypeDef: TXsdDataType);
  {
    for a facet, when nno value, it will get the base's value if exists
    this way a facet will end with first value in upline that is not empty
    assigning that value along the way
  }
    function _Length (t: TXsdDataType): String;
    begin
      if t._Processed then Exit;
      t._Processed := True;
      if (t.Length = '') and Assigned (t.BaseDataType) then t.Length := _Length(t.BaseDataType);
      result := t.Length;
      t._Processed := False;
    end;
    function _MinLength (t: TXsdDataType): String;
    begin
      if t._Processed then Exit;
      t._Processed := True;
      if (t.MinLength = '') and Assigned (t.BaseDataType) then t.MinLength := _MinLength(t.BaseDataType);
      result := t.MinLength;
      t._Processed := False;
    end;
    function _MaxLength (t: TXsdDataType): String;
    begin
      if t._Processed then Exit;
      t._Processed := True;
      if (t.MaxLength = '') and Assigned (t.BaseDataType) then t.MaxLength := _MaxLength(t.BaseDataType);
      result := t.MaxLength;
      t._Processed := False;
    end;
    function _Pattern (t: TXsdDataType): String;
    begin
      if t._Processed then Exit;
      t._Processed := True;
      if (t.Pattern = '') and Assigned (t.BaseDataType) then t.Pattern := _Pattern(t.BaseDataType);
      result := t.Pattern;
      t._Processed := False;
    end;
    function _Whitespace (t: TXsdDataType): String;
    begin
      if t._Processed then Exit;
      t._Processed := True;
      if (t.Whitespace = '') and Assigned (t.BaseDataType) then t.Whitespace := _Whitespace(t.BaseDataType);
      result := t.Whitespace;
      t._Processed := False;
    end;
    function _MaxInclusive (t: TXsdDataType): String;
    begin
      if t._Processed then Exit;
      t._Processed := True;
      if (t.MaxInclusive = '') and Assigned (t.BaseDataType) then t.MaxInclusive := _MaxInclusive(t.BaseDataType);
      result := t.MaxInclusive;
      t._Processed := False;
    end;
    function _MaxExclusive (t: TXsdDataType): String;
    begin
      if t._Processed then Exit;
      t._Processed := True;
      if (t.MaxExclusive = '') and Assigned (t.BaseDataType) then t.MaxExclusive := _MaxExclusive(t.BaseDataType);
      result := t.MaxExclusive;
      t._Processed := False;
    end;
    function _MinInclusive (t: TXsdDataType): String;
    begin
      if t._Processed then Exit;
      t._Processed := True;
      if (t.MinInclusive = '') and Assigned (t.BaseDataType) then t.MinInclusive := _MinInclusive(t.BaseDataType);
      result := t.MinInclusive;
      t._Processed := False;
    end;
    function _MinExclusive (t: TXsdDataType): String;
    begin
      if t._Processed then Exit;
      t._Processed := True;
      if (t.MinExclusive = '') and Assigned (t.BaseDataType) then t.MinExclusive := _MinExclusive(t.BaseDataType);
      result := t.MinExclusive;
      t._Processed := False;
    end;
    function _Numeric (t: TXsdDataType): String;
    begin
      if t._Processed then Exit;
      t._Processed := True;
      if (t.Numeric = '') and Assigned (t.BaseDataType) then t.Numeric := _Numeric(t.BaseDataType);
      result := t.Numeric;
      t._Processed := False;
    end;
    function _TotalDigits (t: TXsdDataType): String;
    begin
      if t._Processed then Exit;
      t._Processed := True;
      if (t.TotalDigits = '') and Assigned (t.BaseDataType) then t.TotalDigits := _TotalDigits(t.BaseDataType);
      result := t.TotalDigits;
      t._Processed := False;
    end;
    function _FractionalDigits (t: TXsdDataType): String;
    begin
      if t._Processed then Exit;
      t._Processed := True;
      if (t.FractionalDigits = '') and Assigned (t.BaseDataType) then t.FractionalDigits := _FractionalDigits(t.BaseDataType);
      result := t.FractionalDigits;
      t._Processed := False;
    end;
    procedure _inheritEnumeration (t: TXsdDataType);
    var
      x: Integer;
    begin
      if t._Processed then Exit;
      t._Processed := True;
      try
        if (t.Enumerations.Count = 0)
        and Assigned (t.BaseDataType) then
        begin
          t.Enumerations.Text := t.BaseDataType.Enumerations.Text;
          for x := 0 to t.BaseDataType.Enumerations.Count - 1 do
            t.Enumerations.Objects[x] :=
              (t.BaseDataType.Enumerations.Objects[x] as TXsdEnumeration).Clone;
          _inheritEnumeration(t.BaseDataType);
        end;
      finally
        t._Processed := False;
      end;
    end;
  var
    x: Integer;
  begin
    if not Assigned (aTypeDef) then Exit;
    if aTypeDef._Processed then
      Exit;
    if not aTypeDef.IsBuiltIn then
    begin
      _Length(aTypeDef);
      _MinLength(aTypeDef);
      _MaxLength(aTypeDef);
      _Pattern(aTypeDef);
      _Whitespace(aTypeDef);
      _MaxInclusive(aTypeDef);
      _MaxExclusive(aTypeDef);
      _MinInclusive(aTypeDef);
      _MinExclusive(aTypeDef);
      _Numeric(aTypeDef);
      _TotalDigits(aTypeDef);
      _FractionalDigits(aTypeDef);
      _inheritEnumeration(aTypeDef);
    end;
    aTypeDef._Processed := True;
    try
      for x := 0 to aTypeDef.AttributeDefs.Count - 1 do
        _inheritFacets(aTypeDef.AttributeDefs.XsdAttrs[x]);
      for x := 0 to aTypeDef.ElementDefs.Count - 1 do
        _inheritFacets(aTypeDef.ElementDefs.Xsds[x].sType);
    finally
      aTypeDef._Processed := False;
    end;
  end;

  procedure _adjustExtendedByTypeDefs(aTypeDef: TXsdDataType);
  var
    x, y: integer;
  begin
    if not Assigned (aTypeDef)
    or aTypeDef._Extended then
      exit;
    aTypeDef._Extended := True;
    for x := 0 to aTypeDef.ExtendedByList.Count - 1 do
    begin
    for y := aTypeDef.ElementDefs.Count - 1 downto 0 do
      aTypeDef.ExtendedByList.XsdDataTypes[x].ElementDefs.InsertObject(0, '', aTypeDef.ElementDefs.Xsds[y]);
    for y := aTypeDef.AttributeDefs.Count - 1 downto 0 do
      aTypeDef.ExtendedByList.XsdDataTypes[x].AttributeDefs.InsertObject(0, '', aTypeDef.AttributeDefs.XsdAttrs[y]);
      _adjustExtendedByTypeDefs(aTypeDef.ExtendedByList.XsdDataTypes[x]);
    end;
  end;

  procedure _checkSTypes (aTag: String; aTypeDef: TXsdDataType);
  var
    x: Integer;
  begin
    if not Assigned(aTypeDef) then Exit;
    if aTypeDef._Processed then Exit;
    aTypeDef._Processed := True;
    try
      for x := 0 to aTypeDef.ElementDefs.Count - 1 do
      with aTypeDef.ElementDefs.Xsds[x] do
      begin
        if not Assigned (sType) then
//          raise Exception.CreateFmt('No definition found for %s%s', [aTag, ElementName]);
          ;
        _checkSTypes(aTag + ElementName + '=>', sType);
      end;
    finally
      TypeDef._Processed := False;
    end;
  end;

  procedure _track (atrack: String; typedef: TXsdDataType);
  var
    x: Integer;
  begin
    for x := 0 to typedef.ElementDefs.Count - 1 do with typedef.ElementDefs.Xsds[x] do
    begin
      ShowMessage(aTrack + '.' + ElementName);
      _track (aTrack + '.' + ElementName, sType);
    end;
  end;

var
  x: integer;
begin
{
  if not systemStarting then
  begin
    ShowMessage('e#: ' + IntToStr (self.TypeDef.ElementDefs.Count));
    _track ('', typedef);
    ShowMessage('t#: ' + IntToStr (self.TypeDefs.Count));
    for x := 0 to TypeDefs.Count - 1 do
      _track('', TypeDefs.XsdDataTypes[x]);
  end;
}
  _LinkElmntToType('', TypeDef);
  for x := 0 to TypeDefs.Count - 1 do
    _LinkElmntToType('', TypeDefs.XsdDataTypes[x]);
  _expandGroups('', TypeDef);
  for x := 0 to TypeDefs.Count - 1 do
    _expandGroups('', TypeDefs.XsdDataTypes[x]);
  _linkElmntToRef(TypeDef, True);
  for x := 0 to TypeDefs.Count - 1 do
    _linkElmntToRef(TypeDefs.XsdDataTypes[x], False);
  _formQualifyNamespaces (TypeDef);
  for x := 0 to TypeDefs.Count - 1 do
    _formQualifyNamespaces (TypeDefs.XsdDataTypes[x]);
  _linkToBase(TypeDef);
  for x := 0 to TypeDefs.Count - 1 do
    _linkToBase(TypeDefs.XsdDataTypes[x]);
  _LinkElmntToDefaultType('', TypeDef);
  for x := 0 to TypeDefs.Count - 1 do
    _LinkElmntToDefaultType('', TypeDefs.XsdDataTypes[x]);
  _inheritFacets(TypeDef);
  for x := 0 to TypeDefs.Count - 1 do
    _inheritFacets(TypeDefs.XsdDataTypes[x]);
  for x := 0 to TypeDefs.Count - 1 do
  begin
    if not TypeDefs.XsdDataTypes[x].IsExtention
    and not TypeDefs.XsdDataTypes[x].IsBuiltIn
    and not Assigned(TypeDefs.XsdDataTypes[x].BaseDataType) then
      _adjustExtendedByTypeDefs(TypeDefs.XsdDataTypes[x]);
  end;
  _checkSTypes ('', TypeDef);
{
    if not systemStarting then
    begin
      ShowMessage('e#: ' + IntToStr (self.TypeDef.ElementDefs.Count));
      _track ('', typedef);
      ShowMessage('t#: ' + IntToStr (self.TypeDefs.Count));
      for x := 0 to TypeDefs.Count - 1 do
        _track('', TypeDefs.XsdDataTypes[x]);
    end;
}
end;

function TXsdDescr.FindTypeDef(aNameSpace, aName: String): TXsdDataType;
var
  f: integer;
begin
  result := nil;
  if TypeDefs.Find(aNameSpace + ';' + aName, f) then
    result := TypeDefs.XsdDataTypes[f]
  else if TypeDefs.Find(scXMLSchemaURI + ';' + aName, f) then
    result := TypeDefs.XsdDataTypes[f];
end;

function TXsdDescr.FindElement (aNameSpace , aName: String): TXsd ;
  function _search (aTypedef: TXsdDataType): TXsd;
  var
    x: integer;
  begin
    result := nil;
    if not Assigned(aTypedef) then Exit;
    with aTypeDef.ElementDefs do
    begin
      for x := 0 to Count - 1 do
      begin
        if (Xsds[x].ElementName = aName)
        and (Xsds[x].ElementNameSpace = aNameSpace) then
        begin
          result := Xsds[x];
          exit;
        end;
      end;
    end;
  end;
var
  x: Integer;
begin
  result := _search(TypeDef);
  if not Assigned (result) then
  begin
    for x := 0 to TypeDefs.Count - 1 do
    begin
      result := _search(TypeDefs.XsdDataTypes[x]);
      if Assigned (result) then
        exit;
    end;
  end;
end;


{ TXsdEnumeration }

function TXsdEnumeration.Clone: TXsdEnumeration;
begin
  result := TXsdEnumeration.Create;
  result.Value := self.Value;
  result.Annotation := self.Annotation;
end;

initialization

systemStarting := True;
defaultXsdMaxDepthBillOfMaterials := 1;
defaultXsdMaxDepthXmlGen := 9999;
xsdMaxDepthXmlGen := defaultXsdMaxDepthXmlGen;
xsdValidateAssignmentsAgainstSchema := True;

finalization

end.
