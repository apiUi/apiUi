unit Xsdz;

interface

uses Classes
   , XmlDom
   , XmlDoc
   , XmlSchema
   , ParserClasses
   , Variants
   , XMLValidate
   ;

type TOnHaveString = procedure ( aString: String) of Object;
type
  TXsdDataTypeList = class;
  TXsdDataType = class;
  TXsdAttrList = class;
  TXsdAttr = class;
  TXsdList = class;
  TXsd = class;
  TXsdDescr = class;


  TXsdDataTypeList = class (TStringList)
    protected
      function GetXsdDataType (Index: integer): TXsdDataType;
    public
      property XsdDataTypes [Index: integer]: TXsdDataType read GetXsdDataType;
      procedure Clear; override;
  end;

  TXsdDataType = class(TObject)
    private
    function getSchemaAsText: String;
    function getNSAttributes: String;
    function getXsiNameSpaceAttribute: String;
    function getNSPrefix: String;
    public
      xsdDescr: TXsdDescr;
      _Error: Boolean;
      NameSpace: String;
      DataTypeName: String;
      BaseDataTypeName: String;
      BaseNameSpace: String;
      ContentModel: String;
      DerivationMethod: String;
      Length: String;
      MinLength: String;
      MaxLength: String;
      Pattern : String;
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
      ExtendedByList: TXsdDataTypeList;
      ElementDefs: TXsdList;
      AttributeDefs: TXsdAttrList;
      property SchemaAsText: String read getSchemaAsText;
      procedure AddXsd (aChildXsd: TXsd);
      constructor Create;
      destructor Destroy; override;
      function IsValidValue (aValue: String; var aMessage: String): Boolean;
      property NSPrefix: String read getNSPrefix;
      property NSAttributes: String read getNSAttributes;
      property XsiNameSpaceAttribute: String read getXsiNameSpaceAttribute;
 end;

  TXsdAttr = class (TXsdDataType)
  private
  public
    Name: String;
    Use: String;
    constructor Create;
    destructor Destroy; override;
  end;

  TXsdAttrList = class (TStringList)
    protected
      function GetXsdAttr (Index: integer): TXsdAttr;
    public
      property XsdAttrs [Index: integer]: TXsdAttr read GetXsdAttr;
      procedure Clear; override;
  end;

  TXsd = class(TXsdDataType)
    private
    public
      _Processed: Boolean;
      _DepthBillOfMaterial: Integer;
      Name: String;
      IsComplex: Boolean;
      IsBuiltIn: Boolean;
      DoNotEncode: Boolean;
      minOccurs: String;
      maxOccurs: String;
      TypeDef: TXsd;
{      Parent: TXsd; }
      function IsTypeDefEnabled: Boolean;
      function ccbPictureClause: string;
      function ccbOccursClause: String;
      procedure GenerateHtmlReport (aStringList: TStringList);
      procedure GenerateReport (aStringList: TStringList);
      procedure GenerateCopyBook (aPrefix, aSuffix, a88Prefix, a88Suffix, aLevel: String; aStringList: TStringList);
      constructor Create;
      destructor Destroy; override;
  end;

  TXsdList = class (TStringList)
    protected
      function GetXsd (Index: integer): TXsd;
    public
      property Xsds [Index: integer]: TXsd read GetXsd;
      procedure ResetProcessed;
      procedure ClearListOnly;
      procedure Clear; override;
  end;

  TXsdDescr = class (TObject)
private
  xsdGlobalXsdList: TXsdList;
  function FindTypeDef (aNameSpace, aName: String): TXsd; {twijfelachtig}
  function ArrayContains(const Value: DOMString; const Values: array of DOMString): Boolean;
  procedure CopyDataType (aXsd: TXsdDataType; aDataType: IXMLTypeDef);
  procedure AddBuiltInType (aParent: TXsd; aList: TXsdList; aSimpleType: IXMLSimpleTypeDef);
  procedure AddSimpleType (aParent: TXsd; aList: TXsdList; aSimpleType: IXMLSimpleTypeDef);
  procedure AddElementsFromBase (aParent: TXsd; aDataType: IXMLTypeDef; aList: TXsdList);
  procedure AddElement (aParent: TXsd; aList: TXsdList; aElementDef: IXMLElementdef);
  procedure AddComplexType (aParent: TXsd; aList: TXsdList; aComplexType: IXMLComplexTypeDef);
  procedure LinkExtendedTypeDefs;
public
  Alias: String;
  FileName: String;
  ElementDefs: TXsdList;
  TypeDefs: TXsdList;
  FileContents: TStringList;
  TargetNamespace: String;
  TargetNSPrefix: String;
  NamespaceURI: String;
  Prefix: String;
  SchemaName: String;
  function xsdGetReference (aNameSpace, aName, aMinOccurs, aMaxOccurs, aDataType: String; var aXsd: TXsd): Boolean;
  function xsdFindTypeDef (aNameSpace, aName: String): TXsdDataType;
  function AddFromSchemaDef (aSchemaDef: IXMLSchemaDef): Boolean;
  function LoadFromFile (aFileName: String; ErrorFound: TOnErrorEvent): Boolean;
  constructor Create;
  destructor Destroy; override;
end;

TXsdDescrList = class (TStringList)
  private
    function GetXsdDescr(Index: integer): TXsdDescr;
  public
    function FindTypeDef(aNameSpace: String; aName: String): TXsd;
    property XsdDescrs [Index: integer]: TXsdDescr read GetXsdDescr;
    procedure Clear; override;
end;

function ValidateXml (aCaption, aXmlString: String; aSchemaLocation, aNameSpace: WideString; var aMessage: String): Boolean;
function NameWithoutPrefix (aName: String): String;
function GetSchemaTargetNameSpace (aFileName: String): String;
function xsdNameSpacePrefix (aNameSpace: String): String;
function xsdGenerateNameSpaceAttributes: String;
function xsdGenerateXsiNameSpaceAttribute: String;
function xsdGenerateXsdNameSpaceAttribute: String;
procedure xsdAddTargetNameSpace (aTargetNameSpace: String);

var
  xsiGenerated: Boolean;
  xsdGenerated: Boolean;
  xsdMaxDepthBillOfMaterials: Integer;
  xsdGlobalNameSpaceList: TStringList;

implementation

uses StrUtils
   , SysUtils
   , RegExpr
   , igGlobals
   , DateUtils
   , Math
   , XMLIntf
   , Dialogs
   , Controls
   , XmlSchemaTags
   ;

{ TXsdList }

function GetSchemaTargetNameSpace (aFileName: String): String;
begin
  result := VarToStr (LoadXMLSchema(aFileName).SchemaDef.TargetNamespace);
end;

function ValidateXml (aCaption, aXmlString: String; aSchemaLocation, aNameSpace: WideString; var aMessage: String): Boolean;
var
  Doc: IXMLDocument;
begin
  aMessage := 'Document validated without errors.';
  result := True;
  try
    Doc := LoadXMLData(aXmlString);
    ValidateXMLDoc(Doc, aSchemaLocation, aNameSpace);
  except
    on E: Exception do
    begin
      aMessage := aCaption + ' ' + E.Message;
      result := False;
    end;
  end;
end;

function NameWithoutPrefix (aName: String): String;
var
  x: Integer;
begin
  result := '';
  for x := 1 to Length (aName) do
  begin
    if aName [x] = ':' then
      result := ''
    else
      result := result + aName [x];
  end;
end;

function AddDocumentation (aStringList: TStringList; aDoc: IXMLDocumentationCollection): String;
var
  x: Integer;
begin
  for x := 0 to aDoc.Count - 1 do
    aStringList.Add ( aDoc.Items [x].Text);
end;


function VarToStr (aVariant: Variant): String;
begin
  if (VarType (aVariant) = varString)
  or (VarType (aVariant) = varOleStr)
  then
    result := aVariant
  else
    result := '';
end;

function MatchPattern (const ARegExpr, AInputStr : RegExprString) : boolean;
 var Rx : TRegExpr;
begin
  if Length (AInputStr) > 8000 then
    raise Exception.Create('Due to size of input, checking against datatype disabled');
  Rx := TRegExpr.Create;
  try
    Rx.Expression := '^' + ARegExpr + '$';
    Result := Rx.Exec (AInputStr);
    finally
      Rx.Free;
   end;
end; { of function ExecRegExpr }

procedure TXsdList.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Xsds [x].Free;
  inherited;
end;

procedure TXsdList.ClearListOnly;
begin
  inherited Clear;
end;

function TXsdList.GetXsd(Index: integer): TXsd;
begin
  result := TXsd (Objects [index]);
end;

procedure TXsdList.ResetProcessed;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Xsds [x]._Processed := False;
end;

{ TXsd }

procedure TXsdDataType.AddXsd(aChildXsd: TXsd);
begin
  ElementDefs.AddObject(aChildXsd.Name, aChildXsd);
{  aChildXsd.Parent := self; }
end;

constructor TXsd.Create;
begin
  inherited;
  minOccurs := '1';
  maxOccurs := '1';
{  Parent := nil; }
end;

destructor TXsd.Destroy;
begin
  inherited;
end;

constructor TXsdDescr.Create;
begin
  xsdGlobalXsdList := TXsdList.Create;
  xsdGlobalXsdList.Sorted := True;
  xsdGlobalXsdList.CaseSensitive := True;
  ElementDefs := TXsdList.Create;
  TypeDefs := TXsdList.Create;
  FileContents := TStringList.Create;
end;


destructor TXsdDescr.Destroy;
begin
  ElementDefs.Free;
  TypeDefs.Free;
  FileContents.Free;
  if Assigned(xsdGlobalXsdList) then
  begin
    while xsdGlobalXsdList.Count > 0 do begin
      xsdGlobalXsdList.Objects[0].Free;
      xsdGlobalXsdList.Delete(0);
    end;
    xsdGlobalXsdList.Free;
  end;
  inherited;
end;

procedure TXsdDescr.CopyDataType (aXsd: TXsdDataType; aDataType: IXMLTypeDef);
var
  I: Integer;
begin
  aXsd.DataTypeName := VarToStr (aDataType.Name);
  aXsd.Length := VarToStr (aDataType.Length);
  aXsd.minLength := VarToStr (aDataType.MinLength);
  aXsd.maxLength := VarToStr (aDataType.MaxLength);
  aXsd.Pattern := VarToStr (aDataType.Pattern);
  aXsd.Whitespace:= VarToStr (aDataType.Whitespace);
  aXsd.MaxInclusive:= VarToStr (aDataType.MaxInclusive);
  aXsd.MaxExclusive:= VarToStr (aDataType.MaxExclusive);
  aXsd.MinInclusive:= VarToStr (aDataType.MinInclusive);
  aXsd.MinExclusive:= VarToStr (aDataType.MinExclusive);
  aXsd.Numeric := VarToStr (aDataType.Numeric);
  aXsd.TotalDigits:= VarToStr (aDataType.TotalDigits);
  aXsd.FractionalDigits := VarToStr (aDataType.FractionalDigits);
  aXsd.BaseDataTypeName := VarToStr (aDataType.BaseTypeName);
  AddDocumentation(aXsd.Documentation, aDataType.Documentation);
  if aXsd.BaseDataTypeName = '' then
    aXsd.BaseDataTypeName := aXsd.DataTypeName;
  for I := 0 to aDataType.Enumerations.Count - 1 do
  begin
    aXsd.Enumerations.Add(VarToStr(aDataType.Enumerations.Items [I].Value));
  end;
end;

function TXsdDescr.ArrayContains(const Value: DOMString; const Values: array of DOMString): Boolean;
var
  I: Integer;
begin
  for I := Low(Values) to High(Values) do
    if Value = Values[I] then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

procedure TXsdDescr.AddBuiltInType (aParent: TXsd; aList: TXsdList; aSimpleType: IXMLSimpleTypeDef);
var
  xXsd: TXsd;
begin
  if xsdGetReference ( ''
                     , aSimpleType.Name
                     , '1'
                     , '1'
                     , VarToStr ((aSimpleType as IXMLTypeDef).BaseTypeName)
                     , xXsd
                     ) = True then
  begin
    xXsd.DataTypeName := aSimpleType.BaseTypeName;
    xXsd.NameSpace := '';
    xXsd.IsComplex := False;
    xXsd.IsBuiltIn := True;
    CopyDataType(xXsd, aSimpleType as IXMLTypeDef);
  end;
  {  xXsd.Parent := aParent; }
  aList.AddObject(xXsd.Name, xXsd);
end;

procedure TXsdDescr.AddSimpleType (aParent: TXsd; aList: TXsdList; aSimpleType: IXMLSimpleTypeDef);
var
  xXsd: TXsd;
begin
  if xsdGetReference ( VarToStr (aSimpleType.SchemaDef.TargetNamespace)
                     , aSimpleType.Name
                     , '1'
                     , '1'
                     , VarToStr ((aSimpleType as IXMLTypeDef).BaseTypeName)
                     , xXsd
                     ) = True then
  begin
    xXsd.DataTypeName := aSimpleType.BaseTypeName;
    xXsd.NameSpace := VarToStr (aSimpleType.SchemaDef.TargetNamespace);
    xXsd.IsComplex := False;
    xXsd.IsBuiltIn := ArrayContains(aSimpleType.BaseTypeName, XmlSchemaTags.BuiltInTypeNames);
    CopyDataType(xXsd, aSimpleType as IXMLTypeDef);
  end;
  {  xXsd.Parent := aParent; }
  aList.AddObject(xXsd.Name, xXsd);
end;

procedure TXsdDescr.AddElementsFromBase (aParent: TXsd; aDataType: IXMLTypeDef; aList: TXsdList);
var
  xType: IXMLComplexTypeDef;
  x: Integer;
begin
  if not Assigned (aDataType) then
    exit;
  AddElementsFromBase(aParent,aDataType.BaseType, aList);
  if aDataType.IsComplex then
  begin
    xType := aDataType as IXMLComplexTypeDef;
    for x := 0 to xType.ElementDefs.Count - 1 do
      AddElement (aParent, aList, xType.ElementDefs.Items [x]);
  end;
end;

procedure TXsdDescr.AddElement (aParent: TXsd; aList: TXsdList; aElementDef: IXMLElementdef);
var
  xXsd: TXsd;
  I: Integer;
  ComplexTypeDef: IXMLComplexTypeDef;
  AttributeDef: IXMLAttributeDef;
  XsdAttr: TXsdAttr;
  DataType: IXMLTypeDef;
begin
  if xsdGetReference ( VarToStr (aElementDef.SchemaDef.TargetNamespace)
                     , NameWithoutPrefix (aElementDef.Name)
                     , aElementDef.MinOccurs
                     , aElementDef.MaxOccurs
                     , VarToStr (aElementDef.DataTypeName)
                     , xXsd
                     ) = True then
  begin
    try
      xXsd.DataTypeName := aElementDef.DataTypeName;
    except
      xXsd.DataTypeName := '?undefined';
      xXsd._Error := True;
    end;
    xXsd.minOccurs := aElementDef.MinOccurs;
    xXsd.maxOccurs := aElementDef.MaxOccurs;
    try
      xXsd.IsComplex := aElementDef.DataType.IsComplex;
    except
      xXsd.IsComplex := False;
      xXsd._Error := True;
    end;
    AddDocumentation(xXsd.Documentation, aElementDef.Documentation);
    if xXsd.IsComplex then
    begin
      ComplexTypeDef := aElementDef.DataType as IXMLComplexTypeDef;
      case ComplexTypeDef.ContentModel of
        cmAll: xXsd.ContentModel := 'All';
        cmChoice: xXsd.ContentModel  := 'Choice';
        cmSequence: xXsd.ContentModel := 'Sequence';
        cmGroupRef: xXsd.ContentModel := 'GroupRef';
        cmEmpty: xXsd.ContentModel := 'Empty';
      end;
      AddElementsFromBase(xXsd, ComplexTypeDef.BaseType, xXsd.ElementDefs);
    end
    else
    begin
      try
        xXsd.IsBuiltIn := ArrayContains(aElementDef.DataTypeName, XmlSchemaTags.BuiltInTypeNames);
      except
        xXsd.IsBuiltIn := False;
        xXsd._Error := True;
      end;
    end;

    try
      DataType := aElementDef.DataType;
      CopyDataType(xXsd, DataType);
      if Assigned (aElementDef.Ref) then
        xXsd.Namespace := VarToStr (aElementDef.Ref.SchemaDef.TargetNamespace)
      else
        xXsd.Namespace := VarToStr (aElementDef.SchemaDef.TargetNamespace);
      xsdAddTargetNameSpace (xXsd.Namespace);

      for I := 0 to aElementDef.AttributeDefs.Count - 1 do
      begin
        AttributeDef := aElementDef.AttributeDefs.AttributeDefs [I];
        XsdAttr := TXsdAttr.Create;
        XsdAttr.xsdDescr := Self;
        XsdAttr.NameSpace := VarToStr (AttributeDef.SchemaDef.TargetNamespace);
        XsdAttr.Name := AttributeDef.Name;
        XsdAttr.Use := VarToStr (AttributeDef.Use);
        CopyDataType(XsdAttr, AttributeDef.DataType);
        xXsd.AttributeDefs.AddObject(XsdAttr.Name, XsdAttr);
      end;

      for I := 0 to aElementDef.ChildElements.Count - 1 do
        AddElement (xXsd, xXsd.ElementDefs, aElementDef.ChildElements.ElementDefs [I]);
    except
      xXsd._Error := True;
    end;
  end; {if xXSD created}
  aList.AddObject(xXsd.Name, xXsd);
end;

procedure TXsdDescr.AddComplexType (aParent: TXsd; aList: TXsdList; aComplexType: IXMLComplexTypeDef);
var
  xXsd: TXsd;
  I: Integer;
  AttributeDef: IXMLAttributeDef;
  XsdAttr: TXsdAttr;
  DataType: IXMLTypeDef;

begin
  if xsdGetReference ( VarToStr (aComplexType.SchemaDef.TargetNamespace)
                     , aComplexType.Name
                     , '1'
                     , '1'
                     , VarToStr ((aComplexType as IXMLTypeDef).BaseTypeName)
                     , xXsd
                     ) = True then
  begin
    xXsd.DataTypeName := aComplexType.BaseTypeName;
    xXsd.NameSpace := VarToStr (aComplexType.SchemaDef.TargetNamespace);
    xXsd.minOccurs := '1';
    xXsd.maxOccurs := '1';
    xXsd.IsComplex := True;
    xXsd.ElementDefs := TXsdList.Create;
    if xXsd.IsComplex then
    begin
      case aComplexType.ContentModel of
        cmAll: xXsd.ContentModel := 'All';
        cmChoice: xXsd.ContentModel  := 'Choice';
        cmSequence: xXsd.ContentModel := 'Sequence';
        cmGroupRef: xXsd.ContentModel := 'GroupRef';
        cmEmpty: xXsd.ContentModel := 'Empty';
      end;
      case aComplexType.DerivationMethod of
        dmNone: xXsd.DerivationMethod := '';
        dmComplexExtension:
        begin
          xXsd.DerivationMethod  := 'ComplexExtension';
          xXsd.BaseNameSpace := VarToStr ((aComplexType as IXMLTypeDef).BaseType.SchemaDef.TargetNamespace);
        end;
        dmComplexRestriction: xXsd.DerivationMethod := 'ComplexRestriction';
        dmSimpleExtension:
        begin
          xXsd.DerivationMethod  := 'SimpleExtension';
          if Assigned ((aComplexType as IXMLTypeDef).BaseType) then
            if Assigned ((aComplexType as IXMLTypeDef).BaseType.SchemaDef) then
              xXsd.BaseNameSpace := VarToStr ((aComplexType as IXMLTypeDef).BaseType.SchemaDef.TargetNamespace);
        end;
        dmSimpleRestriction: xXsd.DerivationMethod := 'SimpleRestriction';
      end;
    end;

    DataType := aComplexType.BaseType;
    CopyDataType(xXsd, aComplexType as IXMLTypeDef);

    for I := 0 to aComplexType.AttributeDefs.Count - 1 do
    begin
      AttributeDef := aComplexType.AttributeDefs.Items [I];
      XsdAttr := TXsdAttr.Create;
      XsdAttr.xsdDescr := Self;
      XsdAttr.NameSpace := VarToStr (AttributeDef.SchemaDef.TargetNamespace);
      XsdAttr.Name := AttributeDef.Name;
      XsdAttr.Use := VarToStr (AttributeDef.Use);
      AddDocumentation(xXsd.Documentation, AttributeDef.Documentation);
      CopyDataType(XsdAttr, AttributeDef.DataType);
      xXsd.AttributeDefs.AddObject(XsdAttr.Name, XsdAttr);
    end;

    AddElementsFromBase(xXsd, aComplexType, xXsd.ElementDefs);
  end;
  aList.AddObject(xXsd.Name, xXsd);
end;

function TXsdDescr.AddFromSchemaDef(aSchemaDef: IXMLSchemaDef): Boolean;
var
  x: Integer;
begin
  TargetNamespace := VarToStr (aSchemadef.TargetNamespace);
  TargetNSPrefix := VarToStr (aSchemaDef.TargetNSPrefix);
  NamespaceURI := VarToStr (aSchemaDef.NamespaceURI);
  Prefix := VarToStr (aSchemaDef.Prefix);
  if VarToStr (aSchemaDef.TargetNSPrefix) <> '' then
    SchemaName := VarToStr (aSchemaDef.TargetNSPrefix)
                + ':'
                + VarToStr (aSchemaDef.TargetNamespace)
  else
    SchemaName := VarToStr (aSchemaDef.TargetNamespace)
                ;
  for x := 0 to aSchemaDef.ElementDefs.Count - 1 do
    AddElement (nil, ElementDefs, aSchemaDef.ElementDefs.Items [x]);
  for x := 0 to aSchemaDef.BuiltInTypes.Count - 1 do
    AddBuiltInType (nil, TypeDefs, aSchemaDef.BuiltInTypes.Items [x]);
  for x := 0 to aSchemaDef.SimpleTypes.Count - 1 do
    AddSimpleType (nil, TypeDefs, aSchemaDef.SimpleTypes.Items [x]);
  for x := 0 to aSchemaDef.ComplexTypes.Count - 1 do
    AddComplexType (nil, TypeDefs, aSchemaDef.ComplexTypes.Items [x]);
  result := True;
end;

function TXsdDescr.FindTypeDef (aNameSpace, aName: String): TXsd;
var
  x: Integer;
begin
  result := nil;
  for x := 0 to TypeDefs.Count - 1 do
  begin
    if (TypeDefs.Xsds [x].NameSpace = aNameSpace)
    and (TypeDefs.Xsds [x].DataTypeName = aName)
    then begin
      result := TypeDefs.Xsds [x];
      exit;
    end;
  end;
end;

procedure TXsdDescr.LinkExtendedTypeDefs;
var
  x: Integer;
  xXsd: TXsd;
  xRefXsd: TXsd;
begin
  for x := 0 to xsdGlobalXsdList.Count - 1 do
  begin
    xXsd := xsdGlobalXsdList.Xsds [x];
    xXsd.TypeDef := FindTypeDef(xXsd.NameSpace, xXsd.DataTypeName);
  end;
  for x := 0 to TypeDefs.Count - 1 do
  begin
    xXsd := TypeDefs.Xsds [x];
    if (xXsd.DerivationMethod = 'ComplexExtension')
    or (xXsd.DerivationMethod = 'SimpleExtension')
    then begin
      xRefXsd := FindTypeDef (xXsd.BaseNameSpace, xXsd.BaseDataTypeName);
      if Assigned (xRefXsd) then
      begin
        xRefXsd.ExtendedByList.AddObject(xXsd.DataTypeName, xXsd);
      end;
    end;
  end;
end;

function TXsdDescr.LoadFromFile(aFileName: String;
  ErrorFound: TOnErrorEvent): Boolean;
var
  xFileNames: TStringList;
  procedure _Load (aFileName: String);
  var
    SchemaDef: IXMLSchemaDef;
    x: Integer;
    Includes: IXMLSchemaIncludes;
    Imports: IXMLSchemaImports;
  begin
    if xFileNames.Find(aFileName, x) = True then
      exit;
    xFileNames.Add(aFileName);
    SchemaDef := LoadXMLSchema(aFileName).SchemaDef;
    Imports := SchemaDef.SchemaImports;
    for x := 0 to Imports.Count - 1 do
    begin
      try
        _Load (ExpandRelativeFileName(aFileName, Imports.Items [x].SchemaLocation));
      except
        on E: Exception do
        begin
          if (MessageDlg ( 'Error parsing '
                         + Imports.Items [x].SchemaLocation
                         + ': '
                         + E.Message
                         + #$D#$A
                         + 'Continue'
                         , mtConfirmation
                         , [mbYes, mbNo]
                         , 0) <> mrYes
             ) then
             raise Exception.Create ('Aborted');
        end;
      end;
    end;
    Includes := SchemaDef.SchemaIncludes;
    for x := 0 to Includes.Count - 1 do
    begin
      try
        _Load (ExpandRelativeFileName(aFileName, Includes.Items [x].SchemaLocation));
      except
        on E: Exception do
        begin
          if (MessageDlg ( 'Error parsing '
                         + Includes.Items [x].SchemaLocation
                         + ': '
                         + E.Message
                         + #$D#$A
                         + 'Continue'
                         , mtConfirmation
                         , [mbYes, mbNo]
                         , 0) <> mrYes
             ) then
             raise Exception.Create ('Aborted');
        end;
      end;
    end;
    AddFromSchemaDef(SchemaDef);
  end;
begin
  result := False;
  xFileNames := TStringList.Create;
  xFileNames.Sorted := True;
  try
    FileName := aFileName;
{
    FileContents.LoadFromFile (aFileName);
}
    ElementDefs.ClearListOnly;
    Typedefs.ClearListOnly;
    xsdGlobalXsdList.Clear;
    _Load (aFileName);
    LinkExtendedTypedefs;
  finally
    xFileNames.Clear;
    xFileNames.Free;
  end;
end;

function Txsd.IsTypeDefEnabled: Boolean;
begin
  result := (Assigned (Typedef))
        and (Typedef.ExtendedByList.Count > 0);
end;

procedure TXsd.GenerateReport(aStringList: TStringList);
  function IndentString (x: Integer): String;
  begin
    result := '';
    while x > 0 do
    begin
      result := result + ' ';
      Dec (x);
    end;
  end;
  procedure _GenerateReport (aIndent: Integer; aXsd: TXsd);
  var
    xString: String;
    xSeparator: String;
    x: Integer;
  begin
    xString := IndentString (aIndent);
    xString := xString + aXsd.Name;
    xString := xString + #9;
    xString := xString + aXsd.minOccurs;
    xString := xString + #9;
    xString := xString + aXsd.maxOccurs;
    xString := xString + #9;
    xString := xString + aXsd.ContentModel;
    xString := xString + #9;
    xString := xString + aXsd.DataTypeName;
    xString := xString + #9;
    xString := xString + aXsd.BaseDataTypeName;
    xString := xString + #9;
    xString := xString + aXsd.MinLength;
    xString := xString + #9;
    xString := xString + aXsd.MaxLength;
    xString := xString + #9;
    xSeparator := '';
    for x := 0 to aXsd.Enumerations.Count - 1 do
    begin
      xString := xString + xSeparator + aXsd.Enumerations.Strings [x];
      xSeparator := ';';
    end;
    aStringList.Add(xString);
    for x := 0 to aXsd.ElementDefs.Count - 1 do
    begin
      _GenerateReport (aIndent + 2, aXsd.ElementDefs.Xsds [x]);
    end;
  end;
begin
  aStringList.Clear;
    aStringList.Add ( 'Tag'
                    + #9
                    + 'minOccurs'
                    + #9
                    + 'maxOccurs'
                    + #9
                    + 'ContentModel'
                    + #9
                    + 'DataType'
                    + #9
                    + 'BaseDataType'
                    + #9
                    + 'minLength'
                    + #9
                    + 'maxLength'
                    + #9
                    + 'Tokens'
                    );
  _GenerateReport (0, Self);
end;

procedure TXsd.GenerateCopyBook(aPrefix, aSuffix, a88Prefix, a88Suffix, aLevel: String; aStringList: TStringList);
  function IndentString (x: Integer): String;
  begin
    result := '';
    x := 3 * x - 2;
    while x > 0 do
    begin
      result := result + ' ';
      Dec (x);
    end;
  end;
  procedure _GenerateReport (aIndent: Integer; aXsd: TXsd);
  var
    xString: String;
    xSeparator: String;
    x: Integer;
  begin
    xString := IndentString (aIndent)
             + Format('0%d ', [aIndent])
             + aPrefix
             + aXsd.Name
             + aSuffix
             + aXsd.ccbPictureClause;
    if (aXsd.maxOccurs <> '1')
    and (aXsd.maxOccurs <> '01')
    then
      xString := xString + ' occurs ' + aXsd.maxOccurs + ' times' ;
    xString := xString + '.';
    aStringList.Add(xString);
    for x := 0 to aXsd.Enumerations.Count - 1 do
    begin
      xString := IndentString (aIndent + 1)
               + '88 '
               + a88Prefix
               + aXsd.Enumerations.Strings [x]
               + a88Suffix
               + ' value "'
               + aXsd.Enumerations.Strings [x]
               + '".'
               ;
      aStringList.Add(xString);
    end;
    for x := 0 to aXsd.ElementDefs.Count - 1 do
    begin
      _GenerateReport (aIndent + 1, aXsd.ElementDefs.Xsds [x]);
    end;
  end;
begin
  aStringList.Clear;
  _GenerateReport (StrToInt (aLevel), Self);
end;

function TXsd.ccbPictureClause: string;
var
  x, l: Integer;
begin
  result := '';
  if ElementDefs.Count > 0 then Exit;
  result := ' '
          + DataTypeName
          + ':'
          + BaseDataTypeName
          ;
  if BaseDataTypeName = 'string' then
  begin
    if MaxLength <> '' then
    begin
      result := ' pic x('
              + MaxLength
              + ')'
              ;
      Exit;
    end;
    if Enumerations.Count > 0 then
    begin
      l := 1;
      for x := 0 to Enumerations.Count - 1 do
      begin
        if system.Length (Enumerations.Strings [x]) > l then
          l := system.Length (Enumerations.Strings [x]);
      end;
      result := ' pic x('
              + IntToStr (l)
              + ')'
              ;
      Exit;
    end;
    if Pattern <> '' then
    begin
      result := ' pic BasedOnPattern('
              + Pattern
              + ')'
              ;
      Exit;
    end;
    Exit;
  end;
  if BaseDataTypeName = 'boolean' then
  begin
    result := ' pic x(1)'
              ;
    Exit;
  end;
end;

function TXsd.ccbOccursClause: String;
begin
  result := '';
  if (maxOccurs <> '1')
  and (maxOccurs <> '01')
  then
    result := ' occurs ' + maxOccurs + ' times' ;
end;

procedure TXsd.GenerateHtmlReport(aStringList: TStringList);
  function IndentString (x: Integer): String;
  begin
    result := '';
    while x > 0 do
    begin
      result := result + '.';
      Dec (x);
    end;
  end;
  procedure _GenerateReport (aIndent: Integer; aXsd: TXsd);
  var
    xString: String;
    xSeparator: String;
    x: Integer;
  begin
    xString := '<tr bgcolor="#FFFFFF" align="left">';
    xString := xString + '<td valign="top">';
    if aXsd.ElementDefs.Count > 0 then
      xString := xString + '<a name="' + aXsd.Name + '"/>';
    xString := xString + IndentString (aIndent) + aXsd.Name + '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.minOccurs + '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.maxOccurs + '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.ContentModel + '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.DataTypeName + '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.BaseDataTypeName + '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.MinLength + '&nbsp;</td>';
    xString := xString + '<td valign="top">' + aXsd.MaxLength + '&nbsp;</td>';
    xString := xString + '<td valign="top">';
    xSeparator := '';
    for x := 0 to aXsd.Enumerations.Count - 1 do
    begin
      xString := xString + xSeparator + aXsd.Enumerations.Strings [x];
      xSeparator := '<br>';
    end;
    xString := xString + '&nbsp;</td>';
    xString := xString + '<td valign="top">';
    xSeparator := '';
    for x := 0 to aXsd.Documentation.Count - 1 do
    begin
      xString := xString + xSeparator + aXsd.Documentation.Strings [x];
      xSeparator := '<br>';
    end;
    xString := xString + '&nbsp;</td>';
    xString := xString + '</tr>';
    aStringList.Add(xString);
    for x := 0 to aXsd.ElementDefs.Count - 1 do
    begin
      _GenerateReport (aIndent + 2, aXsd.ElementDefs.Xsds [x]);
    end;
  end;
begin
  aStringList.Clear;
  aStringList.Add('<html>');
  aStringList.Add('<a name="_home"/>');
  aStringList.Add('<p><table border="1">');
  aStringList.Add('<tr bgcolor="#FFFFFF" align="left">'
                 +'<td valign="top"><b>Tag&nbsp;</b></td>'
                 +'<td valign="top"><b>minOccurs&nbsp;</b></td>'
                 +'<td valign="top"><b>maxOccurs&nbsp;</b></td>'
                 +'<td valign="top"><b>ContentModel&nbsp;</b></td>'
                 +'<td valign="top"><b>DataType&nbsp;</b></td>'
                 +'<td valign="top"><b>BaseDataType&nbsp;</b></td>'
                 +'<td valign="top"><b>minLength&nbsp;</b></td>'
                 +'<td valign="top"><b>maxLength&nbsp;</b></td>'
                 +'<td valign="top"><b>Tokens&nbsp;</b></td>'
                 +'<td valign="top"><b>Annotation&nbsp;</b></td>'
                 +'</tr>');
  _GenerateReport (0, Self);
  aStringList.Add('</table>');
  aStringList.Add('<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>');
  aStringList.Add('<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>');
  aStringList.Add('Previous lines intentional left blank');
  aStringList.Add('</html>');
end;

{ TXsdDataTypeList }

procedure TXsdDataTypeList.Clear;
begin
  inherited;
end;

function TXsdDataTypeList.GetXsdDataType(Index: integer): TXsdDataType;
begin
  result := TXsdDataType (Objects [index]);
end;

{ TXsdAttrList }

procedure TXsdAttrList.Clear;
begin
  inherited;
end;

function TXsdAttrList.GetXsdAttr(Index: integer): TXsdAttr;
begin
  result := TXsdAttr (Objects [index]);
end;

{ TXsdDataType }

constructor TXsdDataType.Create;
begin
  _Error := False;
  Enumerations := TStringList.Create;
  Enumerations.Sorted := True;
  Enumerations.Duplicates := dupIgnore;
  Enumerations.CaseSensitive := True;
  Documentation := TStringList.Create;
  ExtendedByList := TXsdDataTypeList.Create;
  ElementDefs := TXsdList.Create;
  AttributeDefs := TXsdAttrList.Create;
end;

destructor TXsdDataType.Destroy;
begin
  Enumerations.Clear;
  Enumerations.Free;
  Documentation.Clear;
  Documentation.Free;
  ExtendedByList.Clear;
  ExtendedByList.Free;
  ElementDefs.ClearListOnly;
  ElementDefs.Free;
  AttributeDefs.Clear;
  AttributeDefs.Free;
  inherited;
end;


function TXsdDataType.getNSAttributes: String;
begin
  result := xsdGenerateNameSpaceAttributes;
end;

function TXsdDataType.getNSPrefix: String;
begin
  result := xsdNameSpacePrefix (NameSpace);
end;

function TXsdDataType.getSchemaAsText: String;
var
  sl: TStringList;
  x: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Add('<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified" attributeFormDefault="unqualified">');
    sl.Add('	<xs:element name="tstName">');
    sl.Add('		<xs:simpleType>');
    sl.Add('			<xs:restriction base=xs:"' + BaseDataTypeName + '">');
    if Length <> '' then
      sl.Add('				<xs:Length value="' + Length + '"/>');
    if MinLength <> '' then
      sl.Add('				<xs:minLength value="' + MinLength + '"/>');
    if MinInclusive <> '' then
      sl.Add('				<xs:MinInclusive value="' + MinInclusive + '"/>');
    if MinExclusive <> '' then
      sl.Add('				<xs:MinExclusive value="' + MinExclusive + '"/>');
    if MaxLength <> '' then
      sl.Add('				<xs:maxLength value="' + MaxLength + '"/>');
    if MaxInclusive <> '' then
      sl.Add('				<xs:MaxInclusive value="' + MaxInclusive + '"/>');
    if MaxExclusive <> '' then
      sl.Add('				<xs:MaxExclusive value="' + MaxExclusive + '"/>');
    if TotalDigits <> '' then
      sl.Add('				<xs:TotalDigits value="' + TotalDigits + '"/>');
    if FractionalDigits <> '' then
      sl.Add('				<xs:FractionalDigits value="' + FractionalDigits + '"/>');
    if Whitespace <> '' then
      sl.Add('				<xs:Whitespace value="' + Whitespace + '"/>');
    if Pattern <> '' then
      sl.Add('				<xs:pattern value="' + Pattern + '"/>');
    for x := 0 to Enumerations.Count - 1 do
      sl.Add('				<xs:enumeration value="' + Enumerations.Strings[x] + '"/>');
    sl.Add('			</xs:restriction>');
    sl.Add('		</xs:simpleType>');
    sl.Add('	</xs:element>');
    sl.Add('</xs:schema>');
    result := sl.Text;
  finally
    sl.Free;
  end;
end;

function TXsdDataType.getXsiNameSpaceAttribute: String;
begin
  result := xsdGenerateXsiNameSpaceAttribute;
end;

function TXsdDataType.IsValidValue(aValue: String;
  var aMessage: String): Boolean;
var
  x: Integer;
  LCDataType: String;
  ValueAsDate: TDateTime;
  ValueAsDateTime: TDateTime;
  ValueAsInteger: Int64;
  ValueAsDecimal: Extended;
  RefDate: TDateTime;
  RefDateTime: TDateTime;
  RefInteger: Int64;
  RefDecimal: Extended;
  function StringToDate (aString: String): TDateTime;
  var
    xYear: Integer;
    xMonth: Integer;
    xDay: Integer;
  begin
    xYear := StrToInt (Copy (aString, 1, 4));
    xMonth := StrToInt (Copy (aString, 6, 2));
    xDay := StrToInt (Copy (aString, 9, 2));
    StringToDate := EncodeDate (xYear, xMonth, xDay);
  end;

  function StringToDateTime (aString: String): TDateTime;
  var
    xYear: Integer;
    xMonth: Integer;
    xDay: Integer;
    xHour: Integer;
    xMinute: Integer;
    xSecond: Integer;
  begin
    xYear := StrToInt (Copy (aString, 1, 4));
    xMonth := StrToInt (Copy (aString, 6, 2));
    xDay := StrToInt (Copy (aString, 9, 2));
    xHour := StrToInt (Copy (aString, 12, 2));
    xMinute := StrToInt (Copy (aString, 15, 2));
    xSecond := StrToInt (Copy (aString, 18, 2));
    if system.Length (aString) > 20 then
    begin
{
      xSecondFraction := StrToInt (Copy ( aString
                                        , 21
                                        , system.Length (aString) - 20
                                        )
                                  )
                       / Power (10, system.Length (aString) - 20)
                       ;
}
    end;
    StringToDateTime := EncodeDateTime ( xYear
                                       , xMonth
                                       , xDay
                                       , xHour
                                       , xMinute
                                       , xSecond
                                       , 0
                                       );
{
    StringToDateTime := StringToDateTime + Double(xSecondFraction / (24 * 60 * 60));
}
  end;

  function CheckDate: Boolean;
  begin
    result := MatchPattern ('\d{4}\-\d{2}\-\d{2}', aValue);
    if not result then
    begin
      aMessage := 'Value "'
                + aValue
                + '" not according to date pattern'
                ;
      exit;
    end;
    try
      ValueAsDate := StringToDate (aValue);
    except
      result := False;
      aMessage := 'Value "'
                + aValue
                + '" not a valid date';
      exit;
    end;
    if MaxInclusive <> '' then
    begin
      RefDate := StringToDate (MaxInclusive);
      if ValueAsDate > RefDate then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" greater than "'
                  + MaxInclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckDate MaxInclusive}
    if MaxExclusive <> '' then
    begin
      RefDate := StringToDate (MaxExclusive);
      if ValueAsDate >= RefDate then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" greater than or equals to "'
                  + MaxExclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckDate MaxExclusive}
    if MinInclusive <> '' then
    begin
      RefDate := StringToDate (MinInclusive);
      if ValueAsDate < RefDate then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" less than "'
                  + MinInclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckDate MinInclusive}
    if MinExclusive <> '' then
    begin
      RefDate := StringToDate (MinExclusive);
      if ValueAsDate <= RefDate then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" less than or equals to "'
                  + MinExclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckDate MinExclusive}
  end; { CheckDate }

  function CheckDateTime: Boolean;
  var
    xTimePattern: String;
  begin {2002-10-10T12:00:00.906+00:00}
    xTimePattern := '\d{4}\-\d{2}\-\d{2}T\d{2}:\d{2}:\d{2}(\.\d*)?(Z|([+-]\d{2}:\d{2}))?';
    result := MatchPattern (xTimePattern, aValue);
    if not result then
    begin
      aMessage := 'Value "'
                + aValue
                + '" not according to datetime pattern"'
                + xTimePattern
                + '"'
                ;
      exit;
    end;
    try
      ValueAsDateTime := StringToDateTime (aValue);
    except
      result := False;
      aMessage := 'Value "'
                + aValue
                + '" not a valid datetime';
      exit;
    end;
    if MaxInclusive <> '' then
    begin
      RefDateTime := StringToDateTime (MaxInclusive);
      if ValueAsDateTime > RefDateTime then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" greater than "'
                  + MaxInclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckDateTime MaxInclusive}
    if MaxExclusive <> '' then
    begin
      RefDateTime := StringToDateTime (MaxExclusive);
      if ValueAsDateTime >= RefDateTime then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" greater than or equals to "'
                  + MaxExclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckDateTime MaxExclusive}
    if MinInclusive <> '' then
    begin
      RefDateTime := StringToDateTime (MinInclusive);
      if ValueAsDateTime < RefDateTime then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" less than "'
                  + MinInclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckDateTime MinInclusive}
    if MinExclusive <> '' then
    begin
      RefDateTime := StringToDateTime (MinExclusive);
      if ValueAsDateTime <= RefDateTime then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" less than or equals to "'
                  + MinExclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckDateTime MinExclusive}
  end; { CheckDateTime }

  function CheckInteger(aDataTypeName: String; MinIncl: Int64; MaxIncl: Int64): Boolean;
  begin
    result := MatchPattern ('[\+\-]?[0-9]+', aValue);
    if not result then
    begin
      aMessage := 'Value "'
                + aValue
                + '" not according to integer pattern"'
                + '[\+\-]?[0-9]+'
                + '"'
                ;
      exit;
    end;
    try
      ValueAsInteger := StrToInt64 (aValue);
      result := True;
    except
      ValueAsInteger := 0;
    end;
    if not result then
      exit;
    if MaxInclusive <> '' then
    begin
      RefInteger := StrToInt64 (MaxInclusive);
      if ValueAsInteger > RefInteger then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" greater than "'
                  + MaxInclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckInteger MaxInclusive}
    if MaxExclusive <> '' then
    begin
      RefInteger := StrToInt64 (MaxExclusive);
      if ValueAsInteger >= RefInteger then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" greater than or equals to "'
                  + MaxExclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckInteger MaxExclusive}
    if MinInclusive <> '' then
    begin
      RefInteger := StrToInt64 (MinInclusive);
      if ValueAsInteger < RefInteger then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" less than "'
                  + MinInclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckInteger MinInclusive}
    if MinExclusive <> '' then
    begin
      RefInteger := StrToInt64 (MinExclusive);
      if ValueAsInteger <= RefInteger then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" less than or equals to "'
                  + MinExclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckInteger MinExclusive}
    if (MinIncl <> 0)
    or (MaxIncl <> 0) then
    begin
      result := (ValueAsInteger >= MinIncl)
            and (ValueAsInteger <= MaxIncl);
      if not result then
        aMessage := 'Value "'
                  + aValue
                  + '" not between '
                  + IntToStr(MinIncl)
                  + ' and '
                  + IntToStr(MaxIncl)
                  + ' inclusive'
                  ;
    end;
  end; {CheckInteger}

  function CheckDecimal(aDataTypeName: String; MinIncl: Extended; MaxIncl: Extended): Boolean;
  var
    xTotalDigits: Integer;
    xFractionalDigits: Integer;
    xDotPos: Integer;
    xDecimalSeparator: Char;
  begin
    result := MatchPattern ('[\+\-]?[0-9]*(\.[0-9]*)?', aValue);
    if not result then
    begin
      aMessage := 'Value "'
                + aValue
                + '" not according to decimal pattern"'
                + '[\+\-]?[0-9]*(\.[0-9]*)?'
                + '"'
                ;
      exit;
    end;
    xDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    try
      try
        ValueAsDecimal := StrToFloat (aValue);
        result := True;
      except
        ValueAsDecimal := 0;
      end;
    finally
      DecimalSeparator := xDecimalSeparator;
    end;
    if not result then
      exit;
    if MaxInclusive <> '' then
    begin
      RefDecimal := StrToFloat (MaxInclusive);
      if ValueAsDecimal > RefDecimal then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" greater than "'
                  + MaxInclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckDecimal MaxInclusive}
    if MaxExclusive <> '' then
    begin
      RefDecimal := StrToFloat (MaxExclusive);
      if ValueAsDecimal >= RefDecimal then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" greater than or equals to "'
                  + MaxExclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckDecimal MaxExclusive}
    if MinInclusive <> '' then
    begin
      RefDecimal := StrToFloat (MinInclusive);
      if ValueAsDecimal < RefDecimal then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" less than "'
                  + MinInclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckDecimal MinInclusive}
    if MinExclusive <> '' then
    begin
      RefDecimal := StrToFloat (MinExclusive);
      if ValueAsDecimal <= RefDecimal then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" less than or equals to "'
                  + MinExclusive
                  + '"'
                  ;
        exit;
      end;
    end; { CheckDecimal MinExclusive}
    if TotalDigits <> '' then
    begin
      xTotalDigits := system.Length (aValue);
      x := 1;
      { First subtract leading sign and zeroes
      }
      while (x <= system.Length (aValue))
        and (    (aValue [x] = '+')
              or (aValue [x] = '-')
              or (aValue [x] = '0')
            ) do
      begin
        Dec (xTotalDigits);
        Inc (x);
      end;
      { Determine if there's a dot in the value string
      }
      while (x <= system.Length (aValue))
        and (aValue [x] <> '.') do
      begin
        Inc (x);
      end;
      if (x <= system.Length (aValue)) then
      begin
        {there's a dot in the string, so we're gonna
         subtract 1 from digits and subtract trailing zeroes
        }
        x := system.Length (aValue);
        Dec (xTotalDigits);
        while aValue [x] = '0' do
        begin
          Dec (xTotalDigits);
          Dec (x);
        end;
      end;
      if xTotalDigits > StrToInt (TotalDigits) then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" has more then "'
                  + TotalDigits
                  + '" digits'
                  ;
        exit;
      end;
    end; { CheckDecimal TotalDigits}

    if FractionalDigits <> '' then
    begin
      xFractionalDigits := 0;
      xDotPos := Pos ('.', aValue);
      if xDotPos > 0 then
      begin
        x := xDotPos + 1;
        while (x <= system.Length (aValue)) do
        begin
          if aValue [x] <> '0' then
            xFractionalDigits := x - xDotPos;
          Inc (x);
        end;
      end; {if dotpos <> 0}
      if xFractionalDigits > StrToInt (FractionalDigits) then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" has more then "'
                  + FractionalDigits
                  + '" fractional digits'
                  ;
        exit;
      end;
    end; { CheckDecimal FractionalDigits}

    if (MinIncl <> 0)
    or (MaxIncl <> 0) then
    begin
      result := (ValueAsDecimal >= MinIncl)
            and (ValueAsDecimal <= MaxIncl);
      if not result then
        aMessage := 'Value "'
                  + aValue
                  + '" not between '
                  + FloatToStr(MinIncl)
                  + ' and '
                  + FloatToStr(MaxIncl)
                  + ' inclusive'
                  ;
    end;
  end; {CheckDecimal}

begin {IsValidValue(aValue: String;}
  aMessage := SchemaAsText;
  aMessage := '';
  LCDataType := LowerCase (BaseDataTypeName);
  if Length <> '' then
  begin
    if system.Length (aValue) <> StrToInt (Length) then
    begin
      result := False;
      aMessage := 'Length not according to schema "'
                + Length
                + '"'
                ;
      exit;
    end;
  end;
  if MinLength <> '' then
  begin
    if system.Length (aValue) < StrToInt (MinLength) then
    begin
      result := False;
      aMessage := 'Length "'
                + IntToStr (system.Length (aValue))
                + '"less then needed according to schema "'
                + MinLength
                + '"'
                ;
      exit;
    end;
  end;
  if MaxLength <> '' then
  begin
    if system.Length (aValue) > StrToInt (MaxLength) then
    begin
      result := False;
      aMessage := 'Length "'
                + IntToStr (system.Length (aValue))
                + '" exceeds maximum according to schema "'
                + MaxLength
                + '"'
                ;
      exit;
    end;
  end;
  if Pattern <> '' then
  begin
    result := MatchPattern (Pattern, aValue);
    if not result then
    begin
      aMessage := 'Value "'
                + aValue
                + '" does not match schema pattern "'
                + Pattern
                + '"'
                ;
      exit;
    end;
  end;
  if Enumerations.Count > 0 then
  begin
    result := Enumerations.Find(aValue, x);
    if not result then
    begin
      aMessage := 'Value not found in enumeration';
      exit;
    end;
  end;

  result := False;
  aMessage := 'Check for validity of '
            + BaseDataTypeName
            + ' not yet implemented';

  if LCDataType = 'anytype' then
  begin
    result := True;
    exit;
  end;

  if LCDataType = 'base64binary' then
  begin
    result := MatchPattern ('([0-9a-zA-Z\+\/]{4})+', aValue);
    if not result then
    begin
      aMessage := 'Value "'
                + aValue
                + '" not according to Base64Binary pattern "'
                + '([0-9a-zA-Z\+\/]{4})+'
                + '"'
                ;
      exit;
    end;
  end;

  if LCDataType = 'boolean' then
  begin
    result := (aValue = '1')
           or (aValue = 'true')
           or (aValue = '0')
           or (aValue = 'false')
            ;
    if not result then
      aMessage := 'Value "'
                + aValue
                + '" is not a Boolean';
    exit;
  end;

  if LCDataType = 'byte' then
  begin
    result := CheckInteger ('a byte', Int64 (-128), Int64 (127));
    exit;
  end;

  if LCDataType = 'date' then
  begin
    result := CheckDate;
    exit;
  end;

  if LCDataType = 'datetime' then
  begin
    result := CheckDateTime;
    exit;
  end;

  if LCDataType = 'decimal' then
  begin
    result := CheckDecimal ('a decimal', 0, 0);
    exit;
  end;

  if LCDataType = 'double' then
  begin
    if (aValue = 'INF')
    or (aValue = '-INF')
    or (aValue = 'NaN') then
    begin
      result := True;
      if (minExclusive <> '')
      or (minInclusive <> '')
      or (maxExclusive <> '')
      or (maxInclusive <> '')
      then
      begin
        result := False;
        aMessage := 'Check for rangevalues for '
                  + BaseDataTypeName
                  + ' not yet implemented for INF, -INF or NaN literals';
      end;
      exit;
    end;
    result := CheckDecimal ('a double', 0, 0);
    exit;
  end;

{
  <Duration></Duration>
  <Float></Float>
  <gDay></gDay>
  <gMont></gMont>
  <gMontDay></gMontDay>
  <gYear></gYear>
  <gYearMonth></gYearMonth>
  <HexBinary></HexBinary>
  <ID></ID>
}
  if LCDataType = 'int' then
  begin
    result := CheckInteger ('an int', StrToInt64 ('-2147483648'), 2147483647);
    exit;
  end;

  if LCDataType = 'integer' then
  begin
    result := CheckInteger ('an integer', 0, 0);
    exit;
  end;

{
  <Language></Language>
}
  if LCDataType = 'long' then
  begin
    result := CheckInteger ('a long', StrToInt64 ('-9223372036854775808'), StrToInt64 ('9223372036854775807'));
    exit;
  end;

{
  <Name></Name>
  <NCName></NCName>
}
  if LCDataType = 'negativeinteger' then
  begin
    result := CheckInteger ('an negative integer', 0, 0);
    if result then
    begin
      if aValue [1] <> '-' then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" not less then zero'
                  ;
      end;
    end;
    exit;
  end;

  if LCDataType = 'nmtoken' then
  begin
    result := True;
    aMessage := '';
    exit;
  end;

{
  <NmTokens></NmTokens>
}
  if LCDataType = 'nonnegativeinteger' then
  begin
    result := CheckInteger ('an nonnegative integer', 0, 0);
    if result then
    begin
      if aValue [1] = '-' then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" less then zero'
                  ;
      end;
    end;
    exit;
  end;

  if LCDataType = 'nonpositiveinteger' then
  begin
    result := CheckInteger ('an nonpositive integer', 0, 0);
    if result then
    begin
      if (aValue [1] = '-')
      or (ValueAsInteger = 0) then
      else
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" is greater than zero'
                  ;
      end;
    end;
    exit;
  end;

{
  <NormalizedString></NormalizedString>
  <Notation></Notation>
}
  if LCDataType = 'positiveinteger' then
  begin
    result := CheckInteger ('an positive integer', 0, 0);
    if result then
    begin
      if (aValue [1] = '-')
      or (ValueAsInteger = 0) then
      begin
        result := False;
        aMessage := 'Value "'
                  + aValue
                  + '" not positive'
                  ;
      end;
    end;
    exit;
  end;

{
  <QName></QName>
}
  if LCDataType = 'short' then
  begin
    result := CheckInteger ('an short', -32768, 327677);
    exit;
  end;

  if LCDataType = 'string' then
  begin
    result := True;
    aMessage := '';
    exit;
  end;
{
  <Time></Time>
  <Token></Token>
}
  if LCDataType = 'unsignedbyte' then
  begin
    result := CheckInteger ('a unsigned byte', 0, 255);
    exit;
  end;

  if LCDataType = 'unsignedint' then
  begin
    result := CheckInteger ('a unsigned int', 0, 4294967295);
    exit;
  end;

  if LCDataType = 'unsignedlong' then
  begin
    result := CheckInteger ('a unsigned long', 0, Int64 (High (LongWord)));
    exit;
  end;

  if LCDataType = 'unsignedshort' then
  begin
    result := CheckInteger ('a unsigned short', 0, 65535);
    exit;
  end;

{
      DataTypeName: String;
      BaseDataTypeName: String;
      Whitespace: String;
      MaxInclusive: String;
      MaxExclusive: String;
      MinInclusive: String;
      MinExclusive: String;
      Numeric: String;
      TotalDigits: String;
      FractionalDigits: String;
}
end;

{ TXsdAttr }

constructor TXsdAttr.Create;
begin
  inherited;
end;

destructor TXsdAttr.Destroy;
begin
  inherited;
end;

{ TXsdDescrList }

procedure TXsdDescrList.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    XsdDescrs [x].Free;
  inherited;
end;

function TXsdDescrList.GetXsdDescr(Index: integer): TXsdDescr;
begin
  result := TXsdDescr (Objects [index]);
end;

function TXsdDescrList.FindTypeDef(aNameSpace: String; aName: String): TXsd;
var
  xd: Integer;
  xt: Integer;
begin
  result := nil;
  for xd := 0 to Count - 1 do
  begin
    for xt := 0 to XsdDescrs [xd].TypeDefs.Count - 1 do
    begin
      if {(XsdDescrs [xd].TypeDefs.Xsds [xt].NameSpace = aNameSpace)
      and }(XsdDescrs [xd].TypeDefs.Xsds [xt].Name = aName) then
      begin
        result := XsdDescrs [xd].TypeDefs.Xsds [xt];
        exit;
      end;
    end;
  end;
end;

function TXsdDescr.xsdFindTypeDef(aNameSpace, aName: String): TXsdDataType;
var
  x: Integer;
begin
  result := nil;
  for x := 0 to xsdGlobalXsdList.Count - 1 do
  begin
    if (xsdGlobalXsdList.Xsds [x].NameSpace = aNameSpace)
    and (xsdGlobalXsdList.Xsds [x].DataTypeName = aName)
    then begin
      result := xsdGlobalXsdList.Xsds [x];
      exit;
    end;
  end;
end;

function TXsdDescr.xsdGetReference(aNameSpace, aName, aMinOccurs, aMaxOccurs,
  aDataType: String; var aXsd: TXsd): Boolean;
var
  x: Integer;
begin
  if xsdGlobalXsdList.Find( aName
                          + ':'
                          + aNameSpace
                          + ':'
                          + aMinOccurs
                          + ':'
                          + aMaxOccurs
                          + ':'
                          + aDataType
                          , x
                          ) then
  begin
    result := False;
    aXsd := xsdGlobalXsdList.Xsds [x];
  end
  else
  begin
    result := True;
    aXsd := TXsd.Create;
    aXsd.xsdDescr := self;
    aXsd.NameSpace := aNameSpace;
    aXsd.Name := aName;
    xsdGlobalXsdList.AddObject ( aName
                               + ':'
                               + aNameSpace
                               + ':'
                               + aMinOccurs
                               + ':'
                               + aMaxOccurs
                               + ':'
                               + aDataType
                               , aXsd
                               );
  end;
end;

procedure xsdAddTargetNameSpace (aTargetNameSpace: String);
var
  x: Integer;
begin
  if not xsdGlobalNameSpaceList.Find(aTargetNameSpace, x) then
    xsdGlobalNameSpaceList.Add(aTargetNameSpace);
end;

function xsdGenerateNameSpaceAttributes: String;
var
  x: Integer;
begin
  result := '';
  for x := 0 to xsdGlobalNameSpaceList.Count - 1 do
  begin
    result := result
            + ' '
            + 'xmlns:'
            + xsdNameSpacePrefix (xsdGlobalNameSpaceList.Strings [x])
            + '="'
            + xsdGlobalNameSpaceList.Strings [x]
            + '"'
            ;
  end;
end;

function xsdGenerateXsiNameSpaceAttribute: String;
begin
  if not xsiGenerated then
  begin
    xsiGenerated := True;
    result := ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"';
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


function xsdNameSpacePrefix (aNameSpace: String): String;
var
  x: Integer;
begin
  if aNameSpace = 'http://www.w3.org/2001/XMLSchema' then
    result := 'xsd'
  else
  begin
    if xsdGlobalNameSpaceList.Find(aNameSpace, x) then
      result := 'ns' + IntToStr (x + 1)
    else
      result := 'ns';
  end;
end;

initialization
  xsdMaxDepthBillOfMaterials := 2;
  xsdGlobalNameSpaceList := TXsdList.Create;
  xsdGlobalNameSpaceList.Sorted := True;
  xsdGlobalNameSpaceList.CaseSensitive := True;

finalization
  if Assigned (xsdGlobalNameSpaceList) then
  begin
    xsdGlobalNameSpaceList.Clear;
    FreeAndNil(xsdGlobalNameSpaceList);
  end;
end.
