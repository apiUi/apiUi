unit XMLValidate;

// Requirements ----------------------------------------------------------------
//
// MSXML 6.0
// http://www.microsoft.com
//
// -----------------------------------------------------------------------------

interface

uses
  SysUtils, XMLIntf, xmldom, XMLSchema;

type
  EValidateXMLError = class(Exception)
  private
    FErrorCode: Integer;
    FLineNo: Integer;
    FReason: string;
  public
    constructor Create(AErrorCode, aLineNo, aPosNo: Integer; const AReason: string);
    property ErrorCode: Integer read FErrorCode;
    property LineNo: Integer read FLineNo;
    property Reason: string read FReason;
  end;

procedure ValidateXMLDoc(const Doc: IDOMDocument; const SchemaLocation, SchemaNS: WideString); overload;
procedure ValidateXMLDoc(const Doc: XMLIntf.IXMLDocument; const SchemaLocation, SchemaNS: WideString); overload;
procedure ValidateXMLDoc(const Doc: IDOMDocument; const Schema: IXMLSchemaDoc); overload;
procedure ValidateXMLDoc(const Doc: XMLIntf.IXMLDocument; const Schema: IXMLSchemaDoc); overload;

implementation

uses
  Windows, ComObj, msxmldom, MSXML2_TLB;

resourcestring
//  RsValidateError = 'Validate XML Error (%.8x) at [%d, %d], Reason: %s';
  RsValidateError = 'Validate XML Error (%.8x), Reason: %s';

{ EValidateXMLError }

constructor EValidateXMLError.Create(AErrorCode, aLineNo, aPosNo: Integer; const AReason: string);
begin
  inherited CreateResFmt(@RsValidateError, [AErrorCode, AReason]);
  FErrorCode := AErrorCode;
  FLineNo := aLineNo;
  FReason := AReason;
end;

{ Utility routines }

function DOMToMSDom(const Doc: IDOMDocument): IXMLDOMDocument2;
begin
  Result := ((Doc as IXMLDOMNodeRef).GetXMLDOMNode as IXMLDOMDocument2);
end;

function LoadMSDom(const FileName: WideString): IXMLDOMDocument2;
begin
//  Result := CoDOMDocument60.Create;
  Result := CoDOMDocument.Create;
  Result.async := False;
  Result.resolveExternals := False;
  Result.validateOnParse := True;
  Result.load(FileName);
end;

{ Validate }

procedure InternalValidateXMLDoc(const Doc: IDOMDocument; const SchemaDoc: IXMLDOMDocument2; const SchemaNS: WideString);
var
  MsxmlDoc: IXMLDOMDocument2;
  SchemaCache: IXMLDOMSchemaCollection;
  Error: IXMLDOMParseError;
begin
  MsxmlDoc := DOMToMSDom(Doc);
  try
    SchemaCache := CoXMLSchemaCache60.Create;
  except
    SchemaCache := CoXMLSchemaCache40.Create;
  end;
  SchemaCache.add(SchemaNS, SchemaDoc);
  MsxmlDoc.schemas := SchemaCache;
  Error := MsxmlDoc.validate;
  if Error.errorCode <> S_OK then
    raise EValidateXMLError.Create(Error.errorCode, Error.line, Error.linepos, Error.reason);
end;

procedure ValidateXMLDoc(const Doc: IDOMDocument; const SchemaLocation, SchemaNS: WideString);
begin
  InternalValidateXMLDoc(Doc, LoadMSDom(SchemaLocation), SchemaNS);
end;

procedure ValidateXMLDoc(const Doc: XMLIntf.IXMLDocument; const SchemaLocation, SchemaNS: WideString);
begin
  InternalValidateXMLDoc(Doc.DOMDocument, LoadMSDom(SchemaLocation), SchemaNS);
end;

procedure ValidateXMLDoc(const Doc: IDOMDocument; const Schema: IXMLSchemaDoc);
begin
  InternalValidateXMLDoc(Doc, DOMToMSDom(Schema.DOMDocument), '');
end;

procedure ValidateXMLDoc(const Doc: XMLIntf.IXMLDocument; const Schema: IXMLSchemaDoc);
begin
  InternalValidateXMLDoc(Doc.DOMDocument, DOMToMSDom(Schema.DOMDocument), '');
end;

end.
