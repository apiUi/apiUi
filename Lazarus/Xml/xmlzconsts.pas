unit xmlzConsts;

{$mode delphi}{$H+}

interface

uses
  IdSSLOpenSSL
  ;

const

  scSoapUseEncoded = 'encoded';
  scSoapUseLiteral = 'literal';
  scXMLSchemaURI = 'http://www.w3.org/2001/XMLSchema';
  scXMLSchemaInstanceURI = 'http://www.w3.org/2001/XMLSchema-instance';
  scWsdlNameSpace = 'http://schemas.xmlsoap.org/wsdl/';
  scSoapEnvNameSpaceV1_1 = 'http://schemas.xmlsoap.org/soap/envelope/';
  scSoapEnvNameSpaceV1_2 = 'http://www.w3.org/2002/12/soap-envelope';

{ tags }
  tagAbstract = 'abstract';
  tagAddress = 'address';
  tagAll = 'all';
  tagAnnotation = 'annotation';
  tagAny = 'any';
  tagAppInfo = 'appinfo';
  tagAttribute = 'attribute';
  tagAttributeFormDefault = 'attributeFormDefault';
  tagAttributeGroup = 'attributeGroup';
  tagBase = 'base';
  tagBase64 = 'base64';
  tagBinding = 'binding';
  tagBlockDefault = 'blockDefault';
  tagBody = 'body';
  tagChoice = 'choice';
  tagComplexContent = 'complexContent';
  tagComplexType = 'complexType';
  tagDefault = 'default';
  tagDefinitions = 'definitions';
  tagDerivedBy = 'derivedBy';
  tagDocumentation = 'documentation';
  tagElement = 'element';
  tagElementFormDefault = 'elementFormDefault';
  tagEncodingStyle = 'encodingStyle';
  tagExtension = 'extension';
  tagFault = 'fault';
  tagFinal = 'final';
  tagFinalDefault = 'finalDefault';
  tagFixed = 'fixed';
  tagForm = 'form';
  tagGroup = 'group';
  tagHeader = 'header';
  tagHex = 'hex';
  tagImport = 'import';
  tagInclude = 'include';
  tagInput = 'input';
  tagItemType = 'itemType';
  tagList = 'list';
  tagLocation = 'location';
  tagMaxOccurs = 'maxOccurs';
  tagMemberTypes = 'memberTypes';
  tagMessage = 'message';
  tagMinOccurs = 'minOccurs';
  tagMixed = 'mixed';
  tagName = 'name';
  tagNotation = 'notation';
  tagOperation = 'operation';
  tagOptional = 'optional';
  tagOutput = 'output';
  tagPart = 'part';
  tagParts = 'parts';
  tagPort = 'port';
  tagPortType = 'portType';
  tagPublic = 'public';
  tagProhibited = 'prohibited';
  tagQualified = 'qualified';
  tagRef = 'ref';
  tagRequired = 'required';
  tagRestriction = 'restriction';
  tagSchema = 'schema';
  tagSchemaLocation = 'schemaLocation';
  tagSequence = 'sequence';
  tagService = 'service';
  tagSimpleContent = 'simpleContent';
  tagSimpleType = 'simpleType';
  tagSoapAction = 'soapAction';
  tagSource = 'source';
  tagStyle = 'style';
  tagSystem = 'system';
  tagTargetNamespace = 'targetNamespace';
  tagTransport = 'transport';
  tagType = 'type';
  tagTypes = 'types';
  tagUnbounded = 'unbounded';
  tagUnqualified = 'unqualified';
  tagUse = 'use';
  tagUnion = 'union';
  tagValue = 'value';
  tagVersion = 'version';

{ built-in primative dataTypes }
  xsdString = 'string';
  xsdBoolean = 'boolean';
  xsdDecimal = 'decimal';
  xsdFloat = 'float';
  xsdDouble = 'double';
  xsdDuration = 'duration';
  xsdDateTime = 'dateTime';
  xsdTime = 'time';
  xsdDate = 'date';
  xsdGYearMonth = 'gYearMonth';
  xsdGYear = 'gYear';
  xsdGMonthDay = 'gMonthDay';
  xsdGDay = 'gDay';
  xsdGMonth = 'gMonth';
  xsdHexBinary = 'hexBinary';
  xsdBase64Binary = 'base64Binary';
  xsdAnyUri = 'anyURI';
  xsdQName = 'QName';
  xsdNOTATION = 'NOTATION';

{ Built-in Data Types (Derived) }

  xsdNormalizedString = 'normalizedString';
  xsdToken = 'token';
  xsdLanguage = 'language';
  xsdIDREFS = 'IDREFS';
  xsdENTITIES = 'ENTITIES';
  xsdNMTOKEN = 'NMTOKEN';
  xsdNMTOKENS = 'NMTOKENS';
  xsdName = 'Name';
  xsdNCName = 'NCName';
  xsdID = 'ID';
  xsdIDREF = 'IDREF';
  xsdENTITY = 'ENTITY';
  xsdInteger = 'integer';
  xsdNonPositiveInteger = 'nonPositiveInteger';
  xsdNegativeInteger = 'negativeInteger';
  xsdLong = 'long';
  xsdInt = 'int';
  xsdShort = 'short';
  xsdByte = 'byte';
  xsdNonNegativeInteger = 'nonNegativeInteger';
  xsdUnsignedLong = 'unsignedLong';
  xsdUnsignedInt = 'unsignedInt';
  xsdUnsignedShort = 'unsignedShort';
  xsdUnsignedByte = 'unsignedByte';
  xsdPositiveInteger = 'positiveInteger';

{ Legacy Built-in Data Types (pre 2001) }

  xsdTimeDuration = 'timeDuration';             { duration }
  xsdBinary = 'binary';                         { hexBinary }
  xsdUriReference = 'uriReference';             { anyURI }
  xsdTimeInstant = 'timeInstant';               { dateTime }
  xsdRecurringDate= 'recurringDate';            { gMonthDay }
  xsdRecurringDay = 'recurringDay';             { gDay }
  xsdMonth = 'month';                           { gMonth }
  xsdYear = 'year';                             { gYear }
  xsdTimePeriod = 'timePeriod';                 { removed }
  xsdRecurringDuration = 'recurringDuration';   { removed }
  xsdCentury = 'century';                       { removed }

  xsdanySimpleType = 'anySimpleType';
  xsdanyType = 'anyType';

{ facets }

  fctOrdered = 'ordered';
  fctBounded = 'bounded';
  fctCardinality = 'cardinality';
  fctNumeric = 'numeric';

  fctLength = 'length';
  fctMinLength = 'minLength';
  fctMaxLength = 'maxLength';
  fctPattern = 'pattern';
  fctEnumeration = 'enumeration';
  fctMaxInclusive = 'maxInclusive';
  fctMaxExclusive = 'maxExclusive';
  fctMinInclusive = 'minInclusive';
  fctMinExclusive = 'minExclusive';
  fctWhitespace = 'whiteSpace';
  fctTotalDigits = 'totalDigits';
  fctFractionalDigits = 'fractionDigits';

  builtInTypeNames: array[0..45] of String = (xsdString, xsdBoolean,
    xsdDecimal, xsdFloat, xsdDouble, xsdDuration, xsdDateTime, xsdTime,
    xsdDate, xsdGYearMonth, xsdGYear, xsdGMonthDay, xsdGDay, xsdGMonth,
    xsdHexBinary, xsdBase64Binary, xsdAnyUri, xsdQName, xsdNOTATION,
    xsdNormalizedString, xsdToken, xsdLanguage, xsdIDREFS, xsdENTITIES,
    xsdNMTOKEN, xsdNMTOKENS, xsdName, xsdNCName, xsdID, xsdIDREF, xsdENTITY,
    xsdInteger, xsdNonPositiveInteger, xsdNegativeInteger, xsdLong, xsdInt,
    xsdShort, xsdByte, xsdNonNegativeInteger, xsdUnsignedLong, xsdUnsignedInt,
    xsdUnsignedShort, xsdUnsignedByte, xsdPositiveInteger, xsdanySimpleType,
    xsdanyType);

  legacyTypeNames: array[0..10] of String = (xsdTimeDuration, xsdBinary,
    xsdUriReference, xsdTimeInstant, xsdRecurringDate, xsdRecurringDay, xsdMonth,
    xsdYear, xsdTimePeriod, xsdRecurringDuration, xsdCentury);


function sslVersionToString (aSslVersion : TIdSSLVersion ): String ;
function sslVersionFromString (aString : String ): TIdSSLVersion ;

implementation

function sslVersionToString (aSslVersion : TIdSSLVersion ): String ;
begin
  case aSslVersion of
    sslvSSLv2:   result := 'SSL Version 2';
    sslvSSLv23:  result := 'SSL Version 2.3';
    sslvSSLv3:   result := 'SSL Version 3';
    sslvTLSv1:   result := 'TLS Version 1';
    sslvTLSv1_1: result := 'TLS Version 1.1';
    sslvTLSv1_2: result := 'TLS Version 1.2';
  end;
end;

function sslVersionFromString (aString : String ): TIdSSLVersion ;
begin
  result := sslvTLSv1_2;
  if aString = 'SSL Version 2'   then result :=  sslvSSLv2;
  if aString = 'SSL Version 2.3' then result :=  sslvSSLv23;
  if aString = 'SSL Version 3'   then result :=  sslvSSLv3;
  if aString = 'TLS Version 1'   then result :=  sslvTLSv1;
  if aString = 'TLS Version 1.1' then result :=  sslvTLSv1_1;
  if aString = 'TLS Version 1.2' then result :=  sslvTLSv1_2;
end;

end.

