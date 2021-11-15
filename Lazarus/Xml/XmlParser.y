%{
{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
type TXmlParser = class (TCustParser)
private
  CurrentXml: TXml;
  ParentXml: TXml;
  OfIdString: String;
published
  property OnHaveScanned: TOnHaveScannedEvent read FOnHaveScanned write FOnHaveScanned;
  property OnError: TOnErrorEvent read FOnError write FOnError;
public
  BaseXml: TXml;
  procedure AddAttribute (aName: String; aValue: String);
  function yylex: Integer; override;
  function yyparse: Integer; override;
  procedure Prepare; override;
  destructor Destroy; override;
end;
%}

%token _ATTRIBUTENAME
%token _ATTRIBUTEVALUE
%token _CDATA
%token _CDATAENDTAG
%token _CDATATAG
%token _EMPTYENDTAG
%token _ENDTAG
%token _ESCAPECHAR
%token _IGNORE
%token _IS
%token _SLASHTAG
%token _TAG
%token _VALUE

%%
%{

implementation
  (* local definitions: *)
uses SysUtils, Dialogs
   ;

procedure TXmlParser.AddAttribute (aName: String; aValue: String);
var
  xAttribute: TXmlAttribute;
begin
  xAttribute := TXmlAttribute.Create;
  xAttribute.Name := aName;
  xAttribute.Value := xmlDecodeXml(Copy (aValue, 2, Length (aValue) - 2));
  xAttribute.Checked := True;
  CurrentXml.AddAttribute (xAttribute);
end;

procedure TXmlParser.Prepare;
begin
  inherited Prepare;
end;

destructor TXmlParser.Destroy;
begin
  inherited Destroy;
end;

function TXmlParser.yylex: Integer;
begin
  result := inherited yylex;
end;

%}

start:
      {
        ParentXml := nil;
        CurrentXml := nil;
        BaseXml := nil;
      }
      OptionalXmlSpecs
    ;

OptionalXmlSpecs:
      /* void */
    | OptionalIgnoredSpace XmlSpecs OptionalIgnoredSpace
    ;

OptionalIgnoredSpace:
      /* void */
    | _VALUE
      {
        if (Trim ($1.TokenString) <> '')
        and (Trim ($1.TokenString) <> BOM) then
          raise Exception.Create ( '%Illegal: ['
                                 + $1.TokenString
                                 + ']['
                                 + IntToStr ($1.LineNumber)
                                 + ':'
                                 + IntToStr ($1.ColumnNumber)
                                 + ']'
                                 );
      }
    ;


XmlSpecs:
      XmlSpec
    | XmlSpecs XmlSpec
    ;

XmlSpec:
      {
        PushObject (CurrentXml);
        CurrentXml := TXml.Create;
        CurrentXml.Checked := True;
        CurrentXml.Parent := ParentXml;
        if BaseXml = nil then
          BaseXml := CurrentXml;
        if ParentXml <> nil then
          (ParentXml.Items as TXmlList).AddObject ('', CurrentXml);
      }
      TagSpec
      {
        if (CurrentXml.Items.Count > 0)
        and (CurrentXml.Value <> '')
        and (Trim (CurrentXml.Value) = '') then
          CurrentXml.Value := '';
        CurrentXml := PopObject as TXml;
      }
    ;

TagSpec:
      _TAG
      {
        CurrentXml.TagName := Copy ( $1.TokenString
                                   , 2
                                   , system.Length ($1.TokenString) - 1
                                   );
      }
           OptionalAttributes eTagSpec
    | _CDATATAG OptionalCDatas _CDATAENDTAG
      {
        CurrentXml.CData := True;
      }
    ;

eTagSpec:
      _ENDTAG OptionalValues _SLASHTAG _ENDTAG
      {
        if CurrentXml.TagName <> Copy ( $3.TokenString
                                      , 3
                                      , system.Length ($3.TokenString) - 2
                                      )
        then
          raise Exception.Create ( CurrentXml.TagName
                                 + ': Unmatched EndTag ('
                                 + $3.TokenString
                                 + ')'
                                 );
      }
    | _EMPTYENDTAG
    ;

OptionalAttributes:
      /* void */
    | Attributes
    ;

Attributes:
      Attribute
    | Attributes Attribute
    ;

Attribute:
      _ATTRIBUTENAME _IS _ATTRIBUTEVALUE
      {
        AddAttribute ($1.TokenString, $3.TokenString);
      }
    ;


OptionalValues:
      /* void */
    | Values
    ;

Values:
      Value
    | Values Value
    ;

Value:
      _VALUE
      {
        CurrentXml.Value := CurrentXml.Value + $1.TokenString;
      }
    | SubXmlSpec
    ;

SubXmlSpec:
      {
        PushObject (ParentXml);
        ParentXml := CurrentXml;
      }
      XmlSpec
      {
        ParentXml := PopObject as TXml;
      }
      ;

OptionalCDatas:
        /* void */
      | CDatas
      ;

CDatas:
        CData
      | CDatas CData
      ;

CData:
        _CDATA
        {
          CurrentXml.Value := CurrentXml.Value + $1.TokenString;
        }
      ;
