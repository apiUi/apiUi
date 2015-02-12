unit XsdAnalyser;

interface
uses Xsdz
   , Xmlz
   ;

procedure XsdParseElement (aXsd: TXsd; aXml: TXml);
procedure XsdParseSchema (aTypeDef: TXsdDataType; aXml: TXml);

implementation

uses StrUtils
   , igGlobals
   ;

procedure XsdParseSchema (aTypeDef: TXsdDataType; aXml: TXml);
var
  xXml: TXml;
  xXsd: TXsd;
  x: Integer;
begin
  aTypeDef.ElementDefs.Clear;
  if AnsiEndsStr (':schema', aXml.TagName) then
  begin
    aTypeDef.NameSpace := LeftStr (aXml.TagName, Length (aXml.TagName) - 7);
    for x := 0 to aXml.Attributes.Count - 1 do
    begin
      if AnsiStartsStr ('xmlns:', aXml.Attributes.XmlAttributes [x].Name)
      and (aXml.Attributes.XmlAttributes [x].Value = 'http://www.w3.org/2001/XMLSchema') then
        aTypeDef.NameSpace := RestStr ('xmlns:', aXml.Attributes.XmlAttributes [x].Name);
    end;
    for x := 0 to aXml.Items.Count - 1 do
    begin
      xXml := aXml.Items.XmlItems [x];
      if xXml.TagName = aTypeDef.NameSpace + ':' + 'element' then
      begin
        xXsd := TXsd.Create;
        xXsd.Parent := aTypeDef;
        XsdParseElement (xXsd, xXml);
        aTypeDef.Elements.AddObject(xXsd.Name, xXsd);
      end;
      if xXml.TagName = aTypeDef.NameSpace + ':' + 'group' then
      begin
      end;
      if xXml.TagName = aTypeDef.NameSpace + ':' + 'simpleType' then
      begin
      end;
      if xXml.TagName = aTypeDef.NameSpace + ':' + 'complexType' then
      begin
      end;
      if xXml.TagName = aTypeDef.NameSpace + ':' + 'attribute' then
      begin
      end;
      if xXml.TagName = aTypeDef.NameSpace + ':' + 'attributeGroup' then
      begin
      end;
      if xXml.TagName = aTypeDef.NameSpace + ':' + 'notation' then
      begin
      end;
      if xXml.TagName = aTypeDef.NameSpace + ':' + 'annotation' then
      begin
      end;
    end;
  end;
end;

procedure XsdParseElement (aXsd: TXsd; aXml: TXml);
var
  xXml: TXml;
  xXsd: TXsd;
  x: Integer;
begin
  xXsd.minOccurs := 1;
  xXsd.maxOccurs := 1;
  {
    name
    type
    content
    mixed
    substGrp
    abstract
    nillable
    block
    final
    id
  }
  for x := 0 to aXml.Attributes.Count - 1 do
  begin
    if AnsiStartsStr ('xmlns:', aXml.Attributes.XmlAttributes [x].Name)
    and (aXml.Attributes.XmlAttributes [x].Value = 'http://www.w3.org/2001/XMLSchema') then
      aXsd.NameSpace := RestStr ('xmlns:', aXml.Attributes.XmlAttributes [x].Name);
  end;
    for x := 0 to aXml.Items.Count - 1 do
    begin
      xXml := aXml.Items.XmlItems [x];
      if xXml.TagName = aXsd.NameSpace + ':' + 'element' then
      begin
        xXsd := TXsd.Create;
        xXsd.Parent := aXsd;
        XsdParseElement (xXsd, xXml);
        aXsd.Elements.AddObject(xXsd.Name, xXsd);
      end;
      if xXml.TagName = aXsd.NameSpace + ':' + 'group' then
      begin
      end;
      if xXml.TagName = aXsd.NameSpace + ':' + 'simpleType' then
      begin
      end;
      if xXml.TagName = aXsd.NameSpace + ':' + 'complexType' then
      begin
      end;
      if xXml.TagName = aXsd.NameSpace + ':' + 'attribute' then
      begin
      end;
      if xXml.TagName = aXsd.NameSpace + ':' + 'attributeGroup' then
      begin
      end;
      if xXml.TagName = aXsd.NameSpace + ':' + 'notation' then
      begin
      end;
      if xXml.TagName = aXsd.NameSpace + ':' + 'annotation' then
      begin
      end;
    end;
  end;
end;

end.
