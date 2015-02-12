unit forceMsXml6;

interface

implementation
uses ActiveX, MSXML, MSXMLDOM, MSXML2_TLB;

function CreateDOMDocumentJB: msxml.IXMLDOMDocument;
begin
  Result := nil;
  if CoCreateInstance ( CLASS_DOMDocument60
                      , nil
                      , CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER
                      , IXMLDOMDocument
                      , Result
                      ) <> S_OK then
  begin
    MSXMLDOMDocumentCreate := CreateDOMDocument; // fallback to use msxml4
    Result := CreateDOMDocument;
  end;
end;

initialization
  MSXMLDOMDocumentCreate := CreateDOMDocumentJB; // try to use msxml6

end.
