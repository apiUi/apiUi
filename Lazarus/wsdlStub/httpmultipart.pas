unit httpmultipart;

{$mode objfpc}{$H+}

interface

uses
  Classes, StrUtils, SysUtils, IdGlobalProtocols, IdMessageCoderMIME, IdMessageCoder, IdCustomHTTPServer, GZIPUtils, Bind, Xmlz;

function ParseMultiPartBody (ARequestInfo: TIdHTTPRequestInfo): TXml;


implementation

function ParseMultiPartBody (ARequestInfo: TIdHTTPRequestInfo): TXml;
var
  unzipStream: TStream;
  stringStream: TStringStream;
  decoder: TIdMessageDecoderMIME;
  eomessage: Boolean;
  xName, xValue: String;
  x: Integer;
begin
  result := TXml.Create;
  stringStream := TStringStream.Create;
  unzipStream := nil;
  if not StartsText('multipart', ARequestInfo.ContentType) then Exit;
  if not Assigned(ARequestInfo.PostStream) then Exit;
  try
    decoder := TIdMessageDecoderMIME.Create(nil);
    try
      decoder.SourceStream := ARequestInfo.PostStream;
      if (LowerCase(ARequestInfo.ContentEncoding) = 'gzip')
      or (LowerCase(ARequestInfo.ContentEncoding) = 'deflate') then
      begin
        unzipStream := TMemoryStream.Create;
        GZIPUtils.ZUncompressStream(ARequestInfo.PostStream as TMemoryStream, unzipStream as TMemoryStream);
        decoder.SourceStream := unzipStream;
      end;
      decoder.FreeSourceStream := False;
      decoder.MIMEBoundary := ExtractHeaderSubItem(ARequestInfo.ContentType, 'boundary', QuoteHTTP);
      decoder.SourceStream.Position := 0;
      decoder.Headers.Clear;
      decoder.ReadHeader;
      while (decoder.PartType <> mcptEOF)
      and (decoder.SourceStream.Position < decoder.SourceStream.Size) do // workaround, did not receive any mcptEOF...
      begin
        xName := '';
        xValue := '';
        for x := 0 to decoder.headers.count -1 do
        begin
          if StartsText('Content-Disposition=', decoder.headers.Strings [x]) then
            xName := ExtractHeaderSubItem(decoder.headers.Strings [x], 'name', QuoteHTTP);
        end;
        stringStream.Clear;
        decoder.ReadBody(stringStream, eomessage);
        case decoder.PartType of
         mcptAttachment:
         begin
           for x := 0 to decoder.headers.count -1 do
           begin
             if StartsText('Content-Disposition=', decoder.headers.Strings [x]) then
               xValue := ExtractHeaderSubItem(decoder.headers.Strings [x], 'filename', QuoteHTTP);
           end;
         end;
         mcptText:
         begin
           with stringStream do
           begin
             Position := 0;
             if Size > 2 then
               xValue := ReadString(Size - 2);
           end;
         end;
        end;
        if xName <> '' then
          result.AddXml(TXml.CreateAsString(xName, xValue)).jsonType := jsonString;
        decoder.Headers.Clear;
        decoder.ReadHeader;
      end;
    finally
      decoder.Free;
    end;
  finally
    FreeAndNil(unzipStream);
    FreeAndNil(stringStream);
  end;
end;

end.

