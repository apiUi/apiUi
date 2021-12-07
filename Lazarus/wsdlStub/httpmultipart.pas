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
unit httpmultipart;

{$mode objfpc}{$H+}

interface

uses
  Classes, StrUtils, SysUtils, IdGlobal, IdGlobalProtocols, IdMessageCoderMIME, IdMessageCoder, IdCustomHTTPServer, GZIPUtils, Bind, Xmlz;

function ParseMultiPartBody (ARequestInfo: TIdHTTPRequestInfo): TXml;


implementation

function ParseMultiPartBody (ARequestInfo: TIdHTTPRequestInfo): TXml;
var
  unzipStream: TStream;
  stringStream: TStringStream;
  decoder: TIdMessageDecoderMIME;
  bodyDecoder: TIdMessageDecoder;
  eomessage: Boolean;
  xContentTransferEncoding: String;
  LEncoding: IIdTextEncoding;
  xName, xValue: String;
  x: Integer;
begin
  result := TXml.Create;
  stringStream := TStringStream.Create;
  unzipStream := nil;
  eomessage := False;
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
        xContentTransferEncoding := '';
        for x := 0 to decoder.headers.count -1 do
        begin
          if StartsText('Content-Disposition=', decoder.headers.Strings [x]) then
            xName := ExtractHeaderSubItem(decoder.headers.Strings [x], 'name', QuoteHTTP);
          if StartsText('Content-Transfer-Encoding=', decoder.headers.Strings [x]) then
            xContentTransferEncoding := decoder.Headers.ValueFromIndex[x];
        end;
        stringStream.Clear;
        bodyDecoder := decoder.ReadBody(stringStream, eomessage);
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
           stringStream.Position := 0;
           case PosInStrArray(xContentTransferEncoding, ['7bit', 'quoted-printable', 'base64', '8bit', 'binary'], False) of {do not localize}
           0..2:
             if stringStream.Size > 2 then
               xValue := stringStream.ReadString(stringStream.Size - 2);
           else
             xValue := ReadStringFromStream(stringStream, -1, IndyTextEncoding_8Bit{$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}); ;
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

