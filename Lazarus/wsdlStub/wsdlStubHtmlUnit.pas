unit wsdlStubHtmlUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses IdCustomHTTPServer
   , WsdlProjectz
   ;

function createHtmlResponse (aProject: TWsdlProject; aRequest: TIdHTTPRequestInfo): String;

implementation

uses Wsdlz
   , Xmlz
   , igGlobals
   , SysUtils
   , Classes
   , xmlio
   ;
const CRLF = #13#10;


function _DefStr (aValue: String): String;
begin
  result := aValue + '_';
end;

function _logHref (x: Integer): String;
begin
  result := '';
  raise Exception.Create('function _logHref (x: Integer): String;');
//  result := '?Index=' + IntToStr (x) + ';' + 'Check=' + displayedLogs.LogItems [x].CorrId;
end;

function _htmlCreateInvalidCommand (aXml: TXml; aRequest: TIdHTTPRequestInfo): TXml;
begin
  result := aXml;
  with aXml do
  begin
    AddXml (TXml.CreateAsString('a','Invalid Command: '));
    AddXml (TXml.CreateAsString('br',aRequest.Command));
  end;
end;

function createHtmlResponse (aProject: TWsdlProject; aRequest: TIdHTTPRequestInfo): String;
  function _prepIndex(fn: String):String;
  var
    s: String;
    sl, dl: TStringList;
    x: Integer;
  begin
    result :='';
    sl := TStringlist.Create;
    dl := TStringList.Create;
    try
      try
        s := ReadStringFromFile(fn);
      except
        on e: Exception do
          raise Exception.CreateFmt('%s: error opening file: %s%s%s', [_progName, fn, CRLF, e.Message]);
      end;
      s := StringReplace(s, '--progName--', _ProgName, [rfReplaceAll]);
      sl.Text := s;
      for x := 0 to sl.Count - 1 do
      begin
{}{
        if Pos (placeHolder, sl.Strings[x]) > 0 then
          for w := 0 to Wsdls.Count - 1 do with wsdls.Objects[w] as TWsdl do
          begin
            dl.Add(StringReplace(sl.Strings[x], placeHolder, Name, [rfReplaceAll]));
          end
        else
{}
          dl.Add(sl.Strings[x]);
      end;
      result := dl.Text;
    finally
      sl.Free;
      dl.Free;
    end;
  end;
var
  xXml: TXml;
begin
  if (aRequest.Command = 'GET') then
  begin
    if (aRequest.Document = '/index.html')
    or (aRequest.Document = '/') then
    begin
      try
        result := _prepIndex(ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), indexHtmlFileName));
        Exit;
      except
        on e: exception do
        begin
          result := e.Message;
          exit;
        end;
      end;
    end;
  end;

  if (aRequest.Command <> 'POST') then
  begin
    xXml := TXml.CreateAsString('html', '');
    try
      result := _htmlCreateInvalidCommand(xXml, aRequest).asHtmlString;
    finally
      xXml.Free;
    end;
    exit;
  end;
  if (True) then
  begin
    xXml := TXml.CreateAsString('html', '');
    try
      result := _htmlCreateInvalidCommand(xXml, aRequest).asHtmlString;
    finally
      xXml.Free;
    end;
    exit;
  end;
end;

end.
