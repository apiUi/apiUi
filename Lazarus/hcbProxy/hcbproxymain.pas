unit hcbProxyMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, IdHTTPServer,
  IdCustomHTTPServer, IdContext, IdHeaderList, IdHTTP, IdURI, IdGlobal, Xmlz, GZIPUtils, SyncObjs;

type

  { TForm1 }

  TForm1 = class(TForm)
    IdHTTP1: TIdHTTP;
    httpProxyServer: TIdHTTPServer;
    httpConfigureServer: TIdHTTPServer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure httpConfigureServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
    procedure httpProxyServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
    procedure httpProxyServerCommandOther(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
    procedure httpProxyServerCreatePostStream(AContext: TIdContext;
      AHeaders: TIdHeaderList;var VPostStream: TStream);
  private
    { private declarations }
  public
    users: TStringList;
    Lock: TCriticalSection;
    function httpRequestStreamToString(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): String;
    procedure RemoveSomeHttpHeaders (aHeaderList: TIdHeaderList);
    procedure hcbCommand(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.httpProxyServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
begin
  hcbCommand(AContext, ARequestInfo, AResponseInfo);
end;

procedure TForm1.httpConfigureServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
var
  xUid, xHost: String;
begin
  try
    if (LowerCase(ARequestInfo.Command) <> 'post')
    or (ARequestInfo.Document <> '/configure/user')
    then raise Exception.Create('post for /configure/user expected');
    with TXml.Create do
    try
      LoadJsonFromString(httpRequestStreamToString(ARequestInfo, AResponseInfo), nil);
      if Name <> 'json' then raise Exception.Create('json expected');
      xUid := Items.XmlValueByTag['uid'];
      if xUid = '' then raise Exception.Create('no uid found');
      xHost := Items.XmlValueByTag['host'];
      if xHost = '' then raise Exception.Create('no host found');
      Lock.Acquire;
      try
        users.Values [xUid] := xHost;
      finally
        Lock.Release;
      end;
      AResponseInfo.ResponseNo := 200;
    finally
      Free;
    end;
  except
    AResponseInfo.ResponseNo := 404;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  users := TStringList.Create;
  Lock := TCriticalSection.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  users.Free;
  Lock.Free;
end;

procedure TForm1.httpProxyServerCommandOther(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
begin
  hcbCommand(AContext, ARequestInfo, AResponseInfo);
end;

procedure TForm1.httpProxyServerCreatePostStream(AContext: TIdContext;
  AHeaders: TIdHeaderList;var VPostStream: TStream);
begin
  VPostStream := TMemoryStream.Create;
end;

function TForm1.httpRequestStreamToString(ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo): String;
var
  xStream: TMemoryStream;
begin
  result := '';
  if (LowerCase(ARequestInfo.ContentEncoding) = 'gzip')
  or (LowerCase(ARequestInfo.ContentEncoding) = 'deflate') then
  begin
    AResponseInfo.ContentEncoding := ARequestInfo.ContentEncoding;
    xStream := TMemoryStream.Create;
    try
      GZIPUtils.ZUncompressStream(ARequestInfo.PostStream as TMemoryStream, xStream);
      result := IdGlobal.ReadStringFromStream(xStream);
    finally
      xStream.Free;
    end;
  end
  else
  begin
    AResponseInfo.ContentEncoding := 'identity';
    result := IdGlobal.ReadStringFromStream(ARequestInfo.PostStream);
  end;
end;

procedure TForm1.RemoveSomeHttpHeaders(aHeaderList: TIdHeaderList);
var
  x: Integer;
  function _isStdHeaderName (aName: String): Boolean;
  var
    x: Integer;
    stdHeaderStrings : array[0..10] of string =
    ( 'Connection'
    , 'Content-Version'
    , 'Content-Disposition'
    , 'Content-Language'
    , 'Content-Length'
    , 'Cache-control'
    , 'Date'
    , 'ETag'
    , 'Expires'
    , 'Pragma'
    , 'Transfer-Encoding'
    );
  begin
    result := true;
    for x := Low (stdHeaderStrings) to High(stdHeaderStrings) do
      if stdHeaderStrings[x] = aName then
        Exit;
    result := false;
  end;
begin
  for x := aHeaderList.count - 1 downto 0 do
    if _isStdHeaderName(aHeaderList.Names[x]) then
      aHeaderList.Delete(x);
end;

procedure TForm1.hcbCommand(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
var
  x: Integer;
  xUri, xHost: String;
  xHeaderValue: String;
  sStream, dStream: TStream;
begin
  try
    try
      x := ARequestInfo.RawHeaders.IndexOfName('pegaUserId');
      if x < 0 then
        raise Exception.Create('Http header "pegaUserId" not found');
      xHeaderValue := ARequestInfo.RawHeaders.Values['pegaUserId'];

      Lock.Acquire;
      try
        xHost := users.Values[xHeaderValue];
      finally
        Lock.Release;
      end;
      if (xHost = '')
      then
        raise Exception.CreateFmt('Unknown pegaUserId "%s"', [xHeaderValue]);

      with TIdURI.Create('http://' + xHost) do
      try
        Document := Copy (ARequestInfo.Document, 2, MaxInt);
        Params := ARequestInfo.QueryParams;
        xUri := Uri;
      finally
        free;
      end;

      with TIdHTTP.Create do
      try
        sStream := ARequestInfo.PostStream;
        dStream := TMemoryStream.Create;
        try
          Request.CustomHeaders.Text := ARequestInfo.RawHeaders.Text;
          Request.Accept := ARequestInfo.Accept;
          RemoveSomeHttpHeaders (Request.CustomHeaders);
          if ARequestInfo.Command = 'DELETE' then Delete(xUri);
          if ARequestInfo.Command = 'GET' then Get(xUri, dStream);
          if ARequestInfo.Command = 'HEAD' then Head(xUri);
          if ARequestInfo.Command = 'OPTIONS' then Options(xUri);
          if ARequestInfo.Command = 'POST' then Post(xUri, sStream, dStream);
          if ARequestInfo.Command = 'PUT' then Put(xUri, sStream, dStream);
          if ARequestInfo.Command = 'TRACE' then Trace(xUri, dStream);
          AResponseInfo.ResponseNo := ResponseCode;
          AResponseInfo.RawHeaders.Text := Response.RawHeaders.Text;
          AResponseInfo.ContentEncoding := Response.ContentEncoding;
          if dStream.Size > 0 then
          begin
            AResponseInfo.ContentStream := dStream;
            dStream := TMemoryStream.Create;
          end;
        finally
          dStream.Free;
        end;
      finally
        free;
      end;
    except
      on e: Exception do
      begin
        AResponseInfo.ResponseNo := 500;
        AResponseInfo.ContentText := e.Message;
      end;
    end;
  finally
  end;
end;

end.

