program svrouter;
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  , Interfaces
  , math
  , xmlio
  , Xmlz
  , xmlxsdparser
  , exceptionUtils
  , LazFileUtils
  , IdHTTPServer
  , IdCustomHTTPServer
  , IdContext
  , IdHeaderList
  , IdHTTP
  , IdURI
  , IdGlobal
  , IdSSLOpenSSL
  , GZIPUtils
  , SyncObjs
  ;


type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    fSslPassword: String;
    procedure HandleException(Sender: TObject; E: Exception);
    procedure Notify(const aString: String);
    procedure SetSslPassword(AValue: String);
    procedure OnTerminateThread;
    procedure httpServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
    procedure httpServerCreatePostStream(AContext: TIdContext;
      AHeaders: TIdHeaderList;var VPostStream: TStream);
    procedure OnGetSslPassword (var aPassword: String);
  public
    doLog, isSslServer: Boolean;
    mainPortnumber, sslCerticateFileName, sslKeyFileName: String;
    httpMainServer: TIdHTTPServer;
    property SslPassword: String read fSslPassword write SetSslPassword;
    procedure RemoveSomeHttpHeaders (aHeaderList: TIdHeaderList);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  serverHttpXml, sslXml: TXml;
begin
  try
    with TXml.Create do
    try
      try
        LoadFromFile(_progName + 'ini.xml', nil);
      except
        raise Exception.Create ('Could not read ' + _progName + 'ini.xml');
      end;
      if Name <> 'svrouterini' then
        raise Exception.Create (_progName + 'ini.xml does not contain valid xml');
      doLog := Items.XmlBooleanByTagDef['doLog', False];
      serverHttpXml := ItemByTag['serverHttp'];
      if not Assigned (serverHttpXml) then
        raise Exception.Create (_progName + 'ini.xml: serverHttpXml not found');
      with serverHttpXml do
      begin
        mainPortnumber := Items.XmlValueByTag['port'];
        if mainPortnumber = '' then
          raise Exception.Create (_progName + 'ini.xml: serverHttpXml.port not found');
        sslXml := ItemByTag['ssl'];
        if not Assigned (sslXml) then
          raise Exception.Create (_progName + 'ini.xml: serverHttpXml.ssl not found');
        with sslXml do
        begin
          isSslServer := Items.XmlBooleanByTagDef['useSsl', True];
          sslCerticateFileName := Items.XmlValueByTag['certificateFile'];
          sslKeyFileName := Items.XmlValueByTag['keyFile'];
          SslPassword := Items.XmlValueByTag['password'];
        end;
      end;
    finally
      Free;
    end;
    httpMainServer.DefaultPort := StrToInt(mainPortnumber);
    httpMainServer.OnCommandGet := httpServerCommandGet;
    httpMainServer.OnCommandOther := httpServerCommandGet;
    if isSslServer then
    begin
      httpMainServer.IOHandler := TIdServerIOHandlerSSLOpenSSL.Create(nil);
      with (httpMainServer.IOHandler as TIdServerIOHandlerSSLOpenSSL) do
      begin
        OnGetPassword := self.OnGetSslPassword;
        with SSLOptions do
        begin
          Method := sslvTLSv1_2;
          CertFile := sslCerticateFileName;
          KeyFile := sslKeyFileName;
        end;
      end;
    end;
    httpMainServer.Active := True;
    WriteLn('Listening to port: ' + mainPortnumber);
    while not Terminated do
    begin
      CheckSynchronize;
      Sleep (100);
    end;
    httpMainServer.Active := False;
    Terminate;
    Notify ('quit');
  except
    on e: Exception do
    begin
      WriteLn (e.Message);
      Terminate;
      Exit;
    end;
  end;
end;

procedure TMyApplication .HandleException (Sender : TObject ; E : Exception );
var
  s: String;
begin
  try
    s := ExceptionStackListString (E);
  except
  end;
  writeln (Format('Message: %s%s', [e.Message, s]));
end;

procedure TMyApplication .Notify (const aString : String );
begin
  WriteLn (xsdFormatDateTime(now, @TIMEZONE_UTC), ' notify: ', aString);
end;

procedure TMyApplication.SetSslPassword(AValue: String);
begin
  fSslPassword := xmlio.DecryptPassword(AValue);
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  try
    httpMainServer := TIdHTTPServer.Create;
  except
    on e: Exception do
    begin
      WriteLn (e.Message);
      Terminate;
      Exit;
    end;
  end;
end;

destructor TMyApplication.Destroy;
begin
  FreeAndNil(httpMainServer);
  inherited Destroy;
end;

procedure TMyApplication.OnTerminateThread;
begin

end;

procedure TMyApplication.httpServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
var
  x: Integer;
  xApp, xUri, xProtocol, xHost, xPort, xDocument: String;
  sStream, dStream: TStream;
begin
  xApp := '';
  try
    try
      // /svouter/destination/portNumber/mockuri...
      xDocument := Copy (ARequestInfo.Document, 2, MaxInt);
      x := Pos('/', xDocument);
      if x > 0 then
        xApp := Copy (xDocument, 1, x - 1);
      if xApp <> _progName then
        raise Exception.CreateFmt('Path "%s" does not start with "%s"', [ARequestInfo.Document, _progName]);
      xDocument := Copy (xDocument, x + 1, MaxInt);
      x := Pos('/', xDocument);
      if x < 1 then
        raise Exception.CreateFmt('Path "%s" has no host', [ARequestInfo.Document, _progName]);
      xHost := Copy (xDocument, 1, x - 1);
      xDocument := Copy (xDocument, x + 1, MaxInt);
      x := Pos('/', xDocument);
      if x < 1 then
        raise Exception.CreateFmt('Path "%s" has no port', [ARequestInfo.Document, _progName]);
      xport := Copy (xDocument, 1, x - 1);
      xDocument := Copy (xDocument, x + 1, MaxInt);
      try
        StrToInt(xPort);
      except
        raise Exception.CreateFmt('Path "%s" port not an integer', [ARequestInfo.Document, xPort]);
      end;
      if xPort = '443' then
        xProtocol := 'https'
      else
        xProtocol := 'http';
      if doLog then WriteLn (Format('protocol: "%s" Host: "%s" Port: "%s" Document: "%s"', [xProtocol, xHost, xPort, xDocument]));
      with TIdURI.Create(xProtocol + '://' + xHost + ':' + xPort) do
      try
        Document := xDocument;
        Params := ARequestInfo.QueryParams;
        xUri := Uri;
      finally
        free;
      end;
      if doLog then WriteLn(ARequestInfo.RemoteIP + ':' + ARequestInfo.URI + ' => ' + xUri);
      with TIdHTTP.Create do
      try
        sStream := ARequestInfo.PostStream;
        dStream := TMemoryStream.Create;
        try
          Request.CustomHeaders.Text := ARequestInfo.RawHeaders.Text;
          Request.ContentType := ARequestInfo.ContentType;
          Request.Accept := ARequestInfo.Accept;
          RemoveSomeHttpHeaders (Request.CustomHeaders);
          if xProtocol = 'https' then
          begin
            IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
            with (IOHandler as TIdSSLIOHandlerSocketOpenSSL) do
            begin
{
              SSLOptions.CertFile := resolveAliasses (aOperation.sslCertificateFile);
              SSLOptions.KeyFile := resolveAliasses (aOperation.sslKeyFile);
              SSLOptions.RootCertFile := resolveAliasses (aOperation.sslRootCertificateFile);
              if aOperation.sslPassword <> '' then
                OnGetPassword := aOperation.OnGetSslPassword; // TODO resolveAliasses...
}
              SSLOptions.Method := sslvTLSv1_2;
              SSLOptions.Mode := sslmUnassigned;
              SSLOptions.VerifyMode := [];
            end;
          end;
          if doLog then WriteLn(Request.CustomHeaders.Text);
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
          AResponseInfo.ContentType := Response.ContentType;
          if dStream.Size > 0 then
          begin
            AResponseInfo.ContentStream := dStream;
            dStream := TMemoryStream.Create;
          end;
        finally
          dStream.Free;
        end;
      finally
        if Assigned (IOHandler) then
          IOHandler.Free;
        free;
      end;
    except
      on e: Exception do
      begin
        AResponseInfo.ResponseNo := 500;
        AResponseInfo.ContentText := e.Message;
        WriteLn(e.Message);
      end;
    end;
  finally
  end;
end;

procedure TMyApplication.httpServerCreatePostStream(AContext: TIdContext;
  AHeaders: TIdHeaderList;var VPostStream: TStream);
begin
  VPostStream := TMemoryStream.Create;
end;

procedure TMyApplication.OnGetSslPassword(var aPassword: String);
begin
  aPassword := fSslPassword;
end;

procedure TMyApplication.RemoveSomeHttpHeaders(aHeaderList: TIdHeaderList);
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

var
  Application: TMyApplication;

{$R *.res}

begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.



