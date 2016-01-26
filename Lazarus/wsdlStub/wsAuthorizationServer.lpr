program wsAuthorizationServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils , CustApp
  { you can add units after this }
  , sqldb
  , odbcconn
  , SyncObjs
  , strutils
  , IdCustomHTTPServer
  , IdHTTPServer
  , IdContext
  , IdHeaderList
  , Xmlz
  , xmlio
  , GZIPUtils
  , HashUtilz
  , FileUtil
  , lazrichedit
  ;

type

  { TwsAuthorizationServer }

  TwsAuthorizationServer = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    SQLConnector : TSQLConnector ;
    SQLTransaction : TSQLTransaction ;
    SQLQuery: TSQLQuery;
    dbLock: TCriticalSection;
    httpServer: TIdHTTPServer;
    portNumber: Integer;
    licenseOdbcDriver, licenseDatabaseName: String;
    CompanyName, LicenseExpirationDate: String;
    function OpenDatabase: Boolean;
    procedure CloseDatabase;
    function GetAuthorization: String;
    procedure SetAuthorization (aCompany, aLicenseExpirationDate, aLicenseString: String);
    procedure LogUsage (aUserName, aProgramName, aVersion: String);
    function httpRequestStreamToString(
        ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): String;
    procedure WriteStringToStream (aString: String; aStream: TMemoryStream);
    procedure HTTPServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HTTPServerCreatePostStream(AContext: TIdContext;
      AHeaders: TIdHeaderList; var VPostStream: TStream);
    procedure HTTPServerCommandPutPut(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure IntrospectIniXml;
    function generateLicense (aBase: String): String;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TwsdlAuthorizationServer }

procedure TwsAuthorizationServer.DoRun;
var
  ErrorMsg: String;
begin
  with HTTPServer do
  begin
    with Bindings.Add do
    begin
      Port := portNumber;
      IP := '0.0.0.0';
    end;
    Active := True;
    writeln ( format( 'Listening for HTTP trafic on %s:%d.'
                    , [HTTPServer.Bindings[0].IP, HTTPServer.Bindings[0].Port]
                    )
            );
    while not Terminated do
      Sleep (333);
    Active := False;
    Bindings.Clear;
  end;
  Terminate;
end;

function TwsAuthorizationServer .generateLicense (aBase : String ): String ;
var
  S: AnsiString;
begin
  S := Sha1('hdal%)j1asas90' + LowerCase (aBase) + 'cnfdj 3h*q!H');
  result := Copy (S, 4, 4)
          + '-'
          + Copy (S, 8, 4)
          + '-'
          + Copy (S, 12, 4)
          + '-'
          + Copy (S, 16, 4)
          + '-'
          + Copy (S, 18, 4)
          + '-'
          + Copy (S, 26, 4)
          + '-'
          + Copy (S, 30, 4)
          + '-'
          + Copy (S, 34, 4)
          ;
end;

function TwsAuthorizationServer.OpenDatabase : Boolean ;
begin
  CloseDatabase;
  SQLConnector.ConnectorType := 'odbc';
  SQLConnector.DatabaseName := 'MS Access Database';
  SqlConnector.Params.Text := 'DBQ=' + licenseDatabaseName;
  SqlConnector.Connected := True;
  result := SqlConnector.Connected;
end;

procedure TwsAuthorizationServer .CloseDatabase;
begin
  try
    SqlConnector.Connected := False;
  except
  end;
end;

function TwsAuthorizationServer.GetAuthorization : String;
var
  xDt: TDateTime;
  Y, m, d: Word;
  xLicenseDate: TDateTime;
  xLicenseString, xGen: String;
begin
  result := 'false';
  SqlQuery.SQL.Clear;
  SqlQuery.SQL.Add('Select CompanyName, LicenseExpireDate, LicenseString');
  SqlQuery.SQL.Add('from LicenseInformation');
  SqlQuery.ParseSQL := False;
  SqlQuery.Open;
  while not SqlQuery.EOF do
  begin
    CompanyName := SqlQuery.FieldByName('CompanyName').AsString;
    LicenseExpirationDate := SqlQuery.FieldByName('LicenseExpireDate').AsString;
    xLicenseString := SqlQuery.FieldByName('LicenseString').AsString;
    SqlQuery.Next;
  end;
  SqlQuery.Close;
  xGen := generateLicense ( CompanyName
                          + LicenseExpirationDate
                          + generateLicense(licenseDatabaseName)
                          );
  if ( xGen = xLicenseString)
  and (   (AnsiStartsStr('//', licenseDatabaseName))
       or (GetUserName = 'Jan')
       or (GetUserName = 'BouwmanJW')
      ) then
  begin
    Y := StrToInt(Copy(LicenseExpirationDate, 1, 4));
    m := StrToInt(Copy(LicenseExpirationDate, 6, 2));
    d := StrToInt(Copy(LicenseExpirationDate, 9, 2));
    xDt := EncodeDate(Y, m, d);
    if (Now < xDt) then
      result:= 'true';
  end;
end;

procedure TwsAuthorizationServer .SetAuthorization (aCompany ,
  aLicenseExpirationDate , aLicenseString : String );
begin
  SqlConnector.Transaction.StartTransaction;
  try
    SqlQuery.SQL.Clear;
    SqlQuery.SQL.Add('Update LicenseInformation');
    SqlQuery.SQL.Add('set CompanyName = :CompanyName');
    SqlQuery.SQL.Add('  , LicenseExpireDate = :LicenseExpireDate');
    SqlQuery.SQL.Add('  , LicenseString = :LicenseString');
    SqlQuery.Params.ParamValues['CompanyName'] := aCompany;
    SqlQuery.Params.ParamValues['LicenseExpireDate'] := aLicenseExpirationDate;
    SqlQuery.Params.ParamValues['LicenseString'] := aLicenseString;
    SqlQuery.ExecSql;
  finally
    SqlConnector.Transaction.Commit;
  end;
end;

procedure TwsAuthorizationServer .LogUsage (aUserName , aProgramName ,
  aVersion : String );
var
  xUpdated: TDateTime;
  xUsageDate: TDateTime;
begin
  xUpdated := Now;
  xUsageDate := sysutils.Date;
  if (aUserName <> 'JanBo hoho')
  and (aUserName <> 'Bouwman hoho')
  and (SqlConnector.Connected) then
  begin
    SqlConnector.Transaction.StartTransaction;
    try
      try
        SqlQuery.SQL.Clear;
        SqlQuery.SQL.Add('Insert into UsageNames');
        SqlQuery.SQL.Add('( UserName');
        SqlQuery.SQL.Add(', nUsage');
        SqlQuery.SQL.Add(', Updated');
        SqlQuery.SQL.Add(') values');
        SqlQuery.SQL.Add('( :UserName');
        SqlQuery.SQL.Add(', 1');
        SqlQuery.SQL.Add(', :Updated');
        SqlQuery.SQL.Add(')');
        SqlQuery.Params.ParamValues['UserName'] := aUserName;
        SqlQuery.Params.ParamValues['Updated'] := xUpdated;
        SqlQuery.ExecSql;
      except
        on E: Exception do
        begin
          try
            SqlQuery.SQL.Clear;
            SqlQuery.SQL.Add('Update UsageNames');
            SqlQuery.SQL.Add('set nUsage = nUsage + 1');
            SqlQuery.SQL.Add('  , Updated = :Updated');
            SqlQuery.SQL.Add('where UserName = :UserName');
            SqlQuery.Params.ParamValues['Updated'] := xUpdated;
            SqlQuery.Params.ParamValues['UserName'] := aUserName;
            SqlQuery.ExecSql;
          except
          end;
        end; { try to update UsageNames when insert failed }
      end; { try to insert UsageNames }

      try
        SqlQuery.SQL.Clear;
        SqlQuery.SQL.Add('Insert into UsageDates');
        SqlQuery.SQL.Add('( UsageDate');
        SqlQuery.SQL.Add(', nUsage');
        SqlQuery.SQL.Add(', Updated');
        SqlQuery.SQL.Add(') values');
        SqlQuery.SQL.Add('( :UsageDate');
        SqlQuery.SQL.Add(', 1');
        SqlQuery.SQL.Add(', :Updated');
        SqlQuery.SQL.Add(')');
        SqlQuery.Params.ParamValues['UsageDate'] := xUsageDate;
        SqlQuery.Params.ParamValues['Updated'] := xUpdated;
        SqlQuery.ExecSql;
      except
        on E: Exception do
        begin
          try
            SqlQuery.SQL.Clear;
            SqlQuery.SQL.Add('Update UsageDates');
            SqlQuery.SQL.Add('set nUsage = nUsage + 1');
            SqlQuery.SQL.Add('  , Updated = :Updated');
            SqlQuery.SQL.Add('where UsageDate = :UsageDate');
            SqlQuery.Params.ParamValues['Updated'] := xUpdated;
            SqlQuery.Params.ParamValues['UsageDate'] := xUsageDate;
            SqlQuery.ExecSql;
          except
          end;
        end; { try to update UsageDates when insert failed }
      end; { try to insert UsageDates }
    finally
      SqlConnector.Transaction.Commit;
    end; // try
  end;
end;

function TwsAuthorizationServer .httpRequestStreamToString (
  ARequestInfo : TIdHTTPRequestInfo ; AResponseInfo : TIdHTTPResponseInfo
  ): String ;
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
      xStream.Position := 0;
      SetLength(Result,xStream.Size);
      xStream.Read(Pointer(Result)^,xStream.Size);
    finally
      xStream.Free;
    end;
  end
  else
  begin
    AResponseInfo.ContentEncoding := 'identity';
    with ARequestInfo.PostStream as TMemoryStream do
    begin
      Position := 0;
      SetLength(Result, Size);
      Read(Pointer(Result)^, Size);
    end;
  end;
end;

procedure TwsAuthorizationServer .WriteStringToStream (aString : String ;
  aStream : TMemoryStream );
begin
  aStream.Position := 0;
  aStream.Write(Pointer(aString)^, Length (aString));
  aStream.Position := 0;
end;

procedure TwsAuthorizationServer .HTTPServerCommandGet (
  AContext : TIdContext ; ARequestInfo : TIdHTTPRequestInfo ;
  AResponseInfo : TIdHTTPResponseInfo );
var
  RequestBody, ReplyBody: String;
  xUserName, xTimestamp, xProgram, xVersion, xCompany: String;
  xAuthorized, xLicenseString: String;
  xStream: TMemoryStream;
begin
  RequestBody := httpRequestStreamToString(ARequestInfo, AResponseInfo);
  writeln (RequestBody);
  aResponseInfo.ContentText := 'Unrecognized request';
  AResponseInfo.ResponseNo := 400;
  with TXml.Create do
  begin
    LoadFromString (RequestBody, nil);
    if Name = 'getAuthorization' then
    begin
      AResponseInfo.ResponseNo := 200;
      aResponseInfo.ContentText := 'Not authorized';
      xUserName := Items.XmlValueByTag['UserName'];
      xTimeStamp := Items.XmlValueByTag['TimeStamp'];
      xProgram := Items.XmlValueByTag['Program'];
      xVersion := Items.XmlValueByTag['Version'];
      dbLock.Acquire;
      try
        if OpenDataBase then
        begin
          LogUsage(xUserName, xProgram, xVersion);
          xAuthorized := GetAuthorization;
          CloseDatabase;
          if True then
          begin
            with TXml.CreateAsString('wsAutorization', '') do
            try
              AddXml (TXml.CreateAsString('authorized', xAuthorized));
              AddXml (TXml.CreateAsString( 'key'
                                         , Sha1 ( xUserName
                                                + xTimestamp
                                                + '^abra^'
                                                + xAuthorized
                                                )
                                         )
                     );
              AddXml (TXml.CreateAsString('licensee', CompanyName));
              AddXml (TXml.CreateAsString('expireDate', LicenseExpirationDate));
              aResponseInfo.ContentText := Text;
            finally
              Free;
            end;
          end;
        end;
      finally
        dbLock.Release;
      end;
    end;
    if Name = 'getAuthorizationBaseString' then
    begin
      with TXml.CreateAsString ('wsAutorizationBaseString', '') do
      try
        AResponseInfo.ResponseNo := 200;
        AddXml (TXml.CreateAsString ( 'value'
                                    , generateLicense(licenseDatabaseName)
                                    )
               );
        aResponseInfo.ContentText := Text;
      finally
        Free;
      end;
    end;
    if Name = 'setAuthorization' then
    begin
      AResponseInfo.ResponseNo := 500;
      aResponseInfo.ContentText := 'Update failed';
      xCompany := Items.XmlValueByTag['Company'];
      LicenseExpirationDate := Items.XmlValueByTag['LicenseDate'];
      xLicenseString := Items.XmlValueByTag['LicenseString'];
      dbLock.Acquire;
      try
        if OpenDataBase then
        try
          SetAuthorization (xCompany, LicenseExpirationDate, xLicenseString);
          AResponseInfo.ResponseNo := 200;
          with TXml.CreateAsString('wsAutorizationSet', '') do
          try
            AddXml (TXml.CreateAsString('success', 'true'));
            aResponseInfo.ContentText := Text;
          finally
            Free;
          end;
        finally
          CloseDatabase;
        end;
      finally
        dbLock.Release;
      end;
    end;
  end;
  AResponseInfo.ContentType := ARequestInfo.ContentType;
  if AResponseInfo.ContentEncoding <> 'identity' then
  begin
    aResponseInfo.ContentStream := TMemoryStream.Create;
    xStream := TMemoryStream.Create;
    try
      WriteStringToStream(AResponseInfo.ContentText, xStream);
      if AResponseInfo.ContentEncoding = 'deflate' then
        GZIPUtils.deflate(xStream, aResponseInfo.ContentStream as TMemoryStream);
      if AResponseInfo.ContentEncoding = 'gzip' then
        GZIPUtils.GZip(xStream, aResponseInfo.ContentStream as TMemoryStream);
    finally
      xStream.Free;
    end;
    aResponseInfo.ContentText := '';
  end;
end;

procedure TwsAuthorizationServer .HTTPServerCreatePostStream (
  AContext : TIdContext ; AHeaders : TIdHeaderList ; var VPostStream : TStream
  );
begin
  VPostStream := TMemoryStream.Create;
end;

procedure TwsAuthorizationServer .HTTPServerCommandPutPut (
  AContext : TIdContext ; ARequestInfo : TIdHTTPRequestInfo ;
  AResponseInfo : TIdHTTPResponseInfo );
begin

end;

procedure TwsAuthorizationServer .IntrospectIniXml ;
var
  x: Integer;
  xIniFileName: String;
{
<wsdlAuthorizationServerIni>
  <portNumber>5960</portNumber>
  <licenseDatabase>
    <OdbcDriver>Microsoft Access Driver (*.mdb)</OdbcDriver>
    <DatabaseName>Database\wsdlStubLicense.mdb</DatabaseName>
  </licenseDatabase>
</wsdlAuthorizationServerIni>
}
begin
  xIniFileName := Copy(ParamStr(0), 1, Length(ParamStr(0)){$ifdef windows} - 4{$endif}) + 'Ini.xml';
  if not FileExistsUTF8(xIniFileName) { *Converted from FileExists* } then
    raise Exception.CreateFmt(
      '%s coud not open expected inifile: %s,%splease install properly',
      [ParamStr(0), xIniFileName, LineEnding]);
  with TXml.Create do
  try
    LoadFromFile(xIniFileName, nil);
    if Name <> 'wsdlAuthorizationServerIni' then
      raise Exception.CreateFmt('%s is not a valid ini-file', [xIniFileName]);
    portNumber := Items.XmlIntegerByTagDef['portNumber', 5960];
    if Assigned (ItemByTag['licenseDatabase']) then with ItemByTag['licenseDatabase'].Items do
    begin
      licenseOdbcDriver := XmlValueByTagDef['OdbcDriver', 'Microsoft Access Driver'];
      licenseDatabaseName := ExpandRelativeFileName(xIniFileName, XmlValueByTagDef['DatabaseName', '']);
    end;
  finally
    Free;
  end;
end;

constructor TwsAuthorizationServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IntrospectIniXml;
  dbLock := SyncObjs.TCriticalSection.Create;
  httpServer := TIdHTTPServer.Create;
  SqlConnector := TSQLConnector.Create(nil);
  SQLTransaction := TSQLTransaction.Create(nil);
  SQLQuery := TSQLQuery.Create(nil);
  SQLConnector.Transaction := SQLTransaction;
  SQLTransaction.Action := caCommit;
  SQLTransaction.DataBase := SQLConnector;
  SQLQuery.DataBase := SQLConnector;
  SQLQuery.Transaction := SQLTransaction;
  OpenDatabase;
  CloseDatabase;
  with HttpServer do
  begin
    OnCommandGet := @HttpServerCommandGet;
    OnCommandOther := @HttpServerCommandGet;
    OnCreatePostStream := @HttpServerCreatePostStream;
  end;
end;

destructor TwsAuthorizationServer.Destroy;
begin
  FreeAndNil(httpServer);
  FreeAndNil(SQLQuery);
  FreeAndNil(SQLTransaction);
  FreeAndNil(SqlConnector);
  FreeAndNil(dbLock);
  inherited Destroy;
end;

var
  Application: TwsAuthorizationServer;
begin
  Application:=TwsAuthorizationServer.Create(nil);
  Application.Run;
  Application.Free;
end.

