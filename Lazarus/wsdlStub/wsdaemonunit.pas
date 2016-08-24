unit wsDaemonUnit ;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , strutils
  , FileUtil
  , IdHTTPServer
  , DaemonApp
  , Xmlz
  , XmlIo
  , IdCustomHTTPServer
  , IdContext
  , sqldb
  , odbcconn
  , SyncObjs
  , HashUtilz
  , GZIPUtils
//  , exceptionUtils
  , IdHeaderList
  ;

type

  { TDaemon1 }

  TDaemon1 = class(TDaemon )
    IdHTTPServer : TIdHTTPServer ;
    SqlConnector : TODBCConnection ;
    SQLQuery : TSQLQuery ;
    SQLTransaction : TSQLTransaction ;
    procedure DataModuleCreate (Sender : TObject );
    procedure DataModuleDestroy (Sender : TObject );
    procedure DataModuleStart (Sender : TCustomDaemon ; var OK : Boolean );
    procedure DataModuleStop (Sender : TCustomDaemon ; var OK : Boolean );
    procedure IdHTTPServerCommandGet (AContext : TIdContext ;
      ARequestInfo : TIdHTTPRequestInfo ; AResponseInfo : TIdHTTPResponseInfo );
    procedure IdHTTPServerCreatePostStream (AContext : TIdContext ;
      AHeaders : TIdHeaderList ; var VPostStream : TStream );
  Private
    fOdbcDriver: String;
    fDatabaseName: String;
    portNumber: Integer;
    licenseOdbcDriver, licenseDatabaseName: String;
    dbLock: TCriticalSection;
    CompanyName, LicenseExpirationDate: String;
    function OpenDatabase: Boolean;
    procedure CloseDatabase;
    function GetAuthorization: String;
    function generateLicense (aBase: String): String;
    procedure SetAuthorization (aCompany, aLicenseExpirationDate, aLicenseString: String);
    procedure LogUsage (aUserName, aProgramName, aVersion: String);
    procedure IntrospectIniXml;
    function httpRequestStreamToString(
        ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): String;
    procedure WriteStringToStream (aString: String; aStream: TMemoryStream);
  public
    { public declarations }
  end;

var
  Daemon1 : TDaemon1 ;

implementation

procedure RegisterDaemon ;
begin
  RegisterDaemonClass (TDaemon1 )
end;

{$R *.lfm}

{ TDaemon1 }

procedure TDaemon1 .DataModuleStart (Sender : TCustomDaemon ; var OK : Boolean
  );
begin
  IntrospectIniXml;
  IdHTTPServer.DefaultPort := PortNumber;
  IdHTTPServer.Active := True;
  Ok := True;
end;

procedure TDaemon1 .DataModuleCreate (Sender : TObject );
begin
  dbLock := SyncObjs.TCriticalSection.Create;
end;

procedure TDaemon1 .DataModuleDestroy (Sender : TObject );
begin
  FreeAndNil(dbLock);
end;

procedure TDaemon1 .DataModuleStop (Sender : TCustomDaemon ; var OK : Boolean );
begin
  IdHTTPServer.Active := False;
  Ok := True;
end;

procedure TDaemon1 .IdHTTPServerCommandGet (AContext : TIdContext ;
  ARequestInfo : TIdHTTPRequestInfo ; AResponseInfo : TIdHTTPResponseInfo );
var
  RequestBody, ReplyBody: String;
  xUserName, xTimestamp, xProgram, xVersion, xCompany: String;
  xAuthorized, xLicenseString: String;
  xStream: TMemoryStream;
begin
  RequestBody := httpRequestStreamToString(ARequestInfo, AResponseInfo);
  aResponseInfo.ContentText := 'Unrecognized request';
  AResponseInfo.ResponseNo := 400;
  try
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
  except
    on e: Exception do
    begin
      AResponseInfo.ResponseNo := 500;
      AResponseInfo.ContentText := e.Message {+ #10#13 + ExceptionStackListString(e)};
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

procedure TDaemon1 .IdHTTPServerCreatePostStream (AContext : TIdContext ;
  AHeaders : TIdHeaderList ; var VPostStream : TStream );
begin
  VPostStream := TMemoryStream.Create;
end;

function TDaemon1 .OpenDatabase : Boolean ;
begin
  CloseDatabase;
  SQLConnector.Driver := licenseOdbcDriver;
  SqlConnector.Params.Text := 'DBQ=' + licenseDatabaseName;
  SqlConnector.Connected := True;
  result := SqlConnector.Connected;
end;

procedure TDaemon1 .CloseDatabase ;
begin
  try
    SqlConnector.Connected := False;
  except
  end;
end;

function TDaemon1 .GetAuthorization : String ;
var
  xDt: TDateTime;
  Y, m, d: Word;
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
       or (GetUserName = 'JANBO$')
       or (GetUserName = 'RB347565$')
       or (GetUserName = 'RB926266$')
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

function TDaemon1 .generateLicense (aBase : String ): String ;
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

procedure TDaemon1 .SetAuthorization (aCompany , aLicenseExpirationDate ,
  aLicenseString : String );
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

procedure TDaemon1 .LogUsage (aUserName , aProgramName , aVersion : String );
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

procedure TDaemon1 .IntrospectIniXml ;
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
      licenseOdbcDriver := XmlValueByTagDef['OdbcDriver', 'Microsoft Access Driver (*.mdb, *.accdb)'];
      licenseDatabaseName := ExpandRelativeFileName(xIniFileName, XmlValueByTagDef['DatabaseName', '']);
    end;
  finally
    Free;
  end;
  Logger.Info (GetUserName + ' ' + licenseOdbcDriver + ' ' + licenseDatabaseName + ' ' + IntToStr(portNumber));
end;

function TDaemon1 .httpRequestStreamToString (
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

procedure TDaemon1 .WriteStringToStream (aString : String ;
  aStream : TMemoryStream );
begin
  aStream.Position := 0;
  aStream.Write(Pointer(aString)^, Length (aString));
  aStream.Position := 0;
end;

initialization
  RegisterDaemon ;
end.

