program hcbProxy;
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
  , Xmlz
  , exceptionUtils
  , LazFileUtils
  , IdHTTPServer
  , IdCustomHTTPServer
  , IdContext
  , IdHeaderList
  , IdHTTP
  , IdURI
  , IdGlobal
  , GZIPUtils
  , SyncObjs
  , RegExpr
  ;

type
  longOptsArrayType = array [0..4] of String;

const
  helpOpt = 'help';
  serverPortOpt = 'serverPort';
  configPortOpt = 'configPort';
  headerNamerOpt = 'headerName';
  hostRegExpOpt = 'hostRegExp';
  longOpts: longOptsArrayType = ( helpOpt
                                , serverPortOpt + ':'
                                , configPortOpt + ':'
                                , headerNamerOpt + ':'
                                , hostRegExpOpt + ':'
                                );
type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    procedure HandleException(Sender: TObject; E: Exception);
    procedure Notify(const aString: String);
    procedure WriteHelp; virtual;
    procedure OnTerminateThread;
    procedure httpConfigureServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
    procedure httpProxyServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
    procedure httpProxyServerCreatePostStream(AContext: TIdContext;
      AHeaders: TIdHeaderList;var VPostStream: TStream);
  public
    mainPortnumber, configPortnumber, headerName, hostRegExp: String;
    httpMainServer, httpConfigServer: TIdHTTPServer;
    users: TStringList;
    Lock: TCriticalSection;
    procedure RemoveSomeHttpHeaders (aHeaderList: TIdHeaderList);
    function httpRequestStreamToString(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): String;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  sXml: TXml;
begin
  mainPortnumber := '8080';
  configPortnumber := '3739';
  headerName := 'pegaUserId';
  hostRegExp := '.*';
  WriteLn(_xmlProgName, ' ', _xmlProgVersion);
  if ParamCount = 0 then
  begin
    WriteLn(ExeName, ' --', helpOpt, ' for more information');
    Terminate;
    Exit;
  end;

  ErrorMsg := CheckOptions('',longOpts);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  if HasOption('?',helpOpt) then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  if HasOption('?', serverPortOpt) then
    mainPortnumber := GetOptionValue(serverPortOpt);
  if HasOption('?', configPortOpt) then
    configPortnumber := GetOptionValue(configPortOpt);
  if HasOption('?', headerNamerOpt) then
    headerName := GetOptionValue(headerNamerOpt);
  if HasOption('?', hostRegExpOpt) then
    hostRegExp := GetOptionValue(hostRegExpOpt);

  try
    httpConfigServer.DefaultPort := StrToInt(configPortnumber);
    httpConfigServer.OnCommandGet := httpConfigureServerCommandGet;
    httpConfigServer.OnCommandOther := httpConfigureServerCommandGet;
    httpConfigServer.Active := True;
    httpMainServer.DefaultPort := StrToInt(mainPortnumber);
    httpMainServer.OnCommandGet := httpProxyServerCommandGet;
    httpMainServer.OnCommandOther := httpProxyServerCommandGet;
    httpMainServer.Active := True;
    while not Terminated do
    begin
      CheckSynchronize;
      Sleep (100);
    end;
    httpConfigServer.Active := False;
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
  WriteLn ({xsdFormatDateTime(now, @TIMEZONE_UTC),} ' notify: ', aString);
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  try
    users := TStringList.Create;
    httpConfigServer := TIdHTTPServer.Create;
    httpMainServer := TIdHTTPServer.Create;
    Lock := TCriticalSection.Create;
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
  FreeAndNil(users);
  FreeAndNil(httpConfigServer);
  FreeAndNil(httpMainServer);
  FreeAndNil(Lock);
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  WriteLn ('');
  WriteLn ('');
  WriteLn (ExeName);
  WriteLn (ApplicationName, ' [switches]');
  WriteLn ('');
  WriteLn ('');
  WriteLn ('Example');
  WriteLn (ExeName, ' --serverPort=8080 --configPort=3739 --headerName=pegaUserId --hostRegExp=.*');
  WriteLn;
  WriteLn;
  WriteLn ('Switches');
  WriteLn ('  --', helpOpt);
  WriteLn ('     types this helpmessage');
  WriteLn ('  --', serverPortOpt, '=');
  WriteLn ('     sets the main portnumber (default ' , mainPortnumber, ')');
  WriteLn ('  --', configPortOpt, '=');
  WriteLn ('     sets the config portnumber (default ' , configPortnumber, ')');
  WriteLn ('  --', headerNamerOpt);
  WriteLn ('     name for the http header used as base for routing (default ' , headerName, ')');
  WriteLn ('  --', hostRegExpOpt);
  WriteLn ('     regular expression to check host (default ' , hostRegExp, ')');
  WriteLn ('');
  WriteLn ('');
end;

procedure TMyApplication.OnTerminateThread;
begin

end;

procedure TMyApplication.httpConfigureServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
var
  xUid, xHost: String;
begin
  try
    AResponseInfo.ResponseNo := 200; // START OPTIMISTIC
    if (LowerCase(ARequestInfo.Command) <> 'post')
    or (ARequestInfo.Document <> '/configure/user')
    then
    begin
      AResponseInfo.ResponseNo := 500;
      AResponseInfo.ContentText := 'post for /configure/user expected';
    end;
    with TXml.Create do
    try
      LoadJsonFromString(httpRequestStreamToString(ARequestInfo, AResponseInfo), nil);
      if Name <> 'json' then
      begin
        AResponseInfo.ResponseNo := 500;
        AResponseInfo.ContentText := 'json expected';
        exit;
      end;
      xUid := Items.XmlValueByTag['uid'];
      if xUid = '' then
      begin
        AResponseInfo.ResponseNo := 500;
        AResponseInfo.ContentText := 'no uid found';
        exit;
      end;
      xHost := Items.XmlValueByTag['host'];
      if xHost = '' then
      begin
        AResponseInfo.ResponseNo := 500;
        AResponseInfo.ContentText := 'no host found';
        exit;
      end;
      if not ExecRegExpr('^' + hostRegExp + '$', xHost) then
      begin
        AResponseInfo.ResponseNo := 500;
        AResponseInfo.ContentText := Format ('host "%s" does not match regexp "%s"', [xHost, hostRegExp]);
        exit;
      end;
      Lock.Acquire;
      try
        users.Values [xUid] := xHost;
        Notify (Format('endpoint changed for Uid: "%s" to "%s"', [xUid, xHost]));
      finally
        Lock.Release;
      end;
    finally
      Free;
    end;
  except
    on e: Exception do
    begin
      AResponseInfo.ResponseNo := 500;
      AResponseInfo.ContentText := e.Message;
    end;
  end;
end;

procedure TMyApplication.httpProxyServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo);
var
  x: Integer;
  xUri, xHost: String;
  xHeaderValue: String;
  sStream, dStream: TStream;
begin
  try
    try
      x := ARequestInfo.RawHeaders.IndexOfName(headerName);
      if x < 0 then
        raise Exception.Create('Http header "' + headerName + '" not found');
      xHeaderValue := ARequestInfo.RawHeaders.Values[headerName];

      Lock.Acquire;
      try
        xHost := users.Values[xHeaderValue];
      finally
        Lock.Release;
      end;
      if (xHost = '')
      then
        raise Exception.CreateFmt('Unknown % "%s"', [headerName, xHeaderValue]);

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

procedure TMyApplication.httpProxyServerCreatePostStream(AContext: TIdContext;
  AHeaders: TIdHeaderList;var VPostStream: TStream);
begin
  VPostStream := TMemoryStream.Create;
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

function TMyApplication.httpRequestStreamToString(
  ARequestInfo: TIdHTTPRequestInfo;AResponseInfo: TIdHTTPResponseInfo): String;
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

var
  Application: TMyApplication;

{$R *.res}

begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.



