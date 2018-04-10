program wsdlServer;
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
  , WsdlProjectz
  , wsdlcontrolz
  , Xmlz
  , xmlio
  , Ipmz
  , exceptionUtils
  , Logz
  , ExceptionLogz
  , snapshotz
  , Wsdlz
  , xmlxsdparser
  , HashUtilz
  , LazFileUtils
  ;

type
  longOptsArrayType = array [0..5] of String;

const
  helpOpt = 'help';
  portOpt = 'port';
  scriptOpt = 'script';
  terminateOpt = 'terminate';
  trackIOOpt = 'trackIO';
  contextOpt = 'context';
  longOpts: longOptsArrayType = ( helpOpt
                                , portOpt + ':'
                                , scriptOpt + ':'
                                , contextOpt + ':'
                                , terminateOpt
                                , trackIOOpt
                                );
type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    se: TWsdlProject;
    sc: TWsdlControl;
    scriptName: String;
    terminateAfterScript: Boolean;
    osUserName, CompanyName: String;
    LogUsageTime: TDateTime;
    procedure SetLogUsageTimer;
    function ValidateLicenseExpirationDate(eDt: String): Boolean;
    function doDecryptString(aString: AnsiString): AnsiString;
    function doEncryptString(aString: AnsiString): AnsiString;
    procedure HandleException(Sender: TObject; E: Exception);
    procedure Notify(const aString: String);
    procedure OnFinishedScript;
    function ReactivateCommand: String;
    function QuitCommand(aDoRaiseExceptions: Boolean): String;
    function RestartCommand: String;
    function ReloadDesignCommand: String;
    procedure ActivateCommand(aActivate: Boolean);
    procedure OpenProjectCommand(aFileName: String);
    procedure LogServerException(const Msg: String; aException: Boolean; E: Exception);
    procedure FoundErrorInBuffer(ErrorString: String; aObject: TObject);
    procedure RefreshLogger;
    procedure WriteHelp; virtual;
    function GetAuthorization : Boolean ;
    procedure OnTerminateThread;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  sXml: TXml;
begin
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
  if HasOption('?',contextOpt) then
  begin
    WriteLn('option ', contextOpt, ' ', GetOptionValue('?', contextOpt));
  end;
  if HasOption('?',portOpt) then
  begin
    WriteLn('option ', portOpt, ' ', GetOptionValue('?', portOpt));
  end;
  if HasOption('?',scriptOpt) then
  begin
    WriteLn('option ', scriptOpt, ' ', GetOptionValue('?', scriptOpt));
  end;
  se.projectContext := GetOptionValue(contextOpt);
  terminateAfterScript := HasOption('?',terminateOpt);
  if terminateAfterScript then
    WriteLn('option ', terminateOpt);
  xmlio.doTrackXmlIO := HasOption('?',trackIOOpt);
  if xmlio.doTrackXmlIO then
    WriteLn('option ', trackIOOpt);
  se.projectFileName := ParamStr(1);
  if (Copy (se.projectFileName, 1, 1) = '-')  // switch as first argument ??
  or (    (not FileExists(se.projectFileName))
      and (not DirectoryExistsUTF8(se.projectFileName))
     )
  then
  begin
    WriteLn ('First argument not a filename: ' + se.projectFileName);
    Terminate;
    Exit;
  end;
  try
    OpenProjectCommand(se.projectFileName);
  except
    on e: Exception do
    begin
      WriteLn (e.Message);
      Terminate;
      Exit;
    end;
  end;
  scriptName := GetOptionValue(scriptOpt);
  sXml := nil;
  if (scriptName <> '') then
  begin
    sXml := se.FindScript(scriptName);
    if not Assigned (sXml) then
    begin
      WriteLn ('Script ', scriptName, ' not found');
      Terminate;
      Exit;
    end;
  end;
  if HasOption('?', portOpt) then
    sc.portNumber := StrToInt(GetOptionValue(portOpt));
  try
    sc.Active := True;
    ActivateCommand(True);
  except
    on e: Exception do
    begin
      WriteLn (e.Message);
      Terminate;
      Exit;
    end;
  end;
  if Assigned (sXml) then
  begin
    with TProcedureThread.Create(True, False, se, se.ScriptExecute, sXml as TObject) do
    begin
      FreeOnTerminate := True;
      OnFinished := OnFinishedScript;
      Start;
    end;
  end;
  SetLogUsageTimer;
  while not Terminated do
  begin
    CheckSynchronize;
    RefreshLogger;
    Sleep (100);
    if Now > LogUsageTime then
    begin
      se.Licensed := GetAuthorization;
      SetLogUsageTimer;
    end;
  end;
  ActivateCommand(False);
  RefreshLogger;
  Terminate;
  Notify ('quit');
end;

procedure TMyApplication .SetLogUsageTimer ;
begin
  LogUsageTime := Date + 1 + (Random / (24 * 60));
end;

function TMyApplication .ValidateLicenseExpirationDate (eDt : String
  ): Boolean ;
var
  xDt: TDateTime;
  xYear, xMonth, xDay: Word;
begin
  // 2007-01-01
  // 1234567890
  xYear := StrToInt(Copy(eDt, 1, 4));
  xMonth := StrToInt(Copy(eDt, 6, 2));
  xDay := StrToInt(Copy(eDt, 9, 2));
  xDt := EncodeDate(xYear, xMonth, xDay);
  result := (Now < xDt);
  if not result then
  begin
    Raise Exception.Create('Your ' + _progName + ' license has expired on ' + DateToStr
        (xDt) + '.' + LineEnding + 'Please contact your ' + _progName + ' provider.');
  end;
end;

procedure TMyApplication .Notify (const aString : String );
begin
  WriteLn (xsdFormatDateTime(now, @TIMEZONE_UTC), ' notify: ', aString);
end;

procedure TMyApplication .OnFinishedScript;
begin
  if terminateAfterScript then
    Terminate;
end;

function TMyApplication.ReactivateCommand: String;
begin
  result := '';
  raise Exception.Create('Reactivate: Not implemented in ' + ExeName);
end;

function TMyApplication.QuitCommand(aDoRaiseExceptions: Boolean): String;
begin
  result := '';
  WriteLn (ExeName, ' received Quit command');
  Terminate;
end;

function TMyApplication.RestartCommand: String;
begin
  result := '';
  raise Exception.Create('Restart: Not implemented in ' + ExeName);
end;

function TMyApplication.ReloadDesignCommand: String;
begin
  result := '';
  OpenProjectCommand(se.projectFileName);
end;

procedure TMyApplication.ActivateCommand(aActivate: Boolean);
begin
  if aActivate <> se.IsActive then
  begin
    try
      se.Activate(aActivate);
      Notify (ifthen (se.IsActive, 'Is Active' , 'Is Inactive'));
    except
      on e: Exception do
        raise Exception.Create('Activate: ' + e.Message);
    end;
  end;
end;

procedure TMyApplication.OpenProjectCommand(aFileName: String);
var
  wasActive: Boolean;
begin
  wasActive := se.IsActive;
  ActivateCommand(False);
  se.projectFileName := aFileName;
  if DirectoryExistsUTF8(aFileName) then
  begin
    se.ProjectDesignFromString(se.OpenWithFolders (se.projectFileName), aFileName);
  end
  else
  begin
    if FileExistsUTF8(aFileName) then
      se.ProjectDesignFromString(ReadStringFromFile(aFileName), aFileName)
    else
      raise Exception.Create('No such file or folder: ' + aFileName);
  end;
  ActivateCommand(wasActive);
end;

procedure TMyApplication .LogServerException (const Msg : String ;
  aException : Boolean ; E : Exception );
begin
  if aException then
    WriteLn (ExeName, ' exception', LineEnding, Msg, LineEnding, ExceptionStackListString(E))
  else
    Notify (Msg);
end;

procedure TMyApplication .RefreshLogger ;
  function _time (aLog: TLog): String;
  begin
    if aLog.StubAction = saRequest then
      result := xsdFormatDateTime(aLog.OutboundTimeStamp, @TIMEZONE_UTC)
    else
      result := xsdFormatDateTime(aLog.InboundTimeStamp, @TIMEZONE_UTC);
  end;
  function _servicename (aLog: TLog): String;
  begin
    result := '';
    if Assigned (aLog.Operation) then
      result := aLog.Operation.WsdlService.Name;
  end;
  function _operationname (aLog: TLog): String;
  begin
    result := '';
    if Assigned (aLog.Operation) then
      result := aLog.Operation.Name;
  end;
  function _refreshLogging: Boolean;
  var
    x: Integer;
    xLog: TLog;
  begin
    result := False;
    for x := 0 to se.toDisplayLogs.Count - 1 do
    begin
      xLog := se.toDisplayLogs.LogItems[x];
      se.displayedLogs.SaveLog('', xLog);
      xLog.Nr := se.displayedLogs.Number;
      result := True;
      se.LogFilter.Execute(xLog);
      if xLog.PassesFilter then
      begin
        WriteLn ( Format ( '%s %s %s %s %s %s'
                         , [ _time (xLog)
                           , xLog.DurationAsString
                           , xLog.StubActionAsString
                           , _servicename(xLog)
                           , _operationname(xLog)
                           , xLog.CorrelationId
                           ]
                         )
                );
      end;
      if se.displayedLogsmaxEntries > -1 then
      while se.displayedLogs.Count > se.displayedLogsmaxEntries do
      begin
        se.displayedLogs.LogItems[0].displayRef := nil;
        se.displayedLogs.Delete(0);
      end;
    end;
    se.toDisplayLogs.Clear;
    se.toUpdateDisplayLogs.Clear;
  end;
  function _refreshExceptions: Boolean;
  var
    xLog: TExceptionLog;
    x: Integer;
  begin
    result := False;
    for x := 0 to se.toDisplayExceptions.Count - 1 do
    begin
      xLog := se.toDisplayExceptions.EventItems[x];
      se.displayedExceptions.AddObject('', xLog);
      result := True;
    end;
    se.toDisplayExceptions.Clear;
  end;
  function _refreshSnapshots: Boolean;
  var
    xSnapshot: TSnapshot;
    x: Integer;
  begin
    result := False;
    for x := 0 to se.toDisplaySnapshots.Count - 1 do
    begin
      xSnapshot := se.toDisplaySnapshots.SnapshotItems[x];
      se.displayedSnapshots.AddObject('', xSnapshot);
      WriteLn ( Format ( '%s %s %s'
                       , [ xsdFormatDateTime(xSnapshot.timeStamp, @TIMEZONE_UTC)
                         , 'created snapshot '
                         , xSnapshot.Name
                         ]
                       )
              );
      result := True;
    end;
    se.toDisplaySnapshots.Clear;
  end;
begin
  if not Assigned (se) then Exit;
  se.AcquireLogLock;
  try
    if se.doClearLogs then
    begin
      {
        clear means of displaying log data here
      }
      se.doClearLogs := False;
    end;
    if se.doClearSnapshots then
    begin
      {
        clear means of displaying snapshot data here
      }
      se.doClearSnapshots := False;
    end;
    _refreshLogging;
    _refreshExceptions;
    _refreshSnapshots;
  finally
    se.ReleaseLogLock;
  end;
end;

function TMyApplication.doDecryptString(aString: AnsiString): AnsiString;
begin
  result := DecryptPassword(aString);
end;

function TMyApplication.doEncryptString(aString: AnsiString): AnsiString;
begin
  result := EncryptPassword(aString);
end;

procedure TMyApplication .HandleException (Sender : TObject ; E : Exception );
var
  s: String;
begin
  try
    s := ExceptionStackListString (E);
  except
  end;
  writeln (Format('%s: Message: %s%s', [xsdNowAsDateTime, e.Message, s]));
end;

procedure TMyApplication.FoundErrorInBuffer(ErrorString: String; aObject: TObject);
begin
  (aObject as TIpmItem).Value := '?' + _progName + ' Error found: ' + ErrorString;
end;


constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  _xmlUserName := GetUserName;
  _xmlProgName := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(ParamStr(0)), '');
  _xmlProgVersion := xmlio.GetVersion;
  _xmlLicensed := True;
  DecryptString := doDecryptString;
  EncryptString := doEncryptString;
  se := TWsdlProject.Create;
  sc := TWsdlControl.Create;
  sc.se := se;
  se.Notify := Notify;
  se.LogServerMessage := LogServerException;
  se.FoundErrorInBuffer := FoundErrorInBuffer;
  sc.OnActivateEvent := ActivateCommand;
  sc.OnOpenProjectEvent := OpenProjectCommand;
  sc.OnReactivateEvent := ReactivateCommand;
  sc.OnQuitEvent := QuitCommand;
  sc.OnRestartEvent := RestartCommand;
  sc.OnReloadDesignEvent := ReloadDesignCommand;
  xmlz.OnNotify := se.Notify;
  xmlio.OnNotify := se.Notify;
  IntrospectIniXml;
  try
    se.Licensed := GetAuthorization;
  except
    on e: Exception do
    begin
      WriteLn (e.Message);
      Terminate;
      Exit;
    end;
  end;
  Randomize;
  SetLogUsageTimer;
end;

destructor TMyApplication.Destroy;
begin
  FreeAndNil(se);
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  WriteLn (ExeName);
  WriteLn (ApplicationName, ' projectFileName [switches]');
  WriteLn ('');
  WriteLn ('');
  WriteLn ('Example');
  WriteLn (ExeName, ' myProject.wsdlStub --port=6161');
  WriteLn;
  WriteLn ('This command will ...');
  WriteLn ('  start with opening project myProject.wsdlStub');
  WriteLn ('  and listen on port 6161 for wsdlStub webservice calls (remoteControl: default 3738)');
  WriteLn;
  WriteLn;
  WriteLn ('Switches');
  WriteLn ('  --', helpOpt);
  WriteLn ('     types this helpmessage');
  WriteLn ('  --', contextOpt, '=');
  WriteLn ('     sets a value for the "context" project property');
  WriteLn ('     (see wsdlStub menu Project->Properties)');
  WriteLn ('  --', portOpt, '=');
  WriteLn ('     overrules the portnumber for the wsdlServer webservice');
  WriteLn ('  --', scriptOpt, '=');
  WriteLn ('     starts executing the named project-script');
  WriteLn ('  --', terminateOpt);
  WriteLn ('     terminates after executing the named project-script');
  WriteLn ('  --', trackIOOpt);
  WriteLn ('     notifies IO operations (currently only I)');
  WriteLn ('');
  WriteLn ('');
end;

function TMyApplication.GetAuthorization : Boolean ;
var
  xTimestamp, xKey, xLicensed, xExpireDate: String;
begin
  result := False;
  xTimestamp := xsdNowAsDateTime;
  with TXml.CreateAsString ('getAuthorization', '') do
  try
    AddXml (TXml.CreateAsString('UserName', osUserName));
    AddXml (TXml.CreateAsString('TimeStamp', xTimestamp));
    AddXml (TXml.CreateAsString('Program', _ProgName));
    AddXml (TXml.CreateAsString('Version', _xmlProgVersion));
    AddXml (TXml.CreateAsString('key', Sha1 (xTimestamp + '_JanBo')));
    LoadFromString (HttpPostDialog(Text, authorizationServerEndpoint), nil);
    xLicensed := Items.XmlValueByTag['authorized'];
    xExpireDate := Items.XmlValueByTag['expireDate'];
    CompanyName := Items.XmlValueByTag['licensee'];
    xKey := Items.XmlValueByTag['key'];
    if (xKey <> Sha1 ( osUserName
                    + xTimestamp
                    + '^abra^'
                    + xLicensed
                    )) then
      raise Exception.Create ('Received inalid reply from Authorization server');
    if (xLicensed = 'true') then
      result := ValidateLicenseExpirationDate(xExpireDate) // warning...
    else
      raise Exception.CreateFmt('Not authorized%sLicense expiredate: %s', [LineEnding, xExpireDate]);
  finally
    Free;
  end;
end;

procedure TMyApplication .OnTerminateThread ;
begin
  if terminateAfterScript then
    Terminate;
end;

var
  Application: TMyApplication;

{$R *.res}

begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.


initialize
