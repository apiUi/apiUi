program apiServer;
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, CustApp
  , Interfaces
  , math
  , dateutils
  , sysutils
  , WsdlProjectz
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
  {$ifdef windows}
  , ActiveX
  {$endif}
  ;

type
  longOptsArrayType = array [0..6] of String;

const
  helpOpt = 'help';
  lstLogOpt = 'lstLog';
  scriptOpt = 'script';
  terminateOpt = 'terminate';
  trackIOOpt = 'trackIO';
  debugOpt = 'debug';
  contextOpt = 'context';
  longOpts: longOptsArrayType = ( helpOpt
                                , lstLogOpt + ':'
                                , scriptOpt + ':'
                                , contextOpt + ':'
                                , terminateOpt
                                , trackIOOpt
                                , debugOpt
                                );
type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    se: TWsdlProject;
    scriptName, lstLogFileName: String;
    lstLogFile: TextFile;
    terminateAfterScript: Boolean;
    doDebug: Boolean;
    osUserName, CompanyName: String;
    LogUsageTime: TDateTime;
    procedure SetLogUsageTimer;
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
  WriteLn(_progName, ' ', _xmlProgVersion);
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
  if HasOption('?', lstLogOpt) then
  begin
    WriteLn('option ', lstLogOpt, ' ', GetOptionValue('?', lstLogOpt));
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
  doDebug := HasOption('?',debugOpt);
  if doDebug then
    WriteLn('option ', debugOpt);
  se.projectFileName := ExpandRelativeFileName(GetCurrentDirUTF8 + DirectorySeparator, ParamStr(1));

  if (Copy (se.projectFileName, 1, 1) = '-')  // switch as first argument ??
{
  or (    (not FileExists(se.projectFileName))
      and (not DirectoryExistsUTF8(se.projectFileName))
     )
}
  then
  begin
    WriteLn ('First argument not an apiUi project name: ' + se.projectFileName);
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
  lstLogFileName := GetOptionValue(lstLogOpt);
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
  try
    ActivateCommand(True);
  except
    on e: Exception do
    begin
      WriteLn (e.Message);
      Terminate;
      Exit;
    end;
  end;
  if lstLogFileName <> '' then
  begin
    AssignFile(lstLogFile, lstLogFileName);
    Rewrite(lstLogFile);
  end;
  try
    if Assigned (sXml) then
    begin
      with TProcedureThread.Create(True, False, 0, se, se.ScriptExecute, sXml as TObject) do
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
        se.Licensed := True;
        SetLogUsageTimer;
      end;
    end;
    ActivateCommand(False);
    RefreshLogger;
  finally
    if lstLogFileName <> '' then
    begin
      Flush(lstLogFile);
      CloseFile(lstLogFile);
    end;
  end;
  Notify ('quit');
end;

procedure TMyApplication .SetLogUsageTimer ;
begin
  LogUsageTime := SysUtils.Date + 1 + (Random / (24 * 60));
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
    se.OpenFromFolders;
  end
  else
  begin
    if FileExistsUTF8(aFileName) then
      se.ImportFromFile
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
  function _unixms (aValue: TDateTime): Int64;
  begin
    Result:= Round ((AValue - UnixEpoch) * MSecsPerDay);
  end;
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
        if lstLogFileName <> '' then
        begin
          WriteLn ( lstLogFile
                  , Format ( '%d;%d;%s;%s;%s;%s;%d;%d'
                           , [ _unixms (xlog.InboundTimeStamp)
                             , _unixms (xlog.OutBoundTimeStamp)
                             , xLog.DurationAsString
                             , xLog.StubActionAsString
                             , _servicename(xLog)
                             , _operationname(xLog)
                             , Length (xLog.InboundBody)
                             , Length (xLog.OutboundBody)
                             ]
                           )
                  );
        end;
        if doDebug then
        begin
          WriteLn ( Format ( 'Request:%s%s%s%s'
                           , [ LineEnding
                             , xLog.RequestHeaders
                             , LineEnding
                             , xLog.RequestBody
                             ]
                           )
                  );
          WriteLn ( Format ( 'Response:%s%s%s%s'
                           , [ LineEnding
                             , xLog.ReplyHeaders
                             , LineEnding
                             , xLog.ReplyBody
                             ]
                           )
                  );
        end;
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
      se.displayedSnapshots.AddObject(xSnapshot.Name, xSnapshot);
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
  _progName := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(ParamStr(0)), '');
  _xmlProgVersion := xmlio.GetVersion;
  _xmlLicensed := True;
  DecryptString := doDecryptString;
  EncryptString := doEncryptString;
  se := TWsdlProject.Create;
  se.Notify := Notify;
  se.LogServerMessage := LogServerException;
  se.FoundErrorInBuffer := FoundErrorInBuffer;
  xmlz.OnNotify := se.Notify;
  xmlio.OnNotify := se.Notify;
  IntrospectIniXml;
  try
    se.Licensed := True;
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
  WriteLn (ExeName, ' myProject.svpr --', scriptOpt, '=setup');
  WriteLn;
  WriteLn ('This command will ...');
  WriteLn ('  start with opening project myProject.svpr');
  WriteLn ('  and execute the named project-script');
  WriteLn;
  WriteLn;
  WriteLn ('Switches');
  WriteLn ('  --', helpOpt);
  WriteLn ('     types this helpmessage');
  WriteLn ('  --', contextOpt, '=');
  WriteLn ('     sets a value for the "context" project property');
  WriteLn ('     (see apiUi menu Project->Properties)');
  WriteLn ('  --', lstLogOpt, '=');
  WriteLn ('     writes lst information to the named file');
  WriteLn ('  --', scriptOpt, '=');
  WriteLn ('     starts executing the named project-script');
  WriteLn ('  --', terminateOpt);
  WriteLn ('     terminates after executing the named project-script');
  WriteLn ('  --', trackIOOpt);
  WriteLn ('     notifies IO operations');
  WriteLn ('  --', debugOpt);
  WriteLn ('     types full requests and responses');
  WriteLn ('');
  WriteLn ('');
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



