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
program apiUiServer;
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, CustApp
  , Interfaces
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
  , LazFileUtils
  , optionsunit
  ;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    se: TWsdlProject;
    scriptName, lstLogFileName: String;
    nFlush, cFlush: Integer;
    lstLogFile: TextFile;
    terminateAfterScript: Boolean;
    doDebug: Boolean;
    osUserName, CompanyName: String;
    LogUsageTime: TDateTime;
    procedure SetLogUsageTimer;
    function getContextProperty: String;
    function setContextProperty (aName: String): String;
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
var
  Application: TMyApplication;

function _mainSetContext (aName: String): String;
begin
  result := Application.setContextProperty(aName);
end;

function _mainGetContext: String;
begin
  result := Application.getContextProperty;
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
  if HasOption('?', lstLogOpt) then
  begin
    WriteLn('option ', lstLogOpt, ' ', GetOptionValue('?', lstLogOpt));
  end;
  if HasOption('?',scriptOpt) then
  begin
    WriteLn('option ', scriptOpt, ' ', GetOptionValue('?', scriptOpt));
  end;

  _wsdlSetContext := _mainSetContext;
  _wsdlGetContext := _mainGetContext;
  terminateAfterScript := HasOption('?',terminateOpt);
  if terminateAfterScript then
    WriteLn('option ', terminateOpt);
  if HasOption('?',openSslLocOpt) then
  begin
    openSslCertsFolder := GetOptionValue('?', openSslLocOpt);
    WriteLn('option ', openSslLocOpt, ' ', openSslCertsFolder);
  end;
  xmlio.doTrackXmlIO := HasOption('?',trackIOOpt);
  if xmlio.doTrackXmlIO then
    WriteLn('option ', trackIOOpt);
  doDebug := HasOption('?',debugOpt);
  if doDebug then
    WriteLn('option ', debugOpt);
  begin
    WriteLn('option ', scriptOpt, ' ', GetOptionValue('?', scriptOpt));
  end;
  if not HasOption('?',projectOpt) then
  begin
    WriteLn ('missing --project= option');
    Terminate;
    Exit;
  end;

  se.projectFileName := ExpandRelativeFileName(GetCurrentDirUTF8 + DirectorySeparator, GetOptionValue('?', projectOpt));
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
  nFlush := 50; // flush every 5 seconds, refresh 10 times a second
  cFlush := 0;
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
      if lstLogFileName <> '' then
      begin
        Inc (cFlush);
        if cFlush >= nFlush then
        begin
          Flush(lstLogFile);
          cFlush := 0;
        end;
      end;
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

function TMyApplication.setContextProperty(aName: String): String;
begin
  result := se.projectContext;
  se.projectContext := aName;
  xmlio.ProjectContext := aName;
end;

function TMyApplication.getContextProperty: String;
begin
  result := se.projectContext;
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
                  , Format ( '%s;%s;%s;%s;%s;%s;%d;%d,%s %d'
                           , [xsdFormatDateTime(xLog.InboundTimeStamp, @TIMEZONE_UTC)
                             ,   xsdFormatDateTime(xLog.OutboundTimeStamp, @TIMEZONE_UTC)
                             ,      xLog.DurationAsString
                             ,         xLog.StubActionAsString
                             ,            _servicename(xLog)
                             ,               _operationname(xLog)
                             ,                  Length (xLog.RequestBody)
                             ,                     Length (xLog.ReplyBody)
                             ,                        xlog.httpCommand
                             ,                           xLog.httpResponseCode
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
  WriteLn (ApplicationName, ' [switches]');
  WriteLn ('');
  WriteLn ('');
  WriteLn ('Example');
  WriteLn (ExeName, ' --project=myProject.svpr --', scriptOpt, '=setup');
  WriteLn;
  WriteLn ('This command will ...');
  WriteLn ('  start with opening project myProject.svpr');
  WriteLn ('  and execute the named project-script');
  WriteLn;
  WriteLn;
  WriteLn ('Switches');
  WriteLn ('  --', projectOpt);
  WriteLn ('     opens the named project');
  WriteLn ('  --', helpOpt);
  WriteLn ('     types this helpmessage');
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
  WriteLn ('  --', openSslLocOpt, '=');
  WriteLn ('     openSSL certificates folder');
  WriteLn ('');
  WriteLn ('');
end;

procedure TMyApplication .OnTerminateThread ;
begin
  if terminateAfterScript then
    Terminate;
end;


{$R *.res}

begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.



