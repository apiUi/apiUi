// currently at most 1 project due to what's in wsdlz.initialize, should be held by project
unit WsdlProjectz;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  AdoDb,
{$ELSE}
{$ENDIF}
  Classes
   , ParserClasses
   , Xmlz
   , Xsdz
   , Bind
   , Ipmz
   , IpmTypes
   , igGlobals
{$ifndef FPC}
  , jclDebug
{$endif}
  , IdGlobal
  , IdCustomHTTPServer
  , IdSocketHandle
  , IdCmdTCPServer
  , IdHTTPProxyServer
  , IdHTTP
  , IdMessage
  , IdHeaderList
  , IdSMTP
  , IdURI
  , IdSSLOpenSSL
  , IdContext
  , IdCommandHandlers
  , Wsdlz
  , SwiftUnit
  , SysUtils
  , MqInterface
  , MqApi
  , StompInterface
  , StompTypes
  , TacoInterface
  , IdPOP3Server
  , IdReplyPOP3
  , IdSMTPServer
  , IdHTTPServer
  , Listenerz
  , Forms
  , Dialogs
  , Controls
  , FileUtil
  , Logz
  , snapshotz
  , ExceptionLogz
  , SyncObjs
  , ClaimListz
  ;

type TCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);
type TProcedure = procedure of Object;
type TProcedureB = procedure (arg: Boolean) of Object;
type TProcedureS = procedure (arg: String) of Object;
type TProcedureXX = procedure (arg, arg2: Extended) of Object;
type TProcedureOperation = procedure (arg: TWsdlOperation) of Object;
type TProcedureOperationS = procedure (arg: TWsdlOperation; arg2: String) of Object;
type TProcedureObject = procedure (arg: TObject) of Object;
type TProcedureClaimableObjectList = procedure (arg: TClaimableObjectList) of Object;
type TOnFoundErrorInBufferEvent = procedure (aErrorString: String; aObject: TObject) of Object;
type TOnEvent = procedure of Object;
type TOnNotify = procedure (const aString: String) of Object;
type TOnLogEvent = procedure (aLog: TLog) of Object;
type TOnStringEvent = procedure (const Msg: String; aException: Boolean; E: Exception) of Object;
type TBooleanFunction = function: Boolean of Object;
type TStringFunction = function: String of Object;
type TStringFunctionBoolean = function (arg: Boolean): String of Object;
type TBooleanFunctionString = function (arg: String): Boolean of Object;


type

  { TWsdlProject }

  TWsdlProject = class
  private
    fIsActive: Boolean;
    fAbortPressed: Boolean;
    fLogLock: TCriticalSection;
    fClearedLogs: TLogList;
    fClearedSnapshots: TSnapshotList;
    fTacoInterface: TTacoInterface;
    function GetAbortPressed: Boolean;
    function getDoClearSnapshots : Boolean ;
    function getDoClearLogs : Boolean ;
    function SendNoneMessage ( aOperation: TWsdlOperation
                             ; aMessage: String
                             ): String;
    function SendHttpMessage ( aOperation: TWsdlOperation
                             ; aMessage: String
                             ; var aReqHeader, aRpyHeader, aResponseCode: String
                             ): String;
    procedure POP3ServerCheckUser(aContext: TIdContext;
      aServerContext: TIdPOP3ServerContext);
    procedure POP3ServerRetrieve(aCmd: TIdCommand; AMsgNo: Integer);
    procedure POP3ServerList(aCmd: TIdCommand; AMsgNo: Integer);
    procedure POP3ServerStat(aCmd: TIdCommand; out oCount: Integer; out oSize: Int64);
    procedure POP3ServerDelete(aCmd: TIdCommand; AMsgNo: Integer);
    procedure HTTPProxyServerAfterCommandHandler(ASender: TIdCmdTCPServer;
      AContext: TIdContext);
    procedure HTTPProxyServerHTTPBeforeCommand(
      AContext: TIdHTTPProxyServerContext);
    procedure HTTPProxyServerHTTPDocument(
      AContext: TIdHTTPProxyServerContext; var VStream: TStream);
    procedure HTTPServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HttpServerBmtpCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HTTPServerCreatePostStream(AContext: TIdContext;
      AHeaders: TIdHeaderList; var VPostStream: TStream);
    procedure HTTPServerCommandPutPut(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HTTPServerCommandGetGet(aLog: TLog; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HTTPServerCommandTrace(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure setDoClearSnapshots (AValue : Boolean );
    procedure setDoClearLogs (AValue : Boolean );
    procedure setOnNeedTacoHostData (AValue : TOnNeedTacoInterfaceData );
    procedure setOnTacoAutorize (AValue : TNotifyEvent );
    procedure SMTPServerMailFrom(ASender: TIdSMTPServerContext;
      const AAddress: string; AParams: TStrings; var VAction: TIdMailFromReply);
    procedure SMTPServerMsgReceive(ASender: TIdSMTPServerContext; AMsg: TStream;
      var VAction: TIdDataReply);
    procedure SMTPServerRcptTo(ASender: TIdSMTPServerContext;
      const AAddress: string; AParams: TStrings; var VAction: TIdRCPToReply;
      var VForward: string);
    procedure SMTPServerReceived(ASender: TIdSMTPServerContext;
      var AReceived: string);
    procedure SMTPServerUserLogin(ASender: TIdSMTPServerContext;
      const AUsername, APassword: string; var VAuthenticated: Boolean);
    procedure doCoverageReport (aReport: TSnapshot);
    function ProcessInboundReply(aLogItem, rLogItem: TLog): String;
    procedure SetAbortPressed(const Value: Boolean);
    procedure InitSpecialWsdls;
  public
    projectProperties: TStringList;
    ppLock: TCriticalSection;
    doDisplayLog: Boolean;
    uiInvalid: Boolean;
    ProgressMax, ProgressPos: Integer;
    doCloneOperations: Boolean;
    OnRequestViolatingSchema, OnRequestViolatingAddressPath: TOnRequestViolating;
    DatabaseConnectionSpecificationXml: TXml;
    DbsDatabaseName, DbsType, DbsHostName, DbsParams, DbsUserName, DbsPassword, DbsConnectionString: String;
    FreeFormatWsdl, XsdWsdl, CobolWsdl, SwiftMtWsdl: TWsdl;
    FreeFormatService: TWsdlService;
    DebugOperation: TWsdlOperation;
    Wsdls, wsdlNames: TStringList;
    Scripts: TXml;
    DisplayedLogColumns: TStringList;
    projectFileName, LicenseDbName: String;
    displayedExceptions, toDisplayExceptions: TExceptionLogList;
    displayedLogs, toDisplayLogs, toUpdateDisplayLogs, archiveLogs, AsynchRpyLogs: TLogList;
    displayedSnapshots, toDisplaySnapshots: TSnapshotList;
    displayedLogsmaxEntries: Integer;
    CompareLogOrderBy: TCompareLogOrderBy;
    ShowLogCobolStyle: TShowLogCobolStyle;
    unknownOperation: TWsdlOperation;
    LogFilter: TLogFilter;
    refreshNr: Integer;
    refreshCheck: String;
    scriptErrorCount: Integer;
    EnvironmentList, EnvVars: TStringList;
    StubChanged, StubRead, Licensed: Boolean;
    doUseMQ: Boolean;
    NumberOfActiveMqs: Integer;
    mqUse: TMQUse;
    mqMaxWorkingThreads: Integer;
    mqCurWorkingThreads: Integer;
    mmqqMqInterface: TMqInterface;
    StompInterface: TStompInterface;
    mqGetThreads: TStringList;
    Listeners: TListeners;
    doValidateRequests, doValidateReplies, doCheckExpectedValues: Boolean;
    ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn, ignoreOrderOn, regressionSortColumns: TStringList;
    ignoreCoverageOn: TStringList;
    notStubbedExceptionMessage: String;
    FoundErrorInBuffer : TOnFoundErrorInBufferEvent;
    OnDebugOperationEvent: TOnEvent;
    OnStartBlockingThread, OnTerminateBlockingThread, OnStartNonBlockingThread, OnTerminateNonBlockingThread: TOnEvent;
    Notify: TOnNotify;
    LogServerMessage: TOnStringEvent;
    doViaProxyServer: Boolean;
    ViaProxyServer: String;
    ViaProxyPort: Integer;
    HTTPServer, HttpServerSSL, HttpServerBmtp: TIdHTTPServer;
    HTTPProxyServer: TIdHTTPProxyServer;
    SMTPServer, SMTPServerSSL: TIdSMTPServer;
    POP3Server: TIdPOP3Server;
    SMTPOpenSSL: TIdServerIOHandlerSSLOpenSSL;
    ServerOpenSSL: TIdServerIOHandlerSSLOpenSSL;
    OnRestartEvent: TStringFunction;
    OnReactivateEvent: TStringFunction;
    OnReloadDesignEvent: TStringFunction;
    PublishDescriptions: Boolean;
    OperationsWithEndpointOnly: Boolean;
    SaveRelativeFileNames: Boolean;
    CurrentFolder, ReferenceFolder, ReportsFolder: String;
    FocusOperationName, FocusOperationNameSpace: String;
    FocusMessageIndex: Integer;
    OnBooleanDialog: TBooleanFunctionString;
    procedure doRegressionReport (aReport: TSnapshot);
    procedure DatabaseConnectionSpecificationFromXml;
    procedure UpdateOperationAliasses;
    procedure AcquireLogLock;
    procedure ReleaseLogLock;
    procedure DisplayLog (aString: String; aLog: TLog);
    procedure DisplayReport (aString: String; aReport: TSnapshot);
    procedure WriteStringToStream (aString: String; aStream: TMemoryStream);
    function mergeUri (puri, suri: String): String;
    function freeFormatOperationsXml: TXml;
    procedure freeFormatOperationsUpdate (aXml: TXml);
    procedure operationRecognitionUpdate (aOperation: TWsdlOperation; aList: TStringList; aXml: TXml);
    function operationRecognitionXml(aLabel: String; aType: TRecognitionType; aSl: TStringList): TXml;
    function cobolOperationsXml: TXml;
    procedure cobolOperationsUpdate (aXml: TXml; aMainFileName: String);
    function xsdOperationsXml(aMainFileName: String): TXml;
    procedure xsdOperationsUpdate (aXml: TXml; aMainFileName: String);
    function swiftMtOperationsXml: TXml;
    procedure swiftMtOperationsUpdate (aXml: TXml; aMainFileName: String);
    function CreateScriptOperation (aScript: TXml): TWsdlOperation;
    procedure ScriptExecute(aScript: TObject);
    function CreateSnapshot (aName, aFileName, aRefFileName: String; aDoSave, aDoRun: Boolean): TSnapshot;
    procedure CreateSummaryReport (aName: String);
    procedure CreateCoverageReport(aDoRun: Boolean);
    function FindScript (aName: String): TXml;
    procedure ScriptsClear;
    procedure DefaultDisplayMessageData;
    function ReactivateCommand: String;
    function RestartCommand: String;
    function ReloadDesignCommand: String;
    procedure ExecuteAllOperationRequests(aOperation: TWsdlOperation);
    procedure OpenMessagesLog (aString: String; aIsFileName, aPrompt: Boolean; aLogList: TLogList);
    procedure EnvironmentListClear;
    procedure mqOnNewThread ( Sender: TObject);
    procedure mqStubMessage ( Sender: TObject
                            ; aHeader, aBody: String
                            ; aRfhHeader: AnsiString
                            ; MsgType: MQLONG
                            ; MsgDesc: MQMD
                            ; MqReturnCode: String
                            );
    function MessagesRegressionReportAsXml(aReferenceFileName: String; aPromptUser: Boolean): TXml;
    procedure CheckExpectedValues(aLog: TLog; aOperation: TWsdlOperation; aDoCheck: Boolean);
    procedure UpdateMessageRow (aOperation: TWsdlOperation; aMessage: TWsdlMessage);
    procedure DelayMS (aDelayMS: Integer);
    procedure CreateLogReply (aLog: TLog; var aProcessed: Boolean; aIsActive: Boolean);
    procedure Clean;
    procedure TacoPingPong;
    function ProjectDesignAsString (aMainFileName: String): String;
{}
{}
    function SendOperationMessage ( aOperation: TWsdlOperation
                         ; aMessage: String
                         ): String;
    function SendOperationMqMessage ( aOperation: TWsdlOperation
                           ; aMessage: String
                           ; var aMqHeaderAsText: String
                           ): String;
    function SendOperationStompMessage ( aOperation: TWsdlOperation
                                       ; aMessage: String
                                       ; var aRequestHeader: String
                                       ; var aReplyHeader: String
                                       ): String;
    function SendOperationSmtpMessage ( aOperation: TWsdlOperation
                                      ; aMessage: String
                                      ; var aRequestHeader: String
                                      ; var aReplyHeader: String
                                      ): String;
    function SendOperationTacoMessage ( aOperation: TWsdlOperation
                                      ; aMessage: String
                                      ; var aRequestHeader: String
                                      ; var aReplyHeader: String
                                      ): String;
    function RedirectCommandStomp ( aCommand: String
                                  ; aHost: String
                                  ; aPort: Integer
                                  ; aDestination: String
                                  ; aReplyTo: String
                                  ; aTimeOut: Integer
                                  ): String;
    function RedirectCommandMQ ( aCommand: String
                               ; aPutManager: String
                               ; aPutQueue: String
                               ; aReplyToManager: String
                               ; aReplyToQueue: String
                               ; aGetManager: String
                               ; aGetQueue: String
                               ; aTimeOut: Integer
                               ): String;
    function RedirectCommandURC (aCommand: String): String;
    function RedirectCommandHTTP ( aCommand, aStubAddress, aDocument, aSoapAction: String): String;
    function RedirectCommandString ( aCommand: String; aAddress, aSoapAction: String): String;
    function CreateLogReplyPostProcess (aLogItem: TLog; aOperation: TWsdlOperation): String;
    procedure SendAsynchReply ( aLog: TLog);
    function SendMessage ( aOperation: TWsdlOperation
                             ; aRequest: TWsdlMessage
                             ; aCorrelationId: String
                             ): String;
    function SendMessageLater ( aOperation: TWsdlOperation
                                  ; aRequest: TWsdlMessage
                                  ; aCorrelationId: String
                                  ; aLater: Integer
                                  ): String;
    procedure FindRequestReply (aLog: TLog; aDocument, aString: String; var isRequest: Boolean);
    procedure FindReply (aLog: TLog; aDocument, aRequest: String; var aOperation: TWsdlOperation; var aReply: TWsdlMessage; var aCorrelationId: String);
    function FindXmlOperationOnReply (aXml: TXml): TWsdlOperation; Overload;
    function FindCcbOperationOnReply (aCobolString: String): TWsdlOperation; Overload;
    function FindOperationOnReply (aString: String): TWsdlOperation; Overload;
    function FindXmlOperationOnRequest (aDocument: String; aXml: TXml): TWsdlOperation;
    function FindCcbOperationOnRequest (aLog: TLog; aCobolString: String): TWsdlOperation;
    function FindOperationOnDocument (aDocument: String): TWsdlOperation;
    function FindOperationOnRequest (aLog: TLog; aDocument, aString: String; aDoClone: Boolean): TWsdlOperation;
    function RedirectUnknownOperation (aLog: TLog): String;
    function CreateReply ( aLog: TLog
                         ; aDocument, aRequest: String
                         ; var aOperation: TWsdlOperation
                         ; var aReply: TWsdlMessage
                         ; var aCorrelationId: String
                         ; var isAsynchronous: Boolean
                         ; aIsActive: Boolean
                         ): String;
    function ProjectLogOptionsAsXml: TXml;
    function ProjectScriptsAsXml: TXml;
    procedure ProjectScriptsFromXml (aXml: TXml);
    function ProjectOptionsLogDisplayedColumnsAsXml: TXml;
    function BooleanPromptDialog (aPrompt: String): Boolean;
    function WsdlOpenFile (aName: String; aElementsWhenRepeatable: Integer): TWsdl;
    procedure RefuseHttpConnectionsThreaded (aLater, aTime: Extended);
    procedure SaveLogs (aFileName: String);
    procedure SaveSnapshots (aName: String);
    procedure UpdateReplyColumns (aOperation: TWsdlOperation);
    procedure ProjectOptionsLogDisplayedColumnsFromXml(aXml: TXml);
    procedure ProjectLogOptionsFromXml(aXml: TXml);
    function ProjectOptionsAsXml (aRelativeFilenames: Boolean; aFileName: String): TXml;
    procedure ProjectOptionsFromXml(aXml: TXml);
    procedure ProjectOptions36FromXml (aXml: TXml);
    procedure HaveStompFrame (aStompInterface: TStompInterface; aQueue: String; aFrame: IStompFrame);
    procedure ProjectDesignFromString (aString, aMainFileName: String);
    procedure PrepareAllOperations (aLogServerException: TOnStringEvent);
    procedure Activate (aActive: Boolean);
    procedure Clear;
    function httpRequestStreamToString(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): String;
    {$IFnDEF FPC}
    procedure ADOConnectionWillConnect(Connection: TADOConnection;
      var ConnectionString, UserID, Password: WideString;
      var ConnectOptions: TConnectOption; var EventStatus: TEventStatus);
    {$ENDIF}
    property OnNeedTacoHostData: TOnNeedTacoInterfaceData write setOnNeedTacoHostData;
    property OnTacoAutorize: TNotifyEvent write setOnTacoAutorize;
    property doClearLogs: Boolean read getDoClearLogs write setDoClearLogs;
    property doClearSnapshots: Boolean read getDoClearSnapshots write setDoClearSnapshots;
    property IsActive: Boolean read fIsActive;
    property abortPressed: Boolean read fAbortPressed write SetAbortPressed;
    constructor Create;
    destructor Destroy; Override;
  end;

  { TProcedureThread }

  TProcedureThread = class(TThread)
  private
    fProject: TWsdlProject;
    fProcedure: TProcedure;
    fProcedureString: TProcedureS;
    fProcedureXX: TProcedureXX;
    fProcedureOperation: TProcedureOperation;
    fProcedureObject: TProcedureObject;
    fProcedureClaimableObjectList: TProcedureClaimableObjectList;
    fString: String;
    fExtended, fExtended2: Extended;
    fOperation: TWsdlOperation;
    fObject: TObject;
    fClaimableObjectList: TClaimableObjectList;
    fBlocking: Boolean;
    fOnFinished: TOnEvent;
  protected
    procedure Execute; override;
  public
    property OnFinished: TOnEvent read fOnFinished write fOnFinished;
    constructor Create ( aSuspended: Boolean
                       ; aBlocking: Boolean
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedure
                       ); overload;
    constructor Create ( aSuspended: Boolean
                       ; aBlocking: Boolean
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedureS
                       ; aString: String
                       ); overload;
    constructor Create ( aSuspended: Boolean
                       ; aBlocking: Boolean
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedureXX
                       ; aExtended, aExtended2: Extended
                       ); overload;
    constructor Create ( aSuspended: Boolean
                       ; aBlocking: Boolean
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedureOperation
                       ; aOperation: TWsdlOperation
                       ); overload;
    constructor Create ( aSuspended: Boolean
                       ; aBlocking: Boolean
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedureObject
                       ; aObject: TObject
                       ); overload;
    constructor Create ( aSuspended: Boolean
                       ; aBlocking: Boolean
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedureClaimableObjectList
                       ; aClaimableObjectList: TClaimableObjectList
                       ); overload;
  end;

  TSendAsynchReplyThread = class(TThread)
  private
    fProject: TWsdlProject;
    fLog: TLog;
  protected
    procedure Execute; override;
  public
    constructor Create (aProject: TWsdlProject; aLog: TLog);
  end;

  TSendSoapRequestThread = class(TThread)
  private
    fProject: TWsdlProject;
    fOperation: TWsdlOperation;
    fMessage: TWsdlMessage;
    fCorrelation: String;
    fLater: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create ( aProject: TWsdlProject
                       ; aOperation: TWsdlOperation
                       ; aMessage: TWsdlMessage
                       ; aCorrelation: String
                       ; aLater: Integer
                       );
  end;

  TMqGetThread = class(TThread)
  private
    fMqInterface: TMqInterface;
    fProject: TWsdlProject;
  protected
    procedure Execute; override;
  public
    procedure DoTerminate; override;
    constructor Create (aProject: TWsdlProject; aMqInterface: TMqInterface);
    destructor Destroy; override;
  end;

procedure IntrospectIniXml;

var
  BetaMode: Boolean;
  webserviceWsdlFileName, webserviceXsdFileName, wsdlStubXsdFileName: String;
    indexHtmlFileName: String;
    indexWsdlsHtmlFileName: String;
    wsaXsdFileName: String;
    mqPutHeaderEditAllowedFileName: String;
    authorizationServerEndpoint: String;
    stompPutHeaderEditAllowedFileName: String;
    licenseDatabaseName, licenseOdbcDriver: String;
    RemoteControlPortNumber: Integer;
    wsaXsdDescr: TXsdDescr;
    swiftMTXsdDescr: TXsdDescr;
    optionsXsd: TXsd;
    endpointConfigXsd: TXsd;
    webserviceXsdDescr: TXsdDescr;
    webserviceWsdl: TWsdl;
    ScriptsXsd: TXsd;
    OperationDefsXsd: TXsd;
    projectOptionsXsd: TXsd;
    serviceOptionsXsd: TXsd;
    listenersConfigXsd: TXsd;
    operationOptionsXsd: TXsd;


implementation

uses OpenWsdlUnit
   , StrUtils
   , exceptionUtils
   , SchemaLocationz
   , smtpInterface
   , RegExpr
   , jwbBase64
   , base64
   {$ifdef windows}
   , ActiveX
   {$endif}
   , xmlUtilz
   , wrdFunctionz
   , GZIPUtils
   , xmlio
   , htmlxmlutilz
   , htmlreportz
   ;

procedure AddRemark(aOperation: TObject; aString: String);
begin
  if not Assigned (aOperation)
  or not (aOperation is TWsdlOperation)
  or not Assigned ((aOperation as TWsdlOperation).Data) then
  begin
    xmlz.SjowMessage('AddRemark: ' + aString);
    exit;
  end;
  with (aOperation as TWsdlOperation).Data as TLog do
  begin
    AddRemark(aString);
  end;
end;

function OperationFromContext (aContext: TObject; xOperationAlias: String): TWsdlOperation;
begin
  result := nil;
  if aContext is TWsdlOperation then with aContext as TWsdlOperation do
  begin
    result := invokeList.FindOnAliasName(xOperationAlias);
  end
  else
  begin
    if aContext is TWsdlProject then
    begin
      result := allAliasses.FindOnAliasName(xOperationAlias);
    end;
  end;
end;

function RequestAsText (aContext: TObject; xOperationAlias: String): String;
var
  aOper: TWsdlOperation;
begin
  result := '';
  aOper := OperationFromContext(aContext, xOperationAlias);
  if Assigned (aOper) then
    result := aOper.StreamRequest(_progName, True, True, True)
  else
    raise Exception.Create(Format ('RequestAsText: Operation ''%s'' not found', [xOperationAlias]));
end;

function ReplyAsText (aContext: TObject; xOperationAlias: String): String;
var
  aOper: TWsdlOperation;
begin
  result := '';
  aOper := OperationFromContext(aContext, xOperationAlias);
  if Assigned (aOper) then
    result := aOper.StreamReply(_progName, True)
  else
    raise Exception.Create(Format ('RequestAsText: Operation ''%s'' not found', [xOperationAlias]));
end;

procedure SaveSnapshots(aContext: TObject; aName: String);
var
  xProject: TWsdlProject;
begin
  xProject := nil; //candidate context
  if aContext is TWsdlProject then
    xProject := aContext as TWsdlProject
  else
    if aContext is TWsdlOperation then with aContext as TWsdlOperation do
      xProject := Owner as TWsdlProject;
  if not Assigned (xProject) then
    raise Exception.Create(Format ('SaveSnapshots(''%s''); unable to determine context', [aName]));
  xProject.SaveSnapshots(aName);
end;

procedure RequestOperation(aContext: TObject; xOperationAlias: String);
var
  xProject: TWsdlProject;
  xOperation: TWsdlOperation;
begin
  xProject := nil; //candidate context
  xOperation := nil; //candidate context
  if aContext is TWsdlOperation then with aContext as TWsdlOperation do
  begin
    xProject := Owner as TWsdlProject;
    xOperation := invokeList.FindOnAliasName(xOperationAlias);
    if Assigned (xOperation) then
    begin
      xOperation.StubAction := saRequest;
      try
        xProject.SendMessage (xOperation, nil, '');
      except
      end;
    end;
  end
  else
  begin
    if aContext is TWsdlProject then
    begin
      xProject := aContext as TWsdlProject;
      xOperation := allAliasses.FindOnAliasName(xOperationAlias);
      if Assigned (xOperation) then
      try
        xProject.SendMessage (xOperation, nil, '');
      except
      end;
    end;
  end;
  if not Assigned (xProject)
  or not Assigned (xOperation) then
   raise Exception.Create(Format ('RequestOperation: Operation ''%s'' not found', [xOperationAlias]));
end;

procedure NewDesignMessage(aContext: TObject; xOperationAlias: String);
var
  xProject: TWsdlProject;
  xOperation: TWsdlOperation;
  xMessage: TWsdlMessage;
  x: Integer;
begin
  xProject := nil; //candidate context
  xOperation := nil; //candidate context
  xMessage := nil;
  if aContext is TWsdlOperation then with aContext as TWsdlOperation do
  begin
    xProject := Owner as TWsdlProject;
    xOperation := invokeList.FindOnAliasName(xOperationAlias);
  end
  else
  begin
    if aContext is TWsdlProject then
    begin
      xProject := aContext as TWsdlProject;
      xOperation := allAliasses.FindOnAliasName(xOperationAlias);
    end;
  end;
  if not Assigned (xProject)
  or not Assigned (xOperation) then
   raise Exception.Create(Format ('CreateDesignMessage: Operation ''%s'' not found', [xOperationAlias]));
  xOperation.AcquireLock;
  try
    xMessage := TWsdlMessage.Create (xOperation);
    for x := 0 to xMessage.CorrelationBindables.Count - 1 do with xMessage.CorrelationBindables do
      Bindables[x].CorrelationValue := Bindables[x].Value;
    xProject.UpdateMessageRow(xOperation, xMessage);
  finally
    xOperation.ReleaseLock;
  end;
  xProject.AcquireLogLock;
  try
    xProject.uiInvalid := True;
  finally
    xProject.ReleaseLogLock;
  end;
end;

procedure CreateSnapshot(aContext: TObject; aName: String; aDoRun: Boolean);
var
  xProject: TWsdlProject;
begin
  xProject := nil; //candidate context
  if aContext is TWsdlProject then
    xProject := aContext as TWsdlProject
  else
    if aContext is TWsdlOperation then with aContext as TWsdlOperation do
      xProject := Owner as TWsdlProject;
  if not Assigned (xProject) then
    raise Exception.Create(Format ('CreateSnapshot(''%s''); unable to determine context', [aName]));
  if (xProject.CurrentFolder = '')
  or (    aDorun
      and (   (xProject.ReferenceFolder = '')
           or (xProject.CurrentFolder = xProject.ReferenceFolder)
          )
     ) then
    raise Exception.Create('CreateSnapshot: config (ProjectOptions.General.projectFolders) invalid');
  xProject.CreateSnapshot ( aName
                          , xProject.CurrentFolder + DirectorySeparator + aName + '.xml'
                          , xProject.ReferenceFolder + DirectorySeparator + aName + '.xml'
                          , true
                          , aDoRun
                          );
end;

procedure ClearLogs (aContext: TObject);
var
  xProject: TWsdlProject;
begin
  xProject := nil; //candidate context
  if aContext is TWsdlProject then
    xProject := aContext as TWsdlProject
  else
    if aContext is TWsdlOperation then with aContext as TWsdlOperation do
      xProject := Owner as TWsdlProject;
  if not Assigned (xProject) then
    raise Exception.Create('ClearLogs; unable to determine context');
  xProject.doClearLogs := True;
end;

procedure ClearSnapshots (aContext: TObject);
var
  xProject: TWsdlProject;
begin
  xProject := nil; //candidate context
  if aContext is TWsdlProject then
    xProject := aContext as TWsdlProject
  else
    if aContext is TWsdlOperation then with aContext as TWsdlOperation do
      xProject := Owner as TWsdlProject;
  if not Assigned (xProject) then
    raise Exception.Create('ClearLogs; unable to determine context');
  xProject.doClearSnapshots := True;
end;

procedure CreateSummaryReport(aContext: TObject; aName: String);
var
  xProject: TWsdlProject;
begin
  xProject := nil; //candidate context
  if aContext is TWsdlProject then
    xProject := aContext as TWsdlProject
  else
    if aContext is TWsdlOperation then with aContext as TWsdlOperation do
      xProject := Owner as TWsdlProject;
  if not Assigned (xProject) then
    raise Exception.Create(Format ('CreateSummaryReport(''%s''); unable to determine context', [aName]));
  xProject.CreateSummaryReport (aName);
end;

procedure CreateCoverageReport(aContext: TObject; aDoRun: Boolean);
var
  xProject: TWsdlProject;
begin
  xProject := nil; //candidate context
  if aContext is TWsdlProject then
    xProject := aContext as TWsdlProject
  else
    if aContext is TWsdlOperation then with aContext as TWsdlOperation do
      xProject := Owner as TWsdlProject;
  if not Assigned (xProject) then
    raise Exception.Create('CreateCoverageReport(''%s''); unable to determine context');
  xProject.CreateCoverageReport(aDoRun);
end;

procedure ExecuteScript(aContext: TObject; xScriptName: String);
var
  xScript: TXml;
  xProject: TWsdlProject;
begin
  xProject := nil; //candidate context
  if aContext is TWsdlProject then
    xProject := aContext as TWsdlProject
  else
    if aContext is TWsdlOperation then with aContext as TWsdlOperation do
      xProject := Owner as TWsdlProject;
  if not Assigned (xProject) then
    raise Exception.Create(Format ('ExecuteScript(''%s''); unable to determine context', [xScriptName]));
  xScript := xProject.FindScript(xScriptName);
  if Assigned(xScript) then
    xProject.ScriptExecute(xScript)
  else
    raise Exception.Create(Format ('ExecuteScript(''%s''); script not found', [xScriptName]));;
end;

procedure GetDefaultRequestData(aOperation: String);
var
  xOperation: TWsdlOperation;
begin
  xOperation := allOperations.FindOnAliasName(aOperation);
  if not Assigned (xOperation) then
    raise Exception.Create(Format ('GetDefaultMessageData: Operation %s not found', [aOperation]));
  with xOperation do
  begin
    if StubAction <> saRequest then
      raise Exception.Create(Format ('GetDefaultMessageData: Operation %s, only allowed on Operations with action = Request', [aOperation]));
    ReqBindablesFromWsdlMessage(Messages.Messages[0]);
  end;
end;

procedure PutDefaultRequestData(aOperation: String);
var
  xOperation: TWsdlOperation;
begin
  xOperation := allOperations.FindOnAliasName(aOperation);
  if not Assigned (xOperation) then
    raise Exception.Create(Format ('GetDefaultMessageData: Operation %s not found', [aOperation]));
  with xOperation do
  begin
{}{
    if StubAction <> saRequest then
      raise Exception.Create(Format ('GetDefaultMessageData: Operation %s, only allowed on Operations with action = Request', [aOperation]));
{}
    ReqBindablesToWsdlMessage(Messages.Messages[0]);
  end;
end;

procedure SendOperationRequest(aOperation, aCorrelation: String);
var
  x: Integer;
  xOperation: TWsdlOperation;
  xRequest: TWsdlMessage;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    ExplodeStr (aCorrelation, ';', sl);
//  with wsdlStubForm do
    begin
      xOperation := allOperations.FindOnAliasName(aOperation);
      if Assigned(xOperation) then
      begin
        if xOperation.StubAction <> saRequest then
          raise Exception.Create ( 'Operation <' + aOperation + '> not configured as Request'
                                 );
        if xOperation.CorrelationBindables.Count <> sl.Count then
          raise Exception.Create ( 'Number of correlation items is '
                                 + IntToStr (sl.Count)
                                 + '; must be '
                                 + IntToStr (xOperation.CorrelationBindables.Count)
                                 );
        for x := 0 to sl.Count - 1 do
        begin
          xOperation.CorrelationBindables.Bindables [x].Value := sl.Strings [x];
          xOperation.CorrelationBindables.Bindables [x].Checked := True;
        end;
        xRequest := xOperation.MessageBasedOnRequest;
        if not Assigned (xRequest) then
          raise Exception.Create ('Could not find message based on correlation: ' + aCorrelation);
        (xOperation.Owner as TWsdlProject).SendMessage (xOperation, xRequest, aCorrelation);
      end
      else
        raise Exception.Create ( 'Unknown operation: '
                               + aOperation
                               );
    end;
  finally
    FreeAndNil (sl);
  end;
end;

procedure SendOperationRequestLater(aOperation, aCorrelation: String; aLater: Integer);
var
  x: Integer;
  xOperation: TWsdlOperation;
  xRequest: TWsdlMessage;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    ExplodeStr (aCorrelation, ';', sl);
//  with wsdlStubForm do
    begin
      xOperation := allOperations.FindOnAliasName(aOperation);
      if Assigned (xOperation) then
      begin
        if xOperation.StubAction <> saRequest then
          raise Exception.Create ( 'SendOperationRequestLater: Operation <' + aOperation + '> not configured as Request'
                                 );
        if xOperation.CorrelationBindables.Count <> sl.Count then
          raise Exception.Create ( 'SendOperationRequestLater: Number of correlation items is '
                                 + IntToStr (sl.Count)
                                 + '; must be '
                                 + IntToStr (xOperation.CorrelationBindables.Count)
                                 );
        for x := 0 to sl.Count - 1 do
        begin
          xOperation.CorrelationBindables.Bindables [x].Value := sl.Strings [x];
          xOperation.CorrelationBindables.Bindables [x].Checked := True;
        end;
        xRequest := xOperation.MessageBasedOnRequest;
        if not Assigned (xRequest) then
          raise Exception.Create ('Could not find message based on correlation: ' + aCorrelation);
        (xOperation.Owner as TWsdlProject).SendMessageLater (xOperation, xRequest, aCorrelation, Trunc (aLater));
      end
      else
        raise Exception.Create ( 'Unknown operation: '
                               + aOperation
                               );
    end;
  finally
    FreeAndNil (sl);
  end;
end;

{ TMqGetThread }

constructor TMqGetThread.Create (aProject: TWsdlProject; aMqInterface: TMqInterface);
begin
  inherited Create (False);
  FreeOnTerminate := True;
  fMqInterface := aMqInterface;
  fProject := aProject;
  FreeOnTerminate := True;
end;

destructor TMqGetThread.Destroy;
begin
//  FreeAndNil (fMqInterface);
end;

procedure TMqGetThread.doTerminate;
begin
  fMqInterface.doTerminate := True;
end;

procedure TMqGetThread.Execute;
begin
  AcquireLock;
  Inc (fProject.NumberOfActiveMqs);
  ReleaseLock;
  try
    try
      fMqInterface.Use := fProject.mqUse;
      fMqInterface.Browse ( fProject.mqStubMessage
                          , fProject.mqOnNewThread
                          , True
                          , fMqInterface.DeleteMessages = mqdelAll
                          );
    except
      on e: Exception do
      begin
        fProject.LogServerMessage(e.Message, True, e);
      end;
    end;
  finally
    AcquireLock;
    Dec (fProject.NumberOfActiveMqs);
    ReleaseLock;
  end;
end;

{ TProcedureThread }

constructor TProcedureThread.Create(aSuspended, aBlocking: Boolean; aProject: TWsdlProject; aProcedure: TProcedure);
begin
  inherited Create (aSuspended);
  fBlocking := aBlocking;
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedure := aProcedure;
end;

constructor TProcedureThread.Create(aSuspended, aBlocking: Boolean; aProject: TWsdlProject; aProcedure: TProcedureS;
  aString: String);
begin
  inherited Create (aSuspended);
  fBlocking := aBlocking;
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedureString := aProcedure;
  fString := aString;
end;

constructor TProcedureThread.Create(aSuspended, aBlocking: Boolean; aProject: TWsdlProject; aProcedure: TProcedureOperation;
  aOperation: TWsdlOperation);
begin
  inherited Create (aSuspended);
  fBlocking := aBlocking;
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedureOperation := aProcedure;
  fOperation := aOperation;
end;

constructor TProcedureThread.Create(aSuspended, aBlocking: Boolean; aProject: TWsdlProject;
  aProcedure: TProcedureObject; aObject: TObject);
begin
  inherited Create (aSuspended);
  fBlocking := aBlocking;
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedureObject := aProcedure;
  fObject := aObject;
end;

constructor TProcedureThread.Create(aSuspended, aBlocking: Boolean; aProject: TWsdlProject;
  aProcedure: TProcedureXX; aExtended, aExtended2: Extended);
begin
  inherited Create (aSuspended);
  fBlocking := aBlocking;
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedureXX := aProcedure;
  fExtended := aExtended;
  fExtended2 := aExtended2;
end;

constructor TProcedureThread.Create ( aSuspended: Boolean
                                    ; aBlocking: Boolean
                                    ; aProject: TWsdlProject
                                    ; aProcedure: TProcedureClaimableObjectList
                                    ; aClaimableObjectList: TClaimableObjectList
                                    );
begin
  inherited Create (aSuspended);
  fBlocking := aBlocking;
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedureClaimableObjectList := aProcedure;
  fClaimableObjectList := aClaimableObjectList;
end;

procedure TProcedureThread.Execute;
begin
  if fBlocking then
  begin
    if Assigned (fProject.OnStartBlockingThread) then
      Synchronize(fProject.OnStartBlockingThread);
  end
  else
  begin
    if Assigned (fProject.OnStartNonBlockingThread) then
      Synchronize(fProject.OnStartNonBlockingThread);
  end;
  try
    if Assigned (fProcedure) then fProcedure;
    if Assigned (fProcedureString) then fProcedureString (fString);
    if Assigned (fProcedureXX) then fProcedureXX (fExtended, fExtended2);
    if Assigned (fProcedureOperation) then fProcedureOperation (fOperation);
    if Assigned (fProcedureObject) then fProcedureObject (fObject);
    if Assigned (fProcedureClaimableObjectList) then
    begin
      try
        fProcedureClaimableObjectList (fClaimableObjectList);
      finally
        fProject.AcquireLogLock;
        try
          fClaimableObjectList.Clear;
          fClaimableObjectList.Free;
        finally
          fProject.ReleaseLogLock;
        end;
      end;
    end;
  finally
    if fBlocking then
    begin
      if Assigned (fProject.OnTerminateBlockingThread) then
        Synchronize(fProject.OnTerminateBlockingThread);
    end
    else
    begin
      if Assigned (fProject.OnTerminateNonBlockingThread) then
        Synchronize(fProject.OnTerminateNonBlockingThread);
    end;
    if Assigned (fOnFinished) then
      Synchronize(fOnFinished);
  end;
end;

{ TSendAsynchReplyThread }

constructor TSendAsynchReplyThread.Create(aProject: TWsdlProject; aLog: TLog);
begin
  inherited Create (False);
  FreeOnTerminate := True;
  fProject := aProject;
  fLog := aLog;
end;

procedure TSendAsynchReplyThread.Execute;
begin
  fProject.SendAsynchReply (fLog);
end;

{ TSendSoapRequestThread }

constructor TSendSoapRequestThread.Create(aProject: TWsdlProject; aOperation: TWsdlOperation;
  aMessage: TWsdlMessage; aCorrelation: String; aLater: Integer);
begin
  inherited Create (False);
  FreeOnTerminate := True;
  fProject := aProject;
  fOperation := aOperation;
  fMessage := aMessage;
  fCorrelation := aCorrelation;
  fLater := aLater;
end;

procedure TSendSoapRequestThread.Execute;
begin
  Sleep (fLater);
  fProject.SendMessage (fOperation, fMessage, fCorrelation);
end;

{ TWsdlProject }

procedure IntrospectIniXml;
  function _replaceInteger (o, s: String; i: Integer): String;
  begin
    result := ReplaceStrings(o, s, IntToStr(i), True, False);
  end;
  function _Prep (f, s: String): String;
  var
    p: String;
  begin
    result := s;
    p := ExtractFilePath(f);
    result := ReplaceStrings(result, 'schemaLocation="', 'schemaLocation="' + p, True, False);
    result := _replaceInteger(result, '_xsdElementsWhenRepeatable_', defaultXsdElementsWhenRepeatable);
    result := _replaceInteger(result, '_xsdMaxDepthBillOfMaterials_', defaultXsdMaxDepthBillOfMaterials);
    result := _replaceInteger(result, '_xsdMaxDepthXmlGen_', defaultXsdMaxDepthXmlGen);
  end;
var
  x: Integer;
  Xml, xXml, iniXml: TXml;
  xIniFileName: String;
begin
  xIniFileName := Copy(ParamStr(0), 1, Length(ParamStr(0)){$ifdef windows} - 4{$endif}) + 'Ini.xml';
  if not FileExistsUTF8(xIniFileName) { *Converted from FileExists* } then
    raise Exception.CreateFmt(
      '%s coud not open expected inifile: %s,%splease install %s properly',
      [_progName, xIniFileName, CRLF, _progName]);
  iniXml := TXml.Create;
  try
    iniXml.LoadFromFile(xIniFileName, nil);
    webserviceWsdlFileName := iniXml.Items.XmlValueByTag ['wsdlStubWebServiceWsdl'];
    webserviceXsdFileName := iniXml.Items.XmlValueByTag ['wsdlStubWebServiceXsd'];
    indexHtmlFileName := iniXml.Items.XmlValueByTag ['indexHtml'];
    indexWsdlsHtmlFileName  := iniXml.Items.XmlValueByTag ['indexWsdlsHtml'];
    wsdlStubXsdFileName := iniXml.Items.XmlValueByTag ['wsdlStubXsd'];
    wsaXsdFileName := iniXml.Items.XmlValueByTag ['wsaXsd'];
    _swiftMTXsdFileName := iniXml.Items.XmlValueByTag ['swiftMTXsd'];
    mqPutHeaderEditAllowedFileName := iniXml.Items.XmlValueByTag ['mqPutHeaderEditAllowed'];
    stompPutHeaderEditAllowedFileName := iniXml.Items.XmlValueByTag ['stompPutHeaderEditAllowed'];
    authorizationServerEndpoint := iniXml.Items.XmlValueByTag ['authorizationServerEndpoint'];
    if Assigned (iniXml.ItemByTag['licenseDatabase']) then with iniXml.ItemByTag['licenseDatabase'].Items do
    begin
      licenseOdbcDriver := XmlValueByTagDef['OdbcDriver', 'Microsoft Access Driver'];
      licenseDatabaseName := ExpandRelativeFileName(xIniFileName, XmlValueByTagDef['DatabaseName', '']);
    end;
    RemoteControlPortNumber := iniXml.Items.XmlIntegerByTagDef ['commandPort', 3738];
    xsdElementsWhenRepeatable := defaultXsdElementsWhenRepeatable;
    xsdMaxDepthBillOfMaterials := defaultXsdMaxDepthBillOfMaterials;
    xsdMaxDepthXmlGen := defaultXsdMaxDepthXmlGen;
    if Assigned (iniXml.ItemByTag ['cssStylesheet']) then with iniXml.ItemByTag ['cssStylesheet'] do
    begin
      _wsdlStubStylesheet := ExpandRelativeFileName ( ExtractFilePath (ParamStr(0))
                                                                             , Value
                                                     );
    end;
    if wsaXsdFileName <> '' then
    begin
      wsaXsdFileName := ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), wsaXsdFileName);
      wsaXsdDescr := TXsdDescr.Create(1);
      try
        wsaXsdDescr.LoadXsdFromFile (wsaXsdFileName, nil);
        if wsaXsdDescr.TypeDef.ElementDefs.Count > 0 then
          _WsdlWsaXsd := wsaXsdDescr.TypeDef.ElementDefs.Xsds [wsaXsdDescr.TypeDef.ElementDefs.Count - 1];
      except
        raise Exception.Create ('Could not parse ' + wsaXsdFileName);
      end;
    end;
    if _swiftMTXsdFileName <> '' then
    begin
      _swiftMTXsdFileName := ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), _swiftMTXsdFileName);
      swiftMTXsdDescr := TXsdDescr.Create(1);
      try
        swiftMTXsdDescr.LoadXsdFromFile (_swiftMTXsdFileName, nil);
        if swiftMTXsdDescr.TypeDef.ElementDefs.Count > 0 then
        begin
          _swiftMTXsd := swiftMTXsdDescr.TypeDef.ElementDefs.Xsds [swiftMTXsdDescr.TypeDef.ElementDefs.Count - 1];
          _swiftMTXsd.Obj := TSwiftMtProps.Create (_swiftMTXsd);
        end;
      except
        raise Exception.Create ('Could not parse ' + _swiftMTXsdFileName);
      end;
    end;

    if wsdlStubXsdFileName <> '' then
    begin
      wsdlStubXsdFileName := ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), wsdlStubXsdFileName);
      webserviceXsdDescr := TXsdDescr.Create(1);
      try
        webserviceXsdDescr.LoadXsdFromString (_Prep ( wsdlStubXsdFileName
                                                    , ReadStringFromFile(wsdlStubXsdFileName)
                                                    )
                                             , nil
                                             );
      except
        raise Exception.Create (_progName + ' could not parse ' + wsdlStubXsdFileName);
      end;
    end;
    if not Assigned (webserviceXsdDescr) then
      raise exception.Create('No ' + _progName + ' webservice xsd assigned');

    with webserviceXsdDescr.TypeDef.ElementDefs do
    begin
      _WsdlRtiXsd := XsdByName['rti'];
      optionsXsd := XsdByName['wsdlStubOptions'];
      ScriptsXsd := XsdByName['Scripts'];
      OperationDefsXsd := XsdByName['OperationDefs'];
      projectOptionsXsd := XsdByName['projectOptions'];
      serviceOptionsXsd := XsdByName['serviceOptions'];
      operationOptionsXsd := XsdByName['operationOptions'];
      _WsdlServiceDefinitionXsd := XsdByName['ServiceDefinitions'];
      _WsdlListOfFilesXsd := XsdByName['FileNames'];
      endpointConfigXsd := XsdByName['endpointConfig'];
      listenersConfigXsd := XsdByName['Listeners'];
      _WsdlEmailXsd := XsdByName['Email'];
    end;
    if not Assigned (ScriptsXsd) then raise Exception.CreateFmt('XML Element definition for %s Scripts not found', [_progName]);
    if not Assigned (_WsdlRtiXsd) then raise Exception.Create('XML Element definition for RunTimeInterface not found');
    if not Assigned (optionsXsd) then raise Exception.CreateFmt('XML Element definition for %s Options not found', [_progName]);
    if not Assigned (OperationDefsXsd) then raise Exception.Create('XML Element definition for OperationDefs not found');
    if not Assigned (projectOptionsXsd) then raise Exception.Create('XML Element definition for projectOptions not found');
    if not Assigned (serviceOptionsXsd) then raise Exception.Create('XML Element definition for serviceOptions not found');
    if not Assigned (operationOptionsXsd) then raise Exception.Create('XML Element definition for operationOptions not found');
    if not Assigned (_WsdlServiceDefinitionXsd) then raise Exception.Create('XML Element definition for ServiceDefinitions not found');
    if not Assigned (_WsdlListOfFilesXsd) then raise Exception.Create('XML Element definition for FileNames not found');
    if not Assigned (endpointConfigXsd) then raise Exception.Create('XML Element definition for endpointConfig not found');
    if not Assigned (listenersConfigXsd) then raise Exception.Create('XML Element definition for listeners configuration not found');
    if Assigned (_WsdlRtiXsd) then
      _WsdlRtiXml := TXml.Create(-10000, _WsdlRtiXsd);
    if Assigned (endpointConfigXsd)
    and (endpointConfigXsd.sType.ElementDefs.Count > Ord (ttMq)) then
    begin
      with endpointConfigXsd.sType.ElementDefs.Xsds [Ord (ttMq)] do
      begin
        for x := 0 to sType.ElementDefs.Count - 1 do
        begin
          if sType.ElementDefs.Xsds[x].ElementName = 'mqHeader' then
          begin
            _WsdlmqHeaderXsd := sType.ElementDefs.Xsds[x];
            if mqPutHeaderEditAllowedFileName <> '' then
            begin
              mqPutHeaderEditAllowedFileName := ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), mqPutHeaderEditAllowedFileName);
              Xml := TXml.Create;
              try
                xXml := TXml.Create (-10000, _WsdlmqHeaderXsd);
                try
                  Xml.LoadFromFile (mqPutHeaderEditAllowedFileName, nil);
                  xXml.CopyValues (Xml, True, False);
                  xXml.SetXsdReadOnly;
                finally
                  xXml.Free;
                end;
              finally
                Xml.Free;
              end;
            end;
          end;
        end;
      end;
    end;

    if Assigned (endpointConfigXsd)
    and (endpointConfigXsd.sType.ElementDefs.Count > Ord (ttStomp)) then
    begin
      with endpointConfigXsd.sType.ElementDefs.Xsds [Ord (ttStomp)] do
      begin
        for x := 0 to sType.ElementDefs.Count - 1 do
        begin
          if sType.ElementDefs.Xsds[x].ElementName = 'stompHeader' then
          begin
            _WsdlstompHeaderXsd := sType.ElementDefs.Xsds[x];
            if stompPutHeaderEditAllowedFileName <> '' then
            begin
              stompPutHeaderEditAllowedFileName := ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), stompPutHeaderEditAllowedFileName);
              Xml := TXml.Create;
              try
                xXml := TXml.Create (-10000, _WsdlstompHeaderXsd);
                try
                  Xml.LoadFromFile (stompPutHeaderEditAllowedFileName, nil);
                  xXml.CopyValues (Xml, True, False);
                  xXml.SetXsdReadOnly;
                finally
                  xXml.Free;
                end;
              finally
                Xml.Free;
              end;
            end;
          end;
        end;
      end;
    end;

    if Assigned (endpointConfigXsd)
    and (endpointConfigXsd.sType.ElementDefs.Count > Ord (ttTaco)) then
      _WsdlTacoConfigXsd := endpointConfigXsd.sType.ElementDefs.Xsds [Ord (ttTaco)];

    if webserviceXsdFileName <> '' then
      webserviceXsdFileName := ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), webserviceXsdFileName);
    if webserviceWsdlFileName <> '' then
    begin
      webserviceWsdlFileName := ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), webserviceWsdlFileName);
      webserviceWsdl := TWsdl.Create(nil, -1, 1, False);
      webserviceWsdl.LoadFromSchemaFile(webserviceWsdlFileName, nil);
    end;
    if not Assigned (webserviceWsdl) then
      raise exception.Create('No ' + _progName + ' webservice wsdl read');
    if wsdlStubXsdFileName <> '' then
      wsdlStubXsdFileName := ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), wsdlStubXsdFileName);
  finally
    iniXml.Free;
  end;
end;

constructor TWsdlProject.Create;
begin
  {$ifndef FPC}
  jclDebug.JclStartExceptionTracking;
  {$endif}
  doDisplayLog := True;
  doCloneOperations := True;
  OnRestartEvent := RestartCommand;
  OnReactivateEvent := ReactivateCommand;
  OnReloadDesignEvent := ReloadDesignCommand;
  projectProperties := TStringList.Create;
  DatabaseConnectionSpecificationXml := TXml.CreateAsString ('DatabaseConnection', '');
  ppLock := TCriticalSection.Create;
  fTacoInterface := TTacoInterface.Create(nil, nil);
  fLogLock := TCriticalSection.Create;
  LogFilter := TLogFilter.Create;
  Wsdls := TStringList.Create;
  Wsdls.Sorted := True;
  wsdlNames := TStringList.Create;
  wsdlNames.Sorted := True;
  unknownOperation := TWsdlOperation.Create(TWsdl(nil));
  ignoreDifferencesOn := TStringList.Create;
  ignoreDifferencesOn.Sorted := True;
  ignoreDifferencesOn.Duplicates := dupIgnore;
  ignoreAddingOn := TStringList.Create;
  ignoreAddingOn.Sorted := True;
  ignoreAddingOn.Duplicates := dupIgnore;
  ignoreRemovingOn := TStringList.Create;
  ignoreRemovingOn.Sorted := True;
  ignoreRemovingOn.Duplicates := dupIgnore;
  ignoreOrderOn := TStringList.Create;
  ignoreOrderOn.Sorted := True;
  ignoreOrderOn.Duplicates := dupIgnore;
  regressionSortColumns := TStringList.Create;
  ignoreCoverageOn := TStringList.Create;
  xsdElementsWhenRepeatable := 1;
  AsynchRpyLogs := TLogList.Create;
  AsynchRpyLogs.Sorted := True;
  AsynchRpyLogs.Duplicates := dupError;
  displayedLogsmaxEntries := -1;
  displayedLogs := TLogList.Create;
  toDisplayLogs := TLogList.Create;
  toUpdateDisplayLogs := TLogList.Create;
  archiveLogs := TLogList.Create;
  displayedExceptions := TExceptionLogList.Create;
  toDisplayExceptions := TExceptionLogList.Create;
  displayedSnapshots := TSnapshotList.Create;
  toDisplaySnapshots := TSnapshotList.Create;
  Listeners := TListeners.Create;
  Listeners.aliasses := projectProperties;
  mqGetThreads := TStringList.Create;
  EnvironmentList := TStringList.Create;
  EnvironmentList.Sorted := True;
  EnvVars := TStringList.Create;
  mmqqMqInterface := TMqInterface.Create;
  mqUse := mquUndefined;
  mqMaxWorkingThreads := 15;
  if mmqqMqInterface.MQClientOK then mqUse := mquClient;
  if mmqqMqInterface.MQServerOK then mqUse := mquServer;
  ServerOpenSSL := TIdServerIOHandlerSSLOpenSSL.Create(nil);
  HTTPProxyServer := TIdHTTPProxyServer.Create(nil);
  with HTTPProxyServer do
  begin
    OnHTTPBeforeCommand := HTTPProxyServerHTTPBeforeCommand;
    OnHTTPDocument := HTTPProxyServerHTTPDocument;
    OnAfterCommandHandler := HTTPProxyServerAfterCommandHandler;
  end;
  HTTPServer := TIdHTTPServer.Create(nil);
  with HttpServer do
  begin
    OnCommandGet := HttpServerCommandGet;
    OnCommandOther := HttpServerCommandGet;
    OnCreatePostStream := HttpServerCreatePostStream;
  end;
  HTTPServerSSL := TIdHTTPServer.Create(nil);
  with HTTPServerSSL do
  begin
    IOHandler := ServerOpenSSL;
    OnCommandGet := HttpServerCommandGet;
    OnCommandOther := HttpServerCommandGet;
    OnCreatePostStream := HttpServerCreatePostStream;
  end;
  HttpServerBmtp := TIdHTTPServer.Create(nil);
  with HttpServerBmtp do
  begin
    OnCommandGet := HttpServerBmtpCommandGet;
    OnCommandOther := HttpServerBmtpCommandGet;
    OnCreatePostStream := HttpServerCreatePostStream;
  end;
  POP3Server := TIdPOP3Server.Create(nil);
  with Pop3Server do
  begin
    OnCheckUser := Pop3ServerCheckUser;
    OnDelete := POP3ServerDelete;
    OnList := POP3ServerList;
    OnRetrieve := POP3ServerRetrieve;
    OnStat := POP3ServerStat;
  end;
  SmtpOpenSSL := TIdServerIOHandlerSSLOpenSSL.Create(nil);
  SMTPServer := TIdSMTPServer.Create(nil);
  with SmtpServer do
  begin
    OnMailFrom := SmtpServerMailFrom;
    OnMsgReceive := SmtpServerMsgReceive;
    OnRcptTo := SmtpServerRcptTo;
    OnReceived := SmtpServerReceived;
    OnUserLogIn := SmtpServerUserLogin;
  end;
  SMTPServerSSL := TIdSMTPServer.Create(nil);
  with SMTPServerSSL do
  begin
    OnMailFrom := SmtpServerMailFrom;
    OnMsgReceive := SmtpServerMsgReceive;
    OnRcptTo := SmtpServerRcptTo;
    OnReceived := SmtpServerReceived;
    OnUserLogIn := SmtpServerUserLogin;
    IOHandler := SmtpOpenSSL;
  end;
  Scripts := TXml.CreateAsString('Scripts', '');
  DisplayedLogColumns := TStringList.Create;
  OperationsWithEndpointOnly := True;
  SaveRelativeFileNames := True;
  InitSpecialWsdls;
    {$IFnDEF FPC}
  _WsdlDbsAdoConnection.OnWillConnect := ADOConnectionWillConnect;
    {$endif}
end;

destructor TWsdlProject.Destroy;
begin
  Clear;
  FreeAndNil (fTacoInterface);
  FreeAndNil (mmqqMqInterface);
  FreeAndNil (HTTPProxyServer);
  FreeAndNil (HttpServer);
  FreeAndNil (HttpServerSSL);
  FreeAndNil (HttpServerBmtp);
  FreeAndNil (POP3Server);
  FreeAndNil (ServerOpenSSL);
  FreeAndNil (SmtpOpenSSL);
  FreeAndNil (SMTPServer);
  FreeAndNil (SMTPServerSSL);
  projectProperties.Free;
  DatabaseConnectionSpecificationXml.Free;
  ppLock.Free;
  fLogLock.Free;
  Listeners.Free;
  mqGetThreads.Free;
  toDisplayLogs.Clear;
  toDisplayLogs.Free;
  toUpdateDisplayLogs.Clear;
  toUpdateDisplayLogs.Free;
  EnvironmentList.Clear;
  EnvironmentList.Free;
  EnvVars.Clear;
  EnvVars.Free;
  displayedLogs.Clear;
  displayedLogs.Free;
  archiveLogs.Clear;
  archiveLogs.Free;
  LogFilter.Free;
  displayedExceptions.Clear;
  displayedExceptions.Free;
  toDisplayExceptions.Clear;
  toDisplayExceptions.Free;
  displayedSnapshots.Clear;
  displayedSnapshots.Free;
  toDisplaySnapshots.Clear;
  toDisplaySnapshots.Free;
  AsynchRpyLogs.Clear;
  AsynchRpyLogs.Free;
  FreeAndNil (unknownOperation);
  Wsdls.Free;
  wsdlNames.Free;
  FreeAndNil (FreeFormatWsdl);
  FreeAndNil (CobolWsdl);
  FreeAndNil (XsdWsdl);
  FreeAndNil (SwiftMtWsdl);
  ignoreDifferencesOn.Free;
  ignoreAddingOn.Free;
  ignoreRemovingOn.Free;
  ignoreOrderOn.Free;
  regressionSortColumns.Free;
  ignoreCoverageOn.Free;
  Scripts.Free;
  DisplayedLogColumns.Free;
  inherited;
end;

procedure TWsdlProject.DefaultDisplayMessageData;
begin
{}{
  while displayedLogsDisplayed < displayedLogs.Count do
  begin
    xLog := displayedLogs.LogItems [displayedLogs.Count - 1];
    LogFilter.Execute(xLog);
    if xLog.PassesFilter then
    begin
      with xLog.AsXml do
      try
        writeln (Text);
      finally
        Free;
      end;
    end;
    Inc (displayedLogsDisplayed);
  end;
{}
end;

procedure TWsdlProject.DelayMS(aDelayMS: Integer);
begin
  if (aDelayMS > 0) then
  begin
    Sleep (aDelayMS);
  end;
end;

procedure TWsdlProject.PrepareAllOperations(aLogServerException: TOnStringEvent);
  procedure _prepWsdl (xWsdl: TWsdl);
  var
    s, o: Integer;
    xOperation: TWsdlOperation;
  begin
    wsdlNames.AddObject(xWsdl.Name, xWsdl);
    for s := 0 to xWsdl.Services.Count - 1 do
    begin
      for o := 0 to xWsdl.Services.Services[s].Operations.Count - 1 do
      begin
        xOperation := xWsdl.Services.Services[s].Operations.Operations [o];
        if xOperation.WsdlService.DescriptionType = ipmDTEmail then
          xOperation.StubAction := saRequest;
        if Assigned (xOperation.reqBind) then
          xOperation.reqBind.Checked := True;
        if Assigned (xOperation.rpyBind) then
          xOperation.rpyBind.Checked := True;
        xOperation.BindStamper;
        try
          allOperations.AddObject ( xOperation.reqTagName + ';' + xOperation.reqTagNameSpace
                                  , xOperation
                                  );
        except
          aLogServerException ( 'Duplicate operation name (Req) ('
                      + xOperation.Name
                      + '). You may encounter errors due to this name conflict'
                      , False
                      , nil
                      );
        end;
        try
          allOperationsRpy.AddObject ( xOperation.rpyTagName
                                     , xOperation
                                     );
        except
{}{
          aLogServerException ( 'Duplicate operation name (Rpy) ('
                      + xOperation.Name
                      + '). You may encounter errors due to this name conflict'
                      , False
                      );
{}
        end;
        if xOperation.Messages.Count = 0 then
        begin
          if xOperation.StubAction = saRequest then
            TWsdlMessage.CreateRequest(xOperation, 'Default', '.*', 'Default request')
          else
            TWsdlMessage.CreateReply(xOperation, 'Default', '.*', 'Default reply');
        end;
      end;
    end;
  end;
  procedure _updtWsdls(aWsdl: TWsdl);
  var
    f: Integer;
  begin
    if (aWsdl.Services.Count > 0)
    and (aWsdl.Services.Services[0].Operations.Count > 0) then
    begin
      if not Wsdls.Find (aWsdl.Name, f) then
        Wsdls.AddObject(aWsdl.Name, aWsdl);
    end
    else
    begin
      if Wsdls.Find (aWsdl.Name, f) then
        Wsdls.Delete(f);
    end;
  end;
var
  w, o: Integer;
begin
  wsdlNames.Clear;
  allOperations.ClearListOnly;
  allOperationsRpy.ClearListOnly;
  scriptErrorCount := 0;
  _updtWsdls(FreeFormatWsdl);
  _updtWsdls(CobolWsdl);
  _updtWsdls(XsdWsdl);
  _updtWsdls(SwiftMtWsdl);
  for w := 0 to Wsdls.Count - 1 do
    _prepWsdl (Wsdls.Objects [w] as TWsdl);
  UpdateOperationAliasses;

  for o := 0 to allOperations.Count - 1 do with allOperations.Operations[o] do// here since invokeAll
  begin
    OnGetAbortPressed := self.GetAbortPressed;
    if reqBind is TXml then with reqBind as TXml do Checked := True;
    if rpyBind is TXml then with rpyBind as TXml do Checked := True;
    if fltBind is TXml then with fltBind as TXml do Checked := True;
    Owner := Self;
    doInvokeOperations;
    if not lateBinding then
    begin
      try
        PrepareBefore;
      except
      end;
      if not PreparedBefore then
      begin
        aLogServerException ( 'Error in Before script: '
                            + Name
                            , False
                            , nil
                            );
        Inc (scriptErrorCount);
      end;
      try
        PrepareAfter;
      except
      end;
      if not PreparedAfter then
      begin
        aLogServerException ( 'Error in After script: '
                            + Name
                            , False
                            , nil
                            );
        Inc (scriptErrorCount);
      end;
    end;
  end;
  if scriptErrorCount > 0 then
    aLogServerException ( IntToStr (scriptErrorCount) + ' Script(s) found with errors, see Exceptions log', False, nil);
end;

procedure TWsdlProject.AcquireLogLock;
begin
  if Self = nil then
    raise Exception.Create('TWsdlProject.AcquireLogLock: self is nil');
  fLogLock.Acquire;
end;

procedure TWsdlProject.Activate(aActive: Boolean);
var
  Binding : TIdSocketHandle;
  x: Integer;
begin
  abortPressed := not aActive;
  try
    if Assigned (fTacoInterface) then
      fTacoInterface.Disconnect; // (re)connection at first call
    for x := 0 to Listeners.stompInterfaces.Count - 1 do
      (Listeners.stompInterfaces.Objects[x] as TStompInterface).Disconnect;
    for x := mqGetThreads.Count - 1 downto 0 do
    begin
      if Assigned (mqGetThreads.Objects [x]) then
        try (mqGetThreads.Objects [x] as TMqGetThread).DoTerminate; except end;
      mqGetThreads.Objects [x] := nil;
    end;
    mqGetThreads.Clear;
    with HTTPProxyServer do
    begin
      Active := false;
      Bindings.Clear;
    end;
    with HTTPServer do
    begin
      if not Active then
        SessionState := False;
      Active := false;
      Bindings.Clear;
    end;
    with HTTPServerSSL do
    begin
      if not Active then
        SessionState := False;
      Active := false;
      Bindings.Clear;
    end;
    with HttpServerBmtp do
    begin
      if not Active then
        SessionState := False;
      Active := false;
      Bindings.Clear;
    end;
    SMTPServer.Active := false;
    SMTPServer.Bindings.Clear;
    SMTPServerSSL.Active := false;
    SMTPServerSSL.Bindings.Clear;
    Pop3Server.Active := false;
    Pop3Server.Bindings.Clear;
    {$IFnDEF FPC}
    try
      _WsdlDbsAdoConnection.Connected := False;
    except
    end;
    {$endif}

    fIsActive := aActive;

    begin
      if aActive then
      begin
        for x := 0 to allOperations.Count - 1 do with allOperations.Operations [x] do
          if Messages.Count > 0 then
          begin
            ReqBindablesFromWsdlMessage(Messages.Messages[0]);
            RpyBindablesFromWsdlMessage(Messages.Messages[0]);
          end;
    {$IFnDEF FPC}
        try
          _WsdlDbsAdoConnection.Connected := _WsdlDbsEnabled;
        except
          on e: Exception do
          begin
            LogServerMessage(format('Exception %s in Activate Dbs. Exception is:"%s".', [e.ClassName, e.Message]), True);
          end;
        end;
    {$endif}
        Listeners.FromXml(HaveStompFrame); // because of properties...
        DatabaseConnectionSpecificationFromXml; // because of properties...
        for x := 0 to Listeners.stompInterfaces.Count - 1 do
        begin
          with Listeners.stompInterfaces.Objects[x] as TStompInterface do
          try
            Connect;
            Notify(format('Listening for Stomp trafic on %s:%d.',[Host, Port]));
          except
            on e: Exception do
            begin
              LogServerMessage(format('Exception %s in Activate STOMP. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
            end;
          end;
        end;
        if {doUseMq} True
        and (   mmqqMqInterface.MQServerOK
             or mmqqMqInterface.MQClientOK
            ) then
        begin
          for x := 0 to Listeners.mqInterfaces.Count - 1 do
          begin
            with Listeners.mqInterfaces.Objects [x] as TMqInterface do
              Notify(format('Listening for MQ trafic on %s:%s.',[Qmanager, GetQueue]));
            mqGetThreads.AddObject ('', TMqGetThread.Create (Self, Listeners.mqInterfaces.Objects [x] as TMqInterface));
          end;
        end;
        if Listeners.httpPorts.Count > 0 then
        begin
          for x := 0 to Listeners.httpPorts.Count - 1 do
          begin
            try
              Binding := HTTPServer.Bindings.Add;
              Binding.Port := StrToInt(Listeners.httpPorts.Strings[x]);
              Binding.IP := '0.0.0.0';
              Notify(format( 'Listening for HTTP trafic on %s:%d.'
                           , [HTTPServer.Bindings[x].IP, HTTPServer.Bindings[x].Port]
                           )
                    );
            except
              on e: exception do
              begin
                LogServerMessage(format('Exception %s in Activate HTTP. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
              end;
            end;
          end;
          HTTPServer.SessionState := False;
          HTTPServer.Active := true;
        end;
        if Listeners.httpsPorts.Count > 0 then
        begin
          with ServerOpenSSL.SSLOptions do
          begin
            Method := Listeners.sslVersion;
            CertFile := Listeners.sslCertificateFile;
            KeyFile := Listeners.sslKeyFile;
            RootCertFile := Listeners.sslRootCertificateFile;
          end;
          for x := 0 to Listeners.httpsPorts.Count - 1 do
          begin
            try
              Binding := HTTPServerSSL.Bindings.Add;
              Binding.Port := StrToInt(Listeners.httpsPorts.Strings[x]);
              Binding.IP := '0.0.0.0';
              Notify(format('Listening for HTTPS connections on %s:%d.',[HTTPServerSSL.Bindings[x].IP, HTTPServerSSL.Bindings[x].Port]));
            except
              on e: exception do
              begin
                LogServerMessage(format('Exception %s in Activate HTTPS. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
              end;
            end;
          end;
          HTTPServerSSL.SessionState := False;
          HTTPServerSSL.Active := true;
        end;
        if Listeners.httpBmtpPort > 0 then
          try
            Binding := HttpServerBmtp.Bindings.Add;
            Binding.Port := Listeners.httpBmtpPort;
            Binding.IP := '0.0.0.0';
            HttpServerBmtp.SessionState := False;
            HttpServerBmtp.Active := true;
            Notify(format('Listening for HTTP trafic on %s:%d.',[HttpServerBmtp.Bindings[0].IP, HttpServerBmtp.Bindings[0].Port]));
          except
            on e: exception do
            begin
              LogServerMessage(format('Exception %s in Activate HTTP(Bmtp). Exception is:"%s".', [e.ClassName, e.Message]), True, e);
            end;
          end;
        if Listeners.httpProxyPort > 0 then
          try
            HTTPProxyServer.DefaultPort := Listeners.httpProxyPort;
            HTTPProxyServer.Active := true;
            Notify(format('Listening for HTTP proxy trafic on %d.',[HTTPProxyServer.DefaultPort]));
          except
            on e: exception do
            begin
              LogServerMessage(format('Exception %s in Activate HTTP Proxy. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
            end;
          end;
        if Listeners.smtpPort > 0 then
          try
            SMTPServer.DefaultPort := Listeners.smtpPort;
            SMTPServer.Active := true;
            Notify(format('Listening for SMTP connections on %s:%d.',[SMTPServer.Bindings[0].IP, SMTPServer.Bindings[0].Port]));
          except
            on e: exception do
            begin
              LogServerMessage(format('Exception %s in Activate SMTP. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
            end;
          end;
        if Listeners.smtpsPort > 0 then
          try
            with SMTPOpenSSL.SSLOptions do
            begin
              CertFile := Listeners.smtpTlsCertificateFile;
              KeyFile := Listeners.smtpTlsKeyFile;
              RootCertFile := Listeners.smtpTlsRootCertificateFile;
            end;
            Binding := SMTPServerSSL.Bindings.Add;
            Binding.Port := Listeners.smtpsPort;
            Binding.IP := '0.0.0.0';
            SMTPServerSSL.Active := true;
            Notify(format('Listening for SMTPS connections on %s:%d.',[SMTPServerSSL.Bindings[0].IP, SMTPServerSSL.Bindings[0].Port]));
          except
            on e: exception do
            begin
              LogServerMessage(format('Exception %s in Activate SMTPS. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
            end;
          end;
        if Listeners.pop3Port > 0 then
          try
            POP3Server.DefaultPort := Listeners.POP3Port;
            POP3Server.Active := true;
            Notify(format('Listening for POP3 connections on %s:%d.',[POP3Server.Bindings[0].IP, POP3Server.Bindings[0].Port]));
          except
            on e: exception do
            begin
              LogServerMessage(format('Exception %s in Activate POP3. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
            end;
          end;
      end;
    end;
  finally
  end;
end;

function TWsdlProject.BooleanPromptDialog(aPrompt: String): Boolean;
begin
  result := False;
  if Assigned (OnBooleanDialog) then
    result := OnBooleanDialog (aPrompt)
  else
    raise Exception.Create(aPrompt);
end;

function TWsdlProject.ProjectOptionsLogDisplayedColumnsAsXml: TXml;
var
  x: Integer;
begin
  result := TXml.CreateAsString('DisplayedColumns', '');
  with result do
    for x := 0 to DisplayedLogColumns.Count - 1 do
      with AddXml(TXml.CreateAsString('DisplayedColumn', '')) do
        AddXml(TXml.CreateAsString('Header', DisplayedLogColumns.Strings[x]));
end;

function TWsdlProject.ProjectLogOptionsAsXml: TXml;
begin
  result := TXml.CreateAsString('Log', '');
  with result do
  begin
    AddXml(TXml.CreateAsString('maxEntries', IfThen(displayedLogsMaxEntries > -1, IntToStr(displayedLogsMaxEntries), 'unbounded')));
    case CompareLogOrderBy of
      clTimeStamp: AddXml(TXml.CreateAsString('CompareLogsOrdered', 'As is'));
      clOperation: AddXml(TXml.CreateAsString('CompareLogsOrdered', 'Service, Operation'));
      clCorrelation: AddXml(TXml.CreateAsString('CompareLogsOrdered', 'Service, Operation, Correlation'));
    end;
    with AddXml (TXml.CreateAsString('DocumentComparison','')) do
    begin
      AddXml (TXml.CreateAsBoolean('DetectDocumentFormatChanges', wrdFunctionz.wrdDetectFormatChanges));
      AddXml (TXml.CreateAsBoolean('NewDocumentAsReference', wrdFunctionz.wrdNewDocumentAsReference));
      AddXml (TXml.CreateAsInteger('ExpectedDifferenceCount', wrdFunctionz.wrdExpectedDifferenceCount));
    end;
    case ShowLogCobolStyle of
      slCobol: AddXml(TXml.CreateAsString('ShowCobolDataAs', 'Cobol'));
      slXml: AddXml(TXml.CreateAsString('ShowCobolDataAs', 'Xml'));
    end;
    AddXml(ProjectOptionsLogDisplayedColumnsAsXml);
  end;
end;

function TWsdlProject.ProjectOptionsAsXml (aRelativeFilenames: Boolean; aFileName: String): TXml;
begin
  result := TXml.CreateAsString('projectOptions', '');
  with result.AddXml (TXml.CreateAsString('General', '')) do
  begin
    AddXml (TXml.CreateAsBoolean('SaveRelativeFileNames', SaveRelativeFileNames));
    if (CurrentFolder <> '')
    or (ReferenceFolder <> '')
    or (ReportsFolder <> '') then
    begin
      with AddXml (TXml.CreateAsString('projectFolders', '')) do
      begin
        if aRelativeFilenames then
        begin
          AddXml (TXml.CreateAsString('current', ExtractRelativeFileName (aFileName, CurrentFolder)));
          AddXml (TXml.CreateAsString('reference', ExtractRelativeFileName (aFileName, ReferenceFolder)));
          AddXml (TXml.CreateAsString('reports', ExtractRelativeFileName (aFileName, ReportsFolder)));
        end
        else
        begin
          AddXml (TXml.CreateAsString('current', CurrentFolder));
          AddXml (TXml.CreateAsString('reference', ReferenceFolder));
          AddXml (TXml.CreateAsString('reports', ReportsFolder));
        end;
      end;
    end;
  end;
  result.AddXml(ProjectLogOptionsAsXml);
  with result.AddXml (TXml.CreateAsString('Wsdl', '')) do
  begin
    AddXml (TXml.CreateAsBoolean('PublishDescriptions', PublishDescriptions));
    AddXml (TXml.CreateAsBoolean('OperationsWithEndpointOnly', OperationsWithEndpointOnly));
    AddXml (TXml.CreateAsInteger('ElementsWhenRepeatable', xsdElementsWhenRepeatable));
    AddXml (TXml.CreateAsInteger('MaxDepthWhenRecursive', xsdMaxDepthBillOfMaterials));
    AddXml (TXml.CreateAsInteger('MaxDepthXmlGen', xsdMaxDepthXmlGen));
  end;
  with result.AddXml (TXml.CreateAsString('OnCorrelate', '')) do
  begin
      AddXml (TXml.CreateAsBoolean('DisableMessage', _WsdlDisableOnCorrelate));
  end;
  with result.AddXml (TXml.CreateAsString('UnknownOperations', '')) do
  begin
    case unknownOperation.StubAction of
       saStub: AddXml (TXml.CreateAsString('RaiseErrorMessage', notStubbedExceptionMessage));
       saForward: ;
       saRedirect:
         with AddXml (unknownOperation.endpointConfigAsXml) do
           Name := 'Redirect';
       saRequest: ;
    end;
  end;
  with result.AddXml (TXml.CreateAsString('OperationDefaults', '')) do
  begin
    with AddXml (TXml.CreateAsString('OnRequestViolatingAddressPath', '')) do
    begin
      case OnRequestViolatingAddressPath of
        rvsDefault: AddXml (TXml.CreateAsString('Continue', '')); // YES!!
        rvsContinue: AddXml (TXml.CreateAsString('Continue', ''));
        rvsAddRemark: AddXml (TXml.CreateAsString('AddRemark', ''));
        rvsRaiseErrorMessage: AddXml (TXml.CreateAsString('RaiseErrorMessage', ''));
      end;
    end;
    with AddXml (TXml.CreateAsString('OnRequestViolatingSchema', '')) do
    begin
      case OnRequestViolatingSchema of
        rvsDefault: AddXml (TXml.CreateAsString('Continue', '')); // YES!!
        rvsContinue: AddXml (TXml.CreateAsString('Continue', ''));
        rvsAddRemark: AddXml (TXml.CreateAsString('AddRemark', ''));
        rvsRaiseErrorMessage: AddXml (TXml.CreateAsString('RaiseErrorMessage', ''));
      end;
    end;
  end;
  with result.AddXml (TXml.Create) do
    CopyDownLine(DatabaseConnectionSpecificationXml, True);
end;

function TWsdlProject.ProjectScriptsAsXml : TXml ;
var
  x: Integer;
begin
  result := TXml.CreateAsString('projectScripts', '');
  with result.AddXml (TXml.CreateAsString('Scripts', '')) do
    CopyDownLine(Scripts, True);
  with result.AddXml (TXml.CreateAsString('Operations', '')) do
  begin
    for x := 0 to allAliasses.Count - 1 do
    begin
      with AddXml (TXml.CreateAsString('Operation', '')) do
      begin
        AddXml (TXml.CreateAsString('Alias', allAliasses.Operations[x].Alias));
        AddXml (TXml.CreateAsString('BeforeScript', allAliasses.Operations[x].BeforeScriptLines.Text));
        AddXml (TXml.CreateAsString('AfterScript', allAliasses.Operations[x].AfterScriptLines.Text));
      end;
    end;
  end;
end;

procedure TWsdlProject.ProjectScriptsFromXml (aXml : TXml );
var
  xXml, oXml: TXml;
  x, f: Integer;
begin
  if (not Assigned (aXml))
  or (aXml.Name <> 'projectScripts') then
    Exit;
  xXml := aXml.Items.XmlItemByTag['Scripts'];
  if Assigned (xXml) then
    Scripts.CopyDownLine(xXml, True);
  xXml := aXml.Items.XmlItemByTag['Operations'];
  if Assigned (xXml) then
  begin
    for x := 0 to xXml.Items.Count -1 do
    begin
      oXml := xXml.Items.XmlItems[x];
      if allAliasses.Find(oXml.Items.XmlValueByTag['Alias'], f) then
      begin
        with allAliasses.Operations[f] do
        begin
          BeforeScriptLines.Text := oXml.Items.XmlValueByTag['BeforeScript'];
          AfterScriptLines.Text := oXml.Items.XmlValueByTag['AfterScript'];
        end;
      end;
    end;
  end;
end;

function TWsdlProject.ProjectDesignAsString (aMainFileName: String): String;
  procedure _addCheckers (aList, aXml: TXml);
  var
    x: Integer;
  begin
    if (not Assigned (aXml))
    or (not aXml.Checked) then Exit;
    if aXml.isEvaluation then
    with aList.AddXml(TXml.CreateAsString('Checker', '')) do
    begin
      AddXml (TXml.CreateAsString('Name', aXml.FullIndexCaption));
      AddXml (TXml.CreateAsString('Value', aXml.Checker));
    end;
    for x := 0 to aXml.Items.Count - 1 do
      _addCheckers(aList, aXml.Items.XmlItems[x]);
  end;
var
  x, w, s, o, r, p: Integer;
  xOperation: TWsdlOperation;
  xWsdl: TWsdl;
  xMessage: TWsdlMessage;
  xDone: Boolean;
  swapReqParent: TCustomBindable;
  asXml, checkerXmls: TXml;
begin
  AcquireLock;
  try
    with TXml.CreateAsString ('WsdlStubCase', '') do
    try
      AddXml(TXml.CreateAsString('FileName', uncFilename(aMainFileName)));
      with AddXml (TXml.Create) do
        CopyDownLine(Listeners.SpecificationXml, True);
      AddXml(TXml.CreateAsBoolean('ValidateRequests', doValidateRequests));
      AddXml(TXml.CreateAsBoolean('ValidateReplies', doValidateReplies));
      AddXml (TXml.CreateAsBoolean('CheckExpectedValues', doCheckExpectedValues));
      AddXml (TXml.CreateAsBoolean('DisableOnCorrelate', _WsdlDisableOnCorrelate));
      AddXml (ProjectOptionsAsXml(SaveRelativeFileNames, uncFilename(aMainFileName)));
      AddXml (TXml.CreateAsString('PathPrefixes', xmlio.PathPrefixes.Text));
      with AddXml(TXml.CreateAsString('Environments', '')) do
        for x := 0 to EnvironmentList.Count - 1 do
          with AddXml(TXml.CreateAsString('Environment', '')) do
          begin
            AddAttribute(TXmlAttribute.CreateAsString('Name', EnvironmentList.Strings[x])).Checked := True;
            with AddXml (TXml.Create) do
              Text := (EnvironmentList.Objects [x] as TXml).Text;
          end;
      AddXml (TXml.CreateAsString('properties', projectProperties.Text));
      for w := 0 to Wsdls.Count - 1 do
      begin
        xWsdl := Wsdls.Objects [w] as TWsdl;
        with AddXml (TXml.CreateAsString('Wsdl', '')) do
        begin
          xDone := False;
          if xWsdl = FreeFormatWsdl then
          begin
            if FreeFormatWsdl.Services.Services[0].Operations.Count > 0 then
              AddXml (freeFormatOperationsXml);
            xDone := True;
          end;
          if xWsdl = CobolWsdl then
          begin
            if CobolWsdl.Services.Services[0].Operations.Count > 0 then
              with AddXml (cobolOperationsXml) do
              begin
                if SaveRelativeFileNames then
                  SetFileNamesRelative(aMainFileName);
              end;
            xDone := True;
          end;
          if xWsdl = XsdWsdl then
          begin
            if XsdWsdl.Services.Services[0].Operations.Count > 0 then
              with AddXml (xsdOperationsXml(aMainFileName)) do
              begin
                if SaveRelativeFileNames then
                  SetFileNamesRelative(aMainFileName);
              end;
            xDone := True;
          end;
          if xWsdl = SwiftMtWsdl then
          begin
            if SwiftMtWsdl.Services.Services[0].Operations.Count > 0 then
              with AddXml (swiftMtOperationsXml) do
              begin
                if SaveRelativeFileNames then
                  SetFileNamesRelative(aMainFileName);
              end;
            xDone := True;
          end;
          if not xDone then
          begin
            if (aMainFileName <> '')
            and (SaveRelativeFileNames) then
              AddXml(TXml.CreateAsString ( 'WsdlLocation'
                                         , ExtractRelativeFileName ( aMainFileName
                                                                   , Wsdls.Strings [w]
                                                                   )
                                         )
                    )
            else
              AddXml(TXml.CreateAsString ( 'WsdlLocation'
                                         , uncFilename(Wsdls.Strings [w])
                                         )
                    );
          end;
          if Assigned (xWsdl) then
          begin
            if xWsdl.ExtraXsds.Count > 0 then
            begin
              with AddXml (TXml.CreateAsString ('ExtraXsds','')) do
                with AddXml (xWsdl.ExtraXsdsAsXml) do
                begin
                  if SaveRelativeFileNames then
                    SetFileNamesRelative(aMainFileName);
                end;
            end;
            AddXml(TXml.CreateAsInteger('ElementsWhenRepeatable', xWsdl.xsdElementsWhenRepeatable));
            asXml := xWsdl.XsdDescr.ChangedElementTypedefsAsXml as TXml;
            if asXml.Items.Count > 0 then
              AddXml (asXml)
            else
              FreeAndNil (asXml);
            for s := 0 to xWsdl.Services.Count - 1 do
            begin
              with AddXml (TXml.CreateAsString('Service', '')) do
              begin
                AddXml (TXml.CreateAsString('Name', xWsdl.Services.Services[s].Name));
{BEGIN 3.6 style}
                AddXml (TXml.CreateAsInteger('AuthenticationType', Ord (xWsdl.Services.Services[s].AuthenticationType)));
                AddXml (TXml.CreateAsString('UserName', xWsdl.Services.Services[s].UserName));
                AddXml (TXml.CreateAsString('Password', Xmlz.EncryptString(xWsdl.Services.Services[s].Password)));
                AddXml (TXml.CreateAsInteger('PasswordType', Ord (xWsdl.Services.Services[s].PasswordType)));
                AddXml (TXml.CreateAsBoolean('SuppressXmlComment', xWsdl.Services.Services[s].SuppressXmlComment));
                AddXml (TXml.CreateAsBoolean('SuppressHTTP500', xWsdl.Services.Services[s].SuppressHTTP500));
{END 3.6 style}
                AddXml (xWsdl.Services.Services[s].OptionsAsXml);
                for o := 0 to xWsdl.Services.Services [s].Operations.Count - 1 do
                begin
                  xOperation := xWsdl.Services.Services[s].Operations.Operations[o];
                  with AddXml (TXml.CreateAsString('Operation', '')) do
                  begin
                    AddXml (TXml.CreateAsString('Name', xOperation.Name));
                    AddXml (TXml.CreateAsString('Action', IntToStr(Ord(xOperation.StubAction))));
                    if (xOperation.Alias <> xOperation.reqTagName)
                    and (xOperation.Alias <> '') then
                      AddXml (TXml.CreateAsString('Alias', xOperation.Alias));
                    AddXml (TXml.CreateAsBoolean('HiddenFromUI', xOperation.HiddenFromUI));
                    asXml := xOperation.AddedTypeDefElementsAsXml as TXml;
                    if asXml.Items.Count > 0 then
                      AddXml (asXml)
                    else
                      FreeAndNil (asXml);
                    AddXml (TXml.CreateAsBoolean('wsaEnabled', xOperation.wsaEnabled));
                    AddXml (TXml.CreateAsBoolean('wsaSpecificMustUnderstand', xOperation.wsaSpecificMustUnderstand));
                    AddXml (TXml.CreateAsBoolean('wsaMustUnderstand', xOperation.wsaMustUnderstand));
                    AddXml (TXml.CreateAsBoolean('AsynchronousDialog', xOperation.AsynchronousDialog));
                    if Assigned(xOperation.reqWsaXml)
                    and xOperation.reqWsaXml.Checked
                    and (xOperation.reqWsaXml.Name <> '') then
                      with AddXml (TXml.Create) do
                        CopyDownLine (xOperation.reqWsaXml, True);
                    if Assigned(xOperation.rpyWsaXml)
                    and xOperation.rpyWsaXml.Checked
                    and (xOperation.rpyWsaXml.Name <> '') then
                      with AddXml (TXml.Create) do
                        CopyDownLine (xOperation.rpyWsaXml, True);
                    AddXml (xOperation.OptionsAsXml);
                    AddXml (TXml.CreateAsString('DelayTimeMsMin', IntToStr(xOperation.DelayTimeMsMin)));
                    AddXml (TXml.CreateAsString('DelayTimeMsMax', IntToStr(xOperation.DelayTimeMsMax)));
{BEGIN Save in pre 4.0 mode}
                    AddXml (TXml.CreateAsInteger('StubTransport', Ord (xOperation.StubTransport)));
                    AddXml (TXml.CreateAsString('StubHttpAddress', xOperation.StubHttpAddress));
                    AddXml (TXml.CreateAsString('StubMqPutManager', xOperation.StubMqPutManager));
                    AddXml (TXml.CreateAsString('StubMqPutQueue', xOperation.StubMqPutQueue));
                    AddXml (TXml.CreateAsString('StubMqGetManager', xOperation.StubMqGetManager));
                    AddXml (TXml.CreateAsString('StubMqGetQueue', xOperation.StubMqGetQueue));
                    AddXml (TXml.CreateAsInteger('StubMqTimeOut', xOperation.StubMqTimeOut));
                    AddXml (TXml.CreateAsString('StubStompPutHost', xOperation.StubStompPutHost));
                    AddXml (TXml.CreateAsString('StubStompPutPort', xOperation.StubStompPutPort));
                    AddXml (TXml.CreateAsString('StubStompPutClientId', xOperation.StubStompPutClientId));
                    AddXml (TXml.CreateAsInteger('StubStompTimeOut', xOperation.StubStompTimeOut));
                    if Assigned(xOperation.StubMqHeaderXml)
                    and xOperation.StubMqHeaderXml.Checked
                    and (xOperation.StubMqHeaderXml.Name <> '') then
                      with AddXml (TXml.Create) do
                        CopyDownLine (xOperation.StubMqHeaderXml, True);
                    if Assigned (xOperation.StubStompHeaderXml)
                    and xOperation.StubStompHeaderXml.Checked
                    and (xOperation.StubStompHeaderXml.Name <> '') then
                      with AddXml (TXml.Create) do
                        CopyDownLine (xOperation.StubStompHeaderXml, True);
{END Save in pre 4.0 mode}

                    AddXml (xOperation.endpointConfigAsXml); // seave in 4.0++ style

                    AddXml (TXml.CreateAsString('BeforeScript', xOperation.BeforeScriptLines.Text));
                    AddXml (TXml.CreateAsString('AfterScript', xOperation.AfterScriptLines.Text));
                    swapReqParent := (xOperation.reqBind as TCustomBindable).Parent;
                    with AddXml (TXml.CreateAsString('CorrelationElements', '')) do
                      for r := 0 to xOperation.CorrelationBindables.Count - 1 do
                        AddXml (TXml.CreateAsString('CorrelationElement', xOperation.CorrelationBindables.Strings[r]));
                    with AddXml (TXml.CreateAsString('ExpectationElements', '')) do
                      for r := 0 to xOperation.ExpectationBindables.Count - 1 do
                        AddXml (TXml.CreateAsString('ExpectationElement', xOperation.ExpectationBindables.Strings[r]));
                    (xOperation.reqBind as TCustomBindable).Parent := swapReqParent;
                    with AddXml (TXml.CreateAsString('ColumnElements', '')) do
                    begin
                      for r := 0 to xOperation.Messages.Messages[0].ColumnXmls.Count - 1 do
                      begin
                        AddXml (TXml.CreateAsString('ColumnElement', xOperation.Messages.Messages[0].ColumnXmls.Strings[r]));
                      end;
                    end;
                    AddXml (TXml.CreateAsString('LogColumns',xOperation.LogColumns.Text));
                    with AddXml (TXml.CreateAsString('Messages', '')) do
                    begin
                      for r := 0 to xOperation.Messages.Count - 1 do
                      begin
                        xMessage := xOperation.Messages.Messages [r];
                        with AddXml (TXml.CreateAsString('Message', '')) do
                        begin
                          AddXml (TXml.CreateAsString('Name', xMessage.Name));
                          with AddXml (TXml.CreateAsString('Patterns' , '')) do
                            for p := 0 to xMessage.CorrelationBindables.Count - 1 do
                              if Assigned (xMessage.CorrelationBindables.Bindables[p]) then
                                AddXml (TXml.CreateAsString('Pattern', xMessage.CorrelationBindables.Bindables[p].CorrelationValue))
                              else
                                AddXml (TXml.CreateAsString('Pattern', '?'));
                          with AddXml (TXml.CreateAsString('Reply', '')) do
                          begin
                            if xOperation.WsdlService.DescriptionType in [ipmDTFreeFormat] then
                              Value := xMessage.FreeFormatRpy
                            else
                            begin
                              if xOperation.rpyBind.Name <> '' then
                              begin
                                if xOperation.rpyBind is TXml then
                                  with AddXml (TXml.CreateAsString(xOperation.rpyBind.Name, '')) do
                                    CopyRelevancy(xMessage.rpyBind as TXml);
                                if (xOperation.rpyBind is TIpmItem) then
                                  AddXml((xMessage.rpyBind as TIpmItem).AsXml);
                              end;
                            end;
                          end;
                          if xOperation.rpyBind is TXml then
                          begin
                            checkerXmls := AddXml (TXml.CreateAsString('replyCheckers', ''));
                            _addCheckers (checkerXmls, xMessage.rpyBind as TXml);
                          end;
                          with AddXml (TXml.CreateAsString('Request', '')) do
                          begin
                            if xOperation.WsdlService.DescriptionType in [ipmDTFreeFormat] then
                              Value := xMessage.FreeFormatReq
                            else
                            begin
                              if xOperation.reqBind.Name <> '' then
                              begin
                                if xOperation.reqBind is TXml then
                                  with AddXml (TXml.CreateAsString(xOperation.reqBind.Name, '')) do
                                    CopyRelevancy(xMessage.reqBind as TXml);
                                if xOperation.reqBind is TIpmItem then
                                  AddXml((xMessage.reqBind as TIpmItem).AsXml);
                              end;
                            end;
                          end;
                          if xOperation.reqBind is TXml then
                          begin
                            checkerXmls := AddXml (TXml.CreateAsString('requestCheckers', ''));
                            _addCheckers (checkerXmls, xMessage.reqBind as TXml);
                          end;
                          AddXml (TXml.CreateAsString('Documentation', xMessage.Documentation));
{
                          if Assigned (xOperation.FaultMessages) then
                          begin
                            with AddXml (TXml.CreateAsString('Faults', '')) do
                            begin
                              with AddXml (TXml.CreateAsString(xOperation.rpyBind.Name, '')) do
                                CopyCheckedDownLine(xMessage.FltBind);
                            end; // data xml
                          end; // case faultmessages
}
                        end; // message xml
                      end; // for each message
                    end; // messagess xml
                  end; // operation xml
                end; // each operation
              end; // service xml
            end; // each service
          end;  //
        end; // Assigned Wsdl
      end; // for each wsdl
      AddXml(TXml.CreateAsString('ignoreDifferencesOn', ignoreDifferencesOn.Text));
      AddXml(TXml.CreateAsString('ignoreAddingOn', ignoreAddingOn.Text));
      AddXml(TXml.CreateAsString('ignoreRemovingOn', ignoreRemovingOn.Text));
      with AddXml(TXml.CreateAsString('ignoreOrderOn', '')) do
        for x := 0 to ignoreOrderOn.Count - 1 do
          with AddXml(TXml.CreateAsString('Element', '')) do
          begin
            AddXml(TXml.CreateAsString('Id', ignoreOrderOn.Strings[x]));
            AddXml(TXml.CreateAsString('Keys', (ignoreOrderOn.Objects[x] as TStringList).Text));
          end;
      AddXml(TXml.CreateAsString('regressionSortColumns', regressionSortColumns.Text));
      AddXml(TXml.CreateAsString('ignoreCoverageOn', ignoreCoverageOn.Text));
      with AddXml(TXml.CreateAsString('Scripts', '')) do
        CopyDownLine(Scripts, True);
      AddXml (TXml.CreateAsString('FocusOperationName', FocusOperationName));
      AddXml (TXml.CreateAsString('FocusOperationNameSpace', FocusOperationNameSpace));
      AddXml (TXml.CreateAsInteger('FocusMessageIndex', FocusMessageIndex));
      result := AsText(False,0,True,False);
    finally
      Free;
    end;
  finally
    ReleaseLock;
  end;
end;

procedure TWsdlProject.ProjectDesignFromString(aString, aMainFileName: String);
  procedure _loadCheckers (aList, aXml: TXml);
  var
    x: Integer;
    xXml: TCustomBindable;
  begin
    for x := 0 to aList.Items.Count - 1 do with aList.Items.XmlItems[x] do
    begin
      xXml := aXml.FindUQ(Items.XmlValueByTag['Name']);
      if Assigned (xXml) then
        xXml.Checker := Items.XmlValueByTag['Value'];
    end;
  end;
var
  w, x, y, s, o, r, p, c, e: Integer;
  xDone: Boolean;
  xOperation: TWsdlOperation;
  xService: TWsdlService;
  xWsdl: TWsdl;
  xScript: TXml;
  wXml, xXml, sXml, oXml, eXml, eeXml, dXml, rXml, cXml: TXml;
  xBindName: String;
  xMessage: TWsdlMessage;
  xReadAnother: Boolean;
  xPatterns: TStringList;
begin
  Clear;
  xReadAnother := False;
  try
    xPatterns := TStringList.Create;
    try
      try
        xXml := TXml.Create;
        try
          xXml.LoadFromString(aString, nil);
          xXml.CheckDownLine (True);
          if aMainFileName = '' then
            aMainFileName := xXml.Items.XmlValueByTag ['FileName'];
          projectFileName := aMainFileName;
          projectProperties.Text := xXml.Items.XmlValueByTag['properties'];
          sXml := xXml.Items.XmlItemByTag ['Listeners'];
          Listeners.SpecificationXml.Items.Clear;
          if Assigned (sXml) then
          begin
            Listeners.SpecificationXml.CopyDownLine(sXml, True);
            Listeners.FromXml(HaveStompFrame);
          end;
          doValidateRequests := (xXml.Items.XmlValueByTag ['ValidateRequests'] = 'true');
          doValidateReplies := (xXml.Items.XmlValueByTag ['ValidateReplies'] = 'true');
          doCheckExpectedValues := xXml.Items.XmlBooleanByTagDef['CheckExpectedValues', False];
          _WsdlDisableOnCorrelate := xXml.Items.XmlBooleanByTagDef['DisableOnCorrelate', False];
          eXml := xXml.Items.XmlItemByTag ['ProjectOptions'];
          if Assigned (eXml) then
            ProjectOptions36FromXml(eXml);
          eXml := xXml.Items.XmlItemByTag ['projectOptions'];
          if Assigned (eXml) then
          begin
            ProjectOptionsFromXml(eXml);
            CurrentFolder := ExpandRelativeFileName (aMainFileName, CurrentFolder);
            ReferenceFolder := ExpandRelativeFileName (aMainFileName, ReferenceFolder);
            ReportsFolder := ExpandRelativeFileName (aMainFileName, ReportsFolder);
          end;
          xmlio.PathPrefixes.Text := xXml.Items.XmlCheckedValueByTag ['PathPrefixes'];
          eXml := xXml.Items.XmlItemByTag ['Environments'];
          if Assigned (eXml) then
            for e := 0 to eXml.Items.Count - 1 do
            begin
              with eXml.Items.XmlItems[e] do
              begin
                if TagName = 'Environment' then
                begin
                  eeXml := TXml.Create;
                  eeXml.Text := Items.XmlItems[0].Text;
                  EnvironmentList.AddObject ( Attributes.ValueByTag ['Name']
                                            , eeXml
                                            );
                end;
              end;
            end;
          ignoreDifferencesOn.Text := xXml.Items.XmlValueByTag ['ignoreDifferencesOn'];
          ignoreAddingOn.Text := xXml.Items.XmlValueByTag ['ignoreAddingOn'];
          ignoreRemovingOn.Text := xXml.Items.XmlValueByTag ['ignoreRemovingOn'];
          eXml := xXml.Items.XmlItemByTag ['ignoreOrderOn'];
          if Assigned (eXml) then
          begin
            for e := 0 to eXml.Items.Count - 1 do
            begin
              with eXml.Items.XmlItems[e] do
              begin
                if TagName = 'Element' then
                begin
                  y := ignoreOrderOn.Add(Items.XmlValueByTag['Id']);
                  ignoreOrderOn.Objects[y] := TStringList.Create;
                  (ignoreOrderOn.Objects[y] as TStringList).Text:=Items.XmlValueByTag['Keys'];
                end;
              end;
            end;
          end;
          regressionSortColumns.Text := xXml.Items.XmlValueByTag ['regressionSortColumns'];
          ignoreCoverageOn.Text := xXml.Items.XmlValueByTag ['ignoreCoverageOn'];
          FocusOperationName := xXml.Items.XmlValueByTag['FocusOperationName'];
          FocusOperationNameSpace := xXml.Items.XmlValueByTag['FocusOperationNameSpace'];
          FocusMessageIndex := xXml.Items.XmlIntegerByTag['FocusMessageIndex'];
          for w := 0 to xXml.Items.Count - 1 do
          begin
            wXml := xXml.Items.XmlItems [w];
            if wXml.TagName = 'Wsdl' then
            begin
              xDone := False;
              oXml := wXml.Items.XmlItemByTag['FreeFormatOperations'];
              if Assigned (oXml) then
              begin
                freeFormatOperationsUpdate(oXml);
                xWsdl := FreeFormatWsdl;
                xDone := True;
              end;
              oXml := wXml.Items.XmlItemByTag['CobolOperations'];
              if Assigned (oXml) then
              begin
                cobolOperationsUpdate(oXml, aMainFileName);
                xWsdl := CobolWsdl;
                xDone := True;
              end;
              oXml := wXml.Items.XmlItemByTag['XsdOperations'];
              if Assigned (oXml) then
              begin
                xsdOperationsUpdate(oXml, aMainFileName);
                xWsdl := XsdWsdl;
                xDone := True;
              end;
              oXml := wXml.Items.XmlItemByTag['SwiftMtOperations'];
              if Assigned (oXml) then
              begin
                swiftMtOperationsUpdate(oXml, aMainFileName);
                xWsdl := SwiftMtWsdl;
                xDone := True;
              end;
              if not xDone then
              begin
                try
                  xWsdl := WsdlOpenFile ( ExpandRelativeFileName (aMainFileName, wXml.Items.XmlValueByTag['WsdlLocation'])
                                        , wXml.Items.XmlIntegerByTagDef ['ElementsWhenRepeatable', -1]
                                        );
                except
                  on e: Exception do
                  begin
                    if BooleanPromptDialog ( 'Error: '
                                       + e.Message
                                       + #$D#$A
                                       + 'reading: '
                                       + ExpandRelativeFileName (aMainFileName, wXml.Items.XmlValueByTag['WsdlLocation'])
                                       + #$D#$A
                                       + Copy (wXml.AsText(False, 1, False, False),2,500)
                                       + #$D#$A
                                       + #$D#$A
                                       + ' try to open another file?; '
                                       ) then
                    begin
                      Application.CreateForm(TOpenWsdlForm, OpenWsdlForm);
                      try
                        OpenWsdlForm.WsdlLocationEdit.Text := ExpandRelativeFileName (aMainFileName, wXml.Items.XmlValueByTag['WsdlLocation']);
                        OpenWsdlForm.ShowModal;
                        if OpenWsdlForm.ModalResult = mrOK then
                        begin
                          xWsdl := WsdlOpenFile ( OpenWsdlForm.WsdlLocationEdit.Text
                                                , wXml.Items.XmlIntegerByTagDef ['ElementsWhenRepeatable', -1]
                                                );
                          wXml.Items.XmlItemByTag['WsdlLocation'].Value := OpenWsdlForm.WsdlLocationEdit.Text;
                          xReadAnother := True;
                        end;
                      finally
                        FreeAndNil(OpenWsdlForm);
                      end;
                    end
                    else
                      xWsdl := nil;
                  end;
                end;
                if Assigned (xWsdl) then
                  Wsdls.AddObject ( xWsdl.FileName
                                  , xWsdl
                                  );
              end;
              if Assigned (xWsdl) then
              begin
                xWsdl.xsdElementsWhenRepeatable := wXml.Items.XmlIntegerByTagDef['ElementsWhenRepeatable', -1];
                if xWsdl.xsdElementsWhenRepeatable > 0 then
                  xWsdl.XsdDescr.xsdElementsWhenRepeatable := xWsdl.xsdElementsWhenRepeatable
                else
                  xWsdl.XsdDescr.xsdElementsWhenRepeatable := xsdElementsWhenRepeatable;
                xWsdl.FileName := ExpandRelativeFileName (aMainFileName
                                                         , wXml.Items.XmlValueByTag['WsdlLocation']
                                                         );
                dXml := wXml.Items.XmlItemByTag ['ExtraXsds'];
                if Assigned (dXml) then
                begin
                  if dXml.Items.Count = 1 then
                  begin
                    xWsdl.ExtraXsdsFromXml (dXml.Items.XmlItems[0], SaveRelativeFileNames, aMainFileName);
                    xWsdl.LoadExtraXsds;
                  end;
                end;
                dXml := wXml.Items.XmlItemByTag ['ChangedElementDefs'];
                if Assigned (dXml)
                and (dXml.Items.Count > 0) then
                begin
                  xWsdl.XsdDescr.ChangedElementTypedefsFromXml (dXml);
                  for s := 0 to xWsdl.Services.Count - 1 do
                    with xWsdl.Services.Services[s] do
                      for o := 0 to Operations.Count - 1 do
                        with Operations.Operations[o] do
                        begin
                          xBindName := reqBind.Name;
                          reqBind.Free;
                          reqBind := TXml.Create(0, reqXsd);
                          reqBind.Name := xBindName;
                          xBindName := rpyBind.Name;
                          rpyBind.Free;
                          rpyBind := TXml.Create(0, rpyXsd);
                          rpyBind.Name := xBindName;
                        end;
                end;
                xWsdl.XsdDescr.Finalise;
                for x := 0 to wXml.Items.Count - 1 do
                begin
                  sXml := wXml.Items.XmlItems [x];
                  if (sXml.TagName = 'Service') then
                  begin
                    xService := xWsdl.ServiceByName [sXml.Items.XmlValueByTag['Name']];
                    if Assigned (xService) then
                    begin
                      xService.AuthenticationType := TAuthenticationType (sXml.Items.XmlIntegerByTagDef['AuthenticationType', 0]);
                      xService.UserName := sXml.Items.XmlValueByTag['UserName'];
                      xService.Password := Xmlz.DecryptString (sXml.Items.XmlValueByTag['Password']);
                      xService.PasswordType := TPasswordType (sXml.Items.XmlIntegerByTagDef['PasswordType', 0]);
                      xService.SuppressXmlComment := sXml.Items.XmlBooleanByTagDef['SuppressXmlComment', False];
                      xService.SuppressHTTP500 := sXml.Items.XmlBooleanByTagDef['SuppressHTTP500', False];
                      dXml := sXml.Items.XmlCheckedItemByTag['serviceOptions'];
                      if Assigned (dXml) then
                        xService.OptionsFromXml(dXml);
                      for y := 0 to sXml.Items.Count - 1 do
                      begin
                        oXml := sXml.Items.XmlItems [y];
                        if oXml.TagName = 'Operation' then
                        begin
                          xOperation := xService.OperationByName [oXml.Items.XmlValueByTag['Name']];
                          if Assigned (xOperation) then
                          begin
                            xOperation.Alias := oXml.Items.XmlValueByTagDef['Alias', xOperation.reqTagName];
                            if xOperation.Alias <> xOperation.reqTagName then
                            begin
                              xOperation.reqBind.Name := xOperation.alias;
                              xOperation.rpyBind.Name := xOperation.alias;
                            end;
                            dXml := oXml.Items.XmlItemByTag ['AddedTypeDefElements'];
                            if Assigned (dXml) then
                              xOperation.AddedTypeDefElementsFromXml (dXml);
                            dXml := oXml.Items.XmlItemByTag ['Action'];
                            if Assigned (dXml) then
                              xOperation.StubAction := TStubAction (StrToIntDef(dXml.Value, 0));
                            xOperation.HiddenFromUI := oXml.Items.XmlBooleanByTagDef ['HiddenFromUI', False];
                            xOperation.wsaEnabled := oXml.Items.XmlBooleanByTagDef ['wsaEnabled', False];
                            xOperation.wsaSpecificMustUnderstand := oXml.Items.XmlBooleanByTagDef ['wsaSpecificMustUnderstand', False];
                            xOperation.wsaMustUnderstand := oXml.Items.XmlBooleanByTagDef ['wsaMustUnderstand', False];
                            if xOperation.StubAction = saRequest then
                              xOperation.reqWsaXml.LoadValues (oXml.Items.XmlItemByTag ['wsa'], False)
                            else
                              xOperation.rpyWsaXml.LoadValues (oXml.Items.XmlItemByTag ['wsa'], False);
                            dXml := oXml.Items.XmlCheckedItemByTag['operationOptions'];
                            if Assigned (dXml) then
                              xOperation.OptionsFromXml(dXml);
                            xOperation.AsynchronousDialog := oXml.Items.XmlBooleanByTagDef ['AsynchronousDialog', False];
                            xOperation.doSuppressLog := oXml.Items.XmlIntegerByTagDef ['doSuppressLog', 0];
                            xOperation.DelayTimeMsMin := oXml.Items.XmlIntegerByTagDef ['DelayTimeMsMin', -1];
                            if xOperation.DelayTimeMsMin = -1 then
                            begin
                              xOperation.DelayTimeMsMin := oXml.Items.XmlIntegerByTagDef ['DelayTimeMs', 0];
                              xOperation.DelayTimeMsMax := xOperation.DelayTimeMsMin;
                            end
                            else
                              xOperation.DelayTimeMsMax := oXml.Items.XmlIntegerByTagDef ['DelayTimeMsMax', 0];
{BEGIN 3.6 Style}
                            xOperation.StubTransport := TTransportType (StrToIntDef (oXml.Items.XmlValueByTag ['StubTransport'], 0));
                            xOperation.StubHttpAddress := oXml.Items.XmlValueByTag ['Address'];
                            if xOperation.StubHttpAddress = '' then
                              xOperation.StubHttpAddress := oXml.Items.XmlValueByTag ['StubHttpAddress'];
                            xOperation.StubMqPutManager := oXml.Items.XmlValueByTag ['StubMqPutManager'];
                            xOperation.StubMqPutQueue := oXml.Items.XmlValueByTag ['StubMqPutQueue'];
                            xOperation.StubMqGetManager := oXml.Items.XmlValueByTag ['StubMqGetManager'];
                            xOperation.StubMqGetQueue := oXml.Items.XmlValueByTag ['StubMqGetQueue'];
                            xOperation.StubMqTimeOut := StrToIntDef(oXml.Items.XmlValueByTag ['StubMqTimeOut'], 30);
                            xOperation.StubMqHeaderXml.LoadValues (oXml.Items.XmlItemByTag ['mqHeader'], False);
                            xOperation.StubStompPutHost := oXml.Items.XmlValueByTag ['StubStompPutHost'];
                            xOperation.StubStompPutPort := oXml.Items.XmlValueByTagDef ['StubStompPutPort', '61613'];
                            xOperation.StubStompPutClientId := oXml.Items.XmlValueByTag ['StubStompPutClientId'];
                            xOperation.StubStompTimeOut := StrToIntDef(oXml.Items.XmlValueByTag ['StubStompTimeOut'], 30);
                            xOperation.StubStompHeaderXml.LoadValues (oXml.Items.XmlItemByTag ['stompHeader'], False);
                            xOperation.TacoConfigXml.LoadValues (oXml.Items.XmlItemByTag ['tacoConfig'], False);
 {END 3.6 Style}
                            dXml := oXml.Items.XmlItemByTag ['endpointConfig'];
                            if Assigned (dXml) then
                              xOperation.endpointConfigFromXml(dXml);
                            xOperation.BeforeScriptLines.Text := oXml.Items.XmlValueByTag ['BeforeScript'];
                            if (xOperation.BeforeScriptLines.Count = 0) then
                              xOperation.BeforeScriptLines.Text := oXml.Items.XmlValueByTag ['Script'];
                            xOperation.AfterScriptLines.Text := oXml.Items.XmlValueByTag ['AfterScript'];
                            cXml := oXml.Items.XmlItemByTag ['CorrelationElements'];
                            if Assigned (cXml) then
                              for c := 0 to cXml.Items.Count - 1 do
                                if cXml.Items.XmlItems [c].TagName = 'CorrelationElement' then
                                  xOperation.CorrelationBindables.AddObject ( cXml.Items.XmlItems [c].Value
                                                                            , xOperation.FindBind (cXml.Items.XmlItems [c].Value)
                                                                            );
                            cXml := oXml.Items.XmlItemByTag ['ExpectationElements'];
                            if Assigned (cXml) then
                              for c := 0 to cXml.Items.Count - 1 do
                                if cXml.Items.XmlItems [c].TagName = 'ExpectationElement' then
                                  xOperation.ExpectationBindables.AddObject ( cXml.Items.XmlItems [c].Value
                                                                            , xOperation.FindBind (cXml.Items.XmlItems [c].Value)
                                                                            );
                            xOperation.LogColumns.Text := oXml.Items.XmlValueByTag['LogColumns'];
                            for c := 0 to xOperation.LogColumns.Count - 1 do
                              xOperation.LogColumns.Bindables[c] := xOperation.FindBind(xOperation.LogColumns.Strings[c]);
                            dXml := oXml.Items.XmlItemByTag ['Messages'];
                            if not Assigned (dXml) then
                              dXml := oXml.Items.XmlItemByTag ['Replies']; // Old versions
                            if Assigned (dXml) then
                            begin
                              for r := 0 to dXml.Items.Count - 1 do
                              begin
                                with dXml.Items.XmlItems [r] do
                                begin
                                  xPatterns.Text := Items.XmlValueByTag ['Partern'];
                                  if xPatterns.Count = 0 then
                                  try
                                    with Items.XmlItemByTag ['Patterns'] do
                                      for p := 0 to Items.Count - 1 do
                                        xPatterns.Add(Items.XmlItems[p].Value);
                                  except
                                  end;
                                  if xOperation.StubAction = saRequest then
                                    xMessage := TWsdlMessage.CreateRequest( xOperation
                                                               , Items.XmlValueByTag ['Name']
                                                               , xPatterns.Text
                                                               , Items.XmlValueByTag ['Documentation']
                                                               )
                                  else
                                    xMessage := TWsdlMessage.CreateReply( xOperation
                                                               , Items.XmlValueByTag ['Name']
                                                               , xPatterns.Text
                                                               , Items.XmlValueByTag ['Documentation']
                                                               );
                                  rXml := Items.XmlItemByTag ['Request'];
                                  if Assigned (rXml) then
                                  begin
                                    if xOperation.WsdlService.DescriptionType in [ipmDTFreeFormat] then
                                    begin
                                      xMessage.FreeFormatReq := rXml.Value;
                                      xMessage.corBindsInit(xOperation);
                                      xMessage.PopulateCorrelation(xPatterns);
                                    end
                                    else
                                    begin
                                      if (rXml.Items.Count > 0) then
                                      begin
                                        if xOperation.reqBind is TXml then with xMessage.reqBind as TXml do
                                        begin
                                          LoadValues(rXml.Items.XmlItems [0], False);
                                        end;
                                        if xOperation.reqBind is TIpmItem then
                                          try
                                            (xMessage.reqBind as TIpmItem).LoadValues(rXml.Items.XmlItems [0]);
                                          except
                                          end;
                                      end;
                                    end;
                                  end;
                                  if xOperation.reqBind is TXml then
                                  begin
                                    rXml := Items.XmlItemByTag ['requestCheckers'];
                                    if Assigned (rXml) then
                                    begin
                                      _loadCheckers (rXml, xMessage.reqBind as TXml);
                                    end;
                                  end;
                                  rXml := Items.XmlItemByTag ['Reply'];
                                  if not Assigned (rXml) then
                                    rXml := Items.XmlItemByTag ['Data']; // Compatability; now Reply
                                  if Assigned (rXml) then
                                  begin
                                    if xOperation.WsdlService.DescriptionType in [ipmDTFreeFormat] then
                                      xMessage.FreeFormatRpy := rXml.Value
                                    else
                                    begin
                                      if (rXml.Items.Count > 0) then
                                      begin
                                        if xOperation.rpyBind is TXml then with xMessage.rpyBind as TXml do
                                        begin
                                          LoadValues(rXml.Items.XmlItems [0], False);
                                        end;
                                        if xOperation.rpyBind is TIpmItem then
                                          try
                                            (xMessage.rpyBind as TIpmItem).LoadValues(rXml.Items.XmlItems [0]);
                                          except
                                          end;
                                      end;
                                    end;
                                  end;
                                  if xOperation.rpyBind is TXml then
                                  begin
                                    rXml := Items.XmlItemByTag ['replyCheckers'];
                                    if Assigned (rXml) then
                                    begin
                                      _loadCheckers (rXml, xMessage.rpyBind as TXml);
                                    end;
                                  end;
                                  rXml := Items.XmlItemByTag ['Faults'];
                                  if Assigned (rXml)
                                  and (rXml.Items.Count > 0) then with xMessage.FltBind as TXml do
                                  begin
                                    LoadValues(rXml, False);
                                  end;
                                end;
                              end;
                            end;
                            dXml := oXml.Items.XmlItemByTag ['ColumnElements'];
                            if Assigned (dXml)
                            and (xOperation.Messages.Count > 0) then
                            begin
                              xMessage := xOperation.Messages.Messages [0];
                              for r := 0 to dXml.Items.Count - 1 do
                              begin
                                with dXml.Items do
                                begin
                                  xOperation.Messages.Messages[0].ColumnXmls.Add (XmlItems [r].Value);
                                end;
                              end;
                              UpdateReplyColumns(xOperation);
                            end;
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
          AcquireLock;
          try
            PrepareAllOperations (LogServerMessage);
          finally
            ReleaseLock;
          end;
          cXml := xXml.Items.XmlItemByTag ['Scripts'];
          if Assigned (cXml) then
          begin
            for s := 0 to cXml.Items.Count - 1 do with cXml.Items.XmlItems[s] do
            begin
              if (Name = 'Script')
              and (Attributes.ValueByTag['Name'] <> '') then  // compatibility with version 8 and earlier
              begin
                xScript := Scripts.AddXml(TXml.CreateAsString('Script', ''));
                xScript.AddXml (TXml.CreateAsString('Name', Attributes.ValueByTag['Name']));
                with xScript.AddXml (TXml.CreateAsString('Invoke', '')) do
                  with AddXml (TXml.CreateAsString('operations', '')) do
                    for o := 0 to allOperations.Count - 1 do
                      AddXml (TXml.CreateAsString('name', allOperations.Operations[o].Alias));
                xScript.AddXml (TXml.CreateAsString('Code', Value));
              end;
            end;
            if Scripts.Items.Count = 0 then
              Scripts.CopyDownLine(cXml, True)
            else
              Scripts.CheckDownline(True);
          end; // Scripts
        finally
          xXml.Free;
        end;
     finally
        stubChanged := xReadAnother;
        stubRead := True;
      end;
    finally
      xPatterns.Free;
    end;
  finally
{}
{}
  end;
end;

procedure TWsdlProject.ProjectOptions36FromXml(aXml: TXml);
var
  xXml, yXml: TXml;
begin
  _WsdlDbsEnabled := False;
  DbsType := '';
  DbsDatabaseName := '';
  DbsParams := '';
  DbsPassword := '';
  _WsdlDbsConnector.LoginPrompt := False;
  if Assigned (aXml) then {BEGIN 3.6 style}
  begin
    unknownOperation.StubAction := TStubAction (StrToIntDef (aXml.Items.XmlValueByTag ['notStubbedOperationAction'], 0));
    if unknownOperation.StubAction = saStub then
      notStubbedExceptionMessage := aXml.Items.XmlValueByTag ['notStubbedExceptionMessage'];
    unknownOperation.StubTransport := TTransportType (StrToIntDef (aXml.Items.XmlValueByTag ['notStubbedOperationTransportType'], 0));
    case unknownOperation.StubTransport of
      ttHttp: unknownOperation.StubHttpAddress := aXml.Items.XmlValueByTag ['notStubbedRedirectionUrl'];
      ttMq:
        begin
          unknownOperation.StubMqPutManager := aXml.Items.XmlValueByTag ['notStubbedMqPutManager'];
          unknownOperation.StubMqPutQueue := aXml.Items.XmlValueByTag ['notStubbedMqPutQueue'];
          if Assigned (endpointConfigXsd) then
          begin
            xXml := aXml.Items.XmlItemByTag ['notStubbedMqReplyToManager'];
            if Assigned (xXml) then
            begin
              yXml := unknownOperation.StubMqHeaderXml
                                      .Items.XmlItemByTag['mqmd']
                                      .Items.XmlItemByTag['ReplyToQMgr'];
              yXml.Checked := True;
              yXml.Value := xXml.Value;
            end;
            xXml := aXml.Items.XmlItemByTag ['notStubbedMqReplyToQueue'];
            if Assigned (xXml) then
            begin
              yXml := unknownOperation.StubMqHeaderXml
                                      .Items.XmlItemByTag['mqmd']
                                      .Items.XmlItemByTag['ReplyToQ'];
              yXml.Checked := True;
              yXml.Value := xXml.Value;
            end;
          end;
          unknownOperation.StubMqGetManager := aXml.Items.XmlValueByTag ['notStubbedMqGetManager'];
          unknownOperation.StubMqGetQueue := aXml.Items.XmlValueByTag ['notStubbedMqGetQueue'];
          unknownOperation.StubMqTimeOut := StrToIntdef (aXml.Items.XmlValueByTag ['notStubbedMqTimeOut'], 30);
        end;
      ttStomp:
        begin
          if Assigned (endpointConfigXsd) then
          begin
            unknownOperation.StubStompPutHost := aXml.Items.XmlValueByTag ['notStubbedStompHost'];
            unknownOperation.StubStompPutPort := aXml.Items.XmlValueByTag ['notStubbedStompPort'];
            xXml := aXml.Items.XmlItemByTag ['notStubbedStompDestination'];
            if Assigned (xXml) then
            begin
              yXml := unknownOperation.StubStompHeaderXml
                                      .Items.XmlItemByTag['destination'];
              yXml.Checked := True;
              yXml.Value := xXml.Value;
            end;
            xXml := aXml.Items.XmlItemByTag ['notStubbedStompReplyTo'];
            if Assigned (xXml) then
            begin
              yXml := unknownOperation.StubStompHeaderXml
                                      .Items.XmlItemByTag['reply-to'];
              yXml.Checked := True;
              yXml.Value := xXml.Value;
            end;
            unknownOperation.StubStompTimeOut := aXml.Items.XmlIntegerByTag ['notStubbedStompTimeOut'];
          end;
        end;
      ttTaco: ;
    end;
    xsdElementsWhenRepeatable := StrToIntDef (aXml.Items.XmlValueByTag ['ElementsWhenRepeatable'], 1);
  end; {END 3.6 style}
end;

procedure TWsdlProject .CreateLogReply (aLog : TLog ;
  var aProcessed : Boolean ; aIsActive : Boolean );
begin
  aProcessed := False;
  try
{}
    aLog.ReplyBody := CreateReply ( aLog
                                  , aLog.httpDocument
                                  , aLog.RequestBody
                                  , aLog.Operation
                                  , aLog.Mssg
                                  , aLog.CorrelationId
                                  , aLog.isAsynchronousRequest
                                  , aIsActive
                                  );
    aProcessed := True;
  except
    on e: exception do
    begin {}
      aLog.ReplyBody := e.message;
      aLog.Exception := e.Message;
      LogServerMessage(e.Message, True, e);
      if e.Message = S_NO_OPERATION_FOUND then
      begin
        aLog.Operation := nil;
        aLog.Mssg := nil;
        aLog.CorrelationId := '';
        case unknownOperation.StubAction of
          saStub: raise Exception.Create(notStubbedExceptionMessage);
          saForward: raise;
          saRedirect:
            begin
              aLog.ReplyBody := RedirectUnknownOperation(aLog);
              aProcessed := True;
              exit;
            end;
          saRequest: raise;
        end;
        if aLog.TransportType = ttHttp then
          raise Exception.Create (aLog.ReplyBody)
        else
          exit;
      end;
      if aLog.TransportType = ttHttp then
        raise Exception.Create (aLog.ReplyBody)
      else
        exit;
    end;
  end; //except
end;

function TWsdlProject.RedirectUnknownOperation (aLog : TLog ): String ;
var
  xOperation: TWsdlOperation;
  rUri: TIdUri;
begin
  result := '';
  aLog.Exception := '';
  xOperation := TWsdlOperation.Create(unknownOperation);
  try
    xOperation.SoapAction := aLog.httpSoapAction;
    case aLog.TransportType of
      ttHttp, ttHttps, ttSmtp:
      begin
        xOperation.httpVerb := aLog.httpCommand;
        rUri := TIdUri.Create(xOperation.StubHttpAddress);
        try
          if (rUri.Path = '/')
          and (rUri.Document = '') then
          begin
            rUri.Path := '';
            rUri.Document := aLog.httpDocument;
          end;
          xOperation.StubHttpAddress := rUri.URI;
          if alog.httpParams <> '' then
            xOperation.StubHttpAddress := xOperation.StubHttpAddress + '?' + aLog.httpParams;
        finally
          FreeAndNil (rUri);
        end;
      end;
    end;
    result := SendOperationMessage(xOperation, aLog.RequestBody);
  finally
    xOperation.Free;
  end;
end;

function TWsdlProject.CreateReply ( aLog: TLog
                                  ; aDocument, aRequest: String
                                  ; var aOperation: TWsdlOperation
                                  ; var aReply: TWsdlMessage
                                  ; var aCorrelationId: String
                                  ; var isAsynchronous: Boolean
                                  ; aIsActive: Boolean
                                  ): String;
var
  xOperation: TWsdlOperation;
  xReqXml, xRpyXml: TXml;
  x: Integer;
begin
  result := '';
  aOperation := nil;
  aReply := nil;
  aCorrelationId := '';
  isAsynchronous := False;
  xOperation := FindOperationOnRequest(aLog, aDocument, aRequest, doCloneOperations);
  if not Assigned (xOperation) then
    raise Exception.Create(S_NO_OPERATION_FOUND);
  try
    xOperation.Data := aLog;
    aOperation := xOperation;
    while Assigned(aOperation.Cloned) do
      aOperation := aOperation.Cloned;
    if aIsActive then
    begin
      aOperation.AcquireLock;
      Inc (aOperation.OperationCounter);
      aLog.OperationCount := aOperation.OperationCounter;
      aOperation.ReleaseLock;
    end;
    xOperation.InitDelayTime;
    if xOperation.doReadReplyFromFile then
      aReply := xOperation.Messages.Messages[0]
    else
      aReply := xOperation.MessageBasedOnRequest;
    if not Assigned (aReply) then
      Raise Exception.Create('Could not find any reply based on request');
    with xOperation do
    begin
      if lateBinding then
      begin
        for x := 0 to CorrelationBindables.Count - 1 do
          CorrelationBindables.Bindables[x] := FindBind(CorrelationBindables.Strings[x]);
        for x := 0 to ExpectationBindables.Count - 1 do
          ExpectationBindables.Bindables[x] := FindBind(ExpectationBindables.Strings[x]);
      end;
    end;
    aCorrelationId := xOperation.CorrelationIdAsText ('; ');
    if (xOperation.StubAction = saStub)
    and (aIsActive)
    and xOperation.wsaEnabled then
    begin
      if xOperation.AsynchronousDialog
      and Assigned (xOperation.reqWsaXml.FindUQXml('wsa.ReplyTo.Address')) then
      begin
        isAsynchronous := True;
        Exit;
      end;
    end;
    if xOperation.doReadReplyFromFile then
      xOperation.ReadReplyFromFile
    else
      if (xOperation.WsdlService.DescriptionType in [ipmDTFreeFormat]) then
        xOperation.FreeFormatRpy := aReply.FreeFormatRpy;
    if xOperation.lateBinding then
    begin
      if (xOperation.StubAction = saStub)
      and (aIsActive)
      and (Trim(xOperation.BeforeScriptLines.Text) <> '') then
      begin
        xReqXml := TXml.Create;
        xRpyXml := TXml.Create;
        try
          xRpyXml.LoadFromString(aReply.FreeFormatRpy, nil);
          if xRpyXml.Name <> '' then
            xOperation.rpyBind := xRpyXml;
          xReqXml.LoadFromString(aRequest, nil);
          if xReqXml.Name <> '' then
            xOperation.reqBind := xReqXml;
          xOperation.PrepareBefore;
          xOperation.ExecuteBefore;
          xOperation.FreeFormatRpy := xRpyXml.asString;
        finally
          FreeAndNil (xRpyXml);
          FreeAndNil (xReqXml);
          xOperation.reqBind := nil;
          xOperation.rpyBind := nil;
        end;
      end
    end
    else
    begin
      if not xOperation.doReadReplyFromFile then
      begin
        if xOperation.rpyBind is TIpmItem then
      //    xOperation.rpyIpm.BufferToValues (FoundErrorInBuffer, aReply.rpyIpm.ValuesToBuffer (nil))
          (xOperation.rpyBind as TIpmItem).LoadValues (aReply.rpyBind as TIpmItem)
        else
        begin
          (xOperation.rpyBind as TXml).ResetValues;
          (xOperation.rpyBind as TXml).LoadValues (aReply.rpyBind as TXml, True, True);
          (xOperation.fltBind as TXml).ResetValues;
          (xOperation.fltBind as TXml).LoadValues (aReply.fltBind as TXml, True, True);
        end;
      end;
      if (xOperation.StubAction = saStub)
      and (aIsActive) then
      begin
        xOperation.rpyWsaOnRequest;
        xOperation.ExecuteBefore;
        xOperation.ExecuteRpyStampers;
        if xOperation.doDebug
        and Assigned (OnDebugOperationEvent) then
        begin
          DebugOperation := xOperation;
          OnDebugOperationEvent;
        end;
      end;
    end;
    aLog.InitDisplayedColumns(xOperation, DisplayedLogColumns);
    aLog.doSuppressLog := (xOperation.doSuppressLog <> 0);
    aLog.DelayTimeMs := xOperation.DelayTimeMs;
    aLog.OperationName:=xOperation.Alias;
    result := xOperation.StreamReply (_progName, True);
    if xOperation.ReturnSoapFault then
      aLog.Exception := Result;
    if not isAsynchronous then
    begin
      aLog.ReplyBody := result;
      result := CreateLogReplyPostProcess(aLog, xOperation);
    end;
  finally
    if Assigned (xOperation.Cloned) then
      xOperation.Free;
  end;
end;

function TWsdlProject.CreateScriptOperation(aScript: TXml): TWsdlOperation;
var
  x: Integer;
  xWsdl: TWsdl;
  xInvoke: TXml;
  sOperation: TWsdlOperation;
begin
  if not Assigned(aScript)
  or (not (aScript is TXml))
  or (aScript.Name <> 'Script') then
    raise Exception.Create ('Illegal argument: TWsdlProject.CreateScriptOperation(aScript: TXml): TWsdlOperation');
  result := TWsdlOperation.CreateFromScriptXml(self, GetAbortPressed, aScript);
end;

procedure TWsdlProject.HaveStompFrame(aStompInterface: TStompInterface;
  aQueue: String; aFrame: IStompFrame);
var
  xProcessed: Boolean;
  xLog: TLog;
begin
  xProcessed := False;
  if (aFrame.GetCommand = 'MESSAGE') then
  begin
    xLog := TLog.Create;
    try
      xLog.InboundTimestamp := Now;
      xLog.TransportType := ttStomp;
      xLog.RequestHeaders := aFrame.GetHeaders.OutputAsXmlText;
      xLog.RequestBody := aFrame.GetBody
                            + aStompInterface.RequestBodyPostFix; // WORKAROUND, see XSD
      xLog.InboundBody := xLog.RequestBody;
      try
    {$ifdef windows}
        CoInitialize(nil);
    {$endif}
        try
          CreateLogReply (xLog, xProcessed, True);
          DelayMS (xLog.DelayTimeMs);
        finally
    {$ifdef windows}
          CoUninitialize;
    {$endif}
        end;
      except
        on e: exception do
        begin
          LogServerMessage(format('Exception %s. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
          xLog.Exception := e.Message;
        end;
      end;
      if (xLog.ReplyBody <> '')
      and (   (aFrame.GetHeaders.Value('reply-to') <> '')
           or (aFrame.GetHeaders.Value('ReplyQueue') <> '')
          ) then
      begin
        try
          aStompInterface.PutReply (xLog.ReplyBody, aFrame, xLog.ReplyHeaders);
        except
          on e: exception do
          begin
            LogServerMessage(format('Exception %s in Stomp PutReply. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
            xLog.Exception := e.Message;
          end;
        end;
      end;
      if xProcessed
      and (aStompInterface.DequeueOn = 'Process') then
      begin
        try
          aStompInterface.StompClient.Ack (aFrame.GetHeaders.Value ('message-id'));
        except
          on e: exception do
          begin
            LogServerMessage(format('Exception %s in Stomp Ack. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
            xLog.Exception := e.Message;
          end;
        end;
      end;
    finally
      xLog.OutboundTimeStamp := Now;
      DisplayLog ('', xLog);
    end;
  end;
  if (aFrame.GetCommand = 'ERROR') then
    LogServerMessage ( 'received Stomp-ERROR frame on queue ' + aQueue
                       + LINE_END
                       + aFrame.Output
                       , False
                       , nil
                       );
end;

function TWsdlProject.httpRequestStreamToString(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): String;
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

function TWsdlProject.mergeUri(puri, suri: String): String;
var
  s, r: TIdUri;
begin
  r := TIdUri.Create(puri);
  s := TIdUri.Create(suri);
  try
    if r.Protocol = '' then
      r.Protocol := s.Protocol;
    if r.Host = '' then
      r.Host := s.Host;
    if r.Port = '' then
      r.Port := s.Port;
    if (r.Path = '')
    or (r.Path = '/') then
      r.Path := s.Path;
    if r.Document = '' then
      r.Document := s.Document;
    result := r.URI;
  finally
    FreeAndNil(r);
    FreeAndNil(s);
  end;
end;

procedure TWsdlProject.ProjectOptionsLogDisplayedColumnsFromXml(aXml: TXml);
var
  x: Integer;
  s: String;
begin
  if not Assigned (aXml) then Exit;
  if aXml.Name <> 'DisplayedColumns' then
    raise Exception.CreateFmt('TWsdlProject.ProjectOptionsLogDisplayedColumnsFromXml(aXml: TXml); illegal Xml %s', [aXml.Name]);
  s := DisplayedLogColumns.Text;
  DisplayedLogColumns.Clear;
  for x := 0 to aXml.Items.Count - 1 do with aXml.Items.XmlItems[x] do
    if Checked
    and (Name = 'DisplayedColumn') then
      if Assigned (Items.XmlCheckedItemByTag['Header']) then
        DisplayedLogColumns.Add(Items.XmlCheckedItemByTag['Header'].Value);
  if DisplayedLogColumns.Text <> s then
    displayedLogs.InvalidateDisplayedColumns;
end;

procedure TWsdlProject .ProjectLogOptionsFromXml (aXml : TXml );
var
  yXml: TXml;
begin
  displayedLogsMaxEntries := -1;
  CompareLogOrderBy := clTimeStamp;
  ShowLogCobolStyle := slCobol;
  if Assigned (aXml) then
  begin
    try
      displayedLogsMaxEntries := aXml.Items.XmlIntegerByTagDef['maxEntries', -1];
    except
      displayedLogsMaxEntries := -1;
    end;
    yXml := aXml.Items.XmlCheckedItemByTag['CompareLogsOrdered'];
    if Assigned (yXml) then
    begin
      if yXml.Value = 'As is' then CompareLogOrderBy := clTimeStamp;
      if yXml.Value = 'Service, Operation' then CompareLogOrderBy := clOperation;
      if yXml.Value = 'Service, Operation, Correlation' then CompareLogOrderBy := clCorrelation;
    end;
    yXml := aXml.Items.XmlCheckedItemByTag['DocumentComparison'];
    if Assigned (yXml) then
    begin
      wrdFunctionz.wrdDetectFormatChanges := yXml.Items.XmlBooleanByTagDef['DetectDocumentFormatChanges', False];
      wrdFunctionz.wrdNewDocumentAsReference := yXml.Items.XmlBooleanByTagDef['NewDocumentAsReference', False];
      wrdFunctionz.wrdExpectedDifferenceCount := yXml.Items.XmlIntegerByTagDef['ExpectedDifferenceCount', 0];
    end;
    yXml := aXml.Items.XmlCheckedItemByTag['ShowCobolDataAs'];
    if Assigned (yXml) then
    begin
      if yXml.Value = 'Cobol' then ShowLogCobolStyle := slCobol;
      if yXml.Value = 'Xml' then ShowLogCobolStyle := slXml;
    end;
    ProjectOptionsLogDisplayedColumnsFromXml(aXml.Items.XmlCheckedItemByTag['DisplayedColumns']);
  end;
end;

procedure TWsdlProject .ProjectOptionsFromXml (aXml : TXml );
var
  xXml, yXml, hXml: TXml;
begin
  if not Assigned (aXml) then Exit;
  if aXml.Name <> 'projectOptions' then raise Exception.Create('ProjectOptionsFromXml illegal XML' + aXml.Text);
  DatabaseConnectionSpecificationXml.Items.Clear;
  wrdFunctionz.wrdDetectFormatChanges := False;
  wrdFunctionz.wrdNewDocumentAsReference := False;
  wrdFunctionz.wrdExpectedDifferenceCount := 0;
  RemoteControlPortNumber := 3738;
  OnRequestViolatingAddressPath:=rvsContinue;
  OnRequestViolatingSchema:=rvsContinue;
  xsdElementsWhenRepeatable := defaultXsdElementsWhenRepeatable;
  xsdMaxDepthBillOfMaterials := defaultXsdMaxDepthBillOfMaterials;
  xsdMaxDepthXmlGen := defaultXsdMaxDepthXmlGen;
  _WsdlDisableOnCorrelate := False;
  unknownOperation.StubAction := saStub;
  PublishDescriptions := False;
  OperationsWithEndpointOnly := True;
  SaveRelativeFileNames := True;
  CurrentFolder := '';
  ReferenceFolder := '';
  ReportsFolder := '';
  notStubbedExceptionMessage := 'No operation recognised';

  if not aXml.Checked then Exit;
  with aXml.Items do
  begin
    xXml := XmlCheckedItemByTag ['General'];
    if Assigned (xXml) then
    begin
      SaveRelativeFileNames := xXml.Items.XmlCheckedBooleanByTagDef['SaveRelativeFileNames', True];
      yXml := xXml.Items.XmlCheckedItemByTag['projectFolders'];
      if Assigned (yXml) then with yXml.Items do
      begin
        CurrentFolder := XmlCheckedValueByTag['current'];
        ReferenceFolder := XmlCheckedValueByTag['reference'];
        ReportsFolder := XmlCheckedValueByTagDef['reports', CurrentFolder];
      end;
    end;
    ProjectLogOptionsFromXml (XmlCheckedItemByTag ['Log']);
    xXml := XmlCheckedItemByTag ['Wsdl'];
    if Assigned (xXml) then
    begin
      PublishDescriptions := xXml.Items.XmlCheckedBooleanByTagDef['PublishDescriptions', False];
      OperationsWithEndpointOnly := xXml.Items.XmlCheckedBooleanByTagDef['OperationsWithEndpointOnly', True];
      xsdElementsWhenRepeatable := xXml.Items.XmlCheckedIntegerByTagDef['ElementsWhenRepeatable', xsdElementsWhenRepeatable];
      xsdMaxDepthBillOfMaterials := xXml.Items.XmlCheckedIntegerByTagDef['MaxDepthWhenRecursive', xsdMaxDepthBillOfMaterials];
      xsdMaxDepthXmlGen := xXml.Items.XmlCheckedIntegerByTagDef['MaxDepthXmlGen', xsdMaxDepthXmlGen];
    end;
    xXml := XmlCheckedItemByTag ['OnCorrelate'];
    if Assigned (xXml) then
    begin
      _WsdlDisableOnCorrelate := xXml.Items.XmlCheckedBooleanByTagDef['DisableMessage', _WsdlDisableOnCorrelate];
    end;
    xXml := XmlCheckedItemByTag ['UnknownOperations'];
    if Assigned (xXml) then
    begin
      yXml := xXml.Items.XmlCheckedItemByTag ['RaiseErrorMessage'];
      if Assigned (yXml) then
      begin
        unknownOperation.StubAction := saStub;
        notStubbedExceptionMessage := xXml.Items.XmlCheckedValueByTagDef['RaiseErrorMessage', notStubbedExceptionMessage];
      end;
      yXml := xXml.Items.XmlCheckedItemByTag ['Redirect'];
      if Assigned (yXml) then
      begin
        unknownOperation.StubAction := saRedirect;
        unknownOperation.endpointConfigFromXml(yXml);
      end;
    end;
    xXml := XmlCheckedItemByTag ['OperationDefaults'];
    if Assigned (xXml) then
    begin
      yXml := xXml.Items.XmlCheckedItemByTag ['OnRequestViolatingAddressPath'];
      if Assigned (yXml) then
      begin
        if Assigned (yXml.Items.XmlCheckedItemByTag ['AddRemark']) then
          OnRequestViolatingAddressPath:=rvsAddRemark;
        if Assigned (yXml.Items.XmlCheckedItemByTag ['RaiseErrorMessage']) then
          OnRequestViolatingAddressPath:=rvsRaiseErrorMessage;
      end;
      yXml := xXml.Items.XmlCheckedItemByTag ['OnRequestViolatingSchema'];
      if Assigned (yXml) then
      begin
        if Assigned (yXml.Items.XmlCheckedItemByTag ['AddRemark']) then
          OnRequestViolatingSchema:=rvsAddRemark;
        if Assigned (yXml.Items.XmlCheckedItemByTag ['RaiseErrorMessage']) then
          OnRequestViolatingSchema:=rvsRaiseErrorMessage;
      end;
    end;
    xXml := XmlCheckedItemByTag ['DatabaseConnection'];
    if Assigned (xXml) then
      DatabaseConnectionSpecificationXml.CopyDownLine(xXml, True);
    DatabaseConnectionSpecificationFromXml;
  end;
end;

procedure TWsdlProject .UpdateReplyColumns (aOperation : TWsdlOperation );
  procedure _UpdateFirstRow;
  var
    c: Integer;
  begin
    with aOperation.Messages.Messages[0] do
    begin
      for c := 0 to CorrelationBindables.Count - 1 do
        if Assigned (CorrelationBindables.Bindables[c]) then
          CorrelationBindables.Bindables[c].CorrelationValue := '.*' ;
      for c := 0 to ColumnXmls.Count - 1 do
      begin
        if AnsiStartsText ('Rpy.', ColumnXmls.Strings[c]) then
          ColumnXmls.Bindables [c] := rpyBind.FindUQ (Copy ( ColumnXmls.Strings[c]
                                                           , 5
                                                           , Length (ColumnXmls.Strings[c])
                                                           )
                                                     )
        else
          ColumnXmls.Bindables [c] := reqBind.FindUQ (Copy ( ColumnXmls.Strings[c]
                                                           , 5
                                                           , Length (ColumnXmls.Strings[c])
                                                           )
                                                     );
      end;
    end;
  end;
var
  y: Integer;
begin
  with aOperation.Messages do
  begin
    y := 0;
    Messages[y].corBindsInit(aOperation);
    _UpdateFirstRow;
    for y := 1 to Count - 1 do
    begin
      Messages[y].corBindsInit(aOperation);
      Messages[y].ColumnXmls.Clear;
      UpdateMessageRow (aOperation, Messages [y]);
    end;
  end;
end;

function TWsdlProject .WsdlOpenFile (aName : String ;
  aElementsWhenRepeatable : Integer ): TWsdl ;
begin
  if UpperCase (ExtractFileExt (aName)) = '.SDF' then
  begin
    result := TWsdl.Create(EnvVars, aElementsWhenRepeatable, xsdElementsWhenRepeatable, OperationsWithEndpointOnly);
    result.FileName := aName;
    try
      result.LoadFromSdfFile(aName);
    except
      on e: Exception do
      begin
        result.Free;
        result := nil;
        raise Exception.Create ( 'Error opening '
                               + aName
                               + CRLF
                               + e.Message
                               );
      end;
    end;
  end
  else
  begin
    result := TWsdl.Create(EnvVars, aElementsWhenRepeatable, xsdElementsWhenRepeatable, OperationsWithEndpointOnly);
    result.LoadFromSchemaFile(aName, nil);
  end;
end;

procedure TWsdlProject .UpdateMessageRow (aOperation : TWsdlOperation ;
  aMessage : TWsdlMessage );
var
  c: Integer;
  Bind: TCustomBindable;
  sMessage: TWsdlMessage;
begin
  try
    sMessage := aOperation.Messages.Messages [0];
    while aMessage.ColumnXmls.Count < sMessage.ColumnXmls.Count do
      aMessage.ColumnXmls.Add('');
    for c := 0 to sMessage.ColumnXmls.Count - 1 do
    begin
      if Assigned (sMessage.ColumnXmls.Bindables[c]) then
      begin
        if sMessage.rpyBind.IsAncestorOf (sMessage.ColumnXmls.Bindables[c]) then
//      if AnsiStartsStr('Rpy', sMessage.ColumnXmls.Strings[c]) then
        begin
          if aMessage.rpyBind is TXml then
            Bind := (aMessage.rpyBind as TXml).FindByRefId(sMessage.ColumnXmls.Bindables[c].RefId); // fast but unreliable
          if not Assigned (Bind)
          or (aMessage.rpyBind is TIpmItem)
          or (Bind.IndexCaption <> sMessage.ColumnXmls.Bindables[c].IndexCaption) then
            Bind := aMessage.rpyBind.FindUQ(Copy (sMessage.ColumnXmls.Strings[c], 5, 100000)); // slow but reliable
        end
        else
        begin
          if aMessage.reqBind is TXml then
            Bind := (aMessage.reqBind as TXml).FindByRefId(sMessage.ColumnXmls.Bindables[c].RefId); // fast but unreliable
          if not Assigned (Bind)
          or (aMessage.reqBind is TIpmItem)
          or (Bind.IndexCaption <> sMessage.ColumnXmls.Bindables[c].IndexCaption) then
            Bind := aMessage.reqBind.FindUQ(Copy (sMessage.ColumnXmls.Strings[c], 5, 100000)); // slow but reliable
        end;
        aMessage.ColumnXmls.Objects[c] := Bind;
      end
      else
        aMessage.ColumnXmls.Objects[c] := nil;
    end;
  finally
  end;
end;

function TWsdlProject .SendOperationMessage (aOperation : TWsdlOperation ;
  aMessage : String ): String ;
var
  reqheader, rpyheader, responsecode: String;
begin
  reqheader := '';
  rpyheader := '';
  responsecode := '';
  case aOperation.StubTransport of
    ttHttp: result := SendHttpMessage (aOperation, aMessage, reqheader, rpyheader, responsecode);
    ttMq: result := SendOperationMqMessage (aOperation, aMessage, reqheader);
    ttStomp: result := SendOperationStompMessage (aOperation, aMessage, reqheader, rpyheader);
    ttTaco: result := SendOperationTacoMessage(aOperation, aMessage, reqheader, rpyheader);
    ttNone: result := SendNoneMessage(aOperation, aMessage);
  end;
end;

function TWsdlProject .SendOperationMqMessage (aOperation : TWsdlOperation ;
  aMessage : String ; var aMqHeaderAsText : String ): String ;
var
  mq: TMqInterface;
  xIsRequest: Boolean;
  fXml: TXml;
begin
  Result := '';
  fXml := nil;
  if not Assigned (aOperation)
    then raise Exception.Create('SendOperationMqMessage: null arguments');
  mq := TMqInterface.Create;
  try
    mq.Use := mqUse;
    mq.Qmanager := resolveAliasses(aOperation.StubMqPutManager, projectProperties);
    mq.PutQueue := resolveAliasses(aOperation.StubMqPutQueue, projectProperties);
    mq.GetQueue := resolveAliasses(aOperation.StubMqGetQueue, projectProperties);
    mq.TimeOut := IntToStr (aOperation.StubMqTimeOut);
    mq.Expiry := '-1';
    xIsRequest := True;
    if Assigned (aOperation.StubMqHeaderXml) then
      fXml := aOperation.StubMqHeaderXml.FindXml ('mqHeader.mqmd.MsgType');
    if (    Assigned (fXml)
        and fXml.CheckedAllUp
        and (fXml.Value <> '1')
        )
    or (mq.GetQueue = '')
    then
      xIsRequest := False;
    try
      if xIsRequest then
        Result := mq.RequestReply (aMessage, aOperation.StubMqHeaderXml)
      else
        mq.Put (aMessage, aOperation.StubMqHeaderXml);
    finally
      try
        aMqHeaderAsText := mq.MsgDescAsText;
      except
      end;
    end;
  finally
    FreeAndNil (mq);
  end;
end;

function TWsdlProject .SendHttpMessage (aOperation : TWsdlOperation ;
  aMessage : String ; var aReqHeader , aRpyHeader , aResponseCode : String
  ): String ;
  function _Decompress (aContentEncoding: String; aStream: TMemoryStream): String;
  var
    xStream: TMemoryStream;
  begin
    result := '';
    aStream.Position := 0;
    if (aContentEncoding <> '')
    and (aContentEncoding <> 'identity') then
    begin
      xStream := TMemoryStream.Create;
      try
        GZIPUtils.ZUncompressStream(aStream, xStream);
        xStream.Position := 0;
        SetLength(Result,xStream.Size);
        xStream.Read(Pointer(Result)^,xStream.Size);
      finally
        xStream.Free;
      end;
    end
    else
    begin
      SetLength(Result,aStream.Size);
      aStream.Read(Pointer(Result)^,aStream.Size);
    end;
  end;

var
  HttpClient: TIdHTTP;
  HttpRequest, sStream, dStream: TMemoryStream;
  URL: String;
  oUri, sUri: TIdUri;
  x: Integer;
begin
  Result := '';
  if not Assigned (aOperation)
    then raise Exception.Create('SendHttpMessage: null arguments');
  HttpClient := TIdHTTP.Create;
  try
    HttpRequest := TMemoryStream.Create;
    try
      if aOperation.StubHttpAddress <> '' then
      begin
        ppLock.Acquire;
        try
          sUri := TIdUri.Create(resolveAliasses(aOperation.StubHttpAddress, projectProperties));
        finally
          ppLock.Release;
        end;
        if aOperation.SoapAddress <> '' then
        begin
          oUri := TIdUri.Create(aOperation.SoapAddress);
          try
            if (sUri.Protocol = '') then
              sUri.Protocol := oUri.Protocol;
            if aOperation.useSsl then
              sUri.Protocol := 'https';
            if (sUri.Path = '/')
            and (sUri.Document = '') then
            begin
              sUri.Path := oUri.Path;
              sUri.Document := oUri.Document;
            end;
          finally
            FreeAndNil (oUri);
          end;
        end;
        URL := sUri.URI;
        FreeAndNil (sUri);
      end
      else
        URL := aOperation.SoapAddress;
      try
        HttpClient.Request.CustomHeaders.Values ['SOAPAction'] := '"' + aOperation.SoapAction + '"';
      except
      end;
      if Assigned (aOperation.StubCustomHeaderXml)
      and aOperation.StubCustomHeaderXml.Checked then
        with aOperation.StubCustomHeaderXml.Items do
          for x := 0 to Count - 1 do
            if (XmlItems[x].Name = 'Header')
            and (XmlItems[x].Checked) then
              with XmlItems[x].Items do
                HttpClient.Request.CustomHeaders.Values [XmlCheckedValueByTag ['Name']]
                                                      := XmlCheckedValueByTag ['Value'];
      HttpClient.Request.ContentType := 'text/xml';
      HttpClient.Request.CharSet := '';
      HttpClient.Request.ContentEncoding := aOperation.ContentEncoding;
      HttpClient.Request.AcceptEncoding := 'identity';
      if aOperation.AcceptDeflateEncoding then
        HttpClient.Request.AcceptEncoding := HttpClient.Request.AcceptEncoding + ', deflate';
      if aOperation.AcceptGzipEncoding then
        HttpClient.Request.AcceptEncoding := HttpClient.Request.AcceptEncoding + ', gzip';
{}
      if (HttpClient.Request.ContentEncoding = 'deflate')
      or (HttpClient.Request.ContentEncoding = 'gzip') then
      begin
        sStream := TMemoryStream.Create;
        try
          WriteStringToStream(aMessage, sStream);
          sStream.Position := 0;
          if HttpClient.Request.ContentEncoding = 'deflate' then
            GZIPUtils.deflate(sStream, HttpRequest);
          if HttpClient.Request.ContentEncoding = 'gzip' then
            GZIPUtils.GZip(sStream, HttpRequest);
          HttpRequest.Position := 0;
        finally
          sStream.Free;
        end;
      end
      else
        WriteStringToStream(aMessage, HttpRequest);
      if doViaProxyServer then
      begin
        HttpClient.ProxyParams.ProxyServer := ViaProxyServer;
        HttpClient.ProxyParams.ProxyPort := ViaProxyPort;
      end
      else
      begin
        HttpClient.ProxyParams.ProxyServer := '';
        HttpClient.ProxyParams.ProxyPort := 0;
      end;
      if Assigned(aOperation.WsdlService)
      and (aOperation.WsdlService.AuthenticationType = atHTTPBasicAuthentication) then
      begin
        HttpClient.Request.BasicAuthentication := True;
        HttpClient.Request.Username := aOperation.WsdlService.Username;
        HttpClient.Request.Password := aOperation.WsdlService.Password;
      end;
      if aOperation.useSsl then
      begin
        HttpClient.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
        with (HttpClient.IOHandler as TIdSSLIOHandlerSocketOpenSSL) do
        begin
          SSLOptions.CertFile := aOperation.sslCertificateFile;
          SSLOptions.KeyFile := aOperation.sslKeyFile;
          SSLOptions.RootCertFile := aOperation.sslRootCertificateFile;
          SSLOptions.Method := aOperation.sslVersion;
          SSLOptions.Mode := sslmUnassigned;
          SSLOptions.VerifyMode := [];
        end;
      end;
      try
        dStream := TMemoryStream.Create;
        try
          try
            try
              with aOperation do
              begin
                if httpVerb = 'DELETE' then HttpClient.Delete(URL);
                if httpVerb = 'GET' then httpClient.Get(URL, dStream);
                if httpVerb = 'HEAD' then HttpClient.Head(URL);
                if httpVerb = 'OPTIONS' then HttpClient.Options(URL);
                if httpVerb = 'POST' then
                  HttpClient.Post(URL, HttpRequest, dStream);
                if httpVerb = 'PUT' then HttpClient.Put(URL, HttpRequest, dStream);
                if httpVerb = 'TRACE' then httpClient.Trace(URL, dStream);
              end;
            finally
              aReqHeader := HttpClient.Request.RawHeaders.Text;
              aRpyHeader := HttpClient.Response.RawHeaders.Text;
              aResponseCode := IntToStr(HttpClient.ResponseCode);
            end;
            result := _Decompress (HttpClient.Response.ContentEncoding, dStream);
          except
            on e: EIdHTTPProtocolException do
            begin
              result := e.ErrorMessage;
            end;
          end;
        finally
          FreeAndNil (dStream);
        end;
      finally
      end;
      if (HttpClient.ResponseCode = 202) then
        result := S_MESSAGE_ACCEPTED;
      if HttpClient.ResponseCode = 500 then
        raise Exception.Create(Result);
      if HttpClient.Connected then {in case server s-alive}
        HttpClient.Disconnect;
    finally
      FreeAndNil (HttpRequest);
    end;
  finally
    if Assigned (HttpClient.IOHandler) then
    begin
      HttpClient.IOHandler.Free;
      HttpClient.IOHandler := nil;
    end;
    FreeAndNil (HttpClient);
  end;
end;

function TWsdlProject .SendMessage (aOperation : TWsdlOperation ;
  aRequest : TWsdlMessage ; aCorrelationId : String ): String ;
  procedure _OperationCount(aLog: TLog);
  begin
    aLog.Operation.AcquireLock;
    try
      Inc (aLog.Operation.OperationCounter);
      aLog.OperationCount := aLog.Operation.OperationCounter;
    finally
      aLog.Operation.ReleaseLock;
    end;
  end;
var
  xXml: TXml;
  xNow: TDateTime;
  sl: TStringList;
  x: Integer;
  xLog: TLog;
  xMessage: String;
  procedure _setupForAsynchronousReply;
  var
    xXml: TXml;
  begin
    with xLog do
    begin
      ReplyBody := '';
      xXml := aOperation.reqWsaXml.Items.XmlItemByTag ['MessageID'];
      xXml.Value := CorrId;
      xXml.Checked := True;
      AcquireLogLock;
      try
        AsynchRpyLogs.SaveLog(CorrId, xLog);
      finally
        ReleaseLogLock;
      end;
    end;
  end;
begin
  xNow := Now;
  Result := '';
  xMessage := '';
  if not Assigned (aOperation)
    then raise Exception.Create('SendMessage: null arguments');
  xLog := TLog.Create;
  try
    try
      xLog.Operation := aOperation;
      xLog.Operation.Data := xLog;
      while Assigned(xLog.Operation.Cloned) do
        xLog.Operation := xLog.Operation.Cloned;
      _OperationCount(xLog);
      if aOperation.wsaEnabled
      and aOperation.AsynchronousDialog
      and (not aOperation.isOneWay) then
        _setupForAsynchronousReply;
      xLog.TransportType := aOperation.StubTransport;
      if Assigned (aRequest) then
        aOperation.ReqBindablesFromWsdlMessage(aRequest);
      xLog.Mssg := aOperation.CorrelatedMessage;
      if aOperation.wsaEnabled then
        try
          aOperation.reqWsaOnRequest;
          with aOperation.reqWsaXml.FindUQXml('wsa.MessageID') do
          begin
            Value := xLog.CorrId;
            Checked := True;
          end;
        except
        end;
      sl := TStringList.Create;
      try
        ExplodeStr(aCorrelationId, ';', sl);
        for x := 0 to sl.Count - 1 do
        begin
          if x < aOperation.CorrelationBindables.Count then
          begin
            aOperation.CorrelationBindables.Bindables[x].Value := sl.Strings [x];
            aOperation.CorrelationBindables.Bindables[x].Checked := (sl.Strings [x] <> '&nil');
          end;
        end;
      finally
        FreeAndNil (sl);
      end;
      aOperation.ExecuteBefore;
      aOperation.ExecuteReqStampers;
      if doValidateRequests then
      begin
        if not aOperation.reqBind.IsValueValid (xMessage) then
          xLog.RequestValidateResult := xMessage;
        xLog.RequestValidated := True;
      end;
      xLog.RequestBody := aOperation.StreamRequest (_progName, True, True, True);
      xLog.OutboundTimeStamp := Now;
      xLog.httpCommand := aOperation.httpVerb;
      try
        case aOperation.StubTransport of
          ttHttp: xLog.ReplyBody := SendHttpMessage (aOperation, xLog.RequestBody, xLog.RequestHeaders, xLog.ReplyHeaders, xLog.httpResponseCode);
          ttMq: xLog.ReplyBody := SendOperationMqMessage (aOperation, xLog.RequestBody, xLog.RequestHeaders);
          ttStomp: xLog.ReplyBody := SendOperationStompMessage (aOperation, xLog.RequestBody, xLog.RequestHeaders, xLog.ReplyHeaders);
          ttSmtp: xLog.ReplyBody := SendOperationSmtpMessage (aOperation, xLog.RequestBody, xLog.RequestHeaders, xLog.ReplyHeaders);
          ttTaco: xLog.ReplyBody := SendOperationTacoMessage (aOperation, xLog.RequestBody, xLog.RequestHeaders, xLog.ReplyHeaders);
          ttNone: xLog.ReplyBody := SendNoneMessage(aOperation, xlog.RequestBody);
        end;
      finally
        xLog.InboundTimeStamp := Now;
      end;
      if xLog.ReplyBody = S_MESSAGE_ACCEPTED then
      begin
        xLog.ReplyBody := '';
        if aOperation.rpyBind.Name = '' then
          aOperation.ExecuteAfter;
      end
      else
      begin
        if aOperation.lateBinding then
        begin
          with aOperation.rpyBind as TXml do
          begin
            LoadFromString(xlog.ReplyBody, nil);
            if Name = '' then
              Name := 'noXml';
            aOperation.PrepareAfter;
          end;
        end
        else
        begin
          if aOperation.reqBind is TXml then
          begin
            xXml := TXml.Create;
            try
              try
                xXml.LoadFromString(xLog.ReplyBody, nil);
              except
                on e: exception do
                  raise Exception.CreateFmt('%s could not parse XML reply (%s)', [_ProgName, e.Message]);
              end;
              aOperation.SoapXmlReplyToBindables(xXml, True);
          //              aOperation.rpyBind.LoadValues(xXml, True, False);
            finally
              xXml.Free;
            end;
          end;
          if aOperation.reqBind is TIpmItem then
            (aOperation.rpyBind as TIpmItem).BufferToValues (FoundErrorInBuffer, xLog.ReplyBody);
          if doValidateReplies then
          begin
            if not aOperation.rpyBind.IsValueValid (xMessage) then
              xLog.ReplyValidateResult := xMessage;
            xLog.ReplyValidated := True;
          end;
        end;
        aOperation.ExecuteAfter;
        CheckExpectedValues (xLog, aOperation, doCheckExpectedValues);
      end;
      with xLog do
      begin
//      RequestHeaders := HttpClient.Request.CustomHeaders.Text;
        Mssg := aOperation.CorrelatedMessage;
        CorrelationId := aOperation.CorrelationIdAsText ('; ');
        Stubbed := True;
        StubAction := aOperation.StubAction;
        doSuppressLog := (aOperation.doSuppressLog <> 0);
      end;
    except
      on e: exception do
      begin
        LogServerMessage(format('Exception %s in SendSoapRequest. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
        with xLog do
        begin
          if InboundTimeStamp = 0 then
            InboundTimeStamp := Now;
//          RequestHeaders := HttpClient.Request.CustomHeaders.Text;
          Mssg := aOperation.CorrelatedMessage;
          CorrelationId := aCorrelationId;
          Stubbed := True;
          StubAction := aOperation.StubAction;
          Exception := e.Message;
          ReplyBody := Exception;
          if OutboundTimeStamp = 0 then
            OutboundTimeStamp := xNow;
          Nr := displayedLogs.Number;
        end;
        Raise;
      end;
    end;
  finally
    result := xLog.ReplyBody;
    xLog.InitDisplayedColumns(aOperation, DisplayedLogColumns);
    DisplayLog ('', xLog);
  end;
end;

procedure TWsdlProject .SendAsynchReply (aLog : TLog );
var
  xOperation: TWsdlOperation;
begin
  try
    xOperation := aLog.Operation;
    aLog.Operation := nil;
    try
      try
        xOperation.AcquireLock;
        try
          aLog.Operation := TWsdlOperation.Create(xOperation);
        finally
          xOperation.ReleaseLock;
        end;
        aLog.Operation.Data := aLog;
        aLog.Operation.RequestStringToBindables(aLog.RequestBody);
        if aLog.Operation.rpyBind is TIpmItem then
          (aLog.Operation.rpyBind as TIpmItem).LoadValues (aLog.Mssg.rpyBind as TIpmItem)
        else
        begin
          (aLog.Operation.rpyBind as TXml).ResetValues;
          (aLog.Operation.rpyBind as TXml).LoadValues (aLog.Mssg.rpyBind as TXml, True, True);
          (aLog.Operation.fltBind as TXml).ResetValues;
          (aLog.Operation.fltBind as TXml).LoadValues (aLog.Mssg.fltBind as TXml, True, True);
        end;
        try
          aLog.Operation.rpyWsaOnRequest;
          aLog.Operation.InitDelayTime;
          aLog.Operation.ExecuteBefore;
          aLog.Operation.ExecuteRpyStampers;
        except
          on e: exception do
            if e.Message <> 'Exit' then
              raise;
        end;
        if aLog.Operation.rpyBind is TIpmItem then
          aLog.ReplyBody := aLog.Operation.rpyIpm.ValuesToBuffer (nil)
        else
          aLog.ReplyBody := aLog.Operation.StreamReply (_progName, True);
        CreateLogReplyPostProcess(aLog, aLog.Operation);
        if (aLog.Operation.DelayTimeMs > 0) then
          Sleep (aLog.Operation.DelayTimeMs);
        AcquireLock; // ?? to avolid getting socket # 10053  ??
        try
          aLog.Operation.StubHttpAddress := aLog.Operation.rpyWsaXml.Items.XmlValueByTag ['To'];
          aLog.Operation.SoapAction := aLog.Operation.rpyWsaXml.Items.XmlValueByTag ['Action'];
          SendOperationMessage(aLog.Operation, aLog.ReplyBody);
        finally
          ReleaseLock;
        end;
      except
        on e: Exception do
        begin
          aLog.Exception := e.Message;
          LogServerMessage (e.Message, True, e);
        end;
      end;
    finally
      FreeAndNil(aLog.Operation);
      aLog.Operation := xOperation;
    end;
  finally
    AcquireLogLock;
    try
      toUpdateDisplayLogs.SaveLog('', aLog);
      aLog.Disclaim;
    finally
      ReleaseLogLock;
    end;
  end;
end;

procedure TWsdlProject .CheckExpectedValues (aLog : TLog ;
  aOperation : TWsdlOperation ; aDoCheck : Boolean );
begin
  if Assigned (aOperation)
  and aDoCheck
  and Assigned (aLog.Mssg)
  and (aOperation.ExpectationBindables.Count > 0) then
  begin
    aLog.HasUnexpectedValue := aLog.Mssg.CheckValues(aOperation);
    aLog.ExpectedValuesChecked := True;
  end;
end;

function TWsdlProject .SendOperationTacoMessage (aOperation : TWsdlOperation ;
  aMessage : String ; var aRequestHeader : String ; var aReplyHeader : String
  ): String ;
var
  Taco: TTacoInterface;
begin
  Result := '';
  aRequestHeader := '';
  aReplyHeader:= '';
  if not Assigned (aOperation)
    then raise Exception.Create('SendOperationTacoMessage: null arguments');
  result := fTacoInterface.RequestReply(aMessage, 0, aOperation.TacoConfigXml);
end;

procedure TWsdlProject.SetAbortPressed(const Value: Boolean);
begin
  fAbortPressed := Value;
end;

function TWsdlProject .SendOperationSmtpMessage (aOperation : TWsdlOperation ;
  aMessage : String ; var aRequestHeader : String ; var aReplyHeader : String
  ): String ;
var
  Smtp: TIdSMTP;
  mailMessage: TIdMessage;
begin
  Result := '';
  if not Assigned (aOperation)
    then raise Exception.Create('SendOperationSmtpMessage: null arguments');

  mailMessage := smtpCreateMessageFromXml (aOperation.reqBind.Children.Bindables[0] as TXml);
  try
    Smtp := TIdSMTP.Create (nil);
    try
      Smtp.AuthType := satNone;
      Smtp.Host := aOperation.smtpHost;
      Smtp.Port := aOperation.smtpPort;
      Smtp.Connect;
      try
        Smtp.Send(mailMessage);
      finally
        if Smtp.Connected then
          Smtp.Disconnect;
      end;
    finally
      FreeAndNil (Smtp);
    end;
  finally
    mailMessage.Free;
  end;
end;

function TWsdlProject .SendOperationStompMessage (aOperation : TWsdlOperation ;
  aMessage : String ; var aRequestHeader : String ; var aReplyHeader : String
  ): String ;
var
  Stomp: TStompInterface;
  fXml: TXml;
begin
  Result := '';
  if not Assigned (aOperation)
    then raise Exception.Create('SendOperationStompMessage: null arguments');
  Stomp := TStompInterface.Create (nil, HaveStompFrame);
  Stomp.Host := resolveAliasses(aOperation.StubStompPutHost, projectProperties);
  Stomp.Port := StrToIntDef(resolveAliasses(aOperation.StubStompPutPort, projectProperties), 61613);
  Stomp.UseCredentials := aOperation.StubStompPutUseCredentials;
  Stomp.UserName := aOperation.StubStompPutUserName;
  Stomp.Password := aOperation.StubStompPutPassword;
  Stomp.ClientId := resolveAliasses(aOperation.StubStompPutClientId, projectProperties);
  try
    Stomp.Connect;
    try
      fXml := TXml.Create;
      fXml.CopyDownLine (aOperation.StubStompHeaderXml, True);
      try
        try
          if aOperation.isSoapService then
            fXml.Items.XmlCheckedValueByTag ['SOAPAction'] := '"' + aOperation.SoapAction + '"';
        except
        end;
        if aOperation.IsOneWay
        or (fXml.Items.XmlValueByTag ['reply-to'] = '') then
          try
            Stomp.Put ( aMessage
                      + aOperation.StubStompRequestBodyPostFix // WORKAROUND, see XSD
                      , fXml
                      , aOperation.StubCustomHeaderXml
                      , aRequestHeader
                      )
          except
            raise;
          end
        else
          Result := Stomp.RequestReply ( aMessage
                                       + aOperation.StubStompRequestBodyPostFix // WORKAROUND, see XSD
                                       , aOperation.StubStompTimeout
                                       , fXml
                                       , aOperation.StubCustomHeaderXml
                                       , aRequestHeader
                                       , aReplyHeader
                                       )
                                       + aOperation.StubStompReplyBodyPostFix // WORKAROUND, see XSD
                                       ;
      finally
        fXml.Free;
      end;
    finally
      Stomp.Disconnect;
    end;
  finally
    FreeAndNil (Stomp);
  end;
end;

function TWsdlProject .RedirectCommandStomp (aCommand : String ;
  aHost : String ; aPort : Integer ; aDestination : String ;
  aReplyTo : String ; aTimeOut : Integer ): String ;
var
  stomp: TStompInterface;
  xXml: TXml;
  xReqHeader, xRpyHeader: String;
begin
  Result := '';
  xReqHeader := '';
  xRpyHeader := '';
  stomp := TStompInterface.Create (nil, HaveStompFrame);
  try
    Stomp.Host := aHost;
    Stomp.Port := aPort;
    Stomp.ClientId := '';
    Stomp.Connect;
    try
      xXml := TXml.Create;
      try
        xXml.AddXml (TXml.CreateAsString ('destination', aDestination));
        xXml.AddXml (TXml.CreateAsString ('reply-to', aReplyTo));
        Result := Stomp.RequestReply ( aCommand
                                     , aTimeout
                                     , xXml
                                     , nil
                                     , xReqHeader
                                     , xRpyHeader
                                     );
      finally
        xXml.Free;
      end;
    finally
      Stomp.Disconnect;
    end;
  finally
    FreeAndNil (Stomp);
  end;
end;


function TWsdlProject .RedirectCommandMQ (aCommand : String ;
  aPutManager : String ; aPutQueue : String ; aReplyToManager : String ;
  aReplyToQueue : String ; aGetManager : String ; aGetQueue : String ;
  aTimeOut : Integer ): String ;
var
  mq: TMqInterface;
begin
  Result := '';
  mq := TMqInterface.Create;
  try
    mq.Use := mqUse;
    mq.Qmanager := aPutManager;
    mq.PutQueue := aPutQueue;
    // aReplyToManager
    mq.ReplyToQueue := aReplyToQueue;
    // aGetManager
    mq.GetQueue := aGetQueue;
    mq.TimeOut := IntToStr (aTimeOut);
    mq.Expiry := '-1';
    try
      Result := mq.RequestReply (aCommand, nil);
    finally
    end;
  finally
    FreeAndNil (mq);
  end;
end;

function TWsdlProject .RedirectCommandURC (aCommand : String ): String ;
begin
  result := SendOperationMessage(unknownOperation, aCommand);
end;

function doRefuseHttpConnections(aObject: TObject; aLater, aTime: Extended): Extended;
var
  xProject: TWsdlProject;
begin
  result := 1;
  if aObject is TWsdlOperation then
    xProject := (aObject as TWsdlOperation).Owner as TWsdlProject
  else
    xProject := aObject as TWsdlProject;
  TProcedureThread.Create(False, False, xProject, xProject.RefuseHttpConnectionsThreaded, aLater, aTime);
end;

procedure TWsdlProject.RefuseHttpConnectionsThreaded(aLater, aTime: Extended);
begin
  if (aLater > 0) then
    Sleep (Trunc(aLater));
  AcquireLock;
  try
    HTTPServer.Active := False;
  finally
    ReleaseLock;
  end;
  if (aTime > 0) then
    Sleep (Trunc(aTime));
  AcquireLock;
  try
    HTTPServer.Active := True;
  finally
    ReleaseLock;
  end;
end;

function TWsdlProject .RedirectCommandString (aCommand : String ; aAddress ,
  aSoapAction : String ): String ;
var
  HttpClient: TIdHTTP;
  HttpRequest: TMemoryStream;
begin
  HttpClient := TIdHTTP.Create;
  try
    HttpRequest := TMemoryStream.Create;
    try
      try
        HttpClient.Request.CustomHeaders.Values ['SOAPAction'] := aSoapAction;
      except
      end;
      HttpClient.Request.ContentType := 'text/xml';
      HttpClient.Request.CharSet := '';
      WriteStringToStream(aCommand, HttpRequest);
      try
        if doViaProxyServer then
        begin
          HttpClient.ProxyParams.ProxyServer := ViaProxyServer;
          HttpClient.ProxyParams.ProxyPort := ViaProxyPort;
        end
        else
        begin
          HttpClient.ProxyParams.ProxyServer := '';
          HttpClient.ProxyParams.ProxyPort := 0;
        end;
        try
          Result := HttpClient.Post(aAddress, HttpRequest);
        finally
        end;
        if HttpClient.ResponseCode = 500 then
          raise Exception.Create(Result);
      finally
{}{
        if HttpClient.Connected then //in case server s-alive
          HttpClient.Disconnect;
{}
      end;
    finally
      FreeAndNil (HttpRequest);
    end;
  finally
    FreeAndNil (HttpClient);
  end;
end;

function TWsdlProject .RedirectCommandHTTP (aCommand , aStubAddress ,
  aDocument , aSoapAction : String ): String ;
var
  HttpClient: TIdHTTP;
  HttpRequest: TMemoryStream;
  URL: String;
  destUri, docUri: TidURI;
begin
  HttpClient := TIdHTTP.Create;
  try
    HttpRequest := TMemoryStream.Create;
    try
      destUri := TIdURI.Create(aStubAddress);
      docUri := TidUri.Create('http://localhost:6060' + aDocument);
      try
        if (destUri.Path = '')
        or (destUri.Path = '/') then
          destUri.Path  := docUri.Path;
        if destUri.Document = '' then
          destUri.Document  := docUri.Document;
        URL := destUri.URI;
      finally
        FreeAndNil(destUri);
        FreeAndNil(docUri);
      end;
      try
        HttpClient.Request.CustomHeaders.Values ['SOAPAction'] := aSoapAction;
      except
      end;
      HttpClient.Request.ContentType := 'text/xml';
      HttpClient.Request.CharSet := '';
      WriteStringToStream(aCommand, HttpRequest);
      try
        if doViaProxyServer then
        begin
          HttpClient.ProxyParams.ProxyServer := ViaProxyServer;
          HttpClient.ProxyParams.ProxyPort := ViaProxyPort;
        end
        else
        begin
          HttpClient.ProxyParams.ProxyServer := '';
          HttpClient.ProxyParams.ProxyPort := 0;
        end;
        try
          Result := HttpClient.Post(URL, HttpRequest);
        finally
        end;
        if HttpClient.ResponseCode = 500 then
          raise Exception.Create(Result);
      finally
        if HttpClient.Connected then {in case server s-alive}
          HttpClient.Disconnect;
      end;
    finally
      FreeAndNil (HttpRequest);
    end;
  finally
    FreeAndNil (HttpClient);
  end;
end;

function TWsdlProject .CreateLogReplyPostProcess (aLogItem : TLog ;
  aOperation : TWsdlOperation ): String;
var
  xMessage: String;
  xOnRequestViolatingSchema: TOnRequestViolating;
begin
  result := aLogItem.ReplyBody;
  if Assigned (aOperation) then
    aLogItem.StubAction := aOperation.StubAction;
  if Assigned (aOperation) then
  begin
    aLogItem.OperationName:=aOperation.reqTagName;
    CheckExpectedValues(aLogItem, aOperation, doCheckExpectedValues);
    if aOperation.StubAction = saRequest then
    begin
      aLogItem.ReplyBody := _progName + ' - Operation itself is a requestor ('+ aOperation.Name +')';
      raise Exception.Create(aLogItem.ReplyBody);
    end;
    if aOperation.StubAction = saForward then
    begin
      aLogItem.ReplyBody := _progName + ' - Forwarding not supported';
      raise Exception.Create(aLogItem.ReplyBody);
    end;
    if aOperation.StubAction = saRedirect then
    begin
      if Trim (aOperation.BeforeScriptLines.Text) <> '' then // MIM before
      begin
        aLogItem.RequestBodyMiM := aLogItem.RequestBody;
        try
          aOperation.ExecuteBefore;
        except
          on e: exception do
            if e.Message <> 'Exit' then
              raise;
        end;
        aLogItem.RequestBody := aOperation.StreamRequest (_progName, True, True, True);
      end;
      if aLogItem.TransportType = ttHttp then
        aLogItem.ReplyBody := RedirectCommandHTTP ( aLogItem.RequestBody
                                                  , aOperation.StubHttpAddress
                                                  , aLogItem.httpDocument
                                                  , aLogItem.httpSoapAction
                                                  )
      else
        aLogItem.ReplyBody := SendOperationMessage (aOperation, aLogItem.RequestBody);
      aOperation.RpyBindablesFromString (aLogItem.ReplyBody);
      if Trim (aOperation.AfterScriptLines.Text) <> '' then // MIM after action
      begin
        aLogItem.ReplyBodyMiM := aLogItem.ReplyBody;
        try
          aOperation.ExecuteAfter;
        except
          on e: exception do
            if e.Message <> 'Exit' then
              raise;
        end;
        if aOperation.rpyBind is TXml then
          aLogItem.ReplyBody := aOperation.StreamReply (_progName, True);
        if aOperation.rpyBind is TIpmItem then
          aLogItem.ReplyBody := (aOperation.rpyBind as TIpmItem).ValuesToBuffer (nil);
      end;
      aLogItem.Mssg := nil;
      aLogItem.Stubbed := False;
    end;
    if Assigned (aOperation) then
    begin
      with aLogItem do
      begin
        if doValidateRequests
        and (aOperation.WsdlService.DescriptionType <> ipmDTFreeFormat)
        and Assigned (aOperation.reqBind)
        and (aOperation.reqBind is TXml) then
        begin
          xMessage := '';
          if not aOperation.reqBind.IsValueValid (xMessage) then
          begin
            xOnRequestViolatingSchema := aOperation.OnRequestViolatingSchema;
            if xOnRequestViolatingSchema = rvsDefault then
              xOnRequestViolatingSchema := OnRequestViolatingSchema;
            RequestValidateResult := xMessage;
            if (xOnRequestViolatingSchema = rvsRaiseErrorMessage) then
              raise SysUtils.Exception.Create('Schema validation error on request:' + LineEnding + xMessage);
            if (xOnRequestViolatingSchema = rvsAddRemark) then
              aLogItem.AddRemark ('Schema validation error on request:' + LineEnding + xMessage);
          end;
          RequestValidated := True;
        end;
        if doValidateReplies
        and (aOperation.WsdlService.DescriptionType <> ipmDTFreeFormat)
        and Assigned (aOperation.rpyBind)
        and (aOperation.rpyBind is TXml)
        and (not aOperation.ReturnSoapFault) then
        begin
          xMessage := '';
          if not aOperation.rpyBind.IsValueValid (xMessage) then
            ReplyValidateResult := xMessage;
          ReplyValidated := True;
        end;
        BeforeScript := aOperation.BeforeScriptLines.Text;
        Stubbed := True;
      end;
    end;
  end;
  result := aLogItem.ReplyBody;
end;

function TWsdlProject.SendMessageLater(aOperation: TWsdlOperation;
  aRequest: TWsdlMessage; aCorrelationId: String; aLater: Integer): String;
begin
  result := '';
  TSendSoapRequestThread.Create (Self, aOperation, aRequest, aCorrelationId, aLater);
end;

function TWsdlProject.FindXmlOperationOnReply (aXml: TXml): TWsdlOperation;
var
  x, o: Integer;
  xXml, rpyXml: TXml;
  eBind: TCustomBindable;
  xOperation: TWsdlOperation;
  xRecog: TRecognition;
  xName: String;
begin
  result := nil;
  if aXml.isSoapEnvelope then
  begin
    for x := 0 to aXml.Items.Count - 1 do
    begin
      xXml := aXml.Items.XmlItems [x];
      if (xXml.isSoapBody)
      and (xXml.Items.Count > 0) then
      begin
        xName := xXml.Items.XmlItems [0].Name;
        for o := 0 to allOperations.Count - 1 do
        begin
          xOperation := allOperations.Operations [o];
          rpyXml := xOperation.rpyBind as TXml;
          if xOperation.isSoapService
          and (rpyXml.Items.XmlItems [xOperation.OutputHeaders.Count].Name = xName) then
          begin
            result := xOperation;
            exit;
          end;
        end;
        raise Exception.Create (S_NO_OPERATION_FOUND);
      end;
    end;
    raise Exception.Create('no SOAP:Body found');
  end
  else
  begin // no soap envelope, try non-soap operations
    for o := 0 to allOperations.Count - 1 do
    begin
      xOperation := allOperations.Operations [o];
      if not xOperation.isSoapService then
      begin
        case xOperation.RecognitionType of
          rtSoap, rtDocument, rtHeader: Raise Exception.Create ('FindOperationOnReply ' + xOperation.Name + ' RecognistionType not yet supported');
          rtXml:
          begin
            if (not Assigned (xOperation.rpyRecognition))
            or (xOperation.rpyRecognition.Count = 0)
              then raise Exception.Create (xOperation.reqTagName + ': Missing recognition specification');
            xRecog := xOperation.rpyRecognition.Objects[0] as TRecognition;
            eBind := aXml.FindUQBind (xRecog.Name);
            if Assigned (eBind)
            and (   (eBind.Value = xRecog.Value)
                 or (StringMatchesRegExpr (eBind.Value, xRecog.Value) <> '')
                ) then
            begin
              result := xOperation;
              Exit;
            end;
          end;
        end;
      end;
    end;
    raise Exception.Create (S_NO_OPERATION_FOUND);
  end;
end;

function TWsdlProject.FindCcbOperationOnReply(aCobolString: String): TWsdlOperation;
var
  o, r: Integer;
  xOperation: TWsdlOperation;
  xRecog: TRecognition;
  xMatch: Boolean;
begin
  result := nil;
  for o := 0 to allOperations.Count - 1 do
  begin
    if allOperations.Operations [o].WsdlService.DescriptionType = ipmDTCobol then
    begin
      xOperation := allOperations.Operations [o];
      if xOperation.RecognitionType = rtSubString then
      begin
        xMatch := True;
        if Assigned (xOperation.rpyRecognition) then
        begin
          for r := 0 to xOperation.rpyRecognition.Count - 1 do
          begin
            xRecog := xOperation.rpyRecognition.Objects [r] as TRecognition;
            xMatch := (    xMatch
                       and (Trim (Copy ( aCobolString
                                       , xRecog.Start
                                       , xRecog.Length
                                       )
                                 ) = xRecog.Value
                           )
                      );
          end;
        end;
        if xMatch then
        begin
          result := xOperation;
          exit;
        end;
      end
      else
        Raise Exception.Create ('FindOperationOnReply ' + xOperation.Name + ' RecognistionType not yet supported');
    end;
  end;
end;

function TWsdlProject.FindOperationOnDocument(
  aDocument: String): TWsdlOperation;
var
  o: Integer;
begin
  result := nil;
  for o := 0 to allOperations.Count - 1 do
  begin
    with allOperations.Operations[o] do
    begin
      if Assigned (reqRecognition)
      and (reqRecognition.Count > 0) then
      begin
        with (reqRecognition.Objects[0] as TRecognition) do
        begin
          if (Value = aDocument)
          or ('/' + Value = aDocument) then
          begin
            result := allOperations.Operations[o];
            exit;
          end;
        end;
      end;
    end;
  end;
end;

function TWsdlProject.FindOperationOnReply(aString: String): TWsdlOperation;
var
  xXml: TXml;
begin
  result := nil;
  xXml := TXml.Create;
  try
    try
      xXml.LoadFromString(aString, nil);
    except
      xXml.Name := '';
    end;
    if xXml.Name <> '' then
      result := FindXmlOperationOnReply (xXml);
    if not Assigned (result) then
      result := FindCcbOperationOnReply (aString);
    if not Assigned (result) then
      raise Exception.Create (S_NO_OPERATION_FOUND);
    if result.WsdlService.DescriptionType in [ipmDTFreeFormat] then
      result.FreeFormatReq := aString
    else
    begin
      if result.reqBind is TIpmItem then
        (result.reqBind as TIpmItem).BufferToValues (FoundErrorInBuffer, aString)
      else
        result.SoapXmlReplyToBindables (xXml, False);
    end;
  finally
    FreeAndNil (xXml);
  end;
end;

function TWsdlProject.FindCcbOperationOnRequest (aLog: TLog; aCobolString: String): TWsdlOperation;
  function _Matches (aExpr, aString: String): Boolean;
  var
    rx: TRegExpr;
  begin
    result := False;
    Rx := TRegExpr.Create;
    try
      rx.Expression := '^(' + aExpr + ')$';  // bol and eol: must match entire string
      result := rx.Exec(aString);
    finally
      rx.Free;
    end;
  end;
var
  o, r: Integer;
  xOperation: TWsdlOperation;
  xRecog: TRecognition;
  xMatch: Boolean;
begin
  result := nil;
  for o := 0 to allOperations.Count - 1 do
  begin
    if allOperations.Operations [o].WsdlService.DescriptionType
      in [ipmDTFreeFormat, ipmDTCobol, ipmDTSwiftMT] then
    begin
      xOperation := allOperations.Operations [o];
      if xOperation.RecognitionType = rtSubString then
      begin
        xMatch := True;
        if Assigned (xOperation.reqRecognition) then
        begin
          for r := 0 to xOperation.reqRecognition.Count - 1 do
          begin
            xRecog := xOperation.reqRecognition.Objects [r] as TRecognition;
            xMatch := (    xMatch
                       and _Matches ( xRecog.Value
                                    , Trim (Copy ( aCobolString
                                                 , xRecog.Start
                                                 , xRecog.Length
                                                 )
                                           )
                                    )
                      );
          end;
        end;
        if xMatch then
        begin
          result := xOperation;
          exit;
        end;
      end
      else
        Raise Exception.Create ( 'Operation: '
                               + xOperation.Name
                               + '; Request RecognitionType not known or not supported'
                               );
    end
    else
    begin
      if allOperations.Operations [o].WsdlService.DescriptionType = ipmDTBmtp then
      begin
        if (allOperations.Operations [o].WsdlService.Name = aLog.ServiceName)
        and (allOperations.Operations [o].Name = aLog.OperationName) then
        begin
          result := allOperations.Operations [o];
          exit;
        end;
      end;
    end;
  end;
end;

function TWsdlProject.FindXmlOperationOnRequest (aDocument: String; aXml: TXml): TWsdlOperation;
var
  x, o, f: Integer;
  xXml: TXml;
  eBind: TCustomBindable;
  xOperation: TWsdlOperation;
  xRecog: TRecognition;
begin
  result := nil;
  if aXml.isSoapEnvelope then
  begin
    for x := 0 to aXml.Items.Count - 1 do
    begin
      xXml := aXml.Items.XmlItems [x];
      if (xXml.isSoapBody)
      and (xXml.Items.Count > 0) then
      begin
        xXml := xXml.Items.XmlItems [0];
        if allOperations.Find(xXml.Name + ';' + xXml.NameSpace, f) then
          result := allOperations.Operations [f];
        exit;
      end;
    end;
    raise Exception.Create('no SOAP:Body found');
  end
  else
  begin // no soap envelope, try non-soap operations
    for o := 0 to allOperations.Count - 1 do
    begin
      xOperation := allOperations.Operations [o];
      if not xOperation.isSoapService then
      begin
        case xOperation.RecognitionType of
          rtSoap, rtHeader: Raise Exception.Create ('FindOperationOnRequest ' + xOperation.Name + ' RecognistionType not yet supported');
          rtDocument:
          begin
            if (not Assigned (xOperation.reqRecognition))
            or (xOperation.reqRecognition.Count = 0)
              then raise Exception.Create (xOperation.reqTagName + ': Missing recognition specification');
            xRecog := xOperation.reqRecognition.Objects[0] as TRecognition;
            if (StringMatchesRegExpr (aDocument, '/'+ xRecog.Value) <> '')
            or (StringMatchesRegExpr (aDocument, xRecog.Value) <> '') then
            begin
              result := xOperation;
              Exit;
            end;
          end;
          rtXml:
          begin
            if (not Assigned (xOperation.reqRecognition))
            or (xOperation.reqRecognition.Count = 0)
              then raise Exception.Create (xOperation.reqTagName + ': Missing recognition specification');
            xRecog := xOperation.reqRecognition.Objects[0] as TRecognition;
            eBind := aXml.FindUQBind (xRecog.Name);
            if Assigned (eBind)
            and (   (eBind.Value = xRecog.Value)
                 or (StringMatchesRegExpr (eBind.Value, xRecog.Value) <> '')
                ) then
            begin
              result := xOperation;
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TWsdlProject.operationRecognitionUpdate(aOperation: TWsdlOperation; aList: TStringList;
  aXml: TXml);
var
  x, y: Integer;
  xRecog: TRecognition;
begin
// this code works for all recognitiontypes but not all recognitiontypes are supported with every operationtype
// make sure the xsd for recognition per operation only has appropriate elements
  aList.Clear;
  if not Assigned (aXml) then Exit;
  if not aXml.Checked then Exit;
  for x := 0 to aXml.Items.Count - 1 do with aXml.Items.XmlItems[x] do
  begin
    if Checked
    and (Name = 'SubStrings') then
    begin
      for y := 0 to Items.Count - 1 do with Items.XmlItems[y] do
      begin
        if Checked
        and (Name = 'SubString') then
        begin
          aOperation.RecognitionType := rtSubString;
          xRecog := TRecognition.Create;
          xRecog.RecognitionType := aOperation.RecognitionType;
          aList.AddObject('', xRecog);
          xRecog.Start := Items.XmlCheckedIntegerByTagDef['Start', 1];
          xRecog.Length := Items.XmlCheckedIntegerByTagDef['Length', 1];
          xRecog.Value := Items.XmlCheckedValueByTag['Value'];
        end;
      end;
    end;
    if Checked
    and (Name = 'HttpDocument') then
    begin
      aOperation.RecognitionType := rtDocument;
      xRecog := TRecognition.Create;
      xRecog.RecognitionType := aOperation.RecognitionType;
      xRecog.Value := Trim (Value);
      aList.AddObject ('', xRecog);
    end;
    if Checked
    and (Name = 'HTTPHeader') then
    begin
      aOperation.RecognitionType := rtHeader;
      xRecog := TRecognition.Create;
      xRecog.RecognitionType := aOperation.RecognitionType;
      xRecog.Name := Items.XmlCheckedValueByTag['Name'];
      xRecog.Value := Trim (Items.XmlCheckedValueByTag['Value']);
      aList.AddObject ('', xRecog);
    end;
    if Checked
    and (Name = 'XmlElement') then
    begin
      aOperation.RecognitionType := rtXml;
      xRecog := TRecognition.Create;
      xRecog.RecognitionType := aOperation.RecognitionType;
      xRecog.Name := Items.XmlCheckedValueByTag['Path'];
      xRecog.Value := Trim (Items.XmlCheckedValueByTag['Value']);
      aList.AddObject ('', xRecog);
    end;
  end;
end;

procedure TWsdlProject.freeFormatOperationsUpdate(aXml: TXml);
var
  sList: TStringList;
  xXml, oXml: TXml;
  xXsd: TXsd;
  f, x, o: Integer;
  xOperation: TWsdlOperation;
const
  _xsdName = 'OperationDefs.FreeFormatOperations';
begin
  if aXml.Name <> 'FreeFormatOperations' then
    raise Exception.Create('??TWsdlProject.freeFormatOperationsUpdate(aXml: TXml): ' + aXml.Name);
  xXsd := OperationDefsXsd.FindXsd (_xsdName);
  if not Assigned (xXsd) then
    raise  Exception.Create('Xsd not found: ' + _xsdName);
  xXml := TXml.Create(-1000, xXsd);
  try
    xXml.LoadValues(aXml, False);
    aXml.CopyDownLine(xXml, True);
  finally
    xXml.Free;
  end;

  sList := TStringList.Create;
  sList.Sorted := True;
  try
    for x := 0 to aXml.Items.Count - 1 do
      if aXml.Items.XmlItems[x].Checked
      and (aXml.Items.XmlItems[x].Name = 'Operation')
      and (aXml.Items.XmlItems[x].Items.Count > 0) then
        sList.AddObject(aXml.Items.XmlItems[x].Items.XmlCheckedValueByTagDef['Name','freeFormatOperation'], aXml.Items.XmlItems[x]);
    with FreeFormatWsdl.Services.Services[0] do
    begin
      for o := Operations.Count - 1 downto 0 do
      begin
        if not (sList.Find(Operations.Operations[o].Name, f)) then
        begin // remove
          Operations.Operations[o].Free;
          Operations.Delete(o);
        end;
      end;
      for x := 0 to sList.Count - 1 do
      begin
        oXml := sList.Objects[x] as TXml;
        if Operations.Find(sList.Strings[x], f) then
        begin // adjust
          with Operations.Operations[f] do
          begin
            operationRecognitionUpdate (Operations.Operations[f], reqRecognition, oXml.Items.XmlItemByTag['reqRecognition']);
            operationRecognitionUpdate (Operations.Operations[f], rpyRecognition, oXml.Items.XmlItemByTag['rpyRecognition']);
          end;
        end
        else
        begin
          xOperation := TWsdlOperation.Create (FreeFormatWsdl);
          xOperation.Name := sList.Strings[x];
          FreeFormatService.Operations.AddObject(xOperation.Name, xOperation);
          xOperation.Wsdl := FreeFormatWsdl;
          xOperation.WsdlService := FreeFormatService;
          xOperation.reqTagName := xOperation.Name;
          xOperation.Alias := xOperation.reqTagName;
          xOperation.rpyTagName := xOperation.Name;
          xOperation.reqRecognition := TStringList.Create;
          xOperation.rpyRecognition := TStringList.Create;
          xOperation.RecognitionType := rtSubString;
          operationRecognitionUpdate (xOperation, xOperation.reqRecognition, oXml.Items.XmlItemByTag['reqRecognition']);
          operationRecognitionUpdate (xOperation, xOperation.rpyRecognition, oXml.Items.XmlItemByTag['rpyRecognition']);
        end;
      end;
    end;
  finally
    sList.Free;
  end;
end;

procedure TWsdlProject.xsdOperationsUpdate(aXml: TXml; aMainFileName: String);
  procedure _getDescriptionFiles (aXml: TXml; aFileNames: TStringList);
  var
    x, f: Integer;
  begin
    if not aXml.Checked then Exit;
    if aXml.Name = 'DescriptionFile' then
    begin
      if not aFileNames.Find(aXml.Value, f) then
        aFilenames.AddObject (aXml.Value, Pointer (ipmDTCobol));
    end
    else
    begin
      for x := 0 to aXml.Items.Count - 1 do
        _getDescriptionFiles(aXml.Items.XmlItems[x], aFileNames);
    end;
  end;
  function _refXsd (aXsdDescr: TXsdDescr; aXsdName: String): TXsd;
  var
    x: Integer;
    xName: String;
  begin
    result := nil;
    with aXsdDescr.TypeDef.ElementDefs do
    begin
      for x := 0 to Count - 1 do
      begin
         xName := Xsds[x].ElementName;
         if (xName = aXsdName) then
         begin
           result := Xsds[x];
           exit;
         end;
      end;
    end;
  end;
  function _LoadXsdMsg (aLabel: String; sXml: TXml; aXsd: TXsd; var aDescrFileName: String): TXml;
  var
    xXsd: TXsd;
    xXsdDescr: TXsdDescr;
  begin
    result := nil;
    try
      if not Assigned (sXml) then
        exit;
      aDescrFileName := ExpandFileNameUTF8(ExpandRelativeFileName
                            (aMainFileName, sXml.Items.XmlCheckedValueByTag ['DescriptionFile'])
                          );
      if xsdElementsWhenRepeatable > 0 then
        xXsdDescr := TXsdDescr.Create(xsdElementsWhenRepeatable)
      else
        xXsdDescr := TXsdDescr.Create(XsdWsdl.xsdDefaultElementsWhenRepeatable);
      XsdWsdl.sdfXsdDescrs.AddObject('', xXsdDescr);
      try
        xXsdDescr.LoadXsdFromFile(aDescrFileName, nil);
      except
        on E: Exception do
          raise Exception.Create('Error opening ' + aDescrFileName + ': ' + e.Message);
      end;
      xXsd := _refXsd ( xXsdDescr, sXml.Items.XmlValueByTag ['ElementName']);
      if Assigned (xXsd) then
      begin
        aXsd.sType.ElementDefs.AddObject('', xXsd);
        bindRefId := 0;
        result := TXml.Create (0, aXsd);
      end;
    finally
      if not Assigned (result) then
        result := TXml.Create;
      result.Checked := True;
    end;
  end;
var
  sList: TStringList;
  oXml, xXml: TXml;
  xXsd: TXsd;
  f, x, o: Integer;
  xOperation: TWsdlOperation;
const
  _xsdName = 'OperationDefs.XsdOperations';
begin
  if aXml.Name <> 'XsdOperations' then
    raise Exception.Create('??TWsdlProject.XsdOperationsUpdate(aXml: TXml): ' + aXml.Name);
  xXsd := OperationDefsXsd.FindXsd (_xsdName);
  if not Assigned (xXsd) then
    raise  Exception.Create('Xsd not found: ' + _xsdName);
  xXml := TXml.Create(-1000, xXsd);
  try
    xXml.LoadValues(aXml, False);
    aXml.CopyDownLine(xXml, True); // assigns Xsd to downline
  finally
    xXml.Free;
  end;

  sList := TStringList.Create;
  sList.Sorted := True;
  if xmlUtil.CheckAndPromptFileNames(aMainFileName, aXml, True) then
    StubChanged := True;
  try
    for x := 0 to aXml.Items.Count - 1 do
      if (aXml.Items.XmlItems[x].Checked)
      and (aXml.Items.XmlItems[x].Name = 'Operation')
      and (aXml.Items.XmlItems[x].Items.Count > 0) then
        sList.AddObject(aXml.Items.XmlItems[x].Items.XmlCheckedValueByTag['Name'], aXml.Items.XmlItems[x]);
    with XsdWsdl.Services.Services[0] do
    begin
      for o := Operations.Count - 1 downto 0 do
      begin
        if not (sList.Find(Operations.Operations[o].Name, f)) then
        begin // remove
          Operations.Operations[o].Free;
          Operations.Delete(o);
        end;
      end;
      for x := 0 to sList.Count - 1 do
      begin
        oXml := sList.Objects[x] as TXml;
        if not Operations.Find(sList.Strings[x], f) then
        begin
          xOperation := TWsdlOperation.Create (XsdWsdl);
          xOperation.Name := sList.Strings[x];
          XsdWsdl.Services.Services[0].Operations.AddObject(xOperation.Name, xOperation);
          xOperation.Wsdl := XsdWsdl;
          xOperation.WsdlService := XsdWsdl.Services.Services[0];
          xOperation.reqTagName := xOperation.Name + '_Req';
          xOperation.Alias := xOperation.reqTagName;
          xOperation.rpyTagName := xOperation.Name + '_Rpy';
          xOperation.reqRecognition := TStringList.Create;
          xOperation.rpyRecognition := TStringList.Create;
          xOperation.RecognitionType := rtSubString;
          xOperation.reqXsd.ElementName := xOperation.reqTagName;
          xOperation.rpyXsd.ElementName := xOperation.rpyTagName;
        end
        else
        begin
          xOperation := Operations.Operations[f];
        end;
        with xOperation do
        begin
          if Assigned(reqBind) then
            reqBind.Free;
          if Assigned(rpyBind) then
            rpyBind.Free;
          FreeAndNil (fltBind);
          reqBind := _LoadXsdMsg('Req', oXml.Items.XmlCheckedItemByTag['Req'], reqXsd, reqDescrFilename);
          rpyBind := _LoadXsdMsg('Rpy', oXml.Items.XmlCheckedItemByTag['Rpy'], rpyXsd, rpyDescrFilename);
          fltBind := _LoadXsdMsg('Flt', oXml.Items.XmlCheckedItemByTag['Flt'], FaultXsd, fltDescrFilename);
          operationRecognitionUpdate (xOperation, reqRecognition, oXml.Items.XmlCheckedItemByTag['reqRecognition']);
          operationRecognitionUpdate (xOperation, rpyRecognition, oXml.Items.XmlCheckedItemByTag['rpyRecognition']);
        end;
      end;
    end;
  finally
    FreeAndNil(sList);
  end;
end;

procedure TWsdlProject.cobolOperationsUpdate(aXml: TXml; aMainFileName: String);
  procedure _getDescriptionFiles (aXml: TXml; aFileNames: TStringList);
  var
    x, f: Integer;
  begin
    if not aXml.Checked then Exit;
    if aXml.Name = 'DescriptionFile' then
    begin
      if not aFileNames.Find(aXml.Value, f) then
        aFilenames.AddObject ( uncFilename ( ExpandRelativeFileName ( aMainFileName, aXml.Value)

                                                     )
                             , Pointer (ipmDTCobol)
                             );
    end
    else
    begin
      for x := 0 to aXml.Items.Count - 1 do
        _getDescriptionFiles(aXml.Items.XmlItems[x], aFileNames);
    end;
  end;
{}
  function _LoadCobolMsg (sXml: TXml; aFileNames: TStringList; var aDescrFilename: String): TIpmItem;
  var
    f: Integer;
  begin
    result := nil;
    try
      if not Assigned (sXml) then Exit;
      if not aXml.Checked then Exit;
      if aFileNames.Find ( uncFilename(ExpandRelativeFileName ( aMainFileName
                                                                      , sXml.Items.XmlCheckedValueByTag ['DescriptionFile']
                                                                      )
                                             )
                         , f
                         ) then
      begin
        if CobolWsdl.IpmDescrs.Strings [f] <> aFilenames.Strings [f] then
          raise Exception.Create ('Internal error');
        result := TIpmItem.Create (CobolWsdl.IpmDescrs.IpmDescrs [f].IpmItem); // ipmdescr also destroys ipmitems
        aDescrFilename := aFilenames.Strings [f];
      end;
    finally
      if not Assigned (result) then
        result := TIpmItem.Create;
    end;
  end;
{}
var
  sList: TStringList;
  oXml, xXml: TXml;
  xXsd: TXsd;
  f, x, o: Integer;
  xOperation: TWsdlOperation;
  xFileNames: TStringList;
  xIpmDescr: TIpmDescr;
const
  _xsdName = 'OperationDefs.CobolOperations';
begin
  if aXml.Name <> 'CobolOperations' then
    raise Exception.Create('??TWsdlProject.cobolOperationsUpdate(aXml: TXml): ' + aXml.Name);
  xXsd := OperationDefsXsd.FindXsd (_xsdName);
  if not Assigned (xXsd) then
    raise  Exception.Create('Xsd not found: ' + _xsdName);
  xXml := TXml.Create(-1000, xXsd);
  try
    xXml.LoadValues(aXml, False);
    aXml.CopyDownLine(xXml, True); // assigns Xsd to downline
  finally
    xXml.Free;
  end;

  sList := TStringList.Create;
  sList.Sorted := True;
  xFileNames := TStringList.Create;
  xFileNames.Sorted := True;
  if xmlUtil.CheckAndPromptFileNames(aMainFileName, aXml, True) then
    StubChanged := True;
  _getDescriptionFiles (aXml, xFileNames);
  for x := 0 to xFileNames.Count - 1 do
  begin
    try
      xIpmDescr := TIpmDescr.Create;
      xIpmDescr.LoadFromFile(xFileNames.Strings[x], _OnParseErrorEvent);
      CobolWsdl.IpmDescrs.AddObject(xFileNames.Strings[x], xIpmDescr);
    except
      on e: Exception do
        raise Exception.Create ( 'Error opening Cobol description from '
                               + xFileNames.Strings[x]
                               + CRLF
                               + e.Message
                               );
    end;
  end;
  try
    for x := 0 to aXml.Items.Count - 1 do
      if (aXml.Items.XmlItems[x].Checked)
      and (aXml.Items.XmlItems[x].Name = 'Operation')
      and (aXml.Items.XmlItems[x].Items.Count > 0) then
        sList.AddObject(aXml.Items.XmlItems[x].Items.XmlCheckedValueByTag['Name'], aXml.Items.XmlItems[x]);
    with CobolWsdl.Services.Services[0] do
    begin
      for o := Operations.Count - 1 downto 0 do
      begin
        if not (sList.Find(Operations.Operations[o].Name, f)) then
        begin // remove
          Operations.Operations[o].Free;
          Operations.Delete(o);
        end;
      end;
      for x := 0 to sList.Count - 1 do
      begin
        oXml := sList.Objects[x] as TXml;
        if not Operations.Find(sList.Strings[x], f) then
        begin
          xOperation := TWsdlOperation.Create (CobolWsdl);
          xOperation.Name := sList.Strings[x];
          CobolWsdl.Services.Services[0].Operations.AddObject(xOperation.Name, xOperation);
          xOperation.Wsdl := CobolWsdl;
          xOperation.WsdlService := CobolWsdl.Services.Services[0];
          xOperation.reqTagName := xOperation.Name;
          xOperation.Alias := xOperation.reqTagName;
          xOperation.rpyTagName := xOperation.Name;
          xOperation.reqRecognition := TStringList.Create;
          xOperation.rpyRecognition := TStringList.Create;
          xOperation.RecognitionType := rtSubString;
        end
        else
        begin
          xOperation := Operations.Operations[f];
        end;
        with xOperation do
        begin
          FreeAndNil (fltBind);
          if Assigned (reqBind) then
            reqBind.Free;
          if Assigned (rpyBind) then
            rpyBind.Free;
          CobolEnvironment := ceTandem;
          if oXml.Items.XmlCheckedValueByTag ['CobolEnvironment'] = 'IBM Zos' then
            CobolEnvironment := ceIbmZOs;
          reqBind := _LoadCobolMsg(oXml.Items.XmlCheckedItemByTag['Req'], xFileNames, reqDescrFilename);
          rpyBind := _LoadCobolMsg(oXml.Items.XmlCheckedItemByTag['Rpy'], xFileNames, rpyDescrFilename);
          fltBind := _LoadCobolMsg(oXml.Items.XmlCheckedItemByTag['Flt'], xFileNames, fltDescrFileName);
          operationRecognitionUpdate (xOperation, reqRecognition, oXml.Items.XmlCheckedItemByTag['reqRecognition']);
          operationRecognitionUpdate (xOperation, rpyRecognition, oXml.Items.XmlCheckedItemByTag['rpyRecognition']);
        end;
      end;
    end;
  finally
    FreeAndNil(sList);
    FreeAndNil(xFileNames);
  end;
end;

procedure TWsdlProject.swiftMtOperationsUpdate(aXml: TXml; aMainFileName: String);
  procedure _getDescriptionFiles (aXml: TXml; aFileNames: TStringList);
  var
    x, f: Integer;
  begin
    if not aXml.Checked then Exit;
    if aXml.Name = 'DescriptionFile' then
    begin
      if not aFileNames.Find(aXml.Value, f) then
        aFilenames.AddObject (aXml.Value, Pointer (ipmDTSwiftMt));
    end
    else
    begin
      for x := 0 to aXml.Items.Count - 1 do
        _getDescriptionFiles(aXml.Items.XmlItems[x], aFileNames);
    end;
  end;
{}
  function _LoadSwiftMtMsg (sXml: TXml; aXsd: TXsd; var aDescrFileName, aDescrExpansionFileName: String): TXml;
    function _refXsd (aXsdDescr: TXsdDescr; aXsdName: String): TXsd;
    var
      x: Integer;
    begin
      result := nil;
      with aXsdDescr.TypeDef.ElementDefs do
      begin
        for x := 0 to Count - 1 do
        begin
           if (Xsds[x].ElementName = aXsdName) then
           begin
             result := Xsds[x];
             exit;
           end;
        end;
      end;
    end;
    procedure _loadSwiftMtExpansions (aXsd: TXsd; xpXmls: TXml);
      procedure __loadExpansions (aXsd: TXsd; rootXml, dXml: TXml);
        function ___findType (aTypeName: String): TXml;
        var
          x: Integer;
        begin
          result := nil;
          for x := 0 to rootXml.Items.Count - 1 do
          begin
            if (rootXml.Items.XmlItems[x].Name = 'type')
            and (rootXml.Items.XmlItems[x].Attributes.ValueByTag['shortName'] = aTypeName) then
            begin
              result := rootXml.Items.XmlItems[x];
              exit;
            end;
          end;
        end;
      var
        tpXml, eXml: TXml;
        eXsd: TXsd;
        x: Integer;
      begin
        (aXsd.Obj as TSwiftMtProps).longName := dXml.Attributes.ValueByTag['longName'];
        (aXsd.Obj as TSwiftMtProps).expansionName := dXml.Items.XmlValueByTag['expansion'];
        tpXml := ___findType (dXml.Attributes.ValueByTag['type']);
        if Assigned (tpXml) then
        begin
          for x := 0 to tpXml.Items.Count - 1 do
          begin
            eXml := tpXml.Items.XmlItems[x];
            if eXml.Name = 'element' then
            begin
              eXsd := aXsd.FindXsd(aXsd.ElementName + '.' + eXml.Attributes.ValueByTag['shortName']);
              if Assigned (eXsd) then
                __loadExpansions(eXsd, rootXml, eXml);
            end;
          end;
        end;
      end;
    var
      x, d: Integer;
      xXml: TXml;
    begin
      for x := 0 to xpXmls.Items.Count - 1 do
      begin
        xXml := xpXmls.Items.XmlItems [x];
        if xXml.Name = 'names' then
        begin
          for d := 0 to xXml.Items.Count - 1 do
          begin
            if (xXml.Items.XmlItems[d].Name = 'type')
            and (xXml.Items.XmlItems[d].Attributes.ValueByTag ['shortName'] = 'Document')
            and (xXml.Items.XmlItems[d].Items.Count > 0)
            and (xXml.Items.XmlItems[d].Items.XmlItems[0].Name = 'element')
            and (xXml.Items.XmlItems[d].Items.XmlItems[0].Attributes.ValueByTag ['shortName'] = aXsd.ElementName) then
            begin
              __loadExpansions (aXsd, xXml, xXml.Items.XmlItems[d].Items.XmlItems[0]);
              exit;
            end;
          end;
        end;
      end;
    end;
  var
    x: Integer;
    fXsd, b4Xsd: TXsd;
    xXsdDescr: TXsdDescr;
    xpXmls: TXml;
  begin
    aDescrFileName := '';
    aDescrExpansionFileName := '';
    try
      if not Assigned (sXml) then Exit;
      xpXmls := TXml.CreateAsString('expansions', '');
      try
        xXsdDescr := TXsdDescr.Create(1);
        SwiftMtWsdl.sdfXsdDescrs.AddObject('', xXsdDescr);
        xXsdDescr.AddXsdFromFile('', _swiftMTXsdFileName, nil);
        aXsd.ElementName := 'FinMessage';
        fXsd := _refXsd ( xXsdDescr, 'FinMessage');
        if Assigned (fXsd) then
        begin
          for x := 0 to fXsd.sType.ElementDefs.Count - 1 do
            if not Assigned (aXsd.FindXsd ('FinMessage.' + fXsd.sType.ElementDefs.Xsds[x].ElementName)) then
              aXsd.sType.ElementDefs.AddObject('', fXsd.sType.ElementDefs.Xsds[x]);
        end;
        for x := 0 to sXml.Items.Count - 1 do
        begin
          if sXml.Items.XmlItems[x].Name = 'DescriptionFile' then
          begin
          aDescrFileName := uncFilename(ExpandRelativeFileName(aMainFileName, sXml.Items.XmlItems[x].Value));
            xXsdDescr.AddXsdFromFile('', aDescrFileName, nil);
          end;
          if sXml.Items.XmlItems[x].Name = 'DescriptionExpansionFile' then
            with xpXmls.AddXml(TXml.Create) do
            begin
              aDescrExpansionFileName := uncFilename(ExpandRelativeFileName(aMainFileName, sXml.Items.XmlItems[x].Value));
              LoadFromFile(aDescrExpansionFileName, nil);
            end;
        end;
        b4Xsd := aXsd.FindXsd('FinMessage.Block4');
        if Assigned (b4Xsd) then
        begin
          for x := 0 to xXsdDescr.TypeDef.ElementDefs.Count - 1 do with xXsdDescr.TypeDef.ElementDefs.Xsds[x] do
          begin
            if (ElementName = 'Document')
            and (sType.ElementDefs.Count > 0) then
            begin
              sType.ElementDefs.Xsds[0].Obj := TSwiftMtProps.Create(sType.ElementDefs.Xsds[0]);
              b4Xsd.sType.ElementDefs.AddObject('', sType.ElementDefs.Xsds[0]);
              b4Xsd.sType.IsComplex := True;
              b4Xsd.sType.ContentModel := 'Choice';
              _loadSwiftMtExpansions (sType.ElementDefs.Xsds[0], xpXmls);
            end;
          end;
        end;
      finally
        xpXmls.Free;
      end;
    finally
      bindRefId := 0;
      result := TXml.Create (0, aXsd);
      result.Checked := True;
    end;
  end;
{}
var
  sList: TStringList;
  oXml, xXml: TXml;
  xXsd: TXsd;
  f, x, o: Integer;
  xOperation: TWsdlOperation;
const
  _xsdName = 'OperationDefs.SwiftMtOperations';
begin
  if aXml.Name <> 'SwiftMtOperations' then
    raise Exception.Create('??TWsdlProject.SwiftMtOperationsUpdate(aXml: TXml): ' + aXml.Name);
  xXsd := OperationDefsXsd.FindXsd (_xsdName);
  if not Assigned (xXsd) then
    raise  Exception.Create('Xsd not found: ' + _xsdName);
  xXml := TXml.Create(-1000, xXsd);
  try
    xXml.LoadValues(aXml, False);
    aXml.CopyDownLine(xXml, True); // assigns Xsd to downline
  finally
    xXml.Free;
  end;

  sList := TStringList.Create;
  sList.Sorted := True;
  if xmlUtil.CheckAndPromptFileNames(aMainFileName, aXml, True) then
    StubChanged := True;
  try
    for x := 0 to aXml.Items.Count - 1 do
      if (aXml.Items.XmlItems[x].Checked)
      and (aXml.Items.XmlItems[x].Name = 'Operation')
      and (aXml.Items.XmlItems[x].Items.Count > 0) then
        sList.AddObject(aXml.Items.XmlItems[x].Items.XmlCheckedValueByTag['Name'], aXml.Items.XmlItems[x]);
    with SwiftMtWsdl.Services.Services[0] do
    begin
      for o := Operations.Count - 1 downto 0 do
      begin
        if not (sList.Find(Operations.Operations[o].Name, f)) then
        begin // remove
          Operations.Operations[o].Free;
          Operations.Delete(o);
        end;
      end;
      for x := 0 to sList.Count - 1 do
      begin
        oXml := sList.Objects[x] as TXml;
        if not Operations.Find(sList.Strings[x], f) then
        begin
          xOperation := TWsdlOperation.Create (SwiftMtWsdl);
          xOperation.Name := sList.Strings[x];
          SwiftMtWsdl.Services.Services[0].Operations.AddObject(xOperation.Name, xOperation);
          xOperation.Wsdl := SwiftMtWsdl;
          xOperation.WsdlService := SwiftMtWsdl.Services.Services[0];
          xOperation.reqTagName := xOperation.Name;
          xOperation.Alias := xOperation.reqTagName;
          xOperation.rpyTagName := xOperation.Name;
          xOperation.reqRecognition := TStringList.Create;
          xOperation.rpyRecognition := TStringList.Create;
          xOperation.RecognitionType := rtSubString;
        end
        else
        begin
          xOperation := Operations.Operations[f];
        end;
        with xOperation do
        begin
          FreeAndNil (fltBind);
          if Assigned (reqBind) then
            reqBind.Free;
          if Assigned (rpyBind) then
            rpyBind.Free;
          reqBind := _LoadSwiftMtMsg(oXml.Items.XmlCheckedItemByTag['Req'], reqXsd, reqDescrFilename, reqDescrExpansionFilename);
          rpyBind := _LoadSwiftMtMsg(oXml.Items.XmlCheckedItemByTag['Rpy'], rpyXsd, rpyDescrFilename, rpyDescrExpansionFilename);
          fltBind := _LoadSwiftMtMsg(oXml.Items.XmlCheckedItemByTag['Flt'], FaultXsd, fltDescrFilename, fltDescrExpansionFilename);
          operationRecognitionUpdate (xOperation, reqRecognition, oXml.Items.XmlCheckedItemByTag['reqRecognition']);
          operationRecognitionUpdate (xOperation, rpyRecognition, oXml.Items.XmlCheckedItemByTag['rpyRecognition']);
        end;
      end;
    end;
  finally
    FreeAndNil(sList);
  end;
end;

function TWsdlProject.operationRecognitionXml(aLabel: String;
  aType: TRecognitionType; aSl: TStringList): TXml;
var
  r: Integer;
  sr: TRecognition;
begin
  if aSl.Count < 1 then raise Exception.Create('TWsdlProject.operationRecognitionXml: no Recognition');
  sr := aSl.Objects[0] as TRecognition;
  result := TXml.CreateAsString(aLabel, '');
  with result do
  begin
    case aType of
      rtSoap: ;
      rtDocument:
        result.AddXml (TXml.CreateAsString('HttpDocument', sr.Value));
      rtHeader: with result.AddXml (TXml.CreateAsString('HTTPHeader', '')) do
      begin
        AddXml (TXml.CreateAsString('Name', sr.Name));
        AddXml (TXml.CreateAsString('Value', sr.Value));
      end;
      rtXml: with result.AddXml (TXml.CreateAsString('XmlElement', '')) do
      begin
        AddXml (TXml.CreateAsString('Path', sr.Name));
        AddXml (TXml.CreateAsString('Value', sr.Value));
      end;
      rtSubString: with result.AddXml (TXml.CreateAsString('SubStrings', '')) do
      begin
        for r := 0 to aSl.Count - 1 do
        begin
          sr := aSl.Objects[r] as TRecognition;
          with AddXml (TXml.CreateAsString('SubString', '')) do
          begin
            AddXml (TXml.CreateAsInteger('Start', sr.Start));
            AddXml (TXml.CreateAsInteger('Length', sr.Length));
            AddXml (TXml.CreateAsString('Value', sr.Value));
          end;
        end;
      end;
    end;
  end;
end;

function TWsdlProject.xsdOperationsXml(aMainFileName: String): TXml;
var
  x: Integer;
  xOperation: TWsdlOperation;
  xXml: TXml;
begin
  xXml := TXml.CreateAsString('XsdOperations', '');
  try
    for x := 0 to XsdWsdl.Services.Services[0].Operations.Count - 1 do
    begin
      xOperation := XsdWsdl.Services.Services[0].Operations.Operations[x];
      with xXml.AddXml(TXml.CreateAsString('Operation', '')) do
      begin
        AddXml (TXml.CreateAsString('Name', xOperation.Name));
        if Assigned (xOperation.reqBind)
        and (xOperation.reqDescrFilename <> '') then
          with AddXml (TXml.CreateAsString('Req', '')) do
          begin
            AddXml ( TXml.CreateAsString ( 'DescriptionFile', xOperation.reqDescrFilename));
            if xOperation.reqBind.Children.Count > 0 then
              AddXml(TXml.CreateAsString('ElementName', (xOperation.reqBind as TXml).Items.XmlItems[0].Name));
          end;
        if Assigned (xOperation.rpyBind)
        and (xOperation.rpyDescrFilename <> '') then
          with AddXml (TXml.CreateAsString('Rpy', '')) do
          begin
            AddXml ( TXml.CreateAsString ( 'DescriptionFile', xOperation.rpyDescrFilename));
            if xOperation.rpyBind.Children.Count > 0 then
              AddXml(TXml.CreateAsString('ElementName', (xOperation.rpyBind as TXml).Items.XmlItems[0].Name));
          end;
        if Assigned (xOperation.fltBind)
        and (xOperation.fltDescrFilename <> '') then
          with AddXml (TXml.CreateAsString('Flt', '')) do
          begin
            AddXml ( TXml.CreateAsString ( 'DescriptionFile', xOperation.fltDescrFilename));
            if xOperation.fltBind.Children.Count > 0 then
              AddXml(TXml.CreateAsString('ElementName', (xOperation.fltBind as TXml).Items.XmlItems[0].Name));
          end;
        if xOperation.reqRecognition.Count > 0 then
          AddXml (operationRecognitionXml('reqRecognition', xOperation.RecognitionType, xOperation.reqRecognition));
        if xOperation.rpyRecognition.Count > 0 then
          AddXml (operationRecognitionXml('rpyRecognition', xOperation.RecognitionType, xOperation.rpyRecognition));
      end;
    end;
    xXml.CheckDownline(True);
    result := TXml.Create(-1000, OperationDefsXsd.FindXsd ('OperationDefs.XsdOperations'));
    result.CheckDownline(False);
    result.LoadValues(xXml, False, True, False, True);
  finally
    xXml.Free;
  end;
end;

function TWsdlProject.cobolOperationsXml: TXml;
var
  x: Integer;
  xOperation: TWsdlOperation;
  xXml: TXml;
begin
  xXml := TXml.CreateAsString('CobolOperations', '');
  try
    for x := 0 to CobolWsdl.Services.Services[0].Operations.Count - 1 do
    begin
      xOperation := CobolWsdl.Services.Services[0].Operations.Operations[x];
      with xXml.AddXml(TXml.CreateAsString('Operation', '')) do
      begin
        AddXml (TXml.CreateAsString('Name', xOperation.Name));
        if xOperation.CobolEnvironment = ceTandem then
          AddXml (TXml.CreateAsString('CobolEnvironment', 'Tandem'));
        if xOperation.CobolEnvironment = ceIbmZOs then
          AddXml (TXml.CreateAsString('CobolEnvironment', 'IBM Zos'));
        if Assigned (xOperation.reqBind)
        and (xOperation.reqDescrFilename <> '') then
          with AddXml (TXml.CreateAsString('Req', '')) do
            AddXml ( TXml.CreateAsString ( 'DescriptionFile', xOperation.reqDescrFilename));
        if Assigned (xOperation.rpyBind)
        and (xOperation.rpyDescrFilename <> '') then
          with AddXml (TXml.CreateAsString('Rpy', '')) do
            AddXml ( TXml.CreateAsString ( 'DescriptionFile', xOperation.rpyDescrFilename));
        if Assigned (xOperation.fltBind)
        and (xOperation.fltDescrFilename <> '') then
          with AddXml (TXml.CreateAsString('Flt', '')) do
            AddXml ( TXml.CreateAsString ( 'DescriptionFile', xOperation.fltDescrFileName));
        if xOperation.reqRecognition.Count > 0 then
          AddXml (operationRecognitionXml('reqRecognition', xOperation.RecognitionType, xOperation.reqRecognition));
        if xOperation.rpyRecognition.Count > 0 then
          AddXml (operationRecognitionXml('rpyRecognition', xOperation.RecognitionType, xOperation.rpyRecognition));
      end;
    end;
    xXml.CheckDownline(True);
    result := TXml.Create(-1000, OperationDefsXsd.FindXsd ('OperationDefs.CobolOperations'));
    result.CheckDownline(False);
    result.LoadValues(xXml, False, True, False, True);
  finally
    xXml.Free;
  end;
end;

function TWsdlProject.swiftMtOperationsXml: TXml;
var
  x: Integer;
  xOperation: TWsdlOperation;
  xXml: TXml;
begin
  xXml := TXml.Create;
  try
    xXml := TXml.CreateAsString('SwiftMtOperations', '');
    for x := 0 to SwiftMtWsdl.Services.Services[0].Operations.Count - 1 do
    begin
      xOperation := SwiftMtWsdl.Services.Services[0].Operations.Operations[x];
      with xXml.AddXml(TXml.CreateAsString('Operation', '')) do
      begin
        AddXml (TXml.CreateAsString('Name', xOperation.Name));
        if Assigned (xOperation.reqBind)
        and (xOperation.reqDescrFilename <> '') then
          with AddXml (TXml.CreateAsString('Req', '')) do
          begin
            AddXml(TXml.CreateAsString('DescriptionFile', xOperation.reqDescrFilename));
            if xOperation.reqDescrExpansionFilename <> '' then
              AddXml(TXml.CreateAsString('DescriptionExpansionFile', xOperation.reqDescrExpansionFilename));
          end;
        if Assigned (xOperation.rpyBind)
        and (xOperation.rpyDescrFilename <> '') then
          with AddXml (TXml.CreateAsString('Rpy', '')) do
          begin
            AddXml(TXml.CreateAsString('DescriptionFile', xOperation.rpyDescrFilename));
            if xOperation.rpyDescrExpansionFilename <> '' then
              AddXml(TXml.CreateAsString('DescriptionExpansionFile', xOperation.rpyDescrExpansionFilename));
          end;
        if xOperation.reqRecognition.Count > 0 then
          AddXml (operationRecognitionXml('reqRecognition', xOperation.RecognitionType, xOperation.reqRecognition));
        if xOperation.rpyRecognition.Count > 0 then
          AddXml (operationRecognitionXml('rpyRecognition', xOperation.RecognitionType, xOperation.rpyRecognition));
      end;
    end;
    xXml.CheckDownline(True);
    result := TXml.Create(-1000, OperationDefsXsd.FindXsd ('OperationDefs.SwiftMtOperations'));
    result.CheckDownline(False);
    result.LoadValues(xXml, False, True, False, True);
  finally
    xXml.Free;
  end;
end;

function TWsdlProject.freeFormatOperationsXml: TXml;
var
  o, r: Integer;
  xOperation: TWsdlOperation;
  xRecog: TRecognition;
begin
  result := TXml.CreateAsString('FreeFormatOperations', '');
  with result do
  begin
    with FreeFormatWsdl.Services.Services[0] do
    begin
      for o := 0 to Operations.Count - 1 do
      begin
        xOperation := Operations.Operations[o];
        with AddXml (TXml.CreateAsString('Operation', '')) do
        begin
          AddXml (TXml.CreateAsString('Name', xOperation.Name));
          if Assigned (xOperation.reqRecognition)
          and (xOperation.reqRecognition.Count > 0) then
          begin
            with AddXml (TXml.CreateAsString('reqRecognition', '')) do
            begin
              with AddXml (TXml.CreateAsString('SubStrings', '')) do
              begin
                for r := 0 to xOperation.reqRecognition.Count - 1 do
                begin
                  xRecog := xOperation.reqRecognition.Objects[r] as TRecognition;
                  with AddXml (TXml.CreateAsString('SubString', '')) do
                  begin
                    AddXml (TXml.CreateAsInteger('Start', xRecog.Start));
                    AddXml (TXml.CreateAsInteger('Length', xRecog.Length));
                    AddXml (TXml.CreateAsString('Value', xRecog.Value));
                  end;
                end;
              end;
            end;
          end;
          if Assigned (xOperation.rpyRecognition)
          and (xOperation.rpyRecognition.Count > 0) then
          begin
            with AddXml (TXml.CreateAsString('rpyRecognition', '')) do
            begin
              with AddXml (TXml.CreateAsString('SubStrings', '')) do
              begin
                for r := 0 to xOperation.rpyRecognition.Count - 1 do
                begin
                  xRecog := xOperation.rpyRecognition.Objects[r] as TRecognition;
                  with AddXml (TXml.CreateAsString('SubString', '')) do
                  begin
                    AddXml (TXml.CreateAsInteger('Start', xRecog.Start));
                    AddXml (TXml.CreateAsInteger('Length', xRecog.Length));
                    AddXml (TXml.CreateAsString('Value', xRecog.Value));
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    CheckDownline(True);
  end;
end;

function TWsdlProject.GetAbortPressed: Boolean;
begin
  result := fAbortPressed;
end;

function TWsdlProject .getDoClearSnapshots : Boolean ;
begin
  result := Assigned (fClearedSnapshots);
end;

function TWsdlProject .getDoClearLogs : Boolean ;
begin
  result := Assigned (fClearedLogs);
end;

function TWsdlProject.SendNoneMessage(aOperation: TWsdlOperation; aMessage: String): String;
begin
  result := '';
end;

function TWsdlProject.FindOperationOnRequest(aLog: TLog; aDocument, aString: String; aDoClone: Boolean): TWsdlOperation;
var
  xXml: TXml;
begin
  result := nil;
  xXml := TXml.Create;
  try
    try
      xXml.LoadFromString(aString, nil);
    except
      xXml.Name := '';
    end;
    //AcquireLock;
    try
      if xXml.Name = '' then
        if aString <> '' then
          result := FindCcbOperationOnRequest (aLog, aString)
        else
          result := FindOperationOnDocument (aDocument)
      else
      begin
        xXml.SeparateNsPrefixes;
        xXml.ResolveNameSpaces;
        result := FindXmlOperationOnRequest (aDocument, xXml);
        if not Assigned (result) then
          result := FindCcbOperationOnRequest (aLog, aString);
      end;
      if Assigned (result) then
        aLog.OperationName:=result.reqTagName;
    finally
      //ReleaseLock;
    end;
    if not Assigned (result) then
      raise Exception.Create (S_NO_OPERATION_FOUND);
    result.AcquireLock;
    try
      if aDoClone then
      begin
        result := TWsdlOperation.Create(result);
        if result.PrepareErrors <> '' then
          raise Exception.CreateFmt('%s (%s)', [result.PrepareErrors, result.reqTagName]);
      end;
      case result.WsdlService.DescriptionType of
        ipmDTFreeFormat: result.FreeFormatReq := aString;
        ipmDTCobol, ipmDTBmtp: (result.reqBind as TIpmItem).BufferToValues (FoundErrorInBuffer, aString);
        ipmDTXml: result.SoapXmlRequestToBindables (xXml, False);
        ipmDTXsd: result.SoapXmlRequestToBindables (xXml, True);
        ipmDTWsdl: result.SoapXmlRequestToBindables (xXml, True);
        ipmDTEmail: result.SoapXmlRequestToBindables (xXml, False);
        ipmDTSwiftMT: result.SwiftMtRequestToBindables(aString);
      end;
    finally
      result.ReleaseLock;
    end;
  finally
    FreeAndNil (xXml);
  end;
end;

procedure TWsdlProject.mqOnNewThread(Sender: TObject);
var
  xCurThreads: Integer;
begin
  AcquireLock;
  xCurThreads := mqCurWorkingThreads;
  ReleaseLock;
  while xCurThreads >= mqMaxWorkingThreads do
  begin
    Sleep (20);
    AcquireLock;
    xCurThreads := mqCurWorkingThreads;
    ReleaseLock;
  end;
end;

procedure TWsdlProject.mqStubMessage ( Sender: TObject
                                      ; aHeader, aBody: String
                                      ; aRfhHeader: AnsiString
                                      ; MsgType: MQLONG
                                      ; MsgDesc: MQMD
                                      ; MqReturnCode: String
                                      );
var
  aMqInterface: TMqInterface;
  xLog: TLog;
  xProcessed: Boolean;
begin
  xProcessed := False;
  aMqInterface := Sender as TMqInterface;
  if ((MsgType = MQMT_REQUEST) and (not aMqInterface.browseMqMtRequest))
  or ((MsgType = MQMT_REPLY) and (not aMqInterface.browseMqMtReply))
  or ((MsgType = MQMT_REPORT) and (not aMqInterface.browseMqMtReport))
  or ((MsgType = MQMT_DATAGRAM) and (not aMqInterface.browseMqMtDatagram))
  or ((MsgType = MQMT_SYSTEM_LAST) and True)
  or ((MsgType = MQMT_APPL_FIRST) and True)
  or ((MsgType = MQMT_APPL_LAST) and True)
  then exit;
  Inc (mqCurWorkingThreads);
  {$ifdef windows}
  CoInitialize(nil);
  {$endif}
  xLog := TLog.Create;
  try
    xLog.InboundTimeStamp := Now;
    xLog.TransportType := ttMq;
    xLog.RequestHeaders := aHeader;
    xLog.RequestBody := aBody;
    xLog.InboundBody := aBody;
    try
      CreateLogReply (xLog, xProcessed, True);
      DelayMS (xLog.DelayTimeMs);
      if (    (xLog.ReplyBody <> '')
          and (   (MsgType = MQMT_REQUEST)
               or (not aMqInterface.UseReplyToQueue)
              )
         ) then
        xLog.ReplyHeaders := aMqInterface.PutReply(xLog.ReplyBody, aRfhHeader, MsgDesc)
      else
      begin
        xLog.ReplyBody := '';
        xLog.ReplyHeaders := '';
      end;
    except
      on e: exception do
      begin
        LogServerMessage(format('Exception %s. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
        xLog.Exception := e.Message;
      end;
    end;
  finally
    xLog.OutboundTimeStamp := Now;
    DisplayLog ('', xLog);
    AcquireLock;
    Dec (mqCurWorkingThreads);
    ReleaseLock;
    {$ifdef windows}
    CoUninitialize;
    {$endif}
  end;
end;

procedure TWsdlProject.FindRequestReply ( aLog: TLog
                                         ; aDocument, aString: String
                                         ; var isRequest: Boolean
                                         );
begin
  isRequest := False;
  aLog.CorrelationId := '';
  aLog.Mssg := nil;
  try aLog.Operation := FindOperationOnRequest(aLog, aDocument, aString, False); except end;
  if Assigned (aLog.Operation) then
  begin
    isRequest := True;
    aLog.Mssg := aLog.Operation.MessageBasedOnRequest;
    aLog.CorrelationId := aLog.Operation.CorrelationIdAsText ('; ');
  end
  else
  begin
    try aLog.Operation := FindOperationOnReply(aString); except end;
    if Assigned (aLog.Operation) then
      aLog.CorrelationId := aLog.Operation.CorrelationIdAsText ('; ');
  end;
end;

procedure TWsdlProject.FindReply(aLog: TLog; aDocument, aRequest: String; var aOperation: TWsdlOperation;
  var aReply: TWsdlMessage; var aCorrelationId: String);
begin
  aCorrelationId := '';
  aReply := nil;
  aOperation := FindOperationOnRequest(aLog, aDocument, aRequest, False);
  if Assigned (aOperation) then
  begin
    aReply := aOperation.MessageBasedOnRequest;
    aCorrelationId := aOperation.CorrelationIdAsText ('; ');
  end;
end;

procedure TWsdlProject.EnvironmentListClear;
var
  x: Integer;
begin
  for x := 0 to EnvironmentList.Count - 1 do
    (EnvironmentList.Objects [x] as TXml).Free;
  EnvironmentList.Clear;
end;

procedure TWsdlProject.POP3ServerCheckUser(aContext: TIdContext;
  aServerContext: TIdPOP3ServerContext);
begin
  if (aServerContext.Username <> Listeners.pop3UserName)
  or (aServerContext.Password <> Listeners.pop3Password) then
    raise Exception.Create ('Invalid username or password');
end;

procedure TWsdlProject.POP3ServerDelete(aCmd: TIdCommand; AMsgNo: Integer);
var
  xLog: TLog;
begin
  aCmd.Reply.SetReply(ST_OK, 'marked as deleted');
  try
    xLog := displayedLogs.LogItems[AMsgNo];
    if (xLog.TransportType <> ttSmtp)
    or (xLog.StubAction <> saStub)
    or (xLog.markDeleted) then
      raise Exception.Create(_progName + ' - report: Illegal messagenumber');
    xLog.markDeleted := True;
  except
    on e: Exception do
      aCmd.Reply.SetReply(ST_ERR, e.Message);
  end;
end;

procedure TWsdlProject.POP3ServerList(aCmd: TIdCommand; AMsgNo: Integer);
var
  c, x, n, tSize: Integer;
begin
  try
    c := displayedLogs.Count;
    n := 0;
    tSize := 0;
    for x := 0 to c - 1 do
      with displayedLogs.LogItems[x] do
        if (TransportType = ttSmtp)
        and (StubAction = saStub)
        and (not markDeleted) then
        begin
          Inc (n);
          Inc (tSize, Stream.Size);
        end;
    aCmd.Reply.SetReply(ST_OK, IntToStr(n) + ' messages  '+'('+ IntToStr(tSize) + ' octets)');
    aCmd.SendReply;
    for x := 0 to c - 1 do
      with displayedLogs.LogItems[x] do
        if (TransportType = ttSmtp)
        and (StubAction = saStub)
        and (not markDeleted) then
          aCmd.Context.Connection.IOHandler.WriteLn ( IntToStr (x)
                                                    + ' '
                                                    + IntToStr (Stream.Size)
                                                    );
    aCmd.Context.Connection.IOHandler.writeln('.');
  except
    on e: Exception do
      aCmd.Reply.SetReply(ST_ERR, e.Message);
  end;
end;

procedure TWsdlProject.POP3ServerRetrieve(aCmd: TIdCommand; AMsgNo: Integer);
var
  xLog: TLog;
begin
  try
    xLog := displayedLogs.LogItems[AMsgNo];
    if xLog.TransportType <> ttSmtp then
      raise Exception.Create(_progName + ' - report: Illegal messagenumber');
    if not Assigned (xLog.Stream) then
      raise Exception.Create(_progName + ' - report: No message attached to log item');
    if xLog.markDeleted then
      raise Exception.Create(_progName + ' - report: Message marked as deleted');
    aCmd.Reply.SetReply(ST_OK, 'message follows');
    aCmd.SendReply; // <-- YOU MUST DO THIS BEFORE SENDING THE DATA
    xLog.Stream.Position := 0;
    aCmd.Context.Connection.IOHandler.Write(xLog.Stream, xLog.Stream.Size, False);
    aCmd.Context.Connection.IOHandler.WriteLn('.');
  except
    on e: Exception do
      aCmd.Reply.SetReply(ST_ERR, e.Message);
  end;
end;

procedure TWsdlProject.POP3ServerStat(aCmd: TIdCommand; out oCount: Integer; out oSize: Int64);
var
  c, x, n, tSize: Integer;
begin
  try
    c := displayedLogs.Count;
    n := 0;
    tSize := 0;
    for x := 0 to c - 1 do
      with displayedLogs.LogItems[x] do
        if (TransportType = ttSmtp)
        and (StubAction = saStub)
        and (not markDeleted) then
        begin
          Inc (n);
          Inc (tSize, Stream.Size);
        end;
    oCount := n;
    oSize := tSize;
    aCmd.Reply.SetReply(ST_OK, IntToStr(ocount)+' '+IntToStr(osize));
  except
    on e: Exception do
      aCmd.Reply.SetReply(ST_ERR, e.Message);
  end;
end;

procedure TWsdlProject.HTTPServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  function _relatesToKey(aRequestBody: String): String;
  begin
    result := '';
    with TXml.Create do
    try
      try
        LoadFromString(aRequestBody, nil);
        result := FindUQXml ('Envelope.Header.RelatesTo').Value;
      except
      end;
    finally
      free;
    end;
  end;
  function findAsyncReplyLog(aRelatesTo: String; var f: Integer): TLog;
  begin
    result := nil;
    if AsynchRpyLogs.Find(aRelatesTo, f) then
      result := AsynchRpyLogs.LogItems[f];
  end;
var
  rLog, xLog: TLog;
  xProcessed: Boolean;
  f: Integer;
  xStream: TMemoryStream;
  xRelatesTo, xNotification: String;
  xOnRequestViolatingAddressPath: TOnRequestViolating;
begin
  xProcessed := False;
  try // finally set for hhtp reply
    {$ifdef windows}
    CoInitialize (nil);
    {$endif}
    try
      xLog := TLog.Create;
      rLog := nil;
      xLog.InboundTimeStamp := Now;
      xLog.TransportType := ttHttp;
      xLog.httpCommand := ARequestInfo.Command;
      xLog.httpDocument := ARequestInfo.Document;
      xLog.httpSoapAction := ARequestInfo.RawHeaders.Values ['SOAPAction'];
      xLog.RequestHeaders := ARequestInfo.RawHeaders.Text;
      xLog.httpParams := ARequestInfo.QueryParams;
      try
        if ARequestInfo.Command = 'GET' then
        begin
          xLog.RequestBody := '';
          xLog.InboundBody := xLog.RequestBody;
          HTTPServerCommandGetGet(xLog, AContext, ARequestInfo, AResponseInfo);
        end;
        if ARequestInfo.Command = 'TRACE' then
        begin
          AContext.Data := xLog;
          HTTPServerCommandTrace(AContext, ARequestInfo, AResponseInfo);
        end;
        // fromHERE
        if (ARequestInfo.Command = 'POST')
        or (ARequestInfo.Command = 'PUT') then
        begin
          xLog.RequestBody := httpRequestStreamToString(ARequestInfo, AResponseInfo);
          xLog.InboundBody := xLog.RequestBody;
          AResponseInfo.ContentType := ARequestInfo.ContentType;
          rLog := nil;
          AcquireLogLock;
          try
            if AsynchRpyLogs.Count > 0 then
            begin
              xRelatesTo := _relatesToKey(xLog.RequestBody);
              f := -1;  // get rid of compiler warning
              rLog := findAsyncReplyLog(xRelatesTo, f);
            end;
          finally
            ReleaseLogLock;
          end;
          if Assigned (rLog) then // this is an asynchronous reply
          begin
            try
              AResponseInfo.ResponseNo := 202;
              AResponseInfo.ContentText := ProcessInboundReply (xLog, rLog);
              AcquireLogLock;
              try
                rLog := findAsyncReplyLog(xRelatesTo, f); // make sure f addresses rlog again
                AsynchRpyLogs.Delete(f); // rLog may disappear
              finally
                ReleaseLogLock;
              end;
            finally
              FreeAndNil(xLog);
            end;
          end
          else
          begin // request
            try
              try
                AResponseInfo.ResponseNo := 200;
                CreateLogReply (xLog, xProcessed, True);
                if Assigned (xLog.Operation) then
                begin
                  with TIdURI.Create(xLog.Operation.SoapAddress) do
                  try
                    if Path + Document <> xlog.httpDocument then
                    begin
                      xNotification := 'Used path ('
                                     + xlog.httpDocument
                                     + ') differs from expected path ('
                                     + Path
                                     + Document
                                     + ')'
                                     ;
                      xLog.Notifications := xLog.Notifications
                                          + xNotification
                                          + LineEnding
                                          ;
                      if xLog.Operation.OnRequestViolatingAddressPath = rvsDefault then
                        xOnRequestViolatingAddressPath := OnRequestViolatingAddressPath
                      else
                        xOnRequestViolatingAddressPath := xLog.Operation.OnRequestViolatingAddressPath;
                      case xOnRequestViolatingAddressPath of
                        rvsContinue: ;
                        rvsRaiseErrorMessage: raise Exception.Create(xNotification);
                        rvsAddRemark: xLog.AddRemark(xNotification);
                      end;
                    end;
                  finally
                    Free;
                  end;
                end;
                if xLog.Operation.isOneWay
                or xLog.isAsynchronousRequest
                or xLog.isAsynchronousReply then
                  AResponseInfo.ResponseNo := 202;
                if (xLog.Exception <> '')
                and (   (not Assigned (xLog.Operation))
                     or (not xLog.Operation.WsdlService.SuppressHTTP500)
                    ) then
                  AResponseInfo.ResponseNo := 500;
                if (not xLog.isAsynchronousRequest)
                and (not xLog.isAsynchronousReply) then
                  DelayMS (xLog.DelayTimeMs);
                if xLog.isAsynchronousRequest then
                begin
                  AcquireLogLock;
                  try
                    xLog.Claim;
                  finally
                    ReleaseLogLock;
                  end;
                  TSendAsynchReplyThread.Create(self, xLog);
                end;
              except
                on e: exception do
                begin
                  LogServerMessage(format('Exception %s. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
                  xLog.Exception := e.Message;
                  if (not Assigned (xLog.Operation))
                  or (not xLog.Operation.WsdlService.SuppressHTTP500) then
                    AResponseInfo.ResponseNo := 500;
                end;
              end;  // except
            finally
              aResponseInfo.ContentText := xLog.ReplyBody;
            end; // finally request
          end; // request
        end; // post
      finally
        if Assigned (xLog) then {still}
        begin
          xLog.OutboundTimeStamp := Now;
          DisplayLog ('', xLog);
        end;
      end;
    finally
    {$ifdef windows}
      CoUninitialize;
    {$endif}
    end;
  finally
    // setup for HTTP reply
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
end;

procedure TWsdlProject.HTTPServerCreatePostStream(AContext: TIdContext;
  AHeaders: TIdHeaderList; var VPostStream: TStream);
begin
  VPostStream := TMemoryStream.Create;
end;

procedure TWsdlProject.HttpServerBmtpCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  xLog: TLog;
  xProcessed: Boolean;
  mXml: TXml;
  xStream: TMemoryStream;
  s, d: AnsiString;
begin
  {$ifdef windows}
  CoInitialize (nil);
  {$endif}
  try // to always CoFinalize
    if ARequestInfo.Command <> 'POST' then
    begin
      AResponseInfo.ResponseText := 'Bmtp: Http ' + ARequestInfo.Command + ' not supported';
      exit;
    end;
    try // to always reply
      xLog := TLog.Create;
      try // to always log
        try // to catch exceptions
          xLog.InboundTimeStamp := Now;
          xLog.TransportType := ttBmtp;
          xLog.httpCommand := ARequestInfo.Command;
          xLog.httpDocument := ARequestInfo.Document;
          xLog.RequestHeaders := ARequestInfo.RawHeaders.Text;
          xLog.RequestBody := httpRequestStreamToString(ARequestInfo, AResponseInfo);
          with TXml.Create do
          try
            LoadFromString(xLog.RequestBody, nil);
            if Name = '' then
              raise Exception.Create('Bmtp: Could not parse message envelope as XML');
            if Name <> 'bmtpEnvelope' then
              raise Exception.Create('Bmtp: No Bmtp envelope found');
            xLog.ServiceName := Items.XmlValueByTagDef['Service', 'not specified'];
            xLog.OperationName := Items.XmlValueByTagDef['Operation', 'not specified'];
            mXml := Items.XmlItemByTag['Request'];
            if not Assigned (mXml) then
              raise Exception.Create('Bmtp: Element Request not found');
            try
              s := mXml.Value;
              d := Base64DecodeStr(s);
              xLog.RequestBody := d; // when everything Ok loose the Bmtp envelope here
            except
              on e: Exception do
                raise Exception.Create('Bmtp: Exception while b64decoding message: ' + e.Message);
            end;
          finally
            Free;
          end;
          xLog.InboundBody := xLog.RequestBody;
          AResponseInfo.ContentType := ARequestInfo.ContentType;
          xProcessed := False;
          AResponseInfo.ResponseNo := 200;
          CreateLogReply (xLog, xProcessed, True);
          DelayMS (xLog.DelayTimeMs);
          with TXml.CreateAsString('bmtpEnvelope', '') do
          try
            AddXml(TXml.CreateAsString('Service',xLog.ServiceName));
            AddXml(TXml.CreateAsString('Operation',xLog.OperationName));
            s := xLog.ReplyBody;
            d := Base64EncodeStr(s);
            AddXml(TXml.CreateAsString('Reply', d));
            aResponseInfo.ContentText := AsText(False,0,False,False);
          finally
            Free;
          end;
        except
          on e: exception do
          begin
            LogServerMessage(format('Exception %s. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
            xLog.Exception := e.Message;
            AResponseInfo.ContentText := e.Message;
          end;
        end;  // except
      finally
        xLog.OutboundTimeStamp := Now;
        DisplayLog ('', xLog);
      end; // finally request
    finally
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
  finally
    {$ifdef windows}
    CoUninitialize;
    {$endif}
  end;
end;

procedure TWsdlProject.HTTPServerCommandPutPut(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  AResponseInfo.ContentText := 'JanBo PutPut';
  AResponseInfo.ResponseNo := 500;
end;

procedure TWsdlProject.HTTPServerCommandGetGet(aLog: TLog; AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  procedure _replLocations (aName, aFileName: String; aXml: TXml);
  var
    s: String;
    x: Integer;
  begin
    s := NameWithoutPrefix(aXml.Name);
    if (s = 'import')
    or (s = 'include') then
    begin
      for x := 0 to aXml.Attributes.Count - 1 do with aXml.Attributes.XmlAttributes[x] do
        if Name = 'schemaLocation' then
          Value := SchemaLocations.AddSchemaLocation(aName, ExpandRelativeFileName(aFileName, Value));
    end;
    for x := 0 to aXml.Items.Count - 1 do
      _replLocations(aName, aFileName, aXml.Items.XmlItems[x]);
  end;
  function _getWsdl (aWsdlName: String): String;
  var
    x, f: Integer;
    s : String;
    epXml: TXml;
    epAtr: TXmlAttribute;
  begin
    if not wsdlNames.Find (aWsdlName, f) then
      raise Exception.CreateFmt('No WSDL exists with name %s', [aWsdlName]);
    with wsdlNames.Objects [f] as TWsdl do
    begin
      s := ReadStringFromFile(FileName);
      with TXml.Create do
      try
        LoadFromString(s, nil);
        try
          epXml := Items.XmlItemByTag['service'];
          epXml := epXml.Items.XmlItemByTag['port'];
          epXml := epXml.Items.XmlItemByTag['address'];
          epAtr := epXml.Attributes.AttributeByTag['location'];
          epAtr.Value := 'http://' + _WsdlHostName + ':' + _WsdlPortNumber;
        except
          raise Exception.CreateFmt('No endpoint address found in %s', [aWsdlName]);
        end;
        for x := 0 to Items.Count - 1 do
          _replLocations('/' + aWsdlName, FileName, Items.XmlItems[x]);
        result := Text;
      finally
        Free;
      end;
    end;
  end;
  function _getXsd (aUri: String): String;
  var
    x: Integer;
    sLoc: TSchemaLocation;
    s: String;
  begin
    with TXml.Create do
    try
      sLoc := SchemaLocations.SchemaLocations[aUri];
      if not Assigned (sLoc) then
        result := RedirectUnknownOperation(aLog)
      else
      begin
        s := ReadStringFromFile(sLoc.FileName);
        LoadFromString(s, nil);
        for x := 0 to Items.Count - 1 do
          _replLocations(sLoc.DocumentName, sLoc.FileName, Items.XmlItems[x]);
        result := Text;
      end;
    finally
      Free;
    end;
  end;
  function _prepWsdl(fn: String):String;
  var
    w: Integer;
    xXml: TXml;
    xWsdl: TWsdl;
  begin
    result := '';
    xXml := htmlCreateXml(_progName, 'Web Service Descriptions');
    try
      with htmlFindContentXml(xXml) do
      begin
        with AddXml (TXml.CreateAsString('span', '')) do
          with AddXml (TXml.CreateAsString('p', 'Provides basic service information.')) do
            AddAttribute(TXmlAttribute.CreateAsString('class','intro'));
        with AddXml (TXml.CreateAsString('span', '')) do
        begin
          with AddXml (TXml.CreateAsString('p', 'The following Web Service Descriptions are available:')) do
          begin
            AddAttribute(TXmlAttribute.CreateAsString('class','intro'));
            for w := 0 to Wsdls.Count - 1 do
            begin
              xWsdl := wsdls.Objects[w] as TWsdl;
              with AddXml (TXml.CreateAsString('ul', '')) do
                with AddXml (TXml.CreateAsString('li', '')) do
                  with AddXml (TXml.CreateAsString('a', xWsdl.Name)) do
                    AddAttribute(TXmlAttribute.CreateAsString('href', xWsdl.Name + '?WSDL'));
            end
          end;
        end;
        result := htmlXmlAsString (xXml, _wsdlStubStylesheet);
      end;
    finally
      xXml.Free;
    end;
  end;
begin
  AResponseInfo.ContentEncoding := 'identity';
  aLog.RequestBody := ARequestInfo.Document;
  if (ARequestInfo.Document = '/index.html')
  or (ARequestInfo.Document = '/index')
  or (ARequestInfo.Document = '/')
//or (ARequestInfo.QueryParams = 'WSDL')
//or (Copy(ARequestInfo.QueryParams, 1, 4) = 'XSD=')
  then begin
    if not PublishDescriptions then
      raise Exception.CreateFmt('<html><b>%s</b> is configured not to publish webservicedescriptions, in case you need these descriptions, change the %s project options</html>', [_ProgName, _ProgName]);
  end;
  try
    if (ARequestInfo.Document = '/index.html')
    or (ARequestInfo.Document = '/index')
    or (ARequestInfo.Document = '/') then
    begin
      try
        AResponseInfo.ContentText := _prepWsdl(ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), indexWsdlsHtmlFileName));
        Exit;
      except
        on e: exception do
        begin
          AResponseInfo.ContentText := e.Message + #10#13 + ExceptionStackListString(e);
          AResponseInfo.ResponseNo := 500;
          alog.Exception := AResponseInfo.ContentText;
          exit;
        end;
      end;
    end;
    if (ARequestInfo.QueryParams = 'WSDL') then
    begin
      try
        AResponseInfo.ContentText := _getWsdl(Copy (ARequestInfo.Document, 2, 10000));
        Exit;
      except
        on e: exception do
        begin
          AResponseInfo.ContentText := e.Message + #10#13 + ExceptionStackListString(e);
          AResponseInfo.ResponseNo := 500;
          alog.Exception := AResponseInfo.ContentText;
          exit;
        end;
      end;
    end;
    if Copy(ARequestInfo.QueryParams, 1, 4) = 'XSD=' then
    begin
      try
        AResponseInfo.ContentText := _getXsd(ARequestInfo.Document + '?' + ARequestInfo.QueryParams);
        Exit;
      except
        on e: exception do
        begin
          AResponseInfo.ContentText := e.Message + #10#13 + ExceptionStackListString(e);
          AResponseInfo.ResponseNo := 500;
          alog.Exception := AResponseInfo.ContentText;
          exit;
        end;
      end;
    end;
    AResponseInfo.ContentText := RedirectUnknownOperation(aLog);
  finally
    alog.ReplyBody := AResponseInfo.ContentText;
  end;
end;

procedure TWsdlProject.HTTPServerCommandTrace(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  xLog: TLog;
begin
  xLog := AContext.Data as TLog;
  if Assigned(xLog) then
  begin
    xLog.ReplyBody := '';
    AResponseInfo.ContentText := xLog.ReplyBody;
  end;
end;

procedure TWsdlProject .setDoClearSnapshots (AValue : Boolean );
begin
  if AValue = Assigned (fClearedSnapshots) then Exit;
  AcquireLogLock;
  try
    if AValue then
    begin
       toDisplaySnapshots.Clear;
       fClearedSnapshots := displayedSnapshots;
       displayedSnapshots := TSnapshotList.Create;
    end
    else
    begin
      fClearedSnapshots.Clear;
      fClearedSnapshots.Free;
      fClearedSnapshots := nil;
    end;
  finally
    ReleaseLogLock;
  end;
end;

procedure TWsdlProject.setDoClearLogs (AValue : Boolean );
begin
  if AValue = Assigned (fClearedLogs) then Exit;
  AcquireLogLock;
  try
    if AValue then
    begin
       toDisplayLogs.Clear;
       fClearedLogs := displayedLogs;
       displayedLogs := TLogList.Create;
    end
    else
    begin
      fClearedLogs.Clear;
      fClearedLogs.Free;
      fClearedLogs := nil;
    end;
  finally
    ReleaseLogLock;
  end;
end;

procedure TWsdlProject .setOnNeedTacoHostData (
  AValue : TOnNeedTacoInterfaceData );
begin
  fTacoInterface.NeedHostData := AValue;
end;

procedure TWsdlProject .setOnTacoAutorize (AValue : TNotifyEvent );
begin
  fTacoInterface.OnAuthorize := AValue;
end;

function TWsdlProject.ProcessInboundReply(aLogItem, rLogItem: TLog): String;
// rLogItem is already on display so locking needed on changing
var
  xMessage: String;
  xOperation: TWsdlOperation;
begin
  result := '';
  xMessage := '';
  xOperation := aLogItem.Operation;
  try
    aLogItem.Operation := TWsdlOperation.Create(rLogItem.Operation);
    try
      aLogItem.Mssg := rLogItem.Mssg;
      aLogItem.Operation.RequestStringToBindables (rLogItem.RequestBody);
      aLogItem.Operation.ReplyStringToBindables (aLogItem.InboundBody); // reply comes as a request
      if doValidateReplies then
      begin
        if not aLogItem.Operation.rpyBind.IsValueValid (xMessage) then
          aLogItem.ReplyValidateResult := xMessage;
        aLogItem.ReplyValidated := True;
      end;
      aLogItem.Operation.ExecuteAfter;
      CheckExpectedValues (aLogItem, aLogItem.Operation, doCheckExpectedValues);
      AcquireLogLock;
      try
        rLogItem.ReplyBody := aLogItem.InboundBody;
        rLogItem.InboundTimeStamp := aLogItem.InboundTimestamp;
        rLogItem.CorrelationId := aLogItem.Operation.CorrelationIdAsText ('; ');
        rLogItem.wsaCorrelated := True;
        rLogItem.HasUnexpectedValue := aLogItem.HasUnexpectedValue;
        rLogItem.ExpectedValuesChecked := aLogItem.ExpectedValuesChecked;
        toUpdateDisplayLogs.SaveLog('', rLogItem);
      finally
        ReleaseLogLock;
      end;
      result := '';
    finally
      aLogItem.Operation.Free;
    end;
  finally
    aLogItem.Operation := xOperation;
  end;
end;

procedure TWsdlProject.SMTPServerMailFrom(ASender: TIdSMTPServerContext;
  const AAddress: string; AParams: TStrings; var VAction: TIdMailFromReply);
begin
 VAction := mAccept;
end;

procedure TWsdlProject.SMTPServerMsgReceive(ASender: TIdSMTPServerContext;
  AMsg: TStream; var VAction: TIdDataReply);
var
  xLog: TLog;
begin
  try
    xLog := TLog.Create;
    try
      xLog.InboundTimeStamp := Now;
      xLog.TransportType := ttSmtp;
      with smtpParseMessageStreamAsXml( aMsg
                                      , xLog.RequestHeaders
                                      , xLog.CorrelationId
                                      ) do
      begin
        xLog.RequestBody := Text;
        Free;
      end;
    finally
      xLog.Stream := TMemoryStream.Create;
      aMsg.Position := 0;
      xLog.Stream.CopyFrom(aMsg, aMsg.Size);
      xLog.OutboundTimeStamp := Now;
      DisplayLog ('', xLog);
    end;
  except
    on e: Exception do
      LogServerMessage(format('Exception %s in SMTPServerMsgReceive. Exception is:"%s".', [e.ClassName, e.Message]), True,e);
  end;
end;

procedure TWsdlProject.SMTPServerRcptTo(ASender: TIdSMTPServerContext;
  const AAddress: string; AParams: TStrings; var VAction: TIdRCPToReply;
  var VForward: string);
begin
 VAction := rAddressOk;
end;

procedure TWsdlProject.SMTPServerReceived(ASender: TIdSMTPServerContext;
  var AReceived: string);
begin
 AReceived := '';
end;

procedure TWsdlProject.SMTPServerUserLogin(ASender: TIdSMTPServerContext;
  const AUsername, APassword: string; var VAuthenticated: Boolean);
begin
  VAuthenticated := True; // a friendly server
end;

procedure TWsdlProject.doCoverageReport (aReport: TSnapshot);
var
  xLogList: TLoglist;
  xCvrg: TXmlCvrg;
  x: Integer;
begin
  aReport.Status := rsUndefined;
  try
    xLogList := TLogList.Create;
    try
      for x := 0 to displayedSnapshots.Count - 1 do
        if displayedSnapshots.SnapshotItems[x].FileName <> '' then
          OpenMessagesLog (displayedSnapshots.SnapshotItems[x].FileName, True, False, xLogList);
      xCvrg := xLogList.PrepareCoverageReportAsXml ( allAliasses
                                                   , ignoreCoverageOn
                                                   );
      try
        if Assigned (xCvrg) then
        begin
          xCvrg.CalculateCoverage;
          if xCvrg.DisplayPercentage(False) = '100' then
            aReport.Status := rsOk
          else
            aReport.Status := rsNok
        end;
        aReport.Message := xCvrg.DisplayPercentage(False)
                         + '% ('
                         + xCvrg.DisplayCoverage(False)
                         + ')'
                         ;
      finally
        FreeAndNil (xCvrg);
      end;
    finally
      FreeAndNil (xLogList);
    end;
  except
    on e: Exception do
    begin
      aReport.Message := 'Exception: ' + e.Message;
      aReport.Status := rsException;
    end;
  end;
end;

procedure TWsdlProject.doRegressionReport (aReport: TSnapshot);
var
  xLogList, xRefLofList: TLoglist;
  xXml: TXml;
  df: String;
begin
  aReport.Status := rsUndefined;
  try
    xLogList := TLogList.Create;
    try
      OpenMessagesLog (aReport.FileName, True, False, xLogList);
      xRefLofList := TLogList.Create;
      try
        if not abortPressed then
          OpenMessagesLog (aReport.RefFileName, True, False, xRefLofList);
        if not abortPressed then
          xXml := logDifferencesAsXml ( xLogList
                                      , xRefLofList
                                      , aReport.RefFileName
                                      , CompareLogOrderBy
                                      , ignoreDifferencesOn
                                      , ignoreAddingOn
                                      , ignoreRemovingOn
                                      , ignoreOrderOn
                                      , regressionSortColumns
                                      );
        try
          if Assigned (xXml)
          and (not abortPressed) then with xXml do
          begin
            df := xXml.FindUQValue('logDifferences.Header.differencesFound');
            if df = 'true' then
              aReport.Status := rsNok;
            if df = 'false' then
              aReport.Status := rsOk;
          end;
        finally
          FreeAndNil (xXml);
        end;
      finally
        FreeAndNil (xLogList);
      end;
    finally
      FreeAndNil (xLogList);
    end;
  except
    on e: Exception do
    begin
      aReport.Message := 'Exception: ' + e.Message + ExceptionStackListString(e);
      aReport.Status := rsException;
    end;
  end;
end;

procedure TWsdlProject.HTTPProxyServerAfterCommandHandler(
  ASender: TIdCmdTCPServer; AContext: TIdContext);
var
  xLog: TLog;
  xMessage: String;
  xOperation: TWsdlOperation;
begin
  xMessage := '';
  xLog := AContext.Data as TLog;
  try
    xOperation := FindOperationOnRequest(xLog, xLog.httpDocument, xLog.RequestBody, true);
    if Assigned (xOperation) then
    begin
      try
        xLog.Operation := xOperation;
        while Assigned(xLog.Operation.Cloned) do
          xLog.Operation := xLog.Operation.Cloned;
        xLog.Mssg := xOperation.MessageBasedOnRequest;
        if doValidateRequests then
        begin
          if not xOperation.reqBind.IsValueValid (xMessage) then
            xLog.RequestValidateResult := xMessage;
          xLog.RequestValidated := True;
        end;
        if doValidateReplies then
        begin
          if not xOperation.rpyBind.IsValueValid (xMessage) then
            xLog.ReplyValidateResult := xMessage;
          xLog.ReplyValidated := True;
        end;
      finally
        xOperation.Free;
      end;
    end;
  finally
    DisplayLog ('', xLog);
  end;
end;

procedure TWsdlProject.HTTPProxyServerHTTPBeforeCommand(
  AContext: TIdHTTPProxyServerContext);
begin
  AContext.Data := TLog.Create;
  with AContext.Data as TLog do
  begin
    TransportType := ttHttp;
    StubAction := saForward;
    httpCommand := AContext.Command;
    httpDocument := AContext.Document;
    httpSoapAction := AContext.Headers.Values ['SOAPAction'];
  end;
end;

procedure TWsdlProject.HTTPProxyServerHTTPDocument(
  AContext: TIdHTTPProxyServerContext; var VStream: TStream);
  function _streamToString: String;
  var
    s: AnsiString;
  begin
    VStream.Position := 0;
    SetLength(s, VStream.Size);
    VStream.Read(s[1], VStream.Size);
    VStream.Position := 0;
    result := s;
  end;
begin
  with AContext.Data as TLog do
  begin
    if AContext.TransferSource = tsClient then
    begin
      InboundTimestamp := Now;
      RequestHeaders := AContext.Headers.Text;
      RequestBody := _streamToString;
      InboundBody := RequestBody;
    end;
    if AContext.TransferSource = tsServer then
    begin
      OutBoundTimeStamp := Now;
      ReplyHeaders := AContext.Headers.Text;
      ReplyBody := _streamToString;
      OutboundBody := ReplyBody;
    end;
  end;
end;

function TWsdlProject.MessagesRegressionReportAsXml(aReferenceFileName: String; aPromptUser: Boolean): TXml;
var
  xLogList: TLogList;
begin
  xLogList := TLogList.Create;
  try
    OpenMessagesLog (aReferenceFileName, True, aPromptUser, xLogList);
    result := logDifferencesAsXml ( displayedLogs
                                  , xLogList
                                  , aReferenceFileName
                                  , CompareLogOrderBy
                                  , ignoreDifferencesOn
                                  , ignoreAddingOn
                                  , ignoreRemovingOn
                                  , ignoreOrderOn
                                  , regressionSortColumns
                                  );
  finally
    xLogList.Clear;
    FreeAndNil(xLogList);
  end;
end;

procedure TWsdlProject.OpenMessagesLog(aString: String; aIsFileName, aPrompt: Boolean; aLogList: TLogList);
var
  xXml: TXml;
  x: Integer;
  xLog: TLog;
  xBodiesAsBase64: Boolean;
begin
  xXml :=TXml.Create;
  try
    if aIsFileName then
      xXml.LoadFromFile(aString, nil)
    else
      xXml.LoadFromString(aString, nil);
    if xXml.TagName <> 'LogIncrement' then
    begin
      if xXml.TagName <> 'WsdlStubCaseMessages' then
        raise Exception.Create('File does not contain saved ' + _progName + ' messages');
      if (xXml.Items.XmlValueByTag['wsdlStub'] <> projectFileName)
      and aPrompt
      then
        if not BooleanPromptDialog( 'wsdlStub from saved messages ('
                                  + xXml.Items.XmlValueByTag['wsdlStub']
                                  + ') is not the same as current ('
                                  + projectFileName
                                  + ')'
                                  + #$D#$A
                                  + 'Continue'
                                  ) then
          raise Exception.Create('Operation aborted');
    end;
    for x := 0 to xXml.Items.Count - 1 do
    begin
      with xXml.Items.XmlItems [x] do
      begin
        if TagName = 'refreshInfo' then
        begin
          refreshNr := Items.XmlIntegerByTagDef ['Index', -1];
          refreshCheck := Items.XmlValueByTag ['Check'];
        end;
        if TagName = 'RequestReply' then
        begin
          xLog := TLog.Create;
          xLog.CorrId := Items.XmlValueByTagDef ['Check', xLog.CorrId];
          try
            xLog.InboundTimeStamp := XmlToDateTime (Items.XmlValueByTag ['InboundTimeStamp']);
          except
            try
              xLog.InboundTimeStamp := XmlToDateTime (Items.XmlValueByTag ['Time']);
            except
              xLog.InboundTimeStamp := EncodeTime(0,0,0,0);
            end;
          end;
          try
            xLog.OutboundTimeStamp := XmlToDateTime (Items.XmlValueByTag ['OutboundTimeStamp']);
          except
            xLog.OutboundTimeStamp := EncodeTime(0,0,0,0);
          end;
          xBodiesAsBase64 := Items.XmlBooleanByTag['BodiesAsBase64'];
          xLog.DelayTimeMs := Items.XmlIntegerByTagDef['DelayTimeMs', 0];
          xLog.OperationCount := Items.XmlIntegerByTagDef['OperationCount', 0];
          xLog.TransportType := TTransportType (StrToIntDef (Items.XmlValueByTag ['TransportType'], 0));
          xLog.StubAction := TStubAction (StrToIntDef (Items.XmlValueByTag ['StubAction'], 0));
          xLog.CorrelationId := Items.XmlValueByTag ['CorrelationId'];
          xLog.ServiceName := Items.XmlValueByTag ['Service'];
          xLog.OperationName := Items.XmlValueByTag ['Operation'];
          xLog.Exception := Items.XmlValueByTag ['Error'];
          xLog.Remarks := Items.XmlValueByTag ['Remarks'];
          xLog.Notifications := Items.XmlValueByTag ['Notifications'];
          xLog.httpResponseCode := Items.XmlValueByTag ['httpResponseCode'];
          xLog.httpCommand := Items.XmlValueByTag ['httpCommand'];
          xLog.httpDocument := Items.XmlValueByTag ['httpDocument'];
          xLog.httpParams := Items.XmlValueByTag ['httpParams'];
          xLog.httpSoapAction := Items.XmlValueByTag ['httpSoapAction'];
          xLog.RequestHeaders := Items.XmlValueByTag ['HttpRequestHeaders'];
          xLog.RequestBody := Items.XmlValueByTag ['HttpRequestBody'];
          xLog.RequestBodyMiM := Items.XmlValueByTag ['HttpRequestBodyMiM'];
          xLog.ReplyBody := Items.XmlValueByTag ['HttpReplyBody'];
          xLog.ReplyBodyMiM := Items.XmlValueByTag ['HttpReplyBodyMiM'];
          if xBodiesAsBase64 then
          begin
            if xLog.RequestBody <> '' then
              xlog.RequestBody := base64.DecodeStringBase64(xlog.RequestBody);
            if xLog.RequestBodyMiM <> '' then
              xlog.RequestBodyMiM := base64.DecodeStringBase64(xlog.RequestBodyMiM);
            if xLog.ReplyBody <> '' then
              xlog.ReplyBody := base64.DecodeStringBase64(xlog.ReplyBody);
            if xLog.ReplyBodyMiM <> '' then
              xlog.ReplyBodyMiM := base64.DecodeStringBase64(xlog.ReplyBodyMiM);
          end;
          xLog.RequestValidated := Items.XmlBooleanByTag ['RequestValidated'];
          xLog.RequestValidateResult := Items.XmlValueByTag ['RequestValidateResult'];
          xLog.ReplyValidated := Items.XmlBooleanByTag ['ReplyValidated'];
          xLog.ReplyValidateResult := Items.XmlValueByTag ['ReplyValidateResult'];
          xLog.CorrId := Items.XmlValueByTag ['Check'];
          if xLog.ServiceName = '' then
            xLog.ServiceName := Items.XmlValueByTag ['ServiceName'];
          if xLog.OperationName = '' then
            xLog.OperationName := Items.XmlValueByTag ['OperationName'];
          xLog.CorrId := Items.XmlValueByTag ['Check'];
          try
            xLog.Operation := FindOperationOnRequest ( xLog
                                                     , ''
                                                     , xLog.RequestBody
                                                     , False
                                                     );
          except
          end;
          if not Assigned (xLog.Operation) then
          try
            xLog.Operation := FindOperationOnReply(xLog.ReplyBody);
            if Assigned (xLog.Operation) then
              xLog.OperationName:=xLog.Operation.reqTagName;
          except
          end;
          if Assigned (xLog.Operation) then
          begin
            xLog.Mssg := xLog.Operation.MessageBasedOnRequest;
            xLog.Operation.RequestStringToBindables(xLog.RequestBody);
            xLog.Operation.ReplyStringToBindables(xLog.ReplyBody);
            xLog.CorrelationId := xLog.Operation.CorrelationIdAsText('; ');
          end;
          LogFilter.Execute (xLog);
          aLogList.SaveLog ('', xLog);
        end;
      end;
    end; // for each xml
  finally
    FreeAndNil (xXml);
  end;
end;

procedure TWsdlProject.SaveLogs(aFileName: String);
begin
  with TStringList.Create do
  try
    AcquireLogLock;
    try
      Text := displayedLogs.LogsAsString (projectFileName);
    finally
      ReleaseLogLock;
    end;
    SaveToFile(aFileName);
  finally
    free;
  end;
end;

procedure TWsdlProject.SaveSnapshots(aName: String);
var
  x: Integer;
begin
  if (CurrentFolder = '') then
    raise Exception.Create('SaveSnapshots: config (ProjectOptions.General.projectFolders) invalid');
  AcquireLogLock;
  try
    with displayedSnapshots.AsXml do
    try
      for x := 0 to toDisplaySnapshots.Count - 1 do
        AddXml (toDisplaySnapshots.SnapshotItems[x].AsXml);
      SaveStringToFile ( ReportsFolder
                       + DirectorySeparator
                       + aName
                       + '.xml'
                       , Text
                       );
    finally
      Free;
    end;
  finally
    ReleaseLogLock;
  end;
end;

procedure TWsdlProject.DisplayLog(aString: String; aLog: TLog);
begin
  if not Assigned (aLog) then Exit;
  if (not doDisplayLog)
  or (aLog.doSuppressLog)
  then
  begin
    if not aLog.Claimed then
      aLog.Free;
    Exit;
  end;
  AcquireLogLock;
  try
    toDisplayLogs.SaveLog (aString, aLog);
    if Assigned (aLog.Operation) then
      while Assigned (aLog.Operation.Cloned) do
        aLog.Operation := aLog.Operation.Cloned;
  finally
    ReleaseLogLock;
  end;
end;

procedure TWsdlProject.DisplayReport (aString: String; aReport: TSnapshot);
begin
    if not Assigned (aReport) then Exit;
    AcquireLogLock;
    try
      toDisplaySnapshots.SaveObject (aString, aReport);
    finally
      ReleaseLogLock;
    end;
end;

{$ifndef FPC}
procedure TWsdlProject.ADOConnectionWillConnect(Connection: TADOConnection;
  var ConnectionString, UserID, Password: WideString;
  var ConnectOptions: TConnectOption; var EventStatus: TEventStatus);
begin
  ConnectionString := ReplaceStrings( DbsConnectionString
                                    , '%pwd%'
                                    , DbsPassword
                                    , false
                                    , false
                                    );
end;
{$endif}

procedure TWsdlProject.ScriptsClear;
begin
  Scripts.Items.Clear;
end;

procedure TWsdlProject.ScriptExecute(aScript: TObject);
begin
  if not IsActive then
    raise Exception.Create(Format('%s not active', [_progName]));
  with CreateScriptOperation(TXml(aScript)) do
  try
    Wsdl := TWsdl.Create(EnvVars, 1, 1, True);
    try
      if PreparedBefore then
      try
        ExecuteBefore;
      except
        on e: Exception do
          LogServerMessage(e.Message, True, e);
      end;
    finally
      Wsdl.Free;
    end;
  finally
    FreeAndNil(Data);
    Free;
  end;
end;

function TWsdlProject.CreateSnapshot (aName, aFileName, aRefFileName: String; aDoSave, aDoRun: Boolean): TSnapshot;
var
  x: Integer;
begin
  result := nil;
  AcquireLogLock;
  try
    result := TRegressionSnapshot.Create( aName
                                        , ExpandRelativeFileName(projectFileName, aFileName)
                                        , ExpandRelativeFileName(projectFileName, aRefFileName)
                                        );
    result.OnReport := doRegressionReport;
    toDisplaySnapshots.AddObject('', result);
    if aDoSave then
    begin
      with TLogList.Create do
      try
        for x := 0 to displayedLogs.Count - 1 do
          if not displayedLogs.LogItems[x].onSnapshot then
            SaveLog('', displayedLogs.LogItems[x]);
        for x := 0 to toDisplayLogs.Count - 1 do
          if not toDisplayLogs.LogItems[x].onSnapshot then
            SaveLog('', toDisplayLogs.LogItems[x]);
        SaveStringToFile(result.FileName, LogsAsString (projectFileName));
        for x := 0 to Count - 1 do
          LogItems[x].onSnapshot := True;
        Clear;
      finally
        Free;
      end;
    end;
  finally
    ReleaseLogLock;
  end;
  if aDoRun
  and Assigned (result) then
    result.doReport;
end;

procedure TWsdlProject.CreateSummaryReport (aName: String);
var
  xList: TSnapshotList;
  x: Integer;
begin
  if (CurrentFolder = '') then
    raise Exception.Create('CreateSummaryReport: config (ProjectOptions.General.projectFolders) invalid');
  xList := TSnapshotList.Create;
  try
    AcquireLogLock;
    try
      for x := 0 to displayedSnapshots.Count - 1 do
        xList.AddObject('', displayedSnapshots.SnapshotItems[x]);
    finally
      ReleaseLogLock;
    end;
    SaveStringToFile ( ReportsFolder + DirectorySeparator + aName + '.html'
                     , htmlReportTestSummary(Self, xList)
                     );
    xList.Clear;
  finally
    xList.Free;
  end;
end;

procedure TWsdlProject.CreateCoverageReport (aDoRun: Boolean);
var
  xReport: TCoverageReport;
begin
  xReport := TCoverageReport.Create;
  xReport.OnReport := doCoverageReport;
  if aDoRun then
    xReport.doReport;
  AcquireLogLock;
  try
    toDisplaySnapshots.AddObject('', xReport);
  finally
    ReleaseLogLock;
  end;
end;

function TWsdlProject.FindScript (aName : String ): TXml ;
var
  x: Integer;
begin
  result := nil;
  for x := 0 to Scripts.Items.Count - 1 do
    if (Scripts.Items.XmlItems[x].Name = 'Script')
    and (Scripts.Items.XmlItems[x].Items.XmlValueByTag['Name'] = aName) then
    begin
      result := Scripts.Items.XmlItems[x];
      exit;
    end;
end;

procedure TWsdlProject.ExecuteAllOperationRequests(aOperation: TWsdlOperation);
var
  x: Integer;
begin
  for x := 0 to aOperation.Messages.Count - 1 do
    if not aOperation.Messages.Messages[x].Disabled then
      SendMessage (aOperation, aOperation.Messages.Messages[x], '');
end;

procedure TWsdlProject.Clean;
begin
  AcquireLock;
  try
    allOperations.Clean;
    Scripts.Items.Clear;
  finally
    ReleaseLock;
  end;
end;

procedure TWsdlProject .TacoPingPong ;
begin
  try
    fTacoInterface.PingPong;
  except
    fTacoInterface.Disconnect;
  end;
end;

procedure TWsdlProject.Clear;
var
  x: Integer;
begin
  projectProperties.Clear;
  DatabaseConnectionSpecificationXml.Items.Clear;
  Scripts.Items.Clear;
  displayedLogs.Clear;
  archiveLogs.Clear;
  displayedSnapshots.Clear;
  doUseMq := False;
  Listeners.Clear;
  mqGetThreads.Clear;
  doValidateRequests := False;
  doValidateReplies := False;
  doCheckExpectedValues := False;
  _WsdlDisableOnCorrelate := False;
  ignoreDifferencesOn.Clear;
  ignoreAddingOn.Clear;
  ignoreRemovingOn.Clear;
  with ignoreOrderOn do
  begin
    for x := 0 to Count - 1 do
      if Assigned (Objects[x]) then
        Objects[x].Free;
    Clear;
  end;
  regressionSortColumns.Clear;
  ignoreCoverageOn.Clear;
  DisplayedLogColumns.Clear;
  AsynchRpyLogs.Clear;
  EnvironmentListClear;
  SchemaLocations.Clear;
  while Wsdls.Count > 0 do
  begin
    if (Wsdls.Objects[0] <> FreeFormatWsdl)
    and (Wsdls.Objects[0] <> CobolWsdl)
    and (Wsdls.Objects[0] <> XsdWsdl)
    and (Wsdls.Objects[0] <> SwiftMtWsdl) then
      try Wsdls.Objects[0].Free; except end; // there is a project that fails at this point, not a clue yet why
    Wsdls.Delete(0);
  end;
  wsdls.Clear;
  wsdlNames.Clear;
  ScriptsClear;
  DisplayedLogColumns.Clear;
  xsdElementsWhenRepeatable := 1;
  stubRead := False;
  projectFileName := '';
  xmlio.PathPrefixes.Clear;
  InitSpecialWsdls;
end;

function TWsdlProject.ReactivateCommand: String;
begin
  result := '';
  raise Exception.Create('TWsdlProject.ReactivateCommand: String;  should be overloaded');
end;

function TWsdlProject.RestartCommand: String;
begin
  result := '';
  raise Exception.Create('TWsdlProject.RestartCommand: String;  should be overloaded');
end;

procedure TWsdlProject.ReleaseLogLock;
begin
  fLogLock.Release;
end;

function TWsdlProject.ReloadDesignCommand: String;
begin
  result := '';
  raise Exception.Create('TWsdlProject.ReloadDesignCommand: String;  should be overloaded');
end;

procedure TWsdlProject.InitSpecialWsdls;
begin
  FreeAndNil (FreeFormatWsdl);
  FreeFormatService := TWsdlService.Create;
  with FreeFormatService do
  begin
    Name := '_FreeFormat';
    DescriptionType := ipmDTFreeFormat;
  end;
  FreeFormatWsdl := TWsdl.Create(EnvVars, 1, 1, False);
  with FreeFormatWsdl do
  begin
    Name := '_Freeformat';
    isSoapService := False;
    Services.AddObject(FreeFormatService.Name, FreeFormatService);
  end;
  FreeAndNil(CobolWsdl);
  CobolWsdl := TWsdl.Create(EnvVars, 1, 1, False);
  with CobolWsdl do
  begin
    Name := '_Cobol';
    isSoapService := False;
    Services.Add(Name);
    Services.Objects[0] := TWsdlService.Create;
    Services.Services[0].Name := Name;
    Services.Services[0].DescriptionType := ipmDTCobol;
  end;
  FreeAndNil(XsdWsdl);
  XsdWsdl := TWsdl.Create(EnvVars, 1, 1, False);
  with XsdWsdl do
  begin
    Name := '_Xsd';
    isSoapService := False;
    Services.Add(Name);
    Services.Objects[0] := TWsdlService.Create;
    Services.Services[0].Name := Name;
    Services.Services[0].DescriptionType := ipmDTXsd;
  end;
  FreeAndNil(SwiftMtWsdl);
  SwiftMtWsdl := TWsdl.Create(EnvVars, 1, 1, False);
  with SwiftMtWsdl do
  begin
    Name := '_SwiftMT';
    isSoapService := False;
    Services.Add(Name);
    Services.Objects[0] := TWsdlService.Create;
    Services.Services[0].Name := Name;
    Services.Services[0].DescriptionType := ipmDTSwiftMT;
  end;
end;

procedure TWsdlProject.DatabaseConnectionSpecificationFromXml ;
var
  hXml: TXml;
begin
  _WsdlDbsConnector.Connected := False;
  _WsdlDbsConnector.LoginPrompt := False;
  _WsdlDbsEnabled := False;
  DbsType := '';
  DbsDatabaseName := '';
  DbsHostName:='';
  DbsParams := '';
  DbsUserName:='';
  DbsPassword := '';
  hXml := TXml.Create;
  try
    hXml.CopyDownLine(DatabaseConnectionSpecificationXml, True);
    hXml.ResolveAliasses(projectProperties);
    _WsdlDbsEnabled := hXml.Items.XmlCheckedBooleanByTagDef['Enabled', _WsdlDbsEnabled];
    DbsType := hXml.Items.XmlCheckedValueByTagDef['Type', DbsType];
    DbsDatabaseName := hXml.Items.XmlCheckedValueByTagDef['DatabaseName', DbsDatabaseName];
    DbsHostName := hXml.Items.XmlCheckedValueByTagDef['HostName', DbsHostName];
    DbsParams := hXml.Items.XmlCheckedValueByTagDef['Params', DbsParams];
    DbsUserName := hXml.Items.XmlCheckedValueByTagDef['UserName', DbsUserName];
    DbsPassword := xmlz.DecryptString(hXml.Items.XmlCheckedValueByTag['Password']);
    DbsConnectionString := hXml.Items.XmlCheckedValueByTagDef['ConnectionString', DbsConnectionString]; // to be able to create ado version project
  finally
    hXml.Free;
  end;
  with _WsdlDbsConnector do
  begin
    ConnectorType := DbsType;
    DatabaseName := DbsDatabaseName;
    HostName := DbsHostName;
    Params.Text := ReplaceStrings( DbsParams
                                 , '%pwd%'
                                 , DbsPassword
                                 , false
                                 , false
                                 );
    UserName := DbsUserName;
    Password := DbsPassword;
  end;
end;

procedure TWsdlProject .UpdateOperationAliasses ;
var
  o: Integer;
begin
  allAliasses.ClearListOnly;
  for o := 0 to allOperations.Count - 1 do with allOperations do
    allAliasses.AddObject(Operations[o].Alias, Operations[o]);
end;

procedure TWsdlProject .WriteStringToStream (aString : String ;
  aStream : TMemoryStream );
begin
  aStream.Position := 0;
  aStream.Write(Pointer(aString)^, Length (aString));
  aStream.Position := 0;
end;

// TWsdlProject
initialization
  _WsdlAddRemark := AddRemark;
  _WsdlExecuteScript := ExecuteScript;
  _WsdlRequestOperation := RequestOperation;
  _WsdlNewDesignMessage := NewDesignMessage;
  _WsdlRequestAsText := RequestAsText;
  _WsdlReplyAsText := ReplyAsText;
  _WsdlClearLogs := ClearLogs;
  _WsdlClearSnapshots := ClearSnapshots;
  _WsdlCreateSnapshot := CreateSnapshot;
  _WsdlCreateSummaryReport := CreateSummaryReport;
  _WsdlCreateCoverageReport := CreateCoverageReport;
  _WsdlSendOperationRequest := SendOperationRequest;
  _WsdlSendOperationRequestLater := SendOperationRequestLater;
  _WsdlRefuseHttpConnections := doRefuseHttpConnections;
  _WsdlSaveSnapshots := SaveSnapshots;
  _ProgName := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(ParamStr(0)), '');
  IntrospectIniXml;


finalization
  FreeAndNil (webserviceWsdl);
  FreeAndNil (webserviceXsdDescr);
  FreeAndNil (wsaXsdDescr);
  FreeAndNil (swiftMTXsdDescr);
  FreeAndNil (_WsdlRtiXml);
end.

