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
  sqldb,
{$ENDIF}
  Classes
   , ParserClasses
   , Xmlz
   , Xsdz
   , Express
   , Bind
   , Ipmz
   , IpmTypes
   , igGlobals
{$ifndef FPC}
  , jclDebug
{$endif}
  , IdComponent
  , IdTCPServer
  , IdGlobal
  , IdBaseComponent
  , IdIntercept
  , IdIOHandlerSocket
  , IdCustomHTTPServer
  , IdSocketHandle
  , IdTCPConnection
  , IdTCPClient
  , IdCmdTCPServer
  , IdHTTPProxyServer
  , IdServerIOHandler
  , IdCustomTCPServer
  , IdHTTP
  , IdSync
  , IdSSL
  , IdExplicitTLSClientServerBase
  , IdMessage
  , IdMessageParts
  , IdAttachment
  , IdText
  , IdEMailAddress
  , IdHeaderList
  , IdZLibCompressorBase
  , IdCompressorZLib
  , IdZLibHeaders
  , IdHTTPHeaderInfo
  , IdZLib
  , IdStack
  , IdSMTP
  , IdURI
  , IdStreamVCL
  , IdSSLOpenSSL
  , IdContext
  , IdCommandHandlers
  , Wsdlz
  , SwiftUnit
  , SysUtils
  , MqInterface
  , MqApi
  , StompInterface
  , StompClient
  , StompTypes
{$ifdef windows}
  , TacoInterface
{$endif}
  , IdPOP3Server
  , IdReplyPOP3
  , IdSMTPServer
  , IdHTTPServer
  , Listenerz
  , Forms
  , Dialogs
  , Controls
  , Buttons , FileUtil
  , Logz
  , ExceptionLogz
  , SyncObjs
  ;

type TCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);
type TProcedure = procedure of Object;
type TProcedureB = procedure (arg: Boolean) of Object;
type TProcedureS = procedure (arg: String) of Object;
type TProcedureXX = procedure (arg, arg2: Extended) of Object;
type TProcedureOperation = procedure (arg: TWsdlOperation) of Object;
type TProcedureOperationS = procedure (arg: TWsdlOperation; arg2: String) of Object;
type TProcedureObject = procedure (arg: TObject) of Object;
type TOnFoundErrorInBufferEvent = procedure (aErrorString: String; aObject: TObject) of Object;
type TOnEvent = procedure of Object;
type TOnNotify = procedure (aString: String) of Object;
type TOnLogEvent = procedure (aLog: TLog) of Object;
type TOnStringEvent = procedure (const Msg: String; aException: Boolean; E: Exception) of Object;
type TBooleanFunction = function: Boolean of Object;
type TStringFunction = function: String of Object;
type TStringFunctionBoolean = function (arg: Boolean): String of Object;


type
  TWsdlProject = class (TComponent)
  private
    fIsActive: Boolean;
    fAbortPressed: Boolean;
    fLogLock: TCriticalSection;
    function GetAbortPressed: Boolean;
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
    procedure HTTPServerCommandGetGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HTTPServerCommandTrace(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
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
    function ProcessInboundReply(aLogItem, rLogItem: TLog): String;
    procedure HttpWebPageServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    function MessagesRegressionReportAsXml(aReferenceFileName: String): TXml;
    procedure ExecuteAllOperationRequests(aOperation: TWsdlOperation);
    function httpRequestStreamToString(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): String;
    procedure SetAbortPressed(const Value: Boolean);
    procedure InitSpecialWsdls;
  public
    ProgressMax, ProgressPos: Integer;
    doCloneOperations: Boolean;
    DbsDatabaseName, DbsType, DbsHostName, DbsParams, DbsUserName, DbsPassword, DbsConnectionString: String;
    FreeFormatWsdl, XsdWsdl, CobolWsdl, SwiftMtWsdl: TWsdl;
    FreeFormatService: TWsdlService;
    DebugOperation: TWsdlOperation;
    Wsdls, wsdlNames: TStringList;
    Scripts, DisplayedLogColumns: TStringList;
    projectFileName, LicenseDbName: String;
    displayedExceptions, toDisplayExceptions: TExceptionLogList;
    displayedLogs, toDisplayLogs, toUpdateDisplayLogs, archiveLogs, AsynchRpyLogs: TLogList;
    displayedLogsmaxEntries: Integer;
    CompareLogOrderBy: TCompareLogOrderBy;
    ShowLogCobolStyle: TShowLogCobolStyle;
    unknownOperation: TWsdlOperation;
    LogFilter: TLogFilter;
    refreshNr: Integer;
    refreshCheck: String;
    scriptErrorCount: Integer;
    EnvironmentList: TStringList;
    StubChanged, StubRead, Licensed: Boolean;
    doUseMQ: Boolean;
    NumberOfActiveMqs: Integer;
    mqUse: TMQUse;
    mqMaxWorkingThreads: Integer;
    mqCurWorkingThreads: Integer;
    MqInterface: TMqInterface;
    StompInterface: TStompInterface;
    mqGetThreads: TStringList;
    Listeners: TListeners;
    doValidateRequests, doValidateReplies, doCheckExpectedValues: Boolean;
    doLoadFromMasterOnStartUp: Boolean;
    isMasterModeEnabled: Boolean;
    isBusy: Boolean;
    ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn: TStringList;
    ignoreCoverageOn: TStringList;
    notStubbedExceptionMessage: String;
    FoundErrorInBuffer : TOnFoundErrorInBufferEvent;
    OnDebugOperationEvent: TOnEvent;
    OnBusy, OnReady: TOnEvent;
    Notify: TOnNotify;
    LogServerMessage: TOnStringEvent;
    doViaProxyServer: Boolean;
    ViaProxyServer: String;
    ViaProxyPort: Integer;
    HTTPServer, HttpServerSSL, HttpServerBmtp, HttpWebPageServer: TIdHTTPServer;
    HTTPProxyServer: TIdHTTPProxyServer;
    SMTPServer, SMTPServerSSL: TIdSMTPServer;
    POP3Server: TIdPOP3Server;
    SMTPOpenSSL: TIdServerIOHandlerSSLOpenSSL;
    ServerOpenSSL: TIdServerIOHandlerSSLOpenSSL;
    OnRestartEvent: TStringFunction;
    OnReactivateEvent: TStringFunction;
    OnActivateEvent: TProcedureB;
    OnOpenProjectEvent: TProcedureS;
    OnClearLogEvent: TStringFunctionBoolean;
    OnQuitEvent: TStringFunctionBoolean;
    OnReloadDesignEvent: TStringFunction;
    PublishDescriptions: Boolean;
    OperationsWithEndpointOnly: Boolean;
    SaveRelativeFileNames: Boolean;
    FocusOperationName: String;
    FocusMessageIndex: Integer;
    procedure AcquireLogLock;
    procedure ReleaseLogLock;
    procedure SaveLog (aString: String; aLog: TLog);
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
    function CreateScriptOperation (aString: String): TWsdlOperation;
    procedure ScriptExecute(aScript: String);
    procedure ScriptExecuteText (aText: String);
    procedure ScriptsClear;
    procedure DefaultDisplayMessageData;
    function ReactivateCommand: String;
    function RestartCommand: String;
    function QuitCommand(aDoRaiseExceptions: Boolean): String;
    function ReloadDesignCommand: String;
    function ClearLogCommand(aDoRaiseExceptions: Boolean): String;
    procedure OpenMessagesLog (aString: String; aIsFileName: Boolean; aLogList: TLogList);
    procedure IgnoreDataChanged(Sender: TObject);
    procedure EnvironmentListClear;
    procedure mqOnNewThread ( Sender: TObject);
    procedure mqStubMessage ( Sender: TObject
                            ; aHeader, aBody: String
                            ; aRfhHeader: AnsiString
                            ; MsgType: MQLONG
                            ; MsgDesc: MQMD
                            ; MqReturnCode: String
                            );
    procedure CheckExpectedValues(aLog: TLog; aOperation: TWsdlOperation; aDoCheck: Boolean);
    procedure UpdateMessageRow (aOperation: TWsdlOperation; aMessage: TWsdlMessage);
    procedure DelayMS (aDelayMS: Integer);
    procedure CreateLogReply (aLog: TLog; var aProcessed: Boolean; aIsActive: Boolean);
    procedure Clean;
    function ProjectDesignAsString (aMainFileName: String): String;
{}
    function ExceptionStackListString(E: Exception): String;
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
    procedure CreateLogReplyPostProcess (aLogItem: TLog; aOperation: TWsdlOperation);
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
    function CreateReply ( aLog: TLog
                         ; aDocument, aRequest: String
                         ; var aOperation: TWsdlOperation
                         ; var aReply: TWsdlMessage
                         ; var aCorrelationId: String
                         ; var isAsynchronous: Boolean
                         ; aIsActive: Boolean
                         ): String;
    function ProjectLogOptionsAsXml: TXml;
    function ProjectOptionsAsXml: TXml;
    function ProjectOptionsLogDisplayedColumnsAsXml: TXml;
    function BooleanPromptDialog (aPrompt: String): Boolean;
    function WsdlOpenFile (aName: String; aElementsWhenRepeatable: Integer): TWsdl;
    procedure RefuseHttpConnectionsThreaded (aLater, aTime: Extended);
    procedure SaveMessagesLog (aFileName: String);
    procedure UpdateReplyColumns (aOperation: TWsdlOperation);
    procedure ProjectOptionsLogDisplayedColumnsFromXml(aXml: TXml);
    procedure ProjectLogOptionsFromXml(aXml: TXml);
    procedure ProjectOptionsFromXml(aXml: TXml);
    procedure ProjectOptions36FromXml (aXml: TXml);
    procedure HaveStompFrame (aStompInterface: TStompInterface; aQueue: String; aFrame: IStompFrame);
    procedure ProjectDesignFromString (aString, aMainFileName: String);
    procedure PrepareAllOperations (aLogServerException: TOnStringEvent);
    procedure Activate (aActive: Boolean);
    procedure Clear;
    procedure InitMasterServer;
    {$IFnDEF FPC}
    procedure ADOConnectionWillConnect(Connection: TADOConnection;
      var ConnectionString, UserID, Password: WideString;
      var ConnectOptions: TConnectOption; var EventStatus: TEventStatus);
    {$ENDIF}
    property IsActive: Boolean read fIsActive;
    property abortPressed: Boolean read fAbortPressed write SetAbortPressed;
    constructor Create;
    destructor Destroy; override;
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
    fString: String;
    fExtended, fExtended2: Extended;
    fOperation: TWsdlOperation;
    fObject: TObject;
  protected
    procedure Execute; override;
  public
    constructor Create ( aSuspended: Boolean
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedure
                       ); overload;
    constructor Create ( aSuspended: Boolean
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedureS
                       ; aString: String
                       ); overload;
    constructor Create ( aSuspended: Boolean
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedureXX
                       ; aExtended, aExtended2: Extended
                       ); overload;
    constructor Create ( aSuspended: Boolean
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedureOperation
                       ; aOperation: TWsdlOperation
                       ); overload;
    constructor Create ( aSuspended: Boolean
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedureObject
                       ; aObject: TObject
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
  _ProgName: String;
  BetaMode: Boolean;
  webserviceWsdlFileName: String;
    indexHtmlFileName: String;
    indexWsdlsHtmlFileName: String;
    webserviceXsdFileName: String;
    wsaXsdFileName: String;
    mqPutHeaderEditAllowedFileName: String;
    stompPutHeaderEditAllowedFileName: String;
    licenseDatabaseName, licenseOdbcDriver: String;
    MasterPortNumber: Integer;
    wsaXsdDescr: TXsdDescr;
    swiftMTXsdDescr: TXsdDescr;
    optionsXsd: TXsd;
    endpointConfigXsd: TXsd;
    webserviceXsdDescr: TXsdDescr;
    webserviceWsdl: TWsdl;
    OperationDefsXsd: TXsd;
    projectOptionsXsd: TXsd;
    serviceOptionsXsd: TXsd;
    listenersConfigXsd: TXsd;
    operationOptionsXsd: TXsd;

implementation

uses OpenWsdlUnit
   , FormIniFilez
   , StrUtils
   , wsdlStubHtmlUnit
   , SchemaLocationz
   , smtpInterface
   , RegExpr
   , jwbBase64
   {$ifdef windows}
   , ActiveX
   {$endif}
   , xmlUtilz
   , wrdFunctionz
   ;

procedure AddRemark(aOperation: TObject; aString: String);
begin
  if not Assigned (aOperation)
  or not (aOperation is TWsdlOperation)
  or not Assigned ((aOperation as TWsdlOperation).Data) then
    raise Exception.Create('aOperation.Data not assigned; intentention was to remark: ' + aString);
  with (aOperation as TWsdlOperation).Data as TLog do
  begin
    if Remarks = '' then
      Remarks := aString
    else
      Remarks := Remarks + CRLF + aString;
  end;
end;

procedure RequestOperation(aContext: TObject; xOperationName: String);
var
  f: Integer;
  xProject: TWsdlProject;
  xOperation: TWsdlOperation;
begin
  xProject := nil; //candidate context
  xOperation := nil; //candidate context
  if aContext is TWsdlOperation then with aContext as TWsdlOperation do
  begin
    xProject := Owner as TWsdlProject;
    if invokeList.Find(xOperationName, f)then
    begin
      xOperation := invokeList.Operations[f];
      if not Assigned (xOperation) then
        Raise Exception.CreateFmt ('RequestOperation: %s in list but Operation not assigned(?): \n%s', [xOperationName, invokeList.Text]);
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
      if allOperations.Find(xOperationName, f)then
      begin
        xOperation := allOperations.Operations[f];
        try
          xProject.SendMessage (xOperation, nil, '');
        except
        end;
      end;
    end;
  end;
  if not Assigned (xProject)
  or not Assigned (xOperation) then
   raise Exception.Create(Format ('RequestOperation: Operation ''%s'' not found', [xOperationName]));
end;

procedure GetDefaultRequestData(aOperation: String);
var
  f: Integer;
begin
  if not allOperations.Find(aOperation, f) then
    raise Exception.Create(Format ('GetDefaultMessageData: Operation %s not found', [aOperation]));
  with allOperations.Operations[f] do
  begin
    if StubAction <> saRequest then
      raise Exception.Create(Format ('GetDefaultMessageData: Operation %s, only allowed on Operations with action = Request', [aOperation]));
    ReqBindablesFromWsdlMessage(allOperations.Operations[f].Messages.Messages[0]);
  end;
end;

procedure PutDefaultRequestData(aOperation: String);
var
  f: Integer;
begin
  if not allOperations.Find(aOperation, f) then
    raise Exception.Create(Format ('GetDefaultMessageData: Operation %s not found', [aOperation]));
  with allOperations.Operations[f] do
  begin
{}{
    if StubAction <> saRequest then
      raise Exception.Create(Format ('GetDefaultMessageData: Operation %s, only allowed on Operations with action = Request', [aOperation]));
{}
    ReqBindablesToWsdlMessage(allOperations.Operations[f].Messages.Messages[0]);
  end;
end;

procedure SendOperationRequest(aOperation, aCorrelation: String);
var
  x, f: Integer;
  xOperation: TWsdlOperation;
  xRequest: TWsdlMessage;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    ExplodeStr (aCorrelation, ';', sl);
//  with wsdlStubForm do
    begin
      if allOperations.Find(aOperation, f) then
      begin
        xOperation := allOperations.Operations[f];
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
  x, f: Integer;
  xOperation: TWsdlOperation;
  xRequest: TWsdlMessage;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    ExplodeStr (aCorrelation, ';', sl);
//  with wsdlStubForm do
    begin
      if allOperations.Find(aOperation, f) then
      begin
        xOperation := allOperations.Operations[f];
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

constructor TProcedureThread.Create(aSuspended: Boolean; aProject: TWsdlProject; aProcedure: TProcedure);
begin
  inherited Create (aSuspended);
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedure := aProcedure;
end;

constructor TProcedureThread.Create(aSuspended: Boolean; aProject: TWsdlProject; aProcedure: TProcedureS;
  aString: String);
begin
  inherited Create (aSuspended);
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedureString := aProcedure;
  fString := aString;
end;

constructor TProcedureThread.Create(aSuspended: Boolean; aProject: TWsdlProject; aProcedure: TProcedureOperation;
  aOperation: TWsdlOperation);
begin
  inherited Create (aSuspended);
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedureOperation := aProcedure;
  fOperation := aOperation;
end;

constructor TProcedureThread.Create(aSuspended: Boolean; aProject: TWsdlProject;
  aProcedure: TProcedureObject; aObject: TObject);
begin
  inherited Create (aSuspended);
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedureObject := aProcedure;
  fObject := aObject;
end;

constructor TProcedureThread.Create(aSuspended: Boolean; aProject: TWsdlProject;
  aProcedure: TProcedureXX; aExtended, aExtended2: Extended);
begin
  inherited Create (aSuspended);
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedureXX := aProcedure;
  fExtended := aExtended;
  fExtended2 := aExtended2;
end;

procedure TProcedureThread.Execute;
begin
  if Assigned (fProject.OnBusy) then
    Synchronize(fProject.OnBusy);
  try
    if Assigned (fProcedure) then fProcedure;
    if Assigned (fProcedureString) then fProcedureString (fString);
    if Assigned (fProcedureXX) then fProcedureXX (fExtended, fExtended2);
    if Assigned (fProcedureOperation) then fProcedureOperation (fOperation);
    if Assigned (fProcedureObject) then fProcedureObject (fObject);
  finally
    if Assigned (fProject.OnReady) then
      Synchronize(fProject.OnReady);
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
  s, xIniFileName: String;
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
    indexHtmlFileName := iniXml.Items.XmlValueByTag ['indexHtml'];
    indexWsdlsHtmlFileName  := iniXml.Items.XmlValueByTag ['indexWsdlsHtml'];
    webserviceXsdFileName := iniXml.Items.XmlValueByTag ['wsdlStubXsd'];
    wsaXsdFileName := iniXml.Items.XmlValueByTag ['wsaXsd'];
    _swiftMTXsdFileName := iniXml.Items.XmlValueByTag ['swiftMTXsd'];
    mqPutHeaderEditAllowedFileName := iniXml.Items.XmlValueByTag ['mqPutHeaderEditAllowed'];
    stompPutHeaderEditAllowedFileName := iniXml.Items.XmlValueByTag ['stompPutHeaderEditAllowed'];
    if Assigned (iniXml.ItemByTag['licenseDatabase']) then with iniXml.ItemByTag['licenseDatabase'].Items do
    begin
      licenseOdbcDriver := XmlValueByTagDef['OdbcDriver', 'Microsoft Access Driver'];
      licenseDatabaseName := XmlValueByTagDef['DatabaseName', ''];
    end;
    MasterPortNumber := iniXml.Items.XmlIntegerByTagDef ['commandPort', 3738];
    xsdElementsWhenRepeatable := defaultXsdElementsWhenRepeatable;
    xsdMaxDepthBillOfMaterials := defaultXsdMaxDepthBillOfMaterials;
    xsdMaxDepthXmlGen := defaultXsdMaxDepthXmlGen;

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

    if webserviceXsdFileName <> '' then
    begin
      webserviceXsdFileName := ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), webserviceXsdFileName);
      webserviceXsdDescr := TXsdDescr.Create(1);
      try
        webserviceXsdDescr.LoadXsdFromString (_Prep ( webserviceXsdFileName
                                                    , ReadStringFromFile(webserviceXsdFileName)
                                                    )
                                             , nil
                                             );
      except
        raise Exception.Create (_progName + ' could not parse ' + webserviceXsdFileName);
      end;
    end;
    if not Assigned (webserviceXsdDescr) then
      raise exception.Create('No ' + _progName + ' webservice xsd assigned');

    with webserviceXsdDescr.TypeDef.ElementDefs do
    begin
      _WsdlRtiXsd := XsdByName['rti'];
      optionsXsd := XsdByName['wsdlStubOptions'];
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

  {}
    if webserviceWsdlFileName <> '' then
    begin
      webserviceWsdlFileName := ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), webserviceWsdlFileName);
      webserviceWsdl := TWsdl.Create(-1, 1, False);
      webserviceWsdl.LoadFromSchemaFile(webserviceWsdlFileName, nil);
    end;
    if not Assigned (webserviceWsdl) then
      raise exception.Create('No ' + _progName + ' webservice wsdl read');
      {}
  finally
    iniXml.Free;
  end;
end;

constructor TWsdlProject.Create;
begin
  {$ifndef FPC}
  jclDebug.JclStartExceptionTracking;
  {$endif}
  doCloneOperations := True;
  OnRestartEvent := RestartCommand;
  OnReactivateEvent := ReactivateCommand;
  OnClearLogEvent := ClearLogCommand;
  OnQuitEvent := QuitCommand;
  OnReloadDesignEvent := ReloadDesignCommand;
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
  ignoreDifferencesOn.OnChange := IgnoreDataChanged;
  ignoreAddingOn := TStringList.Create;
  ignoreAddingOn.Sorted := True;
  ignoreAddingOn.Duplicates := dupIgnore;
  ignoreAddingOn.OnChange := IgnoreDataChanged;
  ignoreRemovingOn := TStringList.Create;
  ignoreRemovingOn.Sorted := True;
  ignoreRemovingOn.Duplicates := dupIgnore;
  ignoreRemovingOn.OnChange := IgnoreDataChanged;
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
  Listeners := TListeners.Create;
  mqGetThreads := TStringList.Create;
  EnvironmentList := TStringList.Create;
  EnvironmentList.Sorted := True;
  MqInterface := TMqInterface.Create(self);
  mqUse := mquUndefined;
  mqMaxWorkingThreads := 15;
  if MqInterface.MQClientOK then mqUse := mquClient;
  if MqInterface.MQServerOK then mqUse := mquServer;
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
  HttpWebPageServer := TIdHTTPServer.Create(nil);
  with HttpWebPageServer do
  begin
    OnCommandGet := HttpWebPageServerCommandGet;
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
  Scripts := TStringList.Create;
  Scripts.Sorted := True;
  Scripts.Duplicates := dupError;
  Scripts.CaseSensitive := False;
  DisplayedLogColumns := TStringList.Create;
  OperationsWithEndpointOnly := True;
  SaveRelativeFileNames := True;
  InitSpecialWsdls;
    {$IFnDEF FPC}
  _WsdlDbsAdoConnection.OnWillConnect := ADOConnectionWillConnect;
    {$endif}
  isMasterModeEnabled := True;
end;

destructor TWsdlProject.Destroy;
begin
  FreeAndNil (HTTPProxyServer);
  FreeAndNil (HttpServer);
  FreeAndNil (HttpServerSSL);
  FreeAndNil (HttpServerBmtp);
  FreeAndNil (HttpWebPageServer);
  FreeAndNil (POP3Server);
  FreeAndNil (ServerOpenSSL);
  FreeAndNil (SmtpOpenSSL);
  fLogLock.Free;
  Listeners.Free;
  mqGetThreads.Clear;
  mqGetThreads.Free;
  toDisplayLogs.Clear;
  toDisplayLogs.Free;
  toUpdateDisplayLogs.Clear;
  toUpdateDisplayLogs.Free;
  EnvironmentList.Clear;
  EnvironmentList.Free;
  displayedLogs.Clear;
  displayedLogs.Free;
  archiveLogs.Clear;
  archiveLogs.Free;
  displayedExceptions.Clear;
  displayedExceptions.Free;
  toDisplayExceptions.Clear;
  toDisplayExceptions.Free;
  AsynchRpyLogs.Clear;
  AsynchRpyLogs.Free;
  FreeAndNil (unknownOperation);
  while Wsdls.Count > 0 do
  begin
    if (Wsdls.Objects[0] <> FreeFormatWsdl)
    and (Wsdls.Objects[0] <> CobolWsdl)
    and (Wsdls.Objects[0] <> XsdWsdl)
    and (Wsdls.Objects[0] <> SwiftMtWsdl) then
      Wsdls.Objects[0].Free;
    Wsdls.Delete(0);
  end;
  Wsdls.Free;
  wsdlNames.Free;
  FreeAndNil (FreeFormatWsdl);
  FreeAndNil (CobolWsdl);
  FreeAndNil (XsdWsdl);
  FreeAndNil (SwiftMtWsdl);
  ignoreDifferencesOn.Clear;
  ignoreDifferencesOn.Free;
  ignoreAddingOn.Clear;
  ignoreAddingOn.Free;
  ignoreRemovingOn.Clear;
  ignoreRemovingOn.Free;
  ignoreCoverageOn.Clear;
  ignoreCoverageOn.Free;
  FreeAndNil (webserviceWsdl);
  FreeAndNil (webserviceXsdDescr);
  FreeAndNil (wsaXsdDescr);
  FreeAndNil (swiftMTXsdDescr);
  ScriptsClear;
  Scripts.Free;
  DisplayedLogColumns.Free;
  inherited;
end;

procedure TWsdlProject.DefaultDisplayMessageData;
var
  xLog: TLog;
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
    s, o, x: Integer;
    xOperation: TWsdlOperation;
    operationTag: String;
    xMessage: TWsdlMessage;
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
          allOperations.AddObject ( xOperation.reqTagName
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
            xMessage := TWsdlMessage.CreateRequest(xOperation, 'Default', '.*', 'Default request')
          else
{}{
            if (not xOperation.isOneWay) then
{}
              xMessage := TWsdlMessage.CreateReply(xOperation, 'Default', '.*', 'Default reply');
{
          xMessage.Xml.Populate;
          xMessage.FltBind.Populate;
}
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
  f, w, o: Integer;
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

  for o := 0 to allOperations.Count - 1 do with allOperations.Operations[o] do// here since invokeAll
  begin
    OnGetAbortPressed := self.GetAbortPressed;
    if reqBind is TXml then with reqBind as TXml do Checked := True;
    if rpyBind is TXml then with rpyBind as TXml do Checked := True;
    if fltBind is TXml then with fltBind as TXml do Checked := True;
    Owner := Self;
    doInvokeOperations;
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
  AppDir: String;
  Binding : TIdSocketHandle;
  x: Integer;
begin
  abortPressed := not aActive;
  try
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
        and (   MqInterface.MQServerOK
             or MqInterface.MQClientOK
            ) then
        begin
          for x := 0 to Listeners.mqInterfaces.Count - 1 do
          begin
            with Listeners.mqInterfaces.Objects [x] as TMqInterface do
              Notify(format('Listening for MQ trafic on %s:%s.',[Qmanager, GetQueue]));
            mqGetThreads.AddObject ('', TMqGetThread.Create (Self, Listeners.mqInterfaces.Objects [x] as TMqInterface));
          end;
        end;
        if Listeners.httpPort > 0 then
          try
            Binding := HTTPServer.Bindings.Add;
            Binding.Port := Listeners.httpPort;
            Binding.IP := '0.0.0.0';
            HTTPServer.SessionState := False;
            HTTPServer.Active := true;
            Notify(format('Listening for HTTP trafic on %s:%d.',[HTTPServer.Bindings[0].IP, HTTPServer.Bindings[0].Port]));
          except
            on e: exception do
            begin
              LogServerMessage(format('Exception %s in Activate HTTP. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
            end;
          end;
        if Listeners.httpsPort > 0 then
          try
            with ServerOpenSSL.SSLOptions do
            begin
              Method := Listeners.sslVersion;
              CertFile := Listeners.sslCertificateFile;
              KeyFile := Listeners.sslKeyFile;
              RootCertFile := Listeners.sslRootCertificateFile;
            end;
            Binding := HTTPServerSSL.Bindings.Add;
            Binding.Port := Listeners.httpsPort;
            Binding.IP := '0.0.0.0';
            HTTPServerSSL.SessionState := False;
            HTTPServerSSL.Active := true;
            Notify(format('Listening for HTTPS connections on %s:%d.',[HTTPServerSSL.Bindings[0].IP, HTTPServerSSL.Bindings[0].Port]));
          except
            on e: exception do
            begin
              LogServerMessage(format('Exception %s in Activate HTTPS. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
            end;
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
  result := (MessageDlg (aPrompt, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
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
var
  x: Integer;
begin
  result := TXml.CreateAsString('Log', '');
  with result do
  begin
    AddXml(TXml.CreateAsString('maxEntries', IfThen(displayedLogsMaxEntries > -1, IntToStr(displayedLogsMaxEntries), 'unbounded')));
    case CompareLogOrderBy of
      clTimeStamp: AddXml(TXml.CreateAsString('CompareLogsOrdered', 'As is'));
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

function TWsdlProject.ProjectOptionsAsXml: TXml;
var
  x: Integer;
begin
  result := TXml.CreateAsString('projectOptions', '');
  with result.AddXml (TXml.CreateAsString('General', '')) do
  begin
    AddXml (TXml.CreateAsBoolean('SaveRelativeFileNames', SaveRelativeFileNames));
  end;
  result.AddXml(ProjectLogOptionsAsXml);
  with result.AddXml (TXml.CreateAsString('MasterSlave', '')) do
  begin
//    wsdlStubProjectOptionsForm.isSlaveMode := self.isSlaveMode; //only for enable/disable- not to edit
    AddXml (TXml.CreateAsBoolean('Enabled', isMasterModeEnabled));
    AddXml (TXml.CreateAsInteger('Port', MasterPortNumber));
  end;
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
  with result.AddXml (TXml.CreateAsString('DatabaseConnection', '')) do
  begin
    AddXml (TXml.CreateAsBoolean('Enabled', _WsdlDbsEnabled));
    AddXml (TXml.CreateAsString('Type', DbsType));
    AddXml (TXml.CreateAsString('DatabaseName', DbsDatabaseName));
    AddXml (TXml.CreateAsString('HostName', DbsHostName));
    AddXml (TXml.CreateAsString('Params', DbsParams));
    AddXml (TXml.CreateAsString('UserName', DbsUserName));
    AddXml (TXml.CreateAsString('Password', Xmlz.EncryptString(DbsPassword)));
    AddXml (TXml.CreateAsString('ConnectionString', DbsConnectionString))
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
  sl: TStringList;
  swapReqParent: TCustomBindable;
  AddedTypeDefElementsAsXml, checkerXmls: TXml;
begin
  AcquireLock;
  try
    with TXml.CreateAsString ('WsdlStubCase', '') do
    try
      AddXml(TXml.CreateAsString('FileName', ExpandUNCFileNameUTF8(aMainFileName) { *Converted from ExpandUNCFileName* }));

  {BEGIN oldstyle}
      AddXml(TXml.CreateAsInteger('Portnumber', Listeners.httpPort));
      AddXml(TXml.CreateAsBoolean('doUseMQ', doUseMQ));
      with AddXml(TXml.CreateAsString('mqInterfaces', '')) do
        for x := 0 to Listeners.mqInterfaces.Count - 1 do
          AddXml ((Listeners.mqInterfaces.Objects [x] as TMqInterface).AsXmlOldStyle);
      with AddXml(TXml.CreateAsString('stompInterfaces', '')) do
        for x := 0 to Listeners.stompInterfaces.Count - 1 do
          AddXml((Listeners.stompInterfaces.Objects [x] as TStompInterface).AsXmlOldStyle);
  {END oldstyle}
      AddXml (Listeners.AsXml);
      AddXml(TXml.CreateAsBoolean('isMasterModeEnabled', isMasterModeEnabled));
      AddXml(TXml.CreateAsInteger('MasterPortNumber', MasterPortNumber));
      AddXml(TXml.CreateAsBoolean('ValidateRequests', doValidateRequests));
      AddXml(TXml.CreateAsBoolean('ValidateReplies', doValidateReplies));
      AddXml (TXml.CreateAsBoolean('CheckExpectedValues', doCheckExpectedValues));
      AddXml (TXml.CreateAsBoolean('DisableOnCorrelate', _WsdlDisableOnCorrelate));
      AddXml (ProjectOptionsAsXml);
      with AddXml(TXml.CreateAsString('Environments', '')) do
        for x := 0 to EnvironmentList.Count - 1 do
          with AddXml(TXml.CreateAsString('Environment', '')) do
          begin
            AddAttribute(TXmlAttribute.CreateAsString('Name', EnvironmentList.Strings[x]));
            with AddXml (TXml.Create) do
              Text := (EnvironmentList.Objects [x] as TXml).Text;
          end;
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
              AddXml (cobolOperationsXml);
            xDone := True;
          end;
          if xWsdl = XsdWsdl then
          begin
            if XsdWsdl.Services.Services[0].Operations.Count > 0 then
              AddXml (xsdOperationsXml(aMainFileName));
            xDone := True;
          end;
          if xWsdl = SwiftMtWsdl then
          begin
            if SwiftMtWsdl.Services.Services[0].Operations.Count > 0 then
              AddXml (swiftMtOperationsXml);
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
                                         , ExpandUNCFileNameUTF8(Wsdls.Strings [w]) { *Converted from ExpandUNCFileName* }
                                         )
                    );
          end;
          if Assigned (xWsdl) then
          begin
            if xWsdl.ExtraXsds.Count > 0 then
            begin
              with AddXml (TXml.CreateAsString ('ExtraXsds','')) do
                AddXml (xWsdl.ExtraXsdsAsXml(SaveRelativeFileNames));
            end;
            AddXml(TXml.CreateAsInteger('ElementsWhenRepeatable', xWsdl.xsdElementsWhenRepeatable));
            AddedTypeDefElementsAsXml := xWsdl.XsdDescr.AddedTypeDefElementsAsXml as TXml;
            if AddedTypeDefElementsAsXml.Items.Count > 0 then
              AddXml (AddedTypeDefElementsAsXml)
            else
              FreeAndNil (AddedTypeDefElementsAsXml);
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
                    AddXml (TXml.CreateAsBoolean('HiddenFromUI', xOperation.HiddenFromUI));
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
                    AddXml (TXml.CreateAsInteger('StubStompPutPort', xOperation.StubStompPutPort));
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
                            for p := 0 to xMessage.corBinds.Count - 1 do
                              if Assigned (xMessage.corBinds.Bindables[p]) then
                                AddXml (TXml.CreateAsString('Pattern', xMessage.corBinds.Bindables[p].CorrelationValue))
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
      AddXml(TXml.CreateAsString('ignoreCoverageOn', ignoreCoverageOn.Text));
      with AddXml(TXml.CreateAsString('Scripts', '')) do
        for x := 0 to Scripts.Count - 1 do
          with AddXml(Txml.CreateAsString('Script', (Scripts.Objects[x] AS TStringList).Text))
            do AddAttribute(TXmlAttribute.CreateAsString('Name', Scripts.Strings[x]));
      AddXml (TXml.CreateAsString('FocusOperationName', FocusOperationName));
      AddXml (TXml.CreateAsInteger('FocusMessageIndex', FocusMessageIndex));
      result := AsText(False,0,False,False);
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
  wXml, xXml, sXml, oXml, eXml, eeXml, dXml, rXml, iXml, cXml: TXml;
  xBindName: String;
  swapReqParent: TCustomBindable;
  xMessage: TWsdlMessage;
  xReadAnother: Boolean;
  xPatterns, sl: TStringList;
  swapxsdElementsWhenRepeatable: Integer;
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
          Listeners.httpPort := xXml.Items.XmlIntegerByTagDef ['Portnumber', 0];
          doUseMQ := (xXml.Items.XmlValueByTag ['doUseMQ'] = 'true');
          sXml := xXml.Items.XmlItemByTag ['mqInterfaces'];
          if Assigned (sXml) then
            for s := 0 to sXml.Items.Count - 1 do
              Listeners.mqInterfaces.AddObject ('', TMqInterface.CreateFromXmlOldStyle (sXml.Items.XmlItems [s]));
          sXml := xXml.Items.XmlItemByTag ['stompConfig'];
          if Assigned (sXml) then
          begin
            for s := 0 to sXml.Items.Count - 1 do
              Listeners.stompInterfaces.AddObject ('', TStompInterface.CreateFromXml (sXml.Items.XmlItems [s], HaveStompFrame));
          end;
          sXml := xXml.Items.XmlItemByTag ['Listeners'];
          if Assigned (sXml) then
            Listeners.FromXml(sXml, HaveStompFrame);
          isMasterModeEnabled := xXml.Items.XmlBooleanByTagDef ['isMasterModeEnabled', true];
          MasterPortNumber := xXml.Items.XmlIntegerByTagDef ['MasterPortNumber', 3738];
          doValidateRequests := (xXml.Items.XmlValueByTag ['ValidateRequests'] = 'true');
          doValidateReplies := (xXml.Items.XmlValueByTag ['ValidateReplies'] = 'true');
          doCheckExpectedValues := xXml.Items.XmlBooleanByTagDef['CheckExpectedValues', False];
          _WsdlDisableOnCorrelate := xXml.Items.XmlBooleanByTagDef['DisableOnCorrelate', False];
          eXml := xXml.Items.XmlItemByTag ['ProjectOptions'];
          if Assigned (eXml) then
            ProjectOptions36FromXml(eXml);
          eXml := xXml.Items.XmlItemByTag ['projectOptions'];
          if Assigned (eXml) then
            ProjectOptionsFromXml(eXml);
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
          ignoreCoverageOn.Text := xXml.Items.XmlValueByTag ['ignoreCoverageOn'];
          FocusOperationName := xXml.Items.XmlValueByTag['FocusOperationName'];
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
                    xWsdl.ExtraXsdsFromXml (dXml.Items.XmlItems[0]);
                    xWsdl.LoadExtraXsds;
                  end;
                end;
                dXml := wXml.Items.XmlItemByTag ['AddedTypeDefElements'];
                if Assigned (dXml) then
                begin
                  xWsdl.XsdDescr.AddedTypeDefElementsFromXml (dXml);
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
                            xOperation.StubStompPutPort := StrToIntDef(oXml.Items.XmlValueByTag ['StubStompPutPort'], 61613);
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
                                      xMessage.FreeFormatReq := rXml.Value
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
          cXml := xXml.Items.XmlItemByTag ['Scripts'];
          if Assigned (cXml) then
            for s := 0 to cXml.Items.Count - 1 do with cXml.Items.XmlItems[s] do
              if (Name = 'Script')
              and (Attributes.ValueByTag['Name'] <> '') then
              begin
                sl := TStringList.Create;
                sl.Text := Value;
                Scripts.Objects[Scripts.Add(Attributes.ValueByTag['Name'])] := sl;
              end;
          AcquireLock;
          try
            PrepareAllOperations (LogServerMessage);
          finally
            ReleaseLock;
          end;
        finally
          xXml.Free;
        end;
        _WsdlPortNumber := IntToStr(Listeners.httpPort);
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
            unknownOperation.StubStompPutPort := aXml.Items.XmlIntegerByTag ['notStubbedStompPort'];
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

procedure TwsdlProject.CreateLogReply ( aLog: TLog
                                       ; var aProcessed: Boolean
                                       ; aIsActive: Boolean
                                       );
var
  xMessage: String;
  xXml: TXml;
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
              aLog.ReplyBody := SendOperationMessage(unknownOperation, aLog.RequestBody);
              aProcessed := True;
            end;
          saRequest: raise;
        end;
        if aLog.TransportType = ttHttp then
          raise Exception.Create (aLog.ReplyBody)
        else
          exit;
      end;
      if (e.Message = 'RaiseFault')
      or (e.Message = 'RaiseSoapFault') then
      begin
        if Assigned (aLog.Operation) then
          aLog.StubAction := aLog.Operation.StubAction;
        aLog.ReplyBody := aLog.Operation.StreamFault (_progName, True);
      end;
      if AnsiStartsStr(S_RETURN_STRING, e.Message) then
      begin
        aLog.ReplyBody := Copy (e.Message, 1 + Length(S_RETURN_STRING), Length(e.Message));
        aLog.Exception := '';
        exit;
      end;
      if aLog.TransportType = ttHttp then
        raise Exception.Create (aLog.ReplyBody)
      else
        exit;
    end;
  end; //except
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
  xXml: TXml;
  xOperation: TWsdlOperation;
  sl: String;
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
    aReply := xOperation.MessageBasedOnRequest;
    if not Assigned (aReply) then
      Raise Exception.Create('Could not find any reply based on request');
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
    if xOperation.WsdlService.DescriptionType in [ipmDTFreeFormat] then
      xOperation.FreeFormatRpy := aReply.FreeFormatRpy
    else
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
      try
        xOperation.rpyWsaOnRequest;
        xOperation.ExecuteBefore;
        xOperation.ExecuteRpyStampers;
      except
        on e: exception do
          if e.Message <> 'Exit' then
            raise;
      end;
      if xOperation.doDebug
      and Assigned (OnDebugOperationEvent) then
      begin
        DebugOperation := xOperation;
        OnDebugOperationEvent;
      end;
    end;
    aLog.InitDisplayedColumns(xOperation, DisplayedLogColumns);
    aLog.DelayTimeMs := xOperation.DelayTimeMs;
    result := xOperation.StreamReply (_progName, True);
    if not isAsynchronous then
      CreateLogReplyPostProcess(aLog, xOperation);
  finally
    if Assigned (xOperation.Cloned) then
      xOperation.Free;
  end;
end;

function TWsdlProject.CreateScriptOperation(aString: String): TWsdlOperation;
var
  x: Integer;
  xWsdl: TWsdl;
  xOperation: TWsdlOperation;
  xLog: TLog;
begin
  xWsdl := TWsdl.Create(1, 1, False);
  with XsdWsdl do
  begin
    Name := 'Script';
    isSoapService := False;
    Services.Add(Name);
    Services.Objects[0] := TWsdlService.Create;
    Services.Services[0].Name := Name;
    Services.Services[0].DescriptionType := ipmDTXsd;
    xOperation := TWsdlOperation.Create (xWsdl);
    xOperation.Owner := self;
    xOperation.Name := Name;
    Services.Services[0].Operations.AddObject(xOperation.Name, xOperation);
    xOperation.Wsdl := xWsdl;
    xOperation.Owner := self;
    xOperation.WsdlService := Services.Services[0];
    xOperation.reqTagName := xOperation.Name + '_Req';
    xOperation.rpyTagName := xOperation.Name + '_Rpy';
    xOperation.reqRecognition := TStringList.Create;
    xOperation.rpyRecognition := TStringList.Create;
    xOperation.RecognitionType := rtSubString;
    xOperation.reqXsd.ElementName := xOperation.reqTagName;
    xOperation.rpyXsd.ElementName := xOperation.rpyTagName;
    xOperation.BeforeScriptLines.Text := aString;
    xOperation.OnGetAbortPressed := self.GetAbortPressed;
  end;
  try
    for x := 0 to allOperations.Count - 1 do with allOperations.Operations[x] do
    begin
      AcquireLock;
      try
        xOperation.invokeList.Add(reqTagName);
        xOperation.ReqBindablesFromWsdlMessage(Messages.Messages[0]);
      finally
        ReleaseLock;
      end;
    end;
    xOperation.AcquireLock;
    try
      result := TWsdlOperation.Create(xOperation);
    finally
      xOperation.ReleaseLock;
    end;
    try
      with result do
      begin
        BeforeScriptLines.Text := aString;
        PrepareBefore;
      end;
    except
      on e: Exception do
        LogServerMessage (Format('Exception %s%s%swas raised%s', [CRLF, e.Message, CRLF, CRLF]), True, e);
    end;
  finally
//    xWsdl.Free; starnge trick not yet
  end;
end;

procedure TWsdlProject.HaveStompFrame(aStompInterface: TStompInterface;
  aQueue: String; aFrame: IStompFrame);
var
  xProcessed: Boolean;
  xReply: String;
  xLog: TLog;
begin
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
      and (aFrame.GetHeaders.Value('reply-to') <> '') then
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
      SaveLog ('', xLog);
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
  xStream: TStringStream;
begin
  result := '';
  if ARequestInfo.ContentEncoding = 'gzip' then
  begin
    xStream := TStringStream.Create('');
    try
      with TIdCompressorZLib.Create(nil) do
      try
        DecompressGZipStream(ARequestInfo.PostStream, xStream);
        xStream.Position := 0;
        result := xStream.ReadString(xStream.Size);
        AResponseInfo.ContentEncoding := ARequestInfo.ContentEncoding;
      finally
        Free;
      end;
    finally
      xStream.Free;
    end;
  end
  else
  begin
    if ARequestInfo.ContentEncoding = 'deflate' then
    begin
      xStream := TStringStream.Create('');
      try
        with TIdCompressorZLib.Create(nil) do
        try
          DecompressHTTPDeflate(ARequestInfo.PostStream, xStream);
          xStream.Position := 0;
          result := xStream.ReadString(xStream.Size);
          AResponseInfo.ContentEncoding := ARequestInfo.ContentEncoding;
        finally
          Free;
        end;
      finally
        xStream.Free;
      end;
    end
    else
    begin
      if Assigned (ARequestInfo.PostStream) then
      begin
        ARequestInfo.PostStream.Position := 0;
//      result := (ARequestInfo.PostStream as TStringStream).ReadString(ARequestInfo.PostStream.Size);
        result := (ARequestInfo.PostStream as TStringStream).ReadString(ARequestInfo.PostStream.Size);
      end;
      AResponseInfo.ContentEncoding := 'identity';
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

procedure TwsdlProject.ProjectLogOptionsFromXml(aXml: TXml);
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

procedure TwsdlProject.ProjectOptionsFromXml(aXml: TXml);
var
  xXml, yXml: TXml;
  z: Integer;
begin
  if not Assigned (aXml) then Exit;
  if aXml.Name <> 'projectOptions' then raise Exception.Create('ProjectOptionsFromXml illegal XML' + aXml.Text);
  _WsdlDbsConnector.Connected := False;
  _WsdlDbsConnector.LoginPrompt := False;
  _WsdlDbsEnabled := False;
  DbsType := '';
  DbsDatabaseName := '';
  DbsHostName:='';
  DbsParams := '';
  DbsUserName:='';
  DbsPassword := '';
  wrdFunctionz.wrdDetectFormatChanges := False;
  wrdFunctionz.wrdNewDocumentAsReference := False;
  wrdFunctionz.wrdExpectedDifferenceCount := 0;
  isMasterModeEnabled := False;
  MasterPortNumber := 3738;
  xsdElementsWhenRepeatable := defaultXsdElementsWhenRepeatable;
  xsdMaxDepthBillOfMaterials := defaultXsdMaxDepthBillOfMaterials;
  xsdMaxDepthXmlGen := defaultXsdMaxDepthXmlGen;
  _WsdlDisableOnCorrelate := False;
  unknownOperation.StubAction := saStub;
  PublishDescriptions := False;
  OperationsWithEndpointOnly := True;
  SaveRelativeFileNames := True;
  notStubbedExceptionMessage := 'No operation recognised';

  if not aXml.Checked then Exit;
  with aXml.Items do
  begin
    xXml := XmlCheckedItemByTag ['General'];
    if Assigned (xXml) then
    begin
      SaveRelativeFileNames := xXml.Items.XmlCheckedBooleanByTagDef['SaveRelativeFileNames', True];
    end;
    ProjectLogOptionsFromXml (XmlCheckedItemByTag ['Log']);
    xXml := XmlCheckedItemByTag ['MasterSlave'];
    if Assigned (xXml) then
    begin
      isMasterModeEnabled := xXml.Items.XmlCheckedBooleanByTagDef['Enabled', isMasterModeEnabled];
      MasterPortNumber := xXml.Items.XmlCheckedIntegerByTagDef['Port', MasterPortNumber];
    end;
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
    xXml := XmlCheckedItemByTag ['DatabaseConnection'];
    if Assigned (xXml) then
    begin
      _WsdlDbsEnabled := xXml.Items.XmlCheckedBooleanByTagDef['Enabled', _WsdlDbsEnabled];
      DbsType := xXml.Items.XmlCheckedValueByTagDef['Type', DbsType];
      DbsDatabaseName := xXml.Items.XmlCheckedValueByTagDef['DatabaseName', DbsDatabaseName];
      DbsHostName := xXml.Items.XmlCheckedValueByTagDef['HostName', DbsHostName];
      DbsParams := xXml.Items.XmlCheckedValueByTagDef['Params', DbsParams];
      DbsUserName := xXml.Items.XmlCheckedValueByTagDef['UserName', DbsUserName];
      DbsPassword := xmlz.DecryptString(xXml.Items.XmlCheckedValueByTag['Password']);
      DbsConnectionString := xXml.Items.XmlCheckedValueByTagDef['ConnectionString', DbsConnectionString]; // to be able to create ado version project
    { TODO : hide password }
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
  end;
end;

procedure TwsdlProject.UpdateReplyColumns(aOperation: TWsdlOperation);
  procedure _UpdateFirstRow;
  var
    c: Integer;
  begin
    with aOperation.Messages.Messages[0] do
    begin
      for c := 0 to corBinds.Count - 1 do
        if Assigned (corBinds.Bindables[c]) then
          corBinds.Bindables[c].CorrelationValue := '.*' ;
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
  y, c: Integer;
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

function TwsdlProject.WsdlOpenFile(aName: String; aElementsWhenRepeatable: Integer): TWsdl;
var
  s, o: Integer;
begin
  if UpperCase (ExtractFileExt (aName)) = '.SDF' then
  begin
    result := TWsdl.Create(aElementsWhenRepeatable, xsdElementsWhenRepeatable, OperationsWithEndpointOnly);
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
    result := TWsdl.Create(aElementsWhenRepeatable, xsdElementsWhenRepeatable, OperationsWithEndpointOnly);
    result.LoadFromSchemaFile(aName, nil);
  end;
end;

procedure TwsdlProject.UpdateMessageRow(aOperation: TWsdlOperation;
  aMessage: TWsdlMessage);
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

function TwsdlProject.SendOperationMessage(aOperation: TWsdlOperation;
  aMessage: String): String;
var
  reqheader, rpyheader, responsecode: String;
begin
  case aOperation.StubTransport of
    ttHttp: result := SendHttpMessage (aOperation, aMessage, reqheader, rpyheader, responsecode);
    ttMq: result := SendOperationMqMessage (aOperation, aMessage, reqheader);
    ttStomp: result := SendOperationStompMessage (aOperation, aMessage, reqheader, rpyheader);
    ttTaco: result := SendOperationTacoMessage(aOperation, aMessage, reqheader, rpyheader)
  end;
end;

function TwsdlProject.SendOperationMqMessage(aOperation: TWsdlOperation;
  aMessage: String; var aMqHeaderAsText: String): String;
var
  mq: TMqInterface;
  xIsRequest: Boolean;
  fXml: TXml;
begin
  Result := '';
  fXml := nil;
  if not Assigned (aOperation)
    then raise Exception.Create('SendOperationMqMessage: null arguments');
  mq := TMqInterface.Create (nil);
  try
    mq.Use := mqUse;
    mq.Qmanager := aOperation.StubMqPutManager;
    mq.PutQueue := aOperation.StubMqPutQueue;
    mq.GetQueue := aOperation.StubMqGetQueue;
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

function TwsdlProject.SendHttpMessage(aOperation: TWsdlOperation;
  aMessage: String; var aReqHeader, aRpyHeader, aResponseCode: String): String;
  function _Decompress (aResponse: TIdHTTPResponse; aStream: TStringStream): String;
  var
    dStream: TStringStream;
  begin
    aStream.Position := 0;
    if (lowercase(aResponse.ContentEncoding) = 'deflate')
    or (lowercase(aResponse.ContentEncoding) = 'gzip') then
    begin
      dStream := TStringStream.Create('');
      try
        with TIdCompressorZLib.Create(nil) do
        try
          if aResponse.ContentEncoding = 'gzip' then
            DecompressGZipStream(aStream, dStream);
          if aResponse.ContentEncoding = 'deflate' then
            DecompressHTTPDeflate(aStream, dStream);
          dStream.Position := 0;
          result := dStream.ReadString(dStream.Size);
        finally
          Free;
        end;
      finally
        FreeAndNil(dStream);
      end;
    end
    else
    begin
      result := aStream.ReadString(aStream.Size)
    end;
  end;

var
  HttpClient: TIdHTTP;
  HttpRequest, sStream, dStream: TStringStream;
  URL: String;
  xResponse: String;
  oUri, sUri: TIdUri;
  xName, xValue: String;
  x: Integer;
begin
  Result := '';
  if not Assigned (aOperation)
    then raise Exception.Create('SendHttpMessage: null arguments');
  HttpClient := TIdHTTP.Create;
  try
    HttpRequest := TStringStream.Create ('');
    try
      if aOperation.StubHttpAddress <> '' then
      begin
        sUri := TIdUri.Create(aOperation.StubHttpAddress);
        if aOperation.SoapAddress <> '' then
        begin
          oUri := TIdUri.Create(aOperation.SoapAddress);
          try
            if oUri.Protocol <> '' then
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
        sStream := TStringStream.Create('');
        try
          sStream.WriteString(aMessage);
          sStream.Position := 0;
          if HttpClient.Request.ContentEncoding = 'deflate' then
            CompressStreamEx(sStream, HttpRequest, clDefault, zsZLib);
          if HttpClient.Request.ContentEncoding = 'gzip' then
            CompressStreamEx(sStream, HttpRequest, clDefault, zsGZip);
        finally
          sStream.Free;
        end;
      end
      else
        HttpRequest.WriteString (aMessage);
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
        Notify('result := HttpClient.Verb(URL, HttpRequest);<');
        dStream := TStringStream.Create('');
        try
          try
            try
              with aOperation do
              begin
                if httpVerb = 'DELETE' then HttpClient.Delete(URL);
                if httpVerb = 'GET' then httpClient.Get(URL, dStream);
                if httpVerb = 'HEAD' then HttpClient.Head(URL);
                if httpVerb = 'OPTIONS' then HttpClient.Options(URL);
                if httpVerb = 'POST' then HttpClient.Post(URL, HttpRequest, dStream);
                if httpVerb = 'PUT' then HttpClient.Put(URL, HttpRequest, dStream);
                if httpVerb = 'TRACE' then httpClient.Trace(URL, dStream);
              end;
            finally
              aReqHeader := HttpClient.Request.RawHeaders.Text;
              aRpyHeader := HttpClient.Response.RawHeaders.Text;
              aResponseCode := IntToStr(HttpClient.ResponseCode);
            end;
            result := _Decompress (HttpClient.Response, dStream);
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
        Notify('>result := HttpClient.Post(URL, HttpRequest);');
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
    if Assigned (HttpClient.Compressor) then
    begin
      HttpClient.Compressor.Free;
      HttpClient.Compressor := nil;
    end;
    FreeAndNil (HttpClient);
  end;
end;

function TwsdlProject.SendMessage ( aOperation: TWsdlOperation
                                  ; aRequest: TWsdlMessage
                                  ; aCorrelationId: String
                                  ): String;
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
  xResponse: String;
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
      xLog.Mssg := aRequest;
      if Assigned (aRequest) then
        aOperation.ReqBindablesFromWsdlMessage(aRequest);
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
        end;
      finally
        xLog.InboundTimeStamp := Now;
      end;
      if xLog.ReplyBody = S_MESSAGE_ACCEPTED then
        xLog.ReplyBody := ''
      else
      begin
        if not (aOperation.WsdlService.DescriptionType in [ipmDTFreeFormat, ipmDTEmail]) then
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
        if Assigned (aRequest) then
          Mssg := aRequest
        else
          Mssg := aOperation.Messages.Messages[0];
        CorrelationId := aOperation.CorrelationIdAsText ('; ');
        Stubbed := True;
        StubAction := aOperation.StubAction;
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
          if Assigned (aRequest) then
            Mssg := aRequest
          else
            Mssg := aOperation.Messages.Messages[0];
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
    SaveLog ('', xLog);
  end;
end;

procedure TwsdlProject.SendAsynchReply(aLog: TLog);
var
  xAction, xAddress: String;
  xXml: TXml;
  xOperation: TWsdlOperation;
  xTries: Integer;
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

procedure TwsdlProject.CheckExpectedValues(aLog: TLog; aOperation: TWsdlOperation; aDoCheck: Boolean);
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

function TwsdlProject.SendOperationTacoMessage(aOperation: TWsdlOperation;
  aMessage: String; var aRequestHeader, aReplyHeader: String): String;
{$ifdef windows}
var
  Taco: TTacoInterface;
  fXml: TXml;
begin
  Result := '';
  aRequestHeader := '';
  aReplyHeader:= '';
  if not Assigned (aOperation)
    then raise Exception.Create('SendOperationTacoMessage: null arguments');
  Taco := TTacoInterface.Create (nil, nil);
  try
    Taco.UserName := aOperation.WsdlService.UserName;
    Taco.Password := DecryptString(aOperation.WsdlService.Password); // bad news
    result := Taco.RequestReply(aMessage, 0, aOperation.TacoConfigXml);
  finally
    FreeAndNil (Taco);
  end;
end;
{$else}
begin
  raise Exception.Create ('SendOperationTacoMessage not im,plemented');
end;
{$endif}

procedure TWsdlProject.SetAbortPressed(const Value: Boolean);
begin
  fAbortPressed := Value;
end;

function TwsdlProject.SendOperationSmtpMessage(aOperation: TWsdlOperation;
  aMessage: String; var aRequestHeader, aReplyHeader: String): String;
var
  Smtp: TIdSMTP;
  mailMessage: TIdMessage;
  fXml: TXml;
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

function TwsdlProject.SendOperationStompMessage(aOperation: TWsdlOperation;
  aMessage: String; var aRequestHeader: String; var aReplyHeader: String): String;
var
  Stomp: TStompInterface;
  fXml: TXml;
begin
  Result := '';
  if not Assigned (aOperation)
    then raise Exception.Create('SendOperationStompMessage: null arguments');
  Stomp := TStompInterface.Create (nil, HaveStompFrame);
  Stomp.Host := aOperation.StubStompPutHost;
  Stomp.Port := aOperation.StubStompPutPort;
  Stomp.ClientId := aOperation.StubStompPutClientId;
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
          Stomp.Put ( aMessage
                    + aOperation.StubStompRequestBodyPostFix // WORKAROUND, see XSD
                    , fXml
                    , aOperation.StubCustomHeaderXml
                    , aRequestHeader
                    )
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

function TwsdlProject.RedirectCommandStomp ( aCommand: String
                                  ; aHost: String
                                  ; aPort: Integer
                                  ; aDestination: String
                                  ; aReplyTo: String
                                  ; aTimeOut: Integer
                                  ): String;
var
  stomp: TStompInterface;
  xXml: TXml;
  xReqHeader, xRpyHeader: String;
begin
  Result := '';
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


function TwsdlProject.RedirectCommandMQ(aCommand, aPutManager, aPutQueue,
  aReplyToManager, aReplyToQueue, aGetManager, aGetQueue: String;
  aTimeOut: Integer): String;
var
  mq: TMqInterface;
begin
  Result := '';
  mq := TMqInterface.Create (nil);
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

function TwsdlProject.RedirectCommandURC(aCommand: String): String;
begin
  SendOperationMessage(unknownOperation, aCommand);
end;

function doRefuseHttpConnections(aObject: TObject; aLater, aTime: Extended): Extended;
var
  xProject: TWsdlProject;
begin
  if aObject is TWsdlOperation then
    xProject := (aObject as TWsdlOperation).Owner as TWsdlProject
  else
    xProject := aObject as TWsdlProject;
  TProcedureThread.Create(False, xProject, xProject.RefuseHttpConnectionsThreaded, aLater, aTime);
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

function TwsdlProject.RedirectCommandString(aCommand,
  aAddress, aSoapAction: String): String;
var
  HttpClient: TIdHTTP;
  HttpRequest: TStringStream;
begin
  HttpClient := TIdHTTP.Create;
  try
{}{
    HttpClient.Compressor := TIdCompressorZLib.Create(nil);
{}
    HttpRequest := TStringStream.Create ('');
    try
      try
        HttpClient.Request.CustomHeaders.Values ['SOAPAction'] := aSoapAction;
      except
      end;
      HttpClient.Request.ContentType := 'text/xml';
      HttpClient.Request.CharSet := '';
      HttpRequest.WriteString ( aCommand);
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
    if Assigned (HttpClient.Compressor) then
    begin
      HttpClient.Compressor.Free;
      HttpClient.Compressor := nil;
    end;
    FreeAndNil (HttpClient);
  end;
end;

function TwsdlProject.RedirectCommandHTTP (aCommand, aStubAddress, aDocument, aSoapAction: String): String;
var
  HttpClient: TIdHTTP;
  HttpRequest: TStringStream;
  URL: String;
  destUri, docUri: TidURI;
begin
  HttpClient := TIdHTTP.Create;
  try
    HttpClient.Compressor := TIdCompressorZLib.Create(nil);
    HttpRequest := TStringStream.Create ('');
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
      HttpRequest.WriteString (aCommand);
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
    if Assigned (HttpClient.Compressor) then
    begin
      HttpClient.Compressor.Free;
      HttpClient.Compressor := nil;
    end;
    FreeAndNil (HttpClient);
  end;
end;

procedure TwsdlProject.CreateLogReplyPostProcess ( aLogItem: TLog; aOperation: TWsdlOperation);
var
  xMessage: String;
  xXml: TXml;
begin
  if Assigned (aOperation) then
    aLogItem.StubAction := aOperation.StubAction;
  if Assigned (aOperation) then
  begin
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
            RequestValidateResult := xMessage;
          RequestValidated := True;
        end;
        if doValidateReplies
        and (aOperation.WsdlService.DescriptionType <> ipmDTFreeFormat)
        and Assigned (aOperation.rpyBind)
        and (aOperation.rpyBind is TXml) then
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
end;

function TWsdlProject.SendMessageLater(aOperation: TWsdlOperation;
  aRequest: TWsdlMessage; aCorrelationId: String; aLater: Integer): String;
begin
  TSendSoapRequestThread.Create (Self, aOperation, aRequest, aCorrelationId, aLater);
end;

function TWsdlProject.FindXmlOperationOnReply (aXml: TXml): TWsdlOperation;
var
  w, x, s, o, f: Integer;
  xXml, rpyXml: TXml;
  eBind: TCustomBindable;
  xWsdl: TWsdl;
  xService: TWsdlService;
  xOperation: TWsdlOperation;
  xRecog: TRecognition;
  xName: String;
begin
  result := nil;
  {
    <?xml version="1.0" encoding="UTF-8" ?>
    <!-- 2/21/2009 10:31:26 PM: Generated with IpmGun by Bouwman -->
    <SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsd1="http://soapinterop.org/" xmlns:typens="http://soapinterop.org/xsd" xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/">
      <SOAP-ENV:Body>
        <mns:echoStructResponse xmlns:mns="http://soapinterop.org/" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
          <return xsi:type="typens:SOAPStruct">
            <varInt xsi:type="xsd:int">1</varInt>
            <varFloat xsi:type="xsd:float">12</varFloat>
            <varString xsi:type="xsd:string">Jan</varString>
          </return>
        </mns:echoStructResponse>
      </SOAP-ENV:Body>
    </SOAP-ENV:Envelope>
  }
  result := nil;
  if NameWithoutPrefix(aXml.TagName) = 'Envelope' then
  begin
    for x := 0 to aXml.Items.Count - 1 do
    begin
      xXml := aXml.Items.XmlItems [x];
      if (NameWithoutPrefix(xXml.TagName) = 'Body')
      and (xXml.Items.Count > 0) then
      begin
        xName := NameWithoutPrefix (xXml.Items.XmlItems [0].Name);
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
  o, r: Integer;
  xOperation: TWsdlOperation;
  xRecog: TRecognition;
  xMatch: Boolean;
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
  o: Integer;
  xOperation: TWsdlOperation;
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
  w, x, s, o, f: Integer;
  xXml: TXml;
  eBind: TCustomBindable;
  xWsdl: TWsdl;
  xService: TWsdlService;
  xOperation: TWsdlOperation;
  xRecog: TRecognition;
begin
  result := nil;
  {
    <?xml version="1.0" encoding="UTF-8" ?>
    <!-- 2/21/2009 10:31:26 PM: Generated with IpmGun by Bouwman -->
    <SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsd1="http://soapinterop.org/" xmlns:typens="http://soapinterop.org/xsd" xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/">
      <SOAP-ENV:Body>
        <mns:echoStructResponse xmlns:mns="http://soapinterop.org/" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
          <return xsi:type="typens:SOAPStruct">
            <varInt xsi:type="xsd:int">1</varInt>
            <varFloat xsi:type="xsd:float">12</varFloat>
            <varString xsi:type="xsd:string">Jan</varString>
          </return>
        </mns:echoStructResponse>
      </SOAP-ENV:Body>
    </SOAP-ENV:Envelope>
  }
  result := nil;
  if NameWithoutPrefix(aXml.TagName) = 'Envelope' then
  begin
    for x := 0 to aXml.Items.Count - 1 do
    begin
      xXml := aXml.Items.XmlItems [x];
      if (NameWithoutPrefix(xXml.TagName) = 'Body')
      and (xXml.Items.Count > 0) then
      begin
        xXml := xXml.Items.XmlItems [0];
        if allOperations.Find(NameWithoutPrefix (xXml.TagName), f) then
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
  oXml: TXml;
  f, x, o: Integer;
  xOperation: TWsdlOperation;
begin
  if aXml.Name <> 'FreeFormatOperations' then
    raise Exception.Create('??TWsdlProject.freeFormatOperationsUpdate(aXml: TXml): ' + aXml.Name);
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
  function _LoadXsdMsg (aLabel: String; sXml: TXml; aXsd: TXsd; var aDescrFileName: String): TXml;
  var
    x, y, z, f: Integer;
    xXsd: TXsd;
    xXsdDescr: TXsdDescr;
  begin
    result := nil;
    try
      if not Assigned (sXml) then
        exit;
      aDescrFileName := ExpandUNCFileNameUTF8(ExpandRelativeFileName
                            (aMainFileName, sXml.Items.XmlCheckedValueByTag ['DescriptionFile'])
                          ); { *Converted from ExpandUNCFileName* }
      if xsdElementsWhenRepeatable > 0 then
        xXsdDescr := TXsdDescr.Create(xsdElementsWhenRepeatable)
      else
        xXsdDescr := TXsdDescr.Create(XsdWsdl.xsdDefaultElementsWhenRepeatable);
      XsdWsdl.sdfXsdDescrs.AddObject('', xXsdDescr);
      try
        xXsdDescr.AddXsdFromFile(aDescrFileName, nil);
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
  oXml: TXml;
  f, x, o: Integer;
  xOperation: TWsdlOperation;
begin
  if aXml.Name <> 'XsdOperations' then
    raise Exception.Create('??TWsdlProject.XsdOperationsUpdate(aXml: TXml): ' + aXml.Name);
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
        aFilenames.AddObject (aXml.Value, Pointer (ipmDTCobol));
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
      if aFileNames.Find ( ExpandUNCFileNameUTF8(ExpandRelativeFileName ( aMainFileName
                                                                      , sXml.Items.XmlCheckedValueByTag ['DescriptionFile']
                                                                      )
                                             ) { *Converted from ExpandUNCFileName* }
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
  oXml: TXml;
  f, x, o: Integer;
  xOperation: TWsdlOperation;
  xFileNames: TStringList;
  xIpmDescr: TIpmDescr;
begin
  if aXml.Name <> 'CobolOperations' then
    raise Exception.Create('??TWsdlProject.cobolOperationsUpdate(aXml: TXml): ' + aXml.Name);
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
    x, y, z, f: Integer;
    fXsd, xXsd, b4Xsd: TXsd;
    xXsdDescr: TXsdDescr;
    xpXmls, xpXml: TXml;
  begin
    aDescrFileName := '';
    aDescrExpansionFileName := '';
    try
      if not Assigned (sXml) then Exit;
      xpXmls := TXml.CreateAsString('expansions', '');
      try
        xXsdDescr := TXsdDescr.Create(1);
        SwiftMtWsdl.sdfXsdDescrs.AddObject('', xXsdDescr);
        xXsdDescr.AddXsdFromFile(_swiftMTXsdFileName, nil);
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
            aDescrFileName := ExpandUNCFileNameUTF8(ExpandRelativeFileName(aMainFileName, sXml.Items.XmlItems[x].Value)); { *Converted from ExpandUNCFileName* }
            xXsdDescr.AddXsdFromFile(aDescrFileName, nil);
          end;
          if sXml.Items.XmlItems[x].Name = 'DescriptionExpansionFile' then
            with xpXmls.AddXml(TXml.Create) do
            begin
              aDescrExpansionFileName := ExpandUNCFileNameUTF8(ExpandRelativeFileName(aMainFileName, sXml.Items.XmlItems[x].Value)); { *Converted from ExpandUNCFileName* }
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
  oXml: TXml;
  f, x, o: Integer;
  xOperation: TWsdlOperation;
begin
  if aXml.Name <> 'SwiftMtOperations' then
    raise Exception.Create('??TWsdlProject.SwiftMtOperationsUpdate(aXml: TXml): ' + aXml.Name);
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
  o, x, f: Integer;
  xOperation: TWsdlOperation;
begin
  result := TXml.CreateAsString('XsdOperations', '');
  for x := 0 to XsdWsdl.Services.Services[0].Operations.Count - 1 do
  begin
    xOperation := XsdWsdl.Services.Services[0].Operations.Operations[x];
    with result.AddXml(TXml.CreateAsString('Operation', '')) do
    begin
      AddXml (TXml.CreateAsString('Name', xOperation.Name));
      if Assigned (xOperation.reqBind)
      and (xOperation.reqDescrFilename <> '') then
        with AddXml (TXml.CreateAsString('Req', '')) do
        begin
          if (aMainFileName <> '')
          and (SaveRelativeFileNames) then
            AddXml(TXml.CreateAsString ( 'DescriptionFile'
                                       , ExtractRelativeFileName ( aMainFileName
                                                                 , xOperation.reqDescrFilename
                                                                 )
                                       )
                  )
          else
            AddXml(TXml.CreateAsString('DescriptionFile', xOperation.reqDescrFilename));
          if xOperation.reqBind.Children.Count > 0 then
            AddXml(TXml.CreateAsString('ElementName', (xOperation.reqBind as TXml).Items.XmlItems[0].Name));
        end;
      if Assigned (xOperation.rpyBind)
      and (xOperation.rpyDescrFilename <> '') then
        with AddXml (TXml.CreateAsString('Rpy', '')) do
        begin
          if (aMainFileName <> '')
          and (SaveRelativeFileNames) then
            AddXml(TXml.CreateAsString ( 'DescriptionFile'
                                       , ExtractRelativeFileName ( aMainFileName
                                                                 , xOperation.rpyDescrFilename
                                                                 )
                                       )
                  )
          else
            AddXml(TXml.CreateAsString('DescriptionFile', xOperation.rpyDescrFilename));
          if xOperation.rpyBind.Children.Count > 0 then
            AddXml(TXml.CreateAsString('ElementName', (xOperation.rpyBind as TXml).Items.XmlItems[0].Name));
        end;
      if Assigned (xOperation.fltBind)
      and (xOperation.fltDescrFilename <> '') then
        with AddXml (TXml.CreateAsString('Flt', '')) do
        begin
          if (aMainFileName <> '')
          and (SaveRelativeFileNames) then
            AddXml(TXml.CreateAsString ( 'DescriptionFile'
                                       , ExtractRelativeFileName ( aMainFileName
                                                                 , xOperation.fltDescrFilename
                                                                 )
                                       )
                  )
          else
            AddXml(TXml.CreateAsString('DescriptionFile', xOperation.fltDescrFilename));
          if xOperation.fltBind.Children.Count > 0 then
            AddXml(TXml.CreateAsString('ElementName', (xOperation.fltBind as TXml).Items.XmlItems[0].Name));
        end;
      if xOperation.reqRecognition.Count > 0 then
        AddXml (operationRecognitionXml('reqRecognition', xOperation.RecognitionType, xOperation.reqRecognition));
      if xOperation.rpyRecognition.Count > 0 then
        AddXml (operationRecognitionXml('rpyRecognition', xOperation.RecognitionType, xOperation.rpyRecognition));
    end;
  end;
  result.CheckDownline(True);
end;

function TWsdlProject.cobolOperationsXml: TXml;
var
  o, x, f: Integer;
  xOperation: TWsdlOperation;
begin
  result := TXml.CreateAsString('CobolOperations', '');
  for x := 0 to CobolWsdl.Services.Services[0].Operations.Count - 1 do
  begin
    xOperation := CobolWsdl.Services.Services[0].Operations.Operations[x];
    with result.AddXml(TXml.CreateAsString('Operation', '')) do
    begin
      AddXml (TXml.CreateAsString('Name', xOperation.Name));
      if xOperation.CobolEnvironment = ceTandem then
        AddXml (TXml.CreateAsString('CobolEnvironment', 'Tandem'));
      if xOperation.CobolEnvironment = ceIbmZOs then
        AddXml (TXml.CreateAsString('CobolEnvironment', 'IBM Zos'));
      if Assigned (xOperation.reqBind)
      and (xOperation.reqDescrFilename <> '') then
        with AddXml (TXml.CreateAsString('Req', '')) do
          AddXml(TXml.CreateAsString('DescriptionFile', xOperation.reqDescrFilename));
      if Assigned (xOperation.rpyBind)
      and (xOperation.rpyDescrFilename <> '') then
        with AddXml (TXml.CreateAsString('Rpy', '')) do
          AddXml(TXml.CreateAsString('DescriptionFile', xOperation.rpyDescrFilename));
      if Assigned (xOperation.fltBind)
      and (xOperation.fltDescrFilename <> '') then
        with AddXml (TXml.CreateAsString('Flt', '')) do
          AddXml(TXml.CreateAsString('DescriptionFile', xOperation.fltDescrFilename));
      if xOperation.reqRecognition.Count > 0 then
        AddXml (operationRecognitionXml('reqRecognition', xOperation.RecognitionType, xOperation.reqRecognition));
      if xOperation.rpyRecognition.Count > 0 then
        AddXml (operationRecognitionXml('rpyRecognition', xOperation.RecognitionType, xOperation.rpyRecognition));
    end;
  end;
  result.CheckDownline(True);
end;

function TWsdlProject.swiftMtOperationsXml: TXml;
var
  o, x, f: Integer;
  xOperation: TWsdlOperation;
begin
  result := TXml.CreateAsString('SwiftMtOperations', '');
  for x := 0 to SwiftMtWsdl.Services.Services[0].Operations.Count - 1 do
  begin
    xOperation := SwiftMtWsdl.Services.Services[0].Operations.Operations[x];
    with result.AddXml(TXml.CreateAsString('Operation', '')) do
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
  result.CheckDownline(True);
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

function TWsdlProject.FindOperationOnRequest(aLog: TLog; aDocument, aString: String; aDoClone: Boolean): TWsdlOperation;
var
  o: Integer;
  xOperation: TWsdlOperation;
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
        result := FindXmlOperationOnRequest (aDocument, xXml);
        if not Assigned (result) then
          result := FindCcbOperationOnRequest (aLog, aString);
      end;
    finally
      //ReleaseLock;
    end;
    if not Assigned (result) then
      raise Exception.Create (S_NO_OPERATION_FOUND);
    result.AcquireLock;
    try
      if aDoClone then
        result := TWsdlOperation.Create(result);
      case result.WsdlService.DescriptionType of
        ipmDTFreeFormat: result.FreeFormatReq := aString;
        ipmDTCobol, ipmDTBmtp: (result.reqBind as TIpmItem).BufferToValues (FoundErrorInBuffer, aString);
        ipmDTXml: result.SoapXmlRequestToBindables (xXml, aDoClone);
        ipmDTXsd: result.SoapXmlRequestToBindables (xXml, aDoClone);
        ipmDTWsdl: result.SoapXmlRequestToBindables (xXml, aDoClone);
        ipmDTEmail: result.SoapXmlRequestToBindables (xXml, aDoClone);
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
  aOperation: TWsdlOperation;
  aReply: TWsdlMessage;
  aCorrelationId: String;
  xMessage: String;
  xXml, xMqHeaderXml: TXml;
  xLog: TLog;
  xProcessed: Boolean;
begin
  aMqInterface := Sender as TMqInterface;
  if ((MsgType = MQMT_REQUEST) and (not aMqInterface.browseMqMtRequest))
  or ((MsgType = MQMT_REPLY) and (not aMqInterface.browseMqMtReply))
  or ((MsgType = MQMT_REPORT) and (not aMqInterface.browseMqMtReport))
  or ((MsgType = MQMT_DATAGRAM) and (not aMqInterface.browseMqMtDatagram))
  or ((MsgType = MQMT_SYSTEM_LAST) and True)
  or ((MsgType = MQMT_APPL_FIRST) and True)
  or ((MsgType = MQMT_APPL_LAST) and True)
  then exit;
  xMqHeaderXml := nil;
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
    SaveLog ('', xLog);
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
  xAttatchment: TIdAttachment;
  xText: TIdText;
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

procedure TWsdlProject.IgnoreDataChanged(Sender: TObject);
begin
  stubChanged := True;
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
  xStream: TStringStream;
  xRelatesTo: String;
begin
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
      xLog.RequestBody := httpRequestStreamToString(ARequestInfo, AResponseInfo);
      xLog.InboundBody := xLog.RequestBody;
      xLog.httpParams := ARequestInfo.Params.Text;
      try
        if ARequestInfo.Command = 'GET' then
        begin
          HTTPServerCommandGetGet(AContext, ARequestInfo, AResponseInfo);
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
          AResponseInfo.ContentType := ARequestInfo.ContentType;
          rLog := nil;
          AcquireLogLock;
          try
            if AsynchRpyLogs.Count > 0 then
            begin
              xRelatesTo := _relatesToKey(xLog.RequestBody);
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
                if xLog.Operation.isOneWay
                or xLog.isAsynchronousRequest
                or xLog.isAsynchronousReply then
                  AResponseInfo.ResponseNo := 202;
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
          SaveLog ('', xLog);
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
      aResponseInfo.ContentStream := TStringStream.Create('');
      with TIdCompressorZLib.Create(nil) do
      try
        xStream := TStringStream.Create(aResponseInfo.ContentText);
        try
          if AResponseInfo.ContentEncoding = 'deflate' then
            CompressStreamEx(xStream, aResponseInfo.ContentStream, clDefault, zsZLib);
          if AResponseInfo.ContentEncoding = 'gzip' then
            CompressStreamEx(xStream, aResponseInfo.ContentStream, clDefault, zsGZip);
        finally
          xStream.Free;
        end;
      finally
        Free;
      end;
      aResponseInfo.ContentText := '';
    end;
  end;
end;

procedure TWsdlProject.HTTPServerCreatePostStream(AContext: TIdContext;
  AHeaders: TIdHeaderList; var VPostStream: TStream);
begin
  VPostStream := TStringStream.Create('');
end;

procedure TWsdlProject.HttpServerBmtpCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  xLog: TLog;
  xProcessed: Boolean;
  f: Integer;
  xXml, mXml: TXml;
  xStream: TStringStream;
  s, d: AnsiString;
begin
  {$ifdef windows}
  CoInitialize (nil);
  {$endif}
  try
    if ARequestInfo.Command = 'PUT' then
    begin
      AResponseInfo.ResponseText := 'Bmtp: Http PUT not supported';
      exit;
    end;
    if ARequestInfo.Command = 'GET' then
    begin
      AResponseInfo.ResponseText := 'Bmtp: Http GET not yet supported; might scan the service defs for ...';
      exit;
    end;
    try
      xLog := TLog.Create;
      xLog.InboundTimeStamp := Now;
      xLog.TransportType := ttBmtp;
      xLog.httpCommand := ARequestInfo.Command;
      xLog.httpDocument := ARequestInfo.Document;
      xLog.RequestHeaders := ARequestInfo.RawHeaders.Text;
      with TXml.Create do
      try
        LoadFromString(httpRequestStreamToString(ARequestInfo, AResponseInfo), nil);
        if Name = '' then
          raise Exception.Create('Bmtp: Could not parse message envelope as XML');
        if Name <> 'bmtpEnvelope' then
          raise Exception.Create('Bmtp: No Bmtp envelope found');
        xLog.ServiceName := Items.XmlValueByTag['Service'];
        if xLog.ServiceName = '' then
          raise Exception.Create('Bmtp: Element Service not found');
        xLog.OperationName := Items.XmlValueByTag['Operation'];
        if xLog.OperationName = '' then
          raise Exception.Create('Bmtp: Element Operation not found');
        mXml := Items.XmlItemByTag['Request'];
        if not Assigned (mXml) then
          raise Exception.Create('Bmtp: Element Request not found');
        try
          s := mXml.Value;
          d := Base64DecodeStr(s);
          xLog.RequestBody := d;
        except
          on e: Exception do
            raise Exception.Create('Bmtp: Exception while b64decoding message: ' + e.Message);
        end;
      finally
        Free;
      end;
      xLog.InboundBody := xLog.RequestBody;
      AResponseInfo.ContentType := ARequestInfo.ContentType;
      begin // request
        try

          try
            AResponseInfo.ResponseNo := 200;
            CreateLogReply (xLog, xProcessed, True);
//          xLog.ReplyBody := 'Lars was here';
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
          SaveLog ('', xLog);
        end; // finally request
      end; // request
    finally
      if AResponseInfo.ContentEncoding <> 'identity' then
      begin
  {}{}
        aResponseInfo.ContentStream := TStringStream.Create('');
        with TIdCompressorZLib.Create(nil) do
        try
          xStream := TStringStream.Create(aResponseInfo.ContentText);
          try
            if AResponseInfo.ContentEncoding = 'deflate' then
              CompressStreamEx(xStream, aResponseInfo.ContentStream, clDefault, zsZLib);
  //          DeflateStream(xStream, aResponseInfo.ContentStream, 9);
            if AResponseInfo.ContentEncoding = 'gzip' then
              CompressStreamEx(xStream, aResponseInfo.ContentStream, clDefault, zsGZip);
          finally
            xStream.Free;
          end;
        finally
          Free;
        end;
        aResponseInfo.ContentText := '';
  {}{
        AResponseInfo.ContentEncoding := 'identity';
  {}
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

procedure TWsdlProject.HTTPServerCommandGetGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  procedure _replLocations (aName, aFileName: String; aXml: TXml);
  var
    s, v, xUri, xHostName: String;
    x: Integer;
    xAtt: TXmlAttribute;
    sLoc: TSchemaLocation;
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
          epAtr.Value := 'http://' + GetHostName + ':' + IntToStr(Listeners.httpPort);
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
        raise Exception.CreateFmt('Could not find %s', [aUri]);
      s := ReadStringFromFile(sLoc.FileName);
      LoadFromString(s, nil);
      for x := 0 to Items.Count - 1 do
        _replLocations(sLoc.DocumentName, sLoc.FileName, Items.XmlItems[x]);
      result := Text;
    finally
      Free;
    end;
  end;
  function _prepWsdl(fn: String):String;
  const placeHolder = '--wsdl--';
  var
    s: String;
    sl, dl: TStringList;
    x, w: Integer;
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
      sl.Text := StringReplace(s, '--progName--', _ProgName, [rfReplaceAll]);
      for x := 0 to sl.Count - 1 do
      begin
        if Pos (placeHolder, sl.Strings[x]) > 0 then
          for w := 0 to Wsdls.Count - 1 do with wsdls.Objects[w] as TWsdl do
          begin
            dl.Add(StringReplace(sl.Strings[x], placeHolder, Name, [rfReplaceAll]));
          end
        else
          dl.Add(sl.Strings[x]);
      end;
      result := dl.Text;
    finally
      sl.Free;
      dl.Free;
    end;
  end;
begin
  if (ARequestInfo.Document = '/index.html')
  or (ARequestInfo.Document = '/')
  or (ARequestInfo.QueryParams = 'WSDL')
  or (Copy(ARequestInfo.QueryParams, 1, 4) = 'XSD=') then
  begin
    if not PublishDescriptions then
      raise Exception.CreateFmt('%s is configured not to publish webservicedescriptions, in case you need these descriptions, change the %s project options', [_ProgName, _ProgName]);
  end;
  try
    if (ARequestInfo.Document = '/index.html')
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
          exit;
        end;
      end;
    end;
    AResponseInfo.ContentText := '<html>Een <b>Html</b></html>';
  finally
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

function TWsdlProject.ProcessInboundReply(aLogItem, rLogItem: TLog): String;
// rLogItem is already on display so locking needed on changing
var
  rXml: TXml;
  xMessage: String;
  xOperation: TWsdlOperation;
begin
  result := '';
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

function TWsdlProject.ExceptionStackListString(E: Exception): String;
{$ifndef FPC}
var
  str:TStrings;
begin
  result := '';
  str := TStringList.Create;
  try
    jclDebug.JclLastExceptStackListToStrings(str, True, True, True);
    result := str.Text;
  finally
    str.free;
  end;
{$else}
var
  I: Integer;
  Frames: PPointer;
begin
  result := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    result := result + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  result := result + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    result := result + LineEnding + BackTraceStrFunc(Frames[I]);
{$endif}
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
      SaveLog ('', xLog);
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

procedure TWsdlProject.HttpWebPageServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  function _prepWsdl(s: String):String;
    function _replPath(s:String):String;
    var
      x: Integer;
    begin
      result := '';
      for x := 1 to Length (s) do
        if s[x] = '\' then
          result := ''
        else
          result := result + s[x];
    end;
  var
    o,n : String;
  begin
    result := s;
    o := _replPath(ChangeFileExt(webserviceWsdlFileName, '.xsd'));
    n := 'wsdlStubWebService?XSD';
    result := ReplaceText(result, o, n);
    o := 'localhost:3738';
    n := GetHostName + ':' + IntToStr(MasterPortNumber);
    result := ReplaceText(result, o, n);
  end;
  function _prepXsd(fn: String):String;
  const reqs = '--requestElementNames--';
  const scrpts = '--scriptNames--';
  var
    s: String;
    sl, dl: TStringList;
    x, o: Integer;
  begin
    result :='';
    sl := TStringlist.Create;
    dl := TStringList.Create;
    try
      sl.LoadFromFile(fn);
      for x := 0 to sl.Count - 1 do
      begin
        if Pos (reqs, sl.Strings[x]) > 0 then
          for o := 0 to allOperations.Count - 1 do
            dl.Add(ReplaceText(sl.Strings[x], reqs, allOperations.Strings[o]))
        else
        begin
          if Pos (scrpts, sl.Strings[x]) > 0 then
            for o := 0 to Scripts.Count - 1 do
              dl.Add(ReplaceText(sl.Strings[x], scrpts, Scripts.Strings[o]))
          else
            dl.Add(sl.Strings[x]);
        end;
      end;
      result := dl.Text;
    finally
      sl.Free;
      dl.Free;
    end;
  end;
  function _prepHttp(fn: String):String;
  const rops = '--operationNames--';
  var
    sl, dl: TStringList;
    x, o, s: Integer;
  begin
    result :='';
    sl := TStringlist.Create;
    dl := TStringList.Create;
    try
      sl.LoadFromFile(fn);
      for x := 0 to sl.Count - 1 do
      begin
        if Pos (rops, sl.Strings[x]) > 0 then
          with webserviceWsdl.Services do
            for s := 0 to Count - 1 do
              for o := 0 to Services[s].Operations.Count - 1 do
                dl.Add(ReplaceText(sl.Strings[x], rops, Services[s].Operations.Operations[o].Name))
        else
          dl.Add(sl.Strings[x]);
      end;
      result := ReplaceText(dl.Text, '.wsdl', '?WSDL');
    finally
      sl.Free;
      dl.Free;
    end;
  end;
var
  xOperId, xErrorMessage, dCorrelation, dSep, xParams: String;
  xXml, oXml, dXml, eXml: TXml;
  xOperation, oOperation, dOperation: TWsdlOperation;
  dRequest: TWsdlMessage;
  xStream: TStringStream;
  xOk: Boolean;
  x, f: Integer;
begin
  AResponseInfo.ContentEncoding := 'identity';
  try
    if ARequestInfo.Document = '/wsdlStubWebService' then
    begin
      try
        xXml := TXml.Create;
        try
          if ArequestInfo.QueryParams = 'WSDL' then
          begin
            AResponseInfo.ContentText := _prepWsdl(ReadStringFromFile(ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), webserviceWsdlFileName)));
            Exit;
          end;
          if ArequestInfo.QueryParams = 'XSD' then
          begin
            AResponseInfo.ContentText := _prepXsd ( ChangeFileExt(ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), webserviceWsdlFileName), '.xsd'));
            Exit;
          end;
          xParams := httpRequestStreamToString(ARequestInfo, AResponseInfo);
          xXml.LoadFromString(xParams, nil);
          if xXml.Name = '' then
            raise Exception.Create('Could not parse: ' + xParams);
          oXml := xXml.FindUQXml('Envelope.Body');
          if not Assigned (oXml) then
            raise Exception.Create('Not a soap message: ' + xParams);
          if oXml.Items.Count = 0 then
            raise Exception.Create('No operation in: ' + xParams);
          xOperId := NameWithoutPrefix (oXml.Items.XmlItems[0].Name);
          xOperation := webserviceWsdl.OperationByRequest[xOperId];
          if not Assigned (xOperation) then
            raise Exception.Create(format ('Unknown Operation: %s (%s)', [xOperId, xParams]));
          oOperation := TWsdlOperation.Create(xOperation); //creates a private copy
          (oOperation.rpyBind as TXml).CheckDownline(True);
          try
            try
              oOperation.RequestStringToBindables(xParams);
              if xOperId = 'clearLogsReq' then
              begin
                OnClearLogEvent(True);
              end;
              if xOperId = 'activateReq' then
              begin
                if not Assigned (OnActivateEvent) then
                  raise Exception.Create('activateReq refused because ' + _progName + ' has no OnActivateCommand procedure assigned');
                dXml := oXml.FindUQXml('Body.activateReq.Activate');
                if not Assigned (dXml) then
                  raise Exception.Create('Missing Activate/Deactivate argument (boolean) in request');
                OnActivateEvent (dXml.ValueAsBoolean);
              end;
              if xOperId = 'openProjectReq' then
              begin
                if not Assigned (OnOpenProjectEvent) then
                  raise Exception.Create('openProjectReq refused because ' + _progName + ' has no OnOpenProjectRequested procedure assigned');
                if IsActive then
                  raise Exception.Create('openProjectReq refused because ' + _progName + ' is active');
                dXml := oXml.FindUQXml('Body.openProjectReq.projectFileName');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find filename to use in request');
                OnOpenProjectEvent (dXml.Value);
              end;
              if xOperId = 'regressionReportReq' then
              begin
                dXml := oXml.FindUQXml('Body.regressionReportReq.referenceFileName');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find filename to use in request');
                oXml := MessagesRegressionReportAsXml (ExpandRelativeFileName(projectFileName,dXml.Value));
                oXml.Name := (oOperation.rpyBind as TXml).Items.XmlItems[0].Name;
                try
                  (oOperation.rpyBind as TXml).Items.XmlItems[0].ResetValues;
                  (oOperation.rpyBind as TXml).Items.XmlItems[0].LoadValues(oXml, False, False);
                finally
                  oXml.Free;
                end;
              end;
              if xOperId = 'resetEnvVarReq' then
              begin
                dXml := oXml.FindUQXml('Body.resetEnvVarReq.Name');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find name to use in request');
                wsdlz.resetEnvVar(dXml.Value);
              end;
              if xOperId = 'resetEnvVarsReq' then
              begin
                dXml := oXml.FindUQXml('Body.resetEnvVarsReq.RegularExpression');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find regular expression to use in request');
                wsdlz.resetEnvVars(dXml.Value);
              end;
              if xOperId = 'saveLogsToFileReq' then
              begin
                dXml := oXml.FindUQXml('Body.saveLogsToFileReq.fileName');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find filename to use in request');
                SaveMessagesLog(ExpandRelativeFileName(projectFileName, dXml.Value));
              end;
              if xOperId = 'sendAllRequestsReq' then
              begin
                dXml := oXml.FindUQXml('Body.sendAllRequestsReq.elementName');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find elementname to use in request');
                if allOperations.Find(dXml.Value, f) then
                  dOperation := allOperations.Operations [f]
                else
                  raise Exception.Create('Cannot find operation based on: ' + dXml.Value);
                if dOperation.StubAction <> saRequest  then
                  raise Exception.Create('sendAllRequestsReq refused because requested operation is not configured as request');
                if not IsActive then
                  raise Exception.Create('sendAllRequestsReq refused because ' + _progName + ' is inactive');
                TProcedureThread.Create (False, Self, ExecuteAllOperationRequests, dOperation);
              end;
              if xOperId = 'sendRequestReq' then
              begin
                dXml := oXml.FindUQXml('Body.sendRequestReq.elementName');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find elementname to use in request');
                if allOperations.Find(dXml.Value, f) then
                  dOperation := allOperations.Operations [f]
                else
                  raise Exception.Create('Cannot find operation based on: ' + dXml.Value);
                if dOperation.StubAction <> saRequest  then
                  raise Exception.Create('Refused because requested operation is not configured as request');
                dXml := oXml.FindUQXml('Body.sendRequestReq.correlationValues');
                if (not Assigned (dXml))
                and (dOperation.CorrelationBindables.Count > 0) then
                  raise Exception.Create('No correlationValues found in request');
                if Assigned (dXml)
                and (dXml.Items.Count <> dOperation.CorrelationBindables.Count)  then
                  raise Exception.Create ( 'Number of correlation items is '
                                         + IntToStr (dXml.Items.Count)
                                         + '; must be '
                                         + IntToStr (dOperation.CorrelationBindables.Count)
                                         );
                dSep := '';
                dCorrelation := '';
                if Assigned (dXml) then
                begin
                  for x := 0 to dXml.Items.Count - 1 do
                  begin
                    dOperation.CorrelationBindables.Bindables [x].Value := dXml.Items.XmlItems [x].Value;
                    dOperation.CorrelationBindables.Bindables [x].Checked := True;
                    dCorrelation := dCorrelation + dSep + dXml.Items.XmlItems [x].Value;
                    dSep := ';';
                  end;
                end;
                dRequest := dOperation.MessageBasedOnRequest;
                if not Assigned (dRequest) then
                  raise Exception.Create ('Could not find message based on correlation: ' + dXml.Text);
                if not IsActive then
                  raise Exception.Create('sendAllRequestsReq refused because ' + _progName + ' is inactive');
                SendMessage (dOperation, dRequest, dCorrelation);
              end;
              if xOperId = 'setEnvVarReq' then
              begin
                dXml := oXml.FindUQXml('Body.setEnvVarReq.Name');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find name to use in request');
                eXml := oXml.FindUQXml('Body.setEnvVarReq.Value');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find value to use in request');
                wsdlz.setEnvVar(dXml.Value, eXml.Value);
              end;
              if xOperId = 'shutDownReq' then
              begin
                OnQuitEvent(True);
              end;
              if xOperId = 'unexpectedValuesReportReq' then
              begin
                oXml := displayedLogs.UnexpectedsAsXml;
                oXml.Name := (oOperation.rpyBind as TXml).Items.XmlItems[0].Name;
                try
                  (oOperation.rpyBind as TXml).Items.XmlItems[0].ResetValues;
                  (oOperation.rpyBind as TXml).Items.XmlItems[0].LoadValues(oXml, False, False);
                finally
                  oXml.Free;
                end;
              end;
              if xOperId = 'executeScriptReq' then
              begin
                if not IsActive then
                  raise Exception.Create('executeScriptReq refused because ' + _progName + ' is inactive');
                dXml := oXml.FindUQXml('Body.executeScriptReq.scriptName');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find scriptname in request');
                if Scripts.Find(dXml.Value, f) then
                  ScriptExecuteText((Scripts.Objects[f] as TStringList).Text)
                else
                  raise Exception.Create('Cannot find script based on: ' + dXml.Value);
              end;
              AResponseInfo.ContentText := oOperation.StreamReply(_progName, True);
            except
              on e: exception do
              begin
                xErrorMessage := e.Message;
                try
                  dXml := oOperation.fltBind as TXml;
                  dXml.CheckDownline(True);
                  dXml.FindUQXml('Faults.*.applicationFault.code').Value := xOperId;
                  dXml.FindUQXml('Faults.*.applicationFault.text').Value := xErrorMessage;
                  AResponseInfo.ContentText := oOperation.StreamFault(_progName, true);
                  AResponseInfo.ResponseNo := 500;
                except
                  AResponseInfo.ContentText := xErrorMessage;
                  AResponseInfo.ResponseNo := 500;
                end;
              end;
            end;
          finally
            oOperation.Free;
          end;
        finally
          FreeAndNil (xXml);
        end;
      except
        on e: exception do
        begin
          AResponseInfo.ContentText := e.Message + #10#13 + ExceptionStackListString(e);
          AResponseInfo.ResponseNo := 500;
        end;
      end;
    end
    else
      AResponseInfo.ContentText := createHtmlResponse (Self, ARequestInfo);
  finally
    if AResponseInfo.ContentEncoding <> 'identity' then
    begin
      aResponseInfo.ContentStream := TStringStream.Create('');
      with TIdCompressorZLib.Create(nil) do
      try
        xStream := TStringStream.Create(aResponseInfo.ContentText);
        try
          if AResponseInfo.ContentEncoding = 'deflate' then
            CompressStreamEx(xStream, aResponseInfo.ContentStream, clDefault, zsZLib);
          if AResponseInfo.ContentEncoding = 'gzip' then
            CompressStreamEx(xStream, aResponseInfo.ContentStream, clDefault, zsGZip);
        finally
          xStream.Free;
        end;
      finally
        Free;
      end;
      aResponseInfo.ContentText := '';
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
    SaveLog ('', xLog);
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

function TWsdlProject.MessagesRegressionReportAsXml(aReferenceFileName: String): TXml;
var
  xLogList: TLogList;
begin
  xLogList := TLogList.Create;
  try
    OpenMessagesLog (aReferenceFileName, True, xLogList);
    result := logDifferencesAsXml ( displayedLogs
                                  , xLogList
                                  , aReferenceFileName
                                  , CompareLogOrderBy
                                  , ignoreDifferencesOn
                                  , ignoreAddingOn
                                  , ignoreRemovingOn
                                  );
  finally
    xLogList.Clear;
    FreeAndNil(xLogList);
  end;
end;

procedure TWsdlProject.OpenMessagesLog(aString: String; aIsFileName: Boolean; aLogList: TLogList);
var
  xXml: TXml;
  swapCursor: TCursor;
  x: Integer;
  xLog: TLog;
  xOperationName: String;
  xMessageName: String;
  xCorrelationId: String;
begin
  try
    SwapCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
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
        if xXml.Items.XmlValueByTag['wsdlStub'] <> projectFileName then
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
            xLog.TransportType := TTransportType (StrToIntDef (Items.XmlValueByTag ['TransportType'], 0));
            xLog.StubAction := TStubAction (StrToIntDef (Items.XmlValueByTag ['StubAction'], 0));
            xLog.CorrelationId := Items.XmlValueByTag ['CorrelationId'];
            xOperationName := Items.XmlValueByTag ['Operation'];
            xMessageName := Items.XmlValueByTag ['Reply'];
            // Reply
            xLog.Exception := Items.XmlValueByTag ['Error'];
            xLog.Remarks := Items.XmlValueByTag ['Remarks'];
            // Script
            xLog.RequestHeaders := Items.XmlValueByTag ['HttpRequestHeaders'];
            xLog.RequestBody := Items.XmlValueByTag ['HttpRequestBody'];
            xLog.RequestBodyMiM := Items.XmlValueByTag ['HttpRequestBodyMiM'];
            xLog.ReplyBody := Items.XmlValueByTag ['HttpReplyBody'];
            xLog.ReplyBodyMiM := Items.XmlValueByTag ['HttpReplyBodyMiM'];
            xLog.RequestValidated := Items.XmlBooleanByTag ['RequestValidated'];
            xLog.RequestValidateResult := Items.XmlValueByTag ['RequestValidateResult'];
            xLog.ReplyValidated := Items.XmlBooleanByTag ['ReplyValidated'];
            xLog.ReplyValidateResult := Items.XmlValueByTag ['ReplyValidateResult'];
            xLog.CorrId := Items.XmlValueByTag ['Check'];
            xLog.ServiceName := Items.XmlValueByTag ['ServiceName'];
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
            except
            end;
            if Assigned (xLog.Operation) then
              xLog.Mssg := xLog.Operation.MessageBasedOnRequest;
            LogFilter.Execute (xLog);
            aLogList.SaveLog ('', xLog);
          end;
        end;
      end; // for each xml
    finally
      FreeAndNil (xXml);
    end;
  finally
    Screen.Cursor := SwapCursor;
  end;
end;

procedure TWsdlProject.SaveMessagesLog(aFileName: String);
var
  swapCursor: TCursor;
begin
  SwapCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
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
  finally
    Screen.Cursor := swapCursor;
  end;
end;

procedure TWsdlProject.SaveLog(aString: String; aLog: TLog);
begin
  if not Assigned (aLog) then Exit;
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
  while Scripts.Count > 0 do
  begin
    Scripts.Objects[0].Free;
    Scripts.Delete(0);
  end;
end;

procedure TWsdlProject.ScriptExecute(aScript: String);
begin
  if not IsActive then
    raise Exception.Create(Format('%s not active', [_progName]));
  with CreateScriptOperation(aScript) do
  try
    if PreparedBefore then
    try
      ExecuteBefore;
    except
      on e: Exception do
        LogServerMessage(e.Message, True, e);
    end;
  finally
    FreeAndNil(Data);
    FreeAndNil(Wsdl);
    Free;
  end;
end;

procedure TWsdlProject.ScriptExecuteText(aText: String);
begin
  TProcedureThread.Create(False, self, ScriptExecute, aText);
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
var
  o, m: Integer;
begin
  AcquireLock;
  try
    allOperations.Clean;
  finally
    ReleaseLock;
  end;
end;

procedure TWsdlProject.Clear;
begin
  displayedLogs.Clear;
  archiveLogs.Clear;
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
      Wsdls.Objects[0].Free;
    Wsdls.Delete(0);
  end;
  wsdls.Clear;
  wsdlNames.Clear;
  ScriptsClear;
  DisplayedLogColumns.Clear;
  xsdElementsWhenRepeatable := 1;
  stubRead := False;
  projectFileName := '';
  InitSpecialWsdls;
end;

function TWsdlProject.ClearLogCommand(aDoRaiseExceptions: Boolean): String;
begin
  raise Exception.Create('TWsdlProject.ClearLogCommand(aDoRaiseExceptions: Boolean): String;  should be overloaded');
end;

function TWsdlProject.ReactivateCommand: String;
begin
  raise Exception.Create('TWsdlProject.ReactivateCommand: String;  should be overloaded');
end;

function TWsdlProject.QuitCommand(aDoRaiseExceptions: Boolean): String;
begin
  raise Exception.Create('TWsdlProject.QuitCommand(aDoRaiseExceptions: Boolean): String;  should be overloaded');
end;

function TWsdlProject.RestartCommand: String;
begin
  raise Exception.Create('TWsdlProject.RestartCommand: String;  should be overloaded');
end;

procedure TWsdlProject.ReleaseLogLock;
begin
  fLogLock.Release;
end;

function TWsdlProject.ReloadDesignCommand: String;
begin
  raise Exception.Create('TWsdlProject.ReloadDesignCommand: String;  should be overloaded');
end;

procedure TWsdlProject.InitMasterServer;
begin
  if MasterPortNumber <> HttpWebPageServer.DefaultPort then
  begin
    HttpWebPageServer.Active := False;
    HttpWebPageServer.DefaultPort := MasterPortNumber;
  end;
  if not HttpWebPageServer.Active then
  begin
    HttpWebPageServer.Bindings.Clear;
    with HttpWebPageServer.Bindings.Add do
    begin
      Port := MasterPortNumber;
      IP := '0.0.0.0';
    end;
  end;
  if (isMasterModeEnabled <> HttpWebPageServer.Active) then
  begin
    if isMasterModeEnabled
    { and (not isSlaveMode) }then
    begin
      try
        HttpWebPageServer.SessionState := False;
        HttpWebPageServer.Active := true;
        if Assigned (Notify) then
          Notify(format('Listening for HTTP connections on %s:%d.',[HttpWebPageServer.Bindings[0].IP, HttpWebPageServer.Bindings[0].Port]));
      except
        on e: exception do
        begin
          LogServerMessage (format('Exception %s in Activate. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
        end;
      end;
    end
    else
    begin
      HttpWebPageServer.Active := false;
      HttpWebPageServer.Intercept := nil;
      Notify('Stopped listening.');
    end;
  end;
end;

procedure TWsdlProject.InitSpecialWsdls;
begin
  FreeFormatService := TWsdlService.Create;
  with FreeFormatService do
  begin
    Name := '_FreeFormat';
    DescriptionType := ipmDTFreeFormat;
  end;
  FreeFormatWsdl := TWsdl.Create(1, 1, False);
  with FreeFormatWsdl do
  begin
    Name := '_Freeformat';
    isSoapService := False;
    Services.AddObject(FreeFormatService.Name, FreeFormatService);
  end;
  CobolWsdl := TWsdl.Create(1, 1, False);
  with CobolWsdl do
  begin
    Name := '_Cobol';
    isSoapService := False;
    Services.Add(Name);
    Services.Objects[0] := TWsdlService.Create;
    Services.Services[0].Name := Name;
    Services.Services[0].DescriptionType := ipmDTCobol;
  end;
  XsdWsdl := TWsdl.Create(1, 1, False);
  with XsdWsdl do
  begin
    Name := '_Xsd';
    isSoapService := False;
    Services.Add(Name);
    Services.Objects[0] := TWsdlService.Create;
    Services.Services[0].Name := Name;
    Services.Services[0].DescriptionType := ipmDTXsd;
  end;
  SwiftMtWsdl := TWsdl.Create(1, 1, False);
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

// TWsdlProject
initialization
  _WsdlAddRemark := AddRemark;
  _WsdlRequestOperation := RequestOperation;
  _WsdlSendOperationRequest := SendOperationRequest;
  _WsdlSendOperationRequestLater := SendOperationRequestLater;
  _WsdlRefuseHttpConnections := doRefuseHttpConnections;
  _ProgName := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(ParamStr(0)), '');
  IntrospectIniXml;


finalization

end.

