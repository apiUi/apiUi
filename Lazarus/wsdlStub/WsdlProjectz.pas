{
    This file is part of the apiUi project
    Copyright (c) 2009-201 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

// currently at most 1 project due to what's in wsdlz.initialize, should be held by project
unit WsdlProjectz;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes
  {$IFnDEF FPC}
   , AdoDb
  {$ELSE}
  {$ENDIF}
   , ParserClasses
   , xmlio
   , Xmlz
   , Xsdz
   , Bind
   , Ipmz
   , IpmTypes
   , StringListListUnit
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
  , IdURI
  , IdSSLOpenSSL
  , IdContext
  , IdCommandHandlers
  , IdMultipartFormData
  , Wsdlz
  , SysUtils
  , StompInterface
  , StompTypes
  , IdHTTPServer
  , Listenerz
  , LazFileUtils
  , FileUtil
  , Logz
  , snapshotz
  , ExceptionLogz
  , SyncObjs
  , ClaimListz
{$ifndef NoGUI}
  , Forms
  , Dialogs
  , Controls
{$endif}
  , ProgressInterface
  ;

type TCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);
type TCORSType = (corsException, corsRefuse, corsAcceptWithoutLogging, corsAcceptWithLogging);
type TProcedure = procedure of Object;
type TProcedureB = procedure (arg: Boolean) of Object;
type TProcedureS = procedure (arg: String) of Object;
type TProcedureSS = procedure (arg, arg2: String) of Object;
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
    fIsBusy: Boolean;
    fAbortPressed: Boolean;
    fLogLock: TCriticalSection;
    fClearedLogs: TLogList;
    fClearedSnapshots: TSnapshotList;
    fProjectContext: String;
    function GetAbortPressed: Boolean;
    function getDoClearSnapshots : Boolean ;
    function getDoClearLogs : Boolean ;
    function gethasApiByExplampleOperations: Boolean;
    function gethasCobolOperations: Boolean;
    function gethasFormalOperations: Boolean;
    function gethasFreeformatOperations: Boolean;
    function getHasOneTimeContextsColumn: Boolean;
    function getIsBusy: Boolean;
    function getRemoteServerUrl: String;
    function getVersionInfoAsString: String;
    function SendNoneMessage ( aOperation: TWsdlOperation
                             ; aMessage: String
                             ; aLog: TLog
                             ): String;
    function SendHttpMessage ( aOperation: TWsdlOperation; aLog: TLog): String;
    procedure HTTPProxyServerAfterCommandHandler(ASender: TIdCmdTCPServer;
      AContext: TIdContext);
    procedure HTTPProxyServerHTTPBeforeCommand(
      AContext: TIdHTTPProxyServerContext);
    procedure HTTPProxyServerHTTPDocument(
      AContext: TIdHTTPProxyServerContext; var VStream: TStream);
    procedure RemoveStdHttpHeaders (aHeaderList: TIdHeaderList);
    procedure HTTPServerRemoteControlApi ( AContext: TIdContext
                                         ; ARequestInfo: TIdHTTPRequestInfo
                                         ; AResponseInfo: TIdHTTPResponseInfo
                                         );
    procedure HTTPServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HTTPServerCreatePostStream(AContext: TIdContext;
      AHeaders: TIdHeaderList; var VPostStream: TStream);
    procedure setIsBusy(AValue: Boolean);
    procedure SetProjectContext(AValue: String);
    function tryToProcessAsOpenApi (aLog: TLog): Boolean;
    procedure HTTPServerCommandTrace(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure setDoClearSnapshots (AValue : Boolean );
    procedure setDoClearLogs (AValue : Boolean );
    procedure doCoverageReport (aReport: TSnapshot);
    procedure SetAbortPressed(const Value: Boolean);
    procedure InitSpecialWsdls;
    function NumberOfOperationsAndMessages: Integer;
    procedure ProgressBegin (aCaption: String; aMax: Integer);
    procedure ProgressUpdate (aAction: String; aPos: Integer);
    procedure ProgressStep (aAction: String; aInc: Integer);
    procedure ProgressInvalidateConsole;
    procedure ProgressException (E: Exception);
    procedure ProgressEnd;
  public
    majorVersion, minorVersion, revision, build: Integer;
    hasGui: Boolean;
    ProgressInterface: TProgressInterface;
    EditContexts: TThreadMethod;
    doStartOnOpeningProject: Boolean;
    projectContexts: TStringListList;
    ppLock: TCriticalSection;
    doDisplayLog: Boolean;
    uiInvalid: Boolean;
    remoteServerConnectionEnabled: Boolean;
    ProgressMax, ProgressPos: Integer;
    OnRequestViolatingAddressPath: TOnRequestViolating;
    inboundRequestSchemaValidationType, outboundReplySchemaValidationType, outboundRequestSchemaValidationType, inboundReplySchemaValidationType: TSchemaValidationType;
    schemaValidationVioloationHttpResponseCode: Integer;
    remoteServerConnectionXml, DatabaseConnectionSpecificationXml, UnknownOpsReqReplactementsXml, UnknownOpsRpyReplactementsXml: TXml;
    DbsDatabaseName, DbsType, DbsHostName, DbsParams, DbsUserName, DbsPassword, DbsConnectionString: String;
    FreeFormatWsdl, ApiByExampleWsdl, CobolWsdl: TWsdl;
    FreeFormatService: TWsdlService;
    DebugOperation: TWsdlOperation;
    Wsdls, wsdlNames, referencedFilenames: TJBStringList;
    PathInfos, PathRegexps, PathFormats: TJBStringList;
    Scripts: TXml;
    DisplayedLogColumns: TJBStringList;
    projectFileName, remoteProjectName, LicenseDbName: String;
    displayedExceptions, toDisplayExceptions: TExceptionLogList;
    displayedLogs, toDisplayLogs, toUpdateDisplayLogs, archiveLogs: TLogList;
    displayedSnapshots, toDisplaySnapshots: TSnapshotList;
    displayedLogsmaxEntries: Integer;
    CompareLogOrderBy: TCompareLogOrderBy;
    ShowLogCobolStyle: TShowLogCobolStyle;
    LogFilter: TLogFilter;
    refreshNr: Integer;
    refreshCheck: String;
    scriptErrorCount: Integer;
    EnvironmentList, EnvVars: TJBStringList;
    StubChanged, StubRead, Licensed: Boolean;
    StompInterface: TStompInterface;
    Listeners: TListeners;
    doValidateRequests, doValidateReplies: Boolean;
    ignoreDifferencesOn, checkValueAgainst, ignoreAddingOn, ignoreRemovingOn, ignoreOrderOn, regressionSortColumns: TJBStringList;
    ignoreCoverageOn: TJBStringList;
    notStubbedExceptionMessage: String;
    FoundErrorInBuffer : TOnFoundErrorInBufferEvent;
    OnDebugOperationEvent: TOnEvent;
    OnStartBlockingThread, OnTerminateBlockingThread, OnStartNonBlockingThread, OnTerminateNonBlockingThread: TOnEvent;
    Notify: TOnNotify;
    LogServerMessage: TOnStringEvent;
    doViaProxyServer: Boolean;
    ViaProxyServer: String;
    ViaProxyPort: Integer;
    HTTPServer, HttpServerSSL: TIdHTTPServer;
    HTTPProxyServer: TIdHTTPProxyServer;
    ServerOpenSSL: TIdServerIOHandlerSSLOpenSSL;
    OnRestartEvent: TStringFunction;
    OnReactivateEvent: TStringFunction;
    OnReloadDesignEvent: TStringFunction;
    OperationsWithEndpointOnly: Boolean;
    SaveRelativeFileNames: Boolean;
    CurrentFolder, ReferenceFolder, ReportsFolder: String;
    OnBooleanDialog: TBooleanFunctionString;
    OnQuitEvent: TStringFunctionBoolean;
    LastFocusedOperation: TWsdlOperation;
    CORS: TCORSType;
    corsExceptionHttpCode: Integer;
    procedure OnBeforeFileRead (aFileName: String);
    procedure doRegressionReport (aReport: TSnapshot);
    procedure DatabaseConnectionSpecificationFromXml;
    procedure UpdateOperationAliasses;
    procedure LogsFromRemoteServer;
    procedure AcquireLogLock;
    procedure ReleaseLogLock;
    procedure DisplayLog (aString: String; aLog: TLog);
    procedure DisplayReport (aString: String; aReport: TSnapshot);
    procedure WriteStringToStream (aString: String; aStream: TStream);
    function isSpecialWsdl(aWsdl: TWsdl): Boolean;
    function doValidateOutboundRequests (aOperation: TWsdlOperation): Boolean;
    function doValidateInboundReplies (aOperation: TWsdlOperation): Boolean;
    function doValidateInboundRequests (aOperation: TWsdlOperation): Boolean;
    function doReturnExceptionOnViolatingInboundRequest (aOperation: TWsdlOperation): Boolean;
    function doValidateOutboundReplies (aOperation: TWsdlOperation): Boolean;
    procedure UpdateWsdlsList (aNewWsdlsList: TJBStringList);
    function mergeUri (puri, suri: String): String;
    function freeFormatOperationsXml: TXml;
    procedure freeFormatOperationsUpdate (aXml: TXml);
    procedure operationRecognitionUpdate (aOperation: TWsdlOperation; aList: TJBStringList; aXml: TXml);
    function operationRecognitionXml(aLabel: String; aType: TRecognitionType; aSl: TJBStringList): TXml;
    function cobolOperationsXml: TXml;
    procedure cobolOperationsUpdate (aXml: TXml; aMainFileName: String);
    function ApiByExampleOperationsXml(aMainFileName: String): TXml;
    procedure ApiByExampleOperationsUpdate (aXml: TXml; aMainFileName: String);
    function CreateScriptOperation (aScript: TXml): TWsdlOperation;
    procedure ScriptExecute(aScript: TObject);
    function FindSnapshot (aName: String): TSnapshot;
    function UpsertSnapshot (aName, aFileName, aRefFileName: String; aDoClearLoggedOnes: Boolean): TSnapshot;
    function CreateSnapshot (aName, aFileName, aRefFileName: String; aDoSave, aDoRun: Boolean): TSnapshot;
    procedure CreateJUnitReport (aName: String);
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
    function MessagesRegressionReportAsXml(aReferenceFileName: String; aPromptUser: Boolean): TXml;
    function InformationAsXml: TXml;
    function EnvVarsAsXml: TXml;
    function remoteServerConnectionAsXml: TXml;
    procedure remoteServerConnectionFromXml (aXml: TXml);
    procedure EnvVarsFromXml (aXml: TXml);
    procedure UpdateMessageRow (aOperation: TWsdlOperation; aMessage: TWsdlMessage);
    procedure DelayMS (aDelayMS: Integer);
    procedure CreateLogReply (aLog: TLog; var aProcessed: Boolean; aIsActive: Boolean);
    procedure Clean;
    procedure SaveWithFolders;
    procedure ExportToFile;
    procedure ImportFromFile;
    procedure OpenFromFolders;
    procedure OpenFromServerUrl;
    procedure OpenProjectFromString (aString: String);
    procedure IntrospectProject;
    function XmlFromProjectFolders (aFolderName: String): TXml;
    function ProjectDesignAsXml: TXml;
    function ProjectDesignAsString: String;
{}
{}
    function SendOperationMessage ( aOperation: TWsdlOperation
                         ; aMessage: String
                         ): String;
    function SendOperationStompMessage ( aOperation: TWsdlOperation
                                       ; aMessage: String
                                       ; var aRequestHeader: String
                                       ; var aReplyHeader: String
                                       ): String;
    procedure SendOperationInThread (aOperation: TWsdlOperation);
    procedure SendOperation (aOperation: TWsdlOperation);
    procedure SendMessage ( aOperation: TWsdlOperation
                             ; aRequest: TWsdlMessage
                             ; aCorrelationId: String
                             );
    function SendMessageLater ( aOperation: TWsdlOperation
                                  ; aRequest: TWsdlMessage
                                  ; aCorrelationId: String
                                  ; aLater: Integer
                                  ): String;
    procedure FindRequestReply (aLog: TLog; aDocument, aString: String; var isRequest: Boolean);
    function FindXmlOperationOnReply (aXml: TXml): TWsdlOperation; Overload;
    function FindCcbOperationOnReply (aCobolString: String): TWsdlOperation; Overload;
    function FindOperationOnReply (aString: String): TWsdlOperation; Overload;
    function FindXmlOperationOnRequest (aDocument: String; aXml: TXml): TWsdlOperation;
    function FindCcbOperationOnRequest (aLog: TLog; aCobolString: String): TWsdlOperation;
    function FindOperationOnDocument (aDocument: String): TWsdlOperation;
    function FindOperationOnRequest (aLog: TLog; aDocument, aString: String; aDoClone: Boolean): TWsdlOperation;
    function FindOpenApiOnLog (aLog: TLog): TWsdlOperation;
    function FindOperationOnLog (aLog: TLog): TWsdlOperation;
    procedure CreateReply ( aLog: TLog; aIsActive: Boolean);
    function ProjectLogOptionsAsXml: TXml;
    procedure RefreshCommand;
    function ProjectOptionsLogDisplayedColumnsAsXml: TXml;
    function BooleanPromptDialog (aPrompt: String): Boolean;
    function WsdlOpenFile (aName: String): TWsdl;
    procedure SaveLogs (aFileName: String);
    procedure UpdateReplyColumns (aOperation: TWsdlOperation);
    procedure ProjectOptionsLogDisplayedColumnsFromXml(aXml: TXml);
    procedure ProjectLogOptionsFromXml(aXml: TXml);
    function ProjectOptionsAsXml (aRelativeFilenames: Boolean; aFileName: String): TXml;
    procedure ProjectOptionsFromXml(aXml: TXml);
    procedure HaveStompFrame (aStompInterface: TStompInterface; aQueue: String; aFrame: IStompFrame);
    procedure ProjectDesignFromXml (aXml: TXml; aMainFileName: String);
    procedure ProjectDesignFromString (aString, aMainFileName: String);
    procedure ProjectDesignFromApiRequestString (aString, aMainFileName: String);
    procedure PrepareAllOperationsShowingProgress;
    procedure PrepareAllOperations;
    procedure Activate (aActive: Boolean);
    procedure Clear;
    function httpRequestStreamToString(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): String;
    {$IFnDEF FPC}
    procedure ADOConnectionWillConnect(Connection: TADOConnection;
      var ConnectionString, UserID, Password: WideString;
      var ConnectOptions: TConnectOption; var EventStatus: TEventStatus);
    {$ENDIF}
    procedure LogServerException(const Msg: String; aException: Boolean; E: Exception);
    property ProjectContext: String read fProjectContext write SetProjectContext;
    property versionInfoAsString: String read getVersionInfoAsString;
    property RemoteServerUrl: String read getRemoteServerUrl;
    property doClearLogs: Boolean read getDoClearLogs write setDoClearLogs;
    property doClearSnapshots: Boolean read getDoClearSnapshots write setDoClearSnapshots;
    property IsActive: Boolean read fIsActive;
    property abortPressed: Boolean read fAbortPressed write SetAbortPressed;
    property hasOneTimeContextsColumn: Boolean read getHasOneTimeContextsColumn;
    property hasFormalOperations: Boolean read gethasFormalOperations;
    property hasApiByExplampleOperations: Boolean read gethasApiByExplampleOperations;
    property hasCobolOperations: Boolean read gethasCobolOperations;
    property hasFreeformatOperations: Boolean read gethasFreeformatOperations;
    property isBusy: Boolean read getIsBusy write setIsBusy;
    constructor Create;
    destructor Destroy; Override;
  end;

  { TProcedureThread }

  TProcedureThread = class(TThread)
  private
    fProject: TWsdlProject;
    fProcedure: TProcedure;
    fProcedureS: TProcedureS;
    fProcedureSS: TProcedureSS;
    fProcedureXX: TProcedureXX;
    fProcedureOperation: TProcedureOperation;
    fProcedureObject: TProcedureObject;
    fProcedureClaimableObjectList: TProcedureClaimableObjectList;
    fString, fString2: String;
    fExtended, fExtended2: Extended;
    fOperation: TWsdlOperation;
    fObject: TObject;
    fClaimableObjectList: TClaimableObjectList;
    fBlocking: Boolean;
    fOnFinished: TOnEvent;
    fPostponementMs: Integer;
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
                       ; aPostponementMs: Integer
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedureSS
                       ; aString: String
                       ; aString2: String
                       ); overload;
    constructor Create ( aSuspended: Boolean
                       ; aBlocking: Boolean
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedureXX
                       ; aExtended, aExtended2: Extended
                       ); overload;
    constructor CreateProcedureOperation ( aSuspended: Boolean
                       ; aBlocking: Boolean
                       ; aProject: TWsdlProject
                       ; aProcedure: TProcedureOperation
                       ; aOperation: TWsdlOperation
                       );
    constructor Create ( aSuspended: Boolean
                       ; aBlocking: Boolean
                       ; aPostponementMs: Integer
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

procedure IntrospectIniXml;

var
  DebugLogMode: Boolean;
  apiUiXsdFileName, swaggerYamlFileName, faviconIcoFileName: String;
    indexHtmlFileName: String;
    wsaXsdFileName: String;
    authorizationServerEndpoint: String;
    stompPutHeaderEditAllowedFileName: String;
    RemoteControlPortNumber: Integer;
    wsaXsdDescr: TXsdDescr;
    namevaluepairsXsd: TXsd;
    optionsXsd: TXsd;
    remoteServerConnectionXsd: TXsd;
    apiUiXsdDescr: TXsdDescr;
    ScriptsXsd: TXsd;
    OperationDefsXsd: TXsd;
    projectOptionsXsd: TXsd;
    serviceOptionsXsd: TXsd;
    listenersConfigXsd: TXsd;
    operationOptionsXsd: TXsd;

const _ProjectOldFileExtention = '.wsdlStub';
const _ProjectFileExtention = '.svpr';
const _ProjectFileName = '_Project.xml';
const _WsdlFileName = '_Wsdl.xml';
const _ServiceFileName = '_Service.xml';
const _OperationFileName = '_Operation.xml';
const _MessageFileName = '_Message.xml';
const _ScriptFileName = '_Script.xml';
const _ContextsFileName = '_Contexts.xml';

implementation

uses LazVersion
   , StrUtils
   , exceptionUtils
   , RegExpr
   , base64
   {$ifdef windows}
   , ActiveX
   {$endif}
   {$ifndef NoGUI}
   , OpenWsdlUnit
   , xmlUtilz
   {$endif}
   , wrdFunctionz
   , GZIPUtils
   , htmlxmlutilz
   , htmlreportz
   , junitunit
   , IdGlobalProtocols
   , Clipbrd
   , httpmultipart
   ;

const ReadmeFilename = 'readme.txt'
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

procedure RequestOperationLater(aContext: TObject; xOperationAlias: String; aLater: Extended);
var
  xProject: TWsdlProject;
  xOperation, yOperation: TWsdlOperation;
begin
  xProject := nil; //candidate context
  xOperation := nil; //candidate context
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
   raise Exception.Create(Format ('RequestOperationLater: Operation ''%s'' not found', [xOperationAlias]));
  yOperation := TWsdlOperation.Create(xOperation);
  with yOperation do
  begin
    CorrelatedMessage := Messages.Messages[0];
    if reqBind is TXml then
    begin
      reqXml.ResetValues;
      reqXml.LoadValues (xOperation.reqXml, False, True);
    end;
    if reqBind is TIpmItem then
      ReqIpm.BufferToValues(nil, xOperation.ReqIpm.ValuesToBuffer(nil));
    if Assigned (requestInfoBind) then with requestInfoBind as TXml do
    begin
      ResetValues;
      LoadValues (xOperation.requestInfoBind as TXml, False, True);
    end;
    StubAction := saRequest;
    PostponementMs := Trunc (aLater);
    FreeOnTerminateRequest := True;
  end;
  xProject.SendOperationInThread(yOperation);
end;

procedure FetchDefaultDesignMessage(aContext: TObject; xOperationAlias: String);
var
  sOperation, dOperation: TWsdlOperation;
  xMessage: TWsdlMessage;
begin
  if aContext is TWsdlProject then
    raise Exception.Create ('FetchDefaultDesignMessage(aContext: TObject; xOperationAlias: String): Project is illegal context');
  dOperation := nil; //candidate context
  xMessage := nil;
  if aContext is TWsdlOperation then with aContext as TWsdlOperation do
  begin
    if Alias = xOperationAlias then
      dOperation := aContext as TWsdlOperation
    else
      dOperation := invokeList.FindOnAliasName(xOperationAlias);
  end;
  if not Assigned (dOperation) then
   raise Exception.Create(Format ('FetchDefaultDesignMessage: Operation ''%s'' not found', [xOperationAlias]));
  sOperation := dOperation;
  while Assigned (sOperation.Cloned) do
    sOperation := sOperation.Cloned;
  if sOperation.Messages.Count > 0 then
  begin
    xMessage := sOperation.Messages.Messages[0];
    if dOperation.reqBind is TIpmItem then
      dOperation.ReqIpm.LoadValues (xMessage.ReqIpm);
    if dOperation.rpyBind is TIpmItem then
      dOperation.RpyIpm.LoadValues (xMessage.RpyIpm);
    if dOperation.reqBind is TXml then
      dOperation.reqXml.LoadValues (xMessage.reqXml, True, True);
    if dOperation.rpyBind is TXml then
      dOperation.rpyXml.LoadValues (xMessage.rpyXml, True, True);
  end;
end;

procedure ExecSql(aContext: TObject; aSqlCommand: String);
var
  xProject: TWsdlProject;
  xOperation: TWsdlOperation;
  xMessage: TWsdlMessage;
  x: Integer;
  xDoCommit: Boolean;
begin
  xProject := nil; //candidate context
  xOperation := nil; //candidate context
  xMessage := nil;
  xDoCommit := True;
  if aContext is TWsdlOperation then with aContext as TWsdlOperation do
  begin
    xProject := Owner as TWsdlProject;
  end
  else
  begin
    if aContext is TWsdlProject then
    begin
      xProject := aContext as TWsdlProject;
    end;
  end;
  if not Assigned (xProject) then
   raise Exception.Create('ExecSql(aContext: TObject; aUseTransAction, aSqlCommand: String)');
  if xDoCommit then _WsdlDbsConnector.Transaction.Active := True;
  try
    _WsdlDbsConnector.ExecuteDirect(aSqlCommand);
  finally
    if xDoCommit then _WsdlDbsConnector.Transaction.Commit;
  end;
end;

procedure NewDesignMessage(aContext: TObject; xOperationAlias, aName: String);
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
    xMessage.Name := aName;
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

procedure CreateJUnitReport(aContext: TObject; aName: String);
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
    raise Exception.Create(Format ('CreateJUnitReport(''%s''); unable to determine context', [aName]));
  xProject.CreateJUnitReport (aName);
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

procedure ExecuteScriptLater(aContext: TObject; xScriptName: String; aLater: Extended);
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
    TProcedureThread.Create(False, False, Trunc (aLater), xProject, xProject.ScriptExecute, xScript)
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
  sl: TJBStringList;
begin
  sl := TJBStringList.Create;
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
  sl: TJBStringList;
begin
  sl := TJBStringList.Create;
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
  fProcedureS := aProcedure;
  fString := aString;
end;

constructor TProcedureThread.Create(aSuspended, aBlocking: Boolean; aPostponementMs: Integer; aProject: TWsdlProject; aProcedure: TProcedureSS;
  aString, aString2: String);
begin
  inherited Create (aSuspended);
  fBlocking := aBlocking;
  fPostponementMs := aPostponementMs;
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedureSS := aProcedure;
  fString := aString;
  fString2 := aString2;
end;

constructor TProcedureThread.CreateProcedureOperation(aSuspended, aBlocking: Boolean; aProject: TWsdlProject; aProcedure: TProcedureOperation;
  aOperation: TWsdlOperation);
begin
  inherited Create (aSuspended);
  fBlocking := aBlocking;
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedureOperation := aProcedure;
  fOperation := aOperation;
end;

constructor TProcedureThread.Create(aSuspended, aBlocking: Boolean; aPostponementMs: Integer; aProject: TWsdlProject;
  aProcedure: TProcedureObject; aObject: TObject);
begin
  inherited Create (aSuspended);
  fBlocking := aBlocking;
  FreeOnTerminate := True;
  fProject := aProject;
  fProcedureObject := aProcedure;
  fObject := aObject;
  fPostponementMs := aPostponementMs;
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
  if fPostponementMs > 0 then
    Sleep (fPostponementMs); // for operations still implemented differently
  try
    {$ifdef windows}
    CoInitialize(nil);
    {$endif}
    try
      if Assigned (fProcedure) then fProcedure;
      if Assigned (fProcedureS) then fProcedureS (fString);
      if Assigned (fProcedureSS) then fProcedureSS (fString, fString2);
      if Assigned (fProcedureXX) then fProcedureXX (fExtended, fExtended2);
      if Assigned (fProcedureOperation) then
      begin
        if fOperation.PostponementMs > 0 then
          Sleep (fOperation.PostponementMs);
        fProcedureOperation (fOperation);
        if fOperation.FreeOnTerminateRequest then
          FreeAndNil(fOperation);
      end;
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
  finally
    {$ifdef windows}
    CoUninitialize;
    {$endif}
  end;
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
    result := _replaceInteger(result, '_xsdMaxDepthBillOfMaterials_', defaultXsdMaxDepthBillOfMaterials);
    result := _replaceInteger(result, '_xsdMaxDepthXmlGen_', defaultXsdMaxDepthXmlGen);
  end;

var
  x: Integer;
  Xml, xXml, iniXml: TXml;
  xIniFileName: String;
  function _abs (aFileName: String): String;
  begin
    result := ExpandRelativeFileName(xIniFileName, osDirectorySeparators (aFileName));
  end;
begin
  xIniFileName := Copy(ParamStr(0), 1, Length(ParamStr(0)){$ifdef windows} - 4{$endif}) + 'Ini.xml';
  if not LazFileUtils.FileExistsUTF8(xIniFileName) { *Converted from FileExists* } then
    raise Exception.CreateFmt(
      '%s coud not open expected inifile: %s,%splease install %s properly',
      [_progName, xIniFileName, CRLF, _progName]);
  iniXml := TXml.Create;
  try
    iniXml.LoadFromFile(xIniFileName, nil, nil);
    faviconIcoFileName := _abs (iniXml.Items.XmlValueByTag ['faviconIco']);
    swaggerYamlFileName := _abs(iniXml.Items.XmlValueByTag ['swaggerYaml']);
    indexHtmlFileName := _abs (iniXml.Items.XmlValueByTag ['indexHtml']);
    apiUiXsdFileName := _abs (iniXml.Items.XmlValueByTag ['Xsd']);
    wsaXsdFileName := _abs (iniXml.Items.XmlValueByTag ['wsaXsd']);
    stompPutHeaderEditAllowedFileName := _abs (iniXml.Items.XmlValueByTag ['stompPutHeaderEditAllowed']);
    apiaryToken := DecryptPassword(iniXml.Items.XmlValueByTag ['apiaryToken']);
    RemoteControlPortNumber := iniXml.Items.XmlIntegerByTagDef ['commandPort', 3738];
    xsdMaxDepthBillOfMaterials := defaultXsdMaxDepthBillOfMaterials;
    xsdMaxDepthXmlGen := defaultXsdMaxDepthXmlGen;
    if Assigned (iniXml.ItemByTag ['cssStylesheet']) then with iniXml.ItemByTag ['cssStylesheet'] do
    begin
      _wsdlStubStylesheet := _abs (Value);
    end;
    if wsaXsdFileName <> '' then
    begin
      wsaXsdDescr := TXsdDescr.Create;
      try
        wsaXsdDescr.LoadXsdFromFile (wsaXsdFileName, nil, nil);
        if wsaXsdDescr.TypeDef.ElementDefs.Count > 0 then
          _WsdlWsaXsd := wsaXsdDescr.TypeDef.ElementDefs.Xsds [wsaXsdDescr.TypeDef.ElementDefs.Count - 1];
      except
        raise Exception.Create ('Could not parse ' + wsaXsdFileName);
      end;
    end;

    if apiUiXsdFileName <> '' then
    begin
      apiUiXsdDescr := TXsdDescr.Create;
      try
        apiUiXsdDescr.LoadXsdFromString (_Prep ( apiUiXsdFileName
                                                    , ReadStringFromFile(apiUiXsdFileName, nil)
                                                    )
                                             , nil
                                             , nil
                                             );
      except
        raise Exception.Create (_progName + ' could not parse ' + apiUiXsdFileName);
      end;
    end;
    if not Assigned (apiUiXsdDescr) then
      raise exception.Create('No ' + _progName + ' webservice xsd assigned');

    with apiUiXsdDescr.TypeDef.ElementDefs do
    begin
      _WsdlRtiXsd := XsdByName['rti'];
      namevaluepairsXsd := XsdByName['namevaluepairs'];
      optionsXsd := XsdByName['Options'];
      ScriptsXsd := XsdByName['Scripts'];
      OperationDefsXsd := XsdByName['OperationDefs'];
      projectOptionsXsd := XsdByName['projectOptions'];
      serviceOptionsXsd := XsdByName['serviceOptions'];
      operationOptionsXsd := XsdByName['operationOptions'];
      _WsdlListOfFilesXsd := XsdByName['FileNames'];
      endpointConfigXsd := XsdByName['endpointConfig'];
      replyInfoXsd := XsdByName['replyInfo'];
      remoteServerConnectionXsd := XsdByName['remoteServerConnection'];
      listenersConfigXsd := XsdByName['Listeners'];
    end;
    if not Assigned (ScriptsXsd) then raise Exception.CreateFmt('XML Element definition for %s Scripts not found', [_progName]);
    if not Assigned (_WsdlRtiXsd) then raise Exception.Create('XML Element definition for RunTimeInterface not found');
    if not Assigned (namevaluepairsXsd) then raise Exception.CreateFmt('XML Element definition for %s namevaluepairs not found', [_progName]);
    if not Assigned (optionsXsd) then raise Exception.CreateFmt('XML Element definition for %s Options not found', [_progName]);
    if not Assigned (OperationDefsXsd) then raise Exception.Create('XML Element definition for OperationDefs not found');
    if not Assigned (projectOptionsXsd) then raise Exception.Create('XML Element definition for projectOptions not found');
    if not Assigned (serviceOptionsXsd) then raise Exception.Create('XML Element definition for serviceOptions not found');
    if not Assigned (operationOptionsXsd) then raise Exception.Create('XML Element definition for operationOptions not found');
    if not Assigned (_WsdlListOfFilesXsd) then raise Exception.Create('XML Element definition for FileNames not found');
    if not Assigned (endpointConfigXsd) then raise Exception.Create('XML Element definition for endpointConfig not found');
    if not Assigned (replyInfoXsd) then raise Exception.Create('XML Element definition for replyInfo not found');
    if not Assigned (remoteServerConnectionXsd) then raise Exception.Create('XML Element definition for remoteServerConnection not found');
    if not Assigned (listenersConfigXsd) then raise Exception.Create('XML Element definition for listeners configuration not found');
    if Assigned (_WsdlRtiXsd) then
      _WsdlRtiXml := TXml.Create(-10000, _WsdlRtiXsd);
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
                  Xml.LoadFromFile (stompPutHeaderEditAllowedFileName, nil, nil);
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

    if apiUiXsdFileName <> '' then
      apiUiXsdFileName := ExpandRelativeFileName (ExtractFilePath (ParamStr(0)), apiUiXsdFileName);
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
  doValidateRequests := True;
  doValidateReplies := True;
  OnRestartEvent := RestartCommand;
  OnReactivateEvent := ReactivateCommand;
  OnReloadDesignEvent := ReloadDesignCommand;
  projectContexts := TStringListList.Create;
  projectContexts.RowCount := 1;
  projectContexts.ColCount := 1;
  projectContexts.CellValue[0, 0] := ' ';
  remoteServerConnectionXml := TXml.CreateAsString ('remoteServerConnection', '');
  DatabaseConnectionSpecificationXml := TXml.CreateAsString ('DatabaseConnection', '');
  DatabaseConnectionSpecificationXml.Checked := False;
  UnknownOpsReqReplactementsXml := TXml.CreateAsString ('reqReplacements', '');
  UnknownOpsRpyReplactementsXml := TXml.CreateAsString ('rpyReplacements', '');
  ppLock := TCriticalSection.Create;
  fLogLock := TCriticalSection.Create;
  LogFilter := TLogFilter.Create;
  Wsdls := TJBStringList.Create;
  Wsdls.Sorted := True;
  wsdlNames := TJBStringList.Create;
  wsdlNames.Sorted := True;
  referencedFilenames := TJBStringList.Create;
  referencedFilenames.Sorted := True;
  referencedFilenames.Duplicates := dupIgnore;
  PathInfos := TJBStringList.Create;
  PathRegexps := TJBStringList.Create;
  PathFormats := TJBStringList.Create;
  ignoreDifferencesOn := TJBStringList.Create;
  ignoreDifferencesOn.Sorted := True;
  ignoreDifferencesOn.Duplicates := dupIgnore;
  checkValueAgainst := TJBStringList.Create;
  ignoreAddingOn := TJBStringList.Create;
  ignoreAddingOn.Sorted := True;
  ignoreAddingOn.Duplicates := dupIgnore;
  ignoreRemovingOn := TJBStringList.Create;
  ignoreRemovingOn.Sorted := True;
  ignoreRemovingOn.Duplicates := dupIgnore;
  ignoreOrderOn := TJBStringList.Create;
  ignoreOrderOn.Sorted := True;
  ignoreOrderOn.Duplicates := dupIgnore;
  regressionSortColumns := TJBStringList.Create;
//regressionSortColumns.Sorted := True;
//regressionSortColumns.Duplicates := dupIgnore;
  ignoreCoverageOn := TJBStringList.Create;
  ignoreCoverageOn.Sorted := True;
  ignoreCoverageOn.Duplicates := dupIgnore;
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
  EnvironmentList := TJBStringList.Create;
  EnvironmentList.Sorted := True;
  EnvVars := TJBStringList.Create;
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
  Scripts := TXml.CreateAsString('Scripts', '');
  DisplayedLogColumns := TJBStringList.Create;
  OperationsWithEndpointOnly := True;
  SaveRelativeFileNames := True;
  InitSpecialWsdls;
    {$IFnDEF FPC}
  _WsdlDbsAdoConnection.OnWillConnect := ADOConnectionWillConnect;
    {$endif}
  Clear;
end;

destructor TWsdlProject.Destroy;
begin
  Clear;
  FreeAndNil (HTTPProxyServer);
  FreeAndNil (HttpServer);
  FreeAndNil (HttpServerSSL);
  FreeAndNil (ServerOpenSSL);
  projectContexts.Free;
  remoteServerConnectionXml.Free;
  DatabaseConnectionSpecificationXml.Free;
  UnknownOpsReqReplactementsXml.Free;
  UnknownOpsRpyReplactementsXml.Free;
  ppLock.Free;
  fLogLock.Free;
  Listeners.Free;
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
  Wsdls.Free;
  wsdlNames.Free;
  referencedFilenames.Free;
  PathInfos.Free;
  PathRegexps.Free;
  PathFormats.Free;
  FreeAndNil (FreeFormatWsdl);
  FreeAndNil (CobolWsdl);
  FreeAndNil (ApiByExampleWsdl);
  ignoreDifferencesOn.Free;
  checkValueAgainst.Free;
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

function CompareApiPathNames(List: TStringList; Index1, Index2: Integer): Integer;
  function _doCompare(s1, s2: String):Integer;
  begin
    if s1 > s2 then
    begin
      result := 1;
      Exit;
    end;
    if s1 < s2 then
    begin
      result := -1;
      Exit;
    end;
    result := 0;;
  end;
begin
  result := _doCompare ( List.Strings[Index1] + '~'
                       , List.Strings[Index2] + '~'
                       );
end;

procedure TWsdlProject.PrepareAllOperations;
  procedure _prepWsdl (xWsdl: TWsdl);
  var
    s, o, p: Integer;
    xService: TWsdlService;
    xOperation: TWsdlOperation;
  begin
    wsdlNames.AddObject(xWsdl.Name, xWsdl);
    for s := 0 to xWsdl.Services.Count - 1 do
    begin
      xService := xWsdl.Services.Services[s];
      for p := 0 to xService.PathInfos.Count - 1 do
        self.PathInfos.AddObject (xService.PathInfos.Strings[p], xService);
      for o := 0 to xService.Operations.Count - 1 do
      begin
        xOperation := xService.Operations.Operations [o];
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
          LogServerException ( 'Duplicate operation name (Req) ('
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
    if (    (aWsdl.Services.Count > 0)
        and (aWsdl.Services.Services[0].Operations.Count > 0)
       ) then
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
  w, o, m, s, e: Integer;
  f: Boolean;
  xPathRegexp, xPathFormat: String;
  oStep: Integer;
  xMessage: TWsdlMessage;

begin
  wsdlNames.Clear;
  PathInfos.Clear;
  PathRegexps.Clear;
  PathFormats.Clear;
  allOperations.ClearListOnly;
  allOperationsRpy.ClearListOnly;
  scriptErrorCount := 0;
  _updtWsdls(FreeFormatWsdl);
  _updtWsdls(CobolWsdl);
  _updtWsdls(ApiByExampleWsdl);
  for w := 0 to Wsdls.Count - 1 do
    _prepWsdl (Wsdls.Objects [w] as TWsdl);
    // path may look like /api/something/{aId}/{anotherId}
  PathInfos.CustomSort(CompareApiPathNames);
  with TRegExpr.Create ('\{[^\}]+\}') do
  try
    for w := 0 to PathInfos.Count - 1 do
    begin
      xPathRegexp := '';
      s := 1;
      f := Exec(PathInfos.Strings[w]);
      while f do
      begin
        e := MatchPos[0];
        xPathRegexp := xPathRegexp + Copy (PathInfos.Strings[w], s, e - s) + S_OPEN_API_PATHVALUE_REGEXP;
        s := MatchPos[0] + MatchLen[0];
        f := ExecNext;
      end;
      xPathRegexp := xPathRegexp + Copy (PathInfos.Strings[w], s, Length (PathInfos.Strings[w]));
      xPathFormat := ReplaceStrings (xPathRegexp, S_OPEN_API_PATHVALUE_REGEXP, '%s', False, False);
      xPathRegexp := '^(' + xPathRegexp + ')$';
      PathRegexps.AddObject(xPathRegexp, PathInfos.Objects[w]);
      PathFormats.AddObject(xPathFormat, PathInfos.Objects[w]);
      with PathInfos.Objects[w] as TWsdlService do
      begin
        logPathRegExp := xPathRegexp;
      end;
    end;
  finally
    Free;
  end;
  UpdateOperationAliasses;
  ProgressStep('Preparing operations', 100);
  oStep := 800;
  if allOperations.Count > 0 then
    oStep := oStep div allOperations.Count;
  for o := 0 to allOperations.Count - 1 do with allOperations.Operations[o] do// here since invokeAll
  begin
    ProgressStep('Preparing operations', oStep);
    OnGetAbortPressed := self.GetAbortPressed;
    if reqBind is TXml then with reqBind as TXml do Checked := True;
    if rpyBind is TXml then with rpyBind as TXml do Checked := True;
    if fltBind is TXml then with fltBind as TXml do Checked := True;
    Owner := Self;
    if invokeRequestInfo
    and not Assigned (requestInfoBind) then
    begin
      requestInfoBind := TXml.Create (-10000, endpointConfigXsd);
      requestInfoBind.Name := Alias;
    end;
    if invokeReplyInfo
    and not Assigned (replyInfoBind) then
    begin
      replyInfoBind := TXml.Create (-10000, replyInfoXsd);
      replyInfoBind.Name := Alias;
    end;
    doInvokeOperations;
    try
      PrepareBefore;
    except
    end;
    if (not PreparedBefore) then
      Inc (scriptErrorCount);
    try
      PrepareAfter;
    except
    end;
    if (not PreparedAfter) then
      Inc (scriptErrorCount);
    for m := 0 to Messages.Count - 1 do
    begin
      xMessage := Messages.Messages[m];
      if xMessage.BeforeScriptLines.Count > 0 then
      begin
        xMessage.CheckBefore;
        if not xMessage.PreparedBefore then
          Inc (scriptErrorCount);
      end;
      if xMessage.AfterScriptLines.Count > 0 then
      begin
        xMessage.CheckAfter;
        if not xMessage.PreparedAfter then
          Inc (scriptErrorCount);
      end;
    end;
  end;
  if scriptErrorCount > 0 then
    LogServerException ( IntToStr (scriptErrorCount) + ' Script(s) found with errors', False, nil);
  ProgressStep('Preparing operations', 100);
end;

procedure TWsdlProject.PrepareAllOperationsShowingProgress;
begin
  ProgressBegin('Preparing operations', 1000);
  try
    PrepareAllOperations;
    ProgressInvalidateConsole;
  finally
    ProgressEnd;
  end;
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
    for x := 0 to Listeners.stompInterfaces.Count - 1 do
      (Listeners.stompInterfaces.Objects[x] as TStompInterface).Disconnect;
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
        if Listeners.httpPorts.Count > 0 then
        begin
          for x := 0 to Listeners.httpPorts.Count - 1 do
          begin
            try
              Binding := HTTPServer.Bindings.Add;
              Binding.Port := StrToInt(resolveAliasses(Listeners.httpPorts.Strings[x]));
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
          if Listeners.sslPassword <> '' then
            ServerOpenSSL.OnGetPassword := Listeners.OnGetSslPassword; // TODO resolveAliasses...
          for x := 0 to Listeners.httpsPorts.Count - 1 do
          begin
            try
              Binding := HTTPServerSSL.Bindings.Add;
              Binding.Port := StrToInt(resolveAliasses(Listeners.httpsPorts.Strings[x]));
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
  result.AddXml (TXml.CreateAsString('ProjectContext', ProjectContext));
  with result.AddXml (TXml.CreateAsString('versionInfo', '')) do
  begin
    AddXml (TXml.CreateAsInteger('major', majorVersion));
    AddXml (TXml.CreateAsInteger('minor', minorVersion));
    AddXml (TXml.CreateAsInteger('revision', revision));
    AddXml (TXml.CreateAsInteger('build', build));
  end;
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
    AddXml (TXml.CreateAsBoolean('OperationsWithEndpointOnly', OperationsWithEndpointOnly));
    AddXml (TXml.CreateAsInteger('MaxDepthWhenRecursive', xsdMaxDepthBillOfMaterials));
    AddXml (TXml.CreateAsInteger('MaxDepthXmlGen', xsdMaxDepthXmlGen));
  end;
  with result.AddXml (TXml.CreateAsString('CORS', '')) do
  begin
    if CORS = corsException then
    with AddXml (TXml.CreateAsString('exceptionResponse', '')) do
    begin
      AddXml (TXml.CreateAsInteger('responseCode', corsExceptionHttpCode));
    end;
    if CORS = corsRefuse then
      AddXml (TXml.CreateAsString('refuse', ''));
    if CORS in [corsAcceptWithLogging, corsAcceptWithoutLogging] then
    begin
      with AddXml (TXml.CreateAsString('accept', '')) do
      begin
        AddXml (TXml.CreateAsBoolean('showInLog', CORS = corsAcceptWithLogging));
      end;
    end;;
  end;
  with result.AddXml (TXml.CreateAsString('UnknownOperations', '')) do
  begin
    AddXml (TXml.CreateAsString('RaiseErrorMessage', notStubbedExceptionMessage));
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
    with AddXml (TXml.CreateAsString('schemaValidation', '')) do
    begin
      with AddXml (TXml.CreateAsString('inboundRequests', '')) do
      begin
        case inboundRequestSchemaValidationType of
          svAccordingProject: AddXml (TXml.CreateAsString ('accordingProject', ''));
          svNo: AddXml (TXml.CreateAsString ('noSchemaValidation', ''));
          svReportOnly: AddXml (TXml.CreateAsString ('reportSchemaViolations', ''));
          svRaiseException:
          with AddXml (TXml.CreateAsString ('raiseExceptionOnViolation', '')) do
            AddXml (TXml.CreateAsInteger('responseCode', schemaValidationVioloationHttpResponseCode));
        end;
      end;
      with AddXml (TXml.CreateAsString('outboundReplies', '')) do
      begin
        case outboundReplySchemaValidationType of
          svAccordingProject: AddXml (TXml.CreateAsString ('accordingProject', ''));
          svNo: AddXml (TXml.CreateAsString ('noSchemaValidation', ''));
          svReportOnly: AddXml (TXml.CreateAsString ('reportSchemaViolations', ''));
          svRaiseException: AddXml (TXml.CreateAsString ('raiseExceptionOnViolation', ''));
        end;
      end;
      with AddXml (TXml.CreateAsString('outboundRequests', '')) do
      begin
        case outboundRequestSchemaValidationType of
          svAccordingProject: AddXml (TXml.CreateAsString ('accordingProject', ''));
          svNo: AddXml (TXml.CreateAsString ('noSchemaValidation', ''));
          svReportOnly: AddXml (TXml.CreateAsString ('reportSchemaViolations', ''));
          svRaiseException: AddXml (TXml.CreateAsString ('raiseExceptionOnViolation', ''));
        end;
      end;
      with AddXml (TXml.CreateAsString('inboundReplies', '')) do
      begin
        case inboundReplySchemaValidationType of
          svAccordingProject: AddXml (TXml.CreateAsString ('accordingProject', ''));
          svNo: AddXml (TXml.CreateAsString ('noSchemaValidation', ''));
          svReportOnly: AddXml (TXml.CreateAsString ('reportSchemaViolations', ''));
          svRaiseException: AddXml (TXml.CreateAsString ('raiseExceptionOnViolation', ''));
        end;
      end;
    end;
  end;
  with result.AddXml (TXml.Create) do
    CopyDownLine(DatabaseConnectionSpecificationXml, True);
end;

procedure TWsdlProject.RefreshCommand;
var
  wasActive: Boolean;
begin
  wasActive := IsActive;
  Activate(False);
  if LazFileUtils.DirectoryExistsUTF8(projectFileName) then
  begin
    OpenFromFolders;
  end
  else
  begin
    if LazFileUtils.FileExistsUTF8(projectFileName) then
      ImportFromFile
    else
      raise Exception.Create('No such file or folder: ' + projectFileName);
  end;
  Activate(wasActive);
end;

function TWsdlProject.ProjectDesignAsXml: TXml;
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
  c, x, w, s, o, r, p: Integer;
  xOperation: TWsdlOperation;
  xWsdl: TWsdl;
  xMessage: TWsdlMessage;
  xDone: Boolean;
  swapReqParent: TCustomBindable;
  asXml, checkerXmls: TXml;
begin
  AcquireLock;
  try
    result := TXml.CreateAsString ('WsdlStubCase', '');
    with result do
    begin
      AddXml(TXml.CreateAsString('FileName', uncFilename(projectFileName)));
      with AddXml (TXml.Create) do
        CopyDownLine(Listeners.SpecificationXml, True);
      AddXml (remoteServerConnectionAsXml);
      AddXml (ProjectOptionsAsXml(SaveRelativeFileNames, uncFilename(projectFileName)));
      AddXml (TXml.CreateAsString('PathPrefixes', xmlio.PathPrefixes.Text));
      with AddXml(TXml.CreateAsString('Environments', '')) do
        for x := 0 to EnvironmentList.Count - 1 do
          with AddXml(TXml.CreateAsString('Environment', '')) do
          begin
            AddAttribute(TXmlAttribute.CreateAsString('Name', EnvironmentList.Strings[x])).Checked := True;
            with AddXml (TXml.Create) do
              Text := (EnvironmentList.Objects [x] as TXml).Text;
          end;
      if (projectContexts.RowCount > 1)
      or (projectContexts.ColCount > 1) then
      begin
        with TStringListList.Create(projectContexts) do
        try
          for c := 1 to ColCount - 1 do
            if isOneTimeContextsColumn(projectContexts, c) then
              for r := 1 to RowCount - 1 do
                CellValue[c, r] := '';
          with AddXml(AsXml) do
          begin
            Name := 'contexts';
            AddAttribute (TXmlAttribute.CreateAsInteger('version', 3));
          end;
        finally
          Free;
        end;
      end;
      for w := 0 to Wsdls.Count - 1 do
      begin
        xWsdl := Wsdls.Objects [w] as TWsdl;
        with AddXml (TXml.CreateAsString('Wsdl', '')) do
        begin
          AddXml (TXml.CreateAsString('Name', xWsdl.Name));
          if (xWsdl.FileAlias <> '')
          and (xWsdl.FileAlias <> xWsdl.Name) then
            AddXml (TXml.CreateAsString('FileAlias', xWsdl.FileAlias));
          xDone := False;
          if xWsdl = FreeFormatWsdl then
          begin
            if xWsdl.Services.Services[0].Operations.Count > 0 then
              AddXml (freeFormatOperationsXml);
            xDone := True;
          end;
          if xWsdl = CobolWsdl then
          begin
            if xWsdl.Services.Services[0].Operations.Count > 0 then
              with AddXml (cobolOperationsXml) do
              begin
                if SaveRelativeFileNames then
                  SetFileNamesRelative(projectFileName);
              end;
            xDone := True;
          end;
          if xWsdl = ApiByExampleWsdl then
          begin
            if (xWsdl.Services.Count > 0)
            and (xWsdl.Services.Services[0].Operations.Count > 0) then
              with AddXml (ApiByExampleOperationsXml(projectFileName)) do
              begin
                if SaveRelativeFileNames then
                  SetFileNamesRelative(projectFileName);
              end;
            xDone := True;
          end;
          if not xDone then
          begin
            if (projectFileName <> '')
            and (SaveRelativeFileNames) then
              AddXml(TXml.CreateAsString ( 'WsdlLocation'
                                         , ExtractRelativeFileName ( projectFileName
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
                    SetFileNamesRelative(projectFileName);
                end;
            end;
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
                if (xWsdl.Services.Services[s].FileAlias <> '')
                and (xWsdl.Services.Services[s].FileAlias <> xWsdl.Services.Services[s].Name) then
                  AddXml (TXml.CreateAsString('FileAlias', xWsdl.Services.Services[s].FileAlias));
                AddXml (xWsdl.Services.Services[s].OptionsAsXml);
                for o := 0 to xWsdl.Services.Services [s].Operations.Count - 1 do
                begin
                  xOperation := xWsdl.Services.Services[s].Operations.Operations[o];
                  with AddXml (TXml.CreateAsString('Operation', '')) do
                  begin
                    AddXml (TXml.CreateAsString('Name', xOperation.Name));
                    AddXml (TXml.CreateAsString('Action', IntToStr(Ord(xOperation.StubAction))));
                 {   if (xOperation.Alias <> xOperation.reqTagName)
                    and (xOperation.Alias <> '') then  }
                    AddXml (TXml.CreateAsString('Alias', xOperation.Alias));
                    AddXml (TXml.CreateAsString('FileAlias', xOperation.FileAlias));
                    AddXml (TXml.CreateAsBoolean('HiddenFromUI', xOperation.HiddenFromUI));
                    asXml := xOperation.AddedTypeDefElementsAsXml as TXml;
                    if asXml.Items.Count > 0 then
                      AddXml (asXml)
                    else
                      FreeAndNil (asXml);
                    AddXml (TXml.CreateAsBoolean('wsaEnabled', xOperation.wsaEnabled));
                    AddXml (TXml.CreateAsBoolean('wsaSpecificMustUnderstand', xOperation.wsaSpecificMustUnderstand));
                    AddXml (TXml.CreateAsBoolean('wsaMustUnderstand', xOperation.wsaMustUnderstand));
                    if xOperation.wsaType <> '2005/08' then
                      AddXml (TXml.CreateAsString('wsaType', xOperation.wsaType));
                    if (xOperation.StubAction = saRequest)
                    and Assigned(xOperation.reqWsaXml)
                    and xOperation.reqWsaXml.Checked
                    and (xOperation.reqWsaXml.Name <> '') then
                      with AddXml (TXml.Create) do
                        CopyDownLine (xOperation.reqWsaXml, True);
                    if (xOperation.StubAction <> saRequest)
                    and Assigned(xOperation.rpyWsaXml)
                    and xOperation.rpyWsaXml.Checked
                    and (xOperation.rpyWsaXml.Name <> '') then
                      with AddXml (TXml.Create) do
                        CopyDownLine (xOperation.rpyWsaXml, True);
                    with AddXml (xOperation.OptionsAsXml) do
                    begin
                      if xOperation.resolveRequestAliasses
                      and xOperation.resolveReplyAliasses then
                        Items.XmlItemByTag['ResolveAliasses'].Checked := False; // to avoid lots of changed files...
                    end;
                    AddXml (TXml.CreateAsString('DelayTimeMsMin', IntToStr(xOperation.DelayTimeMsMin)));
                    AddXml (TXml.CreateAsString('DelayTimeMsMax', IntToStr(xOperation.DelayTimeMsMax)));
                    AddXml (xOperation.endpointConfigAsXml); // seave in 4.0++ style

                    AddXml (TXml.CreateAsString('BeforeScript', xOperation.BeforeScriptLines.Text));
                    AddXml (TXml.CreateAsString('AfterScript', xOperation.AfterScriptLines.Text));
                    swapReqParent := (xOperation.reqBind as TCustomBindable).Parent;
                    with AddXml (TXml.CreateAsString('CorrelationElements', '')) do
                      for r := 0 to xOperation.CorrelationBindables.Count - 1 do
                        AddXml (TXml.CreateAsString('CorrelationElement', xOperation.CorrelationBindables.Strings[r]));
                    (xOperation.reqBind as TCustomBindable).Parent := swapReqParent;
                    with AddXml (TXml.CreateAsString('ColumnElements', '')) do
                    begin
                      if xOperation.Messages.Count > 0 then
                      begin
                        for r := 0 to xOperation.Messages.Messages[0].ColumnXmls.Count - 1 do
                        begin
                          AddXml (TXml.CreateAsString('ColumnElement', xOperation.Messages.Messages[0].ColumnXmls.Strings[r]));
                        end;
                      end;
                    end;
                    AddXml (TXml.CreateAsString('LogColumns',xOperation.LogColumns.Text));
                    if (xOperation = LastFocusedOperation) then
                      AddXml (TXml.CreateAsBoolean('focusedOperation', True));
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
                          AddXml (TXml.CreateAsBoolean ('DocumentationEdited', xMessage.DocumentationEdited));
                          if Assigned (xMessage.BeforeScriptLines) then
                            AddXml (TXml.CreateAsString('BeforeScript', xMessage.BeforeScriptLines.Text));
                          if Assigned (xMessage.AfterScriptLines) then
                            AddXml (TXml.CreateAsString('AfterScript', xMessage.AfterScriptLines.Text));
                          if (xMessage = xOperation.LastFocusedMessage)
                          and (xMessage <> xOperation.Messages.Messages[0]) then
                            AddXml (TXml.CreateAsBoolean('focusedMessage', True));
                        end; // message xml
                      end; // for each message
                    end; // messagess xml
                  end; // operation xml
                end; // each operation
              end; // service xml
            end; // each service
          end; //
        end; // Assigned Wsdl
      end; // for each wsdl
      AddXml(TXml.CreateAsString('ignoreDifferencesOn', ignoreDifferencesOn.Text));
      AddXml(TXml.CreateAsString('checkValueAgainst', checkValueAgainst.Text));
      AddXml(TXml.CreateAsString('ignoreAddingOn', ignoreAddingOn.Text));
      AddXml(TXml.CreateAsString('ignoreRemovingOn', ignoreRemovingOn.Text));
      with AddXml(TXml.CreateAsString('ignoreOrderOn', '')) do
        for x := 0 to ignoreOrderOn.Count - 1 do
          with AddXml(TXml.CreateAsString('Element', '')) do
          begin
            AddXml(TXml.CreateAsString('Id', ignoreOrderOn.Strings[x]));
            AddXml(TXml.CreateAsString('Keys', (ignoreOrderOn.Objects[x] as TJBStringList).Text));
          end;
      AddXml(TXml.CreateAsString('regressionSortColumns', regressionSortColumns.Text));
      AddXml(TXml.CreateAsString('ignoreCoverageOn', ignoreCoverageOn.Text));
      with AddXml(TXml.CreateAsString('Scripts', '')) do
        CopyDownLine(Scripts, True);
      ForgetNamespaces;
    end;
  finally
    ReleaseLock;
  end;
end;

function TWsdlProject.ProjectDesignAsString: String;
begin
  with ProjectDesignAsXml do
  try
    result := AsText(False,0,True,False);
  finally
    Free;
  end;
end;

procedure TWsdlProject.ProjectDesignFromXml(aXml: TXml; aMainFileName: String);
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
  w, x, y, s, o, r, p, c, e, step: Integer;
  xDone: Boolean;
  xOperation: TWsdlOperation;
  xService: TWsdlService;
  xWsdl: TWsdl;
  xScript: TXml;
  wXml, sXml, oXml, eXml, eeXml, dXml, rXml, cXml: TXml;
  xBindName: String;
  xMessage: TWsdlMessage;
  xReadAnother: Boolean;
  xPatterns: TJBStringList;
begin
  ProgressStep('Analyzing...', 100);
  Clear;
  xReadAnother := False;
  try
    xPatterns := TJBStringList.Create;
    try
      try
        aXml.CheckDownLine (True);
        if aMainFileName = '' then
          aMainFileName := aXml.Items.XmlValueByTag ['FileName'];
        projectFileName := aMainFileName;
        sXml := aXml.Items.XmlItemByTag ['contexts'];
        if Assigned (sXml) then
        begin
          projectContexts.FromXml(sXml);
          if StrToIntDef (sXml.AttributeValueByTagDef['version', '0'], 0) < 3 then with projectContexts do
          begin
            for c := 0 to ColCount - 1 do
              if (RightStr(UpperCase(CellValue[c, 0]), 3) = 'PWD')
              or (RightStr(UpperCase(CellValue[c, 0]), 8) = 'PASSWORD') then
                setPasswordContextsColumn(projectContexts, c, True);
          end;
        end;
        sXml := aXml.Items.XmlItemByTag ['remoteServerConnection'];
        if Assigned (sXml) then
          remoteServerConnectionFromXml(sXml);
        xmlio.ProjectContexts := projectContexts;
        sXml := aXml.Items.XmlItemByTag ['Listeners'];
        Listeners.SpecificationXml.Items.Clear;
        if Assigned (sXml) then
        begin
          Listeners.SpecificationXml.CopyDownLine(sXml, True);
          Listeners.FromXml(HaveStompFrame);
        end;
        eXml := aXml.Items.XmlItemByTag ['projectOptions'];
        if Assigned (eXml) then
        begin
          ProjectOptionsFromXml(eXml);
          CurrentFolder := ExpandRelativeFileName (aMainFileName, CurrentFolder);
          ReferenceFolder := ExpandRelativeFileName (aMainFileName, ReferenceFolder);
          ReportsFolder := ExpandRelativeFileName (aMainFileName, ReportsFolder);
        end;
        xmlio.PathPrefixes.Text := aXml.Items.XmlCheckedValueByTag ['PathPrefixes'];
        eXml := aXml.Items.XmlItemByTag ['Environments'];
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
        ignoreDifferencesOn.Text := aXml.Items.XmlValueByTag ['ignoreDifferencesOn'];
        checkValueAgainst.Text := aXml.Items.XmlValueByTag ['checkValueAgainst'];
        ignoreAddingOn.Text := aXml.Items.XmlValueByTag ['ignoreAddingOn'];
        ignoreRemovingOn.Text := aXml.Items.XmlValueByTag ['ignoreRemovingOn'];
        eXml := aXml.Items.XmlItemByTag ['ignoreOrderOn'];
        if Assigned (eXml) then
        begin
          for e := 0 to eXml.Items.Count - 1 do
          begin
            with eXml.Items.XmlItems[e] do
            begin
              if TagName = 'Element' then
              begin
                y := ignoreOrderOn.Add(Items.XmlValueByTag['Id']);
                ignoreOrderOn.Objects[y] := TJBStringList.Create;
                (ignoreOrderOn.Objects[y] as TJBStringList).Text:=Items.XmlValueByTag['Keys'];
              end;
            end;
          end;
        end;
        regressionSortColumns.Text := aXml.Items.XmlValueByTag ['regressionSortColumns'];
        ignoreCoverageOn.Text := aXml.Items.XmlValueByTag ['ignoreCoverageOn'];
        step := 0;
        for w := 0 to aXml.Items.Count - 1 do
          if aXml.Items.XmlItems [w].TagName = 'Wsdl' then
            Inc (step);
        if step > 0 then
          step := 800 div step
        else
          step := 1;
        for w := 0 to aXml.Items.Count - 1 do
        begin
          wXml := aXml.Items.XmlItems [w];
          if wXml.TagName = 'Wsdl' then
          begin
            ProgressStep('Analyzing...', step);
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
            oXml := wXml.Items.XmlItemByTag['ApiByExampleOperations'];
            if Assigned (oXml) then
            begin
              ApiByExampleOperationsUpdate(oXml, aMainFileName);
              xWsdl := ApiByExampleWsdl;
              xDone := True;
            end;
            if not xDone then
            begin
              try
                xWsdl := WsdlOpenFile ( xmlio.ExpandRelativeFileName ( aMainFileName
                                                                     , wXml.Items.XmlValueByTag['WsdlLocation']
                                                                     )
                                      );
              except
                on e: Exception do
                begin
{%ifndef NoGUI}
{$ifdef NEVEREVER}
                  if BooleanPromptDialog ( 'Error: '
                                     + e.Message
                                     + #$D#$A
                                     + 'reading: '
                                     + ExpandRelativeFileName ( aMainFileName
                                                              , resolveAliasses(wXml.Items.XmlValueByTag['WsdlLocation'])
                                                              )
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
                        xWsdl := WsdlOpenFile ( OpenWsdlForm.WsdlLocationEdit.Text);
                        wXml.Items.XmlItemByTag['WsdlLocation'].Value := OpenWsdlForm.WsdlLocationEdit.Text;
                        xReadAnother := True;
                      end;
                    finally
                      FreeAndNil(OpenWsdlForm);
                    end;
                  end
                  else
                    xWsdl := nil;
{$else}
                  Raise Exception.Create(e.Message + LineEnding + LineEnding + 'Found in ' + wXml.SourceFileName);
{$endif}

                end;
              end;
              if Assigned (xWsdl) then
              begin
                Wsdls.AddObject ( xWsdl.FileName
                                , xWsdl
                                );
              end;
            end;
            if Assigned (xWsdl) then
            begin
              xWsdl.FileAlias := wXml.Items.XmlValueByTagDef['FileAlias', xWsdl.Name];
              xWsdl.FileName := ExpandRelativeFileName ( aMainFileName
                                                       , wXml.Items.XmlValueByTag['WsdlLocation']
                                                       );
              dXml := wXml.Items.XmlItemByTag ['ExtraXsds'];
              if Assigned (dXml) then
              begin
                if dXml.Items.Count = 1 then
                begin
                  xWsdl.ExtraXsdsFromXml (dXml.Items.XmlItems[0], SaveRelativeFileNames, aMainFileName);
                  xWsdl.LoadExtraXsds (OnBeforeFileRead);
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
                    xService.FileAlias := sXml.Items.XmlValueByTagDef['FileAlias', xService.Name];
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
                          if xOperation.Alias = '' then
                            xOperation.Alias := xOperation.reqTagName;
                          xOperation.Alias := oXml.Items.XmlValueByTagDef['Alias', xOperation.Alias];
                          xOperation.FileAlias := oXml.Items.XmlValueByTagDef['FileAlias', xOperation.Alias];
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
                          xOperation.wsaType := oXml.Items.XmlValueByTagDef ['wsaType', '2005/08'];
                          if xOperation.StubAction = saRequest then
                            xOperation.reqWsaXml.LoadValues (oXml.Items.XmlItemByTag ['wsa'], False)
                          else
                            xOperation.rpyWsaXml.LoadValues (oXml.Items.XmlItemByTag ['wsa'], False);
                          dXml := oXml.Items.XmlCheckedItemByTag['operationOptions'];
                          if Assigned (dXml) then
                            xOperation.OptionsFromXml(dXml);
                          xOperation.doSuppressLog := oXml.Items.XmlIntegerByTagDef ['doSuppressLog', 0];
                          xOperation.DelayTimeMsMin := oXml.Items.XmlIntegerByTagDef ['DelayTimeMsMin', -1];
                          if xOperation.DelayTimeMsMin = -1 then
                          begin
                            xOperation.DelayTimeMsMin := oXml.Items.XmlIntegerByTagDef ['DelayTimeMs', 0];
                            xOperation.DelayTimeMsMax := xOperation.DelayTimeMsMin;
                          end
                          else
                            xOperation.DelayTimeMsMax := oXml.Items.XmlIntegerByTagDef ['DelayTimeMsMax', 0];
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
                          xOperation.LogColumns.Text := oXml.Items.XmlValueByTag['LogColumns'];
                          for c := 0 to xOperation.LogColumns.Count - 1 do
                            xOperation.LogColumns.Bindables[c] := xOperation.FindBind(xOperation.LogColumns.Strings[c]);
                          if oXml.Items.XmlBooleanByTagDef['focusedOperation', False] then
                            LastFocusedOperation := xOperation;
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
                                xMessage.DocumentationEdited := Items.XmlBooleanByTagDef ['DocumentationEdited', True];
                                if Assigned (xMessage.BeforeScriptLines) then
                                  xMessage.BeforeScriptLines.Text := Items.XmlValueByTag ['BeforeScript'];
                                if Assigned (xMessage.AfterScriptLines) then
                                  xMessage.AfterScriptLines.Text := Items.XmlValueByTag ['AfterScript'];
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
                                if (r = 0)
                                or (Items.XmlBooleanByTagDef['focusedMessage', False]) then
                                  xOperation.LastFocusedMessage := xMessage;
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
        cXml := aXml.Items.XmlItemByTag ['Scripts'];
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
        stubChanged := xReadAnother;
        stubRead := True;
      end;
    finally
      xPatterns.Free;
    end;
    PrepareAllOperations;
  finally
{}
{}
  end;
end;

procedure TWsdlProject.ProjectDesignFromString(aString, aMainFileName: String);
var
  xXml: TXml;
begin
  xXml := TXml.Create;
  try
    xXml.LoadFromString(aString, nil);
    ProjectDesignFromXml(xXml, aMainFileName);
  finally
    xXml.Free;
  end;
end;

procedure TWsdlProject.ProjectDesignFromApiRequestString(aString, aMainFileName: String);
begin
  ProjectDesignFromString (aString, aMainFileName);
end;

procedure TWsdlProject .CreateLogReply (aLog : TLog ;
  var aProcessed : Boolean ; aIsActive : Boolean );
begin
  aProcessed := False;
  try
{}
    CreateReply (aLog, aIsActive);
    aProcessed := True;
  except
    on e: exception do
    begin {}
      aLog.ReplyBody := e.message;
      aLog.Exception := e.Message;
      if aLog.TransportType = ttHttp then
        if (aLog.httpResponseCode div 100) = 2 then
          aLog.httpResponseCode := 500;
    end;
  end; //except
end;

procedure TWsdlProject.CreateReply (aLog: TLog; aIsActive: Boolean);
var
  xOperation: TWsdlOperation;
begin
  aLog.ReplyBody := '';
  aLog.Operation := nil;
  aLog.Mssg := nil;
  aLog.CorrelationId := '';
  xOperation := FindOperationOnRequest(aLog, aLog.httpDocument, aLog.RequestBody, True);
  if not Assigned (xOperation) then
  begin
    aLog.Exception := notStubbedExceptionMessage;
    aLog.ReplyBody := aLog.Exception;
    if (aLog.TransportType = ttHttp)
    or (aLog.TransportType = ttHttps) then
      aLog.httpResponseCode := 500;
    exit;
  end;
  try
    xOperation.Data := aLog;
    aLog.Operation := xOperation;
    aLog.OperationName := xOperation.Alias;
    aLog.StubAction := saStub;
    while Assigned(aLog.Operation.Cloned) do
      aLog.Operation := aLog.Operation.Cloned;
    if aIsActive then
    begin
      aLog.Operation.AcquireLock;
      Inc (aLog.Operation.OperationCounter);
      aLog.OperationCount := aLog.Operation.OperationCounter;
      aLog.Operation.ReleaseLock;
    end;
    if doValidateInboundRequests(xOperation)
    and (xOperation.WsdlService.DescriptionType <> ipmDTFreeFormat)
    and Assigned (xOperation.reqBind)
    and (xOperation.reqBind is TXml) then
    begin
      aLog.RequestValidateResult := '';
      aLog.RequestValidated := True;
      if not xOperation.reqBind.IsValueValid (aLog.RequestValidateResult) then
      begin
        if doReturnExceptionOnViolatingInboundRequest(xOperation) then
        begin
          if xOperation.inboundRequestSchemaValidationType = svAccordingProject then
            aLog.httpResponseCode := schemaValidationVioloationHttpResponseCode
          else
            aLog.httpResponseCode := xOperation.schemaValidationVioloationHttpResponseCode;
          raise Exception.Create('Schema validation error on request:' + LineEnding + aLog.RequestValidateResult);
        end;
      end;
    end;
    xOperation.InitDelayTime;
    if xOperation.doReadReplyFromFile then
      aLog.Mssg := xOperation.Messages.Messages[0]
    else
      aLog.Mssg := xOperation.MessageBasedOnRequest;
    if not Assigned (aLog.Mssg) then
      Raise Exception.Create('Could not find any reply based on request');
    aLog.CorrelationId := xOperation.CorrelationIdAsText ('; ');
    if xOperation.doReadReplyFromFile then
      xOperation.ReadReplyFromFile
    else
      if (xOperation.WsdlService.DescriptionType in [ipmDTFreeFormat]) then
        xOperation.FreeFormatRpy := aLog.Mssg.FreeFormatRpy;
    if not xOperation.doReadReplyFromFile then
    begin
      if xOperation.rpyBind is TIpmItem then
    //    xOperation.rpyIpm.BufferToValues (FoundErrorInBuffer, aReply.rpyIpm.ValuesToBuffer (nil))
        (xOperation.rpyBind as TIpmItem).LoadValues (aLog.Mssg.rpyBind as TIpmItem)
      else
      begin
        (xOperation.rpyBind as TXml).ResetValues;
        (xOperation.rpyBind as TXml).LoadValues (aLog.Mssg.rpyBind as TXml, True, True);
        (xOperation.fltBind as TXml).ResetValues;
        (xOperation.fltBind as TXml).LoadValues (aLog.Mssg.fltBind as TXml, True, True);
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
      if doValidateOutboundReplies(xOperation)
      and (xOperation.WsdlService.DescriptionType <> ipmDTFreeFormat)
      and Assigned (xOperation.rpyBind)
      and (xOperation.rpyBind is TXml)
      and (not xOperation.ReturnSoapFault) then
      begin
        aLog.ReplyValidated := True;
        aLog.ReplyValidateResult := '';
        xOperation.rpyBind.IsValueValid (aLog.ReplyValidateResult);
      end;
    end;
    aLog.InitDisplayedColumns(xOperation, DisplayedLogColumns);
    aLog.doSuppressLog := (xOperation.doSuppressLog <> 0);
    aLog.DelayTimeMs := xOperation.DelayTimeMs;
    aLog.OperationName:=xOperation.Alias;
    aLog.ReplyBody := xOperation.StreamReply (_progName, True);
    aLog.ReplyInfoFromBindables(xOperation);
    if xOperation.ReturnSoapFault then
      aLog.Exception := aLog.ReplyBody;
    if xOperation.StubAction = saRequest then
    begin
      aLog.ReplyBody := _progName + ' - Operation itself is a requestor ('+ xOperation.Alias +')';
      raise SysUtils.Exception.Create(aLog.ReplyBody);
    end;
  finally
    if Assigned (xOperation.Cloned) then
      xOperation.Free;
  end;
end;

function TWsdlProject.CreateScriptOperation(aScript: TXml): TWsdlOperation;
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
      xLog.RequestBody := aFrame.GetBody;
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
    {
      xStream.Position := 0;
      SetLength(Result,xStream.Size);
      xStream.Read(Pointer(Result)^,xStream.Size);
    }
      result := IdGlobal.ReadStringFromStream(xStream);
    finally
      xStream.Free;
    end;
  end
  else
  begin
    AResponseInfo.ContentEncoding := 'identity';
  {
    with ARequestInfo.PostStream as TMemoryStream do
    begin
      Position := 0;
      SetLength(Result, Size);
      Read(Pointer(Result)^, Size);
    end;
  }
    result := IdGlobal.ReadStringFromStream(ARequestInfo.PostStream);
  end;
end;

procedure TWsdlProject.LogServerException(const Msg: String; aException: Boolean; E: Exception);
var
  xLog: TExceptionLog;
begin
  if aException then
    xLog := TExceptionLog.Create ( Msg
                                 + LineEnding
                                 + ExceptionStackListString(E)
                                 )
  else
    xLog := TExceptionLog.Create (Msg
                                 );
  AcquireLogLock;
  try
    toDisplayExceptions.AddEvent (xLog);
  finally
    ReleaseLogLock;
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
  xXml, yXml, hXml, iXml: TXml;
begin
  if not Assigned (aXml) then Exit;
  if aXml.Name <> 'projectOptions' then raise Exception.Create('ProjectOptionsFromXml illegal XML' + aXml.Text);
  ProjectContext := ''; // BEWARE: updated on Contexts screen and not as option
  majorVersion := 0;
  minorVersion := 1;
  revision := 0;
  build := 0;
  doValidateRequests := True;
  doValidateReplies := True;
  inboundRequestSchemaValidationType := svReportOnly;
  outboundReplySchemaValidationType := svReportOnly;
  outboundRequestSchemaValidationType := svReportOnly;
  inboundReplySchemaValidationType := svReportOnly;
  schemaValidationVioloationHttpResponseCode := 417;
  DatabaseConnectionSpecificationXml.Items.Clear;
  UnknownOpsReqReplactementsXml.Items.Clear;
  UnknownOpsRpyReplactementsXml.Items.Clear;
  wrdFunctionz.wrdDetectFormatChanges := False;
  wrdFunctionz.wrdNewDocumentAsReference := False;
  wrdFunctionz.wrdExpectedDifferenceCount := 0;
  RemoteControlPortNumber := 3738;
  OnRequestViolatingAddressPath := rvsRaiseErrorMessage;
  xsdMaxDepthBillOfMaterials := defaultXsdMaxDepthBillOfMaterials;
  xsdMaxDepthXmlGen := defaultXsdMaxDepthXmlGen;
  OperationsWithEndpointOnly := True;
  SaveRelativeFileNames := True;
  CurrentFolder := '';
  ReferenceFolder := '';
  ReportsFolder := '';
  notStubbedExceptionMessage := 'No operation recognised';
  CORS := corsAcceptWithoutLogging;
  corsExceptionHttpCode := 500;

  if not aXml.Checked then Exit;
  with aXml.Items do
  begin
    ProjectContext := XmlCheckedValueByTagDef['ProjectContext', ProjectContext]; // BEWARE: updated on Contexts screen and not as option
    xXml := XmlCheckedItemByTag ['versionInfo'];
    if Assigned (xXml) then
    begin
      majorVersion := xXml.Items.XmlCheckedIntegerByTagDef['major', majorVersion];
      minorVersion := xXml.Items.XmlCheckedIntegerByTagDef['minor', minorVersion];
      revision := xXml.Items.XmlCheckedIntegerByTagDef['revision', revision];
      build := xXml.Items.XmlCheckedIntegerByTagDef['build', build];
    end;
    xXml := XmlCheckedItemByTag ['General'];
    if Assigned (xXml) then
    begin
      SaveRelativeFileNames := xXml.Items.XmlCheckedBooleanByTagDef['SaveRelativeFileNames', True];
      yXml := xXml.Items.XmlCheckedItemByTag['projectFolders'];
      if Assigned (yXml) then with yXml.Items do
      begin
        CurrentFolder := osDirectorySeparators (XmlCheckedValueByTag['current']);
        ReferenceFolder := osDirectorySeparators (XmlCheckedValueByTag['reference']);
        ReportsFolder := osDirectorySeparators (XmlCheckedValueByTagDef['reports', CurrentFolder]);
      end;
    end;
    ProjectLogOptionsFromXml (XmlCheckedItemByTag ['Log']);
    xXml := XmlCheckedItemByTag ['Wsdl'];
    if Assigned (xXml) then
    begin
      OperationsWithEndpointOnly := xXml.Items.XmlCheckedBooleanByTagDef['OperationsWithEndpointOnly', True];
      xsdMaxDepthBillOfMaterials := xXml.Items.XmlCheckedIntegerByTagDef['MaxDepthWhenRecursive', xsdMaxDepthBillOfMaterials];
      xsdMaxDepthXmlGen := xXml.Items.XmlCheckedIntegerByTagDef['MaxDepthXmlGen', xsdMaxDepthXmlGen];
    end;
    xXml := XmlCheckedItemByTag ['CORS'];
    if Assigned (xXml) then
    begin
      if Assigned (xXml.Items.XmlCheckedItemByTag ['exceptionResponse']) then
      begin
        CORS := corsException;
        with xXml.Items.XmlItemByTag ['exceptionResponse'].Items do
        begin
          corsExceptionHttpCode := XmlIntegerByTagDef['responseCode', corsExceptionHttpCode];
        end;
      end;
      if Assigned (xXml.Items.XmlCheckedItemByTag ['refuse']) then
        CORS := corsRefuse;
      if Assigned (xXml.Items.XmlCheckedItemByTag ['accept']) then
      begin
        if xXml.Items.XmlCheckedItemByTag ['accept'].Items.XmlCheckedBooleanByTagDef['showInLog', False] then
          CORS := corsAcceptWithLogging
        else
          CORS := corsAcceptWithoutLogging;
      end;
    end;
    xXml := XmlCheckedItemByTag ['UnknownOperations'];
    if Assigned (xXml) then
    begin
      yXml := xXml.Items.XmlCheckedItemByTag ['RaiseErrorMessage'];
      if Assigned (yXml) then
      begin
        notStubbedExceptionMessage := xXml.Items.XmlCheckedValueByTagDef['RaiseErrorMessage', notStubbedExceptionMessage];
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
      yXml := xXml.Items.XmlCheckedItemByTag ['schemaValidation'];
      if Assigned (yXml) then
      begin
        hXml := yXml.Items.XmlCheckedItemByTag ['inboundRequests'];
        if Assigned (hXml) then with hXml.Items do
        begin
          if Assigned (XmlCheckedItemByTag ['noSchemaValidation']) then inboundRequestSchemaValidationType := svNo;
          if Assigned (XmlCheckedItemByTag ['reportSchemaViolations']) then inboundRequestSchemaValidationType := svReportOnly;
          iXml := hXml.Items.XmlCheckedItemByTag ['raiseExceptionOnViolation'];
          if Assigned (iXml) then with iXml.Items do
          begin
            inboundRequestSchemaValidationType := svRaiseException;
            schemaValidationVioloationHttpResponseCode := XmlCheckedIntegerByTagDef['responseCode', schemaValidationVioloationHttpResponseCode];
          end;
        end;
        hXml := yXml.Items.XmlCheckedItemByTag ['outboundReplies'];
        if Assigned (hXml) then with hXml.Items do
        begin
          if Assigned (XmlCheckedItemByTag ['noSchemaValidation']) then outboundReplySchemaValidationType := svNo;
          if Assigned (XmlCheckedItemByTag ['reportSchemaViolations']) then outboundReplySchemaValidationType := svReportOnly;
          if Assigned (XmlCheckedItemByTag ['raiseExceptionOnViolation']) then outboundReplySchemaValidationType := svRaiseException;
        end;
        hXml := yXml.Items.XmlCheckedItemByTag ['outboundRequests'];
        if Assigned (hXml) then with hXml.Items do
        begin
          if Assigned (XmlCheckedItemByTag ['noSchemaValidation']) then outboundRequestSchemaValidationType := svNo;
          if Assigned (XmlCheckedItemByTag ['reportSchemaViolations']) then outboundRequestSchemaValidationType := svReportOnly;
          if Assigned (XmlCheckedItemByTag ['raiseExceptionOnViolation']) then outboundRequestSchemaValidationType := svRaiseException;
        end;
        hXml := yXml.Items.XmlCheckedItemByTag ['inboundReplies'];
        if Assigned (hXml) then with hXml.Items do
        begin
          if Assigned (XmlCheckedItemByTag ['noSchemaValidation']) then inboundReplySchemaValidationType := svNo;
          if Assigned (XmlCheckedItemByTag ['reportSchemaViolations']) then inboundReplySchemaValidationType := svReportOnly;
          if Assigned (XmlCheckedItemByTag ['raiseExceptionOnViolation']) then inboundReplySchemaValidationType := svRaiseException;
        end;
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

function TWsdlProject .WsdlOpenFile (aName : String): TWsdl ;
var
  xExt: String;
begin
  xExt := UpperCase (ExtractFileExt (resolveAliasses(aName)));
  result := TWsdl.Create(EnvVars, OperationsWithEndpointOnly);
    if (xExt = '.JSON')
    or (xExt = '.YAML')
    or (xExt = '.JSN')
    or (xExt = '.YML')
  or (AnsiStartsText('APIARY://', aName)) then
    result.LoadFromJsonYamlFile(aName, nil, OnBeforeFileRead)
  else
    result.LoadFromSchemaFile(aName, nil, OnBeforeFileRead);
  if Result.FileName = '' then
    SjowMessage(format('(%s)if Result.FileName = ''''? hoe kan dat dan?', [aName]));
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
  reqheader, rpyheader: String;
begin
  reqheader := '';
  rpyheader := '';
  case aOperation.StubTransport of
    ttHttp: result := SendHttpMessage (aOperation, nil);
    ttStomp: result := SendOperationStompMessage (aOperation, aMessage, reqheader, rpyheader);
    ttNone: result := SendNoneMessage(aOperation, aMessage, nil);
  end;
end;

function TWsdlProject.SendHttpMessage (aOperation: TWsdlOperation; aLog: TLog): String;
  function _download (dStream: TMemoryStream; aResponse: TIdHTTPResponse): String;
  begin
    if (Pos ('attachment', aResponse.ContentDisposition) > 0)
    and (Pos ('filename', aResponse.ContentDisposition) > 0) then
    begin
      with TRegExpr.Create('filename=[^\;]*') do
      try
        if Exec(aResponse.ContentDisposition) then
        begin
          result := CurrentFolder
                     + DirectorySeparator
                     + Copy(Match[0], Length('filename=') + 1, MaxInt)
                     ;
          dStream.SaveToFile(result);
        end;
      finally
        free;
      end;
    end
    else
    begin
      result := CurrentFolder
                 + DirectorySeparator
                 + 'stream ' + xsdNowAsDateTime + '.tmp'
                 ;
      dStream.SaveToFile(result);
    end;
  end;

  function _bmtpPackEnvelope (aString: String): String;
  var
    s, d: AnsiString;
  begin
    result := aString;
  end;

  function _bmtpUnpackEnvelope (aString: String): String;
  var
    s, d: AnsiString;
    mXml: TXml;
  begin
    result := aString;
  end;

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
    result := _bmtpUnpackEnvelope(result);
  end;

var
  HttpClient: TIdHTTP;
  reqStream: TStream;
  sStream, dStream: TMemoryStream;
  URL, querySep, addressFromDescr, headerName: String;
  oUri, sUri: TIdUri;
  x: Integer;
begin
  reqStream := nil;
  Result := '';
  if not Assigned (aOperation)
    then raise Exception.Create('SendHttpMessage: null arguments');
  if (aOperation.isOpenApiService) then
  begin
    if Assigned (aOperation.Wsdl.Servers)
    and (aOperation.Wsdl.Servers.Count > 0) then
    begin
      URL := aOperation.Wsdl.Servers.Strings[0];
      addressFromDescr := URL;
    end;
  end
  else
  begin
    URL := aOperation.SoapAddress;
    addressFromDescr := URL;
  end;
  HttpClient := TIdHTTP.Create;
  try
    try
      if StartsText('multipart', aOperation.Consumes) then
        reqStream := TIdMultiPartFormDataStream.Create
      else
        reqStream := TMemoryStream.Create;
      if aOperation.StubHttpAddress <> '' then
      begin
        ppLock.Acquire;
        try
          sUri := TIdUri.Create(resolveAliasses(aOperation.StubHttpAddress));
        finally
          ppLock.Release;
        end;
        if addressFromDescr <> '' then
        begin
          oUri := TIdUri.Create(addressFromDescr);
          try
            if (sUri.Protocol = '') then
              sUri.Protocol := oUri.Protocol;
            if aOperation.useSsl then
              sUri.Protocol := 'https';
            if (sUri.Path + sUri.Document = '/')
            then begin
              sUri.Path := oUri.Path;
              sUri.Document := oUri.Document;
            end;
          finally
            FreeAndNil (oUri);
          end;
        end;
        URL := sUri.URI;
        FreeAndNil (sUri);
      end;
      if aLog.RequestHeaders <> '' then
      begin
        HttpClient.Request.CustomHeaders.Text := aLog.RequestHeaders;
        RemoveStdHttpHeaders(HttpClient.Request.CustomHeaders);
      end;
      if aOperation.isOpenApiService
      and (not aOperation.HttpAddressIsComplete) then // URL is still without Service specific (one of those Paths) part
      begin
        if URL = '' then
          raise Exception.CreateFmt ('Operation: %s URL empty', [aOperation.Name]);
        if URL [Length (URL)] = '/' then
          SetLength(URL, Length (URL) - 1);
        aLog.PathFormat := aOperation.WsdlService.logPathFormat;
        URL := URL
             + aOperation.WsdlService.openApiPath;
        querySep := '?';
        for x := 0 to aOperation.reqXml.Items.Count - 1 do with aOperation.reqXml.Items.XmlItems[x] do
        begin
          if Checked
          and Assigned (Xsd) then
          begin
            if (Xsd.ParametersType = oppPath) then
            begin
              URL := ReplaceStr(URL, '{' + Name + '}', ValueFromJsonArray(true));
              aLog.PathFormat := ReplaceStr(aLog.PathFormat, '{' + Name + '}', '%s');
            end;
            if (Xsd.ParametersType = oppQuery) then
            begin
              URL := URL + querySep + Name + '=' + ValueFromJsonArray(true);
              querySep := '&';
            end;
            if (Xsd.ParametersType = oppHeader) then
            begin
              HttpClient.Request.CustomHeaders.Values [Name] := ValueFromJsonArray(false);
            end;
            if (Xsd.ParametersType = oppFormData) then with reqStream as TIdMultiPartFormDataStream do
            begin
              if Assigned (TypeDef)
              and (TypeDef.BaseDataTypeName = 'file') then
                AddFile(Name, Value)
              else
                AddFormField(Name, Value);
            end;
            if (Xsd.ParametersType = oppBody)
            and (aOperation.OpenApiVersion [1] <> '2') then
            begin
              aOperation.ContentType := Xsd.MediaType;
            end;
          end;
        end;
        with TIdURI.Create(URL) do
        try
          aLog.httpDocument := Path + Document;
          alog.httpParams := Params;
        finally
          Free;
        end;
      end;

      aLog.httpUri := URL;
      if aOperation.OverruleContentType <> '' then
        HttpClient.Request.ContentType := aOperation.OverruleContentType
      else
      begin
        if (aOperation.httpVerb <> 'PATCH')
        and (aOperation.httpVerb <> 'POST')
        and (aOperation.httpVerb <> 'PUT')
        then
          HttpClient.Request.ContentType := ''
        else
          HttpClient.Request.ContentType := aOperation.ContentType;
      end;
      if (reqStream is TIdMultiPartFormDataStream) then
        HttpClient.Request.ContentType := (reqStream as TIdMultiPartFormDataStream).RequestContentType;
      HttpClient.Request.Accept := aOperation.Accept;
      try
        if aOperation.SoapAction <> '' then
        begin
          if aOperation.SoapAction [1] <> '"' then
            HttpClient.Request.CustomHeaders.Values ['SOAPAction'] := '"' + aOperation.SoapAction + '"'
          else
            HttpClient.Request.CustomHeaders.Values ['SOAPAction'] := aOperation.SoapAction;
        end;
      except
      end;
      if Assigned (aOperation.StubCustomHeaderXml)
      and aOperation.StubCustomHeaderXml.Checked then with aOperation.StubCustomHeaderXml.Items do
      begin
        ppLock.Acquire;
        try
          for x := 0 to Count - 1 do
          begin
            if (XmlItems[x].Name = 'Header')
            and (XmlItems[x].Checked) then
            begin
              with XmlItems[x].Items do
              begin
                headerName := XmlCheckedValueByTag ['Name'];
                HttpClient.Request.CustomHeaders.Values [headerName] := resolveAliasses (XmlCheckedValueByTag ['Value']);
                if headerName = 'Accept' then
                  HttpClient.Request.Accept := resolveAliasses (XmlCheckedValueByTag ['Value']);
                if headerName = 'Content-Type' then
                  HttpClient.Request.ContentType := resolveAliasses (XmlCheckedValueByTag ['Value']);
              end;
            end;
          end;
        finally
          ppLock.Release;
        end;
      end;
      aLog.RequestContentType := HttpClient.Request.ContentType;
      HttpClient.Request.ContentEncoding := aOperation.ContentEncoding;
      HttpClient.Request.AcceptEncoding := 'identity';
      if aOperation.AcceptDeflateEncoding then
        HttpClient.Request.AcceptEncoding := HttpClient.Request.AcceptEncoding + ', deflate';
      if aOperation.AcceptGzipEncoding then
        HttpClient.Request.AcceptEncoding := HttpClient.Request.AcceptEncoding + ', gzip';
{}
      if not (reqStream is TIdMultiPartFormDataStream) then
      begin
        if (HttpClient.Request.ContentEncoding = 'deflate')
        or (HttpClient.Request.ContentEncoding = 'gzip') then
        begin
          sStream := TMemoryStream.Create;
          try
            WriteStringToStream(_bmtpPackEnvelope(aLog.RequestBody), sStream);
            sStream.Position := 0;
            if HttpClient.Request.ContentEncoding = 'deflate' then
              GZIPUtils.deflate(sStream, reqStream as TMemoryStream);
            if HttpClient.Request.ContentEncoding = 'gzip' then
              GZIPUtils.GZip(sStream, reqStream as TMemoryStream);
            reqStream.Position := 0;
          finally
            sStream.Free;
          end;
        end
        else
          WriteStringToStream(_bmtpPackEnvelope(aLog.RequestBody), reqStream);
      end
      else
      begin
        if (HttpClient.Request.ContentEncoding = 'deflate')
        or (HttpClient.Request.ContentEncoding = 'gzip') then
        begin
    //
          raise Exception.Create(_progName + ': compression with multipart not (yet) supported');
    //
          sStream := TMemoryStream.Create;
          try
            sStream.CopyFrom (reqStream, (reqStream as TIdMultiPartFormDataStream).Size);
            sStream.Position := 0;
            reqStream.Free;
            reqStream := TMemoryStream.Create;
            if HttpClient.Request.ContentEncoding = 'deflate' then
              GZIPUtils.deflate(sStream, reqStream as TMemoryStream);
            if HttpClient.Request.ContentEncoding = 'gzip' then
              GZIPUtils.GZip(sStream, reqStream as TMemoryStream);
            reqStream.Position := 0;
          finally
            sStream.Free;
          end;
        end;
      end;

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
      if (UpperCase(Copy (URL, 1, 8)) = 'HTTPS://') then
      begin
        HttpClient.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
        with (HttpClient.IOHandler as TIdSSLIOHandlerSocketOpenSSL) do
        begin
          SSLOptions.CertFile := resolveAliasses (aOperation.sslCertificateFile);
          SSLOptions.KeyFile := resolveAliasses (aOperation.sslKeyFile);
          SSLOptions.RootCertFile := resolveAliasses (aOperation.sslRootCertificateFile);
          if aOperation.sslPassword <> '' then
            OnGetPassword := aOperation.OnGetSslPassword; // TODO resolveAliasses...
          SSLOptions.Method := aOperation.sslVersion;
          SSLOptions.Mode := sslmUnassigned;
          SSLOptions.VerifyMode := [];
        end;
      end;
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
              if httpVerb = 'PATCH' then HttpClient.Patch(URL, reqStream, dStream);
              if httpVerb = 'POST' then HttpClient.Post(URL, reqStream, dStream);
              if httpVerb = 'PUT' then HttpClient.Put(URL, reqStream, dStream);
              if httpVerb = 'TRACE' then httpClient.Trace(URL, dStream);
            end;
          finally
            aOperation.HttpAddressIsComplete := False;
            aLog.RequestHeaders := HttpClient.Request.RawHeaders.Text;
            aLog.ReplyHeaders := HttpClient.Response.RawHeaders.Text;
            aLog.ReplyContentType := HttpClient.Response.ContentType;
            alog.httpResponseCode := HttpClient.ResponseCode;
          end;
          if (HttpClient.Response.ContentType = 'application/octet-stream')
//          and (Pos ('attachment', HttpClient.Response.ContentDisposition) > 0)
//          and (Pos ('filename', HttpClient.Response.ContentDisposition) > 0) then
          then
          begin
            result := _download (dStream, HttpClient.Response)
          end
          else
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
      if HttpClient.Connected then {in case server s-alive}
        HttpClient.Disconnect;
    finally
      FreeAndNil (reqStream);
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

procedure TWsdlProject.OnBeforeFileRead(aFileName: String);
begin
  if Assigned (referencedFilenames) then
    referencedFilenames.Add (aFileName);
end;

procedure TWsdlProject .SendOperation (aOperation : TWsdlOperation);
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
  xLog: TLog;
  xMessage, s: String;
begin
  xNow := Now;
  xMessage := '';
  if not Assigned (aOperation)
    then raise Exception.Create('SendMessage: null arguments');
  if Assigned (aOperation.Data)
  and (aOperation.Data is TLog) then
    xLog := aOperation.Data as TLog
  else
    xLog := TLog.Create;
  try
    try
      xLog.Operation := aOperation;
      xLog.Operation.Data := xLog;
      while Assigned(xLog.Operation.Cloned) do
        xLog.Operation := xLog.Operation.Cloned;
      _OperationCount(xLog);
      xLog.ServiceName := aOperation.WsdlService.Name;
      xLog.OperationName := aOperation.Alias;
      xLog.TransportType := aOperation.StubTransport;
      xLog.Mssg := aOperation.CorrelatedMessage;
      xLog.RequestContentType := aOperation.ContentType;
      aOperation.doSuppressLog := 0;
      if aOperation.wsaEnabled then
        try
          aOperation.reqWsaOnRequest;
          with aOperation.reqWsaXml.FindUQXml('wsa.MessageID') do
          begin
            Value := xLog.MessageId;
            Checked := True;
          end;
        except
        end;
      if (not aOperation.PreparedBefore) then
        aOperation.PrepareBefore;
      aOperation.ExecuteBefore;
      aOperation.ExecuteReqStampers;
      if Assigned (aOperation.requestInfoBind) then
      begin
        s := aOperation.requestInfoBind.Name;
        aOperation.requestInfoBind.Name := 'endpointConfig';
        aOperation.endpointConfigFromXml(aOperation.requestInfoBind as TXml);
        aOperation.requestInfoBind.Name := s;
      end;
      if doValidateOutboundRequests(aOperation) then
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
          ttHttp: xLog.ReplyBody := SendHttpMessage (aOperation, xLog);
          ttStomp: xLog.ReplyBody := SendOperationStompMessage (aOperation, xLog.RequestBody, xLog.RequestHeaders, xLog.ReplyHeaders);
          ttNone: xLog.ReplyBody := SendNoneMessage(aOperation, xlog.RequestBody, xLog);
        end;
      finally
        xLog.InboundTimeStamp := Now;
      end;
      xLog.ReplyInfoToBindables(aOperation);
      if xLog.ReplyBody = S_MESSAGE_ACCEPTED then
      begin
        xLog.ReplyBody := '';
        if aOperation.rpyBind.Name = '' then
        begin
          aOperation.ExecuteAfter;
        end;
      end
      else
      begin
        if aOperation.isOpenApiService then
        begin
          xLog.OpenApiReplyToBindables(aOperation);
          if doValidateInboundReplies(aOperation) then
          begin
            if not aOperation.rpyBind.IsValueValid (xMessage) then
              xLog.ReplyValidateResult := xMessage;
            xLog.ReplyValidated := True;
          end;
        end
        else
        begin
          if aOperation.isFreeFormat then
          begin
            aOperation.FreeFormatRpy := xLog.ReplyBody;
            if doValidateInboundReplies(aOperation) then
            begin
              if not aOperation.rpyBind.IsValueValid (xMessage) then
                xLog.ReplyValidateResult := xMessage;
              xLog.ReplyValidated := True;
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
                if xXml.Name <> '' then
                  aOperation.XmlReplyToBindables (xXml, True);
            //              aOperation.rpyBind.LoadValues(xXml, True, False);
              finally
                xXml.Free;
              end;
            end;
            if aOperation.reqBind is TIpmItem then
              (aOperation.rpyBind as TIpmItem).BufferToValues (FoundErrorInBuffer, xLog.ReplyBody);
            if doValidateInboundReplies(aOperation) then
            begin
              if not aOperation.rpyBind.IsValueValid (xMessage) then
                xLog.ReplyValidateResult := xMessage;
              xLog.ReplyValidated := True;
            end;
          end;
        end;
        aOperation.ExecuteAfter;
        if aOperation.StubTransport = ttNone then
        begin
          xLog.ReplyContentType := aOperation.Produces;
          xLog.ReplyBody := aOperation.StreamReply (_progName, True);
          xLog.httpResponseCode := aOperation.ResponseNo;
        end;
      end;
      with xLog do
      begin
//      RequestHeaders := HttpClient.Request.CustomHeaders.Text;
        if not Assigned (Mssg) then
          Mssg := aOperation.Messages.Messages[0];
        CorrelationId := aOperation.CorrelationIdAsText ('; ');
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
          StubAction := aOperation.StubAction;
          Exception := e.Message;
          if OutboundTimeStamp = 0 then
            OutboundTimeStamp := xNow;
          Nr := displayedLogs.Number;
        end;
        Raise;
      end;
    end;
  finally
    xLog.InitDisplayedColumns(aOperation, DisplayedLogColumns);
    DisplayLog ('', xLog);
  end;
end;

procedure TWsdlProject.SendOperationInThread(aOperation: TWsdlOperation);
begin
  TProcedureThread.CreateProcedureOperation(False, False, self, SendOperation, aOperation);
end;


procedure TWsdlProject .SendMessage (aOperation : TWsdlOperation ;
  aRequest : TWsdlMessage ; aCorrelationId : String );
var
  sl: TJBStringList;
  x: Integer;
begin
  if not Assigned (aOperation)
    then raise Exception.Create('SendMessage: null arguments');
  if Assigned (aRequest) then
    aOperation.ReqBindablesFromWsdlMessage(aRequest);
  aOperation.Data := nil;
  sl := TJBStringList.Create;
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
  SendOperation(aOperation);
end;

procedure TWsdlProject.SetAbortPressed(const Value: Boolean);
begin
  fAbortPressed := Value;
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
  Stomp.Host := resolveAliasses(aOperation.StubStompPutHost);
  Stomp.Port := StrToIntDef(resolveAliasses(aOperation.StubStompPutPort), 61613);
  Stomp.UseCredentials := aOperation.StubStompPutUseCredentials;
  Stomp.UserName := aOperation.StubStompPutUserName;
  Stomp.Password := aOperation.StubStompPutPassword;
  Stomp.ClientId := resolveAliasses(aOperation.StubStompPutClientId);
  try
    Stomp.Connect;
    try
      fXml := TXml.Create;
      fXml.CopyDownLine (aOperation.StubStompHeaderXml, True);
      fXml.ResolveAliasses;
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
                      , fXml
                      , aOperation.StubCustomHeaderXml
                      , aRequestHeader
                      )
          except
            raise;
          end
        else
          Result := Stomp.RequestReply ( aMessage
                                       , aOperation.StubStompTimeout
                                       , fXml
                                       , aOperation.StubCustomHeaderXml
                                       , aRequestHeader
                                       , aReplyHeader
                                       )
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
        result.XmlReplyToBindables (xXml, False);
    end;
  finally
    FreeAndNil (xXml);
  end;
end;

function TWsdlProject.FindCcbOperationOnRequest (aLog: TLog; aCobolString: String): TWsdlOperation;
  function _Matches (aExpr, aString: String): Boolean;
  begin
    result := False;
    if (aString <> '') then
    with TRegExpr.Create do
    try
      Expression := '^(' + aExpr + ')$';  // bol and eol: must match entire string
      result := Exec(aString);
    finally
      Free;
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
      in [ipmDTFreeFormat, ipmDTCobol] then
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

procedure TWsdlProject.operationRecognitionUpdate(aOperation: TWsdlOperation; aList: TJBStringList;
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
  sList: TJBStringList;
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

  sList := TJBStringList.Create;
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
            Documentation.Text := oXml.Items.XmlCheckedValueByTag['Annotation'];
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
          xOperation.reqRecognition := TJBStringList.Create;
          xOperation.rpyRecognition := TJBStringList.Create;
          xOperation.RecognitionType := rtSubString;
          xOperation.Documentation.Text := oXml.Items.XmlCheckedValueByTag['Annotation'];
          operationRecognitionUpdate (xOperation, xOperation.reqRecognition, oXml.Items.XmlItemByTag['reqRecognition']);
          operationRecognitionUpdate (xOperation, xOperation.rpyRecognition, oXml.Items.XmlItemByTag['rpyRecognition']);
        end;
      end;
      f := Wsdls.IndexOfObject(FreeFormatWsdl);
      if (Operations.Count > 0)
      and (f < 0) then
        Wsdls.AddObject(FreeFormatWsdl.Name, FreeFormatWsdl);
      if (Operations.Count = 0)
      and (f > -1) then
        Wsdls.Delete(f);
    end;
  finally
    sList.Free;
  end;
end;

procedure TWsdlProject.ApiByExampleOperationsUpdate(aXml: TXml; aMainFileName: String);
  function _LoadApiByExampleReq (aLabel: String; sXml: TXml; aXsd: TXsd): TXml;
  var
    xXsdDescr: TXsdDescr;
    xXsd: TXsd;
    xXml: TXml;
    x: Integer;
    xSampleFileName: String;
  begin
    result := nil;
    xXsd := nil;
    try
      if not Assigned (sXml) then
        exit;
      aXsd.sType.ElementDefs.Clear;
      xXsdDescr := TXsdDescr.Create;
      ApiByExampleWsdl.sdfXsdDescrs.AddObject('', xXsdDescr);
      for x := 0 to sXml.Items.Count - 1 do
      begin
        with sXml.Items.XmlItems[x] do
        begin
          if Name = 'parameter' then
          begin
            xXsd := TXsd.Create(xXsdDescr);
            xXsdDescr.Garbage.AddObject('', xXsd);
            xXsd.ElementName := Items.XmlValueByTag['name'];
            xXsd.sType := TXsdDataType.Create(xXsdDescr);
            xXsdDescr.Garbage.AddObject('', xXsd.sType);
            xXsd.sType.xsdType:= dtSimpleType;
            xXsd.sType.jsonType := jsonString;
            xXsd.sType.Name := xXsd.ElementName;
            if Items.XmlValueByTagDef['required', 'true'] = 'true' then
              xXsd.minOccurs := '1'
            else
              xXsd.minOccurs := '0';
            xXsd.maxOccurs := '1';
            xXsd.ParametersType := NameToOperationParametersType(Items.XmlValueByTagDef['in', 'Query']);
            aXsd.sType.ElementDefs.AddObject(xXsd.ElementName, xXsd);
          end;
        end;
      end;
      xXml := sXml.ItemByTag['SampleFile'];
      if Assigned (xXml) then
      begin
        xSampleFileName := ExpandRelativeFileName (aMainFileName, xXml.Value);
        try
          if UpperCase(ExtractFileExt(xSampleFileName)) = '.XML' then
            xXsd := xXsdDescr.LoadXsdFromXmlSampleFile(xSampleFileName, nil, OnBeforeFileRead)
          else
            xXsd := xXsdDescr.LoadXsdFromJsonSampleFile(xSampleFileName, nil, OnBeforeFileRead);
        except
          on E: Exception do
            raise Exception.Create('Error opening ' + xSampleFileName + ': ' + e.Message);
        end;
        if Assigned (xXsd) then
        begin
          xXsd.minOccurs := '1';
          xXsd.maxOccurs := '1';
          xXsd.ParametersType := oppBody;
          aXsd.sType.ElementDefs.AddObject('', xXsd);
        end;
      end;
      bindRefId := 0;
      result := TXml.Create (0, aXsd);
    finally
      if not Assigned (result) then
        result := TXml.Create;
      result.Checked := True;
      if result.jsonType = jsonNone then
        Result.jsonType := jsonObject;
    end;
  end;
  function _LoadApiByExampleRpy (aLabel: String; sXml: TXml; aXsd: TXsd): TXml;
  var
    x: Integer;
    xXsdDescr: TXsdDescr;
    xXsd, sXsd: TXsd;
    xFileName: String;
  begin
    result := nil;
    xXsd := nil;
    try
      if not Assigned (sXml) then
        exit;
      aXsd.sType.ElementDefs.Clear;
      xXsdDescr := TXsdDescr.Create;
      ApiByExampleWsdl.sdfXsdDescrs.AddObject('', xXsdDescr);
      for x := 0 to sXml.Items.Count - 1 do
      begin
        with sXml.Items.XmlItems[x] do
        begin
          if Name = 'response' then
          begin
            xXsd := TXsd.Create(xXsdDescr);
            xXsdDescr.Garbage.AddObject('', xXsd);
            xXsd.ElementName := 'rspns' + Items.XmlValueByTag['code'];
            xXsd.ResponseNo := Items.XmlIntegerByTag['code'];
            xXsd.FileName := Items.XmlValueByTag['SampleFile'];
            xXsd.sType := TXsdDataType.Create(xXsdDescr);
            xXsdDescr.Garbage.AddObject('', xXsd.sType);
            xXsd.sType.xsdType:= dtComplexType;
            xXsd.sType.jsonType := jsonObject;
            xXsd.sType.Name := xXsd.ElementName;
            if Items.XmlValueByTagDef['', 'false'] = 'true' then
              xXsd.minOccurs := '1'
            else
              xXsd.minOccurs := '0';
            xXsd.maxOccurs := '1';
            aXsd.sType.ElementDefs.AddObject(xXsd.ElementName, xXsd);
            if Assigned (Items.XmlCheckedItemByTag['SampleFile']) then
            begin
              xFileName := ExpandRelativeFileName ( aMainFileName
                                                  , Items.XmlValueByTag['SampleFile']
                                                  );
              try
                if UpperCase(ExtractFileExt(xFileName)) = '.XML' then
                  sXsd := xXsdDescr.LoadXsdFromXmlSampleFile(xFileName, nil, OnBeforeFileRead)
                else
                  sXsd := xXsdDescr.LoadXsdFromJsonSampleFile (xFileName, nil, OnBeforeFileRead);
              except
                on E: Exception do
                  raise Exception.Create('Error opening ' + xFileName + ': ' + e.Message);
              end;
              xXsd.sType.ElementDefs.AddObject(sXsd.ElementName, sXsd);
            end;
          end;
        end;
      end;
      bindRefId := 0;
      result := TXml.Create (0, aXsd);
    finally
      if not Assigned (result) then
        result := TXml.Create;
      result.Checked := True;
      if result.jsonType = jsonNone then
        Result.jsonType := jsonObject;
    end;
  end;
  function _LoadApiByExampleFlt (aLabel: String): TXml;
  begin
    result := TXml.Create;
    result.Checked := True;
    result.jsonType := jsonObject;
    result.Name := aLabel;
  end;
var
  sList, oList: TJBStringList;
  sXml, oXml, xXml: TXml;
  xXsd: TXsd;
  xWsdl: TWsdl;
  f, x, s, o: Integer;
  xService: TWsdlService;
  xOperation: TWsdlOperation;
const
  _xsdName = 'OperationDefs.ApiByExampleOperations';
begin
  xWsdl := ApiByExampleWsdl;
  if aXml.Name <> 'ApiByExampleOperations' then
    raise Exception.Create('??TWsdlProject.ApiByExampleOperationsUpdate(aXml: TXml): ' + aXml.Name);
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

{$ifndef NoGUI}
//  if xmlUtil.CheckAndPromptFileNames(aMainFileName, aXml, True) then
  //  StubChanged := True;
{$endif}
  sList := TJBStringList.Create;
  try
    sList.Sorted := True;
    for x := 0 to aXml.Items.Count - 1 do
      if (aXml.Items.XmlItems[x].Name = 'Service') then
        sList.AddObject(aXml.Items.XmlItems[x].Items.XmlCheckedValueByTag['Name'], aXml.Items.XmlItems[x]);
    for s := xWsdl.Services.Count - 1 downto 0 do
    begin
      if not sList.Find(xWsdl.Services.Services[s].Name, f) then
      begin
        xWsdl.Services.Services[s].Free;
        xWsdl.Services.Delete(s);
      end;
    end;
    for s := 0 to sList.Count - 1 do
    begin
      sXml := sList.Objects[s] as TXml;
      if not xWsdl.Services.Find(sList.Strings[s], f) then
      begin
        xService := TWsdlService.Create;
        xService.Name := sList.Strings[s];
        xWsdl.Services.AddObject(xService.Name, xService);
        xService.DescriptionType := ipmDTJson; // ?? maybe better to use a seperate type ??
      end
      else
      begin
        xService := xWsdl.Services.Services[f];
      end;
//      xService.Host := sXml.Items.XmlValueByTag['Address'];
      xService.openApiPath := sXml.Items.XmlValueByTag['Path'];
      xService.PathInfos.Add(xService.openApiPath);
      oList := TJBStringList.Create;
      try
        oList.Sorted := True;
        for x := 0 to sXml.Items.Count - 1 do
          if (sXml.Items.XmlItems[x].Name = 'Operation') then
            with sXml.Items.XmlItems[x].Items do
              oList.AddObject ( XmlCheckedValueByTag['Alias']
                              , sXml.Items.XmlItems[x]
                              );
        with xService do
        begin
          for o := Operations.Count - 1 downto 0 do
          begin
            if not (oList.Find ( Operations.Operations[o].Alias
                               , f
                               )
                   ) then
            begin // remove
              Operations.Operations[o].Free;
              Operations.Delete(o);
            end;
          end;
          for o := 0 to oList.Count - 1 do
          begin
            oXml := oList.Objects[o] as TXml;
            if not Operations.Find(oList.Strings[o], f) then
            begin
              xOperation := TWsdlOperation.Create (xWsdl);
              (xOperation.reqBind as TXml).jsonType := jsonObject;
              (xOperation.rpyBind as TXml).jsonType := jsonObject;
              (xOperation.fltBind as TXml).jsonType := jsonObject;
              xOperation.Name := oList.Strings[o];
              xOperation.Alias := xOperation.Name;
              xService.Operations.AddObject(xOperation.Name, xOperation);
              xOperation.Wsdl := xWsdl;
              xOperation.WsdlService := xService;
              xOperation.reqTagName := xOperation.Name + '_Req';
              xOperation.rpyTagName := xOperation.Name + '_Rpy';
              xOperation.reqRecognition := TJBStringList.Create;
              xOperation.rpyRecognition := TJBStringList.Create;
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
              Documentation.Text := oXml.Items.XmlCheckedValueByTag['Annotation'];
              httpVerb := oXml.Items.XmlValueByTagDef['Verb', httpVerb];
              xOperation.ContentType := oXml.Items.XmlCheckedValueByTagDef['produces', 'application/json'];
              xOperation.Accept := oXml.Items.XmlCheckedValueByTagDef['consumes', 'application/json'];
              xOperation.Consumes := xOperation.ContentType;
              xOperation.Produces := xOperation.Accept;
              if (Pos ('/xml', LowerCase(xOperation.Consumes)) > 0) then
                xOperation.ConsumeType := ptXml
              else
                xOperation.ConsumeType := ptJson;
              if (Pos ('/xml', LowerCase(xOperation.Produces)) > 0) then
                xOperation.ProduceType := ptXml
              else
                xOperation.ProduceType := ptJson;
              reqBind := _LoadApiByExampleReq('Req', oXml.Items.XmlCheckedItemByTag['Req'], reqXsd);
              rpyBind := _LoadApiByExampleRpy('Rpy', oXml.Items.XmlCheckedItemByTag['Rpy'], rpyXsd);
              if Assigned (rpyBind)
              and (rpyBind is TXml)
              and Assigned (rpyXml.xsd) then
                for x := 0 to rpyXml.Items.Count - 1 do
                  rpyXml.Items.XmlItems[x].Xsd.MediaType := xOperation.Accept;
              fltBind := _LoadApiByExampleFlt('Flt');
              if Alias <> reqTagName then
              begin
                if Assigned (reqBind) then reqBind.Name := Alias;
                if Assigned (rpyBind) then rpyBind.Name := Alias;
              end;
            end;
          end;
        end;
      finally
        FreeAndNil(oList);
      end;
    end;
    f := Wsdls.IndexOfObject(xWsdl);
    if (xWsdl.Services.Count > 0)
    and (f < 0) then
      Wsdls.AddObject(xWsdl.Name, xWsdl);
    if (xWsdl.Services.Count = 0)
    and (f > -1) then
      Wsdls.Delete(f);
  finally
    FreeAndNil(sList);
  end;
end;

procedure TWsdlProject.cobolOperationsUpdate(aXml: TXml; aMainFileName: String);
  procedure _getDescriptionFiles (aXml: TXml; aFileNames: TJBStringList);
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
  function _LoadCobolMsg (sXml: TXml; aFileNames: TJBStringList; var aDescrFilename: String): TIpmItem;
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
  sList: TJBStringList;
  oXml, xXml: TXml;
  xXsd: TXsd;
  f, x, o: Integer;
  xOperation: TWsdlOperation;
  xFileNames: TJBStringList;
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

  sList := TJBStringList.Create;
  sList.Sorted := True;
  xFileNames := TJBStringList.Create;
  xFileNames.Sorted := True;
{$ifndef NoGUI}
  if xmlUtil.CheckAndPromptFileNames(aMainFileName, aXml, True) then
    StubChanged := True;
{$endif}
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
          xOperation.reqRecognition := TJBStringList.Create;
          xOperation.rpyRecognition := TJBStringList.Create;
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
          Documentation.Text := oXml.Items.XmlCheckedValueByTag['Annotation'];
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
      f := Wsdls.IndexOfObject(CobolWsdl);
      if (Operations.Count > 0)
      and (f < 0) then
        Wsdls.AddObject(CobolWsdl.Name, CobolWsdl);
      if (Operations.Count = 0)
      and (f > -1) then
        Wsdls.Delete(f);
    end;
  finally
    FreeAndNil(sList);
    FreeAndNil(xFileNames);
  end;
end;

function TWsdlProject.operationRecognitionXml(aLabel: String;
  aType: TRecognitionType; aSl: TJBStringList): TXml;
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

function TWsdlProject.ApiByExampleOperationsXml(aMainFileName: String): TXml;
var
  x, y, s: Integer;
  xService: TWsdlService;
  xOperation: TWsdlOperation;
  xXml: TXml;
begin
  xXml := TXml.CreateAsString('ApiByExampleOperations', '');
  try
    for s := 0 to ApiByExampleWsdl.Services.Count - 1 do
    begin
      xService := ApiByExampleWsdl.Services.Services[s];
      with xXml.AddXml(TXml.CreateAsString('Service', '')) do
      begin
        AddXml (TXml.CreateAsString('Name', xService.Name));
//        AddXml (TXml.CreateAsString('Address', xService.Host));
        AddXml (TXml.CreateAsString('Path', xService.openApiPath));
        for x := 0 to ApiByExampleWsdl.Services.Services[s].Operations.Count - 1 do
        begin
          xOperation := ApiByExampleWsdl.Services.Services[s].Operations.Operations[x];
          with AddXml(TXml.CreateAsString('Operation', '')) do
          begin
            AddXml (TXml.CreateAsString('Alias', xOperation.Alias));
            AddXml (TXml.CreateAsString('FileAlias', xOperation.FileAlias));
            if xOperation.Documentation.Count > 0 then
              AddXml (TXml.CreateAsString('Annotation', xOperation.Documentation.Text));
            AddXml (TXml.CreateAsString('Verb', xOperation.httpVerb));
            AddXml (TXml.CreateAsString('consumes', xOperation.Consumes));
            AddXml (TXml.CreateAsString('produces', xOperation.Produces));
            if Assigned (xOperation.reqBind) then
            begin
              with AddXml (TXml.CreateAsString('Req', '')) do
              begin
                with xOperation.reqXsd.sType.ElementDefs do
                begin
                  for y := 0 to Count - 1 do
                  begin
                    if Xsds[y].ParametersType <> oppBody then
                    begin
                      with AddXml (TXml.CreateAsString('parameter', '')) do
                      begin
                        AddXml (TXml.CreateAsString('name', Xsds[y].ElementName));
                        AddXml (TXml.CreateAsString('required', BoolToStr(Xsds[y].minOccurs = '1', 'true', 'false')));
                        AddXml (TXml.CreateAsString('in', OperationParametersTypeNames[Xsds[y].ParametersType]));
                      end;
                    end;
                  end;
                end;
                with xOperation.reqXsd.sType.ElementDefs do
                  for y := 0 to Count - 1 do
                    if Xsds[y].ParametersType = oppBody then
                      AddXml (TXml.CreateAsString('SampleFile', Xsds[y].FileName));
              end;
            end;
            if Assigned (xOperation.rpyBind) then
            begin
              with AddXml (TXml.CreateAsString('Rpy', '')) do
              begin
                with xOperation.rpyXsd.sType.ElementDefs do
                begin
                  for y := 0 to Count - 1 do
                  begin
                    with AddXml (TXml.CreateAsString('response', '')) do
                    begin
                      AddXml (TXml.CreateAsInteger('code', Xsds[y].ResponseNo));
                      AddXml (TXml.CreateAsString('SampleFile', Xsds[y].FileName));
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    xXml.CheckDownline(True);
    result := TXml.Create(-1000, OperationDefsXsd.FindXsd ('OperationDefs.ApiByExampleOperations'));
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
        if xOperation.Documentation.Count > 0 then
          AddXml (TXml.CreateAsString('Annotation', xOperation.Documentation.Text));
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
          if xOperation.Documentation.Count > 0 then
            AddXml (TXml.CreateAsString('Annotation', xOperation.Documentation.Text));
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

function TWsdlProject.gethasApiByExplampleOperations: Boolean;
begin
  result := (ApiByExampleWsdl.Services.Count > 0)
        and (ApiByExampleWsdl.Services.Services[0].Operations.Count > 0)
          ;
end;

function TWsdlProject.gethasCobolOperations: Boolean;
begin
    result := (CobolWsdl.Services.Count > 0)
          and (CobolWsdl.Services.Services[0].Operations.Count > 0)
            ;
end;

function TWsdlProject.gethasFormalOperations: Boolean;
var
  x: Integer;
  xWsdl: TWsdl;
begin
  result := False;
  x := 0;
  while (x < Wsdls.Count) and (not result) do
  begin
    xWsdl := Wsdls.Objects[x] as TWsdl;
    result := (xWsdl <> FreeFormatWsdl)
          and (xWsdl <> ApiByExampleWsdl)
          and (xWsdl <> CobolWsdl)
            ;
    Inc (x);
  end;
end;

function TWsdlProject.gethasFreeformatOperations: Boolean;
begin
    result := (FreeFormatWsdl.Services.Count > 0)
          and (FreeFormatWsdl.Services.Services[0].Operations.Count > 0)
            ;
end;

function TWsdlProject.getHasOneTimeContextsColumn: Boolean;
var
  c: Integer;
begin
  result := False;
  if Assigned (projectContexts) then
    for c := 1 to projectContexts.ColCount - 1 do
      if isOneTimeContextsColumn(projectContexts, c) then
        result := True;
end;

function TWsdlProject.getIsBusy: Boolean;
begin
  AcquireLogLock;
  try
    result := fIsBusy;
  finally
    ReleaseLogLock;
  end;
end;

function TWsdlProject.getRemoteServerUrl: String;
begin
  result := '';
  if Assigned(remoteServerConnectionXml) then
    result := remoteServerConnectionXml.Items.XmlValueByTag['Address'];
end;

function TWsdlProject.getVersionInfoAsString: String;
begin
  result := Format('%d.%d.%d.%d', [majorVersion, minorVersion, revision, build]);
end;

function TWsdlProject.SendNoneMessage(aOperation: TWsdlOperation; aMessage: String; aLog: TLog): String;
var
  URL, querySep, headerSep: String;
  x: Integer;
begin
  if aOperation.isOpenApiService then
  begin
    URL := 'http://none';
    aLog.PathFormat := aOperation.WsdlService.logPathFormat;
    URL := URL
         + aOperation.WsdlService.openApiPath;
    querySep := '?';
    headerSep := '';
    for x := 0 to aOperation.reqXml.Items.Count - 1 do with aOperation.reqXml.Items.XmlItems[x] do
    begin
      if Checked
      and Assigned (Xsd) then
      begin
        if (Xsd.ParametersType = oppPath) then
        begin
          URL := ReplaceStr(URL, '{' + Name + '}', ValueFromJsonArray(true));
          aLog.PathFormat := ReplaceStr(aLog.PathFormat, '{' + Name + '}', '%s');
        end;
        if (Xsd.ParametersType = oppQuery) then
        begin
          URL := URL + querySep + Name + '=' + ValueFromJsonArray(true);
          querySep := '&';
        end;
        if (Xsd.ParametersType = oppHeader) then
        begin
          aLog.RequestHeaders := aLog.RequestHeaders
                               + headerSep
                               + Name
                               + ': '
                               + ValueFromJsonArray(false)
                               ;
          headerSep := LineEnding;
        end;
        if (Xsd.ParametersType = oppBody)
        and (aOperation.OpenApiVersion [1] <> '2') then
        begin
          aOperation.ContentType := Xsd.MediaType;
        end;
      end;
    end;
    with TIdURI.Create(URL) do
    try
      aLog.httpDocument := Path + Document;
      alog.httpParams := Params;
    finally
      Free;
    end;
  end;
end;

function TWsdlProject.FindOperationOnRequest(aLog: TLog; aDocument, aString: String; aDoClone: Boolean): TWsdlOperation;
var
  xXml: TXml;
begin
  result := FindOpenApiOnLog(aLog);
  if Assigned (Result) then
  begin
    if aDoClone then
    begin
      Result.AcquireLock;
      try
        result := TWsdlOperation.Create(Result);
      finally
        Result.ReleaseLock;
      end;
    end;
    aLog.OpenApiRequestToBindables(Result);
    aLog.RequestInfoToBindables (Result);
    Exit;
  end;
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
      begin
        aLog.OperationName:=result.Alias;
      end;
    finally
      //ReleaseLock;
    end;
    if not Assigned (result) then
      Exit;
    result.AcquireLock;
    try
      if aDoClone then
      begin
        result := TWsdlOperation.Create(result);
        if result.PrepareErrors <> '' then
          raise Exception.CreateFmt('%s (%s)', [result.PrepareErrors, result.reqTagName]);
      end;
      aLog.RequestInfoToBindables (Result);
      case result.WsdlService.DescriptionType of
        ipmDTFreeFormat: result.FreeFormatReq := aString;
        ipmDTCobol: (result.reqBind as TIpmItem).BufferToValues (FoundErrorInBuffer, aString);
        ipmDTXml: result.XmlRequestToBindables (xXml, False);
        ipmDTXsd: result.XmlRequestToBindables (xXml, True);
        ipmDTWsdl: result.XmlRequestToBindables (xXml, True);
      end;
    finally
      result.ReleaseLock;
    end;
  finally
    FreeAndNil (xXml);
  end;
end;

function TWsdlProject.FindOpenApiOnLog (aLog : TLog): TWsdlOperation;
  function _ServiceFromPath: TWsdlService;
  var
    x: Integer;
    sx, sd: String;
  begin
    result := nil;
    with TRegExpr.Create do
    try
      for x := 0 to PathRegexps.Count - 1 do
      begin
        sx := PathRegexps.Strings[x];
        Expression := sx;
        sd := aLog.PathFormat;
        if (sd <> '')
        and Exec(sd) then
        begin
          result := PathInfos.Objects[x] as TWsdlService;
          aLog.PathFormat := PathFormats.Strings[x];
          Exit;
        end;
      end;
    finally
      free;
    end;
  end;
var
  x: Integer;
  xService: TWsdlService;
begin
  result := nil;
  xService := _ServiceFromPath;
  if Assigned (xService) then
  begin
    for x := 0 to xService.Operations.Count - 1 do
    with xService.Operations do
    begin
      if Operations[x].httpVerb = aLog.httpCommand then
      begin
        result := Operations[x];
        aLog.OperationName := Operations[x].Alias;
        aLog.ServiceName := xService.Name;
      end;
    end;
  end;
end;

function TWsdlProject.FindOperationOnLog (aLog: TLog): TWsdlOperation;
var
  xXml: TXml;
begin
  result := FindOpenApiOnLog (aLog);
  if Assigned (result) then
    Exit;
  xXml := TXml.Create;
  try
    try
      xXml.LoadFromString(aLog.RequestBody, nil);
    except
      xXml.Name := '';
    end;
    if xXml.Name = '' then
      if aLog.RequestBody <> '' then
        result := FindCcbOperationOnRequest (aLog, aLog.RequestBody)
      else
        result := FindOperationOnDocument (aLog.httpDocument)
    else
    begin
      xXml.SeparateNsPrefixes;
      xXml.ResolveNameSpaces;
      result := FindXmlOperationOnRequest (aLog.httpDocument, xXml);
      if not Assigned (result) then
        result := FindCcbOperationOnRequest (aLog, aLog.RequestBody);
    end;
    if Assigned (result) then
      aLog.OperationName:=result.Alias;
  finally
    FreeAndNil (xXml);
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

procedure TWsdlProject.EnvironmentListClear;
var
  x: Integer;
begin
  for x := 0 to EnvironmentList.Count - 1 do
    (EnvironmentList.Objects [x] as TXml).Free;
  EnvironmentList.Clear;
end;

procedure TWsdlProject.HTTPServerRemoteControlApi ( AContext: TIdContext
                                                  ; ARequestInfo: TIdHTTPRequestInfo
                                                  ;AResponseInfo: TIdHTTPResponseInfo
                                                  );
  function _htmlreportTestSummary: String;
  var
    xList: TSnapshotList;
    x: Integer;
  begin
    result := '';
    xList := TSnapshotList.Create;
    try
      AcquireLogLock;
      try
        for x := 0 to displayedSnapshots.Count - 1 do
          xList.AddObject('', displayedSnapshots.SnapshotItems[x]);
      finally
        ReleaseLogLock;
      end;
      result := htmlReportTestSummary(self, xList);
    finally
      xList.clear;
      xList.Free;
    end;
  end;

  procedure _sjow (aString: String);
  begin
    if true then
    with SeparatedStringList(nil, aString, '/') do
    try
      SjowMessage(Format ('%s Count: %d %s', [ARequestInfo.Command, Count, aString]));
    finally
      Free;
    end;
  end;

var
  x: Integer;
  xRequestBody, xFileName, xsep: String;
  xBodyXml, nameXml, valueXml, fXml, xXml: TXml;
  xSnapshot: TSnapshot;
  xLogList: TLogList;
  xOperation: TWsdlOperation;
  sl: TJBStringList;
begin
  xBodyXml := nil;
  AResponseInfo.ContentEncoding := 'identity';
  AResponseInfo.ContentType := 'application/json';
  AResponseInfo.ResponseNo := 200; // nice defaults
  try   // finally
    try  // Except
      if (ARequestInfo.Command = 'PATCH')
      or (ARequestInfo.Command = 'POST')
      or (ARequestInfo.Command = 'PUT') then
      begin
        xRequestBody := httpRequestStreamToString(ARequestInfo, AResponseInfo);
        xBodyXml := TXml.Create;
        if pos ('XML', UpperCase(ARequestInfo.ContentType)) < 1 then
          xBodyXml.LoadJsonFromString(xRequestBody, nil)
        else
          xBodyXml.LoadFromString(xRequestBody, nil);
      end;
    _sjow (ARequestInfo.Document);
      with SeparatedStringList(nil, ARequestInfo.Document, '/') do // /_progName/api/Rest
                                                                   //0/1        /2  /3...
      try
        if (Count = 2)
        and (Strings[1] = 'favicon.ico')
        and (ARequestInfo.Command = 'GET')
        then begin
          AResponseInfo.SmartServeFile ( AContext
                                       , ARequestInfo
                                       , faviconIcoFileName
                                       );
          Exit;
        end;
        if (   (Count = 3)
            or ((Count = 4) and (Strings[3] = 'index.html'))
           )
        and (ARequestInfo.Command = 'GET')
        then begin
          AResponseInfo.ContentType := 'text/html';
          AResponseInfo.ContentText := ReplaceStrings ( xmlio.ReadStringFromFile(indexHtmlFileName, nil)
                                                      , '__progname__'
//                                                    , _progName
                                                      , 'apiUi'
                                                      , False
                                                      , False
                                                      );
          Exit;
        end;

        if (Count = 4)
        and (Strings[3] = 'swagger.yaml')
        and (ARequestInfo.Command = 'GET')
        then begin
          AResponseInfo.ContentText := ReplaceStrings ( xmlio.ReadStringFromFile(swaggerYamlFileName, nil)
                                                      , '__hostname__'
                                                      , xmlio.GetHostName
                                                      , False
                                                      , False
                                                      );
          Exit;
        end;

        if (Count = 4)
        and (LowerCase(Strings[3]) = 'about')
        and (ARequestInfo.Command = 'GET')
        then with TXml.CreateAsString('json', '') do
        try
          AddXml (TXml.CreateAsString('program', _progName + ' ' + _xmlProgVersion));
          AddXml (TXml.CreateAsString('copyright', '© 2009 - 2021 Jan Bouwman'));
          AddXml (TXml.CreateAsString('built', 'Date: ' + {$I %date%}));
          AddXml (TXml.CreateAsString('lazarusVersion', LazVersion.laz_version));
          AddXml (TXml.CreateAsString('fpcVersion', {$I %fpcversion%}));
          AResponseInfo.ContentText := StreamJSON(0, False);
          Exit;
        finally
          Free;
        end;

        if (Count = 4)
        and (Strings[3] = 'envvars') then
        begin
          if (ARequestInfo.Command = 'GET')
          then begin
            with EnvVarsAsXml do
            try
              AResponseInfo.ContentText := StreamJSON(0, False);
              Exit;
            finally
              Free;
            end;
          end;
          if (ARequestInfo.Command = 'POST')
          then begin
            xBodyXml.Name := 'envVars';
            EnvVarsFromXml(xBodyXml);
            Exit;
          end;
        end;

        if (Count = 4)
        and (Strings[3] = 'testconnection')
        and (ARequestInfo.Command = 'GET')
        then begin
          Exit;
        end;

        if (Count = 4)
        and (Strings[3] = 'testsummaryreport')
        and (ARequestInfo.Command = 'GET')
        then begin
          AResponseInfo.ContentText := _htmlreportTestSummary;
          AResponseInfo.ContentType := 'text/html; charset=UTF-8';
          Exit;
        end;

        if (Count = 4)
        and (Strings[3] = 'logs')
        and (ARequestInfo.Command = 'DELETE')
        then begin
          doClearLogs := True;
          Exit;
        end;

        if (Count = 5)
        and (Strings[3] = 'logs')
        and (Strings[4] = 'getandremove')
//      and (   (ARequestInfo.Command = 'PUT')
        then begin
          AcquireLogLock;
          try
            if Assigned (fClearedLogs) then
            begin
              raise Exception.Create('Service temporarely unavaillable');
            end;
            fClearedLogs := TLogList.Create;
            with fClearedLogs do
            begin
              for x := 0 to displayedLogs.Count - 1 do
                SaveLog('', displayedLogs.LogItems[x]);
              for x := 0 to toDisplayLogs.Count - 1 do
                SaveLog('', toDisplayLogs.LogItems[x]);
              displayedLogs.Clear;
              toDisplayLogs.Clear;
              AResponseInfo.ContentText := LogsAsString (projectFileName);
              AResponseInfo.ContentType := 'text/xml; charset=UTF-8';
            end;
            Exit;
          finally
            ReleaseLogLock;
          end;
        end;

        //   /operations/{operationAlias}/delay:
        if (Count = 6)
        and (Strings[3] = 'operations')
        and ((Strings[5] = 'delay'))
        and (ARequestInfo.Command = 'GET')
        then begin
          xOperation := allOperations.FindOnAliasName(Strings[4]);
          if not Assigned (xOperation) then
          begin
            AResponseInfo.ResponseNo := 404;
            Exit;
          end;
          xOperation.AcquireLock;
          try
            with TXml.CreateAsString('json', '') do
            try
              with AddXml (TXml.CreateAsString('randomBetween', '')) do
              begin
                AddXml (TXml.CreateAsInteger('min', xOperation.DelayTimeMsMin)).jsonType := jsonNumber;
                AddXml (TXml.CreateAsInteger('max', xOperation.DelayTimeMsMax)).jsonType := jsonNumber;
              end;
              AResponseInfo.ContentText := StreamJSON(0, False);
              Exit;
            finally
              free;
            end;
          finally
            xOperation.ReleaseLock;
          end;
        end;

        if (Count = 6)
        and (Strings[3] = 'operations')
        and ((Strings[5] = 'delay'))
        and (ARequestInfo.Command = 'PUT')
        then begin
          xOperation := allOperations.FindOnAliasName(Strings[4]);
          if not Assigned (xOperation) then
          begin
            AResponseInfo.ResponseNo := 404;
            Exit;
          end;
          xXml := xBodyXml.ItemByTag['randomBetween'];
          if not Assigned (xXml) then
            raise Exception.Create('"randomBetween" expected as root element');
          xOperation.AcquireLock;
          try
            xOperation.DelayTimeMsMin := xXml.Items.XmlIntegerByTagDef['min', 0];
            xOperation.DelayTimeMsMax := xXml.Items.XmlIntegerByTagDef['max', xOperation.DelayTimeMsMin];
          finally
            xOperation.ReleaseLock;
          end;
          Exit;
        end;

        if (Count = 5)
        and (Strings[3] = 'logs')
        and ((Strings[4] = 'fetchfromremoteserver'))
        and (ARequestInfo.Command = 'POST')
        then begin
          if ARequestInfo.Params.Values['initclientlog'] = 'true' then
            doClearLogs := True;
          LogsFromRemoteServer;
          Exit;
        end;

        if (Count = 5)
        and (Strings[3] = 'logs')
        and ((Strings[4] = 'snapshot'))
        and (ARequestInfo.Command = 'POST')
        then begin
          nameXml := xBodyXml.FindXml('json.name');
          if not Assigned (nameXml) then
          begin
            AResponseInfo.ResponseNo := 400;
            Exit;
          end;
          UpsertSnapshot ( nameXml.Value
                         , CurrentFolder + DirectorySeparator + nameXml.Value + '.xml'
                         , ReferenceFolder + DirectorySeparator + nameXml.Value + '.xml'
                         , (hasGui = False)
                         );
          Exit;
        end;

        if (Count = 5)
        and (Strings[3] = 'project')
        and (Strings[4] = 'descriptors')
        and (ARequestInfo.Command = 'POST')
        then begin
          nameXml := xBodyXml.FindXml('json.name');
          if not Assigned (nameXml) then
          begin
            AResponseInfo.ResponseNo := 400;
            Exit;
          end;
          xFileName := xmlio.ExpandRelativeFileName(projectFileName, nameXml.Value);
          AResponseInfo.ContentType := GetMIMETypeFromFile(xFileName);
          AResponseInfo.ContentText := xmlio.ReadStringFromFile(xFileName, nil);
          Exit;
        end;

        if (Count = 5)
        and (Strings[3] = 'project')
        and (Strings[4] = 'descriptors')
        and (ARequestInfo.Command = 'GET')
        then begin
          sl := TJBStringList.Create;
          try
            ExplodeStr(ARequestInfo.QueryParams, '&', sl);
            for x := 0 to sl.Count - 1 do
            begin
              if sl.Names[x] = 'filename' then
              begin
                xFileName := xmlio.ExpandRelativeFileName(projectFileName, sl.ValueFromIndex[x]);
                AResponseInfo.ContentType := GetMIMETypeFromFile(xFileName);
                AResponseInfo.ContentText := xmlio.ReadStringFromFile(xFileName, nil);
                Exit;
              end;
            end;
          finally
            sl.Free;
          end;
          with TXml.Create do
          try
            Name := 'json';
            jsonType := jsonArray;
            for x := 0 to Wsdls.Count - 1 do
              AddXml (TXml.CreateAsString('-', ReplaceStrings( ExtractRelativeFileName (projectFileName, Wsdls.Strings [x])
                                                             , DirectorySeparator
                                                             , '/'
                                                             , False
                                                             , False
                                                             )
                                         )
                     );
            AResponseInfo.ContentText := StreamJSON(0, False);
            Exit;
          finally
            Free;
          end;
        end;

        if (Count = 5)
        and (Strings[3] = 'project')
        and (Strings[4] = 'information')
        and (ARequestInfo.Command = 'GET')
        then begin
          with InformationAsXml do
          try
            AResponseInfo.ContentText := StreamJSON(0, False);
            Exit;
          finally
            Free;
          end;
        end;

        if (Count = 5)
        and (Strings[3] = 'snapshot')
        and ((Strings[4] = 'checkschemacompliancy'))
        and (ARequestInfo.Command = 'POST')
        then begin
          nameXml := xBodyXml.FindXml('json.name');
          if not Assigned (nameXml) then
          begin
            AResponseInfo.ResponseNo := 400;
            Exit;
          end;
          if ARequestInfo.Params.Values['createsnapshot'] = 'true' then
          begin
            UpsertSnapshot ( nameXml.Value
                           , CurrentFolder + DirectorySeparator + nameXml.Value + '.xml'
                           , ReferenceFolder + DirectorySeparator + nameXml.Value + '.xml'
                           , (hasGui = False)
                           );
          end;
          xSnapshot := FindSnapshot (nameXml.Value);
          if not Assigned (xSnapshot) then
            raise Exception.Create(nameXml.Value + ' not found');
          xLogList := TLogList.Create;
          try
            OpenMessagesLog(xSnapshot.FileName, True, False, xLogList);
            with xLogList.SchemaCompliancyAsXml do
            try
              AResponseInfo.ContentText := StreamJSON(0, False);
            finally
              Free;
            end;
          finally
            xLogList.Free;
          end;
          Exit;
        end;

        if (Count = 5)
        and (Strings[3] = 'snapshot')
        and ((Strings[4] = 'checkregression'))
        and (ARequestInfo.Command = 'POST')
        then begin
          nameXml := xBodyXml.FindXml('json.name');
          if not Assigned (nameXml) then
          begin
            AResponseInfo.ResponseNo := 400;
            Exit;
          end;
          if ARequestInfo.Params.Values['createsnapshot'] = 'true' then
          begin
            UpsertSnapshot ( nameXml.Value
                           , CurrentFolder + DirectorySeparator + nameXml.Value + '.xml'
                           , ReferenceFolder + DirectorySeparator + nameXml.Value + '.xml'
                           , (hasGui = False)
                           );
          end;
          xSnapshot := FindSnapshot (nameXml.Value);
          if not Assigned (xSnapshot) then
            raise Exception.Create(nameXml.Value + ' not found');
          xSnapshot.doReport;
          with TXml.CreateAsString('json', '') do
          try
            AddXml (TXml.CreateAsString('result', xSnapshot.statusAsText));
            AResponseInfo.ContentText := StreamJSON(0, False);
          finally
            free;
          end;
          Exit;
        end;

        if (Count = 4)
        and (Strings[3] = 'snapshots')
        and (ARequestInfo.Command = 'GET')
        then begin
          with TXml.CreateAsString('json', '') do
          try
            with AddXml (TXml.CreateAsString('snapshots', '')) do
            begin
              jsonType := jsonArray;
              sl := TJBStringList(FileUtil.FindAllFiles(CurrentFolder, '*.xml', False));
              try
                for x := 0 to sl.Count - 1 do
                begin
                  with AddXml (TXml.CreateAsString('_', '')) do
                  begin
                    AddXml (TXml.CreateAsString('name' , LazFileUtils.ExtractFileNameOnly(sl.Strings[x])));
                    AddXml (TXml.CreateAsTimeStamp('createdOn', xmlio.GetFileChangedTime(sl.Strings[x])));
                  end;
                end;
              finally
                sl.Free;
              end;
            end;
            AResponseInfo.ContentText := StreamJSON(0, False);
          finally
            free;
          end;
          Exit;
        end;

        if (Count = 5)
        and (Strings[3] = 'snapshots')
        and ((Strings[4] = 'testsummaryreport'))
        and (ARequestInfo.Command = 'GET')
        then begin
          with TSnapshotList.Create do
          try
            AcquireLogLock;
            try
              for x := 0 to displayedSnapshots.Count - 1 do
                AddObject('', displayedSnapshots.SnapshotItems[x]);
            finally
              ReleaseLogLock;
            end;
            AResponseInfo.ContentText := htmlReportTestSummary(self, thisSnapshotList);
            AResponseInfo.ContentType := 'text/html';
          finally
            Free;
          end;
          Exit;
        end;

        if (Count = 4)
        and (Strings[3] = 'notifications')
        and (ARequestInfo.Command = 'GET')
        then begin
          with TXml.CreateAsString('json', '') do
          try
            with AddXml (TXml.CreateAsString('notifications', '')) do
            begin
              jsonType := jsonArray;
              AcquireLogLock;
              try
                for x := 0 to displayedExceptions.Count - 1 do
                begin
                  with AddXml (TXml.CreateAsString('_', '')) do
                  begin
                    AddXml (TXml.CreateAsTimeStamp('createdOn', displayedExceptions.EventItems[x].TimeStamp));
                    AddXml (TXml.CreateAsString('text' , displayedExceptions.EventItems[x].Text));
                  end;
                end;
              finally
                ReleaseLogLock;
              end;
            end;
            AResponseInfo.ContentText := StreamJSON(0, False);
          finally
            free;
          end;
          Exit;
        end;

        if (Count = 4)
        and (LowerCase(Strings[3]) = 'projectdesign')
        and (ARequestInfo.Command = 'GET')
        then begin
          AResponseInfo.ContentText := ProjectDesignAsString;
          AResponseInfo.ContentType := 'application/xml';
          Exit;
        end;

        if (Count = 5)
        and (LowerCase(Strings[3]) = 'projectdesign')
        and (LowerCase(Strings[4]) = 'files')
        and (ARequestInfo.Command = 'POST')
        then begin
          if xBodyXml.Name <> 'name' then
          begin
            AResponseInfo.ResponseNo := 400;
            Exit;
          end;
          xFileName := ExpandRelativeFileName(projectFileName, xBodyXml.Value);
          AResponseInfo.ServeFile(AContext, xFileName);
{
          AResponseInfo.SmartServeFile ( AContext
                                       , ARequestInfo
                                       , xFileName
                                       );
}
          Exit;
        end;

        if (Count = 5)
        and (LowerCase(Strings[3]) = 'projectdesign')
        and (LowerCase(Strings[4]) = 'files')
        and (ARequestInfo.Command = 'GET')
        then begin
          xFilename := ARequestInfo.Params.Values['name'];
          if xFileName = '' then
          begin
            AResponseInfo.ResponseNo := 400;
            Exit;
          end;
          xFileName := ExpandRelativeFileName(projectFileName, xFileName);
          AResponseInfo.SmartServeFile ( AContext
                                       , ARequestInfo
                                       , xFileName
                                       );
          Exit;
        end;

        if (Count > 5)
        and (LowerCase(Strings[3]) = 'projectdesign')
        and (LowerCase(Strings[4]) = 'files')
        and (ARequestInfo.Command = 'GET')
        then begin
          xFilename := '';
          xsep := '';
          for x := 5 to Count - 1 do
          begin
            xFileName := xFileName + xsep + Strings[x];
            xsep := DirectorySeparator;
          end;
          xFileName := ExpandRelativeFileName(projectFileName, xFileName);
          AResponseInfo.SmartServeFile ( AContext
                                       , ARequestInfo
                                       , xFileName
                                       );
          Exit;
        end;

        if (Count = 4)
        and (LowerCase(Strings[3]) = 'projectdesign')
        and (ARequestInfo.Command = 'POST')
        then begin
          AResponseInfo.ResponseNo := 202;
          nameXml := xBodyXml.FindXml('WsdlStubCase.FileName');
          if not Assigned (nameXml) then
          begin
            AResponseInfo.ResponseNo := 400;
            Exit;
          end;
          SjowMessage('projectdesign received: ' + nameXml.Value);
//        if nameXml.Value <> projectFileName then
//          raise Exception.CreateFmt('Received project name (%s) differs from current projectname (%s)', [nameXml.Value, projectFileName]);
          TProcedureThread.Create(False, True, 200, self, ProjectDesignFromApiRequestString, xRequestBody, projectFileName);
          Exit;
        end;

        if (Count = 4)
        and (LowerCase(Strings[3]) = 'refresh')
        and (ARequestInfo.Command = 'POST')
        then begin
          AResponseInfo.ResponseNo := 202;
          TProcedureThread.Create(False, True, self, RefreshCommand);
          Exit;
        end;

        if (Count = 6)
        and (Strings[3] = 'snapshots')
        and (Strings[4] = 'download')
        and (Strings[5] <> '')
        and (ARequestInfo.Command = 'GET')
        then begin
          if not LazFileUtils.FileExistsUTF8(CurrentFolder + DirectorySeparator + Strings[5] + '.xml') then
            raise Exception.CreateFmt ('Snapshot %s not found', [Strings[5]]);
          AResponseInfo.SmartServeFile ( AContext
                                       , ARequestInfo
                                       , CurrentFolder + DirectorySeparator + Strings[5] + '.xml'
                                       );
          Exit;
        end;

        if (Count = 4)
        and (Strings[3] = 'snapshots')
        and (ARequestInfo.Command = 'DELETE')
        then begin
          doClearSnapshots := True;
          Exit;
        end;

        if (Count = 4)
        and (Strings[3] = 'executescript')
        then begin
          nameXml := xBodyXml.FindXml('json.name');
          if not Assigned (nameXml) then
          begin
            AResponseInfo.ResponseNo := 400;
            Exit;
          end;
          fXml := FindScript (nameXml.Value);
          if Assigned (fXml) then
          begin
            AResponseInfo.ResponseNo := 202;
            ProgressMax := 5;
            ProgressPos := 0;
            TProcedureThread.Create(False, False, 0, Self, Self.ScriptExecute, fXml as TObject);
          end
          else
            raise Exception.Create('Cannot find script based on: ' + nameXml.Value);
          Exit;
        end;

        if (Count = 4)
        and (Strings[3] = 'resetEnvVar')
        then begin
          nameXml := xBodyXml.FindXml('json.name');
          if not Assigned (nameXml) then
          begin
            AResponseInfo.ResponseNo := 400;
            Exit;
          end;
          wsdlz.resetEnvVar(allOperations.Operations[0], nameXml.Value);
          Exit;
        end;

        if (Count = 4)
        and (Strings[3] = 'resetEnvVars')
        then begin
          nameXml := xBodyXml.FindXml('json.regularExpression');
          if not Assigned (nameXml) then
          begin
            AResponseInfo.ResponseNo := 400;
            Exit;
          end;
          wsdlz.ResetEnvVars(allOperations.Operations[0], nameXml.Value);
          Exit;
        end;

        if (Count = 4)
        and (Strings[3] = 'context')
        and (ARequestInfo.Command = 'GET')
        then begin
          with TXml.CreateAsString('json', '') do
          try
            AddXml (TXml.CreateAsString('name', wsdlz.GetContext));
            AResponseInfo.ContentText := StreamJSON(0, False);
            Exit;
          finally
            free;
          end;
          Exit;
        end;

        if (Count = 4)
        and (Strings[3] = 'context')
        and (ARequestInfo.Command = 'POST')
        then begin
          nameXml := xBodyXml.FindXml('json.name');
          if not Assigned (nameXml) then
          begin
            AResponseInfo.ResponseNo := 400;
            Exit;
          end;
          with TXml.CreateAsString('json', '') do
          try
            with AddXml (TXml.CreateAsString('previousContext', '')) do
            begin
              AddXml (TXml.CreateAsString('name', wsdlz.SetContext(nameXml.Value)));
            end;
            AResponseInfo.ContentText := StreamJSON(0, False);
            Exit;
          finally
            free;
          end;
          Exit;
        end;

        if (Count = 4)
        and (Strings[3] = 'setEnvVar')
        then begin
          nameXml := xBodyXml.FindXml('json.name');
          valueXml := xBodyXml.FindXml('json.value');
          if not Assigned (nameXml)
          or not Assigned (valueXml) then
          begin
            AResponseInfo.ResponseNo := 400;
            Exit;
          end;
          wsdlz.setEnvVar(allOperations.Operations[0], nameXml.Value, valueXml.Value);
          Exit;
        end;

        if (Count = 4)
        and (LowerCase(Strings[3]) = 'versioninfo')
        and (ARequestInfo.Command = 'GET')
        then with TXml.CreateAsString('json', '') do
        try
          AddXml (TXml.CreateAsString('version', _xmlProgVersion));
          AResponseInfo.ContentText := StreamJSON(0, False);
          Exit;
        finally
          Free;
        end;


        AResponseInfo.ResponseNo := 400;
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
    FreeAndNil(xBodyXml);
  end;
end;

procedure TWsdlProject.HTTPServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  function _hasCorsHeaders (aList: TStringList): boolean;
  var
    x: Integer;
  begin
    result := false;
    for x := 0 to aList.count - 1 do
    begin
      if AnsiStartsText('Access-Control-Request', aList.Strings[x]) then
        result := true;
    end;
  end;
  procedure _acceptCorsRequest (aRpyList: TStringList);
  begin
    aRpyList.Add ('Access-Control-Allow-Origin: *');
    aRpyList.Add ('Access-Control-Allow-Methods: *');
    aRpyList.Add ('Access-Control-Allow-Headers: *');
    aRpyList.Add ('Access-Control-Expose-Headers: *');
    aRpyList.Add ('Access-Control-Max-Age: 600');
  end;
var
  xLog: TLog;
  xProcessed: Boolean;
  xStream: TMemoryStream;
  xNotification, xDocument: String;
  xOnRequestViolatingAddressPath: TOnRequestViolating;
begin
  xProcessed := False;
  try // finally set for hhtp reply
  //xDocument := '/' + _ProgName + '/api';
    if (ARequestInfo.Command = 'OPTIONS')
    and _hasCorsHeaders (ARequestInfo.RawHeaders) then
    begin
      AResponseInfo.ResponseNo := 204;
      AResponseInfo.ContentEncoding := 'identity';
      if CORS = corsException then
      begin
        AResponseInfo.ResponseNo := corsExceptionHttpCode;
        Exit;
      end;
      if CORS = corsRefuse then
        Exit;
      _acceptCorsRequest (AResponseInfo.CustomHeaders);
      if CORS = corsAcceptWithoutLogging then
        exit;
    end;
    xDocument := '/apiUi/api';
    if (ARequestInfo.Document = xDocument)
    or AnsiStartsStr(xDocument + '/', ARequestInfo.Document)
    or (ARequestInfo.Document = '/favicon.ico')
    then begin
      HTTPServerRemoteControlApi(AContext, ARequestInfo, AResponseInfo);
      Exit;
    end;
    {$ifdef windows}
    CoInitialize (nil);
    {$endif}
    try
      xLog := TLog.Create;
      xLog.ReplyHeaders := AResponseInfo.CustomHeaders.Text;
      xLog.InboundTimeStamp := Now;
      xLog.httpUri := ARequestInfo.URI;
      xLog.TransportType := ttHttp;
      xLog.httpCommand := ARequestInfo.Command;
      xLog.httpDocument := ARequestInfo.Document;
      xLog.PathFormat := xLog.httpDocument;
      xLog.RequestHeaders := ARequestInfo.RawHeaders.Text;
      xLog.RequestContentType := ARequestInfo.RawHeaders.Values['Content-Type'];
      xLog.ReplyContentType := xLog.RequestContentType;
      xLog.httpParams := ARequestInfo.QueryParams;
      if AResponseInfo.ResponseNo = 0 then
        xlog.httpResponseCode := 200
      else
        xLog.httpResponseCode := AResponseInfo.ResponseNo;
      AResponseInfo.ContentEncoding := 'identity';
      try
        if (ARequestInfo.Command = 'PATCH')
        or (ARequestInfo.Command = 'POST')
        or (ARequestInfo.Command = 'PUT') then
        begin
          xLog.RequestBody := httpRequestStreamToString(ARequestInfo, AResponseInfo);
          xLog.InboundBody := xLog.RequestBody;
        end;
        if StartsText('multipart', ARequestInfo.ContentType) then
        with ParseMultiPartBody(ARequestInfo) do
        try
          xLog.RequestBody := StreamJSON(0, False);
        finally
          Free;
        end;
        if tryToProcessAsOpenApi (xLog) then
        begin
          AResponseInfo.ContentText := xLog.ReplyBody;
          Exit;
        end;
        if ARequestInfo.Command = 'TRACE' then
        begin
          AContext.Data := xLog;
          HTTPServerCommandTrace(AContext, ARequestInfo, AResponseInfo);
        end;
        // fromHERE
        if (ARequestInfo.Command = 'PATCH')
        or (ARequestInfo.Command = 'POST')
        or (ARequestInfo.Command = 'PUT') then
        begin
          try
            try
              CreateLogReply (xLog, xProcessed, True);
              if Assigned (xLog.Operation)
              and (xLog.Operation.SoapAddress <> '') then
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
              and (xLog.Operation.StubAction = saStub) then
                xLog.httpResponseCode := 202;
              if (xLog.Exception <> '')
              and ((xLog.httpResponseCode div 100) = 2)
              and (   (not Assigned (xLog.Operation))
                   or (not xLog.Operation.WsdlService.SuppressHTTP500)
                  ) then
                xLog.httpResponseCode := 500;
            except
              on e: exception do
              begin
                LogServerMessage(format('Exception %s. Exception is:"%s".', [e.ClassName, e.Message]), True, e);
                xLog.Exception := e.Message;
                if (not Assigned (xLog.Operation))
                or (not xLog.Operation.WsdlService.SuppressHTTP500) then
                  if (xLog.httpResponseCode div 100) = 2 then
                    xLog.httpResponseCode := 500;
              end;
            end;  // except
          finally
            aResponseInfo.ContentText := xLog.ReplyBody;
          end; // finally request
        end; // post
      finally
        if Assigned (xLog) then {still}
        begin
          DelayMS (xLog.DelayTimeMs);
          xLog.OutboundTimeStamp := Now;
          DisplayLog ('', xLog);
          AResponseInfo.ResponseNo := xLog.httpResponseCode;
          if xLog.ReplyHeaders <> '' then
          begin
            AResponseInfo.CustomHeaders.Text := xLog.ReplyHeaders;
            RemoveStdHttpHeaders (AResponseInfo.CustomHeaders);
          end;
          if xLog.ReplyContentType <> '' then
          begin
            AResponseInfo.ContentType := xLog.ReplyContentType;
            xLog.ReplyHeaders := AResponseInfo.CustomHeaders.Text;
          end;
        end;
      end;
    finally
    {$ifdef windows}
      CoUninitialize;
    {$endif}
    end;
  finally
    // setup for HTTP reply
    aResponseInfo.ContentStream := TMemoryStream.Create;
    if AResponseInfo.ContentEncoding <> 'identity' then
    begin
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
    end
    else
      WriteStringToStream(AResponseInfo.ContentText, AResponseInfo.ContentStream as TMemoryStream);
    aResponseInfo.ContentText := '';
  end;
end;

procedure TWsdlProject.HTTPServerCreatePostStream(AContext: TIdContext;
  AHeaders: TIdHeaderList; var VPostStream: TStream);
begin
  VPostStream := TMemoryStream.Create;
end;

procedure TWsdlProject.setIsBusy(AValue: Boolean);
begin
  AcquireLogLock;
  try
    fIsBusy := AValue;
  finally
    ReleaseLogLock;
  end;
end;

procedure TWsdlProject.SetProjectContext(AValue: String);
begin
  if fProjectContext = AValue then Exit;
  fProjectContext := AValue;
  xmlio.ProjectContext := fProjectContext;
end;

function TWsdlProject.tryToProcessAsOpenApi (aLog: TLog): Boolean;
var
  xOperation: TWsdlOperation;
  xMssg: TWsdlMessage;
begin
  result := False;
  aLog.Operation := FindOpenApiOnLog(aLog);
  if Assigned(aLog.Operation) then
  begin
    aLog.Operation.AcquireLock;
    try
      xOperation := TWsdlOperation.Create(aLog.Operation);
    finally
      aLog.Operation.ReleaseLock;
    end;
    try
      Result := True;
      aLog.OperationName := xOperation.Alias;
      alog.StubAction := saStub;
      try
        if xOperation.PrepareErrors <> '' then
          raise Exception.CreateFmt('%s (%s)', [xOperation.PrepareErrors, xOperation.Alias]);
        aLog.OpenApiRequestToBindables(xOperation);
        aLog.RequestInfoToBindables(xOperation);
        xOperation.Data := aLog;
        if IsActive then with xOperation.Cloned do
        begin
          AcquireLock;
          Inc (OperationCounter);
          aLog.OperationCount := OperationCounter;
          ReleaseLock;
        end;
        if doValidateInboundRequests(xOperation) then
        begin
          aLog.RequestValidateResult := '';
          aLog.RequestValidated := True;
          if not xOperation.reqBind.IsValueValid (aLog.RequestValidateResult) then
          begin
            if doReturnExceptionOnViolatingInboundRequest(xOperation) then
            begin
              if xOperation.inboundRequestSchemaValidationType = svAccordingProject then
                aLog.httpResponseCode := schemaValidationVioloationHttpResponseCode
              else
                aLog.httpResponseCode := xOperation.schemaValidationVioloationHttpResponseCode;
              raise Exception.Create('Schema validation error on request:' + LineEnding + aLog.RequestValidateResult);
            end;
          end;
        end;
        xOperation.InitDelayTime;
        if xOperation.doReadReplyFromFile then
        begin
          xMssg := xOperation.Messages.Messages[0];
          xOperation.ReadReplyFromFile;
        end
        else
          xMssg := xOperation.MessageBasedOnRequest;
        if not Assigned (xMssg) then
          Raise Exception.Create('Could not find any reply based on request');
        xOperation.rpyXml.ResetValues;
        xOperation.rpyXml.LoadValues (xMssg.rpyXml, True, True);
        if (xOperation.StubAction = saStub)
        and (IsActive) then
        begin
          xOperation.ExecuteBefore;
          xOperation.ExecuteRpyStampers;
          if xOperation.doDebug
          and Assigned (OnDebugOperationEvent) then
          begin
            DebugOperation := xOperation;
            OnDebugOperationEvent;
          end;
          if doValidateOutboundReplies(xOperation) then
          begin
            aLog.ReplyValidated := True;
            aLog.ReplyValidateResult := '';
            xOperation.rpyBind.IsValueValid (aLog.ReplyValidateResult);
          end;
        end;
        aLog.CorrelationId := xOperation.CorrelationIdAsText('; ');
        aLog.InitDisplayedColumns(xOperation, DisplayedLogColumns);
        aLog.doSuppressLog := (xOperation.doSuppressLog <> 0);
        aLog.DelayTimeMs := xOperation.DelayTimeMs;
        xOperation.rpyXml.jsonType := jsonObject;
        aLog.ReplyBody := xOperation.StreamReply (_progName, True);
        aLog.ReplyContentType := xOperation.apiReplyMediaType;
        aLog.ReplyInfoFromBindables(xOperation);
        if aLog.ReplyContentType = '' then
          try
            aLog.ReplyContentType := SeparatedStringN(nil, xOperation.Produces, LineEnding, 1);
          except
          end;
        aLog.httpResponseCode := xOperation.ResponseNo;
        if xOperation.StubAction = saRequest then
        begin
          aLog.ReplyBody := _progName + ' - Operation itself is a requestor ('+ xOperation.Alias +')';
          raise SysUtils.Exception.Create(aLog.ReplyBody);
        end;
      except
        on e: exception do
        begin
          if ((aLog.httpResponseCode div 100) = 2) then
            aLog.httpResponseCode := 500;
          aLog.ReplyBody := e.Message;
        end;
      end;
    finally
      xOperation.Free;
    end;
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
                                      , checkValueAgainst
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
            if (df = '') then raise Exception.Create('df not found: "logDifferences.Header.differencesFound"');
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
        if doValidateInboundRequests(xOperation) then
        begin
          if not xOperation.reqBind.IsValueValid (xMessage) then
            xLog.RequestValidateResult := xMessage;
          xLog.RequestValidated := True;
        end;
        if doValidateOutboundReplies(xOperation) then
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

procedure TWsdlProject.RemoveStdHttpHeaders (aHeaderList: TIdHeaderList);

var
  x: Integer;
  function _isStdHeaderName (aName: String): Boolean;
  var
    x: Integer;
    stdHeaderStrings : array[0..12] of string =
    ( 'Connection'
    , 'Content-Version'
    , 'Content-Disposition'
    , 'Content-Encoding'
    , 'Content-Language'
    , 'Content-Type'
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
                                  , checkValueAgainst
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

function TWsdlProject.InformationAsXml: TXml;
  procedure _jsonise (aBind: TCustomBindable);
  var
    x: Integer;
  begin
    if (aBind.Name = 'Scripts')
    or (aBind.Name = 'operations') then
      aBind.jsonType := jsonArray;
    for x := 0 to aBind.Children.Count - 1 do
    begin
      if aBind.jsonType = jsonArray then
        aBind.Children.Bindables[x].Name := '_';
      _jsonise(aBind.Children.Bindables[x]);
    end;
  end;

var
  x: Integer;
begin
  result := TXml.CreateAsString('projectInformation', '');
  with result do
  begin
    AddXml (TXml.CreateAsString('project', projectFileName));
    AddXml (TXml.CreateAsString('version', versionInfoAsString));
    with AddXml (TXml.CreateAsString('operations', '')) do
    begin
      jsonType := jsonArray;
      for x := 0 to allAliasses.Count - 1 do
      begin
        with AddXml (allAliasses.Operations[x].InformationAsXml) do
        begin
          Name := '_';
        end;
      end;
    end;
    if Scripts.Items.Count > 0 then
    with AddXml (TXml.CreateAsString('scripts', '')) do
    begin
      CopyDownLine(Scripts, True);
      _jsonise(thisXml);
    end;
  end;
end;

function TWsdlProject.EnvVarsAsXml: TXml;
var
  x: Integer;
begin
  result := TXml.CreateAsString('envVars', '');
  EnvVarLock.Acquire;
  try
    with result do
    begin
      jsonType := jsonArray;
      EnvVars.Sort;
      for x := 0 to EnvVars.Count - 1 do
      begin
        with AddXml (TXml.CreateAsString('_', '')) do
        begin
          AddXml (TXml.CreateAsString('name', EnvVars.Names[x]));
          AddXml (TXml.CreateAsString('value', EnvVars.ValueFromIndex[x]));
        end;
      end;
    end;
  finally
    EnvVarLock.Release;
  end;
end;

function TWsdlProject.remoteServerConnectionAsXml: TXml;
begin
  result := TXml.Create;
  result.CopyDownLine(remoteServerConnectionXml, True);
end;

procedure TWsdlProject.remoteServerConnectionFromXml(aXml: TXml);
begin
  with remoteServerConnectionXml do
  begin
    CopyDownLine(aXml, True);
    remoteServerConnectionEnabled := Items.XmlBooleanByTagDef['Enabled', False];
  end;
  xmlio.apiUiConnectionConfig := remoteServerConnectionXml;
end;

procedure TWsdlProject.EnvVarsFromXml(aXml: TXml);
var
  x: Integer;
begin
  if not Assigned (aXml)
  or (aXml.Name <> 'envVars') then
    raise Exception.Create('TWsdlProject.EnvVarsFromXml(aXml: TXml): Illegal argument');
  EnvVarLock.Acquire;
  try
    EnvVars.Clear;
    for x := 0 to aXml.Items.Count - 1 do
      with aXml.Items.XmlItems[x].Items do
        EnvVars.Values[XmlCheckedValueByTag['name']] := XmlCheckedValueByTag['value'];
  finally
    EnvVarLock.Release;
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
      xXml.LoadFromFile(aString, nil, nil)
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
          xLog.MessageId := Items.XmlValueByTagDef ['MessageId', xLog.MessageId];
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
          xLog.PathFormat := Items.XmlValueByTag ['PathFormat'];
          xLog.Exception := Items.XmlValueByTag ['Error'];
          xLog.Remarks := Items.XmlValueByTag ['Remarks'];
          xLog.Notifications := Items.XmlValueByTag ['Notifications'];
          xLog.httpUri := Items.XmlValueByTag ['httpUri'];
          xLog.httpResponseCode := Items.XmlIntegerByTag ['httpResponseCode'];
          xLog.httpCommand := Items.XmlValueByTag ['httpCommand'];
          xLog.httpDocument := Items.XmlValueByTag ['httpDocument'];
          xLog.apiDocument := Items.XmlValueByTag ['apiDocument'];
          xLog.httpParams := Items.XmlValueByTag ['httpParams'];
          xLog.RequestContentType := Items.XmlValueByTag ['RequestContentType'];
          xLog.ReplyContentType := Items.XmlValueByTag ['ReplyContentType'];
          xLog.RequestHeaders := Items.XmlValueByTag ['HttpRequestHeaders'];
          xLog.ReplyHeaders := Items.XmlValueByTag ['HttpReplyHeaders'];
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
          xLog.MessageId := Items.XmlValueByTag ['MessageId'];
          if xLog.ServiceName = '' then
            xLog.ServiceName := Items.XmlValueByTag ['ServiceName'];
          if xLog.OperationName = '' then
            xLog.OperationName := Items.XmlValueByTag ['OperationName'];
          xLog.MessageId := Items.XmlValueByTag ['MessageId'];
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
              xLog.OperationName:=xLog.Operation.Alias;
          except
          end;
          if Assigned (xLog.Operation) then
          begin
            xLog.Mssg := xLog.Operation.MessageBasedOnRequest;
            xLog.toBindables(xLog.Operation);
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
  with TJBStringList.Create do
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
    Wsdl := TWsdl.Create(EnvVars, True);
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

function TWsdlProject.FindSnapshot(aName: String): TSnapshot;
var
  f: Integer;
begin
  result := nil;
  AcquireLogLock;
  try
    f := displayedSnapshots.IndexOf(aName);
    if (f > -1) then
      result := displayedSnapshots.SnapshotItems[f]
    else
    begin
      f := toDisplaySnapshots.IndexOf(aName);
      if (f > -1) then
        result := toDisplaySnapshots.SnapshotItems[f]
    end;
  finally
    ReleaseLogLock;
  end;
end;

function TWsdlProject.UpsertSnapshot(aName, aFileName, aRefFileName: String; aDoClearLoggedOnes: Boolean): TSnapshot;
var
  x: Integer;
begin
  result := FindSnapshot(aName);
  if not Assigned (result) then
    result := CreateSnapshot(aName, aFileName, aRefFileName, False, False);
  result.timeStamp := Now;
  result.FileName := ExpandRelativeFileName(projectFileName, aFileName);
  result.RefFileName := ExpandRelativeFileName(projectFileName, aRefFileName);
  AcquireLogLock;
  try
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
      if aDoClearLoggedOnes then
      begin
        displayedLogs.Clear;
        toDisplayLogs.Clear;
      end;
    finally
      Free;
    end;
  finally
    ReleaseLogLock;
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
    toDisplaySnapshots.AddObject(aName, result);
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

procedure TWsdlProject.CreateJUnitReport (aName: String);
var
  xList: TSnapshotList;
  x: Integer;
begin
  if (CurrentFolder = '') then
    raise Exception.Create('CreateJUnitReport: config (ProjectOptions.General.projectFolders) invalid');
  xList := TSnapshotList.Create;
  try
    AcquireLogLock;
    try
      for x := 0 to displayedSnapshots.Count - 1 do
        xList.AddObject('', displayedSnapshots.SnapshotItems[x]);
      for x := 0 to toDisplaySnapshots.Count - 1 do
        xList.AddObject('', toDisplaySnapshots.SnapshotItems[x]);
    finally
      ReleaseLogLock;
    end;
    SaveStringToFile ( ReportsFolder + DirectorySeparator + aName + '.xml'
                     , JUnitSummary(Self, xList)
                     );
    xList.Clear;
  finally
    xList.Free;
  end;
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
      for x := 0 to toDisplaySnapshots.Count - 1 do
        xList.AddObject('', toDisplaySnapshots.SnapshotItems[x]);
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
    SendMessage (aOperation, aOperation.Messages.Messages[x], '');
end;

procedure TWsdlProject.Clean;
begin
  AcquireLock;
  try
    allOperations.Clean;
  finally
    ReleaseLock;
  end;
end;

procedure TWsdlProject.SaveWithFolders;
  procedure _createReadMe (aFolderName: String);
  begin
    xmlio.SaveStringToFile ( LazFileUtils.AppendPathDelim(aFolderName) + ReadmeFilename
                           , 'This is a service virtualisation project folder;'
                           + LineEnding
                           + 'use "' + _ProgName + '" to examine it.'
                           + LineEnding
                           + 'Unless you know how to avoid corrupting this project,'
                           + LineEnding
                           + 'do not add, delete, or modify folders or files here.'
                           );
  end;
  procedure _createFolder (aFolderName: String);
  begin
    if not LazFileUtils.CreateDirUTF8(aFolderName) then
      raise Exception.CreateFmt('Could not create folder "%s"', [aFolderName]);
  end;
  procedure _saveChildElementToFile (aXmlList: TXmlList; aTag, aFolderName: String);
  var
    xXml: TXml;
  begin
    xXml := aXmlList.XmlItemByTag[aTag];
    if Assigned(xXml) then
    begin
      if xXml.Value <> '' then
        xmlio.SaveStringToFile(LazFileUtils.AppendPathDelim(aFolderName) + aTag + '.txt', xXml.Value)
      else
        if xXml.Items.Count > 0 then
          SaveStringToFile ( LazFileUtils.AppendPathDelim(aFolderName) + aTag + '.xml'
                           , xXml.AsText(False,2,True,False)
                           );
      xXml.Checked := False;
    end;
  end;
  procedure _uncheckFileAlias (aXml: TXml);
  var
    xXml: TXml;
  begin
    xXml := aXml.ItemByTag['FileAlias'];
    if Assigned (xXml) then
      xXml.Checked := False;
  end;

var
      xWsdlsFolderName, xWsdlFolderName
    , xScriptsFolderName, xScriptFolderName
    , xServicesFolderName, xServiceFolderName
    , xOperationsFolderName, xOperationFolderName
    , xMessagesFolderName, xMessageFolderName
    , xFileName, xProjectFolderName: String;
  xMPrefix, xMName, xAlias: String;
  xXml: TXml;
  w, s, o, m: Integer;
begin
  ProgressBegin('Saving project ' + projectFileName, 100 + NumberOfOperationsAndMessages);
  xsiGenerated := True; // en dan maar hopen dat er geen andere parallele threads bezig zijn...
{
  xProjectFolderName := Copy (projectFileName, 1, Length(projectFileName) - Length(_ProjectFileExtention))
                      + Copy (_ProjectFileExtention, 2, 100);
}
  ProgressStep('Initializing', 20);
  try
    try
      xProjectFolderName := projectFileName;
      if not LazFileUtils.ForceDirectory(xProjectFolderName) then
        raise Exception.CreateFmt('Could not create folder "%s"', [xProjectFolderName]);
      ProgressUpdate('Initializing Folder', 30);
      xmlio.EraseAllFolderContent(xProjectFolderName);
      _createReadMe(xProjectFolderName);
      xWsdlsFolderName := LazFileUtils.AppendPathDelim(xProjectFolderName) + 'W';
      _createFolder (xWsdlsFolderName);
      ProgressUpdate('Analysing', 40);
      with ProjectDesignAsXml do
      try
        for w := Items.Count - 1 downto 0 do
        begin
          if Items.XmlItems[w].Name = 'Wsdl' then
          begin
            xWsdlFolderName := LazFileUtils.AppendPathDelim(xWsdlsFolderName)
                             + Items.XmlItems[w].Items.XmlValueByTagDef[ 'FileAlias'
                                                                       , Items.XmlItems[w].Items.XmlValueByTag['Name']
                                                                       ];
            _createFolder (xWsdlFolderName);
            xServicesFolderName := LazFileUtils.AppendPathDelim(xWsdlFolderName) + 'S';
            _createFolder (xServicesFolderName);
            with Items.XmlItems[w] do
            for s := Items.Count - 1 downto 0 do
            begin
              if Items.XmlItems[s].Name = 'Service' then
              begin
                xServiceFolderName := LazFileUtils.AppendPathDelim(xServicesFolderName)
                                    + Items.XmlItems[s].Items.XmlValueByTagDef[ 'FileAlias'
                                                                              , Items.XmlItems[s].Items.XmlValueByTag['Name']
                                                                              ]
                                    ;
                _createFolder (xServiceFolderName);
                xOperationsFolderName := LazFileUtils.AppendPathDelim(xServiceFolderName) + 'O';
                _createFolder (xOperationsFolderName);
                with Items.XmlItems[s] do
                for o := Items.Count - 1 downto 0 do
                begin
                  if Items.XmlItems[o].Name = 'Operation' then
                  begin
                    xAlias := Items.XmlItems[o].Items.XmlValueByTag['FileAlias'];
                    xOperationFolderName := LazFileUtils.AppendPathDelim(xOperationsFolderName) + xAlias;
                    ProgressStep('Writing ' + xAlias, 1);
                    _createFolder (xOperationFolderName);
                    xMessagesFolderName := LazFileUtils.AppendPathDelim(xOperationFolderName) + 'M';
                    _createFolder (xMessagesFolderName);
                    with Items.XmlItems[o].ItemByTag['Messages'] do
                    begin
                      xMPrefix := '0';
                      for m := 0 to Items.Count - 1 do
                      with Items.XmlItems[m] do
                      begin
                        xMName := Items.XmlValueByTag ['Name'];
                        if xMName = '' then
                          xMName := Format ('_%4d', [m]);
                        ProgressStep('Writing ' + xAlias, 1);
                        xMessageFolderName := LazFileUtils.AppendPathDelim(xMessagesFolderName)
                                            + xMPrefix
                                            + xMName
                                            ;
                        if LazFileUtils.DirectoryExistsUTF8(xMessageFolderName) then
                        begin
                          // in case of duplicate message names
                          // last one will survice
                          // todo: create a dialogie with Abort, Skip or Overwrite...
                          xmlio.EraseAllFolderContent (xMessageFolderName);
                          LazFileUtils.RemoveDirUTF8(xMessageFolderName);
                        end;
                        _createFolder (xMessageFolderName);
                        _saveChildElementToFile(Items, 'BeforeScript', xMessageFolderName);
                        _saveChildElementToFile(Items, 'BeforeScript', xMessageFolderName);
                        _saveChildElementToFile(Items, 'replyCheckers', xMessageFolderName);
                        _saveChildElementToFile(Items, 'requestCheckers', xMessageFolderName);
                        _saveChildElementToFile(Items, 'Documentation', xMessageFolderName);
                        xFileName := LazFileUtils.AppendPathDelim(xMessageFolderName) + _MessageFileName;
                        SaveStringToFile(xFileName, AsText(False,2,True,False));
                        xMPrefix := '1';
                      end;
                      Checked := False;
                    end;
                    _saveChildElementToFile(Items.XmlItems[o].Items, 'BeforeScript', xOperationFolderName);
                    _saveChildElementToFile(Items.XmlItems[o].Items, 'AfterScript', xOperationFolderName);
                    _saveChildElementToFile(Items.XmlItems[o].Items, 'Documentation', xOperationFolderName);
                    xFileName := LazFileUtils.AppendPathDelim(xOperationFolderName) + _OperationFileName;
                    _uncheckFileAlias(Items.XmlItems[o]);
                    SaveStringToFile(xFileName, Items.XmlItems[o].AsText(False,2,True,False));
                    Items.XmlItems[o].Free;
                    Items.Delete(o);
                  end;
                end;
                xFileName := LazFileUtils.AppendPathDelim(xServiceFolderName) + _ServiceFileName;
                _uncheckFileAlias(Items.XmlItems[s]);
                SaveStringToFile(xFileName, Items.XmlItems[s].AsText(False,2,True,False));
                Items.XmlItems[s].Free;
                Items.Delete(s);
              end;
            end;
            xFileName := LazFileUtils.AppendPathDelim(xWsdlFolderName) + _WsdlFileName;
            _uncheckFileAlias(Items.XmlItems[w]);
            SaveStringToFile(xFileName, Items.XmlItems[w].AsText(False,2,True,False));
            Items.XmlItems[w].Free;
            Items.Delete(w);
          end;
        end;
        _saveChildElementToFile(Items, 'PathPrefixes', xProjectFolderName);
        _saveChildElementToFile(Items, 'Environments', xProjectFolderName);
        xXml := ItemByTag['JanBo'];
        if Assigned (xXml) then
        begin
          xFileName := LazFileUtils.AppendPathDelim(xProjectFolderName) + _ContextsFileName;
          SaveStringToFile(xFileName, xXml.AsText(False,2,True,False));
          xXml.Checked := False;
        end;
        _saveChildElementToFile(Items, 'contexts', xProjectFolderName);
        _saveChildElementToFile(Items, 'properties', xProjectFolderName);
        _saveChildElementToFile(Items, 'ignoreDifferencesOn', xProjectFolderName);
        _saveChildElementToFile(Items, 'checkValueAgainst', xProjectFolderName);
        _saveChildElementToFile(Items, 'ignoreAddingOn', xProjectFolderName);
        _saveChildElementToFile(Items, 'ignoreRemovingOn', xProjectFolderName);
        _saveChildElementToFile(Items, 'ignoreOrderOn', xProjectFolderName);
        _saveChildElementToFile(Items, 'regressionSortColumns', xProjectFolderName);
        _saveChildElementToFile(Items, 'ignoreCoverageOn', xProjectFolderName);
        xScriptsFolderName := LazFileUtils.AppendPathDelim(xProjectFolderName) + 'S';
        _createFolder (xScriptsFolderName);
        if Assigned (Items.XmlItemByTag['Scripts']) then with Items.XmlItemByTag['Scripts'] do
        begin
          for s := 0 to Items.Count - 1 do with Items.XmlItems[s] do
          begin
            if Name = 'Script' then
            begin
              xScriptFolderName := LazFileUtils.AppendPathDelim(xScriptsFolderName)
                                 + Format('%d_%s', [10000 + 100 * s, Items.XmlValueByTag['Name']])
                                 ;
              _createFolder (xScriptFolderName);
              _saveChildElementToFile(Items, 'Code', xScriptFolderName);
              xFileName := LazFileUtils.AppendPathDelim(xScriptFolderName) + _ScriptFileName;
              SaveStringToFile(xFileName, AsText(False,2,True,False));
            end;
          end;
          Checked := False;
        end;
        SaveStringToFile(LazFileUtils.AppendPathDelim(xProjectFolderName) + _ProjectFileName, AsText(False,2,True,False));
    {
        SaveStringToFile(projectFileName, 'projectdesign is in folder: ' + xProjectFolderName);
    }
        stubChanged := False;
        stubRead := True; // well,... but logically ...
    finally
        Free;
      end;
    except
      on e: exception do
      begin
        if Assigned (ProgressInterface) then
          ProgressException(e)
        else
          raise;
      end;
    end;
  finally
    ProgressEnd;
  end;
end;

procedure TWsdlProject.ExportToFile;
begin
  ProgressBegin('Saving ' + projectFileName, 1000);
  try
    try
      StubRead := False;
      SaveStringToFile(projectFileName, ProjectDesignAsString);
    except
      on e: exception do
      begin
        if Assigned (ProgressInterface) then
          ProgressException(e)
        else
          raise;
      end;
    end;
  finally
    ProgressEnd;
  end;
end;

procedure TWsdlProject.ImportFromFile;
begin
  ProgressBegin('Opening ' + projectFileName, 3000);
  try
    try
      ProjectDesignFromString(ReadStringFromFile(projectFileName, nil), projectFileName);
      StubRead := False;
      StubChanged := True;
      ProgressInvalidateConsole;
    except
      on e: exception do
      begin
        if Assigned (ProgressInterface) then
          ProgressException(e)
        else
          raise;
      end;
    end;
  finally
    ProgressEnd;
  end;
end;

procedure TWsdlProject.OpenFromServerUrl;
var
  xXml, dXml: TXml;
  sProjectDesign, sProjectFileName, sRemoteProjectFileName: String;
begin
  ProgressBegin('Opening ' + projectFileName, 3000);
  try
    sProjectFileName := projectFileName;
    try
      xXml := TXml.Create;
      try
        sProjectDesign := xmlio.apiUiServerDialog(remoteServerConnectionXml, '/apiUi/api/projectdesign', '', 'GET', 'application/xml');
        SjowMessage(sProjectDesign);
        xXml.LoadFromString(sProjectDesign, nil);
        if xXml.Name = '' then
          raise Exception.Create('Could not read Xml read from ' + RemoteServerUrl);
        dXml := xXml.FindXml('WsdlStubCase.FileName', '.');
        if not Assigned(dXml) then
          raise Exception.Create('Invalid Xml read from ' + RemoteServerUrl);
//      ProjectDesignFromXml(xXml, dXml.Value, remoteServerConnectionXml);
        sRemoteProjectFileName := dXml.Value;
        dXml.Value := sProjectFileName;
        ProjectDesignFromXml(xXml, dXml.Value);
        remoteProjectName := sRemoteProjectFileName;
        StubRead := False;
        StubChanged := True;
        ProgressInvalidateConsole;
      finally
        xXml.Free;
      end;
    except
      on e: exception do
      begin
        if Assigned (ProgressInterface) then
          ProgressException(e)
        else
          raise;
      end;
    end;
  finally
    ProgressEnd;
  end;
end;

procedure TWsdlProject.OpenProjectFromString (aString: String);
var
  xXml, dXml: TXml;
  sProjectDesign: String;
begin
  ProgressBegin('Opening ' + projectFileName, 3000);
  try
    try
      xXml := TXml.Create;
      try
        sProjectDesign := aString;
        xXml.LoadFromString(sProjectDesign, nil);
        if xXml.Name = '' then
          raise Exception.Create('Could not read Xml read from ' + RemoteServerUrl);
        dXml := xXml.FindXml('WsdlStubCase.FileName', '.');
        if not Assigned(dXml) then
          raise Exception.Create('Invalid Xml read from ' + RemoteServerUrl);
        ProjectDesignFromXml(xXml, dXml.Value);
        StubRead := False;
        StubChanged := True;
        ProgressInvalidateConsole;
      finally
        xXml.Free;
      end;
    except
      on e: exception do
      begin
        if Assigned (ProgressInterface) then
          ProgressException(e)
        else
          raise;
      end;
    end;
  finally
    ProgressEnd;
  end;
end;

procedure TWsdlProject.OpenFromFolders;
var
  xXml: TXml;
begin
  ProgressBegin('Opening ' + projectFileName, 3000);
  try
    try
      xXml := XmlFromProjectFolders(projectFileName);
      try
        ProjectDesignFromXml(xXml, projectFileName);
        ProgressInvalidateConsole;
        if doStartOnOpeningProject then
        try
          if hasOneTimeContextsColumn
          and Assigned (EditContexts) then
          begin
            SynchronizeMethode(EditContexts);
          end;
          Activate(True);
        except
          Activate(False);
          raise;
        end;
      finally
        xXml.Free;
      end;
    except
      on e: exception do
      begin
        if Assigned (ProgressInterface) then
          ProgressException(e)
        else
          raise;
      end;
    end;
  finally
    ProgressEnd;
  end;
end;

procedure TWsdlProject.IntrospectProject;
var
  saveChanged, saveRead: Boolean;
  saveContexts: TStringListList;
begin
  ProgressBegin('Introspecting', 4000);
  try
    try
      saveChanged := stubChanged;
      saveRead := stubRead;
      saveContexts := TStringListList.Create(projectContexts);
      try
        ProjectDesignFromString(ProjectDesignAsString, projectFileName);
        projectContexts.CopyFrom(saveContexts);
      finally
        saveContexts.Free;
      end;
      stubChanged := saveChanged;
      stubRead := saveRead;
      ProgressInvalidateConsole;
    except
      on e: exception do
      begin
        if Assigned (ProgressInterface) then
          ProgressException(e)
        else
          raise;
      end;
    end;
  finally
    ProgressEnd;
  end;
end;

function TWsdlProject.XmlFromProjectFolders(aFolderName: String): TXml;
  procedure _AddChildsFromFolder (aXml: TXml; aFolderName: String);
  var
    xSearchRec: TSearchRec;
    xExtention: String;
    r: Word;
  begin
    r := LazFileUtils.FindFirstUTF8(LazFileUtils.AppendPathDelim(aFolderName) + '*.*', faAnyFile, xSearchRec);
    while r = 0 do
    begin
      if xSearchRec.Name <> ReadmeFilename then
      begin
        xExtention := RightStr(xSearchRec.Name, 4);
        if (xExtention = '.txt') then
          aXml.AddXml (TXml.CreateAsString ( LazFileUtils.ExtractFileNameOnly(xSearchRec.Name)
                                           , xmlio.ReadStringFromFile(LazFileUtils.AppendPathDelim(aFolderName) + xSearchRec.Name, nil)
                                           )
                      );
        if (xExtention = '.xml')
        and (Copy(xSearchRec.Name, 1, 1) <> '_') then
          aXml.AddXml (TXml.Create).LoadFromFile (LazFileUtils.AppendPathDelim(aFolderName) + xSearchRec.Name, nil, nil);
      end;
      r := LazFileUtils.FindNextUTF8(xSearchRec);
    end;
    LazFileUtils.FindCloseUTF8(xSearchRec);
  end;

  procedure _getFolders (aFolderName: String; aList: TJBStringList);
  var
    xSearchRec: TSearchRec;
    r: Word;
  begin
    aList.Clear;
    r := LazFileUtils.FindFirstUTF8(LazFileUtils.AppendPathDelim(aFolderName) + '*', faDirectory, xSearchRec);
    while r = 0 do
    begin
      if (xSearchRec.Name <> '.')
      and (xSearchRec.Name <> '..')
      and (xSearchRec.Name <> ReadmeFilename) then
        aList.Add (xSearchRec.Name);
      r := LazFileUtils.FindNextUTF8(xSearchRec);
    end;
    LazFileUtils.FindCloseUTF8(xSearchRec);
  end;
  procedure _setXmlFileAlias (aXml: TXml; aFileAlias: String);
  var
    xXml: TXml;
  begin
    xXml := aXml.Items.XmlItemByTag['FileAlias'];
    if Assigned (xXml) then
      xXml.Value:=aFileAlias
    else
      aXml.AddXml(TXml.CreateAsString('FileAlias', aFileAlias));
  end;

var
      xWsdlsFolderName, xWsdlFolderName
    , xScriptsFolderName, xScriptFolderName
    , xServicesFolderName, xServiceFolderName
    , xOperationsFolderName, xOperationFolderName
    , xMessagesFolderName, xMessageFolderName
    , xFileName: String;
  w, s, o, m, step: Integer;
  wXml, sXml, oXml, mmXml, mXml: TXml;
  xWList, xSList, xOlist, xMList, xFileList: TJBStringList;
begin
  result := TXml.Create;
  ProgressStep('Reading filesystem...', 100);
  xFileName := LazFileUtils.AppendPathDelim(aFoldername) + _ProjectFileName;
  xWList := TJBStringList.Create;
  xWList.Sorted := True;
  xSList := TJBStringList.Create;
  xSList.Sorted := True;
  xOList := TJBStringList.Create;
  xOList.Sorted := True;
  xMList := TJBStringList.Create;
  xMList.Sorted := True;
  xFileList := TJBStringList.Create;
  try
    result.LoadFromFile(xFileName, nil, nil);
    xWsdlsFolderName := LazFileUtils.AppendPathDelim(aFoldername) + 'W';
    _getFolders (xWsdlsFolderName, xWList);
    if xWList.Count > 0 then
      step := 800 div xWlist.Count
    else
      step := 800;
    for w := 0 to xWList.Count - 1 do
    begin
      xWsdlFolderName := LazFileUtils.AppendPathDelim(xWsdlsFolderName) + xWList.Strings[w];
      ProgressStep(xWsdlFolderName, step);
      xFileName := LazFileUtils.AppendPathDelim(xWsdlFolderName) + _WsdlFileName;
      wXml := result.AddXml(TXml.Create);
      wXml.LoadFromFile(xFileName, nil, nil);
      _setXmlFileAlias(wXml, xWList.Strings[w]);
      xServicesFolderName := LazFileUtils.AppendPathDelim(xWsdlFolderName) + 'S';
      _getFolders (xServicesFolderName, xSList);
      for s := 0 to xSList.Count - 1 do
      begin
        xServiceFolderName := LazFileUtils.AppendPathDelim(xServicesFolderName) + xSList.Strings[s];
        xFileName := LazFileUtils.AppendPathDelim(xServiceFolderName) + _ServiceFileName;
        sXml := wXml.AddXml (TXml.Create);
        sXml.LoadFromFile(xFileName, nil, nil);
        _setXmlFileAlias(sXml, xSList.Strings[s]);
        _AddChildsFromFolder (sXml, xServiceFolderName);
        xOperationsFolderName := LazFileUtils.AppendPathDelim(xServiceFolderName) + 'O';
        _getFolders (xOperationsFolderName, xOList);
        for o := 0 to xOList.Count - 1 do
        begin
          xOperationFolderName := LazFileUtils.AppendPathDelim(xOperationsFolderName) + xOList.Strings[o];
          xFileName := LazFileUtils.AppendPathDelim(xOperationFolderName) + _OperationFileName;
          oXml := sXml.AddXml (TXml.Create);
          oXml.LoadFromFile(xFileName, nil, nil);
          _setXmlFileAlias(oXml, xOList.Strings[o]);
          _AddChildsFromFolder (oXml, xOperationFolderName);
          xMessagesFolderName := LazFileUtils.AppendPathDelim(xOperationFolderName) + 'M';
          _getFolders (xMessagesFolderName, xMList);
          mmXml := oXml.AddXml (TXml.CreateAsString('Messages', ''));
          for m := 0 to xMList.Count - 1 do
          begin
            xMessageFolderName := LazFileUtils.AppendPathDelim(xMessagesFolderName) + xMList.Strings[m];
            xFileName := LazFileUtils.AppendPathDelim(xMessageFolderName) + _MessageFileName;
            mXml := mmXml.AddXml (TXml.Create);
            mXml.LoadFromFile(xFileName, nil, nil);
            _AddChildsFromFolder (mXml, xMessageFolderName);
          end;
        end;
      end;
    end;
    ProgressStep('Scripts...', 100);
    mmXml := result.AddXml (TXml.CreateAsString('Scripts', ''));
    xScriptsFolderName := LazFileUtils.AppendPathDelim(aFoldername) + 'S';
    _getFolders (xScriptsFolderName, xMList);
    for m := 0 to xMList.Count - 1 do
    begin
      xScriptFolderName := LazFileUtils.AppendPathDelim(xScriptsFolderName) + xMList.Strings[m];
      xFileName := LazFileUtils.AppendPathDelim(xScriptFolderName) + _ScriptFileName;
      mXml := mmXml.AddXml (TXml.Create);
      mXml.LoadFromFile(xFileName, nil, nil);
      _AddChildsFromFolder (mXml, xScriptFolderName);
    end;
    _AddChildsFromFolder(result, aFoldername);
  finally
    FreeAndNil(xWList);
    FreeAndNil(xSList);
    FreeAndNil(xOList);
    FreeAndNil(xMList);
    FreeAndNil(xFileList);
  end;
end;

procedure TWsdlProject.Clear;
var
  x: Integer;
begin
  remoteProjectName := '';
  majorVersion := 0;
  minorVersion := 1;
  revision := 0;
  build := 0;
  projectContexts.RowCount := 1;
  projectContexts.ColCount := 1;
  DatabaseConnectionSpecificationXml.Items.Clear;
  DatabaseConnectionSpecificationXml.Reset;
  DbsType := '';
  Scripts.Items.Clear;
  displayedLogs.Clear;
  archiveLogs.Clear;
  displayedSnapshots.Clear;
  referencedFilenames.Clear;
  Listeners.Clear;
  doValidateRequests := True;
  doValidateReplies := True;
  inboundRequestSchemaValidationType := svReportOnly;
  outboundReplySchemaValidationType := svReportOnly;
  outboundRequestSchemaValidationType := svReportOnly;
  inboundReplySchemaValidationType := svReportOnly;
  schemaValidationVioloationHttpResponseCode := 417;
  ignoreDifferencesOn.Clear;
  checkValueAgainst.Clear;
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
  EnvironmentListClear;
  CORS := corsAcceptWithoutLogging;
  corsExceptionHttpCode := 500;
  LastFocusedOperation := nil;
  while Wsdls.Count > 0 do
  begin
    if (Wsdls.Objects[0] <> FreeFormatWsdl)
    and (Wsdls.Objects[0] <> CobolWsdl)
    and (Wsdls.Objects[0] <> ApiByExampleWsdl)
    then
      try Wsdls.Objects[0].Free; except end; // there is a project that fails at this point, not a clue yet why
    Wsdls.Delete(0);
  end;
  wsdls.Clear;
  wsdlNames.Clear;
  allOperations.ClearListOnly;
  allOperationsRpy.ClearListOnly;
  allAliasses.ClearListOnly;
  PathInfos.Clear;
  PathRegexps.Clear;
  PathFormats.Clear;
  ScriptsClear;
  DisplayedLogColumns.Clear;
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
  FreeFormatWsdl := TWsdl.Create(EnvVars, False);
  with FreeFormatWsdl do
  begin
    Name := '_Freeformat';
    isSoapService := False;
    Services.AddObject(FreeFormatService.Name, FreeFormatService);
  end;
  FreeAndNil(CobolWsdl);
  CobolWsdl := TWsdl.Create(EnvVars, False);
  with CobolWsdl do
  begin
    Name := '_Cobol';
    isSoapService := False;
    Services.Add(Name);
    Services.Objects[0] := TWsdlService.Create;
    Services.Services[0].Name := Name;
    Services.Services[0].DescriptionType := ipmDTCobol;
  end;
  FreeAndNil(ApiByExampleWsdl);
  ApiByExampleWsdl := TWsdl.Create(EnvVars, False);
  with ApiByExampleWsdl do
  begin
    Name := '_ApiByExample';
    isSoapService := False;
    isOpenApiService := True;
  end;
end;

function TWsdlProject.NumberOfOperationsAndMessages: Integer;
var
  x: Integer;
begin
  result := allOperations.Count;
  for x := 0 to allOperations.Count - 1 do
    result := result + allOperations.Operations[x].Messages.Count;
end;

procedure TWsdlProject.ProgressBegin(aCaption: String; aMax: Integer);
begin
  if Assigned (ProgressInterface) then
  begin
    AcquireLogLock;
    try
      ProgressInterface.Caption := aCaption;
      ProgressInterface.ProgressMax := aMax;
      ProgressInterface.ProgressPos := 0;
      ProgressInterface.doUpdateConsole := False;
      ProgressInterface.ExceptionRaised := False;
      ProgressInterface.doShowProgress := True;
    finally
      ReleaseLogLock;
    end;
  end;
end;

procedure TWsdlProject.ProgressUpdate(aAction: String; aPos: Integer);
begin
  if Assigned (ProgressInterface) then
  begin
    AcquireLogLock;
    try
      ProgressInterface.CurrentAction := aAction;
      ProgressInterface.ProgressPos := aPos;
    finally
      ReleaseLogLock;
    end;
  end;
end;

procedure TWsdlProject.ProgressStep(aAction: String; aInc: Integer);
begin
  if Assigned (ProgressInterface) then
  begin
    AcquireLogLock;
    try
      ProgressInterface.CurrentAction := aAction;
      ProgressInterface.ProgressPos := ProgressInterface.ProgressPos + aInc;
    finally
      ReleaseLogLock;
    end;
  end;
end;

procedure TWsdlProject.ProgressInvalidateConsole;
begin
  if Assigned (ProgressInterface) then
  begin
    AcquireLogLock;
    try
      ProgressInterface.doUpdateConsole := True;
    finally
      ReleaseLogLock;
    end;
  end;
end;

procedure TWsdlProject.ProgressException(E: Exception);
begin
  if Assigned (ProgressInterface) then
  begin
    ProgressInterface.ExceptionRaised := True;
    ProgressInterface.ExceptionMessage := E.Message;
    ProgressInterface.ExceptionStackTrace := ExceptionStackListString(E);
  end;
end;

procedure TWsdlProject.ProgressEnd;
begin
  if Assigned (ProgressInterface) then
  begin
    AcquireLogLock;
    try
      ProgressInterface.doShowProgress := False;
      fIsBusy := False;
    finally
      ReleaseLogLock;
    end;
  end;
end;

procedure TWsdlProject.DatabaseConnectionSpecificationFromXml ;
var
  hXml: TXml;
begin
  _WsdlDbsConnector.Connected := False;
  _WsdlDbsConnector.LoginPrompt := False;
  _WsdlDbsEnabled := False;
  _WsdlDbsConnector.OnLog := _WsdlSQLConnectorLog;
  DbsType := '';
  DbsDatabaseName := '';
  DbsHostName:='';
  DbsParams := '';
  DbsUserName:='';
  DbsPassword := '';
  hXml := TXml.Create;
  if DatabaseConnectionSpecificationXml.Checked = False then
    Exit;
  try
    hXml.CopyDownLine(DatabaseConnectionSpecificationXml, True);
    hXml.ResolveAliasses;
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
  if DbsType = '' then
    Exit;
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

procedure TWsdlProject.LogsFromRemoteServer;
var
  x: Integer;
  s: String;
  xLogList: TLogList;
  eLog: TLog;
  iTimeStamp: TDateTime;
begin
  try
    iTimeStamp := now;
    s := xmlio.apiUiServerDialog ( remoteServerConnectionXml
                               , '/apiUi/api/logs/getandremove'
                               , ''
                               , 'GET'
                               , 'application/xml'
                               );

    xLogList := TLogList.Create;
    try
      OpenMessagesLog(s, False, False, xLogList);
      for x := 0 to xLogList.Count - 1 do
      begin
        DisplayLog('', xLogList.LogItems[x]);
      end;
    finally
      s:= '';
      xLogList.Clear;
      FreeAndNil(xLogList);
    end;
  except
    on e: exception do
    begin
      eLog := TLog.Create;
      eLog.InboundTimeStamp := iTimeStamp;
      eLog.OutBoundTimeStamp := Now;
      eLog.StubAction := saException;
      eLog.Exception := 'exception retrieving logs: ' + e.Message;
      DisplayLog('', eLog);
    end;
  end;
end;

procedure TWsdlProject .WriteStringToStream (aString : String ;
  aStream : TStream );
begin
{
  aStream.Position := 0;
  aStream.Write(Pointer(aString)^, Length (aString));
  aStream.Position := 0;
}
  IdGlobal.WriteStringToStream(aStream, aString, IndyTextEncoding_OSDefault{$IFDEF STRING_IS_ANSI},nil{$ENDIF});

end;

function TWsdlProject.isSpecialWsdl(aWsdl: TWsdl): Boolean;
begin
  result := (aWsdl = FreeFormatWsdl)
         or (aWsdl = ApiByExampleWsdl)
         or (aWsdl = CobolWsdl)
         ;
end;

function TWsdlProject.doValidateOutboundRequests(aOperation: TWsdlOperation
  ): Boolean;
begin
  if aOperation.outboundRequestSchemaValidationType = svAccordingProject then
    result := (outboundRequestSchemaValidationType <> svNo)
  else
    result := (aOperation.outboundRequestSchemaValidationType <> svNo);
end;

function TWsdlProject.doValidateInboundReplies(aOperation: TWsdlOperation
  ): Boolean;
begin
  if aOperation.inboundReplySchemaValidationType = svAccordingProject then
    result := (inboundReplySchemaValidationType <> svNo)
  else
    result := (aOperation.inboundReplySchemaValidationType <> svNo);
end;

function TWsdlProject.doValidateInboundRequests(aOperation: TWsdlOperation
  ): Boolean;
begin
  if aOperation.inboundRequestSchemaValidationType = svAccordingProject then
    result := (inboundRequestSchemaValidationType <> svNo)
  else
    result := (aOperation.inboundRequestSchemaValidationType <> svNo);
end;

function TWsdlProject.doReturnExceptionOnViolatingInboundRequest(
  aOperation: TWsdlOperation): Boolean;
begin
  if aOperation.inboundRequestSchemaValidationType = svAccordingProject then
    result := (inboundRequestSchemaValidationType = svRaiseException)
  else
    result := (aOperation.inboundRequestSchemaValidationType = svRaiseException);
end;

function TWsdlProject.doValidateOutboundReplies(aOperation: TWsdlOperation
  ): Boolean;
begin
  if aOperation.outboundReplySchemaValidationType = svAccordingProject then
    result := (outboundReplySchemaValidationType <> svNo)
  else
    result := (aOperation.outboundReplySchemaValidationType <> svNo);
end;

procedure TWsdlProject.UpdateWsdlsList(aNewWsdlsList: TJBStringList);
var
  w, s, o: Integer;
  xOperation: TWsdlOperation;
begin
  for w := Wsdls.Count - 1 downto 0 do
  begin
    if not isSpecialWsdl(Wsdls.Objects[w] as TWsdl) then
    begin
      if aNewWsdlsList.IndexOfObject(Wsdls.Objects[w]) < 0 then
      begin
        Wsdls.Objects[w].Free;
        Wsdls.Delete(w);
      end;
    end;
  end;
  for w := 0 to aNewWsdlsList.Count - 1 do
  begin
    if Wsdls.IndexOfObject(aNewWsdlsList.Objects[w]) < 0 then
    begin
      Wsdls.AddObject(aNewWsdlsList.Strings[w], aNewWsdlsList.Objects[w]);
      with aNewWsdlsList.Objects[w] as TWsdl do
      begin
        for s := 0 to Services.Count - 1 do with Services.Services[s] do
        begin
          for o := 0 to Operations.Count - 1 do
          begin
            xOperation := Operations.Operations[o];
            TWsdlMessage.CreateReply(xOperation, 'Default', '.*', 'Default reply');
          end;
        end;
      end;
    end;
  end;
end;

// TWsdlProject
initialization
  _WsdlAddRemark := AddRemark;
  _WsdlExecuteScript := ExecuteScript;
  _WsdlExecuteScriptLater := ExecuteScriptLater;
  _WsdlRequestOperation := RequestOperation;
  _WsdlRequestOperationLater := RequestOperationLater;
  _WsdlNewDesignMessage := NewDesignMessage;
  _WsdlExecSql := ExecSql;
  _wsdlFetchDefaultDesignMessage := FetchDefaultDesignMessage;
  _WsdlRequestAsText := RequestAsText;
  _WsdlReplyAsText := ReplyAsText;
  _WsdlClearLogs := ClearLogs;
  _WsdlClearSnapshots := ClearSnapshots;
  _WsdlCreateSnapshot := CreateSnapshot;
  _WsdlCreateJUnitReport := CreateJUnitReport;
  _WsdlCreateSummaryReport := CreateSummaryReport;
  _WsdlCreateCoverageReport := CreateCoverageReport;
  _WsdlSendOperationRequest := SendOperationRequest;
  _WsdlSendOperationRequestLater := SendOperationRequestLater;
  try
    IntrospectIniXml;
  except
    on e: exception do
    begin
    {$ifndef NoGUI}
      ShowMessage (e.Message);
    {$else}
      writeln (e.message);
    {$endif}
      raise;
    end;
  end;

finalization
  FreeAndNil (apiUiXsdDescr);
  FreeAndNil (wsaXsdDescr);
  FreeAndNil (_WsdlRtiXml);
end.

