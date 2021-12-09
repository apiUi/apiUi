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
unit Wsdlz;

//    SjowMessage({$INCLUDE %FILE%} + ' ' + {$INCLUDE %LINE%} + LineEnding + reqXml.Text);

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses sqldb
   , Classes
   , ParserClasses
   , Xmlz
   , Xsdz
   , Express
   , Bind
   , Ipmz
   , LazFileUtils
   , xmlzConsts
   , IpmTypes
   , IdSSLOpenSSL
   , SyncObjs
   , xmlio
   ;

resourcestring
  S_MESSAGE_ACCEPTED = '[Message accepted by server]';
  S_NO_OPERATION_FOUND = 'No operation recognised';
  S_INBOUND_IS_A_RESPONSE = '[Inbound is a response]';
  S_ALIAS_VALID_PATTERN = '[A-Za-z]([0-9]|[A-Za-z]|\_|\-|\$)*'; // {id} regexp from express/scanner.l
  S_OPEN_API_PATHVALUE_REGEXP = '[^/]+';
  S_DOLLARREF = '$ref';

type TStubAction = (saStub, saForward, saRedirect, saRequest, saException);
type TTransportType = (ttHttp, ttHttps, ttMqDepricated, ttStomp, ttTacoDepricated, ttSmtpDepricated, ttBmtpDepricated, ttNone, ttKafka);
type TRecognitionType = (rtSoap, rtDocument, rtHeader, rtXml, rtSubString);
type TAuthenticationType = (atNone, atHTTPBasicAuthentication, atWsSecurity);
type TPasswordType = (pwText, pwDigest);
type TOnRequestViolating = (rvsDefault, rvsContinue, rvsRaiseErrorMessage, rvsAddRemark);
type TSchemaValidationType = (svAccordingProject, svNo, svReportOnly, svRaiseException);
type TConsumeProduceType = (ptJson, ptXml); // for API's
type TProcedure = procedure of Object;
const TransportTypeNames: array [ttHttp..ttKafka] of String =
( 'Http'
, 'Https'
, 'Mq'
, 'Stomp'
, 'Taco'
, 'Smtp'
, 'Bmtp'
, 'None'
, 'Kafka'
);
const ConsumeProduceTypeNames: array [ptJson..ptXml] of String =
( 'Json'
, 'Xml'
);

type
  TWsdl = class;
  TWsdlHeaders = class;
  TWsdlHeader = class;
  TWsdlServices = class;
  TWsdlService = class;
  TWsdlOperations = class;
  TWsdlBinder = class;
  TWsdlOperation = class;
  TWsdlMsgDescrs = class;
  TWsdlMsgDescr = class;
  TWsdlParts = class;
  TWsdlPart = class;
  TWsdlMessages = class;
  TWsdlMessage = class;
  TRecognition = class (TObject)
  public
    RecognitionType: TRecognitionType;
    Name, Value: String;
    Start, Length: Integer;
  end;

  { TWsdl }

  TWsdl = class(TObject)
    private
      fMssgs: TWsdlMsgDescrs;
      fOpers: TWsdlOperations;
      fStrs: TJBStringList;
      function getServiceByName(Index: String): TWsdlService;
      function getOperationByRequest(Index: String): TWsdlOperation;
    public
      Name, FileAlias, FileName: String;
      Description, Host, Schemes, Consumes, Produces: String;
      isSoapService: Boolean;
      isOpenApiService: Boolean;
      OpenApiVersion: String;
//      xTargetNamespacePrefix: String;
      Services: TWsdlServices;
      Servers: TJBStringList; // introduced with openapi 3.0
      ServerPathNames: TJBStringList;
      XsdDescr: TXsdDescr;
      sdfXsdDescrs: TXsdDescrList;
      IpmDescrs: TIpmDescrs;
      ExtraXsds: TJBStringList;
      SoapVersion: TSOAPVersion;
      OperationsWithEndpointOnly: Boolean;
      EnvVars: TJBStringList;
      _inXml: TXml;
      _outXml: TXml;
      _expectXml: TXml;
      property ServiceByName [Index: String]: TWsdlService read getServiceByName;
      property OperationByRequest [Index: String]: TWsdlOperation read getOperationByRequest;
      procedure ValidateEvaluatedTags (aXml: TXml; aSl: TJBStringList);
      function thisWsdl: TWsdl;
      function ExtraXsdsAsXml: TXml;
      procedure ExtraXsdsFromXml (aXml: TXml; SaveRelativeFileNames: Boolean; aMainFileName: String);
      procedure AddedTypeDefElementsFromXml (aXml: TXml);
      procedure LoadExtraXsds (aOnbeforeRead: TProcedureS);
      procedure LoadFromSchemaFile(aFileName: String; aOnError: TOnErrorEvent; aOnbeforeRead: TProcedureS);
      procedure LoadFromJsonYamlFile(aFileName: String; aOnError: TOnErrorEvent; aOnbeforeRead: TProcedureS);
      constructor Create(aEnvVars: TJBStringList; aOperationsWithEndpointOnly: Boolean);
      destructor Destroy; override;
  end;

  TWsdlHeaders = class (TJBStringList)
  private
    function GetHeader(Index: integer): TWsdlHeader;
  public
    property Headers [Index: integer]: TWsdlHeader read GetHeader;
    procedure Clear; override;
  end;

  TWsdlHeader = class(TObject)
    private
    public
      MessageName, PartName: String;
      Required: Boolean;
      Use: String;
      EncodingStyle: String;
      Part: TWsdlPart;
      constructor Create;
      destructor Destroy; override;
  end;

  TWsdlServices = class (TJBStringList)
  private
    function GetService(Index: integer): TWsdlService;
  public
    property Services [Index: integer]: TWsdlService read GetService;
    procedure Clear; override;
  end;

  { TWsdlService }

  TWsdlService = class(TObject)
    private
    function getOperationByName(Index: String): TWsdlOperation;
    public
      Name, FileAlias, openApiPath, logPathRegExp, logPathFormat: String;
      // at least one pathformat for the logging of requests
      AuthenticationType: TAuthenticationType;
      UserName: String;
      Password: String;
      PasswordType: TPasswordType;
      SuppressXmlComment: Boolean;
      SuppressHTTP500: Boolean;
      UseNameSpacePrefixes: Boolean;
      DescriptionType: TIpmDescrType;
      Operations: TWsdlOperations;
      PathInfos: TJBStringList;
      function thisService: TWsdlService;
      function StreamWsSecurity: String;
      property OperationByName [Index: String]: TWsdlOperation read getOperationByName;
      function OptionsAsXml: TXml;
      procedure OptionsFromXml(aXml: TXml);
      constructor Create;
      destructor Destroy; override;
  end;

  TWsdlMsgDescrs = class (TJBStringList)
  private
    function GetMessage(Index: integer): TWsdlMsgDescr;
    protected
    public
      property Messages [Index: integer]: TWsdlMsgDescr read GetMessage;
      procedure ClearListOnly;
      procedure Clear; override;
  end;

  { TWsdlOperations }

  TWsdlOperations = class (TJBStringList)
  private
    function GetOperation(Index: integer): TWsdlOperation;
    protected
    public
      function SaveFind (aString: String; aIndex: Integer): Boolean;
      function FindOnAliasName(aAlias: String): TWsdlOperation;
      function FindOnOperationName(aName: String): TWsdlOperation;
      property Operations [Index: integer]: TWsdlOperation read GetOperation;
      procedure ClearListOnly;
      procedure Clear; override;
      procedure Clean;
  end;

  { TWsdlBinder }
  TWsdlBinder = class (TObject)
  private
    fInputXsd: TXsd;
    fReqBind: TCustomBindable;
    fOutputXsd: TXsd;
    fRpyBind: TCustomBindable;
    fPreparedBefore: Boolean;
    fPreparedAfter: Boolean;
    procedure FoundErrorInBuffer(ErrorString: String; aObject: TObject);
    function getFreeFormatReq: String;
    function getFreeFormatRpy: String;
    function getRequestAsString : String ;
    function getRpyXml: TXml;
    function getReqXml: TXml;
    function getRpyBind: TCustomBindable;
    function getReqBind: TCustomBindable;
    procedure setReqBind(const Value: TCustomBindable);
    procedure setRequestAsString (AValue : String );
    procedure setRpyBind(const Value: TCustomBindable);
    procedure setFreeFormatReq(const aValue: String);
    procedure setFreeFormatRpy(const aValue: String);
    function getReqIpm: TIpmItem;
    function getInputXsd: TXsd;
    function getOutputXsd: TXsd;
    procedure setInputXsd(const Value: TXsd);
    procedure setOutputXsd(const Value: TXsd);
    function getRpyIpm: TIpmItem;
    function getDescriptionType: TIpmDescrType;
    function cloneBindables (aSrc: TBindableList): TBindableList;
  public
    Name: String;
    WsdlOperation: TWsdlOperation;
    FaultXsd: TXsd;
    fltBind: TCustomBindable;
    CorrelationBindables: TBindableList;
    BeforeScriptLines: TJBStringList;
    AfterScriptLines: TJBStringList;
    onFetchLogFromCloud: String;

    Duplicates, DuplicatesName: TWsdlBinder;
    _compareString: String;
    function FindBind (aCaption: String): TCustomBindable;
    procedure RebindLists; virtual;
    procedure PopulateCorrelation (aPatternsList: TJBStringList);
    property DescriptionType: TIpmDescrType read getDescriptionType;
    property reqXsd: TXsd read getInputXsd write setInputXsd;
    property reqBind: TCustomBindable read getReqBind write setReqBind;
    property rpyXsd: TXsd read getOutputXsd write setOutputXsd;
    property rpyBind: TCustomBindable read getRpyBind write setRpyBind;
    property FreeFormatReq: String read getFreeFormatReq write setFreeFormatReq;
    property FreeFormatRpy: String read getFreeFormatRpy write setFreeFormatRpy;
    property RequestAsString: String read getRequestAsString write setRequestAsString;
    property ReqIpm: TIpmItem read getReqIpm;
    property RpyIpm: TIpmItem read getRpyIpm;
    property reqXml: TXml read getReqXml;
    property rpyXml: TXml read getRpyXml;
    property PreparedBefore: Boolean read fPreparedBefore;
    property PreparedAfter: Boolean read fPreparedAfter;
  end;

  { TWsdlOperation }

  TWsdlOperation = class(TWsdlBinder)
    private
      fCloned: TWsdlOperation;
      fLock: TCriticalSection;
      fStamperStatement: String;
      fExpress: TExpress;
      fExpressStamper: TExpress;
      fExpressChecker: TExpress;
      fDoExit: Boolean;
      fLineNumber: Integer;
      fOnError: TOnErrorEvent;
      fLastFocusedMessage: TWsdlMessage;
      fLastFullCaption: String;
      fOnGetAbortPressed: TBooleanFunction;
      fPrepareErrors: String;
      procedure BufferToValuesErrorFound (aMessage: String; aObject: TObject);
      function getApiReplyMediaType: String;
      function getConsumesOnlyJson: Boolean;
      function getConsumesOnlyXml: Boolean;
      function getDoExit : Boolean ;
      function getHost: String;
      function getIsFreeFormat : Boolean ;
      function getIsOneWay: Boolean;
      function getIsOpenApiService: Boolean;
      function getisSoapService: Boolean;
      function getOpenApiVersion: String;
      procedure setDoExit (AValue : Boolean );
      function getInputXml: TXml;
      function getOutputXml: TXml;
      function getLastFullCaption: String;
      function getLastFocusedMessage: TWsdlMessage;
      function getReplyBasedOnRequest: TWsdlMessage;
      procedure NeedBeforeData(Sender: TObject; var MoreData: Boolean; var Data: String);
      procedure NeedAfterData(Sender: TObject; var MoreData: Boolean; var Data: String);
      procedure NeedStamperData(Sender: TObject; var MoreData: Boolean; var Data: String);
      function StreamWsAddressing (aWsa: TXml; isRequest: Boolean): String;
      function getWsaTo: String;
    procedure setOnGetAbortPressed(const Value: TBooleanFunction);
    function getDebugTokenStringBefore: String;
    public
      _processing: Boolean;
      WsdlService: TWsdlService;
      Wsdl: TWsdl;
      Owner: TObject;
      Data: TObject;
      FileAlias, Alias: String;
      HiddenFromUI: Boolean;
      isDepricated: Boolean;
      inboundRequestSchemaValidationType, outboundReplySchemaValidationType, outboundRequestSchemaValidationType, inboundReplySchemaValidationType: TSchemaValidationType;
      schemaValidationVioloationHttpResponseCode: Integer;
      reqMessageName, reqTagName, reqTagNameSpace, rpyMessageName, rpyTagName, rpyTagNameSpace: String;
      Schemes, Consumes, Produces, ContentType, OverruleContentType, Accept: String;
      ConsumeType, ProduceType: TConsumeProduceType;
      reqDescrFilename, rpyDescrFilename, fltDescrFileName: String;
      reqDescrExpansionFilename, rpyDescrExpansionFilename, fltDescrExpansionFileName: String;
      Documentation: TJBStringList;
      _InputMessageName: String;
      _OutputMessageName: String;
      FaultMessages: TWsdlMsgDescrs;
      InputHeaders: TWsdlHeaders;
      OutputHeaders: TWsdlHeaders;
      OperationCounter: Integer;
      OnRequestViolatingAddressPath: TOnRequestViolating;
      BindName: String;
      SoapTransport: String;
      SoapAction: String;
      SoapBindingStyle: String;
      SoapBodyInputUse: String;
      SoapBodyInputEncodingStype: String;
      SoapBodyInputPartName: String;
      SoapBodyInputRequired: Boolean;
      SoapBodyOutputUse: String;
      SoapBodyOutputEncodingStype: String;
      SoapBodyOutputPartName: String;
      SoapBodyOutputRequired: Boolean;
      SoapAddress: String;
      wsaEnabled: Boolean;
      wsaSpecificMustUnderstand: Boolean;
      wsaMustUnderstand: Boolean;
      wsaType: String;
      reqWsaXml, rpyWsaXml: TXml;
      StubAction: TStubAction;
      StubTransport: TTransportType;
      StubHttpAddress: String;
      HttpAddressIsComplete: Boolean; // only here for use from scripting, in case a callback address is provided
      httpVerb: String;
      ResponseNo: Integer;
      ContentEncoding: String;
      AcceptGzipEncoding, AcceptDeflateEncoding: Boolean;
      smtpHost: String;
      smtpPort: Integer;
      resolveRequestAliasses, resolveReplyAliasses: Boolean;
      useSsl: Boolean;
      sslVersion: TIdSSLVersion;
      sslCertificateFile, sslKeyFile, sslRootCertificateFile, sslPassword: String;
      StubStompHeaderXml: TXml;
      StubCustomHeaderXml: TXml;
      StubStompPutHost: String;
      StubStompPutPort: String;
      StubStompPutUseCredentials: Boolean;
      StubStompPutUserName: String;
      StubStompPutPassword: String;
      StubStompPutClientId: String;
      StubStompTimeOut: Integer;
      CorrelatedMessage: TWsdlMessage;
      Messages: TWsdlMessages;
      doReadReplyFromFile: Boolean;
      ReadReplyFromFileXml: TXml;
      LogColumns, BindablesWithAddedElement: TBindableList;
      faultcode, faultstring, faultactor, LiteralResult: String;
      ReturnSoapFault: Boolean;
      RecognitionType: TRecognitionType;
      reqRecognition: TJBStringList;
      rpyRecognition: TJBStringList;
      oldInvokeSpec: String;
      invokeList: TWsdlOperations;
      invokeRequestInfo, invokeReplyInfo: Boolean;
      requestInfoBind, replyInfoBind: TCustomBindable;
      doDebug: Boolean;
      doSuppressLog: Integer;
      DelayTimeMs: Integer;
      DelayTimeMsMin: Integer;
      DelayTimeMsMax: Integer;
      PostponementMs: Integer;
      FreeOnTerminateRequest: Boolean;
      CobolEnvironment: TCobolEnvironmentType;
      ZoomElementCaption: String;
      property Host: String read getHost;
      property DoExit: Boolean read getDoExit write setDoExit;
      property PrepareErrors: String read fPrepareErrors;
      property OnGetAbortPressed: TBooleanFunction write setOnGetAbortPressed;
      property wsaTo: String read getWsaTo;
      property isSoapService: Boolean read getIsSoapService;
      property OpenApiVersion: string read getOpenApiVersion;
      property isOpenApiService: Boolean read getIsOpenApiService;
      property isOneWay: Boolean read getIsOneWay;
      property isFreeFormat: Boolean read getIsFreeFormat;
      property apiReplyMediaType: String read getApiReplyMediaType;
      property InputXml: TXml read getInputXml;
      property OutputXml: TXml read getOutputXml;
      property LastFocusedMessage: TWsdlMessage read getLastFocusedMessage write fLastFocusedMessage;
      property LastFullCaption: String read getLastFullCaption write fLastFullCaption;
      property MessageBasedOnRequest: TWsdlMessage read getReplyBasedOnRequest;
      property OnError: TOnErrorEvent read fOnError write fOnError;
      property Cloned: TWsdlOperation read fCloned;
      property ConsumesJsonOnly: Boolean read getConsumesOnlyJson;
      property ConsumesXmlOnly: Boolean read getConsumesOnlyXml;
      property DebugTokenStringBefore: String read getDebugTokenStringBefore;
      function thisOperation: TWsdlOperation;
      function AddedTypeDefElementsAsXml: TObject;
      procedure OnGetSslPassword (var aPassword: String);
      procedure AddedTypeDefElementsFromXml(aXml: TObject);
      function BeforeBindsAsText: String;
      procedure RebindLists; override;
      procedure Bind (aRoot: String; aBind: TCustomBindable; aExpress: TExpress);
      procedure AcquireLock;
      procedure ReleaseLock;
      function ReadReplyFromFileName: String;
      function DefaultReadReplyFromFileName: String;
      procedure ReadReplyFromFile;
      function BeforeActivatorDebugString: String;
      procedure InitDelayTime;
      procedure RefreshBindables;
      procedure ReqBindablesFromString (aString: String);
      procedure ReqBindablesFromWsdlMessage (aMessage: TWsdlMessage);
      procedure ReqBindablesToWsdlMessage (aMessage: TWsdlMessage);
      procedure RpyBindablesFromString (aString: String);
      procedure RpyBindablesFromWsdlMessage (aMessage: TWsdlMessage);
      procedure RpyBindablesToWsdlMessage (aMessage: TWsdlMessage);
      procedure XmlRequestToBindables (aRequest: TXml; aAddUnknowns: Boolean);
      procedure XmlReplyToBindables (aReply: TXml; aAddUnknowns: Boolean);
      procedure RequestStringToBindables (aRequest: String);
      procedure ReplyStringToBindables (aReply: String);
      function CorrelationIdAsText (aSeparator: String): String;
      procedure BindCheckerFunction (Id: String; Adr: Pointer; Token: Integer; ArgumentsPrototype: String);
      procedure BindStamperFunction (Id: String; Adr: Pointer; Token: Integer; ArgumentsPrototype: String);
      procedure BindScriptFunction (Id: String; Adr: Pointer; Token: Integer; ArgumentsPrototype: String);
      procedure doPromptReply;
      procedure doPromptRequest;
      procedure BindStamper;
      procedure BindChecker (aBind: TCustomBindable);
      procedure PrepareScripting;
      procedure PrepareBefore;
      procedure doInvokeOperations;
      procedure PrepareAfter;
      procedure PrepareChecker (aBind: TCustomBindable);
      procedure PrepareReqStamper (aBind: TCustomBindable);
      procedure ExecuteReqStampers;
      procedure PrepareRpyStamper (aBind: TCustomBindable);
      procedure ExecuteRpyStampers;
      procedure CheckScript (aStringList: TJBStringList; aOnError: TOnErrorEvent);
      procedure Execute (aStringList: TJBStringList; aOnError: TOnErrorEvent);
      procedure ExecuteBefore;
      procedure ExecuteAfter;
      procedure reqWsaOnRequest;
      procedure rpyWsaOnRequest;
      procedure fltWsaOnRequest;
      procedure Clean;
      function FunctionPrototypes (aAfter: Boolean): TJBStringList;
      function CheckerFunctionPrototypes: TJBStringList;
      function StamperFunctionPrototypes: TJBStringList;
      function StreamRequest ( aGeneratedWith: String
                             ; aGenerateTypes: Boolean
                             ; aGenerateHeaderNameSpaces: Boolean
                             ; aGenerateBodyNameSpaces: Boolean
                             ): String;
      function PrepareReply ( aGeneratedWith: String
                           ; aGenerateTypes: Boolean
                           ): String;
      function StreamFault (aGeneratedWith: String; aGenerateTypes: Boolean): String;
      function endpointConfigAsXml: TXml;
      procedure endpointConfigFromXml (aXml: TXml);
      function OptionsAsXml: TXml;
      procedure OptionsFromXml(aXml: TXml);
      function InformationAsXml: TXml;
      constructor Create (aWsdl: TWsdl); Overload;
      constructor Create (aOperation: TWsdlOperation); Overload;
      constructor CreateFromScriptXml (aOwner: TObject; aOnGetAbortPressed: TBooleanFunction; aScript: TXml);
      destructor Destroy; override;
  end;

  TWsdlMsgDescr = class(TObject)
    private
    public
      Xsd: TXsd;
      Name, NameSpace: String;
      Parts: TWsdlParts;
      constructor Create (aWsdl: TWsdl);
      destructor Destroy; override;
  end;

  TWsdlParts = class (TJBStringList)
  private
    function GetPart(Index: integer): TWsdlPart;
    protected
    public
      property Parts [Index: integer]: TWsdlPart read GetPart;
      procedure Clear; override;
  end;

  TWsdlPart = class(TObject)
    private
      procedure LinkToXsd (aXsds: TXsdList; aWsdl: TWsdl);
    public
      Name: String;
      NameSpace: String;
      _TypeName: String;
      _ElementName: String;
      IsBuiltInXsd: Boolean;
      Xsd: TXsd;
      constructor Create;
      destructor Destroy; override;
  end;

  { TWsdlMessages }

  TWsdlMessages = class (TJBStringList)
  private
    function GetMessage(Index: integer): TWsdlMessage;
  public
    property Messages [Index: integer]: TWsdlMessage read GetMessage;
    procedure SetNameDuplicates;
    procedure ResetNameDuplicates;
    procedure SetDuplicates;
    procedure ResetDuplicates;
    procedure DeleteMessage (aMessage: TWsdlMessage); overload;
    procedure DeleteMessage (aIndex: Integer); overload;
    procedure Clear; override;
  end;

  { TWsdlMessage }

  TWsdlMessage = class(TWsdlBinder)
  private
    public
//      Patterns: TJBStringList;
      rpyBodyBind: TCustomBindable; //if Assigned, the body of the reply
      ColumnXmls: TBindableList;
      Documentation: String;
      DocumentationEdited: Boolean;
      function thisMessage: TWsdlMessage;
      procedure corBindsInit(aOperation: TWsdlOperation);
      procedure Clean;
      procedure CheckBefore;
      procedure CheckAfter;
      constructor Create; Overload;
      constructor Create (aOperation: TWsdlOperation); Overload;
      constructor CreateRequest (aOperation: TWsdlOperation; aName, aPatterns, aDocumentation: String); Overload;
      constructor CreateReply (aOperation: TWsdlOperation; aName, aPatterns, aDocumentation: String); Overload;
      destructor Destroy; override;
  end;

type TOnMessageChange = procedure (aMessage: TWsdlMessage) of Object;
type TOnOperationChange = procedure (aOperation: TWsdlOperation) of Object;
type TOnChange = procedure of Object;
type TOnStringEvent = procedure (const Msg: String) of Object;

procedure Notify(aString: AnsiString);
function DateTimeToJulianStr (aDateTime: TDateTime): String;
function DateTimeToTandemJulianStr (aDateTime: TDateTime): String;
function RoundedX (aSource, aNumber: Extended): Extended;
function RandomX (aLow, aHigh: Extended): Extended;
function FormatDateX (aDate: TDateTime; Mask: String): String;
function GenerateRandomId: String;
function dbLookUp (aTable, aValueColumn, aReferenceColumn, aReferenceValue: String): String;
function NonceAsString (aString: String): String;
function xsdDateTime(aDT: TDateTime): String;
function XmlToDateTime (aString: String): TDateTime;
function xsdNowAsDateTime: String;
function sblNowAsDateTime: String;
procedure OperationFetchMessage (aObject : TObject; aIndex: Integer);
function OperationMessageList (aObject : TObject ; aAlias: String): TParserStringList ;
function RegExprSafeStr (aString: String): String;
function RegExprMatchList (aObject: TObject; aString, aExpr: String): TParserStringList;
function xNewLine: String;
function xTab: String;
function xStringOfChar (aString: String; aNumber: Extended): String;
function StringMatchesRegExpr (aString, aExpr: String): String;
procedure assignAnyType (aDstGroup, aSrcGroup: TObject);
function wsdlRequestAsText (aObject: TObject; aOperation: String): String;
function wsdlReplyAsText (aObject: TObject; aOperation: String): String;
procedure wsdlNewDesignMessage (aObject: TObject; aOperation, aName: String);
procedure wsdlRequestOperation (aObject: TObject; aOperation: String);
procedure wsdlRequestOperationLater (aObject: TObject; aOperation: String; aLaterMs: Extended);
procedure wsdlSendOperationRequest (aOperation, aCorrelation: String);
procedure wsdlSendOperationRequestLater (aOperation, aCorrelation: String; aLater: Extended);
procedure EnableMessage (aOperation: TWsdlOperation);
procedure EnableAllMessages;
procedure DisableMessage (aOperation: TWsdlOperation);
function OccurrencesX (aObject: TObject): Extended;
function LengthX (arg: String): Extended;
function StrToFloatX (arg: String): Extended;
function StrFromClipboard: String;
procedure SleepX (aMS: Extended);
function SubStringX ( s: String; i, c: Extended): String;
function isAccordingSchema (aObject: TObject): Extended;
function isAssigned (aObject: TObject): Extended;
procedure ResetOperationCounters;
function SqlSelectResultRow (aOperation: TWsdlOperation; aQuery: String): TParserStringList;
function EnvVarMatchList (aOperation: TWsdlOperation; aExpr: String): TParserStringList;
procedure ResetEnvVars (aOperation: TWsdlOperation; aRegExp: String);
procedure ResetEnvVar (aOperation: TWsdlOperation; aName: String);
function setEnvNumber (aOperation: TWsdlOperation; aName: String; aValue: Extended): Extended;
function setEnvVar (aOperation: TWsdlOperation; aName, aValue: String): String;
procedure AddRemark (aObject: TObject; aString: String);
function GetContext: String;
function SetContext (aName: String): String;
procedure SaveLogs (aObject: TObject; aString: String);
procedure CreateSnapshot (aObject: TObject; aName: String);
procedure CreateJUnitReport (aObject: TObject; aName: String);
procedure CreateSummaryReport (aObject: TObject; aName: String);
procedure CreateCoverageReport (aObject: TObject; aDoRun: Boolean);
procedure LogsFromRemoteServer (aObject: TObject);
procedure ClearLogs (aObject: TObject);
procedure ClearSnapshots (aObject: TObject);
procedure ExecuteScript (aObject: TObject; aString: String);
procedure ExecuteScriptLater (aObject: TObject; aString: String; aLaterMs: Extended);
function decVarNumber (aOperation: TWsdlOperation; aName: String): Extended;
function getVarNumber (aOperation: TWsdlOperation; aName: String): Extended;
function incVarNumber (aOperation: TWsdlOperation; aName: String): Extended;
function getVarNumberDef (aOperation: TWsdlOperation; aName: String; aDefault: Extended): Extended;
function getVar (aOperation: TWsdlOperation; aName: String): String;
function getVarDef (aOperation: TWsdlOperation; aName, aDefault: String): String;
function getVarDefT (aOperation: TWsdlOperation; aName, aDefault, aSeparator: String; aIndex: Extended): String;
function xsdOperationCount(aOperation: TWsdlOperation): Extended;
function wsdlUserName: String;
function wsdlOperationName(aOper: TWsdlOperation): String;
function wsdlMessagingProtocol(aOper: TWsdlOperation): String;
function wsdlMessageName(aOper: TWsdlOperation): String;
function xsdTodayAsDate: String;
function sblTodayAsDate: String;
procedure PromptReply(aOperation: TWsdlOperation);
procedure PromptRequest(aOperation: TWsdlOperation);
procedure RaiseExit(aOperation: TWsdlOperation);
procedure ReturnString (aOperation: TWsdlOperation; aString: String);
procedure RaiseWsdlFault (aOperation: TWsdlOperation; faultcode, faultstring, faultactor: String);
procedure RaiseHttpFault (aOperation: TWsdlOperation; aResponseCode, aResponseText, aResponseContentType: String);
procedure RaiseSoapFault (aOperation: TWsdlOperation; faultcode, faultstring, faultactor, detail: String);

function wsdlConvertSdfFrom36 (aXml: TXml): Boolean;
procedure AcquireLock;
procedure ReleaseLock;
procedure AcquireEnvVarLock;
procedure ReleaseEnvVarLock;

var
  allOperations, allAliasses: TWsdlOperations;
  allOperationsRpy: TWsdlOperations;
  _wsdlStubStylesheet: String;
  _wsdlGetContext: SFunctionV;
  _wsdlSetContext: SFunctionS;
  _WsdlExecSql: VFunctionOS;
  _WsdlNewDesignMessage: VFunctionOSS;
  _wsdlFetchDefaultDesignMessage: VFunctionOS;
  _WsdlRequestOperation: VFunctionOS;
  _WsdlRequestOperationLater: VFunctionOSX;
  _WsdlRequestAsText, _WsdlReplyAsText: SFunctionOS;
  _WsdlExecuteScript: VFunctionOS;
  _WsdlExecuteScriptLater: VFunctionOSX;
  _WsdlSaveLogs: VFunctionOS;
  _WsdlCreateSnapshot: VFunctionOSB;
  _WsdlCreateJUnitReport: VFunctionOS;
  _WsdlCreateSummaryReport: VFunctionOS;
  _WsdlCreateCoverageReport: VFunctionOB;
  _WsdlClearLogs: VFunctionOV;
  _WsdlLogsFromRemoteServer: VFunctionOV;
  _WsdlClearSnapshots: VFunctionOV;
  _WsdlAddRemark: VFunctionOS;
  _WsdlSendOperationRequest: VFunctionSS;
  _WsdlSendOperationRequestLater: VFunctionSSI;
  _WsdlHostName: String;
  _WsdlPortNumber: String;
  _WsdlProgName: String;
  _WsdlRtiXsd: TXsd;
  _WsdlRtiXml: TXml;
  _WsdlWsaXsd: TXsd;
  _WsdlStompHeaderXsd: TXsd;
  _WsdlListOfFilesXsd: TXsd;
  _WsdlUserNameTokenNumber: Integer;
  _WsdlOnMessageChange: TOnMessageChange;
  _WsdlOnOperationChange: TOnOperationChange;
  _OnChange: TOnChange;
  _OnBeginUpdate, _OnEndUpdate: TProcedure;
  _ipmGun: Boolean;
  _WsdlDbsEnabled: Boolean;
  _WsdlDbsConnector: TSQLConnector;
  _WsdlSQLConnectorLog: TDBLogNotifyEvent;
  _WsdlDbsTransaction: TSQLTransaction;
  _WsdlDbsConnectorType: String;
  _WsdlDbsParams: String;
  UILock: TCriticalSection;
  EnvVarLock: TCriticalSection;
  doOperationLock, doUILock: Boolean;
  endpointConfigXsd, replyInfoXsd: TXsd;

const ESCAPEDCRLF = '_CRLF_';

implementation

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLType,
{$ENDIF}
  StrUtils
   , SysUtils
   , DateUtils
   , igGlobals
   , RegExpr
{$ifndef NoGUI}
   , xmlUtilz
   , Forms
   , Dialogs
   , Controls
{$endif}
   , Math
   , base64
   , Clipbrd
   , HashUtilz
   , IdURI
   , xmlxsdparser
   , Logz
   , LazUTF8
   ;

{ TWsdl }
function Latin1(aString: String): String;
type
  Latin1String = type AnsiString(28591); // codepage 28591 = ISO-8859-1
var
  s: Latin1String;
begin
  s := aString;
  result := s;
end;

function UpperCaseStr (aString: String): String;
begin
  result := UTF8UpperCase(aString);
end;

function LowerCaseStr (aString: String): String;
begin
  result := UTF8LowerCase(aString);
end;

function StrToNameCase (aString: String): String;
begin
  result := UTF8ProperCase(aString, [' ', ',', '.', '/', ';', ':', '''', '-', '&']);
end;

function isValidId (aId: String): Boolean;
begin
  result := RegExpr.ExecRegExpr('^' + S_ALIAS_VALID_PATTERN + '$', aId);
end;

function makeValidId (aId: String): String;
var
  x: Integer;
begin
  if aId = '' then
    result := 'id'
  else
    result := aId;
  while (Length (Result) > 1)
  and not isValidId(Result[1]) do
    result := Copy(Result, 2, Length(Result) - 1);
  case Result[1] of
    'A'..'Z', 'a'..'z': ;
  else
    Result[1] := 'a';
  end;
  for x := 1 to Length (Result) do
  begin
    case Result[x] of
      'A'..'Z', 'a'..'z', '0'..'9', '_', '-', '$': ;
    else
      Result[x] := '_';
    end;
  end;
  Result := ReplaceStrings(Result, '__', '_', False, False);
end;

procedure Notify(aString: AnsiString);
begin
  if Assigned (OnNotify) then
    OnNotify (aString);
end;

procedure AddRemark(aObject: TObject; aString: String);
begin
  if not Assigned (_WsdlAddRemark) then
    raise Exception.Create('No OnAddRemark event assigned: intention was to log remark: ' + aString);
  _WsdlAddRemark (aObject, aString);
end;

procedure LogsFromRemoteServer(aObject: TObject);
begin
  if not Assigned (_WsdlLogsFromRemoteServer) then
    raise Exception.Create('No LogsFromRemoteServer event assigned');
  _WsdlLogsFromRemoteServer (aObject);
end;

procedure ClearLogs (aObject: TObject);
begin
  if not Assigned (_WsdlClearLogs) then
    raise Exception.Create('No OnClearLogs event assigned');
  _WsdlClearLogs (aObject);
end;

procedure ClearSnapshots (aObject: TObject);
begin
  if not Assigned (_WsdlClearSnapshots) then
    raise Exception.Create('No OnClearSnapshots event assigned');
  _WsdlClearSnapshots (aObject);
end;

function SetContext(aName: String): String;
begin
  if not Assigned (_wsdlSetContext) then
    raise Exception.CreateFmt('No SetContext event assigned, intention was to set as (%s)', [aName]);
  result := _wsdlSetContext (aName);
end;

function GetContext: String;
begin
  if not Assigned (_wsdlGetContext) then
    raise Exception.Create('No GetContext event assigned');
  result := _wsdlGetContext;
end;

procedure SaveLogs (aObject : TObject ; aString : String );
begin
  if not Assigned (_WsdlSaveLogs) then
    raise Exception.Create('No OnSaveLogs event assigned: intention was to write to: ' + aString);
  _WsdlSaveLogs (aObject, aString);
end;

procedure CreateSnapshot (aObject : TObject ; aName: String);
begin
  if not Assigned (_WsdlCreateSnapshot) then
    raise Exception.CreateFmt('No OnCreateSnapshot event assigned, intention was to save as (%s)', [aName]);
  _WsdlCreateSnapshot (aObject, aName, false);
end;

procedure CreateSummaryReport (aObject : TObject ; aName: String);
begin
  if not Assigned (_WsdlCreateSummaryReport) then
    raise Exception.CreateFmt('No OnCreateSummaryReport event assigned, intention was to save as (%s)', [aName]);
  _WsdlCreateSummaryReport (aObject, aName);
end;

procedure CreateJUnitReport (aObject : TObject ; aName: String);
begin
  if not Assigned (_WsdlCreateJUnitReport) then
    raise Exception.CreateFmt('No OnCreateJUnitReport event assigned, intention was to save as (%s)', [aName]);
  _WsdlCreateJUnitReport (aObject, aName);
end;

procedure CreateCoverageReport (aObject: TObject; aDoRun: Boolean);
begin
  if not Assigned (_WsdlCreateCoverageReport) then
    raise Exception.Create('No OnCreateCoverageReport event assigned');
  _WsdlCreateCoverageReport (aObject, aDoRun);
end;

procedure ExecuteScript(aObject: TObject; aString: String);
begin
  if not Assigned (_WsdlExecuteScript) then
    raise Exception.Create('No OnExecuteScript event assigned: intention was to execute script: ' + aString);
  _WsdlExecuteScript (aObject, aString);
end;

procedure ExecuteScriptLater(aObject: TObject; aString: String;
  aLaterMs: Extended);
begin
  if not Assigned (_WsdlExecuteScriptLater) then
    raise Exception.Create('No OnExecuteScriptLater event assigned: intention was to execute script: ' + aString);
  _WsdlExecuteScriptLater (aObject, aString, aLaterMs);
end;

procedure AcquireLock;
begin
  if doUILock then UILock.Acquire;
end;

procedure ReleaseLock;
begin
  if doUILock then UILock.Release;
end;

procedure AcquireEnvVarLock;
begin
  if doOperationLock then EnvVarLock.Acquire;
end;

procedure ReleaseEnvVarLock;
begin
  if doOperationLock then EnvVarLock.Release;
end;

function DateTimeToJulianStr (aDateTime: TDateTime): String;
begin
  result := FloatToStr(DateTimeToJulianDate(aDateTime));
end;

function DateTimeToTandemJulianStr (aDateTime: TDateTime): String;
begin
  result := FloatToStrF(DateTimeToJulianDate(aDateTime)*24*60*60*1000000,ffGeneral,18,3);
end;

function xDateTimeToUnix (aDateTime: TDateTime): Extended;
begin
  result := DateTimeToUnix(aDateTime);
end;

function xUnixToDateTime (aUnixDateTime: Extended): TDateTime;
begin
  result := UnixToDateTime(Trunc(aUnixDateTime));
end;

function RoundedX (aSource, aNumber: Extended): Extended;
begin
  result := Round(aSource * power (10, aNumber)) / power (10, aNumber);
end;

function RandomX (aLow, aHigh: Extended): Extended;
begin
  result := (aHigh - aLow) * Random + aLow;
end;

function FormatDateX (aDate: TDateTime; Mask: String): String;
var
  aYear: String;
  aMonth: String;
  aDay: String;
  Dow: Integer;
  DateString: String;
  aPos: Integer;
begin
  DateString := FormatDateTime ('yyyymmdd', aDate);
  aYear := Copy (DateString, 1, 4);
  aMonth := Copy (DateString, 5, 2);
  aDay := Copy (DateString, 7, 2);
  DateString := Mask;

  {Overnemen van de dag}
  aPos := Pos ('03', Mask);
  if (aPos > 0) then
  begin
    Delete (DateString, aPos, 2);
    Insert (aDay, DateString, aPos);
  end
  else
  begin
    aPos := Pos ('3', Mask);
    if (aPos > 0) then
    begin
      if (aDay [1] <> '0') then
      begin
        Delete (DateString, aPos, 1); {verwijderen van de 3}
        Insert (aDay, DateString, aPos); {invoegen twee posities}
        Insert ('$', Mask, aPos); {nu is Mask weer even lang}
      end
      else
      begin
        DateString [aPos] := aDay [2];
      end;
    end;
  end;

  {Overnemen van de maand}
  aPos := Pos ('02', Mask);
  if (aPos > 0) then
  begin
    Delete (DateString, aPos, 2);
    Insert (aMonth, DateString, aPos);
  end
  else
  begin
    aPos := Pos ('2', Mask);
    if (aPos > 0) then
    begin
      if (aMonth [1] <> '0') then
      begin
        Delete (DateString, aPos, 1); {verwijderen van de 3}
        Insert (aMonth, DateString, aPos); {invoegen twee posities}
        Insert ('$', Mask, aPos); {nu is Mask weer even lang}
      end
      else
      begin
        DateString [aPos] := aMonth [2];
      end;
    end;
  end;

  {Overnemen van het jaar}
  aPos := Pos ('1901', Mask);
  if (aPos > 0) then
  begin
    Delete (DateString, aPos, 4);
    Insert (aYear, DateString, aPos);
  end
  else
  begin
    aPos := Pos ('01', Mask);
    if (aPos > 0) then
    begin
      DateString [aPos] := aYear [3];
      DateString [aPos + 1] := aYear [4];
    end;
  end;

  {Overnemen van de maand als tekst}
  aPos := Pos (DefaultFormatSettings.LongMonthNames [2], Mask); {februari}
  if (aPos > 0)then
  begin
    Delete (DateString, aPos, Length (DefaultFormatSettings.LongMonthNames [2]));
    Insert (DefaultFormatSettings.LongMonthNames [StrToInt (aMonth)], DateString, aPos);
  end
  else
  begin
    aPos := Pos (DefaultFormatSettings.ShortMonthNames [2], Mask); {feb}
    if (aPos > 0)then
    begin
      Delete (DateString, aPos, Length (DefaultFormatSettings.ShortMonthNames [2]));
      Insert (DefaultFormatSettings.ShortMonthNames [StrToInt (aMonth)], DateString, aPos);
      {Mask en DateString moeten even lang blijven}
      Delete (Mask, aPos, Length (DefaultFormatSettings.ShortMonthNames [2]));
      Insert (DefaultFormatSettings.ShortMonthNames [StrToInt (aMonth)], Mask, aPos);
    end;
  end;

  {Overnemen van de dag van week als tekst}
  aPos := Pos (DefaultFormatSettings.LongDayNames [1], Mask); {zondag}
  if (aPos > 0)then
  begin
    Dow := DayOfWeek (aDate);
    Delete (DateString, aPos, Length (DefaultFormatSettings.LongDayNames [1]));
    Insert (DefaultFormatSettings.LongDayNames [Dow], DateString, aPos);
    Delete (Mask, aPos, Length (DefaultFormatSettings.LongDayNames [1]));
    Insert (DefaultFormatSettings.LongDayNames [Dow], Mask, aPos);
  end
  else
  begin
    aPos := Pos (DefaultFormatSettings.ShortDayNames [1], Mask); {zon}
    if (aPos > 0)then
    begin
      Dow := DayOfWeek (aDate);
      Delete (DateString, aPos, Length (DefaultFormatSettings.ShortDayNames [1]));
      Insert (DefaultFormatSettings.ShortDayNames [Dow], DateString, aPos);
      Delete (Mask, aPos, Length (DefaultFormatSettings.ShortDayNames [1]));
      Insert (DefaultFormatSettings.ShortDayNames [Dow], Mask, aPos);
    end;
  end;

  Result := DateString;
end;

function GenerateRandomId: String;
begin
  result := Copy (SHA1 (FloatToStr (Random)), 1, 4)
          + '-'
          + Copy (SHA1 (FloatToStr (Random)), 1, 4)
          + '-'
          + Copy (SHA1 (FloatToStr (Random)), 1, 4)
          + '-'
          + Copy (SHA1 (FloatToStr (Random)), 1, 4)
          ;
end;

function dbLookUp (aTable, aValueColumn, aReferenceColumn, aReferenceValue: String): String;
begin
  result := '';
  if not _WsdlDbsConnector.Connected then
    raise Exception.Create('No database connected');
  with TSQLQuery.Create(nil) do
  try
    Database := _WsdlDbsConnector;
    Transaction := _WsdlDbsTransaction;
    UsePrimaryKeyAsKey := False;
    SQL.Clear;
    SQL.Add('Select ' + aValueColumn);
    SQL.Add('from ' + aTable);
    SQL.Add('where ' + aReferenceColumn + '= :ReferenceValue');
    Params.ParamValues ['ReferenceValue'] := aReferenceValue;
    Open;
    while not EOF do
    begin
      result := FieldByName (aValueColumn).AsString;
      Next;
    end;
    Close;
  finally
    Free;
  end;
end;

function NonceAsString (aString: String): String;
begin
  result := Copy (Sha1 (aString), 1, 16);
end;

function xsdDateTime(aDT: TDateTime): String;
var
  year, month, day, hour, min, sec, msec: Word;
begin
  DecodeDate(aDT,year,month,day);
  DecodeTime(aDT,hour,min,sec,msec);
  result := Format('%.*d-%.*d-%.*dT%.*d:%.*d:%.*d.%.*d'
                , [4, year, 2, month, 2, day, 2, hour, 2, min, 2, sec, 3, msec]);
end;

function XmlToDateTime (aString : String ): TDateTime ;
begin
  result := xsdParseDateTime(aString);
end;

function xsdNowAsDateTime: String;
var
  nu: TDateTime;
  year, month, day, hour, min, sec, msec: Word;
begin
  nu := Now;
  DecodeDate(nu,year,month,day);
  DecodeTime(nu,hour,min,sec,msec);
  result := Format('%.*d-%.*d-%.*dT%.*d:%.*d:%.*d.%.*d'
                , [4, year, 2, month, 2, day, 2, hour, 2, min, 2, sec, 3, msec]);
end;

function sblNowAsDateTime: String;
var
  nu: TDateTime;
  year, month, day, hour, min, sec, msec: Word;
begin
  nu := Now;
  DecodeDate(nu,year,month,day);
  DecodeTime(nu,hour,min,sec,msec);
//mm/dd/yyyy hh:mm:ss
  result := Format('%.*d/%.*d/%.*d %.*d:%.*d:%.*d'
                , [2, month, 2, day, 4, year, 2, hour, 2, min, 2, sec]);
end;

function StringMatchesRegExpr (aString, aExpr: String): String;
begin
  result := '';
  if (aString <> '')
  and (aExpr <> '') then
  with TRegExpr.Create do
  try
    Expression := '^(' + aExpr + ')$';  // bol and eol: must match entire string
    if (Exec(aString)) then
      result := aExpr;
  finally
    Free;
  end;
end;

function SqlSelectResultRow(aOperation: TWsdlOperation; aQuery: String
  ): TParserStringList;
var
  xRow: String;
  xSep: String;
  x: Integer;
begin
  _WsdlDbsConnector.Connected := True;
  result := TParserStringList.Create;
  with TSQLQuery.Create(nil) do
  try
    Database := _WsdlDbsConnector;
    Transaction := _WsdlDbsTransaction;
    SQL.Text := aQuery;
    Open;
    while not EOF do
    begin
      xRow := '';
      xSep := '';
      for x := 0 to FieldCount - 1 do
      begin
        xRow := xRow + xSep + Fields.Fields[x].Text;
        xSep := Chr (9);
      end;
      result.Add(xRow);
      Next;
    end;
    Close;
  finally
    Free;
  end;
end;

function EnvVarMatchList (aOperation: TWsdlOperation; aExpr: String): TParserStringList;
var
  i: Integer;
begin
  result := TParserStringList.Create;
  if (aExpr <> '') then
  begin
    AcquireEnvVarLock;
    try
      with TRegExpr.Create do
      try
        Expression := aExpr;
        for I := 0 to aOperation.Wsdl.EnvVars.Count - 1 do
          if (Exec(aOperation.Wsdl.EnvVars.Names[i])) then
            result.Add (aOperation.Wsdl.EnvVars.Names[i]);
      finally
        Free;
      end;
    finally
      ReleaseEnvVarLock;
    end;
  end;
  result.Sort;
end;

procedure OperationFetchMessage (aObject : TObject ; aIndex : Integer );
begin
  with aObject as TWsdlOperation do
  begin
    CorrelatedMessage := Messages.Messages[aIndex];
    ReqBindablesFromWsdlMessage(CorrelatedMessage);
    RpyBindablesFromWsdlMessage(CorrelatedMessage);
  end;
end;

function OperationMessageList (aObject : TObject ; aAlias: String): TParserStringList ;
var
  x: Integer;
  xOperation: TWsdlOperation;
begin
  result := TParserStringList.Create;
  xOperation := nil; //candidate context
  if aObject is TWsdlOperation then with aObject as TWsdlOperation do
  begin
    xOperation := invokeList.FindOnAliasName(aAlias);
    if Assigned (xOperation) then with xOperation do
    begin
      result.aObject := xOperation;
      result.aIndexProcedure := OperationFetchMessage;
      for x := 0 to Messages.Count - 1 do
        result.Add(Messages.Messages[x].Name);
    end;
  end;
end;

function RegExprSafeStr(aString: String): String;
const xRegExprChars = '\^$*+?.(){}[]|';
var
  x: Integer;
begin
  result := '';
  for x := 1 to Length (aString) do
  begin
    if Pos (aString [x], xRegExprChars) > 0 then
      result := result + '\';
    result := result + aString [x];
  end;
end;

function RegExprMatchList (aObject: TObject; aString, aExpr: String): TParserStringList;
var
  f: Boolean;
begin
  result := TParserStringList.Create;
  if (aString <> '')
  and (aExpr <> '') then
  with TRegExpr.Create do
  try
    Expression := aExpr;
    f := Exec (aString);
    while f do
    begin
      result.Add (Match[0]);
      f := ExecNext;
    end;
  finally
    Free;
  end;
end;

function xNewLine : String ;
begin
  result := LineEnding;
end;

function xTab : String ;
begin
  result := Chr (9);
end;

function xStringOfChar (aString : String ; aNumber : Extended ): String ;
var
  c: Char;
begin
  if Length (aString) = 0 then
    c:= ' '
  else
    c:= aString [1];
  result := StringOfChar(c, Trunc (aNumber));
end;

procedure assignAnyType (aDstGroup, aSrcGroup: TObject);
var
  xSaveName: String;
  dXml, sXml: TXml;
begin
  dXml := TXml ((aDstGroup as YYSType).yy.yyPointer);
  sXml := TXml ((aSrcGroup as YYSType).yy.yyPointer);
  xSaveName := dXml.Name;
  dXml.Name := sXml.Name;
  try
    dXml.ResetValues;
    dXml.LoadValues(sXml, True, True);
    dXml.Checked := sXml.Checked;
  finally
    dXml.Name := xSaveName;
  end;
end;

function wsdlRequestAsText (aObject: TObject; aOperation: String): String;
begin
  if not Assigned (_WsdlRequestAsText) then
    raise Exception.Create('wsdlRequestAsText: implementation missing');
  result := _WsdlRequestAsText (aObject, aOperation);
end;

function wsdlReplyAsText (aObject: TObject; aOperation: String): String;
begin
  if not Assigned (_WsdlReplyAsText) then
    raise Exception.Create('wsdlReplyAsText: implementation missing');
  result := _WsdlReplyAsText (aObject, aOperation);
end;

procedure wsdlRequestOperation (aObject: TObject; aOperation: String);
begin
  if not Assigned (_wsdlRequestOperation) then
    raise Exception.Create('wsdlRequestOperation: implementation missing');
  _wsdlRequestOperation (aObject, aOperation);
end;

procedure wsdlRequestOperationLater (aObject: TObject; aOperation: String; aLaterMs: Extended);
begin
  if not Assigned (_WsdlRequestOperationLater) then
    raise Exception.Create('wsdlRequestOperation: implementation missing');
  _WsdlRequestOperationLater (aObject, aOperation, aLaterMs);
end;

procedure wsdlFetchDefaultDesignMessage (aObject: TObject; aOperation: String);
begin
  if not Assigned (_wsdlFetchDefaultDesignMessage) then
    raise Exception.Create('wsdlFetchDefaultDesignMessage: implementation missing');
  _wsdlFetchDefaultDesignMessage (aObject, aOperation);
end;

procedure wsdlExecSql (aObject: TObject; aQuery: String);
begin
  if not Assigned (_WsdlExecSql) then
    raise Exception.Create('WsdlExecSql: implementation missing');
  _WsdlExecSql (aObject, aQuery);
end;

procedure wsdlNewDesignMessage (aObject: TObject; aOperation, aName: String);
begin
  if not Assigned (_WsdlNewDesignMessage) then
    raise Exception.Create('wsdlNewDesignMessage: implementation missing');
  _WsdlNewDesignMessage (aObject, aOperation, aName);
end;

procedure wsdlSendOperationRequest (aOperation, aCorrelation: String);
begin
  if not Assigned (_WsdlSendOperationRequest) then
    raise Exception.Create('wsdlSendOperationRequest: implementation missing');
  _WsdlSendOperationRequest (aOperation, aCorrelation);
end;

procedure wsdlSendOperationRequestLater (aOperation, aCorrelation: String; aLater: Extended);
var
  xLater: Integer;
begin
  xLater := Trunc (aLater);
  if not Assigned (_WsdlSendOperationRequestLater) then
    raise Exception.Create('wsdlSendOperationRequestLater: implementation missing');
  _WsdlSendOperationRequestLater (aOperation, aCorrelation, xLater);
end;

function asXmlString (aOwner: TObject; aArg: TObject): String;
var
  xXml: TXml;
begin
  if not (TCustomBindable ((aArg as YYSType).yy.yyPointer) is TXml) then
    raise Exception.Create ('asXmlString: Only allowed with XML elements');
  xXml := (TXml ((aArg as YYSType).yy.yyPointer));
  result := xXml.StreamXML ( True
                           , False
                           , 2
                           , True
                           , False
                           );
end;

procedure populateFromXmlString (aOwner: TObject; aArg1: TObject; aArg2: String);
var
  xXml, yXml: TXml;
begin
  if not (TCustomBindable ((aArg1 as YYSType).yy.yyPointer) is TXml) then
    raise Exception.Create ('populateFromXmlString: Only allowed with XML elements');
  xXml := (TXml ((aArg1 as YYSType).yy.yyPointer));
  yXml := TXml.Create;
  try
    yXml.LoadFromString(aArg2, nil);
    xXml.LoadValues(yXml, False, False, False, True);
    xXml.Checked := True;
  finally
    yXml.Free;
  end;
end;

procedure EnableMessage (aOperation: TWsdlOperation);
begin
end;

procedure EnableAllMessages;
begin
end;

procedure DisableMessage (aOperation: TWsdlOperation);
begin
end;

function OccurrencesX (aObject: TObject): Extended;
var
  da, dp: TXml;
  x: Integer;
begin
  result := 0;
  if not (TCustomBindable ((aObject as YYSType).yy.yyPointer) is TXml) then
    raise Exception.Create ('Occurrences: Only allowed with XML elements');
  da := TXml ((aObject as YYSType).yy.yyPointer);
  if not Assigned (da.Parent) then
  begin
    if da.Checked then
      result := 1;
    Exit;
  end;
  dp := da.Parent as TXml;
  for x := 0 to dp.Items.Count - 1 do
  begin
    if dp.Items.XmlItems[x].Checked
    and (dp.Items.XmlItems[x].Xsd = da.Xsd) then
      Result := Result + 1;
  end;
end;

function LengthX (arg: String): Extended;
begin
  result := Length (arg);
end;

function SubStringX(s: String; i, c: Extended): String;

begin
  result := Copy (s, Trunc (i), Trunc (c));
end;

function StrToDateX (arg: String): TDateTime;
begin
  result := xsdParseDate(arg);
end;

function StrToFloatX (arg: String): Extended;
begin
  result := StrToFloatDef(arg, 0);
end;

function StrFromClipboard: String;
begin
  result := Clipboard.AsText;
end;

procedure SleepX (aMS: Extended);
var
  x: Integer;
begin
  x := Trunc (aMS);
  Sleep (x);
end;

function isAccordingSchema (aObject: TObject): Extended;
var
  xBind: TCustomBindable;
  xMessage: String;
begin
  xMessage := ''; //avoid warning
  result := 1;
  xBind := TCustomBindable ((aObject as YYSType).yy.yyPointer);
  if not xBind.Checked then Exit;
  if xBind.IsExpression then Exit;
  if xBind is TXmlAttribute then
  begin
    if not (xBind as TXmlAttribute).IsValueValidAgainstXsd(xMessage) then
      result := 0;
  end;
  if xBind is TXml then
  begin
    if not (xBind as TXml).IsValueValidAgainstXsd(xMessage) then
      result := 0;
  end;
  if xBind is TIpmItem then
  begin
    if not (xBind as TIpmItem).IsValueValid(xMessage) then
      result := 0;
  end;
end;

function isAssigned (aObject: TObject): Extended;
begin
  if Assigned ((aObject as YYSType).yy.yyPointer)
  and TXml ((aObject as YYSType).yy.yyPointer).Checked then
    result := 1
  else
    result := 0;
end;

function sqlQuotedString (aString: String): String;
var
  x: Integer;
begin
  result := ''''
          + ReplaceStrings ( aString
                           , ''''
                           , ''''''
                           , false
                           , false
                           )
          + ''''
          ;
end;

procedure ResetOperationCounters;
var
  i: Integer;
begin
  AcquireLock;
  try
    for I := allOperations.Count - 1 downto 0 do
      allOperations.Operations[i].OperationCounter := 0;
  finally
    ReleaseLock;
  end;
end;

procedure ResetEnvVars (aOperation: TWsdlOperation; aRegExp: String);
var
  i: Integer;
begin
  AcquireEnvVarLock;
  try
    with TRegExpr.Create do
    try
      Expression := aRegExp;
      for I := aOperation.Wsdl.EnvVars.Count - 1 downto 0 do
        if (Exec(aOperation.Wsdl.EnvVars.Names[i])) then
          aOperation.Wsdl.EnvVars.Delete (i);
    finally
      Free;
    end;
  finally
    ReleaseEnvVarLock;
  end;
end;

procedure ResetEnvVar (aOperation: TWsdlOperation; aName: String);
var
  i: Integer;
begin
  AcquireEnvVarLock;
  try
    i := aOperation.Wsdl.EnvVars.IndexOfName(aName);
    if i >= 0 then
      aOperation.Wsdl.EnvVars.Delete (i);
  finally
    ReleaseEnvVarLock;
  end;
end;

function setEnvNumber (aOperation: TWsdlOperation; aName: String; aValue: Extended): Extended;
begin
  result := aValue;
  setEnvVar (aOperation, aName, FloatToStr(aValue));
end;

function setEnvVar (aOperation: TWsdlOperation; aName, aValue: String): String;
begin
  AcquireEnvVarLock;
  try
    result := StringReplace (aOperation.Wsdl.EnvVars.Values [aName], ESCAPEDCRLF, CRLF, [rfReplaceAll]);
    aOperation.Wsdl.EnvVars.Values [aName] := StringReplace (aValue, CRLF, ESCAPEDCRLF, [rfReplaceAll]);
  finally
    ReleaseEnvVarLock;
  end;
end;

function GetHostName: String;
begin
  result := _WsdlHostName;
end;

function ifThenString (aBoolean: Boolean; aTrueString, aFalseString: String): String;
begin
  if aBoolean then
    result := aTrueString
  else
    result := aFalseString;
end;

function getVarDef (aOperation: TWsdlOperation; aName, aDefault: String): String;
var
  i: Integer;
begin
  AcquireEnvVarLock;
  try
    i := aOperation.Wsdl.EnvVars.IndexOfName(aName);
    if i >= 0 then
      result := StringReplace (aOperation.Wsdl.EnvVars.ValueFromIndex [i], ESCAPEDCRLF, CRLF, [rfReplaceAll])
    else
      Result := aDefault;
  finally
    ReleaseEnvVarLock;
  end;
end;

function getVarDefT(aOperation: TWsdlOperation;aName, aDefault,
  aSeparator: String;aIndex: Extended): String;
var
  xValue: String;
begin
  result := aDefault;
  result := SeparatedStringT ( nil
                             , getVarDef( aOperation
                                        , aName
                                        , aDefault
                                        )
                             , aSeparator
                             , aIndex
                             );
end;

function getVar (aOperation: TWsdlOperation; aName: String): String;
begin
  result := getVarDef(aOperation, aName, '');
end;

function getVarNumberDef (aOperation: TWsdlOperation; aName: String; aDefault: Extended): Extended;
begin
  result := StrToFloatDef (getVar (aOperation, aName), aDefault);
end;

function getVarNumber (aOperation: TWsdlOperation; aName: String): Extended;
begin
  result := getVarNumberDef(aOperation, aName, 0);
end;

function incVarNumber (aOperation: TWsdlOperation; aName: String): Extended;
begin
  AcquireEnvVarLock; // nests lock....
  try
    result := getVarNumberDef(aOperation, aName, 0) + 1;
    setEnvNumber(aOperation, aName, result);
  finally
    ReleaseEnvVarLock;
  end;
end;

function decVarNumber (aOperation: TWsdlOperation; aName: String): Extended;
begin
  AcquireEnvVarLock; // nests lock....
  try
    result := getVarNumberDef(aOperation, aName, 0) - 1;
    setEnvNumber(aOperation, aName, result);
  finally
    ReleaseEnvVarLock;
  end;
end;

function xsdOperationCount(aOperation: TWsdlOperation): Extended;
begin
  AcquireLock;
  try
    while Assigned (aOperation.Cloned) do
      aOperation := aOperation.Cloned;
    result := aOperation.OperationCounter;
  finally
    ReleaseLock;
  end;
end;

function wsdlUserName: String;
begin
  result := xmlio.getUserName;
end;

function wsdlOperationName(aOper: TWsdlOperation): String;
begin
  result := aOper.Alias;
end;

function wsdlMessagingProtocol(aOper: TWsdlOperation): String;
begin
  if Assigned (aOper.Data)
  and (aOper.Data is TLog) then
    result := TransportTypeNames [(aOper.Data as TLog).TransportType]
  else
    result := TransportTypeNames [aOper.StubTransport];
end;

function wsdlMessageName(aOper: TWsdlOperation): String;
begin
  if Assigned (aOper.CorrelatedMessage) then
    result := aOper.CorrelatedMessage.Name
  else
    result := '';
end;

function xsdTodayAsDate: String;
begin
  result := FormatDateTime('yyyy-mm-dd', Now);
end;

function sblTodayAsDate: String;
begin
  result := FormatDateTime('mm/dd/yyyy', Now);
end;

procedure PromptReply(aOperation: TWsdlOperation);
begin
  SynchronizeMethode(aOperation.doPromptReply);
end;

procedure PromptRequest(aOperation: TWsdlOperation);
begin
  SynchronizeMethode(aOperation.doPromptRequest);
end;

procedure RaiseExit(aOperation: TWsdlOperation);
begin
  aOperation.DoExit := True;
end;

procedure ReturnString (aOperation: TWsdlOperation; aString: String);
begin
  aOperation.LiteralResult := aString;
  aOperation.DoExit := True;
end;

procedure RaiseWsdlFault (aOperation: TWsdlOperation; faultcode, faultstring, faultactor: String);
begin
  aOperation.faultcode := faultcode;
  aOperation.faultstring := faultstring;
  aOperation.faultactor := faultactor;
  aOperation.LiteralResult := aOperation.StreamFault('', False);
  aOperation.ReturnSoapFault := True;
  aOperation.ResponseNo := 500;
  aOperation.DoExit := True;
end;

procedure RaiseHttpFault (aOperation: TWsdlOperation; aResponseCode, aResponseText, aResponseContentType: String);
var
  x: Integer;
begin
  aOperation.LiteralResult := aResponseText;
  aOperation.ContentType := aResponseContentType;
  try
    aOperation.ResponseNo := StrToInt(aResponseCode);
  except
    aOperation.ResponseNo := 500;
    aOperation.LiteralResult := 'exception, could not convert  '
                              + aResponseCode
                              + ' to integer.'
                              + LineEnding
                              + aOperation.LiteralResult
                              ;
  end;
  aOperation.ReturnSoapFault := True;
  aOperation.DoExit := True;
end;

procedure RaiseSoapFault (aOperation: TWsdlOperation; faultcode, faultstring, faultactor, detail: String);
var
  x: Integer;
begin
  aOperation.faultcode := faultcode;
  aOperation.faultstring := faultstring;
  aOperation.faultactor := faultactor;
  with TXml.CreateAsString('soapenv:Envelope','') do
  try
    AddAttribute(TXmlAttribute.CreateAsString('xmlns:soapenv', 'http://schemas.xmlsoap.org/soap/envelope/'));
    AddAttribute(TXmlAttribute.CreateAsString('xmlns:soapenc', 'http://schemas.xmlsoap.org/soap/encoding/'));
    AddAttribute(TXmlAttribute.CreateAsString('xmlns:soap', 'http://schemas.xmlsoap.org/wsdl/soap/'));
    AddAttribute(TXmlAttribute.CreateAsString('xmlns:wsdl', 'http://schemas.xmlsoap.org/wsdl/'));
    AddAttribute(TXmlAttribute.CreateAsString('xmlns:xsd', scXMLSchemaURI));
    AddAttribute(TXmlAttribute.CreateAsString('xmlns:xsi', scXMLSchemaInstanceURI));
    with AddXml(TXml.CreateAsString('soapenv:Header', '')) do
    begin
      for x := 0 to aOperation.OutputHeaders.Count - 1 do
      begin
        AddXml ((aOperation.rpyBind as TXml).Items.XmlItems [x].XmlStreamer
                                 ( True
                                 , True
                                 , 4
                                 , True
                                 , (aOperation.OutputHeaders.Headers[x].Use = scSoapUseEncoded)
                                 )
               );
      end;
    end;
    with AddXml(TXml.CreateAsString('soapenv:Body', '')) do
    begin
      with AddXml (TXml.CreateAsString('soapenv:Fault', '')) do
      begin
        AddXml (TXml.CreateAsString('faultcode', aOperation.faultcode));
        AddXml (TXml.CreateAsString('faultstring', aOperation.faultstring));
        AddXml (TXml.CreateAsString('faultactor', aOperation.faultactor));
        AddXml (TXml.CreateAsString('detail', detail));
      end;
    end;
    aOperation.LiteralResult := AsText(False, 0, False, False);
    aOperation.ReturnSoapFault := True;
    aOperation.DoExit := True;
    aOperation.ResponseNo := 500;
  finally
    Free;
  end;
end;

constructor TWsdl.Create(aEnvVars: TJBStringList; aOperationsWithEndpointOnly: Boolean);
begin
  inherited Create;
  EnvVars := aEnvVars;
  OperationsWithEndpointOnly := aOperationsWithEndpointOnly;
  Servers := TJBStringList.Create;
  Services := TWsdlServices.Create;
  Services.Sorted := True;
  ServerPathNames := TJBStringList.Create;
  XsdDescr := TXsdDescr.Create;
  sdfXsdDescrs := TXsdDescrList.Create;
  IpmDescrs := TIpmDescrs.Create;
  IpmDescrs.Sorted := False;
  ExtraXsds := TJBStringList.Create;
  ExtraXsds.Sorted := True;
  ExtraXsds.Duplicates := dupIgnore;
  fMssgs := TWsdlMsgDescrs.Create;
  fMssgs.Sorted := True;
  fMssgs.Duplicates := dupError;
  fOpers := TWsdlOperations.Create;
  fOpers.Sorted := True;
  fOpers.Duplicates := dupError;
  fStrs := TJBStringList.Create;
  fStrs.NameValueSeparator := '=';
end;

destructor TWsdl.Destroy;
var
  x: Integer;
begin
  XsdDescr.Free;
  for x := 0 to IpmDescrs.Count - 1 do
    IpmDescrs.IpmDescrs [x].Free;
  FreeAndNil (IpmDescrs);
  sdfXsdDescrs.Clear;
  FreeAndNil(sdfXsdDescrs);
  for x := 0 to Services.Count - 1 do
    Services.Services [x].Free;
  FreeAndNil (Services);
  FreeAndNil (ServerPathNames);
  ExtraXsds.Clear;
  ExtraXsds.Free;
  fMssgs.ClearListOnly;
  FreeAndNil(fMssgs);
  fOpers.ClearListOnly;
  FreeAndNil(fOpers);
  FreeAndNil(fStrs);
  FreeAndNil(Servers);
  inherited;
end;


function TWsdl.getServiceByName(Index: String): TWsdlService;
var
  x: Integer;
begin
  result := nil;
  for x := 0 to Services.Count - 1 do
  begin
    if (Services.Services[x].Name = Index) then
    begin
      result := Services.Services[x];
      Exit;
    end;
  end;
end;

function TWsdl.getOperationByRequest(Index: String): TWsdlOperation;
var
  s, o: Integer;
begin
  result := nil;
  for s := 0 to Services.Count - 1 do
    for o := 0 to Services.Services[s].Operations.Count - 1 do
      if Services.Services[s].Operations.Operations[o].reqTagName = Index then
      begin
        result := Services.Services[s].Operations.Operations[o];
      end;
end;

function TWsdl.thisWsdl: TWsdl;
begin
  result := self;
end;

procedure TWsdl .ValidateEvaluatedTags (aXml : TXml ; aSl : TJBStringList );
var
  x, f: Integer;
begin
  for x := 0 to aXml.Items.Count - 1 do
    if not aSl.Find(aXml.Items.XmlItems[x].Name, f) then
      SjowMessage(Format('not evaluated %s at %s', [aXml.Items.XmlItems[x].Name, aXml.FullCaption]));
end;

procedure TWsdl.LoadFromSchemaFile (aFileName : String; aOnError: TOnErrorEvent; aOnbeforeRead: TProcedureS);
  procedure _LoadFromXml (aXml: TXml; aFileName: String);
    procedure _LoadFromFile (aFileName : String);
    var
      xXml: TXml;
    begin
      xXml := TXml.Create;
      try
        xXml.LoadFromFile(aFileName, aOnError, aOnbeforeRead);
        xXml.SeparateNsPrefixes;
        xXml.ResolveNameSpaces;
        _LoadFromXml(xXml, aFileName);
      finally
        xXml.Free;
      end;
    end;
  var
    a, x, y, z, f, h: Integer;
    Mssg: TWsdlMsgDescr;
    Part: TWsdlPart;
    Oper: TWsdlOperation;
    Srvc: TWsdlService;
    xHdr: TWsdlHeader;
    xTargetNamespace: String;
    PortTypeName, SoapBindingStyle, SoapTransport, OperationName, FaultName: String;
  begin
    xTargetNamespace := aXml.Attributes.ValueByTag[tagTargetNamespace];
    Name := aXml.Attributes.ValueByTag[tagName];
    for x := 0 to aXml.Items.Count - 1 do with aXml.Items.XmlItems[x] do
    begin
      if Name = tagImport then
        _LoadFromFile (xmlio.ExpandRelativeFileName (aFileName, Attributes.ValueByTag[tagLocation]));
      if Name = tagInclude then
        _LoadFromFile (xmlio.ExpandRelativeFileName(aFileName, Attributes.ValueByTag[tagLocation]));
    end;
    for x := 0 to aXml.Items.Count - 1 do with aXml.Items do
    begin
      if (XmlItems[x].Name = tagTypes)
      or (XmlItems[x].Name = tagSchema) then
        XsdDescr.AddXsdFromXml (XmlItems[x], aFileName, aOnError, aOnbeforeRead);
    end;
    for x := 0 to aXml.Items.Count - 1 do with aXml.Items do
    begin
      if XmlItems[x].Name = tagMessage then
      begin
        Mssg := TWsdlMsgDescr.Create(self);
        Mssg.Name := XmlItems[x].Attributes.ValueByTag[tagName];
        Mssg.NameSpace := xTargetNamespace;
        if not fMssgs.Find (Mssg.NameSpace + ';' + Mssg.Name, f) then
        begin
          fMssgs.AddObject(Mssg.NameSpace + ';' + Mssg.Name, Mssg);
          for y := 0 to XmlItems[x].Items.Count - 1 do with XmlItems[x].Items.XmlItems[y] do
          begin
            if Name = tagPart then
            begin
              Part := TWsdlPart.Create;
              Part.Name := Attributes.ValueByTag[xmlzConsts.tagName];
              Part._ElementName := ExpandPrefixedName(xTargetNamespace, Attributes.ValueByTag[tagElement]);
              Part._TypeName := ExpandPrefixedName(xTargetNamespace, Attributes.ValueByTag[tagType]);
              Mssg.Parts.AddObject('', Part);
            end;
          end;
        end
        else
          Mssg.Free;  // duplicate; ignore
      end;
    end;
    for x := 0 to aXml.Items.Count - 1 do with aXml.Items do
    begin
      if XmlItems[x].Name = tagPortType then
      begin
        PortTypeName := xTargetNamespace + ';' + XmlItems[x].Attributes.ValueByTag[tagName];
        for y := 0 to XmlItems[x].Items.Count - 1 do with XmlItems[x].Items do
        begin
          if XmlItems[y].Name = tagOperation then
          begin
            Oper := TWsdlOperation.Create(self);
            Oper.Name := XmlItems[y].Attributes.ValueByTag[tagName];
            fOpers.AddObject(Format ('%s;%s', [PortTypeName, Oper.Name]), Oper);
            Oper.SoapAddress := PortTypeName; // for later lookup real address
            for z := 0 to XmlItems[y].Items.Count - 1 do with XmlItems[y].Items.XmlItems[z] do
            begin
              if Name = tagInput then
                Oper._InputMessageName := ExpandPrefixedName(xTargetNamespace, Attributes.ValueByTag[tagMessage]);
              if Name = tagOutput then
                Oper._OutputMessageName := ExpandPrefixedName(xTargetNamespace, Attributes.ValueByTag[tagMessage]);
              if Name = tagFault then
                fStrs.Values [PortTypeName+';'+Oper.Name+';FltMssg;'+AttributeValueByTag[xmlzConsts.tagName]+'.Message']
                  := ExpandPrefixedName(xTargetNamespace, Attributes.ValueByTag[tagMessage]);
            end;
          end;
        end;
      end;
    end;
    for x := 0 to aXml.Items.Count - 1 do with aXml.Items.XmlItems[x] do
    begin
      if Name = tagBinding then
      begin
        PortTypeName := ExpandPrefixedName(xTargetNamespace, Attributes.ValueByTag[tagType]);
        fStrs.Values[xTargetNamespace + ';' + AttributeValueByTag[xmlzConsts.tagName] + '.Bind'] := PortTypeName;
        SoapBindingStyle := ItemByTag[tagBinding].AttributeValueByTag[tagStyle];
        SoapTransport := ItemByTag[tagBinding].AttributeValueByTag[tagTransport];
        for y := 0 to Items.Count - 1 do with Items.XmlItems[y] do
        begin
          if Name = tagOperation then
          begin
            OperationName := AttributeValueByTag[xmlzConsts.tagName];
            if not fOpers.Find(PortTypeName + ';' + OperationName, f) then
              raise Exception.CreateFmt('Operation %s not found in portTypes', [PortTypeName + ';' + OperationName]);
            Oper := fOpers.Operations[f];
            Oper.SoapBindingStyle := SoapBindingStyle;
            Oper.SoapTransport := SoapTransport;
            for z := 0 to Items.Count - 1 do with Items.XmlItems[z] do
            begin
              if Name = tagOperation then
              begin
                if NameSpace = 'http://schemas.xmlsoap.org/wsdl/soap/' then
                  SoapVersion := svSOAP11;
                if NameSpace = 'http://schemas.xmlsoap.org/wsdl/soap12/' then
                  SoapVersion := svSOAP12;
                Oper.SoapAction := AttributeValueByTag[tagSoapAction];
                Oper.SoapBindingStyle := AttributeValueByTagDef[tagStyle, Oper.SoapBindingStyle];
              end;
              if Name = tagInput then
              begin
                for h := 0 to Items.Count - 1 do with Items.XmlItems[h] do
                begin
                  if Name = tagHeader then
                  begin
                    xHdr := TWsdlHeader.Create;
                    xHdr.MessageName := ExpandPrefixedName(xTargetNamespace, AttributeValueByTag[tagMessage]);
                    xHdr.PartName := AttributeValueByTag[tagPart];
                    xHdr.Use := AttributeValueByTagDef[tagUse, scSoapUseLiteral];
                    xHdr.EncodingStyle := AttributeValueByTag[tagEncodingStyle];
                    xHdr.Required := Attributes.BooleanByTagDef[tagRequired, False];
                    Oper.InputHeaders.AddObject(xHdr.MessageName, xHdr);
                  end;
                end;
                Oper.SoapBodyInputPartName := ItemByTag[tagBody].AttributeValueByTag[tagParts];
                Oper.SoapBodyInputUse := ItemByTag[tagBody].AttributeValueByTag[tagUse];
                Oper.SoapBodyInputEncodingStype:= ItemByTag[tagBody].AttributeValueByTag[tagEncodingStyle];
                Oper.SoapBodyInputRequired := ItemByTag[tagBody].AttributeBooleanByTagDef[tagRequired, False];
              end;
              if Name = tagOutput then
              begin
                for h := 0 to Items.Count - 1 do with Items.XmlItems[h] do
                begin
                  if Name = tagHeader then
                  begin
                    xHdr := TWsdlHeader.Create;
                    xHdr.MessageName := ExpandPrefixedName(xTargetNamespace, AttributeValueByTag[tagMessage]);
                    xHdr.PartName := AttributeValueByTag[tagPart];
                    xHdr.Use := AttributeValueByTagDef[tagUse, scSoapUseLiteral];
                    xHdr.EncodingStyle := AttributeValueByTag[tagEncodingStyle];
                    xHdr.Required := Attributes.BooleanByTagDef[tagRequired, False];
                    Oper.OutputHeaders.AddObject(xHdr.MessageName, xHdr);
                  end;
                end;
                Oper.SoapBodyOutputPartName := ItemByTag[tagBody].AttributeValueByTag[tagParts];
                Oper.SoapBodyOutputUse := ItemByTag[tagBody].AttributeValueByTag[tagUse];
                Oper.SoapBodyOutputEncodingStype:= ItemByTag[tagBody].AttributeValueByTag[tagEncodingStyle];
                Oper.SoapBodyOutputRequired := ItemByTag[tagBody].AttributeBooleanByTagDef[tagRequired, False];
              end;
              if Name = tagFault then
              begin
                if not Assigned (Oper.FaultMessages) then
                  Oper.FaultMessages := TWsdlMsgDescrs.Create;
                FaultName := AttributeValueByTag[xmlzConsts.tagName];
                for h := 0 to Items.Count - 1 do with Items.XmlItems[h] do
                begin
                  if Name = tagFault then
                  begin
                    Oper.FaultMessages.AddObject(FaultName, nil);
                    for a := 0 to Attributes.Count - 1 do with Attributes.XmlAttributes[a] do
                      fStrs.Values [PortTypeName+';'+OperationName+';FltMssg;'+FaultName+'.'+Name] := Value;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    for x := 0 to aXml.Items.Count - 1 do with aXml.Items.XmlItems[x] do
    begin
      if Name = tagService then
      begin
        Srvc := TWsdlService.Create;
        Srvc.DescriptionType := ipmDTWsdl;
        Srvc.Name := AttributeValueByTag[xmlzConsts.tagName];
        Services.AddObject(Srvc.Name, Srvc);
        for y := 0 to Items.Count - 1 do with Items.XmlItems[y] do
        begin
          if Name = tagPort then
          begin
            PortTypeName := fStrs.Values[ExpandPrefixedName(xTargetNamespace, AttributeValueByTag[tagBinding]) + '.Bind'];
            fStrs.Values[PortTypeName + '.SoapAddress'] := ItemByTag[tagAddress].AttributeValueByTag[tagLocation];
            fStrs.Values[PortTypeName + '.Service'] := Srvc.Name;
          end;
        end;
      end;
    end;
  end;

var
  xXml: TXml;
  x, h, m, o, p, f: Integer;
  ServiceName, PortTypeName, OperationName, FaultName, MessageName: String;
  Srvc: TWsdlService;
  Mssg: TWsdlMsgDescr;
  xXsds: TXsdList;
begin
  isSoapService := True;
  XsdDescr.Clear;
  fStrs.Clear;
  fMssgs.ClearListOnly;
  fOpers.ClearListOnly;
  Services.Clear;
  XsdDescr.AddBuiltIns;
  XsdDescr.ReadFileNames.Add(aFileName);
  FileName := aFileName;
  xXml := TXml.Create;
  try
    xXml.LoadFromFile(aFileName, aOnError, aOnbeforeRead);
    xXml.SeparateNsPrefixes;
    xXml.ResolveNameSpaces;
    if (xXml.Name <> tagDefinitions)
    or (     (xXml.NameSpace <> scWsdlNameSpace)
       ) and (xXml.NameSpace <> '')then
      raise Exception.CreateFmt ('%s is not a WSDL file', [aFileName]);
    _LoadFromXml (xXml, aFileName);
    XsdDescr.Finalise;

    xXsds := TXsdList.Create;
    xXsds.Sorted := True;
    xXsds.CaseSensitive := True;
    for x := 0 to XsdDescr.TypeDef.ElementDefs.Count - 1 do with XsdDescr.TypeDef.ElementDefs do
      xXsds.AddObject(Xsds[x].ElementNameSpace + ';' + Xsds[x].ElementName, Xsds[x]);
    for m := 0 to fMssgs.Count - 1 do with fMssgs.Messages[m] do
    begin
      for p := 0 to Parts.Count - 1 do
      begin
        Parts.Parts[p].LinkToXsd(xXsds, self);
        Xsd.sType.AddXsd(Parts.Parts[p].Xsd);
      end;
    end;
    FreeAndNil(xXsds);
    for o := 0 to fOpers.Count - 1 do with fOpers.Operations[o] do
    begin
      OperationName := Name;
      PortTypeName := SoapAddress;  // SoapAdress contains PortTypeName
      ServiceName := fStrs.Values[PortTypeName + '.Service'];
      Srvc := ServiceByName[ServiceName];
      case SoapVersion of
        svSOAP11, svUnspecified:
          begin
            ContentType := 'text/xml;charset=utf-8';
            Accept := ContentType;
          end;
        svSOAP12:
          begin
            ContentType := 'application/soap+xml;charset=utf-8';
            Accept := ContentType;
          end;
      end;
      if Assigned (Srvc) then
      begin
        Srvc.Operations.AddObject(fOpers.Operations[o].Name, fOpers.Operations[o]);
        fOpers.Operations[o].WsdlService := Srvc;
        SoapAddress := fStrs.Values[PortTypeName + '.SoapAddress']; // SoapAdress contained PortTypeName
      end;
      for h := 0 to InputHeaders.Count - 1 do with InputHeaders.Headers[h] do
      begin
        if not fMssgs.Find (MessageName, f) then
          raise Exception.CreateFmt('InputHeaderMessage %s not found for operation %s', [MessageName, Name]);
        Mssg := fMssgs.Messages[f];
        for p := 0 to Mssg.Parts.Count - 1 do
          if Mssg.Parts.Parts[p].Name = PartName then
            reqXsd.sType.AddXsd(Mssg.Parts.Parts[p].Xsd);
      end;
      if _InputMessageName <> '' then
      begin
        if not fMssgs.Find (_InputMessageName, f) then
          raise Exception.CreateFmt('InputBodyMessage (%s) not found at operation %s', [_InputMessageName, Name]);
        Mssg := fMssgs.Messages[f];
        for p := 0 to Mssg.Parts.Count - 1 do
          reqXsd.sType.AddXsd(Mssg.Parts.Parts[p].Xsd);
        if (Mssg.Parts.Count > 0) then
        begin
          reqTagName := Mssg.Parts.Parts[0].Xsd.ElementName;
          reqTagNameSpace := Mssg.Parts.Parts[0].Xsd.ElementNameSpace;
        end;
        FreeAndNil(freqBind);
        bindRefId := 0;
        reqBind := TXml.Create (0, reqXsd);
        reqBind.Name := Mssg.Name;
        reqMessageName := Mssg.Name;
        Alias := reqTagName;
      end;

      for h := 0 to OutputHeaders.Count - 1 do with OutputHeaders.Headers[h] do
      begin
        if not fMssgs.Find (MessageName, f) then
          raise Exception.CreateFmt('OutputHeaderMessage %s not found for operation %s', [MessageName, Name]);
        Mssg := fMssgs.Messages[f];
        for p := 0 to Mssg.Parts.Count - 1 do
          if Mssg.Parts.Parts[p].Name = PartName then
            rpyXsd.sType.AddXsd(Mssg.Parts.Parts[p].Xsd);
      end;
      if _OutputMessageName <> '' then
      begin
        if not fMssgs.Find (_OutputMessageName, f) then
          raise Exception.CreateFmt('OutputBodyMessage (%s) not found at operation %s', [_OutputMessageName, Name]);
        Mssg := fMssgs.Messages[f];
        if (Mssg.Parts.Count > 0)
        and Assigned (Mssg.Parts.Parts[0].Xsd) then
        begin
          rpyTagName := Mssg.Parts.Parts[0].Xsd.ElementName;
          rpyTagNameSpace := Mssg.Parts.Parts[0].Xsd.ElementNameSpace;
          rpyXsd.sType.AddXsd(Mssg.Parts.Parts[0].Xsd);
        end;
        FreeAndNil(fRpyBind);
        bindRefId := 0;
        rpyBind := TXml.Create (0, rpyXsd);
        rpyBind.Name := Mssg.Name;
        rpyMessageName := Mssg.Name;
      end;

      if Assigned (FaultMessages) then
      begin
        for m := 0 to FaultMessages.Count - 1 do
        begin
          FaultName := FaultMessages.Strings[m];
          MessageName := fStrs.Values[PortTypeName+';'+OperationName+';FltMssg;'+FaultName+'.Message'];
          if not fMssgs.Find (MessageName, f) then
            raise Exception.CreateFmt('Faultmessage %s not found for soapfault %s', [MessageName, FaultName]);
          FaultMessages.Objects[m] := fMssgs.Messages[f];
          fMssgs.Messages[f].Xsd.ElementName:=FaultName;
          fMssgs.Messages[f].Name:=FaultName;
          FaultXsd.sType.AddXsd(fMssgs.Messages[f].Xsd);
        end;
        FreeAndNil(fltBind);
        bindRefId := 0;
        fltBind := TXml.Create (0, FaultXsd);
        fltBind.Name := 'Faults';
      end;
    end;
  finally
    xXml.Free;
  end;
  fOpers.ClearListOnly;
  fMssgs.ClearListOnly;
end;

procedure TWsdl.LoadFromJsonYamlFile(aFileName: String; aOnError: TOnErrorEvent; aOnbeforeRead: TProcedureS);
//
// Based on https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md
//
  function _makeValidName (aName: String): String;
  var
    naChars: String;
    x: Integer;
  begin
    result := aName;
    naChars := ' \/:*?"<>|';
    for x := 1 to Length(Result) do
    begin
      if Pos (result [x], naChars) > 0 then
        result [x] := '_';
    end;
  end;

  function _prepareRef (aValue: String): String;
  var
    x: Integer;
  begin
    if Copy (aValue, 1, 2) <> '#/' then
      raise Exception.CreateFmt('_prepareRef: preparing for "%s" not yet implemented', [aValue]);
    SetLength(result, Length(aValue));
    for x := 1 to Length (aValue) do
    begin
      if aValue [x] = '/' then
        result [x] := '.'
      else
        result [x] := aValue [x];
    end;
  end;

  procedure _ResolveDollarRefs;
    procedure _resolve (aXsd: TXsd);
    var
      x, f: Integer;
    begin
      if aXsd._Processed then Exit;
      if not Assigned (aXsd.sType) then Exit;
      aXsd._Processed := True;
      try
        if aXsd.sType.dollarRef <> '' then
          if self.XsdDescr.TypeDefs.Find(aXsd.sType.dollarRef, f) then
            aXsd.sType := self.XsdDescr.TypeDefs.XsdDataTypes[f];
        for x := 0 to aXsd.sType.ElementDefs.Count - 1 do
          _resolve (aXsd.sType.ElementDefs.Xsds[x]);
      finally
        aXsd._Processed := False;
      end;
    end;
  var
    s, o, x: Integer;
  begin
    for s := 0 to Services.Count - 1 do with Services.Services[s] do
    begin
      for o := 0 to Operations.Count - 1 do with Operations.Operations[o] do
      begin
        _resolve(reqXsd);
        _resolve(rpyXsd);
      end;
    end;
  end;

  procedure _appendInfo (var aExisting: String; aNew:String);
  begin
    if Length (aExisting) > 0 then
      aExisting := aExisting + LineEnding + aNew
    else
      aExisting := aNew;
  end;

  function _trimPath (aPath: String): String;
  begin
    if (Length (aPath) > 0)
    and (Copy (aPath, Length(aPath), 1) = '/') then
      result := _trimPath (Copy (aPath, 1, Length(aPath) - 1))
    else
      result := aPath;
  end;

  function _initXsd (aParentXsd: TXsd; aName: String): TXsd;
  begin
    result := TXsd.Create(XsdDescr);
    XsdDescr.Garbage.AddObject('', result);
    result.ElementName := aName;
    result.ResponseNo := StrToIntDef(Copy (aName, 6, 3), 200);
    result.sType := TXsdDataType.Create(XsdDescr);
    result.sType.jsonType := jsonObject;
    result.sType.xsdType:= dtComplexType;
    XsdDescr.Garbage.AddObject('', result.sType);
    result.sType.Name := aName;
    result.minOccurs := '0';
    aParentXsd.sType.ElementDefs.AddObject(result.ElementName, result);
  end;

  procedure _evaluateParameter (aRootXml: TXml; aXml: TXml; aParentXsd: TXsd);
  var
    x: Integer;
    yXml: TXml;
  begin
    if (aXml.Items.Count = 1) then
    with aXml.Items.XmlItems[0] do
    begin
      if (Name = S_DOLLARREF) then
      begin
        yXml := XsdDescr.FindReferencedXml(Value) as TXml;
        if not Assigned(yXml) then
          raise Exception.CreateFmt('Coud not resolve %s in %s', [Value, aFileName]);
        _evaluateParameter (aRootXml, yXml, aParentXsd);
        Exit;
      end;
    end;
    with _initXsd (aParentXsd, _makeValidName (aXml.Items.XmlValueByTag['name'])) do
    begin
      sType := XsdDescr.AddTypeDefFromJsonXml(aFileName, aFileName, aXml, aOnError);
      sType.jsonType := jsonObject;
      sType.Name := ElementName;
      for x := 0 to aXml.Items.Count - 1 do with aXml.Items.XmlItems[x] do
      begin
        if Name = 'in' then
        begin
          if Value = 'path' then ParametersType := oppPath;
          if Value = 'query' then ParametersType := oppQuery;
          if Value = 'header' then ParametersType := oppHeader;
          if Value = 'body' then ParametersType := oppBody; // swagger 2.0
          if Value = 'form' then ParametersType := oppFormData;
          if Value = 'formData' then ParametersType := oppFormData;
        end;
        if Name = 'description' then Documentation.Text := Value;
        if Name = 'required' then
          if Value = 'true' then
            minOccurs := '1'
          else
            minOccurs := '0';
        if Name = 'collectionFormat' then
        begin
          sType.jsonType := jsonArray;
          if Value = 'csv' then sType.CollectionFormat := ocfCSV;
          if Value = 'ssv' then sType.CollectionFormat := ocfSSV;
          if Value = 'tsv' then sType.CollectionFormat := ocfTSV;
          if Value = 'pipes' then sType.CollectionFormat := ocfPipes;
          if Value = 'multi' then sType.CollectionFormat := ocfMulti;
        end;
      end;
    end;
  end;

  procedure _addUndefXsd (aParentXsd: TXsd);
  var
    xXsd, yXsd: TXsd;
  begin
    xXsd := TXsd.Create(XsdDescr);
    XsdDescr.Garbage.AddObject('', xXsd);
    xXsd.ElementName := 'undefined';
    xXsd.ResponseNo := -1;
    xXsd.sType := TXsdDataType.Create(XsdDescr);
    xXsd.sType.jsonType := jsonObject;
    xXsd.sType.xsdType:= dtComplexType;
    XsdDescr.Garbage.AddObject('', xXsd.sType);
    xXsd.sType.Name := 'other';
    xXsd.minOccurs := '0';
    aParentXsd.sType.ElementDefs.AddObject(xXsd.ElementName, xXsd);
    yXsd := TXsd.Create(XsdDescr);
    XsdDescr.Garbage.AddObject('', yXsd);
    yXsd.ElementName := 'responseCode';
    yXsd.sType := TXsdDataType.Create(XsdDescr);
    yXsd.sType.Name := yXsd.ElementName;
    yXsd.sType.xsdType := dtSimpleType;
    yXsd.sType.BaseDataTypeName := 'string';
    yXsd.sType.jsonType := jsonString;
    xXsd.sType.ElementDefs.AddObject(yXsd.ElementName, yXsd);
  end;

  procedure _evaluateContent ( aService: TWsdlService
                             ; aOperation: TWsdlOperation
                             ; aXsd: TXsd
                             ; aItems: TXmlList
                             ; aRootXml: TXml
                             );
  var
    x, y, f: Integer;
    yXsd: TXsd;
    xName: String;
  begin
    for x := 0 to aItems.Count - 1 do
    begin
      xName := makeValidId(aItems.XmlItems[x].Name); // Name is filled with a media type
      if AnsiStartsText('application_', xName) then
        xName := Copy (xName, Length('application_') + 1, MaxInt);
      yXsd := TXsd.Create(XsdDescr);
      yXsd.ParametersType := oppBody;
      XsdDescr.Garbage.AddObject('', yXsd);
      yXsd.ElementName := xName;
      yXsd.MediaType := aItems.XmlItems[x].Name;
      XsdDescr.TypeDefs.Find(scXMLSchemaURI + ';' + xmlzConsts.xsdString, f);
      yXsd.sType := XsdDescr.TypeDefs.XsdDataTypes[f];
      yXsd.sType.Name := aXsd.ElementName+xName;
      yXsd.isOneOfGroupLevel := 1;
      aXsd.sType.ElementDefs.AddObject(yXsd.ElementName, yXsd);
      for y := 0 to aItems.XmlItems[x].Items.Count - 1 do
      with aItems.XmlItems[x].Items do
      begin
        if XmlItems[y].Name = 'schema' then
          yXsd.sType := XsdDescr.AddTypeDefFromJsonXml(aFileName, aFileName, XmlItems[y], aOnError);
        if XmlItems[y].Name = 'example' then ; // TODO
        if XmlItems[y].Name = 'examples' then ; // TODO
        if XmlItems[y].Name = 'encoding' then ; // TODO
      end;
    end;
  end;

  procedure _evaluateRequestBody ( aService: TWsdlService
                                 ; aOperation: TWsdlOperation
                                 ; var aDoc: String
                                 ; aItems: TXmlList
                                 ; aRootXml: TXml
                                 );
  var
    x: Integer;
    yXml: TXml;
    xXsd: TXsd;
  begin
    if (aItems.Count = 1) then
    with aItems.XmlItems[0] do
    begin
      if (Name = S_DOLLARREF) then
      begin
        yXml := XsdDescr.FindReferencedXml(Value) as TXml;
        if not Assigned(yXml) then
          raise Exception.CreateFmt('Coud not resolve %s at %s,%s', [Value, aService.Name, aOperation.Name]);
        _evaluateRequestBody(aService, aOperation, aDoc, yXml.Items, aRootXml);
        Exit;
      end;
    end;
    for x := 0 to aItems.Count - 1 do with aItems.XmlItems[x] do
    begin
      if Name = 'description' then _appendInfo(aDoc, Value);
      if Name = 'content' then _evaluateContent (aService, aOperation, aOperation.reqXsd, Items, aRootXml);
      if Name = 'required' then ;
    end;
  end;

  procedure _evaluateResponses200 ( aService: TWsdlService  // swagger 2.0
                                  ; aOperation: TWsdlOperation
                                  ; var aDoc: String
                                  ; Items: TXmlList
                                  ; aRootXml: TXml
                                  );
    procedure _evaluateResponse200 ( aXsd: TXsd
                                   ; var aDoc: String
                                   ; aXml: TXml
                                   );
    var
      w, h: Integer;
      wXml, hXml: TXml;
      hXsd, yXsd: TXsd;
    begin
      if (aXml.Items.Count = 1) then
      with aXml.Items.XmlItems[0] do
      begin
        if (Name = S_DOLLARREF) then
        begin
          hXml := XsdDescr.FindReferencedXml(Value) as TXml;
          if not Assigned(hXml) then
            raise Exception.CreateFmt('Coud not resolve %s at %s,%s', [Value, aService.Name, aOperation.Name]);
          _evaluateResponse200(aXsd, aDoc, hXml);
          Exit;
        end;
      end;
      for w := 0 to aXml.Items.Count - 1 do
      begin
        wXml := aXml.Items.XmlItems[w];
        if wXml.Name = 'description' then aXsd.Documentation.Text := wXml.Value;
        if wXml.Name = 'schema' then
        begin
          yXsd := TXsd.Create(XsdDescr);
          XsdDescr.Garbage.AddObject('', yXsd);
          yXsd.ElementName := 'body';
          yXsd.sType := XsdDescr.AddTypeDefFromJsonXml(aFileName, aFileName, aXml, aOnError);
          yXsd.sType.Name := aXsd.ElementName;
          aXsd.sType.ElementDefs.AddObject(yXsd.ElementName, yXsd);
        end;
        if wXml.Name = 'headers' then
        begin
          for h := 0 to wXml.Items.Count - 1 do
          begin
            hXml := wXml.Items.XmlItems[h];
            hXsd := TXsd.Create(XsdDescr);
            XsdDescr.Garbage.AddObject('', hXsd);
            hXsd.ElementName := hXml.Name;
            hXsd.sType := XsdDescr.AddTypeDefFromJsonXml(aFileName, '', hXml, aOnError);
            hXsd.sType.Name := hXsd.ElementName;
            hXsd.minOccurs := '0';
            hXsd.ParametersType := oppHeader;
            aXsd.sType.ElementDefs.AddObject(hXsd.ElementName, hXsd);
          end;
        end;
      end;
    end;

  var
    x, y, z, v, w, h: Integer;
    xXml, yXml, zXml, vXml, wXml, hXml: TXml;
    xXsd, yXsd, hXsd: TXsd;
  begin
    if (Items.Count = 1) then
    with Items.XmlItems[0] do
    begin
      if (Name = S_DOLLARREF) then
      begin
        yXml := XsdDescr.FindReferencedXml(Value) as TXml;
        if not Assigned(yXml) then
          raise Exception.CreateFmt('Coud not resolve %s at %s,%s', [Value, aService.Name, aOperation.Name]);
        _evaluateResponses200(aService, aOperation, aDoc, yXml.Items, aRootXml);
        Exit;
      end;
    end;
    aOperation.rpyXsd.sType.ContentModel := 'Choice';
    for v := 0 to Items.Count - 1 do
    begin
      vXml := Items.XmlItems[v];
      xXsd := _initXsd(aOperation.rpyXsd, 'rspns' + vXml.Name);
      _evaluateResponse200 (xXsd, aDoc, vXml);
    end;
    _addUndefXsd(aOperation.rpyXsd);
  end;

  procedure _evaluateResponses300 ( aService: TWsdlService // OpenApi 3.0
                                  ; aOperation: TWsdlOperation
                                  ; var aDoc: String
                                  ; Items: TXmlList
                                  ; aRootXml: TXml
                                  );
    procedure _evalresponsecode (aXml: TXml; aXsd: TXsd);
    var
      w, h: Integer;
      hXsd: TXsd;
      yXml: TXml;
    begin
      for w := 0 to aXml.Items.Count - 1 do with aXml.Items.XmlItems[w] do
      begin
        if (Name = S_DOLLARREF) then
        begin
          yXml := XsdDescr.FindReferencedXml(Value) as TXml;
          if not Assigned(yXml) then
            raise Exception.CreateFmt('Coud not resolve %s at %s,%s', [Value, aService.Name, aOperation.Name]);
          _evalresponsecode(yXml, aXsd);
        end;
        if Name = 'description' then aXsd.Documentation.Text := Value;
        if Name = 'content' then _evaluateContent (aService, aOperation, aXsd, thisXml.Items, aRootXml);
        if Name = 'headers' then
        begin
          for h := 0 to Items.Count - 1 do with Items.XmlItems[h] do
          begin
            hXsd := TXsd.Create(XsdDescr);
            XsdDescr.Garbage.AddObject('', hXsd);
            hXsd.ElementName := Name;
            hXsd.sType := XsdDescr.AddTypeDefFromJsonXml(aFileName, '', thisXml, aOnError);
            hXsd.sType.Name := hXsd.ElementName;
            hXsd.minOccurs := '0';
            hXsd.ParametersType := oppHeader;
            aXsd.sType.ElementDefs.AddObject(hXsd.ElementName, hXsd);
          end;
        end;
        if Name = 'links' then ; // TODO, not yet known, by me, what to...
      end;

    end;

  var
    x, y, z, v, w, h: Integer;
    xXml, yXml, zXml, vXml, wXml, hXml: TXml;
    xXsd, yXsd, hXsd: TXsd;
  begin
    if (Items.Count = 1) then
    with Items.XmlItems[0] do
    begin
      if (Name = S_DOLLARREF) then
      begin
        yXml := XsdDescr.FindReferencedXml(Value) as TXml;
        if not Assigned(yXml) then
          raise Exception.CreateFmt('Coud not resolve %s at %s,%s', [Value, aService.Name, aOperation.Name]);
        _evaluateResponses300(aService, aOperation, aDoc, yXml.Items, aRootXml);
        Exit;
      end;
    end;
    aOperation.rpyXsd.sType.ContentModel := 'Choice';
    for v := 0 to Items.Count - 1 do
    begin
      vXml := Items.XmlItems[v];
      xXsd := _initXsd(aOperation.rpyXsd, 'rspns' + vXml.Name);
      _evalresponsecode(vXml, xXsd);
    end;
    _addUndefXsd(aOperation.rpyXsd);
  end;

  procedure _evaluateOperation ( aService: TWsdlService
                               ; aOperation: TWsdlOperation
                               ; var aDoc: String
                               ; aItems: TXmlList
                               ; aRootXml: TXml
                               ; aServiceParamsXml: TXml
                               );
  var
    u, v, w, h: Integer;
    vXml, wXml, hXml: TXml;
    xXsd, yXsd, hXsd: TXsd;
  begin
    if Assigned (aServiceParamsXml) then with aServiceParamsXml do
      for v := 0 to Items.Count - 1 do
        _evaluateParameter (aRootXml, Items.XmlItems[v], aOperation.reqXsd);
    for u := 0 to aItems.Count - 1 do with aitems.XmlItems[u] do
    begin
      if Name = 'tags' then ;
      if Name = 'summary' then _appendInfo(aDoc, Value);
      if Name = 'description' then _appendInfo(aDoc, Value);
      if Name = 'externalDocs' then ;
      if Name = 'operationId' then with aOperation do
      begin
        Alias := Value;
        if Assigned (reqBind) then reqBind.Name := Alias;
        if Assigned (rpyBind) then rpyBind.Name := Alias;
      end;
      if Name = 'consumes' then // swagger 2.0
      begin
        aOperation.Consumes := '';
        for v := 0 to Items.Count - 1 do with Items.XmlItems[v] do
          _appendInfo(aOperation.Consumes, Value);
      end;
      if Name = 'produces' then // swagger 2.0
      begin
        aOperation.produces := '';
        for v := 0 to Items.Count - 1 do with Items.XmlItems[v] do
          _appendInfo(aOperation.produces, Value);
      end;
      if Name = 'parameters' then
        for v := 0 to Items.Count - 1 do
          _evaluateParameter (aRootXml, Items.XmlItems[v], aOperation.reqXsd);
      if Name = 'requestBody' then
        _evaluateRequestBody(aService, aOperation, aDoc, Items, aRootXml);
      if Name = 'responses' then
        if (OpenApiVersion <> '')
        and (OpenApiVersion[1] = '3') then
          _evaluateResponses300 (aService, aOperation, aDoc, Items, aRootXml)
        else
          _evaluateResponses200 (aService, aOperation, aDoc, Items, aRootXml);
      if Name = 'schemes' then // swagger 2.0
      begin
        aOperation.schemes := '';
        for v := 0 to Items.Count - 1 do with Items.XmlItems[v] do
          _appendInfo(aOperation.schemes, Value);
      end;
      if Name = 'deprecated' then
      begin
        aOperation.isDepricated := (Value = 'true');
      end;
      if Name = 'security' then ; // swagger 2.0
    end;
  end;

  procedure _ReadDefinitions (sl: TJBStringList);
  var
    x, y: Integer;
    cXml, dXml: TXml;
  begin
    for x := 0 to XsdDescr.ReadFileNames.Count - 1 do
    begin
      with XsdDescr.ReadFileNames.Objects[x] as TXml do
      begin
        cXml := ItemByTag['components'];
        if Assigned (cXml) then
        with cXml do
        begin
          sl.Add (cXml.Name);
          dXml := ItemByTag['schemas'];
          if Assigned (dXml) then
          begin
            sl.Add (dXml.Name);
            for y := 0 to dXml.Items.Count - 1 do
              XsdDescr.AddTypeDefFromJsonXml ( XsdDescr.ReadFileNames [x]
                                             , XsdDescr.ReadFileNames [x] + '#/components/schemas'
                                             , dXml.Items.XmlItems[y], aOnError
                                             );
          end;
        end;

        dXml := ItemByTag['definitions'];
        if Assigned (dXml) then
        begin
          sl.Add (dXml.Name);
          for y := 0 to dXml.Items.Count - 1 do
            XsdDescr.AddTypeDefFromJsonXml ( XsdDescr.ReadFileNames [x]
                                           , XsdDescr.ReadFileNames [x] + '#/definitions'
                                           , dXml.Items.XmlItems[y], aOnError
                                           );
        end;
      end;
    end;


  end;

  procedure _ReadDollarReferencedFiles (aFileName: String; aXml: TXml);
    procedure __ReadDollarReferencedFile (aaFileName: String);
    var
      f: Integer;
      xXml: TXml;
      xExt: String;
    begin
      if not XsdDescr.ReadFileNames.Find(aaFileName, f)
      then begin
        xExt := UpperCase (ExtractFileExt (aaFileName));
        xXml := TXml.Create;
        if (xExt = '.JSON')
        or (xExt = '.JSN')
        or (xExt = '') then
          xXml.LoadJsonFromFile(aaFileName, aOnError, aOnbeforeRead)
        else
          xXml.LoadYamlFromFile(aaFileName, aOnError, aOnbeforeRead);
        xXml.Name := '#'; // to make it easier to resolve $refs
        XsdDescr.ReadFileNames.AddObject(aaFileName, xXml);
        _ReadDollarReferencedFiles(aaFileName, xXml);
      end;
    end;
  var
    x: Integer;
    xFileName, xPath: String;
  begin
    if (aXml.Name = S_DOLLARREF) then
    begin
      if (Length(aXml.Value) > 0)
      and (aXml.Value[1] <> '#')
      then begin
        xFileName := xmlio.ExpandRelativeFileName(aFileName, SeparatedStringN(nil, aXml.Value, '#/', 1));
        xPath := SeparatedStringN(nil, aXml.Value, '#/', 2);
        __ReadDollarReferencedFile(xFileName);
        aXml.Value := xFileName;
        if xPath <> '' then
          aXml.Value := aXml.Value + '#/' + xPath;
      end
      else
        aXml.Value := aFileName + aXml.Value;
    end;
    for x := 0 to aXml.Items.Count - 1 do
      _ReadDollarReferencedFiles(aFileName, aXml.Items.XmlItems[x]);
  end;

var
  xRootXml, cXml, dXml, eXml, vXml, wXml, rXml, hXml: TXml;
  x, y, z, u, v, w, r, f, h, p: Integer;
  zXmlProcessed: Boolean;
  xService: TWsdlService;
  zOperation: TWsdlOperation;
  xServiceParamsXml: TXml;
  xDoc, xExt: String;
  sl: TJBStringList;
  xXsd, yXsd, hXsd: TXsd;
  s, xHost, xBasePath: String;
  sIdUri: TIdURI;
begin
  FileName := aFileName;
  try
    with TIdURI.Create(resolveAliasses(aFileName)) do
    try
      self.Schemes := Protocol;
      if (UpperCase(Protocol) = 'APIARY')
      or (UpperCase(Protocol) = 'APIUI') then
        self.Schemes := 'https';
      self.Host := Host;
      if Port <> '' then
        self.Host := self.Host + ':' + Port;
    finally
      Free;
    end;
  except
  end;
  Name := aFileName;
  xExt := UpperCase (ExtractFileExt (aFileName));
  xRootXml := TXml.Create;
  sl := TJBStringList.Create;
  sl.Sorted := True;
  sl.Duplicates := dupIgnore;
  XsdDescr.Clear;
  XsdDescr.AddBuiltIns;
  OpenApiVersion := '2.0';
  with xRootXml do
  try
    if (xExt = '.YAML')
    or (xExt = '.YML')
    or (AnsiStartsText('APIARY://', aFileName))
    then
      LoadYamlFromFile(aFileName, aOnError, aOnbeforeRead)
    else
      LoadJsonFromFile(aFileName, aOnError, aOnbeforeRead);
    xRootXml.Name := '#';
    XsdDescr.ReadFileNames.AddObject(aFileName, xRootXml);
    _ReadDollarReferencedFiles (aFileName, xRootXml);
    _ReadDefinitions (sl);
    for x := 0 to Items.Count - 1 do
    begin
      if Items.XmlItems[x].Name = 'swagger' then
      begin
        sl.Add (Items.XmlItems[x].Name);
        isOpenApiService := True;
        if Items.XmlItems[x].Value <> '2.0' then
          SjowMessage (Format('warning (%s): unexpected version number %s (2.0 expected)', [aFileName, Items.XmlItems[x].Value]));
        OpenApiVersion := Items.XmlItems[x].Value;
        // simulate openApi v3 servers
        xHost := Items.XmlValueByTag['host'];
        xBasePath := _trimPath(Items.XmlValueByTag['basePath']);
        dXml := Items.XmlItemByTag['schemes'];
        if Assigned (dXml) then
        begin
          for p := 0 to dXml.Items.Count - 1 do
          begin
            Servers.Add (dXml.Items.XmlItems[p].Value + '://' + xHost + xBasePath);
            ServerPathNames.Add (xBasePath);
          end;
        end
        else
        begin
          Servers.Add ('http://' + xHost + xBasePath);
          Servers.Add ('https://' + xHost + xBasePath);
          ServerPathNames.Add (xBasePath);
        end;
      end;
      if Items.XmlItems[x].Name = 'openapi' then with Items.XmlItems[x] do
      begin
        sl.Add (Name);
        isOpenApiService := True;
        if Copy (Value, 1, 1) <> '3' then
          SjowMessage (Format('warning (%s): unexpected version number %s (3.x.x expected)', [aFileName, Value]));
        OpenApiVersion := Value;
      end;
      if Items.XmlItems[x].Name = 'info' then with Items.XmlItems[x] do
      begin
        sl.Add (Name);
        for y := 0 to Items.Count -1 do with Items.XmlItems[y] do
        begin
          _appendInfo (Description, Name + ': ' + Value);
        end;
        self.Name := Items.XmlValueByTag['title'];
      end;
      if Items.XmlItems[x].Name = 'servers' then
      begin
        sl.Add (Items.XmlItems[x].Name);
        sIdUri := TIdURI.Create;
        with sIdUri do
        try
          with Items.XmlItems[x] do
          begin
            for y := 0 to Items.Count - 1 do with Items.XmlItems[y] do
            begin
              try
                URI := Items.XmlValueByTag['url'];
                if Document <> '' then
                  ServerPathNames.Add (Path + Document)
                else
                  ServerPathNames.Add ('');
                Servers.Add (URI);
              except
              end;
            end;
          end;
        finally
          Free;
        end;
      end;
      if (Items.XmlItems[x].Name = 'host') // swagger 2.0
      or (Items.XmlItems[x].Name = 'basePath') // swagger 2.0
      then begin
        sl.Add (Items.XmlItems[x].Name);
      end;
      if Items.XmlItems[x].Name = 'schemes' then with Items.XmlItems[x] do
      begin
        sl.Add (Name);
        Schemes := '';
        for y := 0 to Items.Count - 1 do with Items.XmlItems[y] do
          _appendInfo(Schemes, Value);
      end;
      if Items.XmlItems[x].Name = 'consumes' then with Items.XmlItems[x] do
      begin
        sl.Add (Name);
        Consumes := '';
        for y := 0 to Items.Count - 1 do with Items.XmlItems[y] do
          _appendInfo(Consumes, Value);
      end;
      if Items.XmlItems[x].Name = 'produces' then with Items.XmlItems[x] do
      begin
        sl.Add (Name);
        Produces := '';
        for y := 0 to Items.Count - 1 do with Items.XmlItems[y] do
          _appendInfo(Produces, Value);
      end;
      if Items.XmlItems[x].Name = 'securityDefinitions' then with Items.XmlItems[x] do
      begin
        sl.Add (Name);
        SjowMessage ('to be implemented: ' + Name + ': ' + Value);
      end;
      if Items.XmlItems[x].Name = 'security' then with Items.XmlItems[x] do
      begin
        sl.Add (Name);
        SjowMessage ('to be implemented: ' + Name + ': ' + Value);
      end;
      if Items.XmlItems[x].Name = 'tags' then with Items.XmlItems[x] do
      begin
        sl.Add (Name);
        SjowMessage ('to be implemented: ' + Name + ': ' + Value);
      end;
    end;

{
    cXml := ItemByTag['components'];
    if Assigned (cXml) then
    with cXml do
    begin
      sl.Add (cXml.Name);
      dXml := ItemByTag['schemas'];
      if Assigned (dXml) then
      begin
        sl.Add (dXml.Name);
        for y := 0 to dXml.Items.Count - 1 do
          XsdDescr.AddTypeDefFromJsonXml(aFileName, aFileName + '/components/schemas', dXml.Items.XmlItems[y], aOnError);
      end;
    end;

    dXml := ItemByTag['definitions'];
    if Assigned (dXml) then
    begin
      sl.Add (dXml.Name);
      for y := 0 to dXml.Items.Count - 1 do
        XsdDescr.AddTypeDefFromJsonXml(aFileName, aFileName + '/definitions', dXml.Items.XmlItems[y], aOnError);
    end;
}
    dXml := ItemByTag['paths'];
    if Assigned (dXml) then with dXml do
    begin
      sl.Add (Name);
      for y := 0 to Items.Count - 1 do with Items.XmlItems[y] do
      begin
        xService := TWsdlService.Create;
        xServiceParamsXml := Items.XmlItemByTag['parameters'];
        xService.Name := Name;
        xService.openApiPath := xService.Name;
        for p := 0 to ServerPathNames.Count - 1 do
          xService.PathInfos.Add (ServerPathNames.Strings[p] + xService.Name);
        if ServerPathNames.Count > 0 then
          xService.logPathFormat := ServerPathNames.Strings[0] + Name
        else
          xService.logPathFormat := Name;
        Services.AddObject(Name, xService);
        xService.DescriptionType := ipmDTJson;
        for z := 0 to Items.Count - 1 do with Items.XmlItems[z] do
        begin
          zXmlProcessed := False;
          if (Name = 'parameters') then
            zXmlProcessed := True;
          if (Name = 'servers')
          then
          begin
            zXmlProcessed := True;
            SjowMessage(Format('%s found at path level %s.%s (not yet implemented)', [Name, aFileName, xService.Name]));
          end;
          if (Name = 'summary') then
          begin
            zXmlProcessed := True;
            // no wsdlStub doc at this service level...
          end;
          if (Name = 'description') then
          begin
            zXmlProcessed := True;
            // no wsdlStub doc at this service level...
          end;
          if not zXmlProcessed then
          begin  // get, post, put, delete, ...
            zOperation := TWsdlOperation.Create (Self);
            zOperation.Wsdl := self;
            zOperation.WsdlService := xService;
            zOperation.Name := xService.Name + ':' + Name;
            xService.Operations.AddObject(zOperation.Name, zOperation);
            zOperation.reqTagName := zOperation.Name;
            zOperation.reqBind.Name := zOperation.reqTagName;
            zOperation.rpyTagName := zOperation.Name;
            zOperation.rpyBind.Name := zOperation.rpyTagName;
            zOperation.Alias := zOperation.reqTagName;
            zOperation.httpVerb := UpperCase(Name);
            zOperation.Schemes := Schemes;
            zOperation.Consumes := Consumes;
            zOperation.Produces := Produces;
            zOperation.ContentType := 'application/json;charset=utf-8';
            zOperation.Accept := 'application/json';
            xDoc := '';
            _evaluateOperation (xService, zOperation, xDoc, Items, xRootXml, xServiceParamsXml);
            with zOperation do
            begin
              if not isValidId (Alias) then
              begin
                Alias := makeValidId (Alias);
                reqBind.Name := Alias;
                rpyBind.Name := Alias;
              end;
              Documentation.Text := xDoc;
              s := reqBind.Name;
              reqBind.Free;
              reqBind := TXml.Create(0, reqXsd);
              reqBind.Name := s;
              s := rpyBind.Name;
              rpyBind.Free;
              rpyBind := TXml.Create(0, rpyXsd);
              rpyBind.Name := s;
            end;
          end;
        end;
      end;
    end;
    ValidateEvaluatedTags (xRootXml, sl);
    sl.Clear;
    _resolveDollarRefs;
    with XsdDescr.TypeDefs do
    begin
      for x := 0 to Count - 1 do
      begin
        if XsdDataTypes[x].dollarRef <> '' then
        begin
          if Find (XsdDataTypes[x].dollarRef, f) then
          begin
            if x = f then
              SjowMessage ('dollarreffed self: ' + Strings[x]);
            Objects[x] := Objects[f];
          end;
        end;
      end;
    end;
    for x := 0 to Services.Count - 1 do with Services.Services[x] do
    begin
      for y := 0 to Operations.Count - 1 do with Operations.Operations[y] do
      begin
        s := reqBind.Name;
        reqBind.Free;
        reqBind := TXml.Create(0, reqXsd);
        reqBind.Name := s;
        s := rpyBind.Name;
        rpyBind.Free;
        rpyBind := TXml.Create(0, rpyXsd);
        rpyBind.Name := s;
      end;
    end;
  finally
//  Free; // done by xsddescr.clear via readfilenamea
    FreeAndNil(sl);
  end;
end;


function TWsdl.ExtraXsdsAsXml: TXml;
var
  x: Integer;
begin
  result := TXml.CreateAsString ('FileNames', '');
  for x := 0 to ExtraXsds.Count - 1 do
  begin
    result.AddXml ( TXml.CreateAsString ( 'FileName'
                                        , uncFilename(ExtraXsds.Strings[x]
                                                            )
                                        )
                  );
  end;
end;

procedure TWsdl.ExtraXsdsFromXml(aXml: TXml; SaveRelativeFileNames: Boolean; aMainFileName: String);
var
  x: Integer;
begin
  ExtraXsds.Clear;
  for x := 0 to aXml.Items.Count - 1 do
    if aXml.Items.XmlItems[x].Checked then
    {$ifndef NoGUI}
      ExtraXsds.Add (CheckAndPromptForExistingFile(aXml.FullIndexCaption, aMainFileName, aXml.Items.XmlItems[x].Value));
    {$else}
      ExtraXsds.Add (aXml.Items.XmlItems[x].Value);
    {$endif}
end;

procedure TWsdl .AddedTypeDefElementsFromXml (aXml : TXml );
var
  x, y, o: Integer;
  cXml, rXml: TXml;
  s, xAlias, xReqRpy, xCaption, xTypeName, xTypeNameSpace, xElementName: String;
  xOperation: TWsdlOperation;
  xType: TXsdDataType;
begin
  if not Assigned (aXml) then Exit;
  if aXml.Name <> 'AddedTypeDefElements' then Exit;
  for x := 0 to aXml.Items.Count - 1 do with aXml.Items.XmlItems[x] do
  begin
    if Name = 'AddedTypeDefElement' then
    begin
      s := Items.XmlValueByTag['UsedAt'];
      xAlias := Copy (s, 1, Pos ('.', s) - 1);
      s := Copy (s, Pos ('.', s) + 1, MaxInt);
      xReqRpy := Copy (s, 1, Pos ('.', s) - 1);
      xCaption := Copy (s, Pos ('.', s) + 1, MaxInt);
      xOperation := nil;
      for y := 0 to Services.Count -1 do with Services.Services[y] do
        for o := 0 to Operations.Count - 1  do with Operations do
          if Operations[o].alias = xAlias then
            xOperation := Operations[o];
      if Assigned(xOperation) then
      begin
        if xReqRpy = 'Req' then
          rXml := xOperation.reqBind.FindUQ(xCaption) as TXml
        else
          rXml := xOperation.rpyBind.FindUQ(xCaption) as TXml;
        if Assigned (rXml) then
        begin
          for y := 0 to Items.Count - 1 do with Items.XmlItems[y] do
          begin
            if Name = 'Added' then
            begin
              xTypeNameSpace := Items.XmlValueByTag['NameSpace'];
              xTypeName := Items.XmlValueByTag['Name'];
              xElementName := Items.XmlValueByTag['ElementName'];
              xType := XsdDescr.FindTypeDef(xTypeNameSpace, xTypeName);
              if Assigned (xType) then
              begin
                rXml.Xsd.AddElementDef ( XsdDescr
                                       , xElementName
                                       , xType
                                       );
                rXml.Xsd.sType.ManuallyUsedAtPath := xOperation.Alias
                                                   + '.'
                                                   + xReqRpy
                                                   + '.'
                                                   + xCaption
                                                   ;

              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TWsdl.LoadExtraXsds (aOnbeforeRead: TProcedureS);
var
  x, f: Integer;
begin
  for x := 0 to ExtraXsds.Count - 1 do
    if not XsdDescr.ReadFileNames.Find (ExtraXsds.Strings[x], f) then
      XsdDescr.AddXsdFromFile ('', ExtraXsds.Strings[x], _OnParseErrorEvent, aOnbeforeRead);
  if ExtraXsds.Count > 0 then
    XsdDescr.Finalise; // again
end;

{ TWsdlServices }

procedure TWsdlServices.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Services [x].Free;
  inherited;
end;

function TWsdlServices.GetService(Index: integer): TWsdlService;
begin
  result := TWsdlService (Objects [index]);
end;

{ TWsdlService }

constructor TWsdlService.Create;
begin
  UseNameSpacePrefixes := True;
  PathInfos := TJBStringList.Create;
  Operations := TWsdlOperations.Create;
  Operations.Sorted := True;
end;

destructor TWsdlService.Destroy;
begin
  Operations.Clear;
  FreeAndNil(PathInfos);
  FreeAndNil (Operations);
  inherited;
end;

function TWsdlService.getOperationByName(Index: String): TWsdlOperation;
var
  x: Integer;
begin
  result := nil;
  for x := 0 to Operations.Count - 1 do
  begin
    if (Operations.Operations[x].Name = Index) then
    begin
      result := Operations.Operations[x];
      Exit;
    end;
  end;
end;

function TWsdlService.thisService: TWsdlService;
begin
  result := self;
end;

procedure TWsdlService.OptionsFromXml(aXml: TXml);
var
  xXml, yXml, zXml: TXml;
begin
  if not Assigned (aXml) then raise Exception.Create('serviceOptionsFromXml: No XML assigned');
  if not (aXml.Name = 'serviceOptions') then raise Exception.Create('serviceOptionsFromXml: Illegal XML: ' + aXml.Text);
  SuppressXmlComment := False;
  SuppressHTTP500 := False;
  AuthenticationType := atNone;
  PasswordType := pwText;
  xXml := aXml.Items.XmlCheckedItemByTag['general'];
  if Assigned (xXml) then
  begin
    SuppressXmlComment := xXml.Items.XmlCheckedBooleanByTagDef['suppressXmlComment', False];
    SuppressHTTP500 := xXml.Items.XmlCheckedBooleanByTagDef['suppressHttp500onSoapFaults', False];
  end;
  xXml := aXml.Items.XmlCheckedItemByTag['Authentication'];
  if Assigned (xXml) then
  begin
    yXml := xXml.Items.XmlCheckedItemByTag['Basic'];
    if Assigned (yXml) then
    begin
      AuthenticationType := atHTTPBasicAuthentication;
      UserName := yXml.Items.XmlCheckedValueByTagDef['userName',''];
      try Password :=  Xmlz.DecryptString(yXml.Items.XmlCheckedValueByTagDef['Password','']); except end
    end;
    yXml := xXml.Items.XmlCheckedItemByTag['wsSecurity'];
    if Assigned (yXml) then
    begin
      AuthenticationType := atWsSecurity;
      UserName := yXml.Items.XmlCheckedValueByTagDef['userName',''];
      try Password :=  Xmlz.DecryptString(yXml.Items.XmlCheckedValueByTagDef['Password','']); except end;
      zXml := yXml.Items.XmlCheckedItemByTag['pwdType'];
      if Assigned (zXml) then
        if zXml.Value = 'Digest' then
          PasswordType := pwDigest;
    end;
  end;
end;

function TWsdlService.OptionsAsXml: TXml;
begin
  result := TXml.CreateAsString ('serviceOptions','');
  with result do
  begin
    with AddXml (TXml.CreateAsString('general', '')) do
    begin
      AddXml (TXml.CreateAsBoolean('suppressXmlComment', SuppressXmlComment));
      AddXml (TXml.CreateAsBoolean('suppressHttp500onSoapFaults', SuppressHTTP500));
    end;
    case AuthenticationType of
      atNone: ;
      atHTTPBasicAuthentication: with AddXml (TXml.CreateAsString('Authentication', '')) do
      begin
        with AddXml (TXml.CreateAsString('Basic', '')) do
        begin
          AddXml (TXml.CreateAsString('userName', UserName));
          AddXml (TXml.CreateAsString('Password', Xmlz.EncryptString(Password)));
        end;
      end;
      atWsSecurity: with AddXml (TXml.CreateAsString('Authentication', '')) do
      begin
        with AddXml (TXml.CreateAsString('wsSecurity', '')) do
        begin
          AddXml (TXml.CreateAsString('userName', UserName));
          AddXml (TXml.CreateAsString('Password', Xmlz.EncryptString(Password)));
          case PasswordType of
            pwText: AddXml (TXml.CreateAsString('pwdType', 'Text'));
            pwDigest: AddXml (TXml.CreateAsString('pwdType', 'Digest'));
          end;
        end;
      end;
    end;
  end;
end;

function TWsdlService.StreamWsSecurity: String;
var
  xNonce: String;
  xCreated: String;
begin
  result := '';
  xCreated := xsdNowAsDateTime;
  xNonce := NonceAsString (xCreated);
{
  xCreated := '2010-09-23T18:16:40.813Z';
  xNonce := b64decode('hrtgto221GRsecgiGXbKCg==');
  if b64encode (xNonce) <> 'hrtgto221GRsecgiGXbKCg==' then
    raise Exception.Create ('??');
{}
  if AuthenticationType = atWsSecurity then
  begin
    Inc (_WsdlUserNameTokenNumber);
    result := StrAdd (result, '    <wsse:Security soapenv:mustUnderstand="1" xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd">');
    result := StrAdd (result, '      <wsse:UsernameToken wsu:Id="UserNameToken-'
                 + IntToStr (_WsdlUserNameTokenNumber)
                 + '" xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">');
    result := StrAdd (result, '        <wsse:Username>'
                 + UserName
                 + '</wsse:Username>'
                 );
    case PasswordType of
      pwText:
        begin
          result := StrAdd (result, '        <wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText">'
                       + Password
                       + '</wsse:Password>'
                       );
        end;
      pwDigest:
        begin
          result := StrAdd (result, '        <wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordDigest">'
                       + EncodeStringBase64 (sha1Bin (xNonce + xCreated + Password))
                       + '</wsse:Password>'
                       );
        end;
    end;
    result := StrAdd (result, '        <wsse:Nonce EncodingType="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-soap-message-security-1.0#Base64Binary">'
                 + EncodeStringBase64(xNonce)
                 + '</wsse:Nonce>'
                 );
    result := StrAdd (result, '        <wsu:Created>'
                 + xCreated
                 + '</wsu:Created>'
                 );
    result := StrAdd (result, '      </wsse:UsernameToken>');
    result := StrAdd (result, '    </wsse:Security>');
{
    <wsse:Security soapenv:mustUnderstand="1" xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd">
      <wsse:UsernameToken wsu:Id="" xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        <wsse:Username>erik</wsse:Username>
        <wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordDigest">VPzSDGPa0RlZpvoU92txI8bY7+E=</wsse:Password>
        <wsse:Nonce EncodingType="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-soap-message-security-1.0#Base64Binary">a24GwPGbhf73Z4X7yfyETQ==</wsse:Nonce>
        <wsu:Created>2010-09-22T22:00:12.002Z</wsu:Created>
      </wsse:UsernameToken>
    </wsse:Security>
}
  end;
end;

{ TWsdlOperation }

procedure TWsdlOperation.BindScriptFunction(Id: String; Adr: Pointer; Token: Integer;
  ArgumentsPrototype: String);
begin
  fExpress.BindFunction (Id, Adr, Token, ArgumentsPrototype);
end;

procedure TWsdlOperation.AcquireLock;
begin
  if doOperationLock then fLock.Acquire;
end;

function TWsdlOperation.BeforeActivatorDebugString: String;
begin
  result := '';
  DoExit := False;
  LiteralResult := '';
  ReturnSoapFault := False;
  if Assigned (BeforeScriptLines)
  and (BeforeScriptLines.Count > 0) then
    result := fExpress.DebugTokenStringList(BeforeScriptLines, nil);
  DoExit := False;
end;

constructor TWsdlOperation.Create  (aWsdl: TWsdl);
begin
  WsdlOperation := self;
  fCloned := nil;
  fLock := SyncObjs.TCriticalSection.Create;
  doSuppressLog := 0;
  StubAction := saStub;
  if Assigned (aWsdl) then
  begin
    Wsdl := aWsdl;
    InputHeaders := TWsdlHeaders.Create;
    OutputHeaders := TWsdlHeaders.Create;
    reqXsd := TXsd.Create(aWsdl.XsdDescr);
    aWsdl.XsdDescr.Garbage.AddObject ('', reqXsd);
    reqXsd.sType := TXsdDataType.Create(aWsdl.XsdDescr);
    aWsdl.XsdDescr.Garbage.AddObject ('', reqXsd.sType);
    reqXsd.sType.IsComplex := True;
    reqXsd.DoNotEncode := True;
    reqXsd.sType.ContentModel := 'Sequence';
    reqXsd.sType.Name := 'Operation';
    reqXsd.sType.BaseDataTypeName := reqXsd.sType.Name;
    reqBind := TXml.Create;
    (reqBind as TXml).Xsd := reqXsd;
    rpyXsd := TXsd.Create(aWsdl.XsdDescr);
    aWsdl.XsdDescr.Garbage.AddObject ('', rpyXsd);
    rpyXsd.sType := TXsdDataType.Create(aWsdl.XsdDescr);
    aWsdl.XsdDescr.Garbage.AddObject ('', rpyXsd.sType);
    rpyXsd.DoNotEncode := True;
    rpyXsd.sType.IsComplex := True;
    rpyXsd.sType.ContentModel := 'Sequence';
    rpyXsd.sType.Name := 'Operation';
    rpyXsd.sType.BaseDataTypeName := rpyXsd.sType.Name;
    rpyBind := TXml.Create;
    (rpyBind as TXml).Xsd := rpyXsd;
    FaultXsd := TXsd.Create(aWsdl.XsdDescr);
    aWsdl.XsdDescr.Garbage.AddObject ('', FaultXsd);
    FaultXsd.sType := TXsdDataType.Create(aWsdl.XsdDescr);
    aWsdl.XsdDescr.Garbage.AddObject ('', FaultXsd.sType);
    FaultXsd.DoNotEncode := True;
    FaultXsd.sType.IsComplex := True;
    FaultXsd.sType.ContentModel := 'Choice';
    FaultXsd.sType.Name := 'Operation';
    FaultXsd.sType.BaseDataTypeName := FaultXsd.sType.Name;
    fltBind := TXml.Create;
    (fltBind as TXml).Xsd := FaultXsd;
    Messages := TWsdlMessages.Create;
    CorrelationBindables := TBindableList.Create;
    LogColumns := TBindableList.Create;
    BindablesWithAddedElement := TBindableList.Create;
    invokeList := TWsdlOperations.Create;
    invokeList.Sorted := True;
  end;
  Documentation := TJBStringList.Create;
  BeforeScriptLines := TJBStringList.Create;
  AfterScriptLines := TJBStringList.Create;
  ContentType := 'text/xml;charset=utf-8';
  Accept := ContentType;
  StubAction := saStub;
  sslVersion := sslvTLSv1_2;
  StubHttpAddress := '';
  httpVerb := 'POST';
  ConsumeType := ptJson;
  ProduceType := ptJson;
  StubStompPutHost := 'localhost';
  StubStompPutPort := '61613';
  StubStompPutUseCredentials := False;
  StubStompPutUserName := '';
  StubStompPutPassword := '';
  StubStompPutClientId := '';
  StubStompTimeOut := 30;
  if Assigned (_WsdlWsaXsd) then
  begin
    reqWsaXml := TXml.Create(-1000, _WsdlWsaXsd);
    reqWsaXml.CheckDownline(False);
    rpyWsaXml := TXml.Create(-1000, _WsdlWsaXsd);
    rpyWsaXml.CheckDownline(False);
  end;
  if Assigned (_WsdlStompHeaderXsd) then
  begin
    StubStompHeaderXml := TXml.Create(-10000, _WsdlStompHeaderXsd);
    StubStompHeaderXml.CheckDownline(False);
  end;
  StubCustomHeaderXml := TXml.CreateAsString('customHeaders', '');
  doReadReplyFromFile := False;
  onFetchLogFromCloud := '';
  resolveRequestAliasses := True;
  resolveReplyAliasses := True;
  ReadReplyFromFileXml := TXml.CreateAsString('ReadReplyFromFile', '');
end;

destructor TWsdlOperation.Destroy;
  procedure _FreeRecog (sl: TJBStringList);
  var
    x: Integer;
  begin
    if not Assigned (sl) then exit;
    for x := 0 to sl.Count - 1 do
      sl.Objects [x].Free;
    sl.Clear;
    sl.Free;
  end;
var
  x: Integer;
begin
  if not Assigned(fCloned) then
  begin
    if Assigned (FaultMessages) then
    begin
      FaultMessages.ClearListOnly;
      FreeAndNil(FaultMessages);
    end;
    if Assigned (InputHeaders) then
      for x := 0 to InputHeaders.Count - 1 do
        InputHeaders.Headers [x].Free;
    FreeAndNil (InputHeaders);
    if Assigned (OutputHeaders) then
      for x := 0 to OutputHeaders.Count - 1 do
        OutputHeaders.Headers [x].Free;
    FreeAndNil (OutputHeaders);
    if Assigned (Documentation) then Documentation.Clear;
    FreeAndNil (Documentation);
    if Assigned (BeforeScriptLines) then BeforeScriptLines.Clear;
    FreeAndNil(BeforeScriptLines);
    if Assigned (AfterScriptLines) then AfterScriptLines.Clear;
    FreeAndNil(AfterScriptLines);
    if Assigned (Messages) then Messages.Clear;
    FreeAndNil(Messages);
    _FreeRecog (reqRecognition);
    _FreeRecog (rpyRecognition);
    FreeAndNil (StubStompHeaderXml);
    FreeAndNil (StubCustomHeaderXml);
    FreeAndNil (ReadReplyFromFileXml);
    FreeAndNil (fLock);
  end;
  if True then
  begin
    if Assigned (invokeList) then
      invokeList.Clear;
    FreeAndNil (invokeList);
    FreeAndNil (fExpress);
    FreeAndNil (fExpressStamper);
    FreeAndNil (fExpressChecker);
    FreeAndNil (CorrelationBindables);
    FreeAndNil (LogColumns);
    FreeAndNil (BindablesWithAddedElement);
    FreeAndNil (freqBind);
    FreeAndNil (frpyBind);
    FreeAndNil (fltBind);
    FreeAndNil (requestInfoBind);
    FreeAndNil (replyInfoBind);
    FreeAndNil (reqWsaXml);
    FreeAndNil (rpyWsaXml);
  end;
  inherited;
end;

procedure TWsdlOperation.ExecuteBefore;
begin
  DoExit := False;
  LiteralResult := '';
  ReturnSoapFault := False;
  if not PreparedBefore then
    raise Exception.Create('Operation (Before)"' + Name + '" not prepared');
  Execute(BeforeScriptLines, nil);
  DoExit := False;
  if Assigned(CorrelatedMessage)
  and not ReturnSoapFault then
    Execute(CorrelatedMessage.BeforeScriptLines, nil);
end;

procedure TWsdlOperation.ExecuteAfter;
begin
  DoExit := False;
  LiteralResult := '';
  ReturnSoapFault := False;
  if not PreparedAfter then
    raise Exception.Create('Operation (After)"' + Name + '" not prepared');
  if Assigned (CorrelatedMessage) then
    Execute(CorrelatedMessage.AfterScriptLines, nil);
  DoExit := False;
  Execute(AfterScriptLines, nil);
end;

function TWsdlOperation.getReplyBasedOnRequest: TWsdlMessage;
  function _Match (aCorrelationBindables, aBindables: TBindableList): Boolean;
  var
    x: Integer;
    rx: TRegExpr;
  begin
    result := True;
    x := 0;
    rx := TRegExpr.Create;
    try
      while Result
      and (x < aCorrelationBindables.Count)
      and (x < aBindables.Count) do
      begin
        rx.Expression := '^(' + aCorrelationBindables.Bindables[x].CorrelationValue + ')$';  // bol and eol: must match entire string
        result := Assigned (aBindables.Bindables[x]);
        if result then
          result := rx.Exec(aBindables.Bindables[x].GetStringData);
        Inc (x);
      end;
    finally
      rx.Free;
    end;
  end;
var
  x: Integer;
  xDefault: TWsdlMessage;
begin
  try
    if (CorrelationBindables.Count = 0)
    and (Messages.Count > 0) then
    begin
      result := Messages.Messages [0];
      exit;
    end;
    result := nil;
    xDefault := nil;
    try
      x := 0;
      while (x < Messages.Count)
      and (not Assigned (result)) do
      begin
        if (Messages.Messages[x].CorrelationBindables.Bindables[0].CorrelationValue = '.*')
        and (not Assigned (xDefault)) then
          xDefault := Messages.Messages [x]
        else
        begin
          if _Match (Messages.Messages[x].CorrelationBindables, CorrelationBindables) then
          begin
            result := Messages.Messages [x];
          end;
        end;
        Inc (x);
      end;
      if not Assigned (Result) then
        Result := xDefault;
    except
      result := xDefault;
    end;
  finally
    CorrelatedMessage := Result;
  end;
end;

procedure TWsdlOperation.NeedBeforeData(Sender: TObject; var MoreData: Boolean;
  var Data: String);
begin
  if fLineNumber = BeforeScriptLines.Count then
    MoreData := False
  else
  begin
    Data := BeforeScriptLines.Strings [fLineNumber];
    Inc (fLineNumber);
  end;
end;

procedure TWsdlOperation.NeedAfterData(Sender: TObject; var MoreData: Boolean;
  var Data: String);
begin
  if fLineNumber = AfterScriptLines.Count then
    MoreData := False
  else
  begin
    Data := AfterScriptLines.Strings [fLineNumber];
    Inc (fLineNumber);
  end;
end;

procedure TWsdlOperation.Bind (aRoot: String; aBind: TCustomBindable; aExpress: TExpress);
begin
  if not Assigned(aExpress)
  or not Assigned(aBind)
  or (aBind.Name = '') then
    Exit;
  aBind.Bind (aRoot, aExpress, 1)
end;

procedure TWsdlOperation.PrepareScripting;
var
  x: Integer;
begin
  try
    FreeAndNil(fExpress);
    fExpress := TExpress.Create (self);
    fExpress.Context := Self;
    fExpress.OnGetAbortPressed := fOnGetAbortPressed;
    fExpress.OnGetDoExit := getDoExit;
    fExpress.OnError := fOnError;
    fExpress.Database := _WsdlDbsConnector;
    Bind ('Req', reqBind, fExpress);
    Bind ('Rpy', rpyBind, fExpress);
    Bind ('requestInfo', requestInfoBind, fExpress);
    Bind ('replyInfo', replyInfoBind, fExpress);
    if Assigned (invokeList) then
    begin
      for x := 0 to invokeList.Count - 1 do
      begin
        if Assigned (invokeList.Operations[x]) then
        begin
          Bind ('Req', invokeList.Operations[x].reqBind, fExpress);
          Bind ('Rpy', invokeList.Operations[x].rpyBind, fExpress);
          Bind ('requestInfo', invokeList.Operations[x].requestInfoBind, fExpress);
          Bind ('replyInfo', invokeList.Operations[x].replyInfoBind, fExpress);
        end;
      end;
    end;
    if fltBind is TIpmItem then
      fltBind.Bind ('Flt', fExpress, 1);
    if fltBind is TXml then
    begin
      for x := 0 to (fltBind as TXml).Items.Count - 1 do
      begin
        (fltBind as TXml).Items.XmlItems [x].Parent := nil;
        try
          (fltBind as TXml).Items.XmlItems [x].Bind('Faults', fExpress, 1);
        finally
          (fltBind as TXml).Items.XmlItems [x].Parent := fltBind;
        end;
      end;
    end;
    if Assigned (reqWsaXml) then
      try reqWsaXml.Bind ('reqWsa', fExpress, 1); except end;
    if Assigned (rpyWsaXml) then
      try rpyWsaXml.Bind ('rpyWsa', fExpress, 1); except end;
    try fExpress.BindInteger('rti.operation.delayms', DelayTimeMs); except end;
    try fExpress.BindInteger('rti.operation.suppresslog', doSuppressLog); except end;
    BindScriptFunction ('AccordingSchema', @isAccordingSchema, XFG, '(aItem)');
    BindScriptFunction ('AddRemark', @AddRemark, VFOS, '(aString)');
    BindScriptFunction ('Assigned', @isAssigned, XFG, '(aItem)');
    BindScriptFunction ('ClearLogs', @ClearLogs, VFOV, '()');
    BindScriptFunction ('ClearSnapshots', @ClearSnapshots, VFOV, '()');
    BindScriptFunction ('AssignAnyType', @assignAnyType, VFGG, '(aDestGroup, aSrcGroup)');
    BindScriptFunction ('AsXmlString', @asXmlString, SFOG, '(aId)');
    BindScriptFunction ('CreateJUnitReport', @CreateJUnitReport, VFOS, '(aName)');
    BindScriptFunction ('CreateSnapshot', @CreateSnapshot, VFOS, '(aName)');
    BindScriptFunction ('CreateSummaryReport', @CreateSummaryReport, VFOS, '(aName)');
    BindScriptFunction ('DateTimeToJulianStr', @DateTimeToJulianStr, SFD, '(aDateTime)');
    BindScriptFunction ('DateTimeToTandemJulianStr', @DateTimeToTandemJulianStr, SFD, '(aDateTime)');
    BindScriptFunction ('DateTimeToXml', @xsdDateTime, SFD, '(aDateTime)');
    BindScriptFunction ('DateTimeToUnix', @xDateTimeToUnix, XFD, '(aDateTime)');
    BindScriptFunction ('dbLookUp', @dbLookUp, SFSSSS, '(aTable, aValueColumn, aReferenceColumn, aReferenceValue)');
    BindScriptFunction ('DecEnvNumber', @decVarNumber, XFOS, '(aKey)');
    BindScriptFunction ('ExecuteScript', @ExecuteScript, VFOS, '(aScript)');
    BindScriptFunction ('ExecuteScriptLater', @ExecuteScriptLater, VFOSX, '(aScript, aLaterMs)');
    BindScriptFunction ('ExecSql', @wsdlExecSql, VFOS, '(aQuery)');
    BindScriptFunction ('Exit', @RaiseExit, VFOV, '()');
    BindScriptFunction ('FetchDefaultDesignMessage', @wsdlFetchDefaultDesignMessage, VFOS, '(aOperation)');
    BindScriptFunction ('FormatDate', @FormatDateX, SFDS, '(aDate, aMask)');
    BindScriptFunction ('GetContext', @GetContext, SFV, '()');
    BindScriptFunction ('GetEnvNumber', @getVarNumber, XFOS, '(aKey)');
    BindScriptFunction ('GetEnvNumberDef', @getVarNumberDef, XFOSX, '(aKey, aDefault)');
    BindScriptFunction ('GetEnvVar', @getVar, SFOS, '(aKey)');
    BindScriptFunction ('GetEnvVarDef', @getVarDef, SFOSS, '(aKey, aDefault)');
    BindScriptFunction ('GetEnvVarDefT', @getVarDefT, SFOSSSX, '(aKey, aDefault, aSeparator, aIndex)');
    BindScriptFunction ('DisableMessage', @DisableMessage, VFOV, '()');
    BindScriptFunction ('HostName', @GetHostName, SFV, '()');
    BindScriptFunction ('ifthen', @ifThenString, SFBSS, '(aCondition, aTrueString, aFalseString)');
    BindScriptFunction ('IncEnvNumber', @incVarNumber, XFOS, '(aKey)');
    BindScriptFunction ('Latin1Str', @Latin1, SFS, '(aString)');
    BindScriptFunction ('LogsFromRemoteServer', @LogsFromRemoteServer, VFOV, '()');
    BindScriptFunction ('NameCaseStr', @StrToNameCase, SFS, '(aString)');
    BindScriptFunction ('LengthStr', @LengthX, XFS, '(aString)');
    BindScriptFunction ('LowercaseStr', @LowerCaseStr, SFS, '(aString)');
    BindScriptFunction ('MatchingEnvVar', @EnvVarMatchList, SLFOS, '(aRegExpr)');
    BindScriptFunction ('MD5', @MD5, SFS, '(aString)');
    BindScriptFunction ('MessageName', @wsdlMessageName, SFOV, '()');
    BindScriptFunction ('MessageOfOperation', @OperationMessageList, SLFOS, '(aOperation)');
    BindScriptFunction ('MessagingProtocol', @wsdlMessagingProtocol, SFOV, '()');
    BindScriptFunction ('NewDesignMessage', @wsdlNewDesignMessage, VFOSS, '(aOperation, aName)');
    BindScriptFunction ('NewLine', @xNewLine, SFV, '()');
    BindScriptFunction ('Tab', @xTab, SFV, '()');
    BindScriptFunction ('NumberToStr', @FloatToStr, SFX, '(aNumber)');
    BindScriptFunction ('NowAsStr', @xsdNowAsDateTime, SFV, '()');
    BindScriptFunction ('Occurrences', @OccurrencesX, XFG, '(aElement)');
    BindScriptFunction ('PopulateFromXmlString', @populateFromXmlString, VFOGS, '(aId, aString)');
    BindScriptFunction ('PromptReply', @PromptReply, VFOV, '()');
    BindScriptFunction ('PromptRequest', @PromptRequest, VFOV, '()');
    BindScriptFunction ('RaiseError', @RaiseError, VFS, '(aString)');
    BindScriptFunction ('RaiseHttpFault', @RaiseHttpFault, VFOSSS, '(aHttpCode, aResponseText, aResponseContentType)');
    BindScriptFunction ('RaiseSoapFault', @RaiseSoapFault, VFOSSSS, '(aFaultCode, aFaultString, aFaultActor, aDetail)');
    BindScriptFunction ('RaiseWsdlFault', @RaiseWsdlFault, VFOSSS, '(aFaultCode, aFaultString, aFaultActor)');
    BindScriptFunction ('Random', @RandomX, XFXX, '(aLow, aHigh)');
    BindScriptFunction ('RegExprSafeStr', @RegExprSafeStr, SFS, '(aString)');
    BindScriptFunction ('RequestAsText', @wsdlRequestAsText, SFOS, '(aOperation)');
    BindScriptFunction ('ReplyAsText', @wsdlReplyAsText, SFOS, '(aOperation)');
    BindScriptFunction ('ResetOperationCounters', @ResetOperationCounters, VFV, '()');
    BindScriptFunction ('ResetEnvVar', @ResetEnvVar, VFOS, '(aKey)');
    BindScriptFunction ('ResetEnvVars', @ResetEnvVars, VFOS, '(aRegularExpr)');
    BindScriptFunction ('ResolveAliasses', @xmlio.resolveAliasses, SFS, '(aString)');
    BindScriptFunction ('ReturnString', @ReturnString, VFOS, '(aString)');
    BindScriptFunction ('SaveLogs', @SaveLogs, VFOS, '(aFileName)');
    BindScriptFunction ('SetContext', @SetContext, SFS, '(aContextName)');
    BindScriptFunction ('SqlSelectResultRow', @SqlSelectResultRow, SLFOS, '(aSqlSelectQuery)');
    BindScriptFunction ('SqlQuotedStr', @sqlQuotedString, SFS, '(aString)');
    BindScriptFunction ('EnableAllMessages', @EnableAllMessages, VFV, '()');
    BindScriptFunction ('EnableMessage', @EnableMessage, VFOV, '()');
    BindScriptFunction ('OperationCount', @xsdOperationCount, XFOV, '()');
    BindScriptFunction ('RegExprMatch', @RegExprMatchList, SLFOSS, '(aString, aRegExpr)');
    BindScriptFunction ('SeparatedString', @SeparatedStringList, SLFOSS, '(aString, aSeparator)');
    BindScriptFunction ('SeparatedStringN', @SeparatedStringN, SFOSSX, '(aString, aSeparator, aIndex)');
    BindScriptFunction ('SeparatedStringT', @SeparatedStringT, SFOSSX, '(aString, aSeparator, aIndex)');
    BindScriptFunction ('RequestOperation', @WsdlRequestOperation, VFOS, '(aOperation)');
    BindScriptFunction ('RequestOperationLater', @WsdlRequestOperationLater, VFOSX, '(aOperation, aLaterMs)');
    BindScriptFunction ('Rounded', @RoundedX, XFXX, '(aNumber, aDecimals)');
    BindScriptFunction ('SendOperationRequest', @WsdlSendOperationRequest, VFSS, '(aOperation, aCorrelation)');
    BindScriptFunction ('SendOperationRequestLater', @WsdlSendOperationRequestLater, VFSSX, '(aOperation, aCorrelation, aLater)');
    BindScriptFunction ('SetEnvNumber', @setEnvNumber, XFOSX, '(aKey, aNumber)');
    BindScriptFunction ('SetEnvVar', @setEnvVar, SFOSS, '(aKey, aValue)');
    BindScriptFunction ('SHA1', @SHA1, SFS, '(aString)');
    BindScriptFunction ('ShowMessage', @SjowMessage, VFS, '(aString)');
    BindScriptFunction ('SiebelNowAsStr', @sblNowAsDateTime, SFV, '()');
    BindScriptFunction ('SiebelTodayAsStr', @sblTodayAsDate, SFV, '()');
    BindScriptFunction ('Sleep', @SleepX, VFX, '(aMilliSeconds)');
    BindScriptFunction ('StrFromClipboard', @StrFromClipboard, SFV, '()');
    BindScriptFunction ('StrHasRegExpr', @StringHasRegExpr, SFSS, '(aString, aRegExpr)');
    BindScriptFunction ('StrMatchesRegExpr', @StringMatchesRegExpr, SFSS, '(aString, aRegExpr)');
    BindScriptFunction ('StrOfChar', @xStringOfChar, SFSX, '(aChar, aNumber)');
    BindScriptFunction ('StrToDate', @StrToDateX, DFS, '(aString)');
    BindScriptFunction ('StrToDateTime', @XmlToDateTime, DFS, '(aString)');
    BindScriptFunction ('StrToNumber', @StrToFloatX, XFS, '(aString)');
    BindScriptFunction ('SubStr', @SubStringX, SFSXX, '(aString, aStart, aLength)');
    BindScriptFunction ('TodayAsStr', @xsdTodayAsDate, SFV, '()');
    BindScriptFunction ('UnixToDateTime', @xUnixToDateTime, DFX, '(aUnixDateTime)');
    BindScriptFunction ('UppercaseStr', @UpperCaseStr, SFS, '(aString)');
    BindScriptFunction ('UserName', @wsdlUserName, SFV, '()');
    BindScriptFunction ('StrToFile', @xmlio.SaveStringToFile, VFSS, '(aFileName, aString)');
    BindScriptFunction ('OperationName', @wsdlOperationName, SFOV, '()');
  except
    raise;
  end
end;

procedure TWsdlOperation.PrepareBefore;
var
  x: Integer;
begin
  fPreparedBefore := False;
  try
    PrepareScripting;
    CheckScript(BeforeScriptLines, nil);
    fPreparedBefore := True;
  except
    raise;
  end
end;

procedure TWsdlOperation.PrepareChecker(aBind: TCustomBindable);
begin
  if aBind.isEvaluation then
  begin
    BindChecker (aBind);
    fStamperStatement := 'Bind_.Checker := (' + aBind.Checker + ');';
    fLineNumber := 0;
    fExpressChecker.OnError := fOnError;
    fExpressChecker.Prepare;
  end;
end;

procedure TWsdlOperation.PrepareAfter;
var
  x: Integer;
begin
  fPreparedAfter := False;
  try
    PrepareScripting;
    CheckScript(AfterScriptLines, nil);
    fPreparedAfter := True;
  except
    raise;
  end
end;

function TWsdlOperation.StreamWsAddressing (aWsa: TXml; isRequest: Boolean): String;
  function mustUnderstand: String;
  begin
    result := '';
    if wsaSpecificMustUnderstand then
      if wsaMustUnderstand then
        result := ' soapenv:mustUnderstand="1"'
      else
        result := ' soapenv:mustUnderstand="0"'
  end;
  function strAddElm (aString, aTag, aValue: String): String;
  begin
    result := StrAdd (aString, '    <' + aTag + mustUnderstand + '>' + aValue + '</' + aTag + '>');
  end;
  function strAddStruct (aXml: TXml): String;
    function _attributes: String;
    var
      x: Integer;
    begin
      result := '';
      for x := 0 to aXml.Attributes.Count - 1 do
        if aXml.Attributes.XmlAttributes[x].Checked then
          result := result
                  + ' '
                  + aXml.Attributes.XmlAttributes[x].Name
                  + '="'
                  + aXml.Attributes.XmlAttributes[x].Value
                  + '"'
                  ;
    end;
  var
    x: Integer;
    xSep: String;
  begin
    result := '';
    if not Assigned (aXml) then Exit;
    if not aXml.Checked then Exit;
    result := result
            + '    <wsa:'
            + aXml.Name
            + mustUnderstand
            + _attributes
            + '>'
            + aXml.Value
            ;
    xSep := '';
    for x := 0 to aXml.Items.Count - 1 do
      if aXml.Items.XmlItems[x].Checked then
      begin
        result := result
                + #$D#$A
                + '      <wsa:'
                + aXml.Items.XmlItems[x].Name
                + mustUnderstand
                + '>'
                + aXml.Items.XmlItems[x].Value
                + '</wsa:'
                + aXml.Items.XmlItems[x].Name
                + '>'
                ;
        xSep := #$D#$A + '    ';
      end;
    result := result
            + xSep
            + '</wsa:'
            + aXml.Name
            + '>'
            + #$D#$A
            ;
  end;
var
  xXml: TXml;
begin
  result := '';
  if not Assigned (_WsdlWsaXsd) then Exit;
  if not wsaEnabled then Exit;
  xXml := aWsa.Items.XmlItemByTag['Action'];
  if Assigned (xXml) and xXml.Checked then
    result := strAddElm (result, 'wsa:Action', xXml.Value)
  else
    result := strAddElm (result, 'wsa:Action', SoapAction);
  if isRequest then
    result := result + strAddStruct (aWsa.Items.XmlItemByTag['FaultTo']);
  result := result + strAddStruct (aWsa.Items.XmlItemByTag['From']);
  result := result + strAddStruct (aWsa.Items.XmlItemByTag['MessageID']);
  result := result + strAddStruct (aWsa.Items.XmlItemByTag['ProblemAction']);
//not yet supported: problemiri
  result := result + strAddStruct (aWsa.Items.XmlItemByTag['RelatesTo']);
  if isRequest
  and (not isOneWay) then
  begin
    xXml := aWsa.Items.XmlItemByTag['ReplyTo'];
    if Assigned (xXml)
    and (xXml.Items.Count > 0) then
      xXml.Items.XmlItems[0].Value := 'http://' + _WsdlHostName + ':' + _wsdlPortNumber;
    result := result + strAddStruct (aWsa.Items.XmlItemByTag['ReplyTo']);
  end;
//not yet supported:retryafter
  xXml := aWsa.Items.XmlItemByTag['To'];
  if Assigned (xXml) and xXml.Checked then
    result := strAddElm (result, 'wsa:To', xXml.Value)
{allow overrule in script}{
  else
    result := strAddElm (result, 'wsa:To', WsaTo){};
end;

function TWsdlOperation.StreamRequest ( aGeneratedWith: String
                                      ; aGenerateTypes: Boolean
                                      ; aGenerateHeaderNameSpaces: Boolean
                                      ; aGenerateBodyNameSpaces: Boolean
                                      ): String;
var
  x: Integer;
  aXml: TXml;
  xName: String;
begin
  result := '';

  if StartsText('multipart', Consumes)
  and (reqBind is TXml) then
  begin
    aXml := TXml.Create;
    try
      aXml.jsonType := jsonObject;
      for x := 0 to reqXml.Items.Count - 1 do
      with reqXml.Items.XmlItems[x] do
      begin
        if Checked
        and Assigned (Xsd)
        and (Xsd.ParametersType = oppFormData) then
          aXml.AddXml (TXml.CreateAsString(Name, Value));
      end;
      result := aXml.StreamJSON(0, False);
    finally
      aXml.Free;
    end;
    Exit;
  end;

  if isFreeFormat then
  begin
    result := FreeformatReq;
    Exit;
  end;

  if isSoapService then
  begin
    result := GenerateXmlHeader(not WsdlService.SuppressXmlComment);
    result := StrAdd (result, '<soapenv:Envelope');
    result := StrAdd (result, ' xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"');
    result := StrAdd (result, ' xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/"');
    result := StrAdd (result, ' xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"');
    result := StrAdd (result, ' xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"');
    result := StrAdd (result, ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"');
    result := StrAdd (result, ' xmlns:xsi="' + scXMLSchemaInstanceURI + '"');
    result := StrAdd (result, ' >');
    if (InputHeaders.Count > 0)
    or (WsdlService.AuthenticationType = atWsSecurity)
    or wsaEnabled then
    begin
      if wsaEnabled then
        result := StrAdd (result, Format ('  <soapenv:Header xmlns:wsa="http://www.w3.org/2005/08/addressing">', [wsaType]))
      else
        result := StrAdd (result, '  <soapenv:Header>');
      result := result + WsdlService.StreamWsSecurity;
      for x := 0 to InputHeaders.Count - 1 do
      begin
        xsdGenerated := True;
        xsiGenerated := True;
        result := result
                + (reqBind as TXml).Items.XmlItems [x].StreamXML
                               ( aGenerateHeaderNameSpaces
                               , True
                               , 4
                               , True
                               , (InputHeaders.Headers[x].Use = scSoapUseEncoded)
                               )
                ;
      end;
      result := result + StreamWsAddressing(reqWsaXml, True);
      result := StrAdd (result, '  </soapenv:Header>');
    end;
    if SoapBodyInputUse = scSoapUseEncoded then
    begin
      result := StrAdd (result, '  <soapenv:Body soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">');
    end
    else
      result := StrAdd (result, '  <soapenv:Body>');
    for x := InputHeaders.Count to (reqBind as TXml).Items.Count - 1 do
    begin
      xsdGenerated := True;
      xsiGenerated := True;
      result := result + (reqBind as TXml).Items.XmlItems[x].StreamXML
                               ( aGenerateBodyNameSpaces
                               , WsdlService.UseNameSpacePrefixes
                               , 4
                               , True
                               , (SoapBodyInputUse = scSoapUseEncoded)
                               )
                       ;
    end;
    result := StrAdd (result, '  </soapenv:Body>');
    result := StrAdd (result, '</soapenv:Envelope>');
    Exit;
  end;

  if isOpenApiService
  and (OpenApiVersion[1] = '3') then
  begin
    with reqBind as TXml do
    for x := 0 to Items.Count - 1 do
    with Items.XmlItems[x] do
    begin
      if Checked
      and Assigned (Xsd)
      and (Xsd.ParametersType = oppBody) then
      begin
        if (pos ('xml', lowercase (Xsd.MediaType)) > 0) then
        begin
          xName := Name;
          if Xsd.sType.JsonXmlName <> '' then
            Name := Xsd.sType.JsonXmlName;
          try
            result := StreamXML(aGenerateBodyNameSpaces, WsdlService.UseNameSpacePrefixes, 0, True, False)
          finally
            Name := xName;
          end;
        end
        else
          result := StreamJSON(0, True);
      end;
    end;
    Exit;
  end;

  if reqBind is TXml then
  begin
    for x := 0 to (reqBind as TXml).Items.Count - 1 do
    begin
      aXml := (reqBind as TXml).Items.XmlItems[x];
      if (not isOpenApiService)
      or (    Assigned (aXml.Xsd)
          and (aXml.Xsd.ParametersType = oppBody)
         ) then
      begin
        xsiGenerated := False;
        xsdGenerated := False;
        if ConsumeType = ptXml then
          result := result + aXml.StreamXML(aGenerateBodyNameSpaces, WsdlService.UseNameSpacePrefixes, 0, True, False)
        else
        begin
          if aXml.jsonType = jsonNone then
            aXml.jsonType := jsonObject;
          result := result + aXml.StreamJSON(0, True);
        end;
      end;
    end;
    Exit;
  end;

  if reqBind is TIpmItem then
  begin
    result := (reqBind as TIpmItem).ValuesToBuffer (nil);
    Exit;
  end;

  Raise Exception.Create ('TWsdlOperation.StreamRequest: New stuf??? Statement should not be reached');
end;

function TWsdlOperation.PrepareReply(aGeneratedWith: String; aGenerateTypes: Boolean): String;
var
  x, y: Integer;
  xXml, yXml, zXml: TXml;
  xName: String;
begin
  if ResponseNo = 0 then
    ResponseNo := 200; // nice default
  result := LiteralResult;
  if Result <> '' then
    Exit;

  if isFreeFormat then
  begin
    result := FreeformatRpy;
    Exit;
  end;

  if (    isSoapService
      and (rpyXsd.sType.ElementDefs.Count = 0)
     )
  or (    (rpyXsd.sType.ElementDefs.Count = 0)
      and (rpyBind is TXml)
     ) then // one way
    exit;

  if isSoapService then
  begin
    result := GenerateXmlHeader(not WsdlService.SuppressXmlComment);
    result := StrAdd (result, '<soapenv:Envelope');
    result := StrAdd (result, ' xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"');
    result := StrAdd (result, ' xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/"');
    result := StrAdd (result, ' xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"');
    result := StrAdd (result, ' xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"');
    result := StrAdd (result, ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"');
    result := StrAdd (result, ' xmlns:xsi="' + scXMLSchemaInstanceURI + '"');
    result := StrAdd (result, ' >');
    if (OutputHeaders.Count > 0)
    or wsaEnabled then
    begin
      if wsaEnabled then
        result := StrAdd (result, Format ('  <soapenv:Header xmlns:wsa="http://www.w3.org/%s/addressing">', [wsaType]))
      else
        result := StrAdd (result, '  <soapenv:Header>');
      for x := 0 to OutputHeaders.Count - 1 do
      begin
        xsdGenerated := True;
        xsiGenerated := True;
        result := result + (rpyBind as TXml).Items.XmlItems[x].StreamXml
                                 ( True
                                 , True
                                 , 4
                                 , True
                                 , (OutputHeaders.Headers[x].Use = scSoapUseEncoded)
                                 );
      end;
      result := result + StreamWsAddressing(rpyWsaXml, False);
      result := StrAdd (result, '  </soapenv:Header>');
    end;
    if SoapBodyOutputUse = scSoapUseEncoded then
    begin
      result := StrAdd (result, '  <soapenv:Body soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">');
    end
    else
      result := StrAdd (result, '  <soapenv:Body>');
    for x := OutputHeaders.Count to (rpyBind as TXml).Items.Count - 1 do
    begin
      xsdGenerated := True;
      xsiGenerated := True;
      result := result + (rpyBind as TXml).Items.XmlItems[x].StreamXML
                               ( True
                               , True
                               , 4
                               , True
                               , (SoapBodyOutputUse = scSoapUseEncoded)
                               )
                       ;
    end;
    result := StrAdd (result, '  </soapenv:Body>');
    result := StrAdd (result, '</soapenv:Envelope>');
    exit
  end;

  if isOpenApiService then
  begin
    xXml := (rpyBind as TXml);
    for x := 0 to xXml.Items.Count - 1 do
    begin
      xXml := (rpyBind as TXml);
      if xXml.Items.XmlItems[x].Checked then
      begin
        yXml := xXml.Items.XmlItems[x];
        ResponseNo := yXml.Xsd.ResponseNo;
        if ResponseNo > -1 then
        begin
          if Copy (OpenApiVersion, 1, 1) <> '2' then
          begin
            for y := 0 to yXml.Items.Count - 1 do
            with yXml.Items.XmlItems[y] do
            begin
              if (Xsd.ParametersType = oppBody)
              and Checked then
              begin
                if (pos ('xml', LowerCase(Xsd.MediaType)) > 0) then
                begin
                  xName := Name;
                  if Xsd.sType.JsonXmlName <> '' then
                    Name := Xsd.sType.JsonXmlName;
                  try
                    result := StreamXML (True, True, 0, True, False);
                  finally
                    Name := xName;
                  end;
                end
                else
                  result := StreamJSON(0, True);
              end;
            end;
          end
          else
          begin
            for y := 0 to yXml.Items.Count - 1 do
            begin
              zXml := yXml.Items.XmlItems[y];
              if zXml.Xsd.ParametersType = oppBody then
              begin
                case ProduceType of
                  ptJson: result := result + zXml.StreamJSON(0, True);
                  ptXml:
                  begin
                    xName := zXml.Name;
                    if zXml.Xsd.sType.JsonXmlName <> '' then
                      zXml.Name := zXml.Xsd.sType.JsonXmlName;
                    try
                      result := result + zXml.StreamXML ( True
                                                        , True
                                                        , 0
                                                        , True
                                                        , False
                                                        );
                    finally
                      zXml.Name := xName;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end
        else
        begin
          zXml := yXml.FindXml('undefined.responseCode');
          if Assigned(zXml) then
            ResponseNo := StrToIntDef(zXml.Value, 500)
          else
            ResponseNo := 500;
          Result := xmlio.HttpResponseCodeToText(ResponseNo);
        end;
      end;
    end;
    Exit;
  end;

  if rpyBind is TXml then
  begin
    xsiGenerated := False;
    xsdGenerated := False;
    xXml := (rpyBind as TXml).Items.XmlItems[0];
    if WsdlService.DescriptionType <> ipmDTJson then
      result := result + xXml.StreamXML
                             ( (xXml.Xsd.sType.NameSpace <> '')
                             , WsdlService.UseNameSpacePrefixes
                             , 0
                             , True
                             , False
                             )
    else
      result := result + xXml.StreamJSON(0, True);
    Exit;
  end;

  if rpyBind is TIpmItem then
  begin
    result := rpyIpm.ValuesToBuffer (nil);
    Exit;
  end;

  result := _ProgName + ': unrecognised rply type. This statement should not be reached...';

end;

function TWsdlOperation.StamperFunctionPrototypes: TJBStringList;
begin
  result := fExpressStamper.FunctionProtoTypes;
end;

function TWsdlOperation.StreamFault (aGeneratedWith: String; aGenerateTypes: Boolean): String;
var
  x: Integer;
  swapName: String;
begin
  result := '';
  if isSoapService then
  begin
    result := GenerateXmlHeader(not WsdlService.SuppressXmlComment);
    result := StrAdd (result, '<soapenv:Envelope');
    result := StrAdd (result, ' xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"');
    result := StrAdd (result, ' xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/"');
    result := StrAdd (result, ' xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"');
    result := StrAdd (result, ' xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"');
    result := StrAdd (result, ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"');
    result := StrAdd (result, ' xmlns:xsi="' + scXMLSchemaInstanceURI + '"');
    result := StrAdd (result, ' >');

    if (OutputHeaders.Count > 0) then
    begin
      result := StrAdd (result, '  <soapenv:Header>');
      for x := 0 to OutputHeaders.Count - 1 do
      begin
        xsdGenerated := True;
        xsiGenerated := True;
        result := result + (rpyBind as TXml).Items.XmlItems [x].StreamXML
                                 ( True
                                 , True
                                 , 4
                                 , True
                                 , (OutputHeaders.Headers[x].Use = scSoapUseEncoded)
                                 )
                         ;
      end;
      result := StrAdd (result, '  </soapenv:Header>');
    end;

    if SoapBodyOutputUse = scSoapUseEncoded then
    begin
      result := StrAdd (result, '  <soapenv:Body soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">');
      result := StrAdd (result, '    <soapenv:Fault>');
    end
    else
    begin
      result := StrAdd (result, '  <soapenv:Body>');
      result := StrAdd (result, '    <soapenv:Fault>');
    end;
    result := StrAdd (result, '      <faultcode>' + faultcode + '</faultcode>');
    result := StrAdd (result, '      <faultstring>' + faultstring + '</faultstring>');
    result := StrAdd (result, '      <faultactor>' + faultactor + '</faultactor>');
    for x := 0 to (fltBind as TXml).Items.Count - 1 do
    begin
      if (fltBind as TXml).Items.XmlItems [x].Checked then
      begin
        xsdGenerated := True;
        xsiGenerated := True;
        swapName := (fltBind as TXml).Items.XmlItems [x].Name;
        (fltBind as TXml).Items.XmlItems [x].Name := 'detail';
        try
          result := result + (fltBind as TXml).Items.XmlItems [x].StreamXML
                                     ( True
                                     , False
                                     , 6
                                     , True
                                     , (SoapBodyOutputUse = scSoapUseEncoded)
                                     )
                           ;
        finally
          (fltBind as TXml).Items.XmlItems [x].Name := swapName;
        end;
      end;
    end;
    result := StrAdd (result, '    </soapenv:Fault>');
    result := StrAdd (result, '  </soapenv:Body>');
    result := StrAdd (result, '</soapenv:Envelope>');
  end
  else
  begin
    if (fltBind is TXml) then
    begin
      xsiGenerated := False;
      xsdGenerated := False;
      if WsdlService.DescriptionType <> ipmDTJson then
        result := result + (fltBind as TXml).Items.XmlItems[0].StreamXML
                                     ( True
                                     , True
                                     , 0
                                     , True
                                     , False
                                     )
      else
        result := result + (fltBind as TXml).Items.XmlItems[0].StreamJSON(0, True);
    end;
    if (fltBind is TIpmItem) then
      result := result + ((fltBind as TIpmItem).ValuesToBuffer (nil));
  end;
end;

function TWsdlOperation.getLastFocusedMessage: TWsdlMessage;
begin
  Result := fLastFocusedMessage;
  if Assigned (fLastFocusedMessage) then
    if Messages.IndexOfObject(fLastFocusedMessage) < 0 then
      result := nil;
end;

function TWsdlOperation.getLastFullCaption: String;
begin
  Result := fLastFullCaption;
end;

function TWsdlOperation.CheckerFunctionPrototypes: TJBStringList;
begin
  result := fExpressChecker.FunctionProtoTypes;
end;

procedure TWsdlOperation.Clean;
var
  m: Integer;
begin
  for m := 0 to Messages.Count - 1 do with Messages.Messages[m] do
    Clean;
end;

function TWsdlOperation.CorrelationIdAsText(aSeparator: String): String;
var
  xSep: String;
  x: Integer;
begin
  result := '';
  xSep := '';
  for x := 0 to CorrelationBindables.Count - 1 do
  begin
    if Assigned (CorrelationBindables.Bindables[x]) then
      try
        result := result + xSep + CorrelationBindables.Bindables[x].GetStringData;
      except
        result := result + xSep + '?';
      end
    else
      result := result + xSep + '?';
    xSep := aSeparator;
  end;
end;

constructor TWsdlOperation.Create(aOperation: TWsdlOperation);
  function _cloneBindables (aSrc: TBindableList): TBindableList;
  var
    x: Integer;
  begin
    result := TBindableList.Create;
    for x := 0 to aSrc.Count - 1 do
    begin
      if AnsiStartsText('Req.',aSrc.Strings[x]) then
        result.AddObject
          ( aSrc.Strings[x]
          , self.reqBind.FindUQ
              (Copy ( aSrc.Strings[x]
                    , 5
                    , Length (aSrc.Strings[x]) - 4
                    )
              )
          );
      if AnsiStartsText('Rpy.',aSrc.Strings[x]) then
        result.AddObject
          ( aSrc.Strings[x]
          , self.rpyBind.FindUQ
              (Copy ( aSrc.Strings[x]
                    , 5
                    , Length (aSrc.Strings[x]) - 4
                    )
              )
          );
      if AnsiStartsText('Flt.',aSrc.Strings[x]) then
        result.AddObject
          ( aSrc.Strings[x]
          , self.fltBind.FindUQ
              (Copy ( aSrc.Strings[x]
                    , 5
                    , Length (aSrc.Strings[x]) - 4
                    )
              )
          );
    end;
  end;
var
  f: Integer;
  xOperation: TWsdlOperation;
  xXml: TXml;
begin
  xOperation := aOperation;
  while Assigned(xOperation.Cloned) do
    xOperation := xOperation.Cloned;
  self.WsdlOperation := self;
  self.fCloned := xOperation;
  self.fLock := xOperation.fLock;
  self.doSuppressLog := xOperation.doSuppressLog;
  self.Owner := xOperation.Owner;
  self.Data := xOperation.Data;
  self.fOnGetAbortPressed := xOperation.fOnGetAbortPressed;
  self.Wsdl := xOperation.Wsdl;
  self.WsdlService := xOperation.WsdlService;
  self.Name := xOperation.Name;
  self.reqTagName := xOperation.reqTagName;
  self.reqMessageName := xOperation.reqMessageName;
  self.Alias := xOperation.Alias;
  self.Schemes := xOperation.Schemes;
  self.Produces := xOperation.Produces;
  self.ConsumeType := xOperation.ConsumeType;
  self.ProduceType := xOperation.ProduceType;
  self.Consumes := xOperation.Consumes;
  self.ContentType := xOperation.ContentType;
  self.OverruleContentType := xOperation.OverruleContentType;
  self.Accept := xOperation.Accept;
  self.reqTagNameSpace := xOperation.reqTagNameSpace;
  self.rpyMessageName := xOperation.rpyMessageName;
  self.rpyTagName := xOperation.rpyTagName;
  self.rpyTagNameSpace := xOperation.rpyTagNameSpace;
  self.Documentation := xOperation.Documentation;
  Self.FaultMessages := xOperation.FaultMessages;
  self.InputHeaders := xOperation.InputHeaders;
  self.OutputHeaders := xOperation.OutputHeaders;
  self.BindName := xOperation.BindName;
  self.inboundRequestSchemaValidationType := xOperation.inboundRequestSchemaValidationType;
  self.outboundReplySchemaValidationType := xOperation.outboundReplySchemaValidationType;
  self.outboundRequestSchemaValidationType := xOperation.outboundRequestSchemaValidationType;
  self.inboundReplySchemaValidationType := xOperation.inboundReplySchemaValidationType;
  self.schemaValidationVioloationHttpResponseCode := xOperation.schemaValidationVioloationHttpResponseCode;
  self.OnRequestViolatingAddressPath := xOperation.OnRequestViolatingAddressPath;
  self.SoapAction := xOperation.SoapAction;
  self.SoapBindingStyle := xOperation.SoapBindingStyle;
  self.SoapBodyInputEncodingStype := xOperation.SoapBodyInputEncodingStype;
  self.SoapBodyInputPartName := xOperation.SoapBodyInputPartName;
  self.SoapBodyInputRequired := xOperation.SoapBodyInputRequired;
  self.SoapBodyInputUse := xOperation.SoapBodyInputUse;
  self.SoapBodyOutputEncodingStype := xOperation.SoapBodyOutputEncodingStype;
  self.SoapBodyOutputPartName := xOperation.SoapBodyOutputPartName;
  self.SoapBodyOutputRequired := xOperation.SoapBodyOutputRequired;
  self.SoapBodyOutputUse := xOperation.SoapBodyOutputUse;
  self.FaultXsd := xOperation.FaultXsd;
  if Assigned (self.FaultXsd) then
  begin
    self.fltBind := TXml.Create (-10000, self.FaultXsd);
    self.fltBind.Name := 'Faults';
  end;
  if Assigned (self.FaultMessages) then
    for f := 0 to self.FaultMessages.Count - 1 do
      if f < (self.fltBind as TXml).Items.Count then
        (self.fltBind as TXml).Items.XmlItems[f].TagName := self.FaultMessages.Messages[f].Name;
  self.SoapAddress := xOperation.SoapAddress;
  self.wsaEnabled := xOperation.wsaEnabled;
  self.wsaSpecificMustUnderstand := xOperation.wsaSpecificMustUnderstand;
  self.wsaMustUnderstand := xOperation.wsaMustUnderstand;
  self.wsaType := xOperation.wsaType;
  self.useSsl := xOperation.useSsl;
  self.sslCertificateFile := xOperation.sslCertificateFile;
  self.sslKeyFile := xOperation.sslKeyFile;
  self.sslRootCertificateFile := xOperation.sslRootCertificateFile;
  self.sslPassword := xOperation.sslPassword;
  self.sslVersion := xOperation.sslVersion;
  if Assigned (_WsdlWsaXsd) then
  begin
    reqWsaXml := TXml.Create(-1000, _WsdlWsaXsd);
    reqWsaXml.CheckDownline(False);
    rpyWsaXml := TXml.Create(-1000, _WsdlWsaXsd);
    rpyWsaXml.CheckDownline(False);
  end;
  self.StubStompHeaderXml := xOperation.StubStompHeaderXml;
  self.StubCustomHeaderXml := xOperation.StubCustomHeaderXml;
  self.doReadReplyFromFile := xOperation.doReadReplyFromFile;
  self.ReadReplyFromFileXml := xOperation.ReadReplyFromFileXml;
  self.StubAction := xOperation.StubAction;
  self.StubTransport := xOperation.StubTransport;
  self.StubHttpAddress := xOperation.StubHttpAddress;
  self.ContentEncoding := xOperation.ContentEncoding;
  self.AcceptDeflateEncoding := xOperation.AcceptDeflateEncoding;
  self.AcceptGzipEncoding := xOperation.AcceptGzipEncoding;
  self.httpVerb := xOperation.httpVerb;
  self.StubStompPutHost := xOperation.StubStompPutHost;
  self.StubStompPutPort := xOperation.StubStompPutPort;
  self.StubStompPutUseCredentials := xOperation.StubStompPutUseCredentials;
  self.StubStompPutUserName := xOperation.StubStompPutUserName;
  self.StubStompPutPassword := xOperation.StubStompPutPassword;
  self.StubStompPutClientId := xOperation.StubStompPutClientId;
  self.StubStompTimeOut := xOperation.StubStompTimeOut;
  self.smtpHost := xOperation.smtpHost;
  self.smtpPort := xOperation.smtpPort;
  self.BeforeScriptLines := xOperation.BeforeScriptLines;
  self.AfterScriptLines := xOperation.AfterScriptLines;
  self.onFetchLogFromCloud := xOperation.onFetchLogFromCloud;
  self.resolveRequestAliasses := xOperation.resolveRequestAliasses;
  self.resolveReplyAliasses := xOperation.resolveReplyAliasses;
  self.CorrelatedMessage := xOperation.CorrelatedMessage;
  self.Messages := xOperation.Messages;
//  self.faultcode, faultstring, faultactor := xOperation.String;
  self.RecognitionType := xOperation.RecognitionType;
  self.invokeRequestInfo := xOperation.invokeRequestInfo;
  self.invokeReplyInfo := xOperation.invokeReplyInfo;
  self.reqRecognition := xOperation.reqRecognition;
  self.rpyRecognition := xOperation.rpyRecognition;
//  self.doDebug: Boolean;
  self.DelayTimeMsMin := xOperation.DelayTimeMsMin;
  self.DelayTimeMsMax := xOperation.DelayTimeMsMax;
  self.reqXsd := xOperation.reqXsd;
  if xOperation.reqBind is TIpmItem then
    self.reqBind := TIpmItem.Create(xOperation.reqBind as TIpmItem)
  else
  begin
    if xOperation.isFreeFormat then
    begin
      reqBind := TXml.CreateAsString(xOperation.reqBind.Name, '');
      reqXml.AddXml(TXml.CreateAsString('Body', ''));
    end
    else
    begin
      if Assigned (self.reqXsd) then
      begin
        self.reqBind := TXml.Create (-10000, self.reqXsd);
        self.reqBind.Name := xOperation.reqBind.Name;
      end;
    end;
  end;
  self.rpyXsd := xOperation.rpyXsd;
  if xOperation.rpyBind is TIpmItem then
    self.rpyBind := TIpmItem.Create(xOperation.rpyBind as TIpmItem)
  else
  begin
    if xOperation.isFreeFormat then
    begin
      rpyBind := TXml.CreateAsString(xOperation.rpyBind.Name, '');
      rpyXml.AddXml(TXml.CreateAsString('Body', ''));
    end
    else
    begin
      if Assigned (self.rpyXsd) then
      begin
        self.rpyBind := TXml.Create (-10000, self.rpyXsd);
        self.rpyBind.Name := xOperation.rpyBind.Name;
      end;
    end;
  end;
  if self.invokeRequestInfo
  and Assigned(endpointConfigXsd) then
  begin
    self.requestInfoBind := TXml.Create (-10000, endpointConfigXsd);
    xXml := self.endpointConfigAsXml;
    try
      with self.requestInfoBind as TXml do
      begin
        Name := xXml.Name;
        CheckDownline(false);
        LoadValues (xXml, False, True);
      end;
    finally
      xXml.Free;
    end;
    self.requestInfoBind.Name := xOperation.Alias;
  end;
  if self.invokeReplyInfo
  and Assigned(replyInfoXsd) then
  begin
    self.replyInfoBind := TXml.Create (-10000, replyInfoXsd);
    self.replyInfoBind.Name := xOperation.Alias;
  end;
  self.OnError := xOperation.OnError;
  if Assigned (xOperation.CorrelationBindables) then
    self.CorrelationBindables := _cloneBindables(xOperation.CorrelationBindables);
  if Assigned (xOperation.BindablesWithAddedElement) then
    self.BindablesWithAddedElement := _cloneBindables(xOperation.BindablesWithAddedElement);
  if Assigned (xOperation.LogColumns) then
    self.LogColumns := _cloneBindables(xOperation.LogColumns);
  if Assigned (xOperation.invokeList) then
  begin
    self.invokeList := TWsdlOperations.Create;
    self.invokeList.Sorted := True;
    self.invokeList.Text := xOperation.invokeList.Text;
    self.doInvokeOperations;
    self.BindStamper;
    try
      self.PrepareBefore;
    except
      on e: Exception do
        fPrepareErrors := fPrepareErrors + 'Found in BeforeScript: ' + e.Message + LineEnding;
    end;
    try
      self.PrepareAfter;
    except
      on e: Exception do
        fPrepareErrors := fPrepareErrors + 'Found in AfterScript: ' + e.Message + LineEnding;
    end;
  end;
end;

constructor TWsdlOperation.CreateFromScriptXml (aOwner: TObject; aOnGetAbortPressed: TBooleanFunction; aScript: TXml);
var
  x: Integer;
  xWsdl: TWsdl;
  xInvoke: TXml;
  sOperation: TWsdlOperation;
begin
  if not Assigned(aScript)
  or (not (aScript is TXml))
  or (aScript.Name <> 'Script') then
    raise Exception.Create ('Illegal argument: constructor TWsdlOperation.CreateFromScriptXml (aScript : TXml );');
  WsdlOperation := self;
  fCloned := nil;
  fLock := SyncObjs.TCriticalSection.Create;
  BeforeScriptLines := TJBStringList.Create;
  invokeList := TWsdlOperations.Create;
  invokeList.Sorted := True;
  Owner := aOwner;
  OnGetAbortPressed := aOnGetAbortPressed;
  Name := 'Script';
  Alias := Name;
  BeforeScriptLines.Text := aScript.Items.XmlCheckedValueByTag['Code'];
  xInvoke := aScript.FindCheckedXml('Script.Invoke.operations');
  if Assigned(xInvoke) then
  begin
    for x := 0 to xInvoke.Items.Count - 1 do
    begin
      if (xInvoke.Items.XmlItems[x].Name = 'name')
      and (xInvoke.Items.XmlItems[x].Checked) then
      begin
        sOperation := allAliasses.FindOnAliasName(xInvoke.Items.XmlItems[x].Value);
        if Assigned (sOperation) then
          invokeList.Add(sOperation.Alias);
      end;
    end;
    doInvokeOperations;
  end;
  try
    PrepareBefore;
  except
    on e: Exception do
      fPrepareErrors := fPrepareErrors + 'Found in Script: ' + e.Message + LineEnding;
  end;
end;


procedure TWsdlOperation.XmlReplyToBindables (aReply: TXml; aAddUnknowns: Boolean);
var
  x, s, d: Integer;
  xXml: TXml;
begin
  (rpyBind as TXml).ResetValues;
  (rpyBind as TXml).Checked := True;
  aReply.SeparateNsPrefixes;
  aReply.ResolveNameSpaces;
  if aReply.Name = '' then Exit;
  if self.isSoapService then
  begin
    for x := 0 to aReply.Items.Count - 1 do
    begin
      xXml := aReply.Items.XmlItems [x];
      if (xXml.TagName = 'Header')
      and (xXml.Items.Count > 0) then
      begin
        for s := 0 to xXml.Items.Count - 1 do
        begin
          for d := 0 to OutputHeaders.Count - 1 do
            (rpyBind as TXml).Items.XmlItems[d].LoadValues (xXml.Items.XmlItems [s], aAddUnknowns, False, True, False);
        end;
      end;
      if (xXml.TagName = 'Body') then
      begin
        for s := 0 to xXml.Items.Count - 1 do
        begin
          for d := OutputHeaders.Count to (rpyBind as TXml).Items.Count - 1 do
            (rpyBind as TXml).Items.XmlItems[d].LoadValues (xXml.Items.XmlItems [s], aAddUnknowns, False, True, False);
        end;
      end;
    end;
  end
  else
  begin
    (rpyBind as TXml).Items.XmlItems[0].LoadValues (aReply, aAddUnknowns, False, True, False);
  end;
end;

procedure TWsdlOperation.XmlRequestToBindables (aRequest: TXml; aAddUnknowns: Boolean);
var
  x, s, d: Integer;
  xXml: TXml;
  xWsaName: String;
begin
  (reqBind as TXml).ResetValues;
  (reqBind as TXml).Checked := True;
  aRequest.SeparateNsPrefixes;
  aRequest.ResolveNameSpaces;
  if aRequest.isSoapEnvelope then
  begin
    for x := 0 to aRequest.Items.Count - 1 do
    begin
      xXml := aRequest.Items.XmlItems [x];
      if (xXml.TagName = 'Header') then
      begin
        for s := 0 to xXml.Items.Count - 1 do
        begin
          for d := 0 to InputHeaders.Count - 1 do
            (reqBind as TXml).Items.XmlItems[d].LoadValues (xXml.Items.XmlItems [s], aAddUnknowns, False, True, False);
        end;
        if Assigned (reqWsaXml) then
        begin
          reqWsaXml.ResetValues;
          xWsaName := xXml.Name;
          try
            xXml.Name := reqWsaXml.Name;
            reqWsaXml.LoadValues(xXml,False,False);
            rpyWsaOnRequest;
          finally
            xXml.Name := xWsaName;
          end;
        end;
      end;
      if (xXml.TagName = 'Body') then
      begin
        for s := 0 to xXml.Items.Count - 1 do
        begin
          for d := InputHeaders.Count to (reqBind as TXml).Items.Count - 1 do
            (reqBind as TXml).Items.XmlItems[d].LoadValues (xXml.Items.XmlItems [s], aAddUnknowns, False, True, False);
        end;
      end;
    end;
  end
  else
  begin
    (reqBind as TXml).Items.XmlItems[0].LoadValues (aRequest, aAddUnknowns, False, True, False);
  end;
end;

function TWsdlOperation.getDebugTokenStringBefore: String;
begin
  result := fExpress.DebugTokenStringList (BeforeScriptLines, nil);
end;

function TWsdlOperation.thisOperation: TWsdlOperation;
begin
  result := Self;
end;

function TWsdlOperation.AddedTypeDefElementsAsXml : TObject ;
var
  x, y, f: integer;
  nTypeDef: TXsdDataType;
  XmlResult: TXml;
  sXml: TXml;
  slx, sly: TJBStringList; // to avoid duplicates
  xKey: String;
begin
  XmlResult := TXml.CreateAsString('AddedTypeDefElements', '');
  result := XmlResult;
  slx := TJBStringList.Create;
  sly := TJBStringList.Create;
  try
    sly.Sorted := True;
    slx.Sorted := True;
    for x := 0 to BindablesWithAddedElement.Count - 1 do
    begin
      if not slx.Find(BindablesWithAddedElement.Strings[x], f) then
      begin
        slx.Add (BindablesWithAddedElement.Strings[x]);
        nTypeDef := (BindablesWithAddedElement.Bindables[x] as TXml).TypeDef;
        sXml := XmlResult.AddXml(TXml.CreateAsString('AddedTypeDefElement', ''));
        with sXml do
        begin
          sly.Clear;
          AddXml (TXml.CreateAsString('UsedAt', BindablesWithAddedElement.Strings[x]));
          for y := 0 to nTypeDef.ElementDefs.Count - 1 do
          with nTypeDef.ElementDefs.Xsds[y] do
          begin
            if _ElementOrTypeDefRef = etElementRef then
            begin
              xKey := Format ('Element;%s;%s;%s', [_RefNameSpace, _RefElementName, ElementName]);
              if not sly.Find(xKey, f) then
              begin
                sly.Add (xKey);
                with AddXml(TXml.CreateAsString('Added', '')) do
                begin
                  AddXml (TXml.CreateAsString('References', 'Element'));
                  AddXml(TXml.CreateAsString('NameSpace', _RefNameSpace));
                  AddXml(TXml.CreateAsString('Name', _RefElementName));
                  AddXml(TXml.CreateAsString('ElementName', ElementName));
                end
              end;
            end
            else
            begin
              xKey := Format ('TypeDef;%s;%s;%s', [sType.NameSpace, sType.Name, ElementName]);
              if not sly.Find(xKey, f) then
              begin
                sly.Add (xKey);
                with AddXml(TXml.CreateAsString('Added', '')) do
                begin
                  AddXml (TXml.CreateAsString('References', 'TypeDef'));
                  AddXml(TXml.CreateAsString('NameSpace', sType.NameSpace));
                  AddXml(TXml.CreateAsString('Name', sType.Name));
                  AddXml(TXml.CreateAsString('ElementName', ElementName));
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil (sly);
    FreeAndNil (slx);
  end;
end;

procedure TWsdlOperation.OnGetSslPassword(var aPassword: String);
begin
  aPassword := sslPassword;
end;

procedure TWsdlOperation.AddedTypeDefElementsFromXml (aXml : TObject );
  procedure _updateTypedef(var n: Integer; aPath: String; nType: TXsdDataType; aXsd: TXsd);
    procedure _update(aXml: TXml; aPath: String);
    var
      x: Integer;
    begin
      for x := 0 to aXml.Items.Count - 1 do
        _update(aXml.Items.XmlItems[x], aPath);
      if aXml.FullCaption = aPath then
      begin
        n := n + 1;
        bindRefId := 0;
        aXml.AddXml(TXml.Create(0, aXsd));
        aXml.TypeDef := nType;
      end;
    end;
  var
    xBind: TCustomBindable;
  begin
    xBind := FindBind(aPath);
    if Assigned (xBind) then
    begin
      if reqBind.IsAncestorOf(xBind) then
        _update(reqXml, xBind.FullCaption)
      else
        _update(rpyXml, xBind.FullCaption);
    end;
  end;
var
  xXml, yXml: TXml;
  n, x, y: Integer;
  xBind: TXml;
  nTypeDef, cTypeDef: TXsdDataType;
  xxsd, cxsd: TXsd;
  xPath, xNameSpace, xName, xElementName: String;
  xIsElementRef: Boolean;
begin
  xXml := aXml as TXml;
  for x := 0 to xXml.Items.Count - 1 do with xXml.Items.XmlItems[x] do
  begin
    if Name = 'AddedTypeDefElement' then
    begin
      xPath := Items.XmlValueByTag['UsedAt'];
      xBind := TXml (FindBind(xPath));
      if not Assigned (xBind) then
        SjowMessage(Format ('AddedTypeDefElement [%s], could not find element [%s]', [Alias, xPath]));
      if Assigned (xBind) then
      begin
        BindablesWithAddedElement.AddObject(xPath, xBind);
        for y := 0 to Items.Count - 1 do
        begin
          if Items.XmlItems[y].Name = 'Added' then with Items.XmlItems[y] do
          begin
            cTypeDef := nil;
            xNameSpace := Items.XmlValueByTag['NameSpace'];
            xName := Items.XmlValueByTag['Name'];
            xElementName := Items.XmlValueByTag['ElementName'];
            xIsElementRef := (Items.XmlValueByTagDef['References', 'TypeDef'] = 'Element');
            if xIsElementRef then
            begin
              cxsd := Wsdl.XsdDescr.FindElement(xNameSpace, xName);
              if not Assigned (cxsd) then
                SjowMessage(Format ( 'AddedTypeDefElement [%s], could not find element [%s;%s]'
                                   , [ Alias
                                     , xNameSpace
                                     , xName
                                     ]
                                   )
                           );
              if Assigned (cxsd) then
                cTypeDef := cxsd.sType;
            end
            else
              cTypeDef := Wsdl.XsdDescr.FindTypeDef(xNameSpace, xName);
            if not Assigned (cTypeDef) then
              SjowMessage(Format ( 'AddedTypeDefElement [%s], could not find typedef [%s;%s]'
                                 , [ Alias
                                   , xNameSpace
                                   , xName
                                   ]
                                 )
                         );
            if Assigned (cTypeDef) then
            begin
              xXsd := xBind.Xsd.AddElementDef ( Wsdl.XsdDescr
                                              , xElementName
                                              , cTypeDef
                                              );
              xXsd._RefNameSpace := xNameSpace;
              xXsd._RefElementName := xName;
              if xIsElementRef then
                xXsd._ElementOrTypeDefRef := etElementRef
              else
                xXsd._ElementOrTypeDefRef := etTypeDefRef;
              n := 0;
              _updateTypedef ( n
                             , xPath
                             , xBind.Xsd.sType
                             , xxsd
                             );
              if n = 0 then
                SjowMessage( Format ( 'AddedTypeDefElement [%s], no binds updated with [%s;%s]'
                                    , [ Alias
                                      , xNameSpace
                                      , xName
                                      ]
                                    )
                           );
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TWsdlOperation.BeforeBindsAsText : String ;
begin
  result := fExpress.BindsAsText;
end;

procedure TWsdlOperation.RebindLists;
var
  x: Integer;
begin
  if Assigned (CorrelationBindables) then with CorrelationBindables do
    for x := 0 to Count - 1 do
      Bindables[x] := FindBind(Strings[x]);
  if Assigned (LogColumns) then with LogColumns do
    for x := 0 to Count - 1 do
      Bindables[x] := FindBind(Strings[x]);
  if Assigned (BindablesWithAddedElement) then with BindablesWithAddedElement do
    for x := 0 to Count - 1 do
      Bindables[x] := FindBind(Strings[x]);
end;

function TWsdlOperation.getInputXml: TXml;
begin
  if _ipmGun
  and (InputHeaders.Count = 0)
  and ((freqBind as TXml).Items.Count > 0)
  then
    result := (freqBind as TXml).Items.XmlItems[0]
  else
    result := freqBind as TXml;
end;

function TWsdlOperation.getOutputXml: TXml;
begin
  if _ipmGun
  and (OutputHeaders.Count = 0)
  and ((frpyBind as TXml).Items.Count > 0)
  then
    result := (frpyBind as TXml).Items.XmlItems[0]
  else
    result := frpyBind as TXml;
end;

procedure TWsdlOperation.setOnGetAbortPressed(const Value: TBooleanFunction);
begin
  fOnGetAbortPressed := Value;
  if Assigned (fExpress) then fExpress.OnGetAbortPressed := Value;
end;

procedure TWsdlOperation.setDoExit (AValue : Boolean );
begin
  fDoExit := AValue;
end;

function TWsdlOperation.getWsaTo: String;
var
  oUri, sUri: TIdUri;
begin
  case StubTransport of
    ttHttp:
      begin
        if StubHttpAddress <> '' then
        begin
          sUri := TIdUri.Create(StubHttpAddress);
          if SoapAddress <> '' then
          begin
            oUri := TIdUri.Create(SoapAddress);
            try
              sUri.Protocol := oUri.Protocol;
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
          result := sUri.URI;
          FreeAndNil (sUri);
        end
        else
          result := SoapAddress;
      end;
    ttStomp:
      result := 'queue//getWsaTo_not_yet_implemented';
  end;
end;

procedure TWsdlOperation.InitDelayTime;
begin
  if (DelayTimeMsMin > 0)
  or (DelayTimeMsMax > 0) then
    DelayTimeMs := DelayTimeMsMin + Random (DelayTimeMsMax - DelayTimeMsMin)
  else
    DelayTimeMs := 0;
end;

procedure TWsdlOperation .RefreshBindables ;
  procedure _refresh (aBinder: TWsdlBinder; aList: TBindableList);
  var
    x: Integer;
  begin
    if Assigned (aBinder)
    and Assigned (aList) then
    begin
      for x := 0 to aList.Count - 1 do
      begin
        if Assigned (aList.Bindables[x]) then
        begin
          if aBinder.reqBind.IsAncestorOf(aList.Bindables[x]) then
            aList.Strings[x] := 'Req.' + aList.Bindables[x].FullIndexCaption;
          if aBinder.rpyBind.IsAncestorOf(aList.Bindables[x]) then
            aList.Strings[x] := 'Rpy.' + aList.Bindables[x].FullIndexCaption;
        end;
      end;
    end;
  end;
var
  m: Integer;
begin
  _refresh(self, CorrelationBindables);
  _refresh(self, LogColumns);
  _refresh(self, BindablesWithAddedElement);
  for m := 0 to Messages.Count - 1 do
  begin
    _refresh(Messages.Messages[m], Messages.Messages[m].CorrelationBindables);
    _refresh(Messages.Messages[m], Messages.Messages[m].ColumnXmls);
  end;
end;

procedure TWsdlOperation.ReqBindablesFromString(aString: String);
var
  xXml: TXml;
begin
  if reqBind is TXml then
  begin
    bindRefId := 0;
    xXml := TXml.Create (0, (reqBind as TXml).Xsd);
    try
      xXml.LoadFromString(aString, nil);
      XmlReplyToBindables(xXml, Assigned(Cloned));
    finally
      FreeAndNil (xXml);
    end;
  end;
  if reqBind is TIpmItem then
    (reqBind as TIpmItem).BufferToValues (nil, aString);
end;

procedure TWsdlOperation.ReqBindablesFromWsdlMessage(aMessage: TWsdlMessage);
begin
  CorrelatedMessage := aMessage;
  if isFreeFormat then
  begin
    FreeFormatReq := aMessage.FreeFormatReq;
  end
  else
  begin
    if reqBind is TXml then
    begin
      (reqBind as TXml).ResetValues;
      (reqBind as TXml).LoadValues ((aMessage.reqBind as TXml), True, True);
    end;
    if reqBind is TIpmItem then
    begin
      (reqBind as TIpmItem).LoadValues (aMessage.reqBind as TIpmItem);
    end;
  end;
end;

procedure TWsdlOperation.ReqBindablesToWsdlMessage(aMessage: TWsdlMessage);
begin
  if isFreeFormat then
    aMessage.FreeFormatReq := FreeFormatReq
  else
  begin
    if reqBind is TXml then
    begin
      (aMessage.reqBind as TXml).ResetValues;
      (aMessage.reqBind as TXml).LoadValues ((reqBind as TXml), True, True);
    end;
    if reqBind is TIpmItem then
    begin
      (aMessage.reqBind as TIpmItem).LoadValues (reqBind as TIpmItem);
    end;
  end;
end;

procedure TWsdlOperation.RpyBindablesFromString(aString: String);
var
  xXml: TXml;
begin
  if rpyBind is TXml then
  begin
    bindRefId := 0;
    xXml := TXml.Create (0, (rpyBind as TXml).Xsd);
    try
      xXml.LoadFromString(aString, nil);
      XmlReplyToBindables(xXml, Assigned(Cloned));
    finally
      FreeAndNil (xXml);
    end;
  end;
  if rpyBind is TIpmItem then
    (rpyBind as TIpmItem).BufferToValues (nil, aString);
end;

procedure TWsdlOperation.RpyBindablesFromWsdlMessage(aMessage: TWsdlMessage);
begin
  if isFreeFormat then
    FreeFormatRpy := aMessage.FreeFormatRpy
  else
  begin
    if RpyBind is TXml then
    begin
      (RpyBind as TXml).ResetValues;
      (RpyBind as TXml).LoadValues ((aMessage.RpyBind as TXml), False, True);
    end;
    if RpyBind is TIpmItem then
    begin
      (RpyBind as TIpmItem).LoadValues (aMessage.RpyBind as TIpmItem);
    end;
  end;
end;

procedure TWsdlOperation.RpyBindablesToWsdlMessage(aMessage: TWsdlMessage);
begin
  if isFreeFormat then
    aMessage.FreeFormatRpy := FreeFormatRpy
  else
  begin
    if RpyBind is TXml then
    begin
      (aMessage.RpyBind as TXml).ResetValues;
      (aMessage.RpyBind as TXml).LoadValues ((RpyBind as TXml), False, True);
    end;
    if RpyBind is TIpmItem then
    begin
      (aMessage.RpyBind as TIpmItem).LoadValues (RpyBind as TIpmItem);
    end;
  end;
end;

procedure TWsdlOperation.fltWsaOnRequest;
var
  reqXml, fltXml: TXml;
  xAttr: TXmlAttribute;
begin
  if not wsaEnabled then Exit;
  reqXml := reqWsaXml.Items.XmlItemByTag ['MessageID'];
  if Assigned (reqXml)
  and reqXml.Checked then
  begin
    fltXml := rpyWsaXml.Items.XmlItemByTag ['RelatesTo'];
    if Assigned (fltXml) then
    begin
      fltXml.Value := reqXml.Value;
      fltXml.Checked := True;
      xAttr := fltXml.Attributes.AttributeByTag['RelationshipType'];
      if Assigned (xAttr) then
      begin
        xAttr.Value := Format('http://www.w3.org/%s/addressing/reply', [wsaType]);
        xAttr.Checked := True;
      end;
    end;
  end;
  reqXml := reqWsaXml.Items.XmlItemByTag ['FaultTo'];
  if (not Assigned (reqXml))
  or (not reqXml.Checked) then
    reqXml := reqWsaXml.Items.XmlItemByTag ['ReplyTo'];
  if Assigned (reqXml)
  and reqXml.Checked
  and (reqXml.Items.Count > 0) then
  begin
    fltXml := rpyWsaXml.Items.XmlItemByTag ['To'];
    if Assigned (fltXml) then
    begin
      fltXml.Value := reqXml.Items.XmlItems[0].Value;
      fltXml.Checked := True;
    end;
  end;
end;

function TWsdlOperation.FunctionPrototypes(aAfter: Boolean): TJBStringList;
begin
  result := fExpress.FunctionProtoTypes;
end;

procedure TWsdlOperation.reqWsaOnRequest;
var
  reqXml: TXml;
begin
  if not wsaEnabled then Exit;
  if not Assigned (reqWsaXml) then Exit;
  reqXml := reqWsaXml.Items.XmlItemByTag ['Action'];
  if Assigned (reqXml) then
  begin
    reqXml.Value := SoapAction;
    reqXml.Checked := True;
  end;
  reqXml := reqWsaXml.Items.XmlItemByTag ['From'];
  if Assigned (reqXml) then
  begin
    reqXml := reqXml.Items.XmlItemByTag ['Address'];
    if Assigned (reqXml) then
    begin
      reqXml.Value := 'http://' + _WsdlHostName + ':' + _wsdlPortNumber;
      reqXml.Checked := True;
    end;
  end;
  reqXml := reqWsaXml.Items.XmlItemByTag ['ReplyTo'];
  if Assigned (reqXml) then
  begin
    reqXml := reqXml.Items.XmlItemByTag ['Address'];
    if Assigned (reqXml) then
    begin
      reqXml.Value := 'http://' + _WsdlHostName + ':' + _wsdlPortNumber;
      reqXml.Checked := True;
    end;
  end;
  reqXml := reqWsaXml.Items.XmlItemByTag ['To'];
  if Assigned (reqXml) then
  begin
    reqXml.Value := wsaTo;
    reqXml.Checked := True;
  end;
end;

procedure TWsdlOperation.rpyWsaOnRequest;
var
  reqXml, rpyXml: TXml;
  xAttr: TXmlAttribute;
begin
  if not wsaEnabled then Exit;
  if not Assigned (reqWsaXml) then Exit;
  reqXml := reqWsaXml.Items.XmlItemByTag ['MessageID'];
  if Assigned (reqXml)
  and reqXml.Checked then
  begin
    rpyXml := rpyWsaXml.Items.XmlItemByTag ['RelatesTo'];
    if Assigned (rpyXml) then
    begin
      rpyXml.Value := reqXml.Value;
      rpyXml.Checked := True;
      xAttr := rpyXml.Attributes.AttributeByTag['RelationshipType'];
      if Assigned (xAttr) then
      begin
        xAttr.Value := Format('http://www.w3.org/%s/addressing/reply', [wsaType]);
        xAttr.Checked := True;
      end;
    end;
  end;
  reqXml := reqWsaXml.Items.XmlItemByTag ['ReplyTo'];
  if Assigned (reqXml)
  and reqXml.Checked
  and (reqXml.Items.Count > 0) then
  begin
    rpyXml := rpyWsaXml.Items.XmlItemByTag ['To'];
    if Assigned (rpyXml) then
    begin
      rpyXml.Value := reqXml.Items.XmlItems[0].Value;
      rpyXml.Checked := True;
    end;
  end;
end;

procedure TWsdlOperation.doInvokeOperations;
var
  x, f: Integer;
begin
{}
  if (oldInvokeSpec = 'allOperations')
  or (oldInvokeSpec = 'allRequestors') then
  begin
    invokeList.Clear;
    for x := 0 to allAliasses.Count - 1 do
    begin
      if (allAliasses.Operations[x] <> Self)
      and (allAliasses.Operations[x] <> Self.Cloned)
      and (   (oldInvokeSpec = 'allOperations')
           or (    (oldInvokeSpec = 'allRequestors')
               and (allAliasses.Operations[x].StubAction = saRequest)
              )
          ) then
      begin
        invokeList.Add(allAliasses.Operations[x].Alias);
      end;
    end;
  end;
{}
  for x := invokeList.Count - 1 downto 0 do
  begin
    if Assigned (invokeList.Operations[x]) then
    begin
      if not allAliasses.Find(invokeList.Operations[x].Alias, f) then
      begin
        invokeList.Objects[x].Free;
        invokeList.Delete(x);
      end;
    end;
  end;
  for x := 0 to invokeList.Count - 1 do
  begin
    if allAliasses.Find(invokeList.Strings[x], f)
    and not (allAliasses.Operations[f]._processing) then
    begin
      allAliasses.Operations[f]._processing := True;
      try
        if Assigned (invokeList.Objects[x]) then // sometimes called from gui ...
          invokeList.Objects[x].Free;
        invokeList.Objects[x] := TWsdlOperation.Create(allAliasses.Operations[f]);
        with invokeList.Operations[x] do
        begin
          if Messages.Count > 0 then
          begin
            if isFreeFormat then
            begin
              FreeFormatReq := Messages.Messages[0].FreeFormatReq;
              FreeFormatRpy := Messages.Messages[0].FreeFormatRpy;
            end
            else
            begin
              if reqBind is TIpmItem then
                (reqBind as TIpmItem).LoadValues (Messages.Messages[0].reqBind as TIpmItem);
              if rpyBind is TIpmItem then
                (rpyBind as TIpmItem).LoadValues (Messages.Messages[0].rpyBind as TIpmItem);
              if reqBind is TXml then
                (reqBind as TXml).LoadValues (Messages.Messages[0].reqBind as TXml, True, True);
              if rpyBind is TXml then
                (rpyBind as TXml).LoadValues (Messages.Messages[0].rpyBind as TXml, True, True);
            end;
          end;
        end;
      finally
        allAliasses.Operations[f]._processing := False;
      end;
    end;
  end;
end;

procedure TWsdlOperation.doPromptReply;
begin
//SetForegroundWindow(Application.Handle);
{$ifndef NoGUI}
  xmlUtil.ViewAsXml(rpyBind, False);
{$endif}
end;

procedure TWsdlOperation.doPromptRequest;
begin
//SetForegroundWindow(Application.Handle);
{$ifndef NoGUI}
  xmlUtil.ViewAsXml(reqBind, False);
{$endif}
end;

function TWsdlOperation.endpointConfigAsXml: TXml;
begin
  result := TXml.CreateAsString ('endpointConfig', '');
  case StubTransport of
    ttHttp:
      with result.AddXml(TXml.CreateAsString('Http', '')) do
      begin
        if StubHttpAddress <> '' then
          AddXml (TXml.CreateAsString('Address', StubHttpAddress));
        AddXml (TXml.CreateAsString('Verb', httpVerb));
        if OverruleContentType <> '' then
          AddXml (TXml.CreateAsString('ContentType', OverruleContentType));
        if ContentEncoding <> '' then
          AddXml (TXml.CreateAsString('ContentEncoding', ContentEncoding));
        with AddXml (TXml.CreateAsString('AcceptEncoding', '')) do
        begin
          AddXml (TXml.CreateAsBoolean('deflate', AcceptDeflateEncoding));
          AddXml (TXml.CreateAsBoolean('gzip', AcceptGzipEncoding));
        end;
        if Assigned(StubCustomHeaderXml)
        and (StubCustomHeaderXml.Checked) then
          with AddXml (TXml.CreateAsString ('customHeaders', '')) do
            CopyDownLine(StubCustomHeaderXml, True);
        if (sslVersion <> sslvTLSv1_2)
        or (sslCertificateFile <> '')
        or (sslKeyFile <> '')
        or (sslRootCertificateFile <> '')
        or (sslPassword <> '')
        then
        begin
          with AddXml(TXml.CreateAsString('SSL', '')) do
          begin
            if (sslVersion <> sslvTLSv1_2) then AddXml(TXml.CreateAsString('Version', sslVersionToString(sslVersion)));
            if (sslCertificateFile <> '') then AddXml(TXml.CreateAsString('CertificateFile', sslCertificateFile));
            if (sslKeyFile <> '') then AddXml(TXml.CreateAsString('KeyFile', sslKeyFile));
            if (sslRootCertificateFile <> '') then AddXml(TXml.CreateAsString('RootCertificateFile', sslRootCertificateFile));
            if sslPassword <> '' then
              AddXml (TXml.CreateAsString('Password', Xmlz.EncryptString(sslPassword)));
          end;
        end;
      end;
    ttStomp:
      with result.AddXml(TXml.CreateAsString('Stomp', '')) do
      begin
        AddXml (TXml.CreateAsString('Host', StubStompPutHost));
        AddXml (TXml.CreateAsString('Port', StubStompPutPort));
        if StubStompPutUseCredentials then
        begin
          with AddXml (tXml.CreateAsString('Credentials', '')) do
          begin
            AddXml (TXml.CreateAsString('Name', StubStompPutUserName));
            AddXml (TXml.CreateAsString('Password', Xmlz.EncryptString(StubStompPutPassword)));
          end;
        end;
        AddXml (TXml.CreateAsString('ClientId', StubStompPutClientId));
        AddXml (TXml.CreateAsInteger('Timeout', StubStompTimeOut));
        if Assigned(StubStompHeaderXml)
        and (StubStompHeaderXml.Checked) then
        begin
          with AddXml (TXml.CreateAsString ('stompHeader', '')) do
          begin
            CopyDownLine(StubStompHeaderXml, True);
          end;
        end;
        if Assigned(StubCustomHeaderXml)
        and (StubCustomHeaderXml.Checked) then
          with AddXml (TXml.CreateAsString ('customHeaders', '')) do
            CopyDownLine(StubCustomHeaderXml, True);
      end;
    ttNone:
      result.AddXml(TXml.CreateAsString('None', ''));
  end;

end;

procedure TWsdlOperation.endpointConfigFromXml(aXml: TXml);
var
  x: Integer;
  xXml, yXml: TXml;
begin
  if not Assigned (aXml) then
    raise Exception.Create('endpointConfigfromXml: nil argument');
  if (aXml.Name <> 'endpointConfig')
  and (aXml.Name <> 'Redirect')
  and (aXml.Name <> 'Transport')
  then raise Exception.Create('endpointConfigfromXml: invalid XML' + aXml.Text);
  StubTransport := ttHttp;
  StubHttpAddress := '';
  HttpAddressIsComplete := False;
  httpVerb := 'POST';
  OverruleContentType := '';
  ContentEncoding := 'identity';
  AcceptDeflateEncoding := True;
  AcceptGzipEncoding := True;
  useSsl := False;
  sslVersion := sslvTLSv1_2;
  sslCertificateFile := '';
  sslKeyFile := '';
  sslRootCertificateFile := '';
  sslPassword := '';
  StubStompPutHost := '';
  StubStompPutPort := '';
  StubStompPutUseCredentials := False;
  StubStompPutUserName := '';
  StubStompPutPassword := '';
  StubStompPutClientId := '';
  StubStompTimeOut := 0;
  StubStompHeaderXml.CheckDownline(False);
  StubCustomHeaderXml.Items.Clear;
  smtpHost := '';
  smtpPort := 0;
  for x := 0 to aXml.Items.Count - 1 do
  begin
    with aXml.Items.XmlItems[x] do
    begin
      if Checked then
      begin
        if (Name = 'Http')
        or (Name = 'Https')
        then begin
          StubTransport := ttHttp;
          StubHttpAddress := Items.XmlCheckedValueByTag['Address'];
          HttpAddressIsComplete := Items.XmlCheckedBooleanByTagDef['AddressIsComplete', False];
          httpVerb := UpperCase(Items.XmlCheckedValueByTagDef['Verb', httpVerb]);
          OverruleContentType := Items.XmlCheckedValueByTagDef['ContentType', OverruleContentType];
          ContentEncoding := Items.XmlCheckedValueByTagDef['ContentEncoding', ContentEncoding];
          xXml := Items.XmlCheckedItemByTag['AcceptEncoding'];
          if Assigned (xXml) then
          begin
            AcceptDeflateEncoding := xXml.Items.XmlCheckedBooleanByTagDef ['deflate', AcceptDeflateEncoding];
            AcceptGzipEncoding := xXml.Items.XmlCheckedBooleanByTagDef ['gzip', AcceptGzipEncoding];
          end;
          xXml := Items.XmlCheckedItemByTag['customHeaders'];
          if Assigned (xXml) then
            StubCustomHeaderXml.LoadValues (xXml, True, True);
          useSsl := (UpperCase(Copy (StubHttpAddress, 1, 8)) = 'HTTPS://');
          xXml := Items.XmlCheckedItemByTag['SSL'];
          if Assigned (xXml) then with xXml do
          begin
            if (StubHttpAddress = '') then
              useSsl := True;
            yXml := Items.XmlCheckedItemByTag['Version'];
            if Assigned (yXml) then
              sslVersion := sslVersionFromString(yXml.Value);
            sslCertificateFile := Items.XmlCheckedValueByTag['CertificateFile'];
            sslKeyFile := Items.XmlCheckedValueByTag['KeyFile'];
            sslRootCertificateFile := Items.XmlCheckedValueByTag['RootCertificateFile'];
            yXml := Items.XmlCheckedItemByTag['Password'];
            if Assigned (yXml) then
            begin
              try
                sslPassword :=  Xmlz.DecryptString(yXml.Value);
              except
                sslPassword :=  '';
              end;
            end;
          end;
        end;
        if Name = 'Stomp' then
        begin
          StubTransport := ttStomp;
          StubStompPutHost := Items.XmlCheckedValueByTagDef['Host', 'localhost'];
          StubStompPutPort := Items.XmlCheckedValueByTagDef['Port', '61613'];
          xXml := Items.XmlCheckedItemByTag['Credentials'];
          if Assigned (xXml) then
          begin
            StubStompPutUseCredentials := True;
            StubStompPutUserName := xXml.Items.XmlValueByTag['Name'];
            StubStompPutPassword := xmlz.DecryptString (xXml.Items.XmlValueByTag['Password']);
          end;
          StubStompPutClientId := Items.XmlCheckedValueByTag['ClientId'];
          StubStompTimeOut := Items.XmlCheckedIntegerByTag['Timeout'];
          xXml := Items.XmlCheckedItemByTag['stompHeader'];
          if Assigned (xXml) then
            StubStompHeaderXml.LoadValues (xXml, False, True);
          xXml := Items.XmlCheckedItemByTag['customHeaders'];
          if Assigned (xXml) then
            StubCustomHeaderXml.LoadValues (xXml, True, True);
        end;
        if Name = 'None' then
        begin
          StubTransport := ttNone;
        end;
      end;
    end;
  end;
end;

procedure TWsdlOperation.BindChecker(aBind: TCustomBindable);
var
  swapName: String;
  swapParent: TCustomBindable;
begin
  swapParent := aBind.Parent;
  swapName := aBind.Name;
  aBind.Parent := nil;
  aBind.Name := 'Self';
  try
    try
      FreeAndNil(fExpressChecker);
      fExpressChecker := TExpress.Create (self);
      fExpressChecker.Context := Self;
      fExpressChecker.OnNeedData := NeedStamperData;
      fExpressChecker.OnError := fOnError;
      fLineNumber := 0;
      fExpressChecker.Database := _WsdlDbsConnector;
      aBind.Bind ('', fExpressChecker, 1);
      fExpressChecker.BindBoolean('Bind_.Checker', aBind.fChecked);
      BindCheckerFunction ('dbLookUp', @dbLookUp, SFSSSS, '(aTable, aValueColumn, aReferenceColumn, aReferenceValue)');
      BindCheckerFunction ('DecEnvNumber', @decVarNumber, XFOS, '(aKey)');
      BindCheckerFunction ('FormatDate', @FormatDateX, SFDS, '(aDate, aMask)');
      BindCheckerFunction ('GetEnvNumber', @getVarNumber, XFOS, '(aKey)');
      BindCheckerFunction ('GetEnvNumberDef', @getVarNumberDef, XFOSX, '(aKey, aDefault)');
      BindCheckerFunction ('GetEnvVar', @getVar, SFOS, '(aKey)');
      BindCheckerFunction ('GetEnvVarDef', @getVarDef, SFOSS, '(aKey, aDefault)');
      BindCheckerFunction ('GetEnvVarDefT', @getVarDefT, SFOSSSX, '(aKey, aDefault, aSeparator, aIndex)');
      BindCheckerFunction ('HostName', @GetHostName, SFV, '()');
      BindCheckerFunction ('ifthen', @ifThenString, SFBSS, '(aCondition, aTrueString, aFalseString)');
      BindCheckerFunction ('IncEnvNumber', @incVarNumber, XFOS, '(aKey)');
      BindCheckerFunction ('Latin1Str', @Latin1, SFS, '(aString)');
      BindCheckerFunction ('NameCaseStr', @StrToNameCase, SFS, '(aString)');
      BindCheckerFunction ('LengthStr', @LengthX, XFS, '(aString)');
      BindCheckerFunction ('LowercaseStr', @LowerCaseStr, SFS, '(aString)');
      BindCheckerFunction ('NewLine', @xNewLine, SFV, '()');
      BindCheckerFunction ('Tab', @xTab, SFV, '()');
      BindCheckerFunction ('NumberToStr', @FloatToStr, SFX, '(aNumber)');
      BindCheckerFunction ('Occurrences', @OccurrencesX, XFG, '(aElement)');
      BindCheckerFunction ('Random', @RandomX, XFXX, '(aLow, aHigh)');
      BindCheckerFunction ('RegExprSafeStr', @RegExprSafeStr, SFS, '(aString)');
      BindCheckerFunction ('Rounded', @RoundedX, XFXX, '(aNumber, aDecimals)');
      BindCheckerFunction ('SetEnvNumber', @setEnvNumber, XFOSX, '(aKey, aNumber)');
      BindCheckerFunction ('SetEnvVar', @setEnvVar, SFOSS, '(aKey, aValue)');
      BindCheckerFunction ('StrFromClipboard', @StrFromClipboard, SFV, '()');
      BindCheckerFunction ('StrHasRegExpr', @StringHasRegExpr, SFSS, '(aString, aRegExpr)');
      BindCheckerFunction ('StrMatchesRegExpr', @StringMatchesRegExpr, SFSS, '(aString, aRegExpr)');
      BindCheckerFunction ('StrOfChar', @xStringOfChar, SFSX, '(aChar, aNumber)');
      BindCheckerFunction ('StrToDate', @StrToDateX, DFS, '(aString)');
      BindCheckerFunction ('StrToDateTime', @XmlToDateTime, DFS, '(aString)');
      BindCheckerFunction ('StrToNumber', @StrToFloatX, XFS, '(aString)');
      BindCheckerFunction ('SubStr', @SubStringX, SFSXX, '(aString, aStart, aLength)');
      BindCheckerFunction ('SqlQuotedStr', @sqlQuotedString, SFS, '(aString)');
      BindCheckerFunction ('UppercaseStr', @UpperCaseStr, SFS, '(aString)');
      BindCheckerFunction ('OperationCount', @xsdOperationCount, XFOV, '()');
      BindCheckerFunction ('UserName', @wsdlUserName, SFV, '()');
      BindCheckerFunction ('StrToFile', @xmlio.SaveStringToFile, VFSS, '(aFileName, aString)');
      BindCheckerFunction ('OperationName', @wsdlOperationName, SFOV, '()');
    except
    end;
  finally
    aBind.Parent := swapParent;
    aBind.Name := swapName;
  end;
end;

procedure TWsdlOperation.BindCheckerFunction(Id: String; Adr: Pointer;
  Token: Integer; ArgumentsPrototype: String);
begin
  fExpressChecker.BindFunction (Id, Adr, Token, ArgumentsPrototype);
end;

procedure TWsdlOperation.BindStamper;
begin
  try
    FreeAndNil(fExpressStamper);
    fExpressStamper := TExpress.Create (self);
    fExpressStamper.Context := Self;
    fExpressStamper.OnNeedData := NeedStamperData;
    fExpressStamper.OnError := fOnError;
//      fExpress.OnError := ExpressError;
//      fExpress.OnHaveData := HaveData;
    fLineNumber := 0;
    fExpressStamper.Database := _WsdlDbsConnector;
    if Assigned (reqBind) then
      reqBind.Bind ('Req', fExpressStamper, 1);
    if Assigned (rpyBind) then
      rpyBind.Bind ('Rpy', fExpressStamper, 1);
    fExpressStamper.BindString('stamper.uwa', fExpressStamper.uwaString);
    BindStamperFunction ('DateTimeToJulianStr', @DateTimeToJulianStr, SFD, '(aDateTime)');
    BindStamperFunction ('DateTimeToTandemJulianStr', @DateTimeToTandemJulianStr, SFD, '(aDateTime)');
    BindStamperFunction ('DateTimeToXml', @xsdDateTime, SFD, '(aDateTime)');
    BindStamperFunction ('DateTimeToUnix', @xDateTimeToUnix, XFD, '(aDateTime)');
    BindStamperFunction ('dbLookUp', @dbLookUp, SFSSSS, '(aTable, aValueColumn, aReferenceColumn, aReferenceValue)');
    BindStamperFunction ('DecEnvNumber', @decVarNumber, XFOS, '(aKey)');
    BindStamperFunction ('FormatDate', @FormatDateX, SFDS, '(aDate, aMask)');
    BindStamperFunction ('GetEnvNumber', @getVarNumber, XFOS, '(aKey)');
    BindStamperFunction ('GetEnvNumberDef', @getVarNumberDef, XFOSX, '(aKey, aDefault)');
    BindStamperFunction ('GetEnvVar', @getVar, SFOS, '(aKey)');
    BindStamperFunction ('GetEnvVarDef', @getVarDef, SFOSS, '(aKey, aDefault)');
    BindStamperFunction ('GetEnvVarDefT', @getVarDefT, SFOSSSX, '(aKey, aDefault, aSeparator, aIndex)');
    BindStamperFunction ('HostName', @GetHostName, SFV, '()');
    BindStamperFunction ('ifthen', @ifThenString, SFBSS, '(aCondition, aTrueString, aFalseString)');
    BindStamperFunction ('IncEnvNumber', @incVarNumber, XFOS, '(aKey)');
    BindStamperFunction ('Latin1Str', @Latin1, SFS, '(aString)');
    BindStamperFunction ('NameCaseStr', @StrToNameCase, SFS, '(aString)');
    BindStamperFunction ('LengthStr', @LengthX, XFS, '(aString)');
    BindStamperFunction ('LowercaseStr', @LowerCaseStr, SFS, '(aString)');
    BindStamperFunction ('MD5', @MD5, SFS, '(aString)');
    BindStamperFunction ('NewLine', @xNewLine, SFV, '()');
    BindStamperFunction ('Tab', @xTab, SFV, '()');
    BindStamperFunction ('NumberToStr', @FloatToStr, SFX, '(aNumber)');
    BindStamperFunction ('NowAsStr', @xsdNowAsDateTime, SFV, '()');
    BindStamperFunction ('Occurrences', @OccurrencesX, XFG, '(aElement)');
    BindStamperFunction ('Random', @RandomX, XFXX, '(aLow, aHigh)');
    BindStamperFunction ('Rounded', @RoundedX, XFXX, '(aNumber, aDecimals)');
    BindStamperFunction ('SetEnvNumber', @setEnvNumber, XFOSX, '(aKey, aNumber)');
    BindStamperFunction ('SetEnvVar', @setEnvVar, SFOSS, '(aKey, aValue)');
    BindStamperFunction ('SHA1', @SHA1, SFS, '(aString)');
    BindStamperFunction ('ShowMessage', @SjowMessage, VFS, '(aString)');
    BindStamperFunction ('SiebelNowAsStr', @sblNowAsDateTime, SFV, '()');
    BindStamperFunction ('SiebelTodayAsStr', @sblTodayAsDate, SFV, '()');
    BindStamperFunction ('SqlQuotedStr', @sqlQuotedString, SFS, '(aString)');
    BindStamperFunction ('StrFromClipboard', @StrFromClipboard, SFV, '()');
    BindStamperFunction ('StrHasRegExpr', @StringHasRegExpr, SFSS, '(aString, aRegExpr)');
    BindStamperFunction ('StrMatchesRegExpr', @StringMatchesRegExpr, SFSS, '(aString, aRegExpr)');
    BindStamperFunction ('StrOfChar', @xStringOfChar, SFSX, '(aChar, aNumber)');
    BindStamperFunction ('StrToDate', @StrToDateX, DFS, '(aString)');
    BindStamperFunction ('StrToDateTime', @XmlToDateTime, DFS, '(aString)');
    BindStamperFunction ('StrToNumber', @StrToFloatX, XFS, '(aString)');
    BindStamperFunction ('SubStr', @SubStringX, SFSXX, '(aString, aStart, aLength)');
    BindStamperFunction ('TodayAsStr', @xsdTodayAsDate, SFV, '()');
    BindStamperFunction ('UnixToDateTime', @xUnixToDateTime, DFX, '(aUnixDateTime)');
    BindStamperFunction ('UppercaseStr', @UpperCaseStr, SFS, '(aString)');
    BindStamperFunction ('OperationCount', @xsdOperationCount, XFOV, '()');
    BindStamperFunction ('UserName', @wsdlUserName, SFV, '()');
    BindStamperFunction ('StrToFile', @xmlio.SaveStringToFile, VFSS, '(aFileName, aString)');
    BindStamperFunction ('OperationName', @wsdlOperationName, SFOV, '()');
  except
  end
end;

procedure TWsdlOperation.BindStamperFunction(Id: String; Adr: Pointer;
  Token: Integer; ArgumentsPrototype: String);
begin
  fExpressStamper.BindFunction (Id, Adr, Token, ArgumentsPrototype);
end;

procedure TWsdlOperation.BufferToValuesErrorFound(aMessage: String;
  aObject: TObject);
begin
  if aObject is TCustomBindable then with aObject as TCustomBindable do
    Value := aMessage;
end;

function TWsdlOperation.getApiReplyMediaType: String;
var
  x, y: Integer;
  yXml: TXml;
begin
  result := '';
  // depending on openApi version, answer can be found on first or on second level
  if isOpenApiService then with rpyBind as TXml do
  begin
    for x := 0 to Items.Count - 1 do with Items.XmlItems[x] do
    begin
      if Checked then
        result := Xsd.MediaType;
      if result <> '' then
        Exit;
      for y := 0 to Items.Count - 1 do with Items.XmlItems[y] do
      begin
        if Checked then
          result := Xsd.MediaType;
        if result <> '' then
          Exit;
      end;
    end;
  end;
end;

function TWsdlOperation.getConsumesOnlyJson: Boolean;
begin
  result := (Pos ('json', Consumes) > 0)
        and (Pos ('xml', Consumes) < 1)
          ;
end;

function TWsdlOperation.getConsumesOnlyXml: Boolean;
begin
  result := (Pos ('xml', Consumes) > 0)
        and (Pos ('json', Consumes) < 1)
          ;
end;

function TWsdlOperation .getDoExit : Boolean ;
begin
  result := fDoExit;
end;

function TWsdlOperation.getHost: String;
begin
  result := '';
  if Assigned (Wsdl) then
    result := wsdl.Host;
end;

function TWsdlOperation.getIsFreeFormat : Boolean ;
begin
  result := Assigned (WsdlService)
        and (WsdlService.DescriptionType in [ipmDTFreeFormat]);
end;


procedure TWsdlOperation.NeedStamperData(Sender: TObject; var MoreData: Boolean;
  var Data: String);
begin
  if fLineNumber = 1 then
    MoreData := False
  else
  begin
    Data := fStamperStatement;
    Inc (fLineNumber);
  end;
end;

function TWsdlOperation.OptionsAsXml: TXml;
var
  x: Integer;
begin
  result := TXml.CreateAsString ('operationOptions','');
  with result do
  begin
    with result.AddXml (TXml.CreateAsString('schemaValidation', '')) do
    begin
      with AddXml (TXml.CreateAsString('inboundRequests', '')) do
      begin
        case inboundRequestSchemaValidationType of
          svAccordingProject: AddXml (TXml.CreateAsString ('accordingProject', ''));
          svNo: AddXml (TXml.CreateAsString ('noSchemaValidation', ''));
          svReportOnly: AddXml (TXml.CreateAsString ('reportSchemaViolations', ''));
          svRaiseException:
            AddXml (TXml.CreateAsString ('raiseExceptionOnViolation', ''))
             .AddXml (TXml.CreateAsInteger('responseCode', schemaValidationVioloationHttpResponseCode));
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
    with result.AddXml (Txml.CreateAsString('OnRequestViolatingAddressPath', '')) do
    begin
      if OnRequestViolatingAddressPath = rvsDefault then
        AddXml (TXml.CreateAsString('UseProjectDefault',''));
      if OnRequestViolatingAddressPath = rvsContinue then
        AddXml (TXml.CreateAsString('Continue',''));
      if OnRequestViolatingAddressPath = rvsRaiseErrorMessage then
        AddXml (TXml.CreateAsString('RaiseErrorMessage',''));
      if OnRequestViolatingAddressPath = rvsAddRemark then
        AddXml (TXml.CreateAsString('AddRemark',''));
    end;
    with AddXml (TXml.CreateAsString('ResolveAliasses', '')) do
    begin
      AddXml(TXml.CreateAsBoolean('BeforeOutboundRequest', resolveRequestAliasses));
      AddXml(TXml.CreateAsBoolean('BeforeOutboundReply', resolveReplyAliasses));
    end;
    with AddXml (TXml.CreateAsString('scripts', '')) do
    begin
      with AddXml (TXml.CreateAsString('invoke', '')) do
      begin
        AddXml(TXml.CreateAsBoolean('requestInfo', invokeRequestInfo));
        AddXml(TXml.CreateAsBoolean('replyInfo', invokeReplyInfo));
        with AddXml (TXml.CreateAsString('operations', '')) do
        begin
          for x := 0 to invokeList.Count - 1 do
          begin
            AddXml (TXml.CreateAsString('name', invokeList.Strings[x]));
          end;
        end;
      end;
    end;
    with AddXml(TXml.CreateAsString('ReadReplyFromFile', '')) do
      CopyDownLine(ReadReplyFromFileXml, False);
    if onFetchLogFromCloud <> '' then
      with AddXml (TXml.CreateAsString('events', '')) do
        AddXml (TXml.CreateAsString('onFetchLogFromCloud', onFetchLogFromCloud));
  end;
end;

procedure TWsdlOperation.OptionsFromXml(aXml: TXml);
var
  xXml, yXml, iXml: TXml;
  x, y: Integer;
begin
  if not Assigned (aXml) then raise Exception.Create('operationOptionsFromXml: No XML assigned');
  if not (aXml.Name = 'operationOptions') then raise Exception.Create('operationOptionsFromXml: Illegal XML: ' + aXml.Text);
  oldInvokeSpec := 'none';
  doReadReplyFromFile := False;
  ReadReplyFromFileXml.Items.Clear;
  onFetchLogFromCloud := '';
  inboundRequestSchemaValidationType := svAccordingProject;
  outboundReplySchemaValidationType := svAccordingProject;
  outboundRequestSchemaValidationType := svAccordingProject;
  inboundReplySchemaValidationType := svAccordingProject;
  schemaValidationVioloationHttpResponseCode := 417;
  invokeRequestInfo := False;
  invokeReplyInfo := False;
  xXml := aXml.Items.XmlCheckedItemByTag ['schemaValidation'];
  if Assigned (xXml) then
  begin
    yXml := xXml.Items.XmlCheckedItemByTag ['inboundRequests'];
    if Assigned (yXml) then with yXml.Items do
    begin
      if Assigned (XmlCheckedItemByTag ['accordingProject']) then inboundRequestSchemaValidationType := svAccordingProject;
      if Assigned (XmlCheckedItemByTag ['noSchemaValidation']) then inboundRequestSchemaValidationType := svNo;
      if Assigned (XmlCheckedItemByTag ['reportSchemaViolations']) then inboundRequestSchemaValidationType := svReportOnly;
      iXml := yXml.Items.XmlCheckedItemByTag ['raiseExceptionOnViolation'];
      if Assigned (iXml) then with iXml.Items do
      begin
        inboundRequestSchemaValidationType := svRaiseException;
        schemaValidationVioloationHttpResponseCode := XmlCheckedIntegerByTagDef['responseCode', schemaValidationVioloationHttpResponseCode];
      end;
    end;
    yXml := xXml.Items.XmlCheckedItemByTag ['outboundReplies'];
    if Assigned (yXml) then with yXml.Items do
    begin
      if Assigned (XmlCheckedItemByTag ['accordingProject']) then outboundReplySchemaValidationType := svAccordingProject;
      if Assigned (XmlCheckedItemByTag ['noSchemaValidation']) then outboundReplySchemaValidationType := svNo;
      if Assigned (XmlCheckedItemByTag ['reportSchemaViolations']) then outboundReplySchemaValidationType := svReportOnly;
      if Assigned (XmlCheckedItemByTag ['raiseExceptionOnViolation']) then outboundReplySchemaValidationType := svRaiseException;
    end;
    yXml := xXml.Items.XmlCheckedItemByTag ['outboundRequests'];
    if Assigned (yXml) then with yXml.Items do
    begin
      if Assigned (XmlCheckedItemByTag ['accordingProject']) then outboundRequestSchemaValidationType := svAccordingProject;
      if Assigned (XmlCheckedItemByTag ['noSchemaValidation']) then outboundRequestSchemaValidationType := svNo;
      if Assigned (XmlCheckedItemByTag ['reportSchemaViolations']) then outboundRequestSchemaValidationType := svReportOnly;
      if Assigned (XmlCheckedItemByTag ['raiseExceptionOnViolation']) then outboundRequestSchemaValidationType := svRaiseException;
    end;
    yXml := xXml.Items.XmlCheckedItemByTag ['inboundReplies'];
    if Assigned (yXml) then with yXml.Items do
    begin
      if Assigned (XmlCheckedItemByTag ['accordingProject']) then inboundReplySchemaValidationType := svAccordingProject;
      if Assigned (XmlCheckedItemByTag ['noSchemaValidation']) then inboundReplySchemaValidationType := svNo;
      if Assigned (XmlCheckedItemByTag ['reportSchemaViolations']) then inboundReplySchemaValidationType := svReportOnly;
      if Assigned (XmlCheckedItemByTag ['raiseExceptionOnViolation']) then inboundReplySchemaValidationType := svRaiseException;
    end;
  end;
  xXml := aXml.Items.XmlCheckedItemByTag ['OnRequestViolatingAddressPath'];
  OnRequestViolatingAddressPath := rvsDefault;
  xXml := aXml.Items.XmlCheckedItemByTag['ResolveAliasses'];
  resolveRequestAliasses := True;
  resolveReplyAliasses := True;
  if Assigned (xXml) then
  begin
    resolveRequestAliasses := xXml.Items.XmlBooleanByTagDef['BeforeOutboundRequest', resolveRequestAliasses];
    resolveReplyAliasses := xXml.Items.XmlBooleanByTagDef['BeforeOutboundReply', resolveReplyAliasses];
  end;
  xXml := aXml.Items.XmlCheckedItemByTag['scripts'];
  if Assigned (xXml) then
  begin
    oldInvokeSpec := xXml.Items.XmlCheckedValueByTagDef['invoke', oldInvokeSpec];
    invokeList.Clear;
    xXml := xXml.Items.XmlCheckedItemByTag['invoke'];
    if Assigned (xXml) then
    begin
      invokeRequestInfo := xXml.Items.XmlBooleanByTagDef['requestInfo', False];
      invokeReplyInfo := xXml.Items.XmlBooleanByTagDef['replyInfo', False];
      yXml := xXml.Items.XmlCheckedItemByTag['operations'];
      if Assigned (yXml) then
      begin
        for y := 0 to yXml.Items.Count - 1 do
          if (yXml.Items.XmlItems[y].Name = 'name')
          and (yXml.Items.XmlItems[y].Checked) then
            invokeList.Add(yXml.Items.XmlItems[y].Value);
      end;
    end;
  end;
  xXml := aXml.Items.XmlCheckedItemByTag['events'];
  if Assigned (xXml) then
  begin
    onFetchLogFromCloud := xXml.Items.XmlValueByTagDef['onFetchLogFromCloud', onFetchLogFromCloud];
  end;
  xXml := aXml.Items.XmlCheckedItemByTag['ReadReplyFromFile'];
  if Assigned (xXml) then
  begin
    ReadReplyFromFileXml.CopyDownLine(xXml, True);
    doReadReplyFromFile := xXml.Items.XmlCheckedBooleanByTag['Enabled'];
  end;
end;

function TWsdlOperation.InformationAsXml: TXml;
var
  x: Integer;
begin
  result := TXml.CreateAsString('operationInformation', '');
  with result do
  begin
    AddXml(Txml.CreateAsString('name', Alias));
    AddXml(Txml.CreateAsString('fileAlias', './W/'
                                          + Wsdl.FileAlias
                                          + '/S/'
                                          + WsdlService.FileAlias
                                          + '/O/'
                                          + FileAlias
                                          )
                              );
    AddXml(Txml.CreateAsString('descriptionFrom', Wsdl.FileName));
    case StubAction of
      saStub: AddXml(TXml.CreateAsString('direction', 'inbound'));
      saRequest: AddXml(TXml.CreateAsString('direction', 'outbound'));
      saForward: AddXml(TXml.CreateAsString('direction', 'forward'));
    end;
    if Assigned (Wsdl.Servers)
    and (Wsdl.Servers.Count > 0) then
    with Wsdl.Servers do
    with AddXml(Txml.CreateAsString('servers', '')) do
    begin
      jsonType := jsonArray;
      for x := 0 to Count - 1 do
        AddXml(Txml.CreateAsString('_', Strings[x]));
    end;
    AddXml(Txml.CreateAsString('service', WsdlService.Name));
    if WsdlService.logPathFormat <> '' then
      AddXml(Txml.CreateAsString('path', WsdlService.logPathFormat));
    AddXml(Txml.CreateAsString('verb', httpVerb));
    if SoapAddress <> '' then
      AddXml (TXml.CreateAsString('soapAddress', SoapAddress));
    AddXml (TXml.CreateAsInteger('messageCandidates', Messages.Count));
    if CorrelationBindables.Count > 0 then
    begin
      with AddXml (TXml.CreateAsString('correlationElements', '')) do
      begin
        jsonType := jsonArray;
        for x := 0 to CorrelationBindables.Count - 1 do
        begin
          AddXml (TXml.CreateAsString('_', CorrelationBindables.Strings[x]));
        end;
      end;
    end;
    if Assigned (BeforeScriptLines)
    and (BeforeScriptLines.Count > 0) then
      AddXml (TXml.CreateAsString('beforeScript', BeforeScriptLines.Text));
    if Assigned (AfterScriptLines)
    and (AfterScriptLines.Count > 0) then
      AddXml (TXml.CreateAsString('afterScript', AfterScriptLines.Text));
  end;
end;

procedure TWsdlOperation.PrepareRpyStamper (aBind: TCustomBindable);
begin
//fStamperStatement := 'Rpy.' + aBind.FullIndexCaption + aBind.Value + ';';
  fStamperStatement := 'stamper.uwa' + aBind.Value + ';';
  fLineNumber := 0;
  fExpressStamper.OnError := fOnError;
  fExpressStamper.Prepare;
end;

procedure TWsdlOperation.ExecuteRpyStampers;
  procedure _Stamp (aBindable: TCustomBindable);
  var
    x: Integer;
  begin
    if (   (aBindable is TXml)
        or (aBindable is TXmlAttribute)
       )
    and (not aBindable.Checked) then
      exit;
    if resolveReplyAliasses then
      aBindable.Value := resolveAliasses (aBindable.Value, true);
    if aBindable.isExpression then
    begin
      PrepareRpyStamper (aBindable);
      fExpressStamper.Execute;
      aBindable.Value := fExpressStamper.uwaString;
    end;
    if aBindable is TXml then
    begin
      for x := 0 to (aBindable as TXml).Attributes.Count - 1 do
        _Stamp ((aBindable as TXml).Attributes.XmlAttributes[x]);
    end;
    if not (aBindable is TXmlAttribute) then
    begin
      for x := 0 to aBindable.Children.Count - 1 do
        _Stamp (aBindable.Children.Bindables[x]);
      if aBindable.isEvaluation then
      begin
        PrepareChecker(aBindable);
        fExpressChecker.Execute;
      end;
    end;
  end;
begin
  _Stamp (rpyBind);
end;

procedure TWsdlOperation.CheckScript (aStringList: TJBStringList; aOnError: TOnErrorEvent);
begin
  fExpress.CheckScript(aStringList, aOnError);
end;

procedure TWsdlOperation.Execute (aStringList: TJBStringList; aOnError: TOnErrorEvent);
begin
  if Assigned (aStringList)
  and (aStringList.Count > 0) then
    fExpress.ExecuteScript(aStringList, aOnError);
end;

procedure TWsdlOperation.ExecuteReqStampers;
  procedure _Stamp (aBindable: TCustomBindable);
  var
    x: Integer;
  begin
    if (   (aBindable is TXml)
        or (aBindable is TXmlAttribute)
       )
    and (not aBindable.Checked) then
      exit;
    if resolveRequestAliasses then
      aBindable.Value := xmlio.resolveAliasses(aBindable.Value, true);
    if aBindable.isExpression then
    begin
      PrepareReqStamper (aBindable);
      fExpressStamper.Execute;
      aBindable.Value := fExpressStamper.uwaString;
    end;
    if aBindable is TXml then
    begin
      for x := 0 to (aBindable as TXml).Attributes.Count - 1 do
        _Stamp ((aBindable as TXml).Attributes.XmlAttributes[x]);
    end;
    if not (aBindable is TXmlAttribute) then
    begin
      for x := 0 to aBindable.Children.Count - 1 do
        _Stamp (aBindable.Children.Bindables[x]);
      if aBindable.isEvaluation then
      begin
        PrepareChecker(aBindable);
        fExpressChecker.Execute;
      end;
    end;
  end;
begin
  _Stamp (ReqBind);
end;

procedure TWsdlOperation.PrepareReqStamper(aBind: TCustomBindable);
begin
//fStamperStatement := 'Req.' + aBind.FullIndexCaption + aBind.Value + ';';
  fStamperStatement := 'stamper.uwa' + aBind.Value + ';';
  fLineNumber := 0;
  fExpressStamper.OnError := fOnError;
  fExpressStamper.Prepare;
end;

procedure TWsdlOperation.ReleaseLock;
begin
  if doOperationLock then fLock.Release;
end;

function TWsdlOperation .ReadReplyFromFileName : String ;
var
  x, p: Integer;
  xFormat: String;
begin
  result := '';
  xFormat := ReadReplyFromFileXml.Items.XmlCheckedValueByTag['FileNameFormat'];
  for x := 0 to CorrelationBindables.Count - 1 do
  begin
    p := Pos('%s', xFormat);
    if p > 0 then
    begin
      result := result
              + Copy (xFormat, 1, p - 1)
              + ifthen(Assigned ( CorrelationBindables.Bindables[x]),  CorrelationBindables.Bindables[x].Value)
              ;
      xFormat := Copy(xFormat, p + 2, MaxInt);
    end;
  end;
  result := result + xFormat;
end;

function TWsdlOperation.DefaultReadReplyFromFileName: String;
begin
  result := ReadReplyFromFileXml.Items.XmlCheckedValueByTag['DefaultFileName'];
end;

procedure TWsdlOperation.ReadReplyFromFile;
var
  xFileName, xString: String;
begin
  xFileName := ReadReplyFromFileName;
  if not FileExists(xFileName) then
    xFileName := DefaultReadReplyFromFileName;
  xString := xmlio.ReadStringFromFile(xFileName, nil);
  ReplyStringToBindables(xString);
end;

procedure TWsdlOperation.ReplyStringToBindables(aReply: String);
var
  xXml: TXml;
begin
  if WsdlService.DescriptionType in [ipmDTFreeFormat] then
    FreeFormatRpy := aReply
  else
  begin
    if rpyBind is TIpmItem then
      (rpyBind as TIpmItem).BufferToValues (BufferToValuesErrorFound, aReply)
    else
    begin
      xXml := TXml.Create;
      try
        xXml.LoadFromString(aReply, nil);
        XmlReplyToBindables (xXml, Assigned(Cloned));
      finally
        xXml.Free;
      end;
    end;
  end;
end;

procedure TWsdlOperation.RequestStringToBindables(aRequest: String);
var
  xXml: TXml;
begin
  if WsdlService.DescriptionType in [ipmDTFreeFormat] then
    FreeFormatReq := aRequest
  else
  begin
    if reqBind is TIpmItem then
      (reqBind as TIpmItem).BufferToValues (nil, aRequest)
    else
    begin
      xXml := TXml.Create;
      try
        xXml.LoadFromString(aRequest, nil);
        XmlRequestToBindables (xXml, Assigned (Cloned));
      finally
        xXml.Free;
      end;
    end;
  end;
end;

function TWsdlOperation.getIsOneWay: Boolean;
begin
  result := False;
  if Assigned (self)
  and Assigned (WsdlService) then
  begin
    if WsdlService.DescriptionType = ipmDTCobol then
      result := ((rpyBind as TIpmItem).Bytes = 0)
    else
      if WsdlService.DescriptionType in [ipmDTFreeFormat] then
        result := (FreeFormatRpy = '')
      else
        result := (rpyXsd.sType.ElementDefs.Count = 0)
              ;
  end;
end;

function TWsdlOperation.getIsOpenApiService: Boolean;
begin
  result := Assigned (Wsdl) and Wsdl.isOpenApiService;
end;

function TWsdlOperation.getisSoapService: Boolean;
begin
  result := Assigned (Wsdl)
        and Wsdl.isSoapService;
end;

function TWsdlOperation.getOpenApiVersion: String;
begin
  if Assigned (Wsdl) then
  begin
    if wsdl.OpenApiVersion = '' then
      result := '2.0'
    else
      result := Wsdl.OpenApiVersion;
  end
  else
    result := '2.0';
end;

{ TWsdlPart }

constructor TWsdlPart.Create;
begin
  Xsd := nil;
end;

destructor TWsdlPart.Destroy;
begin
{ FreeAndNil (Xml);
  already done in Message
}
  inherited;
end;

procedure TWsdlPart.LinkToXsd (aXsds: TXsdList; aWsdl: TWsdl);
var
  f: Integer;
  xTypeDef: TXsdDataType;
begin
  if _ElementName <> '' then
  begin
    if not aXsds.Find(_ElementName, f) then
      raise Exception.CreateFmt('Element %s not found at part %s', [_ElementName, Name]);
    Xsd := aXsds.Xsds[f];
  end
  else
  begin
    if aWsdl.XsdDescr.TypeDefs.Find(_TypeName, f) then
    begin
      xTypedef := aWsdl.XsdDescr.TypeDefs.XsdDataTypes[f];
      Xsd := TXsd.Create(aWsdl.XsdDescr);
      aWsdl.XsdDescr.Garbage.AddObject('', Xsd);
      Xsd.sType := xTypeDef;
      Xsd.ElementName := self.Name;
      Xsd.ElementNameSpace := self.NameSpace;
    end
    else
    begin
      if not aXsds.Find(_TypeName, f) then // lets help the poor people who set up wslds
      begin
      {$ifndef NoGUI}
        XmlUtil.presentAsText ( 'TWsdlPart.LinkToXsd (aXsds: TXsdList; aWsdl: TWsdl)'
                              , aWsdl.XsdDescr.ReadFileNames.Text
                              );
      {$endif}
        raise Exception.CreateFmt('Element(%s)nor TypeDef(%s) found for part %s', [_ElementName, _TypeName, Name]);
      end;
      Xsd := aXsds.Xsds[f];
    end;
  end;
end;

{ TWsdlParts }

procedure TWsdlParts.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Parts [x].Free;
  inherited;
end;

function TWsdlParts.GetPart(Index: integer): TWsdlPart;
begin
  result := TWsdlPart (Objects [index]);
end;

{ TWsdlOperations }

procedure TWsdlOperations.Clean;
var
  o: Integer;
begin
  for o := 0 to Count - 1 do with Operations[o] do
    Clean;
end;

procedure TWsdlOperations.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
  begin
    if Assigned (Operations[x]) then
      Operations [x].Free;
    Objects[x] := nil;
  end;
  inherited;
end;

procedure TWsdlOperations.ClearListOnly;
begin
  inherited Clear;
end;

function TWsdlOperations.GetOperation(Index: integer): TWsdlOperation;
begin
  result := TWsdlOperation (Objects [index]);
end;

function TWsdlOperations.SaveFind(aString: String; aIndex: Integer): Boolean;
begin
  AcquireLock;
  try
    result := Find(aSTring, aIndex);
  finally
    ReleaseLock;
  end;
end;

function TWsdlOperations .FindOnAliasName (aAlias : String ): TWsdlOperation ;
var
  x: Integer;
begin
  result := nil;
  for x := 0 to Count - 1 do
  begin
    if Operations[x].Alias = aAlias then
    begin
      result := Operations[x];
      Exit;
    end;
  end;
end;

function TWsdlOperations .FindOnOperationName (aName : String
  ): TWsdlOperation ;
var
  x: Integer;
begin
  result := nil;
  for x := 0 to Count - 1 do
  begin
    if Operations[x].reqTagName = aName then
    begin
      result := Operations[x];
      Exit;
    end;
  end;
end;

{ TWsdlMessage }

constructor TWsdlMsgDescr.Create (aWsdl: TWsdl);
begin
  aWsdl.XsdDescr.Garbage.AddObject('', self);
  Xsd := TXsd.Create(aWsdl.XsdDescr);
  aWsdl.XsdDescr.Garbage.AddObject('', Xsd);
  Xsd.sType := TXsdDataType.Create(aWsdl.XsdDescr);
  aWsdl.XsdDescr.Garbage.AddObject ('', Xsd.sType);
  Xsd.DoNotEncode := True; {klopt dit????}
  Xsd.ElementName := '';
  Xsd.sType.IsComplex := True;
  Xsd.sType.ContentModel := 'Sequence';
  Xsd.minOccurs := '1';
  Xsd.maxOccurs := '1';
  Parts := TWsdlParts.Create;
  Parts.Sorted := False;
end;

destructor TWsdlMsgDescr.Destroy;
begin
{  Xml.Free; is freed from operation}
  Parts.Clear;
  FreeAndNil (Parts);
  inherited;
end;

{ TWsdlHeaders }

procedure TWsdlHeaders.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Headers [x].Free;
  inherited;
end;

function TWsdlHeaders.GetHeader(Index: integer): TWsdlHeader;
begin
  result := TWsdlHeader (Objects [index]);
end;

{ TWsdlHeader }

constructor TWsdlHeader.Create;
begin

end;

destructor TWsdlHeader.Destroy;
begin

  inherited;
end;

{ TWsdlMessages }

procedure TWsdlMsgDescrs.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Objects [x].Free;
  inherited;
end;

procedure TWsdlMsgDescrs.ClearListOnly;
begin
  inherited Clear;
end;

function TWsdlMsgDescrs.GetMessage(Index: integer): TWsdlMsgDescr;
begin
  result := Objects [Index] as TWsdlMsgDescr;
end;

{ TWsdlMessage }

procedure TWsdlMessage.Clean;
begin
  if reqBind is TXml then
    (reqBind as TXml).Clean(1, xsdMaxDepthBillOfMaterials);
  if rpyBind is TXml then
    (rpyBind as TXml).Clean(1, xsdMaxDepthBillOfMaterials);
  if fltBind is TXml then
    (rpyBind as TXml).Clean(1, xsdMaxDepthBillOfMaterials);
end;

procedure TWsdlMessage.CheckBefore ;
begin
  fPreparedBefore := False;
  if Assigned (WsdlOperation) then
  begin
    try
      WsdlOperation.CheckScript(BeforeScriptLines, nil);
      fPreparedBefore := True;
    except
    end;
  end;
end;

procedure TWsdlMessage .CheckAfter ;
begin
  fPreparedAfter := False;
  if Assigned (WsdlOperation) then
  begin
    try
      WsdlOperation.CheckScript(AfterScriptLines, nil);
      fPreparedAfter := True;
    except
    end;
  end;
end;

function TWsdlMessage.thisMessage: TWsdlMessage;
begin
  result := self;
end;

procedure TWsdlMessage.corBindsInit(aOperation: TWsdlOperation);
var
  x: Integer;
  xCorrId: STring;
begin
  CorrelationBindables.ClearListOnly;
  for x := 0 to aOperation.CorrelationBindables.Count - 1 do
    self.CorrelationBindables.AddObject
      ( aOperation.CorrelationBindables.Strings[x]
      , FindBind(aOperation.CorrelationBindables.Strings[x])
      );
end;

constructor TWsdlMessage.Create;
begin
  reqBind := TXml.Create;
  rpyBind := TXml.Create;
  fltBind := TXml.Create;
  BeforeScriptLines := TJBStringList.Create;
  AfterScriptLines := TJBStringList.Create;
end;

constructor TWsdlMessage.Create(aOperation: TWsdlOperation);
begin
  BeforeScriptLines := TJBStringList.Create;
  AfterScriptLines := TJBStringList.Create;
  ColumnXmls := TBindableList.Create;
  WsdlOperation := aOperation;
  while Assigned(WsdlOperation.Cloned) do
    WsdlOperation := WsdlOperation.Cloned;
  WsdlOperation.Messages.AddObject('', self);
  Name := 'Message' + IntToStr (aOperation.Messages.Count);
  if WsdlOperation.reqBind is TIpmItem then
  begin
    reqBind := TIpmItem.Create(aOperation.reqBind as TIpmItem);
    rpyBind := TIpmItem.Create(aOperation.rpyBind as TIpmItem);
    fltBind := TIpmItem.Create(aOperation.fltBind as TIpmItem);
  end
  else
  begin
    reqBind := TXml.Create(0, WsdlOperation.reqXsd);
    reqBind.Name := WsdlOperation.reqBind.Name;
    (reqBind as TXml).LoadValues(aOperation.reqBind as TXml, True);
    rpyBind := TXml.Create(0, WsdlOperation.rpyXsd);
    rpyBind.Name := WsdlOperation.rpyBind.Name;
    (rpyBind as TXml).LoadValues(aOperation.rpyBind as TXml, True);
    fltBind := TXml.Create(0, WsdlOperation.FaultXsd);
    fltBind.Name := WsdlOperation.fltBind.Name;
    (fltBind as TXml).LoadValues(aOperation.fltBind as TXml, True);
  end;
  CorrelationBindables := cloneBindables(WsdlOperation.CorrelationBindables);
  if aOperation.isFreeFormat then
  begin
    FreeFormatReq := aOperation.FreeFormatReq;
    FreeFormatRpy := aOperation.FreeFormatRpy;
  end;
end;

constructor TWsdlMessage.CreateRequest (aOperation: TWsdlOperation; aName, aPatterns, aDocumentation: String);
var
  x: Integer;
begin
  Name := aName;
  WsdlOperation:= aOperation;
  CorrelationBindables := TBindableList.Create;
  BeforeScriptLines := TJBStringList.Create;
  AfterScriptLines := TJBStringList.Create;
//Patterns := TJBStringList.Create;
{}{
{}
  Documentation := IfThen (   (aDocumentation = 'Default reply')
                           or (aDocumentation = 'Default request')
                          , ''
                          , aDocumentation
                          );

  aOperation.Messages.AddObject('', self);
  if WsdlOperation.WsdlService.DescriptionType in [ipmDTCobol] then
  begin
    if Assigned (aOperation.reqBind) then
      reqBind := TIpmItem.Create (aOperation.reqBind as TIpmItem);
    if Assigned (aOperation.rpyBind) then
      rpyBind := TIpmItem.Create (aOperation.rpyBind as TIpmItem);
    if Assigned (aOperation.fltBind) then
      fltBind := TIpmItem.Create (aOperation.fltBind as TIpmItem);
  end
  else
  begin
    if (aOperation.rpyBind is TXml)
    and Assigned ((aOperation.rpyBind as TXml).Xsd) then
    begin
      bindRefId := 0;
      rpyBind := TXml.Create (0, (aOperation.rpyBind as TXml).Xsd);
      if rpyBind.Children.Count > 0 then
        rpyBodyBind := rpyBind.Children.Bindables[aOperation.OutputHeaders.Count];
        rpyBind.Name := aOperation.rpyBind.Name;
    end;
    if (aOperation.reqBind is TXml)
    and Assigned ((aOperation.reqBind as TXml).Xsd) then
    begin
      bindRefId := 0;
      reqBind := TXml.Create (0, (aOperation.reqBind as TXml).Xsd);
      reqBind.Name := aOperation.reqBind.Name;
    end;
    bindRefId := 0;
    if Assigned (aOperation.FaultMessages) then
      fltBind := TXml.Create (0, aOperation.FaultXsd)
    else
      fltBind := TXml.Create;
  end;
  if not Assigned (reqBind) then
    reqBind := TXml.Create;
  if not Assigned (rpyBind) then
    rpyBind := TXml.Create;
  ColumnXmls := TBindableList.Create;
  corBindsInit(aOperation);
  if aOperation.CorrelationBindables.Count > 0 then
  begin
    with TJBStringList.Create do
    try
      Text := aPatterns;
      x := 0;
      while (x < Count)
      and (x < CorrelationBindables.Count) do
      begin
        if Assigned (CorrelationBindables.Bindables[x]) then
          CorrelationBindables.Bindables[x].CorrelationValue := Strings[x];
        Inc (x);
      end;
      while (x < CorrelationBindables.Count) do
      begin
        CorrelationBindables.Bindables[x].CorrelationValue := '.*';
        Inc (x);
      end;
    finally
      Free;
    end;
  end;
end;

constructor TWsdlMessage.CreateReply (aOperation: TWsdlOperation; aName, aPatterns, aDocumentation: String);
var
  x: Integer;
begin
  try
    Name := aName;
    WsdlOperation := aOperation;
    CorrelationBindables := TBindableList.Create;
    BeforeScriptLines := TJBStringList.Create;
    AfterScriptLines := TJBStringList.Create;
{$ifdef jwbPatterns}
    Patterns := TJBStringList.Create;
    Patterns.Text := aPatterns;
    while Patterns.Count < aOperation.CorrelationBindables.Count do
      Patterns.Add(Patterns.Strings[0]);
{$endif}
    Documentation := IfThen (   (aDocumentation = 'Default reply')
                             or (aDocumentation = 'Default request')
                            , ''
                            , aDocumentation
                            );
    aOperation.Messages.AddObject('', self);
    if WsdlOperation.WsdlService.DescriptionType in [ipmDTCobol] then
    begin
      if Assigned (aOperation.reqBind) then
        reqBind := TIpmItem.Create (aOperation.reqBind as TIpmItem);
      if Assigned (aOperation.rpyBind) then
        rpyBind := TIpmItem.Create (aOperation.rpyBind as TIpmItem);
      if Assigned (aOperation.fltBind) then
        fltBind := TIpmItem.Create (aOperation.fltBind as TIpmItem);
    end
    else
    begin
      if not (aOperation.isFreeFormat) then
      begin
        bindRefId := 0;
        if Assigned (aOperation.rpyBind)
        and Assigned ((aOperation.rpyBind as TXml).Xsd) then
        begin
          rpyBind := TXml.Create (0, (aOperation.rpyBind as TXml).Xsd);
          if rpyBind.Children.Count > 0 then
            rpyBodyBind := rpyBind.Children.Bindables[aOperation.OutputHeaders.Count];
          rpyBind.Name := aOperation.rpyBind.Name;
        end;
        bindRefId := 0;
        if Assigned (aOperation.reqBind)
        and Assigned ((aOperation.reqBind as TXml).Xsd) then
        begin
          reqBind := TXml.Create (0, (aOperation.reqBind as TXml).Xsd);
          reqBind.Name := aOperation.reqBind.Name;
        end;
        bindRefId := 0;
        fltBind := TXml.Create;
        if Assigned (aOperation.FaultMessages) then
        begin
          (fltBind as TXml).CopyDownLine (aOperation.fltBind as TXml, False);
          fltBind.Name := 'Faults';
          fltBind.Checked := False;
        end;
      end;
    end;
    if not Assigned (reqBind) then
      reqBind := TXml.Create;
    if not Assigned (rpyBind) then
      rpyBind := TXml.Create;
    ColumnXmls := TBindableList.Create;
    corBindsInit (aOperation);
    if aOperation.CorrelationBindables.Count > 0 then
    begin
      with TJBStringList.Create do
      try
        Text := aPatterns;
        x := 0;
        while (x < Count)
        and (x < CorrelationBindables.Count) do
        begin
          if Assigned (CorrelationBindables.Bindables[x]) then
            CorrelationBindables.Bindables[x].CorrelationValue := Strings[x];
          Inc (x);
        end;
      finally
        Free;
      end;
    end;
  except
    raise Exception.CreateFmt( 'TWsdlMessage.CreateReply (aOperation: %s; %s, aPatterns, aDocumentation: String)%s'
                             , [aOperation.reqTagName, aName, LineEnding]);
  end;
end;

destructor TWsdlMessage.Destroy;
begin
  reqBind.Free;
  rpyBind.Free;
  FreeAndNil (fltBind);
  ColumnXmls.Free;
  CorrelationBindables.Free;
  FreeAndNil(BeforeScriptLines);
  FreeAndNil(AfterScriptLines);
  inherited;
end;

{ TWsdlMessages }

procedure TWsdlMessages.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Messages [x].Free;
  inherited;
end;

procedure TWsdlMessages.DeleteMessage(aIndex: Integer);
begin
  if (aIndex < 0)
  or (aIndex >= Count) then
    raise Exception.Create('TWsdlMessages.DeleteMessage: Indexout of bounds');
  Messages[aIndex].Free;
  Delete(aIndex);
end;

procedure TWsdlMessages.DeleteMessage(aMessage: TWsdlMessage);
var
  x: Integer;
begin
  x := IndexOfObject(aMessage);
  if x < 0 then
    raise Exception.Create('Passed WsdlMessage object found');
  Delete(x);
  aMessage.Free;
end;

function TWsdlMessages.GetMessage(Index: integer): TWsdlMessage;
begin
  result := TWsdlMessage (Objects [Index]);
end;

procedure TWsdlMessages.SetNameDuplicates;
var
  m, m1: Integer;
begin
  for m := 0 to Count - 1 do
  with Messages[m] do
  begin
    DuplicatesName := nil;
    _compareString := UpperCase(Name);
                    ;
  end;
  for m := 0 to Count - 2 do
  begin
    if not Assigned(Messages[m].DuplicatesName) then
    begin
      for m1 := m + 1 to Count - 1 do
      begin
        if not Assigned(Messages[m1].DuplicatesName) then
        begin
          if Messages[m1]._compareString = Messages[m]._compareString then
          begin
            Messages[m1].DuplicatesName := Messages[m];
          end;
        end;
      end;
    end;
  end;
end;

procedure TWsdlMessages.ResetNameDuplicates;
var
  m: Integer;
begin
  for m := 0 to Count - 1 do
    Messages[m].DuplicatesName := nil;
end;

procedure TWsdlMessages.SetDuplicates;
  function _asString (aBind: TCustomBindable): String;
  begin
    result := '';
    if aBind is TXml then
    with aBind as TXml do
    begin
      result := AsText ( False
                       , 2
                       , False // yes, also when not checked
                       , False
                       );
    end;
    if aBind is TIpmItem then
    begin
      with (aBind as TIpmItem).AsXml do
      begin
        result := AsText ( False
                         , 2
                         , False // yes, also when not checked
                         , False
                         );
        Free;
      end;
    end;
  end;
var
  m, m1: Integer;
begin
  for m := 0 to Count - 1 do
  with Messages[m] do
  begin
    Duplicates := nil;
    _compareString := 'req:'
                    + LineEnding
                    + _asString(reqBind)
                    + LineEnding
                    + 'rpy'
                    + LineEnding
                    + _asString(rpyBind)
                    + LineEnding
                    + 'before'
                    + LineEnding
                    + BeforeScriptLines.Text
                    + LineEnding
                    + 'after'
                    + LineEnding
                    + AfterScriptLines.Text
                    + LineEnding
                    ;
  end;
  for m := 0 to Count - 2 do
  begin
    if not Assigned(Messages[m].Duplicates) then
    begin
      for m1 := m + 1 to Count - 1 do
      begin
        if not Assigned(Messages[m1].Duplicates) then
        begin
          if Messages[m1]._compareString = Messages[m]._compareString then
          begin
            Messages[m1].Duplicates := Messages[m];
          end;
        end;
      end;
    end;
  end;
end;

procedure TWsdlMessages.ResetDuplicates;
var
  m: Integer;
begin
  for m := 0 to Count - 1 do
    Messages[m].Duplicates := nil;
end;

function TWsdlBinder.getDescriptionType: TIpmDescrType;
begin
  result := WsdlOperation.WsdlService.DescriptionType;
end;

function TWsdlBinder .cloneBindables (aSrc : TBindableList ): TBindableList ;
var
  x: Integer;
begin
  result := TBindableList.Create;
  for x := 0 to aSrc.Count - 1 do
  begin
    if AnsiStartsText('Req.',aSrc.Strings[x]) then
      result.AddObject
        ( aSrc.Strings[x]
        , self.reqBind.FindUQ
            (Copy ( aSrc.Strings[x]
                  , 5
                  , Length (aSrc.Strings[x]) - 4
                  )
            )
        );
    if AnsiStartsText('Rpy.',aSrc.Strings[x]) then
      result.AddObject
        ( aSrc.Strings[x]
        , self.rpyBind.FindUQ
            (Copy ( aSrc.Strings[x]
                  , 5
                  , Length (aSrc.Strings[x]) - 4
                  )
            )
        );
    if AnsiStartsText('Flt.',aSrc.Strings[x]) then
      result.AddObject
        ( aSrc.Strings[x]
        , self.fltBind.FindUQ
            (Copy ( aSrc.Strings[x]
                  , 5
                  , Length (aSrc.Strings[x]) - 4
                  )
            )
        );
  end;
end;

procedure TWsdlBinder.PopulateCorrelation (aPatternsList : TJBStringList );
var
  x: Integer;
begin
  x := 0;
  while (x < CorrelationBindables.Count)
  and (x < aPatternsList.Count) do
  begin
    if Assigned (CorrelationBindables.Bindables[x]) then
      CorrelationBindables.Bindables[x].CorrelationValue := aPatternsList.Strings[x];
    Inc (x);
  end;
  while (x < CorrelationBindables.Count) do
  begin
    if Assigned (CorrelationBindables.Bindables[x]) then
      CorrelationBindables.Bindables[x].CorrelationValue := '.*';
    Inc (x);
  end;
end;

function TWsdlBinder.getReqXml: TXml;
begin
  result := reqBind as TXml;
end;

function TWsdlBinder.getRpyXml: TXml;
begin
  result := rpyBind as TXml;
end;

function TWsdlBinder .getRequestAsString : String ;
begin
  case DescriptionType of
    ipmDTFreeFormat: result := FreeFormatReq;
    ipmDTCobol: ;
    ipmDTXml, ipmDTXsd, ipmDTWsdl, ipmDTJson: result := '';
  end;
end;

function TWsdlBinder.FindBind(aCaption: String): TCustomBindable;
var
  p: Integer;
begin
  result := nil;
  if aCaption = '' then Exit;
  p := Pos('.', aCaption);
  if p < 1 then raise Exception.Create('Dot missing, can not determine Req, Rpy or Flt');
  try
    if Copy (aCaption, 1, p) = 'Req.' then
    begin
      result := reqBind.FindUQ(Copy (aCaption, p + 1, 10000));
      exit;
    end;
    if Copy (aCaption, 1, p) = 'Rpy.' then
    begin
      result := rpyBind.FindUQ(Copy (aCaption, p + 1, 10000));
      exit;
    end;
    if Copy (aCaption, 1, p) = 'Flt.' then
    begin
      result := fltBind.FindUQ(Copy (aCaption, p + 1, 10000));
      exit;
    end;
  except
    result := nil;
  end;
end;

procedure TWsdlBinder.RebindLists ;
var
  x: Integer;
begin
  if Assigned (CorrelationBindables) then with CorrelationBindables do
    for x := 0 to Count - 1 do
      Bindables[x] := FindBind(Strings[x]);
end;

function TWsdlBinder.getInputXsd: TXsd;
begin
  result := fInputXsd;
end;

function TWsdlBinder.getOutputXsd: TXsd;
begin
  result := fOutputXsd;
end;

procedure TWsdlBinder.setFreeFormatReq(const aValue: String);
begin
  if reqXml.Items.Count = 0 then
    reqXml.AddXml(TXml.CreateAsString('Body', ''));
  reqXml.Items.XmlItems[0].Value := aValue;
end;

procedure TWsdlBinder.setFreeFormatRpy(const aValue: String);
begin
  if rpyXml.Items.Count = 0 then
    rpyXml.AddXml(TXml.CreateAsString('Body', ''));
  rpyXml.Items.XmlItems[0].Value := aValue;
end;

procedure TWsdlBinder.setInputXsd(const Value: TXsd);
begin
  fInputXsd := Value;
end;

procedure TWsdlBinder.setOutputXsd(const Value: TXsd);
begin
  fOutputXsd := Value;
end;

procedure TWsdlBinder.setRpyBind(const Value: TCustomBindable);
begin
  frpyBind := Value;
end;

function TWsdlBinder.getReqBind: TCustomBindable;
begin
  result := freqBind;
end;

function TWsdlBinder.getReqIpm: TIpmItem;
begin
  result := freqBind as TIpmItem;
end;

procedure TWsdlBinder.setReqBind(const Value: TCustomBindable);
begin
  freqBind := Value;
end;

procedure TWsdlBinder.FoundErrorInBuffer(ErrorString: String; aObject: TObject);
begin
  (aObject as TIpmItem).Value := '?' + _progName + ' Error found: ' + ErrorString;
end;

function TWsdlBinder.getFreeFormatReq: String;
begin
  try
    result := reqXml.Items.XmlItems[0].Value;
  except
    result := '';
  end;
end;

function TWsdlBinder.getFreeFormatRpy: String;
begin
  try
    result := rpyXml.Items.XmlItems[0].Value;
  except
    result := '';
  end;
end;

procedure TWsdlBinder.setRequestAsString (AValue : String );
  procedure _XmlRequestToBindables;
  var
    x, s, d: Integer;
    xXml: TXml;
    xWsaName: String;
  begin
    xXml := TXml.Create;
    try
      xXml.LoadFromString(AValue, nil);
      xXml.SeparateNsPrefixes;
      xXml.ResolveNameSpaces;
      reqXml.ResetValues;
      reqXml.Checked := True;
      if xXml.isSoapEnvelope then
      begin
        for x := 0 to xXml.Items.Count - 1 do
        begin
          xXml := xXml.Items.XmlItems [x];
          if (xXml.TagName = 'Header') then
          begin
            for s := 0 to xXml.Items.Count - 1 do
            begin
              for d := 0 to WsdlOperation.InputHeaders.Count - 1 do
                reqXml.Items.XmlItems[d].LoadValues (xXml.Items.XmlItems [s], False, False);
            end;
            if Assigned (WsdlOperation.reqWsaXml) then
            begin
              WsdlOperation.reqWsaXml.ResetValues;
              xWsaName := xXml.Name;
              try
                xXml.Name := WsdlOperation.reqWsaXml.Name;
                WsdlOperation.reqWsaXml.LoadValues(xXml,False,False);
                WsdlOperation.rpyWsaOnRequest;
              finally
                xXml.Name := xWsaName;
              end;
            end;
          end;
          if (xXml.TagName = 'Body') then
          begin
            for s := 0 to xXml.Items.Count - 1 do
            begin
              for d := WsdlOperation.InputHeaders.Count to reqXml.Items.Count - 1 do
                reqXml.Items.XmlItems[d].LoadValues (xXml.Items.XmlItems [s], False, False);
            end;
          end;
        end;
      end
      else
      begin
        reqXml.Items.XmlItems[0].LoadValues (xXml, DescriptionType <> ipmDTXsd, False);
      end;
    finally
      xXml.Free;
    end;
  end;
begin
  case WsdlOperation.WsdlService.DescriptionType of
    ipmDTFreeFormat:
      begin
        FreeFormatReq := AValue;
        reqXml.LoadFromString(AValue, nil);
      end;
    ipmDTCobol: (reqBind as TIpmItem).BufferToValues (FoundErrorInBuffer, AValue);
    ipmDTXml, ipmDTXsd, ipmDTWsdl: _XmlRequestToBindables;
  end;
end;

function TWsdlBinder.getRpyBind: TCustomBindable;
begin
  result := frpyBind;
end;

function TWsdlBinder.getRpyIpm: TIpmItem;
begin
  result := rpyBind as TIpmItem;
end;

function wsdlConvertSdfFrom36 (aXml: TXml): Boolean;
var
  xDT, xCE: String;
  sdXmls: TXmlList;
  xXml, sXml, oXml: TXml;
  x, s, o: Integer;
begin
  // convert from SDF 3.6 format to SDF 4.0 format
  result := False;
  if (aXml.Items.Count < 1)
  or (aXml.Items.XmlItems[0].Name <> 'DescriptionType') then
    Exit;
  xDT := aXml.Items.XmlValueByTag['DescriptionType'];
  xCE := aXml.Items.XmlValueByTag['CobolEnvironment'];
  sdXmls := TXmlList.Create;
  try
    for x := aXml.Items.Count - 1 downto 0 do
      if aXml.Items.XmlItems[x].Name = 'Service' then
        sdXmls.AddObject('', aXml.Items.XmlItems[x])
      else
        aXml.Items.XmlItems[x].Free;
    aXml.Items.ClearListOnly;
    for x := 0 to sdXmls.Count - 1 do
    begin
      xXml := aXml.AddXml(TXml.CreateAsString('Service', ''));
      xXml.Checked := True;
      xXml.AddXml(sdXmls.XmlItems[x]);
      xXml.Items.XmlItems[0].Name := xDT;
    end;
    sdXmls.ClearListOnly;
    if (xDT = 'Cobol')
    and (xCE <> '') then
    begin
      for s := 0 to aXml.Items.Count - 1 do
      begin
        if aXml.Items.XmlItems[s].Name = 'Service' then
        begin
          sXml := aXml.Items.XmlItems[s].Items.XmlItems[0];
          for o := 0 to sXml.Items.Count - 1 do
          begin
            oXml := sXml.Items.XmlItems[o];
            if oXml.Name = 'Operation' then
              oXml.AddXml(TXml.CreateAsString('CobolEnvironment', xCE));
          end;
        end;
      end;
    end;
    aXml.CheckDownline(True);
  finally
    sdXmls.Free;
  end;
  result := True;
end;


initialization
  Randomize;
  _WsdlProgName := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(ParamStr(0)), '');
  _WsdlHostName := xmlio.GetHostName;
  _ipmGun := (Lowercase(_WsdlProgName) = 'ipmgun');
  _OnParseErrorEvent := nil;
  _WsdlUserNameTokenNumber := 0;
  allOperations := TWsdlOperations.Create;
  allOperations.Sorted := True;
  allOperations.Duplicates := dupError;
  allOperationsRpy := TWsdlOperations.Create;
  allOperationsRpy.Sorted := True;
  allOperationsRpy.Duplicates := dupError;
  allAliasses := TWsdlOperations.Create;
  allAliasses.Sorted := True;
  allAliasses.Duplicates := dupAccept;
  _WsdlDbsConnector := TSQLConnector.Create(nil);
  _WsdlDbsTransaction := TSQLTransaction.Create(nil);
  _WsdlDbsTransaction.Active := False;
  _WsdlDbsTransaction.Action := caCommit;
  _WsdlDbsTransaction.DataBase := _WsdlDbsConnector;
  _WsdlDbsConnector.Transaction := _WsdlDbsTransaction; // linked to each other...
  UILock := SyncObjs.TCriticalSection.Create;
  EnvVarLock := SyncObjs.TCriticalSection.Create;
  doUILock := True;
  doOperationLock := True;

finalization
  allOperations.ClearListOnly;
  allOperations.Free;
  allAliasses.ClearListOnly;
  allAliasses.Free;
  allOperationsRpy.ClearListOnly;
  allOperationsRpy.Free;
  _WsdlDbsTransaction.Free;
  _WsdlDbsConnector.Free;
  UILock.Free;
  EnvVarLock.Free;
end.

