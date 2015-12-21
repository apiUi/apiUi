unit Wsdlz;
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
   , FileUtil
   , xmlzConsts
   , IpmTypes
   , IdSSLOpenSSL
   , IdSync
   , SyncObjs
   ;

resourcestring
  S_MESSAGE_ACCEPTED = '[Message accepted by server]';
  S_NO_OPERATION_FOUND = 'No operation recognised';
  S_INBOUND_IS_A_RESPONSE = '[Inbound is a response]';

type TStubAction = (saStub, saForward, saRedirect, saRequest);
type TTransportType = (ttHttp, ttHttps, ttMq, ttStomp, ttTaco, ttSmtp, ttBmtp);
type TRecognitionType = (rtSoap, rtDocument, rtHeader, rtXml, rtSubString);
type TAuthenticationType = (atNone, atHTTPBasicAuthentication, atWsSecurity);
type TPasswordType = (pwText, pwDigest);
type TProcedure = procedure of Object;
const
TransportTypeNames: array [ttHttp..ttBmtp] of String =
( 'Http'
, 'Https'
, 'Mq'
, 'Stomp'
, 'Taco'
, 'Smtp'
, 'Bmtp'
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
      fStrs: TStringList;
      function getServiceByName(Index: String): TWsdlService;
      function getOperationByRequest(Index: String): TWsdlOperation;
    public
      Name: String;
      FileName: String;
      isSoapService: Boolean;
//      xTargetNamespacePrefix: String;
      Services: TWsdlServices;
      XsdDescr: TXsdDescr;
      sdfXsdDescrs: TXsdDescrList;
      IpmDescrs: TIpmDescrs;
      ExtraXsds: TStringList;
      SoapVersion: TSOAPVersion;
      xsdElementsWhenRepeatable: Integer;
      xsdDefaultElementsWhenRepeatable: Integer;
      OperationsWithEndpointOnly: Boolean;
      _inXml: TXml;
      _outXml: TXml;
      _expectXml: TXml;
      property ServiceByName [Index: String]: TWsdlService read getServiceByName;
      property OperationByRequest [Index: String]: TWsdlOperation read getOperationByRequest;
      function ExtraXsdsAsXml (aSaveRelativeFileNames: Boolean): TXml;
      procedure ExtraXsdsFromXml (aXml: TXml);
      procedure AddedTypeDefElementsFromXml (aXml: TXml);
      procedure LoadExtraXsds;
      procedure LoadFromSchemaFile(aFileName: String; aOnError: TOnErrorEvent);
      procedure LoadFromSdfFile(aFileName: String);
      constructor Create(aElementsWhenRepeatable, aDefaultElementsWhenRepeatable: Integer; aOperationsWithEndpointOnly: Boolean);
      destructor Destroy; override;
  end;

  TWsdlHeaders = class (TStringList)
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

  TWsdlServices = class (TStringList)
  private
    function GetService(Index: integer): TWsdlService;
  public
    property Services [Index: integer]: TWsdlService read GetService;
    procedure Clear; override;
  end;

  TWsdlService = class(TObject)
    private
    function getOperationByName(Index: String): TWsdlOperation;
    public
      Name: String;
      AuthenticationType: TAuthenticationType;
      UserName: String;
      Password: String;
      PasswordType: TPasswordType;
      SuppressXmlComment: Boolean;
      SuppressHTTP500: Boolean;
      UseNameSpacePrefixes: Boolean;
      DescriptionType: TIpmDescrType;
      Operations: TWsdlOperations;
      function StreamWsSecurity: String;
      property OperationByName [Index: String]: TWsdlOperation read getOperationByName;
      function OptionsAsXml: TXml;
      procedure OptionsFromXml(aXml: TXml);
      constructor Create;
      destructor Destroy; override;
  end;

  TWsdlMsgDescrs = class (TStringList)
  private
    function GetMessage(Index: integer): TWsdlMsgDescr;
    protected
    public
      property Messages [Index: integer]: TWsdlMsgDescr read GetMessage;
      procedure ClearListOnly;
      procedure Clear; override;
  end;

  { TWsdlOperations }

  TWsdlOperations = class (TStringList)
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
    fFreeFormatReq: String;
    fFreeFormatRpy: String;
    fWsdlMessage: TWsdlMessage;
    procedure FoundErrorInBuffer(ErrorString: String; aObject: TObject);
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
  public
    Name: String;
    WsdlOperation: TWsdlOperation;
    FaultXsd: TXsd;
    fltBind: TCustomBindable;
    CorrelationBindables: TBindableList;
    procedure SwiftMtRequestToBindables (aString: String);
    function FindBind (aCaption: String): TCustomBindable;
    procedure PopulateCorrelation (aPatternsList: TStringList);
    property DescriptionType: TIpmDescrType read getDescriptionType;
    property reqXsd: TXsd read getInputXsd write setInputXsd;
    property reqBind: TCustomBindable read getReqBind write setReqBind;
    property rpyXsd: TXsd read getOutputXsd write setOutputXsd;
    property rpyBind: TCustomBindable read getRpyBind write setRpyBind;
    property FreeFormatReq: String read fFreeFormatReq write setFreeFormatReq;
    property FreeFormatRpy: String read fFreeFormatRpy write setFreeFormatRpy;
    property RequestAsString: String read getRequestAsString write setRequestAsString;
    property ReqIpm: TIpmItem read getReqIpm;
    property RpyIpm: TIpmItem read getRpyIpm;
    property reqXml: TXml read getReqXml;
    property rpyXml: TXml read getRpyXml;
  end;

  { TWsdlOperation }

  TWsdlOperation = class(TWsdlBinder)
    private
      fCloned: TWsdlOperation;
      fLock: TCriticalSection;
      fStamperStatement: String;
      fExpressBefore: TExpress;
      fExpressAfter: TExpress;
      fExpressStamper: TExpress;
      fExpressChecker: TExpress;
      fPreparedBefore: Boolean;
      fPreparedAfter: Boolean;
      fDoExit: Boolean;
      fLineNumber: Integer;
      fOnError: TOnErrorEvent;
      fLastMessage: TWsdlMessage;
      fLastFullCaption: String;
      fOnGetAbortPressed: TBooleanFunction;
      fPrepareErrors: String;
      fFetchIndex: Integer;
      procedure BufferToValuesErrorFound (aMessage: String; aObject: TObject);
      function getDoExit : Boolean ;
      function getIsOneWay: Boolean;
      function getLateBinding : Boolean ;
      function getisSoapService: Boolean;
      procedure setDoExit (AValue : Boolean );
      function getInputXml: TXml;
      function getOutputXml: TXml;
      function getLastFullCaption: String;
      function getLastMessage: TWsdlMessage;
      function getReplyBasedOnRequest: TWsdlMessage;
      procedure NeedBeforeData(Sender: TObject; var MoreData: Boolean; var Data: String);
      procedure NeedAfterData(Sender: TObject; var MoreData: Boolean; var Data: String);
      procedure NeedStamperData(Sender: TObject; var MoreData: Boolean; var Data: String);
      function StreamWsAddressing (aWsa: TXml; isRequest: Boolean): String;
      function getWsaTo: String;
    procedure setOnGetAbortPressed(const Value: TBooleanFunction);
    function getDebugTokenStringAfter: String;
    function getDebugTokenStringBefore: String;
    public
      _processing: Boolean;
      WsdlService: TWsdlService;
      Wsdl: TWsdl;
      Owner: TObject;
      Data: TObject;
      Alias: String;
      HiddenFromUI: Boolean;
      reqMessageName, reqTagName, reqTagNameSpace, rpyMessageName, rpyTagName, rpyTagNameSpace: String;
      reqDescrFilename, rpyDescrFilename, fltDescrFileName: String;
      reqDescrExpansionFilename, rpyDescrExpansionFilename, fltDescrExpansionFileName: String;
      Documentation: TStringList;
      _InputMessageName: String;
      _OutputMessageName: String;
      FaultMessages: TWsdlMsgDescrs;
      InputHeaders: TWsdlHeaders;
      OutputHeaders: TWsdlHeaders;
      OperationCounter: Integer;
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
      reqWsaXml, rpyWsaXml: TXml;
      AsynchronousDialog: Boolean;
      StubAction: TStubAction;
      StubTransport: TTransportType;
      StubHttpAddress: String;
      httpVerb: String;
      ContentEncoding: String;
      AcceptGzipEncoding, AcceptDeflateEncoding: Boolean;
      smtpHost: String;
      smtpPort: Integer;
      useSsl: Boolean;
      sslVersion: TIdSSLVersion;
      sslCertificateFile, sslKeyFile, sslRootCertificateFile: String;
      StubMqHeaderXml: TXml;
      StubMqPutManager: String;
      StubMqPutQueue: String;
      StubMqGetManager: String;
      StubMqGetQueue: String;
      StubMqTimeOut: Integer;
      StubStompHeaderXml: TXml;
      StubCustomHeaderXml: TXml;
      StubStompPutHost: String;
      StubStompPutPort: Integer;
      StubStompPutClientId: String;
      StubStompTimeOut: Integer;
      StubStompReplyBodyPostFix, StubStompRequestBodyPostFix: String;
      TacoConfigXml: TXml;
      BeforeScriptLines: TStringList;
      AfterScriptLines: TStringList;
      CorrelatedMessage: TWsdlMessage;
      Messages: TWsdlMessages;
      doReadReplyFromFile: Boolean;
      ReadReplyFromFileXml: TXml;
      ExpectationBindables, LogColumns, BindablesWithAddedElement: TBindableList;
      faultcode, faultstring, faultactor, LiteralResult: String;
      ReturnSoapFault: Boolean;
      RecognitionType: TRecognitionType;
      reqRecognition: TStringList;
      rpyRecognition: TStringList;
      oldInvokeSpec: String;
      invokeList: TWsdlOperations;
      doDebug: Boolean;
      doSuppressLog: Integer;
      DelayTimeMs: Integer;
      DelayTimeMsMin: Integer;
      DelayTimeMsMax: Integer;
      CobolEnvironment: TCobolEnvironmentType;
      ZoomElementCaption: String;
      property FetchIndex: Integer read fFetchIndex write fFetchIndex;
      property DoExit: Boolean read getDoExit write setDoExit;
      property PrepareErrors: String read fPrepareErrors;
      property OnGetAbortPressed: TBooleanFunction write setOnGetAbortPressed;
      property wsaTo: String read getWsaTo;
      property isSoapService: Boolean read getIsSoapService;
      property isOneWay: Boolean read getIsOneWay;
      property lateBinding: Boolean read getLateBinding;
      property InputXml: TXml read getInputXml;
      property OutputXml: TXml read getOutputXml;
      property LastMessage: TWsdlMessage read getLastMessage write fLastMessage;
      property LastFullCaption: String read getLastFullCaption write fLastFullCaption;
      property MessageBasedOnRequest: TWsdlMessage read getReplyBasedOnRequest;
      property OnError: TOnErrorEvent read fOnError write fOnError;
      property PreparedBefore: Boolean read fPreparedBefore;
      property PreparedAfter: Boolean read fPreparedAfter;
      property Cloned: TWsdlOperation read fCloned;
      property DebugTokenStringAfter: String read getDebugTokenStringAfter;
      property DebugTokenStringBefore: String read getDebugTokenStringBefore;
      function AddedTypeDefElementsAsXml: TObject;
      procedure AddedTypeDefElementsFromXml(aXml: TObject);
      function BeforeBindsAsText: String;
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
      procedure FreeFormatToBindables (aRequestXml: TXml; aRequestString: String);
      procedure SoapXmlRequestToBindables (aRequest: TXml; aAddUnknowns: Boolean);
      procedure SoapXmlReplyToBindables (aReply: TXml; aAddUnknowns: Boolean);
      procedure RequestStringToBindables (aRequest: String);
      procedure ReplyStringToBindables (aReply: String);
      function CorrelationIdAsText (aSeparator: String): String;
      procedure BindCheckerFunction (Id: String; Adr: Pointer; Token: Integer; ArgumentsPrototype: String);
      procedure BindStamperFunction (Id: String; Adr: Pointer; Token: Integer; ArgumentsPrototype: String);
      procedure BindBeforeFunction (Id: String; Adr: Pointer; Token: Integer; ArgumentsPrototype: String);
      procedure BindAfterFunction (Id: String; Adr: Pointer; Token: Integer; ArgumentsPrototype: String);
      procedure doPromptReply;
      procedure doPromptRequest;
      procedure BindStamper;
      procedure BindChecker (aBind: TCustomBindable);
      procedure PrepareBefore;
      procedure doInvokeOperations;
      procedure PrepareAfter;
      procedure PrepareChecker (aBind: TCustomBindable);
      procedure PrepareReqStamper (aBind: TCustomBindable);
      procedure ExecuteReqStampers;
      procedure PrepareRpyStamper (aBind: TCustomBindable);
      procedure ExecuteRpyStampers;
      procedure InitExecute;
      procedure ExecuteBefore;
      procedure ExecuteAfter;
      procedure reqWsaOnRequest;
      procedure rpyWsaOnRequest;
      procedure fltWsaOnRequest;
      procedure Clean;
      function FunctionPrototypes (aAfter: Boolean): TStringList;
      function CheckerFunctionPrototypes: TStringList;
      function StamperFunctionPrototypes: TStringList;
      function StreamRequest ( aGeneratedWith: String
                             ; aGenerateTypes: Boolean
                             ; aGenerateHeaderNameSpaces: Boolean
                             ; aGenerateBodyNameSpaces: Boolean
                             ): String;
      function StreamReply ( aGeneratedWith: String
                           ; aGenerateTypes: Boolean
                           ): String;
      function StreamFault (aGeneratedWith: String; aGenerateTypes: Boolean): String;
      function endpointConfigAsXml: TXml;
      procedure endpointConfigFromXml (aXml: TXml);
      function OptionsAsXml: TXml;
      procedure OptionsFromXml(aXml: TXml);
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

  TWsdlParts = class (TStringList)
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

  TWsdlMessages = class (TStringList)
  private
    function GetMessage(Index: integer): TWsdlMessage;
  public
    property Messages [Index: integer]: TWsdlMessage read GetMessage;
    procedure DeleteMessage (aMessage: TWsdlMessage); overload;
    procedure DeleteMessage (aIndex: Integer); overload;
    procedure Clear; override;
  end;

  { TWsdlMessage }

  TWsdlMessage = class(TWsdlBinder)
  private
    public
//      Patterns: TStringList;
      rpyBodyBind: TCustomBindable; //if Assigned, the body of the reply
      ColumnXmls: TBindableList;
      Documentation: String;
      Disabled: Boolean;
      function CheckValues(aOperation: TWsdlOperation): Boolean;
      procedure corBindsInit(aOperation: TWsdlOperation);
      procedure Clean;
      constructor Create; Overload;
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
function SwiftStrToNumber (aString: String): Extended;
function SwiftNumberToStr (aNumber: Extended): String;
function DateTimeToTandemJulianStr (aDateTime: TDateTime): String;
function RoundedX (aSource, aNumber: Extended): Extended;
function RandomX (aLow, aHigh: Extended): Extended;
function FormatDateX (aDate: TDateTime; Mask: String): String;
function GenerateRandomId: String;
function dbLookUp (aTable, aValueColumn, aReferenceColumn, aReferenceValue: String): String;
function NonceAsString: String;
function xsdDateTime(aDT: TDateTime): String;
function XmlToDateTime (aString: String): TDateTime;
function xsdNowAsDateTime: String;
function sblNowAsDateTime: String;
function StringHasRegExpr (aString, aExpr: String): String;
function StringMatchesRegExpr (aString, aExpr: String): String;
procedure mergeGroup (aDstGroup, aSrcGroup: TObject);
function wsdlFetchFirstMessage (aObject: TObject; aOperation: String): Extended;
function wsdlFetchNextMessage (aObject: TObject; aOperation: String): Extended;
procedure wsdlRequestOperation (aObject: TObject; aOperation: String);
procedure wsdlSendOperationRequest (aOperation, aCorrelation: String);
procedure wsdlSendOperationRequestLater (aOperation, aCorrelation, aLater: String);
function RefuseHttpConnections (aObject: TObject; aLater, aWhile: Extended): Extended;
function Sum (aSAObject, aSEObject: TObject): Extended;
procedure AssignRecurring (aDAObject, aDEObject, aSAObject, aSEObject: TObject);
procedure CheckRecurringElement (aDAObject, aDEObject, aSAObject, aSEObject: TObject);
procedure EnableMessage (aOperation: TWsdlOperation);
procedure EnableAllMessages;
procedure DisableMessage (aOperation: TWsdlOperation);
function OccurrencesX (aObject: TObject): Extended;
function LengthX (arg: String): Extended;
function StrToFloatX (arg: String): Extended;
procedure SleepX (aMS: Extended);
function SubStringX ( s: String; i, c: Extended): String;
function isAccordingSchema (aObject: TObject): Extended;
function isAssigned (aObject: TObject): Extended;
procedure ResetOperationCounters;
procedure ResetEnvVars (aRegExp: String);
procedure ResetEnvVar (aName: String);
function setEnvNumber (aName: String; aValue: Extended): Extended;
function setEnvVar (aName, aValue: String): String;
procedure AddRemark (aObject: TObject; aString: String);
procedure SetLogGroupId (aObject: TObject; aString: String);
procedure ExecuteScript (aObject: TObject; aString: String);
procedure SjowMessage (aString: String);
function decVarNumber (aName: String): Extended;
function getVarNumber (aName: String): Extended;
function incVarNumber (aName: String): Extended;
function getVarNumberDef (aName: String; aDefault: Extended): Extended;
function getVar (aName: String): String;
function getVarDef (aName, aDefault: String): String;
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
procedure RaiseSoapFault (aOperation: TWsdlOperation; faultcode, faultstring, faultactor, detail: String);

function wsdlConvertSdfFrom36 (aXml: TXml): Boolean;
procedure AcquireLock;
procedure ReleaseLock;
procedure AcquireEnvVarLock;
procedure ReleaseEnvVarLock;

var
  allOperations, allAliasses: TWsdlOperations;
  allOperationsRpy: TWsdlOperations;
  _ProgName: String;
  _WsdlVars: TStringList;
  _WsdlRequestOperation: VFunctionOS;
  _WsdlFetchFirstMessage: XFunctionOS;
  _WsdlFetchNextMessage: XFunctionOS;
  _WsdlExecuteScript: VFunctionOS;
  _WsdlAddRemark: VFunctionOS;
  _WsdlSetLogGroupId: VFunctionOS;
  _WsdlSendOperationRequest: VFunctionSS;
  _WsdlSendOperationRequestLater: VFunctionSSI;
  _WsdlRefuseHttpConnections: XFunctionOXX;
  _WsdlHostName: String;
  _WsdlPortNumber: String;
  _WsdlProgName: String;
  _WsdlRtiXsd: TXsd;
  _WsdlRtiXml: TXml;
  _WsdlWsaXsd: TXsd;
  _WsdlMqHeaderXsd: TXsd;
  _WsdlStompHeaderXsd: TXsd;
  _WsdlEmailXsd: TXsd;
  _WsdlServiceDefinitionXsd: TXsd;
  _WsdlListOfFilesXsd: TXsd;
  _WsdlTacoConfigXsd: TXsd;
  _WsdlUserNameTokenNumber: Integer;
  _WsdlDisableOnCorrelate: Boolean;
  _WsdlOnMessageChange: TOnMessageChange;
  _WsdlOnOperationChange: TOnOperationChange;
  _OnChange: TOnChange;
  _OnBeginUpdate, _OnEndUpdate: TProcedure;
  _ipmGun: Boolean;
  _WsdlDbsEnabled: Boolean;
  _WsdlDbsConnector: TSQLConnector;
  _WsdlDbsTransaction: TSQLTransaction;
  _WsdlDbsConnectorType: String;
  _WsdlDbsDatabaseName: String;
  _WsdlDbsParams: String;
  _WsdlDbsPassword: String;

  UILock: TCriticalSection;
  EnvVarLock: TCriticalSection;
  OnNotify: TOnStringEvent;
  doOperationLock, doUILock: Boolean;

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
   , Dialogs
   , Controls
   , igGlobals
   , RegExpr
   , xmlUtilz
   , Forms
   , Math
   , base64
   , HashUtilz
   , IdURI
   , SwiftUnit
   , xmlxsdparser
   , xmlio
   , Logz
   ;

{ TWsdl }

procedure Notify(aString: String);
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

procedure SetLogGroupId(aObject: TObject; aString: String);
begin
  if not Assigned (_WsdlSetLogGroupId) then
    raise Exception.Create('No SetLogGroupId event assigned: intention was to set: ' + aString);
  _WsdlSetLogGroupId (aObject, aString);
end;

procedure ExecuteScript(aObject: TObject; aString: String);
begin
  if not Assigned (_WsdlExecuteScript) then
    raise Exception.Create('No OnExecuteScript event assigned: intention was to execute script: ' + aString);
  _WsdlExecuteScript (aObject, aString);
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

function SwiftStrToNumber (aString: String): Extended;
var
  x: Integer;
begin
  x := Pos(',', aString);
  if x > 0 then
    aString[x] := '.';
  result := StrToFloat (aString);
end;

function SwiftNumberToStr (aNumber: Extended): String;
var
  x: Integer;
begin
  result := FloatToStr (aNumber);
  x := Pos('.', result);
  if x > 0 then
    result [x] := ','
  else
    result := result + ',';
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
  aPos := Pos (LongMonthNames [2], Mask); {februari}
  if (aPos > 0)then
  begin
    Delete (DateString, aPos, Length (LongMonthNames [2]));
    Insert (LongMonthNames [StrToInt (aMonth)], DateString, aPos);
  end
  else
  begin
    aPos := Pos (ShortMonthNames [2], Mask); {feb}
    if (aPos > 0)then
    begin
      Delete (DateString, aPos, Length (ShortMonthNames [2]));
      Insert (ShortMonthNames [StrToInt (aMonth)], DateString, aPos);
      {Mask en DateString moeten even lang blijven}
      Delete (Mask, aPos, Length (ShortMonthNames [2]));
      Insert (ShortMonthNames [StrToInt (aMonth)], Mask, aPos);
    end;
  end;

  {Overnemen van de dag van week als tekst}
  aPos := Pos (LongDayNames [1], Mask); {zondag}
  if (aPos > 0)then
  begin
    Dow := DayOfWeek (aDate);
    Delete (DateString, aPos, Length (LongDayNames [1]));
    Insert (LongDayNames [Dow], DateString, aPos);
    Delete (Mask, aPos, Length (LongDayNames [1]));
    Insert (LongDayNames [Dow], Mask, aPos);
  end
  else
  begin
    aPos := Pos (ShortDayNames [1], Mask); {zon}
    if (aPos > 0)then
    begin
      Dow := DayOfWeek (aDate);
      Delete (DateString, aPos, Length (ShortDayNames [1]));
      Insert (ShortDayNames [Dow], DateString, aPos);
      Delete (Mask, aPos, Length (ShortDayNames [1]));
      Insert (ShortDayNames [Dow], Mask, aPos);
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

function NonceAsString: String;
begin
  result := Copy (Sha1 (xsdNowAsDateTime), 1, 16);
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

function StringHasRegExpr (aString, aExpr: String): String;
begin
  result := '';
  if (aString <> '')
  and (aExpr <> '') then
  with TRegExpr.Create do
  try
    Expression := aExpr;
    if (Exec(aString)) then
      result := Match[0];
  finally
    Free;
  end;
end;

procedure mergeGroup (aDstGroup, aSrcGroup: TObject);
var
  swapTagName: String;
begin
  with TXml ((aDstGroup as YYSType).yy.yyPointer) do
  begin
    swapTagname := TagName;
    TagName := TXml ((aSrcGroup as YYSType).yy.yyPointer).TagName;
    try
      CopyValues (TXml ((aSrcGroup as YYSType).yy.yyPointer), True, True);
      MergePreviousChecked;
    finally
      TagName := swapTagName;
    end;
  end;
end;

function wsdlFetchFirstMessage (aObject: TObject; aOperation: String): Extended;
begin
  if not Assigned (_WsdlFetchFirstMessage) then
    raise Exception.Create('wsdlFetchFirstMessage: implementation missing');
  result := _WsdlFetchFirstMessage (aObject, aOperation);
end;

function wsdlFetchNextMessage (aObject: TObject; aOperation: String): Extended;
begin
  if not Assigned (_WsdlFetchNextMessage) then
    raise Exception.Create('wsdlFetchNextMessage: implementation missing');
  result := _WsdlFetchNextMessage (aObject, aOperation);
end;

procedure wsdlRequestOperation (aObject: TObject; aOperation: String);
begin
  if not Assigned (_wsdlRequestOperation) then
    raise Exception.Create('wsdlRequestOperation: implementation missing');
  _wsdlRequestOperation (aObject, aOperation);
end;

procedure wsdlSendOperationRequest (aOperation, aCorrelation: String);
begin
  if not Assigned (_WsdlSendOperationRequest) then
    raise Exception.Create('wsdlSendOperationRequest: implementation missing');
  _WsdlSendOperationRequest (aOperation, aCorrelation);
end;

function RefuseHttpConnections (aObject: TObject; aLater, aWhile: Extended): Extended;
begin
  if not Assigned (_WsdlRefuseHttpConnections) then
    raise Exception.Create('WsdlRefuseHttpConnections: implementation missing');
  _WsdlRefuseHttpConnections (aObject, aLater, aWhile);
  result := 1;
end;

procedure wsdlSendOperationRequestLater (aOperation, aCorrelation, aLater: String);
begin
  if not Assigned (_WsdlSendOperationRequestLater) then
    raise Exception.Create('wsdlSendOperationRequestLater: implementation missing');
  _WsdlSendOperationRequestLater (aOperation, aCorrelation, StrToIntDef (aLater, 3000));
end;

function Sum (aSAObject, aSEObject: TObject): Extended;
  procedure _Sum (var aSum: Extended; aXml: TXml; aCaption: String);
  var
    x, p: Integer;
    xSum: Extended;
    xCaption: String;
  begin
    if not aXml.Checked then
      Exit;
    p := Pos('.', aCaption);
    if p = 0 then
    begin
      if aXml.Name = aCaption then
      begin
        try
          xSum := StrToFloat(aXml.Value);
          aSum := aSum + xSum;
        except
        end;
      end;
      Exit;
    end;
    xCaption := Copy (aCaption, 1, p - 1);
    if xCaption <> aXml.Name then
      Exit;
    xCaption := Copy (aCaption, p + 1, 1000000);
    for x := 0 to aXml.Items.Count - 1 do
      _Sum ( result
           , aXml.Items.XmlItems[x]
           , xCaption
           );
  end;
var
  sa, se: TXml;
  saCAption, seCaption: String;
begin
  result := 0;
  if not (TCustomBindable ((aSAObject as YYSType).yy.yyPointer) is TXml)
  or not (TCustomBindable ((aSEObject as YYSType).yy.yyPointer) is TXml) then
    raise Exception.Create ('Sum: Only implemented for XML elements');
  sa := TCustomBindable((aSAObject as YYSType).yy.yyPointer) as TXml;
  saCaption := sa.FullUQCaption;
  se := TCustomBindable((aSEObject as YYSType).yy.yyPointer) as TXml;
  seCaption := se.FullUQCaption;
  if (AnsiLeftStr(seCaption, Length (saCaption)) <> saCaption)
  or (saCaption = seCaption) then
    raise Exception.Create( 'Sum: Element '
                          + seCaption
                          + ' must be a child of'
                          + saCaption
                          );
  _Sum ( result
       , sa
       , sa.Name
       + AnsiRightStr(seCaption, Length (seCaption) - Length (saCAption))
       );
end;

procedure AssignRecurring (aDAObject, aDEObject, aSAObject, aSEObject: TObject);
  procedure _AssignRecurring (da, de, sa, se: TXml);
  var
    dp, sp, currentDE, currentSE: TXml;
    daCaption, deCaption, saCaption, seCaption, deFind, seFind, swapTagname: String;
    x: Integer;
  begin
    daCaption := da.FullUQCaption;
    deCaption := de.FullUQCaption;
    saCaption := sa.FullUQCaption;
    seCaption := se.FullUQCaption;
    if AnsiLeftStr(deCaption, Length (daCaption)) <> daCaption then
      raise Exception.Create( 'AssignRecurring: Element '
                            + deCaption
                            + ' must be the same or child of'
                            + daCaption
                            );
    if AnsiLeftStr(seCaption, Length (saCaption)) <> saCaption then
      raise Exception.Create( 'AssignRecurring: Element '
                            + seCaption
                            + ' must be the same or child of'
                            + saCaption
                            );
    deFind := AnsiRightStr(deCaption, Length (deCaption) - Length (daCaption) - 1);
    seFind := AnsiRightStr(seCaption, Length (SeCaption) - Length (saCaption) - 1);
    {reset all destination occurrences}
    dp := da.Parent as TXml;
    for x := 0 to dp.Items.Count - 1 do
      if dp.Items.XmlItems[x].TagName = da.TagName then
        dp.Items.XmlItems[x].Checked := False;

    sp := sa.Parent as TXml;
    x := 0;
    while (x < sp.Items.Count)
    and (x < dp.Items.Count) do
    begin
      if (sp.Items.XmlItems[x].Checked) then
      begin
        if seFind = '' then
          currentSe := sp.Items.XmlItems[x]
        else
          currentSe := sp.Items.XmlItems[x].FindUQXml(sp.Items.XmlItems[x].Name + '.' + seFind);
        if Assigned (currentSe)
        and currentSe.Checked then
        begin
          if deFind = '' then
            currentDe := dp.Items.XmlItems[x]
          else
            currentDe := dp.Items.XmlItems[x].FindUQXml(dp.Items.XmlItems[x].TagName + '.' + deFind);
          if Assigned (currentDe) then
          begin
            currentDE.Checked := True;
            currentDE.Value := currentSE.Value;
            swapTagname := currentDE.Name;
            currentDE.Name := currentSE.Name;
            try
              currentDE.CopyValues (currentSE, True, True);
              currentDE.MergePreviousChecked;
            finally
              currentDE.Name := swapTagName;
            end;
          end;
        end;
      end;
      Inc (x);
    end;
  end;
begin
  if not (TCustomBindable ((aDAObject as YYSType).yy.yyPointer) is TXml)
  or not (TCustomBindable ((aDEObject as YYSType).yy.yyPointer) is TXml)
  or not (TCustomBindable ((aSAObject as YYSType).yy.yyPointer) is TXml)
  or not (TCustomBindable ((aSEObject as YYSType).yy.yyPointer) is TXml) then
    raise Exception.Create ('AssignRecurring: Only allowed with XML elements');
  _AssignRecurring ( TXml ((aDAObject as YYSType).yy.yyPointer)
                   , TXml ((aDEObject as YYSType).yy.yyPointer)
                   , TXml ((aSAObject as YYSType).yy.yyPointer)
                   , TXml ((aSEObject as YYSType).yy.yyPointer)
                   );
end;

procedure CheckRecurringElement (aDAObject, aDEObject, aSAObject, aSEObject: TObject);
var
  da, de, dp, sa, se, sp, currentDE, currentSE: TXml;
  daCaption, deCaption, saCaption, seCaption, deFind, seFind: String;
  d, s: Integer;
begin
  if not (TCustomBindable ((aDAObject as YYSType).yy.yyPointer) is TXml) then
    raise Exception.Create ('CheckRecurringElement: Only allowed with XML elements');
  da := TXml ((aDAObject as YYSType).yy.yyPointer);
  de := TXml ((aDEObject as YYSType).yy.yyPointer);
  sa := TXml ((aSAObject as YYSType).yy.yyPointer);
  se := TXml ((aSEObject as YYSType).yy.yyPointer);
  daCaption := da.FullUQCaption;
  deCaption := de.FullUQCaption;
  saCaption := sa.FullUQCaption;
  seCaption := se.FullUQCaption;

  if AnsiLeftStr(deCaption, Length (daCaption)) <> daCaption then
    raise Exception.Create( 'CheckRecurringElement: Element '
                          + deCaption
                          + ' must be child of'
                          + daCaption
                          );
  if AnsiLeftStr(seCaption, Length (saCaption)) <> saCaption then
    raise Exception.Create( 'CheckRecurringElement: Element '
                          + deCaption
                          + ' must be child of'
                          + daCaption
                          );
  deFind := AnsiRightStr(deCaption, Length (deCaption) - Length (daCaption) - 1);
  seFind := AnsiRightStr(seCaption, Length (SeCaption) - Length (saCaption) - 1);
  dp := da.Parent as TXml;
  for d := 0 to dp.Items.Count - 1 do
    if dp.Items.XmlItems[d].TagName = da.TagName then
    begin
      dp.Items.XmlItems[d].fPrevChecked := dp.Items.XmlItems[d].fChecked;
      dp.Items.XmlItems[d].fChecked := False;
    end;

  sp := sa.Parent as TXml;
  for s := 0 to sp.Items.Count - 1 do
  begin
    if (sp.Items.XmlItems[s].TagName = sa.TagName)
    and (sp.Items.XmlItems[s].Checked) then
    begin
      if seFind = '' then
        currentSe := sp.Items.XmlItems[s]
      else
        currentSe := sp.Items.XmlItems[s].FindUQXml(sp.Items.XmlItems[s].Name + '.' + seFind);
      if Assigned (currentSe)
      and currentSe.Checked then
      begin
        for d := 0 to dp.Items.Count - 1 do
        begin
          if (dp.Items.XmlItems[d].TagName = da.TagName)
          and (not dp.Items.XmlItems[d].Checked) then
          begin
            if deFind = '' then
              currentDe := dp.Items.XmlItems[d]
            else
              currentDe := dp.Items.XmlItems[d].FindUQXml(dp.Items.XmlItems[d].TagName + '.' + deFind);
            if Assigned (currentDe)
            and currentDe.Checked then
              dp.Items.XmlItems[d].fChecked := (currentSe.Value = currentDe.Value);
          end;
        end;
      end;
    end;
  end;
end;

procedure EnableMessage (aOperation: TWsdlOperation);
begin
  if Assigned (aOperation)
  and Assigned (aOperation.CorrelatedMessage)
  and aOperation.CorrelatedMessage.Disabled then
  begin
    aOperation.CorrelatedMessage.Disabled := False;
    if Assigned (_WsdlOnMessageChange) then
      _WsdlOnMessageChange (aOperation.CorrelatedMessage);
  end;
end;

procedure EnableAllMessages;
var
  o, m: Integer;
begin
  for o := 0 to allOperations.Count - 1 do
  begin
    with allOperations.Operations[o] do
    begin
      for m := 0 to Messages.Count - 1 do
      begin
        Messages.Messages[m].Disabled := False;
      end;
    end;
  end;
  if Assigned (_OnChange) then
    _OnChange;
end;

procedure DisableMessage (aOperation: TWsdlOperation);
begin
  if Assigned (aOperation)
  and Assigned (aOperation.CorrelatedMessage)
  and not aOperation.CorrelatedMessage.Disabled then
  begin
    aOperation.CorrelatedMessage.Disabled := True;
    if Assigned (_WsdlOnMessageChange) then
      _WsdlOnMessageChange (aOperation.CorrelatedMessage);
  end;
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

function SubStringX ( s: String
                    ; i: Extended
                    ; c: Extended
                    ): String;

begin
  result := Copy (s, Trunc (i), Trunc (c));
end;

function StrToFloatX (arg: String): Extended;
begin
  result := StrToFloatDef(arg, 0);
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

procedure SjowMessage (aString: String);
begin
  if not Assigned (wsdlz.OnNotify) then
    raise Exception.Create('No OnNotify event assigned: intention was to show message: ' + LineEnding + aString);
  wsdlz.OnNotify (aString);
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

procedure ResetEnvVars (aRegExp: String);
var
  i: Integer;
begin
  AcquireEnvVarLock;
  try
    with TRegExpr.Create do
    try
      Expression := aRegExp;
      for I := _wsdlVars.Count - 1 downto 0 do
        if (Exec(_wsdlVars.Names[i])) then
          _WsdlVars.Delete (i);
    finally
      Free;
    end;
  finally
    ReleaseEnvVarLock;
  end;
end;

procedure ResetEnvVar (aName: String);
var
  i: Integer;
begin
  AcquireEnvVarLock;
  try
    i := _wsdlVars.IndexOfName(aName);
    if i >= 0 then
      _WsdlVars.Delete (i);
  finally
    ReleaseEnvVarLock;
  end;
end;

function setEnvNumber (aName: String; aValue: Extended): Extended;
begin
  result := aValue;
  setEnvVar (aName, FloatToStr(aValue));
end;

function setEnvVar (aName, aValue: String): String;
begin
  AcquireEnvVarLock;
  try
    result := _WsdlVars.Values [aName];
    _WsdlVars.Values [aName] := aValue;
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

function getVarDef (aName, aDefault: String): String;
var
  i: Integer;
begin
  AcquireEnvVarLock;
  try
    i := _wsdlVars.IndexOfName(aName);
    if i >= 0 then
      result := _WsdlVars.ValueFromIndex [i]
    else
      Result := aDefault;
  finally
    ReleaseEnvVarLock;
  end;
end;

function getVar (aName: String): String;
begin
  result := getVarDef(aName, '');
end;

function getVarNumberDef (aName: String; aDefault: Extended): Extended;
begin
  result := StrToFloatDef (getVar (aName), aDefault);
end;

function getVarNumber (aName: String): Extended;
begin
  result := getVarNumberDef(aName, 0);
end;

function incVarNumber (aName: String): Extended;
begin
  AcquireEnvVarLock; // nests lock....
  try
    result := getVarNumberDef(aName, 0) + 1;
    setEnvNumber(aName, result);
  finally
    ReleaseEnvVarLock;
  end;
end;

function decVarNumber (aName: String): Extended;
begin
  AcquireEnvVarLock; // nests lock....
  try
    result := getVarNumberDef(aName, 0) + 1;
    setEnvNumber(aName, result);
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
  result := igGlobals.getUserName;
end;

function wsdlOperationName(aOper: TWsdlOperation): String;
begin
  result := aOper.reqTagName;
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
  with TIdSync.Create do
  begin
    try
      SynchronizeMethod (aOperation.doPromptReply);
    finally
      free;
    end;
  end;
end;

procedure PromptRequest(aOperation: TWsdlOperation);
begin
  with TIdSync.Create do
  begin
    try
      SynchronizeMethod (aOperation.doPromptRequest);
    finally
      free;
    end;
  end;
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
    AddAttribute(TXmlAttribute.CreateAsString('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance'));
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
  finally
    Free;
  end;
end;

constructor TWsdl.Create(aElementsWhenRepeatable, aDefaultElementsWhenRepeatable: Integer; aOperationsWithEndpointOnly: Boolean);
begin
  inherited Create;
  xsdElementsWhenRepeatable := aElementsWhenRepeatable;
  xsdDefaultElementsWhenRepeatable := aDefaultElementsWhenRepeatable;
  OperationsWithEndpointOnly := aOperationsWithEndpointOnly;
  Services := TWsdlServices.Create;
  Services.Sorted := True;
  if xsdElementsWhenRepeatable > 0 then
    XsdDescr := TXsdDescr.Create(xsdElementsWhenRepeatable)
  else
    XsdDescr := TXsdDescr.Create(xsdDefaultElementsWhenRepeatable);
  sdfXsdDescrs := TXsdDescrList.Create;
  IpmDescrs := TIpmDescrs.Create;
  IpmDescrs.Sorted := False;
  ExtraXsds := TStringList.Create;
  fMssgs := TWsdlMsgDescrs.Create;
  fMssgs.Sorted := True;
  fMssgs.Duplicates := dupError;
  fOpers := TWsdlOperations.Create;
  fOpers.Sorted := True;
  fOpers.Duplicates := dupError;
  fStrs := TStringList.Create;
  fStrs.NameValueSeparator := '=';
  xsdElementsWhenRepeatable := -1;
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
  ExtraXsds.Clear;
  ExtraXsds.Free;
  fMssgs.ClearListOnly;
  FreeAndNil(fMssgs);
  fOpers.ClearListOnly;
  FreeAndNil(fOpers);
  FreeAndNil(fStrs);
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

procedure TWsdl.LoadFromSchemaFile (aFileName : String; aOnError: TOnErrorEvent);
  procedure _LoadFromXml (aXml: TXml; aFileName: String);
    procedure _LoadFromFile (aFileName : String);
    var
      xXml: TXml;
    begin
      xXml := TXml.Create;
      try
        xXml.LoadFromFile(aFileName, aOnError);
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
        _LoadFromFile (ExpandRelativeFileName(aFileName, Attributes.ValueByTag[tagLocation]));
    end;
    for x := 0 to aXml.Items.Count - 1 do with aXml.Items do
    begin
      if (XmlItems[x].Name = tagTypes)
      or (XmlItems[x].Name = tagSchema) then
        XsdDescr.AddXsdFromXml (XmlItems[x], aFileName, aOnError);
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
  if UpperCase(LeftStr(aFileName, 7)) <> 'HTTP://' then
    FileName := ExpandFileNameUTF8(aFileName)
  else
    FileName := aFileName;
  XsdDescr.ReadFileNames.Add(aFileName);
  xXml := TXml.Create;
  try
    xXml.LoadFromFile(aFileName, aOnError);
    xXml.SeparateNsPrefixes;
    xXml.ResolveNameSpaces;
    if (xXml.Name <> tagDefinitions)
    or (     (xXml.NameSpace <> scWsdlNameSpace)
       ) and (xXml.NameSpace <> '')then
      raise Exception.CreateFmt ('%s is not a WSDL file', [aFileName]);
    _LoadFromXml (xXml, FileName);
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


procedure TWsdl.LoadFromSdfFile(aFileName: String);
var
  x, y, z, s, o: Integer;
  xService: TWsdlService;
  xOperation: TWsdlOperation;
  xFileNames: TStringList;
  xServiceName, xOperationName: String;
  aXml, fXml, yXml, zXml: TXml;
  xIpmDescr: TIpmDescr;
  xDescrType: TIpmDescrType;
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
{}{
    if xFileNames.Find(ExpandUNCFileName (ExpandRelativeFileName (aFileName, _aFileName)), f) then
    begin
      with xXsdDescr. [f] do
      begin
        for x := 0 to ElementDefs.Count - 1 do
        begin
          if ElementDefs.Xsds [x].Name = aXsdName then
          begin
            result := ElementDefs.Xsds [x];
            exit;
          end;
        end;
      end;
    end;
{}
  end;
  function _LoadCobolMsg (aLabel: String; sXml: TXml): TIpmItem;
  var
    f: Integer;
  begin
    result := nil;
    if xFileNames.Find ( ExpandUNCFileNameUTF8(ExpandRelativeFileName ( aFileName
                                                                    , sXml.Items.XmlValueByTag ['DescriptionFile']
                                                                    )
                                           ) { *Converted from ExpandUNCFileName* }
                       , f
                       ) then
    begin
      if IpmDescrs.Strings [f] <> xFilenames.Strings [f] then
        raise Exception.Create ('Internal error');
      result := TIpmItem.Create (IpmDescrs.IpmDescrs [f].IpmItem); // ipmdescr also destroys ipmitems
    end;
  end;
  function _LoadCobolRecog (aXml: TXml): TStringList;
  var
    x, y: Integer;
    xXml, yXml: TXml;
    recog: TRecognition;
  begin
    result := TStringList.Create;
    for x := 0 to aXml.Items.Count - 1 do
    begin
      xXml := aXml.Items.XmlItems[x];
      if xXml.Name = 'HttpDocument' then
      begin
        xOperation.RecognitionType := rtDocument;
        recog := TRecognition.Create;
        recog.RecognitionType := xOperation.RecognitionType;
        recog.Value := Trim (xXml.Value);
        result.AddObject ('', recog);
      end;
      if xXml.Name = 'HTTPHeader' then
      begin
        xOperation.RecognitionType := rtHeader;
        recog := TRecognition.Create;
        recog.RecognitionType := xOperation.RecognitionType;
        recog.Name := xXml.Items.XmlValueByTag['Name'];
        recog.Value := Trim (xXml.Items.XmlValueByTag['Value']);
        result.AddObject ('', recog);
      end;
      if xXml.Name = 'SubStrings' then
      begin
        xOperation.RecognitionType := rtSubString;
        for y := 0 to xXml.Items.Count - 1 do
        begin
          yXml := xXml.Items.XmlItems [y];
          recog := TRecognition.Create;
          recog.RecognitionType := xOperation.RecognitionType;
          recog.Start := StrToIntDef (yXml.Items.XmlValueByTag['Start'], 1);
          recog.Length := StrToIntDef (yXml.Items.XmlValueByTag['Length'], 1);
          recog.Value := Trim (yXml.Items.XmlValueByTag['Value']);
          result.AddObject ('', recog);
        end;
      end;
    end;
  end;
  procedure _LoadXsdMsg (aLabel: String; sXml: TXml; aXsd: TXsd);
  var
    xXsd: TXsd;
    xXsdDescr: TXsdDescr;
    xFileName: String;
  begin
    xFileName := ExpandUNCFileNameUTF8(ExpandRelativeFileName
                          (aFileName, sXml.Items.XmlValueByTag ['DescriptionFile'])
                        ); { *Converted from ExpandUNCFileName* }
    if xsdElementsWhenRepeatable > 0 then
      xXsdDescr := TXsdDescr.Create(xsdElementsWhenRepeatable)
    else
      xXsdDescr := TXsdDescr.Create(xsdDefaultElementsWhenRepeatable);
    sdfXsdDescrs.AddObject('', xXsdDescr);
    try
      xXsdDescr.AddXsdFromFile(xFileName, _OnParseErrorEvent);
    except
      on E: Exception do
        raise Exception.Create('Error opening ' + xFileName + ': ' + e.Message);
    end;
    aXsd.ElementName := xOperation.Name;
    xXsd := _refXsd ( xXsdDescr, sXml.Items.XmlValueByTag ['ElementName']);
    if Assigned (xXsd) then
      aXsd.sType.ElementDefs.AddObject('', xXsd);
  end;
  procedure _LoadJsonMsg (aLabel: String; sXml: TXml; aXsd: TXsd);
  var
    xXsdDescr: TXsdDescr;
    xFileName: String;
  begin
    xFileName := ExpandUNCFileNameUTF8(ExpandRelativeFileName
                          (aFileName, sXml.Items.XmlValueByTag ['DescriptionFile'])
                        ); { *Converted from ExpandUNCFileName* }
    if xsdElementsWhenRepeatable > 0 then
      xXsdDescr := TXsdDescr.Create(xsdElementsWhenRepeatable)
    else
      xXsdDescr := TXsdDescr.Create(xsdDefaultElementsWhenRepeatable);
    sdfXsdDescrs.AddObject('', xXsdDescr);
    try
      XmlUtil.CreateXsdFromJsonSchemaFile(xXsdDescr, xFileName);
    except
      on E: Exception do
        raise Exception.Create('Error opening ' + xFileName + ': ' + e.Message);
    end;
    aXsd.ElementName := xOperation.Name + '_' + aLabel;
  {}{
    xXsd := _refXsd ( xXsdDescr, sXml.Items.XmlValueByTag ['ElementName']);
    if Assigned (xXsd) then
      aXsd.sType.ElementDefs.AddObject('', xXsd);
  {}
    if xXsdDescr.TypeDef.ElementDefs.Count > 0 then
    begin
      aXsd.sType.ElementDefs.AddObject('', xXsdDescr.TypeDef.ElementDefs.Xsds[0]);
    end;
  end;
  procedure _LoadSwiftMtMsg (aLabel: String; sXml: TXml; aXsd: TXsd);
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
    xpXmls := TXml.CreateAsString('expansions', '');
    try
      xXsdDescr := TXsdDescr.Create(1);
      sdfXsdDescrs.AddObject('', xXsdDescr);
{$ifdef XMLDOM}
      xXsdDescr.AddFromSchemaDef
        (LoadXMLSchema(_swiftMTXsdFileName).SchemaDef
        );
{$endif}
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
          xXsdDescr.LoadXsdFromFile
                      (ExpandUNCFileNameUTF8
                        (ExpandRelativeFileName
                          (aFileName, sXml.Items.XmlItems[x].Value)
                        )
                      , _OnParseErrorEvent
                      );
        if sXml.Items.XmlItems[x].Name = 'DescriptionExpansionFile' then
          with xpXmls.AddXml(TXml.Create) do
            LoadFromFile(ExpandUNCFileNameUTF8(ExpandRelativeFileName
                              (aFileName, sXml.Items.XmlItems[x].Value)
                            ) { *Converted from ExpandUNCFileName* }
                        , nil
                        );
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
  end;
  function _LoadXsdRecog (aXml: TXml): TStringList;
  var
    x: Integer;
    xXml: TXml;
    recog: TRecognition;
  begin
    result := TStringList.Create;
    for x := 0 to aXml.Items.Count - 1 do
    begin
      xXml := aXml.Items.XmlItems[x];
      if xXml.Name = 'HttpDocument' then
      begin
        xOperation.RecognitionType := rtDocument;
        recog := TRecognition.Create;
        recog.RecognitionType := xOperation.RecognitionType;
        recog.Value := Trim (xXml.Value);
        result.AddObject ('', recog);
      end;
      if xXml.Name = 'HTTPHeader' then
      begin
        xOperation.RecognitionType := rtHeader;
        recog := TRecognition.Create;
        recog.RecognitionType := xOperation.RecognitionType;
        recog.Name := xXml.Items.XmlValueByTag['Name'];
        recog.Value := Trim (xXml.Items.XmlValueByTag['Value']);
        result.AddObject ('', recog);
      end;
      if xXml.Name = 'XmlElement' then
      begin
        xOperation.RecognitionType := rtXml;
        recog := TRecognition.Create;
        recog.RecognitionType := xOperation.RecognitionType;
        recog.Name := xXml.Items.XmlValueByTag['Path'];
        recog.Value := Trim (xXml.Items.XmlValueByTag['Value']);
        result.AddObject ('', recog);
      end;
    end;
  end;
  function _LoadJsonRecog (aXml: TXml): TStringList;
  var
    x: Integer;
    xXml: TXml;
    recog: TRecognition;
  begin
    result := TStringList.Create;
    for x := 0 to aXml.Items.Count - 1 do
    begin
      xXml := aXml.Items.XmlItems[x];
      if xXml.Name = 'HttpDocument' then
      begin
        xOperation.RecognitionType := rtDocument;
        recog := TRecognition.Create;
        recog.RecognitionType := xOperation.RecognitionType;
        recog.Value := Trim (xXml.Value);
        result.AddObject ('', recog);
      end;
      if xXml.Name = 'HTTPHeader' then
      begin
        xOperation.RecognitionType := rtHeader;
        recog := TRecognition.Create;
        recog.RecognitionType := xOperation.RecognitionType;
        recog.Name := xXml.Items.XmlValueByTag['Name'];
        recog.Value := Trim (xXml.Items.XmlValueByTag['Value']);
        result.AddObject ('', recog);
      end;
      if xXml.Name = 'XmlElement' then
      begin
        xOperation.RecognitionType := rtXml;
        recog := TRecognition.Create;
        recog.RecognitionType := xOperation.RecognitionType;
        recog.Name := xXml.Items.XmlValueByTag['Path'];
        recog.Value := Trim (xXml.Items.XmlValueByTag['Value']);
        result.AddObject ('', recog);
      end;
    end;
  end;
begin
  xFileNames := TStringList.Create;
  xFileNames.Sorted := True;
  xFileNames.Duplicates := dupIgnore;
  aXml := TXml.Create(0, _WsdlServiceDefinitionXsd);
  try
    yXml := TXml.Create;
    try
      yXml.LoadFromFile(aFileName, nil);
      wsdlConvertSdfFrom36 (yXml);
      aXml.LoadValues(yXml, True, False, False);
    finally
      yXml.Free;
    end;
    isSoapService := False;
    if not Assigned (aXml) then raise Exception.Create('LoadFromSdfXml: XML not asigned');
    if aXml.Name <> 'ServiceDefinitions' then raise Exception.Create(aFileName + ': No ServerDefinitions');
    if xmlUtil.CheckAndPromptFileNames(aFileName, aXml, True) then
    begin
      if (MessageDlg ( 'Save changed file: '
                     + aFileName
                     , mtConfirmation
                     , [mbYes, mbNo]
                     , 0
                     ) = mrYes
         ) then
        XmlUtil.SaveXmlWithFileNames(aFileName, aXml, True, True);
    end;
    aXml.LoadFromString(aXml.AsText(False, 0, True, False), nil); // get rid of unchecked items
    with aXml.Items do
    begin
      for x := 0 to Count - 1 do
      begin
        with XmlItems[x] do
        begin
          if (Name = 'Service') then
          begin
            if (Items.Count > 0) then
            begin
              with Items.XmlItems[0] do
              begin
                xDescrType := ipmDTFreeFormat;
                if Name = 'Xsd' then xDescrType := ipmDTXsd;
                if Name = 'Json' then xDescrType := ipmDTJson;
                if Name = 'Cobol' then xDescrType := ipmDTCobol;
                if Name = 'Mail' then xDescrType := ipmDTEmail;
                if Name = 'FreeFormat' then xDescrType := ipmDTFreeFormat;
                if Name = 'SwiftMT' then xDescrType := ipmDTSwiftMT;
                if Name = 'Bmtp' then xDescrType := ipmDTBmtp;
{}{
                if (xDescrType = ipmDTFreeFormat) then
                  raise Exception.Create(aFileName + ': Unsupported DescriptionType');
{}
                if (xDescrType = ipmDTEmail)
                and not Assigned (_WsdlEmailXsd) then
                  raise Exception.Create(aFileName + ': EMAIL as description not yet supported');
                xServiceName := Items.XmlValueByTag ['Name'];
                for y := 0 to Items.Count - 1 do
                begin
                  with Items.XmlItems[y] do
                  begin
                    if Name = 'Operation' then
                    begin
                      xOperationName := Items.XmlValueByTag ['Name'];
                      for z := 0 to Items.Count - 1 do
                      begin
                        with Items.XmlItems[z] do
                        begin
                          if (Name = 'Req')
                          or (Name = 'Rpy')
                          or (Name = 'Flt') then
                          begin
                            fXml := Items.XmlItemByTag ['DescriptionFile'];
                            if Assigned (fXml) then
                            begin
                              fXml.Value := CheckAndPromptForExistingFile(fXml.FullIndexCaption, aFileName, fXml.Value);
                              xFilenames.AddObject (fXml.Value, Pointer (xDescrType))
                            end
                            else
                              raise Exception.Create ( 'Service: ' + xServiceName
                                                     + ' Operation: ' + xOperationName
                                                     + '; no descriptionfile specified'
                                                     );
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
      end;
    end;
{}{
    if DescriptionType = ipmDTXsd then
    begin
      for x := 0 to xFileNames.Count - 1 do
        XsdDescr.AddFromSchemaDef(LoadXMLSchema(xFileNames.Strings[x]).SchemaDef);
    end;
{}
    for x := 0 to xFileNames.Count - 1 do
    begin
      if TIpmDescrType (xFileNames.Objects[x]) in [ipmDTCobol, ipmDTBmtp] then
      begin
        try
          xIpmDescr := TIpmDescr.Create;
          xIpmDescr.LoadFromFile(xFileNames.Strings[x], _OnParseErrorEvent);
          IpmDescrs.AddObject(xFileNames.Strings[x], xIpmDescr);
        except
          on e: Exception do
            raise Exception.Create ( 'Error opening Cobol description from '
                                   + xFileNames.Strings[x]
                                   + CRLF
                                   + e.Message
                                   );
        end;
      end;
    end;

    with aXml.Items do
    begin
      for x := 0 to Count - 1 do
      begin
        with XmlItems[x] do
        begin
          if Name = 'Service' then
          begin
            if Items.Count > 0 then
            begin
              with Items.XmlItems[0] do
              begin
                xService := TWsdlService.Create;
                xService.Name := Items.XmlValueByTag ['Name'];
                xService.UseNameSpacePrefixes := Items.XmlCheckedBooleanByTagDef['useNamespacePrefixes', xService.UseNameSpacePrefixes];
                Services.AddObject(xService.Name, xService);
                xService.DescriptionType := ipmDTFreeFormat;
                if Name = 'Xsd' then xService.DescriptionType := ipmDTXsd;
                if Name = 'Json' then xService.DescriptionType := ipmDTJson;
                if Name = 'Cobol' then xService.DescriptionType := ipmDTCobol;
                if Name = 'Mail' then xService.DescriptionType := ipmDTEmail;
                if Name = 'FreeFormat' then xService.DescriptionType := ipmDTFreeFormat;
                if Name = 'SwiftMT' then xService.DescriptionType := ipmDTSwiftMT;
                if Name = 'Bmtp' then xService.DescriptionType := ipmDTBmtp;
                case xService.DescriptionType of
                  ipmDTFreeFormat:
                  begin
                    xService.Name := Items.XmlValueByTagDef ['Name', 'freeFormatService'];
                    for y := 0 to Items.Count - 1 do
                    begin
                      yXml := Items.XmlItems[y];
                      with yXml do
                      begin
                        if Name = 'Operation' then
                        begin
                          xOperation := TWsdlOperation.Create (Self);
                          xOperation.Wsdl := Self;
                          xOperation.WsdlService := xService;
                          xOperation.Name := Items.XmlValueByTagDef ['Name', 'freeFormatOperation' + IntToStr (xService.Operations.Count)];
                          xOperation.reqTagName := xOperation.Name;
                          xOperation.Alias := xOperation.reqTagName;
                          xOperation.rpyTagName := xOperation.Name;
                          xService.Operations.AddObject(xOperation.Name, xOperation);
                          for z := 0 to Items.Count - 1 do
                          begin
                            zXml := Items.XmlItems[z];
                            with zXml do
                            begin
                              if (Name = 'reqRecognition') then
                                xOperation.reqRecognition := _LoadCobolRecog (zXml);
                              if (Name = 'rpyRecognition') then
                                xOperation.rpyRecognition := _LoadCobolRecog (zXml);
                            end;
                          end;
                        end;
                      end;
                    end;
                    if xService.Operations.Count = 0 then
                    begin
                      xOperation := TWsdlOperation.Create (Self);
                      xOperation.Wsdl := Self;
                      xOperation.WsdlService := xService;
                      xOperation.Name := 'freeFormatOperation';
                      xOperation.Alias := xOperation.reqTagName;
                      xOperation.reqTagName := xOperation.Name;
                      xOperation.Alias := xOperation.reqTagName;
                      xOperation.rpyTagName := xOperation.Name;
                      xService.Operations.AddObject(xOperation.Name, xOperation);
                    end;
                  end; // ipmDTFreeFormat
                  ipmDTCobol, ipmDTBmtp:
                  begin
                    for y := 0 to Items.Count - 1 do
                    begin
                      with Items.XmlItems[y] do
                      begin
                        if Name = 'Operation' then
                        begin
                          xOperation := TWsdlOperation.Create (Self);
                          xOperation.Wsdl := Self;
                          xOperation.WsdlService := xService;
                          xOperation.Name := Items.XmlValueByTag ['Name'];
                          xOperation.reqTagName := xOperation.Name;
                          xOperation.Alias := xOperation.reqTagName;
                          xOperation.rpyTagName := xOperation.Name;
                          xOperation.CobolEnvironment := ceTandem;
                          if Items.XmlValueByTag ['CobolEnvironment'] = 'IBM Zos' then
                            xOperation.CobolEnvironment := ceIbmZOs;
                          xService.Operations.AddObject(xOperation.Name, xOperation);
                          FreeAndNil (xOperation.freqBind);
                          FreeAndNil (xOperation.frpyBind);
                          FreeAndNil (xOperation.fltBind);
                          for z := 0 to Items.Count - 1 do
                          begin
                            zXml := Items.XmlItems[z];
                            with zXml do
                            begin
                              if (Name = 'Req') then
                                xOperation.reqBind := _LoadCobolMsg (Name, zXml);
                              if (Name = 'Rpy') then
                                xOperation.rpyBind := _LoadCobolMsg (Name, zXml);
                              if (Name = 'Flt') then
                                xOperation.fltBind := _LoadCobolMsg (Name, zXml);
                              if (Name = 'reqRecognition') then
                                xOperation.reqRecognition := _LoadCobolRecog (zXml);
                              if (Name = 'rpyRecognition') then
                                xOperation.rpyRecognition := _LoadCobolRecog (zXml);
                            end;
                          end;
                          if not Assigned (xOperation.reqBind) then
                            xOperation.reqBind := TIpmItem.Create;
                          if not Assigned (xOperation.rpyBind) then
                            xOperation.rpyBind := TIpmItem.Create;
                          if not Assigned (xOperation.fltBind) then
                            xOperation.fltBind := TIpmItem.Create;
                        end;
                      end;
                    end;
                  end; // ipmDTCobol, ipmDTBmtp
                  ipmDTXml: ;
                  ipmDTXsd:
                  begin
                    for y := 0 to Items.Count - 1 do
                    begin
                      with Items.XmlItems[y] do
                      begin
                        if Name = 'Operation' then
                        begin
                          xOperation := TWsdlOperation.Create (Self);
                          xOperation.Wsdl := Self;
                          xOperation.WsdlService := xService;
                          xOperation.Name := Items.XmlValueByTag ['Name'];
                          xOperation.reqTagName := xOperation.Name;
                          xOperation.Alias := xOperation.reqTagName;
                          xOperation.rpyTagName := xOperation.Name;
                          xService.Operations.AddObject(xOperation.Name, xOperation);
                          for z := 0 to Items.Count - 1 do
                          begin
                            zXml := Items.XmlItems[z];
                            with zXml do
                            begin
                              if (Name = 'Req') then
                                _LoadXsdMsg (Name, zXml, xOperation.reqXsd);
                              if (Name = 'Rpy') then
                                _LoadXsdMsg (Name, zXml, xOperation.rpyXsd);
                              if (Name = 'Flt') then
                                _LoadXsdMsg (Name, zXml, xOperation.FaultXsd);
                              if (Name = 'reqRecognition') then
                                xOperation.reqRecognition := _LoadXsdRecog (zXml);
                              if (Name = 'rpyRecognition') then
                                xOperation.rpyRecognition := _LoadXsdRecog (zXml);
                            end;
                          end;
                        end;
                      end;
                    end;
                  end; // ipmDTXsd
                  ipmDTJson:
                  begin
                    for y := 0 to Items.Count - 1 do
                    begin
                      with Items.XmlItems[y] do
                      begin
                        if Name = 'Operation' then
                        begin
                          xOperation := TWsdlOperation.Create (Self);
                          xOperation.Wsdl := Self;
                          xOperation.WsdlService := xService;
                          xOperation.Name := Items.XmlValueByTag ['Name'];
                          xOperation.reqTagName := xOperation.Name;
                          xOperation.Alias := xOperation.reqTagName;
                          xOperation.rpyTagName := xOperation.Name;
                          xService.Operations.AddObject(xOperation.Name, xOperation);
                          for z := 0 to Items.Count - 1 do
                          begin
                            zXml := Items.XmlItems[z];
                            with zXml do
                            begin
                              if (Name = 'Req') then
                                _LoadJsonMsg (Name, zXml, xOperation.reqXsd);
                              if (Name = 'Rpy') then
                                _LoadJsonMsg (Name, zXml, xOperation.rpyXsd);
                              if (Name = 'Flt') then
                                _LoadJsonMsg (Name, zXml, xOperation.FaultXsd);
                              if (Name = 'reqRecognition') then
                                xOperation.reqRecognition := _LoadJsonRecog (zXml);
                              if (Name = 'rpyRecognition') then
                                xOperation.rpyRecognition := _LoadJsonRecog (zXml);
                            end;
                          end;
                        end;
                      end;
                    end;
                  end; // ipmDTJson
                  ipmDTWsdl: ;
                  ipmDTEmail:
                  begin
                    xService.Name := Items.XmlValueByTagDef ['Name', 'mailService'];
                    for y := 0 to Items.Count - 1 do
                    begin
                      zXml := Items.XmlItems[y];
                      with zXml do
                      begin
                        if Name = 'Operation' then
                        begin
                          xOperation := TWsdlOperation.Create (Self);
                          xOperation.Wsdl := Self;
                          xOperation.WsdlService := xService;
                          xOperation.Name := Items.XmlValueByTagDef ['Name', 'mailOperation' + IntToStr (xService.Operations.Count)];
                          xOperation.reqTagName := xOperation.Name;
                          xOperation.Alias := xOperation.reqTagName;
                          xOperation.rpyTagName := xOperation.Name;
                          xService.Operations.AddObject(xOperation.Name, xOperation);
                          xOperation.reqXsd.ElementName := xOperation.Name;
                          xOperation.reqXsd.sType.ElementDefs.AddObject('', _WsdlEmailXsd);
                        end;
                      end;
                    end;
                    if xService.Operations.Count = 0 then
                    begin
                      xOperation := TWsdlOperation.Create (Self);
                      xOperation.Wsdl := Self;
                      xOperation.WsdlService := xService;
                      xOperation.Name := 'mailOperation';
                      xOperation.reqTagName := xOperation.Name;
                      xOperation.Alias := xOperation.reqTagName;
                      xOperation.rpyTagName := xOperation.Name;
                      xService.Operations.AddObject(xOperation.Name, xOperation);
                      xOperation.reqXsd.ElementName := xOperation.Name;
                      xOperation.reqXsd.sType.ElementDefs.AddObject('', _WsdlEmailXsd);
                    end;
                  end; // ipmDTEmail
                  ipmDTSwiftMT:
                  begin
                    if _swiftMTXsdFileName = '' then
                      raise Exception.Create('SwiftXSD file not sprecified');
                    for y := 0 to Items.Count - 1 do
                    begin
                      with Items.XmlItems[y] do
                      begin
                        if Name = 'Operation' then
                        begin
                          xOperation := TWsdlOperation.Create (Self);
                          xOperation.Wsdl := Self;
                          xOperation.WsdlService := xService;
                          xOperation.Name := Items.XmlValueByTag ['Name'];
                          xOperation.reqTagName := xOperation.Name;
                          xOperation.Alias := xOperation.reqTagName;
                          xOperation.rpyTagName := xOperation.Name;
                          xService.Operations.AddObject(xOperation.Name, xOperation);
                          for z := 0 to Items.Count - 1 do
                          begin
                            zXml := Items.XmlItems[z];
                            with zXml do
                            begin
                              if (Name = 'Req') then
                                _LoadSwiftMtMsg (Name, zXml, xOperation.reqXsd);
                              if (Name = 'Rpy') then
                                _LoadSwiftMtMsg (Name, zXml, xOperation.rpyXsd);
                              if (Name = 'Flt') then
                                _LoadSwiftMtMsg (Name, zXml, xOperation.FaultXsd);
                              if (Name = 'reqRecognition') then
                                xOperation.reqRecognition := _LoadCobolRecog (zXml);
                              if (Name = 'rpyRecognition') then
                                xOperation.rpyRecognition := _LoadCobolRecog (zXml);
                            end;
                          end;
                        end;
                      end;
                    end;
                  end; // ipmDTSwiftMT
                end; // case xService.DescriptionType
              end;
            end;
          end;
        end;
      end;
    end;

    for s := 0 to Services.Count - 1 do
    begin
      if (Services.Services[s].DescriptionType in [ipmDTXsd, ipmDTJson, ipmDTEmail, ipmDTSwiftMT]) then
      begin
        for o := 0 to Services.Services[s].Operations.Count - 1 do
        begin
          xOperation := Services.Services[s].Operations.Operations[o];
          FreeAndNil(xOperation.fReqBind);
          bindRefId := 0;
          xOperation.reqBind := TXml.Create (0, xOperation.reqXsd);
          xOperation.reqBind.Checked := True;
          if Services.Services[s].DescriptionType = ipmDTJson then
            (xOperation.reqBind as TXml).jsonType := jsonObject;
          FreeAndNil(xOperation.frpyBind);
          bindRefId := 0;
          xOperation.rpyBind := TXml.Create (0, xOperation.rpyXsd);
          xOperation.rpyBind.Checked := True;
          if Services.Services[s].DescriptionType = ipmDTJson then
            (xOperation.rpyBind as TXml).jsonType := jsonObject;
          FreeAndNil(xOperation.fltBind);
          bindRefId := 0;
          xOperation.fltBind := TXml.Create (0, xOperation.FaultXsd);
          xOperation.fltBind.Checked := True;
          if Services.Services[s].DescriptionType = ipmDTJson then
            (xOperation.fltBind as TXml).jsonType := jsonObject;
          xOperation.reqTagName := xOperation.Name;
          xOperation.Alias := xOperation.reqTagName;
          xOperation.rpyTagName := xOperation.Name;
        end;
      end;
    end;

  finally
    aXml.Free;
    xFileNames.Free;
  end;
end;

function TWsdl.ExtraXsdsAsXml (aSaveRelativeFileNames: Boolean): TXml;
var
  x: Integer;
begin
  result := TXml.CreateAsString ('FileNames', '');
  for x := 0 to ExtraXsds.Count - 1 do
    if aSaveRelativeFileNames then
      result.AddXml ( TXml.CreateAsString ( 'FileName'
                                          , ExtractRelativeFileName ( FileName
                                                                    , ExtraXsds.Strings[x]
                                                                    )
                                          )
                    ).Checked := True
    else
      result.AddXml ( TXml.CreateAsString ( 'FileName'
                                          , ExpandUNCFileNameUTF8(ExtraXsds.Strings[x]
                                                              ) { *Converted from ExpandUNCFileName* }
                                          )
                    ).Checked := True;
end;

procedure TWsdl.ExtraXsdsFromXml(aXml: TXml);
var
  x: Integer;
begin
  ExtraXsds.Clear;
  for x := 0 to aXml.Items.Count - 1 do
    if aXml.Items.XmlItems[x].Checked then
      ExtraXsds.Add (CheckAndPromptForExistingFile(aXml.FullIndexCaption, FileName, aXml.Items.XmlItems[x].Value));
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

procedure TWsdl.LoadExtraXsds;
var
  x, f: Integer;
begin
  for x := 0 to ExtraXsds.Count - 1 do
    if not XsdDescr.ReadFileNames.Find (ExtraXsds.Strings[x], f) then
      XsdDescr.AddXsdFromFile ( ExtraXsds.Strings[x], _OnParseErrorEvent);
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
  Operations := TWsdlOperations.Create;
  Operations.Sorted := True;
end;

destructor TWsdlService.Destroy;
begin
  Operations.Clear;
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
  xNonce := NonceAsString;
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

procedure TWsdlOperation.BindBeforeFunction(Id: String; Adr: Pointer; Token: Integer;
  ArgumentsPrototype: String);
begin
  fExpressBefore.BindFunction (Id, Adr, Token, ArgumentsPrototype);
end;

procedure TWsdlOperation.AcquireLock;
begin
  if doOperationLock then fLock.Acquire;
end;

function TWsdlOperation.BeforeActivatorDebugString: String;
begin
  result := fExpressBefore.DebugTokenStringList;
end;

procedure TWsdlOperation.BindAfterFunction(Id: String; Adr: Pointer; Token: Integer;
  ArgumentsPrototype: String);
begin
  fExpressAfter.BindFunction (Id, Adr, Token, ArgumentsPrototype);
end;

constructor TWsdlOperation.Create  (aWsdl: TWsdl);
begin
  WsdlOperation := self;
  fCloned := nil;
  fLock := SyncObjs.TCriticalSection.Create;
  doSuppressLog := 0;
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
    ExpectationBindables := TBindableList.Create;
    LogColumns := TBindableList.Create;
    BindablesWithAddedElement := TBindableList.Create;
    invokeList := TWsdlOperations.Create;
    invokeList.Sorted := True;
  end;
  Documentation := TstringList.Create;
  BeforeScriptLines := TStringList.Create;
  AfterScriptLines := TStringList.Create;
  StubAction := saStub;
  StubHttpAddress := '';
  httpVerb := 'POST';
  StubMqPutManager := '';
  StubMqPutQueue := '';
  StubMqGetManager := '';
  StubMqGetQueue := '';
  StubMqTimeOut := 30;
  StubStompPutHost := 'localhost';
  StubStompPutPort := 61613;
  StubStompPutClientId := '';
  StubStompReplyBodyPostFix := '';
  StubStompRequestBodyPostFix := '';
  StubStompTimeOut := 30;
  if Assigned (_WsdlWsaXsd) then
  begin
    reqWsaXml := TXml.Create(-1000, _WsdlWsaXsd);
    reqWsaXml.CheckDownline(False);
    rpyWsaXml := TXml.Create(-1000, _WsdlWsaXsd);
    rpyWsaXml.CheckDownline(False);
  end;
  if Assigned (_WsdlMqHeaderXsd) then
  begin
    StubMqHeaderXml := TXml.Create(-10000, _WsdlMqHeaderXsd);
    StubMqHeaderXml.CheckDownline(False);
  end;
  if Assigned (_WsdlStompHeaderXsd) then
  begin
    StubStompHeaderXml := TXml.Create(-10000, _WsdlStompHeaderXsd);
    StubStompHeaderXml.CheckDownline(False);
  end;
  if Assigned (_WsdlTacoConfigXsd) then
  begin
    TacoConfigXml := TXml.Create(-10000, _WsdlTacoConfigXsd);
    TacoConfigXml.CheckDownline(False);
  end;
  StubCustomHeaderXml := TXml.CreateAsString('customHeaders', '');
  doReadReplyFromFile := False;
  ReadReplyFromFileXml := TXml.CreateAsString('ReadReplyFromFile', '');
end;

destructor TWsdlOperation.Destroy;
  procedure _FreeRecog (sl: TStringList);
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
    FreeAndNil (StubMqHeaderXml);
    FreeAndNil (StubCustomHeaderXml);
    FreeAndNil (TacoConfigXml);
    FreeAndNil (ReadReplyFromFileXml);
    FreeAndNil (fLock);
  end;
  if True then
  begin
    if Assigned (invokeList) then
      invokeList.Clear;
    FreeAndNil (invokeList);
    FreeAndNil (fExpressBefore);
    FreeAndNil (fExpressAfter);
    FreeAndNil (fExpressStamper);
    FreeAndNil (fExpressChecker);
    FreeAndNil (CorrelationBindables);
    FreeAndNil (ExpectationBindables);
    FreeAndNil (LogColumns);
    FreeAndNil (BindablesWithAddedElement);
    FreeAndNil (freqBind);
    FreeAndNil (frpyBind);
    FreeAndNil (fltBind);
    FreeAndNil (reqWsaXml);
    FreeAndNil (rpyWsaXml);
  end;
  inherited;
end;

procedure TWsdlOperation.ExecuteBefore;
begin
  if not PreparedBefore then
    raise Exception.Create('Operation (Before)"' + Name + '" not prepared');
  InitExecute;
  fExpressBefore.Execute;
end;

procedure TWsdlOperation.ExecuteAfter;
begin
  if not PreparedAfter then
    raise Exception.Create('Operation (After)"' + Name + '" not prepared');
  InitExecute;
  fExpressAfter.Execute;
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
          if (not Messages.Messages[x].Disabled)
          and _Match (Messages.Messages[x].CorrelationBindables, CorrelationBindables) then
          begin
            result := Messages.Messages [x];
          end;
        end;
        Inc (x);
      end;
      if not Assigned (Result) then
        Result := xDefault;
      if Assigned (Result) then
      begin
        if not Result.Disabled then // optimized...
        begin
          result.Disabled := _WsdlDisableOnCorrelate;
          if result.Disabled
          and Assigned (_WsdlOnMessageChange) then
            _WsdlOnMessageChange (result);
        end;
      end;
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
  if Assigned (Wsdl) then
    aBind.Bind (aRoot, aExpress, Wsdl.XsdDescr.xsdElementsWhenRepeatable)
  else
    aBind.Bind (aRoot, aExpress, 1)
end;

procedure TWsdlOperation.PrepareBefore;
var
  x: Integer;
begin
  fPreparedBefore := False;
  try
    FreeAndNil(fExpressBefore);
    fExpressBefore := TExpress.Create (nil);
    fExpressBefore.Context := Self;
    fExpressBefore.ScriptText := BeforeScriptLines.Text;
    fExpressBefore.OnGetAbortPressed := fOnGetAbortPressed;
    fExpressBefore.OnGetDoExit := getDoExit;
    fExpressBefore.OnError := fOnError;
//      fExpress.OnError := ExpressError;
//      fExpress.OnHaveData := HaveData;
    fLineNumber := 0;
    fExpressBefore.Database := _WsdlDbsConnector;
    Bind ('Req', reqBind, fExpressBefore);
    Bind ('Rpy', rpyBind, fExpressBefore);
    if Assigned (invokeList) then
    begin
      for x := 0 to invokeList.Count - 1 do
      begin
        if Assigned (invokeList.Operations[x]) then
        begin
          Bind ('Req', invokeList.Operations[x].reqBind, fExpressBefore);
          Bind ('Rpy', invokeList.Operations[x].rpyBind, fExpressBefore);
        end;
      end;
    end;
    if fltBind is TIpmItem then
      fltBind.Bind ('Flt', fExpressBefore, Wsdl.XsdDescr.xsdElementsWhenRepeatable);
    if fltBind is TXml then
    begin
      for x := 0 to (fltBind as TXml).Items.Count - 1 do
      begin
        (fltBind as TXml).Items.XmlItems [x].Parent := nil;
        try
          (fltBind as TXml).Items.XmlItems [x].Bind('Faults', fExpressBefore, Wsdl.xsdElementsWhenRepeatable);
        finally
          (fltBind as TXml).Items.XmlItems [x].Parent := fltBind;
        end;
      end;
    end;
    if Assigned (reqWsaXml) then
      try reqWsaXml.Bind ('reqWsa', fExpressBefore, 1); except end;
    if Assigned (rpyWsaXml) then
      try rpyWsaXml.Bind ('rpyWsa', fExpressBefore, 1); except end;
    if Assigned (StubMqHeaderXml) then
      try StubMqHeaderXml.Bind ('Mq', fExpressBefore, 1); except end;
    try fExpressBefore.BindInteger('rti.operation.delayms', DelayTimeMs); except end;
    try fExpressBefore.BindInteger('rti.operation.suppresslog', doSuppressLog); except end;
//      BindFunction ('Log', @ServerLogMessage, VFS, '(aString)');
    BindBeforeFunction ('AccordingSchema', @isAccordingSchema, XFG, '(aItem)');
    BindBeforeFunction ('AddRemark', @AddRemark, VFOS, '(aString)');
    BindBeforeFunction ('Assigned', @isAssigned, XFG, '(aItem)');
    BindBeforeFunction ('AssignRecurring', @AssignRecurring, VFGGGG, '(aDestRecurringElm, aDestElm, aSrcRecurringElm, aSrcElm)');
    BindBeforeFunction ('CheckRecurringElement', @CheckRecurringElement, VFGGGG, '(aDestElm, aDestCorrElm, aSrcElm, aSrcCorrElm)');
    BindBeforeFunction ('DateTimeToJulianStr', @DateTimeToJulianStr, SFD, '(aDateTime)');
    BindBeforeFunction ('DateTimeToTandemJulianStr', @DateTimeToTandemJulianStr, SFD, '(aDateTime)');
    BindBeforeFunction ('dbLookUp', @dbLookUp, SFSSSS, '(aTable, aValueColumn, aReferenceColumn, aReferenceValue)');
    BindBeforeFunction ('DecEnvNumber', @decVarNumber, XFS, '(aKey)');
    BindBeforeFunction ('ExecuteScript', @ExecuteScript, VFOS, '(aScript)');
    BindBeforeFunction ('Exit', @RaiseExit, VFOV, '()');
    BindBeforeFunction ('FetchFirstMessage', @wsdlFetchFirstMessage, XFOS, '(aOperation)');
    BindBeforeFunction ('FetchNextMessage', @wsdlFetchNextMessage, XFOS, '(aOperation)');
    BindBeforeFunction ('FormatDate', @FormatDateX, SFDS, '(aDate, aMask)');
    BindBeforeFunction ('GetEnvNumber', @getVarNumber, XFS, '(aKey)');
    BindBeforeFunction ('GetEnvNumberDef', @getVarNumberDef, XFSX, '(aKey, aDefault)');
    BindBeforeFunction ('GetEnvVar', @getVar, SFS, '(aKey)');
    BindBeforeFunction ('GetEnvVarDef', @getVarDef, SFSS, '(aKey, aDefault)');
    BindBeforeFunction ('DisableMessage', @DisableMessage, VFOV, '()');
    BindBeforeFunction ('HostName', @GetHostName, SFV, '()');
    BindBeforeFunction ('ifthen', @ifThenString, SFBSS, '(aCondition, aTrueString, aFalseString)');
    BindBeforeFunction ('IncEnvNumber', @incVarNumber, XFS, '(aKey)');
    BindBeforeFunction ('LengthStr', @LengthX, XFS, '(aString)');
    BindBeforeFunction ('LowercaseStr', @lowercase, SFS, '(aString)');
    BindBeforeFunction ('MD5', @MD5, SFS, '(aString)');
    BindBeforeFunction ('MergeGroup', @mergeGroup, VFGG, '(aDestGroup, aSrcGroup)');
    BindBeforeFunction ('MessageName', @wsdlMessageName, SFOV, '()');
    BindBeforeFunction ('MessagingProtocol', @wsdlMessagingProtocol, SFOV, '()');
    BindBeforeFunction ('NumberToStr', @FloatToStr, SFX, '(aNumber)');
    BindBeforeFunction ('NowAsStr', @xsdNowAsDateTime, SFV, '()');
    BindBeforeFunction ('Occurrences', @OccurrencesX, XFG, '(aElement)');
    BindBeforeFunction ('PromptReply', @PromptReply, VFOV, '()');
    BindBeforeFunction ('PromptRequest', @PromptRequest, VFOV, '()');
    BindBeforeFunction ('RaiseError', @RaiseError, VFS, '(aString)');
    BindBeforeFunction ('RaiseSoapFault', @RaiseSoapFault, VFOSSSS, '(aFaultCode, aFaultString, aFaultActor, aDetail)');
    BindBeforeFunction ('RaiseWsdlFault', @RaiseWsdlFault, VFOSSS, '(aFaultCode, aFaultString, aFaultActor)');
    BindBeforeFunction ('Random', @RandomX, XFXX, '(aLow, aHigh)');
    BindBeforeFunction ('RefuseHttpConnections', @RefuseHttpConnections, XFOXX, '(aWait, aWhile)');
    BindBeforeFunction ('ResetOperationCounters', @ResetOperationCounters, VFV, '()');
    BindBeforeFunction ('ResetEnvVar', @ResetEnvVar, VFS, '(aKey)');
    BindBeforeFunction ('ResetEnvVars', @ResetEnvVars, VFS, '(aRegularExpr)');
    BindBeforeFunction ('ReturnString', @ReturnString, VFOS, '(aString)');
    BindBeforeFunction ('EnableAllMessages', @EnableAllMessages, VFV, '()');
    BindBeforeFunction ('EnableMessage', @EnableMessage, VFOV, '()');
    BindBeforeFunction ('OperationCount', @xsdOperationCount, XFOV, '()');
    BindBeforeFunction ('RequestOperation', @WsdlRequestOperation, VFOS, '(aOperation)');
    BindBeforeFunction ('Rounded', @RoundedX, XFXX, '(aNumber, aDecimals)');
    BindBeforeFunction ('SendOperationRequest', @WsdlSendOperationRequest, VFSS, '(aOperation, aCorrelation)');
    BindBeforeFunction ('SendOperationRequestLater', @WsdlSendOperationRequestLater, VFSSS, '(aOperation, aCorrelation, aLater)');
    BindBeforeFunction ('SetEnvNumber', @setEnvNumber, XFSX, '(aKey, aNumber)');
    BindBeforeFunction ('SetEnvVar', @setEnvVar, SFSS, '(aKey, aValue)');
    BindBeforeFunction ('SetLogGroupId', @SetLogGroupId, VFOS, '(aString)');
    BindBeforeFunction ('SHA1', @SHA1, SFS, '(aString)');
    BindBeforeFunction ('ShowMessage', @SjowMessage, VFS, '(aString)');
    BindBeforeFunction ('SiebelNowAsStr', @sblNowAsDateTime, SFV, '()');
    BindBeforeFunction ('SiebelTodayAsStr', @sblTodayAsDate, SFV, '()');
    BindBeforeFunction ('Sleep', @SleepX, VFX, '(aMilliSeconds)');
    BindBeforeFunction ('StrHasRegExpr', @StringHasRegExpr, SFSS, '(aString, aRegExpr)');
    BindBeforeFunction ('StrMatchesRegExpr', @StringMatchesRegExpr, SFSS, '(aString, aRegExpr)');
    BindBeforeFunction ('StrToNumber', @StrToFloatX, XFS, '(aString)');
    BindBeforeFunction ('SubStr', @SubStringX, SFSXX, '(aString, aStart, aLength)');
//    BindBeforeFunction ('Sum', @Sum, XFGG, '(aGroup, aElement)');
    BindBeforeFunction ('SwiftNumberToStr', @SwiftNumberToStr, SFX, '(aNumber)');
    BindBeforeFunction ('SwiftStrToNumber', @SwiftStrToNumber, XFS, '(aString)');
    BindBeforeFunction ('TodayAsStr', @xsdTodayAsDate, SFV, '()');
    BindBeforeFunction ('UppercaseStr', @uppercase, SFS, '(aString)');
    BindBeforeFunction ('UserName', @wsdlUserName, SFV, '()');
    BindBeforeFunction ('OperationName', @wsdlOperationName, SFOV, '()');
    fExpressBefore.Prepare;
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
    FreeAndNil(fExpressAfter);
    fExpressAfter := TExpress.Create (nil);
    fExpressAfter.Context := Self;
    fExpressAfter.ScriptText := AfterScriptLines.Text;
    fExpressAfter.OnError := fOnError;
    fExpressAfter.OnGetAbortPressed := fOnGetAbortPressed;
    fExpressAfter.OnGetDoExit := getDoExit;
//      fExpress.OnError := ExpressError;
//      fExpress.OnHaveData := HaveData;
    fLineNumber := 0;
    fExpressAfter.Database := _WsdlDbsConnector;
    Bind ('Req', reqBind, fExpressAfter);
    Bind ('Rpy', rpyBind, fExpressAfter);
    for x := 0 to invokeList.Count - 1 do
    begin
      if Assigned (invokeList.Operations[x]) then with invokeList.Operations[x] do
      begin
        Bind ('Req', reqBind, fExpressAfter);
        Bind ('Rpy', rpyBind, fExpressAfter);
      end;
    end;
    if fltBind is TIpmItem then
      fltBind.Bind ('Flt', fExpressAfter, Wsdl.XsdDescr.xsdElementsWhenRepeatable);
    if fltBind is TXml then
    begin
      for x := 0 to (fltBind as TXml).Items.Count - 1 do
      begin
        (fltBind as TXml).Items.XmlItems [x].Parent := nil;
        try
          (fltBind as TXml).Items.XmlItems [x].Bind('Faults', fExpressAfter, Wsdl.xsdElementsWhenRepeatable);
        finally
          (fltBind as TXml).Items.XmlItems [x].Parent := fltBind;
        end;
      end;
    end;
    if Assigned (reqWsaXml) then
      try reqWsaXml.Bind ('reqWsa', fExpressAfter, 1); except end;
    if Assigned (rpyWsaXml) then
      try rpyWsaXml.Bind ('rpyWsa', fExpressAfter, 1); except end;
    if Assigned (StubMqHeaderXml) then
      try StubMqHeaderXml.Bind ('Mq', fExpressAfter, 1); except end;
    try fExpressAfter.BindInteger('rti.operation.delayms', DelayTimeMs); except end;
    try fExpressAfter.BindInteger('rti.operation.suppresslog', doSuppressLog); except end;
//      ExpectedXml.Bind ('Exp', fExpressBefore);
//      BindFunction ('Log', @ServerLogMessage, VFS, '(aString)');
    BindAfterFunction ('AccordingSchema', @isAccordingSchema, XFG, '(aItem)');
    BindAfterFunction ('AddRemark', @AddRemark, VFOS, '(aString)');
    BindAfterFunction ('Assigned', @isAssigned, XFG, '(aItem)');
    BindAfterFunction ('AssignRecurring', @AssignRecurring, VFGGGG, '(aDestRecurringElm, aDestElm, aSrcRecurringElm, aSrcElm)');
    BindAfterFunction ('CheckRecurringElement', @CheckRecurringElement, VFGGGG, '(aDestElm, aDestCorrElm, aSrcElm, aSrcCorrElm)');
    BindAfterFunction ('DateTimeToJulianStr', @DateTimeToJulianStr, SFD, '(aDateTime)');
    BindAfterFunction ('DateTimeToTandemJulianStr', @DateTimeToTandemJulianStr, SFD, '(aDateTime)');
    BindAfterFunction ('dbLookUp', @dbLookUp, SFSSSS, '(aTable, aValueColumn, aReferenceColumn, aReferenceValue)');
    BindAfterFunction ('DecEnvNumber', @decVarNumber, XFS, '(aKey)');
    BindAfterFunction ('ExecuteScript', @ExecuteScript, VFOS, '(aScript)');
    BindAfterFunction ('Exit', @RaiseExit, VFOV, '()');
    BindAfterFunction ('FetchFirstMessage', @wsdlFetchFirstMessage, XFOS, '(aOperation)');
    BindAfterFunction ('FetchNextMessage', @wsdlFetchNextMessage, XFOS, '(aOperation)');
    BindAfterFunction ('FormatDate', @FormatDateX, SFDS, '(aDate, aMask)');
    BindAfterFunction ('GetEnvNumber', @getVarNumber, XFS, '(aKey)');
    BindAfterFunction ('GetEnvNumberDef', @getVarNumberDef, XFSX, '(aKey, aDefault)');
    BindAfterFunction ('GetEnvVar', @getVar, SFS, '(aKey)');
    BindAfterFunction ('GetEnvVarDef', @getVarDef, SFSS, '(aKey, aDefault)');
    BindAfterFunction ('DisableMessage', @DisableMessage, VFOV, '()');
    BindAfterFunction ('HostName', @GetHostName, SFV, '()');
    BindAfterFunction ('ifthen', @ifThenString, SFBSS, '(aCondition, aTrueString, aFalseString)');
    BindAfterFunction ('IncEnvNumber', @incVarNumber, XFS, '(aKey)');
    BindAfterFunction ('LengthStr', @LengthX, XFS, '(aString)');
    BindAfterFunction ('LowercaseStr', @lowercase, SFS, '(aString)');
    BindAfterFunction ('MD5', @MD5, SFS, '(aString)');
    BindAfterFunction ('MergeGroup', @mergeGroup, VFGG, '(aDestGroup, aSrcGroup)');
    BindAfterFunction ('MessageName', @wsdlMessageName, SFOV, '()');
    BindAfterFunction ('MessagingProtocol', @wsdlMessagingProtocol, SFOV, '()');
    BindAfterFunction ('NumberToStr', @FloatToStr, SFX, '(aNumber)');
    BindAfterFunction ('NowAsStr', @xsdNowAsDateTime, SFV, '()');
    BindAfterFunction ('Occurrences', @OccurrencesX, XFG, '(aElement)');
    BindAfterFunction ('PromptReply', @PromptReply, VFOV, '()');
    BindAfterFunction ('PromptRequest', @PromptRequest, VFOV, '()');
    BindAfterFunction ('RaiseError', @RaiseError, VFS, '(aString)');
    BindAfterFunction ('RaiseSoapFault', @RaiseSoapFault, VFOSSSS, '(aFaultCode, aFaultString, aFaultActor, aDetail)');
    BindAfterFunction ('RaiseWsdlFault', @RaiseWsdlFault, VFOSSS, '(aFaultCode, aFaultString, aFaultActor)');
    BindAfterFunction ('Random', @RandomX, XFXX, '(aLow, aHigh)');
    BindAfterFunction ('RefuseHttpConnections', @RefuseHttpConnections, XFOXX, '(aWait, aWhile)');
    BindAfterFunction ('ResetOperationCounters', @ResetOperationCounters, VFV, '()');
    BindAfterFunction ('ResetEnvVar', @ResetEnvVar, VFS, '(aKey)');
    BindAfterFunction ('ResetEnvVars', @ResetEnvVars, VFS, '(aRegularExpr)');
    BindAfterFunction ('ReturnString', @ReturnString, VFOS, '(aString)');
    BindAfterFunction ('EnableAllMessages', @EnableAllMessages, VFV, '()');
    BindAfterFunction ('EnableMessage', @EnableMessage, VFOV, '()');
    BindAfterFunction ('RequestOperation', @WsdlRequestOperation, VFOS, '(aOperation)');
    BindAfterFunction ('Rounded', @RoundedX, XFXX, '(aNumber, aDecimals)');
    BindAfterFunction ('SendOperationRequest', @WsdlSendOperationRequest, VFSS, '(aOperation, aCorrelation)');
    BindAfterFunction ('SendOperationRequestLater', @WsdlSendOperationRequestLater, VFSSS, '(aOperation, aCorrelation, aLater)');
    BindAfterFunction ('SetEnvNumber', @setEnvNumber, XFSX, '(aKey, aNumber)');
    BindAfterFunction ('SetEnvVar', @setEnvVar, SFSS, '(aKey, aValue)');
    BindAfterFunction ('SetLogGroupId', @SetLogGroupId, VFOS, '(aString)');
    BindAfterFunction ('SHA1', @SHA1, SFS, '(aString)');
    BindAfterFunction ('ShowMessage', @SjowMessage, VFS, '(aString)');
    BindAfterFunction ('SiebelNowAsStr', @sblNowAsDateTime, SFV, '()');
    BindAfterFunction ('SiebelTodayAsStr', @sblTodayAsDate, SFV, '()');
    BindAfterFunction ('Sleep', @Sleep, VFX, '(aMilliSeconds)');
    BindAfterFunction ('StrHasRegExpr', @StringHasRegExpr, SFSS, '(aString, aRegExpr)');
    BindAfterFunction ('StrMatchesRegExpr', @StringMatchesRegExpr, SFSS, '(aString, aRegExpr)');
    BindAfterFunction ('StrToNumber', @StrToFloatX, XFS, '(aString)');
    BindAfterFunction ('SubStr', @SubStringX, SFSXX, '(aString, aStart, aLength)');
//  BindAfterFunction ('Sum', @Sum, XFGG, '(aGroup, aElement)');
    BindAfterFunction ('SwiftNumberToStr', @SwiftNumberToStr, SFX, '(aNumber)');
    BindAfterFunction ('SwiftStrToNumber', @SwiftStrToNumber, XFS, '(aString)');
    BindAfterFunction ('TodayAsStr', @xsdTodayAsDate, SFV, '()');
    BindAfterFunction ('UppercaseStr', @uppercase, SFS, '(aString)');
    BindAfterFunction ('OperationCount', @xsdOperationCount, XFOV, '()');
    BindAfterFunction ('UserName', @wsdlUserName, SFV, '()');
    BindAfterFunction ('OperationName', @wsdlOperationName, SFOV, '()');
    fExpressAfter.Prepare;
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
begin
  result := '';
  if WsdlService.DescriptionType in [ipmDTSwiftMT] then
  begin
    with TStwiftMtStreamer.Create(reqBind as TXml) do
    try
      result := AsText;
    finally
      Free;
    end;
    Exit;
  end;
  if lateBinding then
  begin
    result := FreeformatReq;
    if Trim (BeforeScriptLines.Text) <> '' then
      result := (reqBind as TXml).asString;
    Exit;
  end;
  try
    if isSoapService then
    begin
      result := GenerateXmlHeader(not WsdlService.SuppressXmlComment);
      result := StrAdd (result, '<soapenv:Envelope');
      result := StrAdd (result, ' xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"');
      result := StrAdd (result, ' xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/"');
      result := StrAdd (result, ' xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"');
      result := StrAdd (result, ' xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"');
      result := StrAdd (result, ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"');
      result := StrAdd (result, ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"');
      result := StrAdd (result, ' >');
      if (InputHeaders.Count > 0)
      or (WsdlService.AuthenticationType = atWsSecurity)
      or wsaEnabled then
      begin
        if wsaEnabled then
          result := StrAdd (result, '  <soapenv:Header xmlns:wsa="http://www.w3.org/2005/08/addressing">')
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
    end
    else
    begin
      if reqBind is TXml then
      begin
        aXml := (reqBind as TXml).Items.XmlItems[0];
        xsiGenerated := False;
        xsdGenerated := False;
        if WsdlService.DescriptionType <> ipmDTJson then
          result := result + aXml.StreamXML(aGenerateBodyNameSpaces, WsdlService.UseNameSpacePrefixes, 0, True, False)
        else
          result := result + aXml.StreamJSON(0, True);
      end;
      if reqBind is TIpmItem then
        result := (reqBind as TIpmItem).ValuesToBuffer (nil);
    end;
  finally
  end;
end;

function TWsdlOperation.StreamReply(aGeneratedWith: String; aGenerateTypes: Boolean): String;
var
  x: Integer;
  xXml: TXml;
begin
  result := LiteralResult;
  if Result <> '' then
    Exit;
  if lateBinding then
  begin
    result := FreeformatRpy;
    Exit;
  end;
  if WsdlService.DescriptionType in [ipmDTSwiftMT] then
  begin
    with TStwiftMtStreamer.Create(rpyBind as TXml) do
    try
      result := AsText;
    finally
      Free;
    end;
    Exit;
  end;
  if (    isSoapService
      and (rpyXsd.sType.ElementDefs.Count = 0)
     )
  or (    (rpyXsd.sType.ElementDefs.Count = 0)
      and (rpyBind is TXml)
     ) then // one way
    exit;
  try
    if isSoapService then
    begin
      result := GenerateXmlHeader(not WsdlService.SuppressXmlComment);
      result := StrAdd (result, '<soapenv:Envelope');
      result := StrAdd (result, ' xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"');
      result := StrAdd (result, ' xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/"');
      result := StrAdd (result, ' xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"');
      result := StrAdd (result, ' xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"');
      result := StrAdd (result, ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"');
      result := StrAdd (result, ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"');
      result := StrAdd (result, ' >');
      if (OutputHeaders.Count > 0)
      or wsaEnabled then
      begin
        if wsaEnabled then
          result := StrAdd (result, '  <soapenv:Header xmlns:wsa="http://www.w3.org/2005/08/addressing">')
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
    end
    else
    begin
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
      end;
      if rpyBind is TIpmItem then
      begin
        result := rpyIpm.ValuesToBuffer (nil);
      end;
    end;
  finally
  end;
end;

function TWsdlOperation.StamperFunctionPrototypes: TStringList;
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
    result := StrAdd (result, ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"');
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

function TWsdlOperation.getLastMessage: TWsdlMessage;
begin
  Result := fLastMessage;
  if Assigned (fLastMessage) then
    if Messages.IndexOfObject(fLastMessage) < 0 then
      result := nil;
end;

function TWsdlOperation.getLastFullCaption: String;
begin
  Result := fLastFullCaption;
end;

function TWsdlOperation.CheckerFunctionPrototypes: TStringList;
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
      result := result + xSep + CorrelationBindables.Bindables[x].GetStringData
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
  self.reqTagNameSpace := xOperation.reqTagNameSpace;
  self.rpyMessageName := xOperation.rpyMessageName;
  self.rpyTagName := xOperation.rpyTagName;
  self.rpyTagNameSpace := xOperation.rpyTagNameSpace;
  self.Documentation := xOperation.Documentation;
  Self.FaultMessages := xOperation.FaultMessages;
  self.InputHeaders := xOperation.InputHeaders;
  self.OutputHeaders := xOperation.OutputHeaders;
  self.BindName := xOperation.BindName;
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
  self.AsynchronousDialog := xOperation.AsynchronousDialog;
  if Assigned (_WsdlWsaXsd) then
  begin
    reqWsaXml := TXml.Create(-1000, _WsdlWsaXsd);
    reqWsaXml.CheckDownline(False);
    rpyWsaXml := TXml.Create(-1000, _WsdlWsaXsd);
    rpyWsaXml.CheckDownline(False);
  end;
  self.StubMqHeaderXml := xOperation.StubMqHeaderXml;
  self.StubStompHeaderXml := xOperation.StubStompHeaderXml;
  self.StubCustomHeaderXml := xOperation.StubCustomHeaderXml;
  self.TacoConfigXml := xOperation.TacoConfigXml;
  self.doReadReplyFromFile := xOperation.doReadReplyFromFile;
  self.ReadReplyFromFileXml := xOperation.ReadReplyFromFileXml;
  self.StubAction := xOperation.StubAction;
  self.StubTransport := xOperation.StubTransport;
  self.StubHttpAddress := xOperation.StubHttpAddress;
  self.ContentEncoding := xOperation.ContentEncoding;
  self.AcceptDeflateEncoding := xOperation.AcceptDeflateEncoding;
  self.AcceptGzipEncoding := xOperation.AcceptGzipEncoding;
  self.httpVerb := xOperation.httpVerb;
  self.StubMqPutManager := xOperation.StubMqPutManager;
  self.StubMqPutQueue := xOperation.StubMqPutQueue;
  self.StubMqGetManager := xOperation.StubMqGetManager;
  self.StubMqGetQueue := xOperation.StubMqGetQueue;
  self.StubMqTimeOut := xOperation.StubMqTimeOut;
  self.StubStompPutHost := xOperation.StubStompPutHost;
  self.StubStompPutPort := xOperation.StubStompPutPort;
  self.StubStompPutClientId := xOperation.StubStompPutClientId;
  self.StubStompReplyBodyPostFix := xOperation.StubStompReplyBodyPostFix;
  self.StubStompRequestBodyPostFix := xOperation.StubStompRequestBodyPostFix;
  self.StubStompTimeOut := xOperation.StubStompTimeOut;
  self.BeforeScriptLines := xOperation.BeforeScriptLines;
  self.AfterScriptLines := xOperation.AfterScriptLines;
  self.CorrelatedMessage := xOperation.CorrelatedMessage;
  self.Messages := xOperation.Messages;
//  self.faultcode, faultstring, faultactor := xOperation.String;
  self.RecognitionType := xOperation.RecognitionType;
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
    if Assigned (self.reqXsd) then
    begin
      self.reqBind := TXml.Create (-10000, self.reqXsd);
      self.reqBind.Name := xOperation.reqBind.Name;
    end;
  end;
  self.rpyXsd := xOperation.rpyXsd;
  if xOperation.rpyBind is TIpmItem then
    self.rpyBind := TIpmItem.Create(xOperation.rpyBind as TIpmItem)
  else
  begin
    if Assigned (self.rpyXsd) then
    begin
      self.rpyBind := TXml.Create (-10000, self.rpyXsd);
      self.rpyBind.Name := xOperation.rpyBind.Name;
    end;
  end;
  self.OnError := xOperation.OnError;
  if Assigned (xOperation.CorrelationBindables) then
    self.CorrelationBindables := _cloneBindables(xOperation.CorrelationBindables);
  if Assigned (xOperation.ExpectationBindables) then
    self.ExpectationBindables := _cloneBindables(xOperation.ExpectationBindables);
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
    if not self.lateBinding then
    begin
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
  BeforeScriptLines := TStringList.Create;
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


procedure TWsdlOperation.SoapXmlReplyToBindables (aReply: TXml; aAddUnknowns: Boolean);
var
  x, s, d: Integer;
  xXml: TXml;
begin
  (rpyBind as TXml).ResetValues;
  (rpyBind as TXml).Checked := True;
  aReply.SeparateNsPrefixes;
  aReply.ResolveNameSpaces;
  if aReply.Name = '' then Exit;
  if aReply.isSoapEnvelope then
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
            (rpyBind as TXml).Items.XmlItems[d].LoadValues (xXml.Items.XmlItems [s], aAddUnknowns, False);
        end;
      end;
      if (xXml.TagName = 'Body') then
      begin
        for s := 0 to xXml.Items.Count - 1 do
        begin
          for d := OutputHeaders.Count to (rpyBind as TXml).Items.Count - 1 do
            (rpyBind as TXml).Items.XmlItems[d].LoadValues (xXml.Items.XmlItems [s], aAddUnknowns, False);
        end;
      end;
    end;
  end
  else
  begin
    (rpyBind as TXml).Items.XmlItems[0].LoadValues (aReply, aAddUnknowns, False);
  end;
end;

procedure TWsdlOperation.SoapXmlRequestToBindables (aRequest: TXml; aAddUnknowns: Boolean);
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
            (reqBind as TXml).Items.XmlItems[d].LoadValues (xXml.Items.XmlItems [s], aAddUnknowns, False);
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
            (reqBind as TXml).Items.XmlItems[d].LoadValues (xXml.Items.XmlItems [s], aAddUnknowns, False);
        end;
      end;
    end;
  end
  else
  begin
    (reqBind as TXml).Items.XmlItems[0].LoadValues (aRequest, aAddUnknowns, False);
  end;
end;

procedure TWsdlOperation .FreeFormatToBindables (aRequestXml : TXml ;
  aRequestString : String );
begin
  FreeFormatReq := aRequestString;
end;

function TWsdlOperation.getDebugTokenStringAfter: String;
begin
  result := fExpressAfter.DebugTokenStringList;
end;

function TWsdlOperation.getDebugTokenStringBefore: String;
begin
  result := fExpressBefore.DebugTokenStringList;
end;

function TWsdlOperation.AddedTypeDefElementsAsXml : TObject ;
var
  x, y: integer;
  nTypeDef: TXsdDataType;
  XmlResult: TXml;
  sXml: TXml;
begin
  XmlResult := TXml.CreateAsString('AddedTypeDefElements', '');
  result := XmlResult;
  for x := 0 to BindablesWithAddedElement.Count - 1 do
  begin
    nTypeDef := (BindablesWithAddedElement.Bindables[x] as TXml).TypeDef;
    sXml := XmlResult.AddXml(TXml.CreateAsString('AddedTypeDefElement', ''));
    with sXml do
    begin
      AddXml (TXml.CreateAsString('UsedAt', BindablesWithAddedElement.Strings[x]));
      for y := 0 to nTypeDef.ElementDefs.Count - 1 do
      begin
        with AddXml(TXml.CreateAsString('Added', '')) do
        begin
          AddXml(TXml.CreateAsString('NameSpace',
              nTypeDef.ElementDefs.Xsds[y].sType.NameSpace));
          AddXml(TXml.CreateAsString('Name',
              nTypeDef.ElementDefs.Xsds[y].sType.Name));
          AddXml(TXml.CreateAsString('ElementName',
              nTypeDef.ElementDefs.Xsds[y].ElementName));
        end;
      end;
    end;
  end;
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
  xxsd: TXsd;
  xPath, xNameSpace, xName, xElementName: String;
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
            xNameSpace := Items.XmlValueByTag['NameSpace'];
            xName := Items.XmlValueByTag['Name'];
            xElementName := Items.XmlValueByTag['ElementName'];
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
              xxsd := xBind.Xsd.AddElementDef ( Wsdl.XsdDescr
                                              , xElementName
                                              , cTypeDef
                                              );
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
  result := fExpressBefore.BindsAsText;
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
  if Assigned (fExpressBefore) then fExpressBefore.OnGetAbortPressed := Value;
  if Assigned (fExpressAfter) then fExpressAfter.OnGetAbortPressed := Value;
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
    ttMq:
      result := 'queue//getWsaTo_not_yet_implemented';
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
  _refresh(self, ExpectationBindables);
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
      SoapXmlReplyToBindables(xXml, Assigned(Cloned));
    finally
      FreeAndNil (xXml);
    end;
  end;
  if reqBind is TIpmItem then
    (reqBind as TIpmItem).BufferToValues (nil, aString);
end;

procedure TWsdlOperation.ReqBindablesFromWsdlMessage(aMessage: TWsdlMessage);
begin
  if lateBinding then
  begin
    FreeFormatReq := aMessage.FreeFormatReq;
    with reqBind as TXml do
    begin
      LoadFromString(FreeFormatReq, nil);
      if Name = '' then
        Name := 'noXml';
      try PrepareBefore; except end;
    end;
  end
  else
  begin
    if reqBind is TXml then
    begin
      (reqBind as TXml).ResetValues;
      (reqBind as TXml).LoadValues ((aMessage.reqBind as TXml), False, True);
    end;
    if reqBind is TIpmItem then
    begin
      (reqBind as TIpmItem).LoadValues (aMessage.reqBind as TIpmItem);
    end;
  end;
end;

procedure TWsdlOperation.ReqBindablesToWsdlMessage(aMessage: TWsdlMessage);
begin
  if WsdlService.DescriptionType in [ipmDTFreeFormat] then
    aMessage.FreeFormatReq := FreeFormatReq
  else
  begin
    if reqBind is TXml then
    begin
      (aMessage.reqBind as TXml).ResetValues;
      (aMessage.reqBind as TXml).LoadValues ((reqBind as TXml), False, True);
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
      SoapXmlReplyToBindables(xXml, Assigned(Cloned));
    finally
      FreeAndNil (xXml);
    end;
  end;
  if rpyBind is TIpmItem then
    (rpyBind as TIpmItem).BufferToValues (nil, aString);
end;

procedure TWsdlOperation.RpyBindablesFromWsdlMessage(aMessage: TWsdlMessage);
begin
  if WsdlService.DescriptionType in [ipmDTFreeFormat] then
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
  if WsdlService.DescriptionType in [ipmDTFreeFormat] then
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
        xAttr.Value := 'http://www.w3.org/2005/08/addressing/reply';
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

function TWsdlOperation.FunctionPrototypes(aAfter: Boolean): TStringList;
begin
  if aAfter then
    result := fExpressAfter.FunctionProtoTypes
  else
    result := fExpressBefore.FunctionProtoTypes;
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
        xAttr.Value := 'http://www.w3.org/2005/08/addressing/reply';
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
      finally
        allAliasses.Operations[f]._processing := False;
      end;
    end;
  end;
end;

procedure TWsdlOperation.doPromptReply;
begin
//SetForegroundWindow(Application.Handle);
  xmlUtil.ViewAsXml(rpyBind, False);
end;

procedure TWsdlOperation.doPromptRequest;
begin
//SetForegroundWindow(Application.Handle);
  xmlUtil.ViewAsXml(reqBind, False);
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
        if useSsl then
        begin
          TagName := 'Https';
          with AddXml(TXml.CreateAsString('SSL', '')) do
          begin
            AddXml(TXml.CreateAsString('Version', sslVersionToString(sslVersion)));
            AddXml(TXml.CreateAsString('CertificateFile', sslCertificateFile));
            AddXml(TXml.CreateAsString('KeyFile', sslKeyFile));
            AddXml(TXml.CreateAsString('RootCertificateFile', sslRootCertificateFile));
          end;
        end;
      end;
    ttMq:
      with result.AddXml(TXml.CreateAsString('Mq', '')) do
      begin
        with AddXml(TXml.CreateAsString('putRequest', '')) do
        begin
          AddXml (TXml.CreateAsString('Manager', StubMqPutManager));
          AddXml (TXml.CreateAsString('Queue', StubMqPutQueue));
        end;
        if (StubMqGetManager <> '')
        or (StubMqGetQueue <> '') then
        begin
          with AddXml(TXml.CreateAsString('getRequest', '')) do
          begin
            AddXml (TXml.CreateAsString('Manager', StubMqGetManager));
            AddXml (TXml.CreateAsString('Queue', StubMqGetQueue));
            AddXml (TXml.CreateAsInteger('Timeout', StubMqTimeOut));
          end;
        end;
        if Assigned(StubMqHeaderXml)
        and (StubMqHeaderXml.Checked) then
        begin
          with AddXml (TXml.CreateAsString ('mqHeader', '')) do
          begin
            CopyDownLine(StubMqHeaderXml, True);
          end;
        end;
      end;
    ttStomp:
      with result.AddXml(TXml.CreateAsString('Stomp', '')) do
      begin
        AddXml (TXml.CreateAsString('Host', StubStompPutHost));
        AddXml (TXml.CreateAsInteger('Port', StubStompPutPort));
        AddXml (TXml.CreateAsString('ClientId', StubStompPutClientId));
        if StubStompReplyBodyPostFix <> '' then
          AddXml (TXml.CreateAsString('ReplyBodyPostFix', StubStompReplyBodyPostFix));
        if StubStompRequestBodyPostFix <> '' then
          AddXml (TXml.CreateAsString('RequestBodyPostFix', StubStompRequestBodyPostFix));
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
    ttTaco:
      with result.AddXml(TXml.CreateAsString('Taco', '')) do
        CopyDownLine(TacoConfigXml, True);
    ttSmtp:
      with result.AddXml(TXml.CreateAsString('Smtp', '')) do
      begin
        AddXml (TXml.CreateAsString('Host', smtpHost));
        AddXml (TXml.CreateAsInteger('Port', smtpPort));
      end;
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
  then raise Exception.Create('endpointConfigfromXml: invalid XML' + aXml.Text);
  StubTransport := ttHttp;
  StubHttpAddress := '';
  httpVerb := 'POST';
  ContentEncoding := 'identity';
  AcceptDeflateEncoding := True;
  AcceptGzipEncoding := True;
  useSsl := False;
  sslVersion := sslvSSLv3;
  sslCertificateFile := '';
  sslKeyFile := '';
  sslRootCertificateFile := '';
  StubMqPutManager := '';
  StubMqPutQueue := '';
  StubMqGetManager := '';
  StubMqGetQueue := '';
  StubMqTimeOut := 0;
  StubMqHeaderXml.CheckDownline(False);
  StubStompPutHost := '';
  StubStompPutPort := 0;
  StubStompPutClientId := '';
  StubStompReplyBodyPostFix := '';
  StubStompRequestBodyPostFix := '';
  StubStompTimeOut := 0;
  StubStompHeaderXml.CheckDownline(False);
  StubCustomHeaderXml.Items.Clear;
  smtpHost := '';
  smtpPort := 0;
  TacoConfigXml.CheckDownline(False);
  for x := 0 to aXml.Items.Count - 1 do
  begin
    with aXml.Items.XmlItems[x] do
    begin
      if Checked then
      begin
        if Name = 'Http' then
        begin
          StubTransport := ttHttp;
          StubHttpAddress := Items.XmlCheckedValueByTag['Address'];
          httpVerb := UpperCase(Items.XmlCheckedValueByTagDef['Verb', httpVerb]);
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
        end;
        if Name = 'Https' then
        begin
          StubTransport := ttHttp;
          StubHttpAddress := Items.XmlCheckedValueByTag['Address'];
          httpVerb := Items.XmlCheckedValueByTagDef['Verb', httpVerb];
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
          useSsl := True;
          xXml := Items.XmlCheckedItemByTag['SSL'];
          if Assigned (xXml) then with xXml do
          begin
            yXml := Items.XmlCheckedItemByTag['Version'];
            if Assigned (yXml) then
              sslVersion := sslVersionFromString(yXml.Value);
            sslCertificateFile := Items.XmlCheckedValueByTag['CertificateFile'];
            sslKeyFile := Items.XmlCheckedValueByTag['KeyFile'];
            sslRootCertificateFile := Items.XmlCheckedValueByTag['RootCertificateFile'];
          end;
        end;
        if Name = 'Mq' then
        begin
          StubTransport := ttMq;
          xXml := Items.XmlCheckedItemByTag['putRequest'];
          if Assigned (xXml) then
          begin
            StubMqPutManager := xXml.Items.XmlCheckedValueByTag['Manager'];
            StubMqPutQueue := xXml.Items.XmlCheckedValueByTag['Queue'];
          end;
          xXml := Items.XmlCheckedItemByTag['getRequest'];
          if Assigned (xXml) then
          begin
            StubMqGetManager := xXml.Items.XmlCheckedValueByTag['Manager'];
            StubMqGetQueue := xXml.Items.XmlCheckedValueByTag['Queue'];
            StubMqTimeOut := xXml.Items.XmlCheckedIntegerByTag['Timeout'];
          end;
          xXml := Items.XmlCheckedItemByTag['mqHeader'];
          if Assigned (xXml) then
            StubMqHeaderXml.LoadValues (xXml, False, True);
        end;
        if Name = 'Stomp' then
        begin
          StubTransport := ttStomp;
          StubStompPutHost := Items.XmlCheckedValueByTagDef['Host', 'localhost'];
          StubStompPutPort := Items.XmlCheckedIntegerByTagDef['Port', 61613];
          StubStompPutClientId := Items.XmlCheckedValueByTag['ClientId'];
          StubStompRequestBodyPostFix := Items.XmlCheckedValueByTag['RequestBodyPostFix'];
          StubStompReplyBodyPostFix := Items.XmlCheckedValueByTag['ReplyBodyPostFix'];
          StubStompTimeOut := Items.XmlCheckedIntegerByTag['Timeout'];
          xXml := Items.XmlCheckedItemByTag['stompHeader'];
          if Assigned (xXml) then
            StubStompHeaderXml.LoadValues (xXml, False, True);
          xXml := Items.XmlCheckedItemByTag['customHeaders'];
          if Assigned (xXml) then
            StubCustomHeaderXml.LoadValues (xXml, True, True);
        end;
        if Name = 'Smtp' then
        begin
          StubTransport := ttSmtp;
          smtpHost := Items.XmlCheckedValueByTagDef['Host', 'localhost'];
          smtpPort := Items.XmlCheckedIntegerByTagDef['Port', 25];
        end;
        if Name = 'Taco' then
        begin
          StubTransport := ttTaco;
          TacoConfigXml.LoadValues (aXml.Items.XmlItems[x], False, True);
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
      fExpressChecker := TExpress.Create (nil);
      fExpressChecker.Context := Self;
      fExpressChecker.OnNeedData := NeedStamperData;
      fExpressChecker.OnError := fOnError;
      fLineNumber := 0;
      fExpressChecker.Database := _WsdlDbsConnector;
      aBind.Bind ('', fExpressChecker, Wsdl.XsdDescr.xsdElementsWhenRepeatable);
      fExpressChecker.BindBoolean('Bind_.Checker', aBind.fChecked);
      BindCheckerFunction ('dbLookUp', @dbLookUp, SFSSSS, '(aTable, aValueColumn, aReferenceColumn, aReferenceValue)');
      BindCheckerFunction ('DecEnvNumber', @decVarNumber, XFS, '(aKey)');
      BindCheckerFunction ('FormatDate', @FormatDateX, SFDS, '(aDate, aMask)');
      BindCheckerFunction ('GetEnvNumber', @getVarNumber, XFS, '(aKey)');
      BindCheckerFunction ('GetEnvNumberDef', @getVarNumberDef, XFSX, '(aKey, aDefault)');
      BindCheckerFunction ('GetEnvVar', @getVar, SFS, '(aKey)');
      BindCheckerFunction ('GetEnvVarDef', @getVarDef, SFSS, '(aKey, aDefault)');
      BindCheckerFunction ('HostName', @GetHostName, SFV, '()');
      BindCheckerFunction ('ifthen', @ifThenString, SFBSS, '(aCondition, aTrueString, aFalseString)');
      BindCheckerFunction ('IncEnvNumber', @incVarNumber, XFS, '(aKey)');
      BindCheckerFunction ('LengthStr', @LengthX, XFS, '(aString)');
      BindCheckerFunction ('LowercaseStr', @lowercase, SFS, '(aString)');
      BindCheckerFunction ('NumberToStr', @FloatToStr, SFX, '(aNumber)');
      BindCheckerFunction ('Occurrences', @OccurrencesX, XFG, '(aElement)');
      BindCheckerFunction ('Random', @RandomX, XFXX, '(aLow, aHigh)');
      BindCheckerFunction ('Rounded', @RoundedX, XFXX, '(aNumber, aDecimals)');
      BindCheckerFunction ('SetEnvNumber', @setEnvNumber, XFSX, '(aKey, aNumber)');
      BindCheckerFunction ('SetEnvVar', @setEnvVar, SFSS, '(aKey, aValue)');
      BindCheckerFunction ('StrHasRegExpr', @StringHasRegExpr, SFSS, '(aString, aRegExpr)');
      BindCheckerFunction ('StrMatchesRegExpr', @StringMatchesRegExpr, SFSS, '(aString, aRegExpr)');
      BindCheckerFunction ('StrToNumber', @StrToFloatX, XFS, '(aString)');
      BindCheckerFunction ('SubStr', @SubStringX, SFSXX, '(aString, aStart, aLength)');
//    BindCheckerFunction ('Sum', @Sum, XFGG, '(aGroup, aElement)');
      BindCheckerFunction ('UppercaseStr', @uppercase, SFS, '(aString)');
      BindCheckerFunction ('OperationCount', @xsdOperationCount, XFOV, '()');
      BindCheckerFunction ('UserName', @wsdlUserName, SFV, '()');
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
    fExpressStamper := TExpress.Create (nil);
    fExpressStamper.Context := Self;
    fExpressStamper.OnNeedData := NeedStamperData;
    fExpressStamper.OnError := fOnError;
//      fExpress.OnError := ExpressError;
//      fExpress.OnHaveData := HaveData;
    fLineNumber := 0;
    fExpressStamper.Database := _WsdlDbsConnector;
    if Assigned (reqBind) then
      reqBind.Bind ('Req', fExpressStamper, Wsdl.XsdDescr.xsdElementsWhenRepeatable);
    if Assigned (rpyBind) then
      rpyBind.Bind ('Rpy', fExpressStamper, Wsdl.XsdDescr.xsdElementsWhenRepeatable);
    fExpressStamper.BindString('stamper.uwa', fExpressStamper.uwaString);
    BindStamperFunction ('DateTimeToJulianStr', @DateTimeToJulianStr, SFD, '(aDateTime)');
    BindStamperFunction ('DateTimeToTandemJulianStr', @DateTimeToTandemJulianStr, SFD, '(aDateTime)');
    BindStamperFunction ('dbLookUp', @dbLookUp, SFSSSS, '(aTable, aValueColumn, aReferenceColumn, aReferenceValue)');
    BindStamperFunction ('DecEnvNumber', @decVarNumber, XFS, '(aKey)');
    BindStamperFunction ('FormatDate', @FormatDateX, SFDS, '(aDate, aMask)');
    BindStamperFunction ('GetEnvNumber', @getVarNumber, XFS, '(aKey)');
    BindStamperFunction ('GetEnvNumberDef', @getVarNumberDef, XFSX, '(aKey, aDefault)');
    BindStamperFunction ('GetEnvVar', @getVar, SFS, '(aKey)');
    BindStamperFunction ('GetEnvVarDef', @getVarDef, SFSS, '(aKey, aDefault)');
    BindStamperFunction ('HostName', @GetHostName, SFV, '()');
    BindStamperFunction ('ifthen', @ifThenString, SFBSS, '(aCondition, aTrueString, aFalseString)');
    BindStamperFunction ('IncEnvNumber', @incVarNumber, XFS, '(aKey)');
    BindStamperFunction ('LengthStr', @LengthX, XFS, '(aString)');
    BindStamperFunction ('LowercaseStr', @lowercase, SFS, '(aString)');
    BindStamperFunction ('MD5', @MD5, SFS, '(aString)');
    BindStamperFunction ('NumberToStr', @FloatToStr, SFX, '(aNumber)');
    BindStamperFunction ('NowAsStr', @xsdNowAsDateTime, SFV, '()');
    BindStamperFunction ('Occurrences', @OccurrencesX, XFG, '(aElement)');
    BindStamperFunction ('Random', @RandomX, XFXX, '(aLow, aHigh)');
    BindStamperFunction ('Rounded', @RoundedX, XFXX, '(aNumber, aDecimals)');
    BindStamperFunction ('SetEnvNumber', @setEnvNumber, XFSX, '(aKey, aNumber)');
    BindStamperFunction ('SetEnvVar', @setEnvVar, SFSS, '(aKey, aValue)');
    BindStamperFunction ('SHA1', @SHA1, SFS, '(aString)');
    BindStamperFunction ('ShowMessage', @SjowMessage, VFS, '(aString)');
    BindStamperFunction ('SiebelNowAsStr', @sblNowAsDateTime, SFV, '()');
    BindStamperFunction ('SiebelTodayAsStr', @sblTodayAsDate, SFV, '()');
    BindStamperFunction ('StrHasRegExpr', @StringHasRegExpr, SFSS, '(aString, aRegExpr)');
    BindStamperFunction ('StrMatchesRegExpr', @StringMatchesRegExpr, SFSS, '(aString, aRegExpr)');
    BindStamperFunction ('StrToNumber', @StrToFloatX, XFS, '(aString)');
    BindStamperFunction ('SubStr', @SubStringX, SFSXX, '(aString, aStart, aLength)');
//  BindStamperFunction ('Sum', @Sum, XFGG, '(aGroup, aElement)');
    BindStamperFunction ('SwiftNumberToStr', @SwiftNumberToStr, SFX, '(aNumber)');
    BindStamperFunction ('SwiftStrToNumber', @SwiftStrToNumber, XFS, '(aString)');
    BindStamperFunction ('TodayAsStr', @xsdTodayAsDate, SFV, '()');
    BindStamperFunction ('UppercaseStr', @uppercase, SFS, '(aString)');
    BindStamperFunction ('OperationCount', @xsdOperationCount, XFOV, '()');
    BindStamperFunction ('UserName', @wsdlUserName, SFV, '()');
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

function TWsdlOperation .getDoExit : Boolean ;
begin
  result := fDoExit;
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
    with AddXml (TXml.CreateAsString('scripts', '')) do
    begin
      with AddXml (TXml.CreateAsString('invoke', '')) do
      begin
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
  end;
end;

procedure TWsdlOperation.OptionsFromXml(aXml: TXml);
var
  xXml: TXml;
  x: Integer;
begin
  if not Assigned (aXml) then raise Exception.Create('operationOptionsFromXml: No XML assigned');
  if not (aXml.Name = 'operationOptions') then raise Exception.Create('operationOptionsFromXml: Illegal XML: ' + aXml.Text);
  oldInvokeSpec := 'none';
  doReadReplyFromFile := False;
  ReadReplyFromFileXml.Items.Clear;
  xXml := aXml.Items.XmlCheckedItemByTag['scripts'];
  if Assigned (xXml) then
  begin
    oldInvokeSpec := xXml.Items.XmlCheckedValueByTagDef['invoke', oldInvokeSpec];
    invokeList.Clear;
    xXml := xXml.Items.XmlCheckedItemByTag['invoke'];
    if Assigned (xXml) then
    begin
      xXml := xXml.Items.XmlCheckedItemByTag['operations'];
      if Assigned (xXml) then
      begin
        for x := 0 to xXml.Items.Count - 1 do
          if (xXml.Items.XmlItems[x].Name = 'name')
          and (xXml.Items.XmlItems[x].Checked) then
            invokeList.Add(xXml.Items.XmlItems[x].Value);
      end;
    end;
  end;
  xXml := aXml.Items.XmlCheckedItemByTag['ReadReplyFromFile'];
  if Assigned (xXml) then
  begin
    ReadReplyFromFileXml.CopyDownLine(xXml, True);
    doReadReplyFromFile := xXml.Items.XmlCheckedBooleanByTag['Enabled'];
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

procedure TWsdlOperation .InitExecute ;
begin
  DoExit := False;
  LiteralResult := '';
  ReturnSoapFault := False;
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
  xString := xmlio.ReadStringFromFile(xFileName);
  ReplyStringToBindables(xString);
end;

procedure TWsdlOperation.ReplyStringToBindables(aReply: String);
var
  xXml: TXml;
begin
  if WsdlService.DescriptionType in [ipmDTFreeFormat, ipmDTSwiftMT] then
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
        SoapXmlReplyToBindables (xXml, Assigned(Cloned));
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
  if WsdlService.DescriptionType in [ipmDTFreeFormat, ipmDTSwiftMT] then
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
        SoapXmlRequestToBindables (xXml, Assigned (Cloned));
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
      if WsdlService.DescriptionType in [ipmDTFreeFormat, ipmDTEmail] then
        result := (FreeFormatRpy = '')
      else
        result := (rpyXsd.sType.ElementDefs.Count = 0)
              ;
  end;
end;

function TWsdlOperation .getLateBinding : Boolean ;
begin
  result := WsdlService.DescriptionType in [ipmDTFreeFormat];
end;

function TWsdlOperation.getisSoapService: Boolean;
begin
  result := Wsdl.isSoapService;
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
        XmlUtil.presentAsText ( 'TWsdlPart.LinkToXsd (aXsds: TXsdList; aWsdl: TWsdl)'
                              , aWsdl.XsdDescr.ReadFileNames.Text
                              );
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
    (reqBind as TXml).Clean(xsdElementsWhenRepeatable, xsdMaxDepthBillOfMaterials);
  if rpyBind is TXml then
    (rpyBind as TXml).Clean(xsdElementsWhenRepeatable, xsdMaxDepthBillOfMaterials);
  if fltBind is TXml then
    (rpyBind as TXml).Clean(xsdElementsWhenRepeatable, xsdMaxDepthBillOfMaterials);
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
  ColumnXmls := TBindableList.Create;
  CorrelationBindables := TBindableList.Create;
//Patterns := TStringList.Create;
end;

constructor TWsdlMessage.CreateRequest (aOperation: TWsdlOperation; aName, aPatterns, aDocumentation: String);
var
  x: Integer;
begin
  Name := aName;
  WsdlOperation:= aOperation;
  CorrelationBindables := TBindableList.Create;
//Patterns := TStringList.Create;
{}{
{}
  Documentation := aDocumentation;
  aOperation.Messages.AddObject('', self);
  if WsdlOperation.WsdlService.DescriptionType in [ipmDTCobol, ipmDTBmtp] then
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
    with TStringList.Create do
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
  {}{
    Patterns := TStringList.Create;
    Patterns.Text := aPatterns;
    while Patterns.Count < aOperation.CorrelationBindables.Count do
      Patterns.Add(Patterns.Strings[0]);
  {}
    Documentation := aDocumentation;
    aOperation.Messages.AddObject('', self);
    if WsdlOperation.WsdlService.DescriptionType in [ipmDTCobol, ipmDTBmtp] then
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
      if not (aOperation.WsdlService.DescriptionType in [ipmDTFreeFormat]) then
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
      with TStringList.Create do
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
//Patterns.Free;
  inherited;
end;

function TWsdlMessage.CheckValues(aOperation: TWsdlOperation): Boolean;
  procedure _setDoXpctd (aBind: TCustomBindable);
  begin
    if Assigned (aBind) then
    begin
      aBind.DoExpectValue := True;
      _setDoXpctd(aBind.Parent as TCustomBindable);
    end;
  end;
  function _getXbind (aObind: TCustomBindable): TCustomBindable;
  begin
    if aOperation.reqBind.isAncestorOf (aObind) then
      result := self.reqBind.FindUQ(aObind.FullIndexCaption)
    else
      result := self.rpyBind.FindUQ(aObind.FullIndexCaption);
  end;
var
  x: Integer;
  oBind: TCustomBindable; // the Bind that has the recieved value
  xBind: TCustomBindable; // the Bind that has the expected value
begin
  aOperation.reqBind.ResetExpectedValues;
  aOperation.rpyBind.ResetExpectedValues;
  for x := 0 to aOperation.ExpectationBindables.Count - 1 do
  begin
    oBind := aOperation.ExpectationBindables.Bindables [x];
    xBind := _getXbind (oBind);
    if Assigned (xBind) then
    begin
      _setDoXpctd (oBind);
      if xBind.Checked then
        oBind.ExpectedValue := xBind.Value
      else
        oBind.ExpectedValue := '&nil';
      if oBind.Checked <> xBind.Checked then
        oBind.HasUnExpectedValue := True
      else
      begin
        if xBind.Checked then
          oBind.HasUnExpectedValue := (StringMatchesRegExpr(oBind.Value, xBind.Value) <> xBind.Value);
      end;
    end;
  end;
  result := aOperation.reqBind.HasUnExpectedValue
         or aOperation.rpyBind.HasUnExpectedValue;
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

procedure TWsdlBinder.SwiftMtRequestToBindables(aString: String);
begin
  (reqBind as TXml).ResetValues;
  with TSwiftMT.Create(aString, reqXsd) do
  try
    (reqBind as TXml).LoadValues (AsXml, False, True);
  finally
    Free;
  end;
end;

function TWsdlBinder.getDescriptionType: TIpmDescrType;
begin
  result := WsdlOperation.WsdlService.DescriptionType;
end;

procedure TWsdlBinder.PopulateCorrelation (aPatternsList : TStringList );
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
    ipmDTCobol, ipmDTBmtp: ;
    ipmDTXml, ipmDTXsd, ipmDTWsdl, ipmDTEmail, ipmDTSwiftMT, ipmDTJson: result := '';
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

function TWsdlBinder.getInputXsd: TXsd;
begin
  result := fInputXsd;
end;

function TWsdlBinder.getOutputXsd: TXsd;
begin
  result := fOutputXsd;
end;

procedure TWsdlBinder.setFreeFormatReq(const aValue: String);
var
  sl: TStringList;
  x: Integer;
begin
//  if Value = fFreeFormatReq then Exit;
  fFreeFormatReq := aValue;
  sl := TStringList.Create;
  try
    for x := 0 to CorrelationBindables.Count - 1 do
      if Assigned (CorrelationBindables.Bindables[x]) then
        sl.Add (CorrelationBindables.Bindables[x].CorrelationValue)
      else
        sl.Add ('?');
    with reqBind as TXml do
    begin
      LoadFromString(aValue, nil);
      SeparateNsPrefixes;
      ResolveNameSpaces;
    end;
    PopulateCorrelation(sl);
  finally
    sl.Free;
  end;
end;

procedure TWsdlBinder.setFreeFormatRpy(const aValue: String);
begin
//  if aValue = fFreeFormatRpy then Exit;
  fFreeFormatRpy := aValue;
  with rpyBind as TXml do
  begin
    LoadFromString(aValue, nil);
    SeparateNsPrefixes;
    ResolveNameSpaces;
  end;
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
    ipmDTCobol, ipmDTBmtp: (reqBind as TIpmItem).BufferToValues (FoundErrorInBuffer, AValue);
    ipmDTSwiftMT: SwiftMtRequestToBindables(AValue);
    ipmDTXml, ipmDTXsd, ipmDTWsdl, ipmDTEmail: _XmlRequestToBindables;
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
  _WsdlVars := TStringList.Create;
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
  _WsdlDbsTransaction.DataBase := _WsdlDbsConnector;
  _WsdlDbsConnector.Transaction := _WsdlDbsTransaction; // linked to each other...
  UILock := SyncObjs.TCriticalSection.Create;
  EnvVarLock := SyncObjs.TCriticalSection.Create;
  doUILock := True;
  doOperationLock := True;

finalization
  FreeAndNil(_WsdlVars);
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

