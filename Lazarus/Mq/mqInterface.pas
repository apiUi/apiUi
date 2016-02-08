unit mqInterface;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages, DynLibs,
{$ENDIF}
  Classes
   , Controls
   , MQAPI
   , MQRfh2Api
   , StrUtils
   , Xmlz
   ;

type CHAR24 = array[0..23] of Char;
type CHAR32 = array[0..31] of Char;

type TMQUse = ( mquUndefined
              , mquServer
              , mquClient
              );

type FARPROC = Pointer;
type TMQDelete = ( mqdelAll
                 , mqdelNone
                 );

type TOnNewThread = procedure ( Sender: TObject) of Object;

type TOnHaveMqMessage = procedure ( Sender: TObject
                                  ; aHeader, aBody: String
                                  ; aRfhHeader: AnsiString
                                  ; MsgType: MQLONG
                                  ; aMsgDesc: MQMD
                                  ; aMqReturnCode: String
                                  ) of Object;

type
  TMQOptions = class (TObject)
  private
    fOptions: MQLONG;
    function getOptionBoolean (const aOption: MQLONG): Boolean;
    procedure setOptionBoolean (const aOption: MQLONG; aValue: Boolean);
  public
    property Options: MQLONG read fOptions write fOptions;
    constructor Create;
  end;

type
  TGetMsgOptions = class(TMQOptions)
  private
    function getWait: Boolean;
    function getAcceptTruncatedMsg: Boolean;
    function getAllMsgsAvailable: Boolean;
    function getAllSegmentsAvailable: Boolean;
    function getBrowseFirst: Boolean;
    function getBrowseMsgUnderCursor: Boolean;
    function getBrowseNext: Boolean;
    function getCompleteMsg: Boolean;
    function getConvert: Boolean;
    function getFailIfQuiescing: Boolean;
    function getLock: Boolean;
    function getLogicalOrder: Boolean;
    function getMsgUnderCursor: Boolean;
    function getNoSyncPoint: Boolean;
    function getMarkSkipBackout: Boolean;
    function getSetSignal: Boolean;
    function getSyncPoint: Boolean;
    function getUnlock: Boolean;
    procedure setWait(const Value: Boolean);
    procedure setAcceptTruncatedMsg(const Value: Boolean);
    procedure setAllMsgsAvailable(const Value: Boolean);
    procedure setAllSegmentsAvailable(const Value: Boolean);
    procedure setBrowseFirst(const Value: Boolean);
    procedure setBrowseMsgUnderCursor(const Value: Boolean);
    procedure setBrowseNext(const Value: Boolean);
    procedure setCompleteMsg(const Value: Boolean);
    procedure setConvert(const Value: Boolean);
    procedure setFailIfQuiescing(const Value: Boolean);
    procedure setLock(const Value: Boolean);
    procedure setLogicalOrder(const Value: Boolean);
    procedure setMsgUnderCursor(const Value: Boolean);
    procedure setNoSyncPoint(const Value: Boolean);
    procedure setMarkSkipBackout(const Value: Boolean);
    procedure setSetSignal(const Value: Boolean);
    procedure setSyncPoint(const Value: Boolean);
    procedure setUnlock(const Value: Boolean);
  public
    property Wait: Boolean read getWait write setWait;
    property SetSignal: Boolean read getSetSignal write setSetSignal;
    property SyncPoint: Boolean read getSyncPoint write setSyncPoint;
    property NoSyncPoint: Boolean read getNoSyncPoint write setNoSyncPoint;
    property MarkSkipBackout: Boolean read getMarkSkipBackout write setMarkSkipBackout;
    property BrowseFirst: Boolean read getBrowseFirst write setBrowseFirst;
    property BrowseNext: Boolean read getBrowseNext write setBrowseNext;
    property BrowseMsgUnderCursor: Boolean read getBrowseMsgUnderCursor write setBrowseMsgUnderCursor;
    property MsgUnderCursor: Boolean read getMsgUnderCursor write setMsgUnderCursor;
    property Lock: Boolean read getLock write setLock;
    property Unlock: Boolean read getUnlock write setUnlock;
    property AcceptTruncatedMsg: Boolean read getAcceptTruncatedMsg write setAcceptTruncatedMsg;
    property FailIfQuiescing: Boolean read getFailIfQuiescing write setFailIfQuiescing;
    property Convert: Boolean read getConvert write setConvert;
    property LogicalOrder: Boolean read getLogicalOrder write setLogicalOrder;
    property CompleteMsg: Boolean read getCompleteMsg write setCompleteMsg;
    property AllMsgsAvailable: Boolean read getAllMsgsAvailable write setAllMsgsAvailable;
    property AllSegmentsAvailable: Boolean read getAllSegmentsAvailable write setAllSegmentsAvailable;
    property Options;
    constructor Create;
  end;

type
  TGetMsgMatchOptions = class(TMQOptions)
  private
    function getMatchCorrelId: Boolean;
    function getMatchGroupId: Boolean;
    function getMatchMsgId: Boolean;
    function getMatchMsgSeqMumber: Boolean;
    procedure setMatchCorrelId(const Value: Boolean);
    procedure setMatchGroupId(const Value: Boolean);
    procedure setMatchMsgId(const Value: Boolean);
    procedure setMatchMsgSeqMumber(const Value: Boolean);
  public
{
  MQMO_MATCH_MSG_ID              = $00000001;
  MQMO_MATCH_CORREL_ID           = $00000002;
  MQMO_MATCH_GROUP_ID            = $00000004;
  MQMO_MATCH_MSG_SEQ_NUMBER      = $00000008;
  MQMO_MATCH_OFFSET              = $00000010;
  MQMO_MATCH_MSG_TOKEN           = $00000020;
  MQMO_NONE                      = $00000000;
}
    property MatchMsgId: Boolean read getMatchMsgId write setMatchMsgId;
    property MatchCorrelId: Boolean read getMatchCorrelId write setMatchCorrelId;
    property MatchGroupId: Boolean read getMatchGroupId write setMatchGroupId;
    property MatchMsgSeqMumber: Boolean read getMatchMsgSeqMumber write setMatchMsgSeqMumber;
    property MatchOffset: Boolean read getMatchMsgSeqMumber write setMatchMsgSeqMumber;
    property MatchMsgToken: Boolean read getMatchMsgSeqMumber write setMatchMsgSeqMumber;
    property Options;
    constructor Create;
  end;

type
  TPutMsgOptions = class(TMQOptions)
  private
    function getAlternateUserAuthority: Boolean;
    function getDefaultContext: Boolean;
    function getFailIfQuiescing: Boolean;
    function getLogicalOrder: Boolean;
    function getNewMsgId: Boolean;
    function getNewCorrelId: Boolean;
    function getNoContext: Boolean;
    function getNoSyncPoint: Boolean;
    function getPassAllContext: Boolean;
    function getPassIdentityContext: Boolean;
    function getSetAllContext: Boolean;
    function getSetIdentityContext: Boolean;
    function getSyncPoint: Boolean;
    procedure setAlternateUserAuthority(const Value: Boolean);
    procedure setDefaultContext(const Value: Boolean);
    procedure setFailIfQuiescing(const Value: Boolean);
    procedure setLogicalOrder(const Value: Boolean);
    procedure setNewCorrelId(const Value: Boolean);
    procedure setNewMsgId(const Value: Boolean);
    procedure setNoContext(const Value: Boolean);
    procedure setNoSyncPoint(const Value: Boolean);
    procedure setPassAllContext(const Value: Boolean);
    procedure setPassIdentityContext(const Value: Boolean);
    procedure setSetAllContext(const Value: Boolean);
    procedure setSetIdentityContext(const Value: Boolean);
    procedure setSyncPoint(const Value: Boolean);
  public
{
  MQPMO_SYNCPOINT                 = $00000002;
  MQPMO_NO_SYNCPOINT              = $00000004;
  MQPMO_NEW_MSG_ID                = $00000040;
  MQPMO_NEW_CORREL_ID             = $00000080;
  MQPMO_LOGICAL_ORDER             = $00008000;
  MQPMO_NO_CONTEXT                = $00004000;
  MQPMO_DEFAULT_CONTEXT           = $00000020;
  MQPMO_PASS_IDENTITY_CONTEXT     = $00000100;
  MQPMO_PASS_ALL_CONTEXT          = $00000200;
  MQPMO_SET_IDENTITY_CONTEXT      = $00000400;
  MQPMO_SET_ALL_CONTEXT           = $00000800;
  MQPMO_ALTERNATE_USER_AUTHORITY  = $00001000;
  MQPMO_FAIL_IF_QUIESCING         = $00002000;
}
    property SyncPoint: Boolean read getSyncPoint write setSyncPoint;
    property NoSyncPoint: Boolean read getNoSyncPoint write setNoSyncPoint;
    property NewMsgId: Boolean read getNewMsgId write setNewMsgId;
    property NewCorrelId: Boolean read getNewCorrelId write setNewCorrelId;
    property LogicalOrder: Boolean read getLogicalOrder write setLogicalOrder;
    property NoContext: Boolean read getNoContext write setNoContext;
    property DefaultContext: Boolean read getDefaultContext write setDefaultContext;
    property PassIdentityContext: Boolean read getPassIdentityContext write setPassIdentityContext;
    property PassAllContext: Boolean read getPassAllContext write setPassAllContext;
    property SetIdentityContext: Boolean read getSetIdentityContext write setSetIdentityContext;
    property SetAllContext: Boolean read getSetAllContext write setSetAllContext;
    property AlternateUserAuthority: Boolean read getAlternateUserAuthority write setAlternateUserAuthority;
    property FailIfQuiescing: Boolean read getFailIfQuiescing write setFailIfQuiescing;
    property Options;
    constructor Create;
  end;

type
  TMQGMO = class (TObject)
  public
    Options: TGetMsgOptions;
    MatchOptions: TGetMsgMatchOptions;
    constructor Create;
    destructor Destroy; Override;
  end;

type
  TMQPMO = class (TObject)
  public
    Options: TPutMsgOptions;
    constructor Create;
    destructor Destroy; Override;
  end;


type
  TMqInterface = class (TObject)
  private
    ErrorString: String;
    ClientDLLHandle: MQLONG;
    ServerDLLHandle: MQLONG;
    ConnectedToMQClientDLL : Boolean;
    ConnectedToMQServerDLL : Boolean;
    fMQClientOK: Boolean;
    fMQServerOK: Boolean;
    allOK, ConnOK, OpenGetOK, OpenPutOK: Boolean;
    Hconn : MQHCONN;
    HGetObj : MQHOBJ;
    HPutObj: MQHOBJ;
    ObjDesc : MQOD;
    Options: MQLONG;
    CompCode: MQLONG;
    Reason : MQLONG;
    MsgDesc : MQMD;
    RfhHeader: AnsiString;
    PutMsgOptions : MQPMO;
    GetMsgOptions : MQGMO;
    BufferLength : MQLONG;
    DataLength: MQLONG;
    ResponseMicroSeconds: Integer;
    fReqTime: TDateTime;
    fRspTime: TDateTime;
    Buffer: AnsiString; // treated as array [0..256*1024]  of Char;
    xCorrelId : MQBYTE24;
    xQmanager: String;
    xGetQueue: String;
    xPutQueue: String;
    xTimeOut: String;
    xExpiryTime: String;
    xReply: String;
    MQCONNServer: TMQCONNPROC;
    MQDISCServer: TMQDISCPROC;
    MQOPENServer: TMQOPENPROC;
    MQCLOSEServer: TMQCLOSEPROC;
    MQGETServer: TMQGETPROC;
    MQPUTServer: TMQPUTPROC;
    MQPUT1Server: TMQPUT1PROC;
    MQBACKServer: TMQBACKPROC;
    MQCMITServer: TMQCMITPROC;
    MQINQServer: TMQINQPROC;
    MQSETServer: TMQSETPROC;
    MQCONNClient: TMQCONNPROC;
    MQDISCClient: TMQDISCPROC;
    MQOPENClient: TMQOPENPROC;
    MQCLOSEClient: TMQCLOSEPROC;
    MQGETClient: TMQGETPROC;
    MQPUTClient: TMQPUTPROC;
    MQPUT1Client: TMQPUT1PROC;
    MQBACKClient: TMQBACKPROC;
    MQCMITClient: TMQCMITPROC;
    MQINQClient: TMQINQPROC;
    MQSETClient: TMQSETPROC;
    fUse: TMQUse;
    fMQGMOGetOnly: TMQGMO;
    fMQGMORequestReply: TMQGMO;
    fMQPMORequestReply: TMQPMO;
    fMQPMOFireAndForget: TMQPMO;
    rfhReverseInteger: Boolean;
    fUseReplyToQueue: Boolean;
    fUseReplyToQmanager: Boolean;
    fDeleteMessages: TMQDelete;
    function getReversedEncoding: Boolean;
    function getReplyToQmanager: String;
    procedure setReplyToQmanager(const Value: String);
    function getReplyToQueue: String;
    procedure setReplyToQueue(const Value: String);
    function getdoReportPassCorrelId: Boolean;
    function getdoReportCopyMsgIdToCorrelId: Boolean;
    function getdoReportPassMsgId: Boolean;
    function getCorrelId: String;
    function getMsgId: String;
    procedure setCorrelId(const Value: String);
    procedure setMsgId(const Value: String);
    procedure CloseAll (aDisconnect: Boolean);
    procedure ConnectToDLLs;
    procedure setUse(const Value: TMQUse);
    function mqiRetErr (aFie: String): String;
    function mqiMqConn (var aDoDisconnect: Boolean): Boolean;
    function mqiMqOpenPut: Boolean;
    function mqiMqOpenGet (aBrowse: Boolean): Boolean;
    function mqiMqCloseGet: Boolean;
    function mqiMqClosePut: Boolean;
    function mqiMqDisc: Boolean;
    function mqiMqResult: String;
    function MsgDescFromXml (aXml: TXml): AnsiString;
    procedure mqiCheckUse;
  public
    overwriteReplyToQManager: String;
    overwriteReplyToQueue: String;
    doTerminate: Boolean;
    browseMqMtRequest: Boolean;
    browseMqMtReply: Boolean;
    browseMqMtReport: Boolean;
    browseMqMtDatagram: Boolean;
    property ReversedEncoding: Boolean read getReversedEncoding;
    constructor Create;
    constructor CreateFromXml(aXml: TXml);
    constructor CreateFromXmlOldStyle(aXml: TXml);
    destructor Destroy; override;
    procedure Put (aMessage: String; aMqHeaderAsXml: TXml); Overload;
    function PutReply (aMessage: String; aRfhHeader: AnsiString; aMsgDesc: MQMD): String; Overload;
    function _PutReply (aInterface: TMqInterface; aMessage: String; aRfhHeader: AnsiString; aReqMsgDesc: MQMD): String; Overload;
    procedure FireAndForget (aMessage: String; aMqHeaderAsXml: TXml); Virtual;
    function MessageTimeStamp: TDateTime;
    function GetOnly: String; Virtual;
    function Get: String; Virtual;
    function RequestReply (aRequest: String; aMqHeaderAsXml: TXml): String; Virtual;
    function MsgDescAsText: String;
    function MsgDescAsXml: TXml;
    procedure GetMsgUnderCursor;
    procedure Browse (OnHaveMessage: TOnHaveMqMessage; OnNewThread: TOnNewThread; doWait, doRemove: Boolean);
    function AsXmlOldStyle: TXml;
    function AsXml: TXml;
    property Qmanager: String read xQmanager write xQmanager;
    property GetQueue: String read xGetQueue write xGetQueue;
    property PutQueue: String read xPutQueue write xPutQueue;
    property useReplyToQmanager: Boolean read fUseReplyToQmanager write fUseReplyToQmanager;
    property ReplyToQmanager: String read getReplyToQmanager write setReplyToQmanager;
    property useReplyToQueue: Boolean read fUseReplyToQueue write fUseReplyToQueue;
    property ReplyToQueue: String read getReplyToQueue write setReplyToQueue;
    property DeleteMessages: TMQDelete read fDeleteMessages write fDeleteMessages;
    property TimeOut: String read xTimeOut write xTimeOut;
    property Expiry: String read xExpiryTime write xExpiryTime;
    property Reply: String read xReply;
    property CorrelId: String read getCorrelId write setCorrelId;
    property MsgId: String read getMsgId write setMsgId;
    property doReportPassMsgId: Boolean read getdoReportPassMsgId;
    property doReportCopyMsgIdToCorrelId: Boolean read getdoReportCopyMsgIdToCorrelId;
    property doReportPassCorrelId: Boolean read getdoReportPassCorrelId;
    property Use: TMQUse read fUse write setUse;
    property MQServerOK: Boolean read fMqServerOK;
    property MQClientOK: Boolean read fMqClientOK;
    property RequestTimestamp: TDateTime read fReqTime;
    property ReplyTimestamp: TDateTime read fRspTime;
  end;

procedure CopyChars (var d: array of AnsiChar; s: String;  size: Integer);

implementation

{ TMqInterface }
uses SysUtils
   , Dialogs
   ;

type TOnHaveMessageThread = class(TThread)
private
  fOnHaveMessage: TOnHaveMqMessage;
  fSender: TObject;
  fHeader, fBody, fRfhHeader, fMqReturnCode: String;
  fMsgType: MQLONG;
  fMsgDesc: MQMD;
protected
  procedure Execute; override;
public
  constructor Create ( aOnHaveMessage: TOnHaveMqMessage
                     ; aSender: TObject
                     ; aHeader, aBody, aRfhHeader: String
                     ; aMsgType: MQLONG
                     ; aMsgDesc: MQMD
                     ; aMqReturnCode: String
                     );
end;


procedure CopyChars (var d: array of AnsiChar; s: String;  size: Integer);
var
  x: Integer;
  xs: AnsiString;
begin
  x := 0;
  xs := s;
  while (x < Size) and (x < Length (xs)) do
  begin
    d [x] := xs [x + 1];
    Inc (x);
  end;
  while (x < Size) do
  begin
    d [x] := ' ';
    Inc (x);
  end;
end;

const
  // Name of the MQ Client DLL.
  MQCLIENTDLL = 'MQIC32.' + SharedSuffix;
  // Name of the MQ local DLL.
  MQLOCALDLL = 'MQM.' + SharedSuffix;

function TMqInterface.mqiRetErr (aFie: String): String;
begin
  result := format ( '%S failed (CompCode=%d,Reason=%s)'
                   , [aFie, CompCode, MQErrorReason(Reason)]
                   );
end;

procedure TMqInterface.ConnectToDLLs;
var
  xDllName: String;
  xDllHandle: MQLONG;
  // Load a function address and give an error if failed.
  function CheckProcAddress(AName: PChar): FARPROC;
  begin
    Result:=GetProcAddress(xDllHandle, AName);
    if Result=nil then
      raise Exception.Create (Format('The procedure "%s" in dll "%s" can not be found.', [AName, xDllName]));
  end;

  procedure ConnectDll;
  begin
    try
      xDllHandle:=LoadLibrary(PChar(xDllName));
    except
    end;
  end;

begin
  ConnectedToMQServerDLL:=False;
  xDllName:=MQLOCALDLL; // Try connecting the MQ Server DLL.
  try
    ConnectDll;
    if xDllHandle <> 0 then
    begin
      ServerDLLHandle := xDllHandle;
      ConnectedToMQServerDLL := True;
      @MQCONNServer := CheckProcAddress('MQCONN');
      @MQDISCServer := CheckProcAddress('MQDISC');
      @MQOPENServer := CheckProcAddress('MQOPEN');
      @MQCLOSEServer := CheckProcAddress('MQCLOSE');
      @MQGETServer := CheckProcAddress('MQGET');
      @MQPUTServer := CheckProcAddress('MQPUT');
      @MQPUT1Server := CheckProcAddress('MQPUT1');
      @MQBACKServer := CheckProcAddress('MQBACK');
      @MQCMITServer := CheckProcAddress('MQCMIT');
      @MQINQServer := CheckProcAddress('MQINQ');
      @MQSETServer := CheckProcAddress('MQSET');
      fMQServerOK := True;
    end;
  except
  end;

  ConnectedToMQClientDLL:=False;
  xDllName:=MQCLIENTDLL; // Try connecting the MQ Client DLL.
  try
    ConnectDll;
    if xDllHandle <> 0 then
    begin
      ClientDLLHandle := xDllHandle;
      ConnectedToMQClientDLL := True;
      @MQCONNClient := CheckProcAddress('MQCONN');
      @MQDISCClient := CheckProcAddress('MQDISC');
      @MQOPENClient := CheckProcAddress('MQOPEN');
      @MQCLOSEClient := CheckProcAddress('MQCLOSE');
      @MQGETClient := CheckProcAddress('MQGET');
      @MQPUTClient := CheckProcAddress('MQPUT');
      @MQPUT1Client := CheckProcAddress('MQPUT1');
      @MQBACKClient := CheckProcAddress('MQBACK');
      @MQCMITClient := CheckProcAddress('MQCMIT');
      @MQINQClient := CheckProcAddress('MQINQ');
      @MQSETClient := CheckProcAddress('MQSET');
      fMQClientOK := True;
    end;
  except
  end;
end;

function TMqInterface.AsXmlOldStyle: TXml;
begin
  result := TXml.CreateAsString ('mqInterface', '');
  result.AddXml (TXml.CreateAsString ('Qmanager', Qmanager));
  result.AddXml (TXml.CreateAsString ('GetQueue', GetQueue));
  result.AddXml (TXml.CreateAsString ('PutQueue', PutQueue));
  result.AddXml (TXml.CreateAsBoolean ('useReplyToQmanager', useReplyToQmanager));
  result.AddXml (TXml.CreateAsString ('ReplyToQmanager', overwriteReplyToQmanager));
  result.AddXml (TXml.CreateAsBoolean ('useReplyToQueue', useReplyToQueue));
  result.AddXml (TXml.CreateAsString ('ReplyToQueue', overwriteReplyToQueue));
  result.AddXml (TXml.CreateAsString ('TimeOut', TimeOut));
  result.AddXml (TXml.CreateAsString ('Expiry', Expiry));
  result.AddXml (TXml.CreateAsInteger ('DeleteMessages', Ord (DeleteMessages)));
  result.AddXml (TXml.CreateAsBoolean ('browseMqMtRequest', browseMqMtRequest));
  result.AddXml (TXml.CreateAsBoolean ('browseMqMtReply', browseMqMtReply));
  result.AddXml (TXml.CreateAsBoolean ('browseMqMtReport', browseMqMtReport));
  result.AddXml (TXml.CreateAsBoolean ('browseMqMtDatagram', browseMqMtDatagram));
end;

function TMqInterface.AsXml: TXml;
begin
  result := TXml.CreateAsString ('Queue', '');
  with result do
  begin
    AddXml (TXml.CreateAsString ('Manager', Qmanager));
    AddXml (TXml.CreateAsString ('Name', GetQueue));
    with AddXml (TXml.CreateAsString ('ReplyTo', '')) do
    begin
      if not useReplyToQueue then
      begin
        with AddXml (TXml.CreateAsString ('Queue', '')) do
        begin
          AddXml (TXml.CreateAsString ('Manager', overwriteReplyToQmanager));
          AddXml (TXml.CreateAsString ('Name', overwriteReplyToQueue));
        end;
      end
      else
      begin
        AddXml (TXml.CreateAsString ('AccordingRequest', ''));
      end;
    end;
    with AddXml (TXml.CreateAsString ('BrowseTypes', '')) do
    begin
      AddXml (TXml.CreateAsBoolean ('Request', browseMqMtRequest));
      AddXml (TXml.CreateAsBoolean ('Reply', browseMqMtReply));
      AddXml (TXml.CreateAsBoolean ('Report', browseMqMtReport));
      AddXml (TXml.CreateAsBoolean ('Datagram', browseMqMtDatagram));
    end;
    AddXml (TXml.CreateAsBoolean ('DeleteOnBrowse', (DeleteMessages = mqdelAll)));
  end;
end;

constructor TMqInterface.CreateFromXml(aXml: TXml);
var
  xXml: TXml;
begin
  if not Assigned (aXml) then raise Exception.Create ('TMqInterface.CreateFromXml: No XML');
  if aXml.Name <> 'Queue' then raise Exception.Create ('TMqInterface.CreateFromXml: Illegal XML ' + aXml.Text);
  inherited Create;  // Initialize inherited parts
  fMQPMORequestReply := TMQPMO.Create;
  fMQGMORequestReply := TMQGMO.Create;
  fMQGMORequestReply.MatchOptions.Options := {MQMO_MATCH_MSG_ID
                                          + }MQMO_MATCH_CORREL_ID
                                          ;
  fMQPMOFireAndForget := TMQPMO.Create;
  fMQGMOGetOnly := TMQGMO.Create;
  fMQGMOGetOnly.MatchOptions.Options := MQMO_MATCH_MSG_ID
                                     + MQMO_MATCH_CORREL_ID
                                     ;
  allOK := True;
  ConnOK := False;
  OpenGetOK := False;
  OpenPutOK := False;
  ErrorString := '';
  browseMqMtRequest := True;
  Qmanager := aXml.Items.XmlCheckedValueByTag ['Manager'];
  GetQueue := aXml.Items.XmlCheckedValueByTag ['Name'];
  useReplyToQmanager := True;
  useReplyToQueue := True;
  xXml := aXml.Items.XmlCheckedItemByTag ['ReplyTo'];
  if Assigned (xXml) then
  begin
    xXml := xXml.Items.XmlCheckedItemByTag ['Queue'];
    if Assigned (xXml) then
    begin
      useReplyToQmanager := False;
      useReplyToQueue := False;
      overwriteReplyToQmanager := xXml.Items.XmlCheckedValueByTag ['Manager'];
      overwriteReplyToQueue := xXml.Items.XmlCheckedValueByTag ['Name'];
    end;
  end;
  xXml := aXml.Items.XmlCheckedItemByTag ['BrowseTypes'];
  if Assigned (xXml) then
  begin
    with xXml.Items do
    begin
      browseMqMtRequest := XmlCheckedBooleanByTagDef ['Request', True];
      browseMqMtReply := XmlCheckedBooleanByTagDef ['Reply', False];
      browseMqMtReport := XmlCheckedBooleanByTagDef ['Report', False];
      browseMqMtDatagram := XmlCheckedBooleanByTagDef ['Datagram', False];
    end;
  end;
  if aXml.Items.XmlCheckedBooleanByTagDef ['DeleteOnBrowse', True] then
    DeleteMessages := mqdelAll
  else
    DeleteMessages := mqdelNone;
  ConnectToDLLs;
end;

constructor TMqInterface.CreateFromXmlOldStyle(aXml: TXml);
begin
  if not Assigned (aXml) then raise Exception.Create ('TMqInterface.CreateFromXml: No XML');
  if aXml.Name <> 'mqInterface' then raise Exception.Create ('TMqInterface.CreateFromXml: Illegal XML ' + aXml.Text);
  fMQPMORequestReply := TMQPMO.Create;
  fMQGMORequestReply := TMQGMO.Create;
  fMQGMORequestReply.MatchOptions.Options := {MQMO_MATCH_MSG_ID
                                          + }MQMO_MATCH_CORREL_ID
                                          ;
  fMQPMOFireAndForget := TMQPMO.Create;
  fMQGMOGetOnly := TMQGMO.Create;
  fMQGMOGetOnly.MatchOptions.Options := MQMO_MATCH_MSG_ID
                                     + MQMO_MATCH_CORREL_ID
                                     ;
  allOK := True;
  ConnOK := False;
  OpenGetOK := False;
  OpenPutOK := False;
  ErrorString := '';
  Qmanager := aXml.Items.XmlValueByTag ['Qmanager'];
  GetQueue := aXml.Items.XmlValueByTag ['GetQueue'];
  PutQueue := aXml.Items.XmlValueByTag ['PutQueue'];
  useReplyToQmanager := aXml.Items.XmlBooleanByTag ['useReplyToQmanager'];
  overwriteReplyToQmanager := aXml.Items.XmlValueByTag ['ReplyToQmanager'];
  useReplyToQueue := aXml.Items.XmlBooleanByTag ['useReplyToQueue'];
  overwriteReplyToQueue := aXml.Items.XmlValueByTag ['ReplyToQueue'];
  TimeOut := aXml.Items.XmlValueByTag ['TimeOut'];
  Expiry := aXml.Items.XmlValueByTag ['Expiry'];
  DeleteMessages := TMQDelete (aXml.Items.XmlIntegerByTag ['DeleteMessages']);
  browseMqMtRequest := aXml.Items.XmlBooleanByTagDef ['browseMqMtRequest', True];
  browseMqMtReply := aXml.Items.XmlBooleanByTag ['browseMqMtReply'];
  browseMqMtReport := aXml.Items.XmlBooleanByTag ['browseMqMtReport'];
  browseMqMtDatagram := aXml.Items.XmlBooleanByTag ['browseMqMtDatagram'];
  ConnectToDLLs;
end;

constructor TMqInterface.Create;
begin
  fMQPMORequestReply := TMQPMO.Create;
  fMQGMORequestReply := TMQGMO.Create;
  fMQGMORequestReply.MatchOptions.Options := {MQMO_MATCH_MSG_ID
                                          + }MQMO_MATCH_CORREL_ID
                                          ;
  fMQPMOFireAndForget := TMQPMO.Create;
  fMQGMOGetOnly := TMQGMO.Create;
  fMQGMOGetOnly.MatchOptions.Options := MQMO_MATCH_MSG_ID
                                     + MQMO_MATCH_CORREL_ID
                                     ;
  allOK := True;
  ConnOK := False;
  OpenGetOK := False;
  OpenPutOK := False;
  ErrorString := '';
  useReplyToQmanager := True;
  useReplyToQueue := True;
  browseMqMtRequest := True;
  ConnectToDLLs;
end;

destructor TMqInterface.Destroy;
begin
  CloseAll (True);
  FreeAndNil(fMQPMORequestReply.Options); // should not be nessecary....
  FreeAndNil(fMQGMORequestReply.Options); // should not be nessecary....
  FreeAndNil(fMQGMORequestReply.MatchOptions); // should not be nessecary....
  FreeAndNil(fMQPMOFireAndForget.Options); // should not be nessecary....
  FreeAndNil(fMQGMOGetOnly.Options); // should not be nessecary....
  FreeAndNil(fMQGMOGetOnly.MatchOptions); // should not be nessecary....
  fMQPMORequestReply.Free;
  fMQGMORequestReply.Free;
  fMQPMOFireAndForget.Free;
  fMQGMOGetOnly.Free;
  if ConnectedToMQClientDLL then
    FreeLibrary(ClientDLLHandle);
  if ConnectedToMQServerDLL then
    FreeLibrary(ServerDLLHandle);
end;

function TMqInterface.mqiMqConn (var aDoDisconnect: Boolean): Boolean;
var
  xAnsiQManager: AnsiString;
begin
  aDoDisconnect := True;
  xAnsiQManager := xQmanager;
  MQCONN (PCHAR(xAnsiQmanager), @HConn, @Compcode, @Reason);
  if (CompCode <> MQCC_OK)
  and (Reason = MQRC_ALREADY_CONNECTED) then
  begin
    aDoDisconnect := False;
    CompCode := MQCC_OK;
  end;
  result := (CompCode = MQCC_OK);
end;

function TMqInterface.mqiMqDisc: Boolean;
begin
  MQDISC (@HConn, @Compcode, @Reason);
  result := (CompCode = MQCC_OK);
end;

function TMqInterface.mqiMqCloseGet: Boolean;
begin
  Options := MQCO_NONE;
  MQCLOSE (HConn, @HGetObj, Options, @Compcode, @Reason);
  result := (CompCode = MQCC_OK);
end;

function TMqInterface.mqiMqClosePut: Boolean;
begin
  Options := MQCO_NONE;
  MQCLOSE (HConn, @HPutObj, Options, @Compcode, @Reason);
  result := (CompCode = MQCC_OK);
end;

function TMqInterface.mqiMqOpenGet (aBrowse: Boolean): Boolean;
var
  x: Integer;
  xAnsiGetQueue: AnsiString;
begin
  ObjDesc := MQOD_DEFAULT;
  xAnsiGetQueue := xGetQueue;
  for x := 1 to Length (xAnsiGetQueue) do
    ObjDesc.ObjectName [x - 1] := xAnsiGetQueue [x];
  Options := MQOO_FAIL_IF_QUIESCING + MQOO_INPUT_SHARED;
  if aBrowse then
    Options := Options + MQOO_BROWSE;
{}
  MQOPEN (Hconn, @ObjDesc, Options, @HGetObj, @Compcode, @Reason );
  result := (CompCode = MQCC_OK);
{}
end;

function TMqInterface.mqiMqOpenPut: Boolean;
var
  x: Integer;
  xAnsiGetQueue: AnsiString;
begin
  ObjDesc := MQOD_DEFAULT;
  xAnsiGetQueue := xPutQueue;
  for x := 1 to Length (xAnsiGetQueue) do
    ObjDesc.ObjectName [x - 1] := xAnsiGetQueue [x];
  Options := MQOO_FAIL_IF_QUIESCING + MQOO_OUTPUT;
{}
  MQOPEN (Hconn, @ObjDesc, Options, @HPutObj, @Compcode, @Reason );
  result := (CompCode = MQCC_OK);
{}
end;

function TMqInterface.mqiMqResult: String;
begin
  rfhReverseInteger := False;
  RfhHeader := '';
  SetLength (Buffer, DataLength);
  if (MsgDesc.Format = MQFMT_RF_HEADER)
  or (MsgDesc.Format = MQFMT_RF_HEADER_2) then
    try
      result := mqRfh2Result (Buffer, RfhHeader);
    except
      on e: Exception do
      begin
        result := Buffer;
        RfhHeader := e.Message;
      end;
    end
  else
    result := Buffer;
end;

procedure TMqInterface.setUse(const Value: TMQUse);
begin
  fUse := Value;
  begin
    if fUse = mquServer then
    begin
      if not MqServerOK then
      begin
        fUse := mquUndefined;
        raise Exception.Create(Format ('Server DLL (%s) not loaded', [MQLOCALDLL])) ;
      end;
      @MQCONN := @MQCONNServer;
      @MQDISC := @MQDISCServer;
      @MQOPEN := @MQOPENServer;
      @MQCLOSE := @MQCLOSEServer;
      @MQGET := @MQGETServer;
      @MQPUT := @MQPUTServer;
      @MQPUT1 := @MQPUT1Server;
      @MQBACK := @MQBACKServer;
      @MQCMIT := @MQCMITServer;
      @MQINQ := @MQINQServer;
      @MQSET := @MQSETServer;
    end;

    if fUse = mquClient then
    begin
      if not MqClientOK then
      begin
        fUse := mquUndefined;
        raise Exception.Create(Format ('Server DLL (%s) not loaded', [MQCLIENTDLL])) ;
      end;
      @MQCONN := @MQCONNClient;
      @MQDISC := @MQDISCClient;
      @MQOPEN := @MQOPENClient;
      @MQCLOSE := @MQCLOSEClient;
      @MQGET := @MQGETClient;
      @MQPUT := @MQPUTClient;
      @MQPUT1 := @MQPUT1Client;
      @MQBACK := @MQBACKClient;
      @MQCMIT := @MQCMITClient;
      @MQINQ := @MQINQClient;
      @MQSET := @MQSETClient;
    end;
  end;
end;

procedure TMqInterface.Put(aMessage: String; aMqHeaderAsXml: TXml);
var
  xMsgId: AnsiString;
  xCorrelId: AnsiString;
  xDisconnect: Boolean;
begin
  xMsgId := MsgId;
  xCorrelId := CorrelId;
  mqiCheckUse;
  allOK := True;
  ConnOK := False;
  OpenPutOK := False;

  xDisconnect := false; // to avoid compiler warnings
  ConnOK := mqiMqConn (xDisconnect);
  if not ConnOK then
    Raise Exception.Create (mqiRetErr('MQCONN'));

// open Put queue
  if AllOK then
  begin
    OpenPutOK := mqiMqOpenPut;
    if not OpenPutOk then
    begin
      ErrorString := mqiRetErr ('MQOPEN (Put)');
      AllOK := False;
    end;
  end;

//Put
  if AllOK then
  begin
    MsgDesc := MQMD_DEFAULT;
    MsgId := xMsgId;
    CorrelId := xCorrelId;
    MsgDesc.Format := MQFMT_STRING;
    MsgDesc.MsgType := MQMT_DATAGRAM;
    MsgDesc.Report := MQRO_NONE;
    MsgDesc.Expiry := StrToInt (xExpiryTime) * 10;
    if MsgDesc.Expiry = -10 then
      MsgDesc.Expiry := -1;
    Buffer := mqRfh2ReverseInts (MsgDescFromXml(aMqHeaderAsXml), ReversedEncoding) + aMessage;
    PutMsgOptions := MQPMO_DEFAULT;
    PutMsgOptions.Options := fMQPMOFireAndForget.Options.Options;
{}
    MQPUT ( Hconn
          , HPutobj
          , @MsgDesc
          , @PutMsgOptions
          , Length (Buffer)
          , PAnsiChar (Buffer)
          , @Compcode
          , @Reason
          );
{}
    fReqTime := Now;
    fRspTime := fReqTime;
    if CompCode <> MQCC_OK then
    begin
      ErrorString := mqiRetErr ('MQPUT');
      AllOK := False;
    end;
  end;

  CloseAll (xDisconnect);
  if not AllOK then
    Raise Exception.Create(ErrorString);
end;

procedure TMqInterface.FireAndForget(aMessage: String; aMqHeaderAsXml: TXml);
var
  xDisconnect: Boolean;
  xMessage: AnsiString;
begin
  mqiCheckUse;
  allOK := True;
  ConnOK := False;
  OpenPutOK := False;

  xDisconnect := false; // to avoid compiler warnings
  ConnOK := mqiMqConn (xDisconnect);
  if not ConnOK then
    Raise Exception.Create (mqiRetErr('MQCONN'));

// open Put queue
  if AllOK then
  begin
    OpenPutOK := mqiMqOpenPut;
    if not OpenPutOk then
    begin
      ErrorString := mqiRetErr ('MQOPEN (Put)');
      AllOK := False;
    end;
  end;

//Put
  if AllOK then
  begin
    MsgDesc := MQMD_DEFAULT;
    MsgDesc.MsgId := MQMI_NONE;
    MsgDesc.CorrelId := MQCI_NONE;
    MsgDesc.Format := MQFMT_STRING;
    MsgDesc.MsgType := MQMT_DATAGRAM;
    MsgDesc.Report := MQRO_NONE;
    MsgDesc.Expiry := StrToInt (xExpiryTime) * 10;
    if MsgDesc.Expiry = -10 then
      MsgDesc.Expiry := -1;
    xMessage := mqRfh2ReverseInts (MsgDescFromXml(aMqHeaderAsXml), ReversedEncoding) + aMessage;

    PutMsgOptions := MQPMO_DEFAULT;
    PutMsgOptions.Options := fMQPMOFireAndForget.Options.Options;
{}
    MQPUT ( Hconn
          , HPutobj
          , @MsgDesc
          , @PutMsgOptions
          , Length (xMessage)
          , PAnsiChar (xMessage)
          , @Compcode
          , @Reason
          );
{}
    fReqTime := Now;
    fRspTime := fReqTime;
    if CompCode <> MQCC_OK then
    begin
      ErrorString := mqiRetErr ('MQPUT');
      AllOK := False;
    end;
  end;

  CloseAll (xDisconnect);
end;

function TMqInterface.Get: String;
var
  xDisconnect: Boolean;
begin
  mqiCheckUse;
  allOK := True;
  ConnOK := False;
  OpenGetOK := False;

  Buffer := 'This is a fake response';
  DataLength := Length (Buffer); // and a fake datalength
  ResponseMicroSeconds := 100; // and a fake responsetime

  xDisconnect := false; // to avoid compiler warnings
  ConnOK := mqiMqConn (xDisconnect);
  if not ConnOK then
    Raise Exception.Create (mqiRetErr('MQCONN'));

// open Get queue
  if AllOK then
  begin
    OpenGetOK := mqiMqOpenGet (False);
    if not OpenGetOk then
    begin
      ErrorString := mqiRetErr ('MQOPEN (Get)');
      AllOK := False;
    end;
  end;

//Get
  if AllOK then
  begin
    MsgDesc := MQMD_DEFAULT;
    GetMsgOptions := MQGMO_GETWAIT;
{
    GetMsgOptions.Options := MQGMOGetOnly.Options.Options;
    GetMsgOptions.MatchOptions := MQGMOGetOnly.MatchOptions.Options;
    GetMsgOptions.WaitInterval := 0;
}
    SetLength (Buffer, 1024*1024);
    BufferLength := 1024*1024;
{}
    MQGET ( Hconn
          , HGetObj
          , @MsgDesc
          , @GetMsgOptions
          , BufferLength
          , PAnsiChar (Buffer)
          , @DataLength
          , @CompCode
          , @Reason
          );
{}
    fRspTime := now;
    if CompCode <> MQCC_OK then
    begin
      ErrorString := mqiRetErr ('MQGET');
      AllOK := False;
    end;
  end;

  CloseAll (xDisconnect);

  if AllOK then
  begin
    result := mqiMqResult;
  end;
end;

function TMqInterface.GetOnly: String;
var
  xDisconnect: Boolean;
begin
  mqiCheckUse;
  allOK := True;
  ConnOK := False;
  OpenGetOK := False;

  Buffer := 'This is a fake response';
  DataLength := Length (Buffer); // and a fake datalength
  ResponseMicroSeconds := 100; // and a fake responsetime

  xDisconnect := false; // to avoid compiler warnings
  ConnOK := mqiMqConn(xDisconnect);
  if not ConnOK then
    Raise Exception.Create (mqiRetErr('MQCONN'));

// open Get queue
  if AllOK then
  begin
    OpenGetOK := mqiMqOpenGet (False);
    if not OpenGetOk then
    begin
      ErrorString := mqiRetErr ('MQOPEN (Get)');
      AllOK := False;
    end;
  end;

//Get (Only)
  if AllOK then
  begin
    MsgDesc := MQMD_DEFAULT;
    GetMsgOptions := MQGMO_DEFAULT;
    GetMsgOptions.Options := fMQGMOGetOnly.Options.Options;
    GetMsgOptions.MatchOptions := fMQGMOGetOnly.MatchOptions.Options;
    GetMsgOptions.WaitInterval := 0;
    SetLength (Buffer, 1024*1024);
    BufferLength := 1024*1024;
{}
    MQGET ( Hconn
          , HGetObj
          , @MsgDesc
          , @GetMsgOptions
          , BufferLength
          , PAnsiChar (Buffer)
          , @DataLength
          , @CompCode
          , @Reason
          );
{}
    fRspTime := now;
    if CompCode <> MQCC_OK then
    begin
      ErrorString := mqiRetErr ('MQGET');
      AllOK := False;
    end;
  end;

  CloseAll (xDisconnect);

  if AllOK then
  begin
    result := mqiMqResult;
  end;
end;

function TMqInterface.RequestReply(aRequest: String; aMqHeaderAsXml: TXml): String;
var
  xDisconnect: Boolean;
  x: Integer;
  xReplyToQueue: String;
  xRequest: AnsiString;
  function genCorrelId (aDT: TDateTime): MQBYTE24;
  var
    xString: String;
    x: Integer;
  begin
    DateTimeToString (xString, 'yyyymmddhhmmsszzz', aDT);
    xString := xString + IntToStr (100000000 + Random (10000000));
    for x := 1 to 24 do
      result [x - 1] := Byte (xString [x]);
  end;
begin
  mqiCheckUse;
  allOK := True;
  ConnOK := False;
  OpenGetOK := False;
  OpenPutOK := False;
  ErrorString := '';

  Buffer := 'This is a fake response';
  DataLength := Length (Buffer); // and a fake datalength
  ResponseMicroSeconds := 100; // and a fake responsetime
  xDisconnect := false; // to avoid compiler warnings
  ConnOK := mqiMqConn (xDisconnect);
  if not ConnOK then
    Raise Exception.Create (mqiRetErr('MQCONN'));

// open Put queue
  if AllOK then
  begin
    OpenPutOK := mqiMqOpenPut;
    if not OpenPutOk then
    begin
      ErrorString := mqiRetErr ('MQOPEN (Put)');
      AllOK := False;
    end;
  end;

// open Get queue
  if AllOK then
  begin
    OpenGetOK := mqiMqOpenGet (False);
    if not OpenGetOk then
    begin
      ErrorString := mqiRetErr ('MQOPEN (Get)');
      AllOK := False;
    end;
  end;

//Put
  if AllOK then
  begin
    fReqTime := now;
    xCorrelID := genCorrelId (fReqTime);
    xReplyToQueue := ReplyToQueue;
    MsgDesc := MQMD_DEFAULT;
    MsgDesc.MsgId := xCorrelId;
    MsgDesc.CorrelId := MQCI_NONE;
    MsgDesc.Format := MQFMT_STRING;
    MsgDesc.MsgType := MQMT_REQUEST;
    ReplyToQueue := xReplyToQueue;
    MsgDesc.Report := MQRO_COPY_MSG_ID_TO_CORREL_ID;
    MsgDesc.Persistence := MQPER_NOT_PERSISTENT;
    MsgDesc.Expiry := StrToInt (xExpiryTime) * 10;
    if MsgDesc.Expiry = -10 then
      MsgDesc.Expiry := -1;
    xRequest := mqRfh2ReverseInts (MsgDescFromXml(aMqHeaderAsXml), ReversedEncoding) + aRequest;

    PutMsgOptions := MQPMO_DEFAULT;
    PutMsgOptions.Options := fMQPMORequestReply.Options.Options;
{}
    MQPUT ( Hconn
          , HPutobj
          , @MsgDesc
          , @PutMsgOptions
          , Length (xRequest)
          , PAnsiChar (xRequest)
          , @Compcode
          , @Reason
          );
{}
    if CompCode <> MQCC_OK then
    begin
      ErrorString := mqiRetErr ('MQPUT');
      AllOK := False;
    end;
//    CorrelID := MsgDesc.MsgId;
  end;

//Get
  if AllOK then
  begin
    MsgDesc.MsgId := MQMI_NONE;
    MsgDesc.CorrelId := xCorrelId;
    GetMsgOptions := MQGMO_DEFAULT;
    GetMsgOptions.Options := 0;
    GetMsgOptions.Options := fMQGMORequestReply.Options.Options;
    GetMsgOptions.MatchOptions := fMQGMORequestReply.MatchOptions.Options;
    GetMsgOptions.WaitInterval := StrToIntDef (xTimeOut, 15)*1000;
    SetLength (Buffer, 1024*1024);
    BufferLength := 1024*1024;
{}
    MQGET ( Hconn
          , HGetObj
          , @MsgDesc
          , @GetMsgOptions
          , BufferLength
          , PAnsiChar (Buffer)
          , @DataLength
          , @CompCode
          , @Reason
          );
{}
    fRspTime := now;
    if CompCode <> MQCC_OK then
    begin
      ErrorString := mqiRetErr ('MQGET');
      AllOK := False;
    end;
  end;
  CloseAll (xDisconnect);
  if AllOK then
    Result := mqiMqResult
  else
    Raise Exception.Create(ErrorString);
end;

procedure TMqInterface.mqiCheckUse;
begin
  if (Use <> mquServer)
  and (Use <> mquClient) then
    raise Exception.Create('Use of mqInterface not defined');
  if (Use = mquServer)
  and (not MqServerOK) then
    raise Exception.Create(Format ('MqServer DLL %s not loaded', [MQLOCALDLL]));
  if (Use = mquClient)
  and (not MqClientOK) then
    raise Exception.Create(Format ('MqClient DLL %s not loaded', [MQLOCALDLL]));
end;

{ TGetMsgOption }

constructor TGetMsgOptions.Create;
begin
  inherited Create;
  Options := MQGMO_ACCEPT_TRUNCATED_MSG
           + MQGMO_NO_SYNCPOINT
           + MQGMO_CONVERT
           + MQGMO_FAIL_IF_QUIESCING
           + MQGMO_WAIT
           ;
end;

function TGetMsgOptions.getAcceptTruncatedMsg: Boolean;
begin
  result := getOptionBoolean (MQGMO_ACCEPT_TRUNCATED_MSG);
end;

function TGetMsgOptions.getAllMsgsAvailable: Boolean;
begin
  result := getOptionBoolean (MQGMO_ALL_MSGS_AVAILABLE);
end;

function TGetMsgOptions.getAllSegmentsAvailable: Boolean;
begin
  result := getOptionBoolean (MQGMO_ALL_SEGMENTS_AVAILABLE);
end;

function TGetMsgOptions.getBrowseFirst: Boolean;
begin
  result := getOptionBoolean (MQGMO_BROWSE_FIRST);
end;

function TGetMsgOptions.getBrowseMsgUnderCursor: Boolean;
begin
  result := getOptionBoolean (MQGMO_BROWSE_MSG_UNDER_CURSOR);
end;

function TGetMsgOptions.getBrowseNext: Boolean;
begin
  result := getOptionBoolean (MQGMO_BROWSE_NEXT);
end;

function TGetMsgOptions.getCompleteMsg: Boolean;
begin
  result := getOptionBoolean (MQGMO_COMPLETE_MSG);
end;

function TGetMsgOptions.getConvert: Boolean;
begin
  result := getOptionBoolean (MQGMO_CONVERT);
end;

function TGetMsgOptions.getFailIfQuiescing: Boolean;
begin
  result := getOptionBoolean (MQGMO_FAIL_IF_QUIESCING);
end;

function TGetMsgOptions.getLock: Boolean;
begin
  result := getOptionBoolean (MQGMO_LOCK);
end;

function TGetMsgOptions.getLogicalOrder: Boolean;
begin
  result := getOptionBoolean (MQGMO_LOGICAL_ORDER);
end;

function TGetMsgOptions.getMsgUnderCursor: Boolean;
begin
  result := getOptionBoolean (MQGMO_MSG_UNDER_CURSOR);
end;

function TGetMsgOptions.getNoSyncPoint: Boolean;
begin
  result := getOptionBoolean (MQGMO_NO_SYNCPOINT);
end;

function TGetMsgOptions.getMarkSkipBackout: Boolean;
begin
  result := getOptionBoolean (MQGMO_MARK_SKIP_BACKOUT);
end;

function TGetMsgOptions.getSetSignal: Boolean;
begin
  result := getOptionBoolean (MQGMO_SET_SIGNAL);
end;

function TGetMsgOptions.getSyncPoint: Boolean;
begin
  result := getOptionBoolean (MQGMO_SYNCPOINT);
end;

function TGetMsgOptions.getUnlock: Boolean;
begin
  result := getOptionBoolean (MQGMO_UNLOCK);
end;

function TGetMsgOptions.getWait: Boolean;
begin
  result := getOptionBoolean (MQGMO_WAIT);
end;

procedure TGetMsgOptions.setAcceptTruncatedMsg(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_ACCEPT_TRUNCATED_MSG, Value);
end;

procedure TGetMsgOptions.setAllMsgsAvailable(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_ALL_MSGS_AVAILABLE, Value);
end;

procedure TGetMsgOptions.setAllSegmentsAvailable(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_ALL_SEGMENTS_AVAILABLE, Value);
end;

procedure TGetMsgOptions.setBrowseFirst(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_BROWSE_FIRST, Value);
end;

procedure TGetMsgOptions.setBrowseMsgUnderCursor(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_BROWSE_MSG_UNDER_CURSOR, Value);
end;

procedure TGetMsgOptions.setBrowseNext(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_BROWSE_NEXT, Value);
end;

procedure TGetMsgOptions.setCompleteMsg(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_COMPLETE_MSG, Value);
end;

procedure TGetMsgOptions.setConvert(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_CONVERT, Value);
end;

procedure TGetMsgOptions.setFailIfQuiescing(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_FAIL_IF_QUIESCING, Value);
end;

procedure TGetMsgOptions.setLock(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_LOCK, Value);
end;

procedure TGetMsgOptions.setLogicalOrder(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_LOGICAL_ORDER, Value);
end;

procedure TGetMsgOptions.setMsgUnderCursor(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_MSG_UNDER_CURSOR, Value);
end;

procedure TGetMsgOptions.setNoSyncPoint(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_NO_SYNCPOINT, Value);
end;

procedure TGetMsgOptions.setMarkSkipBackout(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_MARK_SKIP_BACKOUT, Value);
end;

procedure TGetMsgOptions.setSetSignal(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_SET_SIGNAL, Value);
end;

procedure TGetMsgOptions.setSyncPoint(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_SYNCPOINT, Value);
end;

procedure TGetMsgOptions.setUnlock(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_UNLOCK, Value);
end;

procedure TGetMsgOptions.setWait(const Value: Boolean);
begin
  setOptionBoolean (MQGMO_WAIT, Value);
end;

{ TMqInterface }

function TMqInterface.getCorrelId: String;
var
  x: Integer;
begin
  SetLength(result, 24);
  for x := 0 to 23 do
    result [x + 1] := Char (MsgDesc.CorrelId [x]);
end;

function TMqInterface.getMsgId: String;
var
  x: Integer;
begin
  SetLength(result, 24);
  for x := 0 to 23 do
    result [x + 1] := Char (MsgDesc.MsgId [x]);
end;

procedure TMqInterface.setCorrelId(const Value: String);
var
  x: Integer;
begin
  x := 1;
  while (x <= 24)
  and (x <= Length (Value)) do
  begin
    MsgDesc.CorrelId [x - 1] := Byte (Value [x]);
    Inc (x);
  end;
  while (x <= 24) do
  begin
    MsgDesc.CorrelId [x - 1] := 0;
    Inc (x);
  end;
end;

procedure TMqInterface.setMsgId(const Value: String);
var
  x: Integer;
begin
  x := 1;
  while (x <= 24)
  and (x <= Length (Value)) do
  begin
    MsgDesc.MsgId [x - 1] := Byte (Value [x]);
    Inc (x);
  end;
  while (x <= 24) do
  begin
    MsgDesc.MsgId [x - 1] := 0;
    Inc (x);
  end;
end;

procedure TMqInterface.CloseAll (aDisconnect: Boolean);
begin

//Close Get
  if OpenGetOK then
  begin
    if not mqiMqCloseGet then
    begin
      if AllOK then
        ErrorString := mqiRetErr ('MQCLOSE (Get)');
      AllOK := False;
    end;
  end;
  OpenGetOK := False;

//Close Put
  if OpenPutOK then
  begin
    if not mqiMqClosePut then
    begin
      if AllOK then
        ErrorString := mqiRetErr ('MQCLOSE (Put)');
      AllOK := False;
    end;
  end;
  OpenPutOK := False;


//Disconnect Qmanager
  if ConnOK
  and aDisconnect then
  begin
    if not mqiMqDisc then
    begin
      if AllOK then
        ErrorString := mqiRetErr ('MQDISC');
      AllOK := False;
    end;
  end;
  ConnOK := False;
end;

function TMqInterface.getdoReportCopyMsgIdToCorrelId: Boolean;
begin
  result := (not getdoReportPassMsgId)
        and (not getdoReportPassCorrelId);
end;

function TMqInterface.getdoReportPassMsgId: Boolean;
begin
  result := ((MsgDesc.Report and MQRO_PASS_MSG_ID) > 0);
end;

function TMqInterface.getdoReportPassCorrelId: Boolean;
begin
  result := ((MsgDesc.Report and MQRO_PASS_CORREL_ID) > 0);
end;

procedure TMqInterface.setReplyToQueue(const Value: String);
var
  x: Integer;
  xAnsiValue: AnsiString;
begin
  xAnsiValue := Value;
  x := 1;
  while (x <= 48)
  and (x <= Length (xAnsiValue)) do
  begin
    MsgDesc.ReplyToQ [x - 1] := AnsiChar (xAnsiValue [x]);
    Inc (x);
  end;
  while (x <= 48) do
  begin
    MsgDesc.ReplyToQ [x - 1] := Char (0);
    Inc (x);
  end;
end;

function TMqInterface.getReplyToQueue: String;
var
  x: Integer;
begin
  SetLength(result, 48);
  for x := 0 to 47 do
    result [x + 1] := Char (MsgDesc.ReplyToQ [x]);
end;

function TMqInterface.getReplyToQmanager: String;
var
  x: Integer;
begin
  SetLength(result, 48);
  for x := 0 to 47 do
    result [x + 1] := Char (MsgDesc.ReplyToQMgr [x]);
end;

procedure TMqInterface.setReplyToQmanager(const Value: String);
var
  x: Integer;
  xValue: AnsiString;
begin
  xValue := Value;
  x := 1;
  while (x <= 48)
  and (x <= Length (xValue)) do
  begin
    MsgDesc.ReplyToQMgr [x - 1] := AnsiChar (xValue [x]);
    Inc (x);
  end;
  while (x <= 48) do
  begin
    MsgDesc.ReplyToQMgr [x - 1] := AnsiChar (0);
    Inc (x);
  end;
end;

function TMqInterface.MsgDescFromXml(aXml: TXml): AnsiString;
var
  x, y: Integer;
begin
  result := '';
  if not Assigned (aXml) then Exit;
  if aXml.Name <> 'mqHeader' then raise Exception.Create('wrong XML for Mq.MsgDescFromXML: ' + aXml.Name);
  if not aXml.Checked then Exit;
  for x := 0 to aXml.Items.Count - 1 do
  begin
    with aXml.Items.XmlItems[x] do
    begin
      if (Name = 'mqmd')
      and (Checked) then
      begin
        for y := 0 to Items.Count - 1 do
        begin
          with Items.XmlItems[y] do
          begin
            if Checked then
            begin
              if Name = 'StrucId' then CopyChars (MsgDesc.StrucId, Value, SizeOf (MsgDesc.StrucId));
              if Name = 'Version' then MsgDesc.Version := StrToIntDef (Value, 0);
              if Name = 'Report' then MsgDesc.Report := StrToIntDef (Value, 0);
              if Name = 'MsgType' then MsgDesc.MsgType := StrToIntDef (Value, 0);
              if Name = 'Expiry' then MsgDesc.Expiry := StrToIntDef (Value, 0);
              if Name = 'Feedback' then MsgDesc.Feedback := StrToIntDef (Value, 0);
              if Name = 'Encoding' then MsgDesc.Encoding := StrToIntDef (Value, 0);
              if Name = 'CodedCharSetId' then MsgDesc.CodedCharSetId := StrToIntDef (Value, 0);
              if Name = 'Format' then
              begin
                CopyChars (MsgDesc.Format, Value, SizeOf (MsgDesc.Format));
                if MsgDesc.ForMat = MQFMT_RF_HEADER_2 then
                begin
                  rfhHeader := mqRfh2HeaderFromXml (aXml.Items.XmlItemByTag['Rfh']);
                  result := rfhHeader;
                end;
              end;
              if Name = 'Priority' then MsgDesc.Priority := StrToIntDef (Value, 0);
              if Name = 'Persistence' then MsgDesc.Persistence := StrToIntDef (Value, 0);
        //    if Name = 'MsgId' then CopyChars (MsgDesc.MsgId, Value, SizeOf (MsgDesc.MsgId));
        //    if Name = 'CorrelId' then CopyChars (MsgDesc.CorrelId, Value, SizeOf (MsgDesc.CorrelId));
              if Name = 'BackoutCount' then MsgDesc.BackoutCount := StrToIntDef (Value, 0);
              if Name = 'ReplyToQ' then CopyChars (MsgDesc.ReplyToQ, Value, SizeOf (MsgDesc.ReplyToQ));
              if Name = 'ReplyToQMgr' then CopyChars (MsgDesc.ReplyToQMgr, Value, SizeOf (MsgDesc.ReplyToQMgr));
              if Name = 'UserIdentifier' then CopyChars (MsgDesc.UserIdentifier, Value, SizeOf (MsgDesc.UserIdentifier));
        //    if Name = 'AccountingToken' then CHAR32 (MsgDesc.AccountingToken) := Value;
              if Name = 'ApplIdentityData' then CopyChars (MsgDesc.ApplIdentityData, Value, SizeOf (MsgDesc.ApplIdentityData));
              if Name = 'PutApplType' then MsgDesc.PutApplType := StrToIntDef (Value, 0);
              if Name = 'PutApplName' then CopyChars (MsgDesc.PutApplName, Value, SizeOf (MsgDesc.PutApplName));
        //    if Name = 'PutDate' then CopyChars (MsgDesc.PutDate, Value, SizeOf (MsgDesc.PutDate));
        //    if Name = 'PutTime' then CopyChars (MsgDesc.PutTime, Value, SizeOf (MsgDesc.PutTime));
              if Name = 'ApplOriginData' then CopyChars (MsgDesc.ApplOriginData, Value, SizeOf (MsgDesc.ApplOriginData));
        //    if Name = 'GroupId' then CHAR24 (MsgDesc.GroupId) := Value;
              if Name = 'MsgSeqNumber' then MsgDesc.MsgSeqNumber := StrToIntDef (Value, 0);
              if Name = 'Offset' then MsgDesc.Offset := StrToIntDef (Value, 0);
              if Name = 'MsgFlags' then MsgDesc.MsgFlags := StrToIntDef (Value, 0);
              if Name = 'OriginalLength' then MsgDesc.OriginalLength := StrToIntDef (Value, 0);
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TMqInterface.MsgDescAsXml: TXml;
  function sn(ap: PMQBYTE; n: Integer): AnsiString;
  var
    x: Integer;
    xp: PAnsiChar;
  begin
    result := PAnsiChar(ap);
    exit;
    result := '';
    xp := PAnsiChar (ap);
    x := 1;
    while x <= n do
    begin
      if xp = AnsiChar(0) then
        exit;
      Result := Result + xp;
      Inc (x);
    end;
  end;
begin
  result := TXml.CreateAsString ('mqHeader', '');
  try
    with result do
    begin
      with AddXml (TXml.CreateAsString('mqmd', '')) do
      begin
        AddXml (TXml.CreateAsString('StrucId', MsgDesc.StrucId));
        AddXml (TXml.CreateAsString('Version', IntToStr (MsgDesc.Version)));
        AddXml (TXml.CreateAsString('Report', IntToStr (MsgDesc.Report)));
        AddXml (TXml.CreateAsString('MsgType', IntToStr (MsgDesc.MsgType)));
        AddXml (TXml.CreateAsString('Expiry', IntToStr (MsgDesc.Expiry)));
        AddXml (TXml.CreateAsString('Feedback', IntToStr (MsgDesc.Feedback)));
        AddXml (TXml.CreateAsString('Encoding', IntToStr (MsgDesc.Encoding)));
        AddXml (TXml.CreateAsString('CodedCharSetId', IntToStr (MsgDesc.CodedCharSetId)));
        AddXml (TXml.CreateAsString('Format', MsgDesc.Format));
        AddXml (TXml.CreateAsString('Priority', IntToStr (MsgDesc.Priority)));
        AddXml (TXml.CreateAsString('Persistence', IntToStr (MsgDesc.Persistence)));
        AddXml (TXml.CreateAsString('MsgId', sn (@MsgDesc.MsgId, 24)));
        AddXml (TXml.CreateAsString('CorrelId', sn (@MsgDesc.CorrelId, 24)));
        AddXml (TXml.CreateAsString('BackoutCount', IntToStr (MsgDesc.BackoutCount)));
        AddXml (TXml.CreateAsString('ReplyToQ', MsgDesc.ReplyToQ));
        AddXml (TXml.CreateAsString('ReplyToQMgr', MsgDesc.ReplyToQMgr));
        AddXml (TXml.CreateAsString('UserIdentifier', MsgDesc.UserIdentifier));
        AddXml (TXml.CreateAsString('AccountingToken', sn (@MsgDesc.AccountingToken, 32)));
        AddXml (TXml.CreateAsString('ApplIdentityData', MsgDesc.ApplIdentityData));
        AddXml (TXml.CreateAsString('PutApplType', IntToStr (MsgDesc.PutApplType)));
        AddXml (TXml.CreateAsString('PutApplName', MsgDesc.PutApplName));
        AddXml (TXml.CreateAsString('PutDate', MsgDesc.PutDate));
        AddXml (TXml.CreateAsString('PutTime', MsgDesc.PutTime));
        AddXml (TXml.CreateAsString('ApplOriginData', MsgDesc.ApplOriginData));
        AddXml (TXml.CreateAsString('GroupId', sn (@MsgDesc.GroupId, 24)));
        AddXml (TXml.CreateAsString('MsgSeqNumber', IntToStr (MsgDesc.MsgSeqNumber)));
        AddXml (TXml.CreateAsString('Offset', IntToStr (MsgDesc.Offset)));
        AddXml (TXml.CreateAsString('MsgFlags', IntToStr (MsgDesc.MsgFlags)));
        AddXml (TXml.CreateAsString('OriginalLength', IntToStr (MsgDesc.OriginalLength)));
      end;
      if (MsgDesc.Format = MQFMT_RF_HEADER)
      or (MsgDesc.Format = MQFMT_RF_HEADER_2) then
        try
          AddXml (mqRfh2HeaderAsXml (RfhHeader));
        except
          on e: exception do
            try
              AddXml (TXml.CreateAsString ('RfhHeaderAsText', mqRfh2HeaderAsTxt(RfhHeader)));
            except
              AddXml (TXml.CreateAsString ('RfhHeader', RfhHeader));
            end;
        end;
      CheckDownline (True);
    end;
  except
    result.Free;
    result := nil;
    raise;
  end;
end;

function TMqInterface.MsgDescAsText: String;
var
  s: String;
begin
  with MsgDescAsXml do
  begin
    result := AsText (False, 0, False, False);
    free;
  end;
end;

procedure TMqInterface.Browse(OnHaveMessage: TOnHaveMqMessage; OnNewThread: TOnNewThread; doWait, doRemove: Boolean);
var
  xDisconnect: Boolean;
  xBody, xHeader, xMqReturnCode: String;
begin
  DoTerminate := False;
  mqiCheckUse;
  allOK := True;
  ConnOK := False;
  OpenGetOK := False;

  fMQGMOGetOnly.MatchOptions.MatchCorrelId := False;
  fMQGMOGetOnly.MatchOptions.MatchMsgId := False;
  Buffer := 'This is a fake response';
  DataLength := Length (Buffer); // and a fake datalength
  ResponseMicroSeconds := 100; // and a fake responsetime
  try
    xDisconnect := false; // to avoid compiler warnings
    ConnOK := mqiMqConn (xDisconnect);
    if not ConnOK then
      Raise Exception.Create (mqiRetErr('MQCONN'));

  // open Get queue
    if AllOK then
    begin
      OpenGetOK := mqiMqOpenGet (not doRemove);
      if not OpenGetOk then
      begin
        ErrorString := mqiRetErr ('MQOPEN (Get)');
        CloseAll(xDisconnect);
        Raise Exception.Create (ErrorString);
      end;
    end;

  //Get
    if AllOK then
    begin
      MsgDesc := MQMD_DEFAULT;
      GetMsgOptions := MQGMO_BROWSE;
      if doRemove then
        GetMsgOptions.Options := GetMsgOptions.Options - MQGMO_BROWSE_NEXT;
      if doWait then
      begin
        GetMsgOptions.WaitInterval := 1000;
        GetMsgOptions.Options := GetMsgOptions.Options + MQGMO_WAIT;
      end;
  {
      GetMsgOptions.Options := MQGMOGetOnly.Options.Options;
      GetMsgOptions.MatchOptions := MQGMOGetOnly.MatchOptions.Options;
      GetMsgOptions.WaitInterval := 0;
  }
      SetLength (Buffer, 1024*1024);
      BufferLength := 1024*1024;
      xMqReturnCode := '';
  {}
      MQGET ( Hconn
            , HGetObj
            , @MsgDesc
            , @GetMsgOptions
            , BufferLength
            , PAnsiChar (Buffer)
            , @DataLength
            , @CompCode
            , @Reason
            );
      if (CompCode <> MQCC_OK)
      and (Reason <> MQRC_NO_MSG_AVAILABLE) then
        xMqReturnCode := mqiRetErr ('MQGET (Get)');
  {}
      while (not doTerminate)
      and (   (CompCode = MQCC_OK)
           or (CompCode = MQCC_WARNING)
           or (    doWait
               and (CompCode = MQCC_FAILED)
               and (Reason = MQRC_NO_MSG_AVAILABLE)
              )
          )
      do begin
        if (CompCode <> MQCC_FAILED) then
        begin
{
          if doRemove then
            try GetMsgUnderCursor; except end;
{}
          xBody := mqiMqResult;
          with MsgDescAsXml do
          begin
            xHeader := AsText (False, 0, False, False);
            free;
          end;
        end;
        if Reason <> MQRC_NO_MSG_AVAILABLE then
        begin
  //      OnHaveMessage (Self, xHeader, xBody, MsgDesc.MsgType, MsgDesc, returncode);
          if Assigned (OnNewThread) then
            OnNewThread (Self);
          TOnHaveMessageThread.Create ( OnHaveMessage
                                      , Self
                                      , xHeader
                                      , xBody
                                      , rfhHeader
                                      , MsgDesc.MsgType
                                      , MsgDesc
                                      , xMqReturnCode
                                      );
        end;
        MsgDesc := MQMD_DEFAULT;
        SetLength (Buffer, 1024*1024);
        BufferLength := 1024*1024;
        xMqReturnCode := '';
        MQGET ( Hconn
              , HGetObj
              , @MsgDesc
              , @GetMsgOptions
              , BufferLength
              , PAnsiChar (Buffer)
              , @DataLength
              , @CompCode
              , @Reason
              );
        if (CompCode <> MQCC_OK)
        and (Reason <> MQRC_NO_MSG_AVAILABLE) then
          xMqReturnCode := mqiRetErr ('MQGET (Get)');
      end;
    end;
    CloseAll (xDisconnect);
  finally
  end;
end;

procedure TMqInterface.GetMsgUnderCursor;
var
  xMsgDesc : MQMD;
  xGetMsgOptions : MQGMO;
  xBuffer: String;
  l: Integer;
begin
  xMsgDesc := MQMD_DEFAULT;
  xGetMsgOptions := MQGMO_DEFAULT;
  xGetMsgOptions.Options := xGetMsgOptions.Options + MQGMO_MSG_UNDER_CURSOR;
  SetLength (xBuffer, 1024*1024);
  l := 1024*1024;
  MQGET ( Hconn
        , HGetObj
        , @xMsgDesc
        , @xGetMsgOptions
        , l
        , PAnsiChar (xBuffer)
        , @DataLength
        , @CompCode
        , @Reason
        );
  if (CompCode <> MQCC_OK) then
    Raise Exception.Create (mqiRetErr('GetMsgUnderCursor'));
end;

function TMqInterface._PutReply(aInterface: TMqInterface; aMessage: String;
  aRfhHeader: AnsiString; aReqMsgDesc: MQMD): String;
var
  xDisconnect: Boolean;
  xMessage: AnsiString;
  xQManage, xQName: String;
begin
  result := '';
  if aInterface.useReplyToQmanager then
    xQmanager := aReqMsgDesc.ReplyToQMgr
  else
    xQmanager := aInterface.overwriteReplyToQManager;
  if aInterface.UseReplyToQueue then
    xQName := aReqMsgDesc.ReplyToQ
  else
    xQName := aInterface.overwriteReplyToQueue;
  allOK := True;
  if (xQManager <> Qmanager)
  or (xQName <> PutQueue)
  or True
  then begin
    CloseAll (True);
    PutQueue := xQName;
    QManager := xQManager;
    xDisconnect := false; // to avoid compiler warning
    ConnOK := mqiMqConn (xDisconnect);
    if not ConnOK then
      Raise Exception.Create (mqiRetErr('MQCONN'));
    if AllOK then
    begin
      OpenPutOK := mqiMqOpenPut;
      if not OpenPutOk then
      begin
        ErrorString := mqiRetErr ('MQOPEN (Put)');
        AllOK := False;
      end;
    end;
  end;

  if AllOK then
  begin
    MsgDesc := MQMD_DEFAULT;
    if ((aReqMsgDesc.Report and MQRO_PASS_MSG_ID) > 0) then
      MsgDesc.MsgId := aReqMsgDesc.MsgId
    else
      if ((aReqMsgDesc.Report and MQRO_PASS_CORREL_ID) > 0) then
        MsgDesc.CorrelId := aReqMsgDesc.CorrelId
      else
        MsgDesc.CorrelId := aReqMsgDesc.MsgId;
    MsgDesc.Format := aReqMsgDesc.Format;
    MsgDesc.MsgType := MQMT_REPLY;
    MsgDesc.Report := MQRO_NONE;
    MsgDesc.Expiry := StrToInt (xExpiryTime) * 10;
    if MsgDesc.Expiry = -10 then
      MsgDesc.Expiry := -1;
    xMessage := aRfhHeader + aMessage;
    PutMsgOptions := MQPMO_DEFAULT;
    PutMsgOptions.Options := fMQPMOFireAndForget.Options.Options;
{}
    MQPUT ( Hconn
          , HPutobj
          , @MsgDesc
          , @PutMsgOptions
          , Length (xMessage)
          , PAnsiChar (xMessage)
          , @Compcode
          , @Reason
          );
{}
    fReqTime := Now;
    fRspTime := fReqTime;
    if CompCode <> MQCC_OK then
    begin
      ErrorString := mqiRetErr ('MQPUT');
      AllOK := False;
    end;
  end;
  try result := MsgDescAsText except end;
  if not AllOK then
    Raise Exception.Create(ErrorString);
end;

function TMqInterface.PutReply(aMessage: String; aRfhHeader: AnsiString; aMsgDesc: MQMD): String;
var
  xi: TMqInterface;
begin
  xi := TMqInterface.Create;
  try
    xi.Use := Self.Use;
    xi.Expiry := '-1';
    result := xi._PutReply(Self,aMessage,aRfhHeader,aMsgDesc);
  finally
    xi.Free;
  end;
end;

function TMqInterface.getReversedEncoding: Boolean;
begin
  result := ((MsgDesc.Encoding and MQENC_INTEGER_NORMAL) = MQENC_INTEGER_NORMAL);
end;

function TMqInterface.MessageTimeStamp: TDateTime;
var
  sPutDate: String;
  sPutTime: String;
  yy, mm, dd, hh, mn, ss, pp: Word;
begin
  sPutDate := MsgDesc.PutDate;
  yy := StrToIntDef (Copy (sPutDate, 1, 4), 0);
  mm := StrToIntDef (Copy (sPutDate, 5, 2), 0);
  dd := StrToIntDef (Copy (sPutDate, 7, 2), 0);
  sPutTime := MsgDesc.PutTime;
  hh := StrToIntDef (Copy (sPutTime, 1, 2), 0);
  mn := StrToIntDef (Copy (sPutTime, 3, 2), 0);
  ss := StrToIntDef (Copy (sPutTime, 5, 2), 0);
  pp := StrToIntDef (Copy (sPutTime, 7, 2), 0);
{
  xPutTime := (  hh * 60 * 60 * 100
              +  mm      * 60 * 100
              +  ss           * 100
              +  pp
              )
            / (  24 * 60 * 60 * 100
              );
}
  result := EncodeDate(yy, mm, dd)
          + EncodeTime(hh, mn, ss, pp * 10)
          ;
end;

{ TMQOptions }

constructor TMQOptions.Create;
begin
  inherited Create;
end;

function TMQOptions.getOptionBoolean(const aOption: MQLONG): Boolean;
begin
  result := ((fOptions and aOption) > 0);
end;

procedure TMQOptions.setOptionBoolean(const aOption: MQLONG;
  aValue: Boolean);
var
  xValue: Boolean;
begin
  xValue := ((fOptions and aOption) > 0);
  if xValue = aValue then exit;
  if not aValue then
  begin
    fOptions := fOptions - aOption;
    exit;
  end;
  fOptions := fOptions + aOption;
end;

{ TMQGMO }

constructor TMQGMO.Create;
begin
  Options := TGetMsgOptions.Create;
  MatchOptions := TGetMsgMatchOptions.Create;
end;

{ TGetMsgMatchOption }

constructor TGetMsgMatchOptions.Create;
begin
end;

function TGetMsgMatchOptions.getMatchCorrelId: Boolean;
begin
  result := getOptionBoolean (MQMO_MATCH_CORREL_ID);
end;

function TGetMsgMatchOptions.getMatchGroupId: Boolean;
begin
  result := getOptionBoolean (MQMO_MATCH_GROUP_ID);
end;

function TGetMsgMatchOptions.getMatchMsgId: Boolean;
begin
  result := getOptionBoolean (MQMO_MATCH_MSG_ID);
end;

function TGetMsgMatchOptions.getMatchMsgSeqMumber: Boolean;
begin
  result := getOptionBoolean (MQMO_MATCH_MSG_SEQ_NUMBER);
end;

procedure TGetMsgMatchOptions.setMatchCorrelId(const Value: Boolean);
begin
  setOptionBoolean (MQMO_MATCH_CORREL_ID, Value);
end;

procedure TGetMsgMatchOptions.setMatchGroupId(const Value: Boolean);
begin
  setOptionBoolean (MQMO_MATCH_GROUP_ID, Value);
end;

procedure TGetMsgMatchOptions.setMatchMsgId(const Value: Boolean);
begin
  setOptionBoolean (MQMO_MATCH_MSG_ID, Value);
end;

procedure TGetMsgMatchOptions.setMatchMsgSeqMumber(const Value: Boolean);
begin
  setOptionBoolean (MQMO_MATCH_MSG_SEQ_NUMBER, Value);
end;

destructor TMQGMO.Destroy;
begin
  FreeAndNil (Options);
  FreeAndNil (MatchOptions);
end;

{ TMQPMO }

constructor TMQPMO.Create;
begin
  inherited Create;
  Options := TPutMsgOptions.Create;
end;

{ TPutMsgOption }

constructor TPutMsgOptions.Create;
begin
  inherited Create;
  Options := MQPMO_NO_SYNCPOINT
           + MQPMO_FAIL_IF_QUIESCING
           ;
end;

function TPutMsgOptions.getAlternateUserAuthority: Boolean;
begin
  result := getOptionBoolean (MQPMO_ALTERNATE_USER_AUTHORITY);
end;

function TPutMsgOptions.getDefaultContext: Boolean;
begin
  result := getOptionBoolean (MQPMO_DEFAULT_CONTEXT);
end;

function TPutMsgOptions.getFailIfQuiescing: Boolean;
begin
  result := getOptionBoolean (MQPMO_FAIL_IF_QUIESCING);
end;

function TPutMsgOptions.getLogicalOrder: Boolean;
begin
  result := getOptionBoolean (MQPMO_LOGICAL_ORDER);
end;

function TPutMsgOptions.getNewMsgId: Boolean;
begin
  result := getOptionBoolean (MQPMO_NEW_MSG_ID);
end;

function TPutMsgOptions.getNewCorrelId: Boolean;
begin
  result := getOptionBoolean (MQPMO_NEW_CORREL_ID);
end;

function TPutMsgOptions.getNoContext: Boolean;
begin
  result := getOptionBoolean (MQPMO_NO_CONTEXT);
end;

function TPutMsgOptions.getNoSyncPoint: Boolean;
begin
  result := getOptionBoolean (MQPMO_NO_SYNCPOINT);
end;

function TPutMsgOptions.getPassAllContext: Boolean;
begin
  result := getOptionBoolean (MQPMO_PASS_ALL_CONTEXT);
end;

function TPutMsgOptions.getPassIdentityContext: Boolean;
begin
  result := getOptionBoolean (MQPMO_PASS_IDENTITY_CONTEXT);
end;

function TPutMsgOptions.getSetAllContext: Boolean;
begin
  result := getOptionBoolean (MQPMO_SET_ALL_CONTEXT);
end;

function TPutMsgOptions.getSetIdentityContext: Boolean;
begin
  result := getOptionBoolean (MQPMO_SET_IDENTITY_CONTEXT);
end;

function TPutMsgOptions.getSyncPoint: Boolean;
begin
  result := getOptionBoolean (MQPMO_SYNCPOINT);
end;

procedure TPutMsgOptions.setAlternateUserAuthority(const Value: Boolean);
begin
  setOptionBoolean (MQPMO_ALTERNATE_USER_AUTHORITY, Value);
end;

procedure TPutMsgOptions.setDefaultContext(const Value: Boolean);
begin
  setOptionBoolean (MQPMO_DEFAULT_CONTEXT, Value);
end;

procedure TPutMsgOptions.setFailIfQuiescing(const Value: Boolean);
begin
  setOptionBoolean (MQPMO_FAIL_IF_QUIESCING, Value);
end;

procedure TPutMsgOptions.setLogicalOrder(const Value: Boolean);
begin
  setOptionBoolean (MQPMO_LOGICAL_ORDER , Value);
end;

procedure TPutMsgOptions.setNewCorrelId(const Value: Boolean);
begin
  setOptionBoolean (MQPMO_NEW_CORREL_ID , Value);
end;

procedure TPutMsgOptions.setNewMsgId(const Value: Boolean);
begin
  setOptionBoolean (MQPMO_NEW_MSG_ID , Value);
end;

procedure TPutMsgOptions.setNoContext(const Value: Boolean);
begin
  setOptionBoolean (MQPMO_NO_CONTEXT , Value);
end;

procedure TPutMsgOptions.setNoSyncPoint(const Value: Boolean);
begin
  setOptionBoolean (MQPMO_NO_SYNCPOINT , Value);
end;

procedure TPutMsgOptions.setPassAllContext(const Value: Boolean);
begin
  setOptionBoolean (MQPMO_PASS_ALL_CONTEXT , Value);
end;

procedure TPutMsgOptions.setPassIdentityContext(const Value: Boolean);
begin
  setOptionBoolean (MQPMO_PASS_IDENTITY_CONTEXT , Value);
end;

procedure TPutMsgOptions.setSetAllContext(const Value: Boolean);
begin
  setOptionBoolean (MQPMO_SET_ALL_CONTEXT , Value);
end;

procedure TPutMsgOptions.setSetIdentityContext(const Value: Boolean);
begin
  setOptionBoolean (MQPMO_SET_IDENTITY_CONTEXT , Value);
end;

procedure TPutMsgOptions.setSyncPoint(const Value: Boolean);
begin
  setOptionBoolean (MQPMO_SYNCPOINT , Value);
end;

destructor TMQPMO.Destroy;
begin
  Options.Free;
end;

{ TOnHaveMessageThread }

constructor TOnHaveMessageThread.Create ( aOnHaveMessage: TOnHaveMqMessage
                                        ; aSender: TObject
                                        ; aHeader, aBody, aRfhHeader: String
                                        ; aMsgType: MQLONG
                                        ; aMsgDesc: MQMD
                                        ; aMqReturnCode: String
                                        );
begin
  inherited Create (False);
  fOnHaveMessage := aOnHaveMessage;
  fSender := aSender;
  fHeader := aHeader;
  fBody := aBody;
  fRfhHeader := aRfhHeader;
  fMsgType := aMsgType;
  fMsgDesc := aMsgDesc;
  fMqReturnCode := aMqReturnCode;
  FreeOnTerminate := True;
end;

procedure TOnHaveMessageThread.Execute;
begin
  fOnHaveMessage (fSender, fHeader, fBody, fRfhHeader, fMsgType, fMsgDesc, fMqReturnCode);
end;

end.
