unit mqapi;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

//******************************************************************************
//IBM MQSeries message queuing Component
//*
//History:
//Version    Date         Author     Description
//==============================================================================
//1.0        04-09-2003   S Wloch    Initial revision.
//******************************************************************************

//NOTES...

//MQSeries products enable applications using message queuing to communicate
//across different platforms via client-server technology

interface

{$IFnDEF FPC}
uses
  Windows;
{$ELSE}
uses
  LCLIntf, LCLType, LMessages, DynLibs;
{$ENDIF}


resourcestring
  // Attributes
  S_MQCA_APPL_ID = 'Application ID';
  S_MQCA_BACKOUT_REQ_Q_NAME = 'Backout Req. queue name';
  S_MQCA_BASE_Q_NAME = 'Base queue name';
  S_MQCA_COMMAND_INPUT_Q_NAME = 'Command input queue name';
  S_MQCA_CREATION_DATE = 'Creation date';
  S_MQCA_CREATION_TIME = 'Creation time';
  S_MQCA_DEAD_LETTER_Q_NAME = 'Dead letter queue name';
  S_MQCA_DEF_XMIT_Q_NAME = 'Def. xmit queue name';
  S_MQCA_ENV_DATA = 'Environment data';
  S_MQCA_INITIATION_Q_NAME = 'Initiation queue name';
  S_MQCA_NAMES = 'Names';
  S_MQCA_PROCESS_DESC = 'Process description';
  S_MQCA_PROCESS_NAME = 'Process name';
  S_MQCA_Q_DESC = 'Queue description';
  S_MQCA_Q_MGR_DESC = 'Queue manager description';
  S_MQCA_Q_MGR_NAME = 'Queue manager name';
  S_MQCA_Q_NAME = 'Queue name';
  S_MQCA_REMOTE_Q_MGR_NAME = 'Remote queuemgr. name';
  S_MQCA_REMOTE_Q_NAME = 'Remote queue name';
  S_MQCA_STORAGE_CLASS = 'Storage class';
  S_MQCA_TRIGGER_DATA = 'Trigger data';
  S_MQCA_USER_DATA = 'User data';
  S_MQCA_XMIT_Q_NAME = 'Xmit queue name';

  S_MQIA_APPL_TYPE = 'Application type';
  S_MQIA_AUTHORITY_EVENT = 'Authority event';
  S_MQIA_BACKOUT_THRESHOLD = 'Backout threshold';
  S_MQIA_CODED_CHAR_SET_ID = 'Coded char set ID';
  S_MQIA_COMMAND_LEVEL = 'Command level';
  S_MQIA_CURRENT_Q_DEPTH = 'Current queue depth';
  S_MQIA_DEF_INPUT_OPEN_OPTION = 'Def. input open option';
  S_MQIA_DEF_PERSISTENCE = 'Def. persistence';
  S_MQIA_DEF_PRIORITY = 'Def. priority';
  S_MQIA_DEFINITION_TYPE = 'Def. type';
  S_MQIA_HARDEN_GET_BACKOUT = 'Harden get backout';
  S_MQIA_HIGH_Q_DEPTH = 'High queue depth';
  S_MQIA_INHIBIT_EVENT = 'Inhibit event';
  S_MQIA_INHIBIT_GET = 'Inhibit get';
  S_MQIA_INHIBIT_PUT = 'Inhibit put';
  S_MQIA_LOCAL_EVENT = 'Local event';
  S_MQIA_MAX_HANDLES = 'Max. handles';
  S_MQIA_MAX_MSG_LENGTH = 'Max. message length';
  S_MQIA_MAX_PRIORITY = 'Max. priority';
  S_MQIA_MAX_Q_DEPTH = 'Max. queue depth';
  S_MQIA_MAX_UNCOMMITTED_MSGS = 'Max. uncommitted messages';
  S_MQIA_MSG_DELIVERY_SEQUENCE = 'Msg. delivery sequence';
  S_MQIA_MSG_DEQ_COUNT = 'Msg. dequeue count';
  S_MQIA_MSG_ENQ_COUNT = 'Msg enqueue count';
  S_MQIA_NAME_COUNT = 'Name count';
  S_MQIA_OPEN_INPUT_COUNT = 'Open input count';
  S_MQIA_OPEN_OUTPUT_COUNT = 'Open output count';
  S_MQIA_PERFORMANCE_EVENT = 'Performance event';
  S_MQIA_PLATFORM = 'Platform';
  S_MQIA_Q_DEPTH_HIGH_EVENT = 'Queue depth high event';
  S_MQIA_Q_DEPTH_HIGH_LIMIT = 'Queue depth high limit';
  S_MQIA_Q_DEPTH_LOW_EVENT = 'Queue depth low event';
  S_MQIA_Q_DEPTH_LOW_LIMIT = 'Queue depth low limit';
  S_MQIA_Q_DEPTH_MAX_EVENT = 'Queue depth max. event';
  S_MQIA_Q_SERVICE_INTERVAL = 'Queue service interval';
  S_MQIA_Q_SERVICE_INTERVAL_EVENT = 'Queue service interval event';
  S_MQIA_Q_TYPE = 'Queue type';
  S_MQIA_REMOTE_EVENT = 'Remote event';
  S_MQIA_RETENTION_INTERVAL = 'Retention interval';
  S_MQIA_SCOPE = 'Scope';
  S_MQIA_SHAREABILITY = 'Shareability';
  S_MQIA_START_STOP_EVENT = 'Start stop event';
  S_MQIA_SYNCPOINT = 'Syncpoint';
  S_MQIA_TIME_SINCE_RESET = 'Time since reset';
  S_MQIA_TRIGGER_CONTROL = 'Trigger control';
  S_MQIA_TRIGGER_DEPTH = 'Trigger depth';
  S_MQIA_TRIGGER_INTERVAL = 'Trigger interval';
  S_MQIA_TRIGGER_MSG_PRIORITY = 'Trigger msg. priority';
  S_MQIA_TRIGGER_TYPE = 'Trigger type';
  S_MQIA_USAGE = 'Usage';
  S_MQIAV_NOT_APPLICABLE = 'Not applicable';

  S_MQIACF_Q_MGR_ATTRS = 'Queue manager attributes';
  S_MQIACF_Q_ATTRS = 'Queue attributes';
  S_MQIACF_PROCESS_ATTRS = 'Process attributes';
  S_MQIACF_FORCE = 'Force';
  S_MQIACF_REPLACE = 'Replace';
  S_MQIACF_PURGE = 'Purge';
  S_MQIACF_QUIESCE = 'Quiesce';
  S_MQIACF_ALL = 'All';
  S_MQIACF_PARAMETER_ID = 'Parameter ID';
  S_MQIACF_ERROR_ID = 'Error ID';
  S_MQIACF_SELECTOR = 'Selector';
  S_MQIACF_CHANNEL_ATTRS = 'Channel attributes';
  S_MQIACF_ESCAPE_TYPE = 'Escape type';
  S_MQIACF_ERROR_OFFSET = 'Error offset';
  S_MQIACF_REASON_QUALIFIER = 'Reason qualifier';
  S_MQIACF_COMMAND = 'Command';
  S_MQIACF_OPEN_OPTIONS = 'Open options';
  S_MQIACF_AUX_ERROR_DATA_INT_1 = 'Aux. error data int. 1';
  S_MQIACF_AUX_ERROR_DATA_INT_2 = 'Aux. error data int. 2';
  S_MQIACF_CONV_REASON_CODE = 'Conv. reason code';

  S_MQIACH_XMIT_PROTOCOL_TYPE = 'Xmit protocol type';
  S_MQIACH_BATCH_SIZE = 'Batch size';
  S_MQIACH_DISC_INTERVAL = 'Disconnect interval';
  S_MQIACH_SHORT_TIMER = 'Short timer';
  S_MQIACH_SHORT_RETRY = 'Short retry';
  S_MQIACH_LONG_TIMER = 'Long timer';
  S_MQIACH_LONG_RETRY = 'Long retry';
  S_MQIACH_PUT_AUTHORITY = 'Put authority';
  S_MQIACH_SEQUENCE_NUMBER_WRAP = 'Sequence number wrap';
  S_MQIACH_MAX_MSG_LENGTH = 'Max. message length';
  S_MQIACH_CHANNEL_TYPE = 'Channel type';
  S_MQIACH_DATA_COUNT = 'Data count';
  S_MQIACH_MSG_SEQUENCE_NUMBER = 'Message sequence number';
  S_MQIACH_DATA_CONVERSION = 'Data conversion';
  S_MQIACH_IN_DOUBT = 'In doubt';
  S_MQIACH_MCA_TYPE = 'MCA type';
  S_MQIACH_CHANNEL_INSTANCE_TYPE = 'Channel instance type';
  S_MQIACH_CHANNEL_INSTANCE_ATTRS = 'Channel instance attributes';
  S_MQIACH_CHANNEL_ERROR_DATA = 'Channel error data';
  S_MQIACH_CHANNEL_TABLE = 'Channel table';
  S_MQIACH_CHANNEL_STATUS = 'Channel status';
  S_MQIACH_INDOUBT_STATUS = 'Indoubt status';
  S_MQIACH_LAST_SEQUENCE_NUMBER = 'Last sequence number';
  S_MQIACH_CURRENT_MSGS = 'Current messages';
  S_MQIACH_CURRENT_SEQUENCE_NUMBER = 'Current sequence number';
  S_MQIACH_MSGS = 'Messages';
  S_MQIACH_BYTES_SENT = 'Bytes sent';
  S_MQIACH_BYTES_RECEIVED = 'Bytes received';
  S_MQIACH_BATCHES = 'Batches';
  S_MQIACH_BUFFERS_SENT = 'Buffers sent';
  S_MQIACH_BUFFERS_RECEIVED = 'Buffers received';
  S_MQIACH_LONG_RETRIES_LEFT = 'Long retries left';
  S_MQIACH_SHORT_RETRIES_LEFT = 'Short retries left';
  S_MQIACH_MCA_STATUS = 'MCA status';
  S_MQIACH_STOP_REQUESTED = 'Stop requested';
  S_MQIACH_MR_COUNT = 'MR count';
  S_MQIACH_MR_INTERVAL = 'MR interval';

  S_MQCACF_FROM_Q_NAME = 'From queue name';
  S_MQCACF_TO_Q_NAME = 'To queue name';
  S_MQCACF_FROM_PROCESS_NAME = 'From process name';
  S_MQCACF_TO_PROCESS_NAME = 'To process name';
  S_MQCACF_FROM_CHANNEL_NAME = 'From channel name';
  S_MQCACF_TO_CHANNEL_NAME = 'To channel name';
  S_MQCACF_Q_NAMES = 'Queue name';
  S_MQCACF_PROCESS_NAMES = 'Process names';
  S_MQCACF_ESCAPE_TEXT = 'Escape text';
  S_MQCACF_LOCAL_Q_NAMES = 'Local queue names';
  S_MQCACF_MODEL_Q_NAMES = 'Model queue names';
  S_MQCACF_ALIAS_Q_NAMES = 'Alias queue names';
  S_MQCACF_REMOTE_Q_NAMES = 'Remote queue names';
  S_MQCACF_SENDER_CHANNEL_NAMES = 'Sender channel names';
  S_MQCACF_SERVER_CHANNEL_NAMES = 'Server channel names';
  S_MQCACF_REQUESTER_CHANNEL_NAMES = 'Requester channel names';
  S_MQCACF_RECEIVER_CHANNEL_NAMES = 'Receiver channel names';
  S_MQCACF_OBJECT_Q_MGR_NAME = 'Object queue manager name';
  S_MQCACF_APPL_NAME = 'Application name';
  S_MQCACF_USER_IDENTIFIER = 'User identifier';
  S_MQCACF_AUX_ERROR_DATA_STR_1 = 'Aux. error data str. 1';
  S_MQCACF_AUX_ERROR_DATA_STR_2 = 'Aux. error data str. 2';
  S_MQCACF_AUX_ERROR_DATA_STR_3 = 'Aux. error data str. 3';

  S_MQCACH_CHANNEL_NAME = 'Channel name';
  S_MQCACH_DESC = 'Description';
  S_MQCACH_MODE_NAME = 'Mode name';
  S_MQCACH_TP_NAME = 'TP name';
  S_MQCACH_XMIT_Q_NAME = 'Xmit queue name';
  S_MQCACH_CONNECTION_NAME = 'Connection name';
  S_MQCACH_MCA_NAME = 'MCA name';
  S_MQCACH_SEC_EXIT_NAME = 'Security exit name';
  S_MQCACH_MSG_EXIT_NAME = 'Message exit name';
  S_MQCACH_SEND_EXIT_NAME = 'Send exit name';
  S_MQCACH_RCV_EXIT_NAME = 'Receive exit name';
  S_MQCACH_CHANNEL_NAMES = 'Channel names';
  S_MQCACH_SEC_EXIT_USER_DATA = 'Security exit user data';
  S_MQCACH_MSG_EXIT_USER_DATA = 'Message exit user data';
  S_MQCACH_SEND_EXIT_USER_DATA = 'Send exit user data';
  S_MQCACH_RCV_EXIT_USER_DATA = 'Receive exit user data';
  S_MQCACH_USER_ID = 'User ID';
  S_MQCACH_PASSWORD = 'Password';
  S_MQCACH_LAST_MSG_TIME = 'Last message time';
  S_MQCACH_LAST_MSG_DATE = 'Last message date';
  S_MQCACH_MCA_USER_ID = 'MCA user ID';
  S_MQCACH_CHANNEL_START_TIME = 'Channel start time';
  S_MQCACH_CHANNEL_START_DATE = 'Channel start date';
  S_MQCACH_MCA_JOB_NAME = 'MCA job name';
  S_MQCACH_LAST_LUWID = 'Last LUWID';
  S_MQCACH_CURRENT_LUWID = 'Current LUWID';
  S_MQCACH_FORMAT_NAME = 'Format name';
  S_MQCACH_MR_EXIT_NAME = 'MR exit name';
  S_MQCACH_MR_EXIT_USER_DATA = 'MR exit user data';

  // Values
  S_MQPL_MVS = 'MVS/ESA';
  S_MQPL_OS2 = 'OS2';
  S_MQPL_OS400 = 'OS/400';
  S_MQPL_UNIX = 'AIX / UNIX';
  S_MQPL_WINDOWS_NT = 'Windows NT';

  S_MQCMDL_LEVEL = 'Versie %d Release %d.%d';

  S_MQSP_AVAILABLE = 'Available';
  S_MQSP_NOT_AVAILABLE = 'Not available';

  S_MQEVR_DISABLED = 'Disabled';
  S_MQEVR_ENABLED = 'Enabled';

  S_MQQT_LOCAL = 'Local';
  S_MQQT_MODEL = 'Model';
  S_MQQT_ALIAS = 'Alias';
  S_MQQT_REMOTE = 'Remote';

  S_MQQA_PUT_ALLOWED = 'Put allowed';
  S_MQQA_PUT_INHIBITED = 'Put inhibited';

  S_MQPER_PERSISTENT = 'Msg. is persistent';
  S_MQPER_NOT_PERSISTENT = 'Msg. is not persistent';

  S_MQSCO_Q_MGR = 'Queue-manager scope';
  S_MQSCO_CELL = 'Cell scope';

  S_MQQA_GET_ALLOWED = 'Get allowed';
  S_MQQA_GET_INHIBITED = 'Get inhibited';

  S_MQQA_SHAREABLE = 'Shareable';
  S_MQQA_NOT_SHAREABLE = 'Not shareable';

  S_MQOO_INPUT_EXCLUSIVE = 'Exclusive';
  S_MQOO_INPUT_SHARED = 'Shared';

  S_MQQA_BACKOUT_HARDENED = 'Count remembered';
  S_MQQA_BACKOUT_NOT_HARDENED = 'Count may not be remembered';

  S_MQMDS_PRIORITY = 'Priority order';
  S_MQMDS_FIFO = 'FIFO order';

  S_MQQDT_PREDEFINED = 'Predefined permanent';
  S_MQQDT_PERMANENT_DYNAMIC = 'Dynamically permanent';
  S_MQQDT_TEMPORARY_DYNAMIC = 'Dynamically temporary';

  S_MQUS_NORMAL = 'Normal';
  S_MQUS_TRANSMISSION = 'Transmission';

  S_MQTC_OFF = 'Trigger messages not required';
  S_MQTC_ON = 'Trigger messages required';

  S_MQTT_NONE = 'No trigger';
  S_MQTT_FIRST = 'Queue depth from 0 to 1';
  S_MQTT_EVERY = 'Every message';
  S_MQTT_DEPTH = 'Depth threshold exceeded';

  S_MQQSIE_HIGH = 'Service Interval High';
  S_MQQSIE_OK = 'Service Interval OK';
  S_MQQSIE_NONE = 'No Service Interval events';

  S_MQOT_SAVED_CHANNEL = 'Saved status';
  S_MQOT_CURRENT_CHANNEL = 'Current status';

  S_MQCHIDS_NOT_INDOUBT = 'Not in-doubt';
  S_MQCHIDS_INDOUBT = 'In-doubt';

  S_MQCHS_BINDING = 'Binding';
  S_MQCHS_STARTING = 'Starting';
  S_MQCHS_RUNNING = 'Running';
  S_MQCHS_PAUSED = 'Paused';
  S_MQCHS_STOPPING = 'Stopping';
  S_MQCHS_RETRYING = 'Retrying';
  S_MQCHS_STOPPED = 'Stopped';
  S_MQCHS_REQUESTING = 'Requesting';

  S_MQMCAS_STOPPED = 'Mca agent stopped';
  S_MQMCAS_RUNNING = 'Mca agent running';

  S_MQCHSR_STOP_NOT_REQUESTED = 'No';
  S_MQCHSR_STOP_REQUESTED = 'Yes';

  S_MQCHT_SENDER = 'Sender';
  S_MQCHT_SERVER = 'Server';
  S_MQCHT_RECEIVER = 'Receiver';
  S_MQCHT_REQUESTER = 'Requester';
  S_MQCHT_SVRCONN = 'Svrconn';
  S_MQCHT_CLNTCONN = 'Clntconn';

  S_MQXPT_LU62 = 'LU 6.2';
  S_MQXPT_TCP = 'TCP/IP';
  S_MQXPT_NETBIOS = 'NetBIOS';

  S_MQMCAT_PROCESS = 'Process';
  S_MQMCAT_THREAD = 'Thread';

  S_MQCDC_NO_SENDER_CONVERSION = 'No conversion';
  S_MQCDC_SENDER_CONVERSION = 'Conversion';

  S_MQPA_DEFAULT = 'Default user';
  S_MQPA_CONTEXT = 'Context user';

  S_MQAT_OS400 = 'OS/400';
  S_MQAT_OS2 = 'OS/2';
  S_MQAT_WINDOWS_NT = 'Windows NT';
  S_MQAT_DOS = 'Dos';
  S_MQAT_WINDOWS = 'Windows 3.1';
  S_MQAT_UNIX = 'AIX/Unix';
  S_MQAT_CICS = 'Cics';

  S_MQFC_YES = 'Yes';
  S_MQFC_NO = 'No';

  S_MQRP_YES = 'Yes';
  S_MQRP_NO = 'No';

type
  FARPROC = Pointer;
  //* Byte Datatypes */
  MQBYTE = Byte;
  PMQBYTE = ^MQBYTE;
  MQBYTE16 = array[0..15] of MQBYTE;
  PMQBYTE16 = ^MQBYTE16;
  MQBYTE24 = array[0..23] of MQBYTE;
  PMQBYTE24 = ^MQBYTE24;
  MQBYTE32 = array[0..31] of MQBYTE;
  PMQBYTE32 = ^MQBYTE32;
  MQBYTE40 = array[0..39] of MQBYTE;
  PMQBYTE40 = ^MQBYTE40;

  //* Character Datatypes */
  MQCHAR = AnsiChar;
  PMQCHAR = PAnsiChar;
  MQCHAR4 = array[0..3] of MQCHAR;
  PMQCHAR4 = ^MQCHAR4;
  MQCHAR8 = array[0..7] of MQCHAR;
  PMQCHAR8 = ^MQCHAR8;
  MQCHAR12 = array[0..11] of MQCHAR;
  PMQCHAR12 = ^MQCHAR12;
  MQCHAR28 = array[0..27] of MQCHAR;
  PMQCHAR28 = ^MQCHAR28;
  MQCHAR32 = array[0..31] of MQCHAR;
  PMQCHAR32 = ^MQCHAR32;
  MQCHAR48 = array[0..47] of MQCHAR;
  PMQCHAR48 = ^MQCHAR48;
  MQCHAR64 = array[0..63] of MQCHAR;
  PMQCHAR64 = ^MQCHAR64;
  MQCHAR128 = array[0..127] of MQCHAR;
  PMQCHAR128 = ^MQCHAR128;
  MQCHAR256 = array[0..255] of MQCHAR;
  PMQCHAR256 = ^MQCHAR256;
  MQCHARSTRING = array[0..256000] of MQCHAR;

  //* Other Datatypes */
  MQLONG = LongInt;
  PMQLONG = ^MQLONG;
  MQHCONN = MQLONG;
  PMQHCONN = ^MQHCONN;
  MQHOBJ = MQLONG;
  PMQHOBJ = ^MQHOBJ;
  MQPTR = Pointer;
  PMQPTR = ^MQPTR;
  PMQVOID = Pointer;
  PPMQVOID = ^PMQVOID;

type TMQCONNPROC = procedure (pName: PChar;
                              pHconn: PMQHCONN;
                              pCompCode: PMQLONG;
                              pReason: PMQLONG
                             ); cdecl;

type TMQDISCPROC = procedure(pHconn: PMQHCONN;
                             pCompCode: PMQLONG;
                             pReason  : PMQLONG
                            ); cdecl;

type TMQOPENPROC = procedure (Hconn: MQHCONN;
                              pObjDesc: PMQVOID;
                              Options: MQLONG;
                              pHobj: PMQHOBJ;
                              pCompCode: PMQLONG;
                              pReason: PMQLONG
                             ); cdecl;

type TMQCLOSEPROC = procedure (Hconn      : MQHCONN;
                               pHobj      : PMQHOBJ;
                               Options    : MQLONG;
                               pCompCode  : PMQLONG;
                               pReason    : PMQLONG
                              ); cdecl;

type TMQGETPROC = procedure (Hconn       : MQHCONN;
                             Hobj        : MQHOBJ;
                             pMsgDesc    : PMQVOID;
                             pGetMsgOpts : PMQVOID;
                             BufferLength: MQLONG;
                             pBuffer     : PMQVOID;
                             pDataLength : PMQLONG;
                             pCompCode   : PMQLONG;
                             pReason     : PMQLONG
                            ); cdecl;

type TMQPUTPROC = procedure (Hconn       : MQHCONN;
                             Hobj        : MQHOBJ;
                             pMsgDesc    : PMQVOID;
                             pPutMsgOpts : PMQVOID;
                             BufferLength: MQLONG;
                             pBuffer     : PMQVOID;
                             pCompCode   : PMQLONG;
                             pReason     : PMQLONG
                            ); cdecl;


type TMQPUT1PROC = procedure (Hconn       : MQHCONN;
                              pObjDesc    : PMQVOID;
                              pMsgDesc    : PMQVOID;
                              pPutMsgOpts : PMQVOID;
                              BufferLength: MQLONG;
                              pBuffer     : PMQVOID;
                              pCompCode   : PMQLONG;
                              pReason     : PMQLONG
                             ); cdecl;


type TMQBACKPROC = procedure (Hconn      : MQHCONN;
                              pCompCode  : PMQLONG;
                              pReason    : PMQLONG
                             ); cdecl;


type TMQCMITPROC = procedure (Hconn      : MQHCONN;
                              pCompCode  : PMQLONG;
                              pReason    : PMQLONG
                             ); cdecl;


type TMQINQPROC = procedure (Hconn       : MQHCONN;
                             Hobj        : MQHOBJ;
                             SelectorCount: MQLONG;
                             pSelectors  : PMQLONG;
                             IntAttrCount: MQLONG;
                             pIntAttrs   : PMQLONG;
                             CharAttrLength: MQLONG;
                             pCharAttrs  : PMQCHAR;
                             pCompCode   : PMQLONG;
                             pReason     : PMQLONG
                            ); cdecl;


type TMQSETPROC = procedure (Hconn       : MQHCONN;
                             Hobj        : MQHOBJ;
                             SelectorCount: MQLONG;
                             pSelectors  : PMQLONG;
                             IntAttrCount: MQLONG;
                             pIntAttrs   : PMQLONG;
                             CharAttrLength: MQLONG;
                             pCharAttrs  : PMQCHAR;
                             pCompCode   : PMQLONG;
                             pReason     : PMQLONG
                            ); cdecl;

const
  //* Structure Identifier */
  MQDLH_STRUC_ID = 'DLH ';

  //* Structure Identifier (array form) */
  MQDLH_STRUC_ID_ARRAY: MQCHAR4 = ('D','L','H',' ');

  //* Structure Version Number */
  MQDLH_VERSION_1 = 1;

  //* Structure Identifier */
  MQGMO_STRUC_ID = 'GMO ';

  //* Structure Identifier (array form) */
  MQGMO_STRUC_ID_ARRAY: MQCHAR4 = ('G','M','O',' ');

  //* Structure Version Number */
  MQGMO_VERSION_1 = 1;

  //* Get-Message Options */
  MQGMO_WAIT                      = $00000001;
  MQGMO_NO_WAIT                   = $00000000;
  MQGMO_SET_SIGNAL                = $00000008;
  MQGMO_FAIL_IF_QUIESCING         = $00002000;
  MQGMO_SYNCPOINT                 = $00000002;
  MQGMO_SYNCPOINT_IF_PERSISTENT   = $00001000;
  MQGMO_NO_SYNCPOINT              = $00000004;
  MQGMO_MARK_SKIP_BACKOUT         = $00000080;
  MQGMO_BROWSE_FIRST              = $00000010;
  MQGMO_BROWSE_NEXT               = $00000020;
  MQGMO_BROWSE_MSG_UNDER_CURSOR   = $00000800;
  MQGMO_MSG_UNDER_CURSOR          = $00000100;
  MQGMO_LOCK                      = $00000200;
  MQGMO_UNLOCK                    = $00000400;
  MQGMO_ACCEPT_TRUNCATED_MSG      = $00000040;
  MQGMO_CONVERT                   = $00004000;
  MQGMO_LOGICAL_ORDER             = $00008000;
  MQGMO_COMPLETE_MSG              = $00010000;
  MQGMO_ALL_MSGS_AVAILABLE        = $00020000;
  MQGMO_ALL_SEGMENTS_AVAILABLE    = $00040000;
  MQGMO_NONE                      = $00000000;

{Match Options}
  MQMO_MATCH_MSG_ID              = $00000001;
  MQMO_MATCH_CORREL_ID           = $00000002;
  MQMO_MATCH_GROUP_ID            = $00000004;
  MQMO_MATCH_MSG_SEQ_NUMBER      = $00000008;
  MQMO_MATCH_OFFSET              = $00000010;
  MQMO_MATCH_MSG_TOKEN           = $00000020;
  MQMO_NONE                      = $00000000;

{Group status}
  MQGS_NOT_IN_GROUP              = ' ';
  MQGS_MSG_IN_GROUP              = 'G';
  MQGS_LAST_MSG_IN_GROUP         = 'L';

{Segment status}
  MQSS_NOT_A_SEGMENT             = ' ';
  MQSS_SEGMENT                   = 'S';
  MQSS_LAST_SEGMENT              = 'L';

{Segmentation}
  MQSEG_INHIBITED                = ' ';
  MQSEG_ALLOWED                  = 'A';

{Returned length}
  MQRL_UNDEFINED                 = -1;

{Original length}
  MQOL_UNDEFINED                 = -1;
  //* Wait Interval */
  MQWI_UNLIMITED = -1;


//*********************************************************************/
//*  Values Related to MQMD Structure                                 */
//*********************************************************************/

  //* Structure Identifier */
  MQMD_STRUC_ID = 'MD  ';

  //* Structure Identifier (array form) */
  MQMD_STRUC_ID_ARRAY: MQCHAR4 = ('M','D',' ',' ');

  //* Structure Version Number */
  MQMD_VERSION_1 = 1;

  //* Report Options */
  MQRO_EXCEPTION                 = $01000000;
  MQRO_EXCEPTION_WITH_DATA       = $03000000;
  MQRO_EXCEPTION_WITH_FULL_DATA  = $07000000;
  MQRO_EXPIRATION                = $00200000;
  MQRO_EXPIRATION_WITH_DATA      = $00600000;
  MQRO_EXPIRATION_WITH_FULL_DATA = $00e00000;
  MQRO_COA                       = $00000100;
  MQRO_COA_WITH_DATA             = $00000300;
  MQRO_COA_WITH_FULL_DATA        = $00000700;
  MQRO_COD                       = $00000800;
  MQRO_COD_WITH_DATA             = $00001800;
  MQRO_COD_WITH_FULL_DATA        = $00003800;
  MQRO_NEW_MSG_ID                = $00000000;
  MQRO_PASS_MSG_ID               = $00000080;
  MQRO_COPY_MSG_ID_TO_CORREL_ID  = $00000000;
  MQRO_PASS_CORREL_ID            = $00000040;
  MQRO_DEAD_LETTER_Q             = $00000000;
  MQRO_DISCARD_MSG               = $08000000;
  MQRO_NONE                      = $00000000;

  //* Report Options Masks */
  MQRO_REJECT_UNSUP_MASK         = $101c0000;
  MQRO_ACCEPT_UNSUP_MASK         = $efe000ff;
  MQRO_ACCEPT_UNSUP_IF_XMIT_MASK = $0003ff00;

  //* Message Types */
  MQMT_SYSTEM_FIRST = 1;
  MQMT_REQUEST      = 1;
  MQMT_REPLY        = 2;
  MQMT_DATAGRAM     = 8;
  MQMT_REPORT       = 4;
  MQMT_SYSTEM_LAST  = 65535;
  MQMT_APPL_FIRST   = 65536;
  MQMT_APPL_LAST    = 999999999;

  //* Expiry */
  MQEI_UNLIMITED = -1;

  //* Feedback Values */
  MQFB_NONE                   = 0;
  MQFB_SYSTEM_FIRST           = 1;
  MQFB_EXPIRATION             = 258;
  MQFB_COA                    = 259;
  MQFB_COD                    = 260;
  MQFB_QUIT                   = 256;
  MQFB_CHANNEL_COMPLETED      = 262;
  MQFB_CHANNEL_FAIL_RETRY     = 263;
  MQFB_CHANNEL_FAIL           = 264;
  MQFB_APPL_CANNOT_BE_STARTED = 265;
  MQFB_TM_ERROR               = 266;
  MQFB_APPL_TYPE_ERROR        = 267;
  MQFB_STOPPED_BY_MSG_EXIT    = 268;
  MQFB_XMIT_Q_MSG_ERROR       = 271;
  MQFB_SYSTEM_LAST            = 65535;
  MQFB_APPL_FIRST             = 65536;
  MQFB_APPL_LAST              = 999999999;

  //* Encoding */
  MQENC_NATIVE = $00000222;

  //* Encoding Masks */
  MQENC_INTEGER_MASK  = $0000000f;
  MQENC_DECIMAL_MASK  = $000000f0;
  MQENC_FLOAT_MASK    = $00000f00;
  MQENC_RESERVED_MASK = $fffff000;

  //* Encodings for Binary Integers */
  MQENC_INTEGER_UNDEFINED = $00000000;
  MQENC_INTEGER_NORMAL    = $00000001;
  MQENC_INTEGER_REVERSED  = $00000002;

  //* Encodings for Packed-Decimal Integers */
  MQENC_DECIMAL_UNDEFINED = $00000000;
  MQENC_DECIMAL_NORMAL    = $00000010;
  MQENC_DECIMAL_REVERSED  = $00000020;

  //* Encodings for Floating-Point Numbers */
  MQENC_FLOAT_UNDEFINED     = $00000000;
  MQENC_FLOAT_IEEE_NORMAL   = $00000100;
  MQENC_FLOAT_IEEE_REVERSED = $00000200;
  MQENC_FLOAT_S390          = $00000300;

  //* Coded Character-Set Identifiers */
  MQCCSI_DEFAULT  = 0;
  MQCCSI_Q_MGR    = 0;
  MQCCSI_EMBEDDED = -1;

  //* Formats */
  MQFMT_NONE               = '        ';
  MQFMT_ADMIN              = 'MQADMIN ';
  MQFMT_CHANNEL_COMPLETED  = 'MQCHCOM ';
  MQFMT_CICS               = 'MQCICS  ';
  MQFMT_COMMAND_1          = 'MQCMD1  ';
  MQFMT_COMMAND_2          = 'MQCMD2  ';
  MQFMT_DEAD_LETTER_HEADER = 'MQDEAD  ';
  MQFMT_DIST_HEADER        = 'MQHDIST ';
  MQFMT_EMBEDDED_PCF       = 'MQHEPCF ';
  MQFMT_EVENT              = 'MQEVENT ';
  MQFMT_IMS                = 'MQIMS   ';
  MQFMT_IMS_VAR_STRING     = 'MQIMSVS ';
  MQFMT_MD_EXTENSION       = 'MQHMDE  ';
  MQFMT_PCF                = 'MQPCF   ';
  MQFMT_REF_MSG_HEADER     = 'MQHREF  ';
  MQFMT_RF_HEADER          = 'MQHRF   ';
  MQFMT_RF_HEADER_1        = 'MQHRF   ';
  MQFMT_RF_HEADER_2        = 'MQHRF2  ';
  MQFMT_STRING             = 'MQSTR   ';
  MQFMT_TRIGGER            = 'MQTRIG  ';
  MQFMT_WORK_INFO_HEADER   = 'MQHWIH  ';
  MQFMT_XMIT_Q_HEADER      = 'MQXMIT  ';

  MQRFH_STRUC_LENGTH_FIXED = 32;
  MQRFH_STRUC_LENGTH_FIXED_2 = 36;
  //* Formats (array form) */
  MQFMT_NONE_ARRAY               : MQCHAR8 = (' ',' ',' ',' ',' ',' ',' ',' ');
  MQFMT_ADMIN_ARRAY              : MQCHAR8 = ('M','Q','A','D','M','I','N',' ');
  MQFMT_CHANNEL_COMPLETED_ARRAY  : MQCHAR8 = ('M','Q','C','H','C','O','M',' ');
  MQFMT_COMMAND_1_ARRAY          : MQCHAR8 = ('M','Q','C','M','D','1',' ',' ');
  MQFMT_COMMAND_2_ARRAY          : MQCHAR8 = ('M','Q','C','M','D','2',' ',' ');
  MQFMT_DEAD_LETTER_HEADER_ARRAY : MQCHAR8 = ('M','Q','D','E','A','D',' ',' ');
  MQFMT_EVENT_ARRAY              : MQCHAR8 = ('M','Q','E','V','E','N','T',' ');
  MQFMT_PCF_ARRAY                : MQCHAR8 = ('M','Q','P','C','F',' ',' ',' ');
  MQFMT_STRING_ARRAY             : MQCHAR8 = ('M','Q','S','T','R',' ',' ',' ');
  MQFMT_TRIGGER_ARRAY            : MQCHAR8 = ('M','Q','T','R','I','G',' ',' ');
  MQFMT_XMIT_Q_HEADER_ARRAY      : MQCHAR8 = ('M','Q','X','M','I','T',' ',' ');

{Message Flags}
  MQMF_SEGMENTATION_INHIBITED     = $00000000;
  MQMF_SEGMENTATION_ALLOWED       = $00000001;
  MQMF_MSG_IN_GROUP               = $00000008;
  MQMF_LAST_MSG_IN_GROUP          = $00000010;
  MQMF_SEGMENT                    = $00000002;
  MQMF_LAST_SEGMENT               = $00000004;
  MQMF_NONE                       = $00000000;
  MQMF_REJECT_UNSUP_MASK          = $00000FFF;
  MQMF_ACCEPT_UNSUP_MASK          = $FFF00000;
  MQMF_ACCEPT_UNSUP_IF_XMIT_MASK  = $000FF000;

  //* Priority */
  MQPRI_PRIORITY_AS_Q_DEF = -1;

  //* Persistence Values */
  MQPER_PERSISTENT           = 1;
  MQPER_NOT_PERSISTENT       = 0;
  MQPER_PERSISTENCE_AS_Q_DEF = 2;

  //* Message Id */
 MQMI_NONE    : MQBYTE24  = ( $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
 MQCI_NONE    : MQBYTE24  = ( $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
 MQCT_NONE    : MQCHAR128 = ( #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0);
 MQACT_NONE   : MQBYTE32  = ( $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
 MQGI_NONE    : MQBYTE24  = ( $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
 MQSID_NONE   : MQBYTE40  = ( $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
 MQCFAC_NONE  : MQCHAR8   = ( #0,#0,#0,#0,#0,#0,#0,#0);
 MQMTOK_NONE  : MQBYTE16  = ( $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
 MQIAUT_NONE  : MQCHAR8   = '        ';

  //* Message Id (array form) */
  MQMI_NONE_ARRAY: MQBYTE24 = (0, 0, 0, 0, 0, 0, 0, 0,
                               0, 0, 0, 0, 0, 0, 0, 0,
                               0, 0, 0, 0, 0, 0, 0, 0);


  //* Correlation Id (array form) */
  MQCI_NONE_ARRAY: MQBYTE24 = (0, 0, 0, 0, 0, 0, 0, 0,
                               0, 0, 0, 0, 0, 0, 0, 0,
                               0, 0, 0, 0, 0, 0, 0, 0);


  //* Accounting Token (array form) */
  MQACT_NONE_ARRAY: MQBYTE32 = (0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0);

  //* Put Application Types */
  MQAT_UNKNOWN    = -1;
  MQAT_NO_CONTEXT = 0;
  MQAT_CICS       = 1;
  MQAT_MVS        = 2;
  MQAT_IMS        = 3;
  MQAT_OS2        = 4;
  MQAT_DOS        = 5;
  MQAT_AIX        = 6;
  MQAT_UNIX       = 6;
  MQAT_QMGR       = 7;
  MQAT_OS400      = 8;
  MQAT_WINDOWS    = 9;
  MQAT_CICS_VSE   = 10;
  MQAT_WINDOWS_NT = 11;
  MQAT_VMS        = 12;
  MQAT_GUARDIAN   = 13;
  MQAT_VOS        = 14;
  MQAT_DEFAULT    = 11;
  MQAT_USER_FIRST = 65536;
  MQAT_USER_LAST  = 999999999;

//*********************************************************************/
//*  Values Related to MQOD Structure                                 */
//*********************************************************************/

  //* Structure Identifier */
  MQOD_STRUC_ID = 'OD  ';

  //* Structure Identifier (array form) */
  MQOD_STRUC_ID_ARRAY: MQCHAR4 = ('O','D',' ',' ');

  //* Structure Version Number */
  MQOD_VERSION_1 = 1;

  //* Object Types */
  MQOT_Q       = 1;
  MQOT_PROCESS = 3;
  MQOT_Q_MGR   = 5;
  MQOT_CHANNEL = 6;

  //* Extended Object Types */
  MQOT_ALL               = 1001;
  MQOT_ALIAS_Q           = 1002;
  MQOT_MODEL_Q           = 1003;
  MQOT_LOCAL_Q           = 1004;
  MQOT_REMOTE_Q          = 1005;
  MQOT_SENDER_CHANNEL    = 1007;
  MQOT_SERVER_CHANNEL    = 1008;
  MQOT_REQUESTER_CHANNEL = 1009;
  MQOT_RECEIVER_CHANNEL  = 1010;
  MQOT_CURRENT_CHANNEL   = 1011;
  MQOT_SAVED_CHANNEL     = 1012;

//*********************************************************************/
//*  Values Related to MQPMO Structure                                */
//*********************************************************************/

  //* Structure Identifier */
  MQPMO_STRUC_ID = 'PMO ';

  //* Structure Identifier (array form) */
  MQPMO_STRUC_ID_ARRAY: MQCHAR4 = ('P','M','O',' ');

  //* Structure Version Number */
  MQPMO_VERSION_1 = 1;

  //* Put-Message Options */
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
  MQPMO_NONE                      = $00000000;

//*********************************************************************/
//*  Values Related to MQTM Structure                                 */
//*********************************************************************/

  //* Structure Identifier */
  MQTM_STRUC_ID = 'TM  ';

  //* Structure Identifier (array form) */
  MQTM_STRUC_ID_ARRAY: MQCHAR4 = ('T','M',' ',' ');

  //* Structure Version Number */
  MQTM_VERSION_1 = 1;

//*********************************************************************/
//*  Values Related to MQTMC2 Structure                               */
//*********************************************************************/

  //* Structure Identifier */
  MQTMC_STRUC_ID = 'TMC ';

  //* Structure Identifier (array form) */
  MQTMC_STRUC_ID_ARRAY: MQCHAR4 = ('T','M','C',' ');

  //* Structure Version Number */
  MQTMC_VERSION_1 = '   1';
  MQTMC_VERSION_2 = '   2';

  //* Structure Version Number (array form) */
  MQTMC_VERSION_1_ARRAY: MQCHAR4 = (' ',' ',' ','1');
  MQTMC_VERSION_2_ARRAY: MQCHAR4 = (' ',' ',' ','2');

//*********************************************************************/
//*  Values Related to MQXQH Structure                                */
//*********************************************************************/

  //* Structure Identifier */
  MQXQH_STRUC_ID = 'XQH ';

  //* Structure Identifier (array form) */
  MQXQH_STRUC_ID_ARRAY: MQCHAR4 = ('X','Q','H',' ');

  //* Structure Version Number */
  MQXQH_VERSION_1 = 1;

//*********************************************************************/
//*  Values Related to MQCLOSE Function                               */
//*********************************************************************/

  //* Object Handle */
  MQHO_UNUSABLE_HOBJ = -1;

  //* Close Options */
  MQCO_NONE         = 0;
  MQCO_DELETE       = 1;
  MQCO_DELETE_PURGE = 2;

//*********************************************************************/
//*  Values Related to MQINQ Function                                 */
//*********************************************************************/

  //* Character-Attribute Selectors */
  MQCA_APPL_ID              = 2001;
  MQCA_BACKOUT_REQ_Q_NAME   = 2019;
  MQCA_BASE_Q_NAME          = 2002;
  MQCA_COMMAND_INPUT_Q_NAME = 2003;
  MQCA_CREATION_DATE        = 2004;
  MQCA_CREATION_TIME        = 2005;
  MQCA_DEAD_LETTER_Q_NAME   = 2006;
  MQCA_DEF_XMIT_Q_NAME      = 2025;
  MQCA_ENV_DATA             = 2007;
  MQCA_FIRST                = 2001;
  MQCA_INITIATION_Q_NAME    = 2008;
  MQCA_LAST                 = 4000;
  MQCA_LAST_USED            = 2025;
  MQCA_NAMES                = 2020;
  MQCA_PROCESS_DESC         = 2011;
  MQCA_PROCESS_NAME         = 2012;
  MQCA_Q_DESC               = 2013;
  MQCA_Q_MGR_DESC           = 2014;
  MQCA_Q_MGR_NAME           = 2015;
  MQCA_Q_NAME               = 2016;
  MQCA_REMOTE_Q_MGR_NAME    = 2017;
  MQCA_REMOTE_Q_NAME        = 2018;
  MQCA_STORAGE_CLASS        = 2022;
  MQCA_TRIGGER_DATA         = 2023;
  MQCA_USER_DATA            = 2021;
  MQCA_XMIT_Q_NAME          = 2024;

  //* Integer-Attribute Selectors */
  MQIA_APPL_TYPE                = 1;
  MQIA_AUTHORITY_EVENT          = 47;
  MQIA_BACKOUT_THRESHOLD        = 22;
  MQIA_CODED_CHAR_SET_ID        = 2;
  MQIA_COMMAND_LEVEL            = 31;
  MQIA_CURRENT_Q_DEPTH          = 3;
  MQIA_DEF_INPUT_OPEN_OPTION    = 4;
  MQIA_DEF_PERSISTENCE          = 5;
  MQIA_DEF_PRIORITY             = 6;
  MQIA_DEFINITION_TYPE          = 7;
  MQIA_FIRST                    = 1;
  MQIA_HARDEN_GET_BACKOUT       = 8;
  MQIA_HIGH_Q_DEPTH             = 36;
  MQIA_INHIBIT_EVENT            = 48;
  MQIA_INHIBIT_GET              = 9;
  MQIA_INHIBIT_PUT              = 10;
  MQIA_LAST                     = 2000;
  MQIA_LAST_USED                = 54;
  MQIA_LOCAL_EVENT              = 49;
  MQIA_MAX_HANDLES              = 11;
  MQIA_MAX_MSG_LENGTH           = 13;
  MQIA_MAX_PRIORITY             = 14;
  MQIA_MAX_Q_DEPTH              = 15;
  MQIA_MAX_UNCOMMITTED_MSGS     = 33;
  MQIA_MSG_DELIVERY_SEQUENCE    = 16;
  MQIA_MSG_DEQ_COUNT            = 38;
  MQIA_MSG_ENQ_COUNT            = 37;
  MQIA_NAME_COUNT               = 19;
  MQIA_OPEN_INPUT_COUNT         = 17;
  MQIA_OPEN_OUTPUT_COUNT        = 18;
  MQIA_PERFORMANCE_EVENT        = 53;
  MQIA_PLATFORM                 = 32;
  MQIA_Q_DEPTH_HIGH_EVENT       = 43;
  MQIA_Q_DEPTH_HIGH_LIMIT       = 40;
  MQIA_Q_DEPTH_LOW_EVENT        = 44;
  MQIA_Q_DEPTH_LOW_LIMIT        = 41;
  MQIA_Q_DEPTH_MAX_EVENT        = 42;
  MQIA_Q_SERVICE_INTERVAL       = 54;
  MQIA_Q_SERVICE_INTERVAL_EVENT = 46;
  MQIA_Q_TYPE                   = 20;
  MQIA_REMOTE_EVENT             = 50;
  MQIA_RETENTION_INTERVAL       = 21;
  MQIA_SCOPE                    = 45;
  MQIA_SHAREABILITY             = 23;
  MQIA_START_STOP_EVENT         = 52;
  MQIA_SYNCPOINT                = 30;
  MQIA_TIME_SINCE_RESET         = 35;
  MQIA_TRIGGER_CONTROL          = 24;
  MQIA_TRIGGER_DEPTH            = 29;
  MQIA_TRIGGER_INTERVAL         = 25;
  MQIA_TRIGGER_MSG_PRIORITY     = 26;
  MQIA_TRIGGER_TYPE             = 28;
  MQIA_USAGE                    = 12;

  //* Integer Attribute Value Denoting "Not Applicable" */
  MQIAV_NOT_APPLICABLE = -1;

//*********************************************************************/
//*  Values Related to MQOPEN Function                                */
//*********************************************************************/

  //* Open Options */
  MQOO_INPUT_AS_Q_DEF           = 1;
  MQOO_INPUT_SHARED             = 2;
  MQOO_INPUT_EXCLUSIVE          = 4;
  MQOO_BROWSE                   = 8;
  MQOO_OUTPUT                   = 16;
  MQOO_INQUIRE                  = 32;
  MQOO_SET                      = 64;
  MQOO_SAVE_ALL_CONTEXT         = 128;
  MQOO_PASS_IDENTITY_CONTEXT    = 256;
  MQOO_PASS_ALL_CONTEXT         = 512;
  MQOO_SET_IDENTITY_CONTEXT     = 1024;
  MQOO_SET_ALL_CONTEXT          = 2048;
  MQOO_ALTERNATE_USER_AUTHORITY = 4096;
  MQOO_FAIL_IF_QUIESCING        = 8192;

  //* Connection Handle */
  MQHC_UNUSABLE_HCONN = -1;

  //* String Lengths */
  MQ_ACCOUNTING_TOKEN_LENGTH   = 32;
  MQ_APPL_IDENTITY_DATA_LENGTH = 32;
  MQ_APPL_NAME_LENGTH          = 28;
  MQ_APPL_ORIGIN_DATA_LENGTH   = 4;
  MQ_CHANNEL_DATE_LENGTH       = 12;
  MQ_CHANNEL_DESC_LENGTH       = 64;
  MQ_CHANNEL_NAME_LENGTH       = 20;
  MQ_CHANNEL_TIME_LENGTH       = 8;
  MQ_CONN_NAME_LENGTH          = 264;
  MQ_CORREL_ID_LENGTH          = 24;
  MQ_CREATION_DATE_LENGTH      = 12;
  MQ_CREATION_TIME_LENGTH      = 8;
  MQ_EXIT_DATA_LENGTH          = 32;
  MQ_EXIT_NAME_LENGTH          = 128;
  MQ_EXIT_USER_AREA_LENGTH     = 16;
  MQ_FORMAT_LENGTH             = 8;
  MQ_LUWID_LENGTH              = 16;
  MQ_MCA_JOB_NAME_LENGTH       = 28;
  MQ_MCA_NAME_LENGTH           = 20;
  MQ_MODE_NAME_LENGTH          = 8;
  MQ_MSG_HEADER_LENGTH         = 4000;
  MQ_MSG_ID_LENGTH             = 24;
  MQ_PASSWORD_LENGTH           = 12;
  MQ_PROCESS_APPL_ID_LENGTH    = 256;
  MQ_PROCESS_DESC_LENGTH       = 64;
  MQ_PROCESS_ENV_DATA_LENGTH   = 128;
  MQ_PROCESS_NAME_LENGTH       = 48;
  MQ_PROCESS_USER_DATA_LENGTH  = 128;
  MQ_PUT_APPL_NAME_LENGTH      = 28;
  MQ_PUT_DATE_LENGTH           = 8;
  MQ_PUT_TIME_LENGTH           = 8;
  MQ_Q_DESC_LENGTH             = 64;
  MQ_Q_NAME_LENGTH             = 48;
  MQ_Q_MGR_DESC_LENGTH         = 64;
  MQ_Q_MGR_NAME_LENGTH         = 48;
  MQ_SHORT_CONN_NAME_LENGTH    = 20;
  MQ_STORAGE_CLASS_LENGTH      = 8;
  MQ_TP_NAME_LENGTH            = 64;
  MQ_TRIGGER_DATA_LENGTH       = 64;
  MQ_USER_ID_LENGTH            = 12;

  //* Completion Codes */
  MQCC_OK      = 0;
  MQCC_WARNING = 1;
  MQCC_FAILED  = 2;
  MQCC_UNKNOWN = -1;

  //* Reason Codes */
  MQRC_NONE                      = 0;
  MQRC_ALIAS_BASE_Q_TYPE_ERROR   = 2001;
  MQRC_ALREADY_CONNECTED         = 2002;
  MQRC_BACKED_OUT                = 2003;
  MQRC_BUFFER_ERROR              = 2004;
  MQRC_BUFFER_LENGTH_ERROR       = 2005;
  MQRC_CHAR_ATTR_LENGTH_ERROR    = 2006;
  MQRC_CHAR_ATTRS_ERROR          = 2007;
  MQRC_CHAR_ATTRS_TOO_SHORT      = 2008;
  MQRC_CONNECTION_BROKEN         = 2009;
  MQRC_DATA_LENGTH_ERROR         = 2010;
  MQRC_DYNAMIC_Q_NAME_ERROR      = 2011;
  MQRC_ENVIRONMENT_ERROR         = 2012;
  MQRC_EXPIRY_ERROR              = 2013;
  MQRC_FEEDBACK_ERROR            = 2014;
  MQRC_GET_INHIBITED             = 2016;
  MQRC_HANDLE_NOT_AVAILABLE      = 2017;
  MQRC_HCONN_ERROR               = 2018;
  MQRC_HOBJ_ERROR                = 2019;
  MQRC_INHIBIT_VALUE_ERROR       = 2020;
  MQRC_INT_ATTR_COUNT_ERROR      = 2021;
  MQRC_INT_ATTR_COUNT_TOO_SMALL  = 2022;
  MQRC_INT_ATTRS_ARRAY_ERROR     = 2023;
  MQRC_SYNCPOINT_LIMIT_REACHED   = 2024;
  MQRC_MAX_CONNS_LIMIT_REACHED   = 2025;
  MQRC_MD_ERROR                  = 2026;
  MQRC_MISSING_REPLY_TO_Q        = 2027;
  MQRC_MSG_TYPE_ERROR            = 2029;
  MQRC_MSG_TOO_BIG_FOR_Q         = 2030;
  MQRC_MSG_TOO_BIG_FOR_Q_MGR     = 2031;
  MQRC_NO_MSG_AVAILABLE          = 2033;
  MQRC_NO_MSG_UNDER_CURSOR       = 2034;
  MQRC_NOT_AUTHORIZED            = 2035;
  MQRC_NOT_OPEN_FOR_BROWSE       = 2036;
  MQRC_NOT_OPEN_FOR_INPUT        = 2037;
  MQRC_NOT_OPEN_FOR_INQUIRE      = 2038;
  MQRC_NOT_OPEN_FOR_OUTPUT       = 2039;
  MQRC_NOT_OPEN_FOR_SET          = 2040;
  MQRC_OBJECT_CHANGED            = 2041;
  MQRC_OBJECT_IN_USE             = 2042;
  MQRC_OBJECT_TYPE_ERROR         = 2043;
  MQRC_OD_ERROR                  = 2044;
  MQRC_OPTION_NOT_VALID_FOR_TYPE = 2045;
  MQRC_OPTIONS_ERROR             = 2046;
  MQRC_PERSISTENCE_ERROR         = 2047;
  MQRC_PERSISTENT_NOT_ALLOWED    = 2048;
  MQRC_PRIORITY_EXCEEDS_MAXIMUM  = 2049;
  MQRC_PRIORITY_ERROR            = 2050;
  MQRC_PUT_INHIBITED             = 2051;
  MQRC_Q_DELETED                 = 2052;
  MQRC_Q_FULL                    = 2053;
  MQRC_Q_NOT_EMPTY               = 2055;
  MQRC_Q_SPACE_NOT_AVAILABLE     = 2056;
  MQRC_Q_TYPE_ERROR              = 2057;
  MQRC_Q_MGR_NAME_ERROR          = 2058;
  MQRC_Q_MGR_NOT_AVAILABLE       = 2059;
  MQRC_REPORT_OPTIONS_ERROR      = 2061;
  MQRC_SECOND_MARK_NOT_ALLOWED   = 2062;
  MQRC_SECURITY_ERROR            = 2063;
  MQRC_SELECTOR_COUNT_ERROR      = 2065;
  MQRC_SELECTOR_LIMIT_EXCEEDED   = 2066;
  MQRC_SELECTOR_ERROR            = 2067;
  MQRC_SELECTOR_NOT_FOR_TYPE     = 2068;
  MQRC_SIGNAL_OUTSTANDING        = 2069;
  MQRC_SIGNAL_REQUEST_ACCEPTED   = 2070;
  MQRC_STORAGE_NOT_AVAILABLE     = 2071;
  MQRC_SYNCPOINT_NOT_AVAILABLE   = 2072;
  MQRC_TRIGGER_CONTROL_ERROR     = 2075;
  MQRC_TRIGGER_DEPTH_ERROR       = 2076;
  MQRC_TRIGGER_MSG_PRIORITY_ERR  = 2077;
  MQRC_TRIGGER_TYPE_ERROR        = 2078;
  MQRC_TRUNCATED_MSG_ACCEPTED    = 2079;
  MQRC_TRUNCATED_MSG_FAILED      = 2080;
  MQRC_UNKNOWN_ALIAS_BASE_Q      = 2082;
  MQRC_UNKNOWN_OBJECT_NAME       = 2085;
  MQRC_UNKNOWN_OBJECT_Q_MGR      = 2086;
  MQRC_UNKNOWN_REMOTE_Q_MGR      = 2087;
  MQRC_WAIT_INTERVAL_ERROR       = 2090;
  MQRC_XMIT_Q_TYPE_ERROR         = 2091;
  MQRC_XMIT_Q_USAGE_ERROR        = 2092;
  MQRC_NOT_OPEN_FOR_PASS_ALL     = 2093;
  MQRC_NOT_OPEN_FOR_PASS_IDENT   = 2094;
  MQRC_NOT_OPEN_FOR_SET_ALL      = 2095;
  MQRC_NOT_OPEN_FOR_SET_IDENT    = 2096;
  MQRC_CONTEXT_HANDLE_ERROR      = 2097;
  MQRC_CONTEXT_NOT_AVAILABLE     = 2098;
  MQRC_SIGNAL1_ERROR             = 2099;
  MQRC_OBJECT_ALREADY_EXISTS     = 2100;
  MQRC_OBJECT_DAMAGED            = 2101;
  MQRC_RESOURCE_PROBLEM          = 2102;
  MQRC_ANOTHER_Q_MGR_CONNECTED   = 2103;
  MQRC_UNKNOWN_REPORT_OPTION     = 2104;
  MQRC_STORAGE_CLASS_ERROR       = 2105;
  MQRC_XWAIT_CANCELED            = 2107;
  MQRC_XWAIT_ERROR               = 2108;
  MQRC_SUPPRESSED_BY_EXIT        = 2109;
  MQRC_FORMAT_ERROR              = 2110;
  MQRC_SOURCE_CCSID_ERROR        = 2111;
  MQRC_SOURCE_INTEGER_ENC_ERROR  = 2112;
  MQRC_SOURCE_DECIMAL_ENC_ERROR  = 2113;
  MQRC_SOURCE_FLOAT_ENC_ERROR    = 2114;
  MQRC_TARGET_CCSID_ERROR        = 2115;
  MQRC_TARGET_INTEGER_ENC_ERROR  = 2116;
  MQRC_TARGET_DECIMAL_ENC_ERROR  = 2117;
  MQRC_TARGET_FLOAT_ENC_ERROR    = 2118;
  MQRC_NOT_CONVERTED             = 2119;
  MQRC_CONVERTED_MSG_TOO_BIG     = 2120;
  MQRC_ADAPTER_STORAGE_SHORTAGE  = 2127;
  MQRC_ADAPTER_CONN_LOAD_ERROR   = 2129;
  MQRC_ADAPTER_SERV_LOAD_ERROR   = 2130;
  MQRC_ADAPTER_DEFS_ERROR        = 2131;
  MQRC_ADAPTER_DEFS_LOAD_ERROR   = 2132;
  MQRC_ADAPTER_DISC_LOAD_ERROR   = 2138;
  MQRC_CICS_WAIT_FAILED          = 2140;
  MQRC_DLH_ERROR                 = 2141;
  MQRC_HEADER_ERROR              = 2142;
  MQRC_SOURCE_LENGTH_ERROR       = 2143;
  MQRC_TARGET_LENGTH_ERROR       = 2144;
  MQRC_SOURCE_BUFFER_ERROR       = 2145;
  MQRC_TARGET_BUFFER_ERROR       = 2146;
  MQRC_DBCS_ERROR                = 2150;
  MQRC_TRUNCATED                 = 2151;
  MQRC_ASID_MISMATCH             = 2157;
  MQRC_CONN_ID_IN_USE            = 2160;
  MQRC_Q_MGR_QUIESCING           = 2161;
  MQRC_Q_MGR_STOPPING            = 2162;
  MQRC_DUPLICATE_RECOV_COORD     = 2163;
  MQRC_PMO_ERROR                 = 2173;
  MQRC_API_EXIT_NOT_FOUND        = 2182;
  MQRC_API_EXIT_LOAD_ERROR       = 2183;
  MQRC_REMOTE_Q_NAME_ERROR       = 2184;
  MQRC_GMO_ERROR                 = 2186;
  MQRC_PAGESET_FULL              = 2192;
  MQRC_PAGESET_ERROR             = 2193;
  MQRC_NAME_NOT_VALID_FOR_TYPE   = 2194;
  MQRC_UNEXPECTED_ERROR          = 2195;
  MQRC_UNKNOWN_XMIT_Q            = 2196;
  MQRC_UNKNOWN_DEF_XMIT_Q        = 2197;
  MQRC_DEF_XMIT_Q_TYPE_ERROR     = 2198;
  MQRC_DEF_XMIT_Q_USAGE_ERROR    = 2199;
  MQRC_NAME_IN_USE               = 2201;
  MQRC_CONNECTION_QUIESCING      = 2202;
  MQRC_CONNECTION_STOPPING       = 2203;
  MQRC_ADAPTER_NOT_AVAILABLE     = 2204;
  MQRC_MSG_ID_ERROR              = 2206;
  MQRC_CORREL_ID_ERROR           = 2207;
  MQRC_FILE_SYSTEM_ERROR         = 2208;
  MQRC_NO_MSG_LOCKED             = 2209;
  MQRC_FILE_NOT_AUDITED          = 2216;
  MQRC_CONNECTION_NOT_AUTHORIZED = 2217;
  MQRC_MSG_TOO_BIG_FOR_CHANNEL   = 2218;
  MQRC_CALL_IN_PROGRESS          = 2219;
  MQRC_Q_MGR_ACTIVE              = 2222;
  MQRC_Q_MGR_NOT_ACTIVE          = 2223;
  MQRC_Q_DEPTH_HIGH              = 2224;
  MQRC_Q_DEPTH_LOW               = 2225;
  MQRC_Q_SERVICE_INTERVAL_HIGH   = 2226;
  MQRC_Q_SERVICE_INTERVAL_OK     = 2227;
  MQRC_HCONFIG_ERROR             = 2280;
  MQRC_FUNCTION_ERROR            = 2281;
  MQRC_CHANNEL_STARTED           = 2282;
  MQRC_CHANNEL_STOPPED           = 2283;
  MQRC_CHANNEL_CONV_ERROR        = 2284;
  MQRC_SERVICE_NOT_AVAILABLE     = 2285;
  MQRC_INITIALIZATION_FAILED     = 2286;
  MQRC_TERMINATION_FAILED        = 2287;
  MQRC_UNKNOWN_Q_NAME            = 2288;
  MQRC_SERVICE_ERROR             = 2289;
  MQRC_Q_ALREADY_EXISTS          = 2290;
  MQRC_USER_ID_NOT_AVAILABLE     = 2291;
  MQRC_UNKNOWN_ENTITY            = 2292;
  MQRC_UNKNOWN_AUTH_ENTITY       = 2293;
  MQRC_UNKNOWN_REF_OBJECT        = 2294;
  MQRC_CHANNEL_ACTIVATED         = 2295;
  MQRC_CHANNEL_NOT_ACTIVATED     = 2296;
  MQRC_UOW_CANCELED = 2297 ;
  MQRC_FUNCTION_NOT_SUPPORTED = 2298 ;
  MQRC_SELECTOR_TYPE_ERROR = 2299 ;
  MQRC_COMMAND_TYPE_ERROR = 2300 ;
  MQRC_MULTIPLE_INSTANCE_ERROR = 2301 ;
  MQRC_SYSTEM_ITEM_NOT_ALTERABLE = 2302 ;
  MQRC_BAG_CONVERSION_ERROR = 2303 ;
  MQRC_SELECTOR_OUT_OF_RANGE = 2304 ;
  MQRC_SELECTOR_NOT_UNIQUE = 2305 ;
  MQRC_INDEX_NOT_PRESENT = 2306 ;
  MQRC_STRING_ERROR = 2307 ;
  MQRC_ENCODING_NOT_SUPPORTED = 2308 ;
  MQRC_SELECTOR_NOT_PRESENT = 2309 ;
  MQRC_OUT_SELECTOR_ERROR = 2310 ;
  MQRC_STRING_TRUNCATED = 2311 ;
  MQRC_SELECTOR_WRONG_TYPE = 2312 ;
  MQRC_INCONSISTENT_ITEM_TYPE = 2313 ;
  MQRC_INDEX_ERROR = 2314 ;
  MQRC_SYSTEM_BAG_NOT_ALTERABLE = 2315 ;
  MQRC_ITEM_COUNT_ERROR = 2316 ;
  MQRC_FORMAT_NOT_SUPPORTED = 2317 ;
  MQRC_SELECTOR_NOT_SUPPORTED = 2318 ;
  MQRC_ITEM_VALUE_ERROR = 2319 ;
  MQRC_HBAG_ERROR = 2320 ;
  MQRC_PARAMETER_MISSING = 2321 ;
  MQRC_CMD_SERVER_NOT_AVAILABLE = 2322 ;
  MQRC_STRING_LENGTH_ERROR = 2323 ;
  MQRC_INQUIRY_COMMAND_ERROR = 2324 ;
  MQRC_NESTED_BAG_NOT_SUPPORTED = 2325 ;
  MQRC_BAG_WRONG_TYPE = 2326 ;
  MQRC_ITEM_TYPE_ERROR = 2327 ;
  MQRC_SYSTEM_BAG_NOT_DELETABLE = 2328 ;
  MQRC_SYSTEM_ITEM_NOT_DELETABLE = 2329 ;
  MQRC_CODED_CHAR_SET_ID_ERROR = 2330 ;
  MQRC_MSG_TOKEN_ERROR = 2331 ;
  MQRC_MISSING_WIH = 2332 ;
  MQRC_WIH_ERROR = 2333 ;
  MQRC_RFH_ERROR = 2334 ;
  MQRC_RFH_STRING_ERROR = 2335 ;
  MQRC_RFH_COMMAND_ERROR = 2336 ;
  MQRC_RFH_PARM_ERROR = 2337 ;
  MQRC_RFH_DUPLICATE_PARM = 2338 ;
  MQRC_RFH_PARM_MISSING = 2339 ;
  MQRC_CHAR_CONVERSION_ERROR = 2340 ;
  MQRC_UCS2_CONVERSION_ERROR = 2341 ;
  MQRC_DB2_NOT_AVAILABLE = 2342 ;
  MQRC_OBJECT_NOT_UNIQUE = 2343 ;
  MQRC_CONN_TAG_NOT_RELEASED = 2344 ;
  MQRC_CF_NOT_AVAILABLE = 2345 ;
  MQRC_CF_STRUC_IN_USE = 2346 ;
  MQRC_CF_STRUC_LIST_HDR_IN_USE = 2347 ;
  MQRC_CF_STRUC_AUTH_FAILED = 2348 ;
  MQRC_CF_STRUC_ERROR = 2349 ;
  MQRC_CONN_TAG_NOT_USABLE = 2350 ;
  MQRC_GLOBAL_UOW_CONFLICT = 2351 ;
  MQRC_LOCAL_UOW_CONFLICT = 2352 ;
  MQRC_HANDLE_IN_USE_FOR_UOW = 2353 ;
  MQRC_UOW_ENLISTMENT_ERROR = 2354 ;
  MQRC_UOW_MIX_NOT_SUPPORTED = 2355 ;
  MQRC_WXP_ERROR = 2356 ;
  MQRC_CURRENT_RECORD_ERROR = 2357 ;
  MQRC_NEXT_OFFSET_ERROR = 2358 ;
  MQRC_NO_RECORD_AVAILABLE = 2359 ;
  MQRC_OBJECT_LEVEL_INCOMPATIBLE = 2360 ;
  MQRC_NEXT_RECORD_ERROR = 2361 ;
  MQRC_BACKOUT_THRESHOLD_REACHED = 2362 ;
  MQRC_MSG_NOT_MATCHED = 2363 ;
  MQRC_JMS_FORMAT_ERROR = 2364 ;
  MQRC_SEGMENTS_NOT_SUPPORTED = 2365 ;
  MQRC_WRONG_CF_LEVEL = 2366 ;
  MQRC_CONFIG_CREATE_OBJECT = 2367 ;
  MQRC_CONFIG_CHANGE_OBJECT = 2368 ;
  MQRC_CONFIG_DELETE_OBJECT = 2369 ;
  MQRC_CONFIG_REFRESH_OBJECT = 2370 ;
  MQRC_CHANNEL_SSL_ERROR = 2371 ;
  MQRC_CF_STRUC_FAILED = 2373 ;
  MQRC_API_EXIT_ERROR = 2374 ;
  MQRC_API_EXIT_INIT_ERROR = 2375 ;
  MQRC_API_EXIT_TERM_ERROR = 2376 ;
  MQRC_EXIT_REASON_ERROR = 2377 ;
  MQRC_RESERVED_VALUE_ERROR = 2378 ;
  MQRC_NO_DATA_AVAILABLE = 2379 ;
  MQRC_SCO_ERROR = 2380 ;
  MQRC_KEY_REPOSITORY_ERROR = 2381 ;
  MQRC_CRYPTO_HARDWARE_ERROR = 2382 ;
  MQRC_AUTH_INFO_REC_COUNT_ERROR = 2383 ;
  MQRC_AUTH_INFO_REC_ERROR = 2384 ;
  MQRC_AIR_ERROR = 2385 ;
  MQRC_AUTH_INFO_TYPE_ERROR = 2386 ;
  MQRC_AUTH_INFO_CONN_NAME_ERROR = 2387 ;
  MQRC_LDAP_USER_NAME_ERROR = 2388 ;
  MQRC_LDAP_USER_NAME_LENGTH_ERR = 2389 ;
  MQRC_LDAP_PASSWORD_ERROR = 2390 ;
  MQRC_SSL_ALREADY_INITIALIZED = 2391 ;
  MQRC_SSL_CONFIG_ERROR = 2392 ;
  MQRC_SSL_INITIALIZATION_ERROR = 2393 ;
  MQRC_Q_INDEX_TYPE_ERROR = 2394 ;
  MQRC_CFBS_ERROR = 2395 ;
  MQRC_SSL_NOT_ALLOWED = 2396 ;
  MQRC_JSSE_ERROR = 2397 ;
  MQRC_SSL_PEER_NAME_MISMATCH = 2398 ;
  MQRC_SSL_PEER_NAME_ERROR = 2399 ;
  MQRC_UNSUPPORTED_CIPHER_SUITE = 2400 ;
  MQRC_SSL_CERTIFICATE_REVOKED = 2401 ;
  MQRC_SSL_CERT_STORE_ERROR = 2402 ;
  MQRC_CLIENT_EXIT_LOAD_ERROR = 2406 ;
  MQRC_CLIENT_EXIT_ERROR = 2407 ;
  MQRC_SSL_KEY_RESET_ERROR = 2409 ;
  MQRC_UNKNOWN_COMPONENT_NAME = 2410 ;
  MQRC_LOGGER_STATUS = 2411 ;
  MQRC_COMMAND_MQSC = 2412 ;
  MQRC_COMMAND_PCF = 2413 ;
  MQRC_CFIF_ERROR = 2414 ;
  MQRC_CFSF_ERROR = 2415 ;
  MQRC_CFGR_ERROR = 2416 ;
  MQRC_MSG_NOT_ALLOWED_IN_GROUP = 2417 ;
  MQRC_FILTER_OPERATOR_ERROR = 2418 ;
  MQRC_NESTED_SELECTOR_ERROR = 2419 ;
  MQRC_EPH_ERROR = 2420 ;
  MQRC_RFH_FORMAT_ERROR = 2421 ;
  MQRC_CFBF_ERROR = 2422 ;
  MQRC_CLIENT_CHANNEL_CONFLICT = 2423 ;
  MQRC_REOPEN_EXCL_INPUT_ERROR = 6100 ;
  MQRC_REOPEN_INQUIRE_ERROR = 6101 ;
  MQRC_REOPEN_SAVED_CONTEXT_ERR = 6102 ;
  MQRC_REOPEN_TEMPORARY_Q_ERROR = 6103 ;
  MQRC_ATTRIBUTE_LOCKED = 6104 ;
  MQRC_CURSOR_NOT_VALID = 6105 ;
  MQRC_ENCODING_ERROR = 6106 ;
  MQRC_STRUC_ID_ERROR = 6107 ;
  MQRC_NULL_POINTER = 6108 ;
  MQRC_NO_CONNECTION_REFERENCE = 6109 ;
  MQRC_NO_BUFFER = 6110 ;
  MQRC_BINARY_DATA_LENGTH_ERROR = 6111 ;
  MQRC_BUFFER_NOT_AUTOMATIC = 6112 ;
  MQRC_INSUFFICIENT_BUFFER = 6113 ;
  MQRC_INSUFFICIENT_DATA = 6114 ;
  MQRC_DATA_TRUNCATED = 6115 ;
  MQRC_ZERO_LENGTH = 6116 ;
  MQRC_NEGATIVE_LENGTH = 6117 ;
  MQRC_NEGATIVE_OFFSET = 6118 ;
  MQRC_INCONSISTENT_FORMAT = 6119 ;
  MQRC_INCONSISTENT_OBJECT_STATE = 6120 ;
  MQRC_CONTEXT_OBJECT_NOT_VALID = 6121 ;
  MQRC_CONTEXT_OPEN_ERROR = 6122 ;
  MQRC_STRUC_LENGTH_ERROR = 6123 ;
  MQRC_NOT_CONNECTED = 6124 ;
  MQRC_NOT_OPEN = 6125 ;
  MQRC_DISTRIBUTION_LIST_EMPTY = 6126 ;
  MQRC_INCONSISTENT_OPEN_OPTIONS = 6127 ;
  MQRC_WRONG_VERSION = 6128 ;
  MQRC_REFERENCE_ERROR = 6129 ;




 //*********************************************************************/
 //*  Values Related to Queue Attributes                               */
 //*********************************************************************/

 //* Queue Types */
  MQQT_LOCAL  = 1;
  MQQT_MODEL  = 2;
  MQQT_ALIAS  = 3;
  MQQT_REMOTE = 6;

 //* Extended Queue Types */
  MQQT_ALL = 1001;

 //* Queue Definition Types */
  MQQDT_PREDEFINED        = 1;
  MQQDT_PERMANENT_DYNAMIC = 2;
  MQQDT_TEMPORARY_DYNAMIC = 3;

 //* Inhibit Get */
  MQQA_GET_INHIBITED = 1;
  MQQA_GET_ALLOWED   = 0;

 //* Inhibit Put */
  MQQA_PUT_INHIBITED = 1;
  MQQA_PUT_ALLOWED   = 0;

 //* Queue Shareability */
  MQQA_SHAREABLE     = 1;
  MQQA_NOT_SHAREABLE = 0;

 //* Back-Out Hardening */
  MQQA_BACKOUT_HARDENED     = 1;
  MQQA_BACKOUT_NOT_HARDENED = 0;

 //* Message Delivery Sequence */
  MQMDS_PRIORITY = 0;
  MQMDS_FIFO     = 1;

 //* Trigger Control */
  MQTC_OFF = 0;
  MQTC_ON  = 1;

 //* Trigger Types */
  MQTT_NONE  = 0;
  MQTT_FIRST = 1;
  MQTT_EVERY = 2;
  MQTT_DEPTH = 3;

 //* Queue Usage */
  MQUS_NORMAL       = 0;
  MQUS_TRANSMISSION = 1;

 //* Command Level */
  MQCMDL_LEVEL_1   = 100;
  MQCMDL_LEVEL_200 = 200;
  MQCMDL_LEVEL_201 = 201;
  MQCMDL_LEVEL_221 = 221;

 //* Platform */
  MQPL_MVS        = 1;
  MQPL_OS2        = 2;
  MQPL_AIX        = 3;
  MQPL_UNIX       = 3;
  MQPL_OS400      = 4;
  MQPL_WINDOWS_NT = 11;

 //* Syncpoint Availability */
  MQSP_AVAILABLE     = 1;
  MQSP_NOT_AVAILABLE = 0;

//*********************************************************************/
//*  MQGMO Structure -- Get Message Options                           */
//*********************************************************************/

type
  MQGMO = record
    StrucId             : MQCHAR4;
    Version             : MQLONG;
    Options             : MQLONG;
    WaitInterval        : MQLONG;
    Signal1             : MQLONG;
    Signal2             : MQLONG;
    ResolvedQName       : MQCHAR48;
    MatchOptions        : MQLONG;
    GroupStatus         : MQCHAR;
    SegmentStatus       : MQCHAR;
    Segmentation        : MQCHAR;
    Reserved1           : MQCHAR;
    MsgToken            : MQBYTE16;
    ReturnedLength      : MQLONG;
  end;
  PMQGMO = ^MQGMO;

const
  MQGMO_DEFAULT : MQGMO = (StrucId:MQGMO_STRUC_ID;
                           Version:MQGMO_VERSION_1;
                           Options:MQGMO_NO_WAIT;
                           WaitInterval:0;
                           Signal1:0;
                           Signal2:0;
                           ResolvedQName:#0;
                           MatchOptions:(MQMO_MATCH_MSG_ID+MQMO_MATCH_CORREL_ID);
                           GroupStatus:MQGS_NOT_IN_GROUP;
                           SegmentStatus: MQSS_NOT_A_SEGMENT;
                           Segmentation:MQSEG_INHIBITED;
                           Reserved1:' ';
                           MsgToken:($0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
                           ReturnedLength:MQRL_UNDEFINED;
                          );

  MQGMO_GETWAIT : MQGMO = (StrucId: MQGMO_STRUC_ID;
                           Version: MQGMO_VERSION_1;
                           Options: MQGMO_WAIT
                                  + MQGMO_NO_SYNCPOINT
                                  + MQGMO_FAIL_IF_QUIESCING
                                  + MQGMO_CONVERT
                                  + MQGMO_ACCEPT_TRUNCATED_MSG
                                  ;
                           WaitInterval:MQWI_UNLIMITED;
                           Signal1:0;
                           Signal2:0;
                           ResolvedQName:#0;
                           MatchOptions:(MQMO_NONE);
                           GroupStatus:MQGS_NOT_IN_GROUP;
                           SegmentStatus: MQSS_NOT_A_SEGMENT;
                           Segmentation:MQSEG_INHIBITED;
                           Reserved1:' ';
                           MsgToken:($0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
                           ReturnedLength:MQRL_UNDEFINED;
                          );
  MQGMO_BROWSE  : MQGMO = (StrucId: MQGMO_STRUC_ID;
                           Version: MQGMO_VERSION_1;
                           Options: MQGMO_BROWSE_NEXT
                                  + MQGMO_NO_SYNCPOINT
                                  + MQGMO_FAIL_IF_QUIESCING
                                  + MQGMO_CONVERT
                                  + MQGMO_ACCEPT_TRUNCATED_MSG
                                  ;
                           WaitInterval:0;
                           Signal1:0;
                           Signal2:0;
                           ResolvedQName:#0;
                           MatchOptions:(MQMO_NONE);
                           GroupStatus:MQGS_NOT_IN_GROUP;
                           SegmentStatus: MQSS_NOT_A_SEGMENT;
                           Segmentation:MQSEG_INHIBITED;
                           Reserved1:' ';
                           MsgToken:($0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
                           ReturnedLength:MQRL_UNDEFINED;
                          );

//*********************************************************************/
//*  MQMD Structure -- Message Descriptor                             */
//*********************************************************************/

type
  MQMD = record
    StrucId         : MQCHAR4;   //* Structure identifier */
    Version         : MQLONG;    //* Structure version number */
    Report          : MQLONG;    //* Report options */
    MsgType         : MQLONG;    //* Message type */
    Expiry          : MQLONG;    //* Expiry time */
    Feedback        : MQLONG;    //* Feedback or reason code */
    Encoding        : MQLONG;    //* Data encoding */
    CodedCharSetId  : MQLONG;    //* Coded character set identifier */
    Format          : MQCHAR8;   //* Format name */
    Priority        : MQLONG;    //* Message priority */
    Persistence     : MQLONG;    //* Message persistence */
    MsgId           : MQBYTE24;  //* Message identifier */
    CorrelId        : MQBYTE24;  //* Correlation identifier */
    BackoutCount    : MQLONG;    //* Backout counter */
    ReplyToQ        : MQCHAR48;  //* Name of reply-to queue */
    ReplyToQMgr     : MQCHAR48;  //* Name of reply queue manager */
    UserIdentifier  : MQCHAR12;  //* User identifier */
    AccountingToken : MQBYTE32;  //* Accounting token */
    ApplIdentityData: MQCHAR32;  //* Application data relating to identity */
    PutApplType     : MQLONG;    //* Type of application that put the message */
    PutApplName     : MQCHAR28;  //* Name of application that put the message */
    PutDate         : MQCHAR8;   //* Date when message was put */
    PutTime         : MQCHAR8;   //* Time when message was put */
    ApplOriginData  : MQCHAR4;   //* Application data relating to origin */
    GroupId             : MQBYTE24;
    MsgSeqNumber        : MQLONG;
    Offset              : MQLONG;
    MsgFlags            : MQLONG;
    OriginalLength      : MQLONG;
  end;
  PMQMD = ^MQMD;

const
   MQMD_DEFAULT : MQMD = (StrucId:MQMD_STRUC_ID;
                          Version:MQMD_VERSION_1;
                          Report:MQRO_NONE;
                          MsgType:MQMT_DATAGRAM;
                          Expiry:MQEI_UNLIMITED;
                          FeedBack:MQFB_NONE;
                          Encoding:MQENC_NATIVE;
                          CodedCharSetId:MQCCSI_Q_MGR;
                          Format:MQFMT_NONE;
                          Priority:MQPRI_PRIORITY_AS_Q_DEF;
                          Persistence:MQPER_PERSISTENCE_AS_Q_DEF;
                          MsgId:($0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
                          CorrelId:($0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
                          BackoutCount:0;
                          ReplyToQ:#0;
                          ReplyToQMgr:#0;
                          UserIdentifier:#0;
                          AccountingToken:($0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
                          ApplIdentityData:#0;
                          PutApplType:MQAT_NO_CONTEXT;
                          PutApplName:#0;
                          PutDate:#0;
                          PutTime:#0;
                          ApplOriginData:#0;
                          GroupId:( $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
                          MsgSeqNumber:1;
                          Offset:0;
                          MsgFlags:MQMF_NONE;
                          OriginalLength:MQOL_UNDEFINED;
                         );

//*********************************************************************/
//*  MQOD Structure -- Object Descriptor                              */
//*********************************************************************/

type
  MQOD = record
    StrucId             : MQCHAR4;
    Version             : MQLONG;
    ObjectType          : MQLONG;
    ObjectName          : MQCHAR48;
    ObjectQMgrName      : MQCHAR48;
    DynamicQName        : MQCHAR48;
    AlternateUserID     : MQCHAR12;
    RecsPresent         : MQLONG;
    KnownDestCount      : MQLONG;
    UnKnownDestCount    : MQLONG;
    InvalidDestCount    : MQLONG;
    ObjectRecOffset     : MQLONG;
    ResponseRecOffset   : MQLONG;
    ObjectPointer       : MQPTR;
    ResponseRecPointer  : MQPTR;
    AlternateSecurityID : MQBYTE40;
    ResolvedQName       : MQCHAR48;
    ResolvedQMgrName    : MQCHAR48;
  end;
  PMQOD = ^MQOD;

const
  MQOD_DEFAULT : MQOD = (StrucId:MQOD_STRUC_ID;
                         Version:MQOD_VERSION_1;
                         ObjectType:MQOT_Q;
                         ObjectName:#0;
                         ObjectQMgrName:#0;
                         DynamicQName:'AMQ.*'#0;
                         RecsPresent:0;
                         KnownDestCount:0;
                         UnKnownDestCount:0;
                         InvalidDestCount:0;
                         ObjectRecOffset:0;
                         ResponseRecOffset:0;
                         ObjectPointer:nil;
                         ResponseRecPointer:nil;
                         AlternateSecurityID:($0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
                         ResolvedQName:#0;
                         ResolvedQMgrName:#0;
                         );

//*********************************************************************/
//*  MQPMO Structure -- Put Message Options                           */
//*********************************************************************/

type
  MQPMO = record
    StrucId             : MQCHAR4;
    Version             : MQLONG;
    Options             : MQLONG;
    TimeOut             : MQLONG;
    Context             : MQHOBJ;
    KnownDestCount      : MQLONG;
    UnKnownDestCount    : MQLONG;
    InvalidDestCount    : MQLONG;
    ResolvedQName       : MQCHAR48;
    ResolvedQMgrName    : MQCHAR48;
    RecsPresent         : MQLONG;
    PutMsgRecFields     : MQLONG;
    PutMsgRecOffset     : MQLONG;
    ResponseRecOffset   : MQLONG;
    PutMsgRecPtr        : MQPTR;
    ResponseRecPtr      : MQPTR;
  end;
  PMQPMO = ^MQPMO;

const
  MQPMO_DEFAULT : MQPMO = (StrucId:MQPMO_STRUC_ID;
                           Version:MQPMO_VERSION_1;
                           Options:MQPMO_NONE;
                           TimeOut:-1;
                           Context:0;
                           KnownDestCount:0;
                           UnKnownDestCount:0;
                           InvalidDestCount:0;
                           ResolvedQName:#0;
                           ResolvedQMgrName:#0;
                           RecsPresent:0;
                           PutMsgRecFields:0;
                           PutMsgRecOffset:0;
                           PutMsgRecPtr:nil;
                           ResponseRecPtr:nil;
                          );

var

  MQCONN: TMQCONNPROC;
  MQDISC: TMQDISCPROC;
  MQOPEN: TMQOPENPROC;
  MQCLOSE: TMQCLOSEPROC;
  MQGET: TMQGETPROC;
  MQPUT: TMQPUTPROC;
  MQPUT1: TMQPUT1PROC;
  MQBACK: TMQBACKPROC;
  MQCMIT: TMQCMITPROC;
  MQINQ: TMQINQPROC;
  MQSET: TMQSETPROC;

const
  //* Structure Length */
  MQCFH_STRUC_LENGTH = 36;

  //* Structure Version Number */
  MQCFH_VERSION_1 = 1;

  //* Command Codes */
  MQCMD_CHANGE_Q_MGR           = 1;
  MQCMD_INQUIRE_Q_MGR          = 2;
  MQCMD_CHANGE_PROCESS         = 3;
  MQCMD_COPY_PROCESS           = 4;
  MQCMD_CREATE_PROCESS         = 5;
  MQCMD_DELETE_PROCESS         = 6;
  MQCMD_INQUIRE_PROCESS        = 7;
  MQCMD_CHANGE_Q               = 8;
  MQCMD_CLEAR_Q                = 9;
  MQCMD_COPY_Q                 = 10;
  MQCMD_CREATE_Q               = 11;
  MQCMD_DELETE_Q               = 12;
  MQCMD_INQUIRE_Q              = 13;
  MQCMD_RESET_Q_STATS          = 17;
  MQCMD_INQUIRE_Q_NAMES        = 18;
  MQCMD_INQUIRE_PROCESS_NAMES  = 19;
  MQCMD_INQUIRE_CHANNEL_NAMES  = 20;
  MQCMD_CHANGE_CHANNEL         = 21;
  MQCMD_COPY_CHANNEL           = 22;
  MQCMD_CREATE_CHANNEL         = 23;
  MQCMD_DELETE_CHANNEL         = 24;
  MQCMD_INQUIRE_CHANNEL        = 25;
  MQCMD_PING_CHANNEL           = 26;
  MQCMD_RESET_CHANNEL          = 27;
  MQCMD_START_CHANNEL          = 28;
  MQCMD_STOP_CHANNEL           = 29;
  MQCMD_START_CHANNEL_INIT     = 30;
  MQCMD_START_CHANNEL_LISTENER = 31;
  MQCMD_ESCAPE                 = 38;
  MQCMD_RESOLVE_CHANNEL        = 39;
  MQCMD_PING_Q_MGR             = 40;
  MQCMD_INQUIRE_CHANNEL_STATUS = 42;
  MQCMD_Q_MGR_EVENT            = 44;
  MQCMD_PERFM_EVENT            = 45;
  MQCMD_CHANNEL_EVENT          = 46;

  //* Control Options */
  MQCFC_LAST     = 1;
  MQCFC_NOT_LAST = 0;

  //* Reason Codes */
  MQRCCF_CFH_TYPE_ERROR          = 3001;
  MQRCCF_CFH_LENGTH_ERROR        = 3002;
  MQRCCF_CFH_VERSION_ERROR       = 3003;
  MQRCCF_CFH_MSG_SEQ_NUMBER_ERR  = 3004;
  MQRCCF_CFH_CONTROL_ERROR       = 3005;
  MQRCCF_CFH_PARM_COUNT_ERROR    = 3006;
  MQRCCF_CFH_COMMAND_ERROR       = 3007;
  MQRCCF_COMMAND_FAILED          = 3008;
  MQRCCF_CFIN_LENGTH_ERROR       = 3009;
  MQRCCF_CFST_LENGTH_ERROR       = 3010;
  MQRCCF_CFST_STRING_LENGTH_ERR  = 3011;
  MQRCCF_FORCE_VALUE_ERROR       = 3012;
  MQRCCF_STRUCTURE_TYPE_ERROR    = 3013;
  MQRCCF_CFIN_PARM_ID_ERROR      = 3014;
  MQRCCF_CFST_PARM_ID_ERROR      = 3015;
  MQRCCF_MSG_LENGTH_ERROR        = 3016;
  MQRCCF_CFIN_DUPLICATE_PARM     = 3017;
  MQRCCF_CFST_DUPLICATE_PARM     = 3018;
  MQRCCF_PARM_COUNT_TOO_SMALL    = 3019;
  MQRCCF_PARM_COUNT_TOO_BIG      = 3020;
  MQRCCF_Q_ALREADY_IN_CELL       = 3021;
  MQRCCF_Q_TYPE_ERROR            = 3022;
  MQRCCF_MD_FORMAT_ERROR         = 3023;
  MQRCCF_REPLACE_VALUE_ERROR     = 3025;
  MQRCCF_CFIL_DUPLICATE_VALUE    = 3026;
  MQRCCF_CFIL_COUNT_ERROR        = 3027;
  MQRCCF_CFIL_LENGTH_ERROR       = 3028;
  MQRCCF_QUIESCE_VALUE_ERROR     = 3029;
  MQRCCF_MSG_SEQ_NUMBER_ERROR    = 3030;
  MQRCCF_PING_DATA_COUNT_ERROR   = 3031;
  MQRCCF_PING_DATA_COMPARE_ERROR = 3032;
  MQRCCF_CHANNEL_TYPE_ERROR      = 3034;
  MQRCCF_PARM_SEQUENCE_ERROR     = 3035;
  MQRCCF_XMIT_PROTOCOL_TYPE_ERR  = 3036;
  MQRCCF_BATCH_SIZE_ERROR        = 3037;
  MQRCCF_DISC_INT_ERROR          = 3038;
  MQRCCF_SHORT_RETRY_ERROR       = 3039;
  MQRCCF_SHORT_TIMER_ERROR       = 3040;
  MQRCCF_LONG_RETRY_ERROR        = 3041;
  MQRCCF_LONG_TIMER_ERROR        = 3042;
  MQRCCF_SEQ_NUMBER_WRAP_ERROR   = 3043;
  MQRCCF_MAX_MSG_LENGTH_ERROR    = 3044;
  MQRCCF_PUT_AUTH_ERROR          = 3045;
  MQRCCF_PURGE_VALUE_ERROR       = 3046;
  MQRCCF_CFIL_PARM_ID_ERROR      = 3047;
  MQRCCF_MSG_TRUNCATED           = 3048;
  MQRCCF_CCSID_ERROR             = 3049;
  MQRCCF_ENCODING_ERROR          = 3050;
  MQRCCF_DATA_CONV_VALUE_ERROR   = 3052;
  MQRCCF_INDOUBT_VALUE_ERROR     = 3053;
  MQRCCF_ESCAPE_TYPE_ERROR       = 3054;
  MQRCCF_CHANNEL_TABLE_ERROR     = 3062;
  MQRCCF_MCA_TYPE_ERROR          = 3063;
  MQRCCF_CHL_INST_TYPE_ERROR     = 3064;
  MQRCCF_CHL_STATUS_NOT_FOUND    = 3065;
  MQRCCF_OBJECT_ALREADY_EXISTS   = 4001;
  MQRCCF_OBJECT_WRONG_TYPE       = 4002;
  MQRCCF_LIKE_OBJECT_WRONG_TYPE  = 4003;
  MQRCCF_OBJECT_OPEN             = 4004;
  MQRCCF_ATTR_VALUE_ERROR        = 4005;
  MQRCCF_UNKNOWN_Q_MGR           = 4006;
  MQRCCF_Q_WRONG_TYPE            = 4007;
  MQRCCF_OBJECT_NAME_ERROR       = 4008;
  MQRCCF_ALLOCATE_FAILED         = 4009;
  MQRCCF_HOST_NOT_AVAILABLE      = 4010;
  MQRCCF_CONFIGURATION_ERROR     = 4011;
  MQRCCF_CONNECTION_REFUSED      = 4012;
  MQRCCF_ENTRY_ERROR             = 4013;
  MQRCCF_SEND_FAILED             = 4014;
  MQRCCF_RECEIVED_DATA_ERROR     = 4015;
  MQRCCF_RECEIVE_FAILED          = 4016;
  MQRCCF_CONNECTION_CLOSED       = 4017;
  MQRCCF_NO_STORAGE              = 4018;
  MQRCCF_NO_COMMS_MANAGER        = 4019;
  MQRCCF_LISTENER_NOT_STARTED    = 4020;
  MQRCCF_BIND_FAILED             = 4024;
  MQRCCF_CHANNEL_INDOUBT         = 4025;
  MQRCCF_MQCONN_FAILED           = 4026;
  MQRCCF_MQOPEN_FAILED           = 4027;
  MQRCCF_MQGET_FAILED            = 4028;
  MQRCCF_MQPUT_FAILED            = 4029;
  MQRCCF_PING_ERROR              = 4030;
  MQRCCF_CHANNEL_IN_USE          = 4031;
  MQRCCF_CHANNEL_NOT_FOUND       = 4032;
  MQRCCF_UNKNOWN_REMOTE_CHANNEL  = 4033;
  MQRCCF_REMOTE_QM_UNAVAILABLE   = 4034;
  MQRCCF_REMOTE_QM_TERMINATING   = 4035;
  MQRCCF_MQINQ_FAILED            = 4036;
  MQRCCF_NOT_XMIT_Q              = 4037;
  MQRCCF_CHANNEL_DISABLED        = 4038;
  MQRCCF_USER_EXIT_NOT_AVAILABLE = 4039;
  MQRCCF_COMMIT_FAILED           = 4040;
  MQRCCF_CHANNEL_ALREADY_EXISTS  = 4042;
  MQRCCF_DATA_TOO_LARGE          = 4043;
  MQRCCF_CHANNEL_NAME_ERROR      = 4044;
  MQRCCF_XMIT_Q_NAME_ERROR       = 4045;
  MQRCCF_MCA_NAME_ERROR          = 4047;
  MQRCCF_SEND_EXIT_NAME_ERROR    = 4048;
  MQRCCF_SEC_EXIT_NAME_ERROR     = 4049;
  MQRCCF_MSG_EXIT_NAME_ERROR     = 4050;
  MQRCCF_RCV_EXIT_NAME_ERROR     = 4051;
  MQRCCF_XMIT_Q_NAME_WRONG_TYPE  = 4052;
  MQRCCF_MCA_NAME_WRONG_TYPE     = 4053;
  MQRCCF_DISC_INT_WRONG_TYPE     = 4054;
  MQRCCF_SHORT_RETRY_WRONG_TYPE  = 4055;
  MQRCCF_SHORT_TIMER_WRONG_TYPE  = 4056;
  MQRCCF_LONG_RETRY_WRONG_TYPE   = 4057;
  MQRCCF_LONG_TIMER_WRONG_TYPE   = 4058;
  MQRCCF_PUT_AUTH_WRONG_TYPE     = 4059;
  MQRCCF_MISSING_CONN_NAME       = 4061;
  MQRCCF_CONN_NAME_ERROR         = 4062;
  MQRCCF_MQSET_FAILED            = 4063;
  MQRCCF_CHANNEL_NOT_ACTIVE      = 4064;
  MQRCCF_TERMINATED_BY_SEC_EXIT  = 4065;
  MQRCCF_DYNAMIC_Q_SCOPE_ERROR   = 4067;
  MQRCCF_CELL_DIR_NOT_AVAILABLE  = 4068;
  MQRCCF_MR_COUNT_ERROR          = 4069;
  MQRCCF_MR_COUNT_WRONG_TYPE     = 4070;
  MQRCCF_MR_EXIT_NAME_ERROR      = 4071;
  MQRCCF_MR_EXIT_NAME_WRONG_TYPE = 4072;
  MQRCCF_MR_INTERVAL_ERROR       = 4073;
  MQRCCF_MR_INTERVAL_WRONG_TYPE  = 4074;

  //* Structure Length (Fixed Part) */
  MQCFIL_STRUC_LENGTH_FIXED = 16;

  //* Structure Length */
  MQCFIN_STRUC_LENGTH = 16;

  //* Structure Length (Fixed Part) */
  MQCFSL_STRUC_LENGTH_FIXED = 24;

  //* Structure Length (Fixed Part) */
  MQCFST_STRUC_LENGTH_FIXED = 20;

  //* Structure Type */
  MQCFT_COMMAND      = 1;
  MQCFT_RESPONSE     = 2;
  MQCFT_INTEGER      = 3;
  MQCFT_STRING       = 4;
  MQCFT_INTEGER_LIST = 5;
  MQCFT_STRING_LIST  = 6;
  MQCFT_EVENT        = 7;

  //* Integer Parameter Types */
  MQIACF_FIRST                = 1001;
  MQIACF_Q_MGR_ATTRS          = 1001;
  MQIACF_Q_ATTRS              = 1002;
  MQIACF_PROCESS_ATTRS        = 1003;
  MQIACF_FORCE                = 1005;
  MQIACF_REPLACE              = 1006;
  MQIACF_PURGE                = 1007;
  MQIACF_QUIESCE              = 1008;
  MQIACF_ALL                  = 1009;
  MQIACF_PARAMETER_ID         = 1012;
  MQIACF_ERROR_ID             = 1013;
  MQIACF_ERROR_IDENTIFIER     = 1013;
  MQIACF_SELECTOR             = 1014;
  MQIACF_CHANNEL_ATTRS        = 1015;
  MQIACF_ESCAPE_TYPE          = 1017;
  MQIACF_ERROR_OFFSET         = 1018;
  MQIACF_REASON_QUALIFIER     = 1020;
  MQIACF_COMMAND              = 1021;
  MQIACF_OPEN_OPTIONS         = 1022;
  MQIACF_AUX_ERROR_DATA_INT_1 = 1070;
  MQIACF_AUX_ERROR_DATA_INT_2 = 1071;
  MQIACF_CONV_REASON_CODE     = 1072;
  MQIACF_LAST_USED            = 1072;

  MQIACH_FIRST                   = 1501;
  MQIACH_XMIT_PROTOCOL_TYPE      = 1501;
  MQIACH_BATCH_SIZE              = 1502;
  MQIACH_DISC_INTERVAL           = 1503;
  MQIACH_SHORT_TIMER             = 1504;
  MQIACH_SHORT_RETRY             = 1505;
  MQIACH_LONG_TIMER              = 1506;
  MQIACH_LONG_RETRY              = 1507;
  MQIACH_PUT_AUTHORITY           = 1508;
  MQIACH_SEQUENCE_NUMBER_WRAP    = 1509;
  MQIACH_MAX_MSG_LENGTH          = 1510;
  MQIACH_CHANNEL_TYPE            = 1511;
  MQIACH_DATA_COUNT              = 1512;
  MQIACH_MSG_SEQUENCE_NUMBER     = 1514;
  MQIACH_DATA_CONVERSION         = 1515;
  MQIACH_IN_DOUBT                = 1516;
  MQIACH_MCA_TYPE                = 1517;
  MQIACH_CHANNEL_INSTANCE_TYPE   = 1523;
  MQIACH_CHANNEL_INSTANCE_ATTRS  = 1524;
  MQIACH_CHANNEL_ERROR_DATA      = 1525;
  MQIACH_CHANNEL_TABLE           = 1526;
  MQIACH_CHANNEL_STATUS          = 1527;
  MQIACH_INDOUBT_STATUS          = 1528;
  MQIACH_LAST_SEQUENCE_NUMBER    = 1529;
  MQIACH_CURRENT_MSGS            = 1531;
  MQIACH_CURRENT_SEQUENCE_NUMBER = 1532;
  MQIACH_MSGS                    = 1534;
  MQIACH_BYTES_SENT              = 1535;
  MQIACH_BYTES_RECEIVED          = 1536;
  MQIACH_BATCHES                 = 1537;
  MQIACH_BUFFERS_SENT            = 1538;
  MQIACH_BUFFERS_RECEIVED        = 1539;
  MQIACH_LONG_RETRIES_LEFT       = 1540;
  MQIACH_SHORT_RETRIES_LEFT      = 1541;
  MQIACH_MCA_STATUS              = 1542;
  MQIACH_STOP_REQUESTED          = 1543;
  MQIACH_MR_COUNT                = 1544;
  MQIACH_MR_INTERVAL             = 1545;
  MQIACH_LAST_USED               = 1545;

  //* Character Parameter Types */
  MQCACF_FIRST                   = 3001;
  MQCACF_FROM_Q_NAME             = 3001;
  MQCACF_TO_Q_NAME               = 3002;
  MQCACF_FROM_PROCESS_NAME       = 3003;
  MQCACF_TO_PROCESS_NAME         = 3004;
  MQCACF_FROM_CHANNEL_NAME       = 3007;
  MQCACF_TO_CHANNEL_NAME         = 3008;
  MQCACF_Q_NAMES                 = 3011;
  MQCACF_PROCESS_NAMES           = 3012;
  MQCACF_ESCAPE_TEXT             = 3014;
  MQCACF_LOCAL_Q_NAMES           = 3015;
  MQCACF_MODEL_Q_NAMES           = 3016;
  MQCACF_ALIAS_Q_NAMES           = 3017;
  MQCACF_REMOTE_Q_NAMES          = 3018;
  MQCACF_SENDER_CHANNEL_NAMES    = 3019;
  MQCACF_SERVER_CHANNEL_NAMES    = 3020;
  MQCACF_REQUESTER_CHANNEL_NAMES = 3021;
  MQCACF_RECEIVER_CHANNEL_NAMES  = 3022;
  MQCACF_OBJECT_Q_MGR_NAME       = 3023;
  MQCACF_APPL_NAME               = 3024;
  MQCACF_USER_IDENTIFIER         = 3025;
  MQCACF_AUX_ERROR_DATA_STR_1    = 3026;
  MQCACF_AUX_ERROR_DATA_STR_2    = 3027;
  MQCACF_AUX_ERROR_DATA_STR_3    = 3028;
  MQCACF_LAST_USED               = 3028;

  MQCACH_FIRST               = 3501;
  MQCACH_CHANNEL_NAME        = 3501;
  MQCACH_DESC                = 3502;
  MQCACH_MODE_NAME           = 3503;
  MQCACH_TP_NAME             = 3504;
  MQCACH_XMIT_Q_NAME         = 3505;
  MQCACH_CONNECTION_NAME     = 3506;
  MQCACH_MCA_NAME            = 3507;
  MQCACH_SEC_EXIT_NAME       = 3508;
  MQCACH_MSG_EXIT_NAME       = 3509;
  MQCACH_SEND_EXIT_NAME      = 3510;
  MQCACH_RCV_EXIT_NAME       = 3511;
  MQCACH_CHANNEL_NAMES       = 3512;
  MQCACH_SEC_EXIT_USER_DATA  = 3513;
  MQCACH_MSG_EXIT_USER_DATA  = 3514;
  MQCACH_SEND_EXIT_USER_DATA = 3515;
  MQCACH_RCV_EXIT_USER_DATA  = 3516;
  MQCACH_USER_ID             = 3517;
  MQCACH_PASSWORD            = 3518;
  MQCACH_LAST_MSG_TIME       = 3524;
  MQCACH_LAST_MSG_DATE       = 3525;
  MQCACH_MCA_USER_ID         = 3527;
  MQCACH_CHANNEL_START_TIME  = 3528;
  MQCACH_CHANNEL_START_DATE  = 3529;
  MQCACH_MCA_JOB_NAME        = 3530;
  MQCACH_LAST_LUWID          = 3531;
  MQCACH_CURRENT_LUWID       = 3532;
  MQCACH_FORMAT_NAME         = 3533;
  MQCACH_MR_EXIT_NAME        = 3534;
  MQCACH_MR_EXIT_USER_DATA   = 3535;
  MQCACH_LAST_USED           = 3535;

  //* Indoubt Status */
  MQCHIDS_NOT_INDOUBT = 0;
  MQCHIDS_INDOUBT     = 1;

  //* Channel Status */
  MQCHS_BINDING    = 1;
  MQCHS_STARTING   = 2;
  MQCHS_RUNNING    = 3;
  MQCHS_STOPPING   = 4;
  MQCHS_RETRYING   = 5;
  MQCHS_STOPPED    = 6;
  MQCHS_REQUESTING = 7;
  MQCHS_PAUSED     = 8;

  //* Channel Stop Option */
  MQCHSR_STOP_NOT_REQUESTED = 0;
  MQCHSR_STOP_REQUESTED     = 1;

  //* Channel Table Type */
  MQCHTAB_Q_MGR    = 1;
  MQCHTAB_CLNTCONN = 2;

  //* Escape Type */
  MQET_MQSC = 1;

  //* Event Recording */
  MQEVR_DISABLED = 0;
  MQEVR_ENABLED  = 1;

  //* Force Option */
  MQFC_YES = 1;
  MQFC_NO  = 0;

  //* Indoubt Option */
  MQIDO_COMMIT  = 1;
  MQIDO_BACKOUT = 2;

  //* Message Channel Agent Status */
  MQMCAS_STOPPED = 0;
  MQMCAS_RUNNING = 3;

  //* Purge Option */
  MQPO_YES = 1;
  MQPO_NO  = 0;

  //* Quiesce Option */
  MQQO_YES = 1;
  MQQO_NO  = 0;

  //* Queue Service-Interval Events */
  MQQSIE_NONE = 0;
  MQQSIE_HIGH = 1;
  MQQSIE_OK   = 2;

  //* Replace Option */
  MQRP_YES = 1;
  MQRP_NO  = 0;

  //* Reason Qualifiers */
  MQRQ_CONN_NOT_AUTHORIZED      = 1;
  MQRQ_OPEN_NOT_AUTHORIZED      = 2;
  MQRQ_CLOSE_NOT_AUTHORIZED     = 3;
  MQRQ_CMD_NOT_AUTHORIZED       = 4;
  MQRQ_Q_MGR_STOPPING           = 5;
  MQRQ_Q_MGR_QUIESCING          = 6;
  MQRQ_CHANNEL_STOPPED_OK       = 7;
  MQRQ_CHANNEL_STOPPED_ERROR    = 8;
  MQRQ_CHANNEL_STOPPED_RETRY    = 9;
  MQRQ_CHANNEL_STOPPED_DISABLED = 10;

  //* Queue Definition Scope */
  MQSCO_Q_MGR = 1;
  MQSCO_CELL  = 2;

type
  MQCFH = record
    SType:          MQLONG;  //* Structure type */
    StrucLength:    MQLONG;  //* Structure length */
    Version:        MQLONG;  //* Structure version number */
    Command:        MQLONG;  //* Command identifier */
    MsgSeqNumber:   MQLONG;  //* Message sequence number */
    Control:        MQLONG;  //* Control options */
    CompCode:       MQLONG;  //* Completion code */
    Reason:         MQLONG;  //* Reason code qualifying completion code */
    ParameterCount: MQLONG;  //* Count of parameter structures */
  end;
  PMQCFH = ^MQCFH;

const
  MQCFH_DEFAULT: MQCFH = (SType: MQCFT_COMMAND;
                          StrucLength: MQCFH_STRUC_LENGTH;
                          Version: MQCFH_VERSION_1;
                          Command: 0;
                          MsgSeqNumber: 1;
                          Control: MQCFC_LAST;
                          CompCode: MQCC_OK;
                          Reason: MQRC_NONE;
                          ParameterCount: 0);

type
  MQCFIL = record
    SType:          MQLONG;  //* Structure type */
    StrucLength:    MQLONG;  //* Structure length */
    Parameter:      MQLONG;  //* Parameter identifier */
    Count:          MQLONG;  //* Count of parameter values */
    Values:         array[0..0] of MQLONG;  //* Parameter values - first element */
  end;
  PMQCFIL = ^MQCFIL;

const
  MQCFIL_DEFAULT: MQCFIL = (SType: MQCFT_INTEGER_LIST;
                            StrucLength: MQCFIL_STRUC_LENGTH_FIXED;
                            Parameter: 0;
                            Count: 0);
                            {[0]}

type
  MQCFIN = record
    SType:          MQLONG;  //* Structure type */
    StrucLength:    MQLONG;  //* Structure length */
    Parameter:      MQLONG;  //* Parameter identifier */
    Value:          MQLONG;  //* Parameter value */
  end;
  PMQCFIN = ^MQCFIN;

const
  MQCFIN_DEFAULT: MQCFIN = (SType: MQCFT_INTEGER;
                            StrucLength: MQCFIN_STRUC_LENGTH;
                            Parameter: 0;
                            Value: 0);

type
  MQCFSL = record
    SType:          MQLONG;  //* Structure type */
    StrucLength:    MQLONG;  //* Structure length */
    Parameter:      MQLONG;  //* Parameter identifier */
    CodedCharSetId: MQLONG;  //* Coded character set identifier */
    Count:          MQLONG;  //* Count of parameter values */
    StringLength:   MQLONG;  //* Length of one string */
    Strings:        array[0..0] of AnsiChar;  //* String values - first character */
  end;
  PMQCFSL = ^MQCFSL;

const
  MQCFSL_DEFAULT: MQCFSL = (SType: MQCFT_STRING_LIST;
                            StrucLength: MQCFSL_STRUC_LENGTH_FIXED;
                            Parameter: 0;
                            CodedCharSetId: MQCCSI_DEFAULT;
                            Count: 0;
                            StringLength: 0);
                            {''}

type
  MQCFST = record
    SType:          MQLONG;  //* Structure type */
    StrucLength:    MQLONG;  //* Structure length */
    Parameter:      MQLONG;  //* Parameter identifier */
    CodedCharSetId: MQLONG;  //* Coded character set identifier */
    StringLength:   MQLONG;  //* Length of string */
    Strings:        array[0..0] of AnsiChar;   //* String value - first character */
  end;
  PMQCFST = ^MQCFST;

const
  MQCFST_DEFAULT: MQCFST = (SType: MQCFT_STRING;
                            StrucLength: MQCFST_STRUC_LENGTH_FIXED;
                            Parameter: 0;
                            CodedCharSetId: MQCCSI_DEFAULT;
                            StringLength: 0);
                            {""}

const
  //* Structure Version Number */
  MQCD_VERSION_1       = 1;
  MQCD_VERSION_2       = 2;
  MQCD_VERSION_3       = 3;
  MQCD_CURRENT_VERSION = 3;

  //* Channel Type */
  MQCHT_SENDER    = 1;
  MQCHT_SERVER    = 2;
  MQCHT_RECEIVER  = 3;
  MQCHT_REQUESTER = 4;
  MQCHT_ALL       = 5;
  MQCHT_CLNTCONN  = 6;
  MQCHT_SVRCONN   = 7;

  //* Transport Type */
  MQXPT_LU62    = 1;
  MQXPT_TCP     = 2;
  MQXPT_NETBIOS = 3;

  //* Put Authority */
  MQPA_DEFAULT = 1;
  MQPA_CONTEXT = 2;

  //* Channel Data Conversion */
  MQCDC_SENDER_CONVERSION    = 1;
  MQCDC_NO_SENDER_CONVERSION = 0;

  //* MCA Type */
  MQMCAT_PROCESS = 1;
  MQMCAT_THREAD  = 2;


const
  // 2-dimensional array. Combination of parameter and maximum stringlength.
  ParamLengthCount = 46;
  ParamLength: array[0..ParamLengthCount-1,0..1] of MQLONG =
    ((MQCA_APPL_ID, MQ_PROCESS_APPL_ID_LENGTH),
     (MQCA_BACKOUT_REQ_Q_NAME, MQ_Q_NAME_LENGTH),
     (MQCA_BASE_Q_NAME, MQ_Q_NAME_LENGTH),
     (MQCA_COMMAND_INPUT_Q_NAME, MQ_Q_NAME_LENGTH),
     (MQCA_CREATION_DATE, MQ_CREATION_DATE_LENGTH),
     (MQCA_CREATION_TIME, MQ_CREATION_TIME_LENGTH),
     (MQCA_DEAD_LETTER_Q_NAME, MQ_Q_NAME_LENGTH),
     (MQCA_DEF_XMIT_Q_NAME, MQ_Q_NAME_LENGTH),
     (MQCA_ENV_DATA, MQ_PROCESS_ENV_DATA_LENGTH),
     (MQCA_INITIATION_Q_NAME, MQ_Q_NAME_LENGTH),
     (MQCA_PROCESS_DESC, MQ_PROCESS_DESC_LENGTH),
     (MQCA_PROCESS_NAME, MQ_PROCESS_NAME_LENGTH),
     (MQCA_Q_DESC, MQ_Q_DESC_LENGTH),
     (MQCA_Q_MGR_DESC, MQ_Q_MGR_DESC_LENGTH),
     (MQCA_Q_MGR_NAME, MQ_Q_MGR_NAME_LENGTH),
     (MQCA_Q_NAME, MQ_Q_NAME_LENGTH),
     (MQCA_REMOTE_Q_MGR_NAME, MQ_Q_MGR_NAME_LENGTH),
     (MQCA_REMOTE_Q_NAME, MQ_Q_NAME_LENGTH),
     (MQCA_STORAGE_CLASS, MQ_STORAGE_CLASS_LENGTH),
     (MQCA_TRIGGER_DATA, MQ_TRIGGER_DATA_LENGTH),
     (MQCA_USER_DATA, MQ_PROCESS_USER_DATA_LENGTH),
     (MQCA_XMIT_Q_NAME, MQ_Q_NAME_LENGTH),
     (MQCACF_FROM_Q_NAME, MQ_Q_NAME_LENGTH),
     (MQCACF_TO_Q_NAME, MQ_Q_NAME_LENGTH),
     (MQCACH_CHANNEL_NAME, MQ_CHANNEL_NAME_LENGTH),
     (MQCACH_DESC, MQ_CHANNEL_DESC_LENGTH),
     (MQCACH_SEC_EXIT_NAME, MQ_EXIT_NAME_LENGTH),
     (MQCACH_MSG_EXIT_NAME, MQ_EXIT_NAME_LENGTH),
     (MQCACH_SEND_EXIT_NAME, MQ_EXIT_NAME_LENGTH),
     (MQCACH_RCV_EXIT_NAME, MQ_EXIT_NAME_LENGTH),
     (MQCACH_SEC_EXIT_USER_DATA, MQ_EXIT_DATA_LENGTH),
     (MQCACH_MSG_EXIT_USER_DATA, MQ_EXIT_DATA_LENGTH),
     (MQCACH_SEND_EXIT_USER_DATA, MQ_EXIT_DATA_LENGTH),
     (MQCACH_RCV_EXIT_USER_DATA, MQ_EXIT_DATA_LENGTH),
     (MQCACH_MODE_NAME, MQ_MODE_NAME_LENGTH),
     (MQCACH_TP_NAME, MQ_TP_NAME_LENGTH),
     (MQCACH_CONNECTION_NAME, MQ_CONN_NAME_LENGTH),
     (MQCACH_XMIT_Q_NAME, MQ_Q_NAME_LENGTH),
     (MQCACH_MCA_NAME, MQ_MCA_NAME_LENGTH),
     (MQCACH_MCA_USER_ID, MQ_USER_ID_LENGTH),
     (MQCACH_USER_ID, MQ_USER_ID_LENGTH),
     (MQCACH_PASSWORD, MQ_PASSWORD_LENGTH),
     (MQCACH_MR_EXIT_NAME, MQ_EXIT_NAME_LENGTH),
     (MQCACH_MR_EXIT_USER_DATA, MQ_EXIT_DATA_LENGTH),
     (MQCACF_FROM_CHANNEL_NAME, MQ_CHANNEL_NAME_LENGTH),
     (MQCACF_TO_CHANNEL_NAME, MQ_CHANNEL_NAME_LENGTH));

// Results the maximum stringlength of a given parameter.
function GetParamLength(AParam: MQLONG): MQLONG;

var
  // True if a MQ DLL is linked, otherwise False.
  ConnectedToMQDLL: Boolean;
  // Contains the errordescription if linking the MQ DLL didn't succeed.
  ErrorConnectingToMQDLL: string;


// Link the MQ DLL.
function ConnectToMQDLL: Boolean;

// Result a description of a given MQ error code.
function MQErrorReason(ErrorCode: MQLONG): string;

// Results the name of a PCF attribute.
function MQAttributeName(Attribute: MQLONG): string;

// Result the description of a PCF parameter.
function MQValueDescription(Parameter: MQLONG; Value: MQLONG): string;

implementation

uses
  SysUtils;

const
  // Name of the MQ Client DLL.
  MQCLIENTDLL = 'MQIC32.' + SharedSuffix;
  // Name of the MQ local DLL.
  MQLOCALDLL = 'MQM.' + SharedSuffix;

var
  // Handle of dynamical linked MQ DLL.
  DLLHandle: THandle;

function ConnectToMQDLL: Boolean;
var
  LastError: String;
  MQDLLName: string;

  // Load a function address and give an error if failed.
  function CheckProcAddress(AName: PChar): FARPROC;
  begin
    Result := GetProcAddress(DLLHandle, AName);
    if Result = nil then
    begin
      ErrorConnectingToMQDLL := Format('The procedure "%s" in dll "%s" isn''t found.', [AName, MQDLLName]);
      Abort;
    end;
  end;

begin
  // No dialogbox if the DLL isn't found.
  {$ifdef FPC}
  {$else}
  SetErrorMode(SEM_NOOPENFILEERRORBOX);
  {$endif}
  ErrorConnectingToMQDLL := '';

  // Try connecting the Client DLL.
  MQDLLName := MQCLIENTDLL;
  DLLHandle := LoadLibrary(MQCLIENTDLL);

  // If failed then ...
  if DLLHandle = 0 then
  begin
    // Try to connect the local MQ Server DLL if the Client DLL was not found.
    LastError := GetLoadErrorStr;
    if (LastError = 'ERROR_MOD_NOT_FOUND') or (LastError = 'ERROR_DLL_NOT_FOUND') then
    begin
      // Link the MQ Server DLL.
      MQDLLName := MQLOCALDLL;
      DLLHandle := LoadLibrary(MQLOCALDLL);

      // If failed then ...
      if DLLHandle = 0 then
      begin
        // Create an error description.
        LastError := GetLoadErrorStr;
        ErrorConnectingToMQDLL := LastError;
        if (LastError = 'ERROR_MOD_NOT_FOUND') or (LastError = 'ERROR_DLL_NOT_FOUND') then
          ErrorConnectingToMQDLL := 'MQ Client API or local MQ Server not found.'
        else
        begin
          ErrorConnectingToMQDLL := 'Error linking the local MQ Server "' + MQLOCALDLL + '".';
        end
      end
    end
    else
    begin
      ErrorConnectingToMQDLL := 'Error linking the MQ Client API "' + MQCLIENTDLL + '".';
    end;
  end;

  // If a DLL is linked, load the MQ functions from the DLL.
  if DLLHandle <> 0 then
    try
      @MQCONN  := CheckProcAddress('MQCONN');
      @MQDISC  := CheckProcAddress('MQDISC');
      @MQOPEN  := CheckProcAddress('MQOPEN');
      @MQCLOSE := CheckProcAddress('MQCLOSE');
      @MQGET   := CheckProcAddress('MQGET');
      @MQPUT   := CheckProcAddress('MQPUT');
      @MQPUT1  := CheckProcAddress('MQPUT1');
      @MQBACK  := CheckProcAddress('MQBACK');
      @MQCMIT  := CheckProcAddress('MQCMIT');
      @MQINQ   := CheckProcAddress('MQINQ');
      @MQSET   := CheckProcAddress('MQSET');

      // OK, all functions are loaded.
      ConnectedToMQDLL := True;
      Result := True;
    except
      // Loading the functions has failed.
      ConnectedToMQDLL := False;
      Result := False;
    end
  else
  begin
    // Linking a DLL failed.
    ConnectedToMQDLL := False;
    Result := False;
  end;
end;

function MQErrorReason(ErrorCode: MQLONG): string;
begin
  case ErrorCode of
    MQRC_NONE               :
      Result := '(0) No reason to report.';
    MQRC_ALIAS_BASE_Q_TYPE_ERROR :
      Result := '(2001) Alias base queue not a valid type.';
    MQRC_ALREADY_CONNECTED  :
      Result := '(2002) Application already connected.';
    MQRC_BACKED_OUT         :
      Result := '(2003) Unit of work backed out.';
    MQRC_BUFFER_ERROR       :
      Result := '(2004) Buffer parameter not valid.';
    MQRC_BUFFER_LENGTH_ERROR :
      Result := '(2005) Buffer length parameter not valid.';
    MQRC_CONNECTION_BROKEN  :
      Result := '(2009) Connection to queue manager lost.';
    MQRC_DATA_LENGTH_ERROR  :
      Result := '(2010) Data length parameter not valid.';
    MQRC_DYNAMIC_Q_NAME_ERROR :
      Result := '(2011) Name of dynamic queue not valid.';
    MQRC_EXPIRY_ERROR       :
      Result := '(2013) Expiry time not valid.';
    MQRC_FEEDBACK_ERROR     :
      Result := '(2014) Feedback code not valid.';
    MQRC_NO_MSG_LOCKED      :
      Result := '(2209) No message locked.';
    MQRC_CALL_IN_PROGRESS   :
      Result := '(2219) MQ call reentered before previous call complete.';
    MQRC_NO_MSG_AVAILABLE   :
      Result := '(2033) No message available.';
    MQRC_NO_MSG_UNDER_CURSOR  :
      Result := '(2034) Browse cursor not positioned on message.';
    MQRC_NOT_AUTHORIZED     :
      Result := '(2035) Not authorized for access.';
    MQRC_NOT_OPEN_FOR_BROWSE :
      Result := '(2036) Queue not open for browse.';
    MQRC_NOT_OPEN_FOR_INPUT :
      Result := '(2037) Queue not open for input.';
    MQRC_NOT_OPEN_FOR_INQUIRE :
      Result := '(2038) Queue not open for inquire.';
    MQRC_NOT_OPEN_FOR_OUTPUT :
      Result := '(2039) Queue not open for output.';
    MQRC_OBJECT_CHANGED     :
      Result := '(2041) Object definition changed since opened.';
    MQRC_OBJECT_IN_USE      :
      Result := '(2042) Object already open with conflicting options.';
    MQRC_OBJECT_TYPE_ERROR  :
      Result := '(2043) Object type not valid.';
    MQRC_OD_ERROR           :
      Result := '(2044) Object descriptor structure not valid.';
    MQRC_OPTION_NOT_VALID_FOR_TYPE :
      Result := '(2045) Option not valid for object type.';
    MQRC_OPTIONS_ERROR      :
      Result := '(2046) Options not valid or not consistent.';
    MQRC_PERSISTENCE_ERROR  :
      Result := '(2047) Persistence not valid.';
    MQRC_PERSISTENT_NOT_ALLOWED :
      Result := '(2048) Message on a temporary dynamic queue cannot be persistent.';
    MQRC_PRIORITY_EXCEEDS_MAXIMUM :
      Result := '(2049) Message Priority exceeds maximum value supported.';
    MQRC_PRIORITY_ERROR     :
      Result := '(2050) Message priority not valid.';
    MQRC_PUT_INHIBITED      :
      Result := '(2051) Put calls inhibited for the queue.';
    MQRC_Q_DELETED          :
      Result := '(2052) Queue has been deleted.';
    MQRC_Q_FULL             :
      Result := '(2053) Queue already contains maximum number of messages.';
    MQRC_Q_NOT_EMPTY        :
      Result := '(2055) Queue contains 1 or more messages or uncommitted put or get reqts';
    MQRC_Q_SPACE_NOT_AVAILABLE :
      Result := '(2056) No space available on disk for queue.';
    MQRC_Q_TYPE_ERROR       :
      Result := '(2057) Queue type not valid.';
    MQRC_Q_MGR_NAME_ERROR   :
      Result := '(2058) Queue manager name not valid or not known.';
    MQRC_Q_MGR_NOT_AVAILABLE  :
      Result := '(2059) Queue manager not available for connection.';
    MQRC_REPORT_OPTIONS_ERROR :
      Result := '(2061) Report options in message descriptor not valid.';
    MQRC_SECURITY_ERROR     :
      Result := '(2063) Security error occurred.';
    MQRC_Q_MGR_QUIESCING    :
      Result := '(2161) Queue manager quiescing.';
    MQRC_Q_MGR_STOPPING     :
      Result := '(2162) Queue manager shutting down.';
    MQRC_PMO_ERROR          :
      Result := '(2173) Put-message options structure not valid.';
    MQRC_NOT_OPEN_FOR_SET_IDENT :
      Result := '(2096) Queue not open for set identity context.';
    MQRC_CONTEXT_HANDLE_ERROR :
      Result := '(2097) Queue handle referred to does not save context.';
    MQRC_CONTEXT_NOT_AVAILABLE :
      Result := '(2098) Context not available for queue handle referred to.';
    MQRC_OBJECT_ALREADY_EXISTS :
      Result := '(2100) Object already exists.';
    MQRC_OBJECT_DAMAGED     :
      Result := '(2101) Object damaged.';
    MQRC_RESOURCE_PROBLEM   :
      Result := '(2102) Insufficient system resources available.';
    MQRC_UNKNOWN_REPORT_OPTION :
      Result := '(2104) Report option(s) in message descriptor not recognized.';
    MQRC_FORMAT_ERROR       :
      Result := '(2110) Message format not valid.';
    MQRC_SOURCE_CCSID_ERROR :
      Result := '(2111) Source coded character set identifier not valid.';
    MQRC_STORAGE_NOT_AVAILABLE :
      Result := '(2071) Insufficient storage available.';
    MQRC_SYNCPOINT_NOT_AVAILABLE :
      Result := '(2072) Syncpoint support not available.';
    MQRC_TRUNCATED_MSG_ACCEPTED :
      Result := '(2079) Truncated message returned (processing completed).';
    MQRC_NAME_NOT_VALID_FOR_TYPE :
      Result := '(2194) Object name not valid for object type.';
    MQRC_UNEXPECTED_ERROR   :
      Result := '(2195) Unexpected error occurred';
    MQRC_UNKNOWN_XMIT_Q     :
      Result := '(2196) Unknown transmission queue.';
    MQRC_UNKNOWN_DEF_XMIT_Q :
      Result := '(2197) Unknown default transmission queue.';
    MQRC_DEF_XMIT_Q_TYPE_ERROR :
      Result := '(2198) Default transmission queue not local.';
    MQRC_DEF_XMIT_Q_USAGE_ERROR :
      Result := '(2199) Default transmission queue usage error.';
    MQRC_GET_INHIBITED      :
      Result := '(2016) Gets inhibited for the queue.';
    MQRC_HANDLE_NOT_AVAILABLE :
      Result := '(2017) No more handles available.';
    MQRC_HCONN_ERROR        :
      Result := '(2018) Connection handle not valid.';
    MQRC_HOBJ_ERROR         :
      Result := '(2019) Object handle not valid.';
    MQRC_SYNCPOINT_LIMIT_REACHED :
      Result := '(2024) No more messages can be handled within current unit of work.';
    MQRC_MD_ERROR           :
      Result := '(2026) Message descriptor not valid.';
    MQRC_MISSING_REPLY_TO_Q :
      Result := '(2027) Missing reply-to queue.';
    MQRC_MSG_TYPE_ERROR     :
      Result := '(2029) Message type in message descriptor not valid.';
    MQRC_MSG_TOO_BIG_FOR_Q  :
      Result := '(2030) Message length greater than maximum for queue.';
    MQRC_REMOTE_Q_NAME_ERROR :
      Result := '(2184) Remote queue name not valid.';
    MQRC_GMO_ERROR          :
      Result := '(2186) Get-message options structure not valid.';
    MQRC_TRUNCATED_MSG_FAILED :
      Result := '(2080) Truncated message returned (processing not completed).';
    MQRC_UNKNOWN_ALIAS_BASE_Q :
      Result := '(2082) Unknown alias base queue.';
    MQRC_UNKNOWN_OBJECT_NAME :
      Result := '(2085) Unknown object name.';
    MQRC_UNKNOWN_OBJECT_Q_MGR :
      Result := '(2086) Unknown object queue manager.';
    MQRC_UNKNOWN_REMOTE_Q_MGR :
      Result := '(2087) Unknown remote queue manager.';
    MQRC_WAIT_INTERVAL_ERROR :
      Result := '(2090) Wait interval in MQGMO not valid.';
    MQRC_XMIT_Q_TYPE_ERROR  :
      Result := '(2091) Transmission queue not local.';
    MQRC_XMIT_Q_USAGE_ERROR :
      Result := '(2092) Transmission queue with wrong usage.';
    MQRC_NOT_OPEN_FOR_PASS_ALL :
      Result := '(2093) Queue not open for pass all context.';
    MQRC_NOT_OPEN_FOR_PASS_IDENT :
      Result := '(2094) Queue not open for pass identity context.';
    MQRC_NOT_OPEN_FOR_SET_ALL :
      Result := '(2095) Queue not open for set all context.';
    MQRC_SOURCE_INTEGER_ENC_ERROR :
      Result := '(2112) Integer encoding in message not recognized.';
    MQRC_SOURCE_DECIMAL_ENC_ERROR :
      Result := '(2113) Packed-decimal encoding in message not recognized.';
    MQRC_SOURCE_FLOAT_ENC_ERROR :
      Result := '(2114) Floating-point encoding in message not recognized.';
    MQRC_TARGET_CCSID_ERROR :
      Result := '(2115) Target coded character set identifier not valid.';
    MQRC_TARGET_INTEGER_ENC_ERROR :
      Result := '(2116) Integer encoding specified by receiver not recognized.';
    MQRC_TARGET_DECIMAL_ENC_ERROR :
      Result := '(2117) Packed-decimal encoding specified by receiver not recognized.';
    MQRC_TARGET_FLOAT_ENC_ERROR :
      Result := '(2118) Floating-point encoding specified by receiver not recognized.';
    MQRC_NOT_CONVERTED      :
      Result := '(2119) Application message data not converted.';
    MQRC_CONVERTED_MSG_TOO_BIG :
      Result := '(2120) Converted message too big for application buffer.';
    MQRC_UOW_CANCELED : result := '(2297) MQRC_UOW_CANCELED' ;
    MQRC_DLH_ERROR : result := '(2141) MQRC_DLH_ERROR' ;
    MQRC_HEADER_ERROR : result := '(2142) MQRC_HEADER_ERROR' ;
    MQRC_FUNCTION_NOT_SUPPORTED : result := '(2298 MQRC_FUNCTION_NOT_SUPPORTED' ;
    MQRC_SELECTOR_TYPE_ERROR : result := '(2299) MQRC_SELECTOR_TYPE_ERROR' ;
    MQRC_COMMAND_TYPE_ERROR : result := '(2300) MQRC_COMMAND_TYPE_ERROR' ;
    MQRC_MULTIPLE_INSTANCE_ERROR : result := '(2301) MQRC_MULTIPLE_INSTANCE_ERROR' ;
    MQRC_SYSTEM_ITEM_NOT_ALTERABLE : result := '(2302) MQRC_SYSTEM_ITEM_NOT_ALTERABLE' ;
    MQRC_BAG_CONVERSION_ERROR : result := '(2303) MQRC_BAG_CONVERSION_ERROR' ;
    MQRC_SELECTOR_OUT_OF_RANGE : result := '(2304) MQRC_SELECTOR_OUT_OF_RANGE' ;
    MQRC_SELECTOR_NOT_UNIQUE : result := '(2305) MQRC_SELECTOR_NOT_UNIQUE' ;
    MQRC_INDEX_NOT_PRESENT : result := '(2306) MQRC_INDEX_NOT_PRESENT' ;
    MQRC_STRING_ERROR : result := '(2307) MQRC_STRING_ERROR' ;
    MQRC_ENCODING_NOT_SUPPORTED : result := '(2308) MQRC_ENCODING_NOT_SUPPORTED' ;
    MQRC_SELECTOR_NOT_PRESENT : result := '(2309) MQRC_SELECTOR_NOT_PRESENT' ;
    MQRC_OUT_SELECTOR_ERROR : result := '(2310) MQRC_OUT_SELECTOR_ERROR' ;
    else
      Result := '('+IntToStr(ErrorCode)+') Unknown error occurred.';
  end;
end;

function MQAttributeName(Attribute: MQLONG): string;
begin
  case Attribute of
    MQCA_APPL_ID: Result := S_MQCA_APPL_ID;
    MQCA_BACKOUT_REQ_Q_NAME: Result := S_MQCA_BACKOUT_REQ_Q_NAME;
    MQCA_BASE_Q_NAME: Result := S_MQCA_BASE_Q_NAME;
    MQCA_COMMAND_INPUT_Q_NAME: Result := S_MQCA_COMMAND_INPUT_Q_NAME;
    MQCA_CREATION_DATE: Result := S_MQCA_CREATION_DATE;
    MQCA_CREATION_TIME: Result := S_MQCA_CREATION_TIME;
    MQCA_DEAD_LETTER_Q_NAME: Result := S_MQCA_DEAD_LETTER_Q_NAME;
    MQCA_DEF_XMIT_Q_NAME: Result := S_MQCA_DEF_XMIT_Q_NAME;
    MQCA_ENV_DATA: Result := S_MQCA_ENV_DATA;
    MQCA_INITIATION_Q_NAME: Result := S_MQCA_INITIATION_Q_NAME;
    MQCA_NAMES: Result := S_MQCA_NAMES;
    MQCA_PROCESS_DESC: Result := S_MQCA_PROCESS_DESC;
    MQCA_PROCESS_NAME: Result := S_MQCA_PROCESS_NAME;
    MQCA_Q_DESC: Result := S_MQCA_Q_DESC;
    MQCA_Q_MGR_DESC: Result := S_MQCA_Q_MGR_DESC;
    MQCA_Q_MGR_NAME: Result := S_MQCA_Q_MGR_NAME;
    MQCA_Q_NAME: Result := S_MQCA_Q_NAME;
    MQCA_REMOTE_Q_MGR_NAME: Result := S_MQCA_REMOTE_Q_MGR_NAME;
    MQCA_REMOTE_Q_NAME: Result := S_MQCA_REMOTE_Q_NAME;
    MQCA_STORAGE_CLASS: Result := S_MQCA_STORAGE_CLASS;
    MQCA_TRIGGER_DATA: Result := S_MQCA_TRIGGER_DATA;
    MQCA_USER_DATA: Result := S_MQCA_USER_DATA;
    MQCA_XMIT_Q_NAME: Result := S_MQCA_XMIT_Q_NAME;
    MQIA_APPL_TYPE: Result := S_MQIA_APPL_TYPE;
    MQIA_AUTHORITY_EVENT: Result := S_MQIA_AUTHORITY_EVENT;
    MQIA_BACKOUT_THRESHOLD: Result := S_MQIA_BACKOUT_THRESHOLD;
    MQIA_CODED_CHAR_SET_ID: Result := S_MQIA_CODED_CHAR_SET_ID;
    MQIA_COMMAND_LEVEL: Result := S_MQIA_COMMAND_LEVEL;
    MQIA_CURRENT_Q_DEPTH: Result := S_MQIA_CURRENT_Q_DEPTH;
    MQIA_DEF_INPUT_OPEN_OPTION: Result := S_MQIA_DEF_INPUT_OPEN_OPTION;
    MQIA_DEF_PERSISTENCE: Result := S_MQIA_DEF_PERSISTENCE;
    MQIA_DEF_PRIORITY: Result := S_MQIA_DEF_PRIORITY;
    MQIA_DEFINITION_TYPE: Result := S_MQIA_DEFINITION_TYPE;
    MQIA_HARDEN_GET_BACKOUT: Result := S_MQIA_HARDEN_GET_BACKOUT;
    MQIA_HIGH_Q_DEPTH: Result := S_MQIA_HIGH_Q_DEPTH;
    MQIA_INHIBIT_EVENT: Result := S_MQIA_INHIBIT_EVENT;
    MQIA_INHIBIT_GET: Result := S_MQIA_INHIBIT_GET;
    MQIA_INHIBIT_PUT: Result := S_MQIA_INHIBIT_PUT;
    MQIA_LOCAL_EVENT: Result := S_MQIA_LOCAL_EVENT;
    MQIA_MAX_HANDLES: Result := S_MQIA_MAX_HANDLES;
    MQIA_MAX_MSG_LENGTH: Result := S_MQIA_MAX_MSG_LENGTH;
    MQIA_MAX_PRIORITY: Result := S_MQIA_MAX_PRIORITY;
    MQIA_MAX_Q_DEPTH: Result := S_MQIA_MAX_Q_DEPTH;
    MQIA_MAX_UNCOMMITTED_MSGS: Result := S_MQIA_MAX_UNCOMMITTED_MSGS;
    MQIA_MSG_DELIVERY_SEQUENCE: Result := S_MQIA_MSG_DELIVERY_SEQUENCE;
    MQIA_MSG_DEQ_COUNT: Result := S_MQIA_MSG_DEQ_COUNT;
    MQIA_MSG_ENQ_COUNT: Result := S_MQIA_MSG_ENQ_COUNT;
    MQIA_NAME_COUNT: Result := S_MQIA_NAME_COUNT;
    MQIA_OPEN_INPUT_COUNT: Result := S_MQIA_OPEN_INPUT_COUNT;
    MQIA_OPEN_OUTPUT_COUNT: Result := S_MQIA_OPEN_OUTPUT_COUNT;
    MQIA_PERFORMANCE_EVENT: Result := S_MQIA_PERFORMANCE_EVENT;
    MQIA_PLATFORM: Result := S_MQIA_PLATFORM;
    MQIA_Q_DEPTH_HIGH_EVENT: Result := S_MQIA_Q_DEPTH_HIGH_EVENT;
    MQIA_Q_DEPTH_HIGH_LIMIT: Result := S_MQIA_Q_DEPTH_HIGH_LIMIT;
    MQIA_Q_DEPTH_LOW_EVENT: Result := S_MQIA_Q_DEPTH_LOW_EVENT;
    MQIA_Q_DEPTH_LOW_LIMIT: Result := S_MQIA_Q_DEPTH_LOW_LIMIT;
    MQIA_Q_DEPTH_MAX_EVENT: Result := S_MQIA_Q_DEPTH_MAX_EVENT;
    MQIA_Q_SERVICE_INTERVAL: Result := S_MQIA_Q_SERVICE_INTERVAL;
    MQIA_Q_SERVICE_INTERVAL_EVENT: Result := S_MQIA_Q_SERVICE_INTERVAL_EVENT;
    MQIA_Q_TYPE: Result := S_MQIA_Q_TYPE;
    MQIA_REMOTE_EVENT: Result := S_MQIA_REMOTE_EVENT;
    MQIA_RETENTION_INTERVAL: Result := S_MQIA_RETENTION_INTERVAL;
    MQIA_SCOPE: Result := S_MQIA_SCOPE;
    MQIA_SHAREABILITY: Result := S_MQIA_SHAREABILITY;
    MQIA_START_STOP_EVENT: Result := S_MQIA_START_STOP_EVENT;
    MQIA_SYNCPOINT: Result := S_MQIA_SYNCPOINT;
    MQIA_TIME_SINCE_RESET: Result := S_MQIA_TIME_SINCE_RESET;
    MQIA_TRIGGER_CONTROL: Result := S_MQIA_TRIGGER_CONTROL;
    MQIA_TRIGGER_DEPTH: Result := S_MQIA_TRIGGER_DEPTH;
    MQIA_TRIGGER_INTERVAL: Result := S_MQIA_TRIGGER_INTERVAL;
    MQIA_TRIGGER_MSG_PRIORITY: Result := S_MQIA_TRIGGER_MSG_PRIORITY;
    MQIA_TRIGGER_TYPE: Result := S_MQIA_TRIGGER_TYPE;
    MQIA_USAGE: Result := S_MQIA_USAGE;
    MQIAV_NOT_APPLICABLE: Result := S_MQIAV_NOT_APPLICABLE;
    MQIACF_Q_MGR_ATTRS: Result := S_MQIACF_Q_MGR_ATTRS;
    MQIACF_Q_ATTRS: Result := S_MQIACF_Q_ATTRS;
    MQIACF_PROCESS_ATTRS: Result := S_MQIACF_PROCESS_ATTRS;
    MQIACF_FORCE: Result := S_MQIACF_FORCE;
    MQIACF_REPLACE: Result := S_MQIACF_REPLACE;
    MQIACF_PURGE: Result := S_MQIACF_PURGE;
    MQIACF_QUIESCE: Result := S_MQIACF_QUIESCE;
    MQIACF_ALL: Result := S_MQIACF_ALL;
    MQIACF_PARAMETER_ID: Result := S_MQIACF_PARAMETER_ID;
    MQIACF_ERROR_ID: Result := S_MQIACF_ERROR_ID;
    MQIACF_SELECTOR: Result := S_MQIACF_SELECTOR;
    MQIACF_CHANNEL_ATTRS: Result := S_MQIACF_CHANNEL_ATTRS;
    MQIACF_ESCAPE_TYPE: Result := S_MQIACF_ESCAPE_TYPE;
    MQIACF_ERROR_OFFSET: Result := S_MQIACF_ERROR_OFFSET;
    MQIACF_REASON_QUALIFIER: Result := S_MQIACF_REASON_QUALIFIER;
    MQIACF_COMMAND: Result := S_MQIACF_COMMAND;
    MQIACF_OPEN_OPTIONS: Result := S_MQIACF_OPEN_OPTIONS;
    MQIACF_AUX_ERROR_DATA_INT_1: Result := S_MQIACF_AUX_ERROR_DATA_INT_1;
    MQIACF_AUX_ERROR_DATA_INT_2: Result := S_MQIACF_AUX_ERROR_DATA_INT_2;
    MQIACF_CONV_REASON_CODE: Result := S_MQIACF_CONV_REASON_CODE;
    MQIACH_XMIT_PROTOCOL_TYPE: Result := S_MQIACH_XMIT_PROTOCOL_TYPE;
    MQIACH_BATCH_SIZE: Result := S_MQIACH_BATCH_SIZE;
    MQIACH_DISC_INTERVAL: Result := S_MQIACH_DISC_INTERVAL;
    MQIACH_SHORT_TIMER: Result := S_MQIACH_SHORT_TIMER;
    MQIACH_SHORT_RETRY: Result := S_MQIACH_SHORT_RETRY;
    MQIACH_LONG_TIMER: Result := S_MQIACH_LONG_TIMER;
    MQIACH_LONG_RETRY: Result := S_MQIACH_LONG_RETRY;
    MQIACH_PUT_AUTHORITY: Result := S_MQIACH_PUT_AUTHORITY;
    MQIACH_SEQUENCE_NUMBER_WRAP: Result := S_MQIACH_SEQUENCE_NUMBER_WRAP;
    MQIACH_MAX_MSG_LENGTH: Result := S_MQIACH_MAX_MSG_LENGTH;
    MQIACH_CHANNEL_TYPE: Result := S_MQIACH_CHANNEL_TYPE;
    MQIACH_DATA_COUNT: Result := S_MQIACH_DATA_COUNT;
    MQIACH_MSG_SEQUENCE_NUMBER: Result := S_MQIACH_MSG_SEQUENCE_NUMBER;
    MQIACH_DATA_CONVERSION: Result := S_MQIACH_DATA_CONVERSION;
    MQIACH_IN_DOUBT: Result := S_MQIACH_IN_DOUBT;
    MQIACH_MCA_TYPE: Result := S_MQIACH_MCA_TYPE;
    MQIACH_CHANNEL_INSTANCE_TYPE: Result := S_MQIACH_CHANNEL_INSTANCE_TYPE;
    MQIACH_CHANNEL_INSTANCE_ATTRS: Result := S_MQIACH_CHANNEL_INSTANCE_ATTRS;
    MQIACH_CHANNEL_ERROR_DATA: Result := S_MQIACH_CHANNEL_ERROR_DATA;
    MQIACH_CHANNEL_TABLE: Result := S_MQIACH_CHANNEL_TABLE;
    MQIACH_CHANNEL_STATUS: Result := S_MQIACH_CHANNEL_STATUS;
    MQIACH_INDOUBT_STATUS: Result := S_MQIACH_INDOUBT_STATUS;
    MQIACH_LAST_SEQUENCE_NUMBER: Result := S_MQIACH_LAST_SEQUENCE_NUMBER;
    MQIACH_CURRENT_MSGS: Result := S_MQIACH_CURRENT_MSGS;
    MQIACH_CURRENT_SEQUENCE_NUMBER: Result := S_MQIACH_CURRENT_SEQUENCE_NUMBER;
    MQIACH_MSGS: Result := S_MQIACH_MSGS;
    MQIACH_BYTES_SENT: Result := S_MQIACH_BYTES_SENT;
    MQIACH_BYTES_RECEIVED: Result := S_MQIACH_BYTES_RECEIVED;
    MQIACH_BATCHES: Result := S_MQIACH_BATCHES;
    MQIACH_BUFFERS_SENT: Result := S_MQIACH_BUFFERS_SENT;
    MQIACH_BUFFERS_RECEIVED: Result := S_MQIACH_BUFFERS_RECEIVED;
    MQIACH_LONG_RETRIES_LEFT: Result := S_MQIACH_LONG_RETRIES_LEFT;
    MQIACH_SHORT_RETRIES_LEFT: Result := S_MQIACH_SHORT_RETRIES_LEFT;
    MQIACH_MCA_STATUS: Result := S_MQIACH_MCA_STATUS;
    MQIACH_STOP_REQUESTED: Result := S_MQIACH_STOP_REQUESTED;
    MQIACH_MR_COUNT: Result := S_MQIACH_MR_COUNT;
    MQIACH_MR_INTERVAL: Result := S_MQIACH_MR_INTERVAL;
    MQCACF_FROM_Q_NAME: Result := S_MQCACF_FROM_Q_NAME;
    MQCACF_TO_Q_NAME: Result := S_MQCACF_TO_Q_NAME;
    MQCACF_FROM_PROCESS_NAME: Result := S_MQCACF_FROM_PROCESS_NAME;
    MQCACF_TO_PROCESS_NAME: Result := S_MQCACF_TO_PROCESS_NAME;
    MQCACF_FROM_CHANNEL_NAME: Result := S_MQCACF_FROM_CHANNEL_NAME;
    MQCACF_TO_CHANNEL_NAME: Result := S_MQCACF_TO_CHANNEL_NAME;
    MQCACF_Q_NAMES: Result := S_MQCACF_Q_NAMES;
    MQCACF_PROCESS_NAMES: Result := S_MQCACF_PROCESS_NAMES;
    MQCACF_ESCAPE_TEXT: Result := S_MQCACF_ESCAPE_TEXT;
    MQCACF_LOCAL_Q_NAMES: Result := S_MQCACF_LOCAL_Q_NAMES;
    MQCACF_MODEL_Q_NAMES: Result := S_MQCACF_MODEL_Q_NAMES;
    MQCACF_ALIAS_Q_NAMES: Result := S_MQCACF_ALIAS_Q_NAMES;
    MQCACF_REMOTE_Q_NAMES: Result := S_MQCACF_REMOTE_Q_NAMES;
    MQCACF_SENDER_CHANNEL_NAMES: Result := S_MQCACF_SENDER_CHANNEL_NAMES;
    MQCACF_SERVER_CHANNEL_NAMES: Result := S_MQCACF_SERVER_CHANNEL_NAMES;
    MQCACF_REQUESTER_CHANNEL_NAMES: Result := S_MQCACF_REQUESTER_CHANNEL_NAMES;
    MQCACF_RECEIVER_CHANNEL_NAMES: Result := S_MQCACF_RECEIVER_CHANNEL_NAMES;
    MQCACF_OBJECT_Q_MGR_NAME: Result := S_MQCACF_OBJECT_Q_MGR_NAME;
    MQCACF_APPL_NAME: Result := S_MQCACF_APPL_NAME;
    MQCACF_USER_IDENTIFIER: Result := S_MQCACF_USER_IDENTIFIER;
    MQCACF_AUX_ERROR_DATA_STR_1: Result := S_MQCACF_AUX_ERROR_DATA_STR_1;
    MQCACF_AUX_ERROR_DATA_STR_2: Result := S_MQCACF_AUX_ERROR_DATA_STR_2;
    MQCACF_AUX_ERROR_DATA_STR_3: Result := S_MQCACF_AUX_ERROR_DATA_STR_3;
    MQCACH_CHANNEL_NAME: Result := S_MQCACH_CHANNEL_NAME;
    MQCACH_DESC: Result := S_MQCACH_DESC;
    MQCACH_MODE_NAME: Result := S_MQCACH_MODE_NAME;
    MQCACH_TP_NAME: Result := S_MQCACH_TP_NAME;
    MQCACH_XMIT_Q_NAME: Result := S_MQCACH_XMIT_Q_NAME;
    MQCACH_CONNECTION_NAME: Result := S_MQCACH_CONNECTION_NAME;
    MQCACH_MCA_NAME: Result := S_MQCACH_MCA_NAME;
    MQCACH_SEC_EXIT_NAME: Result := S_MQCACH_SEC_EXIT_NAME;
    MQCACH_MSG_EXIT_NAME: Result := S_MQCACH_MSG_EXIT_NAME;
    MQCACH_SEND_EXIT_NAME: Result := S_MQCACH_SEND_EXIT_NAME;
    MQCACH_RCV_EXIT_NAME: Result := S_MQCACH_RCV_EXIT_NAME;
    MQCACH_CHANNEL_NAMES: Result := S_MQCACH_CHANNEL_NAMES;
    MQCACH_SEC_EXIT_USER_DATA: Result := S_MQCACH_SEC_EXIT_USER_DATA;
    MQCACH_MSG_EXIT_USER_DATA: Result := S_MQCACH_MSG_EXIT_USER_DATA;
    MQCACH_SEND_EXIT_USER_DATA: Result := S_MQCACH_SEND_EXIT_USER_DATA;
    MQCACH_RCV_EXIT_USER_DATA: Result := S_MQCACH_RCV_EXIT_USER_DATA;
    MQCACH_USER_ID: Result := S_MQCACH_USER_ID;
    MQCACH_PASSWORD: Result := S_MQCACH_PASSWORD;
    MQCACH_LAST_MSG_TIME: Result := S_MQCACH_LAST_MSG_TIME;
    MQCACH_LAST_MSG_DATE: Result := S_MQCACH_LAST_MSG_DATE;
    MQCACH_MCA_USER_ID: Result := S_MQCACH_MCA_USER_ID;
    MQCACH_CHANNEL_START_TIME: Result := S_MQCACH_CHANNEL_START_TIME;
    MQCACH_CHANNEL_START_DATE: Result := S_MQCACH_CHANNEL_START_DATE;
    MQCACH_MCA_JOB_NAME: Result := S_MQCACH_MCA_JOB_NAME;
    MQCACH_LAST_LUWID: Result := S_MQCACH_LAST_LUWID;
    MQCACH_CURRENT_LUWID: Result := S_MQCACH_CURRENT_LUWID;
    MQCACH_FORMAT_NAME: Result := S_MQCACH_FORMAT_NAME;
    MQCACH_MR_EXIT_NAME: Result := S_MQCACH_MR_EXIT_NAME;
    MQCACH_MR_EXIT_USER_DATA: Result := S_MQCACH_MR_EXIT_USER_DATA;
  else
    Result := '';
  end;    // case
end;

function MQValueDescription(Parameter: MQLONG; Value: MQLONG): string;
begin
  Result := IntToStr(Value);
  case Parameter of
    MQIA_PLATFORM:
      case Value of
        MQPL_MVS: Result := S_MQPL_MVS;
        MQPL_OS2: Result := S_MQPL_OS2;
        MQPL_OS400: Result := S_MQPL_OS400;
        MQPL_UNIX: Result := S_MQPL_UNIX;
        MQPL_WINDOWS_NT: Result := S_MQPL_WINDOWS_NT;
      end;
    MQIA_COMMAND_LEVEL:
      Result := Format(S_MQCMDL_LEVEL, [Value div 100, (Value mod 100) div 10, Value mod 10]);
    MQIA_SYNCPOINT:
      case Value of
        MQSP_AVAILABLE: Result := S_MQSP_AVAILABLE;
        MQSP_NOT_AVAILABLE: Result := S_MQSP_NOT_AVAILABLE;
      end;    // case
    MQIA_AUTHORITY_EVENT,
    MQIA_INHIBIT_EVENT,
    MQIA_LOCAL_EVENT,
    MQIA_REMOTE_EVENT,
    MQIA_START_STOP_EVENT,
    MQIA_PERFORMANCE_EVENT,
    MQIA_Q_DEPTH_MAX_EVENT,
    MQIA_Q_DEPTH_HIGH_EVENT,
    MQIA_Q_DEPTH_LOW_EVENT:
      case Value of
        MQEVR_DISABLED: Result := S_MQEVR_DISABLED;
        MQEVR_ENABLED: Result := S_MQEVR_ENABLED;
      end;    // case
    MQIA_Q_TYPE:
      case Value of
        MQQT_LOCAL: Result := S_MQQT_LOCAL;
        MQQT_MODEL: Result := S_MQQT_MODEL;
        MQQT_ALIAS: Result := S_MQQT_ALIAS;
        MQQT_REMOTE: Result := S_MQQT_REMOTE;
      end;    // case
    MQIA_INHIBIT_PUT:
      case Value of
        MQQA_PUT_ALLOWED: Result := S_MQQA_PUT_ALLOWED;
        MQQA_PUT_INHIBITED: Result := S_MQQA_PUT_INHIBITED;
      end;    // case
    MQIA_DEF_PERSISTENCE:
      case Value of
        MQPER_PERSISTENT: Result := S_MQPER_PERSISTENT;
        MQPER_NOT_PERSISTENT: Result := S_MQPER_NOT_PERSISTENT;
      end;    // case
    MQIA_SCOPE:
      case Value of
        MQSCO_Q_MGR: Result := S_MQSCO_Q_MGR;
        MQSCO_CELL: Result := S_MQSCO_CELL;
      end;    // case
    MQIA_INHIBIT_GET:
      case Value of
        MQQA_GET_ALLOWED: Result := S_MQQA_GET_ALLOWED;
        MQQA_GET_INHIBITED: Result := S_MQQA_GET_INHIBITED;
      end;    // case
    MQIA_SHAREABILITY:
      case Value of
        MQQA_SHAREABLE: Result := S_MQQA_SHAREABLE;
        MQQA_NOT_SHAREABLE: Result := S_MQQA_NOT_SHAREABLE;
      end;    // case
    MQIA_DEF_INPUT_OPEN_OPTION:
      case Value of
        MQOO_INPUT_EXCLUSIVE: Result := S_MQOO_INPUT_EXCLUSIVE;
        MQOO_INPUT_SHARED: Result := S_MQOO_INPUT_SHARED;
      end;    // case
    MQIA_HARDEN_GET_BACKOUT:
      case Value of
        MQQA_BACKOUT_HARDENED: Result := S_MQQA_BACKOUT_HARDENED;
        MQQA_BACKOUT_NOT_HARDENED: Result := S_MQQA_BACKOUT_NOT_HARDENED;
      end;    // case
    MQIA_MSG_DELIVERY_SEQUENCE:
      case Value of
        MQMDS_PRIORITY: Result := S_MQMDS_PRIORITY;
        MQMDS_FIFO: Result := S_MQMDS_FIFO;
      end;    // case
    MQIA_DEFINITION_TYPE:
      case Value of
        MQQDT_PREDEFINED: Result := S_MQQDT_PREDEFINED;
        MQQDT_PERMANENT_DYNAMIC: Result := S_MQQDT_PERMANENT_DYNAMIC;
        MQQDT_TEMPORARY_DYNAMIC: Result := S_MQQDT_TEMPORARY_DYNAMIC;
      end;    // case
    MQIA_USAGE:
      case Value of
        MQUS_NORMAL: Result := S_MQUS_NORMAL;
        MQUS_TRANSMISSION: Result := S_MQUS_TRANSMISSION;
      end;    // case
    MQIA_TRIGGER_CONTROL:
      case Value of
        MQTC_OFF: Result := S_MQTC_OFF;
        MQTC_ON: Result := S_MQTC_ON;
      end;    // case
    MQIA_TRIGGER_TYPE:
      case Value of
        MQTT_NONE: Result := S_MQTT_NONE;
        MQTT_FIRST: Result := S_MQTT_FIRST;
        MQTT_EVERY: Result := S_MQTT_EVERY;
        MQTT_DEPTH: Result := S_MQTT_DEPTH;
      end;    // case
    MQIA_Q_SERVICE_INTERVAL_EVENT:
      case Value of
        MQQSIE_HIGH: Result := S_MQQSIE_HIGH;
        MQQSIE_OK: Result := S_MQQSIE_OK;
        MQQSIE_NONE: Result := S_MQQSIE_NONE;
      end;    // case
    MQIACH_CHANNEL_INSTANCE_TYPE:
      case Value of
        MQOT_SAVED_CHANNEL: Result := S_MQOT_SAVED_CHANNEL;
        MQOT_CURRENT_CHANNEL: Result := S_MQOT_CURRENT_CHANNEL;
      end;    // case
    MQIACH_INDOUBT_STATUS:
      case Value of
        MQCHIDS_NOT_INDOUBT: Result := S_MQCHIDS_NOT_INDOUBT;
        MQCHIDS_INDOUBT: Result := S_MQCHIDS_INDOUBT;
      end;    // case
    MQIACH_CHANNEL_STATUS:
      case Value of
        MQCHS_BINDING: Result := S_MQCHS_BINDING;
        MQCHS_STARTING: Result := S_MQCHS_STARTING;
        MQCHS_RUNNING: Result := S_MQCHS_RUNNING;
        MQCHS_PAUSED: Result := S_MQCHS_PAUSED;
        MQCHS_STOPPING: Result := S_MQCHS_STOPPING;
        MQCHS_RETRYING: Result := S_MQCHS_RETRYING;
        MQCHS_STOPPED: Result := S_MQCHS_STOPPED;
        MQCHS_REQUESTING: Result := S_MQCHS_REQUESTING;
      end;    // case
    MQIACH_MCA_STATUS:
      case Value of
        MQMCAS_STOPPED: Result := S_MQMCAS_STOPPED;
        MQMCAS_RUNNING: Result := S_MQMCAS_RUNNING;
      end;    // case
    MQIACH_STOP_REQUESTED:
      case Value of
        MQCHSR_STOP_NOT_REQUESTED: Result := S_MQCHSR_STOP_NOT_REQUESTED;
        MQCHSR_STOP_REQUESTED: Result := S_MQCHSR_STOP_REQUESTED;
      end;    // case
    MQIACH_CHANNEL_TYPE:
      case Value of
        MQCHT_SENDER: Result := S_MQCHT_SENDER;
        MQCHT_SERVER: Result := S_MQCHT_SERVER;
        MQCHT_RECEIVER: Result := S_MQCHT_RECEIVER;
        MQCHT_REQUESTER: Result := S_MQCHT_REQUESTER;
        MQCHT_SVRCONN: Result := S_MQCHT_SVRCONN;
        MQCHT_CLNTCONN: Result := S_MQCHT_CLNTCONN;
      end;    // case
    MQIACH_XMIT_PROTOCOL_TYPE:
      case Value of
        MQXPT_LU62: Result := S_MQXPT_LU62;
        MQXPT_TCP: Result := S_MQXPT_TCP;
        MQXPT_NETBIOS: Result := S_MQXPT_NETBIOS;
      end;    // case
    MQIACH_MCA_TYPE:
      case Value of
        MQMCAT_PROCESS: Result := S_MQMCAT_PROCESS;
        MQMCAT_THREAD: Result := S_MQMCAT_THREAD;
      end;    // case
    MQIACH_DATA_CONVERSION:
      case Value of
        MQCDC_NO_SENDER_CONVERSION: Result := S_MQCDC_NO_SENDER_CONVERSION;
        MQCDC_SENDER_CONVERSION: Result := S_MQCDC_SENDER_CONVERSION;
      end;    // case
    MQIACH_PUT_AUTHORITY:
      case Value of
        MQPA_DEFAULT: Result := S_MQPA_DEFAULT;
        MQPA_CONTEXT: Result := S_MQPA_CONTEXT;
      end;    // case
    MQIA_APPL_TYPE:
      case Value of
        MQAT_OS400: Result := S_MQAT_OS400;
        MQAT_OS2: Result := S_MQAT_OS2;
        MQAT_WINDOWS_NT: Result := S_MQAT_WINDOWS_NT;
        MQAT_DOS: Result := S_MQAT_DOS;
        MQAT_WINDOWS: Result := S_MQAT_WINDOWS;
        MQAT_UNIX: Result := S_MQAT_UNIX;
        MQAT_CICS: Result := S_MQAT_CICS;
      end;    // case
    MQIACF_FORCE:
      case Value of
        MQFC_YES: Result := S_MQFC_YES;
        MQFC_NO: Result := S_MQFC_NO;
      end;    // case
    MQIACF_REPLACE:
      case Value of
        MQRP_YES: Result := S_MQRP_YES;
        MQRP_NO: Result := S_MQRP_NO;
      end;    // case
  end;    // case
end;

function GetParamLength(AParam: MQLONG): MQLONG;
var
  iIndex: integer;
begin
  Result := -1;
  for iIndex := 0 to ParamLengthCount-1 do
    if ParamLength[iIndex,0] = AParam then
    begin
      Result := ParamLength[iIndex,1];
      Break;
    end;
end;

end.

