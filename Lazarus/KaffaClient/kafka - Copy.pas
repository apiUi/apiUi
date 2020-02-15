unit Kafka;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, winsock;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
//  RD_KAFKA_VERSION_STRING  = $000905ff;
  RD_KAFKA_VERSION_STRING  = $000b00ff;

  RD_KAFKA_PARTITION_UA  = -1;

  RD_KAFKA_MSG_F_FREE  = $1; // Delegate freeing of payload to rdkafka.
  RD_KAFKA_MSG_F_COPY  = $2; // rdkafka will make a copy of the payload.
  RD_KAFKA_MSG_F_BLOCK = $4; // Block produce*() on message queue full.
            (*
  				  *   WARNING: If a delivery report callback
  				  *            is used the application MUST
  				  *            call rd_kafka_poll() (or equiv.)
  				  *            to make sure delivered messages
  				  *            are drained from the internal
  				  *            delivery report queue.
  				  *            Failure to do so will result
  				  *            in indefinately blocking on
  				  *            the produce() call when the
  				  *            message queue is full.
  				  *)

type
  Prd_kafka_resp_err_t =  ^Trd_kafka_resp_err_t;
  Trd_kafka_resp_err_t =  Longint;

  Prd_kafka_conf_t = ^Trd_kafka_conf_t;
  Trd_kafka_conf_t = Pointer;
  Trd_kafka_conf_s = Trd_kafka_conf_t;

  Prd_kafka_conf_res_t = ^Trd_kafka_conf_res_t;
  Trd_kafka_conf_res_t =  Longint;

  Prd_kafka_t = ^Trd_kafka_t;
  Trd_kafka_t = Pointer;
  Trd_kafka_s = Trd_kafka_t;

  Prd_kafka_topic_t  = ^Trd_kafka_topic_t;
  Trd_kafka_topic_t = Pointer;

  Prd_kafka_type_t  = ^Trd_kafka_type_t;
  Trd_kafka_type_t = Longint;

  Prd_kafka_queue_t = ^Trd_kafka_queue_t;
  Trd_kafka_queue_t = Pointer;
  Trd_kafka_queue_s = Trd_kafka_queue_t;

  Prd_kafka_event_t = ^Trd_kafka_event_t;
  Trd_kafka_event_t = Pointer;
  Trd_kafka_op_s = Trd_kafka_event_t;

  Prd_kafka_event_type_t = ^Trd_kafka_event_type_t;
  Trd_kafka_event_type_t = Longint;



  (*
  Prd_kafka_topic_partition_t = ^Trd_kafka_topic_partition_t;
  Trd_kafka_topic_partition_t = record
    topic : Pchar;
    partition : int32_t;
    offset : int64_t;
    metadata : pointer;
    metadata_size : size_t;
    opaque : pointer;
    err : rd_kafka_resp_err_t;
    _private : pointer;
  end;


  Prd_kafka_topic_partition_list_t = ^Trd_kafka_topic_partition_list_t;
  Trd_kafka_topic_partition_list_t = record
    cnt  : longint;
    size : longint;
    elems: Prd_kafka_topic_partition_t;
  end;
  *)



const
  RD_EXPORT = 'librdkafka.dll';

Const
  RD_KAFKA_TIMESTAMP_NOT_AVAILABLE = 0;
  RD_KAFKA_TIMESTAMP_CREATE_TIME = 1;
  RD_KAFKA_TIMESTAMP_LOG_APPEND_TIME = 2;

  RD_KAFKA_CONF_UNKNOWN = -(2);
  RD_KAFKA_CONF_INVALID = -(1);
  RD_KAFKA_CONF_OK = 0;

  RD_KAFKA_PRODUCER = 0;
  RD_KAFKA_CONSUMER = 1;

  RD_KAFKA_VTYPE_END = 0;
  RD_KAFKA_VTYPE_TOPIC = 1;
  RD_KAFKA_VTYPE_RKT = 2;
  RD_KAFKA_VTYPE_PARTITION = 3;
  RD_KAFKA_VTYPE_VALUE = 4;
  RD_KAFKA_VTYPE_KEY = 5;
  RD_KAFKA_VTYPE_OPAQUE = 6;
  RD_KAFKA_VTYPE_MSGFLAGS = 7;
  RD_KAFKA_VTYPE_TIMESTAMP = 8;


  RD_KAFKA_RESP_ERR__BEGIN = -(200);
  RD_KAFKA_RESP_ERR__BAD_MSG = -(199);
  RD_KAFKA_RESP_ERR__BAD_COMPRESSION = -(198);
  RD_KAFKA_RESP_ERR__DESTROY = -(197);
  RD_KAFKA_RESP_ERR__FAIL = -(196);
  RD_KAFKA_RESP_ERR__TRANSPORT = -(195);
  RD_KAFKA_RESP_ERR__CRIT_SYS_RESOURCE = -(194);
  RD_KAFKA_RESP_ERR__RESOLVE = -(193);
  RD_KAFKA_RESP_ERR__MSG_TIMED_OUT = -(192);
  RD_KAFKA_RESP_ERR__PARTITION_EOF = -(191);
  RD_KAFKA_RESP_ERR__UNKNOWN_PARTITION = -(190);
  RD_KAFKA_RESP_ERR__FS = -(189);
  RD_KAFKA_RESP_ERR__UNKNOWN_TOPIC = -(188);
  RD_KAFKA_RESP_ERR__ALL_BROKERS_DOWN = -(187);
  RD_KAFKA_RESP_ERR__INVALID_ARG = -(186);
  RD_KAFKA_RESP_ERR__TIMED_OUT = -(185);
  RD_KAFKA_RESP_ERR__QUEUE_FULL = -(184);
  RD_KAFKA_RESP_ERR__ISR_INSUFF = -(183);
  RD_KAFKA_RESP_ERR__NODE_UPDATE = -(182);
  RD_KAFKA_RESP_ERR__SSL = -(181);
  RD_KAFKA_RESP_ERR__WAIT_COORD = -(180);
  RD_KAFKA_RESP_ERR__UNKNOWN_GROUP = -(179);
  RD_KAFKA_RESP_ERR__IN_PROGRESS = -(178);
  RD_KAFKA_RESP_ERR__PREV_IN_PROGRESS = -(177);
  RD_KAFKA_RESP_ERR__EXISTING_SUBSCRIPTION = -(176);
  RD_KAFKA_RESP_ERR__ASSIGN_PARTITIONS = -(175);
  RD_KAFKA_RESP_ERR__REVOKE_PARTITIONS = -(174);
  RD_KAFKA_RESP_ERR__CONFLICT = -(173);
  RD_KAFKA_RESP_ERR__STATE = -(172);
  RD_KAFKA_RESP_ERR__UNKNOWN_PROTOCOL = -(171);
  RD_KAFKA_RESP_ERR__NOT_IMPLEMENTED = -(170);
  RD_KAFKA_RESP_ERR__AUTHENTICATION = -(169);
  RD_KAFKA_RESP_ERR__NO_OFFSET = -(168);
  RD_KAFKA_RESP_ERR__OUTDATED = -(167);
  RD_KAFKA_RESP_ERR__TIMED_OUT_QUEUE = -(166);
  RD_KAFKA_RESP_ERR__UNSUPPORTED_FEATURE = -(165);
  RD_KAFKA_RESP_ERR__WAIT_CACHE = -(164);

  RD_KAFKA_RESP_ERR__INTR = -(163);
  RD_KAFKA_RESP_ERR__KEY_SERIALIZATION = -(162);
  RD_KAFKA_RESP_ERR__VALUE_SERIALIZATION = -(161);
  RD_KAFKA_RESP_ERR__KEY_DESERIALIZATION = -(160);
  RD_KAFKA_RESP_ERR__VALUE_DESERIALIZATION = -(159);


  RD_KAFKA_RESP_ERR__END = -(100);
  RD_KAFKA_RESP_ERR_UNKNOWN = -(1);
  RD_KAFKA_RESP_ERR_NO_ERROR = 0;
  RD_KAFKA_RESP_ERR_OFFSET_OUT_OF_RANGE = 1;
  RD_KAFKA_RESP_ERR_INVALID_MSG = 2;
  RD_KAFKA_RESP_ERR_UNKNOWN_TOPIC_OR_PART = 3;
  RD_KAFKA_RESP_ERR_INVALID_MSG_SIZE = 4;
  RD_KAFKA_RESP_ERR_LEADER_NOT_AVAILABLE = 5;
  RD_KAFKA_RESP_ERR_NOT_LEADER_FOR_PARTITION = 6;
  RD_KAFKA_RESP_ERR_REQUEST_TIMED_OUT = 7;
  RD_KAFKA_RESP_ERR_BROKER_NOT_AVAILABLE = 8;
  RD_KAFKA_RESP_ERR_REPLICA_NOT_AVAILABLE = 9;
  RD_KAFKA_RESP_ERR_MSG_SIZE_TOO_LARGE = 10;
  RD_KAFKA_RESP_ERR_STALE_CTRL_EPOCH = 11;
  RD_KAFKA_RESP_ERR_OFFSET_METADATA_TOO_LARGE = 12;
  RD_KAFKA_RESP_ERR_NETWORK_EXCEPTION = 13;
  RD_KAFKA_RESP_ERR_GROUP_LOAD_IN_PROGRESS = 14;
  RD_KAFKA_RESP_ERR_GROUP_COORDINATOR_NOT_AVAILABLE = 15;
  RD_KAFKA_RESP_ERR_NOT_COORDINATOR_FOR_GROUP = 16;
  RD_KAFKA_RESP_ERR_TOPIC_EXCEPTION = 17;
  RD_KAFKA_RESP_ERR_RECORD_LIST_TOO_LARGE = 18;
  RD_KAFKA_RESP_ERR_NOT_ENOUGH_REPLICAS = 19;
  RD_KAFKA_RESP_ERR_NOT_ENOUGH_REPLICAS_AFTER_APPEND = 20;
  RD_KAFKA_RESP_ERR_INVALID_REQUIRED_ACKS = 21;
  RD_KAFKA_RESP_ERR_ILLEGAL_GENERATION = 22;
  RD_KAFKA_RESP_ERR_INCONSISTENT_GROUP_PROTOCOL = 23;
  RD_KAFKA_RESP_ERR_INVALID_GROUP_ID = 24;
  RD_KAFKA_RESP_ERR_UNKNOWN_MEMBER_ID = 25;
  RD_KAFKA_RESP_ERR_INVALID_SESSION_TIMEOUT = 26;
  RD_KAFKA_RESP_ERR_REBALANCE_IN_PROGRESS = 27;
  RD_KAFKA_RESP_ERR_INVALID_COMMIT_OFFSET_SIZE = 28;
  RD_KAFKA_RESP_ERR_TOPIC_AUTHORIZATION_FAILED = 29;
  RD_KAFKA_RESP_ERR_GROUP_AUTHORIZATION_FAILED = 30;
  RD_KAFKA_RESP_ERR_CLUSTER_AUTHORIZATION_FAILED = 31;
  RD_KAFKA_RESP_ERR_INVALID_TIMESTAMP = 32;
  RD_KAFKA_RESP_ERR_UNSUPPORTED_SASL_MECHANISM = 33;
  RD_KAFKA_RESP_ERR_ILLEGAL_SASL_STATE = 34;
  RD_KAFKA_RESP_ERR_UNSUPPORTED_VERSION = 35;
  RD_KAFKA_RESP_ERR_TOPIC_ALREADY_EXISTS = 36;
  RD_KAFKA_RESP_ERR_INVALID_PARTITIONS = 37;
  RD_KAFKA_RESP_ERR_INVALID_REPLICATION_FACTOR = 38;
  RD_KAFKA_RESP_ERR_INVALID_REPLICA_ASSIGNMENT = 39;
  RD_KAFKA_RESP_ERR_INVALID_CONFIG = 40;
  RD_KAFKA_RESP_ERR_NOT_CONTROLLER = 41;
  RD_KAFKA_RESP_ERR_INVALID_REQUEST = 42;
  RD_KAFKA_RESP_ERR_UNSUPPORTED_FOR_MESSAGE_FORMAT = 43;

  RD_KAFKA_RESP_ERR_POLICY_VIOLATION = 44;
  RD_KAFKA_RESP_ERR_OUT_OF_ORDER_SEQUENCE_NUMBER = 45;
  RD_KAFKA_RESP_ERR_DUPLICATE_SEQUENCE_NUMBER = 46;
  RD_KAFKA_RESP_ERR_INVALID_PRODUCER_EPOCH = 47;
  RD_KAFKA_RESP_ERR_INVALID_TXN_STATE = 48;
  RD_KAFKA_RESP_ERR_INVALID_PRODUCER_ID_MAPPING = 49;
  RD_KAFKA_RESP_ERR_INVALID_TRANSACTION_TIMEOUT = 50;
  RD_KAFKA_RESP_ERR_CONCURRENT_TRANSACTIONS = 51;
  RD_KAFKA_RESP_ERR_TRANSACTION_COORDINATOR_FENCED = 52;
  RD_KAFKA_RESP_ERR_TRANSACTIONAL_ID_AUTHORIZATION_FAILED = 53;
  RD_KAFKA_RESP_ERR_SECURITY_DISABLED = 54;
  RD_KAFKA_RESP_ERR_OPERATION_NOT_ATTEMPTED = 55;

  RD_KAFKA_RESP_ERR_END_ALL = 999;

  LIB_RD_KAFKA_EVENT_NONE          = $0;
  LIB_RD_KAFKA_EVENT_DR            = $1;
  LIB_RD_KAFKA_EVENT_FETCH         = $2;
  LIB_RD_KAFKA_EVENT_LOG           = $4;
  LIB_RD_KAFKA_EVENT_ERROR         = $8;
  LIB_RD_KAFKA_EVENT_REBALANCE     = $10;
  LIB_RD_KAFKA_EVENT_OFFSET_COMMIT = $20;
  LIB_RD_KAFKA_EVENT_STATS         = $40;


type
  Trd_kafka_timestamp_type_t =  Longint;
  Prd_kafka_timestamp_type_t  = ^Trd_kafka_timestamp_type_t;

  Trd_kafka_topic_conf_s = Longint;
  Prd_kafka_topic_conf_t = ^Trd_kafka_topic_conf_t;
  Trd_kafka_topic_conf_t = Trd_kafka_topic_conf_s;



  Prd_kafka_err_desc = ^Trd_kafka_err_desc;
  Trd_kafka_err_desc = record
	  code: Trd_kafka_resp_err_t; // < Error code
	  name: PChar;               // Error name, same as code enum sans prefix
	  desc: PChar;               // Human readable error description.
  end;

  Prd_kafka_topic_partition_s = ^Trd_kafka_topic_partition_s;
  Trd_kafka_topic_partition_s = record
    topic: PChar;                  // Topic name
    partition: Int32;              // Partition
    offset   : Int64;              // Offset
    metadata : Pointer;            // Metadata
    metadata_size: size_t;         // Metadata size
    opaque   : Pointer;            // Application opaque
    err      : Trd_kafka_resp_err_t;// Error code, depending on use.
    _private : Pointer;            // INTERNAL USE ONLY,
  end;
  Trd_kafka_topic_partition_t = Trd_kafka_topic_partition_s;
  Prd_kafka_topic_partition_t = ^Trd_kafka_topic_partition_t;

  Prd_kafka_topic_partition_list_s = ^Trd_kafka_topic_partition_list_s;
  Trd_kafka_topic_partition_list_s = record
    cnt : Int32;                         // Current number of elements
    size: Int32;                         // Current allocated size
    elems: Prd_kafka_topic_partition_s;  // Element array[]
  end;
  Trd_kafka_topic_partition_list_t = Trd_kafka_topic_partition_list_s;
  Prd_kafka_topic_partition_list_t = ^Trd_kafka_topic_partition_list_t;



  Prd_kafka_message_s = ^Trd_kafka_message_s;
  Trd_kafka_message_s = record
  	err: Trd_kafka_resp_err_t;   // Non-zero for error signaling.
  	rkt: Prd_kafka_topic_t;     // Topic
  	partion: Int32;            // Partition
  	payload: Pointer;           // Producer: original message payload.
  				                      // Consumer: Depends on the value of \c err :
  				                      // - \c err==0: Message payload.
  				                      // - \c err!=0: Error string
  	len: size_t;                // Depends on the value of \c err :
  				                      // - \c err==0: Message payload length
  				                      // - \c err!=0: Error string length
  	key: Pointer;               // Depends on the value of \c err :
  				                      // - \c err==0: Optional message key
  	key_len: size_t;            // Depends on the value of \c err :
  				                      // - \c err==0: Optional message key length
  	offset: Int64    ;          // Consume:
                                // - Message offset (or offset for error
  				                      //   if \c err!=0 if applicable).
                                // - dr_msg_cb:
                                //   Message offset assigned by broker.
                                //   If \c produce.offset.report is set then
                                //   each message will have this field set,
                                //   otherwise only the last message in
                                //   each produced internal batch will
                                //   have this field set, otherwise 0.
  	_private: Pointer;          // Consume:
  				                      //  - rdkafka private pointer: DO NOT MODIFY
  				                      //  - dr_msg_cb:
                                //    msg_opaque from produce() call
  end;
  Prd_kafka_message_t = ^Trd_kafka_message_t;
  Trd_kafka_message_t = Trd_kafka_message_s;

  Prd_kafka_metadata_broker = ^Trd_kafka_metadata_broker;
  Trd_kafka_metadata_broker = record
    id: Int32;             // Broker Id
    host: PChar;           // Broker hostname
    port: Int32;           // Broker listening port
  end;

  Prd_kafka_metadata_partition = ^Trd_kafka_metadata_partition;
  Trd_kafka_metadata_partition = record
    id: Int32;                // Partition Id
    err: Trd_kafka_resp_err_t; // Partition error reported by broker
    leader: Int32;            // Leader broker
    replica_cnt: Int32;       // Number of brokers in \p replicas
    replicas: pInt32;         // Replica brokers
    isr_cnt: Int32;           // Number of ISR brokers in \p isrs
    isrs: pInt32;             // In-Sync-Replica brokers
  end;

  Prd_kafka_metadata_topic = ^Trd_kafka_metadata_topic;
  Trd_kafka_metadata_topic = record
    topic: PChar;                              // Topic name
    partition_cnt: Int32;                      // Number of partitions in \p partitions
    partitions: Prd_kafka_metadata_partition;  // Partitions
    err: Trd_kafka_resp_err_t;                 // Topic error reported by broker
  end;

  Prd_kafka_metadata = ^Trd_kafka_metadata;
  Trd_kafka_metadata = record
    broker_cnt: Int32;                   // Number of brokers in \p brokers
    brokers: Prd_kafka_metadata_broker;  // Brokers
    topic_cnt: Int32;                    // Number of topics in \p topics
    topics: Prd_kafka_metadata_topic;    // Topics
    orig_broker_id: Int32;               // Broker originating this metadata
    orig_broker_name: PChar;             // Name of originating broker
  end;

  Prd_kafka_group_member_info = ^Trd_kafka_group_member_info;
  Trd_kafka_group_member_info = record
    member_id: PChar;              // Member id (generated by broker)
    client_id: PChar;              // Client's \p client.id
    client_host: PChar;            // Client's hostname
    member_metadata: Pointer;      // Member metadata (binary),
                                   //   format depends on \p protocol_type.
    member_metadata_size: Int32;   // Member metadata size in bytes
    member_assignment: Pointer;    // Member assignment (binary),
                                   //    format depends on \p protocol_type.
    member_assignment_size: Int32; // Member assignment size in bytes
  end;

  Prd_kafka_group_info =  ^Trd_kafka_group_info;
  Trd_kafka_group_info = record
      broker: Trd_kafka_metadata_broker;     // Originating broker info
      group: PChar;                          // Group name
      err: Trd_kafka_resp_err_t;             // Broker-originated error
      state: PChar;                          // Group state
      protocol_type: PChar;                  // Group protocol type
      protocol: PChar;                        // Group protocol
      members: Prd_kafka_group_member_info;  // Group members
      member_cnt: Int32;                     // Group member count
  end;

  Prd_kafka_group_list = ^Trd_kafka_group_list;
  Trd_kafka_group_list = record
    groups: Prd_kafka_group_info;    // Groups
    group_cnt: Int32;                // Group count
  end;

type
  PProc_Consume = ^TProc_Consume;
  TProc_Consume = procedure (rkmessage: Prd_kafka_message_t; opaque: Pointer);

  PProc_dr_msg_cb = ^TProc_dr_msg_cb;
  TProc_dr_msg_cb = procedure(rk: Prd_kafka_t; rkmessage: Prd_kafka_message_t; opaque: Pointer);  cdecl;
  TProc_dr_msg_cb_object = procedure(rk: Prd_kafka_t; rkmessage: Prd_kafka_message_t; opaque: Pointer) of object; cdecl;

  PProc_rebalance_cb = ^TProc_rebalance_cb;
  TProc_rebalance_cb = procedure(rk: Prd_kafka_t; err: Trd_kafka_resp_err_t; partitions: Prd_kafka_topic_partition_list_t; opaque: Pointer);  cdecl;

  PProc_offset_commit_cb = ^TProc_offset_commit_cb;
  TProc_offset_commit_cb = procedure(rk: Prd_kafka_t; err: Trd_kafka_resp_err_t; offsets: Prd_kafka_topic_partition_list_t; opaque: Pointer);  cdecl;

  PProc_error_cb = ^TProc_error_cb;
  TProc_error_cb = procedure(rk: Prd_kafka_t; err: Int32; reason: PChar; opaque: Pointer);  cdecl;

  PProc_throttle_cb = ^TProc_throttle_cb;
  TProc_throttle_cb = procedure (rk: Prd_kafka_t;broker_name: PChar;broker_id: Int32;throttle_time_ms: Int32;opaque: Pointer);  cdecl;

  PProc_log_cb = ^TProc_log_cb;
  TProc_log_cb = procedure(rk: Prd_kafka_t;level: Int32;fac: PChar;buf: PChar); cdecl;

  PFunc_stats_cb = ^TFunc_stats_cb;
  TFunc_stats_cb = function(rk: Prd_kafka_t;json: PChar;json_len: size_t;opaque: Pointer): Int32; cdecl;

  PFunc_socket_cb = ^TFunc_socket_cb;
  TFunc_socket_cb = Function (domain: int32 ; _Type: int32; protocol: int32; opaque: Pointer): Int32; cdecl;

  PFunc_connect_cb = ^TFunc_connect_cb;
  TFunc_connect_cb = Function (sockfd: Int32; var addr: TSockAddr; addrlen: Int32;id: PChar;opaque: Pointer): Int32; cdecl;

  PFunc_closesocket_cb = ^TFunc_closesocket_cb;
  TFunc_closesocket_cb = Function(sockfd: Int32; opaque: Pointer): Int32; cdecl;


  Tmode_t = Int32;
  PFunc_open_cb = ^TFunc_open_cb;
  TFunc_open_cb = Function(pathname: PChar;flags: Int32;mode: Tmode_t;opaque: Pointer): Int32; cdecl;

  PFunc_partitioner = ^TFunc_partitioner;
	TFunc_partitioner = Function(var rkt: Trd_kafka_topic_t;keydata: PChar;keylen: size_t;partition_cnt: int32;rkt_opaque: Pointer;msg_opaque: Pointer): Int32; cdecl;

  PProc_consume_cb = ^TProc_consume_cb;
  TProc_consume_cb = procedure(rkmessage: Prd_kafka_message_t; opaque: Pointer); cdecl;

  PProc_cb = ^TProc_cb;
  TProc_cb = procedure(rk: Prd_kafka_t; err: Trd_kafka_resp_err_t; offsets: Prd_kafka_topic_partition_list_t; opaque: Pointer); cdecl;

  PProc_logger = ^TProc_logger;
  TProc_logger = function(rk: Prd_kafka_t; level: Int32; fac: PChar; buf: PChar): Pointer; cdecl;


function  rd_kafka_version: Int32; cdecl;
function  rd_kafka_version_str: PChar; cdecl;
function  rd_kafka_get_debug_contexts: PChar; cdecl;

procedure rd_kafka_get_err_descs (errdescs: Prd_kafka_err_desc; var cntp: size_t); cdecl;

function  rd_kafka_err2str (err: Trd_kafka_resp_err_t): PChar; cdecl;
function  rd_kafka_err2name (err: Trd_kafka_resp_err_t): PChar; cdecl;
function  rd_kafka_last_error: Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_errno2err(errnox: Int32): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_errno: Int32; cdecl;

procedure rd_kafka_topic_partition_destroy (rktpar: Prd_kafka_topic_partition_t); cdecl;
function  rd_kafka_topic_partition_list_new (size: Int32): Prd_kafka_topic_partition_list_t; cdecl;
procedure rd_kafka_topic_partition_list_destroy (rkparlist: Prd_kafka_topic_partition_list_t); cdecl;
function  rd_kafka_topic_partition_list_add (rktparlist: Prd_kafka_topic_partition_list_t; topic: PChar; partition: Int32): Prd_kafka_topic_partition_t; cdecl;
procedure rd_kafka_topic_partition_list_add_range (rktparlist: Prd_kafka_topic_partition_list_t; topic: PChar; start: Int32; stop: Int32); cdecl;
function  rd_kafka_topic_partition_list_del (rktparlist: Prd_kafka_topic_partition_list_t; topic: PChar; partition: Int32): Int32; cdecl;
function  rd_kafka_topic_partition_list_del_by_idx (rktparlist: Prd_kafka_topic_partition_list_t; idx: Int32): Int32; cdecl;
function  rd_kafka_topic_partition_list_copy (src: Prd_kafka_topic_partition_list_t): Prd_kafka_topic_partition_list_t; cdecl;
function  rd_kafka_topic_partition_list_set_offset (rktparlist: Prd_kafka_topic_partition_list_t; topic: PChar; partition: Int32; offset: Int64): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_topic_partition_list_find (rktparlist: Prd_kafka_topic_partition_list_t; topic: PChar; partition: Int32): Prd_kafka_topic_partition_t; cdecl;
procedure rd_kafka_topic_partition_list_sort (rktparlist: Prd_kafka_topic_partition_list_t; compareFunction: Pointer; opaque: Pointer); cdecl;

procedure rd_kafka_message_destroy(rkmessage: Prd_kafka_message_t); cdecl;
function  rd_kafka_message_timestamp (rkmessage: Prd_kafka_message_t; var tstype: Trd_kafka_timestamp_type_t): Int64; cdecl;
function  rd_kafka_message_latency (rkmessage: Prd_kafka_message_t): Int64; cdecl;
function  rd_kafka_conf_new: Prd_kafka_conf_t; cdecl;
procedure rd_kafka_conf_destroy(conf: Prd_kafka_conf_t); cdecl;
function  rd_kafka_conf_dup(conf: Prd_kafka_conf_t): Prd_kafka_conf_t; cdecl;
function  rd_kafka_conf_set(conf: Prd_kafka_conf_t; name: PChar; value: PChar; errstr: PChar; errstr_size: size_t): Trd_kafka_conf_res_t; cdecl;
procedure rd_kafka_conf_set_events(conf: Prd_kafka_conf_t; events: Int32); cdecl;
procedure rd_kafka_conf_set_dr_cb(conf: Prd_kafka_conf_t; callback: Pointer); cdecl;

procedure rd_kafka_conf_set_dr_msg_cb(conf: Prd_kafka_conf_t; dr_msg_cb: TProc_dr_msg_cb); cdecl;

procedure rd_kafka_conf_set_consume_cb (conf: Prd_kafka_conf_t; consume: PProc_Consume);cdecl;
procedure rd_kafka_conf_set_rebalance_cb (conf: Prd_kafka_conf_t; rebalance: PProc_rebalance_cb);cdecl;
procedure rd_kafka_conf_set_offset_commit_cb (conf: Prd_kafka_conf_t; offset_commit_cb: PProc_offset_commit_cb);cdecl;
procedure rd_kafka_conf_set_error_cb(conf: Prd_kafka_conf_t; error_cb: PProc_error_cb);cdecl;
procedure rd_kafka_conf_set_throttle_cb (conf: Prd_kafka_conf_t; throttle_cb: PProc_throttle_cb);cdecl;
procedure rd_kafka_conf_set_log_cb(conf: Prd_kafka_conf_t; log_cb: PProc_log_cb);cdecl;
procedure rd_kafka_conf_set_stats_cb(conf: Prd_kafka_conf_t; stats_cb: PFunc_stats_cb);cdecl;
procedure rd_kafka_conf_set_socket_cb(conf: Prd_kafka_conf_t; socket_cb: PFunc_socket_cb);cdecl;
procedure rd_kafka_conf_set_connect_cb (conf: Prd_kafka_conf_t; connect_cb: PFunc_connect_cb);cdecl;
procedure rd_kafka_conf_set_closesocket_cb (conf: Prd_kafka_conf_t; closesocket_cb: PFunc_closesocket_cb);cdecl;
procedure rd_kafka_conf_set_open_cb (conf: Prd_kafka_conf_t; open_cb: PFunc_open_cb);cdecl;
procedure rd_kafka_conf_set_opaque(conf: Prd_kafka_conf_t; opaque: Pointer);cdecl;
function  rd_kafka_opaque(conf: Prd_kafka_conf_t): Pointer;cdecl;
procedure rd_kafka_conf_set_default_topic_conf (conf: Prd_kafka_conf_t; tconf: Prd_kafka_topic_conf_t);cdecl;

function  rd_kafka_topic_conf_new: Prd_kafka_topic_conf_t; cdecl;
function  rd_kafka_topic_conf_dup(conf: Prd_kafka_topic_conf_t): Prd_kafka_topic_conf_t; cdecl;
procedure rd_kafka_topic_conf_destroy(var topic_conf: Trd_kafka_topic_conf_t); cdecl;
function  rd_kafka_topic_conf_set(conf: Prd_kafka_topic_conf_t;name: PChar;value: PChar;errstr: PChar;errstr_size: size_t): Trd_kafka_conf_res_t;cdecl;
procedure rd_kafka_topic_conf_set_opaque(conf: Prd_kafka_topic_conf_t; opaque: Pointer);cdecl;
procedure rd_kafka_topic_conf_set_partitioner_cb (topic_conf: Prd_kafka_topic_conf_t; partitioner: PFunc_partitioner);cdecl;
function  rd_kafka_topic_partition_available(rkt: Prd_kafka_topic_t; partition: int32): Int32;cdecl;
function  rd_kafka_msg_partitioner_random(rkt: Prd_kafka_topic_t; var key: Pointer; keylen: size_t; partition_cnt: int32; var opaque: Pointer; var msg_opaque: Pointer): int32;cdecl;
function  rd_kafka_msg_partitioner_consistent (rkt: Prd_kafka_topic_t;var key: Pointer; keylen: size_t;partition_cnt: int32;var opaque: Pointer; var msg_opaque: Pointer): Int32;cdecl;
function  rd_kafka_msg_partitioner_consistent_random (rkt: Prd_kafka_topic_t;var key: Pointer; keylen: size_t;partition_cnt: Int32;var opaque: Pointer; var msg_opaque: Pointer): Int32;cdecl;


function  rd_kafka_new(rdktype: Trd_kafka_type_t; conf: Prd_kafka_conf_t; errstr: PChar; errstr_size: size_t): Prd_kafka_t; cdecl;
procedure rd_kafka_destroy(rk: Prd_kafka_t); cdecl;
function  rd_kafka_name(rk: Prd_kafka_t): PChar; cdecl;
function  rd_kafka_type(rk: Prd_kafka_t): Trd_kafka_type_t; cdecl;
function  rd_kafka_memberid (rk: Prd_kafka_t): PChar; cdecl;

function rd_kafka_clusterid (rk: Prd_kafka_t; timeout_ms: Int32 ): PChar; cdecl;
function  rd_kafka_topic_new(rk: Prd_kafka_t; topic: PChar; conf: Prd_kafka_topic_conf_t): Prd_kafka_topic_t; cdecl;
procedure rd_kafka_topic_destroy(rkt: Prd_kafka_topic_t); cdecl;
function  rd_kafka_topic_name(rkt: Prd_kafka_topic_t): PChar; cdecl;
procedure rd_kafka_topic_opaque (rkt: Prd_kafka_topic_t); cdecl;
function  rd_kafka_poll(rk: Prd_kafka_t; timeout_ms: Int32): Int32; cdecl;
procedure rd_kafka_yield (rk: Prd_kafka_t); cdecl;
function  rd_kafka_pause_partitions (rk: Prd_kafka_t; partitions: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_resume_partitions (rk: Prd_kafka_t; partitions: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_query_watermark_offsets (rk: Prd_kafka_t; topic: PChar; partition: int32; var low: int64; var high: int64; timeout_ms: Int32): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_get_watermark_offsets (rk: Prd_kafka_t; topic: PChar; partition: int32; var low: int64; var high: int64): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_offsets_for_times (rk: Prd_kafka_t; offsets: Prd_kafka_topic_partition_list_t; timeout_ms: Int32): Trd_kafka_resp_err_t; cdecl;
procedure rd_kafka_mem_free (rk: Prd_kafka_t; ptr: Pointer); cdecl;


function  rd_kafka_queue_new(rk: Prd_kafka_t): Prd_kafka_queue_t; cdecl;
procedure rd_kafka_queue_destroy(rkqu: Prd_kafka_queue_t); cdecl;
function  rd_kafka_queue_get_main (rk: Prd_kafka_t): Prd_kafka_queue_t; cdecl;
function  rd_kafka_queue_get_consumer (rk: Prd_kafka_t): prd_kafka_queue_t; cdecl;
function  rd_kafka_queue_get_partition (rk: Prd_kafka_t; topic: PChar; partition: Int32): Prd_kafka_queue_t; cdecl;
procedure rd_kafka_queue_forward (src: Prd_kafka_queue_t; dst: Prd_kafka_queue_t); cdecl;
function  rd_kafka_set_log_queue (rk: Prd_kafka_t; rkqu: Prd_kafka_queue_t): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_queue_length (rkqu: Prd_kafka_queue_t): size_t; cdecl;
procedure rd_kafka_queue_io_event_enable (rkqu: Prd_kafka_queue_t; fd: int32; payload: Pointer; size: size_t); cdecl;


function  rd_kafka_consume_start(rkt: Prd_kafka_topic_t; partition: Int32; offset: int64): Int32; cdecl;
function  rd_kafka_consume_start_queue(rkt: Prd_kafka_topic_t; partition: Int32; offset: Int64; rkqu: Prd_kafka_queue_t): Int32; cdecl;
function  rd_kafka_consume_stop(rkt: Prd_kafka_topic_t; partition: Int32): Int32; cdecl;
function  rd_kafka_seek (rkt: Prd_kafka_topic_t; partition: int32; offset: int64; timeout_ms: int32): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_consume(rkt: Prd_kafka_topic_t; partition: int32; timeout_ms: int32): Prd_kafka_message_t; cdecl;
function  rd_kafka_consume_batch(rkt: Prd_kafka_topic_t; partition: int32; timeout_ms: int32; rkmessages: Prd_kafka_message_t; rkmessages_size: size_t ): size_t; cdecl;  // ssize_t
function  rd_kafka_consume_batch_queue(rkqu: Prd_kafka_queue_t; timeout_ms: Int32; rkmessages: Prd_kafka_message_t; rkmessages_size: size_t): size_t; cdecl;  // ssize_t
function  rd_kafka_consume_callback_queue(rkqu: Prd_kafka_queue_t; timeout_ms: int32; consume_cb: PProc_consume_cb; opaque: Pointer): Int32; cdecl;


function  rd_kafka_offset_store(rkt: Prd_kafka_topic_t; partition: int32; offset: int64): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_offsets_store(rk: Prd_kafka_t; offsets: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_subscribe (rk: Prd_kafka_t; topics: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_unsubscribe (rk: Prd_kafka_t): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_subscription (rk: Prd_kafka_t; topics: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl;

function  rd_kafka_consumer_poll (rk: Prd_kafka_t; timeout_ms: Int32): Prd_kafka_message_t; cdecl;
function  rd_kafka_consumer_close (rk: Prd_kafka_t): Trd_kafka_resp_err_t; cdecl;

function  rd_kafka_assign (rk: Prd_kafka_t; partitions: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_assignment (rk: Prd_kafka_t; partitions: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_commit (rk: Prd_kafka_t; offsets: Prd_kafka_topic_partition_list_t; async: Int32): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_commit_message (rk: Prd_kafka_t; rkmessage: Prd_kafka_message_t; async: Int32): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_commit_queue (rk: Prd_kafka_t; offsets: Prd_kafka_topic_partition_list_t; rkqu: Prd_kafka_queue_t; cb: PProc_cb; opaque: Pointer): Trd_kafka_resp_err_t; cdecl;

function  rd_kafka_committed (rk: Prd_kafka_t; partitions: Prd_kafka_topic_partition_list_t; timeout_ms: int32): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_position (rk: Prd_kafka_t; partitions: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_produce(rkt: Prd_kafka_topic_t; partition: int32; msgflags: int32; payload: Pointer; len: size_t; key: Pointer; keylen: size_t; msg_opaque: Pointer): int32; cdecl;

//function  rd_kafka_producev (rk: Prd_kafka_t; var Args: array of const): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_producev (rk: Prd_kafka_t; Args: Pointer): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_produce_batch(rkt: Prd_kafka_topic_t; partition: int32; msgflags: int32; rkmessages: Prd_kafka_message_t; message_cnt: Int32): Int32; cdecl;
function  rd_kafka_flush (rk: Prd_kafka_t; timeout_ms: Int32): Trd_kafka_resp_err_t; cdecl;


function  rd_kafka_metadata (rk: Prd_kafka_t; all_topics: Int32; only_rkt: Prd_kafka_topic_t; metadatap: Prd_kafka_metadata; timeout_ms: Int32): Trd_kafka_resp_err_t; cdecl;

procedure rd_kafka_metadata_destroy(metadata: Prd_kafka_metadata); cdecl;
function  rd_kafka_list_groups (rk: Prd_kafka_t; group: PChar; grplistp: Prd_kafka_group_list;timeout_ms: Int32): Trd_kafka_resp_err_t; cdecl;
procedure rd_kafka_group_list_destroy (grplist: Prd_kafka_group_list); cdecl;

function  rd_kafka_brokers_add(rk: Prd_kafka_t; brokerlist: PChar ): Int32; cdecl;
procedure rd_kafka_set_logger(rk: Prd_kafka_t; proc_logger: PProc_logger); cdecl;
procedure rd_kafka_set_log_level(rk: Prd_kafka_t; level: int32); cdecl;
procedure rd_kafka_log_print(rk: Prd_kafka_t; level: Int32; fac: PChar; buf: PChar); cdecl;
procedure rd_kafka_log_syslog(rk: Prd_kafka_t; level: int32; fac: PChar; buf: PChar); cdecl;
function  rd_kafka_outq_len(rk: Prd_kafka_t): Int32; cdecl;
procedure rd_kafka_dump(fp: Pointer; rk: Prd_kafka_t); cdecl;
function  rd_kafka_thread_cnt: Int32; cdecl;
function  rd_kafka_wait_destroyed(timeout_ms: Int32): Int32; cdecl;

function  rd_kafka_poll_set_consumer (rk: Prd_kafka_t): Trd_kafka_resp_err_t; cdecl;

function  rd_kafka_event_type (rkev: Prd_kafka_event_t): Trd_kafka_event_type_t; cdecl;
function  rd_kafka_event_name (rkev: Prd_kafka_event_t): PChar; cdecl;
procedure rd_kafka_event_destroy (rkev: Prd_kafka_event_t); cdecl;
function  rd_kafka_event_message_next (rkev: Prd_kafka_event_t): Prd_kafka_event_t; cdecl;
function  rd_kafka_event_message_array(rkev: Prd_kafka_event_t; rkmessages: Prd_kafka_message_t; size: size_t): size_t; cdecl;
function  rd_kafka_event_message_count (rkev: Prd_kafka_event_t): size_t; cdecl;
function  rd_kafka_event_error (rkev: Prd_kafka_event_t): Trd_kafka_resp_err_t; cdecl;
function  rd_kafka_event_error_string (rkev: Prd_kafka_event_t): PChar; cdecl;
function  rd_kafka_event_opaque(rkev: Prd_kafka_event_t): Pointer; cdecl;
function  rd_kafka_event_log (rkev: Prd_kafka_event_t; var fac: PChar; var str: PChar; var level: int32): Int32; cdecl;

function  rd_kafka_event_topic_partition_list (rkev: Prd_kafka_event_t): Prd_kafka_topic_partition_list_t; cdecl;
function  rd_kafka_event_topic_partition (rkev: Prd_kafka_event_t): Prd_kafka_topic_partition_t; cdecl;
function  rd_kafka_queue_poll (rkqu: Prd_kafka_queue_t; timeout_ms: Int32): Prd_kafka_event_t; cdecl;
function  rd_kafka_queue_poll_callback (rkqu: Prd_kafka_queue_t; timeout_ms: Int32): Int32; cdecl;

implementation

function  rd_kafka_version: Int32; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_version_str: PChar; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_get_debug_contexts: PChar; cdecl; EXTERNAL RD_EXPORT;

procedure rd_kafka_get_err_descs (errdescs: Prd_kafka_err_desc; var cntp: size_t); cdecl; EXTERNAL RD_EXPORT;

function  rd_kafka_err2str (err: Trd_kafka_resp_err_t): PChar; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_err2name (err: Trd_kafka_resp_err_t): PChar; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_last_error: Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_errno2err(errnox: Int32): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_errno: Int32; cdecl; EXTERNAL RD_EXPORT;

procedure rd_kafka_topic_partition_destroy (rktpar: Prd_kafka_topic_partition_t); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_topic_partition_list_new (size: Int32): Prd_kafka_topic_partition_list_t; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_topic_partition_list_destroy (rkparlist: Prd_kafka_topic_partition_list_t); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_topic_partition_list_add (rktparlist: Prd_kafka_topic_partition_list_t; topic: PChar; partition: Int32): Prd_kafka_topic_partition_t; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_topic_partition_list_add_range (rktparlist: Prd_kafka_topic_partition_list_t; topic: PChar; start: Int32; stop: Int32); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_topic_partition_list_del (rktparlist: Prd_kafka_topic_partition_list_t; topic: PChar; partition: Int32): Int32; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_topic_partition_list_del_by_idx (rktparlist: Prd_kafka_topic_partition_list_t; idx: Int32): Int32; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_topic_partition_list_copy (src: Prd_kafka_topic_partition_list_t): Prd_kafka_topic_partition_list_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_topic_partition_list_set_offset (rktparlist: Prd_kafka_topic_partition_list_t; topic: PChar; partition: Int32; offset: Int64): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_topic_partition_list_find (rktparlist: Prd_kafka_topic_partition_list_t; topic: PChar; partition: Int32): Prd_kafka_topic_partition_t; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_topic_partition_list_sort (rktparlist: Prd_kafka_topic_partition_list_t; compareFunction: Pointer; opaque: Pointer); cdecl; EXTERNAL RD_EXPORT;

procedure rd_kafka_message_destroy(rkmessage: Prd_kafka_message_t); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_message_timestamp (rkmessage: Prd_kafka_message_t; var tstype: Trd_kafka_timestamp_type_t): Int64; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_message_latency (rkmessage: Prd_kafka_message_t): Int64; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_conf_new: Prd_kafka_conf_t; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_destroy(conf: Prd_kafka_conf_t); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_conf_dup(conf: Prd_kafka_conf_t): Prd_kafka_conf_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_conf_set(conf: Prd_kafka_conf_t; name: PChar; value: PChar; errstr: PChar; errstr_size: size_t): Trd_kafka_conf_res_t; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_events(conf: Prd_kafka_conf_t; events: Int32); cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_dr_cb(conf: Prd_kafka_conf_t; callback: Pointer); cdecl; EXTERNAL RD_EXPORT;

procedure rd_kafka_conf_set_dr_msg_cb(conf: Prd_kafka_conf_t; dr_msg_cb: TProc_dr_msg_cb); cdecl; EXTERNAL RD_EXPORT;

procedure rd_kafka_conf_set_consume_cb (conf: Prd_kafka_conf_t; consume: PProc_Consume);cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_rebalance_cb (conf: Prd_kafka_conf_t; rebalance: PProc_rebalance_cb);cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_offset_commit_cb (conf: Prd_kafka_conf_t; offset_commit_cb: PProc_offset_commit_cb);cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_error_cb(conf: Prd_kafka_conf_t; error_cb: PProc_error_cb);cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_throttle_cb (conf: Prd_kafka_conf_t; throttle_cb: PProc_throttle_cb);cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_log_cb(conf: Prd_kafka_conf_t; log_cb: PProc_log_cb);cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_stats_cb(conf: Prd_kafka_conf_t; stats_cb: PFunc_stats_cb);cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_socket_cb(conf: Prd_kafka_conf_t; socket_cb: PFunc_socket_cb);cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_connect_cb (conf: Prd_kafka_conf_t; connect_cb: PFunc_connect_cb);cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_closesocket_cb (conf: Prd_kafka_conf_t; closesocket_cb: PFunc_closesocket_cb);cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_open_cb (conf: Prd_kafka_conf_t; open_cb: PFunc_open_cb);cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_opaque(conf: Prd_kafka_conf_t; opaque: Pointer);cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_opaque(conf: Prd_kafka_conf_t): Pointer;cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_conf_set_default_topic_conf (conf: Prd_kafka_conf_t; tconf: Prd_kafka_topic_conf_t);cdecl; EXTERNAL RD_EXPORT;

function  rd_kafka_topic_conf_new: Prd_kafka_topic_conf_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_topic_conf_dup(conf: Prd_kafka_topic_conf_t): Prd_kafka_topic_conf_t; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_topic_conf_destroy(var topic_conf: Trd_kafka_topic_conf_t); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_topic_conf_set(conf: Prd_kafka_topic_conf_t;name: PChar;value: PChar;errstr: PChar;errstr_size: size_t): Trd_kafka_conf_res_t;cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_topic_conf_set_opaque(conf: Prd_kafka_topic_conf_t; opaque: Pointer);cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_topic_conf_set_partitioner_cb (topic_conf: Prd_kafka_topic_conf_t; partitioner: PFunc_partitioner);cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_topic_partition_available(rkt: Prd_kafka_topic_t; partition: int32): Int32;cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_msg_partitioner_random(rkt: Prd_kafka_topic_t; var key: Pointer; keylen: size_t; partition_cnt: int32; var opaque: Pointer; var msg_opaque: Pointer): int32;cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_msg_partitioner_consistent (rkt: Prd_kafka_topic_t;var key: Pointer; keylen: size_t;partition_cnt: int32;var opaque: Pointer; var msg_opaque: Pointer): Int32;cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_msg_partitioner_consistent_random (rkt: Prd_kafka_topic_t;var key: Pointer; keylen: size_t;partition_cnt: Int32;var opaque: Pointer; var msg_opaque: Pointer): Int32;cdecl; EXTERNAL RD_EXPORT;


function  rd_kafka_new(rdktype: Trd_kafka_type_t; conf: Prd_kafka_conf_t; errstr: PChar; errstr_size: size_t): Prd_kafka_t; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_destroy(rk: Prd_kafka_t); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_name(rk: Prd_kafka_t): PChar; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_type(rk: Prd_kafka_t): Trd_kafka_type_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_memberid (rk: Prd_kafka_t): PChar; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_clusterid (rk: Prd_kafka_t; timeout_ms: Int32): PChar; cdecl; EXTERNAL RD_EXPORT;

function  rd_kafka_topic_new(rk: Prd_kafka_t; topic: PChar; conf: Prd_kafka_topic_conf_t): Prd_kafka_topic_t; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_topic_destroy(rkt: Prd_kafka_topic_t); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_topic_name(rkt: Prd_kafka_topic_t): PChar; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_topic_opaque (rkt: Prd_kafka_topic_t); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_poll(rk: Prd_kafka_t; timeout_ms: Int32): Int32; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_yield (rk: Prd_kafka_t); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_pause_partitions (rk: Prd_kafka_t; partitions: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_resume_partitions (rk: Prd_kafka_t; partitions: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_query_watermark_offsets (rk: Prd_kafka_t; topic: PChar; partition: int32; var low: int64; var high: int64; timeout_ms: Int32): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_get_watermark_offsets (rk: Prd_kafka_t; topic: PChar; partition: int32; var low: int64; var high: int64): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_offsets_for_times (rk: Prd_kafka_t; offsets: Prd_kafka_topic_partition_list_t; timeout_ms: Int32): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_mem_free (rk: Prd_kafka_t; ptr: Pointer); cdecl; EXTERNAL RD_EXPORT;


function  rd_kafka_queue_new(rk: Prd_kafka_t): Prd_kafka_queue_t; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_queue_destroy(rkqu: Prd_kafka_queue_t); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_queue_get_main (rk: Prd_kafka_t): Prd_kafka_queue_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_queue_get_consumer (rk: Prd_kafka_t): prd_kafka_queue_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_queue_get_partition (rk: Prd_kafka_t; topic: PChar; partition: Int32): Prd_kafka_queue_t; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_queue_forward (src: Prd_kafka_queue_t; dst: Prd_kafka_queue_t); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_set_log_queue (rk: Prd_kafka_t; rkqu: Prd_kafka_queue_t): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_queue_length (rkqu: Prd_kafka_queue_t): size_t; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_queue_io_event_enable (rkqu: Prd_kafka_queue_t; fd: int32; payload: Pointer; size: size_t); cdecl; EXTERNAL RD_EXPORT;


function  rd_kafka_consume_start(rkt: Prd_kafka_topic_t; partition: Int32; offset: int64): Int32; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_consume_start_queue(rkt: Prd_kafka_topic_t; partition: Int32; offset: Int64; rkqu: Prd_kafka_queue_t): Int32; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_consume_stop(rkt: Prd_kafka_topic_t; partition: Int32): Int32; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_seek (rkt: Prd_kafka_topic_t; partition: int32; offset: int64; timeout_ms: int32): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_consume(rkt: Prd_kafka_topic_t; partition: int32; timeout_ms: int32): Prd_kafka_message_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_consume_batch(rkt: Prd_kafka_topic_t; partition: int32; timeout_ms: int32; rkmessages: Prd_kafka_message_t; rkmessages_size: size_t ): size_t; cdecl; EXTERNAL RD_EXPORT; // ssize_t
function  rd_kafka_consume_batch_queue(rkqu: Prd_kafka_queue_t; timeout_ms: Int32; rkmessages: Prd_kafka_message_t; rkmessages_size: size_t): size_t; cdecl; EXTERNAL RD_EXPORT; // ssize_t
function  rd_kafka_consume_callback_queue(rkqu: Prd_kafka_queue_t; timeout_ms: int32; consume_cb: PProc_consume_cb; opaque: Pointer): Int32; cdecl; EXTERNAL RD_EXPORT;


function  rd_kafka_offset_store(rkt: Prd_kafka_topic_t; partition: int32; offset: int64): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_offsets_store(rk: Prd_kafka_t; offsets: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_subscribe (rk: Prd_kafka_t; topics: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_unsubscribe (rk: Prd_kafka_t): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_subscription (rk: Prd_kafka_t; topics: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;

function  rd_kafka_consumer_poll (rk: Prd_kafka_t; timeout_ms: Int32): Prd_kafka_message_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_consumer_close (rk: Prd_kafka_t): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;

function  rd_kafka_assign (rk: Prd_kafka_t; partitions: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_assignment (rk: Prd_kafka_t; partitions: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_commit (rk: Prd_kafka_t; offsets: Prd_kafka_topic_partition_list_t; async: Int32): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_commit_message (rk: Prd_kafka_t; rkmessage: Prd_kafka_message_t; async: Int32): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_commit_queue (rk: Prd_kafka_t; offsets: Prd_kafka_topic_partition_list_t; rkqu: Prd_kafka_queue_t; cb: PProc_cb; opaque: Pointer): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;

function  rd_kafka_committed (rk: Prd_kafka_t; partitions: Prd_kafka_topic_partition_list_t; timeout_ms: int32): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_position (rk: Prd_kafka_t; partitions: Prd_kafka_topic_partition_list_t): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_produce(rkt: Prd_kafka_topic_t; partition: int32; msgflags: int32; payload: Pointer; len: size_t; key: Pointer; keylen: size_t; msg_opaque: Pointer): int32; cdecl; EXTERNAL RD_EXPORT;

//function  rd_kafka_producev (rk: Prd_kafka_t; Args: array of const): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_producev (rk: Prd_kafka_t; Args: Pointer): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_produce_batch(rkt: Prd_kafka_topic_t; partition: int32; msgflags: int32; rkmessages: Prd_kafka_message_t; message_cnt: Int32): Int32; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_flush (rk: Prd_kafka_t; timeout_ms: Int32): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;


function  rd_kafka_metadata (rk: Prd_kafka_t; all_topics: Int32; only_rkt: Prd_kafka_topic_t; metadatap: Prd_kafka_metadata; timeout_ms: Int32): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;

procedure rd_kafka_metadata_destroy(metadata: Prd_kafka_metadata); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_list_groups (rk: Prd_kafka_t; group: PChar; grplistp: Prd_kafka_group_list;timeout_ms: Int32): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_group_list_destroy (grplist: Prd_kafka_group_list); cdecl; EXTERNAL RD_EXPORT;

function  rd_kafka_brokers_add(rk: Prd_kafka_t; brokerlist: PChar ): Int32; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_set_logger(rk: Prd_kafka_t; proc_logger: PProc_logger); cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_set_log_level(rk: Prd_kafka_t; level: int32); cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_log_print(rk: Prd_kafka_t; level: Int32; fac: PChar; buf: PChar); cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_log_syslog(rk: Prd_kafka_t; level: int32; fac: PChar; buf: PChar); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_outq_len(rk: Prd_kafka_t): Int32; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_dump(fp: Pointer; rk: Prd_kafka_t); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_thread_cnt: Int32; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_wait_destroyed(timeout_ms: Int32): Int32; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_unittest: Int32; cdecl; EXTERNAL RD_EXPORT;



function  rd_kafka_poll_set_consumer (rk: Prd_kafka_t): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;

function  rd_kafka_event_type (rkev: Prd_kafka_event_t): Trd_kafka_event_type_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_event_name (rkev: Prd_kafka_event_t): PChar; cdecl; EXTERNAL RD_EXPORT;
procedure rd_kafka_event_destroy (rkev: Prd_kafka_event_t); cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_event_message_next (rkev: Prd_kafka_event_t): Prd_kafka_event_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_event_message_array(rkev: Prd_kafka_event_t; rkmessages: Prd_kafka_message_t; size: size_t): size_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_event_message_count (rkev: Prd_kafka_event_t): size_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_event_error (rkev: Prd_kafka_event_t): Trd_kafka_resp_err_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_event_error_string (rkev: Prd_kafka_event_t): PChar; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_event_opaque(rkev: Prd_kafka_event_t): Pointer; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_event_log (rkev: Prd_kafka_event_t; var fac: PChar; var str: PChar; var level: int32): Int32; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_event_stats (rkev: Prd_kafka_event_t): PChar; cdecl; EXTERNAL RD_EXPORT;


function  rd_kafka_event_topic_partition_list (rkev: Prd_kafka_event_t): Prd_kafka_topic_partition_list_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_event_topic_partition (rkev: Prd_kafka_event_t): Prd_kafka_topic_partition_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_queue_poll (rkqu: Prd_kafka_queue_t; timeout_ms: Int32): Prd_kafka_event_t; cdecl; EXTERNAL RD_EXPORT;
function  rd_kafka_queue_poll_callback (rkqu: Prd_kafka_queue_t; timeout_ms: Int32): Int32; cdecl; EXTERNAL RD_EXPORT;

end.

