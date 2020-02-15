unit kafkaclient;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Kafka;

type
  TOnKafkaMessageReceived = procedure(InMessage: String; InKey: String; OutMsg: Prd_kafka_message_t) of object;
  TOnKafkaMessageEOF = procedure(InMessage: String) of object;
  TOnKafkaMessageErr = procedure(InError: String) of object;
  TOnKafkaTick = procedure(InWhat: String) of object;

  { TKafkaClient }

  TKafkaClient = class (TObject)
    KafkaConf: Prd_kafka_conf_t;
    TopicConf: Prd_kafka_topic_conf_t;
    _Rk: Prd_kafka_t;
    _Topics: Prd_kafka_topic_partition_list_t;
    _Topic: Prd_kafka_topic_partition_t;
    _Rkmessage: Prd_kafka_message_t;
    _Rkt: Prd_kafka_topic_t;

    _RecvBuffer: array[0..(65536 * 100)-1] of char;
    private
      function GetVersionStr: String;
      procedure CheckForKafkaError (aAction: String);
    public
      _BStop: Boolean;
      _OnKafkaMessageReceived: TOnKafkaMessageReceived;
      _OnKafkaMessageEOF: TOnKafkaMessageEOF;
      _OnKafkaMessageErr: TOnKafkaMessageErr;
      _OnKafkaTick: TOnKafkaTick;
      _FormatSettings: TFormatSettings;
      property VersionStr: String read GetVersionStr;
      procedure ConfigureParam (name, value: String);
      procedure ConfigureParamList (Params: TStringList);
      constructor Create;
  end;

  { TKafkaConsumer }

  TKafkaConsumer = class (TKafkaClient)
  public
    constructor Create;
  end;

  { TKafkaProducer }

  TKafkaProducer = class (TKafkaClient)
  public
    procedure StartBroker(aBrokerName: String);
    procedure ProduceMessage (aKey, aMessage: String);
    procedure CreateProducerTopic(InTopicName: String);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TKafkaProducer }
procedure TKafkaProducer.StartBroker(aBrokerName: String);
var
  errstr: array[0..512] of char;
  errstr_size: Int32;
begin
  ConfigureParam('bootstrap.servers', aBrokerName);
  errstr := '';
  errstr_size := SizeOf(errstr);
  _Rk := rd_kafka_new(RD_KAFKA_PRODUCER, KafkaConf, PChar(errstr), errstr_size);
  if _Rk = nil then
    CheckForKafkaError('StartBroker "' + aBrokerName + '"');
end;

procedure TKafkaProducer.ProduceMessage (aKey, aMessage: String);
var
  xMessageLen, xRetLen: Integer;
  xRetVal: Trd_kafka_conf_res_t;
  xTopicName: String;
begin
  xTopicName := rd_kafka_topic_name(_rkt);
  xMessageLen := Length(aMessage);
  xRetLen := Length(aKey);
  xRetVal := rd_kafka_produce ( _rkt
                              , RD_KAFKA_PARTITION_UA
                              , RD_KAFKA_MSG_F_COPY
                              , @aMessage[1]
                              , xMessageLen
                              , @aKey[1]
                              , xRetLen
                              , self
                              );

  if xRetVal <> RD_KAFKA_CONF_OK then
    CheckForKafkaError('ProduceMessage');
  rd_kafka_poll(_rk, 0); //non-blocking
  CheckForKafkaError('ProduceMessage');
end;

procedure TKafkaProducer.CreateProducerTopic(InTopicName: String);
begin
  _Rkt := rd_kafka_topic_new(_rk, PChar(InTopicName), nil);
  CheckForKafkaError('CreateProducerTopic "' + InTopicName + '"');
end;

constructor TKafkaProducer.Create;
begin
  inherited Create;
end;

destructor TKafkaProducer.Destroy;
begin
  try
    if Assigned (_rk) then rd_kafka_flush(_rk, 10*1000); // wait for max 10 seconds
    if Assigned (_rkt) then rd_kafka_topic_destroy(_rkt);
    if Assigned (_rk) then rd_kafka_destroy(_rk);
  finally
    inherited Destroy;
  end;
end;

{ TKafkaConsumer }

constructor TKafkaConsumer.Create;
begin
  inherited Create;
end;

{ TKafkaClient }

function TKafkaClient.GetVersionStr: String;
begin
  result := String(Kafka.rd_kafka_version_str)
end;

procedure TKafkaClient.CheckForKafkaError(aAction: String);
var
  xErrNo: Trd_kafka_resp_err_t;
  xErrNoStr: String;
  xErrName: String;
begin
  xErrNo := rd_kafka_last_error;
  if xErrNo <> RD_KAFKA_RESP_ERR_NO_ERROR then
  begin
    xErrNoStr := string (Kafka.rd_kafka_err2str (xErrNo));
    xErrName := string (Kafka.rd_kafka_err2name (xErrNo));
    raise Exception.CreateFmt('%s failed: "%s" %s', [aAction, xErrNoStr, xErrName]);
  end;
end;

procedure TKafkaClient.ConfigureParam(name, value: String);
var
  MyRetVal: Trd_kafka_conf_res_t;
  MyLastErrorCode: Trd_kafka_resp_err_t;
  errstr: array[0..512] of char;
  errstr_size: Int32;
begin
  errstr := '';
  errstr_size := SizeOf(errstr);
  MyRetVal := rd_kafka_conf_set(KafkaConf, PChar(name), PChar(value), PChar(errstr), errstr_size);
  if MyRetVal <> RD_KAFKA_CONF_OK then
    raise Exception.CreateFmt('Configparam "%s"; value "%s" failed failed:%s', [name, value, String (errstr)]);
end;

procedure TKafkaClient.ConfigureParamList(Params: TStringList);
var
  x: Integer;
begin
  if not Assigned(Params) then
    raise Exception.Create('TKafkaClient.Configure(Params: TStringList); no argument');
  for x := 0 to Params.Count - 1 do
  begin
    ConfigureParam(Params.Names[x], Params.ValueFromIndex[x]);
  end;
end;

constructor TKafkaClient.Create;
begin
  inherited Create;
  _FormatSettings.DecimalSeparator := '.';
  _FormatSettings.ThousandSeparator := ',';
  _FormatSettings.DateSeparator:='-';
  _FormatSettings.TimeSeparator:=':';
  _FormatSettings.LongDateFormat:='yyyy-mm-dd';
  _FormatSettings.ShortDateFormat:='yyyy-mm-dd';
  _FormatSettings.LongTimeFormat:='hh:nn:ss.zzz';
  _FormatSettings.ShortTimeFormat:='hh:nn:ss.zzz';
  if kafka.rd_kafka_handle = 0 then
    raise Exception.CreateFmt ('kafka library "%s" not loaded', [kafka.RD_EXPORT]);
  KafkaConf := rd_kafka_conf_new();
end;

end.

