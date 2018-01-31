unit tacointerface;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  Classes
   , Controls
   , StrUtils
   , Xmlz
   , IdTCPClient
   , IdGlobal
   , IdSync
   , SyncObjs
   , ExtCtrls
   ;

type
  TRtReturnType = (rtUndefined, rtReply, rtError);
  TTacoInterface = class;
  TOnHaveTacoMessage = procedure ( aTacoInterface: TTacoInterface
                                 ; aClientId, aMessage: String
                                 ) of Object;
  TOnNeedTacoInterfaceData = procedure (aTacoInterface: TTacoInterface) of Object;

  { TTacoInterface }

  TTacoInterface = class
  private
    fAuthorized: Boolean;
    fAuthorisation : String ;
    fClient: TIdTCPClient;
    fHost: String;
    fNeedHostData : TOnNeedTacoInterfaceData ;
    fOnAuthorize : TNotifyEvent ;
    fPort: Integer;
    fOnHaveTacoMessage: TOnHaveTacoMessage;
    fReqTime: TDateTime;
    fRspTime: TDateTime;
    fTacoReply: String;
    fReady: Boolean;
    fReturnType: TRtReturnType;
    fReply: String;
    fLock: TCriticalSection;
    procedure clientConnected(Sender: TObject);
    procedure clientDisconnected(Sender: TObject);
    procedure setAuthorized (AValue : boolean );
    procedure syncDoAuthorize;
    procedure syncOnAuthorize;
    procedure doAuthorize;
    function tacoRequest (aRequest: String; aConfig: TXml): String;
    function tacoCopy (aString: String; var x: Integer): String;
    procedure EvaluateResponse;
  public
    UserName, Password: String;
    function tacoString (aString: String): String;
    function decodeTacoString (aString: String; var aOffset: Integer): String;
    property Host: String read fHost write fHost;
    property Port: Integer read fPort write fPort;
    property Authorized: boolean read fAuthorized write setAuthorized;
    property Authorisation: String read fAuthorisation write fAuthorisation;
    property ReturnType: TRtReturnType read fReturnType;
    property NeedHostData: TOnNeedTacoInterfaceData write fNeedHostData;
    property OnAuthorize: TNotifyEvent read fOnAuthorize write fOnAuthorize;
    procedure doTerminate;
    procedure Connect;
    procedure Disconnect;
    constructor Create(Owner: TComponent; aOnHaveTacoMessage: TOnHaveTacoMessage);
    constructor CreateFromXml(aXml: TXml; aOnHaveTacoMessage: TOnHaveTacoMessage);
    destructor Destroy; override;
    procedure PingPong;
    function RequestReply ( aRequest: String
                          ; aTimeOut: Integer
                          ; aConfigAsXml: TXml
                          ): String; Virtual;
    function tacoCommand (aCommand: String): String;
    function AsXml: TXml;
  published
    property RequestTimestamp: TDateTime read fReqTime;
    property ReplyTimestamp: TDateTime read fRspTime;
  end;

implementation

uses SysUtils
   , xmlio
   , igGlobals
   ;
{ TTacoInterface }

function TTacoInterface.AsXml: TXml;
begin
  result := nil;
end;

procedure TTacoInterface.Connect;
begin

end;

constructor TTacoInterface.Create(Owner: TComponent;
  aOnHaveTacoMessage: TOnHaveTacoMessage);
begin
  fLock := SyncObjs.TCriticalSection.Create;
  fOnHaveTacoMessage := aOnHaveTacoMessage;
  fClient := TIdTCPClient.Create(nil);
  fClient.OnDisconnected := clientDisconnected;
  Authorized := False;
end;

constructor TTacoInterface.CreateFromXml(aXml: TXml;
  aOnHaveTacoMessage: TOnHaveTacoMessage);
begin

end;

destructor TTacoInterface.Destroy;
begin
  fClient.Disconnect;
  fClient.Free;
  fLock.Free;
  inherited;
end;

procedure TTacoInterface.PingPong ;
begin
  fLock.Acquire;
  try
    if not Authorized then
      Exit;
    fReturnType := rtUndefined;
    fReady := False;
    fTacoReply := '';
    fClient.IOHandler.Write('<PING><END-OF-DATA>');
    while not fReady do
    begin
      fTacoReply := fTacoReply + fClient.IOHandler.ReadString(1);
      fReady := (system.Length(fTacoReply) > 12)
            and (Copy (fTacoReply, Length (fTacoReply) - 12, 13) = '<END-OF-DATA>')
              ;
    end;
//  EvaluateResponse;
  finally
    fLock.Release;
  end;
end;

procedure TTacoInterface.Disconnect;
begin
  fClient.Disconnect;
end;

procedure TTacoInterface.doTerminate;
begin

end;


procedure TTacoInterface.EvaluateResponse;
var
  x: Integer;
begin
  fReply := '';
  if (fTacoReply = '<PONG><OK><END-OF-DATA>')
  or (fTacoReply = '<PONG><END-OF-DATA>')
  then
  begin
    fReturnType := rtReply;
    Exit;
  end;
  if fTacoReply = '<AUTHORISE-CLOSED><END-OF-DATA>' then
  begin
    fClient.Disconnect;
    Authorized := False;
    Raise Exception.Create('Authorisation failed.' + LineEnding +
        'For security reasons, the server has stopped running;' + LineEnding +
        'Restart the server and supply the new authorisation string');
  end;
  if AnsiStartsStr ('<REPLY>', fTacoReply) then
  begin
    fReturnType := rtReply;
    x := 8;
    fReply := tacoCopy(fTacoReply, x);
    Exit;
  end;
  if AnsiStartsStr ('<ERR>', fTacoReply) then
  begin
    fReturnType := rtError;
    x := 6;
    fReply := tacoCopy(fTacoReply, x);
    raise Exception.Create('Taco server error: ' + fReply);
  end;
  raise Exception.Create('tacoInterface: Illegal Taco response: ' + fTacoReply);
end;

function TTacoInterface.RequestReply(aRequest: String; aTimeOut: Integer;
  aConfigAsXml: TXml): String;
var
  xHost: String;
  xPort: Integer;
  ms: TStringStream;
  b: Byte;
begin
  fLock.Acquire;
  try
    doAuthorize;
    if not Authorized then
      Exit;
    fReturnType := rtUndefined;
    fReady := False;
    fTacoReply := '';
    ms := TStringStream.Create(tacoRequest(aRequest, aConfigAsXml));
    try
      ms.Position := 0;
      fClient.IOHandler.Write(ms, ms.Size);
    finally
      ms.Free;
    end;
    while not fReady do
    begin
      b := fClient.IOHandler.ReadByte;
      SetLength(fTacoReply, system.Length (fTacoReply) + 1);
      fTacoReply [system.Length (fTacoReply)] := Char(b);
      fReady := (system.Length(fTacoReply) > 12)
            and (fTacoReply [system.Length(fTacoReply) - 12] = '<') // avoid unnessary copy
            and (Copy (fTacoReply, Length (fTacoReply) - 12, 13) = '<END-OF-DATA>')
              ;
    end;
    EvaluateResponse;
    result := fReply;
  finally
    fLock.Release;
  end;
end;

function TTacoInterface .tacoCommand (aCommand: String): String;
var
  ms: TStringStream;
  b: Byte;
begin
  result := '';
  fLock.Acquire;
  try
    doAuthorize;
    if not Authorized then
      Exit;
    fReturnType := rtUndefined;
    fReady := False;
    fTacoReply := '';
    ms := TStringStream.Create(aCommand + '<END-OF-DATA>');
    try
      ms.Position := 0;
      fClient.IOHandler.Write(ms, ms.Size);
    finally
      ms.Free;
    end;
    while not fReady do
    begin
      b := fClient.IOHandler.ReadByte;
      SetLength(fTacoReply, system.Length (fTacoReply) + 1);
      fTacoReply [system.Length (fTacoReply)] := Char(b);
      fReady := (system.Length(fTacoReply) > 12)
            and (fTacoReply [system.Length(fTacoReply) - 12] = '<') // avoid unnessary copy
            and (Copy (fTacoReply, Length (fTacoReply) - 12, 13) = '<END-OF-DATA>')
              ;
    end;
    result := fTacoReply;
  finally
    fLock.Release;
  end;
end;

function TTacoInterface.tacoCopy(aString: String;
  var x: Integer): String;
var
  L, S: Integer;
begin
  if aString[x] <> '<' then
    raise Exception.Create('tacoInterface: Illegal Taco response: ' + aString);
  L := x;
  while (x <= Length (aString))
  and (aString [x] <> '>') do
    Inc (x);
  if x > Length (aString) then
    raise Exception.Create('tacoInterface: Illegal Taco response: ' + aString);
  S := StrToInt (Copy (aString, L + 1, x - L - 1));
  result := Copy (aString, x + 1, S);
  x := x + 1 + S;
end;

function TTacoInterface.tacoRequest(aRequest: String; aConfig: TXml): String;
var
  xXml, mqXml: TXml;
  s: String;
  x: Integer;
begin
  result := '';
  xXml := aConfig.FindUQXml('Taco.messageBroker.Pathsend');
  if Assigned (xXml)
  and (xXml.Checked) then
  begin
    result := result + '<SEND-S';
    s := xXml.Items.XmlValueByTag['TMF'];
    if s = 'Commit' then
      result := result + 'T';
    if s = 'Abort' then
      result := result + 'A';
    s := xXml.Items.XmlValueByTag['SBulk'];
    if s = 'NormalIO' then
      result := result + '-B1';
    if s = 'LargeIO' then
      result := result + '-B4';
    result := result + '>';
    result := result + tacoString (resolveAliasses(xXml.Items.XmlValueByTag['Monitor']))
                     + tacoString (resolveAliasses(xXml.Items.XmlValueByTag['Server']))
                     ;
 {}
  end
  else
  begin
    xXml := aConfig.FindUQXml('Taco.messageBroker.Mq');
    if Assigned (xXml)
    and xXml.Checked then
    begin
      mqXml := nil;
      for x := 0 to xXml.Items.Count - 1 do
        if xXml.Items.XmlItems[x].Checked then
          mqXml := xXml.Items.XmlItems[x];
      if not Assigned (mqXml) then
        raise Exception.Create('tacoInterface: Illegal MQ config');
      result := result
              + '<MQSEND>'
              + tacoString (resolveAliasses(mqXml.Items.XmlValueByTag['Manager']))
              + tacoString (resolveAliasses(mqXml.Items.XmlValueByTag['GetQueue']))
              + tacoString (resolveAliasses(mqXml.Items.XmlValueByTag['PutQueue']))
              + tacoString (resolveAliasses(mqXml.Items.XmlValueByTag['ReplyToQueue']))
              + tacoString (resolveAliasses(mqXml.Items.XmlValueByTag['TimeOut']))
              ;
    end;
  end;
  result := result + tacoString (aRequest) + '<END-OF-DATA>';
end;

procedure TTacoInterface .clientConnected (Sender : TObject );
begin
  Authorized := False;
end;

procedure TTacoInterface .clientDisconnected (Sender : TObject );
begin
  Authorized := False;
end;

procedure TTacoInterface .setAuthorized (AValue : boolean );
begin
  if fAuthorized = AValue then Exit ;
  fAuthorized := AValue ;
  if Assigned (fOnAuthorize) then
    with TIdSync.Create do
    begin
      try
        SynchronizeMethod (syncOnAuthorize);
      finally
        free;
      end;
    end;
end;

procedure TTacoInterface .syncDoAuthorize ;
begin
  fNeedHostData(Self);
end;

procedure TTacoInterface .syncOnAuthorize ;
begin
  IF Assigned (fOnAuthorize) then
    fOnAuthorize (self);
end;

procedure TTacoInterface .doAuthorize ;
begin
  if Authorized then
    Exit;
  if not Assigned (fNeedHostData) then
    raise Exception.Create('TTacoInterface: no OnNeedHostData assigned');
  Authorisation := '';
  fClient.Disconnect;
  with TIdSync.Create do
  begin
    try
      SynchronizeMethod (syncDoAuthorize);
    finally
      free;
    end;
  end;
  if Authorisation = '' then
    raise Exception.Create('TacoInterface: aborted due to user action');
  fClient.Host := Host;
  fClient.Port := Port;
  fClient.Connect;
  fReady := False;
  fTacoReply := '';
  fClient.IOHandler.Write ( '<AUTHORISE>'
                          + tacoString(Authorisation)
                          + tacoString(UserName)
                          + '<END-OF-DATA>'
                          );
  while not fReady do
  begin
    fTacoReply := fTacoReply + fClient.IOHandler.ReadString(1);
    fReady := (system.Length(fTacoReply) > 12)
          and (Copy (fTacoReply, Length (fTacoReply) - 12, 13) = '<END-OF-DATA>')
            ;
  end;
  if fTacoReply = '<NOT-AUTHORISED><END-OF-DATA>' then
  begin
    Authorized := False;
    Raise Exception.Create('Not authorised');
  end;
  if fTacoReply = '<AUTHORISE-FAILED><END-OF-DATA>' then
  begin
    Authorized := False;
    Raise Exception.Create(
      'Authorisation failed; Please supply correct authorisation string');
  end;
  if fTacoReply = '<AUTHORISE-CLOSED><END-OF-DATA>' then
  begin
    Authorized := False;
    Raise Exception.Create('Authorisation failed.' + LineEnding +
        'For security reasons, the server has stopped running;' + LineEnding +
        'Restart the server and supply the new authorisation string');
  end;
  if fTacoReply = '<AUTHORISE-SUCCESSFUL><END-OF-DATA>' then
  begin
    Authorized := True;
    exit;
  end;
end;

function TTacoInterface.tacoString(aString: String): String;
begin
  result := '<'
          + IntToStr (Length (aString))
          + '>'
          + aString
          ;
end;

function TTacoInterface .decodeTacoString (aString : String; var aOffset: Integer): String ;
var
  iString: String;
begin
  { <13>sofac.bouwman }

  iString := '';
  if aString[aOffset] = '<' then
    Inc(aOffset);
  iString := '';
  while (aOffset <= Length(aString)) and (aString[aOffset] <> '>') do
  begin
    iString := iString + aString[aOffset];
    Inc(aOffset);
  end;
  Inc(aOffset);
  result := Copy(aString, aOffset, StrToInt(iString));
  aOffset := aOffset + StrToInt(iString);
end;

end.
