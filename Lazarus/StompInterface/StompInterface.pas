unit StompInterface;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ TODO:
  doWait in browse
}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Classes
   , Controls
   , StrUtils
   , StompTypes
   , StompClient
   , Xmlz
   ;

type
  TStompInterface = class;
  TOnHaveFrame = procedure ( aStompInterface: TStompInterface
                           ; aQueue: String
                           ; aFrame: IStompFrame
                           ) of Object;

  TStompInterface = class(TComponent)
  private
    fStompClient: IStompClient;
    fHost, fUserName, fPassword, fClientId, fReplyBodyPostFix, fRequestBodyPostFix: String;
    fUseCredentials: Boolean;
    fPort: Integer;
    fOnHaveFrame: TOnHaveFrame;
    fStompClientOK: Boolean;
    ResponseMicroSeconds: Integer;
    fReqTime: TDateTime;
    fRspTime: TDateTime;
    xTimeOut: String;
    xExpiryTime: String;
    fGetThreads: TStringList;
    fGetQueues: TStringList;
    fDequeueOn: String;
    procedure addCustomHeaders (aHeader: IStompHeaders; aXml: TXml);
    procedure mergeHeader (aHeader: IStompHeaders; aXml: TXml);
    function getConnected: Boolean;
    procedure Browse (aQueue: String);
  public
    property Host: String read fHost write fHost;
    property Port: Integer read fPort write fPort;
    property UseCredentials: Boolean read fUseCredentials write fUseCredentials;
    property UserName: String read fUserName write fUserName;
    property Password: String read fPassword write fPassword;
    property ReplyBodyPostFix: String read fReplyBodyPostFix write fReplyBodyPostFix;
    property RequestBodyPostFix: String read fRequestBodyPostFix write fRequestBodyPostFix;
    property ClientId: String read fClientId write fClientId;
    property DequeueOn: String read fDequeueOn write fDequeueOn;
    property GetQueues: TStringList read fGetQueues;
    property Connected: Boolean read getConnected;
    property StompClient: IStompClient read fStompClient;
    procedure doTerminate;
    procedure Connect;
    procedure Disconnect;
    constructor Create(Owner: TComponent; aOnHaveFrame: TOnHaveFrame);
    constructor CreateFromXml(aXml: TXml; aOnHaveFrame: TOnHaveFrame);
    destructor Destroy; override;
    procedure Put (aMessage: String; aStompHeaderAsXml, aCustomHeaders: TXml; var aHeaderAsText: String);
    procedure PutReply (aMessage: String; aFrame: IStompFrame; var aHeaderAsText: String);
    function RequestReply ( aRequest: String
                          ; aTimeOut: Integer
                          ; aStompHeaderAsXml, aCustomHeaders: TXml
                          ; var aRequestHeader: String
                          ; var aReplyHeader: String
                          ): String; Virtual;
    function AsXml: TXml;
    function AsXmlOldStyle: TXml;
  published
    property TimeOut: String read xTimeOut write xTimeOut;
    property Expiry: String read xExpiryTime write xExpiryTime;
    property RequestTimestamp: TDateTime read fReqTime;
    property ReplyTimestamp: TDateTime read fRspTime;
  end;

  TStompGetThread = class(TThread)
  private
    fStompInterface: TStompInterface;
    fFrame: IStompFrame;
    fQueue: String;
    procedure fSynchronised;
  protected
    procedure Execute; override;
  public
    constructor Create ( aStompInterface: TStompInterface
                       ; aQueue: String
                       );
  end;

implementation

uses IdSync
   , SysUtils
   ;

type TOnHaveFrameThread = class(TThread)
private
  fOnHaveFrame: TOnHaveFrame;
  fSender: TStompInterface;
  fQueue: String;
  fFrame: IStompFrame;
protected
  procedure Execute; override;
public
  constructor Create ( aOnHaveFrame: TOnHaveFrame
                     ; aSender: TStompInterface
                     ; aQueue: String
                     ; aFrame: IStompFrame
                     );
end;

{ TStompGetThread }

constructor TStompGetThread.Create ( aStompInterface: TStompInterface
                                   ; aQueue: String
                                   );
begin
  inherited Create (False);
  fStompInterface := aStompInterface;
  fQueue := aQueue;
  FreeOnTerminate := True;
end;

procedure TStompGetThread.Execute;
begin
  fStompInterface.fStompClient.Subscribe(fQueue, amClient);
  while not terminated do
  begin
    fFrame := nil;
    fFrame := fStompInterface.fStompClient.Receive(500);
    if Assigned (fFrame) then
    begin
      if (fFrame.GetCommand = 'MESSAGE')
      or (fFrame.GetCommand = 'ERROR') then
      begin
        if fStompInterface.DequeueOn = 'Browse' then
          fStompInterface.fStompClient.Ack (fFrame.GetHeaders.Value ('message-id'));
        TOnHaveFrameThread.Create ( fStompInterface.fOnHaveFrame
                                  , fStompInterface
                                  , fQueue
                                  , fFrame
                                  );
      end;
    end;
  end;
  with fStompInterface do
    fStompClient.Unsubscribe(fQueue);
end;

procedure TStompGetThread.fSynchronised;
begin
  fStompInterface.fOnHaveFrame (fStompInterface, fQueue, fFrame);
end;

{ TStompInterface }

constructor TStompInterface.CreateFromXml(aXml: TXml; aOnHaveFrame: TOnHaveFrame);
var
  sXml: TXml;
  x: Integer;
begin
  if not Assigned (aXml) then raise Exception.Create ('TStompInterface.CreateFromXml: No XML');
  if (aXml.Name <> 'stompInterface')
  and (aXml.Name <> 'Broker')
  then raise Exception.Create ('TStompInterface.CreateFromXml: Illegal XML ' + aXml.Text);
  inherited Create(Nil);  // Initialize inherited parts
  fStompClient := TStompClient.Create;
  fOnHaveFrame := aOnHaveFrame;
  fGetQueues := TStringList.Create;
  fGetQueues.Sorted := False;
  fGetThreads := TStringList.Create;
  fGetThreads.Sorted := False;
  Host := aXml.Items.XmlCheckedValueByTagDef ['Host', 'localhost'];
  Port := aXml.Items.XmlCheckedIntegerByTagDef ['Port', 61613];
  sXml := aXml.Items.XmlCheckedItemByTag ['Credentials'];
  if Assigned (sXml) then
  begin
    UseCredentials := True;
    with sXml.Items do
    begin
      UserName := XmlCheckedValueByTagDef['Name', 'guest'];
      Password := Xmlz.DecryptString(XmlCheckedValueByTag['Password']);
    end;
  end;
  ReplyBodyPostFix := aXml.Items.XmlCheckedValueByTagDef ['ReplyBodyPostFix', ''];
  RequestBodyPostFix := aXml.Items.XmlCheckedValueByTagDef ['RequestBodyPostFix', ''];
  ClientId := aXml.Items.XmlCheckedValueByTagDef ['ClientId', ''];
  DequeueOn := aXml.Items.XmlCheckedValueByTagDef ['DequeueOn', 'Process'];
  sXml := aXml.Items.XmlCheckedItemByTag ['Queues'];
  if Assigned (sXml) then
    for x := 0 to sXml.Items.Count - 1 do
      if (sXml.Items.XmlItems[x].Name = 'Queue')
      and sXml.Checked then
        fGetQueues.Add (sXml.Items.XmlItems[x].Value);
end;

constructor TStompInterface.Create(Owner: TComponent; aOnHaveFrame: TOnHaveFrame);
begin
  inherited Create (Owner);
  fStompClient := TStompClient.Create;
  fOnHaveFrame := aOnHaveFrame;
  fGetQueues := TStringList.Create;
  fGetQueues.Sorted := False;
  fGetThreads := TStringList.Create;
  fGetThreads.Sorted := False;
  if not Assigned (fOnHaveFrame) then
    raise Exception.Create ('TStompInterfaces requires an onHaveFrame procedure');
end;

destructor TStompInterface.Destroy;
var
  x: Integer;
begin
  for x := 0 to fGetQueues.Count - 1 do
    if Assigned (fGetQueues.Objects[x]) then
      (fGetQueues.Objects[x] as TStompGetThread).Terminate;
  fGetQueues.Free;
  fGetThreads.Free;
  if fStompClient.Connected then
    fStompClient.Disconnect;
  fStompClient := nil;
  inherited;
end;

function TStompInterface.AsXml: TXml;
var
  x: Integer;
begin
  result := TXml.CreateAsString ('Broker', '');
  with result do
  begin
    AddXml (TXml.CreateAsString ('Host', fHost));
    AddXml (TXml.CreateAsInteger ('Port', fPort));
    if fUseCredentials then
      with AddXml (TXml.CreateAsString('Credentials', '')) do
      begin
        AddXml (TXml.CreateAsString('Name', fUserName));
        AddXml (TXml.CreateAsString('Password', Xmlz.EncryptString(fPassword)));
      end;
    if fReplyBodyPostFix <> '' then
      AddXml (TXml.CreateAsString ('ReplyBodyPostFix', fReplyBodyPostFix));
    if fRequestBodyPostFix <> '' then
      AddXml (TXml.CreateAsString ('RequestBodyPostFix', fRequestBodyPostFix));
    AddXml (TXml.CreateAsString ('ClientId', fClientId));
    AddXml (TXml.CreateAsString ('DequeueOn', DequeueOn));
    with AddXml (TXml.CreateAsString ('Queues', '')) do
      for x := 0 to GetQueues.Count - 1 do
        AddXml (TXml.CreateAsString ('Queue', GetQueues.Strings[x]));
  end;
end;

function TStompInterface.AsXmlOldStyle: TXml;
begin
  result := AsXml;
  result.Name := 'stompInterface';
end;

procedure TStompInterface.Connect;
var
  x: Integer;
begin
  if UseCredentials then
  begin
    fStompClient.SetUserName(UserName);
    fStompClient.SetPassword(Password);
  end;
  fStompClient.Connect (Host, Port, ClientId);
  for x := 0 to GetQueues.Count - 1 do
    Browse (GetQueues.Strings [x]);
end;

procedure TStompInterface.Browse (aQueue: String);
begin
  fGetThreads.AddObject ('', TStompGetThread.Create (self, '/queue/' + aQueue));
end;

procedure TStompInterface.Put(aMessage: String; aStompHeaderAsXml, aCustomHeaders: TXml; var aHeaderAsText: String);
var
  xHeader: IStompHeaders;
  xDestination: String;
begin
  xHeader := StompUtils.NewHeaders
//                      .Add(TStompHeaders.NewPersistentHeader(False))
                        ;
  xDestination := '/queue/' + aStompHeaderAsXml.Items.XmlValueByTagDef ['destination', ''];
  if xDestination = '/queue/' then
    raise Exception.Create ('TStompInterface.Put: No destination specified');
  mergeHeader (xHeader, aStompHeaderAsXml);
  addCustomHeaders(xHeader, aCustomHeaders);
  fStompClient.Send ( xDestination
                    , aMessage
                    , xHeader
                    );
  aHeaderAsText := xHeader.OutputAsXmlText;
end;

procedure TStompInterface.PutReply (aMessage: String; aFrame: IStompFrame; var aHeaderAsText: String);
var
  xHeader: IStompHeaders;
begin
  xHeader := StompUtils.NewHeaders
                        .Add('correlation-id', aFrame.GetHeaders.Value ('correlation-id'))
                        .Add(TStompHeaders.NewPersistentHeader(False))
                        ;
//mergeHeader (xHeader, aStompHeaderAsXml);
  fStompClient.Send ( aFrame.GetHeaders.Value ('reply-to')
                    , aMessage
                    + ReplyBodyPostFix // WORKAROUND see xsd
                    , xHeader
                    );
  aHeaderAsText := xHeader.OutputAsXmlText;
end;

procedure TStompInterface.doTerminate;
var
  x: Integer;
begin
  for x := fGetThreads.Count - 1 downto 0 do
  begin
    if Assigned (fGetThreads.Objects [x]) then
    begin
      try (fGetThreads.Objects [x] as TStompGetThread).DoTerminate; except end;
      fGetThreads.Objects [x] := nil;
    end;
  end;
  fGetThreads.Clear;
end;

procedure TStompInterface.Disconnect;
begin
  if not Assigned (Self) then Exit;
  if not Connected then Exit;
  doTerminate;
  fStompClient.Disconnect;
end;

function TStompInterface.getConnected: Boolean;
begin
  result := fStompClient.Connected;
end;

function TStompInterface.RequestReply(aRequest: String; aTimeOut: Integer;
  aStompHeaderAsXml, aCustomHeaders: TXml; var aRequestHeader: String; var aReplyHeader: String): String;
  function genCorrelId: String;
  var
    x: Integer;
  begin
    DateTimeToString (result, 'yyyymmddhhmmsszzz', Now);
    result := result + IntToStr (Random (10000000));
  end;
var
  x: Integer;
  xPutQueue, xGetQueue, xCorrelationId: String;
  xFrame: IStompFrame;
  xFound: Boolean;
  xTimeOut: TDateTime;
  xHeader: IStompHeaders;
begin
  xPutQueue := '/queue/' + aStompHeaderAsXml.Items.XmlValueByTag ['destination'];
  xGetQueue := '/queue/' + aStompHeaderAsXml.Items.XmlValueByTag ['reply-to'];
  if xPutQueue = '/queue/' then
    raise Exception.Create ('no destination queue specified');
  if xGetQueue = '/queue/' then
    raise Exception.Create ('no reply-to queue specified');
  xCorrelationId := genCorrelId;
  xHeader := StompUtils.NewHeaders
                        .Add('correlation-id', xCorrelationId)
                        .Add(TStompHeaders.NewReplyToHeader(xGetQueue))
//                      .Add(TStompHeaders.NewPersistentHeader(False))
                        ;
  mergeHeader (xHeader, aStompHeaderAsXml);
  addCustomHeaders(xHeader, aCustomHeaders);
  fStompClient.Send ( xPutQueue
                    , aRequest
                    , xHeader
                    );
  aRequestHeader := xHeader.OutputAsXmlText;
  fStompClient.Subscribe(xGetQueue, amClient);
  xFound := False;
  xTimeOut := Now + aTimeOut / (60 * 60 * 24);
  while not xFound
  and (Now < xTimeOut) do
  begin
    xFrame := nil;
    xFrame := fStompClient.Receive(500);
    if Assigned (xFrame) then
    begin
      if xFrame.GetCommand = 'ERROR' then
        fOnHaveFrame (self, xGetQueue, xFrame);
      if (xFrame.GetCommand = 'MESSAGE')
      and (xFrame.GetHeaders.Value ('correlation-id') = xCorrelationId) then
        xFound := True;
    end;
  end;
  if xFound then
  begin
    result := xFrame.GetBody;
    aReplyHeader := xFrame.GetHeaders.OutputAsXmlText;
    fStompClient.Ack (xFrame.GetHeaders.Value ('message-id'));
  end
  else
    raise Exception.Create (xGetQueue + ': timeout');
  fStompClient.Unsubscribe(xGetQueue);
end;

procedure TStompInterface.addCustomHeaders (aHeader: IStompHeaders; aXml: TXml);
var
  x: Integer;
  xName, xValue: String;
begin
  if not Assigned (aXml) then Exit;
  if not aXml.Checked then Exit;
  with aXml.Items do
    for x := 0 to Count - 1 do
      if (XmlItems[x].Name = 'Header')
      and (XmlItems[x].Checked) then
        with XmlItems[x].Items do
          aHeader.Add (XmlCheckedValueByTag ['Name'], XmlCheckedValueByTag ['Value']);
end;

procedure TStompInterface.mergeHeader(aHeader: IStompHeaders; aXml: TXml);
var
  x: Integer;
begin
  if not Assigned (aXml) then Exit;
  with aXml.Items do
  begin
    if XmlValueByTag ['persistent'] = '' then
      aHeader.Add ('persistent', 'False');
    for x := 0 to Count - 1 do
      if XmlItems[x].Checked
      and (XmlItems[x].Name <> 'destination') then
        if (XmlItems[x].Name = 'reply-to') then
          aHeader.Add (XmlItems[x].Name, '/queue/' + XmlItems[x].Value)
        else
          aHeader.Add (XmlItems[x].Name, XmlItems[x].Value);
  end;
end;

{ TOnHaveFrameThread }

constructor TOnHaveFrameThread.Create(aOnHaveFrame: TOnHaveFrame;
  aSender: TStompInterface; aQueue: String; aFrame: IStompFrame);
begin
  inherited Create (False);
  fOnHaveFrame := aOnHaveFrame;
  fSender := aSender;
  fQueue := aQueue;
  fFrame := aFrame;
end;

procedure TOnHaveFrameThread.Execute;
begin
  fOnHaveFrame (fSender, fQueue, fFrame);
end;

end.
